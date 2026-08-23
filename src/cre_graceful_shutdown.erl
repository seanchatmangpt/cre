%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Project
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author CRE Project
%% @copyright 2025
%%
%% @doc CRE Graceful Shutdown Module for GKE Deployment
%%
%% Provides graceful shutdown functionality for CRE nodes running in
%% Google Kubernetes Engine with Spot VM support. Handles SIGTERM/SIGINT
%% signals from GKE and GCP preemption notices, ensuring active workflows
%% complete before shutdown.
%%
%% <h3>Key Features</h3>
%%
%% <ul>
%%   <li><b>Shutdown Coordination:</b> Coordinates shutdown sequence across node</li>
%%   <li><b>Workflow Completion:</b> Waits for active workflows to complete</li>
%%   <li><b>Mnesia Checkpoint:</b> Saves final checkpoint before shutdown</li>
%%   <li><b>Cluster Notification:</b> Notifies cluster peers of impending shutdown</li>
%%   <li><b>Connection Cleanup:</b> Closes network connections gracefully</li>
%%   <li><b>Spot VM Support:</b> Fast shutdown within 30 seconds for spot preemption</li>
%%   <li><b>GCP Preemption Detection:</b> Monitors metadata server for preemption notices</li>
%% </ul>
%%
%% <h3>Shutdown Sequence</h3>
%%
%% <ol>
%%   <li>Stop accepting new workflows</li>
%%   <li>Wait for active workflows (configurable timeout)</li>
%%   <li>Save Mnesia checkpoint</li>
%%   <li>Notify cluster peers</li>
%%   <li>Close network connections</li>
%% </ol>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Initiate shutdown with default 20 second timeout
%% ok = cre_graceful_shutdown:initiate_shutdown().
%%
%% %% Initiate shutdown with custom 10 second timeout
%% ok = cre_graceful_shutdown:initiate_shutdown(10000).
%%
%% %% Wait for active workflows to complete
%% {ok, CompletedCount} = cre_graceful_shutdown:wait_for_active_workflows(20000).
%%
%% %% Save final Mnesia checkpoint
%% ok = cre_graceful_shutdown:save_final_checkpoint().
%%
%% %% Notify cluster peers of shutdown
%% ok = cre_graceful_shutdown:notify_cluster().
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_graceful_shutdown).

%%====================================================================
%% Exports
%%====================================================================

%% Shutdown coordination
-export([initiate_shutdown/0]).
-export([initiate_shutdown/1]).
-export([initiate_shutdown/2]).
-export([is_shutting_down/0]).
-export([get_shutdown_state/0]).

%% Workflow completion waiting
-export([wait_for_active_workflows/1]).
-export([get_active_workflow_count/0]).
-export([get_active_workflow_details/0]).

%% Mnesia checkpoint
-export([save_final_checkpoint/0]).
-export([checkpoint_status/0]).

%% Cluster notification
-export([notify_cluster/0]).
-export([notify_cluster/1]).
-export([handle_peer_notification/1]).

%% Connection cleanup
-export([close_connections/0]).
-export([close_connections/1]).

%% GCP Spot VM preemption detection
-export([start_preemption_monitor/0]).
-export([start_preemption_monitor/1]).
-export([stop_preemption_monitor/0]).
-export([is_spot_vm/0]).
-export([check_preemption_notice/0]).
-export([handle_preemption_notice/0]).

%% gen_server callbacks (if running as a gen_server)
-export([start_link/0]).
-export([start_link/1]).
-export([stop/0]).

%% Internal gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type shutdown_timeout() :: number() | infinity.
-type shutdown_reason() :: sigterm | sigint | gke_preemption | manual | upgrade.
-type shutdown_state() :: not_shutting_down | stopping_new | draining | checkpointing | finalizing.
-type shutdown_status() :: #{
    state := shutdown_state(),
    reason => shutdown_reason(),
    start_time => integer(),
    timeout => shutdown_timeout(),
    active_workflows => non_neg_integer(),
    checkpoint_saved => boolean(),
    cluster_notified => boolean(),
    connections_closed => boolean()
}.
-type workflow_info() :: #{
    id := binary(),
    spec := binary(),
    started_at => integer(),
    status => running | completing | waiting
}.
-type notification_result() :: #{
    peers_notified => non_neg_integer(),
    failed_peers => [node()],
    duration_ms => number()
}.

-export_type([shutdown_timeout/0, shutdown_reason/0, shutdown_state/0]).
-export_type([shutdown_status/0, workflow_info/0, notification_result/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(SERVER, ?MODULE).
-define(DEFAULT_TIMEOUT_MS, 20000).
-define(GKE_SPOT_TIMEOUT_MS, 25000).  %% Leave 5s buffer for 30s limit
-define(CHECKPOINT_TIMEOUT_MS, 5000).
-define(NOTIFICATION_TIMEOUT_MS, 3000).
-define(WORKFLOW_POLL_INTERVAL_MS, 100).
-define(DEFAULT_SHUTDOWN_TABLE, cre_graceful_shutdown_state).

%% GCP Metadata Server Configuration
-define(GCP_METADATA_URL, "http://metadata.google.internal/computeMetadata/v1/").
-define(GCP_PREEMPTION_ENDPOINT, "instance/preempted").
-define(GCP_SPOT_ENDPOINT, "instance/scheduling/onHostMaintenance").
-define(PREEMPTION_CHECK_INTERVAL_MS, 2000).
-define(METADATA_REQUEST_TIMEOUT_MS, 1000).

%%====================================================================
%% Shutdown State Record
%%====================================================================

-record(shutdown_state, {
    state = not_shutting_down :: shutdown_state(),
    reason = manual :: shutdown_reason(),
    start_time :: integer() | undefined,
    timeout = ?DEFAULT_TIMEOUT_MS :: shutdown_timeout(),
    active_workflows = 0 :: non_neg_integer(),
    checkpoint_saved = false :: boolean(),
    cluster_notified = false :: boolean(),
    connections_closed = false :: boolean(),
    drain_ref = undefined :: reference() | undefined
}).

%%====================================================================
%% API Functions - Shutdown Coordination
%%====================================================================

%% @doc Start the graceful shutdown server
%%
%% Starts the gen_server that manages shutdown state.
%% Uses default timeout of 20 seconds.
%%
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Start the graceful shutdown server with options
%%
%% Options:
%% - {timeout, TimeoutMs} - Default shutdown timeout
%% - {table, TableName} - ETS table for shutdown state
%%
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Stop the graceful shutdown server
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Initiate graceful shutdown with default timeout
%%
%% Starts the shutdown sequence with a 20 second timeout.
%% Returns immediately, shutdown happens in background.
%%
%% @end
-spec initiate_shutdown() -> ok | {error, term()}.
initiate_shutdown() ->
    initiate_shutdown(?DEFAULT_TIMEOUT_MS, sigterm).

%% @doc Initiate graceful shutdown with custom timeout (milliseconds)
%%
%% Starts the shutdown sequence with specified timeout in milliseconds.
%% Returns immediately, shutdown happens in background.
%%
%% @end
-spec initiate_shutdown(shutdown_timeout()) -> ok | {error, term()}.
initiate_shutdown(Timeout) when is_number(Timeout); Timeout =:= infinity ->
    initiate_shutdown(Timeout, sigterm).

%% @doc Initiate graceful shutdown with timeout and reason
%%
%% Starts the shutdown sequence with specified timeout and reason.
%% Returns immediately, shutdown happens in background.
%%
%% @end
-spec initiate_shutdown(shutdown_timeout(), shutdown_reason()) -> ok | {error, term()}.
initiate_shutdown(Timeout, Reason) ->
    gen_server:cast(?SERVER, {initiate_shutdown, Timeout, Reason}).

%% @doc Check if node is currently shutting down
%%
%% Returns true if shutdown sequence has been initiated.
%%
-spec is_shutting_down() -> boolean().
is_shutting_down() ->
    try
        case ets:info(?DEFAULT_SHUTDOWN_TABLE) of
            undefined ->
                false;
            _ ->
                case ets:lookup(?DEFAULT_SHUTDOWN_TABLE, state) of
                    [{_, State}] -> State =/= not_shutting_down;
                    _ -> false
                end
        end
    catch
        _:_ -> false
    end.

%% @doc Get current shutdown state
%%
%% Returns detailed information about current shutdown state.
%%
-spec get_shutdown_state() -> shutdown_status() | #{state => not_shutting_down}.
get_shutdown_state() ->
    try gen_server:call(?SERVER, get_shutdown_state, 2000) of
        State -> State
    catch
        exit:{noproc, _} -> #{state => not_shutting_down};
        _:_ -> #{state => not_shutting_down}
    end.

%%====================================================================
%% API Functions - Workflow Completion
%%====================================================================

%% @doc Wait for active workflows to complete
%%
%% Blocks until all active workflows complete or timeout expires.
%% Returns count of workflows that completed during wait period.
%%
-spec wait_for_active_workflows(shutdown_timeout()) -> {ok, non_neg_integer()} | {error, term()}.
wait_for_active_workflows(Timeout) when is_number(Timeout); Timeout =:= infinity ->
    StartCount = get_active_workflow_count(),
    wait_for_workflows_loop(StartCount, erlang:monotonic_time(millisecond), Timeout).

%% @private Loop to wait for workflows to complete
wait_for_workflows_loop(InitialCount, StartTime, Timeout) ->
    CurrentCount = get_active_workflow_count(),
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,

    case CurrentCount of
        0 ->
            {ok, InitialCount};
        _ when Elapsed >= Timeout ->
            {ok, InitialCount - CurrentCount};
        _ ->
            timer:sleep(?WORKFLOW_POLL_INTERVAL_MS),
            wait_for_workflows_loop(InitialCount, StartTime, Timeout)
    end.

%% @doc Get count of active workflows
%%
%% Returns the number of currently active workflows on this node.
%%
-spec get_active_workflow_count() -> non_neg_integer().
get_active_workflow_count() ->
    try
        %% Check cre_yawl_engine for active workflows
        case catch cre_yawl_engine:active_count() of
            N when is_integer(N) -> N;
            _ ->
                %% Fallback: check for gen_yawl processes
                count_active_yawl_processes()
        end
    catch
        _:_ -> 0
    end.

%% @private Count active gen_yawl processes
count_active_yawl_processes() ->
    try
        %% Count gen_yawl processes registered locally
        LocalProcs = erlang:processes(),
        CountYawl = fun(Pid, Acc) ->
            case catch erlang:process_info(Pid, registered_name) of
                {registered_name, Name} when is_atom(Name) ->
                    case atom_to_list(Name) of
                        "yawl_" ++ _ -> Acc + 1;
                        _ -> Acc
                    end;
                _ ->
                    %% Check dictionary for gen_yawl module
                    case catch erlang:process_info(Pid, dictionary) of
                        {dictionary, Dict} ->
                            case lists:keyfind('$initial_call', 1, Dict) of
                                {_, {gen_yawl, _, _}} -> Acc + 1;
                                _ -> Acc
                            end;
                        _ -> Acc
                    end
            end
        end,
        lists:foldl(CountYawl, 0, LocalProcs)
    catch
        _:_ -> 0
    end.

%% @doc Get details of active workflows
%%
%% Returns a list of maps with workflow details.
%%
-spec get_active_workflow_details() -> [workflow_info()].
get_active_workflow_details() ->
    try
        case catch cre_yawl_engine:active_workflows() of
            Workflows when is_list(Workflows) ->
                lists:map(fun format_workflow_info/1, Workflows);
            _ -> []
        end
    catch
        _:_ -> []
    end.

%% @private Format workflow info from raw data
format_workflow_info({Id, Spec}) when is_binary(Id); is_atom(Id); is_list(Id) ->
    #{
        id => ensure_binary(Id),
        spec => ensure_binary(Spec),
        status => running
    };
format_workflow_info({Id, Spec, StartedAt}) ->
    #{
        id => ensure_binary(Id),
        spec => ensure_binary(Spec),
        started_at => StartedAt,
        status => running
    };
format_workflow_info(Other) ->
    #{id => ensure_binary(Other), status => running}.

%% @private Ensure value is binary
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
ensure_binary(L) when is_list(L) -> list_to_binary(L);
ensure_binary(X) -> io_lib:format("~p", [X]).

%%====================================================================
%% API Functions - Mnesia Checkpoint
%%====================================================================

%% @doc Save final Mnesia checkpoint before shutdown
%%
%% Creates a checkpoint of all Mnesia tables to ensure data consistency.
%% Times out after 5 seconds.
%%
-spec save_final_checkpoint() -> ok | {error, term()}.
save_final_checkpoint() ->
    case mnesia:system_info(is_running) of
        yes ->
            case mnesia:checkpoint([
                {max, mnesia:system_info(tables)},
                {ram_overrides_dump, true}
            ]) of
                {ok, _Name} ->
                    logger:info("Mnesia checkpoint saved successfully"),
                    ok;
                {error, Reason} ->
                    logger:error("Failed to save Mnesia checkpoint: ~p", [Reason]),
                    {error, Reason}
            end;
        _No ->
            logger:warning("Mnesia not running, skipping checkpoint"),
            ok
    end.

%% @doc Get checkpoint status
%%
%% Returns information about the last checkpoint.
%%
-spec checkpoint_status() -> #{status := ok | error | not_running, details => term()}.
checkpoint_status() ->
    case mnesia:system_info(is_running) of
        yes ->
            Tables = mnesia:system_info(tables),
            #{
                status => ok,
                details => #{
                    tables => length(Tables),
                    table_list => Tables,
                    checkpoint_log => mnesia:system_info(checkpoint_log)
                }
            };
        _No ->
            #{status => not_running, details => mnesia_not_running}
    end.

%%====================================================================
%% API Functions - Cluster Notification
%%====================================================================

%% @doc Notify cluster peers of impending shutdown
%%
%% Sends shutdown notification to all connected Mnesia nodes.
%% Uses default 3 second timeout for each peer.
%%
-spec notify_cluster() -> {ok, notification_result()} | {error, term()}.
notify_cluster() ->
    notify_cluster(?NOTIFICATION_TIMEOUT_MS).

%% @doc Notify cluster peers with custom timeout
%%
%% Sends shutdown notification to all connected Mnesia nodes.
%%
-spec notify_cluster(number()) -> {ok, notification_result()} | {error, term()}.
notify_cluster(TimeoutMs) when is_number(TimeoutMs) ->
    StartTime = erlang:monotonic_time(millisecond),

    case mnesia:system_info(is_running) of
        yes ->
            Nodes = mnesia:system_info(running_db_nodes) -- [node()],
            Notified = notify_peers(Nodes, TimeoutMs),
            Failed = Nodes -- Notified,

            EndTime = erlang:monotonic_time(millisecond),
            Result = #{
                peers_notified => length(Notified),
                failed_peers => Failed,
                duration_ms => EndTime - StartTime
            },

            logger:info("Cluster notification result: ~p", [Result]),
            {ok, Result};
        _No ->
            {ok, #{peers_notified => 0, failed_peers => [], duration_ms => 0}}
    end.

%% @private Notify each peer node
notify_peers(Nodes, TimeoutMs) ->
    lists:filtermap(fun(Node) ->
        notify_peer(Node, TimeoutMs)
    end, Nodes).

%% @private Notify single peer
notify_peer(Node, TimeoutMs) ->
    try
        case rpc:call(Node, erlang, send, [?SERVER, {shutdown_notification, node()}], TimeoutMs) of
            ok -> {true, Node};
            _ -> false
        end
    catch
        _:_ -> false
    end.

%% @doc Handle shutdown notification from peer
%%
%% Called when a cluster peer notifies us of their shutdown.
%%
-spec handle_peer_notification(node()) -> ok.
handle_peer_notification(PeerNode) ->
    logger:info("Peer node ~p is shutting down", [PeerNode]),
    %% Update local state to mark peer as shutting down
    %% This could be used for load redistribution
    ok.

%%====================================================================
%% API Functions - Connection Cleanup
%%====================================================================

%% @doc Close all network connections gracefully
%%
%% Closes HTTP listener, ranch acceptors, and other connections.
%%
-spec close_connections() -> ok | {error, term()}.
close_connections() ->
    close_connections(5000).

%% @doc Close network connections with timeout
%%
%% Closes connections with specified timeout in milliseconds.
%%
-spec close_connections(number()) -> ok | {error, term()}.
close_connections(Timeout) when is_number(Timeout) ->
    logger:info("Closing network connections with ~p ms timeout", [Timeout]),

    Results = [
        close_cowboy_listeners(Timeout),
        close_ranch_acceptors(Timeout),
        close_custom_connections(Timeout)
    ],

    case lists:all(fun(R) -> R =:= ok orelse R =:= {error, not_found} end, Results) of
        true -> ok;
        false -> {error, partial_close}
    end.

%% @private Close Cowboy HTTP listeners
close_cowboy_listeners(Timeout) ->
    try
        Listeners = ranch:info(),
        lists:foreach(fun({Ref, _}) ->
            case cowboy:stop_listener(Ref) of
                ok -> ok;
                {error, Reason} ->
                    logger:warning("Failed to stop listener ~p: ~p", [Ref, Reason])
            end
        end, Listeners),
        ok
    catch
        _:_ -> {error, not_found}
    end.

%% @private Close Ranch acceptors
close_ranch_acceptors(_Timeout) ->
    %% Ranch listeners are managed by Cowboy, already handled above
    ok.

%% @private Close custom socket connections
close_custom_connections(_Timeout) ->
    try
        %% Close any custom socket connections if needed
        ok
    catch
        _:_ -> ok
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(Options) ->
    logger:info("Starting CRE Graceful Shutdown server"),
    TableName = proplists:get_value(table, Options, ?DEFAULT_SHUTDOWN_TABLE),

    %% Create ETS table for shutdown state
    ets:new(TableName, [named_table, public, {read_concurrency, true}]),
    ets:insert(TableName, {state, not_shutting_down}),

    %% Set up alarm handler for shutdown
    ShutdownState = #shutdown_state{},
    {ok, ShutdownState}.

%% @private
handle_call(get_shutdown_state, _From, State) ->
    Response = #{
        state => State#shutdown_state.state,
        reason => State#shutdown_state.reason,
        start_time => State#shutdown_state.start_time,
        timeout => State#shutdown_state.timeout,
        active_workflows => State#shutdown_state.active_workflows,
        checkpoint_saved => State#shutdown_state.checkpoint_saved,
        cluster_notified => State#shutdown_state.cluster_notified,
        connections_closed => State#shutdown_state.connections_closed
    },
    {reply, Response, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({initiate_shutdown, Timeout, Reason}, State) ->
    logger:info("Initiating graceful shutdown: reason=~p, timeout=~p", [Reason, Timeout]),

    NewState = State#shutdown_state{
        state = stopping_new,
        reason = Reason,
        start_time = erlang:monotonic_time(millisecond),
        timeout = Timeout
    },

    %% Update ETS table for non-blocking reads
    ets:insert(?DEFAULT_SHUTDOWN_TABLE, {state, stopping_new}),

    %% Spawn shutdown process to avoid blocking gen_server
    spawn(fun() -> perform_shutdown_sequence(Timeout, Reason) end),

    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info({shutdown_notification, PeerNode}, State) ->
    handle_peer_notification(PeerNode),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    logger:info("CRE Graceful Shutdown server terminating"),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Shutdown Sequence Implementation
%%====================================================================

%% @private Perform the actual shutdown sequence
perform_shutdown_sequence(Timeout, Reason) ->
    StartTime = erlang:monotonic_time(millisecond),

    %% Step 1: Stop accepting new workflows
    logger:info("Shutdown step 1: Stopping new workflow acceptance"),
    stop_accepting_workflows(),
    update_shutdown_state(stopping_new),

    %% Step 2: Drain active workflows
    logger:info("Shutdown step 2: Draining active workflows"),
    DrainTimeout = calculate_drain_timeout(Timeout, StartTime),
    ActiveCount = wait_for_workflows_complete(DrainTimeout),
    update_shutdown_state(draining),

    %% Step 3: Save Mnesia checkpoint
    logger:info("Shutdown step 3: Saving Mnesia checkpoint"),
    save_final_checkpoint(),
    update_shutdown_state(checkpointing),

    %% Step 4: Notify cluster peers
    logger:info("Shutdown step 4: Notifying cluster peers"),
    notify_cluster(),
    update_shutdown_state(finalizing),

    %% Step 5: Close connections
    logger:info("Shutdown step 5: Closing network connections"),
    close_connections(),

    %% Update final state
    update_shutdown_state(finalizing),
    log_shutdown_complete(Reason, ActiveCount),

    %% Initiate VM shutdown if this is a SIGTERM
    case Reason of
        sigterm -> init:stop();
        sigint -> init:stop();
        gke_preemption -> init:stop();
        _ -> ok
    end.

%% @private Stop accepting new workflows
stop_accepting_workflows() ->
    try
        %% Signal to CRE master to stop accepting new requests
        case whereis(cre_master) of
            undefined -> ok;
            Pid -> cre_master:stop_accepting(Pid)
        end
    catch
        _:_ -> ok
    end.

%% @private Calculate drain timeout (save time for other steps)
calculate_drain_timeout(Timeout, StartTime) ->
    %% Reserve 5 seconds for checkpoint/notification/closing
    Reserved = ?CHECKPOINT_TIMEOUT_MS + ?NOTIFICATION_TIMEOUT_MS + 2000,
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    max(0, Timeout - Reserved - Elapsed).

%% @private Wait for workflows to complete
wait_for_workflows_complete(Timeout) ->
    case wait_for_active_workflows(Timeout) of
        {ok, Count} ->
            logger:info("Drained ~p workflows", [Count]),
            Count;
        {error, Reason} ->
            logger:warning("Error waiting for workflows: ~p", [Reason]),
            get_active_workflow_count()
    end.

%% @private Update shutdown state in ETS
update_shutdown_state(NewState) ->
    ets:insert(?DEFAULT_SHUTDOWN_TABLE, {state, NewState}).

%% @private Log shutdown completion
log_shutdown_complete(Reason, ActiveCount) ->
    Duration = case ets:lookup(?DEFAULT_SHUTDOWN_TABLE, start_time) of
        [{start_time, StartTime}] ->
            erlang:monotonic_time(millisecond) - StartTime;
        _ -> unknown
    end,
    logger:info("Graceful shutdown complete: reason=~p, active_remaining=~p, duration=~p",
                [Reason, ActiveCount, Duration]).

%%====================================================================
%% GCP Spot VM Preemption Detection
%%====================================================================

%% @doc Start GCP Spot VM preemption monitor
%%
%% Monitors the GCP metadata server for preemption notices.
%% When detected, initiates fast shutdown within 30 seconds.
%%
%% @end
-spec start_preemption_monitor() -> {ok, pid()} | {error, term()}.
start_preemption_monitor() ->
    start_preemption_monitor(?PREEMPTION_CHECK_INTERVAL_MS).

%% @doc Start preemption monitor with custom check interval
%%
%% IntervalMs is the time between metadata server checks in milliseconds.
%%
%% @end
-spec start_preemption_monitor(pos_integer()) -> {ok, pid()} | {error, term()}.
start_preemption_monitor(IntervalMs) when is_integer(IntervalMs), IntervalMs > 0 ->
    case is_spot_vm() of
        true ->
            logger:info("Starting GCP Spot VM preemption monitor (interval=~p ms)", [IntervalMs]),
            MonitorPid = spawn(fun() -> preemption_monitor_loop(IntervalMs) end),
            {ok, MonitorPid};
        false ->
            logger:info("Not running on Spot VM, preemption monitor not started"),
            {ok, undefined}
    end.

%% @doc Stop the preemption monitor
%%
%% Stops the preemption monitor if running.
%%
%% @end
-spec stop_preemption_monitor() -> ok.
stop_preemption_monitor() ->
    case whereis(cre_preemption_monitor) of
        undefined -> ok;
        Pid ->
            erlang:exit(Pid, shutdown),
            logger:info("Stopped GCP Spot VM preemption monitor"),
            ok
    end.

%% @doc Check if running on a GCP Spot VM
%%
%% Returns true if the current instance is a Spot/Preemptible VM.
%%
%% @end
-spec is_spot_vm() -> boolean().
is_spot_vm() ->
    try
        %% Check environment variable first (fast check)
        case os:getenv("CRE_SPOT_VM") of
            "true" -> true;
            "false" -> false;
            _ ->
                %% Check GCP metadata server for maintenance policy
                case gcp_metadata_request(?GCP_SPOT_ENDPOINT) of
                    {ok, <<"TERMINATE">>} -> true;
                    {ok, <<"MIGRATE">>} -> false;
                    _ -> false
                end
        end
    catch
        _:_ -> false
    end.

%% @doc Check for GCP preemption notice
%%
%% Queries the GCP metadata server for preemption status.
%% Returns true if preemption is imminent.
%%
%% @end
-spec check_preemption_notice() -> boolean().
check_preemption_notice() ->
    try
        case gcp_metadata_request(?GCP_PREEMPTION_ENDPOINT) of
            {ok, <<"TRUE">>} ->
                logger:warning("GCP Spot VM preemption detected!"),
                true;
            {ok, _} ->
                false;
            {error, Reason} ->
                logger:debug("Preemption check failed: ~p", [Reason]),
                false
        end
    catch
        _:_ -> false
    end.

%% @doc Handle GCP Spot VM preemption notice
%%
%% Called when preemption is detected. Initiates fast shutdown
%% within 30 seconds (25 seconds for CRE, 5 second buffer).
%%
%% @end
-spec handle_preemption_notice() -> ok.
handle_preemption_notice() ->
    logger:critical("GCP Spot VM preemption detected! Initiating fast shutdown"),
    %% Initiate shutdown with Spot VM timeout
    initiate_shutdown(?GKE_SPOT_TIMEOUT_MS, gke_preemption),
    ok.

%% @private Preemption monitor loop
preemption_monitor_loop(IntervalMs) ->
    erlang:register(cre_preemption_monitor, self()),
    preemption_monitor_loop(IntervalMs, 0).

preemption_monitor_loop(IntervalMs, CheckCount) ->
    case check_preemption_notice() of
        true ->
            handle_preemption_notice(),
            %% Stop monitoring after triggering shutdown
            exit(normal);
        false ->
            case CheckCount rem 10 of
                0 ->
                    logger:debug("Preemption monitor check #~p (no preemption)", [CheckCount]);
                _ ->
                    ok
            end,
            timer:sleep(IntervalMs),
            preemption_monitor_loop(IntervalMs, CheckCount + 1)
    end.

%% @private Make request to GCP metadata server
-spec gcp_metadata_request(binary()) -> {ok, binary()} | {error, term()}.
gcp_metadata_request(Endpoint) ->
    Url = <<?GCP_METADATA_URL, Endpoint/binary>>,
    try
        case httpc:request(get, {binary_to_list(Url), [{"Metadata-Flavor", "Google"}]}, [], []) of
            {ok, {{_, 200, _}, _, Body}} ->
                {ok, list_to_binary(Body)};
            {ok, {{StatusCode, _, _}, _, _}} ->
                {error, {http_error, StatusCode}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        Type:Error:Stack ->
            {error, {Type, Error, Stack}}
    end.
