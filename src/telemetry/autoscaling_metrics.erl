%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @doc Custom Autoscaling Metrics for GKE HPA
%%
%% This module provides custom business-logic metrics for Kubernetes
%% Horizontal Pod Autoscaling (HPA) on Google Kubernetes Engine.
%%
%% <h3>Metrics Exported</h3>
%%
%% <ul>
%%   <li><b>active_workflows:</b> Gauge of currently executing workflow instances</li>
%%   <li><b>workflow_queue_depth:</b> Gauge of pending/queued workflow requests</li>
%%   <li><b>erlang_process_count:</b> Gauge of total Erlang processes</li>
%%   <li><b>mnesia_table_size:</b> Gauge of Mnesia database table sizes</li>
%% </ul>
%%
%% <h3>HPA Integration</h3>
%%
%% The metrics are exposed via the Prometheus endpoint for scraping
%% by the Prometheus Adapter or Cloud Monitoring adapter.
%%
%% Example HPA configuration:
%% ```
%%   - type: Pods
%%     pods:
%%       metric:
%%         name: active_workflows
%%       target:
%%         type: AverageValue
%%         averageValue: "50"
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(autoscaling_metrics).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Lifecycle
-export([start_link/0, start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% Metric Collection API
-export([get_active_workflows/0]).
-export([get_workflow_queue_depth/0]).
-export([get_erlang_process_count/0]).
-export([get_mnesia_table_size/0]).
-export([get_all_metrics/0]).

%% Metric Registration (for HPA adapter)
-export([collect_metrics/0]).
-export([export_prometheus/0]).

%% State Query
-export([get_state/0]).
-export([get_metric_history/1]).

%%====================================================================
%% Includes & Macros
%%====================================================================

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).
-define(DEFAULT_COLLECT_INTERVAL, 15000). %% 15 seconds
-define(METRIC_HISTORY_SIZE, 144). %% 6 hours at 15s intervals

%%====================================================================
%% Records
%%====================================================================

-record(metric_value, {
    value :: number(),
    timestamp :: integer(),
    labels :: map()
}).

-record(state, {
    collect_interval :: pos_integer(),
    timer_ref :: reference() | undefined,
    last_collect_time :: integer(),
    %% Current metric values
    active_workflows = 0 :: non_neg_integer(),
    workflow_queue_depth = 0 :: non_neg_integer(),
    erlang_process_count = 0 :: non_neg_integer(),
    mnesia_table_size = 0 :: non_neg_integer(),
    %% Metric history for trend analysis
    history :: #{atom() := queue:queue(#metric_value{})}
}).

-type state() :: #state{}.
-type metric_name() :: active_workflows | workflow_queue_depth
                    | erlang_process_count | mnesia_table_size.
-type metric_value() :: #metric_value{}.
-type metric_map() :: #{metric_name() := number()}.

-export_type([metric_name/0, metric_value/0, metric_map/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start the autoscaling metrics server with default options.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%%--------------------------------------------------------------------
%% @doc Start the autoscaling metrics server with options.
%%
%% Options:
%% - `{collect_interval, Milliseconds}' - Metric collection interval (default: 15000ms)
%% @end
%%--------------------------------------------------------------------
-spec start_link([proplists:property()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Stop the metrics server.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%%--------------------------------------------------------------------
%% @doc Get the current active workflow count.
%%
%% This is the primary metric for autoscaling based on actual workload.
%% @end
%%--------------------------------------------------------------------
-spec get_active_workflows() -> non_neg_integer().
get_active_workflows() ->
    gen_server:call(?SERVER, get_active_workflows).

%%--------------------------------------------------------------------
%% @doc Get the current workflow queue depth.
%%
%% Represents pending workflow requests waiting to be processed.
%% @end
%%--------------------------------------------------------------------
-spec get_workflow_queue_depth() -> non_neg_integer().
get_workflow_queue_depth() ->
    gen_server:call(?SERVER, get_workflow_queue_depth).

%%--------------------------------------------------------------------
%% @doc Get the current Erlang process count.
%%
%% Useful for detecting process leaks or resource exhaustion.
%% @end
%%--------------------------------------------------------------------
-spec get_erlang_process_count() -> non_neg_integer().
get_erlang_process_count() ->
    gen_server:call(?SERVER, get_erlang_process_count).

%%--------------------------------------------------------------------
%% @doc Get the total Mnesia table size (in records).
%%
%% Represents database load and memory pressure.
%% @end
%%--------------------------------------------------------------------
-spec get_mnesia_table_size() -> non_neg_integer().
get_mnesia_table_size() ->
    gen_server:call(?SERVER, get_mnesia_table_size).

%%--------------------------------------------------------------------
%% @doc Get all current metrics as a map.
%%
%% Useful for bulk metric retrieval and monitoring dashboards.
%% @end
%%--------------------------------------------------------------------
-spec get_all_metrics() -> metric_map().
get_all_metrics() ->
    gen_server:call(?SERVER, get_all_metrics).

%%--------------------------------------------------------------------
%% @doc Collect and update all metrics.
%%
%% Called automatically on a timer, but can be triggered manually
%% for on-demand metric collection.
%% @end
%%--------------------------------------------------------------------
-spec collect_metrics() -> ok.
collect_metrics() ->
    gen_server:cast(?SERVER, collect_metrics).

%%--------------------------------------------------------------------
%% @doc Export metrics in Prometheus text format for HPA adapter.
%%
%% Returns an iolist suitable for HTTP response body.
%% @end
%%--------------------------------------------------------------------
-spec export_prometheus() -> iolist().
export_prometheus() ->
    gen_server:call(?SERVER, export_prometheus).

%%--------------------------------------------------------------------
%% @doc Get the internal server state (for debugging/monitoring).
%% @end
%%--------------------------------------------------------------------
-spec get_state() -> state().
get_state() ->
    gen_server:call(?SERVER, get_state).

%%--------------------------------------------------------------------
%% @doc Get historical values for a specific metric.
%%
%% Returns a list of `{Timestamp, Value}` tuples.
%% @end
%%--------------------------------------------------------------------
-spec get_metric_history(metric_name()) -> [{integer(), number()}].
get_metric_history(MetricName) ->
    gen_server:call(?SERVER, {get_metric_history, MetricName}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    Interval = proplists:get_value(collect_interval, Options, ?DEFAULT_COLLECT_INTERVAL),

    %% Initialize history queues
    ActiveQ = queue:new(),
    QueueQ = queue:new(),
    ProcessQ = queue:new(),
    MnesiaQ = queue:new(),

    History = #{
        active_workflows => ActiveQ,
        workflow_queue_depth => QueueQ,
        erlang_process_count => ProcessQ,
        mnesia_table_size => MnesiaQ
    },

    %% Initial metric collection
    {ActiveWorkflows, QueueDepth, ProcessCount, MnesiaSize} = collect_all_metrics(),

    State = #state{
        collect_interval = Interval,
        timer_ref = undefined,
        last_collect_time = erlang:system_time(millisecond),
        active_workflows = ActiveWorkflows,
        workflow_queue_depth = QueueDepth,
        erlang_process_count = ProcessCount,
        mnesia_table_size = MnesiaSize,
        history = History
    },

    %% Start periodic collection
    TimerRef = erlang:send_after(Interval, self(), collect_metrics),
    {ok, State#state{timer_ref = TimerRef}}.

handle_call(get_active_workflows, _From, State = #state{active_workflows = Value}) ->
    {reply, Value, State};

handle_call(get_workflow_queue_depth, _From, State = #state{workflow_queue_depth = Value}) ->
    {reply, Value, State};

handle_call(get_erlang_process_count, _From, State = #state{erlang_process_count = Value}) ->
    {reply, Value, State};

handle_call(get_mnesia_table_size, _From, State = #state{mnesia_table_size = Value}) ->
    {reply, Value, State};

handle_call(get_all_metrics, _From, State) ->
    Metrics = #{
        active_workflows => State#state.active_workflows,
        workflow_queue_depth => State#state.workflow_queue_depth,
        erlang_process_count => State#state.erlang_process_count,
        mnesia_table_size => State#state.mnesia_table_size
    },
    {reply, Metrics, State};

handle_call(export_prometheus, _From, State) ->
    Export = format_prometheus(State),
    {reply, Export, State};

handle_call(get_state, _From, State) ->
    {reply, State, State};

handle_call({get_metric_history, MetricName}, _From, State = #state{history = History}) ->
    Q = maps:get(MetricName, History, queue:new()),
    HistoryList = queue:to_list(Q),
    Result = [{V#metric_value.timestamp, V#metric_value.value} || V <- HistoryList],
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(collect_metrics, State) ->
    {noreply, do_collect_metrics(State)};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_metrics, State = #state{timer_ref = TimerRef, collect_interval = Interval}) ->
    %% Cancel old timer and schedule new one
    case TimerRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TimerRef)
    end,
    NewTimerRef = erlang:send_after(Interval, self(), collect_metrics),
    NewState = do_collect_metrics(State#state{timer_ref = NewTimerRef}),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, #state{timer_ref = TimerRef}) ->
    case TimerRef of
        undefined -> ok;
        _ -> erlang:cancel_timer(TimerRef)
    end,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private Collect all metrics and update state.
%%--------------------------------------------------------------------
-spec do_collect_metrics(state()) -> state().
do_collect_metrics(State) ->
    {ActiveWorkflows, QueueDepth, ProcessCount, MnesiaSize} = collect_all_metrics(),
    Timestamp = erlang:system_time(millisecond),

    %% Update history
    NewHistory = update_history(
        State#state.history,
        ActiveWorkflows, QueueDepth, ProcessCount, MnesiaSize,
        Timestamp
    ),

    State#state{
        last_collect_time = Timestamp,
        active_workflows = ActiveWorkflows,
        workflow_queue_depth = QueueDepth,
        erlang_process_count = ProcessCount,
        mnesia_table_size = MnesiaSize,
        history = NewHistory
    }.

%%--------------------------------------------------------------------
%% @private Collect metrics from various sources.
%%--------------------------------------------------------------------
-spec collect_all_metrics() ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()}.
collect_all_metrics() ->
    ActiveWorkflows = collect_active_workflows(),
    QueueDepth = collect_workflow_queue_depth(),
    ProcessCount = collect_process_count(),
    MnesiaSize = collect_mnesia_size(),
    {ActiveWorkflows, QueueDepth, ProcessCount, MnesiaSize}.

%%--------------------------------------------------------------------
%% @private Count active gen_yawl workflow instances.
%%--------------------------------------------------------------------
-spec collect_active_workflows() -> non_neg_integer().
collect_active_workflows() ->
    %% Count all gen_yawl processes currently running
    try
        %% Method 1: Count gen_yawl processes
        GenYawlCount = count_processes(gen_yawl),

        %% Method 2: Check registered workflow processes
        RegisteredCount = count_registered_workflows(),

        %% Use the maximum of both methods for accuracy
        max(GenYawlCount, RegisteredCount)
    catch
        _:_ ->
            %% Fallback: count processes with 'yawl' in their name
            count_named_processes(yawl)
    end.

%%--------------------------------------------------------------------
%% @private Count processes of a specific module.
%%--------------------------------------------------------------------
-spec count_processes(module()) -> non_neg_integer().
count_processes(Module) ->
    try
        Processes = erlang:processes(),
        Count = lists:foldl(fun(Pid, Acc) ->
            case erlang:process_info(Pid, current_function) of
                {current_function, {Module, _, _}} -> Acc + 1;
                _ -> Acc
            end
        end, 0, Processes),
        Count
    catch
        _:_ -> 0
    end.

%%--------------------------------------------------------------------
%% @private Count registered workflow processes.
%%--------------------------------------------------------------------
-spec count_registered_workflows() -> non_neg_integer().
count_registered_workflows() ->
    try
        %% Look for registered names matching workflow patterns
        Registered = erlang:registered(),
        WorkflowPatterns = [workflow, yawl, case_, task],
        lists:foldl(fun(Name, Acc) ->
            case is_workflow_name(Name, WorkflowPatterns) of
                true -> Acc + 1;
                false -> Acc
            end
        end, 0, Registered)
    catch
        _:_ -> 0
    end.

%%--------------------------------------------------------------------
%% @private Check if a registered name matches workflow patterns.
%%--------------------------------------------------------------------
-spec is_workflow_name(atom(), [atom()]) -> boolean().
is_workflow_name(_Name, []) ->
    false;
is_workflow_name(Name, [Pattern | Rest]) when is_atom(Name) ->
    NameStr = atom_to_list(Name),
    PatternStr = atom_to_list(Pattern),
    case string:find(NameStr, PatternStr) of
        nomatch -> is_workflow_name(Name, Rest);
        _ -> true
    end.

%%--------------------------------------------------------------------
%% @private Count processes with a specific name pattern.
%%--------------------------------------------------------------------
-spec count_named_processes(atom()) -> non_neg_integer().
count_named_processes(Pattern) ->
    try
        PatternStr = atom_to_list(Pattern),
        Processes = erlang:processes(),
        lists:foldl(fun(Pid, Acc) ->
            case erlang:process_info(Pid, registered_name) of
                {registered_name, Name} when is_atom(Name) ->
                    NameStr = atom_to_list(Name),
                    case string:find(NameStr, PatternStr) of
                        nomatch -> Acc;
                        _ -> Acc + 1
                    end;
                _ ->
                    case erlang:process_info(Pid, dictionary) of
                        {dictionary, Dict} ->
                            case lists:keyfind(workflow_id, 1, Dict) of
                                false -> Acc;
                                _ -> Acc + 1
                            end;
                        _ -> Acc
                    end
            end
        end, 0, Processes)
    catch
        _:_ -> 0
    end.

%%--------------------------------------------------------------------
%% @private Estimate workflow queue depth.
%%
%% This measures pending workflow requests across various queues.
%%--------------------------------------------------------------------
-spec collect_workflow_queue_depth() -> non_neg_integer().
collect_workflow_queue_depth() ->
    try
        %% Count messages in gen_yawl mailboxes (pending work)
        GenYawlPids = find_gen_yawl_processes(),
        QueueDepth = lists:foldl(fun(Pid, Acc) ->
            case erlang:process_info(Pid, message_queue_len) of
                {message_queue_len, Len} -> Acc + Len;
                _ -> Acc
            end
        end, 0, GenYawlPids),

        %% Add estimates from other queue sources
        SupervisorQueue = estimate_supervisor_queue(),
        TimerQueue = estimate_timer_queue(),

        QueueDepth + SupervisorQueue + TimerQueue
    catch
        _:_ -> 0
    end.

%%--------------------------------------------------------------------
%% @private Find all gen_yawl process PIDs.
%%--------------------------------------------------------------------
-spec find_gen_yawl_processes() -> [pid()].
find_gen_yawl_processes() ->
    try
        Processes = erlang:processes(),
        lists:filter(fun(Pid) ->
            case erlang:process_info(Pid, current_function) of
                {current_function, {gen_yawl, _, _}} -> true;
                _ -> false
            end
        end, Processes)
    catch
        _:_ -> []
    end.

%%--------------------------------------------------------------------
%% @private Estimate supervisor queue depth.
%%--------------------------------------------------------------------
-spec estimate_supervisor_queue() -> non_neg_integer().
estimate_supervisor_queue() ->
    try
        %% Count pending requests in workflow supervisors
        Supervisors = supervisor:which_children(yawl_supervisor),
        lists:foldl(fun({_Id, Pid, _Type, _Modules}, Acc) when is_pid(Pid) ->
            case erlang:process_info(Pid, message_queue_len) of
                {message_queue_len, Len} -> Acc + Len;
                _ -> Acc
            end;
           (_, Acc) ->
            Acc
        end, 0, Supervisors)
    catch
        _:_ -> 0
    end.

%%--------------------------------------------------------------------
%% @private Estimate timer queue depth for delayed workflows.
%%--------------------------------------------------------------------
-spec estimate_timer_queue() -> non_neg_integer().
estimate_timer_queue() ->
    try
        %% Check for timer-based workflow delays
        case erlang:whereis(yawl_timer_registry) of
            undefined -> 0;
            TimerPid ->
                case erlang:process_info(TimerPid, message_queue_len) of
                    {message_queue_len, Len} -> Len;
                    _ -> 0
                end
        end
    catch
        _:_ -> 0
    end.

%%--------------------------------------------------------------------
%% @private Collect total Erlang process count.
%%--------------------------------------------------------------------
-spec collect_process_count() -> non_neg_integer().
collect_process_count() ->
    erlang:system_info(process_count).

%%--------------------------------------------------------------------
%% @private Collect total Mnesia table size.
%%--------------------------------------------------------------------
-spec collect_mnesia_size() -> non_neg_integer().
collect_mnesia_size() ->
    try
        case application:which_applications() of
            [] -> 0;
            Apps ->
                case lists:keyfind(mnesia, 1, Apps) of
                    false -> 0;
                    {mnesia, _, _} ->
                        Tables = mnesia:system_info(tables),
                        lists:foldl(fun(Table, Acc) ->
                            case mnesia:table_info(Table, size) of
                                Size when is_integer(Size) -> Acc + Size;
                                _ -> Acc
                            end
                        end, 0, Tables);
                    _ -> 0
                end
        end
    catch
        _:_ -> 0
    end.

%%--------------------------------------------------------------------
%% @private Update metric history queues.
%%--------------------------------------------------------------------
-spec update_history(
    map(),
    non_neg_integer(), non_neg_integer(),
    non_neg_integer(), non_neg_integer(),
    integer()
) -> map().
update_history(History, ActiveWorkflows, QueueDepth, ProcessCount, MnesiaSize, Timestamp) ->
    ActiveQ = add_to_queue(
        maps:get(active_workflows, History),
        #metric_value{value = ActiveWorkflows, timestamp = Timestamp, labels = #{}}
    ),
    QueueQ = add_to_queue(
        maps:get(workflow_queue_depth, History),
        #metric_value{value = QueueDepth, timestamp = Timestamp, labels = #{}}
    ),
    ProcessQ = add_to_queue(
        maps:get(erlang_process_count, History),
        #metric_value{value = ProcessCount, timestamp = Timestamp, labels = #{}}
    ),
    MnesiaQ = add_to_queue(
        maps:get(mnesia_table_size, History),
        #metric_value{value = MnesiaSize, timestamp = Timestamp, labels = #{}}
    ),
    History#{
        active_workflows => ActiveQ,
        workflow_queue_depth => QueueQ,
        erlang_process_count => ProcessQ,
        mnesia_table_size => MnesiaQ
    }.

%%--------------------------------------------------------------------
%% @private Add value to queue, maintaining max size.
%%--------------------------------------------------------------------
-spec add_to_queue(queue:queue(metric_value()), metric_value()) -> queue:queue(metric_value()).
add_to_queue(Q, Value) ->
    Q1 = queue:in(Value, Q),
    case queue:len(Q1) > ?METRIC_HISTORY_SIZE of
        true ->
            {{value, _}, Q2} = queue:out(Q1),
            Q2;
        false ->
            Q1
    end.

%%--------------------------------------------------------------------
%% @private Format metrics in Prometheus text format.
%%--------------------------------------------------------------------
-spec format_prometheus(state()) -> iolist().
format_prometheus(State = #state{
    active_workflows = Active,
    workflow_queue_depth = QueueDepth,
    erlang_process_count = ProcessCount,
    mnesia_table_size = MnesiaSize,
    last_collect_time = Timestamp
}) ->
    %% Generate HELP and TYPE headers
    [
        "# HELP cre_autoscaling_active_workflows Number of currently active workflow instances\n",
        "# TYPE cre_autoscaling_active_workflows gauge\n",
        io_lib:format("cre_autoscaling_active_workflows ~p\n", [Active]),

        "# HELP cre_autoscaling_workflow_queue_depth Number of pending workflow requests in queue\n",
        "# TYPE cre_autoscaling_workflow_queue_depth gauge\n",
        io_lib:format("cre_autoscaling_workflow_queue_depth ~p\n", [QueueDepth]),

        "# HELP cre_autoscaling_erlang_process_count Total number of Erlang processes\n",
        "# TYPE cre_autoscaling_erlang_process_count gauge\n",
        io_lib:format("cre_autoscaling_erlang_process_count ~p\n", [ProcessCount]),

        "# HELP cre_autoscaling_mnesia_table_size Total number of records in all Mnesia tables\n",
        "# TYPE cre_autoscaling_mnesia_table_size gauge\n",
        io_lib:format("cre_autoscaling_mnesia_table_size ~p\n", [MnesiaSize]),

        "# HELP cre_autoscaling_scrape_timestamp Unix timestamp of last metric collection\n",
        "# TYPE cre_autoscaling_scrape_timestamp gauge\n",
        io_lib:format("cre_autoscaling_scrape_timestamp ~p\n", [Timestamp]),

        "\n"
    ].
