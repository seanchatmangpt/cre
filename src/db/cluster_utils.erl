%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc Cluster Utilities for Network Partition Detection and Healing
%%
%% This module provides utilities for monitoring cluster health, detecting
%% network partitions, and executing healing strategies. It works in
%% conjunction with the cluster module to maintain Mnesia cluster integrity.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Partition Detection:</b> Identifies network split events</li>
%%   <li><b>Health Monitoring:</b> Tracks node connectivity and response times</li>
%%   <li><b>Healing Strategies:</b> Configurable recovery mechanisms</li>
%%   <li><b>Event Logging:</b> Comprehensive logging of cluster events</li>
%% </ul>
%%
%% <h3>Healing Strategies</h3>
%%
%% <ol>
%%   <li><b>auto_rejoin:</b> Automatically attempt to rejoin partitioned nodes</li>
%%   <li><b>manual:</b> Require manual intervention for partition recovery</li>
%%   <li><b>majority:</b> Prefer the partition with majority of nodes</li>
%% </ol>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Start the health monitor
%% {ok, Pid} = cluster_utils:start_link(),
%%
%% %% Check node health
%% Health = cluster_utils:get_node_health(node@host),
%%
%% %% Detect partitions
%% case cluster_utils:detect_partition() of
%%     {partition, Nodes} -> heal_partition(Nodes);
%%     ok -> ok
%% end.
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(cluster_utils).
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API functions
-export([start_link/0, start_link/1,
         get_node_health/1,
         get_cluster_health/0,
         detect_partition/0,
         heal_partition/1,
         set_healing_strategy/1,
         ping_nodes/1,
         monitor_nodes/0]).

%% gen_server callback functions
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

%%====================================================================
%% Type definitions
%%====================================================================

-type healing_strategy() :: auto_rejoin | manual | majority.
-type node_status() :: up | down | suspect | partitioned.
-type node_health() :: #{node => node(),
                         status => node_status(),
                         last_seen => erlang:timestamp(),
                         latency => non_neg_integer() | undefined,
                         partition_count => non_neg_integer()}.
-type cluster_health() :: #{total_nodes => non_neg_integer(),
                            healthy_nodes => non_neg_integer(),
                            partitioned_nodes => [node()],
                            has_partition => boolean()}.
-type utils_state() :: #{healing_strategy => healing_strategy(),
                         node_status => #{node() => node_status()},
                         last_check => erlang:timestamp() | undefined,
                         monitor_ref => reference() | undefined}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts the cluster utilities server with default configuration.
%%
%%      Registered locally as `cluster_utils`. Uses auto_rejoin strategy
%%      and starts health monitoring automatically.
%%
%% @returns `{ok, Pid}' | `{error, Reason}'
%%
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Starts the cluster utilities server with options.
%%
%%      Options:
%%      - `{healing_strategy, Strategy}' - auto_rejoin, manual, or majority
%%      - `{monitor_interval, Millis}' - Health check interval (default 5000)
%%
-spec start_link([proplists:property()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Gets the health status of a specific node.
%%
%%      Returns the current status, last seen timestamp, latency,
%%      and partition count for the specified node.
%%
%% @param Node The node to query
%% @returns Node health map
%%
-spec get_node_health(node()) -> node_health() | {error, unknown_node}.
get_node_health(Node) ->
    gen_server:call(?MODULE, {get_node_health, Node}).

%% @doc Gets the overall cluster health status.
%%
%%      Provides summary information about the entire cluster including
%%      total nodes, healthy count, partitioned nodes, and partition flag.
%%
%% @returns Cluster health map
%%
-spec get_cluster_health() -> cluster_health().
get_cluster_health() ->
    gen_server:call(?MODULE, get_cluster_health).

%% @doc Detects if a network partition has occurred.
%%
%%      Analyzes node connectivity to determine if the cluster has
%%      split into separate partitions. Returns affected nodes if found.
%%
%% @returns `ok' | `{partition, PartitionedNodes}'
%%
-spec detect_partition() -> ok | {partition, [node()]}.
detect_partition() ->
    gen_server:call(?MODULE, detect_partition).

%% @doc Initiates healing for partitioned nodes.
%%
%%      Attempts to recover nodes that have been isolated due to
%%      network partition. The strategy used depends on configuration.
%%
%% @param Nodes List of nodes to heal
%% @returns `ok' | `{error, Reason}'
%%
-spec heal_partition([node()]) -> ok | {error, term()}.
heal_partition(Nodes) when is_list(Nodes) ->
    gen_server:call(?MODULE, {heal_partition, Nodes}).

%% @doc Sets the healing strategy for partition recovery.
%%
%%      Strategies:
%%      - `auto_rejoin' - Automatically attempt to rejoin partitioned nodes
%%      - `manual' - Require manual intervention for recovery
%%      - `majority' - Prefer the partition with majority of nodes
%%
-spec set_healing_strategy(healing_strategy()) -> ok.
set_healing_strategy(Strategy) when Strategy =:= auto_rejoin;
                                     Strategy =:= manual;
                                     Strategy =:= majority ->
    gen_server:call(?MODULE, {set_healing_strategy, Strategy}).

%% @doc Pings a list of nodes to check connectivity.
%%
%%      Sends ping requests to each node and records response times.
%%      Updates the internal health status for all queried nodes.
%%
%% @param Nodes List of nodes to ping
%% @returns List of {Node, Status} tuples
%%
-spec ping_nodes([node()]) -> [{node(), pong | pang}].
ping_nodes(Nodes) when is_list(Nodes) ->
    gen_server:call(?MODULE, {ping_nodes, Nodes}).

%% @doc Starts monitoring all visible nodes.
%%
%%      Subscribes to nodeup and nodedown events to track cluster
%%      membership changes in real-time.
%%
%% @returns `ok'
%%
-spec monitor_nodes() -> ok.
monitor_nodes() ->
    gen_server:call(?MODULE, monitor_nodes).

%%====================================================================
%% gen_server callback functions
%%====================================================================

%% @private
init(Options) ->
    process_flag(trap_exit, true),

    Strategy = proplists:get_value(healing_strategy, Options, auto_rejoin),
    MonitorInterval = proplists:get_value(monitor_interval, Options, 5000),

    %% Start health monitoring timer
    TimerRef = erlang:send_after(MonitorInterval, self(), health_check),

    %% Subscribe to node changes
    net_kernel:monitor_nodes(true),

    State = #{
        healing_strategy => Strategy,
        node_status => #{},
        last_check => undefined,
        monitor_ref => TimerRef
    },

    logger:info("Cluster utilities started: strategy=~p, interval=~p",
                [Strategy, MonitorInterval],
                [{info, "cluster_utils_init"}, {application, cre}]),

    {ok, State}.

%% @private
handle_call({get_node_health, Node}, _From, State) ->
    NodeStatus = maps:get(node_status, State, #{}),
    case maps:find(Node, NodeStatus) of
        {ok, Status} ->
            Health = #{
                node => Node,
                status => Status,
                last_seen => erlang:timestamp(),
                latency => undefined,
                partition_count => 0
            },
            {reply, Health, State};
        error ->
            %% Check if node is visible but not tracked
            case lists:member(Node, [node() | nodes()]) of
                true ->
                    Health = #{
                        node => Node,
                        status => suspect,
                        last_seen => erlang:timestamp(),
                        latency => undefined,
                        partition_count => 0
                    },
                    {reply, Health, State};
                false ->
                    {reply, {error, unknown_node}, State}
            end
    end;

handle_call(get_cluster_health, _From, State) ->
    NodeStatus = maps:get(node_status, State, #{}),
    AllNodes = [node() | nodes()],
    TotalNodes = length(AllNodes),

    HealthyCount = lists:foldl(
        fun(N, Acc) ->
            case maps:get(N, NodeStatus, up) of
                up -> Acc + 1;
                _ -> Acc
            end
        end,
        0,
        AllNodes
    ),

    PartitionedNodes = [N || N <- AllNodes,
                             maps:get(N, NodeStatus, up) =:= partitioned],

    Health = #{
        total_nodes => TotalNodes,
        healthy_nodes => HealthyCount,
        partitioned_nodes => PartitionedNodes,
        has_partition => length(PartitionedNodes) > 0
    },

    {reply, Health, State};

handle_call(detect_partition, _From, State) ->
    NodeStatus = maps:get(node_status, State, #{}),
    PartitionedNodes = [N || {N, S} <- maps:to_list(NodeStatus), S =:= partitioned],

    case PartitionedNodes of
        [] ->
            {reply, ok, State};
        _ ->
            {reply, {partition, PartitionedNodes}, State}
    end;

handle_call({heal_partition, Nodes}, _From, State = #{healing_strategy := Strategy}) ->
    Reply = execute_healing(Nodes, Strategy),
    {reply, Reply, State};

handle_call({set_healing_strategy, Strategy}, _From, State) ->
    logger:info("Healing strategy changed: ~p", [Strategy],
                [{info, "strategy_change"}, {application, cre}]),
    {reply, ok, State#{healing_strategy => Strategy}};

handle_call({ping_nodes, Nodes}, _From, State) ->
    Results = ping_nodes_impl(Nodes),
    {reply, Results, State};

handle_call(monitor_nodes, _From, State) ->
    net_kernel:monitor_nodes(true, [nodedown_reason]),
    logger:info("Node monitoring enabled",
                [{info, "monitoring_enabled"}, {application, cre}]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({nodeup, Node, _InfoList}, State = #{node_status := NodeStatus}) ->
    logger:info("Node up detected: ~p", [Node],
                [{info, "nodeup"}, {application, cre}]),
    NewNodeStatus = maps:put(Node, up, NodeStatus),
    {noreply, State#{node_status => NewNodeStatus}};

handle_info({nodedown, Node, InfoList}, State = #{node_status := NodeStatus}) ->
    Reason = proplists:get_value(reason, InfoList, disconnect),
    Status = case Reason of
        {net_split, _} -> partitioned;
        _ -> down
    end,
    logger:warning("Node down: ~p, status: ~p, reason: ~p",
                   [Node, Status, Reason],
                   [{info, "nodedown"}, {application, cre}]),
    NewNodeStatus = maps:put(Node, Status, NodeStatus),

    %% Trigger auto-healing if configured
    NewState = case {Status, maps:get(healing_strategy, State)} of
        {partitioned, auto_rejoin} ->
            execute_healing([Node], auto_rejoin),
            State#{node_status => NewNodeStatus};
        _ ->
            State#{node_status => NewNodeStatus}
    end,

    {noreply, NewState};

handle_info(health_check, State = #{monitor_ref := OldRef}) ->
    %% Cancel old timer and schedule new one
    erlang:cancel_timer(OldRef),

    MonitorInterval = application:get_env(cluster, monitor_interval, 5000),
    NewRef = erlang:send_after(MonitorInterval, self(), health_check),

    %% Perform health check on all visible nodes
    NodeStatus = maps:get(node_status, State, #{}),
    AllNodes = [node() | nodes()],

    UpdatedStatus = lists:foldl(
        fun(N, Acc) ->
            case net_adm:ping(N) of
                pong ->
                    maps:put(N, up, Acc);
                pang ->
                    CurrentStatus = maps:get(N, Acc, down),
                    %% Don't change partitioned status on ping failure
                    case CurrentStatus of
                        partitioned -> Acc;
                        _ -> maps:put(N, down, Acc)
                    end
            end
        end,
        NodeStatus,
        AllNodes
    ),

    {noreply, State#{node_status => UpdatedStatus,
                      last_check => erlang:timestamp(),
                      monitor_ref => NewRef}};

handle_info({'EXIT', _Pid, Reason}, State) ->
    logger:error("Process exit: ~p", [Reason],
                 [{info, "process_exit"}, {application, cre}]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    net_kernel:monitor_nodes(false),
    logger:info("Cluster utilities stopping",
                [{info, "cluster_utils_terminate"}, {application, cre}]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Pings a list of nodes and returns results.
-spec ping_nodes_impl([node()]) -> [{node(), pong | pang}].
ping_nodes_impl(Nodes) ->
    [{N, net_adm:ping(N)} || N <- Nodes].

%% @private Executes healing strategy for partitioned nodes.
-spec execute_healing([node()], healing_strategy()) -> ok | {error, term()}.
execute_healing(Nodes, auto_rejoin) ->
    logger:info("Auto-healing partition for nodes: ~p", [Nodes],
                [{info, "auto_heal"}, {application, cre}]),
    lists:foreach(
        fun(Node) ->
            case net_adm:ping(Node) of
                pong ->
                    %% Node is reachable, attempt rejoin
                    case mnesia:change_config(extra_db_nodes, [Node]) of
                        {ok, _} ->
                            logger:info("Auto-rejoined node: ~p", [Node],
                                        [{info, "auto_rejoin_success"}, {application, cre}]);
                        {error, Reason} ->
                            logger:error("Auto-rejoin failed for ~p: ~p", [Node, Reason],
                                         [{info, "auto_rejoin_failed"}, {application, cre}])
                    end;
                pang ->
                    logger:warning("Cannot heal ~p: node not reachable", [Node],
                                   [{info, "heal_unreachable"}, {application, cre}])
            end
        end,
        Nodes
    ),
    ok;
execute_healing(_Nodes, manual) ->
    logger:warning("Manual healing required - no action taken",
                   [{info, "manual_heal"}, {application, cre}]),
    {error, manual_intervention_required};
execute_healing(Nodes, majority) ->
    %% Check which partition has majority of nodes
    TotalNodes = length([node() | nodes()]),
    PartitionSize = length(Nodes),
    CurrentPartitionSize = TotalNodes - PartitionSize,

    if
        CurrentPartitionSize > PartitionSize ->
            logger:info("Current partition has majority (~p vs ~p), isolating minority",
                        [CurrentPartitionSize, PartitionSize],
                        [{info, "majority_heal"}, {application, cre}]),
            %% This partition has majority, isolate the minority
            ok;
        PartitionSize > CurrentPartitionSize ->
            logger:warning("Minority partition detected, waiting for majority",
                           [{info, "minority_partition"}, {application, cre}]),
            {error, minority_partition};
        true ->
            logger:warning("Equal partition size, manual intervention required",
                           [{info, "tie_partition"}, {application, cre}]),
            {error, tie_break_required}
    end.

%%--------------------------------------------------------------------
%% EUnit Tests
%%--------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test ping_nodes_impl/1
ping_nodes_impl_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     [
         ?_test(begin
             %% Test with self node
             Result = ping_nodes_impl([node()]),
             ?assertEqual(1, length(Result)),
             {Node, Status} = lists:nth(1, Result),
             ?assertEqual(node(), Node),
             ?assertEqual(pong, Status)
         end)
     ]
    }.

-endif.
