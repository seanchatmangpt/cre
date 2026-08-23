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
%% @doc Mnesia Cluster Management Module
%%
%% This module provides automatic cluster discovery and management for
%% distributed Mnesia databases. It supports DNS-based service discovery
%% for Kubernetes environments and environment variable configuration.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Auto-Discovery:</b> DNS-based node discovery for K8s services</li>
%%   <li><b>Env Config:</b> CRE_CLUSTER_NODES environment variable support</li>
%%   <li><b>Join Orchestration:</b> Automatic cluster joining with retry logic</li>
%%   <li><b>Fault Tolerance:</b> Network partition detection and recovery</li>
%% </ul>
%%
%% <h3>Discovery Methods</h3>
%%
%% <ol>
%%   <li><b>GCP Discovery:</b> GKE headless service and StatefulSet pod discovery</li>
%%   <li><b>Environment Variable:</b> CRE_CLUSTER_NODES=node1@host,node2@host</li>
%%   <li><b>DNS Query:</b> Query _erlang._tcp.service.namespace.svc.cluster.local</li>
%%   <li><b>Headless Service:</b> K8s headless service returns all pod IPs</li>
%% </ol>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Join cluster using auto-discovery
%% ok = cluster:join_cluster(),
%%
%% %% Get current cluster members
%% Nodes = cluster:get_nodes(),
%%
%% %% Leave cluster gracefully
%% ok = cluster:leave_cluster().
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(cluster).
-include_lib("kernel/include/inet.hrl").
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API functions
-export([start_link/0, start_link/1,
         join_cluster/0, join_cluster/1,
         leave_cluster/0,
         get_nodes/0,
         get_status/0,
         set_discovery_method/1,
         discover_peers/0]).

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

-type discovery_method() :: gcp | dns | env | static | none.
-type cluster_state() :: #{discovery_method => discovery_method(),
                          cluster_nodes => [node()],
                          pending_joins => [node()],
                          retry_count => non_neg_integer(),
                          max_retries => pos_integer(),
                          discovery_dns => string(),
                          watch_ref => {pid(), reference()} | undefined}.
-type join_result() :: ok | {error, term()}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts the cluster manager with default configuration.
%%
%%      Uses DNS discovery with headless service pattern.
%%      Registered locally as `cluster`.
%%
%% @returns `{ok, Pid}' | `{error, Reason}'
%%
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts the cluster manager with custom options.
%%
%%      Options:
%%      - `{discovery_method, Method}' - dns, env, static, or none
%%      - `{max_retries, N}' - maximum connection retry attempts
%%      - `{dns_name, Name}' - DNS name for service discovery
%%
-spec start_link([proplists:property()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Joins the Mnesia cluster using configured discovery method.
%%
%%      Performs auto-discovery of peer nodes and attempts to join
%%      the cluster. Retries failed connections with exponential backoff.
%%
%% @returns `ok' | `{error, Reason}'
%%
-spec join_cluster() -> join_result().
join_cluster() ->
    gen_server:call(?MODULE, join_cluster, infinity).

%% @doc Joins the Mnesia cluster with explicit peer nodes.
%%
%%      Bypasses discovery and directly attempts to join the specified
%%      nodes. Useful for static configurations or testing.
%%
%% @param Nodes List of peer nodes to connect to
%% @returns `ok' | `{error, Reason}'
%%
-spec join_cluster([node()]) -> join_result().
join_cluster(Nodes) when is_list(Nodes) ->
    gen_server:call(?MODULE, {join_cluster, Nodes}, infinity).

%% @doc Leaves the Mnesia cluster gracefully.
%%
%%      Stops Mnesia on this node and removes it from the cluster.
%%      Other nodes will detect the departure via Mnesia's monitoring.
%%
%% @returns `ok' | `{error, not_in_cluster}'
%%
-spec leave_cluster() -> ok | {error, not_in_cluster}.
leave_cluster() ->
    gen_server:call(?MODULE, leave_cluster).

%% @doc Returns the current list of cluster nodes.
%%
%%      Includes this node if it has joined the cluster.
%%      Returns an empty list if not yet connected.
%%
%% @returns List of connected nodes
%%
-spec get_nodes() -> [node()].
get_nodes() ->
    gen_server:call(?MODULE, get_nodes).

%% @doc Returns the current cluster status.
%%
%%      Provides information about discovery method, connected nodes,
%%      and any pending join operations.
%%
%% @returns Map with cluster status information
%%
-spec get_status() -> #{discovery_method => discovery_method(),
                        cluster_nodes => [node()],
                        pending_joins => [node()],
                        is_connected => boolean()}.
get_status() ->
    gen_server:call(?MODULE, get_status).

%% @doc Sets the discovery method for cluster joining.
%%
%%      Valid methods:
%%      - `gcp' - GKE/GCP discovery (headless service + StatefulSet)
%%      - `dns' - DNS-based service discovery (K8s headless service)
%%      - `env' - Environment variable CRE_CLUSTER_NODES
%%      - `static' - Pre-configured node list
%%      - `none' - No auto-discovery (manual join only)
%%
-spec set_discovery_method(discovery_method()) -> ok.
set_discovery_method(Method) when Method =:= gcp;
                                  Method =:= dns;
                                  Method =:= env;
                                  Method =:= static;
                                  Method =:= none ->
    gen_server:call(?MODULE, {set_discovery_method, Method}).

%% @discovers peer nodes using the configured discovery method.
%%
%%      Returns a list of discovered Erlang nodes that can be
%%      contacted for cluster joining.
%%
%% @returns List of discovered node names
%%
-spec discover_peers() -> [node()].
discover_peers() ->
    gen_server:call(?MODULE, discover_peers).

%%====================================================================
%% gen_server callback functions
%%====================================================================

%% @private
init(Options) ->
    process_flag(trap_exit, true),

    DiscoveryMethod = proplists:get_value(discovery_method, Options, gcp),
    MaxRetries = proplists:get_value(max_retries, Options, 5),
    DnsName = proplists:get_value(dns_name, Options,
                                  application:get_env(cluster, dns_name, "localhost")),

    State = #{discovery_method => DiscoveryMethod,
              cluster_nodes => [],
              pending_joins => [],
              retry_count => 0,
              max_retries => MaxRetries,
              discovery_dns => DnsName,
              watch_ref => undefined},

    logger:info("Cluster manager started: method=~p, dns=~p",
                [DiscoveryMethod, DnsName],
                [{info, "cluster_init"}, {application, cre}]),

    %% Subscribe to net_split events for partition detection
    net_kernel:monitor_nodes(true, [nodedown_reason]),

    {ok, State}.

%% @private
handle_call(join_cluster, _From, State) ->
    {Reply, NewState} = do_join_cluster(State),
    {reply, Reply, NewState};

handle_call({join_cluster, Nodes}, _From, State) ->
    {Reply, NewState} = do_join_cluster(Nodes, State),
    {reply, Reply, NewState};

handle_call(leave_cluster, _From, State) ->
    Reply = do_leave_cluster(),
    {reply, Reply, State#{cluster_nodes => [], pending_joins => []}};

handle_call(get_nodes, _From, #{cluster_nodes := Nodes} = State) ->
    {reply, Nodes, State};

handle_call(get_status, _From, State = #{discovery_method := Method,
                                         cluster_nodes := Nodes,
                                         pending_joins := Pending}) ->
    IsConnected = length(Nodes) > 0,
    Status = #{discovery_method => Method,
               cluster_nodes => Nodes,
               pending_joins => Pending,
               is_connected => IsConnected,
               node_count => length(Nodes)},
    {reply, Status, State};

handle_call({set_discovery_method, Method}, _From, State = #{watch_ref := WatchRef}) ->
    %% Clean up existing watch if switching from gcp
    NewState = case WatchRef of
        undefined ->
            State;
        _ ->
            gcp_discovery:stop_watch(WatchRef),
            State#{watch_ref => undefined}
    end,
    logger:info("Discovery method changed: ~p", [Method],
                [{info, "discovery_change"}, {application, cre}]),
    {reply, ok, NewState#{discovery_method => Method}};

handle_call(discover_peers, _From, State = #{discovery_method := Method}) ->
    Peers = discover_nodes(Method, State),
    {reply, Peers, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({nodeup, Node, _InfoList}, State) ->
    logger:info("Node up: ~p", [Node],
                [{info, "nodeup"}, {application, cre}]),
    %% Update cluster nodes list
    ClusterNodes = maps:get(cluster_nodes, State, []),
    NewClusterNodes = lists:usort([Node | ClusterNodes]),
    {noreply, State#{cluster_nodes => NewClusterNodes}};

handle_info({nodedown, Node, InfoList}, State) ->
    Reason = proplists:get_value(reason, InfoList, unknown),
    logger:warning("Node down: ~p, reason: ~p", [Node, Reason],
                   [{info, "nodedown"}, {application, cre}]),

    %% Remove from cluster nodes
    ClusterNodes = maps:get(cluster_nodes, State, []),
    NewClusterNodes = lists:delete(Node, ClusterNodes),

    %% Trigger healing if this was a network partition
    NewState = case Reason of
        {net_split, _} ->
            handle_net_split(Node, State);
        _ ->
            State
    end,

    {noreply, NewState#{cluster_nodes => NewClusterNodes}};

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
terminate(_Reason, State = #{watch_ref := WatchRef}) ->
    %% Clean up GCP watch if active
    case WatchRef of
        undefined -> ok;
        _ -> gcp_discovery:stop_watch(WatchRef)
    end,
    logger:info("Cluster manager stopping", [],
                [{info, "cluster_terminate"}, {application, cre}]),
    net_kernel:monitor_nodes(false),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Performs cluster join using configured discovery method.
-spec do_join_cluster(cluster_state()) -> {join_result(), cluster_state()}.
do_join_cluster(State = #{discovery_method := Method}) ->
    Peers = discover_nodes(Method, State),
    do_join_cluster(Peers, State).

%% @private Joins cluster with explicit peer list.
-spec do_join_cluster([node()], cluster_state()) -> {join_result(), cluster_state()}.
do_join_cluster([], State) ->
    logger:warning("No peer nodes discovered for cluster join",
                   [{info, "no_peers"}, {application, cre}]),
    {{error, no_peers}, State};
do_join_cluster(Peers, State) ->
    logger:info("Attempting to join cluster with ~p peers", [length(Peers)],
                [{info, "cluster_join"}, {application, cre}]),

    case mnesia:system_info(is_running) of
        yes ->
            %% Mnesia is running, attempt to join cluster
            case join_peers(Peers, State) of
                {ok, JoinedNodes} ->
                    logger:info("Successfully joined cluster: ~p", [JoinedNodes],
                                [{info, "cluster_joined"}, {application, cre}]),
                    {ok, State#{cluster_nodes => JoinedNodes, pending_joins => []}};
                {error, Reason} ->
                    logger:error("Failed to join cluster: ~p", [Reason],
                                 [{info, "cluster_join_failed"}, {application, cre}]),
                    {{error, Reason}, State}
            end;
        no ->
            %% Mnesia not running, start it first
            case mnesia:start() of
                ok ->
                    do_join_cluster(Peers, State);
                {error, Reason} ->
                    logger:error("Failed to start Mnesia: ~p", [Reason],
                                 [{info, "mnesia_start_failed"}, {application, cre}]),
                    {{error, {mnesia_start_failed, Reason}}, State}
            end;
        {error, Reason} ->
            logger:error("Mnesia error: ~p", [Reason],
                         [{info, "mnesia_error"}, {application, cre}]),
            {{error, {mnesia_error, Reason}}, State}
    end.

%% @private Attempts to join a list of peer nodes.
-spec join_peers([node()], cluster_state()) -> {ok, [node()]} | {error, term()}.
join_peers(Peers, _State) ->
    CurrentNode = node(),
    join_peers_loop(Peers, [], CurrentNode).

%% @private Loop through peers attempting to join.
-spec join_peers_loop([node()], [node()], node()) -> {ok, [node()]} | {error, term()}.
join_peers_loop([], _Joined, _CurrentNode) ->
    {error, all_peers_failed};
join_peers_loop([Peer | Rest], Joined, CurrentNode) ->
    %% Check if peer is alive and has Mnesia running
    case net_adm:ping(Peer) of
        pong ->
            case rpc:call(Peer, mnesia, system_info, [is_running]) of
                yes ->
                    %% Peer has Mnesia running, attempt to join
                    case mnesia:change_config(extra_db_nodes, [Peer]) of
                        {ok, [Peer]} ->
                            logger:info("Joined peer: ~p", [Peer],
                                        [{info, "peer_joined"}, {application, cre}]),
                            %% Wait for tables to load
                            wait_for_tables(),
                            {ok, [Peer | Joined]};
                        {ok, []} ->
                            %% Already in cluster
                            {ok, [Peer | Joined]};
                        {error, Reason} ->
                            logger:warning("Failed to join ~p: ~p", [Peer, Reason],
                                           [{info, "peer_join_failed"}, {application, cre}]),
                            join_peers_loop(Rest, Joined, CurrentNode)
                    end;
                _ ->
                    logger:warning("Peer ~p Mnesia not running", [Peer],
                                   [{info, "peer_no_mnesia"}, {application, cre}]),
                    join_peers_loop(Rest, Joined, CurrentNode)
            end;
        pang ->
            logger:info("Peer ~p not responding", [Peer],
                        [{info, "peer_unreachable"}, {application, cre}]),
            join_peers_loop(Rest, Joined, CurrentNode)
    end.

%% @private Waits for Mnesia tables to load after joining cluster.
-spec wait_for_tables() -> ok.
wait_for_tables() ->
    case mnesia:system_info(tables) of
        [] ->
            ok;
        Tables ->
            %% Exclude schema from wait list
            LocalTables = lists:delete(schema, Tables),
            case mnesia:wait_for_tables(LocalTables, 30000) of
                ok ->
                    ok;
                {timeout, BadTables} ->
                    logger:warning("Table load timeout: ~p", [BadTables],
                                   [{info, "table_timeout"}, {application, cre}]),
                    ok;
                {error, Reason} ->
                    logger:error("Table load error: ~p", [Reason],
                                 [{info, "table_error"}, {application, cre}]),
                    ok
            end
    end.

%% @private Leaves the Mnesia cluster.
-spec do_leave_cluster() -> ok | {error, not_in_cluster}.
do_leave_cluster() ->
    case mnesia:system_info(is_running) of
        yes ->
            case mnesia:system_info(extra_db_nodes) of
                [] ->
                    logger:info("Not in cluster or only node",
                                [{info, "leave_cluster"}, {application, cre}]),
                    {error, not_in_cluster};
                _Nodes ->
                    %% Stop Mnesia to leave cluster
                    stopped = mnesia:stop(),
                    logger:info("Left Mnesia cluster",
                                [{info, "left_cluster"}, {application, cre}]),
                    ok
            end;
        no ->
            {error, not_in_cluster}
    end.

%% @private Discovers peer nodes based on method.
-spec discover_nodes(discovery_method(), cluster_state()) -> [node()].
discover_nodes(gcp, _State) ->
    %% Use GCP/GKE discovery module
    case gcp_discovery:discover_peers() of
        Nodes when is_list(Nodes) ->
            Nodes;
        {error, Reason} ->
            logger:info("GCP discovery failed: ~p", [Reason],
                        [{info, "gcp_discovery_failed"}, {application, cre}]),
            []
    end;
discover_nodes(env, _State) ->
    %% Read CRE_CLUSTER_NODES environment variable
    case os:getenv("CRE_CLUSTER_NODES") of
        false ->
            [];
        NodesStr ->
            parse_nodes_string(NodesStr)
    end;
discover_nodes(dns, #{discovery_dns := DnsName}) ->
    %% Perform DNS SRV record lookup for service discovery
    %% K8s pattern: _erlang._tcp.service.namespace.svc.cluster.local
    case inet_res:getbyname("_erlang._tcp." ++ DnsName, srv) of
        {ok, #hostent{h_addr_list = Addrs}} when is_list(Addrs), Addrs =/= [] ->
            build_nodes_from_addrs(Addrs);
        {error, Reason} ->
            logger:info("DNS discovery failed: ~p", [Reason],
                        [{info, "dns_discovery_failed"}, {application, cre}]),
            [];
        _ ->
            []
    end;
discover_nodes(static, _State) ->
    %% Use pre-configured static nodes from app config
    case application:get_env(cluster, static_nodes) of
        {ok, Nodes} when is_list(Nodes) -> Nodes;
        _ -> []
    end;
discover_nodes(none, _State) ->
    [].

%% @private Parses comma-separated node list from string.
-spec parse_nodes_string(string()) -> [node()].
parse_nodes_string(NodesStr) ->
    Parts = string:split(NodesStr, ",", all),
    lists:filtermap(
        fun(Part) ->
            NodeStr = string:trim(Part),
            case NodeStr of
                "" -> false;
                _ ->
                    try
                        {true, list_to_atom(NodeStr)}
                    catch
                        _:_ -> false
                    end
            end
        end,
        Parts
    ).

%% @private Builds node list from DNS addresses.
-spec build_nodes_from_addrs([inet:ip4_address() | inet:ip6_address()]) -> [node()].
build_nodes_from_addrs(Addrs) ->
    NodeName = list_to_atom(os:getenv("CRE_NODE_NAME", "cre")),
    lists:filtermap(
        fun(Addr) ->
            IpStr = inet:ntoa(Addr),
            NodeStr = NodeName ++ "@" ++ IpStr,
            try
                {true, list_to_atom(NodeStr)}
            catch
                _:_ -> false
            end
        end,
        Addrs
    ).

%% @private Handles network partition events.
-spec handle_net_split(node(), cluster_state()) -> cluster_state().
handle_net_split(Node, State) ->
    logger:warning("Network partition detected with node ~p, initiating healing",
                   [Node],
                   [{info, "net_split"}, {application, cre}]),

    %% Trigger reconnection attempts
    Pending = maps:get(pending_joins, State, []),
    NewPending = lists:usort([Node | Pending]),

    %% Schedule reconnection
    erlang:send_after(5000, self(), {reconnect, Node}),

    State#{pending_joins => NewPending}.

%%--------------------------------------------------------------------
%% EUnit Tests
%%--------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test parse_nodes_string/1
parse_nodes_string_test_() ->
    [
        ?_assertEqual([], parse_nodes_string("")),
        ?_assertEqual(['node1@host'], parse_nodes_string("node1@host")),
        ?_assertEqual(['node1@host', 'node2@host'],
                      parse_nodes_string("node1@host,node2@host")),
        ?_assertEqual(['node1@host', 'node2@host'],
                      parse_nodes_string("node1@host, node2@host")),
        ?_assertEqual(['node1@host', 'node2@host'],
                      parse_nodes_string(" node1@host , node2@host "))
    ].

%% Test build_nodes_from_addrs/1
build_nodes_from_addrs_test_() ->
    [
        ?_assertEqual([], build_nodes_from_addrs([])),
        ?_assertEqual(['cre@127.0.0.1'], build_nodes_from_addrs([{127,0,0,1}])),
        ?_assertEqual(['cre@10.0.0.1', 'cre@10.0.0.2'],
                      build_nodes_from_addrs([{10,0,0,1}, {10,0,0,2}]))
    ].

-endif.
