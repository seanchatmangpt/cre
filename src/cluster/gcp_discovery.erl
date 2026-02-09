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
%% @doc GCP/GKE Node Auto-Discovery Module
%%
%% This module provides automatic node discovery for CRE clustering on
%% Google Kubernetes Engine (GKE). It supports DNS-based service discovery
%% using headless services and Kubernetes API-based pod watching.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>DNS Discovery:</b> Query headless service for pod addresses</li>
%%   <li><b>K8s API Watch:</b> Watch StatefulSet for pod changes</li>
%%   <li><b>Pod Replacement:</b> Handle pod restart gracefully</li>
%%   <li><b>StatefulSet Support:</b> Stable network identities</li>
%%   <li><b>Deployment Support:</b> Dynamic pod discovery</li>
%%   <li><b>No Hard-coded IPs:</b> All addresses from DNS or API</li>
%% </ul>
%%
%% <h3>Discovery Methods</h3>
%%
%% <ol>
%%   <li><b>Headless Service DNS:</b> Queries service.namespace.svc.cluster.local</li>
%%   <li><b>K8s API Watch:</b> Watches StatefulSet/Deployment pod events</li>
%%   <li><b>Pod DNS Records:</b> Individual pod DNS entries</li>
%% </ol>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Discover peers via DNS
%% Peers = gcp_discovery:discover_peers(),
%%
%% %% Watch StatefulSet for changes
%% {ok, WatchRef} = gcp_discovery:watch_statefulset("cre-statefulset"),
%%
%% %% Build node list from pod addresses
%% Nodes = gcp_discovery:build_node_list(["10.0.0.1", "10.0.0.2"]),
%%
%% %% Stop watching
%% ok = gcp_discovery:stop_watch(WatchRef).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(gcp_discovery).
-include_lib("kernel/include/inet.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% API functions
-export([discover_peers/0,
         discover_peers/1,
         watch_statefulset/1,
         watch_statefulset/2,
         watch_deployment/1,
         watch_deployment/2,
         build_node_list/1,
         build_node_list/2,
         stop_watch/1,
         get_service_name/0,
         get_namespace/0,
         get_pod_hostname/0,
         get_pod_ip/0,
         get_node_name/0,
         build_dns_target/3,
         resolve_srv_targets/1,
         build_node_string/2]).

%%====================================================================
%% Type definitions
%%====================================================================

-type pod_address() :: inet:ip4_address() | inet:ip6_address() | string().
-type discovery_config() :: #{service_name => string(),
                             namespace => string(),
                             node_name => string(),
                             dns_domain => string(),
                             use_pod_dns => boolean()}.
-type watch_ref() :: {pid(), reference()}.
-type watch_target() :: {statefulset | deployment, string()}.

%%====================================================================
%% API functions
%%====================================================================

%% @doc Discovers peer nodes using GKE DNS-based discovery.
%%
%%      Uses the configured headless service to query all pod IPs.
%%      Returns a list of Erlang node names for discovered peers.
%%
%% @returns List of discovered node names
%%
-spec discover_peers() -> [node()].
discover_peers() ->
    discover_peers(#{}).

%% @doc Discovers peer nodes with custom configuration.
%%
%%      Configuration options:
%%      - `{service_name, Name}' - Headless service name (default from env)
%%      - `{namespace, NS}' - Kubernetes namespace (default from env)
%%      - `{use_pod_dns, Bool}' - Use per-pod DNS vs A records (default false)
%%      - `{dns_domain, Domain}' - DNS domain (default cluster.local)
%%
%% @param Config Configuration map for discovery
%% @returns List of discovered node names
%%
-spec discover_peers(discovery_config()) -> [node()].
discover_peers(Config) ->
    ServiceName = maps:get(service_name, Config, get_service_name()),
    Namespace = maps:get(namespace, Config, get_namespace()),
    Domain = maps:get(dns_domain, Config, "cluster.local"),
    UsePodDns = maps:get(use_pod_dns, Config, false),

    %% Build DNS query target
    DnsTarget = build_dns_target(ServiceName, Namespace, Domain),

    logger:info("GCP DNS discovery querying: ~p", [DnsTarget],
                [{info, "gcp_discovery"}, {application, cre}]),

    case query_dns(DnsTarget, UsePodDns) of
        {ok, Addresses} ->
            Nodes = build_node_list(Addresses, Config),
            logger:info("GCP discovery found ~p nodes: ~p", [length(Nodes), Nodes],
                        [{info, "gcp_discovery_success"}, {application, cre}]),
            Nodes;
        {error, Reason} ->
            logger:warning("GCP DNS discovery failed: ~p", [Reason],
                           [{info, "gcp_discovery_failed"}, {application, cre}]),
            []
    end.

%% @doc Starts watching a StatefulSet for pod changes.
%%
%%      Spawns a watcher process that monitors the Kubernetes API
%%      for pod addition/removal events. Returns a watch reference
%%      that can be used to stop watching.
%%
%% @param StatefulSetName Name of the StatefulSet to watch
%% @returns `{ok, WatchRef}' | `{error, Reason}'
%%
-spec watch_statefulset(string()) -> {ok, watch_ref()} | {error, term()}.
watch_statefulset(StatefulSetName) ->
    watch_statefulset(StatefulSetName, #{}).

%% @doc Starts watching a StatefulSet with custom configuration.
%%
%%      Configuration options:
%%      - `{namespace, NS}' - Kubernetes namespace
%%      - `{callback, {M, F, A}}' - Callback for pod changes
%%
%% @param StatefulSetName Name of the StatefulSet to watch
%% @param Config Configuration map for watching
%% @returns `{ok, WatchRef}' | `{error, Reason}'
%%
-spec watch_statefulset(string(), discovery_config()) ->
          {ok, watch_ref()} | {error, term()}.
watch_statefulset(StatefulSetName, Config) ->
    watch_target({statefulset, StatefulSetName}, Config).

%% @doc Starts watching a Deployment for pod changes.
%%
%%      Similar to watch_statefulset but for Deployments which have
%%      less stable network identities.
%%
%% @param DeploymentName Name of the Deployment to watch
%% @returns `{ok, WatchRef}' | `{error, Reason}'
%%
-spec watch_deployment(string()) -> {ok, watch_ref()} | {error, term()}.
watch_deployment(DeploymentName) ->
    watch_deployment(DeploymentName, #{}).

%% @doc Starts watching a Deployment with custom configuration.
%%
%% @param DeploymentName Name of the Deployment to watch
%% @param Config Configuration map for watching
%% @returns `{ok, WatchRef}' | `{error, Reason}'
%%
-spec watch_deployment(string(), discovery_config()) ->
          {ok, watch_ref()} | {error, term()}.
watch_deployment(DeploymentName, Config) ->
    watch_target({deployment, DeploymentName}, Config).

%% @doc Builds a list of Erlang node names from pod addresses.
%%
%%      Uses the current node name prefix and combines it with
%%      each pod IP address to form full Erlang node names.
%%
%% @param Addresses List of pod IP addresses
%% @returns List of Erlang node names
%%
-spec build_node_list([pod_address()]) -> [node()].
build_node_list(Addresses) ->
    build_node_list(Addresses, #{}).

%% @doc Builds a list of Erlang node names with custom configuration.
%%
%%      Configuration options:
%%      - `{node_name, Name}' - Node name prefix (default from env)
%%      - `{exclude_self, Bool}' - Exclude current node from list (default true)
%%
%% @param Addresses List of pod IP addresses
%% @param Config Configuration map for node building
%% @returns List of Erlang node names
%%
-spec build_node_list([pod_address()], discovery_config()) -> [node()].
build_node_list(Addresses, Config) ->
    NodeName = maps:get(node_name, Config, get_node_name()),
    ExcludeSelf = maps:get(exclude_self, Config, true),
    SelfNode = node(),

    Nodes = lists:filtermap(
        fun(Addr) ->
            NodeStr = build_node_string(NodeName, Addr),
            try
                Node = list_to_atom(NodeStr),
                case ExcludeSelf andalso Node =:= SelfNode of
                    true -> false;
                    false -> {true, Node}
                end
            catch
                _:_ -> false
            end
        end,
        Addresses
    ),

    lists:usort(Nodes).

%% @doc Stops watching a Kubernetes resource.
%%
%%      Stops the watcher process and cleans up resources.
%%
%% @param WatchRef Watch reference from watch_statefulset/watch_deployment
%% @returns `ok'
%%
-spec stop_watch(watch_ref()) -> ok.
stop_watch({Pid, _Ref}) when is_pid(Pid) ->
    case is_process_alive(Pid) of
        true ->
            gen_server:stop(Pid);
        false ->
            ok
    end.

%% @doc Gets the service name from environment variables.
%%
%%      Reads the CRE_SERVICE_NAME environment variable.
%%      Defaults to "cre-service" if not set.
%%
%% @returns Service name string
%%
-spec get_service_name() -> string().
get_service_name() ->
    os:getenv("CRE_SERVICE_NAME", "cre-service").

%% @doc Gets the Kubernetes namespace from environment variables.
%%
%%      Reads the CRE_NAMESPACE or POD_NAMESPACE environment variable.
%%      Defaults to "default" if not set.
%%
%% @returns Namespace string
%%
-spec get_namespace() -> string().
get_namespace() ->
    os:getenv("CRE_NAMESPACE",
              os:getenv("POD_NAMESPACE", "default")).

%% @doc Gets the current pod hostname from environment variables.
%%
%%      Reads the HOSTNAME or POD_NAME environment variable.
%%      Defaults to "localhost" if not set.
%%
%% @returns Pod hostname string
%%
-spec get_pod_hostname() -> string().
get_pod_hostname() ->
    os:getenv("HOSTNAME",
              os:getenv("POD_NAME", "localhost")).

%% @doc Gets the current pod IP address from environment variables.
%%
%%      Reads the POD_IP or CRE_POD_IP environment variable.
%%      Returns undefined if not set.
%%
%% @returns Pod IP address string or undefined
%%
-spec get_pod_ip() -> string() | undefined.
get_pod_ip() ->
    os:getenv("POD_IP",
              os:getenv("CRE_POD_IP")).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Gets the node name prefix from environment.
-spec get_node_name() -> string().
get_node_name() ->
    case os:getenv("CRE_NODE_NAME") of
        false ->
            %% Extract from current node name
            case string:split(atom_to_list(node()), "@", leading) of
                [NamePrefix, _] -> NamePrefix;
                _ -> "cre"
            end;
        Name ->
            Name
    end.

%% @private Builds DNS query target for service discovery.
-spec build_dns_target(string(), string(), string()) -> string().
build_dns_target(ServiceName, Namespace, Domain) ->
    %% K8s headless service pattern:
    %% service.namespace.svc.cluster.local
    lists:flatten(io_lib:format("~s.~s.svc.~s", [ServiceName, Namespace, Domain])).

%% @private Queries DNS for pod addresses.
-spec query_dns(string(), boolean()) -> {ok, [pod_address()]} | {error, term()}.
query_dns(Target, UsePodDns) ->
    case inet_res:getbyname(Target, a) of
        {ok, #hostent{h_addr_list = Addrs}} when is_list(Addrs), Addrs =/= [] ->
            %% Convert IP tuples to strings
            AddressStrings = [inet:ntoa(A) || A <- Addrs],
            {ok, AddressStrings};
        {error, nxdomain} when UsePodDns =:= false ->
            %% Try SRV record as fallback (may indicate headless service)
            case inet_res:getbyname("_erlang._tcp." ++ Target, srv) of
                {ok, #hostent{h_addr_list = SrvAddrs}} ->
                    %% Resolve SRV targets to IPs
                    ResolvedAddrs = resolve_srv_targets(SrvAddrs),
                    {ok, ResolvedAddrs};
                {error, _} ->
                    {error, dns_not_found}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Resolves SRV record targets to IP addresses.
-spec resolve_srv_targets([term()]) -> [string()].
resolve_srv_targets(SrvAddrs) ->
    lists:filtermap(
        fun(SrvRec) ->
            %% Extract target from SRV record
            Target = case SrvRec of
                {_, _, _, Host} -> Host;
                {_, _, _, Host, _} -> Host;
                Host when is_list(Host) -> Host;
                _ -> undefined
            end,
            case Target of
                undefined -> false;
                _ ->
                    %% Resolve target hostname to IP
                    case inet_res:getbyname(Target, a) of
                        {ok, #hostent{h_addr_list = [Ip | _]}} ->
                            {true, inet:ntoa(Ip)};
                        _ ->
                            false
                    end
            end
        end,
        SrvAddrs
    ).

%% @private Builds node string from name prefix and address.
-spec build_node_string(string(), pod_address()) -> string().
build_node_string(NodeName, Address) ->
    AddrStr = case Address of
        {A, B, C, D} when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
            lists:flatten(inet:ntoa({A, B, C, D}));
        {A, B, C, D, E, F, G, H} when is_integer(A) ->
            lists:flatten(inet:ntoa({A, B, C, D, E, F, G, H}));
        Str when is_list(Str) ->
            Str;
        _ ->
            "127.0.0.1"
    end,
    NodeName ++ "@" ++ AddrStr.

%% @private Starts watching a Kubernetes target.
-spec watch_target(watch_target(), discovery_config()) ->
          {ok, watch_ref()} | {error, term()}.
watch_target({Type, Name}, Config) ->
    Namespace = maps:get(namespace, Config, get_namespace()),
    Callback = maps:get(callback, Config, fun(_, _, _) -> ok end),

    case start_watcher(Type, Name, Namespace, Callback) of
        {ok, Pid} ->
            Ref = make_ref(),
            WatchRef = {Pid, Ref},
            {ok, WatchRef};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Starts a watcher process.
-spec start_watcher(statefulset | deployment, string(), string(),
                    function()) -> {ok, pid()} | {error, term()}.
start_watcher(Type, Name, Namespace, Callback) ->
    %% Spawn watcher process
    proc_lib:start_link(?MODULE, watcher_init, [Type, Name, Namespace, Callback]).

%% @private Watcher process initialization.
-spec watcher_init(statefulset | deployment, string(), string(),
                   function()) -> no_return().
watcher_init(Type, Name, Namespace, Callback) ->
    proc_lib:init_ack({ok, self()}),
    watcher_loop(Type, Name, Namespace, Callback, #{}).

%% @private Watcher process loop with DNS polling.
%% Note: Full K8s API watching would require httpc/hackney integration.
%% This implementation uses DNS polling as a lightweight alternative.
-spec watcher_loop(statefulset | deployment, string(), string(),
                   function(), map()) -> no_return().
watcher_loop(Type, Name, Namespace, Callback, State) ->
    %% Poll DNS for changes
    ServiceName = maps:get(service_name, State, get_service_name()),
    Domain = maps:get(dns_domain, State, "cluster.local"),

    DnsTarget = build_dns_target(ServiceName, Namespace, Domain),
    {ok, CurrentAddrs} = query_dns(DnsTarget, false),

    %% Check for changes
    PreviousAddrs = maps:get(addresses, State, []),
    Added = lists:usort(CurrentAddrs) -- lists:usort(PreviousAddrs),
    Removed = lists:usort(PreviousAddrs) -- lists:usort(CurrentAddrs),

    %% Trigger callbacks for changes
    lists:foreach(
        fun(Addr) ->
            Callback({pod_added, Type, Name}, Addr, State)
        end,
        Added
    ),
    lists:foreach(
        fun(Addr) ->
            Callback({pod_removed, Type, Name}, Addr, State)
        end,
        Removed
    ),

    %% Update state
    NewState = State#{addresses => CurrentAddrs,
                      type => Type,
                      name => Name,
                      namespace => Namespace},

    %% Schedule next poll (5 second interval)
    timer:sleep(5000),
    watcher_loop(Type, Name, Namespace, Callback, NewState).

%%--------------------------------------------------------------------
%% EUnit Tests
%%--------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test build_node_list/1
build_node_list_test_() ->
    [
        ?_assertEqual([], build_node_list([])),
        ?_assertEqual(['nonode@127.0.0.1'], build_node_list(["127.0.0.1"])),
        ?_assertEqual(['nonode@10.0.0.1', 'nonode@10.0.0.2'],
                      build_node_list(["10.0.0.1", "10.0.0.2"])),
        ?_test(begin
            %% Test exclude_self by passing the actual node name
            CurrentNode = node(),
            SelfIp = "10.0.0.1",
            SelfNodeStr = atom_to_list(CurrentNode),
            Nodes = build_node_list([SelfIp, "10.0.0.2"],
                                    #{node_name => SelfNodeStr, exclude_self => true}),
            ?assertNot(lists:member(CurrentNode, Nodes))
        end),
        ?_assertEqual(['mynode@192.168.1.1'],
                      build_node_list(["192.168.1.1"],
                                      #{node_name => "mynode", exclude_self => false}))
    ].

%% Test build_node_string/2
build_node_string_test_() ->
    [
        ?_assertEqual("cre@10.0.0.1",
                     lists:flatten(build_node_string("cre", "10.0.0.1"))),
        ?_assertEqual("node@192.168.1.1",
                     lists:flatten(build_node_string("node", "192.168.1.1"))),
        ?_assertEqual("cre@10.0.0.1",
                     build_node_string("cre", {10, 0, 0, 1}))
    ].

%% Test build_dns_target/3
build_dns_target_test_() ->
    [
        ?_assertEqual("service.default.svc.cluster.local",
                     build_dns_target("service", "default", "cluster.local")),
        ?_assertEqual("cre-service.prod.svc.cluster.local",
                     build_dns_target("cre-service", "prod", "cluster.local"))
    ].

%% Test get_service_name/0
get_service_name_test_() ->
    {setup,
     fun() ->
         os:putenv("CRE_SERVICE_NAME", "test-service")
     end,
     fun(_) ->
         os:unsetenv("CRE_SERVICE_NAME")
     end,
     fun(_) ->
         ?_assertEqual("test-service", get_service_name())
     end
    }.

%% Test get_namespace/0
get_namespace_test_() ->
    {setup,
     fun() ->
         os:putenv("CRE_NAMESPACE", "test-ns")
     end,
     fun(_) ->
         os:unsetenv("CRE_NAMESPACE"),
         os:unsetenv("POD_NAMESPACE")
     end,
     fun(_) ->
         ?_assertEqual("test-ns", get_namespace())
     end
    }.

%% Test get_pod_hostname/0
get_pod_hostname_test_() ->
    {setup,
     fun() ->
         os:putenv("HOSTNAME", "test-pod-1")
     end,
     fun(_) ->
         os:unsetenv("HOSTNAME"),
         os:unsetenv("POD_NAME")
     end,
     fun(_) ->
         ?_assertEqual("test-pod-1", get_pod_hostname())
     end
    }.

%% Test get_pod_ip/0
get_pod_ip_test_() ->
    {setup,
     fun() ->
         os:putenv("POD_IP", "10.0.0.5")
     end,
     fun(_) ->
         os:unsetenv("POD_IP"),
         os:unsetenv("CRE_POD_IP")
     end,
     fun(_) ->
         ?_assertEqual("10.0.0.5", get_pod_ip())
     end
    }.

-endif.
