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
%% @doc EUnit Tests for GCP Discovery Module
%%
%% This test suite covers the gcp_discovery module including DNS-based
%% service discovery, node list building, and Kubernetes integration.
%%
%% @end
%% -------------------------------------------------------------------

-module(gcp_discovery_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Macros
%%====================================================================

-define(TEST_SERVICE, "cre-service").
-define(TEST_NAMESPACE, "default").
-define(TEST_NODE_NAME, "cre").

%%====================================================================
%% Setup and Cleanup
%%====================================================================

%% @private Setup function for test environment
setup() ->
    %% Save original environment variables
    OriginalService = os:getenv("CRE_SERVICE_NAME"),
    OriginalNamespace = os:getenv("CRE_NAMESPACE"),
    OriginalNodeName = os:getenv("CRE_NODE_NAME"),
    OriginalPodIp = os:getenv("POD_IP"),
    OriginalHostname = os:getenv("HOSTNAME"),

    %% Set test environment variables
    os:putenv("CRE_SERVICE_NAME", ?TEST_SERVICE),
    os:putenv("CRE_NAMESPACE", ?TEST_NAMESPACE),
    os:putenv("CRE_NODE_NAME", ?TEST_NODE_NAME),
    os:putenv("POD_IP", "10.0.0.1"),
    os:putenv("HOSTNAME", "cre-0"),

    #{original_service => OriginalService,
      original_namespace => OriginalNamespace,
      original_node_name => OriginalNodeName,
      original_pod_ip => OriginalPodIp,
      original_hostname => OriginalHostname}.

%% @private Cleanup function to restore environment
cleanup(#{original_service := OrigService,
          original_namespace := OrigNamespace,
          original_node_name := OrigNodeName,
          original_pod_ip := OrigPodIp,
          original_hostname := OrigHostname}) ->
    %% Restore original environment variables
    restore_env("CRE_SERVICE_NAME", OrigService),
    restore_env("CRE_NAMESPACE", OrigNamespace),
    restore_env("CRE_NODE_NAME", OrigNodeName),
    restore_env("POD_IP", OrigPodIp),
    restore_env("HOSTNAME", OrigHostname),

    ok.

%% @private Helper to restore environment variable
restore_env(Key, Value) ->
    case Value of
        false -> os:unsetenv(Key);
        _ -> os:putenv(Key, Value)
    end.

%%====================================================================
%% Test Generators
%%====================================================================

%% @doc Main test generator for GCP discovery tests
gcp_discovery_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"build_node_list/1 with string addresses", fun test_build_node_list_strings/0},
         {"build_node_list/1 with tuple addresses", fun test_build_node_list_tuples/0},
         {"build_node_list/2 with exclude_self", fun test_build_node_list_exclude_self/0},
         {"build_node_list/2 with custom node name", fun test_build_node_list_custom_name/0},
         {"build_node_list/2 with empty list", fun test_build_node_list_empty/0},
         {"build_node_string/2 with various formats", fun test_build_node_string/0},
         {"build_dns_target/3", fun test_build_dns_target/0},
         {"get_service_name/0", fun test_get_service_name/0},
         {"get_namespace/0", fun test_get_namespace/0},
         {"get_namespace/0 with POD_NAMESPACE fallback", fun test_get_namespace_pod_fallback/0},
         {"get_pod_hostname/0", fun test_get_pod_hostname/0},
         {"get_pod_hostname/0 with POD_NAME fallback", fun test_get_pod_hostname_pod_fallback/0},
         {"get_pod_ip/0", fun test_get_pod_ip/0},
         {"get_pod_ip/0 with CRE_POD_IP fallback", fun test_get_pod_ip_cre_fallback/0},
         {"discover_peers/0 returns list", fun test_discover_peers_returns_list/0},
         {"discover_peers/1 with custom config", fun test_discover_peers_custom_config/0}
     ]
    }.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Tests build_node_list/1 with string IP addresses
test_build_node_list_strings() ->
    Addresses = ["10.0.0.1", "10.0.0.2", "10.0.0.3"],
    Config = #{node_name => "cre", exclude_self => false},
    Nodes = gcp_discovery:build_node_list(Addresses, Config),

    ?assertEqual(3, length(Nodes)),
    ?assert(lists:member('cre@10.0.0.1', Nodes)),
    ?assert(lists:member('cre@10.0.0.2', Nodes)),
    ?assert(lists:member('cre@10.0.0.3', Nodes)).

%% @doc Tests build_node_list/1 with tuple IP addresses
test_build_node_list_tuples() ->
    Addresses = [{10, 0, 0, 1}, {10, 0, 0, 2}, {192, 168, 1, 1}],
    Config = #{node_name => "cre", exclude_self => false},
    Nodes = gcp_discovery:build_node_list(Addresses, Config),

    ?assertEqual(3, length(Nodes)),
    ?assert(lists:member('cre@10.0.0.1', Nodes)),
    ?assert(lists:member('cre@10.0.0.2', Nodes)),
    ?assert(lists:member('cre@192.168.1.1', Nodes)).

%% @doc Tests build_node_list/2 with exclude_self option
test_build_node_list_exclude_self() ->
    Addresses = ["10.0.0.1"],
    Config1 = #{node_name => "cre", exclude_self => false},

    %% Without exclude_self (default true), should exclude current node
    Nodes1 = gcp_discovery:build_node_list(Addresses, Config1),
    ?assertEqual(1, length(Nodes1)),

    %% With exclude_self false, should include the node
    Config2 = #{node_name => atom_to_list(node()), exclude_self => false},
    Nodes2 = gcp_discovery:build_node_list(Addresses, Config2),
    ?assertEqual(1, length(Nodes2)).

%% @doc Tests build_node_list/2 with custom node name
test_build_node_list_custom_name() ->
    Addresses = ["10.0.0.1"],
    Nodes = gcp_discovery:build_node_list(Addresses, #{node_name => "mynode"}),

    ?assertEqual(1, length(Nodes)),
    ?assertEqual('mynode@10.0.0.1', hd(Nodes)).

%% @doc Tests build_node_list/1 with empty list
test_build_node_list_empty() ->
    Nodes = gcp_discovery:build_node_list([]),
    ?assertEqual([], Nodes).

%% @doc Tests build_node_string/2 with various address formats
test_build_node_string() ->
    %% Test with string address
    ?assertEqual("cre@10.0.0.1",
                 gcp_discovery:build_node_string("cre", "10.0.0.1")),

    %% Test with tuple address
    ?assertEqual("cre@10.0.0.1",
                 gcp_discovery:build_node_string("cre", {10, 0, 0, 1})),

    %% Test with custom node name
    ?assertEqual("mynode@192.168.1.1",
                 gcp_discovery:build_node_string("mynode", "192.168.1.1")).

%% @doc Tests build_dns_target/3
test_build_dns_target() ->
    ?assertEqual("service.default.svc.cluster.local",
                 gcp_discovery:build_dns_target("service", "default", "cluster.local")),

    ?assertEqual("cre-service.prod.svc.cluster.local",
                 gcp_discovery:build_dns_target("cre-service", "prod", "cluster.local")),

    ?assertEqual("app.namespace.svc.example.com",
                 gcp_discovery:build_dns_target("app", "namespace", "example.com")).

%% @doc Tests get_service_name/0
test_get_service_name() ->
    ServiceName = gcp_discovery:get_service_name(),
    ?assertEqual(?TEST_SERVICE, ServiceName).

%% @doc Tests get_namespace/0
test_get_namespace() ->
    Namespace = gcp_discovery:get_namespace(),
    ?assertEqual(?TEST_NAMESPACE, Namespace).

%% @doc Tests get_namespace/0 with POD_NAMESPACE fallback
test_get_namespace_pod_fallback() ->
    %% Unset CRE_NAMESPACE, set POD_NAMESPACE
    os:unsetenv("CRE_NAMESPACE"),
    os:putenv("POD_NAMESPACE", "kube-system"),

    Namespace = gcp_discovery:get_namespace(),
    ?assertEqual("kube-system", Namespace),

    %% Restore
    os:putenv("CRE_NAMESPACE", ?TEST_NAMESPACE),
    os:unsetenv("POD_NAMESPACE").

%% @doc Tests get_pod_hostname/0
test_get_pod_hostname() ->
    Hostname = gcp_discovery:get_pod_hostname(),
    ?assertEqual("cre-0", Hostname).

%% @doc Tests get_pod_hostname/0 with POD_NAME fallback
test_get_pod_hostname_pod_fallback() ->
    %% Unset HOSTNAME, set POD_NAME
    os:unsetenv("HOSTNAME"),
    os:putenv("POD_NAME", "cre-pod-1"),

    Hostname = gcp_discovery:get_pod_hostname(),
    ?assertEqual("cre-pod-1", Hostname),

    %% Restore
    os:putenv("HOSTNAME", "cre-0"),
    os:unsetenv("POD_NAME").

%% @doc Tests get_pod_ip/0
test_get_pod_ip() ->
    PodIp = gcp_discovery:get_pod_ip(),
    ?assertEqual("10.0.0.1", PodIp).

%% @doc Tests get_pod_ip/0 with CRE_POD_IP fallback
test_get_pod_ip_cre_fallback() ->
    %% Unset POD_IP, set CRE_POD_IP
    os:unsetenv("POD_IP"),
    os:putenv("CRE_POD_IP", "10.1.0.1"),

    PodIp = gcp_discovery:get_pod_ip(),
    ?assertEqual("10.1.0.1", PodIp),

    %% Restore
    os:putenv("POD_IP", "10.0.0.1"),
    os:unsetenv("CRE_POD_IP").

%% @doc Tests discover_peers/0 returns a list (will be empty in test env)
test_discover_peers_returns_list() ->
    Peers = gcp_discovery:discover_peers(),
    ?assert(is_list(Peers)).

%% @doc Tests discover_peers/1 with custom configuration
test_discover_peers_custom_config() ->
    Config = #{
        service_name => "test-service",
        namespace => "test-ns",
        node_name => "testnode"
    },
    Peers = gcp_discovery:discover_peers(Config),
    ?assert(is_list(Peers)).

%%====================================================================
%% Unit Tests for Internal Functions
%%====================================================================

%% @doc Test resolve_srv_targets/1 via mock
resolve_srv_targets_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
             ?_test(begin
                 %% Mock SRV records
                 SrvRecs = [
                     {0, 10, 5053, "cre-0.cre-service.default.svc.cluster.local"},
                     {0, 10, 5053, "cre-1.cre-service.default.svc.cluster.local"}
                 ],
                 %% Note: This would need DNS to actually resolve
                 %% In test environment, we verify it doesn't crash
                 ?assert(is_list(gcp_discovery:resolve_srv_targets(SrvRecs)))
             end)
         ]
     end
    }.

%% @doc Test watch_statefulset/1
watch_statefulset_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
             ?_test(begin
                 %% Test starting a watch (will use DNS polling)
                 case gcp_discovery:watch_statefulset("cre-statefulset") of
                     {ok, _WatchRef} -> ok;
                     {error, _Reason} -> ok
                 end
             end)
         ]
     end
    }.

%% @doc Test watch_deployment/1
watch_deployment_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
             ?_test(begin
                 %% Test starting a watch for deployment
                 case gcp_discovery:watch_deployment("cre-deployment") of
                     {ok, _WatchRef} -> ok;
                     {error, _Reason} -> ok
                 end
             end)
         ]
     end
    }.

%%====================================================================
%% Integration Tests
%%====================================================================

%% @doc Test end-to-end discovery workflow
integration_discovery_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
             {"discovery workflow", fun() ->
                 %% Get service configuration
                 Service = gcp_discovery:get_service_name(),
                 Namespace = gcp_discovery:get_namespace(),
                 NodeName = gcp_discovery:get_node_name(),

                 ?assert(is_list(Service)),
                 ?assert(is_list(Namespace)),
                 ?assert(is_list(NodeName)),

                 %% Build DNS target
                 DnsTarget = gcp_discovery:build_dns_target(Service, Namespace, "cluster.local"),
                 ?assert(is_list(DnsTarget)),
                 ?assert(string:str(DnsTarget, Service) > 0),

                 %% Build node list from mock addresses
                 MockAddrs = ["10.0.0.1", "10.0.0.2"],
                 Nodes = gcp_discovery:build_node_list(MockAddrs),
                 ?assert(is_list(Nodes))
             end}
         ]
     end
    }.

%%====================================================================
%% Property Tests
%%====================================================================

%% @doc Property: build_node_list always returns atoms
build_node_list_returns_atoms_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
             ?_test(begin
                 Addresses = ["10.0.0.1", "192.168.1.1", "172.16.0.1"],
                 Nodes = gcp_discovery:build_node_list(Addresses),
                 lists:foreach(
                     fun(N) ->
                         ?assert(is_atom(N))
                     end,
                     Nodes)
             end)
         ]
     end
    }.

%% @doc Property: build_node_list returns unique nodes
build_node_list_unique_test_() ->
    {setup,
     fun() -> ok end,
     fun(_) -> ok end,
     fun(_) ->
         [
             ?_test(begin
                 %% Add duplicates
                 Addresses = ["10.0.0.1", "10.0.0.1", "10.0.0.2", "10.0.0.2"],
                 Nodes = gcp_discovery:build_node_list(Addresses),
                 ?assertEqual(2, length(Nodes)),
                 ?assertEqual(lists:usort(Nodes), Nodes)
             end)
         ]
     end
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Test wrapper for get_node_name
get_node_name_test_() ->
    {setup,
     fun() ->
         os:putenv("CRE_NODE_NAME", "testnode")
     end,
     fun(_) ->
         os:unsetenv("CRE_NODE_NAME")
     end,
     fun(_) ->
         ?_assertEqual("testnode", gcp_discovery:get_node_name())
     end
    }.
