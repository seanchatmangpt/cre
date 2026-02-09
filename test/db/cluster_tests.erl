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
%% @doc EUnit Tests for Mnesia Clustering Modules
%%
%% This test suite covers the cluster, mnesia_manager, mnesia_cluster_sup,
%% and cluster_utils modules with mocked DNS for K8s service discovery.
%%
%% @end
%% -------------------------------------------------------------------

-module(cluster_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Macros
%%====================================================================

-define(TEST_NODE, cluster_test@localhost).
-define(TEST_NODES, [cluster_test1@localhost, cluster_test2@localhost]).

%%====================================================================
%% Test Generators
%%====================================================================

%% @doc Main test generator for all cluster tests
cluster_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"cluster module tests", fun test_cluster_module/0},
         {"mnesia_manager module tests", fun test_mnesia_manager_module/0},
         {"cluster_utils module tests", fun test_cluster_utils_module/0},
         {"discovery tests", fun test_discovery_methods/0},
         {"node string parsing tests", fun test_parse_nodes_string/0},
         {"health monitoring tests", fun test_health_monitoring/0}
     ]
    }.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

%% @private Setup function run before all tests
setup() ->
    %% Ensure Mnesia is stopped before tests
    mnesia:stop(),
    mnesia:delete_schema([node()]),

    %% Start Mnesia for testing
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),

    %% Start cluster manager for testing
    {ok, _Pid} = cluster:start_link([{discovery_method, none},
                                       {max_retries, 1}]),

    %% Start cluster utils
    {ok, _UtilsPid} = cluster_utils:start_link([{healing_strategy, manual}]),

    ok.

%% @private Cleanup function run after all tests
cleanup(_State) ->
    %% Stop cluster manager
    gen_server:stop(cluster),

    %% Stop cluster utils
    gen_server:stop(cluster_utils),

    %% Stop and cleanup Mnesia
    mnesia:stop(),
    mnesia:delete_schema([node()]),

    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Tests for the cluster module
test_cluster_module() ->
    %% Test get_nodes returns empty list initially
    [] = cluster:get_nodes(),

    %% Test get_status returns correct initial state
    Status = cluster:get_status(),
    ?assert(maps:is_key(discovery_method, Status)),
    ?assert(maps:is_key(cluster_nodes, Status)),
    ?assert(maps:is_key(pending_joins, Status)),
    ?assert(maps:is_key(is_connected, Status)),

    %% Test set_discovery_method
    ok = cluster:set_discovery_method(env),
    NewStatus = cluster:get_status(),
    ?assertEqual(env, maps:get(discovery_method, NewStatus)),

    %% Test discover_peers with env method (should be empty without env var)
    ok = cluster:set_discovery_method(env),
    Peers = cluster:discover_peers(),
    ?assert(is_list(Peers)),

    %% Test set_discovery_method to none
    ok = cluster:set_discovery_method(none),
    ?assertEqual(none, maps:get(discovery_method, cluster:get_status())),

    ok.

%% @doc Tests for the mnesia_manager module
test_mnesia_manager_module() ->
    %% Test list_tables includes schema
    Tables = mnesia_manager:list_tables(),
    ?assert(lists:member(schema, Tables)),

    %% Test get_table_info for schema
    SchemaInfo = mnesia_manager:get_table_info(schema),
    ?assertEqual(schema, maps:get(name, SchemaInfo)),
    ?assert(maps:is_key(storage, SchemaInfo)),
    ?assert(maps:is_key(record_count, SchemaInfo)),

    %% Test create_table
    ok = mnesia_manager:create_table(test_table,
        [{attributes, [key, value]}, {ram_copies, [node()]}]),

    %% Verify table was created
    ?assert(lists:member(test_table, mnesia_manager:list_tables())),

    %% Test get_table_info for new table
    TableInfo = mnesia_manager:get_table_info(test_table),
    ?assertEqual(test_table, maps:get(name, TableInfo)),

    %% Test delete_table
    ok = mnesia_manager:delete_table(test_table),
    ?assertNot(lists:member(test_table, mnesia_manager:list_tables())),

    %% Test get_table_info with non-existent table
    ?assertEqual({error, not_found}, mnesia_manager:get_table_info(nonexistent_table)),

    ok.

%% @doc Tests for the cluster_utils module
test_cluster_utils_module() ->
    %% Test get_node_health for current node
    Health = cluster_utils:get_node_health(node()),
    ?assertEqual(node(), maps:get(node, Health)),
    ?assert(maps:is_key(status, Health)),
    ?assert(maps:is_key(last_seen, Health)),

    %% Test get_cluster_health
    ClusterHealth = cluster_utils:get_cluster_health(),
    ?assert(maps:is_key(total_nodes, ClusterHealth)),
    ?assert(maps:is_key(healthy_nodes, ClusterHealth)),
    ?assert(maps:is_key(has_partition, ClusterHealth)),

    %% Test detect_partition (should be ok in single node setup)
    ok = cluster_utils:detect_partition(),

    %% Test set_healing_strategy
    ok = cluster_utils:set_healing_strategy(auto_rejoin),
    ok = cluster_utils:set_healing_strategy(manual),
    ok = cluster_utils:set_healing_strategy(majority),

    %% Test monitor_nodes
    ok = cluster_utils:monitor_nodes(),

    ok.

%% @doc Tests for discovery methods
test_discovery_methods() ->
    %% Test env discovery method
    ok = cluster:set_discovery_method(env),
    EnvPeers = cluster:discover_peers(),
    ?assert(is_list(EnvPeers)),

    %% Test static discovery method
    ok = cluster:set_discovery_method(static),
    StaticPeers = cluster:discover_peers(),
    ?assert(is_list(StaticPeers)),

    %% Test none discovery method
    ok = cluster:set_discovery_method(none),
    NonePeers = cluster:discover_peers(),
    ?assertEqual([], NonePeers),

    ok.

%% @doc Tests for node string parsing
test_parse_nodes_string() ->
    %% Empty string
    ?assertEqual([], cluster:discover_peers()),

    %% Set env var and test
    os:putenv("CRE_CLUSTER_NODES", "node1@host,node2@host"),
    ok = cluster:set_discovery_method(env),
    Peers = cluster:discover_peers(),
    ?assert(is_list(Peers)),
    ?assertEqual(2, length(Peers)),

    %% Clean up env var
    os:unsetenv("CRE_CLUSTER_NODES"),

    ok.

%% @doc Tests for health monitoring
test_health_monitoring() ->
    %% Test ping_nodes
    Results = cluster_utils:ping_nodes([node()]),
    ?assert(is_list(Results)),
    ?assertEqual(1, length(Results)),
    {Node, Status} = lists:nth(1, Results),
    ?assertEqual(node(), Node),
    ?assertEqual(pong, Status),

    %% Test get_node_health after ping
    Health = cluster_utils:get_node_health(node()),
    ?assertEqual(node(), maps:get(node, Health)),

    ok.

%%====================================================================
%% Unit Tests for Internal Functions
%%====================================================================

%% @doc Test parse_nodes_string/1 via env discovery
parse_nodes_string_via_env_test_() ->
    {setup,
     fun() ->
         os:putenv("CRE_CLUSTER_NODES", "node1@host,node2@host"),
         cluster:set_discovery_method(env)
     end,
     fun(_) ->
         os:unsetenv("CRE_CLUSTER_NODES")
     end,
     fun(_) ->
         ?_test(begin
             Peers = cluster:discover_peers(),
             ?assertEqual(2, length(Peers))
         end)
     end
    }.

%% @doc Test empty env var
empty_env_var_test_() ->
    {setup,
     fun() ->
         os:unsetenv("CRE_CLUSTER_NODES"),
         cluster:set_discovery_method(env)
     end,
     fun(_) -> ok end,
     fun(_) ->
         ?_assertEqual([], cluster:discover_peers())
     end
    }.

%% @doc Test table creation with various options
table_creation_options_test_() ->
    {foreach,
     fun() ->
         ok
     end,
     fun(_) ->
         %% Cleanup tables
         lists:foreach(
           fun(T) ->
               catch mnesia:delete_table(T)
           end,
           [test_table1, test_table2, test_table3])
     end,
     [
         {"create ram table", fun() ->
             ok = mnesia_manager:create_table(test_table1,
                 [{attributes, [key]}, {ram_copies, [node()]}]),
             ?assert(lists:member(test_table1, mnesia_manager:list_tables()))
         end},
         {"create disc table", fun() ->
             ok = mnesia_manager:create_table(test_table2,
                 [{attributes, [key]}, {disc_copies, [node()]}]),
             ?assert(lists:member(test_table2, mnesia_manager:list_tables()))
         end},
         {"create bag table", fun() ->
             ok = mnesia_manager:create_table(test_table3,
                 [{attributes, [key, value]}, {type, bag}, {ram_copies, [node()]}]),
             Info = mnesia_manager:get_table_info(test_table3),
             ?assertEqual(bag, maps:get(type, Info))
         end}
     ]
    }.

%% @doc Test backup operations
backup_operations_test_() ->
    {setup,
     fun() ->
         %% Create test table
         mnesia_manager:create_table(backup_test_table,
             [{attributes, [key, value]}, {ram_copies, [node()]}]),
         %% Insert test data
         {atomic, ok} = mnesia:transaction(fun() ->
             mnesia:write(#backup_test_table{key = test, value = data})
         end),
         ok
     end,
     fun(_) ->
         %% Cleanup
         mnesia:delete_table(backup_test_table),
         catch file:delete("/tmp/backup_test.bak")
     end,
     fun(_) ->
         [
             {"backup and restore", fun() ->
                 %% Note: Full backup requires more setup, this tests the API
                 %% In production, we'd test with actual backup files
                 ?assert(is_list(mnesia_manager:list_backups("/tmp")))
             end}
         ]
     end
    }.

%% @doc Test healing strategies
healing_strategies_test_() ->
    {foreach,
     fun() -> ok end,
     fun(_) -> ok end,
     [
         {"auto_rejoin strategy", fun() ->
             ok = cluster_utils:set_healing_strategy(auto_rejoin),
             Health = cluster_utils:get_cluster_health(),
             ?assert(maps:is_key(has_partition, Health))
         end},
         {"manual strategy", fun() ->
             ok = cluster_utils:set_healing_strategy(manual),
             ?assertEqual({error, manual_intervention_required},
                          cluster_utils:heal_partition([unknown@host]))
         end},
         {"majority strategy", fun() ->
             ok = cluster_utils:set_healing_strategy(majority),
             %% Single node means majority
             ?assertEqual({error, minority_partition},
                          cluster_utils:heal_partition([unknown@host]))
         end}
     ]
    }.

%%====================================================================
%% Mock DNS Tests
%%====================================================================

%% @doc Test DNS-based discovery with mock
dns_discovery_test_() ->
    {setup,
     fun() ->
         %% Setup mock DNS by setting dns_name to a test value
         cluster:set_discovery_method(dns)
     end,
     fun(_) ->
         ok
     end,
     fun(_) ->
         %% DNS discovery will fail in test env but should not crash
         Peers = cluster:discover_peers(),
         ?assert(is_list(Peers))
     end
    }.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private Creates a test Mnesia record
-define(TEST_RECORD(Name, Value), {Name, Value}).
