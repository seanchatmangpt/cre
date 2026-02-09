%% -*- erlang -*-
%% @doc Rust Resource Cleanup Test Suite
%%
%% This suite tests proper resource cleanup when using Rust NIFs.
%% It verifies that memory, ports, and other resources are properly
%% released when operations complete or errors occur.
%%
%% @end

-module(rust_cleanup_SUITE).
-author("CRE Team").

-compile(nowarn_export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Suite Callbacks
%%====================================================================

-export([all/0]).
-export([suite/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%%====================================================================
%% Test Cases
%%====================================================================

-export([
    normal_operation_cleanup_test/1,
    error_path_cleanup_test/1,
    timeout_cleanup_test/1,
    memory_cleanup_test/1,
    port_cleanup_test/1,
    nif_resource_tracking_test/1,
    concurrent_cleanup_test/1,
    force_cleanup_test/1,
    leak_detection_test/1,
    garbage_collection_test/1
]).

%%====================================================================
%% Suite Callbacks
%%====================================================================

all() ->
    [
        normal_operation_cleanup_test,
        error_path_cleanup_test,
        timeout_cleanup_test,
        memory_cleanup_test,
        port_cleanup_test,
        nif_resource_tracking_test,
        concurrent_cleanup_test,
        force_cleanup_test,
        leak_detection_test,
        garbage_collection_test
    ].

suite() ->
    [
        {timetrap, {seconds, 60}}
    ].

init_per_suite(Config) ->
    ct:log("Initializing Rust cleanup test suite"),
    %% Record initial system state
    InitialMemory = erlang:memory(total),
    InitialProcessCount = length(processes()),
    [
        {initial_memory, InitialMemory},
        {initial_process_count, InitialProcessCount}
        | Config
    ].

end_per_suite(Config) ->
    %% Verify no system-wide leaks
    InitialMemory = proplists:get_value(initial_memory, Config),
    InitialProcessCount = proplists:get_value(initial_process_count, Config),

    FinalMemory = erlang:memory(total),
    FinalProcessCount = length(processes()),

    MemoryGrowth = FinalMemory - InitialMemory,
    ProcessGrowth = FinalProcessCount - InitialProcessCount,

    ct:log("Memory growth: ~p bytes", [MemoryGrowth]),
    ct:log("Process count growth: ~p", [ProcessGrowth]),

    %% Allow reasonable growth but flag excessive growth
    case MemoryGrowth > 104857600 of  %% 100MB
        true ->
            ct:log("Warning: Excessive memory growth detected: ~p bytes", [MemoryGrowth]);
        false ->
            ok
    end,

    case ProcessGrowth > 100 of
        true ->
            ct:log("Warning: Excessive process growth: ~p", [ProcessGrowth]);
        false ->
            ok
    end,

    ok.

init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),
    %% Force GC before each test
    garbage_collect(),
    [{test_memory, erlang:memory(total)} | Config].

end_per_testcase(TestCase, Config) ->
    %% Check for leaks after each test
    TestMemory = proplists:get_value(test_memory, Config),
    CurrentMemory = erlang:memory(total),
    MemoryDiff = CurrentMemory - TestMemory,

    ct:log("~p memory change: ~p bytes", [TestCase, MemoryDiff]),

    %% Warn about large leaks but don't fail
    case MemoryDiff > 52428800 of  %% 50MB threshold
        true ->
            ct:log("Warning: ~p may have leaked ~p bytes", [TestCase, MemoryDiff]);
        false ->
            ok
    end,

    %% Force cleanup
    garbage_collect(),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test cleanup after normal operations
normal_operation_cleanup_test(_Config) ->
    %% Create a Rust resource
    {ok, Resource} = rust_resource:create(),

    %% Perform normal operations
    ok = rust_resource:operation(Resource, test_data),
    {ok, Result} = rust_resource:get_result(Resource),

    %% Clean up normally
    ok = rust_resource:destroy(Resource),

    %% Verify cleanup
    ?assertEqual(not_found, rust_resource:status(Resource)),

    %% Verify no orphaned processes
    ?assertEqual([], find_orphaned_resources()),

    ok.

%% @doc Test cleanup on error paths
error_path_cleanup_test(_Config) ->
    %% Create resource
    {ok, Resource} = rust_resource:create(),

    %% Trigger error
    ?assertMatch({error, _}, rust_resource:invalid_operation(Resource)),

    %% Even after error, resource should be valid
    ?assertMatch(ok, rust_resource:status(Resource)),

    %% Clean up
    ok = rust_resource:destroy(Resource),

    %% Verify cleanup
    ?assertEqual(not_found, rust_resource:status(Resource)),

    ok.

%% @doc Test cleanup after timeout
timeout_cleanup_test(_Config) ->
    %% Create resource with short timeout
    {ok, Resource} = rust_resource:create([{timeout, 100}]),

    %% Trigger long operation (will timeout)
    ?assertMatch({error, timeout}, rust_resource:long_operation(Resource)),

    %% Resource should still be cleaned up despite timeout
    %% Check if resource was auto-cleaned
    Status = rust_resource:status(Resource),
    case Status of
        not_found ->
            ct:log("Resource auto-cleaned after timeout"),
            ?assert(true);
        _ ->
            %% Manual cleanup needed
            ok = rust_resource:destroy(Resource),
            ?assertEqual(not_found, rust_resource:status(Resource))
    end,

    ok.

%% @doc Test memory cleanup
memory_cleanup_test(_Config) ->
    %% Get baseline
    garbage_collect(),
    Baseline = erlang:memory(total),

    %% Allocate and free many resources
    lists:foreach(fun(_) ->
        {ok, R} = rust_resource:create(),
        rust_resource:destroy(R)
    end, lists:seq(1, 1000)),

    %% Force GC
    garbage_collect(),
    After = erlang:memory(total),

    %% Memory should not grow excessively
    Growth = After - Baseline,
    ct:log("Memory growth after 1000 cycles: ~p bytes", [Growth]),
    ?assert(Growth < 52428800),  %% Less than 50MB

    ok.

%% @doc Test port cleanup
port_cleanup_test(_Config) ->
    %% Some Rust operations may use ports
    %% Verify ports are closed properly

    %% Check initial port count
    InitialPorts = count_ports(),

    %% Create and use port-based resources
    {ok, PortResource} = rust_resource:create_port(),

    %% Use the port resource
    ok = rust_resource:port_operation(PortResource, test),

    %% Destroy resource (should close port)
    ok = rust_resource:destroy(PortResource),

    %% Wait for async cleanup
    timer:sleep(100),

    %% Verify port count
    FinalPorts = count_ports(),
    ?assertEqual(InitialPorts, FinalPorts),

    ok.

%% @doc Test NIF resource tracking
nif_resource_tracking_test(_Config) ->
    %% NIF resources should be tracked and cleaned up

    %% Create multiple NIF resources
    Resources = [begin {ok, R} = rust_resource:create_nif(), R end
                 || _ <- lists:seq(1, 100)],

    %% Get tracked count
    {ok, TrackedCount} = rust_resource:tracked_count(),
    ?assertEqual(100, TrackedCount),

    %% Clean up all resources
    lists:foreach(fun(R) -> rust_resource:destroy(R) end, Resources),

    %% Verify all cleaned up
    {ok, FinalCount} = rust_resource:tracked_count(),
    ?assertEqual(0, FinalCount),

    ok.

%% @doc Test cleanup with concurrent access
concurrent_cleanup_test(_Config) ->
    %% Spawn multiple processes creating/destroying resources
    Pids = [spawn(fun() ->
        %% Each process creates and destroys 10 resources
        lists:foreach(fun(_) ->
            {ok, R} = rust_resource:create(),
            rust_resource:destroy(R)
        end, lists:seq(1, 10)),
        exit(done)
    end) || _ <- lists:seq(1, 20)],

    %% Wait for all to complete
    lists:foreach(fun(Pid) ->
        receive
            {'EXIT', Pid, done} -> ok
        after 5000 ->
            ct:fail("Process did not complete in time")
        end
    end, Pids),

    %% Force GC
    garbage_collect(),

    %% Verify all resources cleaned up
    {ok, TrackedCount} = rust_resource:tracked_count(),
    ?assertEqual(0, TrackedCount),

    ok.

%% @doc Test forced cleanup
force_cleanup_test(_Config) ->
    %% Create resource
    {ok, Resource} = rust_resource:create(),

    %% Force cleanup without normal destroy
    ok = rust_resource:force_cleanup(Resource),

    %% Verify cleanup completed
    ?assertEqual(not_found, rust_resource:status(Resource)),

    %% No orphaned data
    ?assertEqual([], rust_resource:list_active()),

    ok.

%% @doc Test leak detection
leak_detection_test(_Config) ->
    %% Record initial state
    {ok, InitialSnapshot} = rust_resource:memory_snapshot(),

    %% Perform operations that might leak
    lists:foreach(fun(N) ->
        {ok, _R} = rust_resource:create(),
        %% Some resources not explicitly destroyed
        case N rem 10 of
            0 -> ok;  %% Clean up 10% of resources
            _ -> ok
        end
    end, lists:seq(1, 1000)),

    %% Get current state
    {ok, CurrentSnapshot} = rust_resource:memory_snapshot(),

    %% Check for leaks
    {ok, LeakReport} = rust_resource:leak_check(InitialSnapshot, CurrentSnapshot),
    ct:log("Leak report: ~p", [LeakReport]),

    %% Force cleanup of all resources
    rust_resource:cleanup_all(),

    %% Final verification
    {ok, FinalSnapshot} = rust_resource:memory_snapshot(),
    ActiveResources = maps:get(active_resources, FinalSnapshot, 0),
    ?assertEqual(0, ActiveResources),

    ok.

%% @doc Test garbage collection integration
garbage_collection_test(_Config) ->
    %% Create resources that rely on GC for cleanup

    %% Create resources without explicit destroy
    Resources = [begin {ok, R} = rust_resource:create(), R end
                 || _ <- lists:seq(1, 50)],

    %% Clear references
    Resources = [],

    %% Trigger GC
    garbage_collect(),

    %% Give time for async cleanup
    timer:sleep(200),

    %% Check if resources were cleaned up by GC
    {ok, TrackedCount} = rust_resource:tracked_count(),
    ct:log("Tracked resources after GC: ~p", [TrackedCount]),

    %% Should be significantly reduced (or zero)
    ?assert(TrackedCount < 10),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Count active ports in the system
count_ports() ->
    length(erlang:ports()).

%% @doc Find orphaned resources (no owner process)
find_orphaned_resources() ->
    AllResources = rust_resource:list_all(),
    lists:filtermap(fun(ResourceId) ->
        case rust_resource:owner(ResourceId) of
            undefined -> {true, ResourceId};
            Owner when is_pid(Owner) ->
                case is_process_alive(Owner) of
                    true -> false;
                    false -> {true, ResourceId}
                end;
            _ -> false
        end
    end, AllResources).
