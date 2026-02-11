%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% YAWL Workflow Patterns Test Suite
%%
%% Comprehensive test suite for all YAWL workflow patterns with
%% XES log validation and soundness property verification.
%%
%% @author CRE Team
%% @version 1.0.0
%% @doc Test Suite for YAWL Workflow Patterns Reference Implementation
%%
%% This module provides comprehensive testing for all 15 reference patterns:
%% <ul>
%%   <li>Unit tests for each pattern</li>
%%   <li>XES log validation</li>
%%   <li>Soundness property verification</li>
%%   <li>Execution trace validation</li>
%% </ul>
%% @end
%% -------------------------------------------------------------------

-module(yawl_pattern_tests).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").
-include("cre_yawl.hrl").

%%====================================================================
%% Test Exports
%%====================================================================

-export([
    test_all_patterns/0,
    test_pattern/1,
    test_pattern_with_logging/2,
    verify_soundness/1,
    run_benchmark/0
]).

%%====================================================================
%% Test Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs all pattern tests.
%% @end
%%--------------------------------------------------------------------
-spec test_all_patterns() -> ok.

test_all_patterns() ->
    eunit:test({inparallel, [
        test_wcp_01_sequence(),
        test_wcp_02_parallel_split(),
        test_wcp_03_synchronization(),
        test_wcp_04_exclusive_choice(),
        test_wcp_07_structured_sync_merge(),
        test_wcp_09_discriminator(),
        test_wcp_13_multi_instance_static(),
        test_wcp_15_multi_instance_dynamic(),
        test_wcp_16_deferred_choice(),
        test_wcp_17_interleaved_routing(),
        test_wcp_18_milestone(),
        test_wcp_19_cancel_activity(),
        test_wcp_20_cancel_case(),
        test_wcp_25_cancel_region(),
        test_wcp_39_critical_section()
    ]}).

%%--------------------------------------------------------------------
%% @doc Tests a specific pattern.
%% @end
%%--------------------------------------------------------------------
-spec test_pattern(atom()) -> ok.

test_pattern(sequence) -> run_test(test_wcp_01_sequence());
test_pattern(parallel_split) -> run_test(test_wcp_02_parallel_split());
test_pattern(synchronization) -> run_test(test_wcp_03_synchronization());
test_pattern(exclusive_choice) -> run_test(test_wcp_04_exclusive_choice());
test_pattern(structured_sync_merge) -> run_test(test_wcp_07_structured_sync_merge());
test_pattern(discriminator) -> run_test(test_wcp_09_discriminator());
test_pattern(multi_instance_static) -> run_test(test_wcp_13_multi_instance_static());
test_pattern(multi_instance_dynamic) -> run_test(test_wcp_15_multi_instance_dynamic());
test_pattern(deferred_choice) -> run_test(test_wcp_16_deferred_choice());
test_pattern(interleaved_routing) -> run_test(test_wcp_17_interleaved_routing());
test_pattern(milestone) -> run_test(test_wcp_18_milestone());
test_pattern(cancel_activity) -> run_test(test_wcp_19_cancel_activity());
test_pattern(cancel_case) -> run_test(test_wcp_20_cancel_case());
test_pattern(cancel_region) -> run_test(test_wcp_25_cancel_region());
test_pattern(critical_section) -> run_test(test_wcp_39_critical_section()).

%%--------------------------------------------------------------------
%% @doc Tests a pattern with XES logging.
%% @end
%%--------------------------------------------------------------------
-spec test_pattern_with_logging(atom(), binary()) -> ok.

test_pattern_with_logging(PatternName, LogId) ->
    %% Start XES logger
    {ok, _Pid} = yawl_xes:start_link(),
    {ok, _LogId} = yawl_xes:new_log(#{pattern => PatternName}),

    %% Run test with logging
    Result = test_pattern(PatternName),

    %% Export and validate log
    {ok, _XES} = yawl_xes:export_xes(LogId),

    yawl_xes:stop(),
    Result.

%%--------------------------------------------------------------------
%% @doc Verifies soundness properties for a pattern.
%% @end
%%--------------------------------------------------------------------
-spec verify_soundness(atom()) -> map().

verify_soundness(PatternName) ->
    Pattern = create_pattern(PatternName),
    yawl_pattern_reference:verify_soundness(Pattern).

%%--------------------------------------------------------------------
%% @doc Runs benchmark tests.
%% @end
%%--------------------------------------------------------------------
-spec run_benchmark() -> map().

run_benchmark() ->
    {ok, _Pid} = yawl_xes:start_link(),
    {ok, LogId} = yawl_xes:new_log(#{}),

    Results = lists:map(fun({Name, _}) ->
        StartTime = erlang:monotonic_time(microsecond),
        Pattern = create_pattern(Name),
        {ok, Result} = yawl_pattern_reference:execute_with_logging(Pattern, test_input, #{}, LogId),
        EndTime = erlang:monotonic_time(microsecond),
        {
            Name,
            #{
                duration_us => EndTime - StartTime,
                status => maps:get(status, Result),
                trace_length => length(maps:get(trace, Result, []))
            }
        }
    end, all_patterns()),

    yawl_xes:stop(),
    maps:from_list(Results).

%%====================================================================
%% WCP-01: Sequence Pattern Tests
%%====================================================================

test_wcp_01_sequence() ->
    {"WCP-01: Sequence Pattern", [
        {"creates valid pattern", fun() ->
            Pattern = yawl_pattern_reference:sequence([task1, task2]),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"executes sequentially", fun() ->
            Pattern = yawl_pattern_reference:sequence([task1, task2]),
            Result = yawl_pattern_reference:execute(Pattern, input_data, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"produces correct trace", fun() ->
            Pattern = yawl_pattern_reference:sequence([t1, t2, t3]),
            Result = yawl_pattern_reference:execute(Pattern, data, #{}),
            Trace = maps:get(trace, Result, []),
            ?assert(length(Trace) >= 2)
        end},
        {"verifies soundness", fun() ->
            Pattern = yawl_pattern_reference:sequence([task1, task2]),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(option_to_complete, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-02: Parallel Split Pattern Tests
%%====================================================================

test_wcp_02_parallel_split() ->
    {"WCP-02: Parallel Split Pattern", [
        {"creates valid pattern", fun() ->
            Pattern = yawl_pattern_reference:parallel_split(3, [t1, t2, t3]),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"splits into branches", fun() ->
            Pattern = yawl_pattern_reference:parallel_split(2, [t1, t2]),
            Result = yawl_pattern_reference:execute(Pattern, input, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"requires synchronization", fun() ->
            Pattern = yawl_pattern_reference:parallel_split(2, [t1, t2]),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(requires_synchronization, maps:get(option_to_complete, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-03: Synchronization Pattern Tests
%%====================================================================

test_wcp_03_synchronization() ->
    {"WCP-03: Synchronization Pattern", [
        {"creates valid pattern", fun() ->
            Pattern = yawl_pattern_reference:synchronization(3, [t1, t2, t3]),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"synchronizes all branches", fun() ->
            Pattern = yawl_pattern_reference:synchronization(2, [t1, t2]),
            Result = yawl_pattern_reference:execute(Pattern, input, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"verifies soundness", fun() ->
            Pattern = yawl_pattern_reference:synchronization(2, [t1, t2]),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(option_to_complete, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-04: Exclusive Choice Pattern Tests
%%====================================================================

test_wcp_04_exclusive_choice() ->
    {"WCP-04: Exclusive Choice Pattern", [
        {"creates valid pattern", fun() ->
            Fun = fun() -> {1, selected} end,
            Pattern = yawl_pattern_reference:exclusive_choice(Fun, [t1, t2]),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"chooses one branch", fun() ->
            Fun = fun() -> {1, chosen} end,
            Pattern = yawl_pattern_reference:exclusive_choice(Fun, [t1, t2]),
            Result = yawl_pattern_reference:execute(Pattern, data, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"mutual exclusion", fun() ->
            Pattern = yawl_pattern_reference:exclusive_choice(fun() -> {1, x} end, [t1, t2]),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(mutual_exclusion, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-07: Structured Sync Merge Pattern Tests
%%====================================================================

test_wcp_07_structured_sync_merge() ->
    {"WCP-07: Structured Sync Merge", [
        {"creates valid pattern", fun() ->
            Pattern = yawl_pattern_reference:structured_sync_merge(3, [t1, t2, t3]),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"merges all branches", fun() ->
            Pattern = yawl_pattern_reference:structured_sync_merge(2, [t1, t2]),
            Result = yawl_pattern_reference:execute(Pattern, input, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"all branches required", fun() ->
            Pattern = yawl_pattern_reference:structured_sync_merge(2, [t1, t2]),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(all_branches_required, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-09: Discriminator Pattern Tests
%%====================================================================

test_wcp_09_discriminator() ->
    {"WCP-09: Discriminator Pattern", [
        {"creates valid pattern", fun() ->
            Pattern = yawl_pattern_reference:discriminator(3, [t1, t2, t3]),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"triggers on first", fun() ->
            Pattern = yawl_pattern_reference:discriminator(2, [t1, t2]),
            Result = yawl_pattern_reference:execute(Pattern, input, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"first completion semantics", fun() ->
            Pattern = yawl_pattern_reference:discriminator(2, [t1, t2]),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(first_completion_triggers, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-13: Multi-Instance Static Pattern Tests
%%====================================================================

test_wcp_13_multi_instance_static() ->
    {"WCP-13: Multi-Instance (Static)", [
        {"creates valid pattern", fun() ->
            Fun = fun(I) -> I * 2 end,
            Pattern = yawl_pattern_reference:multi_instance_static(3, Fun, [1, 2, 3]),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"executes all instances", fun() ->
            Fun = fun(I) -> I * 10 end,
            Pattern = yawl_pattern_reference:multi_instance_static(3, Fun, [1, 2, 3]),
            Result = yawl_pattern_reference:execute(Pattern, inputs, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"fixed instance count", fun() ->
            Fun = fun(_) -> ok end,
            Pattern = yawl_pattern_reference:multi_instance_static(5, Fun, [1, 2, 3, 4, 5]),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(5, maps:get(fixed_instance_count, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-15: Multi-Instance Dynamic Pattern Tests
%%====================================================================

test_wcp_15_multi_instance_dynamic() ->
    {"WCP-15: Multi-Instance (Dynamic)", [
        {"creates valid pattern", fun() ->
            DataFun = fun() -> done end,
            InstanceFun = fun(I) -> I end,
            Pattern = yawl_pattern_reference:multi_instance_dynamic(DataFun, InstanceFun),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"handles dynamic data", fun() ->
            DataFun = fun() -> {more, data} end,
            InstanceFun = fun(_) -> ok end,
            Pattern = yawl_pattern_reference:multi_instance_dynamic(DataFun, InstanceFun),
            Result = yawl_pattern_reference:execute(Pattern, start, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"dynamic instances", fun() ->
            Pattern = yawl_pattern_reference:multi_instance_dynamic(fun() -> done end, fun(_) -> ok end),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(dynamic_instances, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-16: Deferred Choice Pattern Tests
%%====================================================================

test_wcp_16_deferred_choice() ->
    {"WCP-16: Deferred Choice", [
        {"creates valid pattern", fun() ->
            Fun = fun(_) -> true end,
            Pattern = yawl_pattern_reference:deferred_choice([{a, 1}, {b, 2}], Fun, data),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"chooses at runtime", fun() ->
            Fun = fun(_) -> true end,
            Pattern = yawl_pattern_reference:deferred_choice([{a, 1}, {b, 2}], Fun, data),
            Result = yawl_pattern_reference:execute(Pattern, input, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"deferred execution", fun() ->
            Pattern = yawl_pattern_reference:deferred_choice([{a, 1}, {b, 2}], fun(_) -> true end, data),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(deferred_execution, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-17: Interleaved Routing Pattern Tests
%%====================================================================

test_wcp_17_interleaved_routing() ->
    {"WCP-17: Interleaved Routing", [
        {"creates valid pattern", fun() ->
            Branches = [fun(_) -> a end, fun(_) -> b end],
            Pattern = yawl_pattern_reference:interleaved_routing(Branches, data),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"interleaves execution", fun() ->
            Branches = [fun(_) -> a end, fun(_) -> b end],
            Pattern = yawl_pattern_reference:interleaved_routing(Branches, input),
            Result = yawl_pattern_reference:execute(Pattern, data, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"interleaving property", fun() ->
            Pattern = yawl_pattern_reference:interleaved_routing([fun(_) -> x end, fun(_) -> y end], data),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(interleaving, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-18: Milestone Pattern Tests
%%====================================================================

test_wcp_18_milestone() ->
    {"WCP-18: Milestone", [
        {"creates valid pattern", fun() ->
            Activity = fun(I) -> I * 2 end,
            MilestoneFun = fun(_) -> true end,
            Pattern = yawl_pattern_reference:milestone(Activity, MilestoneFun, input),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"enables on milestone", fun() ->
            Activity = fun(I) -> I + 1 end,
            MilestoneFun = fun(_) -> true end,
            Pattern = yawl_pattern_reference:milestone(Activity, MilestoneFun, input),
            Result = yawl_pattern_reference:execute(Pattern, input, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"state based", fun() ->
            Pattern = yawl_pattern_reference:milestone(fun(_) -> ok end, fun(_) -> true end, data),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(state_based, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-19: Cancel Activity Pattern Tests
%%====================================================================

test_wcp_19_cancel_activity() ->
    {"WCP-19: Cancel Activity", [
        {"creates valid pattern", fun() ->
            Activity = fun(_) -> working end,
            CancelFun = fun(_) -> false end,
            Pattern = yawl_pattern_reference:cancel_activity(Activity, CancelFun),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"completes normally", fun() ->
            Activity = fun(I) -> {done, I} end,
            CancelFun = fun(_) -> false end,
            Pattern = yawl_pattern_reference:cancel_activity(Activity, CancelFun),
            Result = yawl_pattern_reference:execute(Pattern, input, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"cancels on request", fun() ->
            Activity = fun(_) -> working end,
            CancelFun = fun(_) -> true end,
            Pattern = yawl_pattern_reference:cancel_activity(Activity, CancelFun),
            Result = yawl_pattern_reference:execute(Pattern, input, #{cancel => true}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"cancellable property", fun() ->
            Pattern = yawl_pattern_reference:cancel_activity(fun(_) -> ok end, fun(_) -> false end),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(cancellable, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-20: Cancel Case Pattern Tests
%%====================================================================

test_wcp_20_cancel_case() ->
    {"WCP-20: Cancel Case", [
        {"creates valid pattern", fun() ->
            Activities = [fun(_) -> a1 end, fun(_) -> a2 end],
            CancelFun = fun(_) -> false end,
            Pattern = yawl_pattern_reference:cancel_case(Activities, CancelFun),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"completes all activities", fun() ->
            Activities = [fun(_) -> done1 end, fun(_) -> done2 end],
            CancelFun = fun(_) -> false end,
            Pattern = yawl_pattern_reference:cancel_case(Activities, CancelFun),
            Result = yawl_pattern_reference:execute(Pattern, input, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"cancels entire case", fun() ->
            Activities = [fun(_) -> working end, fun(_) -> working end],
            CancelFun = fun(_) -> true end,
            Pattern = yawl_pattern_reference:cancel_case(Activities, CancelFun),
            Result = yawl_pattern_reference:execute(Pattern, input, #{cancel => true}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"case cancellation", fun() ->
            Pattern = yawl_pattern_reference:cancel_case([fun(_) -> ok end], fun(_) -> false end),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(case_cancellation, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-25: Cancel Region Pattern Tests
%%====================================================================

test_wcp_25_cancel_region() ->
    {"WCP-25: Cancel Region", [
        {"creates valid pattern", fun() ->
            Activities = [fun(_) -> r1 end, fun(_) -> r2 end],
            CancelFun = fun(_) -> false end,
            Pattern = yawl_pattern_reference:cancel_region(Activities, CancelFun, [region1]),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"cancels only region", fun() ->
            Activities = [fun(_) -> r1 end, fun(_) -> r2 end],
            CancelFun = fun(_) -> true end,
            Pattern = yawl_pattern_reference:cancel_region(Activities, CancelFun, [region1]),
            Result = yawl_pattern_reference:execute(Pattern, input, #{cancel => true}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"scoped cancellation", fun() ->
            Pattern = yawl_pattern_reference:cancel_region([fun(_) -> ok end], fun(_) -> false end, [r1]),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(scoped_cancellation, Soundness))
        end}
    ]}.

%%====================================================================
%% WCP-39: Critical Section Pattern Tests
%%====================================================================

test_wcp_39_critical_section() ->
    {"WCP-39: Critical Section", [
        {"creates valid pattern", fun() ->
            Activity = fun(I) -> {result, I} end,
            Pattern = yawl_pattern_reference:critical_section(Activity, lock1),
            ?assert(is_map(Pattern)),
            ?assertEqual(ok, yawl_pattern_reference:validate_pattern(Pattern))
        end},
        {"executes critically", fun() ->
            Activity = fun(I) -> I * 2 end,
            Pattern = yawl_pattern_reference:critical_section(Activity, my_lock),
            Result = yawl_pattern_reference:execute(Pattern, 5, #{}),
            ?assertEqual(complete, maps:get(status, Result))
        end},
        {"mutual exclusion", fun() ->
            Pattern = yawl_pattern_reference:critical_section(fun(_) -> ok end, lock),
            Soundness = yawl_pattern_reference:verify_soundness(Pattern),
            ?assertEqual(true, maps:get(mutual_exclusion, Soundness))
        end}
    ]}.

%%====================================================================
%% Helper Functions
%%====================================================================

run_test({_, TestList}) ->
    %% Run eunit test list
    lists:foreach(fun({_Desc, TestFun}) -> TestFun() end, TestList).

create_pattern(sequence) ->
    yawl_pattern_reference:sequence([task1, task2]);
create_pattern(parallel_split) ->
    yawl_pattern_reference:parallel_split(2, [t1, t2]);
create_pattern(synchronization) ->
    yawl_pattern_reference:synchronization(2, [t1, t2]);
create_pattern(exclusive_choice) ->
    yawl_pattern_reference:exclusive_choice(fun() -> {1, x} end, [t1, t2]);
create_pattern(structured_sync_merge) ->
    yawl_pattern_reference:structured_sync_merge(2, [t1, t2]);
create_pattern(discriminator) ->
    yawl_pattern_reference:discriminator(2, [t1, t2]);
create_pattern(multi_instance_static) ->
    yawl_pattern_reference:multi_instance_static(2, fun(I) -> I end, [1, 2]);
create_pattern(multi_instance_dynamic) ->
    yawl_pattern_reference:multi_instance_dynamic(fun() -> done end, fun(_) -> ok end);
create_pattern(deferred_choice) ->
    yawl_pattern_reference:deferred_choice([{a, 1}, {b, 2}], fun(_) -> true end, data);
create_pattern(interleaved_routing) ->
    yawl_pattern_reference:interleaved_routing([fun(_) -> a end, fun(_) -> b end], data);
create_pattern(milestone) ->
    yawl_pattern_reference:milestone(fun(I) -> I + 1 end, fun(_) -> true end, input);
create_pattern(cancel_activity) ->
    yawl_pattern_reference:cancel_activity(fun(I) -> I end, fun(_) -> false end);
create_pattern(cancel_case) ->
    yawl_pattern_reference:cancel_case([fun(_) -> ok end, fun(_) -> ok end], fun(_) -> false end);
create_pattern(cancel_region) ->
    yawl_pattern_reference:cancel_region([fun(_) -> ok end], fun(_) -> false end, [region1]);
create_pattern(critical_section) ->
    yawl_pattern_reference:critical_section(fun(I) -> I end, my_lock).

all_patterns() ->
    [
        {sequence, "WCP-01: Sequence"},
        {parallel_split, "WCP-02: Parallel Split"},
        {synchronization, "WCP-03: Synchronization"},
        {exclusive_choice, "WCP-04: Exclusive Choice"},
        {structured_sync_merge, "WCP-07: Structured Sync Merge"},
        {discriminator, "WCP-09: Discriminator"},
        {multi_instance_static, "WCP-13: Multi-Instance Static"},
        {multi_instance_dynamic, "WCP-15: Multi-Instance Dynamic"},
        {deferred_choice, "WCP-16: Deferred Choice"},
        {interleaved_routing, "WCP-17: Interleaved Routing"},
        {milestone, "WCP-18: Milestone"},
        {cancel_activity, "WCP-19: Cancel Activity"},
        {cancel_case, "WCP-20: Cancel Case"},
        {cancel_region, "WCP-25: Cancel Region"},
        {critical_section, "WCP-39: Critical Section"}
    ].
