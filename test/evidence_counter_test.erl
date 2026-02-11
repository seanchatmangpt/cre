%%%-------------------------------------------------------------------
%%% @doc EUnit Tests for evidence_counter module.
%%%
%%% Tests the ETS-based effect counter for runtime verification.
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(evidence_counter_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Ensure clean state for test - stops any existing counter
ensure_clean_state() ->
    catch evidence_counter:stop(),
    timer:sleep(50),
    ok.

%% @doc Start counter for test
start_counter_for_test() ->
    case whereis(evidence_counter) of
        undefined ->
            evidence_counter:start_link();
        Pid ->
            %% Already running, just return it
            {ok, Pid}
    end.

%%====================================================================
%% Test Generators - Run tests sequentially using fun() wrappers
%%====================================================================

%% @doc Test starting the counter server
start_server_returns_pid_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, Pid} = evidence_counter:start_link(),
        ?assert(is_pid(Pid)),
        evidence_counter:stop(),
        ?assert(process_info(Pid) =:= undefined)
    end.

%% @doc Test stopping the counter server
stop_server_terminates_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, Pid} = evidence_counter:start_link(),
        ok = evidence_counter:stop(),
        timer:sleep(100),
        ?assert(process_info(Pid) =:= undefined)
    end.

%% @doc Test counting a task_start effect
count_task_start_increments_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(task_start, #{}),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(1, maps:get(task_start, Counts, 0)),
        evidence_counter:stop()
    end.

%% @doc Test counting multiple task_start effects
count_multiple_task_start_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(task_start, #{}),
        evidence_counter:count_effect(task_start, #{}),
        evidence_counter:count_effect(task_start, #{}),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(3, maps:get(task_start, Counts, 0)),
        evidence_counter:stop()
    end.

%% @doc Test counting task_complete effect
count_task_complete_increments_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(task_complete, #{}),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(1, maps:get(task_complete, Counts, 0)),
        evidence_counter:stop()
    end.

%% @doc Test counting cancel effect
count_cancel_increments_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(cancel, #{}),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(1, maps:get(cancel, Counts, 0)),
        evidence_counter:stop()
    end.

%% @doc Test counting fork effect
count_fork_increments_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(fork, #{}),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(1, maps:get(fork, Counts, 0)),
        evidence_counter:stop()
    end.

%% @doc Test counting join effect
count_join_increments_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(join, #{}),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(1, maps:get(join, Counts, 0)),
        evidence_counter:stop()
    end.

%% @doc Test counting scope_enter effect
count_scope_enter_increments_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(scope_enter, #{}),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(1, maps:get(scope_enter, Counts, 0)),
        evidence_counter:stop()
    end.

%% @doc Test counting scope_exit effect
count_scope_exit_increments_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(scope_exit, #{}),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(1, maps:get(scope_exit, Counts, 0)),
        evidence_counter:stop()
    end.

%% @doc Test counting wait_signal effect
count_wait_signal_increments_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(wait_signal, #{}),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(1, maps:get(wait_signal, Counts, 0)),
        evidence_counter:stop()
    end.

%% @doc Test counting effect_receipt effect
count_effect_receipt_increments_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(effect_receipt, #{}),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(1, maps:get(effect_receipt, Counts, 0)),
        evidence_counter:stop()
    end.

%%====================================================================
%% Get Counts Tests
%%====================================================================

%% @doc Test get_counts returns map with all effect types
get_counts_returns_map_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        Counts = evidence_counter:get_counts(),
        ?assert(is_map(Counts)),
        ?assert(maps:is_key(task_start, Counts)),
        ?assert(maps:is_key(task_complete, Counts)),
        ?assert(maps:is_key(cancel, Counts)),
        ?assert(maps:is_key(fork, Counts)),
        ?assert(maps:is_key(join, Counts)),
        ?assert(maps:is_key(scope_enter, Counts)),
        ?assert(maps:is_key(scope_exit, Counts)),
        ?assert(maps:is_key(wait_signal, Counts)),
        ?assert(maps:is_key(effect_receipt, Counts)),
        evidence_counter:stop()
    end.

%% @doc Test get_counts returns zero for uncounted effects
get_counts_initially_zero_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(0, maps:get(task_start, Counts)),
        ?assertEqual(0, maps:get(task_complete, Counts)),
        evidence_counter:stop()
    end.

%%====================================================================
%% Get Count Tests
%%====================================================================

%% @doc Test get_count for existing effect type
get_count_existing_returns_value_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(task_start, #{}),
        evidence_counter:count_effect(task_start, #{}),
        ?assertEqual({ok, 2}, evidence_counter:get_count(task_start)),
        evidence_counter:stop()
    end.

%% @doc Test get_count for uncounted effect type
get_count_uncounted_returns_zero_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        ?assertEqual({ok, 0}, evidence_counter:get_count(task_start)),
        evidence_counter:stop()
    end.

%% @doc Test get_count for invalid type
get_count_invalid_type_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        ?assertEqual({error, not_found}, evidence_counter:get_count(invalid_type)),
        evidence_counter:stop()
    end.

%%====================================================================
%% Reset Tests
%%====================================================================

%% @doc Test reset_counters clears all counts
reset_counters_clears_all_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(task_start, #{}),
        evidence_counter:count_effect(task_complete, #{}),
        evidence_counter:count_effect(cancel, #{}),
        ok = evidence_counter:reset_counters(),
        Counts = evidence_counter:get_counts(),
        ?assertEqual(0, maps:get(task_start, Counts)),
        ?assertEqual(0, maps:get(task_complete, Counts)),
        ?assertEqual(0, maps:get(cancel, Counts)),
        evidence_counter:stop()
    end.

%% @doc Test reset then count works correctly
reset_then_count_works_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(task_start, #{}),
        evidence_counter:count_effect(task_start, #{}),
        ok = evidence_counter:reset_counters(),
        evidence_counter:count_effect(task_start, #{}),
        ?assertEqual({ok, 1}, evidence_counter:get_count(task_start)),
        evidence_counter:stop()
    end.

%%====================================================================
%% Dump Counts Tests
%%====================================================================

%% @doc Test dump_counts returns required fields
dump_counts_has_required_fields_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        Snapshot = evidence_counter:dump_counts(),
        ?assert(maps:is_key(counts, Snapshot)),
        ?assert(maps:is_key(timestamp, Snapshot)),
        ?assert(maps:is_key(uptime_ms, Snapshot)),
        ?assert(maps:is_key(total_effects, Snapshot)),
        evidence_counter:stop()
    end.

%% @doc Test dump_counts includes individual counts
dump_counts_includes_counts_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(task_start, #{}),
        evidence_counter:count_effect(task_complete, #{}),
        Snapshot = evidence_counter:dump_counts(),
        CountsMap = maps:get(counts, Snapshot),
        ?assert(is_map(CountsMap)),
        ?assert(maps:is_key(task_start, CountsMap)),
        ?assert(maps:is_key(task_complete, CountsMap)),
        evidence_counter:stop()
    end.

%% @doc Test dump_counts total_effects is sum of all counts
dump_counts_total_is_sum_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        evidence_counter:count_effect(task_start, #{}),
        evidence_counter:count_effect(task_start, #{}),
        evidence_counter:count_effect(task_complete, #{}),
        Snapshot = evidence_counter:dump_counts(),
        Total = maps:get(total_effects, Snapshot),
        ?assertEqual(3, Total),
        evidence_counter:stop()
    end.

%% @doc Test dump_counts uptime_ms is positive
dump_counts_uptime_positive_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        timer:sleep(50),
        Snapshot = evidence_counter:dump_counts(),
        Uptime = maps:get(uptime_ms, Snapshot),
        ?assert(Uptime >= 0),
        ?assert(Uptime < 10000),
        evidence_counter:stop()
    end.

%% @doc Test dump_counts timestamp is recent
dump_counts_timestamp_recent_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        Before = erlang:system_time(millisecond),
        Snapshot = evidence_counter:dump_counts(),
        After = erlang:system_time(millisecond),
        Timestamp = maps:get(timestamp, Snapshot),
        ?assert(Timestamp >= Before),
        ?assert(Timestamp =< After),
        evidence_counter:stop()
    end.

%%====================================================================
%% Details Tracking Tests
%%====================================================================

%% @doc Test count_effect with details map
count_with_details_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        Details = #{
            workflow_id => <<"wf123">>,
            case_id => <<"case456">>,
            details => #{task => foo}
        },
        evidence_counter:count_effect(task_start, Details),
        Snapshot = evidence_counter:dump_counts(),
        CountsMap = maps:get(counts, Snapshot),
        TaskStartInfo = maps:get(task_start, CountsMap),
        ?assert(maps:is_key(count, TaskStartInfo)),
        ?assert(maps:is_key(last_seen, TaskStartInfo)),
        ?assert(maps:is_key(recent_details, TaskStartInfo)),
        evidence_counter:stop()
    end.

%% @doc Test recent_details captures last events
recent_details_captures_events_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        lists:foreach(fun(I) ->
            Details = #{index => I},
            evidence_counter:count_effect(task_start, Details)
        end, lists:seq(1, 5)),
        Snapshot = evidence_counter:dump_counts(),
        CountsMap = maps:get(counts, Snapshot),
        TaskStartInfo = maps:get(task_start, CountsMap),
        RecentDetails = maps:get(recent_details, TaskStartInfo),
        ?assert(is_list(RecentDetails)),
        ?assert(length(RecentDetails) =< 10),
        evidence_counter:stop()
    end.

%%====================================================================
%% Concurrency Tests
%%====================================================================

%% @doc Test concurrent counting from multiple processes
concurrent_counting_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = start_counter_for_test(),
        %% Spawn 10 processes each counting 10 times
        NumProcesses = 10,
        CountsPerProcess = 10,
        Pids = lists:map(fun(_) ->
            spawn_link(fun() ->
                lists:foreach(fun(_) ->
                    evidence_counter:count_effect(task_start, #{})
                end, lists:seq(1, CountsPerProcess))
            end)
        end, lists:seq(1, NumProcesses)),
        %% Wait for all to complete
        lists:foreach(fun(P) ->
            Ref = monitor(process, P),
            receive
                {'DOWN', Ref, process, P, _} -> ok
            end
        end, Pids),
        timer:sleep(100),
        ?assertEqual({ok, NumProcesses * CountsPerProcess}, evidence_counter:get_count(task_start)),
        evidence_counter:stop()
    end.

%%====================================================================
%% Evidence Hooks Tests
%%====================================================================

%% @doc Test is_installed returns false when no hooks
is_installed_initially_false_test_() ->
    fun() ->
        catch evidence_hooks:uninstall_trace_hooks(),
        ensure_clean_state(),
        {ok, _Pid} = evidence_counter:start_link(),
        ?assertNot(evidence_hooks:is_installed()),
        evidence_hooks:uninstall_trace_hooks(),
        evidence_counter:stop()
    end.

%% @doc Test install_hooks_returns ok
install_hooks_returns_ok_test_() ->
    fun() ->
        catch evidence_hooks:uninstall_trace_hooks(),
        ensure_clean_state(),
        {ok, _Pid} = evidence_counter:start_link(),
        Result = evidence_hooks:install_trace_hooks(),
        ?assertMatch({ok, _Count}, Result),
        evidence_hooks:uninstall_trace_hooks(),
        evidence_counter:stop()
    end.

%% @doc Test uninstall_trace_hooks succeeds
uninstall_hooks_succeeds_test_() ->
    fun() ->
        ?assertEqual(ok, evidence_hooks:uninstall_trace_hooks())
    end.

%% @doc Test install_scope_hooks_returns ok
install_scope_hooks_returns_ok_test_() ->
    fun() ->
        ensure_clean_state(),
        {ok, _Pid} = evidence_counter:start_link(),
        Result = evidence_hooks:install_scope_hooks(),
        ?assertMatch({ok, _Count}, Result),
        evidence_counter:stop()
    end.

%% @doc Test install_hooks_without_counter fails
install_hooks_without_counter_fails_test_() ->
    fun() ->
        ensure_clean_state(),
        Result = evidence_hooks:install_trace_hooks(),
        ?assertEqual({error, evidence_counter_not_running}, Result)
    end.
