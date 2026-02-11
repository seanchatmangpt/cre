%%%-------------------------------------------------------------------
%%% @doc
%%% Unit tests for evidence_sys module.
%%% @end
%%%-------------------------------------------------------------------
-module(evidence_sys_test).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Data Generation
%%%===================================================================

%% Helper to start a test process
start_test_process() ->
    spawn(fun() -> test_loop() end).

test_loop() ->
    receive
        stop -> ok;
        _ -> test_loop()
    end.

%% Helper process that does work
work_process(Iterations) ->
    spawn(fun() -> work_loop(Iterations) end).

work_loop(0) -> receive stop -> ok end;
work_loop(N) ->
    % Do some work to generate reductions
    lists:sum(lists:seq(1, 100)),
    work_loop(N - 1).

%%%===================================================================
%%% sample_pid/2 Tests
%%%===================================================================

sample_pid_valid_process_test() ->
    Pid = start_test_process(),
    Result = evidence_sys:sample_pid(Pid, 100),
    ?assertMatch({ok, _}, Result),
    {ok, Info} = Result,
    ?assertMatch(#{pid := Pid}, Info),
    ?assertMatch(#{stats := _}, Info),
    Pid ! stop,
    timer:sleep(10).

sample_pid_registered_name_test() ->
    % Create a named process using gen_server style
    {ok, Pid} = evidence_sys:start_link(),
    Result = evidence_sys:sample_pid(evidence_sys, 100),
    ?assertMatch({ok, _}, Result),
    {ok, Info} = Result,
    ?assertMatch(#{registered_name := evidence_sys}, Info),
    evidence_sys:stop(),
    timer:sleep(50).

sample_pid_nonexistent_process_test() ->
    % Use a self-terminating process - spawn and exit immediately
    Pid = spawn(fun() -> ok end),
    timer:sleep(10),  % Ensure it has exited
    Result = evidence_sys:sample_pid(Pid, 100),
    ?assertMatch({error, _}, Result).

sample_pid_unregistered_atom_test() ->
    Result = evidence_sys:sample_pid(nonexistent_process_xyz, 100),
    ?assertMatch({error, {not_registered, _}}, Result).

sample_pid_collects_reductions_test() ->
    Pid = work_process(10),
    timer:sleep(50),  % Let it do some work first
    {ok, Info} = evidence_sys:sample_pid(Pid, 100),
    Stats = maps:get(stats, Info),
    Reds = maps:get(reductions, Stats),
    ?assert(maps:get(delta, Reds) >= 0),
    Pid ! stop,
    timer:sleep(10).

sample_pid_collects_memory_test() ->
    Pid = start_test_process(),
    {ok, Info} = evidence_sys:sample_pid(Pid, 100),
    Stats = maps:get(stats, Info),
    Mem = maps:get(memory, Stats),
    ?assert(is_integer(maps:get(initial, Mem))),
    ?assert(is_integer(maps:get(final, Mem))),
    ?assert(maps:get(initial, Mem) > 0),
    Pid ! stop,
    timer:sleep(10).

sample_pid_collects_message_queue_test() ->
    Pid = start_test_process(),
    % Send some messages
    Pid ! msg1,
    Pid ! msg2,
    {ok, Info} = evidence_sys:sample_pid(Pid, 100),
    Stats = maps:get(stats, Info),
    Mql = maps:get(message_queue_len, Stats),
    ?assert(is_integer(maps:get(initial, Mql))),
    Pid ! stop,
    timer:sleep(10).

sample_pid_duration_test() ->
    Pid = start_test_process(),
    StartTime = erlang:monotonic_time(millisecond),
    {ok, Info} = evidence_sys:sample_pid(Pid, 200),
    EndTime = erlang:monotonic_time(millisecond),
    Stats = maps:get(stats, Info),
    ?assertMatch(#{duration_ms := 200}, Stats),
    % Verify actual duration is at least 200ms
    ?assert(EndTime - StartTime >= 200),
    Pid ! stop,
    timer:sleep(10).

%%%===================================================================
%%% sample_all/1 Tests
%%%===================================================================

sample_all_returns_map_test() ->
    Result = evidence_sys:sample_all(50),
    ?assert(is_map(Result)),
    ?assert(map_size(Result) > 0).

sample_all_includes_registered_processes_test() ->
    {ok, _} = evidence_sys:start_link(),
    Result = evidence_sys:sample_all(50),
    ?assert(is_map(Result)),
    % Should include evidence_sys itself
    ?assert(maps:is_key(evidence_sys, Result)),
    evidence_sys:stop(),
    timer:sleep(50).

sample_all_values_are_ok_or_error_test() ->
    Result = evidence_sys:sample_all(50),
    maps:foreach(fun(_Name, Value) ->
        case Value of
            {ok, _} -> ok;
            {error, _} -> ok;
            _ -> erlang:error(bad_value, Value)
        end
    end, Result).

%%%===================================================================
%%% stats_to_json/1 Tests
%%%===================================================================

stats_to_json_single_info_test() ->
    Pid = start_test_process(),
    {ok, Info} = evidence_sys:sample_pid(Pid, 50),
    Json = evidence_sys:stats_to_json(Info),
    ?assert(is_map(Json)),
    ?assert(maps:is_key(<<"pid">>, Json)),
    ?assert(maps:is_key(<<"registered_name">>, Json)),
    ?assert(maps:is_key(<<"sampling">>, Json)),
    ?assert(maps:is_key(<<"status_summary">>, Json)),
    Pid ! stop,
    timer:sleep(10).

stats_to_json_list_test() ->
    Pid1 = start_test_process(),
    Pid2 = start_test_process(),
    {ok, Info1} = evidence_sys:sample_pid(Pid1, 50),
    {ok, Info2} = evidence_sys:sample_pid(Pid2, 50),
    Json = evidence_sys:stats_to_json([Info1, Info2]),
    ?assert(is_list(Json)),
    ?assert(length(Json) >= 2),
    Pid1 ! stop,
    Pid2 ! stop,
    timer:sleep(10).

stats_to_json_results_map_test() ->
    Results = evidence_sys:sample_all(50),
    Json = evidence_sys:stats_to_json(Results),
    ?assert(is_list(Json)),
    % All items should be maps
    lists:foreach(fun(Item) ->
        ?assert(is_map(Item))
    end, Json).

stats_to_json_has_required_fields_test() ->
    Pid = start_test_process(),
    {ok, Info} = evidence_sys:sample_pid(Pid, 50),
    Json = evidence_sys:stats_to_json(Info),
    Sampling = maps:get(<<"sampling">>, Json),
    ?assert(maps:is_key(<<"duration_ms">>, Sampling)),
    ?assert(maps:is_key(<<"reductions">>, Sampling)),
    ?assert(maps:is_key(<<"message_queue_len">>, Sampling)),
    ?assert(maps:is_key(<<"memory">>, Sampling)),
    ?assert(maps:is_key(<<"garbage_collection">>, Sampling)),
    Pid ! stop,
    timer:sleep(10).

stats_to_json_reductions_structure_test() ->
    Pid = start_test_process(),
    {ok, Info} = evidence_sys:sample_pid(Pid, 50),
    Json = evidence_sys:stats_to_json(Info),
    Sampling = maps:get(<<"sampling">>, Json),
    Reds = maps:get(<<"reductions">>, Sampling),
    ?assert(maps:is_key(<<"total_initial">>, Reds)),
    ?assert(maps:is_key(<<"total_final">>, Reds)),
    ?assert(maps:is_key(<<"delta">>, Reds)),
    Pid ! stop,
    timer:sleep(10).

stats_to_json_memory_structure_test() ->
    Pid = start_test_process(),
    {ok, Info} = evidence_sys:sample_pid(Pid, 50),
    Json = evidence_sys:stats_to_json(Info),
    Sampling = maps:get(<<"sampling">>, Json),
    Mem = maps:get(<<"memory">>, Sampling),
    ?assert(maps:is_key(<<"total_initial">>, Mem)),
    ?assert(maps:is_key(<<"total_final">>, Mem)),
    ?assert(maps:is_key(<<"delta">>, Mem)),
    Pid ! stop,
    timer:sleep(10).

%%%===================================================================
%%% save_stats/2 Tests
%%%===================================================================

save_stats_single_test() ->
    Pid = start_test_process(),
    {ok, Info} = evidence_sys:sample_pid(Pid, 50),
    Filename = "/tmp/evidence_sys_single_test.json",
    Result = evidence_sys:save_stats(Info, Filename),
    ?assertEqual(ok, Result),
    ?assert(filelib:is_file(Filename)),
    {ok, Content} = file:read_file(Filename),
    ?assert(is_binary(Content)),
    % Verify it's valid JSON
    Parsed = jsx:decode(Content, [return_maps]),
    ?assert(is_map(Parsed)),
    file:delete(Filename),
    Pid ! stop,
    timer:sleep(10).

save_stats_list_test() ->
    Pid1 = start_test_process(),
    Pid2 = start_test_process(),
    {ok, Info1} = evidence_sys:sample_pid(Pid1, 50),
    {ok, Info2} = evidence_sys:sample_pid(Pid2, 50),
    Filename = "/tmp/evidence_sys_list_test.json",
    Result = evidence_sys:save_stats([Info1, Info2], Filename),
    ?assertEqual(ok, Result),
    ?assert(filelib:is_file(Filename)),
    {ok, Content} = file:read_file(Filename),
    Parsed = jsx:decode(Content, [return_maps]),
    ?assert(is_list(Parsed)),
    file:delete(Filename),
    Pid1 ! stop,
    Pid2 ! stop,
    timer:sleep(10).

save_stats_results_map_test() ->
    Results = evidence_sys:sample_all(50),
    Filename = "/tmp/evidence_sys_map_test.json",
    Result = evidence_sys:save_stats(Results, Filename),
    ?assertEqual(ok, Result),
    ?assert(filelib:is_file(Filename)),
    {ok, Content} = file:read_file(Filename),
    Parsed = jsx:decode(Content, [return_maps]),
    ?assert(is_list(Parsed)),
    file:delete(Filename).

save_stats_creates_valid_json_test() ->
    Pid = start_test_process(),
    {ok, Info} = evidence_sys:sample_pid(Pid, 50),
    Filename = "/tmp/evidence_sys_valid_json_test.json",
    ok = evidence_sys:save_stats(Info, Filename),
    {ok, Content} = file:read_file(Filename),
    % Should be able to decode without error
    Parsed = jsx:decode(Content, [return_maps]),
    ?assert(maps:is_key(<<"pid">>, Parsed)),
    file:delete(Filename),
    Pid ! stop,
    timer:sleep(10).

%%%===================================================================
%%% Integration Tests
%%%===================================================================

full_sampling_workflow_test() ->
    % 1. Sample a process
    Pid = work_process(5),
    {ok, Info} = evidence_sys:sample_pid(Pid, 100),

    % 2. Convert to JSON
    Json = evidence_sys:stats_to_json(Info),
    ?assert(is_map(Json)),

    % 3. Save to file
    Filename = "/tmp/evidence_sys_workflow_test.json",
    ok = evidence_sys:save_stats(Info, Filename),
    ?assert(filelib:is_file(Filename)),

    % 4. Read back and verify
    {ok, Content} = file:read_file(Filename),
    Parsed = jsx:decode(Content, [return_maps]),
    ?assertEqual(maps:get(<<"pid">>, Json), maps:get(<<"pid">>, Parsed)),

    file:delete(Filename),
    Pid ! stop,
    timer:sleep(10).

multiple_process_sampling_test() ->
    % Start multiple processes
    Pids = [work_process(3) || _ <- lists:seq(1, 5)],

    % Sample all
    Results = evidence_sys:sample_all(100),

    % Convert and save
    Json = evidence_sys:stats_to_json(Results),
    ?assert(length(Json) > 0),

    Filename = "/tmp/evidence_sys_multi_test.json",
    ok = evidence_sys:save_stats(Results, Filename),
    ?assert(filelib:is_file(Filename)),

    % Verify file contains valid JSON array
    {ok, Content} = file:read_file(Filename),
    Parsed = jsx:decode(Content, [return_maps]),
    ?assert(is_list(Parsed)),
    ?assert(length(Parsed) > 0),

    % Cleanup
    lists:foreach(fun(P) -> P ! stop end, Pids),
    timer:sleep(50),
    file:delete(Filename).

%%%===================================================================
%%% Stats Content Tests
%%%===================================================================

stats_has_timestamps_test() ->
    Pid = start_test_process(),
    {ok, Info} = evidence_sys:sample_pid(Pid, 100),
    Stats = maps:get(stats, Info),
    Timestamps = maps:get(sampling_timestamp, Stats),
    ?assert(maps:is_key(start_ts, Timestamps)),
    ?assert(maps:is_key(end_ts, Timestamps)),
    ?assert(maps:get(end_ts, Timestamps) >= maps:get(start_ts, Timestamps)),
    Pid ! stop,
    timer:sleep(10).
