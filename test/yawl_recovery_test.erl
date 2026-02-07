%% -*- erlang -*-
%%%% @doc Test suite for yawl_recovery module.
%%
%% Comprehensive tests for checkpoint and resume functionality including:
%% <ul>
%%   <li>Basic checkpoint creation and restoration</li>
%%   <li>Multiple checkpoints per workflow case</li>
%%   <li>Checkpoint listing and sorting</li>
%%   <li>Latest checkpoint selection</li>
%%   <li>Checkpoint deletion</li>
%%   <li>Error handling for missing checkpoints</li>
%%   <li>Transaction consistency</li>
%%   <li>Incremental checkpoints</li>
%%   <li>Version tracking</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_recovery_test).
-include_lib("eunit/include/eunit.hrl").
-include("yawl_recovery.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Main test generator for all recovery tests.
%%--------------------------------------------------------------------
yawl_recovery_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Ctx) ->
         [
          {"checkpoint creates valid checkpoint", fun test_checkpoint_creates_valid/0},
          {"resume restores marking and data", fun test_resume_restores_state/0},
          {"list_checkpoints returns sorted list", fun test_list_checkpoints_sorted/0},
          {"get_latest_checkpoint returns most recent", fun test_get_latest_checkpoint/0},
          {"delete_checkpoint removes checkpoint", fun test_delete_checkpoint/0},
          {"resume non_existent fails", fun test_resume_nonexistent_fails/0},
          {"delete_nonexistent fails", fun test_delete_nonexistent_fails/0},
          {"invalid_marking fails", fun test_invalid_marking_fails/0},
          {"multiple_checkpoints_per_case", fun test_multiple_checkpoints/0},
          {"checkpoint_with_options", fun test_checkpoint_with_options/0},
          {"incremental_checkpoint", fun test_incremental_checkpoint/0},
          {"invalid_prev_checkpoint_fails", fun test_invalid_prev_checkpoint_fails/0},
          {"checkpoint_mismatch_wrong_spec", fun test_checkpoint_mismatch_wrong_spec/0},
          {"checkpoint_mismatch_wrong_case", fun test_checkpoint_mismatch_wrong_case/0},
          {"transaction_consistency", fun test_transaction_consistency/0},
          {"empty_marking_checkpoint", fun test_empty_marking_checkpoint/0},
          {"large_data_checkpoint", fun test_large_data_checkpoint/0},
          {"concurrent_checkpoints", fun test_concurrent_checkpoints/0}
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Setup function - creates the Mnesia schema.
%%--------------------------------------------------------------------
setup() ->
    %% Stop and delete any existing Mnesia schema to ensure clean state
    application:stop(mnesia),
    timer:sleep(100),
    mnesia:delete_schema([node()]),
    timer:sleep(100),

    %% Initialize schema with yawl_recovery
    ok = yawl_recovery:init_schema(),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup function - deletes the Mnesia table.
%%--------------------------------------------------------------------
cleanup(_Ctx) ->
    %% Delete the checkpoint table
    case mnesia:delete_table(yawl_checkpoint) of
        {atomic, ok} -> ok;
        {aborted, {no_exists, _}} -> ok
    end,
    ok.

%%====================================================================
%% Individual Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test that checkpoint creates a valid checkpoint.
%%--------------------------------------------------------------------
test_checkpoint_creates_valid() ->
    SpecId = <<"spec1">>,
    CaseId = <<"case1">>,
    Marking = #{p1 => [a, b], p2 => [c]},
    Data = #{key => value},

    {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data),

    %% Verify checkpoint ID format
    true = is_binary(Cpid),
    ?assertEqual(<<"cp_">>, binary_part(Cpid, {0, 3})),

    %% Verify we can read it back from Mnesia
    [{_, Record}] = ets:lookup(yawl_checkpoint, Cpid),
    ?assertEqual(SpecId, Record#yawl_checkpoint.spec_id),
    ?assertEqual(CaseId, Record#yawl_checkpoint.case_id),
    ?assertEqual(Marking, Record#yawl_checkpoint.marking),
    ?assertEqual(Data, Record#yawl_checkpoint.data),
    ?assert(is_integer(Record#yawl_checkpoint.timestamp)),
    ?assertEqual(1, Record#yawl_checkpoint.version).

%%--------------------------------------------------------------------
%% @doc Test that resume restores marking and data.
%%--------------------------------------------------------------------
test_resume_restores_state() ->
    SpecId = <<"spec2">>,
    CaseId = <<"case2">>,
    Marking = #{p1 => [x, y, z], p2 => []},
    Data = #{order_id => 123, customer => "Alice"},

    {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data),
    {ok, {RestoredMarking, RestoredData}} = yawl_recovery:resume(SpecId, CaseId, Cpid),

    ?assertEqual(Marking, RestoredMarking),
    ?assertEqual(Data, RestoredData).

%%--------------------------------------------------------------------
%% @doc Test that list_checkpoints returns sorted list.
%%--------------------------------------------------------------------
test_list_checkpoints_sorted() ->
    SpecId = <<"spec3">>,
    CaseId = <<"case3">>,

    %% Create multiple checkpoints with a small delay
    {ok, Cpid1} = yawl_recovery:checkpoint(SpecId, CaseId, #{p1 => [a]}, #{}),
    timer:sleep(10),
    {ok, Cpid2} = yawl_recovery:checkpoint(SpecId, CaseId, #{p1 => [b]}, #{}),
    timer:sleep(10),
    {ok, Cpid3} = yawl_recovery:checkpoint(SpecId, CaseId, #{p1 => [c]}, #{}),

    Checkpoints = yawl_recovery:list_checkpoints(SpecId, CaseId),

    ?assertEqual(3, length(Checkpoints)),

    %% Should be sorted by timestamp descending (most recent first)
    [{Cpid3, T3}, {Cpid2, T2}, {Cpid1, T1}] = Checkpoints,
    ?assert(T3 > T2),
    ?assert(T2 > T1).

%%--------------------------------------------------------------------
%% @doc Test that get_latest_checkpoint returns most recent.
%%--------------------------------------------------------------------
test_get_latest_checkpoint() ->
    SpecId = <<"spec4">>,
    CaseId = <<"case4">>,

    {ok, _Cpid1} = yawl_recovery:checkpoint(SpecId, CaseId, #{p1 => [old]}, #{}),
    timer:sleep(10),
    {ok, Cpid2} = yawl_recovery:checkpoint(SpecId, CaseId, #{p1 => [new]}, #{}),

    {ok, LatestCpid} = yawl_recovery:get_latest_checkpoint(SpecId, CaseId),
    ?assertEqual(Cpid2, LatestCpid).

%%--------------------------------------------------------------------
%% @doc Test that delete_checkpoint removes checkpoint.
%%--------------------------------------------------------------------
test_delete_checkpoint() ->
    SpecId = <<"spec5">>,
    CaseId = <<"case5">>,

    {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, #{p1 => [x]}, #{}),

    %% Verify it exists
    {ok, _} = yawl_recovery:resume(SpecId, CaseId, Cpid),

    %% Delete it
    ok = yawl_recovery:delete_checkpoint(SpecId, CaseId, Cpid),

    %% Verify it's gone
    {error, not_found} = yawl_recovery:resume(SpecId, CaseId, Cpid),
    ?assertEqual([], yawl_recovery:list_checkpoints(SpecId, CaseId)).

%%--------------------------------------------------------------------
%% @doc Test that resume of non-existent checkpoint fails.
%%--------------------------------------------------------------------
test_resume_nonexistent_fails() ->
    SpecId = <<"spec6">>,
    CaseId = <<"case6">>,
    FakeCpid = <<"cp_9999999999999_99999">>,

    {error, not_found} = yawl_recovery:resume(SpecId, CaseId, FakeCpid).

%%--------------------------------------------------------------------
%% @doc Test that delete of non-existent checkpoint fails.
%%--------------------------------------------------------------------
test_delete_nonexistent_fails() ->
    SpecId = <<"spec7">>,
    CaseId = <<"case7">>,
    FakeCpid = <<"cp_9999999999999_99999">>,

    {error, not_found} = yawl_recovery:delete_checkpoint(SpecId, CaseId, FakeCpid).

%%--------------------------------------------------------------------
%% @doc Test that checkpoint with invalid marking fails.
%%--------------------------------------------------------------------
test_invalid_marking_fails() ->
    SpecId = <<"spec8">>,
    CaseId = <<"case8">>,
    InvalidMarking = #{p1 => not_a_list},

    {error, invalid_marking} = yawl_recovery:checkpoint(SpecId, CaseId, InvalidMarking, #{}).

%%--------------------------------------------------------------------
%% @doc Test multiple checkpoints for the same case.
%%--------------------------------------------------------------------
test_multiple_checkpoints() ->
    SpecId = <<"spec9">>,
    CaseId = <<"case9">>,

    %% Create 5 checkpoints
    CheckpointIds = lists:map(
        fun(N) ->
            Marking = #{p1 => [N], p2 => [N * 2]},
            Data = #{step => N},
            {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data),
            Cpid
        end,
        lists:seq(1, 5)
    ),

    %% Verify all are listed
    Checkpoints = yawl_recovery:list_checkpoints(SpecId, CaseId),
    ?assertEqual(5, length(Checkpoints)),

    %% Verify we can resume each one
    lists:foreach(
        fun(Cpid) ->
            {ok, {_Marking, _Data}} = yawl_recovery:resume(SpecId, CaseId, Cpid)
        end,
        CheckpointIds
    ).

%%--------------------------------------------------------------------
%% @doc Test checkpoint with options (version).
%%--------------------------------------------------------------------
test_checkpoint_with_options() ->
    SpecId = <<"spec10">>,
    CaseId = <<"case10">>,

    Marking = #{p1 => [a]},
    Data = #{key => value},

    %% Create checkpoint with version 2
    {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data, #{version => 2}),

    %% Verify version is stored
    [{_, Record}] = ets:lookup(yawl_checkpoint, Cpid),
    ?assertEqual(2, Record#yawl_checkpoint.version).

%%--------------------------------------------------------------------
%% @doc Test incremental checkpoint with previous checkpoint.
%%--------------------------------------------------------------------
test_incremental_checkpoint() ->
    SpecId = <<"spec11">>,
    CaseId = <<"case11">>,

    %% Create initial checkpoint
    {ok, Cpid1} = yawl_recovery:checkpoint(
        SpecId, CaseId,
        #{p1 => [a], p2 => [b]},
        #{step => 1}
    ),

    %% Create incremental checkpoint pointing to previous
    {ok, Cpid2} = yawl_recovery:checkpoint(
        SpecId, CaseId,
        #{p1 => [c], p2 => [d]},
        #{step => 2},
        #{prev => Cpid1}
    ),

    %% Both should be accessible
    {ok, {M1, D1}} = yawl_recovery:resume(SpecId, CaseId, Cpid1),
    ?assertEqual(#{p1 => [a], p2 => [b]}, M1),
    ?assertEqual(#{step => 1}, D1),

    {ok, {M2, D2}} = yawl_recovery:resume(SpecId, CaseId, Cpid2),
    ?assertEqual(#{p1 => [c], p2 => [d]}, M2),
    ?assertEqual(#{step => 2}, D2).

%%--------------------------------------------------------------------
%% @doc Test that checkpoint with invalid previous checkpoint fails.
%%--------------------------------------------------------------------
test_invalid_prev_checkpoint_fails() ->
    SpecId = <<"spec12">>,
    CaseId = <<"case12">>,

    %% Try to create incremental checkpoint with non-existent previous
    Marking = #{p1 => [x]},
    Data = #{key => value},
    FakePrev = <<"cp_9999999999999_99999">>,

    Result = yawl_recovery:checkpoint(
        SpecId, CaseId,
        Marking, Data,
        #{prev => FakePrev}
    ),

    ?assertMatch({error, {previous_checkpoint_not_found, FakePrev}}, Result).

%%--------------------------------------------------------------------
%% @doc Test checkpoint mismatch with wrong spec_id on resume.
%%--------------------------------------------------------------------
test_checkpoint_mismatch_wrong_spec() ->
    SpecId = <<"spec13">>,
    CaseId = <<"case13">>,

    {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, #{p1 => [a]}, #{}),

    %% Try to resume with wrong spec_id
    WrongSpecId = <<"wrong_spec">>,
    Result = yawl_recovery:resume(WrongSpecId, CaseId, Cpid),

    ?assertMatch({error, {checkpoint_mismatch, Cpid}}, Result).

%%--------------------------------------------------------------------
%% @doc Test checkpoint mismatch with wrong case_id on resume.
%%--------------------------------------------------------------------
test_checkpoint_mismatch_wrong_case() ->
    SpecId = <<"spec14">>,
    CaseId = <<"case14">>,

    {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, #{p1 => [a]}, #{}),

    %% Try to resume with wrong case_id
    WrongCaseId = <<"wrong_case">>,
    Result = yawl_recovery:resume(SpecId, WrongCaseId, Cpid),

    ?assertMatch({error, {checkpoint_mismatch, Cpid}}, Result).

%%--------------------------------------------------------------------
%% @doc Test transaction consistency.
%%--------------------------------------------------------------------
test_transaction_consistency() ->
    SpecId = <<"spec15">>,
    CaseId = <<"case15">>,

    %% Create checkpoint in transaction
    Marking = #{p1 => [a, b, c]},
    Data = #{items => [1, 2, 3]},

    {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data),

    %% Verify atomicity - data should be complete
    {ok, {RestoredMarking, RestoredData}} = yawl_recovery:resume(SpecId, CaseId, Cpid),

    ?assertEqual(Marking, RestoredMarking),
    ?assertEqual(Data, RestoredData),
    ?assertEqual([a, b, c], lists:sort(maps:get(p1, RestoredMarking))),
    ?assertEqual([1, 2, 3], maps:get(items, RestoredData)).

%%--------------------------------------------------------------------
%% @doc Test checkpoint with empty marking.
%%--------------------------------------------------------------------
test_empty_marking_checkpoint() ->
    SpecId = <<"spec16">>,
    CaseId = <<"case16">>,

    EmptyMarking = #{p1 => [], p2 => [], p3 => []},
    Data = #{status => empty},

    {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, EmptyMarking, Data),
    {ok, {RestoredMarking, RestoredData}} = yawl_recovery:resume(SpecId, CaseId, Cpid),

    ?assertEqual(EmptyMarking, RestoredMarking),
    ?assertEqual(Data, RestoredData).

%%--------------------------------------------------------------------
%% @doc Test checkpoint with large data.
%%--------------------------------------------------------------------
test_large_data_checkpoint() ->
    SpecId = <<"spec17">>,
    CaseId = <<"case17">>,

    %% Create large marking with many tokens
    LargeMarking = lists:foldl(
        fun(N, Acc) ->
            maps:put(list_to_existing_atom("p" ++ integer_to_list(N)), lists:seq(1, 100), Acc)
        end,
        #{},
        lists:seq(1, 10)
    ),

    %% Create large data map
    LargeData = lists:foldl(
        fun(N, Acc) ->
            maps:put(list_to_existing_atom("key" ++ integer_to_list(N)), lists:seq(1, 100), Acc)
        end,
        #{},
        lists:seq(1, 20)
    ),

    {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, LargeMarking, LargeData),
    {ok, {RestoredMarking, RestoredData}} = yawl_recovery:resume(SpecId, CaseId, Cpid),

    ?assertEqual(LargeMarking, RestoredMarking),
    ?assertEqual(LargeData, RestoredData).

%%--------------------------------------------------------------------
%% @doc Test concurrent checkpoints.
%%--------------------------------------------------------------------
test_concurrent_checkpoints() ->
    SpecId = <<"spec18">>,
    CaseId = <<"case18">>,

    %% Spawn multiple processes creating checkpoints concurrently
    Parent = self(),

    Pids = lists:map(
        fun(N) ->
            spawn(fun() ->
                Marking = #{p1 => [N]},
                Data = #{worker => N},
                Result = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data),
                Parent ! {N, Result}
            end)
        end,
        lists:seq(1, 10)
    ),

    %% Collect results
    Results = lists:map(
        fun(_) ->
            receive
                {N, Result} -> {N, Result}
            end
        end,
        lists:seq(1, 10)
    ),

    %% All should succeed
    lists:foreach(
        fun({_N, Result}) ->
            ?assertMatch({ok, _Cpid}, Result)
        end,
        Results
    ),

    %% All checkpoints should be listed
    Checkpoints = yawl_recovery:list_checkpoints(SpecId, CaseId),
    ?assertEqual(10, length(Checkpoints)).

%%====================================================================
%% Helper Functions
%%====================================================================
