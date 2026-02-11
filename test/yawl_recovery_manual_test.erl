%% -*- erlang -*-
%% Manual test module for yawl_recovery

-module(yawl_recovery_manual_test).
-export([run/0]).

-include("yawl_recovery.hrl").

run() ->
    io:format("Starting manual yawl_recovery test...~n"),

    % Stop and delete any existing Mnesia schema for clean state
    application:stop(mnesia),
    timer:sleep(50),
    mnesia:delete_schema([node()]),
    timer:sleep(50),

    % Start Mnesia
    case mnesia:start() of
        ok -> io:format("Mnesia started~n");
        {error, {already_started, mnesia}} -> io:format("Mnesia already running~n")
    end,

    % Create table using init_schema
    case yawl_recovery:init_schema() of
        ok -> io:format("Table created~n");
        {error, Reason} -> io:format("Error creating table: ~p~n", [Reason])
    end,

    % Test data
    SpecId = <<"test_spec">>,
    CaseId = <<"test_case">>,
    Marking = #{p1 => [a, b], p2 => [c]},
    Data = #{key => value, order_id => 123},

    % Test 1: Create checkpoint
    io:format("~n--- Test 1: Create checkpoint ---~n"),
    io:format("Creating checkpoint with spec_id=~s, case_id=~s~n", [SpecId, CaseId]),
    io:format("Marking: ~p~n", [Marking]),
    io:format("Data: ~p~n", [Data]),

    % Check if marking is valid
    IsValid = pnet_types:is_marking(Marking),
    io:format("Marking valid? ~p~n", [IsValid]),

    {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data),
    io:format("Checkpoint ID: ~s~n", [Cpid]),

    % Verify it was actually written
    Written = mnesia:dirty_read(yawl_checkpoint, Cpid),
    io:format("Written to Mnesia? ~p~n", [Written =/= []]),

    % Test 2: List checkpoints
    io:format("~n--- Test 2: List checkpoints ---~n"),
    Checkpoints = yawl_recovery:list_checkpoints(SpecId, CaseId),
    io:format("Found ~p checkpoint(s)~n", [length(Checkpoints)]),

    % Test 3: Get latest checkpoint
    io:format("~n--- Test 3: Get latest checkpoint ---~n"),
    {ok, Latest} = yawl_recovery:get_latest_checkpoint(SpecId, CaseId),
    io:format("Latest checkpoint: ~s~n", [Latest]),

    % Test 4: Resume from checkpoint
    io:format("~n--- Test 4: Resume from checkpoint ---~n"),
    io:format("Attempting to resume checkpoint: ~s~n", [Cpid]),
    io:format("SpecId: ~s, CaseId: ~s~n", [SpecId, CaseId]),

    % Debug: Check what's actually in Mnesia
    DirectRead = mnesia:dirty_read(yawl_checkpoint, Cpid),
    io:format("Direct Mnesia read: ~p~n", [DirectRead]),

    {ok, {RestoredMarking, RestoredData}} = yawl_recovery:resume(SpecId, CaseId, Cpid),
    io:format("Restored marking: ~p~n", [RestoredMarking]),
    io:format("Restored data: ~p~n", [RestoredData]),

    % Verify
    if
        RestoredMarking =:= Marking -> io:format("✓ Marking matches~n");
        true -> io:format("✗ Marking MISMATCH~n")
    end,
    if
        RestoredData =:= Data -> io:format("✓ Data matches~n");
        true -> io:format("✗ Data MISMATCH~n")
    end,

    % Test 5: Create multiple checkpoints
    io:format("~n--- Test 5: Create multiple checkpoints ---~n"),
    {ok, Cpid2} = yawl_recovery:checkpoint(SpecId, CaseId, #{p1 => [x]}, #{step => 2}),
    io:format("Created second checkpoint: ~s~n", [Cpid2]),

    Checkpoints2 = yawl_recovery:list_checkpoints(SpecId, CaseId),
    io:format("Now have ~p checkpoint(s)~n", [length(Checkpoints2)]),

    {ok, Latest2} = yawl_recovery:get_latest_checkpoint(SpecId, CaseId),
    io:format("Latest is now: ~s~n", [Latest2]),
    if
        Latest2 =:= Cpid2 -> io:format("✓ Latest checkpoint updated~n");
        true -> io:format("✗ Latest checkpoint NOT updated~n")
    end,

    % Test 6: Delete checkpoint
    io:format("~n--- Test 6: Delete checkpoint ---~n"),
    ok = yawl_recovery:delete_checkpoint(SpecId, CaseId, Cpid),
    io:format("Deleted first checkpoint~n"),

    Checkpoints3 = yawl_recovery:list_checkpoints(SpecId, CaseId),
    io:format("Now have ~p checkpoint(s)~n", [length(Checkpoints3)]),

    % Test 7: Error handling - resume non-existent
    io:format("~n--- Test 7: Error handling ---~n"),
    {error, not_found} = yawl_recovery:resume(SpecId, CaseId, <<"nonexistent">>),
    io:format("✓ Non-existent checkpoint returns not_found~n"),

    {error, not_found} = yawl_recovery:delete_checkpoint(SpecId, CaseId, <<"nonexistent">>),
    io:format("✓ Delete non-existent checkpoint returns not_found~n"),

    {error, not_found} = yawl_recovery:get_latest_checkpoint(<<"nospec">>, <<"nocase">>),
    io:format("✓ Get latest for non-existent case returns not_found~n"),

    % Cleanup
    mnesia:delete_table(yawl_checkpoint),

    io:format("~n=== All tests passed! ===~n"),
    ok.
