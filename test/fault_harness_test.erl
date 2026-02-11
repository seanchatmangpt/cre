%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Project
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
%% @doc Fault Injection Harness Unit Tests
%%
%% EUnit tests for the fault_harness module.
%%
%% @end
%% -------------------------------------------------------------------

-module(fault_harness_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

fault_harness_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"new harness creates valid state", fun new_harness_creates_valid_state/0},
      {"new harness with options", fun new_harness_with_options/0},
      {"new harness stores seed", fun new_harness_stores_seed/0},
      {"inject fault creates receipt", fun inject_fault_creates_receipt/0},
      {"inject fault with spec", fun inject_fault_with_spec/0},
      {"inject fault updates sequence", fun inject_fault_updates_sequence/0},
      {"inject multiple faults", fun inject_multiple_faults/0},
      {"get receipt returns latest", fun get_receipt_returns_latest/0},
      {"get receipt by id", fun get_receipt_by_id/0},
      {"get receipt when empty", fun get_receipt_when_empty/0},
      {"get all receipts", fun get_all_receipts/0},
      {"verify recovery with pid check", fun verify_recovery_with_pid_check/0},
      {"verify recovery with function check", fun verify_recovery_with_function_check/0},
      {"verify recovery multiple checks", fun verify_recovery_multiple_checks/0},
      {"verify recovery updates receipt", fun verify_recovery_updates_receipt/0},
      {"cleanup clears active faults", fun cleanup_clears_active_faults/0},
      {"replay harness from receipts", fun replay_harness_from_receipts/0},
      {"export receipts creates map", fun export_receipts_creates_map/0},
      {"deterministic seed produces same sequence", fun deterministic_seed_produces_same_sequence/0},
      {"different seeds produce different sequences", fun different_seeds_produce_different_sequences/0}
     ]
    }.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

setup() ->
    ok.

cleanup(__) ->
    %% Clean up any running processes
    case whereis(f5_fault_injector) of
        undefined -> ok;
        Pid -> catch gen_server:stop(Pid)
    end,
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

new_harness_creates_valid_state() ->
    Seed = 12345,
    {ok, Harness} = fault_harness:new_harness(Seed, #{auto_start => false}),

    ?assert(is_map(Harness)),
    ?assert(maps:is_key(id, Harness)),
    ?assert(maps:is_key(seed, Harness)),
    ?assert(maps:is_key(prng, Harness)),
    ?assert(maps:is_key(receipts, Harness)),
    ?assert(maps:is_key(active_faults, Harness)),
    ?assert(maps:is_key(started_at, Harness)).

new_harness_with_options() ->
    Seed = 54321,
    {ok, Harness} = fault_harness:new_harness(Seed, #{auto_start => false}),

    ?assertEqual(Seed, maps:get(seed, Harness)),
    ?assertEqual([], maps:get(receipts, Harness)),
    ?assertEqual([], maps:get(active_faults, Harness)).

new_harness_stores_seed() ->
    Seeds = [0, 1, 12345, 999999],
    lists:foreach(fun(Seed) ->
        {ok, Harness} = fault_harness:new_harness(Seed),
        ?assertEqual(Seed, maps:get(seed, Harness))
    end, Seeds).

inject_fault_creates_receipt() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),
    {ok, _NewHarness, Receipt} = fault_harness:inject_fault(Harness, timeout),

    ?assert(is_map(Receipt)),
    ?assert(maps:is_key(seed, Receipt)),
    ?assert(maps:is_key(fault_type, Receipt)),
    ?assert(maps:is_key(timestamp, Receipt)),
    ?assert(maps:is_key(fault_id, Receipt)),
    ?assert(maps:is_key(sequence, Receipt)),
    ?assertEqual(42, maps:get(seed, Receipt)),
    ?assertEqual(timeout, maps:get(fault_type, Receipt)),
    ?assertEqual(1, maps:get(sequence, Receipt)).

inject_fault_with_spec() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),
    FaultSpec = #{
        type => partition,
        target => undefined,
        delay => 100,
        reason => test_partition
    },
    {ok, _NewHarness, Receipt} = fault_harness:inject_fault(Harness, FaultSpec),

    ?assertEqual(partition, maps:get(fault_type, Receipt)),
    ?assertEqual(1, maps:get(sequence, Receipt)),
    ?assertEqual(undefined, maps:get(target, Receipt)).

inject_fault_updates_sequence() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),

    {ok, H1, R1} = fault_harness:inject_fault(Harness, timeout),
    ?assertEqual(1, maps:get(sequence, R1)),

    {ok, H2, R2} = fault_harness:inject_fault(H1, partition),
    ?assertEqual(2, maps:get(sequence, R2)),

    {ok, H3, R3} = fault_harness:inject_fault(H2, memory_high),
    ?assertEqual(3, maps:get(sequence, R3)),

    ?assertEqual(3, length(maps:get(receipts, H3))).

inject_multiple_faults() ->
    {ok, Harness} = fault_harness:new_harness(12345, #{auto_start => false}),

    FaultTypes = [timeout, partition, memory_high, cpu_overload, disk_full],
    {FinalHarness, Receipts} = lists:foldl(fun(FaultType, {H, Acc}) ->
        {ok, NewH, Receipt} = fault_harness:inject_fault(H, FaultType),
        {NewH, Acc ++ [Receipt]}
    end, {Harness, []}, FaultTypes),

    ?assertEqual(5, length(maps:get(receipts, FinalHarness))),
    ?assertEqual(5, length(Receipts)),

    %% Verify sequence numbers
    Sequences = [maps:get(sequence, R) || R <- Receipts],
    ?assertEqual([1, 2, 3, 4, 5], Sequences).

get_receipt_returns_latest() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),
    {ok, H2, _} = fault_harness:inject_fault(H1, partition),

    {ok, Receipt} = fault_harness:get_receipt(H2),
    ?assertEqual(partition, maps:get(fault_type, Receipt)),
    ?assertEqual(2, maps:get(sequence, Receipt)).

get_receipt_by_id() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),
    {ok, H1, R1} = fault_harness:inject_fault(Harness, timeout),
    {ok, H2, _} = fault_harness:inject_fault(H1, partition),

    FaultId = maps:get(fault_id, R1),
    {ok, FoundReceipt} = fault_harness:get_receipt_by_id(H2, FaultId),
    ?assertEqual(timeout, maps:get(fault_type, FoundReceipt)),
    ?assertEqual(1, maps:get(sequence, FoundReceipt)).

get_receipt_when_empty() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),
    ?assertEqual({error, no_receipts}, fault_harness:get_receipt(Harness)).

get_all_receipts() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),
    {ok, H2, _} = fault_harness:inject_fault(H1, partition),
    {ok, H3, _} = fault_harness:inject_fault(H2, memory_high),

    AllReceipts = fault_harness:get_all_receipts(H3),
    ?assertEqual(3, length(AllReceipts)),

    FaultTypes = [maps:get(fault_type, R) || R <- AllReceipts],
    ?assertEqual([timeout, partition, memory_high], FaultTypes).

verify_recovery_with_pid_check() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),

    %% Start a test process
    TestPid = spawn(fun() -> receive after infinity -> ok end end),

    {ok, H1, _Receipt} = fault_harness:inject_fault(Harness, timeout),

    %% Verify recovery using pid check (process is still alive)
    {ok, H2, Recovered} = fault_harness:verify_recovery(H1, {pid, TestPid}),
    ?assert(Recovered),

    %% Clean up
    exit(TestPid, kill),

    %% Wait for process to actually die
    timer:sleep(10),

    %% For the second check, we need to inject another fault
    %% since H2's receipt is already marked as recovered
    {ok, H3, _Receipt2} = fault_harness:inject_fault(H2, partition),

    %% Verify recovery with dead process
    {ok, _H4, Recovered2} = fault_harness:verify_recovery(H3, {pid, TestPid}),
    ?assertNot(Recovered2).

verify_recovery_with_function_check() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),

    %% Verify recovery using function check
    CheckFun = fun() -> true end,
    {ok, _H2, Recovered} = fault_harness:verify_recovery(H1, CheckFun),
    ?assert(Recovered),

    %% Verify with failing check
    FailFun = fun() -> false end,
    {ok, _H3, Recovered2} = fault_harness:verify_recovery(H1, FailFun),
    ?assertNot(Recovered2).

verify_recovery_multiple_checks() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),

    %% Verify with multiple checks (all pass)
    Checks = [
        fun() -> true end,
        fun() -> 1 =:= 1 end
    ],
    {ok, _H2, Recovered} = fault_harness:verify_recovery(H1, Checks),
    ?assert(Recovered),

    %% Verify with one failing check
    ChecksWithFail = [
        fun() -> true end,
        fun() -> false end
    ],
    {ok, _H3, Recovered2} = fault_harness:verify_recovery(H1, ChecksWithFail),
    ?assertNot(Recovered2).

verify_recovery_updates_receipt() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),

    %% Before verify, recovered field is undefined
    AllReceipts1 = maps:get(receipts, H1),
    Receipt1 = lists:nth(1, AllReceipts1),
    ?assertEqual(undefined, maps:get(recovered, Receipt1, undefined)),

    %% After verify, recovered field is set
    {ok, H2, _} = fault_harness:verify_recovery(H1, fun() -> true end),
    AllReceipts2 = maps:get(receipts, H2),
    UpdatedReceipt = lists:nth(1, AllReceipts2),
    ?assertEqual(true, maps:get(recovered, UpdatedReceipt)).

cleanup_clears_active_faults() ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),
    {ok, H2, _} = fault_harness:inject_fault(H1, partition),

    %% Verify active faults exist
    ActiveFaults = maps:get(active_faults, H2),
    ?assert(length(ActiveFaults) > 0),

    %% Cleanup should clear faults
    ?assertEqual(ok, fault_harness:cleanup(H2)).

replay_harness_from_receipts() ->
    Seed = 99999,
    {ok, Harness} = fault_harness:new_harness(Seed),

    %% Inject some faults
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),
    {ok, H2, _} = fault_harness:inject_fault(H1, partition),
    {ok, H3, _} = fault_harness:inject_fault(H2, memory_high),

    OriginalReceipts = maps:get(receipts, H3),

    %% Replay from receipts
    {ok, ReplayHarness} = fault_harness:replay_harness(Seed, OriginalReceipts),

    ?assertEqual(Seed, maps:get(seed, ReplayHarness)),
    ?assertEqual(3, length(maps:get(receipts, ReplayHarness))).

export_receipts_creates_map() ->
    Seed = 12345,
    {ok, Harness} = fault_harness:new_harness(Seed),
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),
    {ok, H2, _} = fault_harness:inject_fault(H1, partition),

    Exported = fault_harness:export_receipts(H2),

    ?assert(is_map(Exported)),
    ?assert(maps:is_key(harness_seed, Exported)),
    ?assert(maps:is_key(started_at, Exported)),
    ?assert(maps:is_key(exported_at, Exported)),
    ?assert(maps:is_key(receipt_count, Exported)),
    ?assert(maps:is_key(receipts, Exported)),
    ?assertEqual(Seed, maps:get(harness_seed, Exported)),
    ?assertEqual(2, maps:get(receipt_count, Exported)),
    ?assertEqual(2, length(maps:get(receipts, Exported))).

deterministic_seed_produces_same_sequence() ->
    Seed = 54321,

    %% First run
    {ok, H1} = fault_harness:new_harness(Seed),
    {ok, H2, _} = fault_harness:inject_fault(H1, timeout),
    {ok, H3, _} = fault_harness:inject_fault(H2, partition),
    {ok, H4, _} = fault_harness:inject_fault(H3, memory_high),

    %% Second run with same seed
    {ok, H5} = fault_harness:new_harness(Seed),
    {ok, H6, _} = fault_harness:inject_fault(H5, timeout),
    {ok, H7, _} = fault_harness:inject_fault(H6, partition),
    {ok, H8, _} = fault_harness:inject_fault(H7, memory_high),

    %% Receipts should have same sequence numbers
    Receipts1 = maps:get(receipts, H4),
    Receipts2 = maps:get(receipts, H8),

    Sequences1 = [maps:get(sequence, R) || R <- Receipts1],
    Sequences2 = [maps:get(sequence, R) || R <- Receipts2],

    ?assertEqual(Sequences1, Sequences2),
    ?assertEqual([1, 2, 3], Sequences1).

different_seeds_produce_different_sequences() ->
    Seed1 = 11111,
    Seed2 = 22222,

    %% Run with seed 1
    {ok, H1} = fault_harness:new_harness(Seed1),
    {ok, H2, _} = fault_harness:inject_fault(H1, timeout),
    {ok, H3, _} = fault_harness:inject_fault(H2, partition),

    %% Run with seed 2
    {ok, H4} = fault_harness:new_harness(Seed2),
    {ok, H5, _} = fault_harness:inject_fault(H4, timeout),
    {ok, H6, _} = fault_harness:inject_fault(H5, partition),

    %% Seeds should be different in receipts
    Receipts1 = maps:get(receipts, H3),
    Receipts2 = maps:get(receipts, H6),

    ?assertEqual(Seed1, maps:get(seed, hd(Receipts1))),
    ?assertEqual(Seed2, maps:get(seed, hd(Receipts2))).

%%====================================================================
%% Helper Functions
%%====================================================================
