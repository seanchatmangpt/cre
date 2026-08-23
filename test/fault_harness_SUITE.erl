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
%% @doc Fault Injection Harness Common Test Suite
%%
%% Common Test suite for fault_harness module with deterministic
%% fault injection testing and receipt tracking.
%%
%% @end
%% -------------------------------------------------------------------

-module(fault_harness_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Common Test callbacks
-export([all/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([deterministic_fault_sequence/1]).
-export([receipt_tracking/1]).
-export([recovery_verification/1]).
-export([fault_types/1]).
-export([harness_cleanup/1]).
-export([export_and_replay/1]).
-export([seed_reproducibility/1]).
-export([pid_recovery_check/1]).
-export([function_recovery_check/1]).
-export([multiple_recovery_checks/1]).

%%====================================================================
%% Common Test Callbacks
%%====================================================================

%% @doc Returns list of test cases to execute.
-spec all() -> [atom()].
all() ->
    [
        deterministic_fault_sequence,
        receipt_tracking,
        recovery_verification,
        fault_types,
        harness_cleanup,
        export_and_replay,
        seed_reproducibility,
        pid_recovery_check,
        function_recovery_check,
        multiple_recovery_checks
    ].

%% @doc Suite-level configuration.
-spec suite() -> [{atom(), term()}].
suite() ->
    [
        {timetrap, {seconds, 30}}
    ].

%% @doc Suite-level setup.
-spec init_per_suite(Config) -> Config.
init_per_suite(Config) ->
    Config.

%% @doc Suite-level cleanup.
-spec end_per_suite(_) -> term().
end_per_suite(_) ->
    ok.

%% @doc Test case setup.
-spec init_per_testcase(TestCase, Config) -> Config when
      TestCase :: atom(),
      Config :: proplists:proplist().
init_per_testcase(TestCase, Config) ->
    ct:log("Starting test case: ~p", [TestCase]),
    Config.

%% @doc Test case teardown.
-spec end_per_testcase(TestCase, Config) -> term() when
      TestCase :: atom(),
      Config :: proplists:proplist().
end_per_testcase(TestCase, _Config) ->
    ct:log("Completed test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Tests deterministic fault injection with fixed seed.
-spec deterministic_fault_sequence(Config) -> term() when
      Config :: proplists:proplist().
deterministic_fault_sequence(_Config) ->
    Seed = 12345,
    {ok, Harness} = fault_harness:new_harness(Seed, #{auto_start => false}),

    %% Inject sequence of faults
    {ok, H1, R1} = fault_harness:inject_fault(Harness, timeout),
    {ok, H2, R2} = fault_harness:inject_fault(H1, partition),
    {ok, _H3, R3} = fault_harness:inject_fault(H2, memory_high),

    %% Verify sequence numbers
    ?assertEqual(1, maps:get(sequence, R1)),
    ?assertEqual(2, maps:get(sequence, R2)),
    ?assertEqual(3, maps:get(sequence, R3)),

    %% Verify seed is consistent
    ?assertEqual(Seed, maps:get(seed, R1)),
    ?assertEqual(Seed, maps:get(seed, R2)),
    ?assertEqual(Seed, maps:get(seed, R3)),

    {comment, "Deterministic fault sequence verified"}.

%% @doc Tests receipt tracking throughout fault injection.
-spec receipt_tracking(Config) -> term() when
      Config :: proplists:proplist().
receipt_tracking(_Config) ->
    {ok, Harness} = fault_harness:new_harness(42, #{auto_start => false}),

    %% Inject faults
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),
    {ok, H2, _} = fault_harness:inject_fault(H1, partition),

    %% Get all receipts
    AllReceipts = fault_harness:get_all_receipts(H2),
    ?assertEqual(2, length(AllReceipts)),

    %% Get latest receipt
    {ok, Latest} = fault_harness:get_receipt(H2),
    ?assertEqual(partition, maps:get(fault_type, Latest)),

    %% Get receipt by ID
    FirstReceipt = lists:nth(1, AllReceipts),
    FaultId = maps:get(fault_id, FirstReceipt),
    {ok, FoundReceipt} = fault_harness:get_receipt_by_id(H2, FaultId),
    ?assertEqual(timeout, maps:get(fault_type, FoundReceipt)),

    {comment, "Receipt tracking verified"}.

%% @doc Tests recovery verification functionality.
-spec recovery_verification(Config) -> term() when
      Config :: proplists:proplist().
recovery_verification(_Config) ->
    {ok, Harness} = fault_harness:new_harness(999, #{auto_start => false}),

    %% Inject fault
    {ok, H1, _} = fault_harness:inject_fault(Harness, cpu_overload),

    %% Verify with passing check
    CheckFun = fun() -> true end,
    {ok, H2, Recovered} = fault_harness:verify_recovery(H1, CheckFun),
    ?assert(Recovered),

    %% Verify receipt was updated
    AllReceipts = fault_harness:get_all_receipts(H2),
    UpdatedReceipt = lists:nth(1, AllReceipts),
    ?assertEqual(true, maps:get(recovered, UpdatedReceipt)),

    {comment, "Recovery verification verified"}.

%% @doc Tests all fault types.
-spec fault_types(Config) -> term() when
      Config :: proplists:proplist().
fault_types(_Config) ->
    FaultTypes = [
        {kill, kill},
        {exit, exit},
        {timeout, timeout},
        {message_dropped, message_dropped},
        {partition, partition},
        {memory_high, memory_high},
        {cpu_overload, cpu_overload},
        {disk_full, disk_full}
    ],

    lists:foreach(fun({Name, Type}) ->
        {ok, Harness} = fault_harness:new_harness(123, #{auto_start => false}),
        {ok, _H, Receipt} = fault_harness:inject_fault(Harness, Type),
        ?assertEqual(Type, maps:get(fault_type, Receipt)),
        ct:log("Fault type ~p: ~p", [Name, Type])
    end, FaultTypes),

    {comment, io_lib:format("All ~p fault types tested", [length(FaultTypes)])}.

%% @doc Tests harness cleanup functionality.
-spec harness_cleanup(Config) -> term() when
      Config :: proplists:proplist().
harness_cleanup(_Config) ->
    {ok, Harness} = fault_harness:new_harness(555, #{auto_start => false}),

    %% Inject some faults
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),
    {ok, H2, _} = fault_harness:inject_fault(H1, partition),

    %% Cleanup should succeed
    ?assertEqual(ok, fault_harness:cleanup(H2)),

    {comment, "Harness cleanup verified"}.

%% @doc Tests export and replay functionality.
-spec export_and_replay(Config) -> term() when
      Config :: proplists:proplist().
export_and_replay(_Config) ->
    Seed = 777,
    {ok, Harness} = fault_harness:new_harness(Seed, #{auto_start => false}),

    %% Inject faults
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),
    {ok, H2, _} = fault_harness:inject_fault(H1, partition),

    %% Export receipts
    Exported = fault_harness:export_receipts(H2),
    ?assertEqual(Seed, maps:get(harness_seed, Exported)),
    ?assertEqual(2, maps:get(receipt_count, Exported)),
    ?assert(is_list(maps:get(receipts, Exported))),

    %% Replay from exported receipts
    OriginalReceipts = maps:get(receipts, Exported),
    {ok, ReplayHarness} = fault_harness:replay_harness(Seed, OriginalReceipts),
    ?assertEqual(Seed, maps:get(seed, ReplayHarness)),
    ?assertEqual(2, length(maps:get(receipts, ReplayHarness))),

    {comment, "Export and replay verified"}.

%% @doc Tests seed reproducibility across runs.
-spec seed_reproducibility(Config) -> term() when
      Config :: proplists:proplist().
seed_reproducibility(_Config) ->
    Seed = 888,

    %% First run
    {ok, H1} = fault_harness:new_harness(Seed, #{auto_start => false}),
    {ok, H2, _} = fault_harness:inject_fault(H1, timeout),
    {ok, H3, _} = fault_harness:inject_fault(H2, partition),
    Receipts1 = fault_harness:get_all_receipts(H3),

    %% Second run with same seed
    {ok, H4} = fault_harness:new_harness(Seed, #{auto_start => false}),
    {ok, H5, _} = fault_harness:inject_fault(H4, timeout),
    {ok, H6, _} = fault_harness:inject_fault(H5, partition),
    Receipts2 = fault_harness:get_all_receipts(H6),

    %% Sequences should match
    Sequences1 = [maps:get(sequence, R) || R <- Receipts1],
    Sequences2 = [maps:get(sequence, R) || R <- Receipts2],
    ?assertEqual(Sequences1, Sequences2),

    {comment, "Seed reproducibility verified"}.

%% @doc Tests PID-based recovery checks.
-spec pid_recovery_check(Config) -> term() when
      Config :: proplists:proplist().
pid_recovery_check(_Config) ->
    {ok, Harness} = fault_harness:new_harness(111, #{auto_start => false}),

    %% Start a test process
    TestPid = spawn(fun() -> receive after infinity -> ok end end),

    %% Inject fault
    {ok, H1, _} = fault_harness:inject_fault(Harness, timeout),

    %% Verify with alive PID
    {ok, H2, Recovered} = fault_harness:verify_recovery(H1, {pid, TestPid}),
    ?assert(Recovered),

    %% Kill process
    exit(TestPid, kill),
    timer:sleep(10),

    %% Inject another fault for second check
    {ok, H3, _} = fault_harness:inject_fault(H2, partition),

    %% Verify with dead PID
    {ok, _H4, Recovered2} = fault_harness:verify_recovery(H3, {pid, TestPid}),
    ?assertNot(Recovered2),

    {comment, "PID recovery check verified"}.

%% @doc Tests function-based recovery checks.
-spec function_recovery_check(Config) -> term() when
      Config :: proplists:proplist().
function_recovery_check(_Config) ->
    {ok, Harness} = fault_harness:new_harness(222, #{auto_start => false}),

    %% Inject fault
    {ok, H1, _} = fault_harness:inject_fault(Harness, memory_high),

    %% Verify with passing function
    {ok, H2, R1} = fault_harness:verify_recovery(H1, fun() -> true end),
    ?assert(R1),

    %% Inject another fault
    {ok, H3, _} = fault_harness:inject_fault(H2, cpu_overload),

    %% Verify with failing function
    {ok, _H4, R2} = fault_harness:verify_recovery(H3, fun() -> false end),
    ?assertNot(R2),

    {comment, "Function recovery check verified"}.

%% @doc Tests multiple recovery checks.
-spec multiple_recovery_checks(Config) -> term() when
      Config :: proplists:proplist().
multiple_recovery_checks(_Config) ->
    {ok, Harness} = fault_harness:new_harness(333, #{auto_start => false}),

    %% Inject fault
    {ok, H1, _} = fault_harness:inject_fault(Harness, disk_full),

    %% Multiple passing checks
    Checks1 = [fun() -> true end, fun() -> 1 =:= 1 end],
    {ok, H2, R1} = fault_harness:verify_recovery(H1, Checks1),
    ?assert(R1),

    %% Inject another fault
    {ok, H3, _} = fault_harness:inject_fault(H2, timeout),

    %% Multiple checks with one failing
    Checks2 = [fun() -> true end, fun() -> false end],
    {ok, _H4, R2} = fault_harness:verify_recovery(H3, Checks2),
    ?assertNot(R2),

    {comment, "Multiple recovery checks verified"}.
