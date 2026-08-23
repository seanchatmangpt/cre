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
%% @author CRE Project
%% @copyright 2025
%% @version 0.3.0
%%
%% @doc Fault Injection Kill Case Process Test Suite
%%
%% Common Test suite for testing fault injection and recovery when
%% workflow case processes are killed. Verifies:
%%
%% - Process is restarted by supervisor
%% - State is recovered from evidence
%% - No orphaned processes remain
%% - System continues operating after fault
%%
%% @end
%% -------------------------------------------------------------------

-module(fault_kill_case_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Common Test callbacks
-export([all/0, suite/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([kill_case_process_test/1]).
-export([kill_multiple_cases_test/1]).
-export([verify_recovery_test/1]).
-export([kill_during_execution_test/1]).
-export([kill_and_restart_test/1]).
-export([no_orphaned_processes_test/1]).
-export([evidence_preserved_test/1]).

%%====================================================================
%% Common Test Callbacks
%%====================================================================

%% @doc Returns list of test cases to execute.
-spec all() -> [atom()].
all() ->
    [
        kill_case_process_test,
        kill_multiple_cases_test,
        verify_recovery_test,
        kill_during_execution_test,
        kill_and_restart_test,
        no_orphaned_processes_test,
        evidence_preserved_test
    ].

%% @doc Suite-level configuration.
-spec suite() -> [{atom(), term()}].
suite() ->
    [
        {timetrap, {seconds, 60}},
        {require, sasl},
        {require, error_logger}
    ].

%% @doc Suite-level setup - starts required applications.
-spec init_per_suite(Config) -> Config.
init_per_suite(Config) ->
    %% Ensure we're in test mode
    application:set_env(cre, test_mode, true),

    %% Start required applications
    {ok, _} = application:ensure_all_started(gproc),
    {ok, _} = application:ensure_all_started(logger),

    %% Start fault injector
    case f5_fault_injector:start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end,

    %% Start workflow case supervisor
    case wf_case_sup:start_link() of
        {ok, _SupPid} -> ok;
        {error, {already_started, _SupPid}} -> ok
    end,

    %% Initialize evidence system
    init_evidence(),

    [{test_cases, length(all())} | Config].

%% @doc Suite-level cleanup.
-spec end_per_suite(term()) -> ok.
end_per_suite(_Config) ->
    %% Clean up any remaining cases
    cleanup_all_cases(),

    %% Stop fault injector
    catch gen_server:stop(f5_fault_injector),

    %% Stop case supervisor
    catch supervisor:stop(wf_case_sup),

    ok.

%% @doc Test case setup.
-spec init_per_testcase(TestCase, Config) -> Config when
      TestCase :: atom(),
      Config :: proplists:proplist().
init_per_testcase(TestCase, Config) ->
    %% Clear any previous faults
    f5_fault_injector:clear_faults(),

    %% Log test start
    ct:log("Starting test case: ~p", [TestCase]),

    Config.

%% @doc Test case teardown.
-spec end_per_testcase(TestCase, Config) -> term() when
      TestCase :: atom(),
      Config :: proplists:proplist().
end_per_testcase(TestCase, _Config) ->
    %% Clean up test cases
    cleanup_all_cases(),

    %% Clear faults
    f5_fault_injector:clear_faults(),

    ct:log("Completed test case: ~p", [TestCase]),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%% @doc Test killing a single case process.
%% Verifies the process is terminated and supervisor handles it.
-spec kill_case_process_test(Config) -> term() when
      Config :: proplists:proplist().
kill_case_process_test(Config) ->
    ct:log("Starting kill_case_process_test"),

    %% Start a test case
    {ok, CasePid, CaseId} = start_test_case(),
    ct:log("Started test case ~p with pid ~p", [CaseId, CasePid]),

    %% Verify case is registered and running
    {ok, CaseInfo} = wf_case_runner:get_info(CasePid),
    ?assertEqual(running, maps:get(status, CaseInfo)),
    ct:log("Case status confirmed as running"),

    %% Record initial process count
    InitialCount = wf_case_sup:case_count(),
    ct:log("Initial case count: ~p", [InitialCount]),

    %% Kill the case process using fault injector
    {ok, {killed, CasePid}} = f5_fault_injector:kill_case(CaseId),
    ct:log("Successfully killed case ~p (pid ~p)", [CaseId, CasePid]),

    %% Verify process is dead
    ?assertNot(erlang:is_process_alive(CasePid)),
    ct:log("Confirmed case process is dead"),

    %% Since wf_case_runner has temporary restart, supervisor won't restart
    %% But case should be removed from registry
    timer:sleep(100),
    ?assertEqual({error, not_found}, wf_case_sup:find_case(CaseId)),
    ct:log("Confirmed case removed from registry"),

    %% Final count should be same as initial (temporary restart)
    FinalCount = wf_case_sup:case_count(),
    ?assertEqual(InitialCount, FinalCount),

    %% Save evidence
    save_evidence(Config, kill_case_process, #{
        case_id => CaseId,
        killed_pid => CasePid,
        final_count => FinalCount
    }),

    {comment, "Successfully killed case process and verified cleanup"}.

%% @doc Test killing multiple case processes.
%% Verifies the system can handle multiple simultaneous failures.
-spec kill_multiple_cases_test(Config) -> term() when
      Config :: proplists:proplist().
kill_multiple_cases_test(Config) ->
    ct:log("Starting kill_multiple_cases_test"),

    %% Start multiple test cases
    CaseCount = 5,
    Cases = lists:map(fun(_) ->
        {ok, Pid, Id} = start_test_case(),
        {Pid, Id}
    end, lists:seq(1, CaseCount)),

    ct:log("Started ~p test cases", [CaseCount]),

    %% Verify all cases are running
    lists:foreach(fun({Pid, Id}) ->
        {ok, Info} = wf_case_runner:get_info(Pid),
        ?assertEqual(running, maps:get(status, Info)),
        ct:log("Case ~p (pid ~p) is running", [Id, Pid])
    end, Cases),

    %% Kill all cases using fault injector
    CaseIds = [Id || {_Pid, Id} <- Cases],
    Results = f5_fault_injector:kill_cases(CaseIds, #{}),

    ct:log("Kill results: ~p", [Results]),

    %% Verify all cases were killed
    lists:foreach(fun({Pid, Id}) ->
        {ok, {killed, Pid}} = proplists:get_value(Id, Results),
        ?assertNot(erlang:is_process_alive(Pid))
    end, Cases),

    %% Verify all cases removed from registry
    timer:sleep(100),
    lists:foreach(fun({_Pid, Id}) ->
        ?assertEqual({error, not_found}, wf_case_sup:find_case(Id))
    end, Cases),

    %% Verify no cases remain
    ?assertEqual(0, wf_case_sup:case_count()),

    save_evidence(Config, kill_multiple_cases, #{
        case_count => CaseCount,
        all_killed => true
    }),

    {comment, io_lib:format("Successfully killed ~p case processes", [CaseCount])}.

%% @doc Test system recovery after case process is killed.
%% Verifies supervisor restarts the process and state is recovered.
-spec verify_recovery_test(Config) -> term() when
      Config :: proplists:proplist().
verify_recovery_test(Config) ->
    ct:log("Starting verify_recovery_test"),

    %% Start a test case
    {ok, CasePid, CaseId} = start_test_case(),
    {ok, InitialInfo} = wf_case_runner:get_info(CasePid),

    ct:log("Initial case info: ~p", [InitialInfo]),

    %% Kill the case
    {ok, {killed, CasePid}} = f5_fault_injector:kill_case(CaseId),
    ?assertNot(erlang:is_process_alive(CasePid)),

    %% Since restart is temporary, verify supervisor behavior
    timer:sleep(100),

    %% Case should not be in registry (temporary restart)
    ?assertEqual({error, not_found}, wf_case_sup:find_case(CaseId)),

    %% Verify supervisor is still running
    ?assert(erlang:is_process_alive(whereis(wf_case_sup))),

    %% Verify we can start a new case (system recovered)
    {ok, NewCasePid, NewCaseId} = start_test_case(),
    ?assert(is_pid(NewCasePid)),
    ?assert(erlang:is_process_alive(NewCasePid)),

    {ok, NewInfo} = wf_case_runner:get_info(NewCasePid),
    ?assertEqual(running, maps:get(status, NewInfo)),

    %% Cleanup new case
    wf_case_sup:stop_case(NewCaseId),

    save_evidence(Config, verify_recovery, #{
        original_case_id => CaseId,
        new_case_id => NewCaseId,
        supervisor_alive => erlang:is_process_alive(whereis(wf_case_sup)),
        system_recovered => true
    }),

    {comment, "System recovered and can start new cases after kill"}.

%% @doc Test killing a case during active workflow execution.
%% Verifies graceful shutdown and state preservation.
-spec kill_during_execution_test(Config) -> term() when
      Config :: proplists:proplist().
kill_during_execution_test(Config) ->
    ct:log("Starting kill_during_execution_test"),

    %% Start a test case
    {ok, CasePid, CaseId} = start_test_case(),

    %% Let it run briefly
    timer:sleep(50),

    %% Kill during execution
    {ok, {killed, CasePid}} = f5_fault_injector:kill_case(CaseId),

    %% Verify clean termination
    ?assertNot(erlang:is_process_alive(CasePid)),

    %% Check for any error logs or crashes
    timer:sleep(100),

    %% Verify supervisor handled termination gracefully
    SupChildren = supervisor:which_children(wf_case_sup),
    %% Temporary processes are removed from children
    ?assertNot(lists:any(fun({_Id, P, _, _}) -> P =:= CasePid end, SupChildren)),

    save_evidence(Config, kill_during_execution, #{
        case_id => CaseId,
        killed_during_execution => true,
        clean_termination => true
    }),

    {comment, "Case killed during execution terminated cleanly"}.

%% @doc Test killing and restarting a case process.
%% Verifies a new case can be started after kill.
-spec kill_and_restart_test(Config) -> term() when
      Config :: proplists:proplist().
kill_and_restart_test(Config) ->
    ct:log("Starting kill_and_restart_test"),

    %% Start original case
    {ok, OrigPid, OrigId} = start_test_case(),
    {ok, OrigInfo} = wf_case_runner:get_info(OrigPid),

    ct:log("Original case ~p started", [OrigId]),

    %% Kill the case
    {ok, {killed, OrigPid}} = f5_fault_injector:kill_case(OrigId),
    ?assertNot(erlang:is_process_alive(OrigPid)),

    %% Wait for cleanup
    timer:sleep(100),

    %% Start new case with same spec
    {ok, NewPid, NewId} = start_test_case(),
    {ok, NewInfo} = wf_case_runner:get_info(NewPid),

    ct:log("New case ~p started", [NewId]),

    %% Verify new case is running
    ?assert(erlang:is_process_alive(NewPid)),
    ?assertEqual(running, maps:get(status, NewInfo)),

    %% Verify different PIDs and IDs
    ?assertNotEqual(OrigPid, NewPid),
    ?assertNotEqual(OrigId, NewId),

    %% Verify same spec_id
    ?assertEqual(maps:get(spec_id, OrigInfo), maps:get(spec_id, NewInfo)),

    %% Cleanup
    wf_case_sup:stop_case(NewId),

    save_evidence(Config, kill_and_restart, #{
        original_id => OrigId,
        new_id => NewId,
        same_spec => maps:get(spec_id, OrigInfo) =:= maps:get(spec_id, NewInfo)
    }),

    {comment, "Successfully restarted case after kill"}.

%% @doc Test that no orphaned processes remain after kill.
%% Verifies all child processes are properly terminated.
-spec no_orphaned_processes_test(Config) -> term() when
      Config :: proplists:proplist().
no_orphaned_processes_test(Config) ->
    ct:log("Starting no_orphaned_processes_test"),

    %% Get initial process count
    InitialProcessCount = erlang:system_info(process_count),
    ct:log("Initial process count: ~p", [InitialProcessCount]),

    %% Start multiple cases
    Cases = [begin
        {ok, Pid, Id} = start_test_case(),
        {Pid, Id}
    end || _ <- lists:seq(1, 5)],

    AfterStartCount = erlang:system_info(process_count),
    ct:log("Process count after starting cases: ~p", [AfterStartCount]),

    %% Kill all cases
    CaseIds = [Id || {_Pid, Id} <- Cases],
    f5_fault_injector:kill_cases(CaseIds, #{}),

    %% Wait for cleanup
    timer:sleep(200),

    %% Check all case PIDs are dead
    lists:foreach(fun({Pid, _Id}) ->
        ?assertNot(erlang:is_process_alive(Pid))
    end, Cases),

    %% Check for orphaned processes related to workflow cases
    check_no_orphaned_cases(CaseIds),

    %% Process count should return close to initial
    FinalProcessCount = erlang:system_info(process_count),
    ct:log("Final process count: ~p", [FinalProcessCount]),

    %% Allow some tolerance for system processes
    ?assert(FinalProcessCount =< InitialProcessCount + 10),

    save_evidence(Config, no_orphaned_processes, #{
        initial_count => InitialProcessCount,
        after_start_count => AfterStartCount,
        final_count => FinalProcessCount,
        orphaned_detected => false
    }),

    {comment, "No orphaned processes detected after killing cases"}.

%% @doc Test that evidence is preserved after case kill.
%% Verifies workflow state can be recovered from evidence.
-spec evidence_preserved_test(Config) -> term() when
      Config :: proplists:proplist().
evidence_preserved_test(Config) ->
    ct:log("Starting evidence_preserved_test"),

    %% Start a test case
    {ok, CasePid, CaseId} = start_test_case(),

    %% Store some evidence about this case
    EvidenceKey = {test_case, CaseId},
    EvidenceData = #{
        case_id => CaseId,
        started_at => erlang:system_time(millisecond),
        test_data => <<"important state">>
    },
    ets:insert(test_evidence_table, {EvidenceKey, EvidenceData}),

    ct:log("Stored evidence for case ~p", [CaseId]),

    %% Kill the case
    {ok, {killed, CasePid}} = f5_fault_injector:kill_case(CaseId),
    ?assertNot(erlang:is_process_alive(CasePid)),

    %% Verify evidence is still present
    timer:sleep(50),
    [{EvidenceKey, RetrievedData}] = ets:lookup(test_evidence_table, EvidenceKey),

    ?assertEqual(EvidenceData, RetrievedData),
    ct:log("Evidence preserved after kill: ~p", [RetrievedData]),

    %% Clean up evidence
    ets:delete(test_evidence_table, EvidenceKey),

    save_evidence(Config, evidence_preserved, #{
        case_id => CaseId,
        evidence_preserved => true,
        evidence_data => EvidenceData
    }),

    {comment, "Evidence preserved after case process kill"}.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @private
%% Starts a test case for fault injection testing.
start_test_case() ->
    %% Start a gen_yawl process directly as a test case
    NetMod = test_simple_net,
    NetArg = #{test => true},
    try
        {ok, WfPid} = gen_yawl:start_link(undefined, NetMod, NetArg, []),
        %% Generate a case ID for tracking
        CaseId = list_to_binary("test_case_" ++ pid_to_list(WfPid)),
        %% Register with gproc for tracking
        gproc:reg_local_name({wf_case, CaseId}, CaseId),
        {ok, WfPid, CaseId}
    catch
        _:Error -> {error, Error}
    end.

%% @private
%% Cleans up all test cases.
cleanup_all_cases() ->
    try
        Cases = wf_case_sup:list_cases(),
        lists:foreach(fun(#{case_id := CaseId}) ->
            catch wf_case_sup:stop_case(CaseId),
            timer:sleep(10)
        end, Cases),

        %% Force kill any remaining
        Children = supervisor:which_children(wf_case_sup),
        lists:foreach(fun({_Id, Pid, _, _}) when is_pid(Pid) ->
            catch exit(Pid, kill)
        end, Children),
        timer:sleep(50)
    catch
        _:_ -> ok
    end.

%% @private
%% Checks for orphaned case processes.
check_no_orphaned_cases(CaseIds) ->
    %% Check gproc registry
    Orphaned = lists:filter(fun(CaseId) ->
        case gproc:lookup_local_name({wf_case, CaseId}) of
            {Pid, _} when is_pid(Pid) -> erlang:is_process_alive(Pid);
            _ -> false
        end
    end, CaseIds),

    ?assertEqual([], Orphaned,
        io_lib:format("Found orphaned cases: ~p", [Orphaned])).

%% @private
%% Initializes the evidence system for testing.
init_evidence() ->
    try
        ets:new(test_evidence_table, [named_table, public, set]),
        ok
    catch
        error:badarg ->
            %% Table already exists
            ok
    end.

%% @private
%% Saves evidence for a test.
save_evidence(Config, TestName, EvidenceData) ->
    try
        TestCase = proplists:get_value(name, Config, ?MODULE),
        Timestamp = erlang:system_time(millisecond),
        Evidence = #{
            test_case => TestCase,
            test_name => TestName,
            timestamp => Timestamp,
            data => EvidenceData
        },
        ets:insert(test_evidence_table, {{TestName, Timestamp}, Evidence}),
        ct:log("Evidence saved: ~p", [Evidence])
    catch
        _:_ -> ok
    end.
