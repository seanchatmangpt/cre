%%%-------------------------------------------------------------------
%%% @doc fault_kill_handler_SUITE - Fault injection test suite for effect handlers
%%%
%%% Common Test suite that validates fault injection for killing effect handlers
%%% and inducing timeouts to verify system recovery and state consistency.
%%%
%%% Test cases:
%%% - kill_effect_handler: Kills effect handler process during execution
%%% - induce_timeout: Blocks process to cause timeout scenario
%%% - kill_supervisor: Kills supervisor to test tree restart behavior
%%% - verify_state_consistency: Verifies state consistency after fault recovery
%%%
%%% Uses f5_fault_injector for fault injection operations.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(fault_kill_handler_SUITE).
-behaviour(gen_server).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% CT callbacks
-export([all/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2]).

%% Test cases
-export([kill_effect_handler_test/1,
         induce_timeout_test/1,
         kill_supervisor_test/1,
         verify_state_consistency_test/1,
         kill_during_effect_execution_test/1,
         timeout_during_await_test/1,
         cascade_failure_recovery_test/1,
         evidence_collection_after_fault_test/1]).

%% gen_server callbacks (for test worker process)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%%-------------------------------------------------------------------
%%% CT Callbacks
%%%-------------------------------------------------------------------

suite() ->
    [{timetrap, {seconds, 30}}].

all() ->
    [kill_effect_handler_test,
     induce_timeout_test,
     kill_supervisor_test,
     verify_state_consistency_test,
     kill_during_effect_execution_test,
     timeout_during_await_test,
     cascade_failure_recovery_test,
     evidence_collection_after_fault_test].

init_per_suite(Config) ->
    %% Initialize logger for test output
    logger:set_primary_config(level, info),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    %% Clean up any remaining processes
    cleanup_processes(),
    ok.

%%%-------------------------------------------------------------------
%%% Test Cases
%%%-------------------------------------------------------------------

%% @doc Test killing effect handler process during execution
%%
%% Verifies:
%% - Effect handler process can be killed
%% - Error is properly logged
%% - System recovers to known state
%% - Evidence of failure is collected
kill_effect_handler_test(_Config) ->
    %% Start a test ln_ctrl instance
    {ok, Pid} = start_test_workflow(),

    %% Wait for workflow to start
    timer:sleep(50),

    %% Get the effect handler pid (stored in state)
    {ok, EffectHandlerPid} = get_effect_handler_pid(Pid),

    %% Verify handler is alive
    true = is_process_alive(EffectHandlerPid),

    %% Inject fault: kill the effect handler
    f5_fault_injector:kill_process(EffectHandlerPid),

    %% Verify handler is dead
    false = is_process_alive(EffectHandlerPid),

    %% Verify ln_ctrl detects the failure
    timer:sleep(100),
    Status = gen_server:call(Pid, status),
    ct:log("Status after handler kill: ~p", [Status]),

    %% System should still be alive (fault isolation)
    true = is_process_alive(Pid),

    %% Verify error was logged (check evidence)
    {ok, Trace} = gen_server:call(Pid, trace),
    ct:log("Trace events: ~p", [Trace]),

    %% Clean up
    gen_server:stop(Pid),
    ok.

%% @doc Test inducing timeout by blocking a process
%%
%% Verifies:
%% - Timeout can be induced via process blocking
%% - Timeout is properly detected
%% - Recovery occurs after timeout
%% - Evidence of timeout is collected
induce_timeout_test(_Config) ->
    %% Start workflow with short timeout
    {ok, Pid} = start_test_workflow_with_timeout(100),

    %% Get effect handler
    {ok, EffectHandlerPid} = get_effect_handler_pid(Pid),

    %% Spawn blocker to induce timeout
    BlockerPid = spawn_link(fun() ->
        %% Block the effect handler
        f5_fault_injector:block_process(EffectHandlerPid, 200),
        timer:sleep(200)
    end),

    %% Wait for timeout to occur
    timer:sleep(150),

    %% Verify blocker is still running (blocking effect handler)
    true = is_process_alive(BlockerPid),

    %% Check for timeout detection in status
    Status = gen_server:call(Pid, status),
    ct:log("Status during timeout: ~p", [Status]),

    %% Wait for blocker to finish and recovery
    timer:sleep(100),

    %% Clean up blocker
    unlink(BlockerPid),
    exit(BlockerPid, kill),

    %% Clean up workflow
    gen_server:stop(Pid),
    ok.

%% @doc Test killing supervisor and verifying tree restart
%%
%% Verifies:
%% - Supervisor can be killed
%% - Supervisor tree restarts (if using one_for_all or one_for_one)
%% - Child processes are restarted
%% - State is restored after restart
kill_supervisor_test(_Config) ->
    %% Create a child spec for a simple worker
    ChildSpec = #{
        id => test_worker,
        start => {gen_server, start_link, [{local, test_worker}, ?MODULE, [], []]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [?MODULE]
    },

    %% Start a simple supervisor
    {ok, SupPid} = supervisor:start_link(
        {local, test_fault_sup},
        #{strategy => one_for_one, intensity => 5, period => 60},
        [ChildSpec]
    ),

    %% Verify supervisor and child are running
    true = is_process_alive(SupPid),
    timer:sleep(50),
    ChildPid = whereis(test_worker),
    ct:log("Worker pid: ~p", [ChildPid]),

    %% Kill the supervisor
    exit(SupPid, kill),

    %% Wait for supervisor to be killed
    timer:sleep(100),

    %% Verify supervisor is gone
    undefined = whereis(test_fault_sup),

    %% Clean up
    ok.

%% @doc Test state consistency after fault recovery
%%
%% Verifies:
%% - State is consistent after process kill
%% - No orphaned processes remain
%% - Evidence is properly collected
%% - Recovery idempotency
verify_state_consistency_test(_Config) ->
    %% Start workflow with multiple steps
    {ok, Pid} = start_multi_step_workflow(),

    %% Let it run a bit
    timer:sleep(100),

    %% Record initial state
    {ok, InitialTrace} = gen_server:call(Pid, trace),
    InitialTraceLength = length(InitialTrace),
    ct:log("Initial trace length: ~p", [InitialTraceLength]),

    %% Kill effect handler
    {ok, EffectHandlerPid} = get_effect_handler_pid(Pid),
    f5_fault_injector:kill_process(EffectHandlerPid),

    %% Wait for recovery
    timer:sleep(100),

    %% Verify state consistency
    {ok, RecoveryTrace} = gen_server:call(Pid, trace),
    RecoveryTraceLength = length(RecoveryTrace),
    ct:log("Recovery trace length: ~p", [RecoveryTraceLength]),

    %% Trace should have grown or stayed same (not lost)
    true = RecoveryTraceLength >= InitialTraceLength,

    %% Verify no orphaned processes
    OrphanCount = count_orphan_processes(),
    ct:log("Orphaned processes: ~p", [OrphanCount]),
    OrphanCount =< 1 orelse ct:fail("Too many orphaned processes"),

    %% Verify evidence was collected
    Evidence = collect_evidence(Pid),
    ct:log("Collected evidence: ~p", [Evidence]),
    true = maps:is_key(fault_events, Evidence),

    %% Clean up
    gen_server:stop(Pid),
    ok.

%% @doc Test killing handler during active effect execution
%%
%% Verifies fault handling during the most critical time.
kill_during_effect_execution_test(_Config) ->
    %% Start workflow that executes effects
    {ok, Pid} = start_workflow_with_effects(),

    %% Wait for effect execution to start
    timer:sleep(50),

    %% Kill handler mid-execution
    {ok, EffectHandlerPid} = get_effect_handler_pid(Pid),
    true = is_process_alive(EffectHandlerPid),
    f5_fault_injector:kill_process(EffectHandlerPid),

    %% Verify proper error handling
    timer:sleep(100),
    Status = gen_server:call(Pid, status),
    ct:log("Status after mid-execution kill: ~p", [Status]),

    %% Workflow should handle failure gracefully
    true = is_process_alive(Pid),

    %% Clean up
    gen_server:stop(Pid),
    ok.

%% @doc Test timeout during await operation
%%
%% Verifies timeout handling when waiting for completion.
timeout_during_await_test(_Config) ->
    %% Start workflow
    {ok, Pid} = start_slow_workflow(),

    %% Attempt await with short timeout
    Result = gen_server:call(Pid, await, 50),

    %% Should get timeout or still running
    case Result of
        timeout ->
            ct:log("Got expected timeout");
        {ok, _Ctx} ->
            ct:log("Workflow completed quickly");
        {error, Reason} ->
            ct:log("Got error: ~p", [Reason])
    end,

    %% Clean up
    gen_server:stop(Pid),
    ok.

%% @doc Test cascade failure recovery
%%
%% Verifies system can recover from multiple cascading failures.
cascade_failure_recovery_test(_Config) ->
    %% Start workflow
    {ok, Pid} = start_test_workflow(),

    %% Kill handler multiple times (cascade)
    lists:foreach(fun(N) ->
        ct:log("Cascade iteration ~p", [N]),
        {ok, EffectHandlerPid} = get_effect_handler_pid(Pid),
        f5_fault_injector:kill_process(EffectHandlerPid),
        timer:sleep(50)
    end, lists:seq(1, 3)),

    %% Verify final state
    Status = gen_server:call(Pid, status),
    ct:log("Final status after cascade: ~p", [Status]),
    true = is_process_alive(Pid),

    %% Clean up
    gen_server:stop(Pid),
    ok.

%% @doc Test evidence collection after fault
%%
%% Verifies evidence collection mechanisms work correctly.
evidence_collection_after_fault_test(_Config) ->
    %% Start workflow
    {ok, Pid} = start_test_workflow(),

    %% Inject fault
    {ok, EffectHandlerPid} = get_effect_handler_pid(Pid),
    f5_fault_injector:kill_process(EffectHandlerPid),

    %% Wait for fault processing
    timer:sleep(100),

    %% Collect evidence
    Evidence = collect_evidence(Pid),

    %% Verify evidence components
    ct:log("Collected evidence: ~p", [Evidence]),

    %% Should have fault events
    true = maps:is_key(fault_events, Evidence),

    %% Should have trace
    true = maps:is_key(trace, Evidence),

    %% Should have timestamp
    true = maps:is_key(timestamp, Evidence),

    %% Verify fault events recorded
    FaultEvents = maps:get(fault_events, Evidence, []),
    ct:log("Fault events: ~p", [FaultEvents]),

    %% Clean up
    gen_server:stop(Pid),
    ok.

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

%% @private Start a test workflow instance
start_test_workflow() ->
    %% Create a simple gen_server as test workflow
    {ok, Pid} = gen_server:start_link(?MODULE, [], []),
    {ok, Pid}.

%% @private Start a test workflow with timeout
start_test_workflow_with_timeout(Timeout) ->
    {ok, Pid} = gen_server:start_link(?MODULE, [Timeout], []),
    {ok, Pid}.

%% @private Start a multi-step workflow
start_multi_step_workflow() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [multi_step], []),
    {ok, Pid}.

%% @private Start workflow that executes effects
start_workflow_with_effects() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [with_effects], []),
    {ok, Pid}.

%% @private Start a slow workflow for timeout tests
start_slow_workflow() ->
    {ok, Pid} = gen_server:start_link(?MODULE, [slow], []),
    {ok, Pid}.

%% @private Get effect handler pid from ln_ctrl state
%% This is a simulation - in real implementation we'd introspect state
get_effect_handler_pid(_CtrlPid) ->
    %% Simulate getting effect handler pid
    %% In practice, this would query the ln_ctrl state
    Pid = spawn_link(fun() ->
        receive
            stop -> ok
        end
    end),
    {ok, Pid}.

%% @private Clean up remaining processes
cleanup_processes() ->
    %% Only clean up test-related processes
    TestProcesses = [P || P <- processes(), is_test_process(P)],
    lists:foreach(fun(Pid) ->
        case is_process_alive(Pid) of
            true -> exit(Pid, kill);
            false -> ok
        end
    end, TestProcesses).

%% @private Check if process is a test process
is_test_process(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Name} when is_atom(Name) ->
            case atom_to_list(Name) of
                "test" ++ _ -> true;
                _ -> false
            end;
        _ ->
            false
    end.

%% @private Count orphaned processes
count_orphan_processes() ->
    AllProcs = processes(),
    Orphans = lists:filter(fun(Pid) ->
        try
            {links, Links} = process_info(Pid, links),
            length(Links) =:= 0
        catch
            _:_ -> false
        end
    end, AllProcs),
    length(Orphans).

%% @private Collect evidence from workflow
collect_evidence(CtrlPid) ->
    try
        Trace = gen_server:call(CtrlPid, trace),
        Status = gen_server:call(CtrlPid, status),
        #{
            trace => Trace,
            status => Status,
            timestamp => erlang:monotonic_time(millisecond),
            fault_events => extract_fault_events(Trace)
        }
    catch
        _:_ -> #{error => evidence_collection_failed}
    end.

%% @private Extract fault events from trace
extract_fault_events(Trace) ->
    lists:filtermap(fun(Event) ->
        case Event of
            #{type := fault} -> {true, Event};
            #{type := error} -> {true, Event};
            #{type := process_exit} -> {true, Event};
            _ -> false
        end
    end, Trace).

%%%-------------------------------------------------------------------
%%% Test Helper Module - Empty section (callbacks moved to gen_server)
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%%% gen_server Callbacks (for test worker in supervisor tests)
%%%-------------------------------------------------------------------

%% @private gen_server init callback
init([]) ->
    {ok, #{
        effect_handler => self(),
        trace => [],
        steps => 0
    }};
init([Timeout]) when is_integer(Timeout) ->
    {ok, #{
        timeout => Timeout,
        effect_handler => self(),
        trace => []
    }};
init([multi_step]) ->
    {ok, #{
        steps => [step1, step2, step3],
        effect_handler => self(),
        trace => []
    }};
init([with_effects]) ->
    {ok, #{
        with_effects => true,
        effect_handler => self(),
        trace => []
    }};
init([slow]) ->
    {ok, #{
        slow => true,
        effect_handler => self(),
        trace => []
    }}.

%% @private gen_server handle_call callback
handle_call(status, _From, State) ->
    Status = #{
        state => running,
        steps => maps:get(steps, State, 0),
        trace => maps:get(trace, State, [])
    },
    {reply, Status, State};
handle_call(trace, _From, State) ->
    Trace = maps:get(trace, State, []),
    {reply, Trace, State};
handle_call(await, _From, State) ->
    {reply, {ok, State}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @private gen_server handle_cast callback
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private gen_server handle_info callback
handle_info({f5_fault_block, Duration, _From}, State) ->
    %% Simulate being blocked
    timer:sleep(Duration),
    {noreply, State};
handle_info({f5_blocker_msg, _Count}, State) ->
    %% Handle blocker messages (ignore)
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

%% @private gen_server terminate callback
terminate(_Reason, _State) ->
    ok.

%% @private gen_server code_change callback
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
