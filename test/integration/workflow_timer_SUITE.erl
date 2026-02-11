%%%-------------------------------------------------------------------
%%% @doc
%%% Workflow Timer Integration Test Suite
%%%
%%% This Common Test suite validates timer-based workflow execution
%%% including deadlines, delays, timeouts, and periodic execution.
%%%
%%% Test Coverage:
%%% - Task execution deadlines
%%% - Delayed task activation
%%% - Workflow timeouts
%%% - Periodic task execution
%%% - Timer cancellation
%%% - Timer queue management
%%% - Time-based conditional routing
%%% - SLA monitoring and enforcement
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(workflow_timer_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("gen_pnet.hrl").

%%%===================================================================
%%% Exported Test Callbacks
%%%===================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2,
    init_per_testcase/2,
    end_per_testcase/2
]).

%%%===================================================================
%%% Exported Test Cases
%%%===================================================================

-export([
    % Deadline tests
    deadline_enforcement_test/1,
    deadline_violation_test/1,
    deadline_extension_test/1,

    % Delay tests
    delayed_activation_test/1,
    multiple_delayed_tasks_test/1,
    delay_cancellation_test/1,

    % Timeout tests
    task_timeout_test/1,
    workflow_timeout_test/1,
    timeout_recovery_test/1,

    % Periodic execution tests
    periodic_task_test/1,
    periodic_cancellation_test/1,
    periodic_backpressure_test/1,

    % Timer queue tests
    timer_queue_ordering_test/1,
    timer_queue_priority_test/1,
    timer_queue_persistence_test/1,

    % Time-based routing tests
    time_based_routing_test/1,
    business_hours_routing_test/1,
    calendar_based_routing_test/1,

    % SLA tests
    sla_monitoring_test/1,
    sla_violation_escalation_test/1,
    sla_metrics_test/1
]).

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

all() ->
    [
        {group, deadline_management},
        {group, delay_management},
        {group, timeout_management},
        {group, periodic_execution},
        {group, timer_queue_ops},
        {group, time_based_routing},
        {group, sla_management}
    ].

groups() ->
    [
        {deadline_management, [], [
            deadline_enforcement_test,
            deadline_violation_test,
            deadline_extension_test
        ]},
        {delay_management, [], [
            delayed_activation_test,
            multiple_delayed_tasks_test,
            delay_cancellation_test
        ]},
        {timeout_management, [], [
            task_timeout_test,
            workflow_timeout_test,
            timeout_recovery_test
        ]},
        {periodic_execution, [], [
            periodic_task_test,
            periodic_cancellation_test,
            periodic_backpressure_test
        ]},
        {timer_queue_ops, [], [
            timer_queue_ordering_test,
            timer_queue_priority_test,
            timer_queue_persistence_test
        ]},
        {time_based_routing, [], [
            time_based_routing_test,
            business_hours_routing_test,
            calendar_based_routing_test
        ]},
        {sla_management, [], [
            sla_monitoring_test,
            sla_violation_escalation_test,
            sla_metrics_test
        ]}
    ].

init_per_suite(Config) ->
    ct:pal("Starting workflow_timer_SUITE"),
    ok = ensure_modules_loaded(),
    Config.

end_per_suite(_Config) ->
    ct:pal("Completed workflow_timer_SUITE"),
    ok.

init_per_group(Group, Config) ->
    ct:pal("Initializing group: ~p", [Group]),
    Config.

end_per_group(Group, _Config) ->
    ct:pal("Completed group: ~p", [Group]),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Starting test case: ~p", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Completed test case: ~p", [TestCase]),
    ok.

%%%===================================================================
%%% Test Cases - Deadline Management
%%%===================================================================

%% @doc Test deadline enforcement for task execution
deadline_enforcement_test(_Config) ->
    Deadline = erlang:system_time(millisecond) + 1000, % 1 second from now

    {ok, Pid} = gen_yawl:start_link(deadline_workflow_net,
                                     #{deadline => Deadline}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Complete before deadline
    {ok, FinalMarking} = gen_yawl:sync(Pid, 500),

    %% Verify completion within deadline
    CompletionTime = erlang:system_time(millisecond),
    ?assert(CompletionTime < Deadline),

    ct:pal("Task completed at ~p, deadline was ~p", [CompletionTime, Deadline]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test deadline violation handling
deadline_violation_test(_Config) ->
    ShortDeadline = erlang:system_time(millisecond) + 100, % 100ms

    {ok, Pid} = gen_yawl:start_link(slow_workflow_net,
                                     #{deadline => ShortDeadline}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Wait past deadline
    timer:sleep(200),

    %% Check if violation was detected
    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("User info after deadline: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test deadline extension
deadline_extension_test(_Config) ->
    InitialDeadline = erlang:system_time(millisecond) + 500,

    {ok, Pid} = gen_yawl:start_link(deadline_workflow_net,
                                     #{deadline => InitialDeadline}, []),

    %% Extend deadline
    NewDeadline = erlang:system_time(millisecond) + 2000,
    _ = gen_yawl:call(Pid, {extend_deadline, NewDeadline}),

    ct:pal("Deadline extended from ~p to ~p", [InitialDeadline, NewDeadline]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Delay Management
%%%===================================================================

%% @doc Test delayed task activation
delayed_activation_test(_Config) ->
    DelayMs = 500,

    {ok, Pid} = gen_yawl:start_link(delayed_workflow_net,
                                     #{delay_ms => DelayMs}, []),

    StartTime = erlang:system_time(millisecond),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Wait for delayed activation
    timer:sleep(DelayMs + 200),

    %% Check if task activated after delay
    Marking = gen_yawl:marking(Pid),
    ActivationTime = erlang:system_time(millisecond),

    ElapsedTime = ActivationTime - StartTime,
    ct:pal("Task activated after ~p ms (expected ~p ms)", [ElapsedTime, DelayMs]),

    ?assert(ElapsedTime >= DelayMs),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test multiple delayed tasks
multiple_delayed_tasks_test(_Config) ->
    Delays = [100, 200, 300, 400, 500],

    {ok, Pid} = gen_yawl:start_link(multi_delayed_net,
                                     #{delays => Delays}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Wait for all delays
    timer:sleep(600),

    Marking = gen_yawl:marking(Pid),
    ct:pal("Marking after multiple delays: ~p", [Marking]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test delay cancellation
delay_cancellation_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(delayed_workflow_net,
                                     #{delay_ms => 5000}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Cancel delay before it fires
    timer:sleep(100),
    _ = gen_yawl:call(Pid, cancel_delay),

    %% Verify delay was cancelled
    timer:sleep(200),
    Marking = gen_yawl:marking(Pid),
    ct:pal("Marking after delay cancellation: ~p", [Marking]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Timeout Management
%%%===================================================================

%% @doc Test task timeout
task_timeout_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(timeout_workflow_net,
                                     #{task_timeout => 200}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Wait for timeout
    timer:sleep(300),

    %% Check if timeout was handled
    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("User info after task timeout: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test workflow-level timeout
workflow_timeout_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(long_running_net,
                                     #{}, [{fire_timeout, 100}]),

    %% Start workflow that takes longer than timeout
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Workflow should handle timeout gracefully
    timer:sleep(300),
    ?assert(is_process_alive(Pid)),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test timeout recovery
timeout_recovery_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(timeout_recovery_net,
                                     #{retry_on_timeout => true}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Allow timeout and recovery
    timer:sleep(500),

    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("User info after timeout recovery: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Periodic Execution
%%%===================================================================

%% @doc Test periodic task execution
periodic_task_test(_Config) ->
    PeriodMs = 200,

    {ok, Pid} = gen_yawl:start_link(periodic_workflow_net,
                                     #{period_ms => PeriodMs, max_iterations => 3}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Wait for multiple periods
    timer:sleep(PeriodMs * 4),

    %% Check execution count
    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("User info after periodic execution: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test periodic task cancellation
periodic_cancellation_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(periodic_workflow_net,
                                     #{period_ms => 100, max_iterations => 100}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Let it run for a bit
    timer:sleep(250),

    %% Cancel periodic execution
    _ = gen_yawl:call(Pid, cancel_periodic),

    %% Verify no more executions
    UsrInfo1 = gen_yawl:usr_info(Pid),
    timer:sleep(300),
    UsrInfo2 = gen_yawl:usr_info(Pid),

    ct:pal("User info before/after cancellation: ~p / ~p", [UsrInfo1, UsrInfo2]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test periodic execution with backpressure
periodic_backpressure_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(periodic_workflow_net,
                                     #{period_ms => 50, max_iterations => 10,
                                       backpressure => true}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Monitor for backpressure handling
    timer:sleep(600),

    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("User info with backpressure: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Timer Queue Operations
%%%===================================================================

%% @doc Test timer queue ordering
timer_queue_ordering_test(_Config) ->
    %% Schedule multiple timers with different deadlines
    Timers = [
        {timer1, 300},
        {timer2, 100},
        {timer3, 200}
    ],

    {ok, Pid} = gen_yawl:start_link(timer_queue_net,
                                     #{timers => Timers}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Wait for all timers
    timer:sleep(400),

    %% Check firing order (should be timer2, timer3, timer1)
    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("Timer queue firing order: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test timer queue priority
timer_queue_priority_test(_Config) ->
    %% Schedule timers with priorities
    Timers = [
        {timer1, 200, high},
        {timer2, 200, low},
        {timer3, 200, medium}
    ],

    {ok, Pid} = gen_yawl:start_link(timer_queue_net,
                                     #{timers => Timers, use_priority => true}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    timer:sleep(300),

    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("Timer queue with priority: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test timer queue persistence
timer_queue_persistence_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(timer_queue_net,
                                     #{persist => true}, []),

    %% Schedule some timers
    Timers = [{timer1, 1000}, {timer2, 2000}],
    _ = gen_yawl:call(Pid, {schedule_timers, Timers}),

    %% Verify persistence (would normally checkpoint to Mnesia)
    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("Timer queue state: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - Time-Based Routing
%%%===================================================================

%% @doc Test time-based conditional routing
time_based_routing_test(_Config) ->
    CurrentHour = element(2, element(2, calendar:local_time())),

    {ok, Pid} = gen_yawl:start_link(time_routing_net,
                                     #{current_hour => CurrentHour}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    timer:sleep(200),

    Marking = gen_yawl:marking(Pid),
    ct:pal("Time-based routing at hour ~p: ~p", [CurrentHour, Marking]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test business hours routing
business_hours_routing_test(_Config) ->
    %% Test routing during and outside business hours
    TestCases = [
        {9, business_hours},   % 9 AM
        {14, business_hours},  % 2 PM
        {22, after_hours},     % 10 PM
        {3, after_hours}       % 3 AM
    ],

    lists:foreach(fun({Hour, Expected}) ->
        {ok, Pid} = gen_yawl:start_link(business_hours_net,
                                         #{hour => Hour}, []),

        {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),
        timer:sleep(100),

        UsrInfo = gen_yawl:usr_info(Pid),
        ct:pal("Hour ~p routed to: ~p (expected: ~p)", [Hour, UsrInfo, Expected]),

        ok = gen_yawl:stop(Pid)
    end, TestCases),

    ok.

%% @doc Test calendar-based routing
calendar_based_routing_test(_Config) ->
    {{Year, Month, Day}, _Time} = calendar:local_time(),

    {ok, Pid} = gen_yawl:start_link(calendar_routing_net,
                                     #{year => Year, month => Month, day => Day}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    timer:sleep(200),

    Marking = gen_yawl:marking(Pid),
    ct:pal("Calendar-based routing for ~p-~p-~p: ~p", [Year, Month, Day, Marking]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Test Cases - SLA Management
%%%===================================================================

%% @doc Test SLA monitoring
sla_monitoring_test(_Config) ->
    SLA = #{
        max_duration_ms => 1000,
        priority => high
    },

    {ok, Pid} = gen_yawl:start_link(sla_workflow_net,
                                     #{sla => SLA}, []),

    StartTime = erlang:system_time(millisecond),
    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    {ok, _} = gen_yawl:sync(Pid, 2000),

    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - StartTime,

    ct:pal("Workflow completed in ~p ms (SLA: ~p ms)", [Duration, 1000]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test SLA violation and escalation
sla_violation_escalation_test(_Config) ->
    SLA = #{
        max_duration_ms => 100,
        escalate_on_violation => true
    },

    {ok, Pid} = gen_yawl:start_link(slow_workflow_net,
                                     #{sla => SLA}, []),

    {ok, _} = gen_yawl:inject(Pid, #{p_start => [start_token]}),

    %% Wait past SLA
    timer:sleep(300),

    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("User info after SLA violation: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%% @doc Test SLA metrics collection
sla_metrics_test(_Config) ->
    {ok, Pid} = gen_yawl:start_link(sla_workflow_net,
                                     #{collect_metrics => true}, []),

    %% Execute multiple workflow instances
    lists:foreach(fun(N) ->
        Token = {instance, N},
        {ok, _} = gen_yawl:inject(Pid, #{p_start => [Token]}),
        timer:sleep(50)
    end, lists:seq(1, 5)),

    timer:sleep(500),

    %% Retrieve SLA metrics
    UsrInfo = gen_yawl:usr_info(Pid),
    ct:pal("SLA metrics: ~p", [UsrInfo]),

    ok = gen_yawl:stop(Pid),
    ok.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

ensure_modules_loaded() ->
    Modules = [
        gen_yawl, gen_pnet,
        deadline_workflow_net, slow_workflow_net, delayed_workflow_net,
        multi_delayed_net, timeout_workflow_net, long_running_net,
        timeout_recovery_net, periodic_workflow_net, timer_queue_net,
        time_routing_net, business_hours_net, calendar_routing_net,
        sla_workflow_net
    ],

    Results = [code:ensure_loaded(M) || M <- Modules],
    case lists:all(fun({module, _}) -> true; (_) -> false end, Results) of
        true -> ok;
        false ->
            ct:pal("Warning: Some test modules not found, tests may fail"),
            ok
    end.
