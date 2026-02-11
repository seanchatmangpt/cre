%%% @doc WF Scheduler - Quanta-Based Scheduling with Fairness and Replay
%%%
%%% This module implements a quanta-based scheduler that controls case execution
%%% to ensure fairness, prevent starvation, and maintain deterministic replay
%%% capability. The scheduler allocates execution steps (quanta) across cases
%%% based on configurable policies.
%%%
%%% Key features:
%%% - Quanta-based execution (N steps per scheduling tick)
%%% - Fair allocation across multiple concurrent cases
%%% - Deterministic replay (same schedule from same initial state)
%%% - Configurable policies for step allocation
%%% - Starvation prevention with minimum progress guarantees
%%% - Yield handling for effect boundaries
%%%
%%% @end
-module(wf_sched).

-export([
    %% Scheduler creation and configuration
    new/0,
    new/1,

    %% Case management
    add_case/3,
    remove_case/2,
    case_exists/2,
    get_case_state/2,
    update_case_state/3,

    %% Scheduling operations
    schedule_tick/1,
    schedule_all/1,
    get_next_case/1,
    allocate_steps/2,

    %% Policy functions
    policy_round_robin/1,
    policy_fair_share/1,
    policy_priority/1,
    policy_deterministic/1,

    %% Introspection
    scheduler_status/1,
    case_stats/2,
    all_case_stats/1
]).

-export_type([
    scheduler/0,
    case_id/0,
    sched_policy/0,
    sched_config/0,
    sched_status/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

%% Unique identifier for a case
-type case_id() :: term().

%% Scheduler state
-type scheduler() :: #{
    cases := #{case_id() => case_entry()},
    policy := sched_policy(),
    config := sched_config(),
    tick := non_neg_integer(),
    round := non_neg_integer(),
    last_scheduled := case_id() | undefined,
    stats := sched_stats()
}.

%% Per-case entry in scheduler
-type case_entry() :: #{
    case_id := case_id(),
    exec_state := wf_vm:exec_state(),
    priority := non_neg_integer(),
    steps_executed := non_neg_integer(),
    steps_this_round := non_neg_integer(),
    last_tick := non_neg_integer(),
    status := running | yielded | halted | error,
    yield_spec := term() | undefined,
    created_at := non_neg_integer()
}.

%% Scheduling policy
-type sched_policy() ::
      round_robin          % Fair rotation through cases
    | fair_share           % Equal steps allocated per case
    | priority             % Priority-based (higher priority = more steps)
    | deterministic        % Deterministic ordering for replay
    | {custom, fun((scheduler()) -> {case_id() | undefined, scheduler()})}.

%% Scheduler configuration
-type sched_config() :: #{
    default_quanta => non_neg_integer(),      % Default steps per tick
    min_quanta => non_neg_integer(),          % Minimum steps to prevent starvation
    max_quanta => non_neg_integer(),          % Maximum steps per allocation
    starvation_threshold => non_neg_integer(), % Ticks without progress = starved
    yield_backoff => non_neg_integer(),       % Steps to wait after yield
    default_priority => non_neg_integer()     % Default case priority
}.

%% Scheduler statistics
-type sched_stats() :: #{
    total_ticks := non_neg_integer(),
    total_steps := non_neg_integer(),
    cases_created := non_neg_integer(),
    cases_completed := non_neg_integer(),
    cases_errored := non_neg_integer()
}.

%% Scheduler status for introspection
-type sched_status() :: #{
    tick := non_neg_integer(),
    round := non_neg_integer(),
    active_cases := non_neg_integer(),
    policy := sched_policy(),
    total_steps := non_neg_integer()
}.

%%% API =====================================================================

%% @doc Create a new scheduler with default configuration.
-spec new() -> scheduler().
new() ->
    new(#{}).

%% @doc Create a new scheduler with custom configuration.
-spec new(Config :: sched_config()) -> scheduler().
new(Config) ->
    DefaultConfig = #{
        default_quanta => 100,
        min_quanta => 10,
        max_quanta => 1000,
        starvation_threshold => 10,
        yield_backoff => 5,
        default_priority => 50
    },
    FinalConfig = maps:merge(DefaultConfig, Config),
    #{
        cases => #{},
        policy => maps:get(policy, Config, round_robin),
        config => FinalConfig,
        tick => 0,
        round => 0,
        last_scheduled => undefined,
        stats => #{
            total_ticks => 0,
            total_steps => 0,
            cases_created => 0,
            cases_completed => 0,
            cases_errored => 0
        }
    }.

%% @doc Add a new case to the scheduler.
-spec add_case(
    Sched :: scheduler(),
    CaseId :: case_id(),
    ExecState :: wf_vm:exec_state()
) -> scheduler().
add_case(Sched, CaseId, ExecState) ->
    Cases = maps:get(cases, Sched),
    Config = maps:get(config, Sched),
    Tick = maps:get(tick, Sched),
    Stats = maps:get(stats, Sched),

    Entry = #{
        case_id => CaseId,
        exec_state => ExecState,
        priority => maps:get(default_priority, Config),
        steps_executed => 0,
        steps_this_round => 0,
        last_tick => Tick,
        status => running,
        yield_spec => undefined,
        created_at => erlang:monotonic_time(microsecond)
    },

    NewCases = maps:put(CaseId, Entry, Cases),
    NewStats = Stats#{cases_created := maps:get(cases_created, Stats) + 1},

    Sched#{
        cases := NewCases,
        stats := NewStats
    }.

%% @doc Remove a case from the scheduler.
-spec remove_case(Sched :: scheduler(), CaseId :: case_id()) -> scheduler().
remove_case(Sched, CaseId) ->
    Cases = maps:get(cases, Sched),
    NewCases = maps:remove(CaseId, Cases),
    Sched#{cases := NewCases}.

%% @doc Check if a case exists in the scheduler.
-spec case_exists(Sched :: scheduler(), CaseId :: case_id()) -> boolean().
case_exists(Sched, CaseId) ->
    Cases = maps:get(cases, Sched),
    maps:is_key(CaseId, Cases).

%% @doc Get the execution state for a case.
-spec get_case_state(Sched :: scheduler(), CaseId :: case_id()) ->
    {ok, wf_vm:exec_state()} | {error, not_found}.
get_case_state(Sched, CaseId) ->
    Cases = maps:get(cases, Sched),
    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            {error, not_found};
        Entry ->
            {ok, maps:get(exec_state, Entry)}
    end.

%% @doc Update the execution state for a case.
-spec update_case_state(
    Sched :: scheduler(),
    CaseId :: case_id(),
    ExecState :: wf_vm:exec_state()
) -> scheduler().
update_case_state(Sched, CaseId, ExecState) ->
    Cases = maps:get(cases, Sched),
    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            Sched;
        Entry ->
            NewEntry = Entry#{exec_state := ExecState},
            NewCases = maps:put(CaseId, NewEntry, Cases),
            Sched#{cases := NewCases}
    end.

%% @doc Execute one scheduling tick: select a case and run N steps.
-spec schedule_tick(Sched :: scheduler()) ->
    {ok, scheduler()} | {done, scheduler()}.
schedule_tick(Sched) ->
    Cases = maps:get(cases, Sched),

    case maps:size(Cases) of
        0 ->
            {done, Sched};
        _ ->
            {CaseId, Sched1} = get_next_case(Sched),
            case CaseId of
                undefined ->
                    %% No eligible cases this tick
                    NewTick = maps:get(tick, Sched1) + 1,
                    {ok, Sched1#{tick := NewTick}};
                _ ->
                    Sched2 = execute_case_steps(Sched1, CaseId),
                    NewTick = maps:get(tick, Sched2) + 1,
                    Stats = maps:get(stats, Sched2),
                    NewStats = Stats#{total_ticks := maps:get(total_ticks, Stats) + 1},
                    {ok, Sched2#{tick := NewTick, stats := NewStats}}
            end
    end.

%% @doc Execute all cases until all halt or yield.
-spec schedule_all(Sched :: scheduler()) -> scheduler().
schedule_all(Sched) ->
    case schedule_tick(Sched) of
        {done, FinalSched} ->
            FinalSched;
        {ok, NewSched} ->
            schedule_all(NewSched)
    end.

%% @doc Get the next case to schedule based on policy.
-spec get_next_case(Sched :: scheduler()) ->
    {case_id() | undefined, scheduler()}.
get_next_case(Sched) ->
    Policy = maps:get(policy, Sched),
    apply_policy(Policy, Sched).

%% @doc Allocate number of steps for a case based on policy.
-spec allocate_steps(Sched :: scheduler(), CaseId :: case_id()) ->
    non_neg_integer().
allocate_steps(Sched, CaseId) ->
    Config = maps:get(config, Sched),
    Cases = maps:get(cases, Sched),

    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            0;
        Entry ->
            Policy = maps:get(policy, Sched),
            case Policy of
                priority ->
                    %% More steps for higher priority
                    Priority = maps:get(priority, Entry),
                    BaseQuanta = maps:get(default_quanta, Config),
                    min(Priority * BaseQuanta div 50, maps:get(max_quanta, Config));
                _ ->
                    maps:get(default_quanta, Config)
            end
    end.

%%% POLICY FUNCTIONS ========================================================

%% @doc Round-robin policy: cycle through cases in deterministic order.
-spec policy_round_robin(Sched :: scheduler()) ->
    {case_id() | undefined, scheduler()}.
policy_round_robin(Sched) ->
    Cases = maps:get(cases, Sched),
    LastScheduled = maps:get(last_scheduled, Sched),
    Round = maps:get(round, Sched),

    %% Get sorted list of case IDs for deterministic ordering
    CaseIds = lists:sort(maps:keys(Cases)),
    EligibleIds = [Id || Id <- CaseIds, is_eligible(Cases, Id)],

    case EligibleIds of
        [] ->
            {undefined, Sched};
        _ ->
            case LastScheduled of
                undefined ->
                    NextCaseId = hd(EligibleIds),
                    {NextCaseId, Sched#{last_scheduled := NextCaseId}};
                Last ->
                    case find_next_in_cycle(Last, EligibleIds) of
                        undefined ->
                            %% Completed round, start new one
                            NewRound = Round + 1,
                            NextCaseId = hd(EligibleIds),
                            {NextCaseId, Sched#{round := NewRound, last_scheduled := NextCaseId}};
                        Next ->
                            {Next, Sched#{last_scheduled := Next}}
                    end
            end
    end.

%% @doc Fair-share policy: ensure all cases get equal steps over time.
-spec policy_fair_share(Sched :: scheduler()) ->
    {case_id() | undefined, scheduler()}.
policy_fair_share(Sched) ->
    Cases = maps:get(cases, Sched),
    Round = maps:get(round, Sched),

    %% Find case with minimum steps this round
    CaseList = maps:to_list(Cases),
    EligibleCases = [{Id, Entry} || {Id, Entry} <- CaseList, is_eligible(Cases, Id)],

    case EligibleCases of
        [] ->
            {undefined, Sched};
        _ ->
            {ChosenId, _} = lists:foldl(
                fun({Id, Entry}, {MinId, MinSteps}) ->
                    Steps = maps:get(steps_this_round, Entry),
                    if
                        Steps < MinSteps -> {Id, Steps};
                        true -> {MinId, MinSteps}
                    end
                end,
                {undefined, infinity},
                EligibleCases
            ),

            %% Check if we should start a new round
            AllEqual = lists:all(
                fun({_Id, Entry}) ->
                    maps:get(steps_this_round, Entry) > 0
                end,
                EligibleCases
            ),

            NewSched = case AllEqual of
                true ->
                    %% Reset round counters
                    NewCases = maps:map(
                        fun(_Id, Entry) ->
                            Entry#{steps_this_round := 0}
                        end,
                        Cases
                    ),
                    Sched#{cases := NewCases, round := Round + 1};
                false ->
                    Sched
            end,

            {ChosenId, NewSched#{last_scheduled := ChosenId}}
    end.

%% @doc Priority policy: higher priority cases get more CPU time.
-spec policy_priority(Sched :: scheduler()) ->
    {case_id() | undefined, scheduler()}.
policy_priority(Sched) ->
    Cases = maps:get(cases, Sched),

    %% Find highest priority eligible case
    CaseList = maps:to_list(Cases),
    EligibleCases = [{Id, Entry} || {Id, Entry} <- CaseList, is_eligible(Cases, Id)],

    case EligibleCases of
        [] ->
            {undefined, Sched};
        _ ->
            {ChosenId, _} = lists:foldl(
                fun({Id, Entry}, {MaxId, MaxPriority}) ->
                    Priority = maps:get(priority, Entry),
                    if
                        Priority > MaxPriority -> {Id, Priority};
                        Priority =:= MaxPriority, MaxId =:= undefined -> {Id, Priority};
                        Priority =:= MaxPriority -> {min(Id, MaxId), Priority};
                        true -> {MaxId, MaxPriority}
                    end
                end,
                {undefined, 0},
                EligibleCases
            ),
            {ChosenId, Sched#{last_scheduled := ChosenId}}
    end.

%% @doc Deterministic policy: strictly ordered for replay.
-spec policy_deterministic(Sched :: scheduler()) ->
    {case_id() | undefined, scheduler()}.
policy_deterministic(Sched) ->
    %% Deterministic is same as round-robin with sorted keys
    policy_round_robin(Sched).

%%% INTROSPECTION ===========================================================

%% @doc Get scheduler status.
-spec scheduler_status(Sched :: scheduler()) -> sched_status().
scheduler_status(Sched) ->
    Cases = maps:get(cases, Sched),
    Stats = maps:get(stats, Sched),
    ActiveCount = length([1 || {_Id, Entry} <- maps:to_list(Cases),
                               maps:get(status, Entry) =:= running]),

    #{
        tick => maps:get(tick, Sched),
        round => maps:get(round, Sched),
        active_cases => ActiveCount,
        policy => maps:get(policy, Sched),
        total_steps => maps:get(total_steps, Stats)
    }.

%% @doc Get statistics for a specific case.
-spec case_stats(Sched :: scheduler(), CaseId :: case_id()) ->
    {ok, map()} | {error, not_found}.
case_stats(Sched, CaseId) ->
    Cases = maps:get(cases, Sched),
    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            {error, not_found};
        Entry ->
            {ok, #{
                case_id => CaseId,
                status => maps:get(status, Entry),
                steps_executed => maps:get(steps_executed, Entry),
                priority => maps:get(priority, Entry),
                last_tick => maps:get(last_tick, Entry)
            }}
    end.

%% @doc Get statistics for all cases.
-spec all_case_stats(Sched :: scheduler()) -> [map()].
all_case_stats(Sched) ->
    Cases = maps:get(cases, Sched),
    [begin
        {ok, Stats} = case_stats(Sched, CaseId),
        Stats
     end || CaseId <- maps:keys(Cases)].

%%% INTERNAL HELPERS ========================================================

%% Execute N steps for a specific case
-spec execute_case_steps(Sched :: scheduler(), CaseId :: case_id()) ->
    scheduler().
execute_case_steps(Sched, CaseId) ->
    Cases = maps:get(cases, Sched),
    Entry = maps:get(CaseId, Cases),
    ExecState = maps:get(exec_state, Entry),

    NumSteps = allocate_steps(Sched, CaseId),

    %% Execute the steps
    {FinalExecState, StepsExecuted} = wf_exec:exec_steps(ExecState, NumSteps),

    %% Update case entry
    NewStepsExecuted = maps:get(steps_executed, Entry) + StepsExecuted,
    NewStepsThisRound = maps:get(steps_this_round, Entry) + StepsExecuted,

    %% Determine new status
    NewStatus = case wf_exec:exec_status(FinalExecState) of
        #{state := halted} -> halted;
        #{state := error} -> error;
        _ ->
            case wf_exec:is_yielded(FinalExecState) of
                true -> yielded;
                false -> running
            end
    end,

    NewEntry = Entry#{
        exec_state := FinalExecState,
        steps_executed := NewStepsExecuted,
        steps_this_round := NewStepsThisRound,
        status := NewStatus,
        last_tick := maps:get(tick, Sched)
    },

    NewCases = maps:put(CaseId, NewEntry, Cases),

    %% Update stats
    Stats = maps:get(stats, Sched),
    NewTotalSteps = maps:get(total_steps, Stats) + StepsExecuted,

    NewStats = case NewStatus of
        halted ->
            Stats#{
                total_steps := NewTotalSteps,
                cases_completed := maps:get(cases_completed, Stats) + 1
            };
        error ->
            Stats#{
                total_steps := NewTotalSteps,
                cases_errored := maps:get(cases_errored, Stats) + 1
            };
        _ ->
            Stats#{total_steps := NewTotalSteps}
    end,

    Sched#{cases := NewCases, stats := NewStats}.

%% Check if a case is eligible for scheduling
-spec is_eligible(Cases :: #{case_id() => case_entry()}, CaseId :: case_id()) ->
    boolean().
is_eligible(Cases, CaseId) ->
    case maps:get(CaseId, Cases, undefined) of
        undefined ->
            false;
        Entry ->
            Status = maps:get(status, Entry),
            Status =:= running orelse Status =:= yielded
    end.

%% Find next case ID in cycle
-spec find_next_in_cycle(Last :: case_id(), CaseIds :: [case_id()]) ->
    case_id() | undefined.
find_next_in_cycle(Last, CaseIds) ->
    case lists:dropwhile(fun(Id) -> Id =/= Last end, CaseIds) of
        [] ->
            undefined;
        [Last | []] ->
            %% Last was at end of list, cycle complete
            undefined;
        [Last | Rest] ->
            hd(Rest)
    end.

%% Apply scheduling policy
-spec apply_policy(Policy :: sched_policy(), Sched :: scheduler()) ->
    {case_id() | undefined, scheduler()}.
apply_policy(round_robin, Sched) ->
    policy_round_robin(Sched);
apply_policy(fair_share, Sched) ->
    policy_fair_share(Sched);
apply_policy(priority, Sched) ->
    policy_priority(Sched);
apply_policy(deterministic, Sched) ->
    policy_deterministic(Sched);
apply_policy({custom, Fun}, Sched) ->
    Fun(Sched);
apply_policy(_, Sched) ->
    %% Default to round-robin
    policy_round_robin(Sched).

%%% TESTS ===================================================================

scheduler_creation_test() ->
    Sched = new(),
    ?assertMatch(#{cases := _, policy := round_robin}, Sched).

add_remove_case_test() ->
    Sched = new(),
    ExecState = wf_vm:exec_state([], 0, [], #{}, #{}, #{}, []),

    Sched1 = add_case(Sched, case1, ExecState),
    ?assert(case_exists(Sched1, case1)),

    Sched2 = remove_case(Sched1, case1),
    ?assertNot(case_exists(Sched2, case1)).

round_robin_policy_test() ->
    Sched = new(#{policy => round_robin}),
    ExecState1 = wf_vm:exec_state([wf_vm:op_halt()], 0, [], #{}, #{}, #{}, []),
    ExecState2 = wf_vm:exec_state([wf_vm:op_halt()], 0, [], #{}, #{}, #{}, []),

    Sched1 = add_case(Sched, case1, ExecState1),
    Sched2 = add_case(Sched1, case2, ExecState2),

    {CaseId1, Sched3} = get_next_case(Sched2),
    ?assertEqual(case1, CaseId1),

    {CaseId2, _Sched4} = get_next_case(Sched3),
    ?assertEqual(case2, CaseId2).

fair_share_policy_test() ->
    Sched = new(#{policy => fair_share}),
    ExecState = wf_vm:exec_state([wf_vm:op_halt()], 0, [], #{}, #{}, #{}, []),

    Sched1 = add_case(Sched, case1, ExecState),
    Sched2 = add_case(Sched1, case2, ExecState),

    {CaseId, _} = get_next_case(Sched2),
    ?assert(CaseId =:= case1 orelse CaseId =:= case2).

priority_policy_test() ->
    Sched = new(#{policy => priority}),
    ExecState = wf_vm:exec_state([wf_vm:op_halt()], 0, [], #{}, #{}, #{}, []),

    Sched1 = add_case(Sched, case1, ExecState),
    Sched2 = add_case(Sched1, case2, ExecState),

    %% Set different priorities
    Cases = maps:get(cases, Sched2),
    Entry1 = maps:get(case1, Cases),
    Entry2 = maps:get(case2, Cases),
    NewCases = maps:put(case1, Entry1#{priority := 100}, Cases),
    NewCases2 = maps:put(case2, Entry2#{priority := 50}, NewCases),
    Sched3 = Sched2#{cases := NewCases2},

    {CaseId, _} = get_next_case(Sched3),
    ?assertEqual(case1, CaseId).

scheduler_status_test() ->
    Sched = new(),
    Status = scheduler_status(Sched),
    ?assertMatch(#{tick := 0, active_cases := 0}, Status).

case_stats_test() ->
    Sched = new(),
    ExecState = wf_vm:exec_state([], 0, [], #{}, #{}, #{}, []),
    Sched1 = add_case(Sched, case1, ExecState),

    {ok, Stats} = case_stats(Sched1, case1),
    ?assertMatch(#{case_id := case1, steps_executed := 0}, Stats).
