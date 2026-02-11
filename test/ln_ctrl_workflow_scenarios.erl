%%%-------------------------------------------------------------------
%%% @doc ln_ctrl Workflow Scenarios - Test Data and Expected Results
%%%
%%% This module provides realistic workflow scenarios for ln_ctrl testing.
%%% Each scenario includes:
%%% - wf_term() representation
%%% - Expected execution path
%%% - Expected receipts/effect outputs
%%% - Configurable parameters for variation
%%%
%%% Scenarios are reusable across different test suites.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_ctrl_workflow_scenarios).

%% Scenario constructors
-export([
    fortune_5_workflow/1,
    fortune_5_workflow/0,
    order_processing_workflow/1,
    order_processing_workflow/0,
    multi_stage_approval_workflow/1,
    multi_stage_approval_workflow/0,
    resource_intensive_workflow/1,
    resource_intensive_workflow/0,
    cancellation_prone_workflow/1,
    cancellation_prone_workflow/0
]).

%% Scenario accessors
-export([
    workflow/1,
    expected_context/1,
    expected_receipts/1,
    expected_steps/1,
    scenario_options/1,
    validate_scenario/1
]).

%% Task funs for scenarios (exported for test access)
%% Note: task functions have varying arity depending on use
-export([
    fib_task/2,
    validate_order_task/2,
    check_inventory_task/2,
    process_payment_task/2,
    approval_task/3,
    exec_approval/1,
    escalate_approval/1,
    resource_heavy_task/1,
    long_running_task/2
]).

-include_lib("eunit/include/eunit.hrl").

%%%====================================================================
%%% TYPES
%%%====================================================================

-type scenario() :: #{
    name => atom(),
    description => binary(),
    workflow => wf_term:wf_term(),
    expected_results => expected_results(),
    default_options => map()
}.

-type expected_results() :: #{
    final_context => wf_term:context(),
    receipt_count => non_neg_integer(),
    execution_steps => non_neg_integer(),
    effect_ids => [atom()],
    termination => normal | cancelled | {error, term()}
}.

-type scenario_options() :: #{
    steps => pos_integer(),
    parallel_branches => pos_integer(),
    approval_levels => pos_integer(),
    max_effects => pos_integer() | unlimited,
    cancellation_point => early | mid | late
}.

%%%====================================================================
%%% SCENARIO 1: Fortune 5 Workflow
%%%====================================================================
%%% Sequential Fibonacci calculation - pure sequential computation
%%% Tests deterministic execution and linear progress

%% @doc Create Fortune 5 workflow with default 5 steps.
-spec fortune_5_workflow() -> scenario().
fortune_5_workflow() ->
    fortune_5_workflow(#{steps => 5}).

%% @doc Create Fortune 5 workflow with custom options.
%% Options:
%%   - steps: Number of Fibonacci calculations (default: 5)
-spec fortune_5_workflow(scenario_options()) -> scenario().
fortune_5_workflow(Options) ->
    Steps = maps:get(steps, Options, 5),
    Steps = max(1, min(20, Steps)),  % Bound between 1-20

    %% Build sequential Fibonacci workflow
    Workflow = build_fibonacci_sequence(Steps),

    %% Expected: each step produces one receipt
    ExpectedReceipts = lists:seq(1, Steps),
    EffectIds = [list_to_atom(["fib", integer_to_list(I)]) || I <- ExpectedReceipts],

    Scenario = #{
        name => fortune_5,
        description => <<"Fortune 5: Sequential Fibonacci calculation for deterministic testing">>,
        workflow => Workflow,
        expected_results => #{
            final_context => #{
                fib_sequence => compute_fibonacci(Steps),
                steps_completed => Steps
            },
            receipt_count => Steps,
            execution_steps => Steps * 2,  % Each fib: task + result return
            effect_ids => EffectIds,
            termination => normal
        },
        default_options => Options
    }.

%% @doc Build sequential Fibonacci calculation workflow.
build_fibonacci_sequence(Steps) ->
    lists:foldl(fun(I, Acc) ->
        wf_term:seq(Acc, wf_term:task({fib, I}, fun(Ctx) -> fib_task(I, Ctx) end))
    end, wf_term:task(fib_init, fun(Ctx) -> fib_init(Ctx) end), lists:seq(1, Steps)).

%% @doc Initialize Fibonacci sequence.
fib_init(Ctx) ->
    {ok, Ctx#{fib_sequence => [0, 1], last_fib => 1}}.

%% @doc Compute next Fibonacci number (takes step number and context).
fib_task(N, Ctx) ->
    [0, 1 | Rest] = maps:get(fib_sequence, Ctx, [0, 1]),
    Next = compute_next_fib([1, 0 | Rest]),
    NewSequence = [Next | lists:sublist([0, 1 | Rest], N)],
    {ok, Ctx#{
        fib_sequence => NewSequence,
        last_fib => Next,
        current_step => N
    }}.

%% @doc Compute next Fibonacci number from sequence.
compute_next_fib([A, B | _]) -> A + B;
compute_next_fib([A, B]) -> A + B.

%% @doc Compute Nth Fibonacci number (fast doubling).
compute_fibonacci(0) -> 0;
compute_fibonacci(1) -> 1;
compute_fibonacci(N) when N > 1 ->
    {Fib, _} = fib_fast_doubling(N),
    Fib.

%% Fast doubling Fibonacci algorithm.
fib_fast_doubling(N) ->
    {F, K} = fib_fast(N),
    {F rem 1000000007, K rem 1000000007}.  % Mod for large numbers

fib_fast(0) -> {0, 1};
fib_fast(N) ->
    {A, B} = fib_fast(N div 2),
    C = A * ((B * 2 - A) rem 1000000007) rem 1000000007,
    D = (A * A + B * B) rem 1000000007,
    case N rem 2 of
        0 -> {C, D};
        1 -> {D, (C + D) rem 1000000007}
    end.

%%%====================================================================
%%% SCENARIO 2: Order Processing Workflow
%%%====================================================================
%%% Parallel branches with AND-join synchronization

%% @doc Create order processing workflow with default 3 branches.
-spec order_processing_workflow() -> scenario().
order_processing_workflow() ->
    order_processing_workflow(#{parallel_branches => 3}).

%% @doc Create order processing workflow with custom options.
%% Options:
%%   - parallel_branches: Number of parallel branches (default: 3)
-spec order_processing_workflow(scenario_options()) -> scenario().
order_processing_workflow(Options) ->
    Branches = maps:get(parallel_branches, Options, 3),
    Branches = max(2, min(10, Branches)),  % Bound between 2-10

    %% Build parallel order processing workflow
    Workflow = wf_term:par([
        wf_term:task(validate_order, fun(Ctx) -> validate_order_task(Branches, Ctx) end),
        wf_term:task(check_inventory, fun(Ctx) -> check_inventory_task(Branches, Ctx) end),
        wf_term:task(process_payment, fun(Ctx) -> process_payment_task(Branches, Ctx) end)
    ]),

    %% Expected: 3 parallel receipts (one per branch)
    EffectIds = [validate_order, check_inventory, process_payment],

    Scenario = #{
        name => order_processing,
        description => <<"Order processing with parallel validation, inventory, and payment">>,
        workflow => Workflow,
        expected_results => #{
            final_context => #{
                order_validated => true,
                inventory_checked => true,
                payment_processed => true,
                branches_completed => Branches
            },
            receipt_count => Branches,
            execution_steps => Branches + 1,  % Each task + join
            effect_ids => EffectIds,
            termination => normal
        },
        default_options => Options
    }.

%% @doc Simulate order validation.
validate_order_task(_Branches, Ctx) ->
    OrderId = maps:get(order_id, Ctx, "ORD-" ++ integer_to_list(erlang:unique_integer())),
    {ok, Ctx#{
        order_validated => true,
        order_id => OrderId,
        validation_timestamp => erlang:monotonic_time(millisecond)
    }}.

%% @doc Simulate inventory check.
check_inventory_task(_Branches, Ctx) ->
    OrderId = maps:get(order_id, Ctx, "unknown"),
    StockLevel = rand:uniform(100),
    {ok, Ctx#{
        inventory_checked => true,
        stock_available => StockLevel > 10,
        stock_level => StockLevel
    }}.

%% @doc Simulate payment processing.
process_payment_task(_Branches, Ctx) ->
    OrderId = maps:get(order_id, Ctx, "unknown"),
    Amount = maps:get(amount, Ctx, 100.00),
    {ok, Ctx#{
        payment_processed => true,
        payment_amount => Amount,
        payment_id => "PAY-" ++ integer_to_list(erlang:unique_integer([positive]))
    }}.

%%%====================================================================
%%% SCENARIO 3: Multi-Stage Approval Workflow
%%%====================================================================
%%% Sequential approvals with conditional branching

%% @doc Create multi-stage approval workflow with default 3 levels.
-spec multi_stage_approval_workflow() -> scenario().
multi_stage_approval_workflow() ->
    multi_stage_approval_workflow(#{approval_levels => 3}).

%% @doc Create multi-stage approval workflow with custom options.
%% Options:
%%   - approval_levels: Number of approval levels (default: 3)
-spec multi_stage_approval_workflow(scenario_options()) -> scenario().
multi_stage_approval_workflow(Options) ->
    Levels = maps:get(approval_levels, Options, 3),
    Levels = max(2, min(5, Levels)),  % Bound between 2-5

    %% Build sequential approval workflow with choice at end
    Workflow = build_approval_chain(Levels),

    %% Expected: one receipt per approval level
    EffectIds = [list_to_atom(["approval_", integer_to_list(L)]) || L <- lists:seq(1, Levels)],

    Scenario = #{
        name => multi_stage_approval,
        description => <<"Multi-stage approval workflow with conditional escalation">>,
        workflow => Workflow,
        expected_results => #{
            final_context => #{
                approved => true,
                approval_level => Levels,
                approver => list_to_atom(["level_", integer_to_list(Levels)])
            },
            receipt_count => Levels,
            execution_steps => Levels + 1,  % Approvals + final check
            effect_ids => EffectIds,
            termination => normal
        },
        default_options => Options
    }.

%% @doc Build approval chain with final escalation choice.
build_approval_chain(Levels) ->
    ApprovalTasks = lists:map(fun(N) ->
        TaskName = list_to_atom(["approval_", integer_to_list(N)]),
        wf_term:task(TaskName, fun(ApprovalCtx) -> approval_task(N, Levels, ApprovalCtx) end)
    end, lists:seq(1, Levels)),

    %% Chain approvals sequentially, ending with choice
    ChainWithChoice = lists:foldl(fun(Task, Acc) ->
        wf_term:seq(Acc, Task)
    end, hd(ApprovalTasks), tl(ApprovalTasks)),

    %% Add final choice for escalation
    wf_term:seq(ChainWithChoice, wf_term:choice([
        wf_term:task(exec_approved, fun(Ctx) -> exec_approval(Ctx) end),
        wf_term:task(escalate, fun(Ctx) -> escalate_approval(Ctx) end)
    ])).

%% @doc Execute approval task at given level.
approval_task(Level, MaxLevel, Ctx) ->
    ApprovalThreshold = Level * 20,  % Escalating approval amount
    RequestAmount = maps:get(request_amount, Ctx, 50),
    CanApprove = RequestAmount =< ApprovalThreshold orelse Level =:= MaxLevel,
    {ok, Ctx#{
        approval_level => Level,
        approval_granted => CanApprove,
        approver => list_to_atom(["level_", integer_to_list(Level)]),
        approval_timestamp => erlang:monotonic_time(millisecond)
    }}.

%% @doc Execute approved path.
exec_approval(Ctx) ->
    {ok, Ctx#{approved => true, exec_path => approved}}.

%% @doc Execute escalation path.
escalate_approval(Ctx) ->
    {ok, Ctx#{approved => false, exec_path => escalated}}.

%%%====================================================================
%%% SCENARIO 4: Resource-Intensive Workflow
%%%====================================================================
%%% Loop with budget limit testing

%% @doc Create resource-intensive workflow with default budget.
-spec resource_intensive_workflow() -> scenario().
resource_intensive_workflow() ->
    resource_intensive_workflow(#{
        max_effects => 10,
        max_cost => 1000.0,
        max_time => 5000
    }).

%% @doc Create resource-intensive workflow with custom options.
%% Options:
%%   - max_effects: Maximum effects before budget exceeded (default: 10)
%%   - max_cost: Maximum cost budget (default: 1000.0)
%%   - max_time: Maximum time in ms (default: 5000)
-spec resource_intensive_workflow(scenario_options()) -> scenario().
resource_intensive_workflow(Options) ->
    MaxEffects = maps:get(max_effects, Options, 10),
    MaxCost = maps:get(max_cost, Options, 1000.0),
    MaxTime = maps:get(max_time, Options, 5000),

    %% Build loop with effect-emitting task
    Workflow = wf_term:loop({max_iter, MaxEffects * 2},  % Double to trigger budget exceed
        wf_term:task(resource_heavy, fun(Ctx) -> resource_heavy_task(Ctx) end)),

    %% Expected: budget exceeded at max_effects
    EffectIds = [list_to_atom(["resource_", integer_to_list(I)]) || I <- lists:seq(1, MaxEffects)],

    Scenario = #{
        name => resource_intensive,
        description => <<"Resource-intensive workflow testing budget enforcement">>,
        workflow => Workflow,
        expected_results => #{
            final_context => #{
                effects_attempted => MaxEffects * 2,
                effects_completed => MaxEffects,
                budget_exceeded => true
            },
            receipt_count => MaxEffects,
            execution_steps => MaxEffects + 1,  % Effects + budget check
            effect_ids => EffectIds,
            termination => {error, budget_exceeded}
        },
        default_options => Options
    }.

%% @doc Simulate resource-heavy task.
resource_heavy_task(Ctx) ->
    EffectsCount = maps:get(effects_count, Ctx, 0),
    Cost = 10.0 + rand:uniform() * 5.0,  % Random cost 10-15
    {ok, Ctx#{
        effects_count => EffectsCount + 1,
        last_cost => Cost,
        total_cost => maps:get(total_cost, Ctx, 0.0) + Cost
    }}.

%%%====================================================================
%%% SCENARIO 5: Cancellation-Prone Workflow
%%%====================================================================
%%% Long-running tasks with cancellation scopes

%% @doc Create cancellation-prone workflow with default mid cancellation.
-spec cancellation_prone_workflow() -> scenario().
cancellation_prone_workflow() ->
    cancellation_prone_workflow(#{cancellation_point => mid}).

%% @doc Create cancellation-prone workflow with custom options.
%% Options:
%%   - cancellation_point: When to cancel (early/mid/late, default: mid)
-spec cancellation_prone_workflow(scenario_options()) -> scenario().
cancellation_prone_workflow(Options) ->
    CancelPoint = maps:get(cancellation_point, Options, mid),

    %% Build workflow with cancellation scopes
    Workflow = wf_term:seq([
        wf_term:cancel_scope({region, phase1},
            wf_term:task(long_task_1, fun(Ctx) -> long_running_task(1, Ctx) end)
        ),
        wf_term:cancel_scope({region, phase2},
            wf_term:task(long_task_2, fun(Ctx) -> long_running_task(2, Ctx) end)
        ),
        wf_term:cancel_scope({region, phase3},
            wf_term:task(long_task_3, fun(Ctx) -> long_running_task(3, Ctx) end)
        )
    ]),

    %% Expected: partial completion based on cancellation point
    {CompletedPhases, ExpectedReceipts} = case CancelPoint of
        early -> {1, [long_task_1]};
        mid -> {2, [long_task_1, long_task_2]};
        late -> {3, [long_task_1, long_task_2, long_task_3]}
    end,

    EffectIds = ExpectedReceipts,

    Scenario = #{
        name => cancellation_prone,
        description => <<"Long-running workflow with cancellation scopes">>,
        workflow => Workflow,
        expected_results => #{
            final_context => #{
                phases_completed => CompletedPhases,
                cancelled_at => CancelPoint
            },
            receipt_count => length(ExpectedReceipts),
            execution_steps => CompletedPhases + 1,
            effect_ids => EffectIds,
            termination => cancelled
        },
        default_options => Options
    }.

%% @doc Simulate long-running task.
long_running_task(Phase, Ctx) ->
    %% Simulate work with delay context
    WorkUnits = Phase * 100,
    {ok, Ctx#{
        current_phase => Phase,
        work_completed => WorkUnits,
        phase_timestamp => erlang:monotonic_time(millisecond)
    }}.

%%%====================================================================
%%% SCENARIO ACCESSORS
%%%====================================================================

%% @doc Extract workflow from scenario.
-spec workflow(scenario()) -> wf_term:wf_term().
workflow(#{workflow := Workflow}) -> Workflow.

%% @doc Extract expected final context from scenario.
-spec expected_context(scenario()) -> wf_term:context().
expected_context(#{expected_results := #{final_context := Ctx}}) -> Ctx.

%% @doc Extract expected receipt count from scenario.
-spec expected_receipts(scenario()) -> non_neg_integer().
expected_receipts(#{expected_results := #{receipt_count := Count}}) -> Count.

%% @doc Extract expected execution steps from scenario.
-spec expected_steps(scenario()) -> non_neg_integer().
expected_steps(#{expected_results := #{execution_steps := Steps}}) -> Steps.

%% @doc Extract scenario options.
-spec scenario_options(scenario()) -> map().
scenario_options(#{default_options := Options}) -> Options.

%% @doc Validate scenario structure.
-spec validate_scenario(scenario()) -> ok | {error, term()}.
validate_scenario(#{name := Name, workflow := Workflow, expected_results := Expected}) ->
    case wf_term:is_valid(Workflow) of
        true -> ok;
        false -> {error, {invalid_workflow, Name}}
    end,
    validate_expected_results(Expected),
    ok.

%% @doc Validate expected results structure.
validate_expected_results(#{final_context := _, receipt_count := RC,
                         execution_steps := _, effect_ids := _}) when is_integer(RC), RC >= 0 ->
    ok;
validate_expected_results(Expected) ->
    {error, {invalid_expected_results, Expected}}.

%%%====================================================================
%%% TEST GENERATORS
%%%====================================================================

%% @doc Test all scenarios are valid.
all_scenarios_valid_test_() ->
    Scenarios = [
        fortune_5_workflow(),
        order_processing_workflow(),
        multi_stage_approval_workflow(),
        resource_intensive_workflow(),
        cancellation_prone_workflow()
    ],
    lists:map(fun(S) ->
        ?_assertEqual(ok, validate_scenario(S))
    end, Scenarios).

%% @doc Fortune 5 scenario produces valid workflow.
fortune_5_valid_test_() ->
    Scenario = fortune_5_workflow(),
    ?_assert(wf_term:is_valid(workflow(Scenario))).

%% @doc Order processing scenario produces valid workflow.
order_processing_valid_test_() ->
    Scenario = order_processing_workflow(),
    ?_assert(wf_term:is_valid(workflow(Scenario))).

%% @doc Multi-stage approval scenario produces valid workflow.
multi_stage_approval_valid_test_() ->
    Scenario = multi_stage_approval_workflow(),
    ?_assert(wf_term:is_valid(workflow(Scenario))).

%% @doc Resource-intensive scenario produces valid workflow.
resource_intensive_valid_test_() ->
    Scenario = resource_intensive_workflow(),
    ?_assert(wf_term:is_valid(workflow(Scenario))).

%% @doc Cancellation-prone scenario produces valid workflow.
cancellation_prone_valid_test_() ->
    Scenario = cancellation_prone_workflow(),
    ?_assert(wf_term:is_valid(workflow(Scenario))).

%% @doc Fortune 5 expected results are consistent.
fortune_5_expected_consistent_test_() ->
    Scenario = fortune_5_workflow(#{steps => 5}),
    ?_assertEqual(5, expected_receipts(Scenario)),
    ?_assertEqual(5 * 2, expected_steps(Scenario)).

%% @doc Order processing expected results include parallel branches.
order_processing_parallel_test_() ->
    Scenario = order_processing_workflow(#{parallel_branches => 3}),
    ExpectedCtx = expected_context(Scenario),
    ?_assertEqual(true, maps:get(order_validated, ExpectedCtx)),
    ?_assertEqual(true, maps:get(inventory_checked, ExpectedCtx)),
    ?_assertEqual(true, maps:get(payment_processed, ExpectedCtx)).

%% @doc Multi-stage approval expected results escalate correctly.
multi_stage_approval_escalation_test_() ->
    Scenario = multi_stage_approval_workflow(#{approval_levels => 3}),
    ExpectedCtx = expected_context(Scenario),
    ?_assertEqual(3, maps:get(approval_level, ExpectedCtx)).

%% @doc Resource-intensive workflow expects budget exceeded.
resource_intensive_budget_exceeded_test_() ->
    Scenario = resource_intensive_workflow(),
    ExpectedResults = maps:get(expected_results, Scenario),
    ?_assertMatch({error, budget_exceeded}, maps:get(termination, ExpectedResults)).

%% @doc Cancellation-prone workflow expects partial completion.
cancellation_prone_partial_test_() ->
    Scenario = cancellation_prone_workflow(#{cancellation_point => mid}),
    ExpectedResults = maps:get(expected_results, Scenario),
    ?_assertEqual(2, maps:get(receipt_count, ExpectedResults)),
    ?_assertEqual(cancelled, maps:get(termination, ExpectedResults)).
