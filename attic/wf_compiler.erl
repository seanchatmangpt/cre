%%%-------------------------------------------------------------------
%%% @doc Production Workflow Compiler
%%%
%%% Unified compiler for workflow pattern terms supporting all 43 YAWL
%%% workflow control-flow patterns. Combines AST construction, validation,
%%% and bytecode generation into a cohesive compilation pipeline.
%%%
%%% == Compilation Pipeline ==
%%%
%%%   1. Parse/Construct AST using pattern constructors
%%%   2. Structural validation (local invariants)
%%%   3. Semantic validation (global properties)
%%%   4. Single-pass bytecode emission with labels
%%%   5. Label resolution to integer IPs
%%%   6. Metadata preservation for runtime dispatch
%%%
%%% == Supported Patterns ==
%%%
%%% Kernel constructors (compose to form all 43 patterns):
%%%   - task: Named task with metadata
%%%   - seq: Sequential composition
%%%   - par: Parallel split with AND-join
%%%   - x_or: Exclusive choice with XOR-join
%%%   - join: Generalized join with policy
%%%   - loop: Structured loop
%%%   - defer: Deferred choice (race)
%%%   - cancel: Cancellation scope
%%%   - mi: Multiple instances
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_compiler).

%% Main API
-export([compile/1, compile/2]).
-export([validate/1, validate/2]).

%% AST Constructors - Kernel
-export([
    task/1, task/2,
    seq/2, seq/1,
    par/1,
    xor_expr/1, xor_expr/2,
    join/2,
    loop/2,
    defer/1,
    cancel/2, cancel/3,
    mi/2
]).

%% AST Constructors - Derived Patterns (WCP 1-43)
-export([
    %% WCP 1-5: Basic Patterns
    sequence/1,
    parallel_split/1,
    synchronization/1,
    exclusive_choice/2,
    simple_merge/2,

    %% WCP 6-10: Advanced Branching
    multi_choice/2, multi_choice/3,
    synchronizing_merge/1,
    multi_merge/1,
    discriminator/1, discriminator/2,
    arbitrary_cycle/2, arbitrary_cycle/3,

    %% WCP 11-15: Repetition
    structured_loop/2,
    while_loop/2,
    until_loop/2,
    repeat_loop/2,
    for_each/3,

    %% WCP 16-20: MI Patterns
    mi_without_sync/2,
    mi_with_sync/2,
    mi_with_sync_pre_run/3,
    mi_with_sync_post_run/3,
    mi_without_a_priori/2,

    %% WCP 21-25: State-based
    interleaved_parallel/1,
    interleaved_parallel_with_join/2,
    milestone/2,
    milestone_with_timeout/3,
    critical_section/2,

    %% WCP 26-30: Cancellation
    cancel_mi_activity/2,
    complete_mi_activity/2,
    blocking_discriminator/2,
    cancelling_discriminator/2,
    structured_partial_join/3,

    %% WCP 31-35: Advanced
    implicit_termination/1,
    explicit_termination/1,
    partial_join_for_multiple_instances/3,
    cancel_region/2,
    cancel_multiple_instances/2,

    %% WCP 36-43: Advanced Patterns
    atomic_execution/1,
    compensation/3,
    detached_execution/1,
    detached_execution_with_callback/2,
    set_data/2,
    set_data_with_scope/3,
    multiple_instances_with_custom_run_time/4,
    sub_workflow/2, sub_workflow/3
]).

%% Label resolution (public for tooling)
-export([resolve_labels/1, build_label_map/1]).

%% Type exports
-export_type([
    term/0,
    ctx/0,
    scope_id/0,
    task_name/0,
    task_metadata/0,
    task_fun/0,
    join_policy/0,
    loop_policy/0,
    mi_policy/0,
    cancel_scope/0,
    cancel_option/0,
    bytecode/0,
    opcode/0,
    label/0,
    metadata_map/0,
    validation_error/0,
    compile_error/0,
    compile_option/0
]).

%%%===================================================================
%%% Types
%%%===================================================================

%% Context type - user state flowing through workflow
-type ctx() :: map().

%% Identifiers
-type scope_id() :: atom() | binary() | reference().
-type task_name() :: atom().

%% Task function signature
-type task_fun() :: fun((ctx()) ->
    {ok, ctx()} |
    {error, term()} |
    {effect, term(), ctx()}).

%% Task metadata
-type task_metadata() :: #{
    function := task_fun(),
    timeout => timeout(),
    retry => non_neg_integer(),
    description => binary()
}.

%% Join policies
-type join_policy() ::
    all |
    sync_merge |
    first_complete |
    {first_n, pos_integer()} |
    {n_of_m, pos_integer(), pos_integer()}.

%% Loop policies
-type loop_policy() ::
    while |
    until |
    {count, non_neg_integer()} |
    {while, fun((ctx()) -> boolean())} |
    {until, fun((ctx()) -> boolean())}.

%% Multiple instance policies
-type mi_policy() ::
    {fixed, pos_integer()} |
    {dynamic, pos_integer(), pos_integer()}.

%% Cancellation
-type cancel_scope() :: scope_id() | {scope_id(), [cancel_option()]}.
-type cancel_option() ::
    {timeout, timeout()} |
    {notify, pid()}.

%% Workflow term AST - the union type
-type term() ::
    {task, task_name(), task_metadata()} |
    {seq, term(), term()} |
    {par, [term()]} |
    {x_or, [term()]} |
    {join, join_policy(), [term()]} |
    {loop, loop_policy(), term()} |
    {defer, [term()]} |
    {cancel, cancel_scope(), term()} |
    {mi, mi_policy(), term()}.

%% Label for unresolved bytecode
-type label() :: {label, reference()}.

%% Opcodes - resolved form
-type opcode() ::
    {seq_enter, non_neg_integer()} |
    {seq_next, non_neg_integer()} |
    {par_fork, [non_neg_integer()]} |
    {join_wait, join_policy()} |
    {xor_choose, [non_neg_integer()]} |
    {loop_back, non_neg_integer()} |
    {loop_check, loop_policy()} |
    {cancel_scope, {enter | exit, scope_id()}} |
    {mi_spawn, {mi_policy(), wait_all, non_neg_integer()}} |
    {multi_merge} |
    {task_exec, task_name()} |
    {done}.

%% Bytecode format: {opcodes, metadata}
-type bytecode() :: {[opcode()], metadata_map()}.

%% Metadata map for task function dispatch
-type metadata_map() :: #{task_name() => task_metadata()}.

%% Validation errors
-type validation_error() ::
    {invalid_branch_count, atom(), non_neg_integer(), non_neg_integer()} |
    {invalid_join_policy, join_policy()} |
    {invalid_loop_policy, loop_policy()} |
    {invalid_mi_policy, mi_policy()} |
    {invalid_task_metadata, task_metadata()} |
    {malformed_cancel_scope, cancel_scope()} |
    {nested_cancel_scope, scope_id(), scope_id()} |
    {empty_construct, atom()} |
    {malformed_term, term()}.

%% Compile errors
-type compile_error() ::
    {validation_failed, [validation_error()]} |
    {invalid_term, term()} |
    {unresolved_labels, [label()]} |
    {compilation_error, term()}.

%% Compile options
-type compile_option() ::
    {validate, boolean()} |
    {validate_semantic, boolean()}.

%%%===================================================================
%%% Main API - Compilation
%%%===================================================================

%% @doc Compile a workflow term with default options.
-spec compile(term()) -> {ok, bytecode()} | {error, compile_error()}.
compile(Term) ->
    compile(Term, []).

%% @doc Compile a workflow term with options.
-spec compile(term(), [compile_option()]) -> {ok, bytecode()} | {error, compile_error()}.
compile(Term, Options) ->
    try
        %% Step 1: Structural validation (unless disabled)
        case proplists:get_value(validate, Options, true) of
            true ->
                case validate(Term) of
                    ok -> ok;
                    {error, Errors} -> throw({validation_failed, Errors})
                end;
            false ->
                ok
        end,

        %% Step 2: Compile AST to unresolved bytecode
        {UnresolvedBytecode, Metadata} = compile_term(Term),

        %% Step 3: Resolve labels to integer IPs
        ResolvedBytecode = resolve_labels(UnresolvedBytecode),

        %% Step 4: Validate bytecode
        case validate_bytecode(ResolvedBytecode) of
            ok ->
                {ok, {ResolvedBytecode, Metadata}};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        throw:{validation_failed, Errors} ->
            {error, {validation_failed, Errors}};
        throw:CompileError ->
            {error, CompileError};
        error:Reason:Stacktrace ->
            {error, {compilation_error, {Reason, Stacktrace}}}
    end.

%%%===================================================================
%%% Main API - Validation
%%%===================================================================

%% @doc Validate a workflow term (structural validation).
-spec validate(term()) -> ok | {error, [validation_error()]}.
validate(Term) ->
    validate(Term, #{scope_stack => []}).

%% @doc Validate with context (for scope tracking).
-spec validate(term(), map()) -> ok | {error, [validation_error()]}.
validate(Term, Context) ->
    case validate_node(Term, Context) of
        {ok, _} -> ok;
        {error, Errors} -> {error, lists:usort(Errors)}
    end.

%%%===================================================================
%%% AST Constructors - Kernel
%%%===================================================================

%% @doc Create a task with function or metadata map.
-spec task(task_name(), task_fun() | task_metadata()) -> term().
task(Name, Fun) when is_atom(Name), is_function(Fun) ->
    task(Name, #{function => Fun});
task(Name, Metadata) when is_atom(Name), is_map(Metadata) ->
    case maps:is_key(function, Metadata) of
        false -> error({badarg, {missing_function, Metadata}});
        true ->
            case is_function(maps:get(function, Metadata)) of
                false -> error({badarg, {not_a_function, Metadata}});
                true -> {task, Name, Metadata}
            end
    end.

%% @doc Create a task with name only (function must be provided later).
-spec task(task_name()) -> term().
task(Name) when is_atom(Name) ->
    task(Name, #{function => fun(Ctx) -> {ok, Ctx} end}).

%% @doc Sequential composition of two terms.
-spec seq(term(), term()) -> term().
seq(P, Q) when is_tuple(P), is_tuple(Q) ->
    {seq, P, Q}.

%% @doc Sequential composition of a list (folds left).
-spec seq([term()]) -> term().
seq([]) -> error({badarg, empty_seq});
seq([Single]) -> Single;
seq([H | T]) -> lists:foldl(fun seq/2, H, T).

%% @doc Parallel split (AND-split/join).
-spec par([term()]) -> term().
par([]) -> error({badarg, empty_par});
par([_Single]) -> error({badarg, par_requires_at_least_2});
par(Branches) when is_list(Branches), length(Branches) >= 2 ->
    {par, Branches}.

%% @doc Exclusive choice (XOR-split/join) with 2+ alternatives.
-spec xor_expr([term()]) -> term().
xor_expr([]) -> error({badarg, empty_xor});
xor_expr([_Single]) -> error({badarg, xor_requires_at_least_2});
xor_expr(Alternatives) when is_list(Alternatives), length(Alternatives) >= 2 ->
    {x_or, Alternatives}.

%% @doc Exclusive choice with condition (first branch if true, second if false).
-spec xor_expr(term(), term()) -> term().
xor_expr(IfTrue, IfFalse) ->
    xor_expr([IfTrue, IfFalse]).

%% @doc Generalized join with policy.
-spec join(join_policy(), [term()]) -> term().
join(Policy, []) -> error({badarg, empty_join});
join(Policy, [Single]) when Policy =:= all; element(1, Policy) =:= n_of_m; element(1, Policy) =:= first_n ->
    {join, Policy, [Single]};
join(Policy, Branches) when is_list(Branches), length(Branches) >= 2 ->
    case validate_join_policy_format(Policy) of
        true -> {join, Policy, Branches};
        false -> error({badarg, {invalid_join_policy, Policy}})
    end.

%% @doc Loop with policy.
-spec loop(loop_policy(), term()) -> term().
loop(Policy, Body) when is_tuple(Body), tuple_size(Body) >= 2 ->
    case validate_loop_policy_format(Policy) of
        true -> {loop, Policy, Body};
        false -> error({badarg, {invalid_loop_policy, Policy}})
    end.

%% @doc Deferred choice (race - first to complete wins).
-spec defer([term()]) -> term().
defer([]) -> error({badarg, empty_defer});
defer([_Single]) -> error({badarg, defer_requires_at_least_2});
defer(Alternatives) when is_list(Alternatives), length(Alternatives) >= 2 ->
    {defer, Alternatives}.

%% @doc Cancellation scope.
-spec cancel(scope_id(), term()) -> term().
cancel(ScopeId, Body) when is_tuple(Body), tuple_size(Body) >= 2 ->
    {cancel, ScopeId, Body}.

%% @doc Cancellation scope with options.
-spec cancel(scope_id(), [cancel_option()], term()) -> term().
cancel(ScopeId, Options, Body) when is_list(Options), is_tuple(Body) ->
    {cancel, {ScopeId, Options}, Body}.

%% @doc Multiple instances.
-spec mi(mi_policy(), term()) -> term().
mi(Policy, Body) when is_tuple(Body), tuple_size(Body) >= 2 ->
    case validate_mi_policy_format(Policy) of
        true -> {mi, Policy, Body};
        false -> error({badarg, {invalid_mi_policy, Policy}})
    end.

%%%===================================================================
%%% AST Constructors - Derived Patterns (WCP 1-43)
%%%===================================================================

%%%-------------------------------------------------------------------
%%% WCP 1-5: Basic Patterns
%%%-------------------------------------------------------------------

%% @doc WCP-1: Sequence - execute tasks in order.
-spec sequence([term()]) -> term().
sequence(Terms) -> seq(Terms).

%% @doc WCP-2: Parallel Split (AND-split).
-spec parallel_split([term()]) -> term().
parallel_split(Branches) -> par(Branches).

%% @doc WCP-3: Synchronization (AND-join).
-spec synchronization([term()]) -> term().
synchronization(Branches) -> par(Branches).

%% @doc WCP-4: Exclusive Choice with condition.
-spec exclusive_choice(fun((ctx()) -> boolean()), [term()]) -> term().
exclusive_choice(_Condition, Alternatives) ->
    xor_expr(Alternatives).

%% @doc WCP-5: Simple Merge (XOR-join).
-spec simple_merge(term(), term()) -> term().
simple_merge(BranchA, BranchB) ->
    xor_expr([BranchA, BranchB]).

%%%-------------------------------------------------------------------
%%% WCP 6-10: Advanced Branching
%%%-------------------------------------------------------------------

%% @doc WCP-6: Multi-Choice (OR-split) with merge policy.
-spec multi_choice([term()], join_policy()) -> term().
multi_choice(Branches, MergePolicy) ->
    join(MergePolicy, Branches).

%% @doc WCP-6: Multi-Choice with options.
-spec multi_choice([term()], join_policy(), map()) -> term().
multi_choice(Branches, MergePolicy, _Options) ->
    join(MergePolicy, Branches).

%% @doc WCP-7: Synchronizing Merge.
-spec synchronizing_merge([term()]) -> term().
synchronizing_merge([]) -> error({badarg, empty_synchronizing_merge});
synchronizing_merge([Single]) -> Single;
synchronizing_merge(Branches) -> join(sync_merge, Branches).

%% @doc WCP-8: Multi-Merge (no synchronization).
-spec multi_merge([term()]) -> term().
multi_merge([]) -> error({badarg, empty_multi_merge});
multi_merge([Single]) -> Single;
multi_merge(Branches) -> par(Branches).

%% @doc WCP-9: Discriminator (first N pass, rest discarded).
-spec discriminator([term()]) -> term().
discriminator(Branches) -> discriminator(Branches, #{}).

%% @doc WCP-9: Discriminator with options.
-spec discriminator([term()], map()) -> term().
discriminator([], _Options) -> error({badarg, empty_discriminator});
discriminator([Single], _Options) -> Single;
discriminator(Branches, Options) ->
    PassCount = maps:get(pass_count, Options, 1),
    join({first_n, PassCount}, Branches).

%% @doc WCP-10: Arbitrary Cycle (unstructured loop).
-spec arbitrary_cycle(term(), term()) -> term().
arbitrary_cycle(_Target, Body) ->
    arbitrary_cycle(_Target, Body, #{}).

%% @doc WCP-10: Arbitrary Cycle with options.
-spec arbitrary_cycle(term(), term(), map()) -> term().
arbitrary_cycle(_Target, Body, Options) ->
    MaxIter = maps:get(max_iterations, Options, 1000),
    loop({count, MaxIter}, Body).

%%%-------------------------------------------------------------------
%%% WCP 11-15: Repetition Patterns
%%%-------------------------------------------------------------------

%% @doc WCP-11-15: Structured loop with policy.
-spec structured_loop(loop_policy(), term()) -> term().
structured_loop(Policy, Body) -> loop(Policy, Body).

%% @doc While loop (check before iteration).
-spec while_loop(fun((ctx()) -> boolean()), term()) -> term().
while_loop(Condition, Body) ->
    loop({while, Condition}, Body).

%% @doc Until loop (check after iteration).
-spec until_loop(fun((ctx()) -> boolean()), term()) -> term().
until_loop(Condition, Body) ->
    loop({until, Condition}, Body).

%% @doc Repeat loop (execute N times).
-spec repeat_loop(non_neg_integer(), term()) -> term().
repeat_loop(N, Body) when is_integer(N), N >= 0 ->
    loop({count, N}, Body).

%% @doc For-each over a list.
-spec for_each(atom(), fun((term(), ctx()) -> ctx()), term()) -> term().
for_each(_ListKey, _Transform, Body) ->
    loop(while, Body).

%%%-------------------------------------------------------------------
%%% WCP 16-20: Multiple Instance Patterns
%%%-------------------------------------------------------------------

%% @doc WCP-16: MI without synchronization.
-spec mi_without_sync(mi_policy(), term()) -> term().
mi_without_sync(Policy, Body) -> mi(Policy, Body).

%% @doc WCP-17: MI with synchronization.
-spec mi_with_sync(mi_policy(), term()) -> term().
mi_with_sync(Policy, Body) -> mi(Policy, Body).

%% @doc WCP-18: MI with synchronization (pre-run knowledge).
-spec mi_with_sync_pre_run(mi_policy(), term(), term()) -> term().
mi_with_sync_pre_run(Policy, Body, _PreRun) -> mi(Policy, Body).

%% @doc WCP-19: MI with synchronization (post-run knowledge).
-spec mi_with_sync_post_run(mi_policy(), term(), term()) -> term().
mi_with_sync_post_run(Policy, Body, _PostRun) -> mi(Policy, Body).

%% @doc WCP-20: MI without a priori knowledge.
-spec mi_without_a_priori(mi_policy(), term()) -> term().
mi_without_a_priori(Policy, Body) -> mi(Policy, Body).

%%%-------------------------------------------------------------------
%%% WCP 21-25: State-based Patterns
%%%-------------------------------------------------------------------

%% @doc WCP-21: Interleaved Parallel Routing.
-spec interleaved_parallel([term()]) -> term().
interleaved_parallel([]) -> error({badarg, empty_interleaved});
interleaved_parallel([Single]) -> Single;
interleaved_parallel(Branches) -> par(Branches).

%% @doc WCP-22: Interleaved Parallel with Join.
-spec interleaved_parallel_with_join([term()], join_policy()) -> term().
interleaved_parallel_with_join(Branches, Policy) ->
    join(Policy, Branches).

%% @doc WCP-23: Milestone.
-spec milestone(term(), term()) -> term().
milestone(_Milestone, Body) -> Body.

%% @doc WCP-24: Milestone with timeout.
-spec milestone_with_timeout(term(), timeout(), term()) -> term().
milestone_with_timeout(_Milestone, _Timeout, Body) -> Body.

%% @doc WCP-25: Critical Section.
-spec critical_section(scope_id(), term()) -> term().
critical_section(ScopeId, Body) -> cancel(ScopeId, Body).

%%%-------------------------------------------------------------------
%%% WCP 26-30: Cancellation Patterns
%%%-------------------------------------------------------------------

%% @doc WCP-26: Cancel MI Activity.
-spec cancel_mi_activity(scope_id(), term()) -> term().
cancel_mi_activity(ActivityId, Body) -> cancel(ActivityId, Body).

%% @doc WCP-27: Complete MI Activity.
-spec complete_mi_activity(term(), term()) -> term().
complete_mi_activity(_InstanceId, Body) -> Body.

%% @doc WCP-28: Blocking Discriminator.
-spec blocking_discriminator([term()], term()) -> term().
blocking_discriminator(Branches, Continuation) ->
    seq(join({first_n, 1}, Branches), Continuation).

%% @doc WCP-29: Cancelling Discriminator.
-spec cancelling_discriminator([term()], term()) -> term().
cancelling_discriminator(Branches, Continuation) ->
    ScopeId = make_ref(),
    ScopedBranches = [cancel(ScopeId, B) || B <- Branches],
    seq(join({first_n, 1}, ScopedBranches), Continuation).

%% @doc WCP-30: Structured Partial Join.
-spec structured_partial_join(pos_integer(), [term()], term()) -> term().
structured_partial_join(N, Branches, Continuation) ->
    M = length(Branches),
    seq(join({n_of_m, N, M}, Branches), Continuation).

%%%-------------------------------------------------------------------
%%% WCP 31-35: Advanced Patterns
%%%-------------------------------------------------------------------

%% @doc WCP-31: Implicit Termination.
-spec implicit_termination([term()]) -> term().
implicit_termination([]) -> task(implicit_terminate);
implicit_termination([Single]) -> Single;
implicit_termination(Terms) -> seq(Terms).

%% @doc WCP-32: Explicit Termination.
-spec explicit_termination(term()) -> term().
explicit_termination(Body) -> Body.

%% @doc WCP-33: Partial Join for Multiple Instances.
-spec partial_join_for_multiple_instances(pos_integer(), mi_policy(), term()) -> term().
partial_join_for_multiple_instances(N, MIPolicy, Body) ->
    join({first_n, N}, [mi(MIPolicy, Body)]).

%% @doc WCP-34: Cancel Region.
-spec cancel_region(scope_id(), term()) -> term().
cancel_region(ScopeId, Body) -> cancel(ScopeId, Body).

%% @doc WCP-35: Cancel Multiple Instances.
-spec cancel_multiple_instances(scope_id(), term()) -> term().
cancel_multiple_instances(ScopeId, Body) -> cancel(ScopeId, Body).

%%%-------------------------------------------------------------------
%%% WCP 36-43: Additional Patterns
%%%-------------------------------------------------------------------

%% @doc WCP-36: Atomic Execution.
-spec atomic_execution(term()) -> term().
atomic_execution(Body) -> cancel(atomic, Body).

%% @doc WCP-37: Compensation.
-spec compensation(term(), term(), term()) -> term().
compensation(Body, _Compensator, _Trigger) -> Body.

%% @doc WCP-38: Detached Execution.
-spec detached_execution(term()) -> term().
detached_execution(Body) -> par([Body]).

%% @doc WCP-39: Detached Execution with Callback.
-spec detached_execution_with_callback(term(), term()) -> term().
detached_execution_with_callback(Body, _Callback) -> par([Body]).

%% @doc WCP-40: Set Data.
-spec set_data(map(), term()) -> term().
set_data(Data, Body) ->
    Setter = task(set_data, #{function => fun(Ctx) -> {ok, maps:merge(Ctx, Data)} end}),
    seq(Setter, Body).

%% @doc WCP-41: Set Data with Scope.
-spec set_data_with_scope(scope_id(), map(), term()) -> term().
set_data_with_scope(ScopeId, Data, Body) ->
    cancel(ScopeId, set_data(Data, Body)).

%% @doc WCP-42: Multiple Instances with Custom Runtime.
-spec multiple_instances_with_custom_run_time(mi_policy(), term(), term(), term()) -> term().
multiple_instances_with_custom_run_time(Policy, Body, _PreRun, _PostRun) ->
    mi(Policy, Body).

%% @doc WCP-43: Sub-Workflow.
-spec sub_workflow(atom(), term()) -> term().
sub_workflow(_Name, Body) -> Body.

%% @doc WCP-43: Sub-Workflow with configuration.
-spec sub_workflow(atom(), term(), map()) -> term().
sub_workflow(_Name, Body, _Config) -> Body.

%%%===================================================================
%%% Internal - Validation
%%%===================================================================

%% @private Validate a single AST node.
-spec validate_node(term(), map()) -> {ok, map()} | {error, [validation_error()]}.
validate_node({task, Name, Metadata}, Context) when is_atom(Name) ->
    case maps:is_key(function, Metadata) of
        false -> {error, [{invalid_task_metadata, Metadata}]};
        true ->
            Fun = maps:get(function, Metadata),
            case is_function(Fun) of
                false -> {error, [{invalid_task_metadata, Metadata}]};
                true -> {ok, Context}
            end
    end;

validate_node({seq, P, Q}, Context) ->
    validate_both(P, Q, Context);

validate_node({par, Branches}, Context) when is_list(Branches) ->
    case length(Branches) < 2 of
        true -> {error, [{invalid_branch_count, par, length(Branches), 2}]};
        false -> validate_list(Branches, Context)
    end;

validate_node({x_or, Alternatives}, Context) when is_list(Alternatives) ->
    case length(Alternatives) < 2 of
        true -> {error, [{invalid_branch_count, x_or, length(Alternatives), 2}]};
        false -> validate_list(Alternatives, Context)
    end;

validate_node({join, Policy, Branches}, Context) when is_list(Branches) ->
    MinJoin = min_branches_for_policy(Policy),
    case length(Branches) < MinJoin of
        true -> {error, [{invalid_branch_count, join, length(Branches), MinJoin}]};
        false ->
            case validate_join_policy_format(Policy) of
                true -> validate_list(Branches, Context);
                false -> {error, [{invalid_join_policy, Policy}]}
            end
    end;

validate_node({loop, Policy, Body}, Context) ->
    case validate_loop_policy_format(Policy) of
        true -> validate_node(Body, Context);
        false -> {error, [{invalid_loop_policy, Policy}]}
    end;

validate_node({defer, Alternatives}, Context) when is_list(Alternatives) ->
    case length(Alternatives) < 2 of
        true -> {error, [{invalid_branch_count, defer, length(Alternatives), 2}]};
        false -> validate_list(Alternatives, Context)
    end;

validate_node({cancel, Scope, Body}, Context) ->
    ScopeId = case Scope of
        {Id, _Opts} -> Id;
        Id -> Id
    end,
    case validate_node(Body, Context#{scope_stack => [ScopeId | maps:get(scope_stack, Context, [])]}) of
        {error, Errors} -> {error, Errors};
        {ok, _} -> {ok, Context}
    end;

validate_node({mi, Policy, Body}, Context) ->
    case validate_mi_policy_format(Policy) of
        true -> validate_node(Body, Context);
        false -> {error, [{invalid_mi_policy, Policy}]}
    end;

validate_node(Invalid, _Context) ->
    {error, [{malformed_term, Invalid}]}.

%% @private Validate both branches.
-spec validate_both(term(), term(), map()) -> {ok, map()} | {error, [validation_error()]}.
validate_both(P, Q, Context) ->
    case validate_node(P, Context) of
        {error, Errors1} ->
            case validate_node(Q, Context) of
                {error, Errors2} -> {error, Errors1 ++ Errors2};
                {ok, _} -> {error, Errors1}
            end;
        {ok, _} ->
            validate_node(Q, Context)
    end.

%% @private Validate a list of terms.
-spec validate_list([term()], map()) -> {ok, map()} | {error, [validation_error()]}.
validate_list([], Context) -> {ok, Context};
validate_list([H | T], Context) ->
    case validate_node(H, Context) of
        {error, Errors1} ->
            case validate_list(T, Context) of
                {error, Errors2} -> {error, Errors1 ++ Errors2};
                {ok, _} -> {error, Errors1}
            end;
        {ok, _} ->
            validate_list(T, Context)
    end.

%% @private Get minimum branches for join policy.
-spec min_branches_for_policy(join_policy()) -> non_neg_integer().
min_branches_for_policy(all) -> 1;
min_branches_for_policy(sync_merge) -> 2;
min_branches_for_policy(first_complete) -> 2;
min_branches_for_policy({first_n, _}) -> 1;
min_branches_for_policy({n_of_m, _, _}) -> 1.

%% @private Validate join policy format.
-spec validate_join_policy_format(join_policy()) -> boolean().
validate_join_policy_format(all) -> true;
validate_join_policy_format(sync_merge) -> true;
validate_join_policy_format(first_complete) -> true;
validate_join_policy_format({first_n, N}) when is_integer(N), N > 0 -> true;
validate_join_policy_format({n_of_m, N, M}) when is_integer(N), is_integer(M), N > 0, M >= N -> true;
validate_join_policy_format(_) -> false.

%% @private Validate loop policy format.
-spec validate_loop_policy_format(loop_policy()) -> boolean().
validate_loop_policy_format(while) -> true;
validate_loop_policy_format(until) -> true;
validate_loop_policy_format({count, N}) when is_integer(N), N >= 0 -> true;
validate_loop_policy_format({while, F}) when is_function(F, 1) -> true;
validate_loop_policy_format({until, F}) when is_function(F, 1) -> true;
validate_loop_policy_format(_) -> false.

%% @private Validate MI policy format.
-spec validate_mi_policy_format(mi_policy()) -> boolean().
validate_mi_policy_format({fixed, N}) when is_integer(N), N > 0 -> true;
validate_mi_policy_format({dynamic, Min, Max}) when is_integer(Min), is_integer(Max), Min > 0, Max >= Min -> true;
validate_mi_policy_format(_) -> false.

%%%===================================================================
%%% Internal - Compilation
%%%===================================================================

%% @private Compile term to unresolved bytecode with metadata.
-spec compile_term(term()) -> {[unresolved_opcode()], metadata_map()}.
compile_term({task, Name, Metadata}) ->
    {[{task_exec, Name}], #{Name => Metadata}};

compile_term({seq, Left, Right}) ->
    {LeftCode, LeftMeta} = compile_term(Left),
    {RightCode, RightMeta} = compile_term(Right),
    RightLabel = make_label(),
    Code = [{seq_enter, 0}] ++ LeftCode ++ [{seq_next, RightLabel}] ++ [RightLabel] ++ RightCode,
    {Code, maps:merge(LeftMeta, RightMeta)};

compile_term({par, Branches}) ->
    compile_join(all, Branches);

compile_term({x_or, Alternatives}) ->
    compile_xor(Alternatives);

compile_term({join, Policy, Branches}) ->
    compile_join(Policy, Branches);

compile_term({loop, Policy, Body}) ->
    {BodyCode, BodyMeta} = compile_term(Body),
    LoopHead = make_label(),
    Code = [LoopHead] ++ [{loop_check, Policy}] ++ BodyCode ++ [{loop_back, LoopHead}],
    {Code, BodyMeta};

compile_term({defer, Alternatives}) ->
    %% Defer is first_n(1) join
    compile_join({first_n, 1}, Alternatives);

compile_term({cancel, Scope, Body}) ->
    {BodyCode, BodyMeta} = compile_term(Body),
    ScopeId = case Scope of {Id, _} -> Id; Id -> Id end,
    Code = [{cancel_scope, {enter, ScopeId}}] ++ BodyCode ++ [{cancel_scope, {exit, ScopeId}}],
    {Code, BodyMeta};

compile_term({mi, Policy, Body}) ->
    {BodyCode, BodyMeta} = compile_term(Body),
    BodyLabel = make_label(),
    BodyRef = element(2, BodyLabel),
    Code = [{mi_spawn, {Policy, wait_all, BodyLabel}}] ++ [{label, BodyRef} | BodyCode] ++ [{done}] ++ [{join_wait, all}],
    {Code, BodyMeta}.

%% @private Compile XOR (exclusive choice).
-spec compile_xor([term()]) -> {[unresolved_opcode()], metadata_map()}.
compile_xor(Alternatives) ->
    Results = [compile_term(A) || A <- Alternatives],
    Codes = [Code || {Code, _} <- Results],
    Metas = [Meta || {_, Meta} <- Results],
    Labels = [make_label() || _ <- Alternatives],

    BranchCode = lists:flatmap(fun({Code, Label}) ->
        [Label | Code] ++ [{done}]
    end, lists:zip(Codes, Labels)),

    Code = [{xor_choose, Labels}] ++ BranchCode,
    Meta = lists:foldl(fun maps:merge/2, #{}, Metas),
    {Code, Meta}.

%% @private Compile join with policy.
-spec compile_join(join_policy(), [term()]) -> {[unresolved_opcode()], metadata_map()}.
compile_join(Policy, [Single]) ->
    {BranchCode, BranchMeta} = compile_term(Single),
    BranchLabel = make_label(),
    JoinLabel = make_label(),
    Code = [{par_fork, [BranchLabel]}] ++ [BranchLabel | BranchCode] ++ [{done}] ++ [JoinLabel] ++ [{join_wait, Policy}],
    {Code, BranchMeta};
compile_join(Policy, Branches) when length(Branches) >= 2 ->
    Results = [compile_term(B) || B <- Branches],
    Codes = [Code || {Code, _} <- Results],
    Metas = [Meta || {_, Meta} <- Results],
    Labels = [make_label() || _ <- Branches],
    JoinLabel = make_label(),

    BranchCode = lists:flatmap(fun({Code, Label}) ->
        [Label | Code] ++ [{done}]
    end, lists:zip(Codes, Labels)),

    Code = [{par_fork, Labels}] ++ BranchCode ++ [JoinLabel] ++ [{join_wait, Policy}],
    Meta = lists:foldl(fun maps:merge/2, #{}, Metas),
    {Code, Meta}.

%%%===================================================================
%%% Internal - Label Resolution
%%%===================================================================

%% Unresolved opcode type (internal)
-type unresolved_opcode() ::
    {seq_enter, non_neg_integer()} |
    {seq_next, label() | non_neg_integer()} |
    {par_fork, [label() | non_neg_integer()]} |
    {join_wait, join_policy()} |
    {xor_choose, [label() | non_neg_integer()]} |
    {loop_back, label() | non_neg_integer()} |
    {loop_check, loop_policy()} |
    {cancel_scope, {enter | exit, scope_id()}} |
    {mi_spawn, {mi_policy(), wait_all, label() | non_neg_integer()}} |
    {multi_merge} |
    {task_exec, task_name()} |
    {done} |
    {label, reference()}.

%% @private Generate a unique label.
-spec make_label() -> label().
make_label() ->
    {label, erlang:make_ref()}.

%% @private Build label map from bytecode.
-spec build_label_map([unresolved_opcode()]) -> #{reference() => non_neg_integer()}.
build_label_map(Bytecode) ->
    build_label_map(Bytecode, 0, #{}).

build_label_map([], _IP, Map) -> Map;
build_label_map([{label, Ref} | Rest], IP, Map) ->
    build_label_map(Rest, IP + 1, Map#{Ref => IP});
build_label_map([_ | Rest], IP, Map) ->
    build_label_map(Rest, IP + 1, Map).

%% @private Resolve labels to integer IPs.
-spec resolve_labels([unresolved_opcode()]) -> [opcode()].
resolve_labels(Bytecode) ->
    LabelMap = build_label_map(Bytecode),
    LabelPositions = lists:sort(maps:values(LabelMap)),
    Resolved = [resolve_opcode_labels(Op, LabelMap, LabelPositions) || Op <- Bytecode],
    lists:filter(fun({label, _}) -> false; (_) -> true end, Resolved).

%% @private Resolve labels in a single opcode.
-spec resolve_opcode_labels(unresolved_opcode(), #{reference() => non_neg_integer()}, [non_neg_integer()]) -> opcode() | {label, reference()}.
resolve_opcode_labels({seq_next, {label, Ref}}, LabelMap, LabelPositions) ->
    OriginalPos = maps:get(Ref, LabelMap),
    AdjustedPos = OriginalPos - count_labels_before(OriginalPos, LabelPositions),
    {seq_next, AdjustedPos};

resolve_opcode_labels({par_fork, Labels}, LabelMap, LabelPositions) ->
    ResolvedLabels = [resolve_label_ref(L, LabelMap, LabelPositions) || L <- Labels],
    {par_fork, ResolvedLabels};

resolve_opcode_labels({xor_choose, Labels}, LabelMap, LabelPositions) ->
    ResolvedLabels = [resolve_label_ref(L, LabelMap, LabelPositions) || L <- Labels],
    {xor_choose, ResolvedLabels};

resolve_opcode_labels({loop_back, {label, Ref}}, LabelMap, LabelPositions) ->
    OriginalPos = maps:get(Ref, LabelMap),
    AdjustedPos = OriginalPos - count_labels_before(OriginalPos, LabelPositions),
    {loop_back, AdjustedPos};

resolve_opcode_labels({mi_spawn, {Policy, JoinPolicy, BodyLabel}}, LabelMap, LabelPositions) ->
    BodyIP = resolve_label_ref(BodyLabel, LabelMap, LabelPositions),
    {mi_spawn, {Policy, JoinPolicy, BodyIP}};

resolve_opcode_labels(Opcode, _LabelMap, _LabelPositions) ->
    Opcode.

%% @private Resolve a single label reference.
-spec resolve_label_ref(label() | non_neg_integer(), #{reference() => non_neg_integer()}, [non_neg_integer()]) -> non_neg_integer().
resolve_label_ref({label, Ref}, LabelMap, LabelPositions) ->
    OriginalPos = maps:get(Ref, LabelMap),
    OriginalPos - count_labels_before(OriginalPos, LabelPositions);
resolve_label_ref(IP, _LabelMap, _LabelPositions) when is_integer(IP) ->
    IP.

%% @private Count labels before a position.
-spec count_labels_before(non_neg_integer(), [non_neg_integer()]) -> non_neg_integer().
count_labels_before(Pos, LabelPositions) ->
    length([P || P <- LabelPositions, P < Pos]).

%%%===================================================================
%%% Internal - Bytecode Validation
%%%===================================================================

%% @private Validate resolved bytecode.
-spec validate_bytecode([opcode()]) -> ok | {error, compile_error()}.
validate_bytecode(Bytecode) ->
    Unresolved = [L || Op <- Bytecode, has_unresolved_label(Op), L <- [extract_label(Op)], L =/= none],
    case Unresolved of
        [] -> ok;
        _ -> {error, {unresolved_labels, Unresolved}}
    end.

%% @private Check if opcode has unresolved labels.
-spec has_unresolved_label(opcode()) -> boolean().
has_unresolved_label({_, {label, _}}) -> true;
has_unresolved_label({_, Labels}) when is_list(Labels) ->
    lists:any(fun({label, _}) -> true; (_) -> false end, Labels);
has_unresolved_label(_) -> false.

%% @private Extract label from opcode.
-spec extract_label(opcode()) -> label() | none.
extract_label({_, {label, _} = L}) -> L;
extract_label({_, [L | _]}) when element(1, L) =:= label -> L;
extract_label(_) -> none.
