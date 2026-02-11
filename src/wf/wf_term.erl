%%% @doc WF Substrate AST and Pattern Constructors
%%%
%%% This module defines the workflow pattern algebra (AST) and provides
%%% constructor functions for building patterns. The algebra is closed and
%%% generated only by the kernel constructors defined here.
%%%
%%% Patterns are immutable term structures that compile to bytecode.
%%%
%%% @end
-module(wf_term).

%% Constructor API
-export([
    %% Kernel constructors
    task/2,
    seq/2,
    par/1,
    choice/1,
    join/2,
    loop/2,
    defer/1,
    cancel_scope/2,
    mi/2,

    %% Utility functions
    is_valid/1,
    term_size/1,
    to_string/1
]).

%% Type exports
-export_type([
    wf_term/0,
    task_name/0,
    task_fun/0,
    join_policy/0,
    loop_policy/0,
    mi_policy/0,
    scope_spec/0,
    context/0,
    effect_spec/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

%% Core workflow pattern type
-type wf_term() ::
      {task, task_name(), task_fun()}
    | {seq, wf_term(), wf_term()}
    | {par, [wf_term()]}
    | {choice, [wf_term()]}
    | {join, join_policy(), [wf_term()]}
    | {loop, loop_policy(), wf_term()}
    | {defer, [wf_term()]}
    | {cancel_scope, scope_spec(), wf_term()}
    | {mi, mi_policy(), wf_term()}.

%% Task definition
-type task_name() :: atom() | {atom(), term()}.
-type task_fun() :: fun((context()) ->
    {ok, context()}
    | {error, term()}
    | {effect, effect_spec(), context()}).

%% Join policies for synchronization
-type join_policy() ::
      all                      % AND-join: wait for all branches
    | xor_merge                % XOR-join: take first available, discard others
    | sync_merge               % Synchronizing merge: coordinate branches
    | {first_n, non_neg_integer()}      % Wait for first N to complete
    | {n_of_m, non_neg_integer(), non_neg_integer()}.  % N out of M join

%% Loop policies
-type loop_policy() ::
      {max_iter, non_neg_integer()}      % Loop at most N times
    | {while, fun((context()) -> boolean())}  % Loop while condition holds
    | {until, fun((context()) -> boolean())}.  % Loop until condition holds

%% Multiple instance policies
-type mi_policy() ::
      {fixed, non_neg_integer()}              % Fixed number of instances
    | {dynamic, fun((context()) -> {next, context()} | done)}.  % Dynamic collection

%% Cancellation scope specifier
-type scope_spec() ::
      {region, atom()}       % Named cancellation region
    | {activity, atom()}     % Single activity scope
    | {root_case, atom()}.   % Case-level scope

%% User context (passed through execution)
-type context() :: #{
    data => any(),              % User-provided data store
    signals => [term()],        % Inbound signals from external world
    results => #{atom() => any()},  % Effect results
    token_data => #{atom() => any()}  % Token-specific data
}.

%% Effect specification (yielded during task execution)
-type effect_spec() :: {
    effect,
    atom(),           % Effect type name
    any(),            % Payload
    atom()            % Effect ID (for receipt tracking)
}.

%%% KERNEL CONSTRUCTORS =====================================================

%% @doc Create a task node.
%%
%% A task is a named computation that may succeed, fail, or yield an effect.
%% The task function receives the current context and returns one of:
%% - {ok, UpdatedContext}
%% - {error, Reason}
%% - {effect, EffectSpec, ContextWithContinuation}
%%
%% @end
-spec task(Name :: task_name(), Fun :: task_fun()) -> wf_term().
task(Name, Fun) when is_atom(Name), is_function(Fun, 1) ->
    {task, Name, Fun};
task(Name, Fun) when is_tuple(Name), is_function(Fun, 1) ->
    {task, Name, Fun}.

%% @doc Create a sequence: execute P, then Q.
%%
%% The sequence P → Q executes P to completion, then executes Q using
%% P's resulting context.
%%
%% @end
-spec seq(P :: wf_term(), Q :: wf_term()) -> wf_term().
seq(P, Q) ->
    {seq, validate_term(P), validate_term(Q)}.

%% @doc Create a parallel split: execute all branches concurrently.
%%
%% par([P1, P2, ..., Pn]) forks into n concurrent branches and
%% implicitly waits for all (AND-join) to complete before proceeding.
%% Use join/2 to specify explicit join policy.
%%
%% @end
-spec par(Branches :: [wf_term()]) -> wf_term().
par(Branches) when is_list(Branches), length(Branches) > 0 ->
    {par, [validate_term(B) || B <- Branches]}.

%% @doc Create an exclusive choice: execute exactly one branch.
%%
%% choice([P1, P2, ..., Pn]) evaluates branch conditions (if any) and
%% executes the first enabled branch, cancelling others.
%% Used for control-flow choice where only one path is taken.
%%
%% @end
-spec choice(Branches :: [wf_term()]) -> wf_term().
choice(Branches) when is_list(Branches), length(Branches) > 0 ->
    {choice, [validate_term(B) || B <- Branches]}.

%% @doc Create a generalized join with explicit policy.
%%
%% join(Policy, [P1, P2, ..., Pn]) merges multiple branches using
%% the specified join policy. Common policies:
%% - all: AND-join (wait for all)
%% - xor_merge: take first available
%% - sync_merge: synchronized merge
%% - {first_n, N}: wait for first N
%% - {n_of_m, N, M}: wait for N out of M
%%
%% @end
-spec join(Policy :: join_policy(), Branches :: [wf_term()]) -> wf_term().
join(Policy, Branches) when is_list(Branches), length(Branches) > 0 ->
    true = is_valid_join_policy(Policy),
    {join, Policy, [validate_term(B) || B <- Branches]}.

%% @doc Create a loop: execute P repeatedly under control of condition.
%%
%% loop(Policy, P) executes P in a loop. Policy determines loop control:
%% - {max_iter, N}: execute at most N times
%% - {while, Fun}: execute while Fun(ctx) = true
%% - {until, Fun}: execute until Fun(ctx) = true
%%
%% @end
-spec loop(Policy :: loop_policy(), Body :: wf_term()) -> wf_term().
loop(Policy, Body) when is_tuple(Policy) ->
    true = is_valid_loop_policy(Policy),
    {loop, Policy, validate_term(Body)}.

%% @doc Create a deferred choice: race external events with branches.
%%
%% defer([P1, P2, ..., Pn]) waits for an external signal or the first
%% branch to enable. Once triggered, that branch executes and others
%% are cancelled.
%%
%% @end
-spec defer(Branches :: [wf_term()]) -> wf_term().
defer(Branches) when is_list(Branches), length(Branches) > 0 ->
    {defer, [validate_term(B) || B <- Branches]}.

%% @doc Create a cancellation scope.
%%
%% cancel_scope(ScopeSpec, P) wraps P in a cancellation scope. If the scope
%% receives a cancel signal, all activities within it halt immediately.
%% ScopeSpec identifies the scope:
%% - {region, RegionId}: named cancellation region
%% - {activity, ActivityId}: single activity scope
%% - {root_case, CaseId}: case-level scope
%%
%% @end
-spec cancel_scope(ScopeSpec :: scope_spec(), Body :: wf_term()) -> wf_term().
cancel_scope(ScopeSpec, Body) when is_tuple(ScopeSpec) ->
    true = is_valid_scope_spec(ScopeSpec),
    {cancel_scope, ScopeSpec, validate_term(Body)}.

%% @doc Create multiple instances.
%%
%% mi(Policy, P) creates and manages multiple instances of P.
%% Policy determines instance count and joining:
%% - {fixed, N}: create exactly N instances
%% - {dynamic, CollectorFun}: create instances while CollectorFun returns {next, data}
%%
%% @end
-spec mi(Policy :: mi_policy(), Body :: wf_term()) -> wf_term().
mi(Policy, Body) when is_tuple(Policy) ->
    true = is_valid_mi_policy(Policy),
    {mi, Policy, validate_term(Body)}.

%%% UTILITY FUNCTIONS =======================================================

%% @doc Check if a term is a valid workflow pattern.
-spec is_valid(Term :: term()) -> boolean().
is_valid({task, Name, Fun}) ->
    is_task_name(Name) andalso is_function(Fun, 1);
is_valid({seq, P, Q}) ->
    is_valid(P) andalso is_valid(Q);
is_valid({par, Branches}) ->
    is_list(Branches) andalso length(Branches) > 0 andalso
    lists:all(fun is_valid/1, Branches);
is_valid({choice, Branches}) ->
    is_list(Branches) andalso length(Branches) > 0 andalso
    lists:all(fun is_valid/1, Branches);
is_valid({join, Policy, Branches}) ->
    is_valid_join_policy(Policy) andalso
    is_list(Branches) andalso length(Branches) > 0 andalso
    lists:all(fun is_valid/1, Branches);
is_valid({loop, Policy, Body}) ->
    is_valid_loop_policy(Policy) andalso is_valid(Body);
is_valid({defer, Branches}) ->
    is_list(Branches) andalso length(Branches) > 0 andalso
    lists:all(fun is_valid/1, Branches);
is_valid({cancel_scope, ScopeSpec, Body}) ->
    is_valid_scope_spec(ScopeSpec) andalso is_valid(Body);
is_valid({mi, Policy, Body}) ->
    is_valid_mi_policy(Policy) andalso is_valid(Body);
is_valid(_) ->
    false.

%% @doc Measure the size of a pattern term (for statistics).
-spec term_size(Term :: wf_term()) -> non_neg_integer().
term_size({task, _, _}) -> 1;
term_size({seq, P, Q}) -> 1 + term_size(P) + term_size(Q);
term_size({par, Branches}) -> 1 + lists:sum([term_size(B) || B <- Branches]);
term_size({choice, Branches}) -> 1 + lists:sum([term_size(B) || B <- Branches]);
term_size({join, _, Branches}) -> 1 + lists:sum([term_size(B) || B <- Branches]);
term_size({loop, _, Body}) -> 1 + term_size(Body);
term_size({defer, Branches}) -> 1 + lists:sum([term_size(B) || B <- Branches]);
term_size({cancel_scope, _, Body}) -> 1 + term_size(Body);
term_size({mi, _, Body}) -> 1 + term_size(Body).

%% @doc Convert a pattern term to a readable string.
-spec to_string(Term :: wf_term()) -> string().
to_string({task, Name, _Fun}) ->
    io_lib:format("task(~w)", [Name]);
to_string({seq, P, Q}) ->
    io_lib:format("seq(~s, ~s)", [to_string(P), to_string(Q)]);
to_string({par, Branches}) ->
    BranchStrs = [to_string(B) || B <- Branches],
    io_lib:format("par([~s])", [string:join(BranchStrs, ", ")]);
to_string({choice, Branches}) ->
    BranchStrs = [to_string(B) || B <- Branches],
    io_lib:format("choice([~s])", [string:join(BranchStrs, ", ")]);
to_string({join, Policy, Branches}) ->
    BranchStrs = [to_string(B) || B <- Branches],
    io_lib:format("join(~w, [~s])", [Policy, string:join(BranchStrs, ", ")]);
to_string({loop, Policy, Body}) ->
    io_lib:format("loop(~w, ~s)", [Policy, to_string(Body)]);
to_string({defer, Branches}) ->
    BranchStrs = [to_string(B) || B <- Branches],
    io_lib:format("defer([~s])", [string:join(BranchStrs, ", ")]);
to_string({cancel_scope, ScopeSpec, Body}) ->
    io_lib:format("cancel_scope(~w, ~s)", [ScopeSpec, to_string(Body)]);
to_string({mi, Policy, Body}) ->
    io_lib:format("mi(~w, ~s)", [Policy, to_string(Body)]).

%%% INTERNAL VALIDATION =====================================================

-spec validate_term(Term :: wf_term()) -> wf_term().
validate_term(Term) ->
    case is_valid(Term) of
        true -> Term;
        false -> error({invalid_term, Term})
    end.

-spec is_task_name(Name :: term()) -> boolean().
is_task_name(Name) when is_atom(Name) -> true;
is_task_name(Name) when is_tuple(Name) ->
    size(Name) >= 1 andalso is_atom(element(1, Name));
is_task_name(_) -> false.

-spec is_valid_join_policy(Policy :: term()) -> boolean().
is_valid_join_policy(all) -> true;
is_valid_join_policy(xor_merge) -> true;
is_valid_join_policy(sync_merge) -> true;
is_valid_join_policy({first_n, N}) -> is_integer(N), N >= 1;
is_valid_join_policy({n_of_m, N, M}) ->
    is_integer(N), is_integer(M), N >= 1, M >= N;
is_valid_join_policy(_) -> false.

-spec is_valid_loop_policy(Policy :: term()) -> boolean().
is_valid_loop_policy({max_iter, N}) -> is_integer(N), N >= 1;
is_valid_loop_policy({while, F}) -> is_function(F, 1);
is_valid_loop_policy({until, F}) -> is_function(F, 1);
is_valid_loop_policy(_) -> false.

-spec is_valid_mi_policy(Policy :: term()) -> boolean().
is_valid_mi_policy({fixed, N}) -> is_integer(N), N >= 1;
is_valid_mi_policy({dynamic, F}) -> is_function(F, 1);
is_valid_mi_policy(_) -> false.

-spec is_valid_scope_spec(Spec :: term()) -> boolean().
is_valid_scope_spec({region, Id}) -> is_atom(Id);
is_valid_scope_spec({activity, Id}) -> is_atom(Id);
is_valid_scope_spec({root_case, Id}) -> is_atom(Id);
is_valid_scope_spec(_) -> false.

%%% TESTS ===================================================================

%% Unit tests for basic pattern construction
constructor_test_() ->
    [
        ?_assert(is_valid(task(simple_task, fun(_) -> {ok, #{}} end))),
        ?_assert(is_valid(seq(
            task(a, fun(_) -> {ok, #{}} end),
            task(b, fun(_) -> {ok, #{}} end)
        ))),
        ?_assert(is_valid(par([
            task(a, fun(_) -> {ok, #{}} end),
            task(b, fun(_) -> {ok, #{}} end)
        ]))),
        ?_assert(is_valid(choice([
            task(a, fun(_) -> {ok, #{}} end),
            task(b, fun(_) -> {ok, #{}} end)
        ]))),
        ?_assert(is_valid(join(all, [
            task(a, fun(_) -> {ok, #{}} end),
            task(b, fun(_) -> {ok, #{}} end)
        ]))),
        ?_assert(is_valid(loop({max_iter, 5}, task(loop_task, fun(_) -> {ok, #{}} end)))),
        ?_assert(is_valid(cancel_scope({region, my_region}, task(t, fun(_) -> {ok, #{}} end)))),
        ?_assert(is_valid(mi({fixed, 3}, task(mi_task, fun(_) -> {ok, #{}} end))))
    ].

%% Test invalid patterns
invalid_test_() ->
    [
        ?_assertNot(is_valid({task, invalid_name, not_a_function})),
        ?_assertNot(is_valid({seq, task(a, fun(_) -> {ok, #{}} end)})),  % missing Q
        ?_assertNot(is_valid({par, []})),  % empty branches
        ?_assertNot(is_valid({join, invalid_policy, []}))
    ].

%% Test term size calculation
term_size_test_() ->
    [
        ?_assertEqual(1, term_size(task(t, fun(_) -> {ok, #{}} end))),
        ?_assertEqual(3, term_size(seq(
            task(a, fun(_) -> {ok, #{}} end),
            task(b, fun(_) -> {ok, #{}} end)
        ))),
        ?_assertEqual(3, term_size(par([
            task(a, fun(_) -> {ok, #{}} end),
            task(b, fun(_) -> {ok, #{}} end)
        ])))
    ].

