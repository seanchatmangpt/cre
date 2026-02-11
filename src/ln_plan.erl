%%%-------------------------------------------------------------------
%%% @doc ln_plan - Plan constructors for choreography workflows.
%%%
%%% This module provides constructors for building plan terms that
%%% define choreography structure. Plans are compiled to bytecode
%%% by ln_compile for efficient execution.
%%%
%%% <h2>Plan Term Types</h2>
%%% <ul>
%%%   <li>`{task, TaskId}' - Execute a single task</li>
%%%   <li>`{seq, [Plan]}' - Sequential execution</li>
%%%   <li>`{par, [Plan]}' - Parallel execution (fork)</li>
%%%   <li>`{choice, [Plan]}' - Exclusive choice</li>
%%%   <li>`{join, Policy, [Plan]}' - Parallel join with policy</li>
%%%   <li>`{loop, Policy, Plan}' - Loop construct</li>
%%%   <li>`{defer, [Plan]}' - External choice (race)</li>
%%%   <li>`{scope, ScopeId, Plan}' - Cancellation scope</li>
%%%   <li>`{mi, Policy, Plan}' - Multiple instances</li>
%%%   <li>`{wait, MatchSpec}' - Wait for signal</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_plan).

%% Plan constructors
-export([task/1]).
-export([seq/1]).
-export([par/1]).
-export([choice/1]).
-export([join/2]).
-export([loop/2]).
-export([defer/1]).
-export([scope/2]).
-export([mi/2]).
-export([wait/1]).

%% Validation
-export([validate/1]).

%% Type exports
-export_type([plan/0, task_id/0, scope_id/0, policy/0, join_policy/0, loop_policy/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type task_id() :: atom().
-type scope_id() :: atom() | binary().

-type join_policy() :: all
                     | {n_of_m, pos_integer()}
                     | first
                     | sync_merge.

-type loop_policy() :: while
                     | until
                     | times
                     | {timeout_ms, pos_integer()}
                     | {retry, #{max := pos_integer(), backoff_ms := pos_integer()}}.

-type mi_policy() :: {concurrent, pos_integer()}
                   | {sequential, pos_integer()}
                   | one_for_one.

-type policy() :: join_policy() | loop_policy() | mi_policy().

-type plan() :: {task, task_id()}
              | {seq, [plan()]}
              | {par, [plan()]}
              | {choice, [plan()]}
              | {join, join_policy(), [plan()]}
              | {loop, loop_policy(), plan()}
              | {defer, [plan()]}
              | {scope, scope_id(), plan()}
              | {mi, mi_policy(), plan()}
              | {wait, term()}.

%%%-------------------------------------------------------------------
%%% Plan Constructors
%%%-------------------------------------------------------------------

%% @doc Create a task node.
-spec task(task_id()) -> plan().
task(TaskId) when is_atom(TaskId) ->
    {task, TaskId}.

%% @doc Create a sequential composition of plans.
-spec seq([plan()]) -> plan().
seq([]) ->
    error({empty_seq, "seq cannot be empty"});
seq(Plans) when is_list(Plans) ->
    {seq, Plans}.

%% @doc Create a parallel composition of plans.
-spec par([plan()]) -> plan().
par([]) ->
    error({empty_par, "par cannot be empty"});
par(Plans) when is_list(Plans) ->
    {par, Plans}.

%% @doc Create an exclusive choice (xor) of plans.
-spec choice([plan()]) -> plan().
choice([]) ->
    error({empty_choice, "choice cannot be empty"});
choice(Plans) when is_list(Plans) ->
    {choice, Plans}.

%% @doc Create a join with policy.
-spec join(join_policy(), [plan()]) -> plan().
join(Policy, []) ->
    error({empty_join, "join cannot have empty branches"});
join(Policy, Plans) when is_list(Plans) ->
    {join, Policy, Plans}.

%% @doc Create a loop construct.
-spec loop(loop_policy(), plan()) -> plan().
loop(Policy, Plan) ->
    {loop, Policy, Plan}.

%% @doc Create a defer (external choice/race).
-spec defer([plan()]) -> plan().
defer([]) ->
    error({empty_defer, "defer cannot be empty"});
defer(Plans) when is_list(Plans) ->
    {defer, Plans}.

%% @doc Create a cancellation scope.
-spec scope(scope_id(), plan()) -> plan().
scope(ScopeId, Plan) when is_atom(ScopeId); is_binary(ScopeId) ->
    {scope, ScopeId, Plan}.

%% @doc Create multiple instances.
-spec mi(mi_policy(), plan()) -> plan().
mi(Policy, Plan) ->
    {mi, Policy, Plan}.

%% @doc Create a wait for signal.
-spec wait(term()) -> plan().
wait(MatchSpec) ->
    {wait, MatchSpec}.

%%%-------------------------------------------------------------------
%%% Validation
%%%-------------------------------------------------------------------

%% @doc Validate a plan term for well-formedness.
-spec validate(plan()) -> ok | {error, term()}.
validate({task, _TaskId}) ->
    ok;
validate({seq, Plans}) when is_list(Plans) ->
    case Plans of
        [] -> {error, empty_seq};
        _ -> validate_list(Plans)
    end;
validate({par, Plans}) when is_list(Plans) ->
    case Plans of
        [] -> {error, empty_par};
        _ -> validate_list(Plans)
    end;
validate({choice, Plans}) when is_list(Plans) ->
    case Plans of
        [] -> {error, empty_xor};
        _ -> validate_list(Plans)
    end;
validate({join, _Policy, Plans}) when is_list(Plans) ->
    case Plans of
        [] -> {error, empty_join};
        _ -> validate_list(Plans)
    end;
validate({loop, _Policy, Plan}) ->
    validate(Plan);
validate({defer, Plans}) when is_list(Plans) ->
    case Plans of
        [] -> {error, empty_defer};
        _ -> validate_list(Plans)
    end;
validate({scope, ScopeId, Plan}) when is_atom(ScopeId); is_binary(ScopeId) ->
    validate(Plan);
validate({mi, _Policy, Plan}) ->
    validate(Plan);
validate({wait, _MatchSpec}) ->
    ok;
validate(Other) ->
    {error, {invalid_plan, Other}}.

%% @doc Validate a list of plans.
validate_list([]) ->
    ok;
validate_list([Plan | Rest]) ->
    case validate(Plan) of
        ok -> validate_list(Rest);
        Error -> Error
    end.
