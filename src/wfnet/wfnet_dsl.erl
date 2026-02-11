%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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

-module(wfnet_dsl).
-moduledoc """
Workflow DSL for Composing Workflow Patterns.

This module provides a clean Erlang syntax for pattern composition with
operators for sequence, parallel, choice, loop, and nesting. It supports
compile-time validation and produces executable workflow specifications.

## DSL Syntax

The DSL uses nested tuples to compose patterns:

```erlang
Workflow = wfnet_dsl:compile([
    {sequence, [
        {parallel_split, [branch1, branch2, branch3]},
        {choice, [
            {loop, body_pattern, #{max_iterations => 5}},
            alternative_pattern
        ]},
        final_pattern
    ]}
])
```

## Pattern Operators

- **{sequence, [Patterns]}** - Execute patterns sequentially
- **{parallel, [Patterns]}** - Execute patterns concurrently
- **{parallel_split, [Patterns]}** - Split into parallel branches with synchronization
- **{choice, [Patterns]}** - Select exactly one branch (exclusive choice)
- **{multi_choice, [Patterns]}** - Select one or more branches
- **{loop, Body, Config}** - Repeat body while/until condition
- **{while, Condition, Body}** - While loop construct
- **{until, Condition, Body}** - Until loop construct
- **{nest, Pattern, Config}** - Create nested scope for pattern
- **{deferred_choice, [Patterns]}** - Deferred exclusive choice
- **{discriminator, [Patterns]}** - First to complete wins
- **{n_out_of_m, N, Patterns}** - Exactly N of M branches

## Configuration Options

Pattern configuration maps support:

- `max_iterations` - Maximum loop iterations (default: 1000)
- `timeout` - Execution timeout in milliseconds
- `condition` - Loop condition function
- `init_state` - Initial state for loops
- `scope_id` - Scope identifier for nesting
- `binding_table` - Variable binding for nested scopes

## Examples

### Simple Sequence

```erlang
> Seq = wfnet_dsl:compile([
>     {sequence, [task_a, task_b, task_c]}
> ]).
{ok, #{type => sequence, ...}}
```

### Parallel Execution

```erlang
> Par = wfnet_dsl:compile([
>     {parallel, [task_a, task_b, task_c]}
> ]).
{ok, #{type => parallel, ...}}
```

### Choice with Loop

```erlang
> ChoiceLoop = wfnet_dsl:compile([
>     {choice, [
>         {loop, retry_task, #{max_iterations => 3}},
>         fallback_task
>     ]}
> ]).
{ok, #{type => choice, ...}}
```

### Nested Workflow

```erlang
> Nested = wfnet_dsl:compile([
>     {sequence, [
>         init_task,
>         {nest, {parallel, [worker_a, worker_b]}, #{
>             scope_id => my_scope,
>             binding_table => #{input => output}
>         }},
>         cleanup_task
>     ]}
> ]).
{ok, #{type => sequence, ...}}
```

### Complex Composition

```erlang
> Complex = wfnet_dsl:compile([
>     {sequence, [
>         {parallel_split, [
>             validate_data,
>             check_permissions,
>             verify_signature
>         ]},
>         {choice, [
>             {sequence, [process_a, commit_a]},
>             {sequence, [process_b, commit_b]},
>             {sequence, [process_c, commit_c]}
>         ]},
>         {while, fun(S) -> S < 10 end, increment_step}
>     ]}
> ]).
{ok, #{type => sequence, ...}}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Compilation API
-export([compile/1, compile/2, from_ast/1, validate/1]).
-export([to_yaml/1, to_json/1]).

%% DSL Element Constructors
-export([sequence/1, parallel/1, parallel_split/1]).
-export([choice/1, multi_choice/1, deferred_choice/1]).
-export([loop/2, while/2, until/2]).
-export([nest/2, discriminator/1, n_out_of/2]).
-export([task/1, task/2]).
-export([conditional/2, retry/3]).

%% Query Functions
-export([type/1, config/1, children/1, is_valid/1]).
-export([find_patterns/2, count_patterns/1]).

%% Operators (syntactic sugar)
-export([then/2, par/2, either/2, times/3]).

%% Types
-type dsl_pattern() :: {atom(), term() | [dsl_pattern()]} |
                        {atom(), term(), term() | map()} |
                        atom() | function().
-type dsl_ast() :: #{type => atom(), children => [dsl_ast()],
                     config => map(), meta => map()}.
-type compile_result() :: {ok, dsl_ast()} | {error, compile_error()}.
-type compile_error() :: {validation_error, [binary()]} |
                         {parse_error, term()} |
                         {config_error, term()}.
-type validation_error() :: {error, term(), [binary()]}.

-export_type([dsl_pattern/0, dsl_ast/0, compile_result/0, compile_error/0]).

%%====================================================================
%% Records
%%====================================================================

-record(compilation_state, {
    depth = 0 :: non_neg_integer(),
    max_depth = 100 :: pos_integer(),
    scope_stack = [] :: [binary()],
    errors = [] :: [binary()],
    warnings = [] :: [binary()]
}).

%%====================================================================
%% API Functions - Compilation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compiles a DSL workflow specification into an executable AST.
%%
%% @param DSL The DSL specification (list of patterns).
%% @return {ok, AST} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec compile([dsl_pattern()]) -> compile_result().

compile(DSL) when is_list(DSL) ->
    compile(DSL, #{}).

%%--------------------------------------------------------------------
%% @doc Compiles a DSL workflow with options.
%%
%% Options:
%% - `max_depth` - Maximum nesting depth (default: 100)
%% - `strict_validation` - Enable strict validation (default: true)
%% - `scope_id` - Default scope identifier
%%
%% @param DSL The DSL specification.
%% @param Options Compilation options map.
%% @return {ok, AST} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec compile([dsl_pattern()], map()) -> compile_result().

compile(DSL, Options) when is_list(DSL), is_map(Options) ->
    MaxDepth = maps:get(max_depth, Options, 100),
    State = #compilation_state{
        max_depth = MaxDepth,
        scope_stack = maps:get(initial_scope, Options, [])
    },

    case validate(DSL) of
        ok ->
            case compile_ast(DSL, State) of
                {ok, AST, #compilation_state{errors = []}} ->
                    {ok, finalize_ast(AST, Options)};
                {ok, _AST, #compilation_state{errors = Errs}} when Errs =/= [] ->
                    {error, {validation_error, lists:reverse(Errs)}};
                {error, Reason} ->
                    {error, {parse_error, Reason}}
            end;
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Converts a pre-built AST into a workflow module.
%%
%% @param AST The abstract syntax tree from compile/1.
%% @return {ok, WorkflowModule} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec from_ast(dsl_ast()) -> {ok, term()} | {error, term()}.

from_ast(AST) when is_map(AST) ->
    case validate_ast(AST) of
        ok ->
            {ok, build_workflow(AST)};
        {error, Reason} ->
            {error, {ast_error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Validates a DSL specification without compiling.
%%
%% @param DSL The DSL specification to validate.
%% @return ok | {error, Errors}
%%
%% @end
%%--------------------------------------------------------------------
-spec validate([dsl_pattern()]) -> ok | validation_error().

validate(DSL) when is_list(DSL) ->
    Errors = lists:filtermap(
        fun(Pattern) ->
            case validate_pattern(Pattern, 0) of
                ok -> false;
                {error, _} = Error -> {true, Error}
            end
        end,
        DSL
    ),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

%%--------------------------------------------------------------------
%% @doc Converts compiled workflow to YAML representation.
%%
%% @param AST The compiled workflow AST.
%% @return {ok, YAMLBinary} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec to_yaml(dsl_ast()) -> {ok, binary()} | {error, term()}.

to_yaml(AST) when is_map(AST) ->
    try
        YAML = ast_to_yaml(AST, 0),
        {ok, iolist_to_binary(YAML)}
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Converts compiled workflow to JSON representation.
%%
%% @param AST The compiled workflow AST.
%% @return {ok, JSONBinary} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec to_json(dsl_ast()) -> {ok, binary()} | {error, term()}.

to_json(AST) when is_map(AST) ->
    try
        JSON = ast_to_json(AST),
        {ok, iolist_to_binary(JSON)}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%%====================================================================
%% API Functions - DSL Constructors
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a sequence pattern.
%%
%% @param Patterns List of patterns to execute sequentially.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec sequence([dsl_pattern()]) -> dsl_pattern().

sequence(Patterns) when is_list(Patterns) ->
    {sequence, Patterns}.

%%--------------------------------------------------------------------
%% @doc Creates a parallel pattern.
%%
%% @param Patterns List of patterns to execute concurrently.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec parallel([dsl_pattern()]) -> dsl_pattern().

parallel(Patterns) when is_list(Patterns) ->
    {parallel, Patterns}.

%%--------------------------------------------------------------------
%% @doc Creates a parallel split with synchronization.
%%
%% @param Patterns List of patterns to split into parallel branches.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec parallel_split([dsl_pattern()]) -> dsl_pattern().

parallel_split(Patterns) when is_list(Patterns) ->
    {parallel_split, Patterns}.

%%--------------------------------------------------------------------
%% @doc Creates an exclusive choice pattern.
%%
%% @param Patterns List of alternative patterns (exactly one selected).
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec choice([dsl_pattern()]) -> dsl_pattern().

choice(Patterns) when is_list(Patterns) ->
    {choice, Patterns}.

%%--------------------------------------------------------------------
%% @doc Creates a multiple choice pattern (one or more branches).
%%
%% @param Patterns List of patterns (one or more selected).
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec multi_choice([dsl_pattern()]) -> dsl_pattern().

multi_choice(Patterns) when is_list(Patterns) ->
    {multi_choice, Patterns}.

%%--------------------------------------------------------------------
%% @doc Creates a deferred choice pattern.
%%
%% @param Patterns List of patterns with deferred selection.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec deferred_choice([dsl_pattern()]) -> dsl_pattern().

deferred_choice(Patterns) when is_list(Patterns) ->
    {deferred_choice, Patterns}.

%%--------------------------------------------------------------------
%% @doc Creates a loop pattern.
%%
%% @param Body The pattern to repeat.
%% @param Config Loop configuration (max_iterations, condition, etc.).
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec loop(dsl_pattern(), map()) -> dsl_pattern().

loop(Body, Config) when is_map(Config) ->
    {loop, Body, Config}.

%%--------------------------------------------------------------------
%% @doc Creates a while loop pattern.
%%
%% @param Condition Function that returns boolean().
%% @param Body The pattern to execute while condition is true.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec while(function(), dsl_pattern()) -> dsl_pattern().

while(Condition, Body) when is_function(Condition) ->
    {while, Body, #{condition => Condition}}.

%%--------------------------------------------------------------------
%% @doc Creates an until loop pattern.
%%
%% @param Condition Function that returns boolean().
%% @param Body The pattern to execute until condition is true.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec until(function(), dsl_pattern()) -> dsl_pattern().

until(Condition, Body) when is_function(Condition) ->
    {until, Body, #{condition => Condition}}.

%%--------------------------------------------------------------------
%% @doc Creates a nested scope pattern.
%%
%% @param Pattern The pattern to nest.
%% @param Config Scope configuration (scope_id, binding_table).
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec nest(dsl_pattern(), map()) -> dsl_pattern().

nest(Pattern, Config) when is_map(Config) ->
    {nest, Pattern, Config}.

%%--------------------------------------------------------------------
%% @doc Creates a discriminator pattern (first to complete wins).
%%
%% @param Patterns List of competing patterns.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec discriminator([dsl_pattern()]) -> dsl_pattern().

discriminator(Patterns) when is_list(Patterns) ->
    {discriminator, Patterns}.

%%--------------------------------------------------------------------
%% @doc Creates an N-out-of-M pattern.
%%
%% @param N Number of branches that must complete.
%% @param Patterns List of M patterns.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec n_out_of(pos_integer(), [dsl_pattern()]) -> dsl_pattern().

n_out_of(N, Patterns) when is_integer(N), N > 0, is_list(Patterns) ->
    {n_out_of_m, N, Patterns}.

%%--------------------------------------------------------------------
%% @doc Creates a task pattern from an atom.
%%
%% @param TaskName The task identifier.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec task(atom()) -> dsl_pattern().

task(TaskName) when is_atom(TaskName) ->
    {task, TaskName}.

%%--------------------------------------------------------------------
%% @doc Creates a configured task pattern.
%%
%% @param TaskName The task identifier.
%% @param Config Task configuration map.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec task(atom(), map()) -> dsl_pattern().

task(TaskName, Config) when is_atom(TaskName), is_map(Config) ->
    {task, TaskName, Config}.

%%--------------------------------------------------------------------
%% @doc Creates a conditional pattern.
%%
%% @param Condition Function that returns boolean().
%% @param TruePattern Pattern to execute when condition is true.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec conditional(function(), dsl_pattern()) -> dsl_pattern().

conditional(Condition, TruePattern) when is_function(Condition) ->
    {conditional, Condition, TruePattern}.

%%--------------------------------------------------------------------
%% @doc Creates a retry pattern.
%%
%% @param Pattern Pattern to retry on failure.
%% @param MaxRetries Maximum number of retry attempts.
%% @param Backoff Backoff strategy (constant | exponential | {custom, Fun}).
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec retry(dsl_pattern(), non_neg_integer(), constant | exponential) -> dsl_pattern().

retry(Pattern, MaxRetries, Backoff) when is_integer(MaxRetries), MaxRetries >= 0 ->
    {retry, Pattern, #{max_retries => MaxRetries, backoff => Backoff}}.

%%====================================================================
%% API Functions - Operators (Syntactic Sugar)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Sequences two patterns (A then B).
%%
%% @param A First pattern.
%% @param B Second pattern.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec then(dsl_pattern(), dsl_pattern()) -> dsl_pattern().

then(A, B) ->
    {sequence, [normalize_pattern(A), normalize_pattern(B)]}.

%%--------------------------------------------------------------------
%% @doc Runs two patterns in parallel.
%%
%% @param A First pattern.
%% @param B Second pattern.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec par(dsl_pattern(), dsl_pattern()) -> dsl_pattern().

par(A, B) ->
    {parallel, [normalize_pattern(A), normalize_pattern(B)]}.

%%--------------------------------------------------------------------
%% @doc Creates a choice between two patterns (A or B).
%%
%% @param A First pattern.
%% @param B Second pattern.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec either(dsl_pattern(), dsl_pattern()) -> dsl_pattern().

either(A, B) ->
    {choice, [normalize_pattern(A), normalize_pattern(B)]}.

%%--------------------------------------------------------------------
%% @doc Repeats a pattern N times.
%%
%% @param N Number of iterations.
%% @param Pattern Pattern to repeat.
%% @param Config Optional configuration.
%% @return DSL pattern tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec times(pos_integer(), dsl_pattern(), map()) -> dsl_pattern().

times(N, Pattern, Config) when is_integer(N), N > 0, is_map(Config) ->
    {loop, Pattern, Config#{max_iterations => N}}.

%%====================================================================
%% API Functions - Query
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets the type of a pattern or AST node.
%%
%% @param PatternOrAST Pattern or compiled AST.
%% @return atom() representing the type.
%%
%% @end
%%--------------------------------------------------------------------
-spec type(dsl_pattern() | dsl_ast()) -> atom().

type({Type, _}) when is_atom(Type) -> Type;
type({Type, _, _}) when is_atom(Type) -> Type;
type({Type, _, _, _}) when is_atom(Type) -> Type;
type(Task) when is_atom(Task) -> task;
type(AST) when is_map(AST) -> maps:get(type, AST, unknown).

%%--------------------------------------------------------------------
%% @doc Gets the configuration of a pattern or AST node.
%%
%% @param PatternOrAST Pattern or compiled AST.
%% @return Configuration map.
%%
%% @end
%%--------------------------------------------------------------------
-spec config(dsl_pattern() | dsl_ast()) -> map().

config({_, _, Config}) when is_map(Config) -> Config;
config({_, _, _, Config}) when is_map(Config) -> Config;
config(AST) when is_map(AST) -> maps:get(config, AST, #{});
config(_) -> #{}.

%%--------------------------------------------------------------------
%% @doc Gets the children of a pattern or AST node.
%%
%% @param PatternOrAST Pattern or compiled AST.
%% @return List of child patterns.
%%
%% @end
%%--------------------------------------------------------------------
-spec children(dsl_pattern() | dsl_ast()) -> [dsl_pattern() | dsl_ast()].

children({_, Children}) when is_list(Children) -> Children;
children({_, _, Children}) when is_list(Children) -> Children;
children(AST) when is_map(AST) -> maps:get(children, AST, []);
children(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a pattern or AST is valid.
%%
%% @param PatternOrAST Pattern or compiled AST.
%% @return true | false.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid(dsl_pattern() | dsl_ast()) -> boolean().

is_valid(Pattern) when is_tuple(Pattern) ->
    case validate_pattern(Pattern, 0) of
        ok -> true;
        {error, _} -> false
    end;
is_valid(AST) when is_map(AST) ->
    case validate_ast(AST) of
        ok -> true;
        {error, _} -> false
    end;
is_valid(_) -> false.

%%--------------------------------------------------------------------
%% @doc Finds all patterns of a given type in the AST.
%%
%% @param Type The pattern type to find.
%% @param AST The compiled AST.
%% @return List of matching AST nodes.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_patterns(atom(), dsl_ast()) -> [dsl_ast()].

find_patterns(Type, AST) when is_map(AST) ->
    find_patterns(Type, AST, []).

%%--------------------------------------------------------------------
%% @doc Counts patterns by type in the AST.
%%
%% @param AST The compiled AST.
%% @return Map of pattern type to count.
%%
%% @end
%%--------------------------------------------------------------------
-spec count_patterns(dsl_ast()) -> #{atom() => non_neg_integer()}.

count_patterns(AST) when is_map(AST) ->
    count_patterns(AST, #{}).

%%====================================================================
%% Internal Functions - Compilation
%%====================================================================

%% @private
-spec compile_ast([dsl_pattern()], #compilation_state{}) ->
          {ok, dsl_ast(), #compilation_state{}} | {error, term()}.

compile_ast(Patterns, State) when is_list(Patterns) ->
    compile_patterns(Patterns, State, []).

%% @private
compile_patterns([], State, Acc) ->
    {ok, lists:reverse(Acc), State};
compile_patterns([Pattern | Rest], State, Acc) ->
    case compile_pattern(Pattern, State) of
        {ok, Compiled, NewState} ->
            compile_patterns(Rest, NewState, [Compiled | Acc]);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
compile_pattern(_Pattern, #compilation_state{depth = Depth, max_depth = Max})
        when Depth >= Max ->
    {error, {max_depth_exceeded, Depth, Max}};

compile_pattern({sequence, Patterns}, State) when is_list(Patterns) ->
    #compilation_state{depth = Depth} = State,
    case compile_ast(Patterns, State#compilation_state{depth = Depth + 1}) of
        {ok, Children, NewState} ->
            AST = #{
                type => sequence,
                children => Children,
                config => #{},
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({parallel, Patterns}, State) when is_list(Patterns) ->
    #compilation_state{depth = Depth} = State,
    case compile_ast(Patterns, State#compilation_state{depth = Depth + 1}) of
        {ok, Children, NewState} ->
            AST = #{
                type => parallel,
                children => Children,
                config => #{},
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({parallel_split, Patterns}, State) when is_list(Patterns) ->
    #compilation_state{depth = Depth} = State,
    case compile_ast(Patterns, State#compilation_state{depth = Depth + 1}) of
        {ok, Children, NewState} ->
            AST = #{
                type => parallel_split,
                children => Children,
                config => #{},
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({choice, Patterns}, State) when is_list(Patterns) ->
    #compilation_state{depth = Depth} = State,
    case compile_ast(Patterns, State#compilation_state{depth = Depth + 1}) of
        {ok, Children, NewState} ->
            AST = #{
                type => choice,
                children => Children,
                config => #{},
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({multi_choice, Patterns}, State) when is_list(Patterns) ->
    #compilation_state{depth = Depth} = State,
    case compile_ast(Patterns, State#compilation_state{depth = Depth + 1}) of
        {ok, Children, NewState} ->
            AST = #{
                type => multi_choice,
                children => Children,
                config => #{},
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({deferred_choice, Patterns}, State) when is_list(Patterns) ->
    #compilation_state{depth = Depth} = State,
    case compile_ast(Patterns, State#compilation_state{depth = Depth + 1}) of
        {ok, Children, NewState} ->
            AST = #{
                type => deferred_choice,
                children => Children,
                config => #{},
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({loop, Body, Config}, State) when is_map(Config) ->
    #compilation_state{depth = Depth} = State,
    case compile_pattern(Body, State#compilation_state{depth = Depth + 1}) of
        {ok, BodyAST, NewState} ->
            AST = #{
                type => loop,
                children => [BodyAST],
                config => Config,
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({while, Body, Config}, State) when is_map(Config) ->
    #compilation_state{depth = Depth} = State,
    case compile_pattern(Body, State#compilation_state{depth = Depth + 1}) of
        {ok, BodyAST, NewState} ->
            AST = #{
                type => while,
                children => [BodyAST],
                config => Config,
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({until, Body, Config}, State) when is_map(Config) ->
    #compilation_state{depth = Depth} = State,
    case compile_pattern(Body, State#compilation_state{depth = Depth + 1}) of
        {ok, BodyAST, NewState} ->
            AST = #{
                type => until,
                children => [BodyAST],
                config => Config,
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({nest, Pattern, Config}, State) when is_map(Config) ->
    #compilation_state{depth = Depth, scope_stack = Stack} = State,
    ScopeId = maps:get(scope_id, Config, generate_scope_id()),
    NewStack = [ScopeId | Stack],
    case compile_pattern(Pattern, State#compilation_state{
        depth = Depth + 1,
        scope_stack = NewStack
    }) of
        {ok, NestedAST, NewState} ->
            AST = #{
                type => nest,
                children => [NestedAST],
                config => Config#{scope_id => ScopeId},
                meta => #{
                    depth => Depth,
                    scope_stack => lists:reverse(NewStack)
                }
            },
            {ok, AST, NewState#compilation_state{scope_stack = Stack}};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({discriminator, Patterns}, State) when is_list(Patterns) ->
    #compilation_state{depth = Depth} = State,
    case compile_ast(Patterns, State#compilation_state{depth = Depth + 1}) of
        {ok, Children, NewState} ->
            AST = #{
                type => discriminator,
                children => Children,
                config => #{},
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({n_out_of_m, N, Patterns}, State)
        when is_integer(N), N > 0, is_list(Patterns) ->
    #compilation_state{depth = Depth} = State,
    case compile_ast(Patterns, State#compilation_state{depth = Depth + 1}) of
        {ok, Children, NewState} ->
            AST = #{
                type => n_out_of_m,
                children => Children,
                config => #{n => N, m => length(Patterns)},
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({task, TaskName}, State) when is_atom(TaskName) ->
    AST = #{
        type => task,
        children => [],
        config => #{name => TaskName},
        meta => #{}
    },
    {ok, AST, State};

compile_pattern({task, TaskName, Config}, State) when is_atom(TaskName), is_map(Config) ->
    AST = #{
        type => task,
        children => [],
        config => maps:merge(#{name => TaskName}, Config),
        meta => #{}
    },
    {ok, AST, State};

compile_pattern({conditional, Condition, TruePattern}, State) when is_function(Condition) ->
    #compilation_state{depth = Depth} = State,
    case compile_pattern(TruePattern, State#compilation_state{depth = Depth + 1}) of
        {ok, TrueAST, NewState} ->
            AST = #{
                type => conditional,
                children => [TrueAST],
                config => #{condition => Condition},
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern({retry, Pattern, Config}, State) when is_map(Config) ->
    #compilation_state{depth = Depth} = State,
    case compile_pattern(Pattern, State#compilation_state{depth = Depth + 1}) of
        {ok, PatternAST, NewState} ->
            AST = #{
                type => retry,
                children => [PatternAST],
                config => Config,
                meta => #{depth => Depth}
            },
            {ok, AST, NewState};
        {error, Reason} ->
            {error, Reason}
    end;

compile_pattern(TaskName, State) when is_atom(TaskName) ->
    AST = #{
        type => task,
        children => [],
        config => #{name => TaskName},
        meta => #{}
    },
    {ok, AST, State};

compile_pattern(Unknown, _State) ->
    {error, {unknown_pattern, Unknown}}.

%% @private
finalize_ast(AST, _Options) when is_list(AST) ->
    case AST of
        [Single] -> Single;
        _ ->
            #{type => workflow, children => AST, config => #{}, meta => #{}}
    end;
finalize_ast(AST, _Options) ->
    AST.

%%====================================================================
%% Internal Functions - Validation
%%====================================================================

%% @private
-spec validate_pattern(dsl_pattern(), non_neg_integer()) ->
          ok | {error, term()}.

validate_pattern({sequence, Patterns}, Depth) when is_list(Patterns) ->
    validate_children(Patterns, Depth + 1, sequence);
validate_pattern({parallel, Patterns}, Depth) when is_list(Patterns) ->
    validate_children(Patterns, Depth + 1, parallel);
validate_pattern({parallel_split, Patterns}, Depth) when is_list(Patterns) ->
    validate_children(Patterns, Depth + 1, parallel_split);
validate_pattern({choice, Patterns}, Depth) when is_list(Patterns) ->
    case length(Patterns) < 2 of
        true -> {error, {choice_needs_at_least_two_branches, length(Patterns)}};
        false -> validate_children(Patterns, Depth + 1, choice)
    end;
validate_pattern({multi_choice, Patterns}, Depth) when is_list(Patterns) ->
    case length(Patterns) < 2 of
        true -> {error, {multi_choice_needs_at_least_two_branches, length(Patterns)}};
        false -> validate_children(Patterns, Depth + 1, multi_choice)
    end;
validate_pattern({deferred_choice, Patterns}, Depth) when is_list(Patterns) ->
    case length(Patterns) < 2 of
        true -> {error, {deferred_choice_needs_at_least_two_branches, length(Patterns)}};
        false -> validate_children(Patterns, Depth + 1, deferred_choice)
    end;
validate_pattern({loop, _Body, Config}, _Depth) when is_map(Config) ->
    MaxIter = maps:get(max_iterations, Config, 1000),
    case MaxIter > 0 of
        true -> ok;
        false -> {error, {max_iterations_must_be_positive, MaxIter}}
    end;
validate_pattern({while, _Body, Config}, _Depth) when is_map(Config) ->
    Condition = maps:get(condition, Config),
    case is_function(Condition) of
        true -> ok;
        false -> {error, {while_condition_must_be_function, Condition}}
    end;
validate_pattern({until, _Body, Config}, _Depth) when is_map(Config) ->
    Condition = maps:get(condition, Config),
    case is_function(Condition) of
        true -> ok;
        false -> {error, {until_condition_must_be_function, Condition}}
    end;
validate_pattern({nest, Pattern, Config}, Depth) when is_map(Config) ->
    case maps:is_key(scope_id, Config) of
        true -> validate_pattern(Pattern, Depth + 1);
        false -> {error, nest_needs_scope_id}
    end;
validate_pattern({discriminator, Patterns}, Depth) when is_list(Patterns) ->
    case length(Patterns) < 2 of
        true -> {error, {discriminator_needs_at_least_two_branches, length(Patterns)}};
        false -> validate_children(Patterns, Depth + 1, discriminator)
    end;
validate_pattern({n_out_of_m, N, Patterns}, Depth)
        when is_integer(N), N > 0, is_list(Patterns) ->
    M = length(Patterns),
    case N =< M of
        true -> validate_children(Patterns, Depth + 1, {n_out_of_m, N, M});
        false -> {error, {n_cannot_exceed_m, N, M}}
    end;
validate_pattern({task, _Name}, _Depth) ->
    ok;
validate_pattern({task, _Name, _Config}, _Depth) ->
    ok;
validate_pattern({conditional, Condition, _TruePattern}, _Depth) when is_function(Condition) ->
    ok;
validate_pattern({retry, _Pattern, Config}, _Depth) when is_map(Config) ->
    MaxRetries = maps:get(max_retries, Config, 3),
    case MaxRetries >= 0 of
        true -> ok;
        false -> {error, {max_retries_cannot_be_negative, MaxRetries}}
    end;
validate_pattern(Task, _Depth) when is_atom(Task) ->
    ok;
validate_pattern(Unknown, _Depth) ->
    {error, {unknown_pattern_type, Unknown}}.

%% @private
validate_children([], _Depth, _ParentType) ->
    ok;
validate_children([Pattern | Rest], Depth, ParentType) ->
    case validate_pattern(Pattern, Depth) of
        ok -> validate_children(Rest, Depth, ParentType);
        Error -> Error
    end.

%% @private
-spec validate_ast(dsl_ast()) -> ok | {error, term()}.

validate_ast(#{type := Type, children := Children}) when is_list(Children) ->
    case lists:all(fun(C) -> is_map(C) end, Children) of
        true -> ok;
        false -> {error, {invalid_children, Type}}
    end;
validate_ast(#{type := _Type}) ->
    ok;
validate_ast(Unknown) ->
    {error, {invalid_ast, Unknown}}.

%%====================================================================
%% Internal Functions - Query Helpers
%%====================================================================

%% @private
find_patterns(Type, #{type := Type} = AST, Acc) ->
    NewAcc = [AST | Acc],
    lists:foldl(
        fun(Child, AccIn) ->
            find_patterns(Type, Child, AccIn)
        end,
        NewAcc,
        maps:get(children, AST, [])
    );
find_patterns(Type, #{children := Children}, Acc) when is_list(Children) ->
    lists:foldl(
        fun(Child, AccIn) ->
            find_patterns(Type, Child, AccIn)
        end,
        Acc,
        Children
    );
find_patterns(_Type, _AST, Acc) ->
    Acc.

%% @private
count_patterns(#{type := Type, children := Children}, Acc) ->
    NewAcc = maps:update_with(Type, fun(V) -> V + 1 end, 1, Acc),
    lists:foldl(
        fun(Child, AccIn) ->
            count_patterns(Child, AccIn)
        end,
        NewAcc,
        Children
    );
count_patterns(#{children := Children}, Acc) when is_list(Children) ->
    lists:foldl(
        fun(Child, AccIn) ->
            count_patterns(Child, AccIn)
        end,
        Acc,
        Children
    );
count_patterns(_, Acc) ->
    Acc.

%%====================================================================
%% Internal Functions - Utility
%%====================================================================

%% @private
normalize_pattern({Type, Data}) when is_atom(Type) ->
    {Type, Data};
normalize_pattern({Type, Data1, Data2}) when is_atom(Type) ->
    {Type, Data1, Data2};
normalize_pattern(Atom) when is_atom(Atom) ->
    {task, Atom};
normalize_pattern(Pattern) ->
    Pattern.

%% @private
generate_scope_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"scope_", Hex/binary>>.

%% @private
build_workflow(AST) when is_map(AST) ->
    case maps:get(type, AST) of
        task ->
            TaskName = maps:get(name, maps:get(config, AST)),
            {task, TaskName};
        sequence ->
            Children = [build_workflow(C) || C <- maps:get(children, AST)],
            {sequence, Children};
        parallel ->
            Children = [build_workflow(C) || C <- maps:get(children, AST)],
            {parallel, Children};
        Type ->
            Children = [build_workflow(C) || C <- maps:get(children, AST)],
            Config = maps:get(config, AST, #{}),
            {Type, Children, Config}
    end.

%% @private
ast_to_yaml(AST, Indent) when is_map(AST) ->
    Type = maps:get(type, AST, unknown),
    Config = maps:get(config, AST, #{}),
    Children = maps:get(children, AST, []),
    IndentStr = lists:duplicate(Indent, $\s),

    Lines = [
        io_lib:format("~s- type: ~p~n", [IndentStr, Type])
    ] ++
    case Children of
        [] -> [];
        _ -> [io_lib:format("~s  children:~n", [IndentStr])] ++
             [ast_to_yaml(C, Indent + 4) || C <- Children]
    end ++
    case maps:size(Config) of
        0 -> [];
        _ -> [io_lib:format("~s  config:~n", [IndentStr])] ++
             yaml_config(Config, Indent + 4)
    end,

    Lines.

%% @private
yaml_config(Config, Indent) ->
    IndentStr = lists:duplicate(Indent, $\s),
    maps:fold(fun
        (_K, V, Acc) when is_function(V) ->
            Acc;
        (K, V, Acc) ->
            [io_lib:format("~s  ~p: ~p~n", [IndentStr, K, V]) | Acc]
    end, [], Config).

%% @private
ast_to_json(AST) when is_map(AST) ->
    Type = maps:get(type, AST, unknown),
    Config = maps:get(config, AST, #{}),
    Children = maps:get(children, AST, []),

    TypePart = [<<"\"type\": \"">>, atom_to_binary(Type), <<"\"">>],
    ChildrenPart = case Children of
        [] -> [];
        _ -> [
            <<", \"children\": [">>,
            join_json([ast_to_json(C) || C <- Children]),
            <<"]">>
        ]
    end,
    ConfigPart = case maps:size(Config) of
        0 -> [];
        _ -> [
            <<", \"config\": {">>,
            json_config(Config),
            <<"}">>
        ]
    end,

    [<<"{">>, TypePart, ChildrenPart, ConfigPart, <<"}">>].

%% @private
join_json([]) ->
    [];
join_json([Single]) ->
    Single;
join_json(List) when is_list(List) ->
    lists:foldr(fun(X, Acc) ->
        case Acc of
            [] -> [X];
            _ -> [X, <<", ">> | Acc]
        end
    end, [], List).

%% @private
json_config(Config) when is_map(Config) ->
    maps:fold(fun
        (_K, _V, Acc) when is_function(_V) ->
            Acc;
        (K, V, Acc) ->
            KeyValue = [json_key(K), <<": ">>, json_value(V)],
            case Acc of
                [] -> KeyValue;
                _ -> [KeyValue, <<", ">> | Acc]
            end
    end, [], Config).

%% @private
json_key(K) when is_atom(K) ->
    [<<"\"">>, atom_to_binary(K), <<"\"">>];
json_key(K) when is_binary(K) ->
    [<<"\"">>, K, <<"\"">>];
json_key(K) ->
    [<<"\"">>, to_binary(K), <<"\"">>].

%% @private
to_binary(V) when is_atom(V) -> atom_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_binary(V) -> V.

%% @private
json_value(V) when is_atom(V) ->
    [<<"\"">>, atom_to_binary(V), <<"\"">>];
json_value(V) when is_binary(V) ->
    [<<"\"">>, V, <<"\"">>];
json_value(V) when is_integer(V) ->
    integer_to_binary(V);
json_value(V) when is_float(V) ->
    float_to_binary(V, [{scientific, 60}]);
json_value(V) when is_list(V) ->
    [<<"[">>, join_json([json_value(E) || E <- V]), <<"]">>];
json_value(V) when is_map(V) ->
    [<<"{">>, json_config(V), <<"}">>];
json_value(true) ->
    <<"true">>;
json_value(false) ->
    <<"false">>;
json_value(null) ->
    <<"null">>;
json_value(undefined) ->
    <<"null">>.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test basic sequence compilation
compile_sequence_test() ->
    DSL = [{sequence, [task_a, task_b, task_c]}],
    {ok, AST} = compile(DSL),
    ?assertEqual(sequence, maps:get(type, AST)),
    ?assertEqual(3, length(maps:get(children, AST))).

%% Test parallel compilation
compile_parallel_test() ->
    DSL = [{parallel, [task_a, task_b, task_c]}],
    {ok, AST} = compile(DSL),
    ?assertEqual(parallel, maps:get(type, AST)),
    ?assertEqual(3, length(maps:get(children, AST))).

%% Test choice compilation
compile_choice_test() ->
    DSL = [{choice, [task_a, task_b]}],
    {ok, AST} = compile(DSL),
    ?assertEqual(choice, maps:get(type, AST)),
    ?assertEqual(2, length(maps:get(children, AST))).

%% Test loop compilation
compile_loop_test() ->
    DSL = [{loop, task_body, #{max_iterations => 5}}],
    {ok, AST} = compile(DSL),
    ?assertEqual(loop, maps:get(type, AST)),
    ?assertEqual(5, maps:get(max_iterations, maps:get(config, AST))).

%% Test nested compilation
compile_nested_test() ->
    DSL = [{sequence, [
        {parallel, [task_a, task_b]},
        {choice, [task_c, task_d]}
    ]}],
    {ok, AST} = compile(DSL),
    ?assertEqual(sequence, maps:get(type, AST)),
    [First, Second] = maps:get(children, AST),
    ?assertEqual(parallel, maps:get(type, First)),
    ?assertEqual(choice, maps:get(type, Second)).

%% Test validation - valid
validate_valid_test() ->
    DSL = [{sequence, [task_a, task_b]}],
    ?assertEqual(ok, validate(DSL)).

%% Test validation - choice needs 2+
validate_choice_needs_two_test() ->
    DSL = [{choice, [task_a]}],
    ?assertMatch({error, [{error, {choice_needs_at_least_two_branches, 1}}]}, validate(DSL)).

%% Test validation - n out of m
validate_n_out_of_m_test() ->
    DSL = [{n_out_of_m, 2, [task_a, task_b, task_c]}],
    ?assertEqual(ok, validate(DSL)).

%% Test validation - n exceeds m
validate_n_exceeds_m_test() ->
    DSL = [{n_out_of_m, 5, [task_a, task_b]}],
    ?assertMatch({error, [{error, {n_cannot_exceed_m, 5, 2}}]}, validate(DSL)).

%% Test type query
type_query_test() ->
    ?assertEqual(sequence, type({sequence, []})),
    ?assertEqual(parallel, type({parallel, []})),
    ?assertEqual(task, type(some_task)).

%% Test config query
config_query_test() ->
    Config = #{max_iterations => 5},
    ?assertEqual(Config, config({loop, body, Config})).

%% Test children query
children_query_test() ->
    Children = [task_a, task_b],
    ?assertEqual(Children, children({sequence, Children})).

%% Test is_valid
is_valid_test() ->
    ?assert(is_valid({sequence, [task_a, task_b]})),
    ?assert(is_valid({choice, [task_a, task_b]})),
    ?assertNot(is_valid({choice, [task_a]})).

%% Test find_patterns
find_patterns_test() ->
    DSL = [{sequence, [
        {parallel, [task_a, task_b]},
        {choice, [task_c, task_d]}
    ]}],
    {ok, AST} = compile(DSL),
    Tasks = find_patterns(task, AST),
    ?assertEqual(4, length(Tasks)).

%% Test count_patterns
count_patterns_test() ->
    DSL = [{sequence, [
        {parallel, [task_a, task_b]},
        {choice, [task_c, task_d]}
    ]}],
    {ok, AST} = compile(DSL),
    Counts = count_patterns(AST),
    ?assertEqual(1, maps:get(sequence, Counts, 0)),
    ?assertEqual(1, maps:get(parallel, Counts, 0)),
    ?assertEqual(1, maps:get(choice, Counts, 0)),
    ?assertEqual(4, maps:get(task, Counts, 0)).

%% Test DSL constructors
sequence_constructor_test() ->
    Pattern = sequence([a, b, c]),
    ?assertEqual({sequence, [a, b, c]}, Pattern).

parallel_constructor_test() ->
    Pattern = parallel([a, b]),
    ?assertEqual({parallel, [a, b]}, Pattern).

choice_constructor_test() ->
    Pattern = choice([a, b]),
    ?assertEqual({choice, [a, b]}, Pattern).

loop_constructor_test() ->
    Pattern = loop(body, #{max_iterations => 5}),
    ?assertMatch({loop, body, #{max_iterations := 5}}, Pattern).

%% Test operators
then_operator_test() ->
    Pattern = then(a, b),
    ?assertMatch({sequence, [{task, a}, {task, b}]}, Pattern).

par_operator_test() ->
    Pattern = par(a, b),
    ?assertMatch({parallel, [{task, a}, {task, b}]}, Pattern).

either_operator_test() ->
    Pattern = either(a, b),
    ?assertMatch({choice, [{task, a}, {task, b}]}, Pattern).

times_operator_test() ->
    Pattern = times(5, body, #{}),
    ?assertMatch({loop, body, #{max_iterations := 5}}, Pattern).

%% Test max depth
max_depth_test() ->
    %% Create deeply nested pattern
    Deep = lists:foldl(fun(_, Acc) ->
        {sequence, [Acc]}
    end, task_a, lists:seq(1, 50)),
    {ok, _} = compile([Deep], #{max_depth => 100}).

%% Test to_yaml
to_yaml_test() ->
    DSL = [{sequence, [task_a, task_b]}],
    {ok, AST} = compile(DSL),
    {ok, YAML} = to_yaml(AST),
    ?assert(is_binary(YAML)),
    ?assertNotEqual(<<>>, YAML).

%% Test complex nested DSL
complex_dsl_test() ->
    DSL = [{sequence, [
        {parallel_split, [validate_data, check_permissions, verify_signature]},
        {choice, [
            {sequence, [process_a, commit_a]},
            {sequence, [process_b, commit_b]},
            {sequence, [process_c, commit_c]}
        ]},
        {loop, retry_step, #{max_iterations => 3}}
    ]}],
    {ok, AST} = compile(DSL),
    ?assertEqual(sequence, maps:get(type, AST)),
    [First, Second, Third] = maps:get(children, AST),
    ?assertEqual(parallel_split, maps:get(type, First)),
    ?assertEqual(choice, maps:get(type, Second)),
    ?assertEqual(loop, maps:get(type, Third)),
    ?assertEqual(3, length(maps:get(children, First))),
    ?assertEqual(3, length(maps:get(children, Second))).

-endif.
