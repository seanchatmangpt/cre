%% -*- erlang -*-
%%%% @doc Linear Nesting Budget Manager
%%
%% This module provides budget tracking for workflow execution, enforcing
%% limits on steps, effects, and wall-clock time. Used for resource control
%% and early termination of runaway computations.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Step counting with configurable maximum</li>
%%   <li>Effect counting with configurable maximum</li>
%%   <li>Wall-clock time tracking with millisecond precision</li>
%%   <li>Pure functional state management</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Initializing a budget:
%% ```erlang
%% > Budget0 = ln_budget:init(#{max_steps => 1000, max_effects => 100, max_wall_ms => 5000}).
%% {budget, #{max_steps => 1000, ...}, 0, 0, Start, 0, false}
%% ```
%%
%% Checking and recording steps:
%% ```erlang
%% > ok = ln_budget:check_step(Budget0),
%% > Budget1 = ln_budget:record_step(Budget0).
%% ```
%%
%% Checking if exceeded:
%% ```erlang
%% > false = ln_budget:is_exceeded(Budget1),
%% > Status = ln_budget:status(Budget1).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_budget).

%%====================================================================
%% Exports
%%====================================================================

%% Budget lifecycle
-export([init/1]).

%% Step tracking
-export([check_step/1, record_step/1]).

%% Effect tracking
-export([check_effect/1, record_effect/1]).

%% Time tracking
-export([check_time/1]).

%% Status queries
-export([is_exceeded/1, status/1, error/1]).

%%====================================================================
%% Records
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Budget configuration.
%%
%% Keys:
%% - <b>max_steps:</b> Maximum number of steps allowed (optional)
%% - <b>max_effects:</b> Maximum number of effects allowed (optional)
%% - <b>max_wall_ms:</b> Maximum wall-clock time in milliseconds (optional)
%%--------------------------------------------------------------------
-record(config, {
    max_steps :: undefined | non_neg_integer(),
    max_effects :: undefined | non_neg_integer(),
    max_wall_ms :: undefined | pos_integer()
}).

%%--------------------------------------------------------------------
%% @doc Budget state.
%%
%% Tracks current resource usage against configured limits.
%%--------------------------------------------------------------------
-record(budget, {
    config :: #config{},
    steps = 0 :: non_neg_integer(),
    effects = 0 :: non_neg_integer(),
    start_time :: integer(),
    elapsed_ms = 0 :: non_neg_integer(),
    exceeded = false :: boolean()
}).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Budget configuration map.
%%
%% Optional keys:
%% - <b>max_steps:</b> Maximum steps before exceeding (0 = no limit)
%% - <b>max_effects:</b> Maximum effects before exceeding (0 = no limit)
%% - <b>max_wall_ms:</b> Maximum wall time milliseconds before exceeding (0 = no limit)
%%--------------------------------------------------------------------
-type budget_config() :: #{
    max_steps => non_neg_integer(),
    max_effects => non_neg_integer(),
    max_wall_ms => non_neg_integer()
}.

%%--------------------------------------------------------------------
%% @doc Budget state handle.
%%
%% Opaque record tracking resource usage. Use init/1 to create,
%% and record_step/1, record_effect/1 to update.
%%--------------------------------------------------------------------
-opaque budget() :: #budget{}.

%%--------------------------------------------------------------------
%% @doc Budget status result.
%%
%% Returns current usage and limits when queried.
%%--------------------------------------------------------------------
-type budget_status() :: #{
    steps := non_neg_integer(),
    max_steps := undefined | non_neg_integer(),
    effects := non_neg_integer(),
    max_effects := undefined | non_neg_integer(),
    elapsed_ms := non_neg_integer(),
    max_wall_ms := undefined | non_neg_integer(),
    exceeded := boolean()
}.

%%--------------------------------------------------------------------
%% @doc Budget check result.
%%
%% ok: Resource is within limits
%% {error, Reason}: Budget exceeded for the given reason
%%--------------------------------------------------------------------
-type budget_result() :: ok | {error, term()}.

%% Export types
-export_type([budget/0, budget_config/0, budget_status/0, budget_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes a new budget state.
%%
%% Creates a budget tracker from a configuration map. All limits are optional;
%% omit a key to disable that limit. Use 0 for a limit to mean "no limit".
%%
%% <h4>Configuration Options</h4>
%% <ul>
%%   <li><b>max_steps:</b> Maximum execution steps (0 or omitted = unlimited)</li>
%%   <li><b>max_effects:</b> Maximum side-effect operations (0 or omitted = unlimited)</li>
%%   <li><b>max_wall_ms:</b> Maximum wall-clock milliseconds (0 or omitted = unlimited)</li>
%% </ul>
%%
%% @param Config Configuration map with optional limits
%% @returns New budget state
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Config :: budget_config()) -> budget().

init(Config) when is_map(Config) ->
    StartTime = erlang:monotonic_time(millisecond),

    BudgetConfig = #config{
        max_steps = parse_limit(maps:get(max_steps, Config, undefined)),
        max_effects = parse_limit(maps:get(max_effects, Config, undefined)),
        max_wall_ms = parse_limit(maps:get(max_wall_ms, Config, undefined))
    },

    #budget{
        config = BudgetConfig,
        start_time = StartTime
    }.

%%--------------------------------------------------------------------
%% @doc Checks if a step can be taken without exceeding budget.
%%
%% Returns ok if the current step count is below the limit.
%% Returns {error, steps_exceeded} if at or over the limit.
%%
%% @param Budget Budget state to check
%% @returns ok | {error, steps_exceeded}
%%
%% @end
%%--------------------------------------------------------------------
-spec check_step(Budget :: budget()) -> budget_result().

check_step(#budget{exceeded = true}) ->
    {error, budget_exceeded};
check_step(#budget{config = #config{max_steps = undefined}}) ->
    ok;
check_step(#budget{config = #config{max_steps = 0}}) ->
    ok;
check_step(#budget{steps = Steps, config = #config{max_steps = Max}})
        when Steps >= Max ->
    {error, steps_exceeded};
check_step(#budget{}) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Records a step taken in the budget.
%%
%% Increments the step counter and returns the updated budget state.
%% Does NOT check if the limit was exceeded - use check_step/1 first.
%%
%% @param Budget Budget state to update
%% @returns Updated budget state with incremented step count
%%
%% @end
%%--------------------------------------------------------------------
-spec record_step(Budget :: budget()) -> budget().

record_step(#budget{steps = Steps, exceeded = false} = Budget) ->
    Budget#budget{steps = Steps + 1};
record_step(#budget{exceeded = true} = Budget) ->
    Budget.

%%--------------------------------------------------------------------
%% @doc Checks if an effect can be performed without exceeding budget.
%%
%% Returns ok if the current effect count is below the limit.
%% Returns {error, effects_exceeded} if at or over the limit.
%%
%% @param Budget Budget state to check
%% @returns ok | {error, effects_exceeded}
%%
%% @end
%%--------------------------------------------------------------------
-spec check_effect(Budget :: budget()) -> budget_result().

check_effect(#budget{exceeded = true}) ->
    {error, budget_exceeded};
check_effect(#budget{config = #config{max_effects = undefined}}) ->
    ok;
check_effect(#budget{config = #config{max_effects = 0}}) ->
    ok;
check_effect(#budget{effects = Effects, config = #config{max_effects = Max}})
        when Effects >= Max ->
    {error, effects_exceeded};
check_effect(#budget{}) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Records an effect performed in the budget.
%%
%% Increments the effect counter and returns the updated budget state.
%% Does NOT check if the limit was exceeded - use check_effect/1 first.
%%
%% @param Budget Budget state to update
%% @returns Updated budget state with incremented effect count
%%
%% @end
%%--------------------------------------------------------------------
-spec record_effect(Budget :: budget()) -> budget().

record_effect(#budget{effects = Effects, exceeded = false} = Budget) ->
    Budget#budget{effects = Effects + 1};
record_effect(#budget{exceeded = true} = Budget) ->
    Budget.

%%--------------------------------------------------------------------
%% @doc Checks if wall-clock time has exceeded the budget.
%%
%% Recalculates elapsed time and returns ok if under the limit.
%% Returns {error, time_exceeded} if at or over the time limit.
%%
%% @param Budget Budget state to check
%% @returns ok | {error, time_exceeded}
%%
%% @end
%%--------------------------------------------------------------------
-spec check_time(Budget :: budget()) -> budget_result().

check_time(#budget{exceeded = true}) ->
    {error, budget_exceeded};
check_time(#budget{config = #config{max_wall_ms = undefined}}) ->
    ok;
check_time(#budget{config = #config{max_wall_ms = 0}}) ->
    ok;
check_time(#budget{start_time = StartTime, config = #config{max_wall_ms = Max}}) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    case Elapsed >= Max of
        true -> {error, time_exceeded};
        false -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Checks if the budget has been exceeded.
%%
%% Returns true if any limit has been exceeded. This is a faster
%% check than running individual check functions.
%%
%% @param Budget Budget state to check
%% @returns true if exceeded, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_exceeded(Budget :: budget()) -> boolean().

is_exceeded(#budget{exceeded = Exceeded}) ->
    Exceeded.

%%--------------------------------------------------------------------
%% @doc Returns the current budget status as a map.
%%
%% Provides a snapshot of current resource usage against all configured
%% limits. Useful for logging and monitoring.
%%
%% @param Budget Budget state to query
%% @returns Status map with current usage and limits
%%
%% @end
%%--------------------------------------------------------------------
-spec status(Budget :: budget()) -> budget_status().

status(#budget{
    config = #config{
        max_steps = MaxSteps,
        max_effects = MaxEffects,
        max_wall_ms = MaxWallMs
    },
    steps = Steps,
    effects = Effects,
    start_time = StartTime,
    elapsed_ms = Elapsed,
    exceeded = Exceeded
}) ->
    #{
        steps => Steps,
        max_steps => MaxSteps,
        effects => Effects,
        max_effects => MaxEffects,
        elapsed_ms => Elapsed,
        max_wall_ms => MaxWallMs,
        exceeded => Exceeded
    }.

%%--------------------------------------------------------------------
%% @doc Returns an error tuple for an exceeded budget.
%%
%% Creates a standardized error tuple with budget status included.
%% Use this when budget limits are hit during execution.
%%
%% @param Budget Budget state that was exceeded
%% @returns {error, {budget_exceeded, Status}}
%%
%% @end
%%--------------------------------------------------------------------
-spec error(Budget :: budget()) -> {error, {budget_exceeded, budget_status()}}.

error(Budget) ->
    Status = status(Budget),
    {error, {budget_exceeded, Status}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Parses a limit value from config.
%%
%% undefined remains undefined (no limit).
%% 0 is converted to undefined (unlimited).
%% Positive integer is kept as-is.
-spec parse_limit(undefined | non_neg_integer()) -> undefined | non_neg_integer().

parse_limit(undefined) ->
    undefined;
parse_limit(0) ->
    undefined;
parse_limit(N) when is_integer(N), N > 0 ->
    N;
parse_limit(_) ->
    undefined.
