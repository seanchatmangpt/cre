%%% @doc ln_ctrl_budget: Budget tracking and enforcement
%%%
%%% Tracks effects executed, latency, and cost spent. Raises andon red
%%% (triggers case halt) if budget exceeded.
%%%
%%% @end
-module(ln_ctrl_budget).

-export([
    new_budget/3,
    check_effect/2,
    check_latency/2,
    status/1
]).

-export_type([
    budget/0,
    budget_spec/0,
    budget_status/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

-type budget_spec() :: non_neg_integer() | unlimited.

-record(budget, {
    max_effects :: budget_spec(),
    max_latency_ms :: budget_spec(),
    max_cost_usd :: budget_spec(),
    effects_used :: non_neg_integer(),
    latency_used_ms :: non_neg_integer(),
    cost_used_usd :: float(),
    exceeded :: boolean()
}).

-type budget() :: #budget{}.

-type budget_status() :: #{
    max_effects => budget_spec(),
    max_latency_ms => budget_spec(),
    max_cost_usd => budget_spec(),
    effects_used => non_neg_integer(),
    latency_used_ms => non_neg_integer(),
    cost_used_usd => float(),
    exceeded => boolean()
}.

%%% API =====================================================================

%% @doc Create a new budget with limits.
%%
%% Each limit can be a non-negative integer or 'unlimited'.
%%
%% @end
-spec new_budget(
    MaxEffects :: budget_spec(),
    MaxLatencyMs :: budget_spec(),
    MaxCostUSD :: budget_spec()
) -> budget().
new_budget(MaxEffects, MaxLatencyMs, MaxCostUSD) ->
    #budget{
        max_effects = MaxEffects,
        max_latency_ms = MaxLatencyMs,
        max_cost_usd = MaxCostUSD,
        effects_used = 0,
        latency_used_ms = 0,
        cost_used_usd = 0.0,
        exceeded = false
    }.

%% @doc Check if an effect can be issued within budget.
%%
%% Returns {ok, UpdatedBudget} if effect can be issued, or
%% {budget_exceeded, Reason, UpdatedBudget} if not.
%%
%% Cost is optional (default 0).
%%
%% @end
-spec check_effect(Budget :: budget(), Cost :: float()) ->
    {ok, budget()} | {budget_exceeded, term(), budget()}.
check_effect(Budget, Cost) when is_number(Cost), Cost >= 0 ->
    EffectsUsed = Budget#budget.effects_used + 1,
    CostUsed = Budget#budget.cost_used_usd + Cost,

    NewBudget = Budget#budget{
        effects_used = EffectsUsed,
        cost_used_usd = CostUsed
    },

    %% Check effect count
    case Budget#budget.max_effects of
        unlimited ->
            ok;
        MaxEffects when EffectsUsed > MaxEffects ->
            return_exceeded(NewBudget, {max_effects_exceeded, EffectsUsed, MaxEffects})
    end,

    %% Check cost
    case Budget#budget.max_cost_usd of
        unlimited ->
            {ok, NewBudget};
        MaxCost when CostUsed > MaxCost ->
            return_exceeded(NewBudget, {max_cost_exceeded, CostUsed, MaxCost});
        _ ->
            {ok, NewBudget}
    end.

%% @doc Check if latency is within budget.
%%
%% Call this periodically with elapsed milliseconds.
%% Returns ok or {timeout, Reason, UpdatedBudget}.
%%
%% @end
-spec check_latency(Budget :: budget(), ElapsedMs :: non_neg_integer()) ->
    ok | {timeout, term(), budget()}.
check_latency(Budget, ElapsedMs) when is_integer(ElapsedMs), ElapsedMs >= 0 ->
    NewBudget = Budget#budget{latency_used_ms = ElapsedMs},

    case Budget#budget.max_latency_ms of
        unlimited ->
            ok;
        MaxLatency when ElapsedMs > MaxLatency ->
            return_exceeded(NewBudget, {max_latency_exceeded, ElapsedMs, MaxLatency});
        _ ->
            ok
    end.

%% @doc Get budget status.
%%
%% Returns a map with current usage and limits.
%%
%% @end
-spec status(Budget :: budget()) -> budget_status().
status(Budget) ->
    #{
        max_effects => Budget#budget.max_effects,
        max_latency_ms => Budget#budget.max_latency_ms,
        max_cost_usd => Budget#budget.max_cost_usd,
        effects_used => Budget#budget.effects_used,
        latency_used_ms => Budget#budget.latency_used_ms,
        cost_used_usd => Budget#budget.cost_used_usd,
        exceeded => Budget#budget.exceeded
    }.

%%% INTERNAL FUNCTIONS ======================================================

-spec return_exceeded(budget(), term()) ->
    {budget_exceeded, term(), budget()} | {timeout, term(), budget()}.
return_exceeded(Budget, Reason) ->
    NewBudget = Budget#budget{exceeded = true},
    case element(1, Reason) of
        max_latency_exceeded ->
            {timeout, Reason, NewBudget};
        _ ->
            {budget_exceeded, Reason, NewBudget}
    end.

%%% TESTS ===================================================================

new_budget_test_() ->
    [
        ?_assertMatch(#budget{max_effects = 100}, new_budget(100, unlimited, unlimited)),
        ?_assertMatch(#budget{exceeded = false}, new_budget(10, 5000, 10.0))
    ].

check_effect_test_() ->
    Budget = new_budget(5, unlimited, 100.0),
    {ok, B1} = check_effect(Budget, 0.0),
    {ok, B2} = check_effect(B1, 0.0),
    [
        ?_assertEqual(2, B2#budget.effects_used),
        ?_assertMatch({ok, _}, check_effect(B2, 0.0))
    ].

budget_exceeded_test_() ->
    Budget = new_budget(2, unlimited, unlimited),
    {ok, B1} = check_effect(Budget, 0.0),
    {ok, B2} = check_effect(B1, 0.0),
    [
        ?_assertMatch({budget_exceeded, _, _}, check_effect(B2, 0.0))
    ].

latency_check_test_() ->
    Budget = new_budget(unlimited, 1000, unlimited),
    [
        ?_assertEqual(ok, check_latency(Budget, 500)),
        ?_assertMatch({timeout, _, _}, check_latency(Budget, 1500))
    ].
