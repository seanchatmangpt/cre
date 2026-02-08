# Strategy Plugin System: Design Specification

**Version:** 1.0
**Date:** 2026-02-07
**Status:** Design (Interface Specification Only)
**Scope:** Extensible pattern behaviors for CRE YAWL workflow engine

---

## Executive Summary

The Strategy Plugin System provides a modular, composable architecture for extending pattern behaviors in CRE. Inspired by the vision for adaptive patterns (Adaptive Discriminator, N-of-M strategies), this system enables:

1. **Pluggable strategies** for N-of-M patterns and beyond
2. **Custom strategies** via callback modules
3. **Strategy registration** and discovery
4. **Strategy composition** for complex behaviors
5. **Runtime strategy switching** for adaptive optimization

---

## Table of Contents

1. [Behavior Specification](#1-behavior-specification)
2. [Registration Protocol](#2-registration-protocol)
3. [Strategy Lifecycle Management](#3-strategy-lifecycle-management)
4. [Built-in Strategy Catalog](#4-built-in-strategy-catalog)
5. [Example Custom Strategies](#5-example-custom-strategies)
6. [Type Definitions](#6-type-definitions)
7. [Integration Points](#7-integration-points)

---

## 1. Behavior Specification

### 1.1 Core Behavior: `yawl_strategy`

The `yawl_strategy` behavior defines the contract for all strategy modules.

```erlang
%%%-------------------------------------------------------------------
%%% @doc Strategy Behavior for Extensible Pattern Behaviors
%%%
%%% This behavior defines the contract for strategy modules that
%%% can be plugged into patterns (N-of-M, Discriminator, etc.) to
%%% customize their execution semantics.
%%% @end
%%%-------------------------------------------------------------------
-module(yawl_strategy).
-callback """
Strategy modules implement this behavior to provide custom
execution semantics for workflow patterns.

Required callbacks:
- init/1: Initialize strategy state
- select/2: Select from available options
- update/3: Update strategy state based on feedback
- terminate/2: Cleanup strategy state

Optional callbacks:
- validate/1: Validate strategy configuration
- compose/2: Compose with another strategy
- describe/0: Human-readable description
""".

%%--------------------------------------------------------------------
%% Required Callbacks
%%--------------------------------------------------------------------

-callback init(Config :: map()) ->
    {ok, State :: term()} | {error, Reason :: term()}.

-callback select(Options :: [term()], State :: term()) ->
    {selected, Selected :: [term()], NewState :: term()} |
    {defer, Reason :: term(), NewState :: term()} |
    {error, Reason :: term()}.

-callback update(State :: term(), Event :: term(), Metadata :: map()) ->
    {ok, NewState :: term()} | {error, Reason :: term()}.

-callback terminate(Reason :: term(), State :: term()) ->
    ok.

%%--------------------------------------------------------------------
%% Optional Callbacks
%%--------------------------------------------------------------------

-callback validate(Config :: map()) ->
    ok | {error, Reason :: term()}.

-callback compose(InnerStrategy :: module(), State :: term()) ->
    {ok, ComposedState :: term()} | {error, Reason :: term()}.

-callback describe() ->
    Description :: #{
        name => binary(),
        category => binary(),
        description => binary(),
        parameters => [#{name => atom(), type => atom(), required => boolean()}]
    }.
```

### 1.2 Specialized Behavior: `n_of_m_strategy`

For N-of-M patterns specifically, a specialized behavior provides additional structure.

```erlang
%%%-------------------------------------------------------------------
%%% @doc N-of-M Strategy Behavior
%%%
%%% Specialized strategy behavior for N-of-M patterns with quorum
%%% semantics.
%%% @end
%%%-------------------------------------------------------------------
-module(n_of_m_strategy).
-behaviour(yawl_strategy).

%% Required callbacks (inherited from yawl_strategy)
-export([init/1, select/2, update/3, terminate/2]).

%% N-of-M specific callbacks
-callback should_complete(CompletedCount :: non_neg_integer(),
                         Required :: pos_integer(),
                         Total :: pos_integer(),
                         State :: term()) ->
    boolean().

-callback completion_cutoff(CompletedCount :: non_neg_integer(),
                           Required :: pos_integer(),
                           Total :: pos_integer(),
                           State :: term()) ->
    proceed | cancel_remaining | wait_all.

-callback rank_completions(Completed :: [{term(), term()}],
                          State :: term()) ->
    Ranked :: [{term(), term()}].
```

### 1.3 Selection Behavior: `discriminator_strategy`

For discriminator patterns that select among branches.

```erlang
%%%-------------------------------------------------------------------
%%% @doc Discriminator Strategy Behavior
%%%
%%% Strategy behavior for discriminator patterns that select the
%%% first-completing branch.
%%% @end
%%%-------------------------------------------------------------------
-module(discriminator_strategy).
-behaviour(yawl_strategy).

%% Required callbacks (inherited from yawl_strategy)
-export([init/1, select/2, update/3, terminate/2]).

%% Discriminator specific callbacks
-callback on_branch_complete(BranchId :: term(),
                            Result :: term(),
                            State :: term()) ->
    {accept, NewState :: term()} |
    {reject, NewState :: term()} |
    {defer, NewState :: term()}.

-callback should_reset(State :: term()) ->
    boolean().

-callback select_preferred(Branches :: [term()],
                          BranchMetadata :: map(),
                          State :: term()) ->
    {BranchId :: term(), NewState :: term()}.
```

---

## 2. Registration Protocol

### 2.1 Strategy Registry Module

```erlang
%%%-------------------------------------------------------------------
%%% @doc Strategy Registry
%%%
%%% Central registry for strategy modules with discovery and
%%% lifecycle management capabilities.
%%% @end
%%%-------------------------------------------------------------------
-module(yawl_strategy_registry).
-behaviour(gen_server).

%% API
-export([
    start_link/0,
    register/2,
    unregister/1,
    lookup/1,
    lookup_by_category/1,
    list/0,
    list_available/0
]).

%% Types
-type strategy_id() :: atom() | binary().
-type strategy_entry() :: #{
    id => strategy_id(),
    module => module(),
    category => binary(),
    description => binary(),
    registered_at => integer(),
    config_schema => map(),
    metadata => map()
}.

%%--------------------------------------------------------------------
%% @doc Register a strategy module with the registry.
%%
%% Parameters:
%% - StrategyId: Unique identifier for the strategy
%% - Module: The strategy module implementing yawl_strategy
%%
%% Returns: ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec register(StrategyId :: strategy_id(), Module :: module()) ->
    ok | {error, term()}.

register(StrategyId, Module) when is_atom(Module) ->
    gen_server:call(?MODULE, {register, normalize_id(StrategyId), Module}).

%%--------------------------------------------------------------------
%% @doc Unregister a strategy from the registry.
%% @end
%%--------------------------------------------------------------------
-spec unregister(StrategyId :: strategy_id()) -> ok.
unregister(StrategyId) ->
    gen_server:cast(?MODULE, {unregister, normalize_id(StrategyId)}).

%%--------------------------------------------------------------------
%% @doc Look up a strategy by ID.
%% @end
%%--------------------------------------------------------------------
-spec lookup(StrategyId :: strategy_id()) ->
    {ok, strategy_entry()} | {error, not_found}.
lookup(StrategyId) ->
    gen_server:call(?MODULE, {lookup, normalize_id(StrategyId)}).

%%--------------------------------------------------------------------
%% @doc List all strategies in a category.
%% @end
%%--------------------------------------------------------------------
-spec lookup_by_category(Category :: binary()) ->
    {ok, [strategy_entry()]}.
lookup_by_category(Category) when is_binary(Category) ->
    gen_server:call(?MODULE, {lookup_by_category, Category}).

%%--------------------------------------------------------------------
%% @doc List all registered strategies.
%% @end
%%--------------------------------------------------------------------
-spec list() -> [strategy_entry()].
list() ->
    gen_server:call(?MODULE, list_all).

%%--------------------------------------------------------------------
%% @doc List available (built-in and registered) strategies.
%% @end
%%--------------------------------------------------------------------
-spec list_available() -> #{strategy_id() => strategy_entry()}.
list_available() ->
    gen_server:call(?MODULE, list_available).
```

### 2.2 Registration State Management

```erlang
%%--------------------------------------------------------------------
%% Internal state for the strategy registry
%%--------------------------------------------------------------------
-record(registry_state, {
    strategies :: #{strategy_id() => strategy_entry()},
    categories :: #{binary() => [strategy_id()]},
    monitors :: #{reference() => strategy_id()}
}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------
init([]) ->
    State = #registry_state{
        strategies = #{},
        categories = #{},
        monitors = #{}
    },
    {ok, register_builtin_strategies(State)}.

handle_call({register, StrategyId, Module}, _From, State) ->
    case validate_strategy_module(Module) of
        ok ->
            Entry = create_strategy_entry(StrategyId, Module),
            NewState = store_entry(Entry, State),
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({lookup, StrategyId}, _From, State) ->
    case maps:get(StrategyId, State#registry_state.strategies, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Entry -> {reply, {ok, Entry}, State}
    end;

handle_call({lookup_by_category, Category}, _From, State) ->
    Result = case maps:get(Category, State#registry_state.categories, []) of
        [] -> {ok, []};
        Ids -> {ok, [maps:get(I, State#registry_state.strategies) || I <- Ids]}
    end,
    {reply, Result, State};

handle_call(list_all, _From, State) ->
    Strategies = maps:values(State#registry_state.strategies),
    {reply, Strategies, State};

handle_call(list_available, _From, State) ->
    Builtins = get_builtin_strategies(),
    Registered = State#registry_state.strategies,
    All = maps:merge(Builtins, Registered),
    {reply, All, State}.
```

---

## 3. Strategy Lifecycle Management

### 3.1 Strategy Manager Module

```erlang
%%%-------------------------------------------------------------------
%%% @doc Strategy Manager
%%%
%%% Manages the lifecycle of strategy instances within pattern
%%% execution contexts.
%%% @end
%%%-------------------------------------------------------------------
-module(yawl_strategy_manager).

%% API
-export([
    create_instance/3,
    destroy_instance/2,
    get_instance_state/2,
    update_instance/4,
    switch_strategy/4
]).

%% Types
-type instance_id() :: reference().
-type instance_state() :: #{
    id => instance_id(),
    strategy_id => binary(),
    module => module(),
    state => term(),
    created_at => integer(),
    updated_at => integer(),
    metrics => map()
}.

%%--------------------------------------------------------------------
%% @doc Create a new strategy instance with configuration.
%%
%% Parameters:
%% - StrategyId: The registered strategy identifier
%% - Config: Strategy-specific configuration map
%% - Options: Additional options (timeout, metadata, etc.)
%%
%% Returns: {ok, InstanceId, State} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec create_instance(StrategyId :: binary(),
                     Config :: map(),
                     Options :: map()) ->
    {ok, instance_id(), instance_state()} | {error, term()}.
create_instance(StrategyId, Config, Options) ->
    case yawl_strategy_registry:lookup(StrategyId) of
        {ok, Entry} ->
            Module = maps:get(module, Entry),
            case Module:init(Config) of
                {ok, StrategyState} ->
                    InstanceId = make_ref(),
                    Instance = #{
                        id => InstanceId,
                        strategy_id => StrategyId,
                        module => Module,
                        state => StrategyState,
                        created_at => erlang:system_time(millisecond),
                        updated_at => erlang:system_time(millisecond),
                        metrics => #{}
                    },
                    {ok, InstanceId, Instance};
                {error, Reason} ->
                    {error, {init_failed, Reason}}
            end;
        {error, not_found} ->
            {error, {unknown_strategy, StrategyId}}
    end.

%%--------------------------------------------------------------------
%% @doc Destroy a strategy instance with proper cleanup.
%% @end
%%--------------------------------------------------------------------
-spec destroy_instance(InstanceId :: instance_id(),
                      InstanceState :: instance_state()) ->
    ok.
destroy_instance(InstanceId, #{id := InstanceId, module := Module, state := State}) ->
    Module:terminate(normal, State),
    ok.

%%--------------------------------------------------------------------
%% @doc Get the current state of a strategy instance.
%% @end
%%--------------------------------------------------------------------
-spec get_instance_state(InstanceId :: instance_id(),
                         InstanceState :: instance_state()) ->
    {ok, term()}.
get_instance_state(InstanceId, #{id := InstanceId, state := State}) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Update strategy instance state with an event.
%% @end
%%--------------------------------------------------------------------
-spec update_instance(InstanceId :: instance_id(),
                      Event :: term(),
                      Metadata :: map(),
                      InstanceState :: instance_state()) ->
    {ok, instance_state()} | {error, term()}.
update_instance(InstanceId, Event, Metadata,
                #{id := InstanceId, module := Module, state := State} = InstanceState) ->
    case Module:update(State, Event, Metadata) of
        {ok, NewState} ->
            NewInstance = InstanceState#{
                state => NewState,
                updated_at => erlang:system_time(millisecond),
                metrics => update_metrics(InstanceState, Event)
            },
            {ok, NewInstance};
        {error, Reason} ->
            {error, {update_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Switch from one strategy to another at runtime.
%%
%% Preserves compatible state between strategies when possible.
%% @end
%%--------------------------------------------------------------------
-spec switch_strategy(InstanceId :: instance_id(),
                      OldInstanceState :: instance_state(),
                      NewStrategyId :: binary(),
                      Options :: map()) ->
    {ok, instance_state()} | {error, term()}.
switch_strategy(InstanceId, _OldInstanceState, NewStrategyId, Options) ->
    % Extract migratable state from old instance
    % Create new instance with new strategy
    % Attempt state migration if compatible
    case yawl_strategy_registry:lookup(NewStrategyId) of
        {ok, NewEntry} ->
            NewModule = maps:get(module, NewEntry),
            Config = maps:get(config, Options, #{}),
            case NewModule:init(Config) of
                {ok, NewState} ->
                    {ok, #{
                        id => InstanceId,
                        strategy_id => NewStrategyId,
                        module => NewModule,
                        state => NewState,
                        created_at => erlang:system_time(millisecond),
                        updated_at => erlang:system_time(millisecond),
                        metrics => #{switched_from => maps:get(strategy_id, Options)}
                    }};
                {error, Reason} ->
                    {error, {init_failed, Reason}}
            end;
        {error, not_found} ->
            {error, {unknown_strategy, NewStrategyId}}
    end.
```

---

## 4. Built-in Strategy Catalog

### 4.1 N-of-M Strategies

#### `strategy_n_of_m_first_n`

Selects the first N branches to complete.

```erlang
%%%-------------------------------------------------------------------
%%% @doc First-N Strategy for N-of-M Patterns
%%%
%%% Selects the first N branches that complete, regardless of
%%% their result quality.
%%% @end
%%%-------------------------------------------------------------------
-module(strategy_n_of_m_first_n).
-behaviour(n_of_m_strategy).

%% yawi_strategy callbacks
-export([init/1, select/2, update/3, terminate/2, describe/0]).

%% n_of_m_strategy callbacks
-export([should_complete/4, completion_cutoff/4, rank_completions/2]).

-record(state, {
    n :: pos_integer(),
    m :: pos_integer(),
    completed_count :: non_neg_integer()
}).

%%--------------------------------------------------------------------
%% @doc Initialize the first-N strategy.
%% @end
%%--------------------------------------------------------------------
init(Config) ->
    N = maps:get(n, Config, 1),
    M = maps:get(m, Config, 1),
    {ok, #state{n = N, m = M, completed_count = 0}}.

%%--------------------------------------------------------------------
%% @doc Always proceed when N branches complete.
%% @end
%%--------------------------------------------------------------------
should_complete(Completed, N, _M, _State) when Completed >= N ->
    true;
should_complete(_Completed, _N, _M, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Cancel remaining branches after N complete.
%% @end
%%--------------------------------------------------------------------
completion_cutoff(Completed, N, _M, _State) when Completed >= N ->
    cancel_remaining;
completion_cutoff(_Completed, _N, _M, _State) ->
    proceed.

%%--------------------------------------------------------------------
%% @doc Rank completions by arrival time (first come, first served).
%% @end
%%--------------------------------------------------------------------
rank_completions(Completed, _State) ->
    % Already ordered by completion time
    Completed.

%% Other callbacks...
select(_Options, State) ->
    {selected, [], State}.

update(State, _Event, _Metadata) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

describe() ->
    #{
        name => <<"first_n">>,
        category => <<"n_of_m">>,
        description => <<"Selects the first N branches to complete">>,
        parameters => [
            #{name => n, type => pos_integer, required => true},
            #{name => m, type => pos_integer, required => true}
        ]
    }.
```

#### `strategy_n_of_m_fastest_n`

Selects the N fastest branches based on completion time.

```erlang
%%%-------------------------------------------------------------------
%%% @doc Fastest-N Strategy for N-of-M Patterns
%%%
%%% Tracks completion times and selects the N fastest branches.
%%% Useful for performance-critical workflows.
%%% @end
%%%-------------------------------------------------------------------
-module(strategy_n_of_m_fastest_n).
-behaviour(n_of_m_strategy).

%% yawi_strategy callbacks
-export([init/1, select/2, update/3, terminate/2, describe/0]).

%% n_of_m_strategy callbacks
-export([should_complete/4, completion_cutoff/4, rank_completions/2]).

-record(state, {
    n :: pos_integer(),
    m :: pos_integer(),
    completions :: [{term(), integer()}],  % {BranchId, CompletionTimeMs}
    start_time :: integer()
}).

init(Config) ->
    N = maps:get(n, Config, 1),
    M = maps:get(m, Config, 1),
    {ok, #state{
        n = N,
        m = M,
        completions = [],
        start_time = erlang:monotonic_time(millisecond)
    }}.

%%--------------------------------------------------------------------
%% @doc Only complete when all M have completed (to rank them).
%% @end
%%--------------------------------------------------------------------
should_complete(Completed, _N, M, _State) when Completed >= M ->
    true;
should_complete(_Completed, _N, _M, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Wait for all branches to complete.
%% @end
%%--------------------------------------------------------------------
completion_cutoff(_Completed, _N, _M, _State) ->
    wait_all.

%%--------------------------------------------------------------------
%% @doc Rank completions by completion time (fastest first).
%% @end
%%--------------------------------------------------------------------
rank_completions(Completed, #state{start_time = StartTime}) ->
    % Extract completion times and sort
    WithTimes = [{BranchId, Time} || {BranchId, Time} <- Completed],
    lists:keysort(2, WithTimes).

%% Update state with branch completion
update(State, {branch_complete, BranchId}, Metadata) ->
    CompletionTime = maps:get(completion_time, Metadata, erlang:monotonic_time(millisecond)),
    Duration = CompletionTime - State#state.start_time,
    NewCompletions = [{BranchId, Duration} | State#state.completions],
    {ok, State#state{completions = NewCompletions}};
update(State, _Event, _Metadata) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

describe() ->
    #{
        name => <<"fastest_n">>,
        category => <<"n_of_m">>,
        description => <<"Selects the N fastest branches to complete">>,
        parameters => [
            #{name => n, type => pos_integer, required => true},
            #{name => m, type => pos_integer, required => true}
        ]
    }.
```

#### `strategy_n_of_m_quality_threshold`

Selects branches based on quality metrics.

```erlang
%%%-------------------------------------------------------------------
%%% @doc Quality Threshold Strategy for N-of-M Patterns
%%%
%%% Selects branches that meet a quality threshold. Continues
%%% until N branches meet the threshold or M are exhausted.
%%% @end
%%%-------------------------------------------------------------------
-module(strategy_n_of_m_quality_threshold).
-behaviour(n_of_m_strategy).

%% yawi_strategy callbacks
-export([init/1, select/2, update/3, terminate/2, describe/0]).

%% n_of_m_strategy callbacks
-export([should_complete/4, completion_cutoff/4, rank_completions/2]).

-record(state, {
    n :: pos_integer(),
    m :: pos_integer(),
    threshold :: float(),
    quality_function :: function(),
    qualified :: [{term(), float()}]
}).

init(Config) ->
    N = maps:get(n, Config, 1),
    M = maps:get(m, Config, 1),
    Threshold = maps:get(threshold, Config, 0.8),
    QualityFun = maps:get(quality_function, Config,
                          fun(_Result) -> 1.0 end),
    {ok, #state{
        n = N,
        m = M,
        threshold = Threshold,
        quality_function = QualityFun,
        qualified = []
    }}.

%%--------------------------------------------------------------------
%% @doc Complete when N qualified or M total.
%% @end
%%--------------------------------------------------------------------
should_complete(Completed, N, M, #state{qualified = Qualified}) ->
    QualifiedCount = length(Qualified),
    QualifiedCount >= N orelse Completed >= M.

%%--------------------------------------------------------------------
%% @doc Wait for more branches if qualified < N.
%% @end
%%--------------------------------------------------------------------
completion_cutoff(_Completed, N, _M, #state{qualified = Qualified}) ->
    case length(Qualified) >= N of
        true -> cancel_remaining;
        false -> proceed
    end.

%%--------------------------------------------------------------------
%% @doc Rank qualified by quality score.
%% @end
%%--------------------------------------------------------------------
rank_completions(_Completed, #state{qualified = Qualified}) ->
    lists:reverse(lists:keysort(2, Qualified)).

%% Update with quality assessment
update(State, {branch_complete, BranchId, Result}, _Metadata) ->
    QualityFun = State#state.quality_function,
    Quality = QualityFun(Result),
    case Quality >= State#state.threshold of
        true ->
            NewQualified = [{BranchId, Quality} | State#state.qualified],
            {ok, State#state{qualified = NewQualified}};
        false ->
            {ok, State}
    end;
update(State, _Event, _Metadata) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

describe() ->
    #{
        name => <<"quality_threshold">>,
        category => <<"n_of_m">>,
        description => <<"Selects branches meeting quality threshold">>,
        parameters => [
            #{name => n, type => pos_integer, required => true},
            #{name => m, type => pos_integer, required => true},
            #{name => threshold, type => float, required => true},
            #{name => quality_function, type => function, required => false}
        ]
    }.
```

### 4.2 Discriminator Strategies

#### `strategy_discriminator_first`

Standard discriminator: first branch to complete wins.

```erlang
%%%-------------------------------------------------------------------
%%% @doc First-Wins Discriminator Strategy
%%%
%%% Standard discriminator semantics: the first branch to complete
%%% is selected immediately.
%%% @end
%%%-------------------------------------------------------------------
-module(strategy_discriminator_first).
-behaviour(discriminator_strategy).

%% yawi_strategy callbacks
-export([init/1, select/2, update/3, terminate/2, describe/0]).

%% discriminator_strategy callbacks
-export([on_branch_complete/3, should_reset/1, select_preferred/3]).

-record(state, {
    winner :: undefined | term(),
    completed_count :: non_neg_integer()
}).

init(_Config) ->
    {ok, #state{winner = undefined, completed_count = 0}}.

%% Accept the first completion
on_branch_complete(_BranchId, _Result, #state{winner = undefined}) ->
    {accept, #state{}};
on_branch_complete(_BranchId, _Result, State) ->
    {reject, State}.

%% Only reset after consuming all
should_reset(#state{completed_count = Count, total = Total}) when Count >= Total ->
    true;
should_reset(_) ->
    false.

%% No preference, accept first
select_preferred(Branches, _Metadata, _State) ->
    {hd(Branches), #state{}}.

select(_Options, State) ->
    {selected, [], State}.

update(State, _Event, _Metadata) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

describe() ->
    #{
        name => <<"first_wins">>,
        category => <<"discriminator">>,
        description => <<"First branch to complete wins">>,
        parameters => []
    }.
```

#### `strategy_discriminator_adaptive`

Adaptive discriminator that learns from historical performance.

```erlang
%%%-------------------------------------------------------------------
%%% @doc Adaptive Discriminator Strategy
%%%
%%% Learns which branches complete faster and adapts selection
%%% to prefer faster branches while maintaining diversity.
%%% @end
%%%-------------------------------------------------------------------
-module(strategy_discriminator_adaptive).
-behaviour(discriminator_strategy).

%% yawi_strategy callbacks
-export([init/1, select/2, update/3, terminate/2, describe/0]).

%% discriminator_strategy callbacks
-export([on_branch_complete/3, should_reset/1, select_preferred/3]).

-record(state, {
    branch_weights :: #{term() => float()},
    branch_history :: #{term() => [integer()]},
    learning_rate :: float(),
    diversity_factor :: float()
}).

init(Config) ->
    Branches = maps:get(branches, Config, []),
    InitialWeight = 1.0 / length(Branches),
    Weights = maps:from_list([{B, InitialWeight} || B <- Branches]),
    {ok, #state{
        branch_weights = Weights,
        branch_history = maps:from_list([{B, []} || B <- Branches]),
        learning_rate = maps:get(learning_rate, Config, 0.1),
        diversity_factor = maps:get(diversity_factor, Config, 0.2)
    }}.

%% Accept first completion but track for learning
on_branch_complete(BranchId, CompletionTime, State) ->
    % Update weights based on completion time
    NewWeights = update_weights(
        State#state.branch_weights,
        State#state.branch_history,
        BranchId,
        CompletionTime,
        State#state.learning_rate
    ),
    {accept, State#state{branch_weights = NewWeights}}.

%% Always reset to continue learning
should_reset(_State) ->
    true.

%% Select based on weighted probability
select_preferred(Branches, _Metadata, #state{branch_weights = Weights, diversity_factor = Diversity}) ->
    % Apply diversity factor to prevent pure exploitation
    AdjustedWeights = maps:map(fun(_K, V) ->
        V * (1 - Diversity) + Diversity / length(Branches)
    end, Weights),
    WeightedBranches = [{B, maps:get(B, AdjustedWeights, 1.0)} || B <- Branches],
    weighted_select(WeightedBranches).

%% Update weights using exponential moving average
update_weights(Weights, History, BranchId, CompletionTime, Alpha) ->
    % Update history for this branch
    BranchHistory = maps:get(BranchId, History, []),
    NewHistory = [CompletionTime | lists:sublist(BranchHistory, 9)],
    AvgTime = lists:sum(NewHistory) / length(NewHistory),

    % Normalize times (lower is better, so use reciprocal)
    NormalizedScore = 1.0 / (1.0 + AvgTime),

    % Update weight for completed branch
    OldWeight = maps:get(BranchId, Weights, 1.0),
    NewWeight = Alpha * NormalizedScore + (1 - Alpha) * OldWeight,

    % Normalize all weights to sum to 1
    NewWeights = maps:put(BranchId, NewWeight, Weights),
    Total = maps:fold(fun(_K, V, Acc) -> Acc + V end, 0, NewWeights),
    maps:map(fun(_K, V) -> V / Total end, NewWeights).

%% Weighted random selection
weighted_select(WeightedItems) ->
    Total = lists:foldl(fun({_Item, W}, Acc) -> Acc + W end, 0, WeightedItems),
    Rnd = rand:uniform(),
    select_weighted(WeightedItems, Rnd, 0.0, Total).

select_weighted([{Item, W} | _Rest], Rnd, Acc, Total) when (Acc + W) / Total >= Rnd ->
    Item;
select_weighted([{_Item, W} | Rest], Rnd, Acc, Total) ->
    select_weighted(Rest, Rnd, Acc + W, Total).

select(_Options, State) ->
    {selected, [], State}.

update(State, _Event, _Metadata) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

describe() ->
    #{
        name => <<"adaptive">>,
        category => <<"discriminator">>,
        description => <<"Learns which branches complete faster">>,
        parameters => [
            #{name => branches, type => list, required => true},
            #{name => learning_rate, type => float, required => false},
            #{name => diversity_factor, type => float, required => false}
        ]
    }.
```

### 4.3 Composite Strategies

#### `strategy_composite`

Combines multiple strategies with composition rules.

```erlang
%%%-------------------------------------------------------------------
%%% @doc Composite Strategy
%%%
%%% Combines multiple strategies using composition patterns:
%%% - fallback: Try primary, use secondary if it fails
%%% - consensus: All strategies must agree
%%% - weighted: Weighted voting among strategies
%%% - pipeline: Chain strategies sequentially
%%% @end
%%%-------------------------------------------------------------------
-module(strategy_composite).
-behaviour(yawl_strategy).

%% yawi_strategy callbacks
-export([init/1, select/2, update/3, terminate/2, describe/0]).

-record(state, {
    composition_type :: fallback | consensus | weighted | pipeline,
    strategies :: [{module(), term()}],  % {Module, State}
    weights :: [float()] | undefined,
    primary_error_count :: non_neg_integer()
}).

init(Config) ->
    StrategySpecs = maps:get(strategies, Config, []),
    CompositionType = maps:get(composition_type, Config, fallback),
    Weights = maps:get(weights, Config, undefined),

    % Initialize all strategies
    Strategies = lists:map(fun({Module, ModuleConfig}) ->
        {ok, StrategyState} = Module:init(ModuleConfig),
        {Module, StrategyState}
    end, StrategySpecs),

    {ok, #state{
        composition_type = CompositionType,
        strategies = Strategies,
        weights = Weights,
        primary_error_count = 0
    }}.

%% Select using composition strategy
select(Options, #state{composition_type = Type, strategies = Strategies} = State) ->
    case Type of
        fallback ->
            select_fallback(Options, Strategies, State);
        consensus ->
            select_consensus(Options, Strategies, State);
        weighted ->
            select_weighted(Options, Strategies, State#state.weights, State);
        pipeline ->
            select_pipeline(Options, Strategies, State)
    end.

%% Fallback: try primary, use secondary on failure
select_fallback(Options, [{PrimaryModule, PrimaryState} | Rest], State) ->
    case PrimaryModule:select(Options, PrimaryState) of
        {selected, Selected, NewState} ->
            {selected, Selected, State#state{
                strategies = [{PrimaryModule, NewState} | Rest]
            }};
        {error, _Reason} when Rest =/= [] ->
            % Fall through to secondary
            select_fallback(Options, Rest, State);
        {error, Reason} ->
            {error, {all_strategies_failed, Reason}}
    end.

%% Consensus: all strategies must agree
select_consensus(Options, Strategies, State) ->
    Results = [Module:select(Options, S) || {Module, S} <- Strategies],
    case all_agree(Results) of
        {true, Agreed} ->
            {selected, Agreed, State};
        false ->
            {defer, no_consensus, State}
    end.

%% Weighted voting
select_weighted(Options, Strategies, Weights, State) ->
    WeightedResults = lists:map(fun({{Module, S}, W}) ->
        case Module:select(Options, S) of
            {selected, Selected, _} -> {Selected, W};
            _ -> {undefined, 0}
        end
    end, lists:zip(Strategies, Weights)),

    % Aggregate by selection
    Aggregated = aggregate_weights(WeightedResults),
    case maps:keys(Aggregated) of
        [] -> {error, no_selection};
        Selected ->
            % Select highest weight
            {Best, _} = lists:max(fun({_, A}, {_, B}) -> A > B end,
                                 maps:to_list(Aggregated)),
            {selected, Best, State}
    end.

%% Pipeline: chain strategies
select_pipeline(Options, [{Module, S} | Rest], State) ->
    case Module:select(Options, S) of
        {selected, Filtered, NewState} when Rest =/= [] ->
            % Pass filtered options to next strategy
            select_pipeline(Filtered, Rest, State);
        {selected, Selected, NewState} ->
            {selected, Selected, State};
        {error, Reason} ->
            {error, Reason}
    end.

%% Update all strategies
update(State, Event, Metadata) ->
    NewStrategies = lists:map(fun({Module, S}) ->
        {ok, NewS} = Module:update(S, Event, Metadata),
        {Module, NewS}
    end, State#state.strategies),
    {ok, State#state{strategies = NewStrategies}}.

%% Terminate all strategies
terminate(_Reason, #state{strategies = Strategies}) ->
    lists:foreach(fun({Module, S}) ->
        Module:terminate(normal, S)
    end, Strategies),
    ok.

describe() ->
    #{
        name => <<"composite">>,
        category => <<"composite">>,
        description => <<"Combines multiple strategies">>,
        parameters => [
            #{name => composition_type, type => atom, required => true},
            #{name => strategies, type => list, required => true},
            #{name => weights, type => list, required => false}
        ]
    }.

%% Helper: check if all results agree
all_agree([]) ->
    {true, undefined};
all_agree([{selected, Selected, _} | Rest]) ->
    case lists:all(fun
        ({selected, S, _}) when S =:= Selected -> true;
        (_) -> false
    end, Rest) of
        true -> {true, Selected};
        false -> false
    end;
all_agree(_) ->
    false.

%% Helper: aggregate weights by selection
aggregate_weights(WeightedResults) ->
    lists:foldl(fun
        ({undefined, _}, Acc) -> Acc;
        ({Selected, W}, Acc) ->
            maps:update_with(Selected, fun(V) -> V + W end, W, Acc)
    end, #{}, WeightedResults).
```

---

## 5. Example Custom Strategies

### 5.1 Cost-Aware Strategy

Selects branches based on estimated execution cost.

```erlang
%%%-------------------------------------------------------------------
%%% @doc Cost-Aware Strategy Example
%%%
%%% Custom strategy that selects branches based on cost constraints.
%%% Demonstrates how to implement a domain-specific strategy.
%%% @end
%%%-------------------------------------------------------------------
-module(strategy_cost_aware).
-behaviour(yawl_strategy).

%% yawi_strategy callbacks
-export([init/1, select/2, update/3, terminate/2, describe/0]).

-record(state, {
    max_cost :: float(),
    cost_function :: function(),
    spent :: float()
}).

init(Config) ->
    MaxCost = maps:get(max_cost, Config),
    CostFun = maps:get(cost_function, Config, fun(_) -> 1.0 end),
    {ok, #state{
        max_cost = MaxCost,
        cost_function = CostFun,
        spent = 0.0
    }}.

%% Select options that fit within remaining budget
select(Options, #state{max_cost = Max, spent = Spent, cost_function = CostFun}) ->
    Remaining = Max - Spent,
    Affordable = lists:filter(fun(Opt) ->
        CostFun(Opt) =< Remaining
    end, Options),
    case Affordable of
        [] ->
            {defer, budget_exceeded, #state{}};
        _ ->
            % Prefer lowest cost
            Sorted = lists:sort(fun(A, B) ->
                CostFun(A) =< CostFun(B)
            end, Affordable),
            {selected, [hd(Sorted)], #state{}}
    end.

%% Track spending
update(State, {cost_incurred, Amount}, _Metadata) ->
    {ok, State#state{spent = State#state.spent + Amount}};
update(State, _Event, _Metadata) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

describe() ->
    #{
        name => <<"cost_aware">>,
        category => <<"custom">>,
        description => <<"Selects branches based on cost constraints">>,
        parameters => [
            #{name => max_cost, type => float, required => true},
            #{name => cost_function, type => function, required => false}
        ]
    }.
```

### 5.2 Time-Window Strategy

Selects branches that complete within a time window.

```erlang
%%%-------------------------------------------------------------------
%%% @doc Time-Window Strategy Example
%%%
%%% Custom strategy that only accepts completions within a
%%% specified time window from start.
%%% @end
%%%-------------------------------------------------------------------
-module(strategy_time_window).
-behaviour(yawl_strategy).

%% yawi_strategy callbacks
-export([init/1, select/2, update/3, terminate/2, describe/0]).

-record(state, {
    window_ms :: pos_integer(),
    start_time :: integer(),
    completions :: [{term(), integer()}]
}).

init(Config) ->
    WindowMs = maps:get(window_ms, Config, 5000),
    {ok, #state{
        window_ms = WindowMs,
        start_time = erlang:monotonic_time(millisecond),
        completions = []
    }}.

select(_Options, #state{completions = Completions}) ->
    % Return all within-window completions
    {selected, [Id || {Id, _} <- Completions], #state{}}.

update(State, {branch_complete, BranchId}, _Metadata) ->
    Now = erlang:monotonic_time(millisecond),
    Completions = [{BranchId, Now} | State#state.completions],
    {ok, State#state{completions = Completions}};
update(State, _Event, _Metadata) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

describe() ->
    #{
        name => <<"time_window">>,
        category => <<"custom">>,
        description => <<"Only accepts completions within time window">>,
        parameters => [
            #{name => window_ms, type => pos_integer, required => true}
        ]
    }.
```

### 5.3 ML-Guided Strategy (Skeleton)

Placeholder for machine learning guided strategy.

```erlang
%%%-------------------------------------------------------------------
%%% @doc ML-Guided Strategy (Skeleton)
%%%
%%% Demonstrates integration with external ML model for
%%% strategy decisions. This is a design specification for
%%% future integration with ML backends.
%%% @end
%%%-------------------------------------------------------------------
-module(strategy_ml_guided).
-behaviour(yawl_strategy).

%% yawi_strategy callbacks
-export([init/1, select/2, update/3, terminate/2, describe/0]).

-record(state, {
    model_endpoint :: binary(),
    model_version :: binary(),
    feature_extractor :: function(),
    prediction_cache :: map(),
    cache_hits :: non_neg_integer()
}).

init(Config) ->
    ModelEndpoint = maps:get(model_endpoint, Config),
    ModelVersion = maps:get(model_version, Config, <<"latest">>),
    FeatureExtractor = maps:get(feature_extractor, Config,
                               fun(_) -> #{} end),
    {ok, #state{
        model_endpoint = ModelEndpoint,
        model_version = ModelVersion,
        feature_extractor = FeatureExtractor,
        prediction_cache = #{},
        cache_hits = 0
    }}.

%% Select using ML model predictions
select(Options, #state{feature_extractor = Extractor,
                         model_endpoint = Endpoint,
                         prediction_cache = Cache} = State) ->
    % Extract features for each option
    Features = [{Opt, Extractor(Opt)} || Opt <- Options],

    % Query model for predictions (cached)
    Scores = lists:map(fun({Opt, Feats}) ->
        case maps:get(Feats, Cache, undefined) of
            undefined ->
                Score = query_model(Endpoint, Feats),
                {Opt, Score};
            CachedScore ->
                State1 = State#state{cache_hits = State#state.cache_hits + 1},
                {Opt, CachedScore}
        end
    end, Features),

    % Sort by score and select top
    Sorted = lists:sort(fun({_, A}, {_, B}) -> A > B end, Scores),
    {selected, [Opt || {Opt, _} <- Sorted], State}.

%% Update cache with actual outcomes for learning
update(State, {outcome, Features, ActualScore}, _Metadata) ->
    % Update cache with ground truth
    NewCache = maps:put(Features, ActualScore, State#state.prediction_cache),
    {ok, State#state{prediction_cache = NewCache}};
update(State, _Event, _Metadata) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

describe() ->
    #{
        name => <<"ml_guided">>,
        category => <<"experimental">>,
        description => <<"Uses ML model for strategy decisions">>,
        parameters => [
            #{name => model_endpoint, type => binary, required => true},
            #{name => model_version, type => binary, required => false},
            #{name => feature_extractor, type => function, required => false}
        ]
    }.

%%--------------------------------------------------------------------
%% @doc Query external ML model for prediction.
%% @private
%% @end
%%--------------------------------------------------------------------
query_model(Endpoint, Features) ->
    % In actual implementation, this would make HTTP request
    % to ML service and return prediction score
    case httpc:request(post, {Endpoint, [], "application/json",
                              encode_features(Features)}, [], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            #{<<"score">> := Score} = decode_response(Body),
            Score;
        {error, Reason} ->
            logger:error("ML model query failed: ~p", [Reason]),
            0.5  % Default neutral score
    end.
```

---

## 6. Type Definitions

### 6.1 Core Type Specification

```erlang
%%%-------------------------------------------------------------------
%%% @doc Strategy Types
%%%
%%% Central type definitions for the strategy plugin system.
%%% @end
%%%-------------------------------------------------------------------
-module(yawl_strategy_types).

%%--------------------------------------------------------------------
%% Strategy Identifiers
%%--------------------------------------------------------------------
-type strategy_id() :: atom() | binary().
-type strategy_module() :: module().
-type strategy_category() :: binary().

%%--------------------------------------------------------------------
%% Strategy Instance
%%--------------------------------------------------------------------
-type strategy_instance() :: #{
    id := instance_id(),
    strategy_id := strategy_id(),
    module := strategy_module(),
    state := term(),
    created_at => integer(),
    updated_at => integer(),
    metrics => map()
}.

-type instance_id() :: reference().

%%--------------------------------------------------------------------
%% Strategy Configuration
%%--------------------------------------------------------------------
-type strategy_config() :: #{
    required => #{atom() => term()},
    optional => #{atom() => term()},
    metadata => map()
}.

%%--------------------------------------------------------------------
%% Strategy Events
%%--------------------------------------------------------------------
-type strategy_event() ::
    {branch_complete, term()} |
    {branch_complete, term(), term()} |
    {branch_error, term(), term()} |
    {cost_incurred, float()} |
    {outcome, term(), float()} |
    {timeout, term()}.

-type strategy_metadata() :: #{
    timestamp => integer(),
    branch_id => term(),
    completion_time => integer(),
    result => term()
}.

%%--------------------------------------------------------------------
%% Strategy Selection Results
%%--------------------------------------------------------------------
-type selection_result() ::
    {selected, [term()], term()} |
    {defer, term(), term()} |
    {error, term()}.

%%--------------------------------------------------------------------
%% Strategy Lifecycle States
%%--------------------------------------------------------------------
-type strategy_lifecycle() ::
    initializing |
    active |
    switching |
    terminating |
    terminated.

%%--------------------------------------------------------------------
%% Composite Types
%%--------------------------------------------------------------------
-type strategy_entry() :: #{
    id => strategy_id(),
    module => strategy_module(),
    category => strategy_category(),
    description => binary(),
    registered_at => integer(),
    config_schema => strategy_config(),
    metadata => map()
}.

-type strategy_registry() :: #{strategy_id() => strategy_entry()}.

%%--------------------------------------------------------------------
%% Export all types
%%--------------------------------------------------------------------
-export_type([
    strategy_id/0,
    strategy_module/0,
    strategy_category/0,
    strategy_instance/0,
    instance_id/0,
    strategy_config/0,
    strategy_event/0,
    strategy_metadata/0,
    selection_result/0,
    strategy_lifecycle/0,
    strategy_entry/0,
    strategy_registry/0
]).
```

---

## 7. Integration Points

### 7.1 Integration with N-of-M Pattern

```erlang
%%%-------------------------------------------------------------------
%%% @doc Enhanced N-of-M Pattern with Strategy Support
%%%
%%% This specification shows how the existing n_out_of_m pattern
%%% would be modified to support pluggable strategies.
%%% @end
%%%-------------------------------------------------------------------
-module(n_out_of_m_strategy).
-behaviour(gen_yawl).

%% Add strategy parameter to state record
-record(n_out_of_m_state, {
    m :: pos_integer(),
    n :: pos_integer(),
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    results = [] :: [{pos_integer(), term()}],
    quorum_met = false :: boolean(),
    wait_for_all = false :: boolean(),
    log_id :: binary() | undefined,

    %% NEW: Strategy support
    strategy_module :: module() | undefined,
    strategy_state :: term() | undefined,
    strategy_instance :: reference() | undefined
}).

%%--------------------------------------------------------------------
%% @doc Create N-of-M with strategy.
%% @end
%%--------------------------------------------------------------------
-spec new_with_strategy(N :: pos_integer(),
                        M :: pos_integer(),
                        BranchFuns :: [function()],
                        StrategyModule :: module(),
                        StrategyConfig :: map()) ->
    n_out_of_m_state().

new_with_strategy(N, M, BranchFuns, StrategyModule, StrategyConfig) ->
    case yawl_strategy_manager:create_instance(
        StrategyModule,
        StrategyConfig,
        #{n => N, m => M}
    ) of
        {ok, InstanceId, InstanceState} ->
            #n_out_of_m_state{
                m = M,
                n = N,
                branch_funs = BranchFuns,
                strategy_module = StrategyModule,
                strategy_state = InstanceState,
                strategy_instance = InstanceId,
                log_id = generate_log_id()
            };
        {error, Reason} ->
            error({strategy_init_failed, Reason})
    end.

%%--------------------------------------------------------------------
%% @doc Enhanced fire/3 with strategy callbacks.
%% @end
%%--------------------------------------------------------------------
fire('t_check_quorum', #{'p_completed' := Completed},
      #n_out_of_m_state{n = N, m = M, strategy_module = undefined} = State) ->
    % Original behavior when no strategy
    RemainingCount = M - length(Completed),
    case RemainingCount of
        0 ->
            {produce, #{'p_completed' => [], 'p_output' => [{quorum_met, Completed}]}};
        _ ->
            {produce, #{
                'p_completed' => [],
                'p_quorum_met' => [quorum, Completed],
                'p_remaining' => [remaining, RemainingCount]
            }}
    end;

fire('t_check_quorum', #{'p_completed' := Completed},
      #n_out_of_m_state{n = N, m = M, strategy_module = StrategyModule,
                       strategy_state = StrategyState} = State) ->
    % Use strategy to determine completion
    CompletedCount = length(Completed),
    case StrategyModule:should_complete(CompletedCount, N, M, StrategyState) of
        true ->
            % Use strategy to rank and select completions
            Ranked = StrategyModule:rank_completions(
                [{I, element(2, lists:keyfind(I, 1, State#n_out_of_m_state.results))} ||
                 I <- Completed],
                StrategyState
            ),
            SelectedN = lists:sublist(Ranked, N),
            {produce, #{
                'p_completed' => [],
                'p_output' => [{strategy_selected, SelectedN}]
            }};
        false ->
            case StrategyModule:completion_cutoff(CompletedCount, N, M, StrategyState) of
                cancel_remaining ->
                    {produce, #{'p_completed' => [], 'p_output' => [{cancelled, Completed}]}};
                wait_all ->
                    {produce, #{
                        'p_completed' => [],
                        'p_remaining' => [remaining, M - CompletedCount]
                    }};
                proceed ->
                    {produce, #{
                        'p_completed' => [],
                        'p_quorum_met' => [quorum, Completed],
                        'p_remaining' => [remaining, M - CompletedCount]
                    }}
            end
    end.
```

### 7.2 Integration with Discriminator Pattern

```erlang
%%%-------------------------------------------------------------------
%%% @doc Enhanced Discriminator Pattern with Strategy Support
%%%
%%% This specification shows how the existing discriminator pattern
%%% would be modified to support pluggable strategies.
%%% @end
%%%-------------------------------------------------------------------
-module(discriminator_strategy).
-behaviour(gen_yawl).

%% Add strategy parameter to state record
-record(discriminator_state, {
    branch_count :: pos_integer(),
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    triggered_by :: undefined | pos_integer(),
    cycle_count = 0 :: non_neg_integer(),
    log_id :: binary() | undefined,

    %% NEW: Strategy support
    strategy_module :: module() | undefined,
    strategy_state :: term() | undefined,
    branch_metadata :: #{pos_integer() => map()}
}).

%%--------------------------------------------------------------------
%% @doc Create discriminator with strategy.
%% @end
%%--------------------------------------------------------------------
-spec new_with_strategy(BranchFuns :: [function()],
                        BranchCount :: pos_integer(),
                        StrategyModule :: module(),
                        StrategyConfig :: map()) ->
    discriminator_state().

new_with_strategy(BranchFuns, BranchCount, StrategyModule, StrategyConfig) ->
    case yawl_strategy_manager:create_instance(
        StrategyModule,
        StrategyConfig,
        #{branch_count => BranchCount, branches => BranchFuns}
    ) of
        {ok, InstanceId, InstanceState} ->
            #discriminator_state{
                branch_count = BranchCount,
                branch_funs = BranchFuns,
                strategy_module = StrategyModule,
                strategy_state = InstanceState,
                branch_metadata = initialize_branch_metadata(BranchFuns)
            };
        {error, Reason} ->
            error({strategy_init_failed, Reason})
    end.

%%--------------------------------------------------------------------
%% @doc Enhanced fire/3 with strategy callbacks.
%% @end
%%--------------------------------------------------------------------
fire('t_complete_branch',
      #{'p_branch_pool' := [{{branch, Index}, _Fun} | Rest]},
      #discriminator_state{triggered_by = undefined,
                          strategy_module = StrategyModule,
                          strategy_state = StrategyState} = State) ->
    % Use strategy to decide on this branch completion
    CompletionTime = erlang:monotonic_time(millisecond),
    Metadata = #{completion_time => CompletionTime},

    case StrategyModule:on_branch_complete(Index, CompletionTime, StrategyState) of
        {accept, NewStrategyState} ->
            log_event(State, <<"Discriminator">>, <<"BranchAccepted">>,
                     #{<<"branch">> => Index}),
            {produce, #{
                'p_branch_pool' => Rest,
                'p_triggered' => [first_complete, Index]
            }, State#discriminator_state{
                triggered_by = Index,
                strategy_state = NewStrategyState
            }};
        {reject, NewStrategyState} ->
            log_event(State, <<"Discriminator">>, <<"BranchRejected">>,
                     #{<<"branch">> => Index}),
            % Continue waiting
            {produce, #{
                'p_branch_pool' => Rest
            }, State#discriminator_state{strategy_state = NewStrategyState}};
        {defer, NewStrategyState} ->
            log_event(State, <<"Discriminator">>, <<"BranchDeferred">>,
                     #{<<"branch">> => Index}),
            {produce, #{
                'p_branch_pool' => Rest
            }, State#discriminator_state{strategy_state = NewStrategyState}}
    end.
```

### 7.3 Strategy Switching API

```erlang
%%%-------------------------------------------------------------------
%%% @doc Runtime Strategy Switching API
%%%
%%% Allows switching strategies during pattern execution for
%%% adaptive optimization.
%%% @end
%%%-------------------------------------------------------------------
-module(yawl_strategy_switch).

%% API
-export([
    switch_to/3,
    switch_with_migration/3,
    get_available_switches/1,
    validate_switch/2
]).

%%--------------------------------------------------------------------
%% @doc Switch to a new strategy, discarding current state.
%% @end
%%--------------------------------------------------------------------
-spec switch_to(PatternPid :: pid(),
                NewStrategyId :: binary(),
                Options :: map()) ->
    ok | {error, term()}.
switch_to(PatternPid, NewStrategyId, Options) ->
    gen_yawl:call(PatternPid, {switch_strategy, NewStrategyId, Options}).

%%--------------------------------------------------------------------
%% @doc Switch to a new strategy with state migration.
%% @end
%%--------------------------------------------------------------------
-spec switch_with_migration(PatternPid :: pid(),
                            NewStrategyId :: binary(),
                            MigrationFun :: function()) ->
    ok | {error, term()}.
switch_with_migration(PatternPid, NewStrategyId, MigrationFun) ->
    gen_yawl:call(PatternPid, {switch_strategy_with_migration,
                               NewStrategyId, MigrationFun}).

%%--------------------------------------------------------------------
%% @doc Get available strategy switches for a running pattern.
%% @end
%%--------------------------------------------------------------------
-spec get_available_switches(PatternPid :: pid()) ->
    {ok, [binary()]}.
get_available_switches(PatternPid) ->
    gen_yawl:call(PatternPid, get_strategy_switches).

%%--------------------------------------------------------------------
%% @doc Validate if a strategy switch is valid.
%% @end
%%--------------------------------------------------------------------
-spec validate_switch(FromStrategy :: binary(),
                      ToStrategy :: binary()) ->
    ok | {error, term()}.
validate_switch(FromId, ToId) ->
    case yawl_strategy_registry:lookup(FromId) of
        {ok, FromEntry} ->
            case yawl_strategy_registry:lookup(ToId) of
                {ok, ToEntry} ->
                    FromCat = maps:get(category, FromEntry),
                    ToCat = maps:get(category, ToEntry),
                    case FromCat =:= ToCat of
                        true -> ok;
                        false -> {error, category_mismatch}
                    end;
                {error, _} ->
                    {error, unknown_target_strategy}
            end;
        {error, _} ->
            {error, unknown_source_strategy}
    end.
```

---

## Appendix A: Configuration Schema

### Strategy Configuration JSON Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "title": "YAWL Strategy Configuration",
  "type": "object",
  "required": ["strategy_id"],
  "properties": {
    "strategy_id": {
      "type": "string",
      "description": "Unique identifier for the strategy"
    },
    "strategy_type": {
      "type": "string",
      "enum": ["n_of_m", "discriminator", "composite", "custom"],
      "description": "Type of strategy"
    },
    "config": {
      "type": "object",
      "description": "Strategy-specific configuration"
    },
    "composition": {
      "type": "object",
      "description": "Composition rules for composite strategies",
      "properties": {
        "type": {
          "type": "string",
          "enum": ["fallback", "consensus", "weighted", "pipeline"]
        },
        "strategies": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "strategy_id": {"type": "string"},
              "weight": {"type": "number"}
            }
          }
        }
      }
    }
  }
}
```

---

## Appendix B: Error Handling Specification

### Strategy Error Types

```erlang
%%%-------------------------------------------------------------------
%%% @doc Strategy Error Types
%%%
%%% Standardized error types for strategy operations.
%%% @end
%%%-------------------------------------------------------------------
-module(yawl_strategy_errors).

%% Strategy errors
-type strategy_error() ::
    {unknown_strategy, strategy_id()} |
    {strategy_init_failed, term()} |
    {strategy_update_failed, term()} |
    {invalid_strategy_config, term()} |
    {category_mismatch, binary(), binary()} |
    {incompatible_state, term()} |
    {all_strategies_failed, term()} |
    {no_consensus, term()} |
    {budget_exceeded, float()} |
    {timeout, term()}.

%% Export error type
-export_type([strategy_error/0]).

%%--------------------------------------------------------------------
%% @doc Format strategy error for logging.
%% @end
%%--------------------------------------------------------------------
-spec format_error(strategy_error()) -> iolist().
format_error({unknown_strategy, Id}) ->
    io_lib:format("Unknown strategy: ~p", [Id]);
format_error({strategy_init_failed, Reason}) ->
    io_lib:format("Strategy initialization failed: ~p", [Reason]);
format_error({strategy_update_failed, Reason}) ->
    io_lib:format("Strategy update failed: ~p", [Reason]);
format_error({invalid_strategy_config, Config}) ->
    io_lib:format("Invalid strategy configuration: ~p", [Config]);
format_error({category_mismatch, Expected, Actual}) ->
    io_lib:format("Strategy category mismatch: expected ~s, got ~s",
                  [Expected, Actual]);
format_error({incompatible_state, State}) ->
    io_lib:format("Incompatible strategy state: ~p", [State]);
format_error({all_strategies_failed, Reason}) ->
    io_lib:format("All strategies failed: ~p", [Reason]);
format_error({no_consensus, Details}) ->
    io_lib:format("No consensus among strategies: ~p", [Details]);
format_error({budget_exceeded, Budget}) ->
    io_lib:format("Budget exceeded: ~p", [Budget]);
format_error({timeout, Details}) ->
    io_lib:format("Strategy timeout: ~p", [Details]).
```

---

## Conclusion

This specification defines a complete, extensible strategy plugin system for CRE. It:

1. **Provides a clean behavior contract** via `yawl_strategy` with required and optional callbacks
2. **Enables registration and discovery** through `yawl_strategy_registry`
3. **Manages strategy lifecycle** via `yawl_strategy_manager` with create/destroy/switch operations
4. **Includes built-in strategies** for common N-of-M and discriminator patterns
5. **Demonstrates extensibility** with custom strategy examples
6. **Defines integration points** for existing patterns

The system is designed for:
- **Composability**: Strategies can be combined using composite patterns
- **Adaptivity**: Runtime strategy switching enables adaptive optimization
- **Extensibility**: New strategies follow a simple behavior contract
- **Type Safety**: Comprehensive type definitions enable dialyzer checking

---

**Document Version:** 1.0
**Author:** Claude (CRE Strategy Plugin Design)
**Status:** Design Specification - Interface Only
