# Utility Modules Guide

This guide provides detailed documentation for the 8 new utility modules that form the core of the refactored CRE architecture.

## Core Philosophy

All utility modules follow these principles:
- **Pure Functions**: Stateless, referentially transparent
- **Total Functions**: Handle all inputs gracefully (no crashes)
- **Immutable Operations**: Return new states instead of modifying existing ones
- **Type Safety**: Comprehensive type definitions and validation
- **Determinism**: Reproducible execution where randomness is involved

---

## 1. `pnet_types.erl` - Type System and Validation

### Overview
The type system module defines all core data structures and provides validation functions for the Petri net framework. All validation functions are total - they return boolean() and never crash, making them safe to use in guards and assertions.

### Key Types Defined

#### Basic Petri Net Types
```erlang
% Basic building blocks
-type place() :: atom().                    % Place identifier
-type trsn() :: atom().                    % Transition identifier
-type token() :: term().                   % Token value (can be any Erlang term)

% State representations
-type marking() :: #{place() => [token()]}. % Map of places to token lists
-type consume_map() :: #{place() => [token()]}. % Tokens to consume
-type produce_map() :: #{place() => [token()]}. % Tokens to produce

% Mode representation
-type mode() :: #{place() => [token()]}.   % One possible input combination
```

#### Colored Petri Net Extension
```erlang
% Colored net types
-type var() :: atom().                     % Variable name
-type binding() :: #{var() => term()}.     % Variable to value mapping
-type cmode() :: {binding(), mode()}.      % Colored mode with binding
```

#### Execution Types
```erlang
% Transition execution
-type move() :: #{
    trsn := trsn(),                       % Transition identifier
    mode := mode() | cmode(),             % Firing mode
    produce := produce_map()               % Output tokens
}.

% Audit trail
-type receipt() :: #{
    before_hash := binary(),              % Hash before execution
    after_hash := binary(),               % Hash after execution
    move := move(),                       % Transition execution
    ts := integer()                        % Unix timestamp
}.
```

### Key Functions

#### Validation Functions (All return boolean())
```erlang
% Basic structure validation
is_marking(Term) -> boolean().             % Valid marking structure
is_mode(Term) -> boolean().               % Valid mode structure
is_consume_map(Term) -> boolean().        % Valid consume map
is_produce_map(Term) -> boolean().        % Valid produce map

% Colored net validation
is_binding(Term) -> boolean().            % Valid variable binding
is_cmode(Term) -> boolean().              % Valid colored mode (tuple of {binding(), mode()})
```

### Usage Examples

```erlang
% Basic validation
ValidMarking = #{p1 => [a, b], p2 => [c]},
pnet_types:is_marking(ValidMarking).      % true

InvalidMarking = #{p1 => a, p2 => [c]},
pnet_types:is_marking(InvalidMarking).    % false

% Colored net validation
Binding = #{x => 42, y => "test"},
pnet_types:is_binding(Binding).            % true

% Mode validation
Mode = #{p1 => [a], p2 => [b]},
pnet_types:is_mode(Mode).                 % true
```

### Error Handling
All validation functions are total:
- Never crash on any input
- Return false for invalid structures
- Safe to use in guards and assertions
    maps:fold(fun(Place, NewTokens, Acc) ->
        case maps:find(Place, Acc) of
            {ok, ExistingTokens} ->
                Acc#{Place => ExistingTokens ++ NewTokens};
            error ->
                {error, bad_place}
        end
    end, Marking, ProduceMap).

% Take tokens (multiset difference - order-insensitive)
-spec take(marking(), consume_map()) -> {ok, marking()} | {error, bad_place | insufficient}.
take(Marking, ConsumeMap) ->
    maps:fold(fun(Place, TokensToTake, Acc) ->
        case maps:find(Place, Acc) of
            {ok, ExistingTokens} ->
                case consume_tokens(ExistingTokens, TokensToTake) of
                    {ok, RemainingTokens} -> Acc#{Place => RemainingTokens};
                    {error, insufficient} -> throw({error, insufficient})
                end;
            error -> throw({error, bad_place})
        end
    end, Marking, ConsumeMap).

% Helper: order-insensitive bag subtraction
consume_tokens(Available, []) -> {ok, Available};
consume_tokens([], [_|_]) -> {error, insufficient};
consume_tokens([Token|Rest], TokensToTake) ->
    case lists:member(Token, TokensToTake) of
        true -> consume_tokens(Rest, lists:delete(Token, TokensToTake));
        false ->
            case consume_tokens(Rest, TokensToTake) of
                {ok, Remaining} -> {ok, [Token | Remaining]};
                {error, _} = Err -> Err
            end
    end.
```

#### State Management
```erlang
% Apply transition: consume then produce atomically
-spec apply(marking(), consume_map(), produce_map()) ->
    {ok, marking()} | {error, bad_place | insufficient}.
apply(Marking, ConsumeMap, ProduceMap) ->
    case take(Marking, ConsumeMap) of
        {ok, Marking1} -> add(Marking1, ProduceMap);
        {error, Reason} -> {error, Reason}
    end.

% Generate consistent hash for marking (canonical for bags)
-spec hash(marking()) -> binary().
hash(Marking) ->
    crypto:hash(sha256, term_to_binary(Marking)).

% Create immutable snapshot (identity for immutable maps)
-spec snapshot(marking()) -> marking().
snapshot(Marking) -> Marking.
```

### Usage Examples

```erlang
% Create and manipulate marking
M0 = pnet_marking:new([p1, p2, p3]),
{ok, M1} = pnet_marking:add(M0, #{p1 => [a, b], p2 => [c]}),

% Get tokens
{ok, Tokens} = pnet_marking:get(M1, p1),  % [a, b]

% Take tokens (order-insensitive)
case pnet_marking:take(M1, #{p1 => [b, a]}) of  % Order doesn't matter
    {ok, M2} ->
        % Tokens removed from p1
        {ok, Remaining} = pnet_marking:get(M2, p1),  % []
        ok;
    {error, insufficient} -> insufficient_tokens
end.

% Apply transition: consume + produce atomically
{ok, M3} = pnet_marking:apply(M2, #{p2 => [c]}, #{p3 => [d]}).

% Hash is canonical for bags (order doesn't matter)
{ok, Ma} = pnet_marking:set(M0, p, [a, b]),
{ok, Mb} = pnet_marking:set(M0, p, [b, a]),
pnet_marking:hash(Ma) =:= pnet_marking:hash(Mb).  % true
```

### Key Features
- **Multiset Semantics**: Token multiplicity matters, order doesn't
- **Total Functions**: Always return {ok, ...} or {error, ...}
- **Unknown Place Handling**: Returns {error, bad_place} instead of defaulting
- **Canonical Hashing**: Same multiset in any order produces same hash
- **Immutable**: All operations return new markings

---

## 3. `pnet_choice.erl` - Deterministic Nondeterminism

### Overview
Provides reproducible random selection for workflow execution. All randomness goes through this module to ensure reproducible behavior when seeded. Uses a pure-functional RNG with no process state.

### Key Data Structure
```erlang
% RNG state using simple XOR-shift PRNG or exs1024s algorithm
-type rng_state() :: {non_neg_integer(), non_neg_integer(), non_neg_integer(),
                      non_neg_integer()} | {exs1024s,
                                               non_neg_integer(),
                                               non_neg_integer(),
                                               non_neg_integer(),
                                               non_neg_integer()}.
```

### Core Operations

#### Random Number Generation
```erlang
% Create deterministic RNG from seed term
-spec seed(term()) -> rng_state().
seed(SeedTerm) ->
    IntSeed = erlang:phash2(SeedTerm),
    {A, B, C} = {IntSeed bxor 16#5deece66d, IntSeed bxor 16#babe, IntSeed bxor 16#deadbeef},
    State = seed_splitmix(A, B, C),
    State.

% Pick random element from list
-spec pick([T], rng_state()) -> {T, rng_state()} | {error, empty}.
pick([], _RngState) -> {error, empty};
pick(List, RngState) ->
    Len = length(List),
    {Index, NewRngState} = rand_uniform(Len, RngState),
    Element = lists:nth(Index + 1, List),
    {Element, NewRngState}.

% Pick from weighted options
-spec pick_weighted([{T, pos_integer()}], rng_state()) -> {T, rng_state()} |
                                                           {error, empty | bad_weights}.
pick_weighted([], _RngState) -> {error, empty};
pick_weighted(Items, RngState) ->
    case validate_weights(Items) of
        false -> {error, bad_weights};
        true ->
            TotalWeight = lists:sum([W || {_, W} <- Items]),
            {RandValue, NewRngState} = rand_uniform(TotalWeight, RngState),
            select_weighted(Items, RandValue, 0)
    end.
```

### Internal Functions

#### Weight Selection
```erlang
% Validate positive weights
validate_weights([]) -> true;
validate_weights([{_Item, Weight} | Rest]) when is_integer(Weight), Weight > 0 ->
    validate_weights(Rest);
validate_weights(_) -> false.

% Select using cumulative weights
select_weighted([{Item, _Weight} | _Rest], RandValue, _Acc) when RandValue < 0 ->
    {Item, undefined};
select_weighted([{Item, Weight} | Rest], RandValue, Acc) ->
    NewAcc = Acc + Weight,
    if
        RandValue < NewAcc -> {Item, undefined};
        true -> select_weighted(Rest, RandValue, NewAcc)
    end.
```

#### RNG Implementation
```erlang
% Uniform random integer in [0, Max-1]
rand_uniform(Max, {S1, S2, S3} = State) when Max > 0 ->
    %% XOR-shift PRNG implementation
    S1_ = (S1 bxor (S1 bsl 13)) band 16#ffffffff,
    S1_1 = S1_ bxor (S1_ bsr 17),
    S1_2 = S1_1 bxor (S1_1 bsl 5),

    S2_ = (S2 bxor (S2 bsl 2)) band 16#ffffffff,
    S2_1 = S2_ bxor (S2_ bsr 7),
    S2_2 = S2_1 bxor (S2_1 bsl 3),

    S3_ = (S3 bxor (S3 bsl 21)) band 16#ffffffff,
    S3_1 = S3_ bxor (S3_ bsr 13),
    S3_2 = S3_1 bxor (S3_1 bsl 8),

    Rand = (S1_2 + S2_2 + S3_2) band 16#ffffffff,
    NewState = {S1_2, S2_2, S3_2},

    {(Rand rem Max), NewState}.

% Splitmix-style seeding
seed_splitmix(A, B, C) ->
    S1 = mix(A),
    S2 = mix(B),
    S3 = mix(C),
    {S1, S2, S3}.

% Mixing function for seeding
mix(X) ->
    X1 = X bxor (X bsr 30),
    X2 = X1 * 16#bf58476d1ce4e5b9 band 16#ffffffffffffffff,
    X3 = X2 bxor (X2 bsr 27),
    X4 = X3 * 16#94d049bb133111eb band 16#ffffffffffffffff,
    X4 bxor (X4 bsr 31).
```

### Usage Examples

```erlang
% Basic random selection
Rng0 = pnet_choice:seed(12345),
{Choice, Rng1} = pnet_choice:pick([a, b, c], Rng0).

% Weighted selection
Options = [{a, 1}, {b, 3}, {c, 1}],  % b has 3x weight
{WeightedChoice, Rng2} = pnet_choice:pick_weighted(Options, Rng1).

% Reproducible results
SameRng0 = pnet_choice:seed(12345),
{SameChoice, _} = pnet_choice:pick([a, b, c], SameRng0).
% SameChoice will always be the same for the same seed
```

### Key Features
- **Pure Functional**: No process state, returns updated state
- **Deterministic**: Reproducible when seeded
- **Weighted Selection**: Support for probability distributions
- **Total Functions**: Handles empty lists gracefully

---

## 4. `pnet_mode.erl` - Mode Enumeration

### Overview
Enumerates all possible firing modes for transitions. A mode specifies which tokens are consumed from each input place when a transition fires. Supports both basic and colored Petri nets.

### Core Operations

#### Basic Mode Enumeration
```erlang
% Get required token counts for preset places (handles multiplicity)
-spec preset_counts([place()]) -> #{place() => non_neg_integer()}.
preset_counts(PresetPlaces) ->
    lists:foldl(fun(P, Acc) ->
        maps:update_with(P, fun(V) -> V + 1 end, 1, Acc)
    end, #{}, PresetPlaces).

% Example: preset_counts([p, p, q]) => #{p => 2, q => 1}

% Enumerate all possible modes given current marking
% Handles repeated places via preset_counts
-spec enum_modes([place()], marking()) -> [mode()].
enum_modes(PresetPlaces, Marking) ->
    Counts = preset_counts(PresetPlaces),
    UniquePlaces = lists:usort(PresetPlaces),
    enum_modes_for_places(UniquePlaces, Counts, Marking).
```

#### Colored Net Extension
```erlang
% Enumerate colored modes with variable bindings
-spec enum_cmodes(atom(), marking(), term(), module()) -> [cmode()].
enum_cmodes(Trsn, Marking, Ctx, NetMod) ->
    %% Check if net module implements colored modes
    case erlang:function_exported(NetMod, cmodes, 3) of
        true ->
            try
                NetMod:cmodes(Trsn, Marking, Ctx)
            catch
                _:_:_-> []
            end;
        false ->
            %% Fall back to basic mode enumeration
            Modes = enum_modes(NetMod:preset(Trsn), Marking),
            [{#{}, M} || M <- Modes]
    end.
```

### Usage Examples

```erlang
% Basic mode enumeration
PresetPlaces = [p1, p2, p3],
Marking = #{p1 => [a, b], p2 => [c], p3 => [d, e]},
Modes = pnet_mode:enum_modes(PresetPlaces, Marking),
% Returns all combinations:
% [#{p1 => [a], p2 => [c], p3 => [d]},
%  #{p1 => [a], p2 => [c], p3 => [e]},
%  #{p1 => [b], p2 => [c], p3 => [d]},
%  #{p1 => [b], p2 => [c], p3 => [e]}]

% Colored mode enumeration
% Assuming my_net:cmodes/3 exists and returns colored modes
Cmodes = pnet_mode:enum_cmodes(t1, Marking, my_context, my_net).
% Returns list of {Binding, Mode} tuples
```

### Key Features
- **Cartesian Product**: Generates all possible token combinations
- **Colored Net Support**: Variable bindings for complex patterns
- **Graceful Handling**: Empty token lists return no modes
- **Fallback Mechanism**: Basic enumeration for uncolored nets

---

## 5. `pnet_receipt.erl` - Receipt Tracking

### Overview
Creates audit trails for all state transitions. Receipts provide immutable records of what changed during a transition, including before/after hashes and timestamps.

### Key Data Structure
```erlang
% Immutable audit trail record
-type receipt() :: #{
    before_hash := binary(),              % Hash before execution
    after_hash := binary(),               % Hash after execution
    move := move(),                       % Transition execution
    ts := integer()                        % Unix timestamp
}.
```

### Core Operations

#### Receipt Creation
```erlang
% Create receipt for state transition
-spec make(binary(), binary(), move()) -> receipt().
make(BeforeHash, AfterHash, Move) ->
    #{
        before_hash => BeforeHash,
        after_hash => AfterHash,
        move => Move,
        ts => timestamp()
    }.

% Get current monotonic timestamp (milliseconds)
-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time(millisecond).
```

#### Receipt Analysis
```erlang
% Extract transition effects from receipt
% By default, receipts have no associated effects and return an empty list.
% This design allows callback modules to extend the receipt system with
% custom effects extraction logic.
-spec effects(Receipt :: receipt()) -> [effect()].
effects(#{}) -> [].
```

### Usage Examples

```erlang
% Create receipt for transition
BeforeHash = crypto:hash(sha256, term_to_binary(Marking1)),
Move = #{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}},
AfterHash = crypto:hash(sha256, term_to_binary(Marking2)),
Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move).

% Extract effects (returns empty list by default)
Effects = pnet_receipt:effects(Receipt),
% Effects = []
```

### Key Features
- **Immutable Audit Trail**: Complete state transition records
- **Timestamped**: Precise timing for all operations
- **State Hashing**: Before/after state verification
- **Extensible Effects**: Default empty list, customizable by callbacks

---

## 6. `wf_timerq.erl` - Deadline Queue

### Overview
Manages time-based token injection for deadlines and timeouts. Implements a pure-functional priority queue where events are processed when their deadlines are reached.

### Key Data Structure
```erlang
% Timer queue entries
-type deadline() :: non_neg_integer().      % Unix timestamp
-type key() :: term().                     % Unique identifier
-type event() -> term().                   % Event data
-type entry() :: {deadline(), key(), event()}.
-type timerq() -> [entry()].                % Priority-sorted list
```

### Core Operations

#### Queue Management
```erlang
% Create empty timer queue
-spec new() -> timerq().
new() -> [].

% Arm (schedule) an event
-spec arm(timerq(), key(), deadline(), event()) -> timerq().
arm(TimerQ, Key, Deadline, Event) ->
    Q1 = disarm(TimerQ, Key),              % Remove existing entry
    Entry = {Deadline, Key, Event},
    lists:keysort(1, [Entry | Q1]).        % Sort by deadline

% Disarm (cancel) an event
-spec disarm(timerq(), key()) -> timerq().
disarm(TimerQ, Key) ->
    lists:filter(fun({_, K, _}) -> K /= Key end, TimerQ).
```

#### Event Processing
```erlang
% Poll for ready events (returns {Events, UpdatedQ})
-spec poll(timerq(), deadline()) -> {[timer_event()], timerq()}.
poll(TimerQ, Now) ->
    poll(TimerQ, Now, []).

% Internal accumulator
poll([], _Now, Acc) ->
    {lists:reverse(Acc), []};
poll([{Deadline, _Key, Event} | Rest], Now, Acc) when Deadline =< Now ->
    poll(Rest, Now, [Event | Acc]);
poll([{Deadline, _Key, _Event} | _] = TimerQ, Now, Acc) when Deadline > Now ->
    {lists:reverse(Acc), TimerQ}.

% Check if queue is empty
-spec is_empty(timerq()) -> boolean().
is_empty([]) -> true;
is_empty(_) -> false.

% Get queue size
-spec size(timerq()) -> non_neg_integer().
size(TimerQ) -> length(TimerQ).

% Peek at next event without removing
-spec peek(timerq()) -> {ok, deadline(), timer_key(), timer_event()} | empty.
peek([]) -> empty;
peek([{Deadline, Key, Event} | _]) -> {ok, Deadline, Key, Event}.
```

### Usage Examples

```erlang
% Create and manage timer queue
Tq0 = wf_timerq:new(),

% Schedule events
Tq1 = wf_timerq:arm(Tq0, task1, 1640995200, {timeout, task1}),
Tq2 = wf_timerq:arm(Tq1, task2, 1640995260, {timeout, task2}),

% Poll for ready events
Now = 1640995210,  % Current time
{Events, Tq3} = wf_timerq:poll(Tq2, Now),
% Events = [{timeout, task1}] if task1 deadline <= Now

% Cancel event
Tq4 = wf_timerq:disarm(Tq3, task2).
```

### Key Features
- **Pure Functional**: No side effects, returns new queues
- **Priority Queue**: Events sorted by deadline
- **Key-based Management**: Unique identifiers for event management
- **Efficient Operations**: O(n) operations for simplicity

---

## 7. `wf_task.erl` - Task Lifecycle Constructors

### Overview
Creates produce maps for external task state transitions. Standardizes the creation of task tokens for different states (enabled, running, done, failed, cancelled).

### Core Operations

#### Task State Transitions
```erlang
% Enabled state
-spec enabled(task_id(), term(), place()) -> {produce, produce_map()}.
enabled(TaskId, Payload, Place) ->
    Token = {task, TaskId, enabled, Payload},
    {produce, #{Place => [Token]}}.

% Running state
-spec running(task_id(), term(), place()) -> {produce, produce_map()}.
running(TaskId, Payload, Place) ->
    Token = {task, TaskId, running, Payload},
    {produce, #{Place => [Token]}}.

% Completed state
-spec done(task_id(), term(), place()) -> {produce, produce_map()}.
done(TaskId, Output, Place) ->
    Token = {task, TaskId, done, Output},
    {produce, #{Place => [Token]}}.

% Failed state
-spec failed(task_id(), term(), place()) -> {produce, produce_map()}.
failed(TaskId, Payload, Place) ->
    Token = {task, TaskId, failed, Payload},
    {produce, #{Place => [Token]}}.

% Cancelled state
-spec cancelled(task_id(), term(), place()) -> {produce, produce_map()}.
cancelled(TaskId, Payload, Place) ->
    Token = {task, TaskId, cancelled, Payload},
    {produce, #{Place => [Token]}}.
```

### Type Definitions
```erlang
% Task identifier
-type task_id() :: term().

% Task payload (any Erlang term)
-type payload() :: term().
```

### Usage Examples

```erlang
% Create task tokens
EnabledToken = wf_task:enabled(my_task_id, #{type => "approval"}, approval_place),
RunningToken = wf_task:running(my_task_id, #{progress => 50}, running_place),
DoneToken = wf_task:done(my_task_id, #{result => "approved"}, done_place).

% Use in transition
fire(Transition, Marking, Ctx) ->
    %% Create produce map for completed task
    wf_task:done(my_task_id, #{result => "approved"}, done_place).
```

### Key Features
- **Standardized Interface**: Consistent task state tokens
- **Flexible Payloads**: Any Erlang term can be stored
- **Produce Map Format**: Returns standard {produce, Map} structure
- **Integration Ready**: Works with external work systems

---

## 8. `wf_scope.erl` - Scope Boundary Mapping

### Overview
Handles place namespace translation between parent and child workflows. Essential for hierarchical workflows where place names differ between parent and child contexts.

### Key Data Structures
```erlang
% Scope identifier
-type scope_id() :: term().

% Place mapping table
-type binding_table() :: #{scope_id() => #{place() => place()}}.

% Various input formats
-type input() :: marking() | produce_map() | [place()].
```

### Core Operations

#### Scope Boundary Operations
```erlang
% Enter scope: translate parent tokens to child places
-spec enter(binding_table(), scope_id(), input()) -> produce_map().
enter(BindingTable, ScopeId, ParentDeltaOrMarking) ->
    case maps:get(ScopeId, BindingTable, undefined) of
        undefined -> normalize_to_produce_map(ParentDeltaOrMarking);
        Mapping when is_map(Mapping) -> translate_places(ParentDeltaOrMarking, Mapping)
    end.

% Leave scope: translate child results back to parent places
-spec leave(binding_table(), scope_id(), input()) -> produce_map().
leave(BindingTable, ScopeId, ChildDeltaOrMarking) ->
    case maps:get(ScopeId, BindingTable, undefined) of
        undefined -> normalize_to_produce_map(ChildDeltaOrMarking);
        Mapping when is_map(Mapping) ->
            ReverseMapping = maps:from_list(
                [{ChildPlace, ParentPlace} ||
                 {ParentPlace, ChildPlace} <- maps:to_list(Mapping)]
            ),
            translate_places(ChildDeltaOrMarking, ReverseMapping)
    end.

% Get bindings for specific scope
-spec bindings(binding_table(), scope_id()) ->
                   #{place() => place()} | {error, unknown_scope}.
bindings(BindingTable, ScopeId) ->
    case maps:get(ScopeId, BindingTable, undefined) of
        undefined -> {error, unknown_scope};
        Mapping -> Mapping
    end.
```

### Internal Functions
```erlang
% Normalize input to produce map
normalize_to_produce_map(Input) when is_map(Input) -> Input;
normalize_to_produce_map(Input) when is_list(Input) ->
    maps:from_list([{P, []} || P <- Input]).

% Translate places using mapping
translate_places(Input, Mapping) when is_map(Input), is_map(Mapping) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        NewPlace = maps:get(Place, Mapping, Place),
        Acc#{NewPlace => Tokens}
    end, #{}, Input).
```

### Usage Examples

```erlang
% Define binding table
BindingTable = #{my_subflow => #{parent_input => child_input,
                                parent_output => child_output}},

% Enter scope
ParentTokens = #{parent_input => [data1, data2]},
ChildProduceMap = wf_scope:enter(BindingTable, my_subflow, ParentTokens),
% => #{child_input => [data1, data2]}

% Leave scope
ChildTokens = #{child_output => [result]},
ParentProduceMap = wf_scope:leave(BindingTable, my_subflow, ChildTokens),
% => #{parent_output => [result]}
```

### Key Features
- **Namespace Translation**: Seamless parent-child workflow integration
- **Bidirectional Mapping**: Both enter and leave operations
- **Flexible Input**: Accepts markings, produce maps, or place lists
- **Error Handling**: Graceful handling of unknown scopes

---

## Integration Patterns

All utility modules are designed to work together seamlessly:

1. **Typical Workflow**:
   ```erlang
   % 1. Validate and initialize
   pnet_types:is_marking(Marking),
   {ok, Marking1} = pnet_marking:add(Marking0, ProduceMap),

   % 2. Enumerate modes (handles multiplicity)
   Modes = pnet_mode:enum_modes(Preset, Marking1),

   % 3. Make choice
   {SelectedMode, NewRng} = pnet_choice:pick(Modes, Rng),

   % 4. Apply transition: consume + produce atomically
   {ok, Marking2} = pnet_marking:apply(Marking1, ConsumeMap, ProduceMap),

   % 5. Create receipt with monotonic timestamp
   Receipt = pnet_receipt:make(Hash1, Hash2, Move).
   ```

2. **Hierarchical Workflows**:
   ```erlang
   % Parent workflow
   ChildProduceMap = wf_scope:enter(Bindings, ChildId, ParentTokens),
   % ... execute child ...
   ParentProduceMap = wf_scope:leave(Bindings, ChildId, ChildTokens).
   ```

3. **External Tasks**:
   ```erlang
   % Enable task
   Enabled = wf_task:enabled(TaskId, Payload, TaskPlace),
   % Monitor with timers
   TimerQ = wf_timerq:arm(TimerQ, TaskId, Deadline, {produce, ProduceMap}).
   ```

This completes the comprehensive documentation for all utility modules.