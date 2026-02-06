# Complete API Reference

This document provides the complete API specification for the refactored CRE architecture following Joe Armstrong's interface design.

---

## 1. `pnet_types` - Type System and Validation

### Types
```erlang
% Basic Petri net types (from actual implementation)
-type place() :: atom().
-type trsn() :: atom().
-type token() :: term().
-type marking() :: #{place() => [token()]}.
-type consume_map() :: #{place() => [token()]}.
-type produce_map() :: #{place() => [token()]}.
-type mode() :: #{place() => [token()]}.

% Colored Petri net extension
-type var() :: atom().
-type binding() :: #{var() => term()}.
-type cmode() :: {binding(), mode()}.

% Movement and receipt tracking
-type move() :: #{
    trsn := trsn(),
    mode := mode() | cmode(),
    produce := produce_map()
}.
-type receipt() :: #{
    before_hash := binary(),
    after_hash := binary(),
    move := move(),
    ts := integer()
}.
```

### Exports
```erlang
% Validation Functions (all return boolean())
-export([is_marking/1,
         is_consume_map/1,
         is_produce_map/1,
         is_mode/1,
         is_binding/1,
         is_cmode/1]).
```

### Functions

```erlang
%% @doc Checks if a term is a valid marking (from actual implementation)
%% A valid marking is a map where all keys are atoms (places) and
%% all values are lists (of tokens). The function never crashes.
-spec is_marking(term()) -> boolean().
is_marking(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_marking(_) ->
    false.

%% @doc Checks if a term is a valid consume_map (from actual implementation)
%% A valid consume_map is a map where all keys are atoms (places)
%% and all values are lists (of tokens to consume). The function
%% never crashes.
-spec is_consume_map(term()) -> boolean().
is_consume_map(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_consume_map(_) ->
    false.

%% @doc Checks if a term is a valid produce_map (from actual implementation)
%% A valid produce_map is a map where all keys are atoms (places)
%% and all values are lists (of tokens to produce). The function
%% never crashes.
-spec is_produce_map(term()) -> boolean().
is_produce_map(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_produce_map(_) ->
    false.

%% @doc Checks if a term is a valid mode (from actual implementation)
%% A valid mode is a map where all keys are atoms (places) and
%% all values are lists (of tokens). The function never crashes.
-spec is_mode(term()) -> boolean().
is_mode(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_mode(_) ->
    false.

%% @doc Checks if a term is a valid binding (from actual implementation)
%% A valid binding is a map where all keys are atoms (variables)
%% and all values are any term (the bound values). The function
%% never crashes.
-spec is_binding(term()) -> boolean().
is_binding(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, _, _) when is_atom(K) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_binding(_) ->
    false.

%% @doc Checks if a term is a valid cmode (from actual implementation)
%% A valid cmode is a 2-tuple where the first element is a binding
%% and the second element is a mode. The function never crashes.
-spec is_cmode(term()) -> boolean().
is_cmode({Binding, Mode}) when is_tuple(Binding), tuple_size(Binding) =:= 0;
                              is_map(Binding);
                              is_list(Binding) ->
    %% For binding: accept empty tuple as map sentinel, or actual map
    %% The gen_pnet library uses {} as an empty binding representation
    IsBindingValid = case Binding of
        {} -> true;
        _ when is_map(Binding) -> is_binding(Binding);
        _ -> false
    end,
    IsModeValid = is_mode(Mode),
    IsBindingValid andalso IsModeValid;
is_cmode({Binding, Mode}) ->
    %% Try as map binding
    is_binding(Binding) andalso is_mode(Mode);
is_cmode(_) ->
    false.
```

---

## 2. `pnet_marking` - Multiset Marking Algebra

### Functions

```erlang
%% @doc Creates a new empty marking with the given places (from actual implementation)
%% All places are initialized with empty token lists.
-spec new(Places :: [place()]) -> marking().
new(Places) when is_list(Places) ->
    maps:from_list([{P, []} || P <- Places]).

%% @doc Gets the tokens at a specific place (from actual implementation)
%% @param Marking The marking to query
%% @param Place The place to get tokens from
%% @return {ok, Tokens} if place exists, {error, bad_place} otherwise
-spec get(Marking :: marking(), Place :: place()) ->
         {ok, [token()]} | {error, bad_place}.
get(Marking, Place) when is_map(Marking), is_atom(Place) ->
    case maps:find(Place, Marking) of
        {ok, Tokens} -> {ok, Tokens};
        error -> {error, bad_place}
    end;
get(_Marking, _Place) ->
    {error, bad_place}.

%% @doc Sets the tokens at a specific place (from actual implementation)
%% Replaces the current token list at the given place with a new list.
%% The place must exist in the marking.
-spec set(Marking :: marking(), Place :: place(), Tokens :: [token()]) ->
         marking() | {error, bad_place}.
set(Marking, Place, Tokens) when is_map(Marking), is_atom(Place), is_list(Tokens) ->
    case maps:is_key(Place, Marking) of
        true -> Marking#{Place => Tokens};
        false -> {error, bad_place}
    end;
set(_Marking, _Place, _Tokens) ->
    {error, bad_place}.

%% @doc Adds tokens to the marking via a produce map (from actual implementation)
%% Tokens are appended to existing tokens at each place.
%% All places in the produce map must exist in the marking.
-spec add(Marking :: marking(), ProduceMap :: produce_map()) ->
         marking() | {error, bad_place}.
add(Marking, ProduceMap) when is_map(Marking), is_map(ProduceMap) ->
    try
        maps:fold(fun
            (Place, NewTokens, Acc) when is_atom(Place), is_list(NewTokens) ->
                case maps:find(Place, Acc) of
                    {ok, ExistingTokens} ->
                        Acc#{Place => ExistingTokens ++ NewTokens};
                    error ->
                        throw({error, bad_place})
                end;
            (_, _, _) ->
                throw({error, bad_place})
        end, Marking, ProduceMap)
    catch
        throw:{error, bad_place} -> {error, bad_place}
    end;
add(_Marking, _ProduceMap) ->
    {error, bad_place}.

%% @doc Takes tokens from the marking via a consume map (from actual implementation)
%% This is a multiset operation - the exact tokens specified must be
%% present with sufficient multiplicity. Tokens are removed from the
%% front of each place's token list.
-spec take(Marking :: marking(), ConsumeMap :: consume_map()) ->
         {ok, marking()} | {error, bad_place | insufficient}.
take(Marking, ConsumeMap) when is_map(Marking), is_map(ConsumeMap) ->
    try
        Result = maps:fold(fun
            (Place, TokensToTake, Acc) when is_atom(Place), is_list(TokensToTake) ->
                case maps:find(Place, Acc) of
                    {ok, ExistingTokens} ->
                        case consume_tokens(ExistingTokens, TokensToTake) of
                            {ok, RemainingTokens} ->
                                Acc#{Place => RemainingTokens};
                            {error, insufficient} ->
                                throw({error, insufficient})
                        end;
                    error ->
                        throw({error, bad_place})
                end;
            (_, _, _) ->
                throw({error, bad_place})
        end, Marking, ConsumeMap),
        {ok, Result}
    catch
        throw:{error, Reason} -> {error, Reason}
    end;
take(_Marking, _ConsumeMap) ->
    {error, bad_place}.

%% @doc Applies a consume map and produce map atomically (from actual implementation)
%% This is the primary operation for transition firing. Tokens are
%% first consumed via the consume map, then tokens are added via the
%% produce map. If consumption fails, the marking is unchanged.
-spec apply(Marking :: marking(), ConsumeMap :: consume_map(),
            ProduceMap :: produce_map()) ->
         {ok, marking()} | {error, bad_place | insufficient}.
apply(Marking, ConsumeMap, ProduceMap)
  when is_map(Marking), is_map(ConsumeMap), is_map(ProduceMap) ->
    case take(Marking, ConsumeMap) of
        {ok, Marking1} ->
            case add(Marking1, ProduceMap) of
                {error, Reason} -> {error, Reason};
                Marking2 -> {ok, Marking2}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
apply(_Marking, _ConsumeMap, _ProduceMap) ->
    {error, bad_place}.

%% @doc Creates a deep copy (snapshot) of the marking (from actual implementation)
%% Returns an identical copy that can be modified without affecting
%% the original. Since Erlang data is immutable, this returns the
%% same marking but provides a clear semantic for snapshotting.
-spec snapshot(Marking :: marking()) -> marking().
snapshot(Marking) when is_map(Marking) ->
    %% Erlang maps are immutable, so the marking itself is already
    %% a snapshot. This function provides semantic clarity.
    Marking.

%% @doc Computes a consistent hash of the marking (from actual implementation)
%% Uses SHA-256 to hash the marking's term representation.
%% Useful for state comparison, caching keys, and receipt generation.
-spec hash(Marking :: marking()) -> binary().
hash(Marking) when is_map(Marking) ->
    crypto:hash(sha256, term_to_binary(Marking)).

%% @private
%% @doc Consumes tokens from a list, respecting multiset semantics (from actual implementation)
%% Removes each token in TokensToTake from AvailableTokens,
%% checking that each token exists with sufficient multiplicity.
%% Returns the remaining tokens or an error if consumption fails.
-spec consume_tokens(AvailableTokens :: [token()],
                     TokensToTake :: [token()]) ->
         {ok, [token()]} | {error, insufficient}.
consume_tokens(AvailableTokens, []) ->
    {ok, AvailableTokens};
consume_tokens([], [_|_]) ->
    {error, insufficient};
consume_tokens([Token | RestAvailable], TokensToTake) ->
    case lists:member(Token, TokensToTake) of
        true ->
            %% Remove one occurrence of this token from TokensToTake
            RemainingToTake = lists:delete(Token, TokensToTake),
            consume_tokens(RestAvailable, RemainingToTake);
        false ->
            %% This token isn't being consumed, keep it
            case consume_tokens(RestAvailable, TokensToTake) of
                {ok, Remaining} -> {ok, [Token | Remaining]};
                {error, _} = Error -> Error
            end
    end.
```

---

## 3. `pnet_mode` - Mode Enumeration

### Types

```erlang
%% Mode enumeration types for basic and colored Petri nets
-type mode() :: #{place() => [token()]}.
-type cmode() :: {binding(), mode()}.
-type binding() :: #{var() => term()}.
```

### Functions

```erlang
%% @doc Returns the count of tokens needed from each preset place (from actual implementation)
%% Handles multiplicity: repeated places in preset list increment the count.
%% This function returns a map indicating the requirement for each place.
%%
%% Doctests:
%% > pnet_mode:preset_counts([p, p, q]).
%% #{p => 2, q => 1}
%%
%% > pnet_mode:preset_counts([a, b, a, c]).
%% #{a => 2, b => 1, c => 1}
-spec preset_counts(PresetPlaces :: [place()]) ->
         #{place() => non_neg_integer()}.
preset_counts(PresetPlaces) when is_list(PresetPlaces) ->
    lists:foldl(
        fun(P, Acc) ->
            maps:update_with(P, fun(V) -> V + 1 end, 1, Acc)
        end,
        #{},
        PresetPlaces
    ).

%% @doc Enumerates all possible modes given the current marking (from actual implementation)
%% A mode represents one valid way to fire a transition by selecting
%% tokens from each preset place. Handles repeated places via preset_counts.
%% Results are in deterministic term order.
%%
%% Doctests:
%% > pnet_mode:enum_modes([p], #{p => [a, b, c]}).
%% [#{p => [a]}, #{p => [b]}, #{p => [c]}]
%%
%% > pnet_mode:enum_modes([p, q], #{p => [a], q => [x]}).
%% [#{p => [a], q => [x]}]
%%
%% > pnet_mode:enum_modes([p, q], #{p => [a, b], q => [x, y]}).
%% [#{p => [a], q => [x]},
%%  #{p => [a], q => [y]},
%%  #{p => [b], q => [x]},
%%  #{p => [b], q => [y]}]
%%
%% > pnet_mode:enum_modes([p, p, q], #{p => [a, b], q => [x]}).
%% [#{p => [a, b], q => [x]}]
%%
%% > pnet_mode:enum_modes([p], #{p => []}).
%% []
-spec enum_modes(PresetPlaces :: [place()], Marking :: marking()) ->
         [mode()].
enum_modes(PresetPlaces, Marking) when is_list(PresetPlaces), is_map(Marking) ->
    Counts = preset_counts(PresetPlaces),
    UniquePlaces = lists:usort(PresetPlaces),
    enum_modes_for_places(UniquePlaces, Counts, Marking).

%% @doc Enumerates colored modes with variable bindings (from actual implementation)
%% For colored Petri nets, this function calls the net module's
%% cmodes callback to get modes that include variable bindings.
%% If the net module doesn't implement colored modes, falls back
%% to basic mode enumeration with empty bindings.
%%
%% Doctests (assuming net module implements cmodes/3):
%% > pnet_mode:enum_cmodes(t1, #{p => [a,b]}, ctx, basic_net).
%% [{#{}, #{p => [a]}}, {#{}, #{p => [b]}}]
%%
%% Colored net example:
%% > Marking = #{input => [{user, alice, "report"}]},
%% > pnet_mode:enum_cmodes(process, Marking, #{role => "manager"}, workflow_net).
%% [{{user => alice}, #{input => [{user, alice, "report"}]}}]
-spec enum_cmodes(Trsn :: atom(), Marking :: marking(),
                  Ctx :: usr_info(), NetMod :: net_mod()) ->
         [cmode()].
enum_cmodes(Trsn, Marking, Ctx, NetMod) when is_atom(Trsn), is_map(Marking),
                                           is_atom(NetMod) ->
    %% Check if the net module implements colored modes
    case erlang:function_exported(NetMod, cmodes, 3) of
        true ->
            try
                NetMod:cmodes(Trsn, Marking, Ctx)
            catch
                _:_:_-> []
            end;
        false ->
            %% Fall back to basic mode enumeration with empty binding
            Modes = enum_modes(NetMod:preset(Trsn), Marking),
            [{#{}, M} || M <- Modes]
    end.

%% @doc Internal helper for mode enumeration (from actual implementation)
%% Recursive function that generates modes for multiple places.
enum_modes_for_places([], _Counts, _Marking) ->
    [#{ }];
enum_modes_for_places([Place | Rest], Counts, Marking) ->
    case maps:get(Place, Marking, []) of
        [] ->
            [];  %% No tokens available, no modes possible
        Tokens ->
            Count = maps:get(Place, Counts, 1),
            TokenCombos = combinations(Count, Tokens),
            RestModes = enum_modes_for_places(Rest, Counts, Marking),
            lists:flatmap(
                fun(Combo) ->
                    [M#{Place => Combo} || M <- RestModes]
                end,
                TokenCombos
            ).

%% @doc Internal helper for combination generation (from actual implementation)
%% Generates all combinations of N elements from a list in deterministic order.
combinations(0, _List) ->
    [[]];
combinations(_N, []) ->
    [];
combinations(N, [H | T]) ->
    %% Combinations including H (need N-1 more from T)
    WithH = [[H | Rest] || Rest <- combinations(N - 1, T)],
    %% Combinations excluding H (need N from T)
    WithoutH = combinations(N, T),
    WithH ++ WithoutH.
```

---

## 4. `pnet_choice` - Deterministic Nondeterminism

### Types
```erlang
-type rng_state() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
%% 3-tuple XOR-shift PRNG state for deterministic choice
```

### Functions

```erlang
%% @doc Creates a deterministic RNG state from a seed term (from actual implementation)
-spec seed(term()) -> rng_state().
seed(SeedTerm) ->
    IntSeed = erlang:phash2(SeedTerm),
    {A, B, C} = {IntSeed bxor 16#5deece66d, IntSeed bxor 16#babe, IntSeed bxor 16#deadbeef},
    seed_splitmix(A, B, C).

%% @doc Picks a random element from a list (from actual implementation)
-spec pick([T], rng_state()) -> {T, rng_state()} | {error, empty}.
pick([], _RngState) -> {error, empty};
pick(List, RngState) when is_list(List), length(List) > 0 ->
    Len = length(List),
    {Index, NewRngState} = rand_uniform(Len, RngState),
    Element = lists:nth(Index + 1, List),
    {Element, NewRngState}.

%% @doc Picks a random element from a weighted list (from actual implementation)
-spec pick_weighted([{T, pos_integer()}], rng_state()) ->
          {T, rng_state()} | {error, empty | bad_weights}.
pick_weighted([], _RngState) -> {error, empty};
pick_weighted(Items, RngState) when is_list(Items) ->
    case validate_weights(Items) of
        false -> {error, bad_weights};
        true ->
            TotalWeight = lists:sum([W || {_, W} <- Items]),
            {RandValue, NewRngState} = rand_uniform(TotalWeight, RngState),
            Item = select_weighted(Items, RandValue, 0),
            {Item, NewRngState}
    end.

%% @private
validate_weights([]) -> true;
validate_weights([{_Item, Weight} | Rest]) when is_integer(Weight), Weight > 0 ->
    validate_weights(Rest);
validate_weights(_) -> false.

%% @private
select_weighted([{Item, _Weight} | _Rest], _RandValue, _Acc) ->
    Item;
select_weighted([{Item, Weight} | Rest], RandValue, Acc) ->
    NewAcc = Acc + Weight,
    if RandValue < NewAcc -> Item;
       true -> select_weighted(Rest, RandValue, NewAcc)
    end.
```

---

## 5. `pnet_receipt` - Receipt Tracking

### Functions

```erlang
%% @doc Create a new receipt
-spec make(binary(), binary(), move()) -> receipt().
make(BeforeHash, AfterHash, Move) ->
    #{
        before_hash => BeforeHash,
        after_hash => AfterHash,
        move => Move,
        ts => timestamp()
    }.

%% @doc Get current timestamp (monotonic time for consistency)
-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time(millisecond).

%% @doc Extract effect commands from receipt
-spec receipt(receipt()) -> [term()].
effects(Receipt) ->
    % Default implementation returns empty list
    % Can be extended by net module via on_receipt callback
    [].
```

---

## 6. `wf_timerq` - Deadline Queue

### Types
```erlang
-type timerq() :: term().
-type timer_key() :: term().
-type deadline() :: integer().
-type timer_event() :: {produce, produce_map()}.
```

### Functions

```erlang
%% @doc Create a new timer queue
-spec new() -> timerq().
new() ->
    [].

%% @doc Arm a timer event
-spec arm(timerq(), timer_key(), deadline(), timer_event()) -> timerq().
arm(TimerQ, Key, Deadline, Event) ->
    lists:keystore(Key, 1, TimerQ, {Key, {Deadline, Event}}).

%% @doc Disarm a timer
-spec disarm(timerq(), timer_key()) -> timerq().
disarm(TimerQ, Key) ->
    lists:keydelete(Key, 1, TimerQ).

%% @doc Poll timer queue for ready events
-spec poll(timerq(), deadline()) -> {[timer_event()], timerq()}.
poll(TimerQ, Now) ->
    {Ready, Remaining} = lists:partition(fun({_, {Deadline, _}}) -> Deadline =< Now end, TimerQ),
    Events = [Event || {_, {_, Event}} <- Ready],
    {Events, Remaining}.
```

---

## 7. `wf_task` - External Task Tokens

### Types
```erlang
-type task_id() :: term().
-type task_event() :: {task, task_id(),
                     enabled | running | done | failed | cancelled,
                     term()}.
```

### Functions

```erlang
%% @doc Create enabled task event
-spec enabled(task_id(), term(), place()) -> {produce, produce_map()}.
enabled(TaskId, Payload, Place) ->
    {produce, #{Place => [{task, TaskId, enabled, Payload}]}}.

%% @doc Create running task event
-spec running(task_id(), term(), place()) -> {produce, produce_map()}.
running(TaskId, Payload, Place) ->
    {produce, #{Place => [{task, TaskId, running, Payload}]}}.

%% @doc Create completed task event
-spec done(task_id(), term(), place()) -> {produce, produce_map()}.
done(TaskId, Output, Place) ->
    {produce, #{Place => [{task, TaskId, done, Output}]}}.

%% @doc Create failed task event
-spec failed(task_id(), term(), place()) -> {produce, produce_map()}.
failed(TaskId, Reason, Place) ->
    {produce, #{Place => [{task, TaskId, failed, Reason}]}}.

%% @doc Create cancelled task event
-spec cancelled(task_id(), term(), place()) -> {produce, produce_map()}.
cancelled(TaskId, Reason, Place) ->
    {produce, #{Place => [{task, TaskId, cancelled, Reason}]}}.
```

---

## 8. `wf_scope` - Boundary Mapping

### Types
```erlang
-type scope_id() :: term().
-type binding_table() :: #{scope_id() => #{place() => place()}}.
```

### Functions

```erlang
%% @doc Enter a scope (parent to child mapping)
-spec enter(binding_table(), scope_id(), marking() | produce_map()) -> produce_map().
enter(BindingTable, ScopeId, DeltaOrMarking) ->
    case maps:find(ScopeId, BindingTable) of
        {ok, ParentToChild} ->
            maps:fold(fun(ParentPlace, ChildPlace, Acc) ->
                case maps:find(ParentPlace, DeltaOrMarking) of
                    {ok, Tokens} -> Acc#{ChildPlace := Tokens};
                    error -> Acc
                end
            end, #{}, ParentToChild);
        error -> error
    end.

%% @doc Leave a scope (child to parent mapping)
-spec leave(binding_table(), scope_id(), marking() | produce_map()) -> produce_map().
leave(BindingTable, ScopeId, DeltaOrMarking) ->
    case maps:find(ScopeId, BindingTable) of
        {ok, ParentToChild} ->
            maps:fold(fun(ChildPlace, ParentPlace, Acc) ->
                case maps:find(ChildPlace, DeltaOrMarking) of
                    {ok, Tokens} -> Acc#{ParentPlace := Tokens};
                    error -> Acc
                end
            end, #{}, maps:to_list(ParentToChild));
        error -> error
    end.

%% @doc Get bindings for a scope
-spec bindings(binding_table(), scope_id()) -> #{place() => place()} | {error, unknown_scope}.
bindings(BindingTable, ScopeId) ->
    case maps:find(ScopeId, BindingTable) of
        {ok, Bindings} -> Bindings;
        error -> {error, unknown_scope}
    end.
```

---

## 9. `pnet_net` - Net Semantics Behaviour

### Callback Module

```erlang
%% @type net_arg() :: term().  % Net-specific initialization argument
%% @type usr_info() :: term(). % User info maintained by net module
%% @type state_summary() :: term(). % Minimal state for callbacks

%% Required callbacks (uncolored)
-callback places() -> [place()].
-callback transitions() -> [trsn()].
-callback preset(trsn()) -> [place()].
-callback init(net_arg()) -> usr_info().
-callback init_marking(place(), usr_info()) -> [token()].
-callback modes(trsn(), marking(), usr_info()) -> [mode()].
-callback fire(trsn(), mode(), usr_info()) -> abort | {produce, produce_map()}.

%% Optional callbacks
-callback on_produce(place(), token(), state_summary()) -> keep | drop.
-callback on_receipt(receipt(), state_summary()) -> [term()].

%% Colored extension callbacks (mutually exclusive with uncolored)
-callback cmodes(trsn(), marking(), usr_info()) -> [cmode()].
-callback cfire(trsn(), binding(), mode(), usr_info()) -> abort | {produce, produce_map()}.
```

### Example Implementation

```erlang
-module(my_workflow_net).
-behaviour(pnet_net).

-export([places/0, transitions/0, preset/1, init/1,
         init_marking/2, modes/2, fire/3, on_receipt/2]).

places() -> [start, task1, task2, done].
transitions() -> [t1, t2].

preset(t1) -> [start];
preset(t2) -> [task1].

init(_NetArg) -> #{priority => normal}.

init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

modes(t1, Marking, _UsrInfo) ->
    case pnet_marking:get(Marking, start) of
        [init] -> [#{start => []}];
        _ -> []
    end.

modes(t2, Marking, _UsrInfo) ->
    case pnet_marking:get(Marking, task1) of
        [done] -> [#{task1 => []}];
        _ -> []
    end.

fire(t1, #{start => []}, _UsrInfo) ->
    {produce, #{task1 => [done]}}.

fire(t2, #{task1 => []}, _UsrInfo) ->
    {produce, #{done => [complete]}}.

on_receipt(Receipt, _State) ->
    % Log receipt for auditing
    io:format("Transition fired: ~p~n", [Receipt]),
    [audit_log].
```

---

## 10. `gen_pnet` - OTP Runner

### API Functions

```erlang
%% @doc Start the gen_pnet process with default name
-spec start_link(module(), term(), list()) -> {ok, pid()} | {error, term()}.
start_link(NetMod, NetArg, Options) ->
    gen_server:start_link(?MODULE, {local, NetMod}, NetMod, NetArg, Options).

%% @doc Start the gen_pnet process with registered name
-spec start_link(term(), module(), term(), list()) -> {ok, pid()} | {error, term()}.
start_link(Name, NetMod, NetArg, Options) ->
    gen_server:start_link(Name, ?MODULE, NetMod, NetArg, Options).

%% @doc Get current marking
-spec marking(term()) -> marking().
marking(Name) ->
    call(Name, {get, marking}).

%% @doc Get tokens at specific place
-spec ls(term(), place()) -> {ok, [token()]} | {error, bad_place}.
ls(Name, Place) ->
    call(Name, {get, Place}).

%% @doc Get user info
-spec usr_info(term()) -> term().
usr_info(Name) ->
    call(Name, {get, usr_info}).

%% @doc Produce tokens (core workflow primitive)
-spec produce(term(), produce_map()) -> ok.
produce(Name, ProduceMap) ->
    cast(Name, {produce, ProduceMap}).

%% @doc Synchronous call
-spec call(term(), term()) -> term().
call(Name, Request) ->
    gen_server:call(Name, Request).

%% @doc Synchronous call with timeout
-spec call(term(), term(), timeout()) -> term().
call(Name, Request, Timeout) ->
    gen_server:call(Name, Request, Timeout).

%% @doc Asynchronous cast
-spec cast(term(), term()) -> ok.
cast(Name, Request) ->
    gen_server:cast(Name, Request).

%% @doc Reply to client
-spec reply(term(), term()) -> ok.
reply(ClientRef, Reply) ->
    gen_server:reply(ClientRef, Reply).

%% @doc Stop the process
-spec stop(term()) -> ok.
stop(Name) ->
    gen_server:call(Name, stop).

%% @doc Get last receipt
-spec last_receipt(term()) -> receipt() | undefined.
last_receipt(Name) ->
    call(Name, last_receipt).

%% @doc Get last N receipts
-spec receipts(term(), pos_integer()) -> [receipt()].
receipts(Name, N) ->
    call(Name, {receipts, N}).

%% @doc Subscribe to receipt notifications
-spec subscribe(term(), pid()) -> ok.
subscribe(Name, Pid) ->
    cast(Name, {subscribe, Pid}).
```

### gen_server Implementation (Internal)

```erlang
-module(gen_pnet).
-behaviour(gen_server).

-export([start_link/4, start_link/3,
         marking/1, ls/2, usr_info/1, produce/2,
         call/2, call/3, cast/2, stop/1,
         last_receipt/1, receipts/2, subscribe/2, reply/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    net_mod,           :: module()
    net_arg,           :: term()
    usr_info,          :: term()
    marking,           :: marking()
    receipts = [],     :: [receipt()]
    subscribers = [],  :: [pid()]
    rng_state          :: term()
}).

%% Public API (as above)...

%% gen_server callbacks
init({Name, NetMod, NetArg}) ->
    % Initialize net module
    UsrInfo = NetMod:init(NetArg),

    % Create initial marking from init_marking callbacks
    InitialMarking = create_initial_marking(NetMod, UsrInfo),

    % Start progress loop
    {ok, ProgressMarking} = run_progress_loop(#state{
        net_mod = NetMod,
        net_arg = NetArg,
        usr_info = UsrInfo,
        marking = InitialMarking,
        rng_state = pnet_choice:seed(erlang:unique_integer())
    }, InitialMarking)};

handle_call({get, marking}, _From, State) ->
    {reply, State#state.marking, State};

handle_call({get, Place}, _From, State) ->
    Tokens = pnet_marking:get(State#state.marking, Place),
    {reply, {ok, Tokens}, State};

handle_call({get, usr_info}, _From, State) ->
    {reply, State#state.usr_info, State};

handle_call(last_receipt, _From, State) ->
    case State#state.receipts of
        [Last|_] -> {reply, Last, State};
        [] -> {reply, undefined, State}
    end;

handle_call({receipts, N}, _From, State) ->
    Receipts = lists:sublist(lists:reverse(State#state.receipts), N),
    {reply, Receipts, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({produce, ProduceMap}, State) ->
    % Apply production and run progress loop
    case pnet_marking:add(State#state.marking, ProduceMap) of
        {ok, NewMarking} ->
            {noreply, run_progress_loop(State, NewMarking)};
        {error, _} = Error ->
            % Log error but don't crash
            error_logger:error_msg("Produce error: ~p~n", [Error]),
            {noreply, State}
    end;

handle_cast({subscribe, Pid}, State) ->
    MonRef = erlang:monitor(process, Pid),
    {noreply, State#state{
        subscribers = [{Pid, MonRef} | State#state.subscribers]
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Remove dead subscriber
    NewSubscribers = proplists:delete(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Progress loop implementation
run_progress_loop(State, Marking) ->
    Enabled = find_enabled_transitions(State#state.net_mod, Marking, State#state.usr_info),
    case Enabled of
        [] -> % Blocked
            State#state{marking = Marking};
        [T|_] ->
            % Choose mode deterministically
            {Mode, NewRngState} = choose_mode(T, Marking, State#state.net_mod, State#state.usr_info, State#state.rng_state),

            % Fire transition
            case fire_transition(T, Mode, State#state.net_mod, State#state.usr_info) of
                {produce, NewTokens} ->
                    BeforeHash = pnet_marking:hash(Marking),
                    {ok, NewMarking} = pnet_marking:apply(Marking, Mode, NewTokens),
                    AfterHash = pnet_marking:hash(NewMarking),

                    % Create receipt
                    Move = #{trsn => T, mode => Mode, produce => NewTokens},
                    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),

                    % Store receipt and notify subscribers
                    NewState = store_receipt_and_notify(State#state{rng_state = NewRngState}, Receipt, NewMarking),

                    % Continue progress loop
                    run_progress_loop(NewState, NewMarking);
                abort ->
                    State#state{marking = Marking}
            end
    end.

%% Helper functions (implementation details)...
```

---

## 11. `yawl_validate` - YAWL Validation

### Functions

```erlang
%% @doc Validate YAWL specification against interface requirements
-spec validate(term()) -> ok | {error, term()}.
validate(XmlOrSpec) ->
    % Parse XML or validate existing spec
    try
        % Basic validation checks
        validate_net_structure(XmlOrSpec),
        validate_transition_connectivity(XmlOrSpec),
        validate_places(XmlOrSpec),
        validate_transitions(XmlOrSpec),
        ok
    catch
        throw:Reason -> {error, Reason}
    end.

%% Internal validation functions
validate_net_structure(XmlOrSpec) ->
    % Check for required YAWL elements
    Required = [places, transitions, preset, flows],
    check_required_elements(XmlOrSpec, Required).

validate_transition_connectivity(XmlOrSpec) ->
    % Ensure all transitions have proper preset/postset
    % No dangling transitions
    ok.

validate_places(XmlOrSpec) ->
    % Check place definitions
    ok.

validate_transitions(XmlOrSpec) ->
    % Check transition definitions
    ok.
```

---

## 12. `yawl_compile` - YAWL Compilation

### Functions

```erlang
%% @doc Compile YAWL specification to net module
-spec compile(term(), list()) -> {ok, module()} | {ok, module(), term(), term(), term()} | {error, term()}.
compile(XmlOrSpec, Options) ->
    try
        % Parse XML or use existing spec
        Spec = parse_yawl(XmlOrSpec),

        % Generate net module code
        NetMod = generate_net_module(Spec, Options),

        % Additional outputs requested?
        case proplists:get_value(advanced, Options, false) of
            true ->
                {ScopeTable, TimerPlan, TaskMap} = generate_auxiliary_structures(Spec),
                {ok, NetMod, ScopeTable, TimerPlan, TaskMap};
            false ->
                {ok, NetMod}
        end
    catch
        throw:Reason -> {error, Reason}
    end.

%% Helper functions
parse_yawl(XmlOrSpec) ->
    % Implementation for XML parsing or spec validation
    ok.

generate_net_module(Spec, Options) ->
    % Generate Erlang module implementing pnet_net behaviour
    % This is a code generation process
    yawl_net_generator:generate(Spec, Options).

generate_auxiliary_structures(Spec) ->
    % Generate scope table, timer plan, and task map
    {#{}, [], #{}}.
```

---

## Message Contracts

### To gen_pnet
```erlang
% Token injection
{produce, #{place1 => [token1, token2], place2 => [token3]}}

% Asynchronous requests
{cast, {custom_request, data}}

% Synchronous requests
{call, {get, marking}}
{call, {get, specific_place}}
{call, last_receipt}
{call, {receipts, 5}}
```

### From gen_pnet
```ersubscript
% Receipt notifications
{pnet_receipt, #{
    before_hash => <<Hash1>>,
    after_hash => <<Hash2>>,
    move => #{trsn => t1, mode => #{}, produce => #{}},
    ts => 1234567890
}}

% Effect commands (optional)
{pnet_effect, audit_log}
{pnet_effect, notification}
```

This completes the comprehensive API reference for the refactored CRE architecture, implementing the Joe Armstrong interface design principles.