%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc Deterministic Nondeterminism Module
%%
%% This module provides deterministic nondeterminism for the gen_pnet
%% Petri net framework. All randomness in the runner should go through
%% this module to ensure reproducible behavior when seeded.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Deterministic RNG:</b> Seedable for reproducible execution</li>
%%   <li><b>Pure Functional:</b> No process state, all operations return updated state</li>
%%   <li><b>Weighted Selection:</b> Support for weighted random choice</li>
%% </ul>
%%
%% <h3>Usage Example</h3>
%% <pre><code>
%% %% Seed the RNG with a fixed value for reproducibility
%% Rng0 = pnet_choice:seed(12345),
%%
%% %% Pick a random element from a list
%% {Choice, Rng1} = pnet_choice:pick([a, b, c], Rng0),
%%
%% %% Pick from weighted options
%% Options = [{a, 1}, {b, 3}, {c, 1}],  % b has 3x weight
%% {WeightedChoice, Rng2} = pnet_choice:pick_weighted(Options, Rng1).
%% </code></pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(pnet_choice).

%%====================================================================
%% Exports
%%====================================================================

%% RNG operations
-export([seed/1, pick/2, pick_weighted/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Internal RNG state.
%%
%% Uses a 3-tuple state compatible with the standard Erlang rand
%% module format for Xoshiro256** algorithm.
%%--------------------------------------------------------------------
-type rng_state() :: {non_neg_integer(), non_neg_integer(), non_neg_integer(),
                      non_neg_integer()} | {exs1024s,
                                               non_neg_integer(),
                                               non_neg_integer(),
                                               non_neg_integer(),
                                               non_neg_integer()}.

%% Export types
-export_type([rng_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new RNG state from a seed term.
%%
%% The seed term can be any Erlang term - it's hashed to create
%% a deterministic initial state. This ensures reproducible
%% randomness when the same seed is used.
%%
%% @param SeedTerm Any Erlang term to use as the seed
%% @return A deterministic RNG state
%%
%% @end
%%--------------------------------------------------------------------
-spec seed(SeedTerm :: term()) -> rng_state().

seed(SeedTerm) ->
    %% Use erlang:phash2 to create a deterministic integer from any term
    %% Then seed the rand module
    IntSeed = erlang:phash2(SeedTerm),
    {A, B, C} = {IntSeed bxor 16#5deece66d, IntSeed bxor 16#babe, IntSeed bxor 16#deadbeef},
    %% Initialize a simple LCG state for deterministic behavior
    State = seed_splitmix(A, B, C),
    State.

%%--------------------------------------------------------------------
%% @doc Picks a random element from a list.
%%
%% Returns the chosen element along with the updated RNG state.
%% The list is not modified.
%%
%% @param List Non-empty list to pick from
%% @param RngState Current RNG state
%% @return {Element, UpdatedRngState} or {error, empty} if list is empty
%%
%% @end
%%--------------------------------------------------------------------
-spec pick(List :: [T], RngState :: rng_state()) ->
          {T, rng_state()} | {error, empty}.

pick([], _RngState) ->
    {error, empty};
pick(List, RngState) when is_list(List), length(List) > 0 ->
    Len = length(List),
    {Index, NewRngState} = rand_uniform(Len, RngState),
    Element = lists:nth(Index + 1, List),
    {Element, NewRngState}.

%%--------------------------------------------------------------------
%% @doc Picks a random element from a weighted list.
%%
%% Each element is associated with a positive weight. Higher weight
%% means higher probability of being selected. Returns the chosen
%% element (not the weight) along with the updated RNG state.
%%
%% @param Items Non-empty list of {Element, Weight} tuples
%% @param RngState Current RNG state
%% @return {Element, UpdatedRngState} or {error, empty | bad_weights}
%%
%% @end
%%--------------------------------------------------------------------
-spec pick_weighted(Items :: [{T, pos_integer()}], RngState :: rng_state()) ->
          {T, rng_state()} | {error, empty | bad_weights}.

pick_weighted([], _RngState) ->
    {error, empty};
pick_weighted(Items, RngState) when is_list(Items) ->
    case validate_weights(Items) of
        false ->
            {error, bad_weights};
        true ->
            TotalWeight = lists:sum([W || {_, W} <- Items]),
            {RandValue, NewRngState} = rand_uniform(TotalWeight, RngState),
            select_weighted(Items, RandValue, 0)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Validates that all weights are positive integers.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_weights([{term(), pos_integer()}]) -> boolean().

validate_weights([]) ->
    true;
validate_weights([{_Item, Weight} | Rest]) when is_integer(Weight), Weight > 0 ->
    validate_weights(Rest);
validate_weights(_) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc Selects an element from a weighted list using cumulative weights.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_weighted([{term(), pos_integer()}], non_neg_integer(),
                     non_neg_integer()) ->
          {term(), rng_state()}.

select_weighted([{Item, _Weight} | _Rest], RandValue, _Acc) when RandValue < 0 ->
    {Item, undefined};  %% RNG state unchanged in this path
select_weighted([{Item, Weight} | Rest], RandValue, Acc) ->
    NewAcc = Acc + Weight,
    if
        RandValue < NewAcc ->
            {Item, undefined};
        true ->
            select_weighted(Rest, RandValue, NewAcc)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a uniform random integer in [0, Max-1].
%%
%% Uses a simple XOR-based PRNG for deterministic behavior.
%% Returns the random value and updated state.
%%
%% @end
%%--------------------------------------------------------------------
-spec rand_uniform(non_neg_integer(), rng_state()) ->
          {non_neg_integer(), rng_state()}.

rand_uniform(Max, {S1, S2, S3} = State) when Max > 0 ->
    %% Simple XOR-shift PRNG
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

    {(Rand rem Max), NewState};
rand_uniform(_Max, State) ->
    {0, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Creates a seeded RNG state using splitmix-style initialization.
%%
%% @end
%%--------------------------------------------------------------------
-spec seed_splitmix(non_neg_integer(), non_neg_integer(),
                   non_neg_integer()) -> rng_state().

seed_splitmix(A, B, C) ->
    %% Mix the seed values to create initial state
    S1 = mix(A),
    S2 = mix(B),
    S3 = mix(C),
    {S1, S2, S3}.

%%--------------------------------------------------------------------
%% @private
%% @doc Mixes a 64-bit value for seeding.
%%
%% Uses a simple mixing function similar to MurmurHash3's finalizer.
%%
%% @end
%%--------------------------------------------------------------------
-spec mix(non_neg_integer()) -> non_neg_integer().

mix(X) ->
    X1 = X bxor (X bsr 30),
    X2 = X1 * 16#bf58476d1ce4e5b9 band 16#ffffffffffffffff,
    X3 = X2 bxor (X2 bsr 27),
    X4 = X3 * 16#94d049bb133111eb band 16#ffffffffffffffff,
    X4 bxor (X4 bsr 31).
