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
%% @doc Petri Net Marking Algebra Module
%%
%% This module implements multiset marking algebra for the gen_pnet
%% Petri net framework in CRE. Markings represent the state of a Petri
%% net by mapping places to their token multisets.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Multiset Semantics:</b> Token multiplicity matters in operations</li>
%%   <li><b>Total Functions:</b> All operations return {error, ...} instead of crashing</li>
%%   <li><b>Immutable Updates:</b> All operations return new markings</li>
%%   <li><b>Hashing Support:</b> Consistent hashing for state comparison</li>
%% </ul>
%%
%% <h3>Usage Example</h3>
%% <pre><code>
%% %% Create empty marking with places
%% M0 = pnet_marking:new([p1, p2, p3]),
%%
%% %% Add tokens via produce map
%% M1 = pnet_marking:add(M0, #{p1 => [a, b], p2 => [c]}),
%%
%% %% Get tokens at a place
%% {ok, Tokens} = pnet_marking:get(M1, p1),
%%
%% %% Take tokens via consume map (multiset operation)
%% case pnet_marking:take(M1, #{p1 => [a]}) of
%%     {ok, M2} -> have_token;
%%     {error, insufficient} -> need_more_tokens
%% end,
%%
%% %% Atomic take + add (transition firing)
%% case pnet_marking:apply(M1, #{p1 => [a]}, #{p2 => [d]}) of
%%     {ok, M3} -> transition_fired;
%%     {error, Reason} -> handle_error(Reason)
%% end.
%% </code></pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(pnet_marking).

%%====================================================================
%% Exports
%%====================================================================

%% Creation and basic operations
-export([new/1, get/2, set/3]).

%% Marking algebra operations
-export([add/2, take/2, apply/3]).

%% Inspection and utilities
-export([snapshot/1, hash/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the Petri net.
%%
%% Places are locations where tokens reside. Represented as atoms
%% for efficient pattern matching.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A token in the Petri net.
%%
%% Tokens can be any Erlang term, allowing flexible data flow.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%
%% Each place atom maps to a list of tokens currently in that place.
%% Empty lists represent places with no tokens.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A consume map specifies tokens to be removed.
%%
%% Maps places to the specific list of tokens that will be consumed.
%%--------------------------------------------------------------------
-type consume_map() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A produce map specifies tokens to be added.
%%
%% Maps places to the list of tokens that will be produced.
%%--------------------------------------------------------------------
-type produce_map() :: #{place() => [token()]}.

%% Export types
-export_type([marking/0, place/0, token/0, consume_map/0, produce_map/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new empty marking with the given places.
%%
%% All places are initialized with empty token lists.
%%
%% @param Places List of place atoms to include in the marking
%% @return A new marking with all places initialized to empty
%%
%% @end
%%--------------------------------------------------------------------
-spec new(Places :: [place()]) -> marking().

new(Places) when is_list(Places) ->
    maps:from_list([{P, []} || P <- Places]).

%%--------------------------------------------------------------------
%% @doc Gets the tokens at a specific place.
%%
%% @param Marking The marking to query
%% @param Place The place to get tokens from
%% @return {ok, Tokens} if place exists, {error, bad_place} otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec get(Marking :: marking(), Place :: place()) ->
          {ok, [token()]} | {error, bad_place}.

get(Marking, Place) when is_map(Marking), is_atom(Place) ->
    case maps:find(Place, Marking) of
        {ok, Tokens} -> {ok, Tokens};
        error -> {error, bad_place}
    end;
get(_Marking, _Place) ->
    {error, bad_place}.

%%--------------------------------------------------------------------
%% @doc Sets the tokens at a specific place.
%%
%% Replaces the current token list at the given place with a new list.
%% The place must exist in the marking.
%%
%% @param Marking The marking to modify
%% @param Place The place to set tokens at
%% @param Tokens The new token list for the place
%% @return Updated marking, or {error, bad_place} if place doesn't exist
%%
%% @end
%%--------------------------------------------------------------------
-spec set(Marking :: marking(), Place :: place(), Tokens :: [token()]) ->
          marking() | {error, bad_place}.

set(Marking, Place, Tokens)
  when is_map(Marking), is_atom(Place), is_list(Tokens) ->
    case maps:is_key(Place, Marking) of
        true -> Marking#{Place => Tokens};
        false -> {error, bad_place}
    end;
set(_Marking, _Place, _Tokens) ->
    {error, bad_place}.

%%--------------------------------------------------------------------
%% @doc Adds tokens to the marking via a produce map.
%%
%% Tokens are appended to existing tokens at each place.
%% All places in the produce map must exist in the marking.
%%
%% @param Marking The marking to add tokens to
%% @param ProduceMap Map of places to token lists to add
%% @return Updated marking, or {error, bad_place} if any place doesn't exist
%%
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc Takes tokens from the marking via a consume map.
%%
%% This is a multiset operation - the exact tokens specified must be
%% present with sufficient multiplicity. Tokens are removed from the
%% front of each place's token list.
%%
%% @param Marking The marking to take tokens from
%% @param ConsumeMap Map of places to token lists to remove
%% @return {ok, UpdatedMarking} on success, {error, bad_place | insufficient} on failure
%%
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc Applies a consume map and produce map atomically.
%%
%% This is the primary operation for transition firing. Tokens are
%% first consumed via the consume map, then tokens are added via the
%% produce map. If consumption fails, the marking is unchanged.
%%
%% @param Marking The marking to apply the operation to
%% @param ConsumeMap Map of places to token lists to remove
%% @param ProduceMap Map of places to token lists to add
%% @return {ok, UpdatedMarking} on success, {error, bad_place | insufficient} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec 'apply'(Marking :: marking(),
            ConsumeMap :: consume_map(),
            ProduceMap :: produce_map()) ->
          {ok, marking()} | {error, bad_place | insufficient}.

'apply'(Marking, ConsumeMap, ProduceMap)
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
'apply'(_Marking, _ConsumeMap, _ProduceMap) ->
    {error, bad_place}.

%%--------------------------------------------------------------------
%% @doc Creates a deep copy (snapshot) of the marking.
%%
%% Returns an identical copy that can be modified without affecting
%% the original. Since Erlang data is immutable, this returns the
%% same marking but provides a clear semantic for snapshotting.
%%
%% @param Marking The marking to snapshot
%% @return An identical copy of the marking
%%
%% @end
%%--------------------------------------------------------------------
-spec snapshot(Marking :: marking()) -> marking().

snapshot(Marking) when is_map(Marking) ->
    %% Erlang maps are immutable, so the marking itself is already
    %% a snapshot. This function provides semantic clarity.
    Marking.

%%--------------------------------------------------------------------
%% @doc Computes a consistent hash of the marking.
%%
%% Uses SHA-256 to hash the marking's term representation.
%% Useful for state comparison, caching keys, and receipt generation.
%%
%% @param Marking The marking to hash
%% @return Binary hash of the marking
%%
%% @end
%%--------------------------------------------------------------------
-spec hash(Marking :: marking()) -> binary().

hash(Marking) when is_map(Marking) ->
    crypto:hash(sha256, term_to_binary(Marking)).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Consumes tokens from a list, respecting multiset semantics.
%%
%% Removes each token in TokensToTake from AvailableTokens,
%% checking that each token exists with sufficient multiplicity.
%% Returns the remaining tokens or an error if consumption fails.
%%
%% @end
%%--------------------------------------------------------------------
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
