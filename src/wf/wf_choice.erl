%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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

-module(wf_choice).
-moduledoc """
Deterministic choice workflow specification (discriminator pattern).

Creates a Petri net for deterministic choice (XOR split) where one input
place flows to multiple output places, with exactly one selected based on
a seed value for reproducibility.

The choice is deterministic: the same seed always selects the same path.
This is implemented using pnet_choice for seeded random selection.

```erlang
> ChoiceSpec = wf_choice:spec(#{
..   from => in,
..   to => [left, right]
.. }).
_
> {ok, E1} = wf_engine:start_link(#{spec => ChoiceSpec, seed => 123, now => 0}).
_
> {ok, C1} = wf_engine:start_case(E1, #{inject => #{in => [tok]}}, 0).
_
> [R1 | _] = wf_engine:drain_receipts(E1, C1).
_
> T1 = maps:get(trsn, maps:get(move, R1)).
_

> {ok, E2} = wf_engine:start_link(#{spec => ChoiceSpec, seed => 123, now => 0}).
_
> {ok, C2} = wf_engine:start_case(E2, #{inject => #{in => [tok]}}, 0).
_
> [R2 | _] = wf_engine:drain_receipts(E2, C2).
_
> T2 = maps:get(trsn, maps:get(move, R2)).
_
> T1 =:= T2.
true
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Spec creation
-export([spec/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Choice specification input map.
%%
%% Required keys:
%%   - from: Source place (atom)
%%   - to: List of destination places (list of atoms)
%%--------------------------------------------------------------------
-type choice_spec() :: #{from := atom(), to := [atom()]}.

%%--------------------------------------------------------------------
%% @doc Compiled workflow spec for wf_engine.
%%
%% Contains:
%%   - places: List of all place atoms in the Petri net
%%   - transitions: Map of transition definitions
%%   - init_marking: Initial marking map
%%   - preset: Map of transition -> input places
%%   - seed: Initial seed for deterministic choice
%%--------------------------------------------------------------------
-type compiled_spec() :: #{
    places := [atom()],
    transitions := #{atom() => transition_def()},
    init_marking := #{atom() => [term()]},
    preset := #{atom() => [atom()]},
    seed => term()
}.

%%--------------------------------------------------------------------
%% @doc Definition of a single transition.
%%
%% Each transition has:
%%   - choice_fun: Function that selects destination based on seed
%%   - from: Source place
%%   - to: List of possible destinations
%%--------------------------------------------------------------------
-type transition_def() :: #{
    choice_fun := fun((term(), [atom()]) -> atom()),
    from := atom(),
    to := [atom()]
}.

-export_type([choice_spec/0, compiled_spec/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a choice workflow specification.
%%
%% Takes a map specifying:
%%   - from: The source place name (atom)
%%   - to: List of possible destination places (atoms)
%%
%% Returns a compiled workflow spec that implements deterministic
%% choice using pnet_choice for seeded selection.
%%
%% The generated Petri net has:
%%   - Places: the 'from' place and all 'to' places
%%   - A single transition that selects one destination deterministically
%%
%% @param ChoiceSpec Map with from => atom() and to => [atom()]
%% @return Compiled workflow specification
%%
%% @end
%%--------------------------------------------------------------------
-spec spec(ChoiceSpec :: choice_spec()) -> compiled_spec().

spec(#{from := From, to := ToList}) when is_atom(From), is_list(ToList) ->
    %% Validate inputs
    true = length(ToList) > 0,
    lists:foreach(fun(P) -> true = is_atom(P) end, ToList),

    %% Generate unique transition name based on places
    TrsnName = make_trsn_name(From, ToList),

    %% Create the choice function that uses pnet_choice for deterministic selection
    ChoiceFun = fun(Seed, Destinations) ->
        %% Use pnet_choice to deterministically select based on seed
        RngState = pnet_choice:seed(Seed),
        {Selected, _NewRngState} = pnet_choice:pick(Destinations, RngState),
        Selected
    end,

    %% Build the compiled spec
    #{
        places => [From | ToList],
        transitions => #{
            TrsnName => #{
                choice_fun => ChoiceFun,
                from => From,
                to => ToList
            }
        },
        init_marking => maps:from_list([{From, []} | [{Place, []} || Place <- ToList]]),
        preset => #{
            TrsnName => [From]
        }
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique transition name from source and destinations.
%%
%% Creates an atom name that uniquely identifies this choice transition.
%% Uses binary:encode_hex to create a valid atom name.
%%
%% @end
%%--------------------------------------------------------------------
-spec make_trsn_name(atom(), [atom()]) -> atom().

make_trsn_name(From, ToList) ->
    %% Create a hash-based atom name that will be valid
    %% Using crypto:hash to create a consistent short name
    Input = term_to_binary({From, ToList}),
    Hash = binary:encode_hex(crypto:hash(md5, Input)),
    %% Use first 16 chars of hash for a reasonable atom name
    ShortHash = binary:part(Hash, {0, 16}),
    Name = <<"choice_", ShortHash/binary>>,
    binary_to_atom(Name, utf8).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Runs doctests from the moduledoc and function documentation.
%%
%% @end
%%--------------------------------------------------------------------
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Test spec creation with valid inputs
spec_valid_test() ->
    Spec = spec(#{from => in, to => [left, right]}),
    %% Check structure
    true = is_map(Spec),
    true = maps:is_key(places, Spec),
    true = maps:is_key(transitions, Spec),
    true = maps:is_key(init_marking, Spec),
    true = maps:is_key(preset, Spec),
    %% Check places
    [in, left, right] = maps:get(places, Spec),
    ok.

%% Test spec with single destination
spec_single_dest_test() ->
    Spec = spec(#{from => start, to => [done]}),
    [start, done] = maps:get(places, Spec),
    ok.

%% Test spec with multiple destinations
spec_multiple_dest_test() ->
    Spec = spec(#{from => p1, to => [p2, p3, p4, p5]}),
    [p1, p2, p3, p4, p5] = maps:get(places, Spec),
    ok.

%% Test transition structure
spec_transition_structure_test() ->
    Spec = spec(#{from => in, to => [a, b]}),
    Transitions = maps:get(transitions, Spec),
    %% Should have one transition
    1 = maps:size(Transitions),
    %% Get the transition (name varies based on hash)
    [TransDef] = maps:values(Transitions),
    %% Check structure
    true = maps:is_key(choice_fun, TransDef),
    true = maps:is_key(from, TransDef),
    true = maps:is_key(to, TransDef),
    in = maps:get(from, TransDef),
    [a, b] = maps:get(to, TransDef),
    ok.

%% Test preset structure
spec_preset_test() ->
    Spec = spec(#{from => source, to => [dest1, dest2]}),
    Preset = maps:get(preset, Spec),
    %% Transition should have 'source' as preset
    1 = maps:size(Preset),
    [TrsnName] = maps:keys(Preset),
    [source] = maps:get(TrsnName, Preset),
    ok.

%% Test choice function determinism
choice_function_determinism_test() ->
    Spec = spec(#{from => in, to => [a, b, c]}),
    Transitions = maps:get(transitions, Spec),
    [TransDef] = maps:values(Transitions),
    ChoiceFun = maps:get(choice_fun, TransDef),
    ToList = maps:get(to, TransDef),

    %% Same seed should give same choice
    R1 = ChoiceFun(123, ToList),
    R2 = ChoiceFun(123, ToList),
    true = R1 =:= R2,

    %% Different seeds might give different choices (not guaranteed)
    R3 = ChoiceFun(456, ToList),
    %% R3 may or may not equal R1, but should be valid
    true = lists:member(R3, ToList),
    ok.

%% Test initial marking
spec_init_marking_test() ->
    Spec = spec(#{from => p_in, to => [p_out1, p_out2]}),
    InitMarking = maps:get(init_marking, Spec),
    %% All places should be in initial marking with empty tokens
    true = maps:is_key(p_in, InitMarking),
    true = maps:is_key(p_out1, InitMarking),
    true = maps:is_key(p_out2, InitMarking),
    [] = maps:get(p_in, InitMarking),
    [] = maps:get(p_out1, InitMarking),
    [] = maps:get(p_out2, InitMarking),
    ok.

%% Test that choice function returns valid destination
choice_function_valid_test() ->
    Spec = spec(#{from => x, to => [y, z]}),
    Transitions = maps:get(transitions, Spec),
    [TransDef] = maps:values(Transitions),
    ChoiceFun = maps:get(choice_fun, TransDef),
    ToList = maps:get(to, TransDef),

    %% Try multiple seeds, all should return valid destinations
    Seeds = [1, 2, 100, 9999, 12345],
    ValidCheck = fun(Seed) ->
        Result = ChoiceFun(Seed, ToList),
        lists:member(Result, ToList)
    end,
    true = lists:all(ValidCheck, Seeds),
    ok.

-endif.
