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

-module(wf_ipc).
-moduledoc """
Interprocess communication workflow specification.

Creates a Petri net that waits for channel signals. The workflow blocks
until a matching message is published to the specified channel.

This is a pure module - it creates specs for the engine to use.

## Examples

```erlang
> IpcSpec = wf_ipc:spec(#{
..   wait_channel => payment,
..   wait_msg => done
.. }).
_

> {ok, EngI} = wf_engine:start_link(#{spec => IpcSpec, seed => 1, now => 0}).
_

> {ok, CI} = wf_engine:start_case(EngI, #{data => #{}}, 0).
_

> wf_engine:enabled(EngI, CI).
[]

> ok = wf_engine:publish(EngI, payment, done, 1).
ok

> length(wf_engine:enabled(EngI, CI)) > 0.
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
%% @doc IPC specification input map.
%%
%% Required keys:
%%   - wait_channel: Channel to wait for (atom)
%%   - wait_msg: Message to wait for (term)
%%
%% Optional keys:
%%   - timeout_place: Place to produce token on timeout (atom)
%%   - timeout_ms: Timeout in milliseconds (integer)
%%--------------------------------------------------------------------
-type ipc_spec() :: #{
    wait_channel := atom(),
    wait_msg := term(),
    timeout_place => atom(),
    timeout_ms => non_neg_integer()
}.

%%--------------------------------------------------------------------
%% @doc Compiled workflow spec for wf_engine.
%%
%% Contains:
%%   - places: List of all place atoms in the Petri net
%%   - transitions: Map of transition definitions
%%   - init_marking: Initial marking map
%%   - preset: Map of transition -> input places
%%   - wait_channel: Channel this spec waits on
%%   - wait_msg: Message this spec waits for
%%--------------------------------------------------------------------
-type compiled_spec() :: #{
    places := [atom()],
    transitions := #{atom() => transition_def()},
    init_marking := #{atom() => [term()]},
    preset := #{atom() => [atom()]},
    wait_channel := atom(),
    wait_msg := term()
}.

%%--------------------------------------------------------------------
%% @doc Definition of a single transition.
%%
%% Each transition has:
%%   - channel: The channel to wait for
%%   - msg: The message to wait for
%%   - produce: The produce map when signal arrives
%%--------------------------------------------------------------------
-type transition_def() :: #{
    channel := atom(),
    msg := term(),
    produce := #{atom() => [term()]}
}.

-export_type([ipc_spec/0, compiled_spec/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an IPC wait workflow specification.
%%
%% Takes a map specifying:
%%   - wait_channel: The channel name (atom) to wait on
%%   - wait_msg: The message (term) to wait for
%%
%% Returns a compiled workflow spec that implements a wait for
%% an interprocess signal. The workflow has:
%%   - A waiting place that holds the wait token
%%   - A transition that becomes enabled when matching signal arrives
%%   - A completion place where the signal token is deposited
%%
%% The engine's publish/4 function will inject the signal token,
%% enabling the waiting transition.
%%
%% @param IpcSpec Map with wait_channel => atom() and wait_msg => term()
%% @return Compiled workflow specification
%%
%% @end
%%--------------------------------------------------------------------
-spec spec(IpcSpec :: ipc_spec()) -> compiled_spec().

spec(#{wait_channel := Channel, wait_msg := Msg}) when is_atom(Channel) ->
    %% Generate unique place and transition names
    WaitPlace = make_place_name(wait, Channel),
    SignalPlace = make_place_name(signal, Channel),
    DonePlace = make_place_name(done, Channel),
    TrsnName = make_trsn_name(Channel, Msg),

    %% Create the wait token
    WaitToken = {wait, Channel, Msg},

    %% Build the compiled spec
    #{
        places => [WaitPlace, SignalPlace, DonePlace],
        transitions => #{
            TrsnName => #{
                channel => Channel,
                msg => Msg,
                produce => #{
                    DonePlace => [{signal_received, Channel, Msg}]
                }
            }
        },
        init_marking => #{
            WaitPlace => [WaitToken],
            SignalPlace => [],
            DonePlace => []
        },
        preset => #{
            TrsnName => [WaitPlace, SignalPlace]
        },
        wait_channel => Channel,
        wait_msg => Msg
    }.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique place name.
%%
%% @end
%%--------------------------------------------------------------------
-spec make_place_name(Type :: atom(), Channel :: atom()) -> atom().

make_place_name(Type, Channel) ->
    Name = atom_to_list(Type) ++ "_" ++ atom_to_list(Channel),
    to_existing_atom(Name).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique transition name from channel and message.
%%
%% @end
%%--------------------------------------------------------------------
-spec make_trsn_name(Channel :: atom(), Msg :: term()) -> atom().

make_trsn_name(Channel, Msg) ->
    %% Create a readable atom name for the transition
    MsgStr = case Msg of
        A when is_atom(A) -> atom_to_list(A);
        B when is_binary(B) -> binary_to_list(B);
        I when is_integer(I) -> integer_to_list(I);
        _ -> "msg"
    end,
    Name = "ipc_wait_" ++ atom_to_list(Channel) ++ "_" ++ MsgStr,
    to_existing_atom(Name).

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a string to an existing atom, creating if needed.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_existing_atom(string()) -> atom().

to_existing_atom(Str) ->
    try
        erlang:list_to_existing_atom(Str)
    catch
        error:_ ->
            list_to_atom(Str)
    end.

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
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%% Test spec creation with valid inputs
spec_valid_test() ->
    Spec = spec(#{wait_channel => payment, wait_msg => done}),
    %% Check structure
    true = is_map(Spec),
    true = maps:is_key(places, Spec),
    true = maps:is_key(transitions, Spec),
    true = maps:is_key(init_marking, Spec),
    true = maps:is_key(preset, Spec),
    true = maps:is_key(wait_channel, Spec),
    true = maps:is_key(wait_msg, Spec),
    %% Check values
    payment = maps:get(wait_channel, Spec),
    done = maps:get(wait_msg, Spec),
    ok.

%% Test places are generated correctly
spec_places_test() ->
    Spec = spec(#{wait_channel => payment, wait_msg => done}),
    Places = maps:get(places, Spec),
    %% Should have wait, signal, and done places
    3 = length(Places),
    true = lists:member(wait_payment, Places),
    true = lists:member(signal_payment, Places),
    true = lists:member(done_payment, Places),
    ok.

%% Test initial marking has wait token
spec_init_marking_test() ->
    Spec = spec(#{wait_channel => payment, wait_msg => done}),
    InitMarking = maps:get(init_marking, Spec),
    %% Wait place should have the wait token
    WaitTokens = maps:get(wait_payment, InitMarking),
    1 = length(WaitTokens),
    {wait, payment, done} = lists:nth(1, WaitTokens),
    %% Signal and done places should be empty
    [] = maps:get(signal_payment, InitMarking),
    [] = maps:get(done_payment, InitMarking),
    ok.

%% Test transition structure
spec_transition_structure_test() ->
    Spec = spec(#{wait_channel => payment, wait_msg => done}),
    Transitions = maps:get(transitions, Spec),
    %% Should have one transition
    1 = maps:size(Transitions),
    %% Get the transition
    [TrsnDef] = maps:values(Transitions),
    %% Check structure
    true = maps:is_key(channel, TrsnDef),
    true = maps:is_key(msg, TrsnDef),
    true = maps:is_key(produce, TrsnDef),
    payment = maps:get(channel, TrsnDef),
    done = maps:get(msg, TrsnDef),
    ok.

%% Test produce map creates signal received token
spec_produce_test() ->
    Spec = spec(#{wait_channel => payment, wait_msg => done}),
    Transitions = maps:get(transitions, Spec),
    [TrsnDef] = maps:values(Transitions),
    ProduceMap = maps:get(produce, TrsnDef),
    %% Should produce token at done place
    true = maps:is_key(done_payment, ProduceMap),
    [{signal_received, payment, done}] = maps:get(done_payment, ProduceMap),
    ok.

%% Test preset has both places
spec_preset_test() ->
    Spec = spec(#{wait_channel => payment, wait_msg => done}),
    Preset = maps:get(preset, Spec),
    %% Transition should have both places as preset
    1 = maps:size(Preset),
    [TrsnName] = maps:keys(Preset),
    PresetPlaces = maps:get(TrsnName, Preset),
    true = lists:member(wait_payment, PresetPlaces),
    true = lists:member(signal_payment, PresetPlaces),
    2 = length(PresetPlaces),
    ok.

%% Test with atom message
spec_atom_msg_test() ->
    Spec = spec(#{wait_channel => notify, wait_msg => ready}),
    notify = maps:get(wait_channel, Spec),
    ready = maps:get(wait_msg, Spec),
    ok.

%% Test with binary message
spec_binary_msg_test() ->
    Spec = spec(#{wait_channel => notify, wait_msg => <<"ready">>}),
    notify = maps:get(wait_channel, Spec),
    <<"ready">> = maps:get(wait_msg, Spec),
    ok.

%% Test with integer message
spec_integer_msg_test() ->
    Spec = spec(#{wait_channel => counter, wait_msg => 42}),
    counter = maps:get(wait_channel, Spec),
    42 = maps:get(wait_msg, Spec),
    ok.

-endif.
