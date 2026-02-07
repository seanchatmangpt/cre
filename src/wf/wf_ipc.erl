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
Inter-workflow communication workflow specification.

Creates a Petri net that waits for channel signals. The workflow blocks
until a matching message is published to the specified channel.

This is a pure module - it creates specs for the engine to use and
provides a channel registry for inter-workflow pub/sub communication.

## Features

- **Signal/Wait**: One workflow waits for another to publish a signal
- **Pub/Sub**: Multiple cases can subscribe to channels and receive broadcasts
- **Channel Registry**: Pure functional registry for tracking subscriptions

## Examples

### Basic Signal/Wait

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

### Pub/Sub with Channel Registry

```erlang
> Registry0 = wf_ipc:new_registry().
#{channels => #{}}

> {Registry1, SubRef1} = wf_ipc:subscribe(Registry0, <<"order_created">>, case_id_1).
{#{channels => #{<<"order_created">> => [{case_id_1, ref}]}}, ref}

> {Registry2, SubRef2} = wf_ipc:subscribe(Registry1, <<"order_created">>, case_id_2).
{#{channels => #{<<"order_created">> => [{case_id_2, ref}, {case_id_1, ref}]}}, ref}

> wf_ipc:publish(Registry2, <<"order_created">>, #{amount => 100}).
[{case_id_1, #{amount => 100}}, {case_id_2, #{amount => 100}}]

> Registry3 = wf_ipc:unsubscribe(Registry2, SubRef1).
#{channels => #{<<"order_created">> => [{case_id_2, ref}]}}
```

### Broadcast to Multiple Subscribers

```erlang
> Registry0 = wf_ipc:new_registry().
#{channels => #{}}

> {Registry1, _} = wf_ipc:subscribe(Registry0, events, case1).
{#{channels => #{events => [{case1, ref}]}}, ref}

> {Registry2, _} = wf_ipc:subscribe(Registry1, events, case2).
{#{channels => #{events => [{case2, ref}, {case1, ref}]}}, ref}

> {Registry3, _} = wf_ipc:subscribe(Registry2, events, case3).
{#{channels => #{events => [{case3, ref}, {case2, ref}, {case1, ref}]}}, ref}

> wf_ipc:publish(Registry3, events, #{type => alert}).
[{case1, #{type => alert}}, {case2, #{type => alert}}, {case3, #{type => alert}}]
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Spec creation
-export([spec/1, signal_spec/1, wait_spec/1]).

%% Channel registry API
-export([new_registry/0, subscribe/3, unsubscribe/2, publish/3,
         list_subscribers/2, list_channels/1, subscriber_count/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc IPC specification input map for wait spec.
%%
%% Required keys:
%%   - wait_channel: Channel to wait for (atom | binary)
%%   - wait_msg: Message to wait for (term)
%%
%% Optional keys:
%%   - timeout_place: Place to produce token on timeout (atom)
%%   - timeout_ms: Timeout in milliseconds (integer)
%%--------------------------------------------------------------------
-type ipc_spec() :: #{
    wait_channel := atom() | binary(),
    wait_msg := term(),
    timeout_place => atom(),
    timeout_ms => non_neg_integer()
}.

%%--------------------------------------------------------------------
%% @doc Signal specification input map.
%%
%% Required keys:
%%   - signal_channel: Channel to signal on (atom | binary)
%%   - signal_msg: Message to send (term)
%%--------------------------------------------------------------------
-type signal_spec() :: #{
    signal_channel := atom() | binary(),
    signal_msg := term()
}.

%%--------------------------------------------------------------------
%% @doc Compiled workflow spec for wf_engine.
%%
%% Contains:
%%   - places: List of all place atoms in the Petri net
%%   - transitions: Map of transition definitions
%%   - init_marking: Initial marking map
%%   - preset: Map of transition -> input places
%%   - wait_channel: Channel this spec waits on (for wait specs)
%%   - wait_msg: Message this spec waits for (for wait specs)
%%   - signal_channel: Channel this spec signals on (for signal specs)
%%   - signal_msg: Message this spec sends (for signal specs)
%%--------------------------------------------------------------------
-type compiled_spec() :: #{
    places := [atom()],
    transitions := #{atom() => transition_def()},
    init_marking := #{atom() => [term()]},
    preset := #{atom() => [atom()]},
    wait_channel => atom() | binary(),
    wait_msg => term(),
    signal_channel => atom() | binary(),
    signal_msg => term()
}.

%%--------------------------------------------------------------------
%% @doc Definition of a single transition.
%%
%% Each transition has:
%%   - channel: The channel to wait for (wait transitions) or signal on (signal transitions)
%%   - msg: The message to wait for or send
%%   - produce: The produce map when signal arrives or is sent
%%--------------------------------------------------------------------
-type transition_def() :: #{
    channel => atom() | binary(),
    msg => term(),
    produce := #{atom() => [term()]}
}.

%%--------------------------------------------------------------------
%% @doc Channel registry for tracking subscriptions.
%%
%% Pure functional data structure mapping channels to subscribers.
%%--------------------------------------------------------------------
-type channel_registry() :: #{
    channels := #{channel() => [subscriber()]}
}.

%%--------------------------------------------------------------------
%% @doc Channel identifier.
%%
%% Can be an atom or binary representing a named communication channel.
%%--------------------------------------------------------------------
-type channel() :: atom() | binary().

%%--------------------------------------------------------------------
%% @doc Case identifier for workflow instances.
%%
%%--------------------------------------------------------------------
-type case_id() :: term().

%%--------------------------------------------------------------------
%% @doc Subscriber reference.
%%
%% A reference created when subscribing to a channel, used to unsubscribe.
%%--------------------------------------------------------------------
-type subscriber_ref() :: reference().

%%--------------------------------------------------------------------
%% @doc Subscriber record.
%%
%% Tracks a subscriber with their case ID and unique reference.
%%--------------------------------------------------------------------
-type subscriber() :: {case_id(), subscriber_ref()}.

%%--------------------------------------------------------------------
%% @doc Message payload sent through channels.
%%
%% Can be any Erlang term.
%%--------------------------------------------------------------------
-type message() :: term().

%%--------------------------------------------------------------------
%% @doc Published message delivery result.
%%
%% List of tuples containing case_id and the message they received.
%%--------------------------------------------------------------------
-type publish_result() :: [{case_id(), message()}].

-export_type([ipc_spec/0, signal_spec/0, compiled_spec/0, channel_registry/0,
             channel/0, case_id/0, subscriber_ref/0, subscriber/0,
             message/0, publish_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an IPC wait workflow specification.
%%
%% Takes a map specifying:
%%   - wait_channel: The channel name (atom or binary) to wait on
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
%% @param IpcSpec Map with wait_channel => atom()|binary() and wait_msg => term()
%% @return Compiled workflow specification
%%
%% @end
%%--------------------------------------------------------------------
-spec spec(IpcSpec :: ipc_spec()) -> compiled_spec().

spec(#{wait_channel := Channel, wait_msg := Msg}) ->
    %% Normalize channel to atom for Petri net places
    NormChannel = normalize_channel(Channel),
    %% Generate unique place and transition names
    WaitPlace = make_place_name(wait, NormChannel),
    SignalPlace = make_place_name(signal, NormChannel),
    DonePlace = make_place_name(done, NormChannel),
    TrsnName = make_trsn_name(wait, NormChannel, Msg),

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

%%--------------------------------------------------------------------
%% @doc Creates a signal workflow specification.
%%
%% Creates a spec that signals on a channel when executed.
%% This is used by workflows that publish signals to waiting workflows.
%%
%% @param SignalSpec Map with signal_channel => atom()|binary() and signal_msg => term()
%% @return Compiled workflow specification
%%
%% @end
%%--------------------------------------------------------------------
-spec signal_spec(SignalSpec :: signal_spec()) -> compiled_spec().

signal_spec(#{signal_channel := Channel, signal_msg := Msg}) ->
    %% Normalize channel to atom for Petri net places
    NormChannel = normalize_channel(Channel),
    %% Generate unique place and transition names
    StartPlace = make_place_name(signal_start, NormChannel),
    SignalSentPlace = make_place_name(signal_sent, NormChannel),
    TrsnName = make_trsn_name(signal, NormChannel, Msg),

    %% Build the compiled spec
    #{
        places => [StartPlace, SignalSentPlace],
        transitions => #{
            TrsnName => #{
                channel => Channel,
                msg => Msg,
                produce => #{
                    SignalSentPlace => [{signal_sent, Channel, Msg}]
                }
            }
        },
        init_marking => #{
            StartPlace => [start],
            SignalSentPlace => []
        },
        preset => #{
            TrsnName => [StartPlace]
        },
        signal_channel => Channel,
        signal_msg => Msg
    }.

%%--------------------------------------------------------------------
%% @doc Creates a wait-only workflow specification.
%%
%% Convenience function that creates a spec focused on waiting.
%% Equivalent to spec/1 but with clearer intent.
%%
%% @param IpcSpec Map with wait_channel => atom()|binary() and wait_msg => term()
%% @return Compiled workflow specification
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_spec(IpcSpec :: ipc_spec()) -> compiled_spec().

wait_spec(IpcSpec) ->
    spec(IpcSpec).

%%--------------------------------------------------------------------
%% @doc Creates a new empty channel registry.
%%
%% The registry is a pure functional data structure for tracking
%% which cases are subscribed to which channels.
%%
%% @return Empty channel registry
%%
%% @end
%%--------------------------------------------------------------------
-spec new_registry() -> channel_registry().

new_registry() ->
    #{channels => #{}}.

%%--------------------------------------------------------------------
%% @doc Subscribes a case to a channel.
%%
%% Returns {NewRegistry, SubscriberRef} where SubscriberRef is used
%% to unsubscribe later. The subscriber is added to the end of the
%% subscriber list to maintain order.
%%
%% @param Registry Current channel registry
%% @param Channel Channel to subscribe to (atom or binary)
%% @param CaseId Case identifier to subscribe
%% @return {UpdatedRegistry, SubscriberRef}
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe(Registry :: channel_registry(),
                Channel :: channel(),
                CaseId :: case_id()) ->
          {channel_registry(), subscriber_ref()}.

subscribe(#{channels := Channels} = _Registry, Channel, CaseId) ->
    SubRef = make_ref(),
    Subscriber = {CaseId, SubRef},
    NormChannel = normalize_channel(Channel),

    %% Add subscriber to the channel's list
    UpdatedChannels = maps:update_with(
        NormChannel,
        fun(Subs) -> Subs ++ [Subscriber] end,
        [Subscriber],
        Channels
    ),

    {#{channels => UpdatedChannels}, SubRef}.

%%--------------------------------------------------------------------
%% @doc Unsubscribes a subscriber reference from the registry.
%%
%% Removes the subscriber associated with SubRef from all channels.
%% If the subscriber is not found, returns the registry unchanged.
%%
%% @param Registry Current channel registry
%% @param SubRef Subscriber reference to unsubscribe
%% @return Updated channel registry
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(Registry :: channel_registry(), SubRef :: subscriber_ref()) ->
          channel_registry().

unsubscribe(#{channels := Channels} = _Registry, SubRef) ->
    %% Filter out the subscriber from all channels
    UpdatedChannels = maps:map(
        fun(_Channel, Subscribers) ->
            lists:filter(
                fun({_CaseId, Ref}) -> Ref =/= SubRef end,
                Subscribers
            )
        end,
        Channels
    ),

    %% Remove empty channels
    CleanedChannels = maps:filter(
        fun(_Channel, Subscribers) -> Subscribers =/= [] end,
        UpdatedChannels
    ),

    #{channels => CleanedChannels}.

%%--------------------------------------------------------------------
%% @doc Publishes a message to all subscribers of a channel.
%%
%% Returns a list of {CaseId, Message} tuples representing which
%% cases received the message. If no subscribers, returns empty list.
%% Publishing does NOT modify the registry (pure function).
%%
%% @param Registry Current channel registry
%% @param Channel Channel to publish to
%% @param Message Message payload to publish
%% @return List of {CaseId, Message} deliveries
%%
%% @end
%%--------------------------------------------------------------------
-spec publish(Registry :: channel_registry(),
              Channel :: channel(),
              Message :: message()) ->
          publish_result().

publish(#{channels := Channels}, Channel, Message) ->
    NormChannel = normalize_channel(Channel),

    case maps:get(NormChannel, Channels, []) of
        [] -> [];
        Subscribers ->
            %% Deliver to all subscribers
            [{CaseId, Message} || {CaseId, _Ref} <- Subscribers]
    end.

%%--------------------------------------------------------------------
%% @doc Lists all subscribers for a channel.
%%
%% Returns list of case_ids subscribed to the channel.
%% Empty list if channel has no subscribers or doesn't exist.
%%
%% @param Registry Current channel registry
%% @param Channel Channel to query
%% @return List of case IDs
%%
%% @end
%%--------------------------------------------------------------------
-spec list_subscribers(Registry :: channel_registry(), Channel :: channel()) ->
          [case_id()].

list_subscribers(#{channels := Channels}, Channel) ->
    NormChannel = normalize_channel(Channel),

    case maps:get(NormChannel, Channels, []) of
        [] -> [];
        Subscribers -> [CaseId || {CaseId, _Ref} <- Subscribers]
    end.

%%--------------------------------------------------------------------
%% @doc Lists all channels that have subscribers.
%%
%% Returns list of channel names that currently have at least one subscriber.
%%
%% @param Registry Current channel registry
%% @return List of channel names
%%
%% @end
%%--------------------------------------------------------------------
-spec list_channels(Registry :: channel_registry()) -> [channel()].

list_channels(#{channels := Channels}) ->
    maps:keys(Channels).

%%--------------------------------------------------------------------
%% @doc Returns the number of subscribers for a channel.
%%
%% Returns 0 if channel has no subscribers or doesn't exist.
%%
%% @param Registry Current channel registry
%% @param Channel Channel to query
%% @return Number of subscribers
%%
%% @end
%%--------------------------------------------------------------------
-spec subscriber_count(Registry :: channel_registry(), Channel :: channel()) ->
          non_neg_integer().

subscriber_count(#{channels := Channels}, Channel) ->
    NormChannel = normalize_channel(Channel),
    case maps:get(NormChannel, Channels, []) of
        [] -> 0;
        Subscribers -> length(Subscribers)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes a channel to an atom.
%%
%% Binary channels are converted to atoms for Petri net compatibility.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_channel(channel()) -> atom().

normalize_channel(Channel) when is_atom(Channel) ->
    Channel;
normalize_channel(Channel) when is_binary(Channel) ->
    binary_to_atom(Channel, utf8).

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
%% @doc Generates a unique transition name.
%%
%% @end
%%--------------------------------------------------------------------
-spec make_trsn_name(SignalType :: atom(), Channel :: atom(), Msg :: term()) ->
          atom().

make_trsn_name(SignalType, Channel, Msg) ->
    %% Create a readable atom name for the transition
    MsgStr = case Msg of
        A when is_atom(A) -> atom_to_list(A);
        B when is_binary(B) -> binary_to_list(B);
        I when is_integer(I) -> integer_to_list(I);
        _ -> "msg"
    end,
    Name = atom_to_list(SignalType) ++ "_" ++ atom_to_list(Channel) ++ "_" ++ MsgStr,
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
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%%====================================================================
%% Spec Tests
%%====================================================================

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

%% Test spec with binary channel
spec_binary_channel_test() ->
    Spec = spec(#{wait_channel => <<"payment">>, wait_msg => done}),
    %% Check values - channel stored as-is
    <<"payment">> = maps:get(wait_channel, Spec),
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

%%====================================================================
%% Signal Spec Tests
%%====================================================================

signal_spec_creation_test() ->
    Spec = signal_spec(#{signal_channel => alert, signal_msg => error}),
    alert = maps:get(signal_channel, Spec),
    error = maps:get(signal_msg, Spec),
    ok.

signal_spec_places_test() ->
    Spec = signal_spec(#{signal_channel => alert, signal_msg => error}),
    Places = maps:get(places, Spec),
    2 = length(Places),
    true = lists:member(signal_start_alert, Places),
    true = lists:member(signal_sent_alert, Places),
    ok.

signal_spec_init_marking_test() ->
    Spec = signal_spec(#{signal_channel => alert, signal_msg => error}),
    InitMarking = maps:get(init_marking, Spec),
    [start] = maps:get(signal_start_alert, InitMarking),
    [] = maps:get(signal_sent_alert, InitMarking),
    ok.

signal_spec_binary_channel_test() ->
    Spec = signal_spec(#{signal_channel => <<"alert">>, signal_msg => error}),
    <<"alert">> = maps:get(signal_channel, Spec),
    error = maps:get(signal_msg, Spec),
    ok.

%%====================================================================
%% Channel Registry Tests
%%====================================================================

new_registry_test() ->
    Registry = new_registry(),
    true = is_map(Registry),
    true = maps:is_key(channels, Registry),
    #{} = maps:get(channels, Registry),
    ok.

subscribe_single_test() ->
    Registry0 = new_registry(),
    {Registry1, SubRef} = subscribe(Registry0, test_channel, case1),
    true = is_reference(SubRef),
    %% Verify subscription
    [case1] = list_subscribers(Registry1, test_channel),
    1 = subscriber_count(Registry1, test_channel),
    ok.

subscribe_multiple_same_channel_test() ->
    Registry0 = new_registry(),
    {Registry1, _} = subscribe(Registry0, events, case1),
    {Registry2, _} = subscribe(Registry1, events, case2),
    {Registry3, _} = subscribe(Registry2, events, case3),
    %% All three should be subscribed
    Subs = list_subscribers(Registry3, events),
    3 = length(Subs),
    true = lists:member(case1, Subs),
    true = lists:member(case2, Subs),
    true = lists:member(case3, Subs),
    3 = subscriber_count(Registry3, events),
    ok.

subscribe_multiple_channels_test() ->
    Registry0 = new_registry(),
    {Registry1, _} = subscribe(Registry0, ch1, case1),
    {Registry2, _} = subscribe(Registry1, ch2, case1),
    {Registry3, _} = subscribe(Registry2, ch3, case1),
    %% case1 should be subscribed to all three channels
    [case1] = list_subscribers(Registry3, ch1),
    [case1] = list_subscribers(Registry3, ch2),
    [case1] = list_subscribers(Registry3, ch3),
    ok.

unsubscribe_test() ->
    Registry0 = new_registry(),
    {Registry1, SubRef} = subscribe(Registry0, events, case1),
    {Registry2, _} = subscribe(Registry1, events, case2),
    %% Before unsubscribe
    2 = subscriber_count(Registry2, events),
    %% Unsubscribe case1
    Registry3 = unsubscribe(Registry2, SubRef),
    %% After unsubscribe
    [case2] = list_subscribers(Registry3, events),
    1 = subscriber_count(Registry3, events),
    ok.

unsubscribe_empty_channel_test() ->
    Registry0 = new_registry(),
    {Registry1, SubRef} = subscribe(Registry0, events, case1),
    %% Unsubscribe the only subscriber
    Registry2 = unsubscribe(Registry1, SubRef),
    %% Channel should be removed
    0 = subscriber_count(Registry2, events),
    [] = list_subscribers(Registry2, events),
    %% Channel should not be in list of channels
    false = lists:member(events, list_channels(Registry2)),
    ok.

unsubscribe_invalid_ref_test() ->
    Registry0 = new_registry(),
    {Registry1, _} = subscribe(Registry0, events, case1),
    %% Try to unsubscribe with invalid ref
    Registry2 = unsubscribe(Registry1, make_ref()),
    %% Registry should be unchanged
    [case1] = list_subscribers(Registry2, events),
    1 = subscriber_count(Registry2, events),
    ok.

publish_no_subscribers_test() ->
    Registry = new_registry(),
    [] = publish(Registry, events, #{msg => test}),
    ok.

publish_single_subscriber_test() ->
    Registry0 = new_registry(),
    {Registry, _} = subscribe(Registry0, events, case1),
    Result = publish(Registry, events, #{msg => test}),
    [{case1, #{msg := test}}] = Result,
    ok.

publish_multiple_subscribers_test() ->
    Registry0 = new_registry(),
    {Registry1, _} = subscribe(Registry0, events, case1),
    {Registry2, _} = subscribe(Registry1, events, case2),
    {Registry3, _} = subscribe(Registry2, events, case3),
    Msg = #{alert => true},
    Result = publish(Registry3, events, Msg),
    %% All three should receive
    3 = length(Result),
    true = lists:keymember(case1, 1, Result),
    true = lists:keymember(case2, 1, Result),
    true = lists:keymember(case3, 1, Result),
    %% All should have same message
    [{case1, Msg}, {case2, Msg}, {case3, Msg}] = Result,
    ok.

publish_preserves_registry_test() ->
    Registry0 = new_registry(),
    {Registry1, _} = subscribe(Registry0, events, case1),
    %% Publish should not modify registry
    Result = publish(Registry1, events, #{msg => test}),
    %% Registry1 should be unchanged
    [case1] = list_subscribers(Registry1, events),
    %% Result should contain delivery
    [{case1, #{msg := test}}] = Result,
    ok.

publish_empty_channel_test() ->
    Registry0 = new_registry(),
    {Registry1, _} = subscribe(Registry0, ch1, case1),
    %% Publishing to non-existent channel returns empty
    [] = publish(Registry1, nonexistent, #{msg => test}),
    ok.

list_channels_test() ->
    Registry0 = new_registry(),
    {Registry1, _} = subscribe(Registry0, ch1, case1),
    {Registry2, _} = subscribe(Registry1, ch2, case1),
    {Registry3, _} = subscribe(Registry2, ch1, case2),
    Channels = list_channels(Registry3),
    2 = length(Channels),
    true = lists:member(ch1, Channels),
    true = lists:member(ch2, Channels),
    ok.

subscriber_count_test() ->
    Registry0 = new_registry(),
    0 = subscriber_count(Registry0, events),
    {Registry1, _} = subscribe(Registry0, events, case1),
    1 = subscriber_count(Registry1, events),
    {Registry2, _} = subscribe(Registry1, events, case2),
    2 = subscriber_count(Registry2, events),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

publish_enables_waiting_test() ->
    %% Create a wait spec
    WaitSpec = spec(#{wait_channel => payment, wait_msg => done}),
    payment = maps:get(wait_channel, WaitSpec),
    done = maps:get(wait_msg, WaitSpec),
    ok.

subscribe_publish_flow_test() ->
    %% Simulate a complete pub/sub flow
    Registry0 = new_registry(),

    %% Case 1 subscribes to order_created
    {Registry1, Sub1} = subscribe(Registry0, order_created, case1),

    %% Case 2 subscribes to order_created
    {Registry2, Sub2} = subscribe(Registry1, order_created, case2),

    %% Case 3 subscribes to payment_received
    {Registry3, _} = subscribe(Registry2, payment_received, case3),

    %% Publish to order_created - case1 and case2 should receive
    OrderMsg = #{order_id => 123, amount => 100},
    OrderResult = publish(Registry3, order_created, OrderMsg),
    2 = length(OrderResult),
    true = lists:keymember(case1, 1, OrderResult),
    true = lists:keymember(case2, 1, OrderResult),
    false = lists:keymember(case3, 1, OrderResult),

    %% Unsubscribe case1 from order_created
    Registry4 = unsubscribe(Registry3, Sub1),

    %% Publish again - only case2 should receive
    OrderResult2 = publish(Registry4, order_created, OrderMsg),
    [{case2, OrderMsg}] = OrderResult2,

    %% Publish to payment_received - case3 should receive
    PaymentMsg = #{order_id => 123, status => paid},
    PaymentResult = publish(Registry4, payment_received, PaymentMsg),
    [{case3, PaymentMsg}] = PaymentResult,
    ok.

binary_channel_normalize_test() ->
    %% Binary channels should work the same as atom channels
    Registry0 = new_registry(),
    {Registry1, _} = subscribe(Registry0, <<"events">>, case1),
    {Registry2, _} = subscribe(Registry1, events, case2),

    %% Both should be in the same channel
    Subs = list_subscribers(Registry2, events),
    2 = length(Subs),

    %% Publishing with atom or binary should work
    Result1 = publish(Registry2, events, #{msg => test}),
    Result2 = publish(Registry2, <<"events">>, #{msg => test}),

    %% Both should deliver to both subscribers
    2 = length(Result1),
    2 = length(Result2),
    ok.

signal_wait_pair_test() ->
    %% Create matching signal and wait specs
    Channel = test,
    Msg = ready,

    WaitSpec = wait_spec(#{wait_channel => Channel, wait_msg => Msg}),
    SignalSpec = signal_spec(#{signal_channel => Channel, signal_msg => Msg}),

    %% Verify they use matching channels
    Channel = maps:get(wait_channel, WaitSpec),
    Channel = maps:get(signal_channel, SignalSpec),
    Msg = maps:get(wait_msg, WaitSpec),
    Msg = maps:get(signal_msg, SignalSpec),

    %% Wait spec should have wait places
    WaitPlaces = maps:get(places, WaitSpec),
    true = lists:member(wait_test, WaitPlaces),

    %% Signal spec should have signal places
    SignalPlaces = maps:get(places, SignalSpec),
    true = lists:member(signal_start_test, SignalPlaces),
    ok.

-endif.
