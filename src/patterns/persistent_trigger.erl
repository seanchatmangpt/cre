%% -*- erlang -*-
%% @doc Persistent Trigger Pattern (WCP-24) for YAWL.
%%
%% This module implements Persistent Trigger pattern (WCP-24) as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Persistent Trigger pattern represents an event that persists in the workflow
%% until it is consumed by a waiting task. Unlike transient triggers,
%% the event remains available even if the consuming task is not yet ready.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Initial place, workflow starts
%%     p_event_pool    - Pool of persistent events
%%     p_consume_ready - Consumer task ready to consume
%%     p_consumed       - Event has been consumed
%%     p_end           - Final place, workflow complete
%%
%%   Transitions:
%%     t_event_arrives - Event arrives and persists
%%     t_consume       - Consume the persistent event
%%     t_complete      - Complete the workflow
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always reachable after consumption</li>
%%   <li><b>Proper completion:</b> Event consumed once</li>
%%   <li><b>No dead transitions:</b> All transitions fireable when conditions met</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(persistent_trigger).
-moduledoc """
Persistent Trigger Pattern (WCP-24) for YAWL.

This pattern represents an event that persists until consumed by a waiting task.

## Example

```erlang
%% Create a persistent trigger pattern
State = persistent_trigger:new(consumer_task),
{ok, Pid} = gen_yawl:start_link(persistent_trigger, State, []).

%% Event arrives and persists
ok = gen_yawl:cast(Pid, {event_arrives, data1}).

%% Later, consumer consumes the persistent event
```

## API

- `new/1` - Create new persistent trigger with consumer task
- `new/2` - Create with consumer and initial data
- `get_event_data/1` - Get persistent event data
- `is_consumed/1` - Check if event was consumed
- `get_consumer/1` - Get the consumer task
- `set_event_data/2` - Set new event data

""".
-behaviour(gen_yawl).

%% gen_pnet callbacks
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2,
    trigger/3
]).

%% gen_yawl callbacks
-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3
]).

%% API exports
-export([
    new/1,
    new/2,
    get_event_data/1,
    is_consumed/1,
    get_consumer/1,
    set_event_data/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(trigger_state, {
    consumer :: atom() | undefined,
    event_data :: term() | undefined,
    event_persistent = true :: boolean(),
    consumed = false :: boolean(),
    log_id :: binary() | undefined
}).

-type trigger_state() :: #trigger_state{}.
-export_type([trigger_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Persistent Trigger pattern state.

## Example

```erlang
> State = persistent_trigger:new(consumer_task).
#trigger_state{consumer = consumer_task, event_persistent = true, ...}
```
""".
-spec new(Consumer :: atom()) -> trigger_state().
new(Consumer) when is_atom(Consumer) ->
    LogId = generate_log_id(),
    #trigger_state{
        consumer = Consumer,
        event_data = undefined,
        event_persistent = true,
        consumed = false,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Creates a new Persistent Trigger pattern state with initial event data.

## Example

```erlang
> State = persistent_trigger:new(consumer_task, initial_data).
#trigger_state{consumer = consumer_task, event_data = initial_data, ...}
```
""".
-spec new(Consumer :: atom(), EventData :: term()) -> trigger_state().
new(Consumer, EventData) when is_atom(Consumer) ->
    LogId = generate_log_id(),
    #trigger_state{
        consumer = Consumer,
        event_data = EventData,
        event_persistent = true,
        consumed = false,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Gets the persistent event data.

## Example

```erlang
> State = #trigger_state{event_data = some_data},
> persistent_trigger:get_event_data(State).
some_data
```
""".
-spec get_event_data(State :: trigger_state()) -> term() | undefined.
get_event_data(#trigger_state{event_data = Data}) ->
    Data.

%%--------------------------------------------------------------------
-doc """
Checks if the event has been consumed.

## Example

```erlang
> State0 = persistent_trigger:new(consumer),
> persistent_trigger:is_consumed(State0).
false
> State1 = State0#trigger_state{consumed = true},
> persistent_trigger:is_consumed(State1).
true
```
""".
-spec is_consumed(State :: trigger_state()) -> boolean().
is_consumed(#trigger_state{consumed = Consumed}) ->
    Consumed.

%%--------------------------------------------------------------------
-doc """
Gets the consumer task name.

## Example

```erlang
> State = persistent_trigger:new(consumer_task),
> persistent_trigger:get_consumer(State).
consumer_task
```
""".
-spec get_consumer(State :: trigger_state()) -> atom() | undefined.
get_consumer(#trigger_state{consumer = Consumer}) ->
    Consumer.

%%--------------------------------------------------------------------
-doc """
Sets new event data for the persistent trigger.

## Example

```erlang
> State0 = persistent_trigger:new(consumer),
> State1 = persistent_trigger:set_event_data(new_data, State0).
#trigger_state{event_data = new_data, ...}
```
""".
-spec set_event_data(EventData :: term(), State :: trigger_state()) -> trigger_state().
set_event_data(EventData, State) ->
    State#trigger_state{event_data = EventData}.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Persistent Trigger Petri net.

```erlang
> persistent_trigger:place_lst().
[p_start, p_event_pool, p_consume_ready, p_consumed, p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [p_start, p_event_pool, p_consume_ready, p_consumed, p_end].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Persistent Trigger Petri net.

```erlang
> persistent_trigger:trsn_lst().
[t_event_arrives, t_consume, t_complete]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [t_event_arrives, t_consume, t_complete].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: trigger_state()) ->
          [term()].

init_marking(p_start, _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> persistent_trigger:preset(t_event_arrives).
[p_start]
> persistent_trigger:preset(t_consume).
[p_event_pool, p_consume_ready]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset(t_event_arrives) -> [p_start];
preset(t_consume) -> [p_event_pool, p_consume_ready];
preset(t_complete) -> [p_consumed];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: trigger_state()) ->
          boolean().

is_enabled(t_event_arrives, _Mode, _UsrInfo) ->
    true;
is_enabled(t_consume, #{p_event_pool := [_Event], p_consume_ready := [ready]}, _UsrInfo) ->
    true;
is_enabled(t_complete, #{p_consumed := [consumed]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: trigger_state()) ->
          {produce, map()} | {produce, map(), trigger_state()} | abort.

fire(t_event_arrives, #{p_start := [start]}, #trigger_state{log_id = LogId} = State) ->
    log_event(LogId, <<"PersistentTrigger">>, <<"EventArrives">>, #{
        <<"consumer">> => atom_to_binary(State#trigger_state.consumer)
    }),
    {produce, #{
        p_start => [],
        p_event_pool => [{event, undefined}]
    }, State};

fire(t_consume, #{p_event_pool := [{event, Data}], p_consume_ready := [ready]},
        #trigger_state{consumer = Consumer, event_data = CurrentData, log_id = LogId} = State) ->
    log_event(LogId, <<"PersistentTrigger">>, <<"Consume">>, #{
        <<"consumer">> => atom_to_binary(Consumer),
        <<"event_data">> => format_event_data(Data)
    }),
    {produce, #{
        p_event_pool => [],
        p_consume_ready => [],
        p_consumed => [{consumed, Data}]
    }, State#trigger_state{
        event_data = Data,
        consumed = true
    }};

fire(t_complete, #{p_consumed := [{consumed, _Data}]}, #trigger_state{log_id = LogId} = State) ->
    log_event(LogId, <<"PersistentTrigger">>, <<"Complete">>, #{
        <<"consumer">> => atom_to_binary(State#trigger_state.consumer)
    }),
    {produce, #{
        p_consumed => [],
        p_end => [done]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), NetState :: trigger_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: trigger_state()) ->
          {ok, trigger_state()}.

init(TriggerState) ->
    LogId = TriggerState#trigger_state.log_id,
    case yawl_xes:new_log(#{<<"process">> => <<"PersistentTrigger">>}) of
        {ok, XesLogId} ->
            State1 = TriggerState#trigger_state{log_id = XesLogId},
            yawl_xes:log_case_start(XesLogId, generate_case_id()),
            {ok, State1};
        _ when LogId =/= undefined ->
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, TriggerState};
        _ ->
            {ok, TriggerState}
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_event_data, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Data = get_event_data(UsrInfo),
    {reply, {ok, Data}, NetState};
handle_call(is_consumed, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Consumed = is_consumed(UsrInfo),
    {reply, {ok, Consumed}, NetState};
handle_call(get_consumer, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Consumer = get_consumer(UsrInfo),
    {reply, {ok, Consumer}, NetState};
handle_call({set_event_data, Data}, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    NewUsrInfo = set_event_data(Data, UsrInfo),
    NewNetState = gen_yawl:set_usr_info(NetState, NewUsrInfo),
    {reply, ok, NewNetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_cast({event_arrives, Data}, NetState) ->
    %% Set event data and notify
    UsrInfo = gen_yawl:get_usr_info(NetState),
    NewUsrInfo = set_event_data(Data, UsrInfo),
    NewNetState = gen_yawl:set_usr_info(NetState, NewUsrInfo),
    {noreply, NewNetState};
handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles non-gen_pnet messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_info(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles code changes.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @doc Cleanup on termination.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), NetState :: term()) ->
          ok.

terminate(_Reason, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #trigger_state{log_id = LogId} when LogId =/= undefined ->
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        _ ->
            ok
    end,
    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Formats event data for XES logging.
%% @end
%%--------------------------------------------------------------------
-spec format_event_data(term()) -> binary().
format_event_data(Data) when is_binary(Data) -> Data;
format_event_data(Data) when is_atom(Data) -> atom_to_binary(Data);
format_event_data(Data) when is_integer(Data) -> integer_to_binary(Data);
format_event_data(Data) -> term_to_binary(Data).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique log ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().
generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"persistent_trigger_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique case ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id() -> binary().
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Logs an XES event.
%% @end
%%--------------------------------------------------------------------
-spec log_event(LogId :: binary() | undefined, Concept :: binary(),
                Lifecycle :: binary(), Data :: map()) -> ok.
log_event(undefined, _Concept, _Lifecycle, _Data) ->
    ok;
log_event(LogId, Concept, Lifecycle, Data) ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test new/1
%%--------------------------------------------------------------------
new_test() ->
    State = new(consumer_task),
    ?assert(is_record(State, trigger_state)),
    ?assertEqual(consumer_task, State#trigger_state.consumer),
    ?assertEqual(true, State#trigger_state.event_persistent),
    ?assertEqual(false, State#trigger_state.consumed).

%%--------------------------------------------------------------------
%% Test new/2
%%--------------------------------------------------------------------
new_with_data_test() ->
    State = new(consumer_task, test_data),
    ?assertEqual(test_data, State#trigger_state.event_data).

%%--------------------------------------------------------------------
%% Test get_event_data/1
%%--------------------------------------------------------------------
get_event_data_test() ->
    State = #trigger_state{event_data = some_data},
    ?assertEqual(some_data, get_event_data(State)).

%%--------------------------------------------------------------------
%% Test is_consumed/1
%%--------------------------------------------------------------------
is_consumed_test() ->
    State0 = #trigger_state{consumed = false},
    ?assertNot(is_consumed(State0)),
    State1 = #trigger_state{consumed = true},
    ?assert(is_consumed(State1)).

%%--------------------------------------------------------------------
%% Test get_consumer/1
%%--------------------------------------------------------------------
get_consumer_test() ->
    State = #trigger_state{consumer = test_consumer},
    ?assertEqual(test_consumer, get_consumer(State)).

%%--------------------------------------------------------------------
%% Test set_event_data/2
%%--------------------------------------------------------------------
set_event_data_test() ->
    State0 = #trigger_state{event_data = old_data},
    State1 = set_event_data(new_data, State0),
    ?assertEqual(new_data, State1#trigger_state.event_data).

%%--------------------------------------------------------------------
%% Test place_lst/0
%%--------------------------------------------------------------------
place_lst_test() ->
    Places = place_lst(),
    ?assert(lists:member(p_start, Places)),
    ?assert(lists:member(p_event_pool, Places)),
    ?assert(lists:member(p_consume_ready, Places)),
    ?assert(lists:member(p_consumed, Places)),
    ?assert(lists:member(p_end, Places)).

%%--------------------------------------------------------------------
%% Test trsn_lst/0
%%--------------------------------------------------------------------
trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assert(lists:member(t_event_arrives, Transitions)),
    ?assert(lists:member(t_consume, Transitions)),
    ?assert(lists:member(t_complete, Transitions)).

%%--------------------------------------------------------------------
%% Test preset/1
%%--------------------------------------------------------------------
preset_test() ->
    ?assertEqual([p_start], preset(t_event_arrives)),
    ?assertEqual([p_event_pool, p_consume_ready], preset(t_consume)),
    ?assertEqual([p_consumed], preset(t_complete)).

%%--------------------------------------------------------------------
%% Test format_event_data/1
%%--------------------------------------------------------------------
format_event_data_test() ->
    ?assertEqual(<<"binary">>, format_event_data(<<"binary">>)),
    ?assertEqual(<<"atom">>, format_event_data(atom)),
    ?assertEqual(<<"123">>, format_event_data(123)).

-endif.
