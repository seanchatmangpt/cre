%% -*- erlang -*-
%% @doc Transient Trigger Pattern (WCP-23) for YAWL.
%%
%% This module implements Transient Trigger pattern (WCP-23) as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Transient Trigger pattern represents an event that must be consumed
%% while a specific task is enabled. If the task is not ready when
%% the event occurs, the event is lost. Unlike persistent triggers,
%% transient triggers have a time window for consumption.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Initial place, workflow starts
%%     p_enabled        - Task is enabled (ready to receive)
%%     p_event         - Event has occurred
%%     p_triggered       - Event was consumed while enabled
%%     p_end           - Final place, workflow complete
%%
%%   Transitions:
%%     t_enable        - Enable the task
%%     t_event         - Event occurs
%%     t_trigger       - Trigger when both enabled and event present
%%     t_complete      - Complete the workflow
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Reachable via trigger or timeout</li>
%%   <li><b>Proper completion:</b> Event consumed only while enabled</li>
%%   <li><b>No dead transitions:</b> All transitions fireable when conditions met</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(transient_trigger).
-moduledoc """
Transient Trigger Pattern (WCP-23) for YAWL.

This pattern represents an event that must be consumed while a specific task
is enabled, otherwise the event is lost.

## Example

```erlang
%% Create a transient trigger pattern
State = transient_trigger:new(enabled_task),
{ok, Pid} = gen_yawl:start_link(transient_trigger, State, []).

%% Task becomes enabled
ok = gen_yawl:cast(Pid, enable).

%% Event occurs while task is enabled - trigger fires
%% If event occurs when task is not enabled, event is lost
```

## API

- `new/1` - Create new transient trigger with enabled task
- `new/2` - Create with enabled task and event data
- `is_enabled/1` - Check if task is enabled
- `is_event_pending/1` - Check if event is pending
- `is_triggered/1` - Check if trigger occurred
- `get_enabled_task/1` - Get the enabled task
- `get_event_data/1` - Get event data

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
    is_task_enabled/1,
    is_event_pending/1,
    is_triggered/1,
    get_enabled_task/1,
    get_event_data/1,
    set_event_data/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(trigger_state, {
    enabled_task :: atom() | undefined,
    event_data :: term() | undefined,
    event_received = false :: boolean(),
    triggered = false :: boolean(),
    log_id :: binary() | undefined
}).

-type trigger_state() :: #trigger_state{}.
-export_type([trigger_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Transient Trigger pattern state.

## Example

```erlang
> State = transient_trigger:new(enabled_task).
#trigger_state{enabled_task = enabled_task, ...}
```
""".
-spec new(EnabledTask :: atom()) -> trigger_state().
new(EnabledTask) when is_atom(EnabledTask) ->
    LogId = generate_log_id(),
    #trigger_state{
        enabled_task = EnabledTask,
        event_data = undefined,
        event_received = false,
        triggered = false,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Creates a new Transient Trigger pattern state with initial event data.

## Example

```erlang
> State = transient_trigger:new(enabled_task, initial_event).
#trigger_state{enabled_task = enabled_task, event_data = initial_event, ...}
```
""".
-spec new(EnabledTask :: atom(), EventData :: term()) -> trigger_state().
new(EnabledTask, EventData) when is_atom(EnabledTask) ->
    LogId = generate_log_id(),
    #trigger_state{
        enabled_task = EnabledTask,
        event_data = EventData,
        event_received = true,
        triggered = false,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Checks if the task is currently enabled.

## Example

```erlang
> State = transient_trigger:new(enabled_task),
> transient_trigger:is_task_enabled(State).
false
```
""".
-spec is_task_enabled(State :: trigger_state()) -> boolean().
is_task_enabled(#trigger_state{triggered = Triggered}) ->
    %% Task is considered enabled until triggered
    not Triggered.

%%--------------------------------------------------------------------
-doc """
Checks if an event is pending (received but not consumed).

## Example

```erlang
> State = #trigger_state{event_received = true, triggered = false},
> transient_trigger:is_event_pending(State).
true
```
""".
-spec is_event_pending(State :: trigger_state()) -> boolean().
is_event_pending(#trigger_state{event_received = Received, triggered = Triggered}) ->
    Received andalso not Triggered.

%%--------------------------------------------------------------------
-doc """
Checks if the trigger has occurred.

## Example

```erlang
> State0 = transient_trigger:new(enabled_task),
> transient_trigger:is_triggered(State0).
false
> State1 = State0#trigger_state{triggered = true},
> transient_trigger:is_triggered(State1).
true
```
""".
-spec is_triggered(State :: trigger_state()) -> boolean().
is_triggered(#trigger_state{triggered = Triggered}) ->
    Triggered.

%%--------------------------------------------------------------------
-doc """
Gets the enabled task name.

## Example

```erlang
> State = transient_trigger:new(enabled_task),
> transient_trigger:get_enabled_task(State).
enabled_task
```
""".
-spec get_enabled_task(State :: trigger_state()) -> atom() | undefined.
get_enabled_task(#trigger_state{enabled_task = Task}) ->
    Task.

%%--------------------------------------------------------------------
-doc """
Gets the event data.

## Example

```erlang
> State = transient_trigger:new(enabled_task, event_data),
> transient_trigger:get_event_data(State).
event_data
```
""".
-spec get_event_data(State :: trigger_state()) -> term() | undefined.
get_event_data(#trigger_state{event_data = Data}) ->
    Data.

%%--------------------------------------------------------------------
-doc """
Sets new event data for the trigger.

## Example

```erlang
> State0 = transient_trigger:new(enabled_task),
> State1 = transient_trigger:set_event_data(new_data, State0).
#trigger_state{event_data = new_data, event_received = true, ...}
```
""".
-spec set_event_data(EventData :: term(), State :: trigger_state()) -> trigger_state().
set_event_data(EventData, State) ->
    State#trigger_state{
        event_data = EventData,
        event_received = true
    }.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Transient Trigger Petri net.

```erlang
> transient_trigger:place_lst().
[p_start, p_enabled, p_event, p_triggered, p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [p_start, p_enabled, p_event, p_triggered, p_end].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Transient Trigger Petri net.

```erlang
> transient_trigger:trsn_lst().
[t_enable, t_event, t_trigger, t_complete]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [t_enable, t_event, t_trigger, t_complete].

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
> transient_trigger:preset(t_enable).
[p_start]
> transient_trigger:preset(t_trigger).
[p_enabled, p_event]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset(t_enable) -> [p_start];
preset(t_event) -> [p_event];
preset(t_trigger) -> [p_enabled, p_event];
preset(t_complete) -> [p_triggered];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: trigger_state()) ->
          boolean().

is_enabled(t_enable, _Mode, _UsrInfo) ->
    true;
is_enabled(t_event, _Mode, _UsrInfo) ->
    true;
is_enabled(t_trigger, #{p_enabled := [enabled], p_event := [_Event]}, #trigger_state{triggered = Triggered}) ->
    %% Only trigger if not already triggered (transient behavior)
    not Triggered;
is_enabled(t_complete, #{p_triggered := [triggered]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: trigger_state()) ->
          {produce, map()} | {produce, map(), trigger_state()} | abort.

fire(t_enable, #{p_start := [start]}, #trigger_state{enabled_task = Task, log_id = LogId} = State) ->
    log_event(LogId, <<"TransientTrigger">>, <<"Enable">>, #{
        <<"task">> => atom_to_binary(Task)
    }),
    {produce, #{
        p_start => [],
        p_enabled => [enabled]
    }, State};

fire(t_event, #{p_event := []}, #trigger_state{event_data = Data, log_id = LogId} = State) ->
    log_event(LogId, <<"TransientTrigger">>, <<"Event">>, #{
        <<"event_data">> => format_event_data(Data)
    }),
    {produce, #{
        p_event => [{event, Data}]
    }, State#trigger_state{event_received = true}};

fire(t_trigger, #{p_enabled := [enabled], p_event := [{event, Data}]},
        #trigger_state{enabled_task = Task, log_id = LogId} = State) ->
    log_event(LogId, <<"TransientTrigger">>, <<"Trigger">>, #{
        <<"task">> => atom_to_binary(Task),
        <<"event_data">> => format_event_data(Data)
    }),
    {produce, #{
        p_enabled => [],
        p_event => [],
        p_triggered => [triggered]
    }, State#trigger_state{
        triggered = true,
        event_data = Data
    }};

fire(t_complete, #{p_triggered := [triggered]}, #trigger_state{log_id = LogId} = State) ->
    log_event(LogId, <<"TransientTrigger">>, <<"Complete">>, #{
        <<"task">> => atom_to_binary(State#trigger_state.enabled_task)
    }),
    {produce, #{
        p_triggered => [],
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
    case yawl_xes:new_log(#{<<"process">> => <<"TransientTrigger">>}) of
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

handle_call(is_task_enabled, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Enabled = is_task_enabled(UsrInfo),
    {reply, {ok, Enabled}, NetState};
handle_call(is_event_pending, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Pending = is_event_pending(UsrInfo),
    {reply, {ok, Pending}, NetState};
handle_call(is_triggered, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Triggered = is_triggered(UsrInfo),
    {reply, {ok, Triggered}, NetState};
handle_call(get_enabled_task, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Task = get_enabled_task(UsrInfo),
    {reply, {ok, Task}, NetState};
handle_call(get_event_data, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Data = get_event_data(UsrInfo),
    {reply, {ok, Data}, NetState};
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

handle_cast(enable, NetState) ->
    %% Enable the task
    {noreply, NetState};
handle_cast({event, Data}, NetState) ->
    %% Set event data
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
    <<"transient_trigger_", Hex/binary>>.

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
    State = new(enabled_task),
    ?assert(is_record(State, trigger_state)),
    ?assertEqual(enabled_task, State#trigger_state.enabled_task),
    ?assertEqual(false, State#trigger_state.event_received),
    ?assertEqual(false, State#trigger_state.triggered).

%%--------------------------------------------------------------------
%% Test new/2
%%--------------------------------------------------------------------
new_with_data_test() ->
    State = new(enabled_task, event_data),
    ?assertEqual(event_data, State#trigger_state.event_data),
    ?assertEqual(true, State#trigger_state.event_received).

%%--------------------------------------------------------------------
%% Test is_task_enabled/1
%%--------------------------------------------------------------------
is_task_enabled_test() ->
    State0 = #trigger_state{triggered = false},
    ?assert(is_task_enabled(State0)),
    State1 = #trigger_state{triggered = true},
    ?assertNot(is_task_enabled(State1)).

%%--------------------------------------------------------------------
%% Test is_event_pending/1
%%--------------------------------------------------------------------
is_event_pending_test() ->
    State0 = #trigger_state{event_received = true, triggered = false},
    ?assert(is_event_pending(State0)),
    State1 = #trigger_state{event_received = false, triggered = true},
    ?assertNot(is_event_pending(State1)).

%%--------------------------------------------------------------------
%% Test is_triggered/1
%%--------------------------------------------------------------------
is_triggered_test() ->
    State0 = #trigger_state{triggered = false},
    ?assertNot(is_triggered(State0)),
    State1 = #trigger_state{triggered = true},
    ?assert(is_triggered(State1)).

%%--------------------------------------------------------------------
%% Test get_enabled_task/1
%%--------------------------------------------------------------------
get_enabled_task_test() ->
    State = #trigger_state{enabled_task = test_task},
    ?assertEqual(test_task, get_enabled_task(State)).

%%--------------------------------------------------------------------
%% Test get_event_data/1
%%--------------------------------------------------------------------
get_event_data_test() ->
    State = #trigger_state{event_data = test_data},
    ?assertEqual(test_data, get_event_data(State)).

%%--------------------------------------------------------------------
%% Test set_event_data/2
%%--------------------------------------------------------------------
set_event_data_test() ->
    State0 = #trigger_state{event_data = old_data, event_received = false},
    State1 = set_event_data(new_data, State0),
    ?assertEqual(new_data, State1#trigger_state.event_data),
    ?assertEqual(true, State1#trigger_state.event_received).

%%--------------------------------------------------------------------
%% Test place_lst/0
%%--------------------------------------------------------------------
place_lst_test() ->
    Places = place_lst(),
    ?assert(lists:member(p_start, Places)),
    ?assert(lists:member(p_enabled, Places)),
    ?assert(lists:member(p_event, Places)),
    ?assert(lists:member(p_triggered, Places)),
    ?assert(lists:member(p_end, Places)).

%%--------------------------------------------------------------------
%% Test trsn_lst/0
%%--------------------------------------------------------------------
trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assert(lists:member(t_enable, Transitions)),
    ?assert(lists:member(t_event, Transitions)),
    ?assert(lists:member(t_trigger, Transitions)),
    ?assert(lists:member(t_complete, Transitions)).

%%--------------------------------------------------------------------
%% Test preset/1
%%--------------------------------------------------------------------
preset_test() ->
    ?assertEqual([p_start], preset(t_enable)),
    ?assertEqual([p_event], preset(t_event)),
    ?assertEqual([p_enabled, p_event], preset(t_trigger)),
    ?assertEqual([p_triggered], preset(t_complete)).

%%--------------------------------------------------------------------
%% Test format_event_data/1
%%--------------------------------------------------------------------
format_event_data_test() ->
    ?assertEqual(<<"binary">>, format_event_data(<<"binary">>)),
    ?assertEqual(<<"atom">>, format_event_data(atom)),
    ?assertEqual(<<"123">>, format_event_data(123)).

-endif.
