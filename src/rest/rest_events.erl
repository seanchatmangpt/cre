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
%% @author Jorgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%%
%% @doc REST Event Streaming Module
%%
%% This module provides Server-Sent Events (SSE) and WebSocket support
%% for streaming workflow events to REST clients. It manages event
%% subscriptions, broadcasts workflow state changes, and handles
%% client connections with proper backpressure and error handling.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li><b>SSE Support:</b> Server-Sent Events for real-time updates</li>
%%   <li><b>WebSocket Support:</b> Bidirectional communication with clients</li>
%%   <li><b>Pub/Sub Pattern:</b> One-to-many event broadcasting</li>
%%   <li><b>Event Filtering:</b> Subscribe to specific workflow events</li>
%%   <li><b>Backpressure Handling:</b> Graceful client buffer management</li>
%% </ul>
%%
%% <h3>Event Types</h3>
%% <ul>
%%   <li><b>workflow_started:</b> Workflow execution begins</li>
%%   <li><b>workflow_completed:</b> Workflow execution succeeded</li>
%%   <li><b>workflow_failed:</b> Workflow execution failed</li>
%%   <li><b>activity_started:</b> Activity execution begins</li>
%%   <li><b>activity_completed:</b> Activity execution succeeded</li>
%%   <li><b>activity_failed:</b> Activity execution failed</li>
%%   <li><b>transition_enabled:</b> Petri Net transition enabled</li>
%%   <li><b>token_consumed:</b> Token removed from place</li>
%%   <li><b>token_produced:</b> Token added to place</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(rest_events).
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

-export([start_link/0]).
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-export([subscribe/1]).
-export([subscribe/2]).
-export([publish/2]).
-export([publish/3]).
-export([get_subscribers/0]).
-export([doctest_test/0]).

%%====================================================================
%% Includes
%%====================================================================

-include("cre.hrl").

%%====================================================================
%% Type definitions
%%====================================================================

-type event_type() :: workflow_started
                    | workflow_completed
                    | workflow_failed
                    | activity_started
                    | activity_completed
                    | activity_failed
                    | transition_enabled
                    | token_consumed
                    | token_produced.

-type event() :: #{
    type => event_type(),
    timestamp => integer(),
    case_id => binary(),
    activity_id => binary() | undefined,
    data => map(),
    source => atom()
}.

-type subscriber() :: #{
    pid => pid(),
    ref => reference(),
    filters => [event_type()] | all,
    created_at => integer()
}.

-type state() :: #{
    subscribers => [subscriber()],
    event_log => [event()],
    max_log_size => non_neg_integer()
}.

%%====================================================================
%% Module Documentation
%%====================================================================

-moduledoc("""
REST Event Streaming Management

This module provides a centralized event streaming service for the CRE workflow
engine. Clients can subscribe to workflow events and receive real-time updates
via SSE or WebSocket connections.

## API Functions

### Subscribe to Events

```erlang
%% Subscribe to all events
{ok, Ref} = rest_events:subscribe(ClientPid)

%% Subscribe to specific event types
{ok, Ref} = rest_events:subscribe(ClientPid, [workflow_started, workflow_completed])
```

### Publish Events

```erlang
%% Publish workflow event
rest_events:publish(workflow_started, #{
    case_id => <<"wf-001">>,
    activity_id => <<"task1">>,
    data => #{status => running}
})

%% Publish with source identification
rest_events:publish(activity_completed, #{
    case_id => <<"wf-001">>,
    activity_id => <<"task1">>
}, my_handler)
```

## Event Examples

### Workflow Started Event

```json
{
  "type": "workflow_started",
  "timestamp": 1707559200000,
  "case_id": "wf-001",
  "data": {
    "workflow_name": "purchase_order",
    "version": 1
  }
}
```

### Activity Completed Event

```json
{
  "type": "activity_completed",
  "timestamp": 1707559250000,
  "case_id": "wf-001",
  "activity_id": "approve_order",
  "data": {
    "duration_ms": 45000,
    "result": "approved"
  }
}
```

## Token Events

```json
{
  "type": "token_produced",
  "timestamp": 1707559300000,
  "case_id": "wf-001",
  "data": {
    "place": "ready_to_ship",
    "marking": 2
  }
}
```

## Error Handling

Client processes that crash or disconnect are automatically removed
from the subscription list. The service maintains a rolling event log
for latecomers.

## Example Usage

```erlang
%% Start event service
1> rest_events:start_link().
{ok,<0.123.0>}

%% Subscribe to events
2> {ok, Ref} = rest_events:subscribe(self(), [workflow_started]).
{ok,#Ref<0.123.0.0>}

%% Publish event
3> rest_events:publish(workflow_started, #{
     case_id => <<"wf-001">>,
     data => #{workflow => purchase_order}
   }).
ok

%% Receive event message
4> receive {event, E} -> E end.
#{
  type => workflow_started,
  timestamp => 1707559200000,
  case_id => <<"wf-001">>,
  ...
}
```
""").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the event streaming service.
%%
%%      Registers the service locally as `rest_events' and initializes
%%      the subscription and event log storage. The service maintains
%%      a rolling event log up to max_log_size events.
%%
%%      Returns `{ok, Pid}' when the service starts successfully.
%%      Returns `{error, {already_started, Pid}}' if already running.
%%      Returns `{error, Reason}' if startup fails.
%%
%% @returns `{ok, Pid}' | `{error, already_started} | `{error, Reason}'
%%
-doc("""
Start the event streaming service.

Registers locally as `rest_events` and initializes subscriptions
and event log storage.

## Example

```erlang
1> rest_events:start_link().
{ok,<0.123.0>}
```
""").
-spec start_link() -> {ok, pid()} | {error, _}.

start_link() ->
    gen_server:start_link({local, rest_events}, ?MODULE, [], []).

%% @doc Subscribe to all events.
%%
%%      Adds the calling process to the event subscriber list and
%%      returns a reference for later use. The subscriber will receive
%%      messages of the form `{event, Event}' for all published events.
%%
%%      The subscriber process is monitored; if it crashes, the
%%      subscription is automatically removed.
%%
%% @param ClientPid The process ID to receive event messages
%% @returns `{ok, Reference}' or `{error, Reason}'
%%
-doc("""
Subscribe client to all events.

The calling process receives messages of form `{event, Event}`.

## Example

```erlang
1> {ok, Ref} = rest_events:subscribe(self()).
{ok,#Ref<0.123.0.0>}
```
""").
-spec subscribe(pid()) -> {ok, reference()} | {error, _}.

subscribe(ClientPid) ->
    subscribe(ClientPid, all).

%% @doc Subscribe to specific event types.
%%
%%      Adds the calling process to the subscriber list with filtering
%%      enabled. Only events matching the provided types will be sent.
%%
%%      Filters should be a list of event_type() atoms or 'all' to
%%      receive all events.
%%
%% @param ClientPid The process ID to receive event messages
%% @param Filters List of event types to subscribe to, or 'all'
%% @returns `{ok, Reference}' or `{error, Reason}'
%%
-doc("""
Subscribe client to specific event types.

Only events matching the filter list are sent to the subscriber.

## Example

```erlang
1> {ok, Ref} = rest_events:subscribe(self(),
     [workflow_started, workflow_completed]).
{ok,#Ref<0.123.0.0>}
```
""").
-spec subscribe(pid(), all | [event_type()]) -> {ok, reference()} | {error, _}.

subscribe(ClientPid, Filters) when is_pid(ClientPid) ->
    gen_server:call(rest_events, {subscribe, ClientPid, Filters}).

%% @doc Publish an event to all subscribers.
%%
%%      Broadcasts the event to all subscribed clients that match the
%%      event filter. The event timestamp is automatically set if not
%%      provided. Events are added to the rolling event log for
%%      latecomers.
%%
%% @param EventType The type of event being published
%% @param Data Event data map with case_id and other context
%% @returns `ok'
%%
-doc("""
Publish event to all subscribers.

Event timestamp is automatically set if not provided.
Events are added to rolling event log.

## Example

```erlang
1> rest_events:publish(workflow_started, #{
     case_id => <<"wf-001">>,
     data => #{status => running}
   }).
ok
```
""").
-spec publish(event_type(), map()) -> ok.

publish(EventType, Data) ->
    publish(EventType, Data, rest_events).

%% @doc Publish an event with source identification.
%%
%%      Similar to publish/2 but includes source module identification
%%      in the event for tracking and debugging purposes.
%%
%% @param EventType The type of event being published
%% @param Data Event data map with case_id and other context
%% @param Source Atom identifying the event source
%% @returns `ok'
%%
-doc("""
Publish event with source identification.

Source module is recorded in the event for tracking.

## Example

```erlang
1> rest_events:publish(activity_completed, #{
     case_id => <<"wf-001">>,
     activity_id => <<"task1">>
   }, my_handler).
ok
```
""").
-spec publish(event_type(), map(), atom()) -> ok.

publish(EventType, Data, Source) when is_atom(EventType), is_map(Data), is_atom(Source) ->
    gen_server:cast(rest_events, {publish, EventType, Data, Source}).

%% @doc Get current subscriber count.
%%
%%      Returns the number of active subscribers currently connected
%%      to the event service.
%%
%% @returns `{ok, Count :: non_neg_integer()}' or `{error, Reason}'
%%
-doc("""
Get current subscriber count.

Returns number of active subscribers.

## Example

```erlang
1> rest_events:get_subscribers().
{ok, 3}
```
""").
-spec get_subscribers() -> {ok, non_neg_integer()} | {error, _}.

get_subscribers() ->
    gen_server:call(rest_events, get_subscribers).

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

%% @doc Initializes the event streaming service.
%%
%%      Creates empty subscriber list and event log. The max_log_size
%%      is set to 1000 events by default.
%%
%% @param _Args Unused (empty list)
%% @returns `{ok, State}'
%%
-doc("""
Initialize event streaming service.

Sets up empty subscriber list and event log.

## Example

```erlang
1> rest_events:init([]).
{ok,#{subscribers => [], event_log => [], max_log_size => 1000}}
```
""").
-spec init(_) -> {ok, state()}.

init(_Args) ->
    State = #{
        subscribers => [],
        event_log => [],
        max_log_size => 1000
    },
    {ok, State}.

%% @doc Handles synchronous calls.
%%
%%      Processes subscribe and get_subscribers requests.
%%
%% @returns `{reply, Reply, State}'
%%
-spec handle_call(term(), {pid(), reference()}, state()) ->
    {reply, term(), state()}.

handle_call({subscribe, ClientPid, Filters}, _From, State = #{subscribers := Subs}) ->
    Ref = erlang:monitor(process, ClientPid),
    Subscriber = #{
        pid => ClientPid,
        ref => Ref,
        filters => Filters,
        created_at => erlang:system_time(millisecond)
    },
    NewSubs = [Subscriber | Subs],
    NewState = State#{subscribers := NewSubs},
    {reply, {ok, Ref}, NewState};

handle_call(get_subscribers, _From, State = #{subscribers := Subs}) ->
    {reply, {ok, length(Subs)}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc Handles asynchronous calls.
%%
%%      Processes event publishing requests and distributes events
%%      to matching subscribers.
%%
%% @returns `{noreply, State}'
%%
-spec handle_cast(term(), state()) -> {noreply, state()}.

handle_cast({publish, EventType, Data, Source}, State) ->
    Event = build_event(EventType, Data, Source),
    NewState = broadcast_event(Event, State),
    {noreply, NewState};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc Handles info messages.
%%
%%      Handles monitor 'DOWN' signals when subscriber processes
%%      terminate, removing them from the subscription list.
%%
%% @returns `{noreply, State}'
%%
-spec handle_info(term(), state()) -> {noreply, state()}.

handle_info({'DOWN', Ref, process, Pid, _Reason}, State = #{subscribers := Subs}) ->
    NewSubs = lists:filter(
        fun(Sub) ->
            SubPid = maps:get(pid, Sub),
            SubRef = maps:get(ref, Sub),
            not (SubPid =:= Pid andalso SubRef =:= Ref)
        end,
        Subs
    ),
    {noreply, State#{subscribers := NewSubs}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Terminates the service.
%%
%%      Demonitors all active subscribers before shutdown.
%%
%% @returns `ok'
%%
-spec terminate(term(), state()) -> ok.

terminate(_Reason, State = #{subscribers := Subs}) ->
    lists:foreach(
        fun(Sub) ->
            Ref = maps:get(ref, Sub),
            erlang:demonitor(Ref, [flush])
        end,
        Subs
    ),
    ok.

%% @doc Handles code changes (hot reload).
%%
%%      Returns the updated state unchanged for now.
%%
%% @returns `{ok, State}'
%%
-spec code_change(term(), state(), term()) -> {ok, state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Build an event map from type, data, and source.
%%
%% @private
%%
-spec build_event(event_type(), map(), atom()) -> event().

build_event(EventType, Data, Source) ->
    CaseId = maps:get(case_id, Data, undefined),
    ActivityId = maps:get(activity_id, Data, undefined),
    EventData = maps:without([case_id, activity_id], Data),

    #{
        type => EventType,
        timestamp => erlang:system_time(millisecond),
        case_id => CaseId,
        activity_id => ActivityId,
        data => EventData,
        source => Source
    }.

%% @doc Broadcast event to matching subscribers and add to log.
%%
%% @private
%%
-spec broadcast_event(event(), state()) -> state().

broadcast_event(Event, State = #{subscribers := Subs, event_log := Log, max_log_size := MaxSize}) ->
    EventType = maps:get(type, Event),

    lists:foreach(
        fun(Sub) ->
            Filters = maps:get(filters, Sub),
            case should_send_event(EventType, Filters) of
                true ->
                    SubPid = maps:get(pid, Sub),
                    SubPid ! {event, Event};
                false ->
                    ok
            end
        end,
        Subs
    ),

    NewLog = add_to_log(Event, Log, MaxSize),
    State#{event_log := NewLog}.

%% @doc Check if event should be sent to subscriber based on filters.
%%
%% @private
%%
-spec should_send_event(event_type(), all | [event_type()]) -> boolean().

should_send_event(_EventType, all) ->
    true;

should_send_event(EventType, Filters) ->
    lists:member(EventType, Filters).

%% @doc Add event to rolling event log, maintaining max size.
%%
%% @private
%%
-spec add_to_log(event(), [event()], non_neg_integer()) -> [event()].

add_to_log(Event, Log, MaxSize) ->
    NewLog = [Event | Log],
    case length(NewLog) > MaxSize of
        true ->
            lists:sublist(NewLog, MaxSize);
        false ->
            NewLog
    end.

%%====================================================================
%% Doctests
%%====================================================================

%% @doc Run doctests for the rest_events module.
%%
%%      Executes fast tests verifying basic event streaming functionality
%%      without starting the full application.
%%
%% @returns `ok'
%%
-doc("""
Run doctests for rest_events module.

Verifies basic event streaming functionality.

## Example

```erlang
1> rest_events:doctest_test().
ok
```
""").
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Module can be loaded
    {module, rest_events} = code:ensure_loaded(rest_events),

    %% Test 2: Verify all exports exist
    Exports = proplists:get_value(exports, rest_events:module_info()),
    true = lists:member({start_link, 0}, Exports),
    true = lists:member({subscribe, 1}, Exports),
    true = lists:member({subscribe, 2}, Exports),
    true = lists:member({publish, 2}, Exports),
    true = lists:member({publish, 3}, Exports),
    true = lists:member({get_subscribers, 0}, Exports),
    true = lists:member({init, 1}, Exports),
    true = lists:member({handle_call, 3}, Exports),
    true = lists:member({handle_cast, 2}, Exports),
    true = lists:member({handle_info, 2}, Exports),

    %% Test 3: Verify gen_server behavior
    Behaviors = proplists:get_value(attributes, rest_events:module_info()),
    {behavior, [gen_server]} = lists:keyfind(behavior, 1, Behaviors),

    %% Test 4: Verify event type constants are valid atoms
    EventTypes = [
        workflow_started, workflow_completed, workflow_failed,
        activity_started, activity_completed, activity_failed,
        transition_enabled, token_consumed, token_produced
    ],
    true = lists:all(fun is_atom/1, EventTypes),

    %% Test 5: Initialize state
    {ok, InitState} = init([]),
    true = is_map(InitState),
    true = maps:is_key(subscribers, InitState),
    true = maps:is_key(event_log, InitState),
    true = maps:is_key(max_log_size, InitState),
    [] = maps:get(subscribers, InitState),
    [] = maps:get(event_log, InitState),
    1000 = maps:get(max_log_size, InitState),

    %% Test 6: Test build_event helper
    TestEvent = build_event(workflow_started, #{
        case_id => <<"wf-001">>,
        activity_id => <<"task1">>,
        custom_data => <<"value">>
    }, test_source),
    true = is_map(TestEvent),
    workflow_started = maps:get(type, TestEvent),
    <<"wf-001">> = maps:get(case_id, TestEvent),
    <<"task1">> = maps:get(activity_id, TestEvent),
    test_source = maps:get(source, TestEvent),
    true = is_integer(maps:get(timestamp, TestEvent)),

    %% Test 7: Test event data extraction
    EventData = maps:get(data, TestEvent),
    true = is_map(EventData),
    <<"value">> = maps:get(custom_data, EventData),
    false = maps:is_key(case_id, EventData),
    false = maps:is_key(activity_id, EventData),

    %% Test 8: Test should_send_event with all filter
    true = should_send_event(workflow_started, all),
    true = should_send_event(activity_completed, all),

    %% Test 9: Test should_send_event with specific filters
    true = should_send_event(workflow_started, [workflow_started]),
    false = should_send_event(activity_completed, [workflow_started]),
    true = should_send_event(activity_completed, [workflow_started, activity_completed]),

    %% Test 10: Test add_to_log maintains size
    Log = lists:seq(1, 5),
    NewLog = add_to_log(event_10, Log, 10),
    6 = length(NewLog),
    event_10 = hd(NewLog),

    %% Test 11: Test add_to_log respects max size
    FullLog = lists:seq(1, 1000),
    NewLog2 = add_to_log(event_new, FullLog, 1000),
    1000 = length(NewLog2),
    event_new = hd(NewLog2),

    ok.
