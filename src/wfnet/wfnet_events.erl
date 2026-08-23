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
%% @doc Workflow Event System for Observability
%%
%% This module provides a pub/sub event system for workflow observability.
%% It allows workflow components to emit events and subscribers to receive
%% notifications based on event type filters.
%%
%% <h3>Event Types</h3>
%%
%% <ul>
%%   <li><b>case_created:</b> New workflow case instance created</li>
%%   <li><b>case_started:</b> Workflow case started execution</li>
%%   <li><b>case_completed:</b> Workflow case completed successfully</li>
%%   <li><b>case_suspended:</b> Workflow case suspended (paused)</li>
%%   <li><b>case_resumed:</b> Workflow case resumed from suspension</li>
%%   <li><b>transition_enabled:</b> Petri net transition became enabled</li>
%%   <li><b>transition_fired:</b> Petri net transition fired</li>
%%   <li><b>transition_failed:</b> Transition firing failed</li>
%%   <li><b>token_produced:</b> Token produced to a place</li>
%%   <li><b>token_consumed:</b> Token consumed from a place</li>
%%   <li><b>error:</b> Error event</li>
%%   <li><b>warning:</b> Warning event</li>
%%   <li><b>info:</b> Informational event</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Emitting an event:
%% ```erlang
%% > Event = wfnet_events:emit_event(
%%     case_started,
%%     <<"case-123">>,
%%     #{spec_id => <<"order_fulfillment">>}
%% ).
%% #{id => <<"evt-...">>, type => case_started, ...}
%% ```
%%
%% Subscribing to events:
%% ```erlang
%% > Subscriber = self(),
%% > Filter = #{case_id => <<"case-123">>},
%% > ok = wfnet_events:subscribe(Subscriber, Filter).
%% ok
%%
%% %% Receive event
%% > receive
%%     #wfnet_event{type = case_started} = E ->
%%         io:format("Event: ~p~n", [wfnet_events:format_event(E)])
%% end.
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_events).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

%% Event API
-export([emit_event/1, emit_event/2, emit_event/3]).
-export([subscribe/0, subscribe/1, subscribe/2]).
-export([unsubscribe/0, unsubscribe/1]).
-export([notify_subscribers/2]).
-export([event_filter_match/2]).
-export([format_event/1]).
-export([init_table/0]).

%% Utility exports
-export([generate_event_id/0]).
-export([get_subscribers/0]).
-export([get_event_history/0]).
-export([clear_history/0]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Event type atom.
%%
%% Defines all possible workflow event types for categorization
%% and filtering.
%%--------------------------------------------------------------------
-type event_type() ::
    case_created | case_started | case_completed | case_suspended | case_resumed |
    transition_enabled | transition_fired | transition_failed |
    token_produced | token_consumed |
    error | warning | info.

%%--------------------------------------------------------------------
%% @doc Case identifier for workflow instances.
%%
%% Binary identifier uniquely identifying a workflow case instance.
%% Use `undefined' for events not associated with a specific case.
%%--------------------------------------------------------------------
-type case_id() :: binary() | undefined.

%%--------------------------------------------------------------------
%% @doc Subscription information.
%%
%% Maintains subscriber process with filter criteria and subscription time.
%%--------------------------------------------------------------------
-type subscription() :: #{
    subscriber := pid(),
    filter := event_filter(),
    subscribe_time := integer()
}.

%%--------------------------------------------------------------------
%% @doc Event filter map for subscription filtering.
%%
%% Keys can include:
%% - `type': Event type atom or list of event types
%% - `case_id': Specific case ID or `undefined' for all cases
%% - `min_timestamp': Only events after this timestamp
%%
%% An empty map matches all events.
%%--------------------------------------------------------------------
-type event_filter() :: #{
    type => event_type() | [event_type()],
    case_id => case_id(),
    min_timestamp => integer()
}.

%%--------------------------------------------------------------------
%% @doc Subscription record (internal).
%%
%% Internal record for maintaining subscriber information with filter criteria.
%%--------------------------------------------------------------------
-record(subscription, {
    subscriber :: pid(),
    filter :: event_filter(),
    subscribe_time :: integer()
}).

%%--------------------------------------------------------------------
%% @doc Event record.
%%
%% Core event structure emitted by the workflow system.
%%--------------------------------------------------------------------
-record(wfnet_event, {
    id :: binary(),
    type :: event_type(),
    timestamp :: integer(),
    case_id :: case_id(),
    data :: map()
}).

-export_type([event_type/0, case_id/0, event_filter/0, subscription/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Emit a workflow event with minimal data.
%%
%% Creates an event with the given type and empty data map.
%% The case_id is set to `undefined'.
%%
%% @param EventType The type of event to emit
%% @returns The emitted event record
%%
%% @end
%%--------------------------------------------------------------------
-spec emit_event(EventType :: event_type()) -> #wfnet_event{}.
emit_event(EventType) ->
    emit_event(EventType, undefined, #{}).

%%--------------------------------------------------------------------
%% @doc Emit a workflow event with case ID.
%%
%% Creates an event associated with a specific workflow case.
%%
%% @param EventType The type of event to emit
%% @param CaseId The workflow case ID (or `undefined')
%% @returns The emitted event record
%%
%% @end
%%--------------------------------------------------------------------
-spec emit_event(EventType :: event_type(), CaseId :: case_id()) -> #wfnet_event{}.
emit_event(EventType, CaseId) ->
    emit_event(EventType, CaseId, #{}).

%%--------------------------------------------------------------------
%% @doc Emit a workflow event with full data.
%%
%% Creates a complete event with type, case ID, and associated data.
%% The event is broadcast to all matching subscribers.
%%
%% <h4>Example</h4>
%% ```erlang
%% > wfnet_events:emit_event(
%%     transition_fired,
%%     <<"case-123">>,
%%     #{
%%         transition => submit_order,
%%         input_tokens => [order_form],
%%         output_tokens => [order_confirmed]
%%     }
%% ).
%% '''
%%
%% @param EventType The type of event to emit
%% @param CaseId The workflow case ID (or `undefined')
%% @param Data Event-specific data map
%% @returns The emitted event record
%%
%% @end
%%--------------------------------------------------------------------
-spec emit_event(EventType :: event_type(), CaseId :: case_id(), Data :: map()) ->
    #wfnet_event{}.
emit_event(EventType, CaseId, Data) ->
    Event = #wfnet_event{
        id = generate_event_id(),
        type = EventType,
        timestamp = erlang:system_time(millisecond),
        case_id = CaseId,
        data = Data
    },

    %% Log the event
    log_event(Event),

    %% Notify matching subscribers
    notify_subscribers(Event, get_subscribers_state()),

    %% Store in history
    store_event(Event),

    Event.

%%--------------------------------------------------------------------
%% @doc Subscribe to all workflow events.
%%
%% The calling process will receive all emitted events as messages.
%% Events are sent as `#wfnet_event{}' records.
%%
%% <h4>Example</h4>
%% ```erlang
%% > ok = wfnet_events:subscribe(),
%% > receive
%%     #wfnet_event{type = Type} = Event ->
%%         io:format("Got event: ~p~n", [Type])
%% end.
%% '''
%%
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe() -> ok.
subscribe() ->
    subscribe(self(), #{}).

%%--------------------------------------------------------------------
%% @doc Subscribe to filtered workflow events.
%%
%% The calling process will receive events matching the filter criteria.
%%
%% <h4>Filter Options</h4>
%% <ul>
%%   <li>`type': Single event type or list of types to match</li>
%%   <li>`case_id': Specific case ID to match</li>
%%   <li>`min_timestamp': Only events after this timestamp</li>
%% </ul>
%%
%% <h4>Example</h4>
%% ```erlang
%% > %% Subscribe to all events for a specific case
%% > Filter = #{case_id => <<"case-123">>},
%% > ok = wfnet_events:subscribe(Filter).
%%
%% > %% Subscribe to specific event types
%% > Filter2 = #{type => [transition_fired, transition_failed]},
%% > ok = wfnet_events:subscribe(Filter2).
%% '''
%%
%% @param Filter Event filter map (empty map matches all)
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe(Filter :: event_filter()) -> ok.
subscribe(Filter) ->
    subscribe(self(), Filter).

%%--------------------------------------------------------------------
%% @doc Register a subscriber process with filter.
%%
%% Registers the given process to receive events matching the filter.
%%
%% @param Subscriber The process ID to receive events
%% @param Filter Event filter map
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe(Subscriber :: pid(), Filter :: event_filter()) -> ok.
subscribe(Subscriber, Filter) ->
    Subscription = #subscription{
        subscriber = Subscriber,
        filter = Filter,
        subscribe_time = erlang:system_time(millisecond)
    },
    ets:insert(?MODULE, {Subscriber, Subscription}),
    monitor(process, Subscriber),
    ok.

%%--------------------------------------------------------------------
%% @doc Unsubscribe the calling process from all events.
%%
%% Removes the calling process from the subscriber registry.
%%
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe() -> ok.
unsubscribe() ->
    unsubscribe(self()).

%%--------------------------------------------------------------------
%% @doc Unsubscribe a specific process from all events.
%%
%% Removes the given process from the subscriber registry.
%%
%% @param Subscriber The process ID to unsubscribe
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(Subscriber :: pid()) -> ok.
unsubscribe(Subscriber) ->
    ets:delete(?MODULE, Subscriber),
    ok.

%%--------------------------------------------------------------------
%% @doc Notify all subscribers of an event.
%%
%% Sends the event to all subscribers whose filters match.
%% Non-matching subscribers are skipped.
%%
%% <h4>Delivery</h4>
%% Events are delivered as asynchronous messages:
%% ```erlang
%% {wfnet_event, #wfnet_event{...}}
%% '''
%%
%% @param Event The event to broadcast
%% @param Subscribers Map of subscribers to their subscriptions
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec notify_subscribers(Event :: #wfnet_event{},
                         Subscribers :: #{pid() := #subscription{}}) -> ok.
notify_subscribers(Event, Subscribers) ->
    maps:fold(fun(_Pid, #subscription{subscriber = Sub, filter = Filter}, _Acc) ->
        case event_filter_match(Event, Filter) of
            true ->
                Sub ! {wfnet_event, Event},
                ok;
            false ->
                ok
        end
    end, ok, Subscribers),
    ok.

%%--------------------------------------------------------------------
%% @doc Check if an event matches a filter.
%%
%% Tests the event against all filter criteria.
%% All criteria in the filter must match for `true' to be returned.
%%
%% <h4>Filter Matching Rules</h4>
%% <ul>
%%   <li>If `type' is a list, matches if event type is in the list</li>
%%   <li>If `type' is a single atom, matches if event type equals it</li>
%%   <li>If `case_id' is present, matches exactly</li>
%%   <li>If `min_timestamp' is present, event must be newer</li>
%% </ul>
%%
%% @param Event The event to test
%% @param Filter The filter criteria
%% @returns true if event matches all filter criteria
%%
%% @end
%%--------------------------------------------------------------------
-spec event_filter_match(Event :: #wfnet_event{}, Filter :: event_filter()) -> boolean().
event_filter_match(#wfnet_event{type = EventType, case_id = CaseId, timestamp = TS},
                   Filter) ->
    TypeMatch = case maps:get(type, Filter, undefined) of
        undefined -> true;
        Types when is_list(Types) -> lists:member(EventType, Types);
        SingleType -> EventType =:= SingleType
    end,

    CaseIdMatch = case maps:get(case_id, Filter, undefined) of
        undefined -> true;
        FilterCaseId -> CaseId =:= FilterCaseId
    end,

    TimeMatch = case maps:get(min_timestamp, Filter, undefined) of
        undefined -> true;
        MinTS -> TS >= MinTS
    end,

    TypeMatch andalso CaseIdMatch andalso TimeMatch.

%%--------------------------------------------------------------------
%% @doc Format an event for human-readable logging.
%%
%% Returns an iolist suitable for logging or display.
%%
%% <h4>Example</h4>
%% ```erlang
%% > Event = wfnet_events:emit_event(transition_fired, <<"case-1">>, #{}),
%% > io:format("~s~n", [wfnet_events:format_event(Event)]).
%% [2025-02-10 13:25:00.123] case-1: transition_fired
%% '''
%%
%% @param Event The event to format
%% @returns Formatted iolist
%%
%% @end
%%--------------------------------------------------------------------
-spec format_event(Event :: #wfnet_event{}) -> iolist().
format_event(#wfnet_event{timestamp = TS, case_id = CaseId, type = Type, data = Data}) ->
    DateTimeStr = format_timestamp(TS),
    CaseIdStr = format_case_id(CaseId),
    DataStr = format_data(Data),
    io_lib:format("[~s] ~s: ~p~s", [DateTimeStr, CaseIdStr, Type, DataStr]).

%%--------------------------------------------------------------------
%% @doc Generate a unique event ID.
%%
%% Creates a cryptographically unique event identifier using
%% timestamp and random components.
%%
%% @returns Binary event ID
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_event_id() -> binary().
generate_event_id() ->
    TS = erlang:system_time(millisecond),
    Rand = rand:uniform(16#ffffffff),
    iolist_to_binary(io_lib:format("evt-~p-~8.16.0b", [TS, Rand])).

%%--------------------------------------------------------------------
%% @doc Get all current subscribers.
%%
%% Returns a map of all registered subscribers and their subscriptions.
%%
%% @returns Map of subscriber pids to subscription records
%%
%% @end
%%--------------------------------------------------------------------
-spec get_subscribers() -> #{pid() := #subscription{}}.
get_subscribers() ->
    get_subscribers_state().

%%--------------------------------------------------------------------
%% @doc Get event history.
%%
%% Returns the list of recently emitted events, ordered newest first.
%% History size is limited by `max_history' (default: 1000).
%%
%% @returns List of historical events
%%
%% @end
%%--------------------------------------------------------------------
-spec get_event_history() -> [#wfnet_event{}].
get_event_history() ->
    case ets:lookup(?MODULE, history) of
        [{_, History}] -> History;
        [] -> []
    end.

%%--------------------------------------------------------------------
%% @doc Clear event history.
%%
%% Removes all stored events from history.
%%
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_history() -> ok.
clear_history() ->
    ets:insert(?MODULE, {history, []}),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Log event based on type and verbosity.
%%--------------------------------------------------------------------
log_event(#wfnet_event{type = Type} = Event) ->
    case Type of
        error ->
            ?LOG_ERROR("Workflow error: ~s", [format_event(Event)]);
        warning ->
            ?LOG_WARNING("Workflow warning: ~s", [format_event(Event)]);
        info ->
            ?LOG_INFO("Workflow info: ~s", [format_event(Event)]);
        _ ->
            ?LOG_DEBUG("Workflow event: ~s", [format_event(Event)])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Store event in history with size limit.
%%--------------------------------------------------------------------
store_event(Event) ->
    [{_, History}] = ets:lookup(?MODULE, history),
    NewHistory = [Event | History],
    LimitedHistory = case length(NewHistory) > 1000 of
        true -> lists:sublist(NewHistory, 1000);
        false -> NewHistory
    end,
    ets:insert(?MODULE, {history, LimitedHistory}),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Get subscribers state from ETS.
%%--------------------------------------------------------------------
get_subscribers_state() ->
    Subscriptions = ets:tab2list(?MODULE),
    SubscriptionsFiltered = lists:filter(
        fun
            ({_, #subscription{}}) -> true;
            ({history, _}) -> false;
            (_) -> false
        end,
        Subscriptions
    ),
    maps:from_list(SubscriptionsFiltered).

%%--------------------------------------------------------------------
%% @private
%% @doc Format timestamp as ISO 8601 string.
%%--------------------------------------------------------------------
format_timestamp(Millis) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:system_time_to_universal_time(Millis div 1000, seconds),
    MillisPart = Millis rem 1000,
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
                  [Year, Month, Day, Hour, Min, Sec, MillisPart]).

%%--------------------------------------------------------------------
%% @private
%% @doc Format case ID for display.
%%--------------------------------------------------------------------
format_case_id(undefined) -> <<"no-case">>;
format_case_id(CaseId) when is_binary(CaseId) -> CaseId;
format_case_id(CaseId) -> list_to_binary(io_lib:format("~p", [CaseId])).

%%--------------------------------------------------------------------
%% @private
%% @doc Format event data for display.
%%--------------------------------------------------------------------
format_data(Data) when map_size(Data) =:= 0 -> [];
format_data(Data) ->
    [" ", io_lib:format("~p", [Data])].

%%====================================================================
%% ETS Table Management
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initialize the ETS table for event system.
%%
%% Called by application supervisor during startup.
%%--------------------------------------------------------------------
-spec init_table() -> ok.
init_table() ->
    case ets:whereis(?MODULE) of
        undefined ->
            ets:new(?MODULE, [named_table, set, public, {read_concurrency, true}]),
            ets:insert(?MODULE, {history, []}),
            ok;
        _ ->
            ok
    end.