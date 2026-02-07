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

-module(wf_timerq).
-compile({no_auto_import, [size/1]}).

-moduledoc """
Deadline queue for time-based token injection (pure).

Timers store arbitrary events (typically `{inject, Place, Token}` tuples)
and fire based on monotonic millisecond deadlines. This is a pure module:
all operations return updated state without side effects.

```erlang
> Q0 = wf_timerq:new().
_
> wf_timerq:is_empty(Q0).
true
> wf_timerq:size(Q0).
0
> wf_timerq:peek(Q0).
undefined
> Q1 = wf_timerq:arm(Q0, k1, 1100, {inject, p1, a}),
.. Q2 = wf_timerq:arm(Q1, k2, 1200, {inject, p2, b}).
_
> wf_timerq:size(Q2).
2
> wf_timerq:peek(Q2).
{1100,{inject,p1,a}}
> {E0, Qx} = wf_timerq:poll(Q2, 1000).
_
> E0.
[]
> wf_timerq:size(Qx).
2
> {E1, Q3} = wf_timerq:poll(Q2, 1100).
_
> E1.
[{inject,p1,a}]
> wf_timerq:peek(Q3).
{1200,{inject,p2,b}}
> Q4 = wf_timerq:disarm(Q2, k2).
_
> wf_timerq:size(Q4).
1
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Timer queue API
-export([new/0]).
-export([arm/4, disarm/2, poll/2]).
-export([is_empty/1, size/1, peek/1]).

%% Extended timer API
-export([arm_from_now/4, get_deadline/2, clear_all/1, arm_duration/4]).

%%====================================================================
%% Types
%%====================================================================

%% Unique key identifying a timer
-type timer_key() :: term().

%% Deadline as monotonic milliseconds
-type deadline() :: integer().

%% Duration in milliseconds
-type duration_ms() :: non_neg_integer().

%% ISO 8601 duration string (e.g., "PT30S", "PT1H30M", "P1DT12H")
-type iso8601_duration() :: binary() | string().

%% Timer event can be any Erlang term
%% Typical usage: {inject, Place, Token} for token injection
-type timer_event() :: term().

%% Error reasons for duration parsing
-type duration_error() :: bad_duration.

%% Timer entry: {Deadline, Event}
-type timer_entry() :: {deadline(), timer_event()}.

%% Deadline-sorted list of timer entries (ascending order)
-type deadline_list() :: [timer_entry()].

%% Map from timer keys to deadline entries
%% This enables O(log n) lookup by key and O(1) key replacement
-type key_map() :: #{timer_key() => timer_entry()}.

%% Timer queue combining map for key lookup and list for deadline ordering
-opaque timerq() :: #{keys := key_map(), deadlines := deadline_list()}.

-export_type([timerq/0, timer_key/0, deadline/0, timer_event/0]).
-export_type([duration_ms/0, iso8601_duration/0, duration_error/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new empty timer queue.
%%
%% @returns An empty timer queue
%%
%% @end
%%--------------------------------------------------------------------
-spec new() -> timerq().

new() ->
    #{keys => #{}, deadlines => []}.

%%--------------------------------------------------------------------
%% @doc Arms a new timer or replaces an existing timer with the same key.
%%
%% If a timer with the same key already exists, it is replaced with the
%% new deadline and event. The deadlines list is maintained in sorted order
%% (ascending) for efficient polling.
%%
%% @param TimerQ The current timer queue
%% @param Key Unique identifier for this timer
%% @param Deadline Monotonic millisecond timestamp when timer should fire
%% @param Event The event to fire when deadline expires
%% @returns Updated timer queue with timer armed
%%
%% @end
%%--------------------------------------------------------------------
-spec arm(TimerQ :: timerq(), Key :: timer_key(),
         Deadline :: deadline(), Event :: timer_event()) -> timerq().

arm(#{keys := KeyMap, deadlines := DeadlineList}, Key, Deadline, Event) ->
    %% Create new timer entry
    Entry = {Deadline, Event},

    %% Remove any existing timer with the same key from deadlines list
    NewDeadlineList = case maps:find(Key, KeyMap) of
        {ok, {OldDeadline, _OldEvent}} ->
            %% Key exists: remove its old entry from deadlines list
            lists:keydelete(OldDeadline, 1, DeadlineList);
        error ->
            %% Key doesn't exist: keep list as is
            DeadlineList
    end,

    %% Insert new entry into deadlines list (sorted by deadline)
    UpdatedDeadlineList = insert_sorted(Entry, NewDeadlineList),

    %% Update the key map
    UpdatedKeyMap = KeyMap#{Key => Entry},

    #{keys => UpdatedKeyMap, deadlines => UpdatedDeadlineList}.

%%--------------------------------------------------------------------
%% @doc Disarms (removes) a timer by its key.
%%
%% If no timer with the given key exists, the queue is returned unchanged.
%% This is a no-op for non-existent keys.
%%
%% @param TimerQ The current timer queue
%% @param Key The key of the timer to remove
%% @returns Updated timer queue with timer removed (or unchanged if not found)
%%
%% @end
%%--------------------------------------------------------------------
-spec disarm(TimerQ :: timerq(), Key :: timer_key()) -> timerq().

disarm(#{keys := KeyMap, deadlines := DeadlineList}, Key) ->
    case maps:take(Key, KeyMap) of
        {{OldDeadline, _OldEvent}, UpdatedKeyMap} ->
            %% Key existed: remove from deadlines list and update key map
            UpdatedDeadlineList = lists:keydelete(OldDeadline, 1, DeadlineList),
            #{keys => UpdatedKeyMap, deadlines => UpdatedDeadlineList};
        error ->
            %% Key didn't exist: return queue unchanged
            #{keys => KeyMap, deadlines => DeadlineList}
    end.

%%--------------------------------------------------------------------
%% @doc Polls the timer queue for expired timers.
%%
%% Returns all events whose deadline is less than or equal to Now, along
%% with the updated queue having those entries removed. Events are returned
%% in deadline order (oldest first).
%%
%% @param TimerQ The current timer queue
%% @param Now Current monotonic millisecond timestamp
%% @returns {ReadyEvents, UpdatedQ} where ReadyEvents is list of expired
%%          timer events in deadline order, and UpdatedQ is the queue with
%%          expired entries removed
%%
%% @end
%%--------------------------------------------------------------------
-spec poll(TimerQ :: timerq(), Now :: deadline()) ->
          {[timer_event()], timerq()}.

poll(#{keys := KeyMap, deadlines := DeadlineList}, Now) ->
    %% Split deadlines list into expired and remaining
    {ExpiredEntries, RemainingDeadlines} = take_expired(DeadlineList, Now, []),

    %% Extract events from expired entries
    ReadyEvents = [Event || {_Deadline, Event} <- ExpiredEntries],

    %% Remove expired keys from key map
    %% We need to find which keys correspond to expired entries
    %% Since we don't have a reverse mapping, we rebuild the map
    UpdatedKeyMap = maps:filter(fun(_K, {D, _E}) -> D > Now end, KeyMap),

    {ReadyEvents, #{keys => UpdatedKeyMap, deadlines => RemainingDeadlines}}.

%%--------------------------------------------------------------------
%% @doc Checks if the timer queue is empty.
%%
%% @param TimerQ The timer queue to check
%% @returns true if queue has no timers, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_empty(TimerQ :: timerq()) -> boolean().

is_empty(#{keys := KeyMap}) ->
    map_size(KeyMap) =:= 0.

%%--------------------------------------------------------------------
%% @doc Returns the number of timers in the queue.
%%
%% @param TimerQ The timer queue
%% @returns Count of timers in the queue
%%
%% @end
%%--------------------------------------------------------------------
-spec size(TimerQ :: timerq()) -> non_neg_integer().

size(#{keys := KeyMap}) ->
    maps:size(KeyMap).

%%--------------------------------------------------------------------
%% @doc Peeks at the next timer to expire without removing it.
%%
%% Returns the deadline and event for the earliest timer, or undefined
%% if the queue is empty.
%%
%% @param TimerQ The timer queue
%% @returns {Deadline, Event} for the next timer, or undefined if empty
%%
%% @end
%%--------------------------------------------------------------------
-spec peek(TimerQ :: timerq()) -> {deadline(), timer_event()} | undefined.

peek(#{deadlines := []}) ->
    undefined;
peek(#{deadlines := [{Deadline, Event} | _]}) ->
    {Deadline, Event}.

%%--------------------------------------------------------------------
%% @doc Arms a timer with a deadline relative to the current monotonic time.
%%
%% This is a convenience function that calculates the absolute deadline
%% by adding the duration to the current monotonic time.
%%
%% ```erlang
%% > Q0 = wf_timerq:new(),
%% > Q1 = wf_timerq:arm_from_now(Q0, k1, 5000, {inject, p1, a}),
%% > wf_timerq:size(Q1).
%% 1
%% ```
%%
%% @param TimerQ The current timer queue
%% @param Key Unique identifier for this timer
%% @param DurationMs Milliseconds from now when timer should fire
%% @param Event The event to fire when deadline expires
%% @returns Updated timer queue with timer armed
%%
%% @end
%%--------------------------------------------------------------------
-spec arm_from_now(TimerQ :: timerq(), Key :: timer_key(),
                   DurationMs :: duration_ms(), Event :: timer_event()) -> timerq().

arm_from_now(TimerQ, Key, DurationMs, Event) ->
    Now = erlang:monotonic_time(millisecond),
    Deadline = Now + DurationMs,
    arm(TimerQ, Key, Deadline, Event).

%%--------------------------------------------------------------------
%% @doc Gets the deadline for a timer by its key.
%%
%% Returns the deadline associated with the given key, or undefined if
%% no timer with that key exists.
%%
%% ```erlang
%% > Q0 = wf_timerq:new(),
%% > Q1 = wf_timerq:arm(Q0, k1, 1500, {inject, p1, a}),
%% > wf_timerq:get_deadline(Q1, k1).
%% {ok, 1500}
%% > wf_timerq:get_deadline(Q1, k999).
%% undefined
%% ```
%%
%% @param TimerQ The timer queue
%% @param Key The key to look up
%% @returns {ok, Deadline} if key exists, undefined otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec get_deadline(TimerQ :: timerq(), Key :: timer_key()) ->
          {ok, deadline()} | undefined.

get_deadline(#{keys := KeyMap}, Key) ->
    case maps:find(Key, KeyMap) of
        {ok, {Deadline, _Event}} -> {ok, Deadline};
        error -> undefined
    end.

%%--------------------------------------------------------------------
%% @doc Clears all timers from the queue.
%%
%% Returns a new empty timer queue. This is equivalent to calling `new/0`
%% but provided for convenience and API completeness.
%%
%% ```erlang
%% > Q0 = wf_timerq:new(),
%% > Q1 = wf_timerq:arm(Q0, k1, 1500, {inject, p1, a}),
%% > wf_timerq:size(Q1).
%% 1
%% > Q2 = wf_timerq:clear_all(Q1),
%% > wf_timerq:size(Q2).
%% 0
%% ```
%%
%% @param TimerQ The timer queue to clear
%% @returns A new empty timer queue
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_all(TimerQ :: timerq()) -> timerq().

clear_all(_TimerQ) ->
    new().

%%--------------------------------------------------------------------
%% @doc Arms a timer with an ISO 8601 duration string.
%%
%% Parses an ISO 8601 duration string and arms a timer with that duration
%% from the current monotonic time.
%%
%% Supported formats:
%% - PTnS - seconds (e.g., PT30S = 30 seconds)
%% - PTnM - minutes (e.g., PT5M = 5 minutes)
%% - PTnH - hours (e.g., PT2H = 2 hours)
%% - PnD - days (e.g., P3D = 3 days)
%% - Combined: PnDTnHnMnS (e.g., P1DT2H30M45S = 1 day 2 hours 30 minutes 45 seconds)
%%
%% ```erlang
%% > Q0 = wf_timerq:new(),
%% > {ok, Q1} = wf_timerq:arm_duration(Q0, k1, <<"PT30S">>, {inject, p1, a}),
%% > wf_timerq:size(Q1).
%% 1
%% > {error, _} = wf_timerq:arm_duration(Q0, k2, <<"invalid">>, {inject, p2, b}).
%% ```
%%
%% @param TimerQ The current timer queue
%% @param Key Unique identifier for this timer
%% @param Duration ISO 8601 duration string (binary or list)
%% @param Event The event to fire when deadline expires
%% @returns {ok, UpdatedQ} on success, {error, bad_duration} on parse failure
%%
%% @end
%%--------------------------------------------------------------------
-spec arm_duration(TimerQ :: timerq(), Key :: timer_key(),
                   Duration :: iso8601_duration(), Event :: timer_event()) ->
          {ok, timerq()} | {error, duration_error()}.

arm_duration(TimerQ, Key, Duration, Event) ->
    case parse_iso8601_duration(Duration) of
        {ok, DurationMs} ->
            {ok, arm_from_now(TimerQ, Key, DurationMs, Event)};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Parses an ISO 8601 duration string to milliseconds.
%%
%% Supports the following format components:
%% - PnD - days (e.g., P3D = 3 days)
%% - PTnH - hours (e.g., PT2H = 2 hours)
%% - PTnM - minutes (e.g., PT30M = 30 minutes)
%% - PTnS - seconds (e.g., PT45S = 45 seconds)
%%
%% Components can be combined: PnDTnHnMnS
%% Note: Weeks (PnW) are not supported for simplicity.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_iso8601_duration(iso8601_duration()) ->
          {ok, duration_ms()} | {error, duration_error()}.

parse_iso8601_duration(Duration) when is_binary(Duration) ->
    parse_iso8601_duration(binary_to_list(Duration));
parse_iso8601_duration(Duration) when is_list(Duration) ->
    try
        {Days, TimePart} = parse_date_part(Duration),
        {Hours, Mins, Secs} = parse_time_part(TimePart),
        TotalMs = (Days * 24 * 3600 + Hours * 3600 + Mins * 60 + Secs) * 1000,
        {ok, TotalMs}
    catch
        _:_ -> {error, bad_duration}
    end.

%% @private
%% @doc Parses the date part (PnD or PT) from an ISO 8601 duration.
%%
%% Returns {Days, RemainingString}. Days defaults to 0 if not specified.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_date_part(string()) -> {non_neg_integer(), string()}.

parse_date_part([$P, $T | Rest]) ->
    {0, Rest};  %% No days, just time part
parse_date_part([$P | Rest]) ->
    %% Look for D followed by T or end
    case parse_digits_until(Rest, $D) of
        {Days, [$T | TimeRest]} -> {Days, TimeRest};
        {Days, []} -> {Days, []};  %% No time part
        {0, _TimeRest} -> {0, Rest};  %% No D marker, treat as time part
        _ -> erlang:error(bad_duration)
    end;
parse_date_part(_) ->
    erlang:error(bad_duration).

%% @private
%% @doc Parses the time part (TnHnMnS) from an ISO 8601 duration.
%%
%% Returns {Hours, Minutes, Seconds}. All components default to 0.
%%
%% This function extracts all time components in a single pass by scanning
%% for the H, M, S delimiters and extracting the numbers that precede them.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_time_part(string()) -> {non_neg_integer(), non_neg_integer(), non_neg_integer()}.

parse_time_part([]) ->
    {0, 0, 0};
parse_time_part(String) ->
    %% Extract all components by finding each delimiter
    Hours = extract_component(String, $H),
    Mins = extract_component(String, $M),
    Secs = extract_component(String, $S),
    {Hours, Mins, Secs}.

%% @private
%% @doc Extracts a time component value from a string by finding the delimiter.
%%
%% For example, extract_component("30S", $S) returns 30.
%% If the delimiter is not found, returns 0.
%%
%% Scans the string for the specified delimiter and extracts the number
%% immediately preceding it. Other delimiters (H, M, S) are treated as
%% non-digits and skipped over.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_component(string(), char()) -> non_neg_integer().

extract_component(String, Delimiter) ->
    case extract_component(String, Delimiter, [], false) of
        {ok, Value} -> Value;
        {error, _} -> 0
    end.

extract_component([], _Delimiter, [], _FoundDigit) ->
    {error, not_found};
extract_component([], _Delimiter, Digits, true) ->
    %% String ended while accumulating digits - incomplete, return error
    {error, incomplete};
extract_component([Delimiter | _Rest], Delimiter, [], _FoundDigit) ->
    %% Delimiter found but no digits before it
    {ok, 0};
extract_component([Delimiter | _Rest], Delimiter, Digits, _FoundDigit) ->
    %% Delimiter found after digits
    {ok, list_to_integer(lists:reverse(Digits))};
extract_component([Ch | Rest], Delimiter, Digits, FoundDigit) when Ch >= $0, Ch =< $9 ->
    %% Accumulate digits
    extract_component(Rest, Delimiter, [Ch | Digits], true);
extract_component([Ch | Rest], Delimiter, Digits, _FoundDigit)
  when Ch =:= $H; Ch =:= $M; Ch =:= $S ->
    %% Hit a time delimiter - if it's our delimiter, handle it above
    %% If it's a different delimiter, reset digits and continue
    case Ch of
        Delimiter ->
            %% This case should be handled by earlier clauses
            {error, unexpected};
        _ ->
            %% Different delimiter - the accumulated digits belong to another component
            %% Reset and continue looking for our delimiter
            extract_component(Rest, Delimiter, [], false)
    end;
extract_component([_Ch | Rest], Delimiter, [], _FoundDigit) ->
    %% Skip non-digit, non-delimiter characters
    extract_component(Rest, Delimiter, [], false);
extract_component([_Ch | _Rest], _Delimiter, _Digits, true) ->
    %% Non-digit found after accumulating some - wrong delimiter sequence
    {error, wrong_order}.

%% @private
%% @doc Parses digits until a delimiter character.
%%
%% Returns {Number, RemainingString}.
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_digits_until(string(), char()) -> {non_neg_integer(), string()}.

parse_digits_until(String, Delimiter) ->
    parse_digits_until(String, Delimiter, []).

parse_digits_until([Delimiter | Rest], Delimiter, []) ->
    %% Delimiter found immediately, value is 0
    {0, Rest};
parse_digits_until([Delimiter | Rest], Delimiter, Digits) ->
    %% Delimiter found after digits
    {list_to_integer(lists:reverse(Digits)), Rest};
parse_digits_until([Ch | Rest], Delimiter, Digits) when Ch >= $0, Ch =< $9 ->
    %% Accumulate digits
    parse_digits_until(Rest, Delimiter, [Ch | Digits]);
parse_digits_until(_Other, _Delimiter, _Digits) ->
    erlang:error(bad_duration).

%% @private
%% @doc Inserts a timer entry into a sorted list, maintaining deadline order.
%%
%% The list is kept in ascending order by deadline for efficient polling.
%% Uses linear insertion which is O(n) but simple and pure.
%%
%% @end
%%--------------------------------------------------------------------
-spec insert_sorted(timer_entry(), deadline_list()) -> deadline_list().

insert_sorted(Entry, []) ->
    [Entry];
insert_sorted({Deadline, _} = Entry, [{D, _} | _] = Rest) when Deadline =< D ->
    [Entry | Rest];
insert_sorted(Entry, [Head | Rest]) ->
    [Head | insert_sorted(Entry, Rest)].

%% @private
%% @doc Takes expired entries from the deadline list.
%%
%% Traverses the sorted list from the front, collecting all entries with
%% deadline <= Now and returning the remaining list.
%%
%% @end
%%--------------------------------------------------------------------
-spec take_expired(deadline_list(), deadline(), [timer_entry()]) ->
          {[timer_entry()], deadline_list()}.

take_expired([], _Now, Acc) ->
    {lists:reverse(Acc), []};
take_expired([{Deadline, _} = Entry | Rest], Now, Acc) when Deadline =< Now ->
    %% This timer is expired, collect it and continue
    take_expired(Rest, Now, [Entry | Acc]);
take_expired(Remaining, _Now, Acc) ->
    %% Remaining timers are in the future (sorted list)
    {lists:reverse(Acc), Remaining}.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Basic operations test
basic_test() ->
    Q0 = new(),
    ?assertEqual(true, is_empty(Q0)),
    ?assertEqual(0, size(Q0)),
    ?assertEqual(undefined, peek(Q0)).

%% Arm single timer test
arm_single_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1100, {inject, p1, a}),
    ?assertEqual(1, size(Q1)),
    ?assertEqual(false, is_empty(Q1)),
    ?assertEqual({1100, {inject, p1, a}}, peek(Q1)).

%% Arm multiple timers test
arm_multiple_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1100, {inject, p1, a}),
    Q2 = arm(Q1, k2, 1200, {inject, p2, b}),
    ?assertEqual(2, size(Q2)),
    ?assertEqual({1100, {inject, p1, a}}, peek(Q2)).

%% Arm with out-of-order deadlines test
arm_out_of_order_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1200, {inject, p1, a}),
    Q2 = arm(Q1, k2, 1100, {inject, p2, b}),
    ?assertEqual(2, size(Q2)),
    %% Should still return earliest deadline
    ?assertEqual({1100, {inject, p2, b}}, peek(Q2)).

%% Replace existing key test
replace_key_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1100, {inject, p1, a}),
    Q2 = arm(Q1, k1, 1300, {inject, p1, c}),
    ?assertEqual(1, size(Q2)),
    ?assertEqual({1300, {inject, p1, c}}, peek(Q2)).

%% Poll no ready events test
poll_none_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1100, {inject, p1, a}),
    Q2 = arm(Q1, k2, 1200, {inject, p2, b}),
    {E0, Qx} = poll(Q2, 1000),
    ?assertEqual([], E0),
    ?assertEqual(2, size(Qx)).

%% Poll one ready event test
poll_one_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1100, {inject, p1, a}),
    Q2 = arm(Q1, k2, 1200, {inject, p2, b}),
    {E1, Q3} = poll(Q2, 1100),
    ?assertEqual([{inject, p1, a}], E1),
    ?assertEqual(1, size(Q3)),
    ?assertEqual({1200, {inject, p2, b}}, peek(Q3)).

%% Poll all ready events test
poll_all_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1100, {inject, p1, a}),
    Q2 = arm(Q1, k2, 1200, {inject, p2, b}),
    {E1, Q3} = poll(Q2, 1200),
    ?assertEqual([{inject, p1, a}, {inject, p2, b}], E1),
    ?assertEqual(0, size(Q3)),
    ?assertEqual(undefined, peek(Q3)).

%% Disarm test
disarm_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1100, {inject, p1, a}),
    Q2 = arm(Q1, k2, 1200, {inject, p2, b}),
    Q3 = disarm(Q2, k2),
    ?assertEqual(1, size(Q3)),
    ?assertEqual({1100, {inject, p1, a}}, peek(Q3)).

%% Disarm non-existent key test
disarm_nonexistent_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1100, {inject, p1, a}),
    Q2 = disarm(Q1, k999),  %% Non-existent key
    ?assertEqual(1, size(Q2)),
    ?assertEqual({1100, {inject, p1, a}}, peek(Q2)).

%% Poll then poll again test
poll_twice_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1100, {inject, p1, a}),
    Q2 = arm(Q1, k2, 1200, {inject, p2, b}),

    %% First poll gets nothing
    {E0, Qx} = poll(Q2, 1000),
    ?assertEqual([], E0),
    ?assertEqual(2, size(Qx)),

    %% Second poll gets first event
    {E1, Qy} = poll(Qx, 1100),
    ?assertEqual([{inject, p1, a}], E1),
    ?assertEqual(1, size(Qy)),

    %% Third poll gets remaining event
    {E2, Qz} = poll(Qy, 1200),
    ?assertEqual([{inject, p2, b}], E2),
    ?assertEqual(0, size(Qz)).

%%====================================================================
%% Extended API Tests
%%====================================================================

%% Arm from now test
arm_from_now_test() ->
    Q0 = new(),
    Before = erlang:monotonic_time(millisecond),
    Q1 = arm_from_now(Q0, k1, 5000, {inject, p1, a}),
    After = erlang:monotonic_time(millisecond),
    ?assertEqual(1, size(Q1)),
    %% Verify deadline is approximately 5000ms from now
    {ok, Deadline} = get_deadline(Q1, k1),
    ?assert(Deadline >= Before + 5000),
    ?assert(Deadline =< After + 5000).

%% Get deadline existing key test
get_deadline_existing_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1500, {inject, p1, a}),
    ?assertEqual({ok, 1500}, get_deadline(Q1, k1)).

%% Get deadline non-existent key test
get_deadline_nonexistent_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1500, {inject, p1, a}),
    ?assertEqual(undefined, get_deadline(Q1, k999)).

%% Clear all test
clear_all_test() ->
    Q0 = new(),
    Q1 = arm(Q0, k1, 1500, {inject, p1, a}),
    Q2 = arm(Q1, k2, 2000, {inject, p2, b}),
    ?assertEqual(2, size(Q2)),
    Q3 = clear_all(Q2),
    ?assertEqual(0, size(Q3)),
    ?assertEqual(true, is_empty(Q3)),
    ?assertEqual(undefined, peek(Q3)).

%%====================================================================
%% ISO 8601 Duration Parser Tests
%%====================================================================

%% Parse duration seconds only test
parse_duration_seconds_test() ->
    ?assertEqual({ok, 30000}, parse_iso8601_duration(<<"PT30S">>)),
    ?assertEqual({ok, 1000}, parse_iso8601_duration(<<"PT1S">>)),
    ?assertEqual({ok, 0}, parse_iso8601_duration(<<"PT0S">>)).

%% Parse duration minutes only test
parse_duration_minutes_test() ->
    ?assertEqual({ok, 300000}, parse_iso8601_duration(<<"PT5M">>)),
    ?assertEqual({ok, 60000}, parse_iso8601_duration(<<"PT1M">>)).

%% Parse duration hours only test
parse_duration_hours_test() ->
    ?assertEqual({ok, 7200000}, parse_iso8601_duration(<<"PT2H">>)),
    ?assertEqual({ok, 3600000}, parse_iso8601_duration(<<"PT1H">>)).

%% Parse duration days only test
parse_duration_days_test() ->
    ?assertEqual({ok, 259200000}, parse_iso8601_duration(<<"P3D">>)),
    ?assertEqual({ok, 86400000}, parse_iso8601_duration(<<"P1D">>)).

%% Parse duration combined test
parse_duration_combined_test() ->
    %% 1 day 2 hours 30 minutes 45 seconds = 95445000 ms
    ?assertEqual({ok, 95445000}, parse_iso8601_duration(<<"P1DT2H30M45S">>)),
    %% 5 hours 30 minutes = 19800000 ms
    ?assertEqual({ok, 19800000}, parse_iso8601_duration(<<"PT5H30M">>)),
    %% 2 minutes 30 seconds = 150000 ms
    ?assertEqual({ok, 150000}, parse_iso8601_duration(<<"PT2M30S">>)).

%% Parse duration string format test
parse_duration_string_test() ->
    ?assertEqual({ok, 30000}, parse_iso8601_duration("PT30S")),
    ?assertEqual({ok, 60000}, parse_iso8601_duration("PT1M")).

%% Parse duration invalid test
parse_duration_invalid_test() ->
    ?assertEqual({error, bad_duration}, parse_iso8601_duration(<<"invalid">>)),
    ?assertEqual({error, bad_duration}, parse_iso8601_duration(<<"">>)),
    ?assertEqual({error, bad_duration}, parse_iso8601_duration(<<"30S">>)),  %% Missing P
    ?assertEqual({error, bad_duration}, parse_iso8601_duration(<<"P30S">>)).  %% Missing T

%% Arm duration valid test
arm_duration_valid_test() ->
    Q0 = new(),
    {ok, Q1} = arm_duration(Q0, k1, <<"PT30S">>, {inject, p1, a}),
    ?assertEqual(1, size(Q1)),
    Before = erlang:monotonic_time(millisecond),
    {ok, Deadline} = get_deadline(Q1, k1),
    After = erlang:monotonic_time(millisecond),
    ?assert(Deadline >= Before + 30000),
    ?assert(Deadline =< After + 30000).

%% Arm duration combined test
arm_duration_combined_test() ->
    Q0 = new(),
    {ok, Q1} = arm_duration(Q0, k1, <<"P1DT2H30M">>, {inject, p1, a}),
    ?assertEqual(1, size(Q1)),
    {ok, Deadline} = get_deadline(Q1, k1),
    Now = erlang:monotonic_time(millisecond),
    %% 1 day + 2 hours + 30 minutes = 95400000 ms
    ExpectedDuration = 95400000,
    ?assert(Deadline >= Now + ExpectedDuration - 100),  %% Allow 100ms tolerance
    ?assert(Deadline =< Now + ExpectedDuration + 100).

%% Arm duration invalid test
arm_duration_invalid_test() ->
    Q0 = new(),
    ?assertEqual({error, bad_duration}, arm_duration(Q0, k1, <<"invalid">>, {inject, p1, b})),
    ?assertEqual({error, bad_duration}, arm_duration(Q0, k2, <<>>, {inject, p2, c})).

-endif.
