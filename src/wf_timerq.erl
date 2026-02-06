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
%% @doc Timer Queue for Petri Net Token Injection
%%
%% This module provides a pure-functional deadline queue that produces token
%% injection events for gen_pnet Petri net execution. The timer queue
%% manages time-based transitions that should fire after a specified deadline.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Pure Functional:</b> No processes, all operations return updated state</li>
%%   <li><b>Priority Queue:</b> Efficient O(n) operations using sorted list</li>
%%   <li><b>Deadline-Based:</b> Timers fire based on monotonic milliseconds</li>
%%   <li><b>Token Events:</b> Returns {produce, ProduceMap} events for gen_pnet</li>
%% </ul>
%%
%% <h3>Data Structure</h3>
%%
%% The timer queue uses an orddict-like structure with deadlines as keys
%% for efficient retrieval of expired timers. Each timer entry contains:
%% <ul>
%%   <li><b>Deadline:</b> Monotonic milliseconds when timer fires</li>
%%   <li><b>Key:</b> Unique identifier for the timer</li>
%%   <li><b>Event:</b> {produce, ProduceMap} token injection event</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_timerq).

-moduledoc """
Deadline queue for time-based token injection.

Time choice: deadlines are monotonic milliseconds (integers).
wf_timerq is pure: it returns ready {produce, ProduceMap} events via poll/2.

```erlang
> Q0 = wf_timerq:new().
_
> Now = erlang:monotonic_time(millisecond).
_
> Q1 = wf_timerq:arm(Q0, k1, Now + 10, {produce, #{p => [t]}}).
_
> {E0, _Qx} = wf_timerq:poll(Q1, Now + 5), E0.
[]
> {E1, _Qy} = wf_timerq:poll(Q1, Now + 10), E1.
[{produce, #{p => [t]}}]

> Q2 = wf_timerq:disarm(Q1, k1).
_
> {E2, _} = wf_timerq:poll(Q2, Now + 10), E2.
[]
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Timer queue API
-export([new/0]).
-export([arm/4, disarm/2, poll/2]).
-export([is_empty/1, size/1, peek/1]).

%%====================================================================
%% Types
%%====================================================================

%% A timer event produces tokens into Petri net places
-type timer_event() :: {produce, produce_map()}.

%% Map from place names to token lists for gen_pnet
-type produce_map() :: #{place() => [token()]}.

%% Place identifiers (atoms or binaries)
-type place() :: atom() | binary().

%% Tokens can be any Erlang term
-type token() :: term().

%% Unique key identifying a timer
-type timer_key() :: term().

%% Deadline as Unix timestamp in milliseconds
-type deadline() :: integer().

%% Timer entry: {Deadline, Key, Event}
-type timer_entry() :: {deadline(), timer_key(), timer_event()}.

%% Timer queue: list of entries sorted by deadline (ascending)
%% Using orddict convention: sorted list with unique keys per deadline
-type timerq() :: [timer_entry()].

%% Export types
-export_type([timerq/0, timer_key/0, deadline/0, timer_event/0]).
-export_type([produce_map/0, place/0, token/0]).

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
    [].

%%--------------------------------------------------------------------
%% @doc Arms a new timer or replaces an existing timer with the same key.
%%
%% If a timer with the same key already exists, it is replaced with the
%% new deadline and event. The queue is maintained in sorted order by
%% deadline for efficient polling.
%%
%% @param TimerQ The current timer queue
%% @param Key Unique identifier for this timer
%% @param Deadline Unix timestamp in milliseconds when timer should fire
%% @param Event The {produce, ProduceMap} event to fire when deadline expires
%% @returns Updated timer queue with timer armed
%%
%% @end
%%--------------------------------------------------------------------
-spec arm(TimerQ :: timerq(), Key :: timer_key(),
         Deadline :: deadline(), Event :: timer_event()) -> timerq().

arm(TimerQ, Key, Deadline, Event) ->
    %% Remove any existing timer with the same key
    Q1 = disarm(TimerQ, Key),
    %% Insert new entry and re-sort by deadline
    Entry = {Deadline, Key, Event},
    lists:keysort(1, [Entry | Q1]).

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

disarm(TimerQ, Key) ->
    lists:keydelete(Key, 2, TimerQ).

%%--------------------------------------------------------------------
%% @doc Polls the timer queue for expired timers.
%%
%% Returns all events whose deadline is less than or equal to Now, along
%% with the updated queue having those entries removed. Events are returned
%% in deadline order (oldest first).
%%
%% @param TimerQ The current timer queue
%% @param Now Current Unix timestamp in milliseconds
%% @returns {Events, UpdatedQ} where Events is list of expired timer events
%%          and UpdatedQ is the queue with expired entries removed
%%
%% @end
%%--------------------------------------------------------------------
-spec poll(TimerQ :: timerq(), Now :: deadline()) ->
          {Events :: [timer_event()], UpdatedQ :: timerq()}.

poll(TimerQ, Now) ->
    poll(TimerQ, Now, []).

%%--------------------------------------------------------------------
%% @doc Checks if the timer queue is empty.
%%
%% @param TimerQ The timer queue to check
%% @returns true if queue has no timers, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_empty(TimerQ :: timerq()) -> boolean().

is_empty([]) ->
    true;
is_empty(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Returns the number of timers in the queue.
%%
%% @param TimerQ The timer queue
%% @returns Count of timers in the queue
%%
%% @end
%%--------------------------------------------------------------------
-spec size(TimerQ :: timerq()) -> non_neg_integer().

size(TimerQ) ->
    length(TimerQ).

%%--------------------------------------------------------------------
%% @doc Peeks at the next timer to expire without removing it.
%%
%% @param TimerQ The timer queue
%% @returns {ok, Deadline, Key, Event} for the next timer, or empty if queue empty
%%
%% @end
%%--------------------------------------------------------------------
-spec peek(TimerQ :: timerq()) ->
          {ok, deadline(), timer_key(), timer_event()} | empty.

peek([]) ->
    empty;
peek([{Deadline, Key, Event} | _]) ->
    {ok, Deadline, Key, Event}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Poll helper that accumulates expired events recursively.
%%
%% Processes the sorted timer queue from the front, collecting all entries
%% with deadline <= Now and returning the remaining queue.
%%
%% @end
-spec poll(TimerQ :: timerq(), Now :: deadline(),
           Acc :: [timer_event()]) -> {[timer_event()], timerq()}.

poll([], _Now, Acc) ->
    {lists:reverse(Acc), []};
poll([{Deadline, _Key, Event} | Rest], Now, Acc) when Deadline =< Now ->
    %% This timer is expired, collect it and continue
    poll(Rest, Now, [Event | Acc]);
poll([{Deadline, _Key, _Event} | _] = TimerQ, Now, Acc) when Deadline > Now ->
    %% Remaining timers are in the future (sorted list)
    {lists:reverse(Acc), TimerQ}.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).
-endif.
