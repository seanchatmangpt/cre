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
%% @doc Time Manipulation Mocks for Testing
%%
%% This module provides utilities for mocking and controlling time
%% in tests. It allows freezing time, advancing time programmatically,
%% and unfreezing for deterministic testing.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Freeze time at a specific point</li>
%%   <li>Advance time by specified amounts</li>
%%   <li>Unfreeze and restore normal time</li>
%%   <li>Mock timestamps for event generation</li>
%%   <li>Meck-compatible for module mocking</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% Freeze and control time:
%% ```erlang
%% > ok = time_mocks:freeze_time(),
%% > ok = time_mocks:advance_time(1000),
%% > Ms = time_mocks:mock_timestamp(),
%% > ok = time_mocks:unfreeze_time().
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(time_mocks).

%%====================================================================
%% Exports
%%====================================================================

%% Time freezing/control
-export([freeze_time/0, freeze_time/1]).
-export([advance_time/1, advance_time/2]).
-export([unfreeze_time/0]).
-export([is_frozen/0]).

%% Timestamp generation
-export([mock_timestamp/0]).
-export([mock_timestamp/1]).
-export([mock_datetime/0]).
-export([mock_datetime/1]).

%% Time conversion helpers
-export([millis_to_datetime/1]).
-export([datetime_to_millis/1]).
-export([add_millis/2]).
-export([subtract_millis/2]).

%% Test helpers
-export([wait_until/2]).
-export([wait_until/3]).
-export([sleep_mock/1]).

%%====================================================================
%% Types
%%====================================================================

-type millis() :: non_neg_integer().
-type datetime() :: {{integer(), integer(), integer()}, {integer(), integer(), integer()}}.
-type time_unit() :: millisecond | second | minute | hour | day.

%%====================================================================
%% Time Freezing/Control
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Freezes time at the current moment.
%%
%% After calling this, all time_mocks timestamp functions will return
%% the frozen time. advance_time/1 can be used to move forward.
%%
%% @end
%%--------------------------------------------------------------------
-spec freeze_time() -> ok.

freeze_time() ->
    freeze_time(erlang:system_time(millisecond)).

%%--------------------------------------------------------------------
%% @doc Freezes time at a specific timestamp.
%%
%% Timestamp is in milliseconds since Unix epoch.
%%
%% @end
%%--------------------------------------------------------------------
-spec freeze_time(millis()) -> ok.

freeze_time(Timestamp) ->
    put(frozen_time, Timestamp),
    put(time_frozen, true),
    ok.

%%--------------------------------------------------------------------
%% @doc Advances frozen time by a number of milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec advance_time(millis()) -> ok.

advance_time(Millis) ->
    advance_time(millisecond, Millis).

%%--------------------------------------------------------------------
%% @doc Advances frozen time by a specific unit and amount.
%%
%% @end
%%--------------------------------------------------------------------
-spec advance_time(time_unit(), number()) -> ok.

advance_time(Unit, Amount) ->
    Millis = case Unit of
        millisecond -> round(Amount);
        second -> round(Amount * 1000);
        minute -> round(Amount * 60 * 1000);
        hour -> round(Amount * 60 * 60 * 1000);
        day -> round(Amount * 24 * 60 * 60 * 1000)
    end,
    case is_frozen() of
        true ->
            CurrentTime = get(frozen_time),
            put(frozen_time, CurrentTime + Millis),
            ok;
        false ->
            {error, not_frozen}
    end.

%%--------------------------------------------------------------------
%% @doc Unfreezes time, returning to normal system time.
%%
%% @end
%%--------------------------------------------------------------------
-spec unfreeze_time() -> ok.

unfreeze_time() ->
    erase(frozen_time),
    erase(time_frozen),
    ok.

%%--------------------------------------------------------------------
%% @doc Checks if time is currently frozen.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_frozen() -> boolean().

is_frozen() ->
    case get(time_frozen) of
        true -> true;
        _ -> false
    end.

%%====================================================================
%% Timestamp Generation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets the current (possibly frozen) timestamp.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_timestamp() -> millis().

mock_timestamp() ->
    case is_frozen() of
        true -> get(frozen_time);
        false -> erlang:system_time(millisecond)
    end.

%%--------------------------------------------------------------------
%% @doc Gets a timestamp offset from current (frozen) time.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_timestamp(millis()) -> millis().

mock_timestamp(OffsetMillis) ->
    mock_timestamp() + OffsetMillis.

%%--------------------------------------------------------------------
%% @doc Gets the current (possibly frozen) datetime.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_datetime() -> datetime().

mock_datetime() ->
    millis_to_datetime(mock_timestamp()).

%%--------------------------------------------------------------------
%% @doc Gets a datetime offset from current (frozen) time.
%%
%% @end
%%--------------------------------------------------------------------
-spec mock_datetime(millis()) -> datetime().

mock_datetime(OffsetMillis) ->
    millis_to_datetime(mock_timestamp(OffsetMillis)).

%%====================================================================
%% Time Conversion Helpers
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Converts milliseconds to datetime tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec millis_to_datetime(millis()) -> datetime().

millis_to_datetime(Millis) ->
    Seconds = Millis div 1000,
    calendar:system_time_to_universal_time(Seconds, second).

%%--------------------------------------------------------------------
%% @doc Converts datetime tuple to milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec datetime_to_millis(datetime()) -> millis().

datetime_to_millis(DateTime) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime) -
              calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Seconds * 1000.

%%--------------------------------------------------------------------
%% @doc Adds milliseconds to a timestamp.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_millis(millis(), millis()) -> millis().

add_millis(Timestamp, MillisToAdd) ->
    Timestamp + MillisToAdd.

%%--------------------------------------------------------------------
%% @doc Subtracts milliseconds from a timestamp.
%%
%% @end
%%--------------------------------------------------------------------
-spec subtract_millis(millis(), millis()) -> millis().

subtract_millis(Timestamp, MillisToSubtract) ->
    max(0, Timestamp - MillisToSubtract).

%%====================================================================
%% Test Helpers
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Waits until a predicate function returns true.
%%
%% Timeout is in milliseconds. Returns true if predicate becomes true,
%% timeout if timeout expires.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_until(fun(() -> boolean()), timeout()) -> true | timeout.

wait_until(Pred, Timeout) ->
    wait_until(Pred, Timeout, 100).

%%--------------------------------------------------------------------
%% @doc Waits until a predicate with check interval.
%%
%% Checks the predicate every IntervalMs milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_until(fun(() -> boolean()), timeout(), pos_integer()) ->
          true | timeout.

wait_until(Pred, Timeout, IntervalMs) ->
    Start = erlang:monotonic_time(millisecond),
    wait_until_loop(Pred, Start, Timeout, IntervalMs).

%% @private
wait_until_loop(Pred, Start, Timeout, IntervalMs) ->
    case Pred() of
        true -> true;
        false ->
            Elapsed = erlang:monotonic_time(millisecond) - Start,
            if
                Elapsed >= Timeout -> timeout;
                true ->
                    timer:sleep(IntervalMs),
                    wait_until_loop(Pred, Start, Timeout, IntervalMs)
            end
    end.

%%--------------------------------------------------------------------
%% @doc Mock sleep that respects frozen time.
%%
%% If time is frozen, this advances time by the sleep duration.
%% Otherwise, calls timer:sleep/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec sleep_mock(timeout()) -> ok.

sleep_mock(Duration) ->
    case is_frozen() of
        true ->
            advance_time(millisecond, Duration),
            ok;
        false ->
            timer:sleep(Duration),
            ok
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test freeze_time/0
freeze_time_test() ->
    ?assertEqual(false, is_frozen()),
    ?assertEqual(ok, freeze_time()),
    ?assertEqual(true, is_frozen()),
    ?assertEqual(ok, unfreeze_time()),
    ?assertEqual(false, is_frozen()).

%% Test freeze_time/1
freeze_time_value_test() ->
    ?assertEqual(ok, freeze_time(12345)),
    ?assertEqual(12345, mock_timestamp()),
    ?assertEqual(ok, unfreeze_time()).

%% Test advance_time/1
advance_time_test() ->
    ?assertEqual(ok, freeze_time(10000)),
    ?assertEqual(10000, mock_timestamp()),
    ?assertEqual(ok, advance_time(500)),
    ?assertEqual(10500, mock_timestamp()),
    ?assertEqual(ok, advance_time(second, 1)),
    ?assertEqual(11500, mock_timestamp()),
    ?assertEqual(ok, advance_time(minute, 1)),
    ?assertEqual(71500, mock_timestamp()),
    ?assertEqual(ok, unfreeze_time()).

%% Test advance_time when not frozen
advance_time_not_frozen_test() ->
    ?assertEqual({error, not_frozen}, advance_time(100)).

%% Test mock_timestamp/0
mock_timestamp_test() ->
    ?assertEqual(ok, freeze_time(9999)),
    ?assertEqual(9999, mock_timestamp()),
    ?assertEqual(ok, unfreeze_time()),
    ?assert(is_integer(mock_timestamp())),
    ?assert(mock_timestamp() > 1000000000000).

%% Test mock_timestamp/1
mock_timestamp_offset_test() ->
    ?assertEqual(ok, freeze_time(10000)),
    ?assertEqual(10500, mock_timestamp(500)),
    ?assertEqual(11000, mock_timestamp(1000)),
    ?assertEqual(ok, unfreeze_time()).

%% Test mock_datetime/0
mock_datetime_test() ->
    ?assertEqual(ok, freeze_time(1704067200000)), %% 2024-01-01 00:00:00 UTC
    DateTime = mock_datetime(),
    ?assertMatch({{2024, _, _}, {_, _, _}}, DateTime),
    ?assertEqual(ok, unfreeze_time()).

%% Test mock_datetime/1
mock_datetime_offset_test() ->
    ?assertEqual(ok, freeze_time(1704067200000)),
    DateTime1 = mock_datetime(3600000), %% +1 hour
    ?assertMatch({{2024, 1, 1}, {1, 0, 0}}, DateTime1),
    ?assertEqual(ok, unfreeze_time()).

%% Test millis_to_datetime/1
millis_to_datetime_test() ->
    ?assertEqual({{1970, 1, 1}, {0, 0, 0}},
                 millis_to_datetime(0)),
    ?assertEqual({{1970, 1, 1}, {0, 0, 1}},
                 millis_to_datetime(1000)),
    ?assertEqual({{1970, 1, 2}, {0, 0, 0}},
                 millis_to_datetime(86400000)).

%% Test datetime_to_millis/1
datetime_to_millis_test() ->
    ?assertEqual(0, datetime_to_millis({{1970, 1, 1}, {0, 0, 0}})),
    ?assertEqual(1000, datetime_to_millis({{1970, 1, 1}, {0, 0, 1}})),
    ?assertEqual(86400000, datetime_to_millis({{1970, 1, 2}, {0, 0, 0}})).

%% Test add_millis/2
add_millis_test() ->
    ?assertEqual(1500, add_millis(1000, 500)),
    ?assertEqual(2000, add_millis(1000, 1000)).

%% Test subtract_millis/2
subtract_millis_test() ->
    ?assertEqual(500, subtract_millis(1000, 500)),
    ?assertEqual(0, subtract_millis(1000, 2000)).

%% Test wait_until/2
wait_until_test() ->
    ?assertEqual(true, wait_until(fun() -> true end, 100)),
    ?assertEqual(timeout, wait_until(fun() -> false end, 100)),

    %% Test state change using ETS
    ets:new(test_table, [named_table, public, set]),
    ets:insert(test_table, {key, false}),
    spawn(fun() ->
        timer:sleep(50),
        ets:insert(test_table, {key, true})
    end),
    ?assertEqual(true, wait_until(fun() ->
        case ets:lookup(test_table, key) of
            [{key, Val}] -> Val;
            _ -> false
        end
    end, 500)),
    ets:delete(test_table).

%% Test wait_until/3
wait_until_interval_test() ->
    ets:new(test_table2, [named_table, public, set]),
    ets:insert(test_table2, {counter, 0}),
    spawn(fun() ->
        timer:sleep(100),
        ets:insert(test_table2, {counter, 1})
    end),
    ?assertEqual(true, wait_until(fun() ->
        case ets:lookup(test_table2, counter) of
            [{counter, 1}] -> true;
            _ -> false
        end
    end, 500, 50)),
    ets:delete(test_table2).

%% Test sleep_mock/1
sleep_mock_test() ->
    ?assertEqual(ok, freeze_time(10000)),
    ?assertEqual(ok, sleep_mock(100)),
    ?assertEqual(10100, mock_timestamp()),
    ?assertEqual(ok, unfreeze_time()).

-endif.
