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

-module(wf_calendar).

-moduledoc """
Business calendar for calculating due dates in business time.

Calculates due dates by adding business days, skipping weekends and
non-workdays. Supports configurable workdays and business hours.

```erlang
> Cal = wf_calendar:new(#{
..   tz => "UTC",
..   workdays => [mon,tue,wed,thu,fri],
..   hours => {9,17}
.. }).
_

% Feb 6, 2026 is a Friday. Add 2 business days at 10:00 -> Tuesday Feb 10 at 10:00.
> wf_calendar:due_in(Cal, #{days => 2}, {{2026,2,6},{10,0,0}}).
{{2026,2,10},{10,0,0}}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Calendar API
-export([new/1, due_in/3]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Time zone identifier (e.g., "UTC", "America/New_York").
%%
%% Currently stored for reference and future timezone support.
%%--------------------------------------------------------------------
-type tz() :: binary() | string().

%%--------------------------------------------------------------------
%% @doc Day of week atom.
%%
%% Standard Erlang calendar module day representation.
%%--------------------------------------------------------------------
-type weekday() :: mon | tue | wed | thu | fri | sat | sun.

%%--------------------------------------------------------------------
%% @doc Business hours as {StartHour, EndHour}.
%%
%% Hours are in 24-hour format. {9, 17} means 9 AM to 5 PM.
%%--------------------------------------------------------------------
-type hours() :: {Hour24 :: hour24(), Hour24 :: hour24()}.

%%--------------------------------------------------------------------
%% @doc Hour in 24-hour format (0-23).
%%--------------------------------------------------------------------
-type hour24() :: 0..23.

%%--------------------------------------------------------------------
%% @doc Calendar configuration map.
%%
%% Required keys:
%% <ul>
%%   <li><b>tz:</b> Time zone identifier (binary or string)</li>
%%   <li><b>workdays:</b> List of workdays (mon/tue/wed/thu/fri/sat/sun)</li>
%%   <li><b>hours:</b> Business hours as {StartHour, EndHour}</li>
%% </ul>
%%--------------------------------------------------------------------
-type calendar_config() :: #{
    tz => tz(),
    workdays => [weekday()],
    hours => hours()
}.

%%--------------------------------------------------------------------
%% @doc Business calendar record.
%%
%% Opaque calendar type for business time calculations.
%%--------------------------------------------------------------------
-record(calendar, {
    tz :: tz(),
    workdays :: [weekday()],
    hours :: hours()
}).

-opaque calendar() :: #calendar{}.

%%--------------------------------------------------------------------
%% @doc Date/time tuple.
%%
%% Standard Erlang datetime format: {{Year, Month, Day}, {Hour, Minute, Second}}.
%%--------------------------------------------------------------------
-type datetime() :: calendar:datetime().

%%--------------------------------------------------------------------
%% @doc Date tuple.
%%
%% Standard Erlang date format: {Year, Month, Day}.
%%--------------------------------------------------------------------
-type date() :: calendar:date().

%%--------------------------------------------------------------------
%% @doc Time tuple.
%%
%% Standard Erlang time format: {Hour, Minute, Second}.
%%--------------------------------------------------------------------
-type time() :: calendar:time().

%%--------------------------------------------------------------------
%% @doc Duration specification for due_in/3.
%%
%% Supported keys:
%% <ul>
%%   <li><b>days:</b> Number of business days to add (non-negative integer)</li>
%% </ul>
%% Future extensions may support hours, minutes, etc.
%%--------------------------------------------------------------------
-type duration() :: #{
    days => non_neg_integer()
}.

%% Export types
-export_type([calendar/0, tz/0, weekday/0, hours/0, duration/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new business calendar from configuration.
%%
%% The calendar configures which days are considered workdays and what
%% hours constitute business hours. This information is used to calculate
%% due dates in business time.
%%
%% @param Config Map with tz, workdays, and hours keys
%% @returns A new calendar record
%%
%% @end
%%--------------------------------------------------------------------
-spec new(Config :: calendar_config()) -> calendar().

new(#{tz := TZ, workdays := Workdays, hours := Hours}) when is_list(Workdays) ->
    #calendar{
        tz = normalize_tz(TZ),
        workdays = normalize_workdays(Workdays),
        hours = normalize_hours(Hours)
    }.

%%--------------------------------------------------------------------
%% @doc Calculates a due date by adding business days to a start time.
%%
%% Business days are counted by skipping non-workdays (weekends, etc.).
%% The time component is preserved - adding 2 business days from 10:00
%% results in 10:00 on the target day.
%%
%% @param Cal The business calendar to use
%% @param Duration Map containing 'days' key with number of business days
%% @param Start The starting datetime
%% @returns The due datetime after adding business days
%%
%% @end
%%--------------------------------------------------------------------
-spec due_in(Cal :: calendar(), Duration :: duration(), Start :: datetime()) ->
          datetime().

due_in(#calendar{workdays = Workdays}, #{days := Days}, {Date, Time})
  when is_integer(Days), Days >= 0 ->
    add_business_days(Date, Time, Days, Workdays).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes timezone to binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_tz(tz()) -> binary().

normalize_tz(TZ) when is_binary(TZ) -> TZ;
normalize_tz(TZ) when is_list(TZ) -> list_to_binary(TZ).

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes workday list to sorted list of atoms.
%%
%% Ensures all workdays are valid weekday atoms.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_workdays([weekday()]) -> [weekday()].

normalize_workdays(Workdays) ->
    Norm = [normalize_day(D) || D <- Workdays],
    lists:usort(Norm).

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes a single weekday to atom.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_day(weekday() | binary() | string()) -> weekday().

normalize_day(D) when is_atom(D) -> D;
normalize_day(D) when is_binary(D) -> binary_to_existing_atom(D, utf8);
normalize_day(D) when is_list(D) -> list_to_existing_atom(D).

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes hours tuple, ensuring valid 24-hour format.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_hours(hours()) -> hours().

normalize_hours({Start, End}) when is_integer(Start), is_integer(End),
                                   Start >= 0, Start =< 23,
                                   End >= 0, End =< 23 ->
    {Start, End}.

%%--------------------------------------------------------------------
%% @private
%% @doc Adds business days to a date, preserving time.
%%
%% Iterates through calendar days, counting only workdays until the
%% specified number of business days have been added.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_business_days(date(), time(), non_neg_integer(), [weekday()]) ->
          datetime().

add_business_days(Date, Time, 0, _Workdays) ->
    {Date, Time};
add_business_days(Date, Time, DaysToAdd, Workdays) ->
    NextDay = next_day(Date),
    case is_workday(NextDay, Workdays) of
        true ->
            add_business_days(NextDay, Time, DaysToAdd - 1, Workdays);
        false ->
            add_business_days(NextDay, Time, DaysToAdd, Workdays)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the next calendar day.
%%
%% Handles month and year rollovers correctly using the calendar module.
%%
%% @end
%%--------------------------------------------------------------------
-spec next_day(date()) -> date().

next_day({Year, Month, Day}) ->
    SecondsInDay = 86400,
    {Date, _Time} = calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds({{Year, Month, Day}, {0, 0, 0}}) + SecondsInDay
    ),
    Date.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if a date is a workday.
%%
%% A workday is determined by checking if its weekday atom is in the
%% configured workdays list. Uses calendar:day_of_the_week/1 which returns
%% 1 = Monday, 2 = Tuesday, ..., 7 = Sunday.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_workday(date(), [weekday()]) -> boolean().

is_workday(Date, Workdays) ->
    DayNum = calendar:day_of_the_week(Date),
    Weekday = day_num_to_atom(DayNum),
    lists:member(Weekday, Workdays).

%%--------------------------------------------------------------------
%% @private
%% @doc Converts Erlang calendar day number to weekday atom.
%%
%% Erlang's calendar:day_of_week returns:
%% 1 = Monday, 2 = Tuesday, ..., 7 = Sunday
%%
%% @end
%%--------------------------------------------------------------------
-spec day_num_to_atom(1..7) -> weekday().

day_num_to_atom(1) -> mon;
day_num_to_atom(2) -> tue;
day_num_to_atom(3) -> wed;
day_num_to_atom(4) -> thu;
day_num_to_atom(5) -> fri;
day_num_to_atom(6) -> sat;
day_num_to_atom(7) -> sun.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%%--------------------------------------------------------------------
%% Test: new/1 creates calendar correctly
%%--------------------------------------------------------------------
new_test() ->
    Cal = wf_calendar:new(#{
        tz => "UTC",
        workdays => [mon, tue, wed, thu, fri],
        hours => {9, 17}
    }),
    ?assertEqual(#calendar{
        tz = <<"UTC">>,
        workdays = [mon, tue, wed, thu, fri],
        hours = {9, 17}
    }, Cal).

%%--------------------------------------------------------------------
%% Test: due_in/3 with zero days returns same time
%%--------------------------------------------------------------------
due_in_zero_days_test() ->
    Cal = wf_calendar:new(#{
        tz => "UTC",
        workdays => [mon, tue, wed, thu, fri],
        hours => {9, 17}
    }),
    Start = {{2026, 2, 6}, {10, 0, 0}},
    ?assertEqual(Start, wf_calendar:due_in(Cal, #{days => 0}, Start)).

%%--------------------------------------------------------------------
%% Test: due_in/3 skips weekend (Friday -> Tuesday with 2 days)
%%--------------------------------------------------------------------
due_in_skip_weekend_test() ->
    Cal = wf_calendar:new(#{
        tz => "UTC",
        workdays => [mon, tue, wed, thu, fri],
        hours => {9, 17}
    }),
    % Feb 6, 2026 is a Friday
    % Adding 2 business days: Fri -> Mon (1) -> Tue (2)
    Start = {{2026, 2, 6}, {10, 0, 0}},
    ?assertEqual({{2026, 2, 10}, {10, 0, 0}}, wf_calendar:due_in(Cal, #{days => 2}, Start)).

%%--------------------------------------------------------------------
%% Test: due_in/3 within same week (Monday -> Wednesday)
%%--------------------------------------------------------------------
due_in_same_week_test() ->
    Cal = wf_calendar:new(#{
        tz => "UTC",
        workdays => [mon, tue, wed, thu, fri],
        hours => {9, 17}
    }),
    % Feb 2, 2026 is a Monday
    % Adding 2 business days: Mon -> Tue (1) -> Wed (2)
    Start = {{2026, 2, 2}, {14, 30, 0}},
    ?assertEqual({{2026, 2, 4}, {14, 30, 0}}, wf_calendar:due_in(Cal, #{days => 2}, Start)).

%%--------------------------------------------------------------------
%% Test: due_in/3 preserves time component
%%--------------------------------------------------------------------
due_in_preserves_time_test() ->
    Cal = wf_calendar:new(#{
        tz => "UTC",
        workdays => [mon, tue, wed, thu, fri],
        hours => {9, 17}
    }),
    Start = {{2026, 2, 6}, {16, 45, 30}},
    Result = wf_calendar:due_in(Cal, #{days => 1}, Start),
    ?assertEqual({16, 45, 30}, element(2, Result)).

%%--------------------------------------------------------------------
%% Test: due_in/3 crosses month boundary
%%--------------------------------------------------------------------
due_in_month_boundary_test() ->
    Cal = wf_calendar:new(#{
        tz => "UTC",
        workdays => [mon, tue, wed, thu, fri],
        hours => {9, 17}
    }),
    % Jan 30, 2026 is a Friday
    % Adding 1 business day should land on Monday Feb 2
    Start = {{2026, 1, 30}, {10, 0, 0}},
    ?assertEqual({{2026, 2, 2}, {10, 0, 0}}, wf_calendar:due_in(Cal, #{days => 1}, Start)).

%%--------------------------------------------------------------------
%% Test: Custom workdays (Saturday is a workday)
%%--------------------------------------------------------------------
custom_workdays_test() ->
    Cal = wf_calendar:new(#{
        tz => "UTC",
        workdays => [mon, tue, wed, thu, fri, sat],
        hours => {9, 17}
    }),
    % Feb 6, 2026 is a Friday
    % Adding 2 business days: Fri -> Sat (1) -> Mon (2, Sun skipped)
    Start = {{2026, 2, 6}, {10, 0, 0}},
    ?assertEqual({{2026, 2, 9}, {10, 0, 0}}, wf_calendar:due_in(Cal, #{days => 2}, Start)).

%%--------------------------------------------------------------------
%% Test: Only Sunday as workday (unusual but valid)
%%--------------------------------------------------------------------
only_sunday_workday_test() ->
    Cal = wf_calendar:new(#{
        tz => "UTC",
        workdays => [sun],
        hours => {9, 17}
    }),
    % Feb 1, 2026 is a Sunday
    % Adding 2 business days means finding next 2 Sundays
    Start = {{2026, 2, 1}, {12, 0, 0}},
    ?assertEqual({{2026, 2, 15}, {12, 0, 0}}, wf_calendar:due_in(Cal, #{days => 2}, Start)).

-endif.
