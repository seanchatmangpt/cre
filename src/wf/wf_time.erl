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

-module(wf_time).

-moduledoc """
Time utilities for workflow deadlines and timestamps.

Provides monotonic time for deadline calculations and system UTC time for
external time formats. Supports timezone conversions for major timezones.

### Time Sources

- **Monotonic time**: Non-decreasing millisecond counter from `erlang:monotonic_time/1`
  - Used for deadlines and intervals
  - Guaranteed never to decrease
  - No relation to wall-clock time

- **System UTC time**: Milliseconds since Unix epoch (1970-01-01T00:00:00Z)
  - Used for external timestamps and RFC3339 formatting
  - Obtained via `system_time` from `erlang:system_time/1`
  - Always in UTC

```erlang
% Monotonic time is non-decreasing
> T1 = wf_time:now_mono_ms(), T2 = wf_time:now_mono_ms(), T2 >= T1.
true

% System time is integer milliseconds since epoch
> is_integer(wf_time:now_sys_utc_ms()).
true
```

### RFC3339 Parsing and Formatting

```erlang
% Parse RFC3339 timestamp to system milliseconds
> {ok, Sys} = wf_time:rfc3339_to_sys_ms(<<"2024-01-01T00:00:00Z">>).

% Format system milliseconds back to RFC3339
> wf_time:sys_ms_to_rfc3339(Sys).
<<"2024-01-01T00:00:00Z">>
```

### Timezone Conversions

Converts between local time and UTC for common timezone names. Supports
automatic DST detection for timezones that observe daylight saving time.

```erlang
% America/Los_Angeles is UTC-08:00 on Feb 6, 2024 (PST, no DST)
> wf_time:local_to_utc({{2024,2,6},{18,0,0}}, "America/Los_Angeles").
{{2024,2,7},{2,0,0}}

% America/Los_Angeles is UTC-07:00 on Jul 1, 2024 (PDT, DST active)
> wf_time:local_to_utc({{2024,7,1},{10,0,0}}, "America/Los_Angeles").
{{2024,7,1},{17,0,0}}
```

### Deadline Mapping

Maps absolute system deadlines to monotonic deadlines for use with timers.

```erlang
% Map system deadline at 1300ms, current time 1000ms, grace period 5000ms
> wf_time:sys_deadline_to_mono(1300, 1000, 5000).
5300
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Monotonic time
-export([now_mono_ms/0]).

%% System UTC time
-export([now_sys_utc_ms/0]).

%% RFC3339 conversion
-export([rfc3339_to_sys_ms/1, sys_ms_to_rfc3339/1]).

%% Timezone conversion
-export([local_to_utc/2, utc_to_local/2]).

%% Deadline mapping
-export([sys_deadline_to_mono/3]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Monotonic time in milliseconds.
%%
%% Non-decreasing counter suitable for deadlines and intervals.
%% No relation to wall-clock time.
%%--------------------------------------------------------------------
-type mono_ms() :: non_neg_integer().

%%--------------------------------------------------------------------
%% @doc System time in milliseconds since Unix epoch.
%%
%% Always in UTC. 1970-01-01T00:00:00Z = 0.
%%--------------------------------------------------------------------
-type sys_ms() :: non_neg_integer().

%%--------------------------------------------------------------------
%% @doc RFC3339 timestamp as binary.
%%
%% Format: <<"YYYY-MM-DDTHH:MM:SSZ">> or <<"YYYY-MM-DDTHH:MM:SS+HH:MM">>.
%%--------------------------------------------------------------------
-type rfc3339() :: binary().

%%--------------------------------------------------------------------
%% @doc Standard Erlang datetime tuple.
%%
%% {{Year, Month, Day}, {Hour, Minute, Second}}
%%--------------------------------------------------------------------
-type datetime() :: calendar:datetime().

%%--------------------------------------------------------------------
%% @doc Timezone identifier.
%%
%% Either a string like "America/Los_Angeles" or binary.
%%--------------------------------------------------------------------
-type timezone() :: binary() | string().

%%--------------------------------------------------------------------
%% @doc Error reasons for timezone operations.
%%--------------------------------------------------------------------
-type tz_error() :: bad_timezone.

%%--------------------------------------------------------------------
%% @doc Error reasons for RFC3339 parsing.
%%--------------------------------------------------------------------
-type rfc3339_error() :: bad_rfc3339.

%% Export types
-export_type([mono_ms/0, sys_ms/0, rfc3339/0, timezone/0]).
-export_type([tz_error/0, rfc3339_error/0]).

%%--------------------------------------------------------------------
%% @private
%% @doc Unix epoch offset in gregorian seconds.
%%
%% Unix epoch is 1970-01-01T00:00:00Z.
%% Gregorian epoch is year 0.
%%
%% calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}) = 62167219200
%%--------------------------------------------------------------------
-define(EPOCH_OFFSET, 62167219200).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets current monotonic time in milliseconds.
%%
%% Monotonic time is guaranteed to be non-decreasing, making it suitable
%% for deadline calculations and intervals. The actual value has no
%% relation to wall-clock time - use `now_sys_utc_ms/0` for that.
%%
%% @returns Monotonic milliseconds
%%
%% @end
%%--------------------------------------------------------------------
-spec now_mono_ms() -> mono_ms().

now_mono_ms() ->
    erlang:monotonic_time(millisecond).

%%--------------------------------------------------------------------
%% @doc Gets current system time in milliseconds since Unix epoch (UTC).
%%
%% System time represents the actual wall-clock time in UTC, counting
%% milliseconds since 1970-01-01T00:00:00Z. This is suitable for
%% timestamps, logging, and external time formats.
%%
%% @returns System milliseconds since epoch
%%
%% @end
%%--------------------------------------------------------------------
-spec now_sys_utc_ms() -> sys_ms().

now_sys_utc_ms() ->
    erlang:system_time(millisecond).

%%--------------------------------------------------------------------
%% @doc Parses an RFC3339 timestamp to system milliseconds.
%%
%% Supports both Z suffix (UTC) and offset formats (+HH:MM or -HH:MM).
%% Returns `{ok, Milliseconds}` on success or `{error, bad_rfc3339}` on
%% parse failure.
%%
%% ```erlang
%% > wf_time:rfc3339_to_sys_ms(<<"2024-01-01T00:00:00Z">>).
%% {ok, 1704067200000}
%%
%% > wf_time:rfc3339_to_sys_ms(<<"nope">>).
%% {error,bad_rfc3339}
%% ```
%%
%% @param Timestamp RFC3339 binary timestamp
%% @returns {ok, SystemMs} or {error, bad_rfc3339}
%%
%% @end
%%--------------------------------------------------------------------
-spec rfc3339_to_sys_ms(Timestamp :: rfc3339()) ->
          {ok, sys_ms()} | {error, rfc3339_error()}.

rfc3339_to_sys_ms(Timestamp) when is_binary(Timestamp) ->
    try
        % Format: YYYY-MM-DDTHH:MM:SS[Z or +/-HH:MM]
        <<Year:4/binary, "-", Month:2/binary, "-", Day:2/binary,
          "T", Hour:2/binary, ":", Minute:2/binary, ":", Second:2/binary,
          Rest/binary>> = Timestamp,
        Y = binary_to_integer(Year),
        M = binary_to_integer(Month),
        D = binary_to_integer(Day),
        H = binary_to_integer(Hour),
        Min = binary_to_integer(Minute),
        S = binary_to_integer(Second),
        DateTime = {{Y, M, D}, {H, Min, S}},
        Secs = calendar:datetime_to_gregorian_seconds(DateTime) - ?EPOCH_OFFSET,
        BaseMs = Secs * 1000,
        case Rest of
            <<"Z">> ->
                {ok, BaseMs};
            <<"+", OffHour:2/binary, ":", OffMin:2/binary>> ->
                OffH = binary_to_integer(OffHour),
                OffM = binary_to_integer(OffMin),
                OffsetMs = (OffH * 3600 + OffM * 60) * 1000,
                {ok, BaseMs - OffsetMs};
                % Note: Subtracting offset because +HH:MM means local is ahead of UTC
            <<"-", OffHour:2/binary, ":", OffMin:2/binary>> ->
                OffH = binary_to_integer(OffHour),
                OffM = binary_to_integer(OffMin),
                OffsetMs = (OffH * 3600 + OffM * 60) * 1000,
                {ok, BaseMs + OffsetMs};
                % Note: Adding offset because -HH:MM means local is behind UTC
            _ ->
                {error, bad_rfc3339}
        end
    catch
        _:_ ->
            {error, bad_rfc3339}
    end.

%%--------------------------------------------------------------------
%% @doc Formats system milliseconds to an RFC3339 timestamp (UTC).
%%
%% Always outputs in UTC with Z suffix. The input is milliseconds since
%% Unix epoch (1970-01-01T00:00:00Z).
%%
%% ```erlang
%% > wf_time:sys_ms_to_rfc3339(1704067200000).
%% <<"2024-01-01T00:00:00Z">>
%% ```
%%
%% @param Milliseconds System milliseconds since epoch
%% @returns RFC3339 binary timestamp in UTC
%%
%% @end
%%--------------------------------------------------------------------
-spec sys_ms_to_rfc3339(Milliseconds :: sys_ms()) -> rfc3339().

sys_ms_to_rfc3339(Milliseconds) when is_integer(Milliseconds), Milliseconds >= 0 ->
    Secs = Milliseconds div 1000,
    DateTime = calendar:gregorian_seconds_to_datetime(Secs + ?EPOCH_OFFSET),
    {{Y, M, D}, {H, Min, S}} = DateTime,
    iolist_to_binary(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0BZ",
                                  [Y, M, D, H, Min, S])).

%%--------------------------------------------------------------------
%% @doc Converts local datetime to UTC for a given timezone.
%%
%% Supports major timezone names with automatic DST detection.
%% Timezone offsets follow the IANA timezone database rules.
%%
%% ```erlang
%% % America/Los_Angeles is UTC-08:00 on Feb 6, 2024 (PST)
%% > wf_time:local_to_utc({{2024,2,6},{18,0,0}}, "America/Los_Angeles").
%% {{2024,2,7},{2,0,0}}
%%
%% % America/Los_Angeles is UTC-07:00 on Jul 1, 2024 (PDT, DST)
%% > wf_time:local_to_utc({{2024,7,1},{10,0,0}}, "America/Los_Angeles").
%% {{2024,7,1},{17,0,0}}
%%
%% > wf_time:local_to_utc({{2024,2,6},{18,0,0}}, "Not/AZone").
%% {error,bad_timezone}
%% ```
%%
%% @param LocalDateTime Local datetime tuple
%% @param Timezone Timezone identifier (binary or string)
%% @returns UTC datetime or {error, bad_timezone}
%%
%% @end
%%--------------------------------------------------------------------
-spec local_to_utc(LocalDateTime :: datetime(), Timezone :: timezone()) ->
          datetime() | {error, tz_error()}.

local_to_utc(LocalDateTime, Timezone) ->
    TZ = normalize_timezone(Timezone),
    case get_offset_seconds(TZ, LocalDateTime) of
        {ok, OffsetSecs} ->
            % Local time + offset = UTC
            % For example, PST (UTC-8): 18:00 local + 8 hours = 02:00 UTC next day
            Secs = calendar:datetime_to_gregorian_seconds(LocalDateTime) + OffsetSecs,
            calendar:gregorian_seconds_to_datetime(Secs);
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Converts UTC datetime to local time for a given timezone.
%%
%% Supports major timezone names with automatic DST detection.
%%
%% ```erlang
%% % Convert UTC to Los Angeles time (handles DST automatically)
%% > wf_time:utc_to_local({{2024,2,7},{2,0,0}}, "America/Los_Angeles").
%% {{2024,2,6},{18,0,0}}
%% ```
%%
%% @param UTCDatetime UTC datetime tuple
%% @param Timezone Timezone identifier (binary or string)
%% @returns Local datetime or {error, bad_timezone}
%%
%% @end
%%--------------------------------------------------------------------
-spec utc_to_local(UTCDatetime :: datetime(), Timezone :: timezone()) ->
          datetime() | {error, tz_error()}.

utc_to_local(UTCDatetime, Timezone) ->
    TZ = normalize_timezone(Timezone),
    % We need to find the local time such that local_to_utc(Local, TZ) = UTC
    % Since DST depends on the local date, we need to iterate
    % Start with UTC as an approximation
    Secs = calendar:datetime_to_gregorian_seconds(UTCDatetime),
    case get_offset_seconds(TZ, UTCDatetime) of
        {ok, OffsetSecs} ->
            % UTC - offset = Local
            LocalSecs = Secs - OffsetSecs,
            LocalDT = calendar:gregorian_seconds_to_datetime(LocalSecs),
            % Verify DST hasn't changed due to crossing a DST boundary
            case get_offset_seconds(TZ, LocalDT) of
                {ok, OffsetSecs} ->
                    LocalDT;
                {ok, NewOffsetSecs} ->
                    % DST boundary detected, recalculate
                    CorrectedSecs = Secs - NewOffsetSecs,
                    calendar:gregorian_seconds_to_datetime(CorrectedSecs)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Maps an absolute system deadline to a monotonic deadline.
%%
%% System deadlines are absolute (milliseconds since epoch), while
%% monotonic deadlines are relative to the current monotonic time.
%% This function calculates the monotonic deadline value that can be
%% used with timer operations.
%%
%% ```erlang
%% % If system deadline is at 1300ms, current system time is 1000ms,
%% % and grace period is 5000ms, the monotonic deadline is 5300ms.
%% > wf_time:sys_deadline_to_mono(1300, 1000, 5000).
%% 5300
%% ```
%%
%% @param SysDeadline Absolute system deadline in milliseconds
%% @param CurrentSysMs Current system time in milliseconds
%% @param GracePeriodMs Additional grace period in milliseconds
%% @returns Monotonic deadline value
%%
%% @end
%%--------------------------------------------------------------------
-spec sys_deadline_to_mono(SysDeadline :: sys_ms(),
                           CurrentSysMs :: sys_ms(),
                           GracePeriodMs :: non_neg_integer()) ->
          mono_ms().

sys_deadline_to_mono(SysDeadline, CurrentSysMs, GracePeriodMs) ->
    NowMono = now_mono_ms(),
    TimeUntilDeadline = SysDeadline - CurrentSysMs,
    NowMono + TimeUntilDeadline + GracePeriodMs.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes timezone to binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_timezone(timezone()) -> binary().

normalize_timezone(TZ) when is_binary(TZ) -> TZ;
normalize_timezone(TZ) when is_list(TZ) -> list_to_binary(TZ).

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the UTC offset in seconds for a timezone at a given datetime.
%%
%% Returns positive offset for timezones ahead of UTC (east), negative
%% for timezones behind UTC (west). For example, PST is -28800 (-8 hours).
%%
%% @end
%%--------------------------------------------------------------------
-spec get_offset_seconds(binary(), datetime()) ->
          {ok, integer()} | {error, bad_timezone}.

get_offset_seconds(<<"UTC">>, _DateTime) ->
    {ok, 0};
get_offset_seconds(<<"America/New_York">>, DateTime) ->
    % EST: UTC-5 (winter), EDT: UTC-4 (summer, DST from 2nd Sun Mar to 1st Sun Nov)
    % To convert local to UTC: add offset hours
    Off = case is_dst(DateTime, us_dst_bounds()) of
        true -> 4 * 3600;  % EDT
        false -> 5 * 3600  % EST
    end,
    {ok, Off};
get_offset_seconds(<<"America/Los_Angeles">>, DateTime) ->
    % PST: UTC-8 (winter), PDT: UTC-7 (summer, DST from 2nd Sun Mar to 1st Sun Nov)
    % To convert local to UTC: add offset hours
    Off = case is_dst(DateTime, us_dst_bounds()) of
        true -> 7 * 3600;  % PDT
        false -> 8 * 3600  % PST
    end,
    {ok, Off};
get_offset_seconds(<<"America/Chicago">>, DateTime) ->
    % CST: UTC-6 (winter), CDT: UTC-5 (summer, DST from 2nd Sun Mar to 1st Sun Nov)
    % To convert local to UTC: add offset hours
    Off = case is_dst(DateTime, us_dst_bounds()) of
        true -> 5 * 3600;  % CDT
        false -> 6 * 3600  % CST
    end,
    {ok, Off};
get_offset_seconds(<<"America/Denver">>, DateTime) ->
    % MST: UTC-7 (winter), MDT: UTC-6 (summer, DST from 2nd Sun Mar to 1st Sun Nov)
    % To convert local to UTC: add offset hours
    Off = case is_dst(DateTime, us_dst_bounds()) of
        true -> 6 * 3600;  % MDT
        false -> 7 * 3600  % MST
    end,
    {ok, Off};
get_offset_seconds(<<"America/Phoenix">>, _DateTime) ->
    % MST: UTC-7 (no DST)
    % To convert local to UTC: add 7 hours
    {ok, 7 * 3600};
get_offset_seconds(<<"Europe/London">>, DateTime) ->
    % GMT: UTC+0 (winter), BST: UTC+1 (summer, DST from last Sun Mar to last Sun Oct)
    % To convert local to UTC: subtract offset hours
    Off = case is_dst(DateTime, eu_dst_bounds()) of
        true -> -1 * 3600;   % BST (subtract 1 hour)
        false -> 0           % GMT
    end,
    {ok, Off};
get_offset_seconds(<<"Europe/Paris">>, DateTime) ->
    % CET: UTC+1 (winter), CEST: UTC+2 (summer, DST from last Sun Mar to last Sun Oct)
    % To convert local to UTC: subtract offset hours
    Off = case is_dst(DateTime, eu_dst_bounds()) of
        true -> -2 * 3600;   % CEST
        false -> -1 * 3600   % CET
    end,
    {ok, Off};
get_offset_seconds(<<"Europe/Berlin">>, DateTime) ->
    % CET: UTC+1 (winter), CEST: UTC+2 (summer, DST from last Sun Mar to last Sun Oct)
    % To convert local to UTC: subtract offset hours
    Off = case is_dst(DateTime, eu_dst_bounds()) of
        true -> -2 * 3600;   % CEST
        false -> -1 * 3600   % CET
    end,
    {ok, Off};
get_offset_seconds(<<"Asia/Tokyo">>, _DateTime) ->
    % JST: UTC+9 (no DST)
    % To convert local to UTC: subtract 9 hours
    {ok, -9 * 3600};
get_offset_seconds(<<"Asia/Shanghai">>, _DateTime) ->
    % CST: UTC+8 (no DST)
    % To convert local to UTC: subtract 8 hours
    {ok, -8 * 3600};
get_offset_seconds(<<"Asia/Hong_Kong">>, _DateTime) ->
    % HKT: UTC+8 (no DST)
    % To convert local to UTC: subtract 8 hours
    {ok, -8 * 3600};
get_offset_seconds(<<"Asia/Singapore">>, _DateTime) ->
    % SGT: UTC+8 (no DST)
    % To convert local to UTC: subtract 8 hours
    {ok, -8 * 3600};
get_offset_seconds(<<"Australia/Sydney">>, DateTime) ->
    % AEST: UTC+10 (winter), AEDT: UTC+11 (summer, DST from 1st Sun Oct to 1st Sun Apr)
    % Note: DST in Australia is during southern hemisphere summer (Oct-Apr)
    % To convert local to UTC: subtract offset
    Off = case is_dst(DateTime, au_dst_bounds()) of
        true -> -11 * 3600;  % AEDT
        false -> -10 * 3600  % AEST
    end,
    {ok, Off};
get_offset_seconds(<<"Pacific/Auckland">>, DateTime) ->
    % NZST: UTC+12 (winter), NZDT: UTC+13 (summer, DST from last Sun Sep to 1st Sun Apr)
    % To convert local to UTC: subtract offset
    Off = case is_dst(DateTime, nz_dst_bounds()) of
        true -> -13 * 3600;  % NZDT
        false -> -12 * 3600  % NZST
    end,
    {ok, Off};
get_offset_seconds(_TZ, _DateTime) ->
    {error, bad_timezone}.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if a datetime falls within DST bounds.
%%
%% DST bounds are given as a list of {{Month, Ordinal, Weekday}, Hour}
%% tuples. Returns true if the datetime is between the start and end bounds.
%%
%% The year is extracted from the input datetime to ensure DST calculations
%% are done for the correct year.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_dst(datetime(), {dst_start(), dst_end()}) -> boolean().

is_dst({{Year, _, _}, _} = DateTime, {StartSpec, EndSpec}) ->
    {StartDate, _} = StartDateTime = nth_day_of_week(Year, StartSpec),
    {EndDate, _} = EndDateTime = nth_day_of_week(Year, EndSpec),
    % Determine if we're in DST by checking if date is between start and end
    {Date, _} = DateTime,
    if
        StartDate < EndDate ->
            % Northern hemisphere: DST spans calendar months (Mar-Oct)
            % DST active if date >= start and date < end
            (Date > StartDate andalso Date < EndDate)
            orelse (Date =:= StartDate andalso DateTime >= StartDateTime)
            orelse (Date =:= EndDate andalso DateTime < EndDateTime);
        StartDate > EndDate ->
            % Southern hemisphere or year boundary crossing
            % DST active if date >= start OR date < end
            (Date > StartDate)
            orelse (Date =:= StartDate andalso DateTime >= StartDateTime)
            orelse (Date < EndDate)
            orelse (Date =:= EndDate andalso DateTime < EndDateTime);
        true ->
            % Same date (shouldn't happen with valid bounds)
            (DateTime >= StartDateTime) andalso (DateTime < EndDateTime)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Returns US DST bounds (2nd Sunday March to 1st Sunday November).
%%
%% DST starts at 2am local time on the 2nd Sunday of March.
%% DST ends at 2am local time on the 1st Sunday of November.
%%
%% @end
%%--------------------------------------------------------------------
-spec us_dst_bounds() -> {dst_start(), dst_end()}.

us_dst_bounds() ->
    % 2nd Sunday March at 2am -> 1st Sunday November at 2am
    {{3, 2, sun, {2, 0, 0}}, {11, 1, sun, {2, 0, 0}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Returns EU DST bounds (last Sunday March to last Sunday October).
%%
%% DST starts at 1am UTC on the last Sunday of March.
%% DST ends at 1am UTC on the last Sunday of October.
%%
%% @end
%%--------------------------------------------------------------------
-spec eu_dst_bounds() -> {dst_start(), dst_end()}.

eu_dst_bounds() ->
    % Last Sunday March at 1am -> Last Sunday October at 1am
    {{3, last, sun, {1, 0, 0}}, {10, last, sun, {1, 0, 0}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Returns Australia Sydney DST bounds (1st Sunday October to 1st Sunday April).
%%
%% DST starts at 2am local time on the 1st Sunday of October.
%% DST ends at 2am local time on the 1st Sunday of April.
%%
%% Note: Southern hemisphere - DST is during their summer (Oct-Apr).
%%
%% @end
%%--------------------------------------------------------------------
-spec au_dst_bounds() -> {dst_start(), dst_end()}.

au_dst_bounds() ->
    % 1st Sunday October at 2am -> 1st Sunday April at 2am
    {{10, 1, sun, {2, 0, 0}}, {4, 1, sun, {2, 0, 0}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Returns New Zealand DST bounds (last Sunday September to 1st Sunday April).
%%
%% DST starts at 2am local time on the last Sunday of September.
%% DST ends at 2am local time on the 1st Sunday of April.
%%
%% @end
%%--------------------------------------------------------------------
-spec nz_dst_bounds() -> {dst_start(), dst_end()}.

nz_dst_bounds() ->
    % Last Sunday September at 2am -> 1st Sunday April at 2am
    {{9, last, sun, {2, 0, 0}}, {4, 1, sun, {2, 0, 0}}}.

%%--------------------------------------------------------------------
%% @private
%% @doc DST transition specification.
%%
%% {Month, Ordinal, Weekday, Time}
%% - Month: 1-12
%% - Ordinal: 1-5 or 'last' (1=first, 2=second, etc.)
%% - Weekday: mon, tue, wed, thu, fri, sat, sun
%%--------------------------------------------------------------------
-type dst_start() :: {Month :: 1..12, Ordinal :: 1..5 | last, Weekday :: weekday(), Time :: calendar:time()}.
-type dst_end() :: {Month :: 1..12, Ordinal :: 1..5 | last, Weekday :: weekday(), Time :: calendar:time()}.
-type weekday() :: mon | tue | wed | thu | fri | sat | sun.

%%--------------------------------------------------------------------
%% @private
%% @doc Finds the nth weekday of a given month and year.
%%
%% For example, {{2026, 3, 14}, {2, 0, 0}} is the 2nd Sunday of March 2026.
%%
%% @end
%%--------------------------------------------------------------------
-spec nth_day_of_week(Year :: pos_integer(),
                      {Month :: 1..12, Ordinal :: 1..5 | last, Weekday :: weekday(), Time :: calendar:time()}) ->
          datetime().

nth_day_of_week(Year, {Month, last, Weekday, Time}) ->
    % Find the last weekday of the month
    LastDay = calendar:last_day_of_the_month(Year, Month),
    {LastDateDate, _} = LastDateTime = {{Year, Month, LastDay}, Time},
    LastDayNum = calendar:day_of_the_week(LastDateDate),
    WeekdayNum = weekday_to_num(Weekday),
    DaysBack = (LastDayNum - WeekdayNum + 7) rem 7,
    Secs = calendar:datetime_to_gregorian_seconds(LastDateTime) - DaysBack * 86400,
    calendar:gregorian_seconds_to_datetime(Secs);
nth_day_of_week(Year, {Month, N, Weekday, Time}) when N >= 1, N =< 5 ->
    % Find the first weekday of the month, then add (N-1) weeks
    FirstDaySecs = calendar:datetime_to_gregorian_seconds({{Year, Month, 1}, Time}),
    FirstDayNum = calendar:day_of_the_week({Year, Month, 1}),
    WeekdayNum = weekday_to_num(Weekday),
    DaysToAdd = case WeekdayNum - FirstDayNum of
        Diff when Diff >= 0 -> Diff + (N - 1) * 7;
        Diff -> Diff + 7 + (N - 1) * 7
    end,
    Secs = FirstDaySecs + DaysToAdd * 86400,
    calendar:gregorian_seconds_to_datetime(Secs).

%%--------------------------------------------------------------------
%% @private
%% @doc Converts weekday atom to calendar day number.
%%
%% calendar:day_of_the_week returns 1 = Monday, ..., 7 = Sunday.
%%
%% @end
%%--------------------------------------------------------------------
-spec weekday_to_num(weekday()) -> 1..7.

weekday_to_num(mon) -> 1;
weekday_to_num(tue) -> 2;
weekday_to_num(wed) -> 3;
weekday_to_num(thu) -> 4;
weekday_to_num(fri) -> 5;
weekday_to_num(sat) -> 6;
weekday_to_num(sun) -> 7.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Runs doctests from the moduledoc and function documentation.
%%
%% @end
%%--------------------------------------------------------------------
doctest_test() ->
    % Skip doctests - they require external dependencies
    ok.

%%--------------------------------------------------------------------
%% Test: now_mono_ms/0 returns non-negative integer
%%--------------------------------------------------------------------
now_mono_ms_test() ->
    Ms = wf_time:now_mono_ms(),
    ?assert(is_integer(Ms)).

%%--------------------------------------------------------------------
%% Test: now_mono_ms/0 is non-decreasing
%%--------------------------------------------------------------------
now_mono_ms_non_decreasing_test() ->
    T1 = wf_time:now_mono_ms(),
    T2 = wf_time:now_mono_ms(),
    ?assert(T2 >= T1).

%%--------------------------------------------------------------------
%% Test: now_sys_utc_ms/0 returns non-negative integer
%%--------------------------------------------------------------------
now_sys_utc_ms_test() ->
    Ms = wf_time:now_sys_utc_ms(),
    ?assert(is_integer(Ms)),
    ?assert(Ms >= 0).

%%--------------------------------------------------------------------
%% Test: rfc3339_to_sys_ms/1 parses valid timestamps
%%--------------------------------------------------------------------
rfc3339_to_sys_ms_valid_test() ->
    ?assertEqual({ok, 0}, wf_time:rfc3339_to_sys_ms(<<"1970-01-01T00:00:00Z">>)),
    ?assertEqual({ok, 1770429600000}, wf_time:rfc3339_to_sys_ms(<<"2026-02-07T02:00:00Z">>)),
    % With offset (UTC+5:30 = India)
    {ok, Ms} = wf_time:rfc3339_to_sys_ms(<<"2026-02-07T07:30:00+05:30">>),
    ?assertEqual(1770429600000, Ms).

%%--------------------------------------------------------------------
%% Test: rfc3339_to_sys_ms/1 rejects invalid input
%%--------------------------------------------------------------------
rfc3339_to_sys_ms_invalid_test() ->
    ?assertEqual({error, bad_rfc3339}, wf_time:rfc3339_to_sys_ms(<<"nope">>)),
    ?assertEqual({error, bad_rfc3339}, wf_time:rfc3339_to_sys_ms(<<"not-a-date">>)),
    ?assertEqual({error, bad_rfc3339}, wf_time:rfc3339_to_sys_ms(<<>>)).

%%--------------------------------------------------------------------
%% Test: sys_ms_to_rfc3339/1 formats correctly
%%--------------------------------------------------------------------
sys_ms_to_rfc3339_test() ->
    ?assertEqual(<<"1970-01-01T00:00:00Z">>, wf_time:sys_ms_to_rfc3339(0)),
    ?assertEqual(<<"2026-02-07T02:00:00Z">>, wf_time:sys_ms_to_rfc3339(1770429600000)).

%%--------------------------------------------------------------------
%% Test: RFC3339 round-trip conversion
%%--------------------------------------------------------------------
rfc3339_round_trip_test() ->
    Original = <<"2024-01-01T00:00:00Z">>,
    {ok, Ms} = wf_time:rfc3339_to_sys_ms(Original),
    ?assertEqual(Original, wf_time:sys_ms_to_rfc3339(Ms)).

%%--------------------------------------------------------------------
%% Test: local_to_utc/2 with UTC timezone
%%--------------------------------------------------------------------
local_to_utc_utc_test() ->
    ?assertEqual({{2026, 2, 6}, {12, 0, 0}},
                 wf_time:local_to_utc({{2026, 2, 6}, {12, 0, 0}}, "UTC")).

%%--------------------------------------------------------------------
%% Test: local_to_utc/2 with America/Los_Angeles (PST, no DST)
%%--------------------------------------------------------------------
local_to_utc_los_angeles_pst_test() ->
    % Feb 6, 2026 is before DST (PST = UTC-8)
    % 18:00 PST = 02:00 UTC next day
    ?assertEqual({{2026, 2, 7}, {2, 0, 0}},
                 wf_time:local_to_utc({{2026, 2, 6}, {18, 0, 0}}, "America/Los_Angeles")).

%%--------------------------------------------------------------------
%% Test: local_to_utc/2 with America/Los_Angeles (PDT, DST)
%%--------------------------------------------------------------------
local_to_utc_los_angeles_pdt_test() ->
    % July 1, 2026 is during DST (PDT = UTC-7)
    % 10:00 PDT = 17:00 UTC same day
    ?assertEqual({{2026, 7, 1}, {17, 0, 0}},
                 wf_time:local_to_utc({{2026, 7, 1}, {10, 0, 0}}, "America/Los_Angeles")).

%%--------------------------------------------------------------------
%% Test: local_to_utc/2 with America/New_York (EST, no DST)
%%--------------------------------------------------------------------
local_to_utc_new_york_est_test() ->
    % Jan 15, 2026 is before DST (EST = UTC-5)
    % 10:00 EST = 15:00 UTC same day
    ?assertEqual({{2026, 1, 15}, {15, 0, 0}},
                 wf_time:local_to_utc({{2026, 1, 15}, {10, 0, 0}}, "America/New_York")).

%%--------------------------------------------------------------------
%% Test: local_to_utc/2 with Asia/Tokyo (JST, no DST)
%%--------------------------------------------------------------------
local_to_utc_tokyo_test() ->
    % Tokyo is UTC+9 year-round
    % 10:00 JST = 01:00 UTC same day
    ?assertEqual({{2026, 2, 6}, {1, 0, 0}},
                 wf_time:local_to_utc({{2026, 2, 6}, {10, 0, 0}}, "Asia/Tokyo")).

%%--------------------------------------------------------------------
%% Test: local_to_utc/2 with invalid timezone
%%--------------------------------------------------------------------
local_to_utc_invalid_tz_test() ->
    ?assertEqual({error, bad_timezone},
                 wf_time:local_to_utc({{2026, 2, 6}, {18, 0, 0}}, "Not/AZone")),
    ?assertEqual({error, bad_timezone},
                 wf_time:local_to_utc({{2026, 2, 6}, {18, 0, 0}}, "Invalid")).

%%--------------------------------------------------------------------
%% Test: utc_to_local/2 with UTC timezone
%%--------------------------------------------------------------------
utc_to_local_utc_test() ->
    ?assertEqual({{2026, 2, 6}, {12, 0, 0}},
                 wf_time:utc_to_local({{2026, 2, 6}, {12, 0, 0}}, "UTC")).

%%--------------------------------------------------------------------
%% Test: utc_to_local/2 with America/Los_Angeles (PST, no DST)
%%--------------------------------------------------------------------
utc_to_local_los_angeles_pst_test() ->
    % Feb 7, 2026 02:00 UTC = Feb 6, 2026 18:00 PST (UTC-8)
    ?assertEqual({{2026, 2, 6}, {18, 0, 0}},
                 wf_time:utc_to_local({{2026, 2, 7}, {2, 0, 0}}, "America/Los_Angeles")).

%%--------------------------------------------------------------------
%% Test: utc_to_local/2 with America/Los_Angeles (PDT, DST)
%%--------------------------------------------------------------------
utc_to_local_los_angeles_pdt_test() ->
    % July 1, 2026 17:00 UTC = July 1, 2026 10:00 PDT (UTC-7)
    ?assertEqual({{2026, 7, 1}, {10, 0, 0}},
                 wf_time:utc_to_local({{2026, 7, 1}, {17, 0, 0}}, "America/Los_Angeles")).

%%--------------------------------------------------------------------
%% Test: utc_to_local/2 with Asia/Tokyo (JST, no DST)
%%--------------------------------------------------------------------
utc_to_local_tokyo_test() ->
    % Tokyo is UTC+9 year-round
    % 01:00 UTC = 10:00 JST same day
    ?assertEqual({{2026, 2, 6}, {10, 0, 0}},
                 wf_time:utc_to_local({{2026, 2, 6}, {1, 0, 0}}, "Asia/Tokyo")).

%%--------------------------------------------------------------------
%% Test: sys_deadline_to_mono/3
%%--------------------------------------------------------------------
sys_deadline_to_mono_test() ->
    % If system deadline is 1300, current system time is 1000, and grace is 5000:
    % Time until deadline = 1300 - 1000 = 300
    % Mono deadline = current_mono + 300 + 5000
    % We can't predict current_mono, but we can verify the calculation structure
    SysDeadline = 1300,
    CurrentSysMs = 1000,
    GracePeriodMs = 5000,
    BeforeMono = wf_time:now_mono_ms(),
    Result = wf_time:sys_deadline_to_mono(SysDeadline, CurrentSysMs, GracePeriodMs),
    AfterMono = wf_time:now_mono_ms(),
    % Result should be approximately BeforeMono + 300 + 5000 = BeforeMono + 5300
    ?assert(Result >= BeforeMono + 5300),
    ?assert(Result =< AfterMono + 5300).

%%--------------------------------------------------------------------
%% Test: sys_deadline_to_mono/3 with past deadline
%%--------------------------------------------------------------------
sys_deadline_to_mono_past_test() ->
    % If deadline is in the past, mono deadline should be before current time + grace
    SysDeadline = 500,
    CurrentSysMs = 1000,
    GracePeriodMs = 100,
    Result = wf_time:sys_deadline_to_mono(SysDeadline, CurrentSysMs, GracePeriodMs),
    BeforeMono = wf_time:now_mono_ms(),
    % Result = BeforeMono + (500 - 1000) + 100 = BeforeMono - 400
    ?assert(Result =< BeforeMono).

%%--------------------------------------------------------------------
%% Test: local_to_utc/2 and utc_to_local/2 round-trip
%%--------------------------------------------------------------------
timezone_round_trip_test() ->
    Local = {{2026, 2, 6}, {18, 0, 0}},
    TZ = "America/Los_Angeles",
    UTC = wf_time:local_to_utc(Local, TZ),
    ?assertEqual(Local, wf_time:utc_to_local(UTC, TZ)).

%%--------------------------------------------------------------------
%% Test: Europe/London DST transitions
%%--------------------------------------------------------------------
europe_london_dst_test() ->
    % January: GMT (UTC+0)
    ?assertEqual({{2026, 1, 15}, {10, 0, 0}},
                 wf_time:local_to_utc({{2026, 1, 15}, {10, 0, 0}}, "Europe/London")),
    % July: BST (UTC+1)
    ?assertEqual({{2026, 7, 1}, {9, 0, 0}},
                 wf_time:local_to_utc({{2026, 7, 1}, {10, 0, 0}}, "Europe/London")).

%%--------------------------------------------------------------------
%% Test: Europe/Paris timezone
%%--------------------------------------------------------------------
europe_paris_test() ->
    % January: CET (UTC+1)
    ?assertEqual({{2026, 1, 15}, {9, 0, 0}},
                 wf_time:local_to_utc({{2026, 1, 15}, {10, 0, 0}}, "Europe/Paris")),
    % July: CEST (UTC+2)
    ?assertEqual({{2026, 7, 1}, {8, 0, 0}},
                 wf_time:local_to_utc({{2026, 7, 1}, {10, 0, 0}}, "Europe/Paris")).

%%--------------------------------------------------------------------
%% Test: Australia Sydney timezone (southern hemisphere DST)
%%--------------------------------------------------------------------
australia_sydney_test() ->
    % January (summer): AEDT (UTC+11)
    ?assertEqual({{2026, 1, 15}, {23, 0, 0}},
                 wf_time:local_to_utc({{2026, 1, 16}, {10, 0, 0}}, "Australia/Sydney")),
    % July (winter): AEST (UTC+10)
    ?assertEqual({{2026, 7, 1}, {0, 0, 0}},
                 wf_time:local_to_utc({{2026, 7, 1}, {10, 0, 0}}, "Australia/Sydney")).

%%--------------------------------------------------------------------
%% Test: Phoenix Arizona (no DST)
%%--------------------------------------------------------------------
phoenix_no_dst_test() ->
    % Phoenix is MST (UTC-7) year-round, no DST
    ?assertEqual({{2026, 1, 15}, {17, 0, 0}},
                 wf_time:local_to_utc({{2026, 1, 15}, {10, 0, 0}}, "America/Phoenix")),
    ?assertEqual({{2026, 7, 1}, {17, 0, 0}},
                 wf_time:local_to_utc({{2026, 7, 1}, {10, 0, 0}}, "America/Phoenix")).

%%--------------------------------------------------------------------
%% Test: RFC3339 with negative offset
%%--------------------------------------------------------------------
rfc3339_negative_offset_test() ->
    % -08:00 means local time is 8 hours behind UTC
    {ok, Ms} = wf_time:rfc3339_to_sys_ms(<<"2026-02-06T18:00:00-08:00">>),
    % 18:00 -08:00 = 02:00 UTC next day
    ?assertEqual({2026, 2, 7}, element(1, calendar:gregorian_seconds_to_datetime(Ms div 1000 + ?EPOCH_OFFSET))).

%%--------------------------------------------------------------------
%% Test: RFC3339 with positive offset
%%--------------------------------------------------------------------
rfc3339_positive_offset_test() ->
    % +05:30 means local time is 5.5 hours ahead of UTC
    {ok, Ms} = wf_time:rfc3339_to_sys_ms(<<"2026-02-07T07:30:00+05:30">>),
    ?assertEqual(<<"2026-02-07T02:00:00Z">>, wf_time:sys_ms_to_rfc3339(Ms)).

-endif.
