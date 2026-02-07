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

-module(wf_timer).

-moduledoc """
ISO 8601 duration parser for YAWL timers.

Parses duration strings like P3D, PT12H, PT1H30M into milliseconds.
Used for YAWL timer task configuration.

### Duration Format

ISO 8601 durations consist of:
- `P` designator starts the duration (required)
- Date components before `T`: `Y` (years), `M` (months), `D` (days)
- Time components after `T`: `H` (hours), `M` (minutes), `S` (seconds)
- Components must appear in order: Y, M, D, H, M, S
- At least one component is required

### Examples

```erlang
% Simple duration: 3 days
> wf_timer:to_ms(<<"P3D">>).
259200000

% 12 hours
> wf_timer:to_ms(<<"PT12H">>).
43200000

% 1 hour 30 minutes
> wf_timer:to_ms(<<"PT1H30M">>).
5400000

% 1 day 2 hours 30 minutes
> wf_timer:to_ms(<<"P1DT2H30M">>).
95400000

% Full duration with all components
> wf_timer:to_ms(<<"P1Y2M3DT4H5M6S">>).
36993906000

% Parse returns result tuple
> wf_timer:parse_duration(<<"PT30S">>).
{ok, 30000}

% Invalid format returns error
> wf_timer:parse_duration(<<"invalid">>).
{error, invalid_format}

% To seconds for timeout values
> wf_timer:to_seconds(<<"PT5M">>).
300
```

### Conversion Rules

Duration to milliseconds uses simplified conversions:
- Year: 365 days
- Month: 30 days
- Day: 24 hours
- Hour: 60 minutes
- Minute: 60 seconds
- Second: 1000 milliseconds

Note: Years and months are approximations. For precise deadlines,
use days, hours, minutes, and seconds.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Duration parsing API
-export([parse_duration/1, to_ms/1, to_seconds/1]).

%%====================================================================
%% Types
%%====================================================================

%% ISO 8601 duration string
-type duration() :: binary().

%% Milliseconds since epoch
-type milliseconds() :: non_neg_integer().

%% Seconds since epoch
-type seconds() :: non_neg_integer().

%% Parse result
-type parse_result() :: {ok, milliseconds()} | {error, term()}.

-export_type([duration/0, milliseconds/0, seconds/0, parse_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Parses an ISO 8601 duration string into milliseconds.
%%
%% Returns `{ok, Milliseconds}` on success or `{error, Reason}` on failure.
%%
%% @param DurationBin Binary containing ISO 8601 duration string
%% @returns {ok, Milliseconds} or {error, invalid_format | unknown_unit}
%%
%% @end
%%--------------------------------------------------------------------
-spec parse_duration(DurationBin :: duration()) -> parse_result().

parse_duration(DurationBin) when is_binary(DurationBin) ->
    case parse_components(DurationBin) of
        {ok, Components} ->
            {ok, components_to_ms(Components)};
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Converts an ISO 8601 duration string to milliseconds.
%%
%% Throws on error - use `parse_duration/1` for safe parsing.
%%
%% @param DurationBin Binary containing ISO 8601 duration string
%% @returns Total milliseconds
%%
%% @end
%%--------------------------------------------------------------------
-spec to_ms(DurationBin :: duration()) -> milliseconds().

to_ms(DurationBin) when is_binary(DurationBin) ->
    case parse_duration(DurationBin) of
        {ok, Ms} -> Ms;
        {error, Reason} -> error(Reason)
    end.

%%--------------------------------------------------------------------
%% @doc Converts an ISO 8601 duration string to seconds.
%%
%% Throws on error - use `parse_duration/1` for safe parsing.
%%
%% @param DurationBin Binary containing ISO 8601 duration string
%% @returns Total seconds
%%
%% @end
%%--------------------------------------------------------------------
-spec to_seconds(DurationBin :: duration()) -> seconds().

to_seconds(DurationBin) when is_binary(DurationBin) ->
    to_ms(DurationBin) div 1000.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Parses duration string into component map.
%%
%% Returns {ok, #{...}} or {error, Reason}.
%% @end
%%--------------------------------------------------------------------
-spec parse_components(binary()) ->
    {ok, #{unit() => non_neg_integer()}} | {error, term()}.

parse_components(<<>>) ->
    {error, invalid_format};
parse_components(<<$P, Rest/binary>>) ->
    parse_components_loop(Rest, date, #{});
parse_components(_) ->
    {error, invalid_format}.

%%--------------------------------------------------------------------
%% @private
%% @doc Main parsing loop.
%%
%% State tracks whether we're in date or time part.
%% @end
%%--------------------------------------------------------------------
-type parse_state() :: date | time.

-spec parse_components_loop(binary(), parse_state(), #{unit() => non_neg_integer()}) ->
    {ok, #{unit() => non_neg_integer()}} | {error, term()}.

parse_components_loop(<<>>, _State, Acc) when map_size(Acc) > 0 ->
    {ok, Acc};
parse_components_loop(<<$T, Rest/binary>>, date, Acc) ->
    parse_components_loop(Rest, time, Acc);
parse_components_loop(Bin, State, Acc) ->
    case parse_number(Bin) of
        {ok, Number, Rest} ->
            case Rest of
                <<>> -> {error, invalid_format};
                <<Unit, Rest2/binary>> ->
                    case validate_unit(Unit, State, Acc) of
                        {ok, UnitKey} ->
                            parse_components_loop(Rest2, State, Acc#{UnitKey => Number});
                        {error, _} = Error ->
                            Error
                    end
            end;
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Parses a number from the beginning of a binary.
%%
%% Returns {ok, Number, Rest} or {error, invalid_format}.
%% @end
%%--------------------------------------------------------------------
-spec parse_number(binary()) -> {ok, non_neg_integer(), binary()} | {error, invalid_format}.

parse_number(<<>>) ->
    {error, invalid_format};
parse_number(Bin) ->
    parse_number(Bin, []).

parse_number(<<Digit, Rest/binary>>, Acc) when Digit >= $0, Digit =< $9 ->
    parse_number(Rest, [Digit | Acc]);
parse_number(_Rest, []) ->
    {error, invalid_format};
parse_number(Rest, Acc) ->
    Number = list_to_integer(lists:reverse(Acc)),
    {ok, Number, Rest}.

%%--------------------------------------------------------------------
%% @private
%% @doc Validates a unit character and returns the map key.
%%
%% Also enforces ordering rules.
%% @end
%%--------------------------------------------------------------------
-spec validate_unit(char(), parse_state(), #{unit() => non_neg_integer()}) ->
    {ok, unit()} | {error, invalid_format}.

%% Date units - must be in order: Y, M, D
validate_unit($Y, date, Acc) -> ensure_order(y, [m, d], Acc);
validate_unit($M, date, Acc) -> ensure_order(m, [d], Acc);
validate_unit($D, date, Acc) -> ensure_order(d, [], Acc);

%% Time units - must be in order: H, M, S
validate_unit($H, time, Acc) -> ensure_order(h, [min, s], Acc);
validate_unit($M, time, Acc) -> ensure_order(min, [s], Acc);
validate_unit($S, time, Acc) -> ensure_order(s, [], Acc);

%% Invalid unit for state
validate_unit(_, _, _) ->
    {error, invalid_format}.

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures units appear in correct order.
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_order(unit(), [unit()], #{unit() => non_neg_integer()}) ->
    {ok, unit()} | {error, invalid_format}.

ensure_order(Unit, ForbiddenUnits, Acc) ->
    case lists:any(fun(U) -> maps:is_key(U, Acc) end, ForbiddenUnits) of
        true -> {error, invalid_format};
        false -> {ok, Unit}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts component map to total milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec components_to_ms(#{unit() => non_neg_integer()}) -> milliseconds().

components_to_ms(Components) ->
    Years = maps:get(y, Components, 0),
    Months = maps:get(m, Components, 0),
    Days = maps:get(d, Components, 0),
    Hours = maps:get(h, Components, 0),
    Minutes = maps:get(min, Components, 0),
    Seconds = maps:get(s, Components, 0),

    %% Simplified conversions (not calendar-accurate for Y/M)
    YearMs = Years * 365 * day_ms(),
    MonthMs = Months * 30 * day_ms(),
    DayMs = Days * day_ms(),
    HourMs = Hours * hour_ms(),
    MinuteMs = Minutes * minute_ms(),
    SecondMs = Seconds * 1000,

    YearMs + MonthMs + DayMs + HourMs + MinuteMs + SecondMs.

%%--------------------------------------------------------------------
%% @private
%% @doc Unit conversion constants.
%%
%% @end
%%--------------------------------------------------------------------
-type unit() :: y | m | d | h | min | s.

minute_ms() -> 60000.
hour_ms() -> 3600000.
day_ms() -> 86400000.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Basic day duration test
parse_day_test() ->
    ?assertEqual({ok, 259200000}, parse_duration(<<"P3D">>)),
    ?assertEqual(259200000, to_ms(<<"P3D">>)).

%% Basic hour duration test
parse_hour_test() ->
    ?assertEqual({ok, 43200000}, parse_duration(<<"PT12H">>)),
    ?assertEqual(43200000, to_ms(<<"PT12H">>)).

%% Hour and minute duration test
parse_hour_minute_test() ->
    ?assertEqual({ok, 5400000}, parse_duration(<<"PT1H30M">>)),
    ?assertEqual(5400000, to_ms(<<"PT1H30M">>)).

%% Day and time duration test
parse_day_time_test() ->
    ?assertEqual({ok, 95400000}, parse_duration(<<"P1DT2H30M">>)),
    ?assertEqual(95400000, to_ms(<<"P1DT2H30M">>)).

%% Full duration test
parse_full_test() ->
    ?assertEqual({ok, 36993906000}, parse_duration(<<"P1Y2M3DT4H5M6S">>)),
    ?assertEqual(36993906000, to_ms(<<"P1Y2M3DT4H5M6S">>)).

%% Seconds only test
parse_seconds_test() ->
    ?assertEqual({ok, 30000}, parse_duration(<<"PT30S">>)),
    ?assertEqual(30000, to_ms(<<"PT30S">>)).

%% Minutes only test
parse_minutes_test() ->
    ?assertEqual({ok, 300000}, parse_duration(<<"PT5M">>)),
    ?assertEqual(300000, to_ms(<<"PT5M">>)).

%% Years only test
parse_years_test() ->
    ?assertEqual({ok, 31536000000}, parse_duration(<<"P1Y">>)),
    ?assertEqual(31536000000, to_ms(<<"P1Y">>)).

%% Months only test
parse_months_test() ->
    ?assertEqual({ok, 2592000000}, parse_duration(<<"P1M">>)),
    ?assertEqual(2592000000, to_ms(<<"P1M">>)).

%% Zero values test
parse_zero_test() ->
    ?assertEqual({ok, 0}, parse_duration(<<"P0D">>)),
    ?assertEqual(0, to_ms(<<"P0D">>)).

%% Invalid format - no P designator
parse_no_p_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"3D">>)).

%% Invalid format - empty string
parse_empty_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"">>)).

%% Invalid format - only P
parse_only_p_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"P">>)).

%% Invalid format - only PT
parse_only_pt_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"PT">>)).

%% Invalid format - no number
parse_no_number_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"PD">>)).

%% Invalid characters
parse_invalid_chars_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"P1X">>)).

%% To seconds test (PT1H30M = 5400 seconds)
to_seconds_test() ->
    ?assertEqual(300, to_seconds(<<"PT5M">>)),
    ?assertEqual(5400, to_seconds(<<"PT1H30M">>)),
    ?assertEqual(86400, to_seconds(<<"P1D">>)).

%% Large duration test
parse_large_test() ->
    ?assertEqual({ok, 7776000000}, parse_duration(<<"P90D">>)),
    ?assertEqual(7776000000, to_ms(<<"P90D">>)).

%% Multiple of same component (last wins)
parse_repeat_component_test() ->
    ?assertEqual({ok, 86400000}, parse_duration(<<"P1D1D">>)).

%% No date components, only time
parse_time_only_test() ->
    ?assertEqual({ok, 3661000}, parse_duration(<<"PT1H1M1S">>)).

%% No time components, only date
parse_date_only_test() ->
    ?assertEqual({ok, 950400000}, parse_duration(<<"P11D">>)).

%% All date components (1Y=365d, 2M=60d, 3D=3d => 428d = 36979200000 ms)
parse_all_date_test() ->
    ?assertEqual({ok, 36979200000}, parse_duration(<<"P1Y2M3D">>)).

%% All time components
parse_all_time_test() ->
    ?assertEqual({ok, 14706000}, parse_duration(<<"PT4H5M6S">>)).

%% Decimal numbers not supported (parse error)
parse_decimal_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"PT1.5H">>)).

%% Week component not supported (W is not valid)
parse_week_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"P1W">>)).

%% Out of order components should fail
parse_out_of_order_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"P1DY">>)),
    ?assertEqual({error, invalid_format}, parse_duration(<<"PT1M5H">>)).

%% Time component in date part should fail
parse_time_in_date_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"P1H">>)).

%% Date component in time part should fail
parse_date_in_time_test() ->
    ?assertEqual({error, invalid_format}, parse_duration(<<"PT1D">>)).

%% Very large number
parse_very_large_test() ->
    ?assertEqual({ok, 315360000000000}, parse_duration(<<"P10000Y">>)).

%% Maximum practical duration
parse_max_test() ->
    ?assertEqual({ok, 31536000000000}, parse_duration(<<"P1000Y">>)).

-endif.
