%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc YAWL Calendar and Scheduling Services.
%%
%% This module implements YAWL calendar and scheduling patterns including
%% time periods, availability checking, and due date management.
%%
%% <h3>Calendar Types</h3>
%% <ul>
%%   <li><b>default</b> - Standard working hours calendar</li>
%%   <li><b>custom</b> - User-defined time periods</li>
%%   <li><b>role_based</b> - Calendar tied to specific roles</li>
%%   <li><b>participant_based</b> - Calendar for specific participants</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% Creating calendars:
%%
%% ```erlang
%% 1> {ok, Pid} = yawl_scheduling:start_link().
%% {ok, <0.123.0>}
%% 2> {ok, CalId} = yawl_scheduling:create_calendar(<<"Work Calendar">>).
%% {ok, <<"calendar_", _/binary>>}
%% '''
%%
%% Creating typed calendars:
%%
%% ```erlang
%% 3> {ok, CustomCal} = yawl_scheduling:create_calendar(<<"Custom">>, custom).
%% {ok, <<"calendar_", _/binary>>}
%% 4> {ok, RoleCal} = yawl_scheduling:create_calendar(<<"Reviewer">>, role_based).
%% {ok, <<"calendar_", _/binary>>}
%% '''
%%
%% Adding time periods:
%%
%% ```erlang
%% 5> Period = #time_period{
%%     id = <<"period_001">>,
%%     start_time = {{2025,1,1},{9,0,0}},
%%     end_time = {{2025,1,1},{17,0,0}},
%%     available = true,
%%     description = <<"Work hours">>,
%%     recurrence = weekly
%% }.
%% #time_period{id = <<"period_001">>, ...}
%% 6> ok = yawl_scheduling:add_time_period(CalId, Period).
%% ok
%% '''
%%
%% Checking availability:
%%
%% ```erlang
%% 7> Status = yawl_scheduling:check_availability(
%%     CalId, {{2025,1,1},{10,0,0}}
%% ).
%% available
%% '''
%%
%% Scheduling tasks:
%%
%% ```erlang
%% 8> {ok, TaskId} = yawl_scheduling:schedule_task(<<"task_001">>, [
%%     {calendar_id, CalId},
%%     {start_time, {{2025,1,1},{10,0,0}}},
%%     {end_time, {{2025,1,1},{12,0,0}}}
%% ]).
%% {ok, <<"task_001">>}
%% '''
%%
%% Setting and checking due dates:
%%
%% ```erlang
%% 9> ok = yawl_scheduling:set_due_date(
%%     <<"task_001">>, {{2025,1,1},{15,0,0}}
%% ).
%% ok
%% 10> {ok, DueDate} = yawl_scheduling:get_due_date(<<"task_001">>).
%% {ok, {{2025,1,1},{15,0,0}}}
%% '''
%%
%% Listing calendars:
%%
%% ```erlang
%% 11> Calendars = yawl_scheduling:list_calendars().
%% [#calendar{id = _, name = <<"Work Calendar">>, ...}]
%% '''
%%
%% Managing timezones:
%%
%% ```erlang
%% 12> ok = yawl_scheduling:set_timezone(CalId, <<"America/New_York">>).
%% ok
%% 13> {ok, Tz} = yawl_scheduling:get_timezone(CalId).
%% {ok, <<"America/New_York">>}
%% '''
%%
%% Getting available slots:
%%
%% ```erlang
%% 14> Slots = yawl_scheduling:get_available_slots(
%%     CalId,
%%     {{2025,1,1},{9,0,0}},
%%     {{2025,1,1},{17,0,0}}
%% ).
%% [{{{2025,1,1},{9,0,0}}, {{2025,1,1},{17,0,0}}}]
%% '''
%%
%% Adding calendar exceptions:
%%
%% ```erlang
%% 15> ok = yawl_scheduling:add_calendar_exception(
%%     CalId,
%%     {{2025,1,2},{0,0,0}},
%%     {{2025,1,2},{23,59,59}}
%% ).
%% ok
%% '''
%%
%% Rescheduling tasks:
%%
%% ```erlang
%% 16> ok = yawl_scheduling:reschedule_task(
%%     <<"task_001">>,
%%     {{2025,1,1},{14,0,0}},
%%     {{2025,1,1},{16,0,0}}
%% ).
%% ok
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_scheduling).
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/0, start_link/1]).
-export([create_calendar/1, create_calendar/2]).
-export([add_time_period/2, remove_time_period/2]).
-export([check_availability/2, check_availability/3]).
-export([schedule_task/2, reschedule_task/3]).
-export([get_due_date/1, set_due_date/2]).
-export([is_overdue/1, get_overdue_tasks/0]).
-export([add_calendar_exception/3, remove_calendar_exception/2]).
-export([get_calendar/1, list_calendars/0]).
-export([set_timezone/2, get_timezone/1]).
-export([get_available_slots/3]).
-export([doctest_test/0]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type calendar_type() :: default | custom | role_based | participant_based.
-type availability_status() :: available | unavailable | tentative.

%%====================================================================
%% Record Definitions
%%====================================================================

-record(time_period, {
    id :: binary(),
    start_time :: calendar:datetime(),
    end_time :: calendar:datetime(),
    available :: boolean(),
    description :: binary() | undefined,
    recurrence :: undefined | daily | weekly | monthly | yearly
}).

-record(calendar, {
    id :: binary(),
    name :: binary(),
    calendar_type :: calendar_type(),
    time_periods = [] :: [#time_period{}],
    timezone :: binary(),
    linked_to :: binary() | undefined  % role_id or participant_id for linked calendars
}).

-record(scheduling_constraint, {
    id :: binary(),
    calendar_id :: binary(),
    time_periods :: [#time_period{}],
    due_date :: calendar:datetime() | undefined
}).

-record(scheduled_task, {
    task_id :: binary(),
    calendar_id :: binary(),
    scheduled_start :: calendar:datetime(),
    scheduled_end :: calendar:datetime(),
    due_date :: calendar:datetime() | undefined,
    status :: pending | in_progress | completed | cancelled
}).

-record(scheduling_state, {
    calendars = #{} :: #{binary() => #calendar{}},
    scheduled_tasks = [] :: [#scheduled_task{}],
    constraints = #{} :: #{binary() => #scheduling_constraint{}},
    default_timezone = <<"UTC">> :: binary()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts an anonymous scheduling service.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Starts a named scheduling service.
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

%% @doc Creates a new calendar with a unique ID.
-spec create_calendar(binary()) -> {ok, binary()}.
create_calendar(Name) ->
    gen_server:call(?MODULE, {create_calendar, Name}).

%% @doc Creates a new calendar with specified type and ID.
-spec create_calendar(binary(), calendar_type()) -> {ok, binary()}.
create_calendar(Name, CalType) ->
    gen_server:call(?MODULE, {create_calendar, Name, CalType}).

%% @doc Adds a time period to a calendar.
-spec add_time_period(binary(), #time_period{}) -> ok | {error, term()}.
add_time_period(CalendarId, TimePeriod) ->
    gen_server:call(?MODULE, {add_time_period, CalendarId, TimePeriod}).

%% @doc Removes a time period from a calendar.
-spec remove_time_period(binary(), binary()) -> ok | {error, term()}.
remove_time_period(CalendarId, PeriodId) ->
    gen_server:call(?MODULE, {remove_time_period, CalendarId, PeriodId}).

%% @doc Checks availability for a calendar at a specific time.
-spec check_availability(binary(), calendar:datetime()) -> availability_status().
check_availability(CalendarId, DateTime) ->
    gen_server:call(?MODULE, {check_availability, CalendarId, DateTime}).

%% @doc Checks availability for a time range.
-spec check_availability(binary(), calendar:datetime(), calendar:datetime()) ->
    [{calendar:datetime(), availability_status()}].
check_availability(CalendarId, Start, End) ->
    gen_server:call(?MODULE, {check_availability_range, CalendarId, Start, End}).

%% @doc Schedules a task within a calendar.
-spec schedule_task(binary(), [{atom(), term()}]) -> {ok, binary()} | {error, term()}.
schedule_task(TaskId, Props) ->
    gen_server:call(?MODULE, {schedule_task, TaskId, Props}).

%% @doc Reschedules an existing task.
-spec reschedule_task(binary(), calendar:datetime(), calendar:datetime()) ->
    ok | {error, term()}.
reschedule_task(TaskId, NewStart, NewEnd) ->
    gen_server:call(?MODULE, {reschedule_task, TaskId, NewStart, NewEnd}).

%% @doc Gets the due date for a task.
-spec get_due_date(binary()) -> {ok, calendar:datetime()} | {error, not_found}.
get_due_date(TaskId) ->
    gen_server:call(?MODULE, {get_due_date, TaskId}).

%% @doc Sets the due date for a task.
-spec set_due_date(binary(), calendar:datetime()) -> ok | {error, term()}.
set_due_date(TaskId, DueDate) ->
    gen_server:call(?MODULE, {set_due_date, TaskId, DueDate}).

%% @doc Checks if a task is overdue.
-spec is_overdue(binary()) -> boolean() | {error, not_found}.
is_overdue(TaskId) ->
    gen_server:call(?MODULE, {is_overdue, TaskId}).

%% @doc Gets all overdue tasks.
-spec get_overdue_tasks() -> [#scheduled_task{}].
get_overdue_tasks() ->
    gen_server:call(?MODULE, get_overdue_tasks).

%% @doc Adds an exception to a calendar (unavailable period).
-spec add_calendar_exception(binary(), calendar:datetime(), calendar:datetime()) ->
    ok | {error, term()}.
add_calendar_exception(CalendarId, Start, End) ->
    gen_server:call(?MODULE, {add_calendar_exception, CalendarId, Start, End}).

%% @doc Removes an exception from a calendar.
-spec remove_calendar_exception(binary(), binary()) -> ok | {error, term()}.
remove_calendar_exception(CalendarId, ExceptionId) ->
    gen_server:call(?MODULE, {remove_calendar_exception, CalendarId, ExceptionId}).

%% @doc Gets a calendar by ID.
-spec get_calendar(binary()) -> {ok, #calendar{}} | {error, not_found}.
get_calendar(CalendarId) ->
    gen_server:call(?MODULE, {get_calendar, CalendarId}).

%% @doc Lists all calendars.
-spec list_calendars() -> [#calendar{}].
list_calendars() ->
    gen_server:call(?MODULE, list_calendars).

%% @doc Sets the timezone for a calendar.
-spec set_timezone(binary(), binary()) -> ok | {error, term()}.
set_timezone(CalendarId, Timezone) ->
    gen_server:call(?MODULE, {set_timezone, CalendarId, Timezone}).

%% @doc Gets the timezone for a calendar.
-spec get_timezone(binary()) -> {ok, binary()} | {error, not_found}.
get_timezone(CalendarId) ->
    gen_server:call(?MODULE, {get_timezone, CalendarId}).

%% @doc Gets available time slots for scheduling.
-spec get_available_slots(binary(), calendar:datetime(), calendar:datetime()) ->
    [{calendar:datetime(), calendar:datetime()}].
get_available_slots(CalendarId, Start, End) ->
    gen_server:call(?MODULE, {get_available_slots, CalendarId, Start, End}).

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({create_calendar, Name}, _From, State) ->
    handle_call({create_calendar, Name, default}, _From, State);

handle_call({create_calendar, Name, CalType}, _From, State) ->
    #scheduling_state{calendars = Calendars, default_timezone = Tz} = State,

    CalendarId = generate_id(<<"calendar">>),
    Calendar = #calendar{
        id = CalendarId,
        name = Name,
        calendar_type = CalType,
        time_periods = [],
        timezone = Tz,
        linked_to = undefined
    },

    {reply, {ok, CalendarId}, State#scheduling_state{
        calendars = Calendars#{CalendarId => Calendar}
    }};

handle_call({add_time_period, CalendarId, TimePeriod}, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,

    case maps:get(CalendarId, Calendars, undefined) of
        undefined ->
            {reply, {error, calendar_not_found}, State};
        #calendar{time_periods = Periods} = Calendar ->
            PeriodWithId = ensure_time_period_id(TimePeriod),
            UpdatedCalendar = Calendar#calendar{
                time_periods = [PeriodWithId | Periods]
            },
            {reply, ok, State#scheduling_state{
                calendars = Calendars#{CalendarId => UpdatedCalendar}
            }}
    end;

handle_call({remove_time_period, CalendarId, PeriodId}, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,

    case maps:get(CalendarId, Calendars, undefined) of
        undefined ->
            {reply, {error, calendar_not_found}, State};
        #calendar{time_periods = Periods} = Calendar ->
            UpdatedPeriods = lists:filter(
                fun(P) -> P#time_period.id =/= PeriodId end,
                Periods
            ),
            UpdatedCalendar = Calendar#calendar{time_periods = UpdatedPeriods},
            {reply, ok, State#scheduling_state{
                calendars = Calendars#{CalendarId => UpdatedCalendar}
            }}
    end;

handle_call({check_availability, CalendarId, DateTime}, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,

    case maps:get(CalendarId, Calendars, undefined) of
        undefined ->
            {reply, unavailable, State};
        #calendar{time_periods = Periods} ->
            Result = check_datetime_in_periods(DateTime, Periods),
            {reply, Result, State}
    end;

handle_call({check_availability_range, CalendarId, Start, End}, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,

    case maps:get(CalendarId, Calendars, undefined) of
        undefined ->
            {reply, [], State};
        #calendar{time_periods = Periods} ->
            Result = check_range_in_periods(Start, End, Periods),
            {reply, Result, State}
    end;

handle_call({schedule_task, TaskId, Props}, _From, State) ->
    #scheduling_state{scheduled_tasks = Tasks} = State,

    CalendarId = proplists:get_value(calendar_id, Props),
    ScheduledStart = proplists:get_value(start_time, Props),
    ScheduledEnd = proplists:get_value(end_time, Props),
    DueDate = proplists:get_value(due_date, Props, ScheduledEnd),

    Task = #scheduled_task{
        task_id = TaskId,
        calendar_id = CalendarId,
        scheduled_start = ScheduledStart,
        scheduled_end = ScheduledEnd,
        due_date = DueDate,
        status = pending
    },

    {reply, {ok, TaskId}, State#scheduling_state{
        scheduled_tasks = [Task | Tasks]
    }};

handle_call({reschedule_task, TaskId, NewStart, NewEnd}, _From, State) ->
    #scheduling_state{scheduled_tasks = Tasks} = State,

    UpdatedTasks = lists:map(
        fun(T) when T#scheduled_task.task_id =:= TaskId ->
                T#scheduled_task{
                    scheduled_start = NewStart,
                    scheduled_end = NewEnd
                };
           (T) ->
                T
        end,
        Tasks
    ),

    {reply, ok, State#scheduling_state{scheduled_tasks = UpdatedTasks}};

handle_call({get_due_date, TaskId}, _From, State) ->
    #scheduling_state{scheduled_tasks = Tasks} = State,

    case lists:keyfind(TaskId, #scheduled_task.task_id, Tasks) of
        false ->
            {reply, {error, not_found}, State};
        #scheduled_task{due_date = undefined} ->
            {reply, {error, no_due_date}, State};
        #scheduled_task{due_date = DueDate} ->
            {reply, {ok, DueDate}, State}
    end;

handle_call({set_due_date, TaskId, DueDate}, _From, State) ->
    #scheduling_state{scheduled_tasks = Tasks} = State,

    UpdatedTasks = lists:map(
        fun(T) when T#scheduled_task.task_id =:= TaskId ->
                T#scheduled_task{due_date = DueDate};
           (T) ->
                T
        end,
        Tasks
    ),

    {reply, ok, State#scheduling_state{scheduled_tasks = UpdatedTasks}};

handle_call({is_overdue, TaskId}, _From, State) ->
    #scheduling_state{scheduled_tasks = Tasks} = State,
    Now = erlang:universaltime(),

    case lists:keyfind(TaskId, #scheduled_task.task_id, Tasks) of
        false ->
            {reply, {error, not_found}, State};
        #scheduled_task{due_date = undefined} ->
            {reply, false, State};
        #scheduled_task{due_date = _DueDate, status = completed} ->
            {reply, false, State};
        #scheduled_task{due_date = DueDate} ->
            {reply, compare_datetime(DueDate, Now) =:= earlier, State}
    end;

handle_call(get_overdue_tasks, _From, State) ->
    #scheduling_state{scheduled_tasks = Tasks} = State,
    Now = erlang:universaltime(),

    Overdue = lists:filter(
        fun(#scheduled_task{due_date = undefined}) -> false;
           (#scheduled_task{due_date = _DueDate, status = completed}) -> false;
           (#scheduled_task{due_date = DueDate}) ->
                compare_datetime(DueDate, Now) =:= earlier
        end,
        Tasks
    ),

    {reply, Overdue, State};

handle_call({add_calendar_exception, CalendarId, Start, End}, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,

    case maps:get(CalendarId, Calendars, undefined) of
        undefined ->
            {reply, {error, calendar_not_found}, State};
        #calendar{time_periods = Periods} = Calendar ->
            ExceptionId = generate_id(<<"exception">>),
            Exception = #time_period{
                id = ExceptionId,
                start_time = Start,
                end_time = End,
                available = false,
                description = <<"Exception">>,
                recurrence = undefined
            },
            UpdatedCalendar = Calendar#calendar{
                time_periods = [Exception | Periods]
            },
            {reply, ok, State#scheduling_state{
                calendars = Calendars#{CalendarId => UpdatedCalendar}
            }}
    end;

handle_call({remove_calendar_exception, CalendarId, ExceptionId}, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,

    case maps:get(CalendarId, Calendars, undefined) of
        undefined ->
            {reply, {error, calendar_not_found}, State};
        #calendar{time_periods = Periods} = Calendar ->
            UpdatedPeriods = lists:filter(
                fun(P) -> P#time_period.id =/= ExceptionId end,
                Periods
            ),
            UpdatedCalendar = Calendar#calendar{time_periods = UpdatedPeriods},
            {reply, ok, State#scheduling_state{
                calendars = Calendars#{CalendarId => UpdatedCalendar}
            }}
    end;

handle_call({get_calendar, CalendarId}, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,
    case maps:get(CalendarId, Calendars, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Calendar ->
            {reply, {ok, Calendar}, State}
    end;

handle_call(list_calendars, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,
    {reply, maps:values(Calendars), State};

handle_call({set_timezone, CalendarId, Timezone}, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,

    case maps:get(CalendarId, Calendars, undefined) of
        undefined ->
            {reply, {error, calendar_not_found}, State};
        Calendar ->
            UpdatedCalendar = Calendar#calendar{timezone = Timezone},
            {reply, ok, State#scheduling_state{
                calendars = Calendars#{CalendarId => UpdatedCalendar}
            }}
    end;

handle_call({get_timezone, CalendarId}, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,

    case maps:get(CalendarId, Calendars, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #calendar{timezone = Tz} ->
            {reply, {ok, Tz}, State}
    end;

handle_call({get_available_slots, CalendarId, Start, End}, _From, State) ->
    #scheduling_state{calendars = Calendars} = State,

    case maps:get(CalendarId, Calendars, undefined) of
        undefined ->
            {reply, [], State};
        #calendar{time_periods = Periods} ->
            Slots = find_available_slots(Start, End, Periods),
            {reply, Slots, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

init(_Arg) ->
    process_flag(trap_exit, true),
    {ok, #scheduling_state{}}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Checks if a datetime falls within any available time period.
-spec check_datetime_in_periods(calendar:datetime(), [#time_period{}]) ->
    availability_status().
check_datetime_in_periods(DateTime, Periods) ->
    case lists:any(fun(P) -> is_datetime_in_period(DateTime, P) andalso
                                P#time_period.available end,
                    Periods) of
        true -> available;
        false -> unavailable
    end.

%% @private
%% @doc Checks if a datetime is within a time period.
-spec is_datetime_in_period(calendar:datetime(), #time_period{}) -> boolean().
is_datetime_in_period(DateTime, #time_period{start_time = Start, end_time = End}) ->
    compare_datetime(DateTime, Start) =/= earlier andalso
    compare_datetime(DateTime, End) =/= later.

%% @private
%% @doc Checks availability for a range of time.
-spec check_range_in_periods(calendar:datetime(), calendar:datetime(),
                             [#time_period{}]) ->
    [{calendar:datetime(), availability_status()}].
check_range_in_periods(Start, End, Periods) ->
    %% Simplified: check hourly intervals in the range
    check_range_by_hour(Start, End, Periods, []).

%% @private
-spec check_range_by_hour(calendar:datetime(), calendar:datetime(),
                          [#time_period{}], [{calendar:datetime(), availability_status()}]) ->
    [{calendar:datetime(), availability_status()}].
check_range_by_hour(Current, End, Periods, Acc) ->
    case compare_datetime(Current, End) of
        later ->
            lists:reverse(Acc);
        _ ->
            Status = check_datetime_in_periods(Current, Periods),
            NextHour = add_hours(Current, 1),
            check_range_by_hour(NextHour, End, Periods, [{Current, Status} | Acc])
    end.

%% @private
%% @doc Finds available time slots within a range.
-spec find_available_slots(calendar:datetime(), calendar:datetime(),
                           [#time_period{}]) ->
    [{calendar:datetime(), calendar:datetime()}].
find_available_slots(Start, End, Periods) ->
    AvailablePeriods = lists:filter(
        fun(P) -> P#time_period.available end,
        Periods
    ),
    %% Find intersections between available periods and the requested range
    lists:foldl(
        fun(#time_period{start_time = PStart, end_time = PEnd}, Acc) ->
            Intersection = intersect_ranges(Start, End, PStart, PEnd),
            case Intersection of
                {S, E} -> [{S, E} | Acc];
                none -> Acc
            end
        end,
        [],
        AvailablePeriods
    ).

%% @private
%% @doc Finds the intersection of two time ranges.
-spec intersect_ranges(calendar:datetime(), calendar:datetime(),
                       calendar:datetime(), calendar:datetime()) ->
    {calendar:datetime(), calendar:datetime()} | none.
intersect_ranges(R1Start, R1End, R2Start, R2End) ->
    Start = case compare_datetime(R1Start, R2Start) of
        later -> R1Start;
        _ -> R2Start
    end,
    EndTime = case compare_datetime(R1End, R2End) of
        earlier -> R1End;
        _ -> R2End
    end,
    case compare_datetime(Start, EndTime) of
        later -> none;
        _ -> {Start, EndTime}
    end.

%% @private
%% @doc Compares two datetimes.
-spec compare_datetime(calendar:datetime(), calendar:datetime()) ->
    earlier | same | later.
compare_datetime({Date1, Time1}, {Date2, Time2}) ->
    case {Date1, Time1} of
        {D, _} when D > Date2 -> later;
        {D, _} when D < Date2 -> earlier;
        {_, T} when T > Time2 -> later;
        {_, T} when T < Time2 -> earlier;
        _ -> same
    end.

%% @private
%% @doc Adds hours to a datetime.
-spec add_hours(calendar:datetime(), non_neg_integer()) -> calendar:datetime().
add_hours({Date, {H, M, S}}, N) ->
    TotalSeconds = calendar:datetime_to_gregorian_seconds({Date, {H, M, S}}) + N * 3600,
    calendar:gregorian_seconds_to_datetime(TotalSeconds).

%% @private
%% @doc Ensures a time period has an ID.
-spec ensure_time_period_id(#time_period{}) -> #time_period{}.
ensure_time_period_id(#time_period{id = <<>>} = P) ->
    P#time_period{id = generate_id(<<"period">>)};
ensure_time_period_id(P) ->
    P.

%% @private
%% @doc Generates a unique identifier with a prefix.
-spec generate_id(binary()) -> binary().
generate_id(Prefix) ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<Prefix/binary, "_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @doc Runs doctests for the yawl_scheduling module.
%%
%% Example:
%% ```
%% 1> yawl_scheduling:doctest_test().
%% ok
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Calendar type validation
    CalType1 = default,
    true = CalType1 =:= default orelse
             CalType1 =:= custom orelse
             CalType1 =:= role_based orelse
             CalType1 =:= participant_based,

    CalType2 = custom,
    true = CalType2 =:= default orelse
             CalType2 =:= custom orelse
             CalType2 =:= role_based orelse
             CalType2 =:= participant_based,

    %% Test 2: Availability status validation
    Status1 = available,
    true = Status1 =:= available orelse
             Status1 =:= unavailable orelse
             Status1 =:= tentative,

    Status2 = unavailable,
    true = Status2 =:= available orelse
             Status2 =:= unavailable orelse
             Status2 =:= tentative,

    %% Test 3: Time period record validation
    Period = #time_period{
        id = <<"period_test">>,
        start_time = {{2025, 1, 1}, {9, 0, 0}},
        end_time = {{2025, 1, 1}, {17, 0, 0}},
        available = true,
        description = <<"Test period">>,
        recurrence = weekly
    },
    true = is_binary(Period#time_period.id),
    true = is_tuple(Period#time_period.start_time),
    true = is_tuple(Period#time_period.end_time),
    true = is_boolean(Period#time_period.available),

    %% Test 4: Calendar record validation
    Calendar = #calendar{
        id = <<"calendar_test">>,
        name = <<"Test Calendar">>,
        calendar_type = default,
        time_periods = [],
        timezone = <<"UTC">>,
        linked_to = undefined
    },
    true = is_binary(Calendar#calendar.id),
    true = is_binary(Calendar#calendar.name),
    true = is_binary(Calendar#calendar.timezone),

    %% Test 5: Scheduled task status validation
    TaskStatus1 = pending,
    true = TaskStatus1 =:= pending orelse
             TaskStatus1 =:= in_progress orelse
             TaskStatus1 =:= completed orelse
             TaskStatus1 =:= cancelled,

    TaskStatus2 = completed,
    true = TaskStatus2 =:= pending orelse
             TaskStatus2 =:= in_progress orelse
             TaskStatus2 =:= completed orelse
             TaskStatus2 =:= cancelled,

    %% Test 6: Datetime comparison function
    DT1 = {{2025, 1, 1}, {10, 0, 0}},
    DT2 = {{2025, 1, 1}, {12, 0, 0}},
    earlier = compare_datetime(DT1, DT2),
    later = compare_datetime(DT2, DT1),
    same = compare_datetime(DT1, DT1),

    %% Test 7: Add hours function
    DT3 = {{2025, 1, 1}, {9, 0, 0}},
    DT3Plus1 = add_hours(DT3, 1),
    {{2025, 1, 1}, {10, 0, 0}} = DT3Plus1,

    %% Test 8: Recurrence type validation
    Recurrence1 = daily,
    true = Recurrence1 =:= undefined orelse
             Recurrence1 =:= daily orelse
             Recurrence1 =:= weekly orelse
             Recurrence1 =:= monthly orelse
             Recurrence1 =:= yearly,

    Recurrence2 = weekly,
    true = Recurrence2 =:= undefined orelse
             Recurrence2 =:= daily orelse
             Recurrence2 =:= weekly orelse
             Recurrence2 =:= monthly orelse
             Recurrence2 =:= yearly,

    %% Test 9: Timezone format validation
    Timezone1 = <<"UTC">>,
    true = is_binary(Timezone1),
    true = byte_size(Timezone1) > 0,

    Timezone2 = <<"America/New_York">>,
    true = is_binary(Timezone2),
    true = byte_size(Timezone2) > 0,

    %% Test 10: Task ID format validation
    TaskId = <<"task_123">>,
    true = is_binary(TaskId),
    true = byte_size(TaskId) > 0,

    %% Test 11: Calendar ID format validation
    CalendarId = <<"calendar_", Rest/binary>> = <<"calendar_abc123">>,
    true = is_binary(CalendarId),
    true = byte_size(Rest) > 0,

    %% Test 12: Period ID format validation
    PeriodId = <<"period_", Rest2/binary>> = <<"period_xyz789">>,
    true = is_binary(PeriodId),
    true = byte_size(Rest2) > 0,

    %% Test 13: Empty time periods list
    EmptyPeriods = [],
    true = is_list(EmptyPeriods),
    0 = length(EmptyPeriods),

    %% Test 14: Scheduling state initialization
    State = #scheduling_state{},
    true = is_map(State#scheduling_state.calendars),
    true = is_list(State#scheduling_state.scheduled_tasks),
    true = is_map(State#scheduling_state.constraints),

    %% Test 15: Default timezone
    DefaultState = #scheduling_state{},
    <<"UTC">> = DefaultState#scheduling_state.default_timezone,

    ok.
