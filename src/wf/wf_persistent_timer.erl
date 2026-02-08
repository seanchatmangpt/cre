%% -*- erlang -*-
%%%% @doc wf_persistent_timer - Persistent timer service with work-day calendar.
%%
%% This module provides durable timers that survive process restarts and
%% support work-day calendars with timezone awareness. Timers are stored in
%% Mnesia and restored on application restart.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Persistent storage:</b> Timers stored in Mnesia for durability</li>
%%   <li><b>Work-day calendar:</b> Skip weekends and holidays</li>
%%   <li><b>Timezone support:</b> Convert times to target timezone</li>
%%   <li><b>Recovery:</b> Restore active timers on restart</li>
%%   <li><b>Retry logic:</b> Configurable retry on failure</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_persistent_timer).
-author("CRE Team").

-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Timer management
-export([start_link/0]).
-export([start_link/1]).
-export([start_timer/2]).
-export([start_timer/3]).
-export([start_timer/4]).
-export([cancel_timer/1]).
-export([cancel_all_timers/0]).
-export([get_timer/1]).
-export([list_timers/0]).
-export([list_timers_by_execution/1]).

%% Calendar management
-export([add_holiday/2]).
-export([remove_holiday/1]).
-export([list_holidays/0]).
-export([set_work_hours/2]).
-export([get_work_hours/0]).
-export([set_timezone/1]).
-export([get_timezone/0]).

%% Recovery
-export([restore_timers/0]).
-export([get_pending_timers/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Types
%%====================================================================

-type hour() :: 0..23.
-type minute() :: 0..59.
-type timer_id() :: binary().
-type execution_id() :: binary().
-type timer_status() :: scheduled | fired | cancelled | failed.
-type timer_callback() :: {module(), atom(), [term()]}.
-type work_hours() :: {{hour(), minute()}, {hour(), minute()}}.

-record(persistent_timer, {
    timer_id :: binary(),
    execution_id :: binary(),
    target_time :: integer(),
    timezone :: binary(),
    callback :: {module(), atom(), [term()]},
    status :: timer_status(),
    created_at :: integer(),
    fired_at :: undefined | integer(),
    result :: undefined | term(),
    retry_count = 0 :: non_neg_integer(),
    max_retries = 3 :: non_neg_integer(),
    metadata = #{} :: map(),
    timer_ref :: undefined | reference()
}).

-record(calendar_settings, {
    timezone = <<"UTC">> :: binary(),
    work_hours :: work_hours(),
    weekend_days = [6, 0] :: [integer()],
    holidays = [] :: [{binary(), binary()}]
}).

-record(timer_state, {
    active_timers = #{} :: #{binary() => #persistent_timer{}},
    calendar :: #calendar_settings{},
    tick_ref :: undefined | reference()
}).

-export_type([timer_id/0, execution_id/0, timer_status/0, timer_callback/0, work_hours/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @private
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    start_link([]).

%% @private
-spec start_link([proplists:property()]) -> {ok, pid()} | {error, term()}.

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

-spec start_timer(execution_id(), integer()) ->
          {ok, timer_id()} | {error, term()}.

start_timer(ExecutionId, TargetTime) ->
    start_timer(ExecutionId, TargetTime, {?MODULE, execute_callback, []}).

-spec start_timer(execution_id(), integer(), timer_callback()) ->
          {ok, timer_id()} | {error, term()}.

start_timer(ExecutionId, TargetTime, {Module, Function, Args} = Callback) ->
    start_timer(ExecutionId, TargetTime, Callback, #{}).

-spec start_timer(execution_id(), integer(), timer_callback(), map()) ->
          {ok, timer_id()} | {error, term()}.

start_timer(ExecutionId, TargetTime, Callback, Options) ->
    gen_server:call(?MODULE, {start_timer, ExecutionId, TargetTime, Callback, Options}).

-spec cancel_timer(timer_id()) -> ok | {error, not_found}.

cancel_timer(TimerId) ->
    gen_server:call(?MODULE, {cancel_timer, TimerId}).

-spec cancel_all_timers() -> ok.

cancel_all_timers() ->
    gen_server:call(?MODULE, cancel_all_timers).

-spec get_timer(timer_id()) -> {ok, #persistent_timer{}} | {error, not_found}.

get_timer(TimerId) ->
    gen_server:call(?MODULE, {get_timer, TimerId}).

-spec list_timers() -> [#persistent_timer{}].

list_timers() ->
    gen_server:call(?MODULE, list_timers).

-spec list_timers_by_execution(execution_id()) -> [#persistent_timer{}].

list_timers_by_execution(ExecutionId) ->
    gen_server:call(?MODULE, {list_timers_by_execution, ExecutionId}).

-spec add_holiday(binary(), binary()) -> ok.

add_holiday(Date, Description) ->
    gen_server:call(?MODULE, {add_holiday, Date, Description}).

-spec remove_holiday(binary()) -> ok | {error, not_found}.

remove_holiday(Date) ->
    gen_server:call(?MODULE, {remove_holiday, Date}).

-spec list_holidays() -> [{binary(), binary()}].

list_holidays() ->
    gen_server:call(?MODULE, list_holidays).

-spec set_work_hours({hour(), minute()}, {hour(), minute()}) -> ok.

set_work_hours(StartTime, EndTime) ->
    gen_server:call(?MODULE, {set_work_hours, StartTime, EndTime}).

-spec get_work_hours() -> work_hours().

get_work_hours() ->
    gen_server:call(?MODULE, get_work_hours).

-spec set_timezone(binary()) -> ok.

set_timezone(Timezone) ->
    gen_server:call(?MODULE, {set_timezone, Timezone}).

-spec get_timezone() -> binary().

get_timezone() ->
    gen_server:call(?MODULE, get_timezone).

-spec restore_timers() -> ok.

restore_timers() ->
    gen_server:cast(?MODULE, restore_timers).

-spec get_pending_timers() -> [{timer_id(), integer()}].

get_pending_timers() ->
    gen_server:call(?MODULE, get_pending_timers).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(Options) ->
    %% Get initial calendar settings
    Timezone = proplists:get_value(timezone, Options, <<"UTC">>),
    WorkStart = proplists:get_value(work_start, Options, {9, 0}),
    WorkEnd = proplists:get_value(work_end, Options, {17, 0}),
    Weekends = proplists:get_value(weekend_days, Options, [6, 0]),

    Calendar = #calendar_settings{
        timezone = Timezone,
        work_hours = {WorkStart, WorkEnd},
        weekend_days = Weekends,
        holidays = load_holidays()
    },

    State = #timer_state{
        calendar = Calendar
    },

    %% Restore pending timers
    restore_timers_internal(State),

    %% Start timer tick process
    {ok, TRef} = timer:send_interval(1000, self(), tick),

    {ok, State#timer_state{tick_ref = TRef}}.

%% @private
handle_call({start_timer, ExecutionId, TargetTime, Callback, Options}, _From, State) ->
    TimerId = generate_timer_id(),

    %% Adjust target time based on calendar
    AdjustedTime = adjust_for_calendar(TargetTime, State#timer_state.calendar),

    Timer = #persistent_timer{
        timer_id = TimerId,
        execution_id = ExecutionId,
        target_time = AdjustedTime,
        timezone = State#timer_state.calendar#calendar_settings.timezone,
        callback = Callback,
        status = scheduled,
        created_at = erlang:system_time(millisecond),
        max_retries = maps:get(max_retries, Options, 3),
        metadata = maps:get(metadata, Options, #{})
    },

    %% Save to Mnesia (if available)
    case catch mnesia:table_info(wf_persistent_timer, where_to_write) of
        {'EXIT', _} -> ok;
        _ -> mnesia:dirty_write(wf_persistent_timer, Timer)
    end,

    %% Calculate delay
    Now = erlang:system_time(millisecond),
    Delay = max(0, AdjustedTime - Now),

    %% Start timer
    case timer:send_after(Delay, self(), {timer_fire, TimerId}) of
        {ok, TRef} ->
            UpdatedTimer = Timer#persistent_timer{timer_ref = TRef},
            ActiveTimers = maps:put(TimerId, UpdatedTimer, State#timer_state.active_timers),
            {reply, {ok, TimerId}, State#timer_state{active_timers = ActiveTimers}};
        {error, Reason} ->
            ?LOG_ERROR("Failed to start timer ~p: ~p", [TimerId, Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call({cancel_timer, TimerId}, _From, State) ->
    case maps:get(TimerId, State#timer_state.active_timers, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Timer ->
            %% Cancel the Erlang timer (ignore if already fired)
            case Timer#persistent_timer.timer_ref of
                undefined -> ok;
                TRef ->
                    %% cancel_timer throws badarg if timer already fired
                    catch erlang:cancel_timer(TRef),
                    ok
            end,

            %% Update status
            UpdatedTimer = Timer#persistent_timer{status = cancelled},
            case catch mnesia:table_info(wf_persistent_timer, where_to_write) of
                {'EXIT', _} -> ok;
                _ -> mnesia:dirty_write(wf_persistent_timer, UpdatedTimer)
            end,

            ActiveTimers = maps:put(TimerId, UpdatedTimer, State#timer_state.active_timers),
            {reply, ok, State#timer_state{active_timers = ActiveTimers}}
    end;

handle_call(cancel_all_timers, _From, State) ->
    lists:foreach(fun({TimerId, _Timer}) ->
        case maps:get(TimerId, State#timer_state.active_timers, undefined) of
            undefined -> ok;
            Timer ->
                case Timer#persistent_timer.timer_ref of
                    undefined -> ok;
                    TRef -> catch erlang:cancel_timer(TRef)
                end
        end
    end, maps:to_list(State#timer_state.active_timers)),

    {reply, ok, State#timer_state{active_timers = #{}}};

handle_call({get_timer, TimerId}, _From, State) ->
    Reply = case maps:get(TimerId, State#timer_state.active_timers, undefined) of
        undefined -> {error, not_found};
        Timer -> {ok, Timer}
    end,
    {reply, Reply, State};

handle_call(list_timers, _From, State) ->
    Timers = maps:values(State#timer_state.active_timers),
    {reply, Timers, State};

handle_call({list_timers_by_execution, ExecutionId}, _From, State) ->
    Timers = [T || T <- maps:values(State#timer_state.active_timers),
               T#persistent_timer.execution_id =:= ExecutionId],
    {reply, Timers, State};

handle_call({add_holiday, Date, Description}, _From, State) ->
    Calendar = State#timer_state.calendar,
    Holidays = Calendar#calendar_settings.holidays,
    UpdatedHolidays = case lists:keymember(Date, 1, Holidays) of
        true ->
            Holidays;  %% Already exists, skip
        false ->
            [{Date, Description} | Holidays]
    end,
    save_holidays(UpdatedHolidays),
    UpdatedCalendar = Calendar#calendar_settings{holidays = UpdatedHolidays},
    {reply, ok, State#timer_state{calendar = UpdatedCalendar}};

handle_call({remove_holiday, Date}, _From, State) ->
    Calendar = State#timer_state.calendar,
    Holidays = Calendar#calendar_settings.holidays,
    UpdatedHolidays = lists:keydelete(Date, 1, Holidays),
    save_holidays(UpdatedHolidays),
    UpdatedCalendar = Calendar#calendar_settings{holidays = UpdatedHolidays},
    {reply, ok, State#timer_state{calendar = UpdatedCalendar}};

handle_call(list_holidays, _From, State) ->
    {reply, State#timer_state.calendar#calendar_settings.holidays, State};

handle_call({set_work_hours, StartTime, EndTime}, _From, State) ->
    Calendar = State#timer_state.calendar,
    UpdatedCalendar = Calendar#calendar_settings{work_hours = {StartTime, EndTime}},
    {reply, ok, State#timer_state{calendar = UpdatedCalendar}};

handle_call(get_work_hours, _From, State) ->
    {reply, State#timer_state.calendar#calendar_settings.work_hours, State};

handle_call({set_timezone, Timezone}, _From, State) ->
    Calendar = State#timer_state.calendar,
    UpdatedCalendar = Calendar#calendar_settings{timezone = Timezone},
    {reply, ok, State#timer_state{calendar = UpdatedCalendar}};

handle_call(get_timezone, _From, State) ->
    {reply, State#timer_state.calendar#calendar_settings.timezone, State};

handle_call(get_pending_timers, _From, State) ->
    Now = erlang:system_time(millisecond),
    Pending = [{T#persistent_timer.timer_id, T#persistent_timer.target_time}
               || T <- maps:values(State#timer_state.active_timers),
                  T#persistent_timer.target_time < Now],
    {reply, Pending, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(restore_timers, State) ->
    restore_timers_internal(State),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(tick, State) ->
    NewState = process_timers(State),
    {noreply, NewState};

handle_info({timer_fire, TimerId}, State) ->
    NewState = fire_timer(TimerId, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, #timer_state{tick_ref = TRef}) ->
    case TRef of
        undefined -> ok;
        _ -> timer:cancel(TRef)
    end,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
process_timers(State) ->
    Now = erlang:system_time(millisecond),

    %% Find timers that should fire
    Ready = [{TimerId, Timer}
             || {TimerId, #persistent_timer{target_time = TTime}} = Timer
                <- maps:to_list(State#timer_state.active_timers),
                TTime =< Now],

    %% Fire each ready timer
    lists:foldl(
        fun({TimerId, _Timer}, AccState) ->
            fire_timer(TimerId, AccState)
        end,
        State,
        Ready
    ).

%% @private
fire_timer(TimerId, State) ->
    case maps:get(TimerId, State#timer_state.active_timers, undefined) of
        undefined ->
            State;
        Timer ->
            %% Execute callback
            Callback = Timer#persistent_timer.callback,
            Result = execute_callback(Callback, Timer),

            %% Update timer status
            UpdatedTimer = Timer#persistent_timer{
                status = fired,
                fired_at = erlang:system_time(millisecond),
                result = Result
            },
            case catch mnesia:table_info(wf_persistent_timer, where_to_write) of
                {'EXIT', _} -> ok;
                _ -> mnesia:dirty_write(wf_persistent_timer, UpdatedTimer)
            end,

            %% Remove from active timers
            ActiveTimers = maps:remove(TimerId, State#timer_state.active_timers),
            State#timer_state{active_timers = ActiveTimers}
    end.

%% @private
execute_callback({Module, Function, Args}, Timer) ->
    try
        apply(Module, Function, Args ++ [Timer])
    catch
        Type:Error:Stack ->
            ?LOG_ERROR("Timer callback failed: ~p:~p~n~p", [Type, Error, Stack]),
            {error, {Type, Error}}
    end.

%% @private
adjust_for_calendar(TargetTimeMs, Calendar) ->
    %% Convert target time to configured timezone
    TargetDateTime = calendar:system_time_to_universal_time(TargetTimeMs div 1000, second),

    %% Check if target is on a weekend
    {Date, {Hour, _Min, _Sec}} = TargetDateTime,
    DayOfWeek = calendar:day_of_the_week(Date),

    %% Determine adjusted datetime based on calendar
    {{WorkStartH, WorkStartM}, _WorkEndTime} = Calendar#calendar_settings.work_hours,
    AdjustedDateTime = case lists:member(DayOfWeek, Calendar#calendar_settings.weekend_days) of
        true ->
            %% Weekend - move to next work day and set to work start time
            add_days_until_workday(TargetDateTime, Calendar, WorkStartH, WorkStartM);
        false ->
            %% Check if within work hours
            {{_WorkStartH, _WorkStartM}, {WorkEndH, _WorkEndM}} =
                Calendar#calendar_settings.work_hours,

            case Hour of
                H when H < WorkStartH ->
                    %% Before work hours - adjust to work start today
                    {Date, {WorkStartH, WorkStartM, 0}};
                H when H >= WorkEndH ->
                    %% After work hours - move to next work day and set to work start
                    add_days_until_workday(TargetDateTime, Calendar, WorkStartH, WorkStartM);
                _ ->
                    %% Within work hours - proceed
                    TargetDateTime
            end
    end,

    %% Convert back to milliseconds using universal_time_to_system_time
    %% This returns POSIX (Unix epoch) seconds, not Gregorian seconds
    SecsSinceEpoch = calendar:datetime_to_gregorian_seconds(AdjustedDateTime) -
                     calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}}),
    SecsSinceEpoch * 1000.

%% @private
add_days_until_workday(DateTime, Calendar, WorkStartHour, WorkStartMinute) ->
    %% Add days until we hit a work day
    NewDateTime = add_day(DateTime),
    DayOfWeek = calendar:day_of_the_week(element(1, NewDateTime)),

    case lists:member(DayOfWeek, Calendar#calendar_settings.weekend_days) of
        true ->
            add_days_until_workday(NewDateTime, Calendar, WorkStartHour, WorkStartMinute);
        false ->
            %% Check if this day is a holiday
            DateStr = format_date(element(1, NewDateTime)),
            IsHoliday = lists:any(fun({D, _}) -> D =:= DateStr end,
                               Calendar#calendar_settings.holidays),
            case IsHoliday of
                true ->
                    add_days_until_workday(NewDateTime, Calendar, WorkStartHour, WorkStartMinute);
                false ->
                    %% Valid work day - set to work start time
                    {element(1, NewDateTime), {WorkStartHour, WorkStartMinute, 0}}
            end
    end.

%% @private
add_day({Date, Time}) ->
    NextDate = calendar:gregorian_days_to_date(
        calendar:date_to_gregorian_days(Date) + 1
    ),
    {NextDate, Time}.

%% @private
format_date({Year, Month, Day}) ->
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0B",
        [Year, Month, Day])).

%% @private
restore_timers_internal(_State) ->
    %% TODO: Implement Mnesia restoration
    ok.

%% @private
save_holidays(Holidays) ->
    application:set_env(?MODULE, holidays, Holidays),
    ok.

%% @private
load_holidays() ->
    case application:get_env(?MODULE, holidays, []) of
        Holidays when is_list(Holidays) -> Holidays;
        _ -> []
    end.

%% @private
generate_timer_id() ->
    Time = erlang:system_time(microsecond),
    Node = erlang:phash2(node()),
    <<Time:48, Node:16>>.
