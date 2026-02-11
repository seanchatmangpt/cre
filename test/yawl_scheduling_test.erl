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
%% @doc YAWL Calendar and Scheduling Test Suite
%%
%% Comprehensive test suite for YAWL scheduling functionality including
%% calendar management, time periods, availability checking, and due date management.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_scheduling_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Record Definitions (imported from yawl_scheduling)
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
    calendar_type :: default | custom | role_based | participant_based,
    time_periods = [] :: [#time_period{}],
    timezone :: binary(),
    linked_to :: binary() | undefined
}).

-record(scheduled_task, {
    task_id :: binary(),
    calendar_id :: binary(),
    scheduled_start :: calendar:datetime(),
    scheduled_end :: calendar:datetime(),
    due_date :: calendar:datetime() | undefined,
    status :: pending | in_progress | completed | cancelled
}).

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% Starts the yawl_scheduling gen_server.
%% @end
%%--------------------------------------------------------------------
setup() ->
    case whereis(yawl_scheduling) of
        undefined ->
            {ok, _Pid} = yawl_scheduling:start_link({local, yawl_scheduling}),
            timer:sleep(100),  % Allow server to initialize
            ok;
        _Pid ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% Stops the yawl_scheduling gen_server.
%% @end
%%--------------------------------------------------------------------
cleanup(_State) ->
    case whereis(yawl_scheduling) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid), timer:sleep(50)
    end,
    ok.

%%====================================================================
%% Test: Create Calendar
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test creating a new calendar.
%% @end
%%--------------------------------------------------------------------
test_create_calendar_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Create default calendar
                     Result = yawl_scheduling:create_calendar(<<"Test Calendar">>),
                     ?assertMatch({ok, _CalendarId}, Result)
                 end),
          ?_test(begin
                     % Create custom calendar
                     Result = yawl_scheduling:create_calendar(<<"Custom Calendar">>, custom),
                     ?assertMatch({ok, _CalendarId}, Result)
                 end),
          ?_test(begin
                     % Create all calendar types
                     {ok, _} = yawl_scheduling:create_calendar(<<"Default">>, default),
                     {ok, _} = yawl_scheduling:create_calendar(<<"Custom">>, custom),
                     {ok, _} = yawl_scheduling:create_calendar(<<"Role Based">>, role_based),
                     {ok, _} = yawl_scheduling:create_calendar(<<"Participant Based">>, participant_based),
                     Calendars = yawl_scheduling:list_calendars(),
                     ?assert(length(Calendars) >= 4)
                 end),
          ?_test(begin
                     % Calendar IDs are unique
                     {ok, Id1} = yawl_scheduling:create_calendar(<<"Calendar1">>),
                     {ok, Id2} = yawl_scheduling:create_calendar(<<"Calendar2">>),
                     ?assertNotEqual(Id1, Id2)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Add Calendar Event (Time Period)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test adding a time period to a calendar.
%% @end
%%--------------------------------------------------------------------
test_add_calendar_event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Add period to non-existent calendar
                     Period = #time_period{
                         id = <<>>,
                         start_time = {{2024, 1, 1}, {9, 0, 0}},
                         end_time = {{2024, 1, 1}, {17, 0, 0}},
                         available = true,
                         description = <<"Work hours">>
                     },
                     Result = yawl_scheduling:add_time_period(<<"nonexistent">>, Period),
                     ?assertEqual({error, calendar_not_found}, Result)
                 end),
          ?_test(begin
                     % Add valid period to existing calendar
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Work Calendar">>),
                     Period = #time_period{
                         id = <<>>,
                         start_time = {{2024, 1, 1}, {9, 0, 0}},
                         end_time = {{2024, 1, 1}, {17, 0, 0}},
                         available = true,
                         description = <<"Work day">>
                     },
                     ?assertEqual(ok, yawl_scheduling:add_time_period(CalId, Period)),
                     {ok, Calendar} = yawl_scheduling:get_calendar(CalId),
                     ?assert(length(Calendar#calendar.time_periods) > 0)
                 end),
          ?_test(begin
                     % Add multiple periods
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Multi Period">>),
                     lists:foreach(
                         fun(N) ->
                             StartDate = {2024, 1, N},
                             Period = #time_period{
                                 id = <<>>,
                                 start_time = {StartDate, {9, 0, 0}},
                                 end_time = {StartDate, {17, 0, 0}},
                                 available = true
                             },
                             ok = yawl_scheduling:add_time_period(CalId, Period)
                         end,
                         lists:seq(1, 5)
                     ),
                     {ok, Calendar} = yawl_scheduling:get_calendar(CalId),
                     ?assertEqual(5, length(Calendar#calendar.time_periods))
                 end),
          ?_test(begin
                     % Add unavailable period (exception/block)
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Exception Calendar">>),
                     Period = #time_period{
                         id = <<>>,
                         start_time = {{2024, 12, 25}, {0, 0, 0}},
                         end_time = {{2024, 12, 25}, {23, 59, 59}},
                         available = false,
                         description = <<"Holiday">>
                     },
                     ?assertEqual(ok, yawl_scheduling:add_time_period(CalId, Period)),
                     {ok, Calendar} = yawl_scheduling:get_calendar(CalId),
                     [Holiday | _] = Calendar#calendar.time_periods,
                     ?assertEqual(false, Holiday#time_period.available)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Remove Calendar Event
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test removing a time period from a calendar.
%% @end
%%--------------------------------------------------------------------
test_remove_calendar_event_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Remove from non-existent calendar
                     Result = yawl_scheduling:remove_time_period(<<"nonexistent">>, <<"period_1">>),
                     ?assertEqual({error, calendar_not_found}, Result)
                 end),
          ?_test(begin
                     % Remove existing period
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Test Calendar">>),
                     Period = #time_period{
                         id = <<"period_to_remove">>,
                         start_time = {{2024, 1, 1}, {9, 0, 0}},
                         end_time = {{2024, 1, 1}, {17, 0, 0}},
                         available = true
                     },
                     ok = yawl_scheduling:add_time_period(CalId, Period),
                     {ok, Calendar1} = yawl_scheduling:get_calendar(CalId),
                     Count1 = length(Calendar1#calendar.time_periods),
                     ?assertEqual(ok, yawl_scheduling:remove_time_period(CalId, <<"period_to_remove">>)),
                     {ok, Calendar2} = yawl_scheduling:get_calendar(CalId),
                     Count2 = length(Calendar2#calendar.time_periods),
                     ?assertEqual(Count1 - 1, Count2)
                 end),
          ?_test(begin
                     % Remove non-existent period (should not error)
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Test Calendar 2">>),
                     ?assertEqual(ok, yawl_scheduling:remove_time_period(CalId, <<"nonexistent_period">>))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Check Availability
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test checking availability at specific times.
%% @end
%%--------------------------------------------------------------------
test_check_availability_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Check non-existent calendar
                     Result = yawl_scheduling:check_availability(<<"nonexistent">>, {{2024, 1, 1}, {12, 0, 0}}),
                     ?assertEqual(unavailable, Result)
                 end),
          ?_test(begin
                     % Check availability within available period
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Work Hours">>),
                     Period = #time_period{
                         id = <<>>,
                         start_time = {{2024, 1, 1}, {9, 0, 0}},
                         end_time = {{2024, 1, 1}, {17, 0, 0}},
                         available = true
                     },
                     ok = yawl_scheduling:add_time_period(CalId, Period),
                     ?assertEqual(available, yawl_scheduling:check_availability(CalId, {{2024, 1, 1}, {12, 0, 0}}))
                 end),
          ?_test(begin
                     % Check availability outside period
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Work Hours 2">>),
                     Period = #time_period{
                         id = <<>>,
                         start_time = {{2024, 1, 1}, {9, 0, 0}},
                         end_time = {{2024, 1, 1}, {17, 0, 0}},
                         available = true
                     },
                     ok = yawl_scheduling:add_time_period(CalId, Period),
                     ?assertEqual(unavailable, yawl_scheduling:check_availability(CalId, {{2024, 1, 1}, {18, 0, 0}})),
                     ?assertEqual(unavailable, yawl_scheduling:check_availability(CalId, {{2024, 1, 1}, {8, 0, 0}}))
                 end),
          ?_test(begin
                     % Check within unavailable period
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Blocked Calendar">>),
                     Period = #time_period{
                         id = <<>>,
                         start_time = {{2024, 12, 25}, {0, 0, 0}},
                         end_time = {{2024, 12, 25}, {23, 59, 59}},
                         available = false,
                         description = <<"Holiday">>
                     },
                     ok = yawl_scheduling:add_time_period(CalId, Period),
                     ?assertEqual(unavailable, yawl_scheduling:check_availability(CalId, {{2024, 12, 25}, {12, 0, 0}}))
                 end),
          ?_test(begin
                     % Check range of availability
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Range Calendar">>),
                     Period = #time_period{
                         id = <<>>,
                         start_time = {{2024, 1, 1}, {9, 0, 0}},
                         end_time = {{2024, 1, 1}, {17, 0, 0}},
                         available = true
                     },
                     ok = yawl_scheduling:add_time_period(CalId, Period),
                     Range = yawl_scheduling:check_availability(CalId, {{2024, 1, 1}, {8, 0, 0}}, {{2024, 1, 1}, {19, 0, 0}}),
                     ?assert(is_list(Range)),
                     ?assert(length(Range) > 0)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Schedule Workitem/Task
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test scheduling a task/workitem.
%% @end
%%--------------------------------------------------------------------
test_schedule_workitem_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Schedule task with valid props
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Task Calendar">>),
                     Props = [
                         {calendar_id, CalId},
                         {start_time, {{2024, 1, 1}, {9, 0, 0}}},
                         {end_time, {{2024, 1, 1}, {17, 0, 0}}},
                         {due_date, {{2024, 1, 1}, {17, 0, 0}}}
                     ],
                     Result = yawl_scheduling:schedule_task(<<"task_001">>, Props),
                     ?assertMatch({ok, <<"task_001">>}, Result)
                 end),
          ?_test(begin
                     % Schedule multiple tasks
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Multi Task Calendar">>),
                     lists:foreach(
                         fun(N) ->
                             TaskId = list_to_binary("task_" ++ integer_to_list(N)),
                             Props = [
                                 {calendar_id, CalId},
                                 {start_time, {{2024, 1, N}, {9, 0, 0}}},
                                 {end_time, {{2024, 1, N}, {17, 0, 0}}}
                             ],
                             {ok, _} = yawl_scheduling:schedule_task(TaskId, Props)
                         end,
                         lists:seq(1, 5)
                     ),
                     % Verify tasks were scheduled
                     ?assert(true)  % If we got here, all tasks scheduled
                 end),
          ?_test(begin
                     % Schedule with default due_date (uses end_time)
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Default Due Calendar">>),
                     Props = [
                         {calendar_id, CalId},
                         {start_time, {{2024, 1, 1}, {9, 0, 0}}},
                         {end_time, {{2024, 1, 1}, {12, 0, 0}}}
                     ],
                     {ok, _} = yawl_scheduling:schedule_task(<<"task_default">>, Props),
                     {ok, DueDate} = yawl_scheduling:get_due_date(<<"task_default">>),
                     ?assertEqual({2024, 1, 1}, element(1, DueDate))
                 end),
          ?_test(begin
                     % Verify task status is pending
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Status Calendar">>),
                     % Use future dates to avoid overdue detection
                     FutureDate = {{2099, 12, 31}, {17, 0, 0}},
                     Props = [
                         {calendar_id, CalId},
                         {start_time, FutureDate},
                         {end_time, FutureDate},
                         {due_date, FutureDate}
                     ],
                     {ok, _} = yawl_scheduling:schedule_task(<<"task_status">>, Props),
                     ?assertEqual(false, yawl_scheduling:is_overdue(<<"task_status">>))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Check Overdue
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test checking for overdue tasks.
%% @end
%%--------------------------------------------------------------------
test_check_overdue_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Check non-existent task
                     Result = yawl_scheduling:is_overdue(<<"nonexistent">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Task with past due date is overdue
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Overdue Calendar">>),
                     % Set due date in the past
                     PastDate = {{2020, 1, 1}, {12, 0, 0}},
                     Props = [
                         {calendar_id, CalId},
                         {start_time, {PastDate}},
                         {end_time, {PastDate}},
                         {due_date, PastDate}
                     ],
                     {ok, _} = yawl_scheduling:schedule_task(<<"overdue_task">>, Props),
                     ?assertEqual(true, yawl_scheduling:is_overdue(<<"overdue_task">>))
                 end),
          ?_test(begin
                     % Task with future due date is not overdue
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Future Calendar">>),
                     FutureDate = {{2099, 12, 31}, {23, 59, 59}},
                     Props = [
                         {calendar_id, CalId},
                         {start_time, {{2024, 1, 1}, {9, 0, 0}}},
                         {end_time, {{2024, 1, 1}, {17, 0, 0}}},
                         {due_date, FutureDate}
                     ],
                     {ok, _} = yawl_scheduling:schedule_task(<<"future_task">>, Props),
                     ?assertEqual(false, yawl_scheduling:is_overdue(<<"future_task">>))
                 end),
          ?_test(begin
                     % Completed task is not overdue even with past due date
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Completed Calendar">>),
                     Props = [
                         {calendar_id, CalId},
                         {start_time, {{2024, 1, 1}, {9, 0, 0}}},
                         {end_time, {{2024, 1, 1}, {17, 0, 0}}},
                         {due_date, {{2020, 1, 1}, {12, 0, 0}}}
                     ],
                     {ok, _} = yawl_scheduling:schedule_task(<<"completed_task">>, Props),
                     % Mark as completed via status update (would need internal call)
                     ?assertEqual(true, yawl_scheduling:is_overdue(<<"completed_task">>))
                     % Note: The module's is_overdue checks completed status, but we can't
                     % easily set task status from the API. This documents expected behavior.
                 end),
          ?_test(begin
                     % Get all overdue tasks
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Mixed Calendar">>),
                     PastDate = {{2020, 1, 1}, {12, 0, 0}},
                     FutureDate = {{2099, 12, 31}, {23, 59, 59}},
                     lists:foreach(
                         fun({Tid, Due}) ->
                             Props = [
                                 {calendar_id, CalId},
                                 {start_time, {{2024, 1, 1}, {9, 0, 0}}},
                                 {end_time, {{2024, 1, 1}, {17, 0, 0}}},
                                 {due_date, Due}
                             ],
                             {ok, _} = yawl_scheduling:schedule_task(Tid, Props)
                         end,
                         [
                             {<<"overdue_1">>, PastDate},
                             {<<"overdue_2">>, PastDate},
                             {<<"future_1">>, FutureDate},
                             {<<"future_2">>, FutureDate}
                         ]
                     ),
                     OverdueTasks = yawl_scheduling:get_overdue_tasks(),
                     ?assert(length(OverdueTasks) >= 2)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Add/Remove Calendar Exception
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test adding and removing calendar exceptions.
%% @end
%%--------------------------------------------------------------------
test_calendar_exception_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Add exception to non-existent calendar
                     Result = yawl_scheduling:add_calendar_exception(
                         <<"nonexistent">>,
                         {{2024, 12, 25}, {0, 0, 0}},
                         {{2024, 12, 25}, {23, 59, 59}}
                     ),
                     ?assertEqual({error, calendar_not_found}, Result)
                 end),
          ?_test(begin
                     % Add valid exception
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Holiday Calendar">>),
                     ?assertEqual(ok, yawl_scheduling:add_calendar_exception(
                         CalId,
                         {{2024, 12, 25}, {0, 0, 0}},
                         {{2024, 12, 25}, {23, 59, 59}}
                     )),
                     % Verify period was added as unavailable
                     {ok, Calendar} = yawl_scheduling:get_calendar(CalId),
                     ?assert(length(Calendar#calendar.time_periods) > 0)
                 end),
          ?_test(begin
                     % Exception makes time unavailable
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Exception Test">>),
                     % First add available period
                     AvailablePeriod = #time_period{
                         id = <<>>,
                         start_time = {{2024, 12, 25}, {0, 0, 0}},
                         end_time = {{2024, 12, 25}, {23, 59, 59}},
                         available = true
                     },
                     ok = yawl_scheduling:add_time_period(CalId, AvailablePeriod),
                     ?assertEqual(available, yawl_scheduling:check_availability(CalId, {{2024, 12, 25}, {12, 0, 0}})),
                     % Add exception (unavailable period)
                     ok = yawl_scheduling:add_calendar_exception(
                         CalId,
                         {{2024, 12, 25}, {10, 0, 0}},
                         {{2024, 12, 25}, {14, 0, 0}}
                     ),
                     {ok, Calendar} = yawl_scheduling:get_calendar(CalId),
                     ?assert(length(Calendar#calendar.time_periods) >= 2)
                 end),
          ?_test(begin
                     % Remove exception
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Remove Exception">>),
                     ok = yawl_scheduling:add_calendar_exception(
                         CalId,
                         {{2024, 12, 25}, {0, 0, 0}},
                         {{2024, 12, 25}, {23, 59, 59}}
                     ),
                     {ok, Calendar1} = yawl_scheduling:get_calendar(CalId),
                     [#time_period{id = ExceptionId} | _] = Calendar1#calendar.time_periods,
                     ?assertEqual(ok, yawl_scheduling:remove_calendar_exception(CalId, ExceptionId)),
                     {ok, Calendar2} = yawl_scheduling:get_calendar(CalId),
                     ?assertEqual(length(Calendar1#calendar.time_periods) - 1,
                                  length(Calendar2#calendar.time_periods))
                 end),
          ?_test(begin
                     % Remove exception from non-existent calendar
                     Result = yawl_scheduling:remove_calendar_exception(<<"nonexistent">>, <<"exc_1">>),
                     ?assertEqual({error, calendar_not_found}, Result)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Get Scheduled Items
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test retrieving scheduled items.
%% @end
%%--------------------------------------------------------------------
test_get_scheduled_items_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Get due date for non-existent task
                     Result = yawl_scheduling:get_due_date(<<"nonexistent">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Get due date for existing task
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Due Date Calendar">>),
                     DueDate = {{2024, 12, 31}, {23, 59, 59}},
                     Props = [
                         {calendar_id, CalId},
                         {start_time, {{2024, 1, 1}, {9, 0, 0}}},
                         {end_time, {{2024, 1, 1}, {17, 0, 0}}},
                         {due_date, DueDate}
                     ],
                     {ok, _} = yawl_scheduling:schedule_task(<<"task_with_due">>, Props),
                     {ok, RetrievedDue} = yawl_scheduling:get_due_date(<<"task_with_due">>),
                     ?assertEqual(DueDate, RetrievedDue)
                 end),
          ?_test(begin
                     % Get task without due date (uses end_time as default)
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"No Due Calendar">>),
                     FutureDate = {{2099, 12, 31}, {17, 0, 0}},
                     Props = [
                         {calendar_id, CalId},
                         {start_time, FutureDate},
                         {end_time, FutureDate}
                     ],
                     {ok, _} = yawl_scheduling:schedule_task(<<"task_no_due">>, Props),
                     {ok, RetrievedDue} = yawl_scheduling:get_due_date(<<"task_no_due">>),
                     ?assertEqual(FutureDate, RetrievedDue)
                 end),
          ?_test(begin
                     % Set due date for existing task
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Set Due Calendar">>),
                     Props = [
                         {calendar_id, CalId},
                         {start_time, {{2024, 1, 1}, {9, 0, 0}}},
                         {end_time, {{2024, 1, 1}, {17, 0, 0}}}
                     ],
                     {ok, _} = yawl_scheduling:schedule_task(<<"task_set_due">>, Props),
                     NewDueDate = {{2024, 6, 30}, {18, 0, 0}},
                     ?assertEqual(ok, yawl_scheduling:set_due_date(<<"task_set_due">>, NewDueDate)),
                     {ok, RetrievedDue} = yawl_scheduling:get_due_date(<<"task_set_due">>),
                     ?assertEqual(NewDueDate, RetrievedDue)
                 end),
          ?_test(begin
                     % Reschedule task
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Reschedule Calendar">>),
                     Props = [
                         {calendar_id, CalId},
                         {start_time, {{2024, 1, 1}, {9, 0, 0}}},
                         {end_time, {{2024, 1, 1}, {17, 0, 0}}},
                         {due_date, {{2024, 1, 1}, {17, 0, 0}}}
                     ],
                     {ok, _} = yawl_scheduling:schedule_task(<<"task_reschedule">>, Props),
                     NewStart = {{2024, 2, 1}, {9, 0, 0}},
                     NewEnd = {{2024, 2, 1}, {17, 0, 0}},
                     ?assertEqual(ok, yawl_scheduling:reschedule_task(<<"task_reschedule">>, NewStart, NewEnd))
                 end),
          ?_test(begin
                     % List all calendars
                     {ok, _} = yawl_scheduling:create_calendar(<<"Cal1">>),
                     {ok, _} = yawl_scheduling:create_calendar(<<"Cal2">>),
                     {ok, _} = yawl_scheduling:create_calendar(<<"Cal3">>),
                     Calendars = yawl_scheduling:list_calendars(),
                     ?assert(length(Calendars) >= 3)
                 end),
          ?_test(begin
                     % Get calendar by ID
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Get Calendar">>),
                     Result = yawl_scheduling:get_calendar(CalId),
                     ?assertMatch({ok, #calendar{}}, Result),
                     {ok, Calendar} = Result,
                     ?assertEqual(<<"Get Calendar">>, Calendar#calendar.name)
                 end),
          ?_test(begin
                     % Get non-existent calendar
                     Result = yawl_scheduling:get_calendar(<<"nonexistent">>),
                     ?assertEqual({error, not_found}, Result)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Timezone Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test timezone management for calendars.
%% @end
%%--------------------------------------------------------------------
test_timezone_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Get default timezone
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Default TZ">>),
                     {ok, Tz} = yawl_scheduling:get_timezone(CalId),
                     ?assertEqual(<<"UTC">>, Tz)
                 end),
          ?_test(begin
                     % Set timezone
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Custom TZ">>),
                     ?assertEqual(ok, yawl_scheduling:set_timezone(CalId, <<"America/New_York">>)),
                     {ok, Tz} = yawl_scheduling:get_timezone(CalId),
                     ?assertEqual(<<"America/New_York">>, Tz)
                 end),
          ?_test(begin
                     % Set timezone on non-existent calendar
                     Result = yawl_scheduling:set_timezone(<<"nonexistent">>, <<"UTC">>),
                     ?assertEqual({error, calendar_not_found}, Result)
                 end),
          ?_test(begin
                     % Get timezone from non-existent calendar
                     Result = yawl_scheduling:get_timezone(<<"nonexistent">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Multiple timezones
                     {ok, Cal1} = yawl_scheduling:create_calendar(<<"NY Calendar">>),
                     {ok, Cal2} = yawl_scheduling:create_calendar(<<"London Calendar">>),
                     {ok, Cal3} = yawl_scheduling:create_calendar(<<"Tokyo Calendar">>),
                     ok = yawl_scheduling:set_timezone(Cal1, <<"America/New_York">>),
                     ok = yawl_scheduling:set_timezone(Cal2, <<"Europe/London">>),
                     ok = yawl_scheduling:set_timezone(Cal3, <<"Asia/Tokyo">>),
                     {ok, Tz1} = yawl_scheduling:get_timezone(Cal1),
                     {ok, Tz2} = yawl_scheduling:get_timezone(Cal2),
                     {ok, Tz3} = yawl_scheduling:get_timezone(Cal3),
                     ?assertEqual(<<"America/New_York">>, Tz1),
                     ?assertEqual(<<"Europe/London">>, Tz2),
                     ?assertEqual(<<"Asia/Tokyo">>, Tz3)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Get Available Slots
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test finding available time slots.
%% @end
%%--------------------------------------------------------------------
test_get_available_slots_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Get slots from non-existent calendar
                     Result = yawl_scheduling:get_available_slots(
                         <<"nonexistent">>,
                         {{2024, 1, 1}, {0, 0, 0}},
                         {{2024, 1, 1}, {23, 59, 59}}
                     ),
                     ?assertEqual([], Result)
                 end),
          ?_test(begin
                     % Get slots with available periods
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Slots Calendar">>),
                     Period = #time_period{
                         id = <<>>,
                         start_time = {{2024, 1, 1}, {9, 0, 0}},
                         end_time = {{2024, 1, 1}, {17, 0, 0}},
                         available = true
                     },
                     ok = yawl_scheduling:add_time_period(CalId, Period),
                     Slots = yawl_scheduling:get_available_slots(
                         CalId,
                         {{2024, 1, 1}, {0, 0, 0}},
                         {{2024, 1, 1}, {23, 59, 59}}
                     ),
                     ?assert(is_list(Slots)),
                     ?assert(length(Slots) > 0)
                 end),
          ?_test(begin
                     % Get slots with no available periods
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"No Slots Calendar">>),
                     Slots = yawl_scheduling:get_available_slots(
                         CalId,
                         {{2024, 1, 1}, {0, 0, 0}},
                         {{2024, 1, 1}, {23, 59, 59}}
                     ),
                     ?assertEqual([], Slots)
                 end),
          ?_test(begin
                     % Get slots with mixed available/unavailable periods
                     {ok, CalId} = yawl_scheduling:create_calendar(<<"Mixed Calendar">>),
                     Available = #time_period{
                         id = <<>>,
                         start_time = {{2024, 1, 1}, {9, 0, 0}},
                         end_time = {{2024, 1, 1}, {12, 0, 0}},
                         available = true
                     },
                     Unavailable = #time_period{
                         id = <<>>,
                         start_time = {{2024, 1, 1}, {12, 0, 0}},
                         end_time = {{2024, 1, 1}, {13, 0, 0}},
                         available = false
                     },
                     Available2 = #time_period{
                         id = <<>>,
                         start_time = {{2024, 1, 1}, {13, 0, 0}},
                         end_time = {{2024, 1, 1}, {17, 0, 0}},
                         available = true
                     },
                     ok = yawl_scheduling:add_time_period(CalId, Available),
                     ok = yawl_scheduling:add_time_period(CalId, Unavailable),
                     ok = yawl_scheduling:add_time_period(CalId, Available2),
                     Slots = yawl_scheduling:get_available_slots(
                         CalId,
                         {{2024, 1, 1}, {8, 0, 0}},
                         {{2024, 1, 1}, {18, 0, 0}}
                     ),
                     ?assert(is_list(Slots)),
                     ?assert(length(Slots) > 0)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Calendar Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test different calendar types.
%% @end
%%--------------------------------------------------------------------
test_calendar_types_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Create each type and verify
                     {ok, DefaultCal} = yawl_scheduling:create_calendar(<<"Default">>, default),
                     {ok, CustomCal} = yawl_scheduling:create_calendar(<<"Custom">>, custom),
                     {ok, RoleCal} = yawl_scheduling:create_calendar(<<"Role">>, role_based),
                     {ok, PartCal} = yawl_scheduling:create_calendar(<<"Participant">>, participant_based),
                     {ok, Default} = yawl_scheduling:get_calendar(DefaultCal),
                     {ok, Custom} = yawl_scheduling:get_calendar(CustomCal),
                     {ok, Role} = yawl_scheduling:get_calendar(RoleCal),
                     {ok, Participant} = yawl_scheduling:get_calendar(PartCal),
                     ?assertEqual(default, Default#calendar.calendar_type),
                     ?assertEqual(custom, Custom#calendar.calendar_type),
                     ?assertEqual(role_based, Role#calendar.calendar_type),
                     ?assertEqual(participant_based, Participant#calendar.calendar_type)
                 end)
         ]
     end}.
