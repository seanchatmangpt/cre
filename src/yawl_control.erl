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
%% @author Jörgen Brandt <joergen@cuneiform-lang.org>
%% @copyright 2015
%%
%% @doc YAWL Control Panel / Runtime Management for CRE.
%%
%% This module implements the control interface for managing YAWL workflow
%% execution, similar to the YAWL control panel in the Java reference
%% implementation.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Case Management:</b> Start, cancel, suspend, resume workflow cases</li>
%%   <li><b>Monitoring:</b> Get running cases and their status</li>
%%   <li><b>Statistics:</b> Execution metrics and engine status</li>
%%   <li><b>Configuration:</b> Runtime parameter tuning</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_control).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Control API
-export([start_control/0, start_control/1, stop/0]).
-export([get_running_cases/0, get_case_status/1]).
-export([cancel_case/2, suspend_case/2, resume_case/2]).
-export([get_case_statistics/0, get_engine_status/0, set_engine_parameter/2]).
-export([list_all_cases/0, get_case_history/1]).
-export([register_case/2, unregister_case/1, update_case_status/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Records
%%====================================================================

-record(case_info, {
    case_id :: binary(),
    spec_id :: binary(),
    status :: running | suspended | cancelled | completed | failed,
    start_time :: erlang:timestamp(),
    end_time :: erlang:timestamp() | undefined,
    current_task :: binary() | undefined,
    tasks_completed = 0 :: non_neg_integer(),
    tasks_total = 0 :: non_neg_integer(),
    data = #{} :: map()
}).

-record(engine_status, {
    start_time :: erlang:timestamp(),
    cases_completed = 0 :: non_neg_integer(),
    cases_cancelled = 0 :: non_neg_integer(),
    cases_failed = 0 :: non_neg_integer(),
    parameters = #{} :: map()
}).

-record(control_state, {
    cases = #{} :: #{binary() => #case_info{}},
    status :: #engine_status{},
    subscribers = [] :: [{pid(), reference()}]
}).

%%====================================================================
%% Types
%%====================================================================

-type case_status() :: running | suspended | cancelled | completed | failed.
-type case_info() :: #case_info{}.
-type engine_status() :: #engine_status{}.

-export_type([case_status/0, case_info/0, engine_status/0]).

%%====================================================================
%% Control API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the YAWL control panel with default name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_control() -> {ok, pid()} | {error, term()}.

start_control() ->
    start_control(yawl_control).

%%--------------------------------------------------------------------
%% @doc Starts the YAWL control panel with a registered name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_control(atom()) -> {ok, pid()} | {error, term()}.

start_control(Name) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the YAWL control panel.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.

stop() ->
    gen_server:stop(yawl_control).

%%--------------------------------------------------------------------
%% @doc Gets all currently running workflow cases.
%%
%% @return List of case information maps.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_running_cases() -> [map()].

get_running_cases() ->
    gen_server:call(yawl_control, get_running_cases).

%%--------------------------------------------------------------------
%% @doc Gets the status of a specific workflow case.
%%
%% @param CaseId The case identifier.
%% @return Case status map or {error, not_found}.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_status(binary()) -> map() | {error, not_found}.

get_case_status(CaseId) when is_binary(CaseId) ->
    gen_server:call(yawl_control, {get_case_status, CaseId}).

%%--------------------------------------------------------------------
%% @doc Cancels a running workflow case.
%%
%% @param CaseId The case identifier.
%% @param Reason Reason for cancellation.
%% @return ok or {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_case(binary(), binary()) -> ok | {error, term()}.

cancel_case(CaseId, Reason) when is_binary(CaseId), is_binary(Reason) ->
    gen_server:call(yawl_control, {cancel_case, CaseId, Reason}).

%%--------------------------------------------------------------------
%% @doc Suspends a running workflow case.
%%
%% @param CaseId The case identifier.
%% @param Reason Reason for suspension.
%% @return ok or {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-spec suspend_case(binary(), binary()) -> ok | {error, term()}.

suspend_case(CaseId, Reason) when is_binary(CaseId), is_binary(Reason) ->
    gen_server:call(yawl_control, {suspend_case, CaseId, Reason}).

%%--------------------------------------------------------------------
%% @doc Resumes a suspended workflow case.
%%
%% @param CaseId The case identifier.
%% @param Reason Reason for resumption.
%% @return ok or {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-spec resume_case(binary(), binary()) -> ok | {error, term()}.

resume_case(CaseId, Reason) when is_binary(CaseId), is_binary(Reason) ->
    gen_server:call(yawl_control, {resume_case, CaseId, Reason}).

%%--------------------------------------------------------------------
%% @doc Gets execution statistics for all cases.
%%
%% @return Map of statistics.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_statistics() -> map().

get_case_statistics() ->
    gen_server:call(yawl_control, get_case_statistics).

%%--------------------------------------------------------------------
%% @doc Gets the engine status information.
%%
%% @return Map of engine status.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_engine_status() -> map().

get_engine_status() ->
    gen_server:call(yawl_control, get_engine_status).

%%--------------------------------------------------------------------
%% @doc Sets an engine runtime parameter.
%%
%% @param Parameter Parameter name.
%% @param Value Parameter value.
%% @return ok or {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_engine_parameter(atom(), term()) -> ok | {error, term()}.

set_engine_parameter(Parameter, Value) when is_atom(Parameter) ->
    gen_server:call(yawl_control, {set_parameter, Parameter, Value}).

%%--------------------------------------------------------------------
%% @doc Lists all cases (running and completed).
%%
%% @return List of case information maps.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_all_cases() -> [map()].

list_all_cases() ->
    gen_server:call(yawl_control, list_all_cases).

%%--------------------------------------------------------------------
%% @doc Gets the execution history of a case.
%%
%% @param CaseId The case identifier.
%% @return List of history events or {error, not_found}.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_history(binary()) -> [map()] | {error, not_found}.

get_case_history(CaseId) when is_binary(CaseId) ->
    gen_server:call(yawl_control, {get_case_history, CaseId}).

%%--------------------------------------------------------------------
%% @doc Registers a new workflow case.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_case(binary(), binary()) -> ok.

register_case(CaseId, SpecId) ->
    gen_server:cast(yawl_control, {register_case, CaseId, SpecId}).

%%--------------------------------------------------------------------
%% @doc Unregisters a workflow case.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_case(binary()) -> ok.

unregister_case(CaseId) ->
    gen_server:cast(yawl_control, {unregister_case, CaseId}).

%%--------------------------------------------------------------------
%% @doc Updates the status of a workflow case.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_case_status(binary(), case_status()) -> ok.

update_case_status(CaseId, Status) ->
    gen_server:cast(yawl_control, {update_case_status, CaseId, Status}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the control panel state.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, #control_state{}}.

init([]) ->
    logger:info("YAWL control starting", [{yawl_control, starting}]),
    {ok, #control_state{
        status = #engine_status{
            start_time = erlang:timestamp(),
            parameters = #{
                max_concurrent_cases => 100,
                case_timeout => 3600000,  % 1 hour default
                enable_auto_cleanup => true,
                cleanup_interval => 300000  % 5 minutes
            }
        }
    }}.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, #control_state{}) ->
          {reply, term(), #control_state{}}.

handle_call(get_running_cases, _From, State) ->
    Cases = maps:fold(
        fun(_CaseId, #case_info{status = running} = CaseInfo, Acc) ->
            [case_info_to_map(CaseInfo) | Acc];
           (_CaseId, _CaseInfo, Acc) ->
            Acc
        end,
        [],
        State#control_state.cases
    ),
    {reply, lists:reverse(Cases), State};

handle_call({get_case_status, CaseId}, _From, State) ->
    case maps:get(CaseId, State#control_state.cases, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        CaseInfo ->
            {reply, case_info_to_map(CaseInfo), State}
    end;

handle_call({cancel_case, CaseId, Reason}, _From, State) ->
    case maps:get(CaseId, State#control_state.cases, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #case_info{status = completed} ->
            {reply, {error, already_completed}, State};
        #case_info{status = cancelled} ->
            {reply, {error, already_cancelled}, State};
        CaseInfo ->
            UpdatedCase = CaseInfo#case_info{
                status = cancelled,
                end_time = erlang:timestamp()
            },
            NewCases = maps:put(CaseId, UpdatedCase, State#control_state.cases),
            NewStatus = State#control_state.status#engine_status{
                cases_cancelled = State#control_state.status#engine_status.cases_cancelled + 1
            },
            logger:info("Case cancelled: id=~p reason=~p", [CaseId, Reason],
                        [{yawl_control, case_cancelled}]),
            notify_subscribers(case_cancelled, CaseId, State#control_state.subscribers),
            {reply, ok, State#control_state{cases = NewCases, status = NewStatus}}
    end;

handle_call({suspend_case, CaseId, Reason}, _From, State) ->
    case maps:get(CaseId, State#control_state.cases, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #case_info{status = suspended} ->
            {reply, {error, already_suspended}, State};
        #case_info{status = Status} when Status =/= running ->
            {reply, {error, not_running}, State};
        CaseInfo ->
            UpdatedCase = CaseInfo#case_info{status = suspended},
            NewCases = maps:put(CaseId, UpdatedCase, State#control_state.cases),
            logger:info("Case suspended: id=~p reason=~p", [CaseId, Reason],
                         [{yawl_control, case_suspended}]),
            notify_subscribers(case_suspended, CaseId, State#control_state.subscribers),
            {reply, ok, State#control_state{cases = NewCases}}
    end;

handle_call({resume_case, CaseId, Reason}, _From, State) ->
    case maps:get(CaseId, State#control_state.cases, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #case_info{status = running} ->
            {reply, {error, not_suspended}, State};
        #case_info{status = Status} when Status =/= suspended ->
            {reply, {error, invalid_state}, State};
        CaseInfo ->
            UpdatedCase = CaseInfo#case_info{status = running},
            NewCases = maps:put(CaseId, UpdatedCase, State#control_state.cases),
            logger:info("Case resumed: id=~p reason=~p", [CaseId, Reason],
                         [{yawl_control, case_resumed}]),
            notify_subscribers(case_resumed, CaseId, State#control_state.subscribers),
            {reply, ok, State#control_state{cases = NewCases}}
    end;

handle_call(get_case_statistics, _From, State) ->
    Stats = compute_statistics(State),
    {reply, Stats, State};

handle_call(get_engine_status, _From, State) ->
    StatusMap = engine_status_to_map(State#control_state.status),
    {reply, StatusMap, State};

handle_call({set_parameter, Parameter, Value}, _From, State) ->
    NewParams = maps:put(Parameter, Value, State#control_state.status#engine_status.parameters),
    NewStatus = State#control_state.status#engine_status{parameters = NewParams},
    logger:info("Parameter set: ~p=~p", [Parameter, Value],
                 [{yawl_control, parameter_set}]),
    {reply, ok, State#control_state{status = NewStatus}};

handle_call(list_all_cases, _From, State) ->
    Cases = maps:fold(
        fun(_CaseId, CaseInfo, Acc) ->
            [case_info_to_map(CaseInfo) | Acc]
        end,
        [],
        State#control_state.cases
    ),
    {reply, lists:reverse(Cases), State};

handle_call({get_case_history, CaseId}, _From, State) ->
    case maps:get(CaseId, State#control_state.cases, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        _CaseInfo ->
            %% History would be stored separately; for now return case info
            {reply, [], State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), #control_state{}) -> {noreply, #control_state{}}.

handle_cast({register_case, CaseId, SpecId}, State) ->
    CaseInfo = #case_info{
        case_id = CaseId,
        spec_id = SpecId,
        status = running,
        start_time = erlang:timestamp(),
        end_time = undefined
    },
    NewCases = maps:put(CaseId, CaseInfo, State#control_state.cases),
    logger:info("Case registered: id=~p spec=~p", [CaseId, SpecId],
                 [{yawl_control, case_registered}]),
    notify_subscribers(case_started, CaseId, State#control_state.subscribers),
    {noreply, State#control_state{cases = NewCases}};

handle_cast({unregister_case, CaseId}, State) ->
    NewCases = maps:remove(CaseId, State#control_state.cases),
    {noreply, State#control_state{cases = NewCases}};

handle_cast({update_case_status, CaseId, Status}, State) ->
    case maps:get(CaseId, State#control_state.cases, undefined) of
        undefined ->
            {noreply, State};
        CaseInfo ->
            UpdatedCase = CaseInfo#case_info{status = Status},
            NewCases = maps:put(CaseId, UpdatedCase, State#control_state.cases),

            NewStatus = case Status of
                completed ->
                    State#control_state.status#engine_status{
                        cases_completed = State#control_state.status#engine_status.cases_completed + 1
                    };
                failed ->
                    State#control_state.status#engine_status{
                        cases_failed = State#control_state.status#engine_status.cases_failed + 1
                    };
                _ ->
                    State#control_state.status
            end,

            notify_subscribers({status_update, Status}, CaseId, State#control_state.subscribers),
            {noreply, State#control_state{cases = NewCases, status = NewStatus}}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handles non-GEN_SERVER messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), #control_state{}) -> {noreply, #control_state{}}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Remove dead subscriber
    NewSubscribers = lists:filter(
        fun({SubPid, _RefInner}) -> SubPid =/= Pid end,
        State#control_state.subscribers
    ),
    {noreply, State#control_state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handles code change.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), #control_state{}, term()) -> {ok, #control_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Handles termination.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), #control_state{}) -> ok.

terminate(_Reason, _State) ->
    logger:info("YAWL control stopping", [{yawl_control, stopping}]),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Converts a case_info record to a map.
%%
%% @end
%%--------------------------------------------------------------------
-spec case_info_to_map(#case_info{}) -> map().

case_info_to_map(#case_info{
    case_id = CaseId,
    spec_id = SpecId,
    status = Status,
    start_time = StartTime,
    end_time = EndTime,
    current_task = CurrentTask,
    tasks_completed = Completed,
    tasks_total = Total,
    data = Data
}) ->
    Duration = case EndTime of
        undefined -> timer:now_diff(erlang:timestamp(), StartTime);
        _ -> timer:now_diff(EndTime, StartTime)
    end,
    #{
        case_id => CaseId,
        spec_id => SpecId,
        status => Status,
        start_time => timestamp_to_binary(StartTime),
        end_time => timestamp_to_binary(EndTime),
        duration_ms => Duration div 1000,
        current_task => CurrentTask,
        progress => case Total of
            0 -> 0.0;
            _ -> Completed / Total
        end,
        tasks_completed => Completed,
        tasks_total => Total,
        data => Data
    }.

%%--------------------------------------------------------------------
%% @doc Converts engine_status record to a map.
%%
%% @end
%%--------------------------------------------------------------------
-spec engine_status_to_map(#engine_status{}) -> map().

engine_status_to_map(#engine_status{
    start_time = StartTime,
    cases_completed = Completed,
    cases_cancelled = Cancelled,
    cases_failed = Failed,
    parameters = Params
}) ->
    Uptime = timer:now_diff(erlang:timestamp(), StartTime),
    #{
        start_time => timestamp_to_binary(StartTime),
        uptime_ms => Uptime div 1000,
        cases_completed => Completed,
        cases_cancelled => Cancelled,
        cases_failed => Failed,
        parameters => Params
    }.

%%--------------------------------------------------------------------
%% @doc Computes case statistics from current state.
%%
%% @end
%%--------------------------------------------------------------------
-spec compute_statistics(#control_state{}) -> map().

compute_statistics(State) ->
    Cases = maps:values(State#control_state.cases),

    RunningCount = length([C || C <- Cases, C#case_info.status =:= running]),
    SuspendedCount = length([C || C <- Cases, C#case_info.status =:= suspended]),
    CompletedCount = length([C || C <- Cases, C#case_info.status =:= completed]),
    CancelledCount = length([C || C <- Cases, C#case_info.status =:= cancelled]),
    FailedCount = length([C || C <- Cases, C#case_info.status =:= failed]),

    AvgDuration = case [C || C <- Cases, C#case_info.end_time =/= undefined] of
        [] -> 0;
        CompletedCases ->
            TotalDuration = lists:foldl(
                fun(C, Acc) ->
                    D = timer:now_diff(C#case_info.end_time, C#case_info.start_time),
                    Acc + D
                end,
                0,
                CompletedCases
            ),
            TotalDuration div length(CompletedCases) div 1000
    end,

    #{
        running => RunningCount,
        suspended => SuspendedCount,
        completed => CompletedCount,
        cancelled => CancelledCount,
        failed => FailedCount,
        total => length(Cases),
        avg_completion_time_ms => AvgDuration
    }.

%%--------------------------------------------------------------------
%% @doc Converts timestamp to binary string.
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp_to_binary(erlang:timestamp() | undefined) -> binary() | undefined.

timestamp_to_binary(undefined) -> undefined;
timestamp_to_binary({MegaSecs, Secs, MicroSecs}) ->
    list_to_binary(io_lib:format("~p.~p.~p", [MegaSecs, Secs, MicroSecs])).

%%--------------------------------------------------------------------
%% @doc Notifies all subscribers of a case event.
%%
%% @end
%%--------------------------------------------------------------------
-spec notify_subscribers(term(), binary(), [{pid(), reference()}]) -> ok.

notify_subscribers(Event, CaseId, Subscribers) ->
    lists:foreach(
        fun({Pid, _Ref}) ->
            Pid ! {yawl_control_event, Event, CaseId}
        end,
        Subscribers
    ),
    ok.
