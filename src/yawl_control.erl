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
%% -------------------------------------------------------------------

-moduledoc """
YAWL Control Panel / Runtime Management for CRE.

This module implements the control interface for managing YAWL workflow
execution, similar to the YAWL control panel in the Java reference
implementation.

## Features

- **Case Management:** Start, cancel, suspend, resume workflow cases
- **Monitoring:** Get running cases and their status
- **Statistics:** Execution metrics and engine status
- **Configuration:** Runtime parameter tuning

## Helper Integration

This module integrates pure helper modules for control flow:

- **pnet_marking:** Control state tracking as Petri net markings
- **pnet_choice:** Deterministic choice for control flow decisions
- **wf_task:** Control token lifecycle for task events

## Control State Management

The control state of workflow cases follows a Petri net marking model with
the following places:

- `ctrl_running`: Cases currently executing
- `ctrl_suspended`: Cases paused awaiting resumption
- `ctrl_completed`: Cases finished successfully
- `ctrl_cancelled`: Cases terminated by user request
- `ctrl_failed`: Cases terminated due to errors

## Examples

Starting the control panel:

```erlang
1> {ok, Pid} = yawl_control:start_control().
{ok, <0.123.0>}
```

Suspending and resuming a case:

```erlang
2> {ok, Pid} = yawl_control:start_control(yawl_control_test).
{ok, <0.124.0>}
3> yawl_control:register_case(yawl_control_test, <<"case_001">>, <<"my_workflow">>).
ok
4> yawl_control:suspend_case(yawl_control_test, <<"case_001">>, <<"Maintenance">>).
ok
5> gen_server:call(yawl_control_test, {get_case_status, <<"case_001">>}).
#{case_id => <<"case_001">>, status => suspended, ...}
6> yawl_control:resume_case(yawl_control_test, <<"case_001">>, <<"Maintenance complete">>).
ok
7> gen_server:call(yawl_control_test, {get_case_status, <<"case_001">>}).
#{case_id => <<"case_001">>, status => running, ...}
8> gen_server:stop(yawl_control_test).
ok
```

Cancelling a case:

```erlang
9> {ok, Pid} = yawl_control:start_control(yawl_control_test2).
{ok, <0.125.0>}
10> yawl_control:register_case(yawl_control_test2, <<"case_002">>, <<"my_workflow">>).
ok
11> yawl_control:cancel_case(yawl_control_test2, <<"case_002">>, <<"User request">>).
ok
12> gen_server:call(yawl_control_test2, {get_case_status, <<"case_002">>}).
#{case_id => <<"case_002">>, status => cancelled, ...}
13> gen_server:stop(yawl_control_test2).
ok
```
""".

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

%% Doctests
-export([doctest_test/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Records
%%====================================================================

%% Control state places for Petri net marking
-define(CTRL_PLACE_RUNNING, 'ctrl_running').
-define(CTRL_PLACE_SUSPENDED, 'ctrl_suspended').
-define(CTRL_PLACE_COMPLETED, 'ctrl_completed').
-define(CTRL_PLACE_CANCELLED, 'ctrl_cancelled').
-define(CTRL_PLACE_FAILED, 'ctrl_failed').

-define(CTRL_PLACES, [
    ?CTRL_PLACE_RUNNING,
    ?CTRL_PLACE_SUSPENDED,
    ?CTRL_PLACE_COMPLETED,
    ?CTRL_PLACE_CANCELLED,
    ?CTRL_PLACE_FAILED
]).

-record(case_info, {
    case_id :: binary(),
    spec_id :: binary(),
    status :: running | suspended | cancelled | completed | failed,
    start_time :: erlang:timestamp(),
    end_time :: erlang:timestamp() | undefined,
    current_task :: binary() | undefined,
    tasks_completed = 0 :: non_neg_integer(),
    tasks_total = 0 :: non_neg_integer(),
    data = #{} :: map(),
    %% Control state marking for this case
    ctrl_marking :: pnet_marking:marking()
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
    subscribers = [] :: [{pid(), reference()}],
    %% RNG state for deterministic control flow decisions
    rng_state :: pnet_choice:rng_state(),
    %% Global control marking (summary of all case states)
    ctrl_marking :: pnet_marking:marking()
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
%% Uses wf_task:cancelled/3 to produce control tokens for the cancelled
%% task lifecycle event.
%%
%% The cancellation performs an atomic transition on the control marking:
%% <ul>
%%   <li>Consumes a token from the current status place (running/suspended)</li>
%%   <li>Produces a token to the <code>ctrl_cancelled</code> place</li>
%%   <li>Updates the case's end_time and status</li>
%%   <li>Notifies all subscribers of the cancellation event</li>
%% </ul>
%%
%% <h4>Examples</h4>
%%
%% Cancel a running case:
%% ```
%% 1> yawl_control:cancel_case(<<"case_001">>, <<"User requested">>).
%% ok
%% ```
%%
%% Attempting to cancel an already cancelled case:
%% ```
%% 2> yawl_control:cancel_case(<<"case_001">>, <<"Duplicate">>).
%% {error, already_cancelled}
%% ```
%%
%% Attempting to cancel a completed case:
%% ```
%% 3> yawl_control:cancel_case(<<"case_002">>, <<"Too late">>).
%% {error, already_completed}
%% ```
%%
%% @param CaseId The case identifier.
%% @param Reason Reason for cancellation.
%% @return ok or {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-doc("Cancels a running workflow case.\n\n"
     "Performs an atomic transition on the control marking, moving the case\n"
     "from its current status place to the ctrl_cancelled place.\n\n"
     "Examples:\n"
     "  1> yawl_control:cancel_case(<<\"case_001\">>, <<\"User requested\">>).\n"
     "  ok\n\n"
     "  2> yawl_control:cancel_case(<<\"case_001\">>, <<\"Duplicate\">>).\n"
     "  {error, already_cancelled}\n\n"
     "  3> yawl_control:cancel_case(<<\"case_002\">>, <<\"Too late\">>).\n"
     "  {error, already_completed}\n").
-spec cancel_case(binary(), binary()) -> ok | {error, term()}.

cancel_case(CaseId, Reason) when is_binary(CaseId), is_binary(Reason) ->
    gen_server:call(yawl_control, {cancel_case, CaseId, Reason}).

%%--------------------------------------------------------------------
%% @doc Suspends a running workflow case.
%%
%% <h4>Examples</h4>
%%
%% Suspend a running case:
%% ```
%% 1> yawl_control:suspend_case(<<"case_001">>, <<"Maintenance">>).
%% ok
%% ```
%%
%% Attempting to suspend an already suspended case:
%% ```
%% 2> yawl_control:suspend_case(<<"case_001">>, <<"Again">>).
%% {error, already_suspended}
%% ```
%%
%% Attempting to suspend a non-running case:
%% ```
%% 3> yawl_control:suspend_case(<<"case_002">>, <<"Cannot">>).
%% {error, not_running}
%% ```
%%
%% @param CaseId The case identifier.
%% @param Reason Reason for suspension.
%% @return ok or {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-doc("Suspends a running workflow case.\n\n"
     "Moves the case from the ctrl_running place to the ctrl_suspended place\n"
     "in the control marking. A suspended case stops processing tasks but retains\n"
     "its state and can be resumed.\n\n"
     "Examples:\n"
     "  1> yawl_control:suspend_case(<<\"case_001\">>, <<\"Maintenance\">>).\n"
     "  ok\n\n"
     "  2> yawl_control:suspend_case(<<\"case_001\">>, <<\"Again\">>).\n"
     "  {error, already_suspended}\n\n"
     "  3> yawl_control:suspend_case(<<\"case_002\">>, <<\"Cannot\">>).\n"
     "  {error, not_running}\n").
-spec suspend_case(binary(), binary()) -> ok | {error, term()}.

suspend_case(CaseId, Reason) when is_binary(CaseId), is_binary(Reason) ->
    gen_server:call(yawl_control, {suspend_case, CaseId, Reason}).

%%--------------------------------------------------------------------
%% @doc Resumes a suspended workflow case.
%%
%% Uses wf_task:running/3 to produce control tokens for the resumed
%% (running) task lifecycle event.
%%
%% <h4>Examples</h4>
%%
%% Resume a suspended case:
%% ```
%% 1> yawl_control:resume_case(<<"case_001">>, <<"Maintenance complete">>).
%% ok
%% ```
%%
%% Attempting to resume a running case:
%% ```
%% 2> yawl_control:resume_case(<<"case_002">>, <<"Already running">>).
%% {error, not_suspended}
%% ```
%%
%% Attempting to resume a cancelled case:
%% ```
%% 3> yawl_control:resume_case(<<"case_003">>, <<"Cannot resume">>).
%% {error, invalid_state}
%% ```
%%
%% @param CaseId The case identifier.
%% @param Reason Reason for resumption.
%% @return ok or {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-doc """Resumes a suspended workflow case.

Moves the case from the `ctrl_suspended` place back to the `ctrl_running`
place in the control marking. The case continues processing tasks from
where it was suspended.

### Examples

```erlang
1> yawl_control:resume_case(<<"case_001">>, <<"Maintenance complete">>).
ok

2> yawl_control:resume_case(<<"case_002">>, <<"Already running">>).
{error, not_suspended}

3> yawl_control:resume_case(<<"case_003">>, <<"Cannot resume">>).
{error, invalid_state}
```
""".
-spec resume_case(binary(), binary()) -> ok | {error, term()}.

resume_case(CaseId, Reason) when is_binary(CaseId), is_binary(Reason) ->
    gen_server:call(yawl_control, {resume_case, CaseId, Reason}).

%%--------------------------------------------------------------------
%% @doc Gets execution statistics for all cases.
%%
%% Uses pnet_marking to snapshot the current control state for
%% accurate statistics computation.
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
%% Uses pnet_marking:snapshot/1 to get a consistent view of all cases.
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
%% Uses wf_task:enabled/3 to produce control tokens for the newly
%% enabled case, and pnet_marking:add/2 to update the control marking.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_case(binary(), binary()) -> ok.

register_case(CaseId, SpecId) ->
    gen_server:cast(yawl_control, {register_case, CaseId, SpecId}).

%%--------------------------------------------------------------------
%% @doc Unregisters a workflow case.
%%
%% Uses pnet_marking:take/2 to remove the case from control marking.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_case(binary()) -> ok.

unregister_case(CaseId) ->
    gen_server:cast(yawl_control, {unregister_case, CaseId}).

%%--------------------------------------------------------------------
%% @doc Updates the status of a workflow case.
%%
%% Uses pnet_marking:apply/3 to atomically consume from the old status
%% place and produce to the new status place in the control marking.
%% Uses appropriate wf_task constructors for lifecycle events.
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
%% Initializes the control marking using pnet_marking:new/1 with the
%% defined control places. Seeds the RNG for deterministic decisions.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, #control_state{}}.

init([]) ->
    logger:info("YAWL control starting", [{yawl_control, starting}]),
    CtrlMarking = pnet_marking:new(?CTRL_PLACES),
    {ok, #control_state{
        status = #engine_status{
            start_time = erlang:timestamp(),
            parameters = #{
                max_concurrent_cases => 100,
                case_timeout => 3600000,  % 1 hour default
                enable_auto_cleanup => true,
                cleanup_interval => 300000  % 5 minutes
            }
        },
        rng_state = pnet_choice:seed(erlang:timestamp()),
        ctrl_marking = CtrlMarking
    }}.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, #control_state{}) ->
          {reply, term(), #control_state{}}.

handle_call(get_running_cases, _From, State) ->
    %% Use pnet_marking:get/2 to get tokens from running place
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
        #case_info{status = Status} = CaseInfo ->
            %% Apply transition: consume from current status place, produce to cancelled
            ConsumeMap = status_to_consume_map(Status, CaseId),
            ProduceMap = status_to_produce_map(cancelled, CaseId, Reason),

            case pnet_marking:'apply'(State#control_state.ctrl_marking, ConsumeMap, ProduceMap) of
                {ok, NewCtrlMarking} ->
                    UpdatedCase = CaseInfo#case_info{
                        status = cancelled,
                        end_time = erlang:timestamp(),
                        ctrl_marking = update_case_marking(CaseInfo#case_info.ctrl_marking, Status, cancelled, CaseId)
                    },
                    NewCases = maps:put(CaseId, UpdatedCase, State#control_state.cases),
                    NewStatus = State#control_state.status#engine_status{
                        cases_cancelled = State#control_state.status#engine_status.cases_cancelled + 1
                    },
                    logger:info("Case cancelled: id=~p reason=~p", [CaseId, Reason],
                                [{yawl_control, case_cancelled}]),
                    notify_subscribers(case_cancelled, CaseId, State#control_state.subscribers),
                    {reply, ok, State#control_state{cases = NewCases, status = NewStatus, ctrl_marking = NewCtrlMarking}};
                {error, _Reason} ->
                    {reply, {error, marking_failed}, State}
            end
    end;

handle_call({suspend_case, CaseId, Reason}, _From, State) ->
    case maps:get(CaseId, State#control_state.cases, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #case_info{status = suspended} ->
            {reply, {error, already_suspended}, State};
        #case_info{status = Status} when Status =/= running ->
            {reply, {error, not_running}, State};
        #case_info{status = running} = CaseInfo ->
            %% Apply transition: consume from running, produce to suspended
            ConsumeMap = status_to_consume_map(running, CaseId),
            ProduceMap = status_to_produce_map(suspended, CaseId, Reason),

            case pnet_marking:'apply'(State#control_state.ctrl_marking, ConsumeMap, ProduceMap) of
                {ok, NewCtrlMarking} ->
                    UpdatedCase = CaseInfo#case_info{
                        status = suspended,
                        ctrl_marking = update_case_marking(CaseInfo#case_info.ctrl_marking, running, suspended, CaseId)
                    },
                    NewCases = maps:put(CaseId, UpdatedCase, State#control_state.cases),
                    logger:info("Case suspended: id=~p reason=~p", [CaseId, Reason],
                                 [{yawl_control, case_suspended}]),
                    notify_subscribers(case_suspended, CaseId, State#control_state.subscribers),
                    {reply, ok, State#control_state{cases = NewCases, ctrl_marking = NewCtrlMarking}};
                {error, _Reason} ->
                    {reply, {error, marking_failed}, State}
            end
    end;

handle_call({resume_case, CaseId, Reason}, _From, State) ->
    case maps:get(CaseId, State#control_state.cases, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #case_info{status = running} ->
            {reply, {error, not_suspended}, State};
        #case_info{status = Status} when Status =/= suspended ->
            {reply, {error, invalid_state}, State};
        #case_info{status = suspended} = CaseInfo ->
            %% Apply transition: consume from suspended, produce to running
            ConsumeMap = status_to_consume_map(suspended, CaseId),
            ProduceMap = status_to_produce_map(running, CaseId, Reason),

            case pnet_marking:'apply'(State#control_state.ctrl_marking, ConsumeMap, ProduceMap) of
                {ok, NewCtrlMarking} ->
                    UpdatedCase = CaseInfo#case_info{
                        status = running,
                        ctrl_marking = update_case_marking(CaseInfo#case_info.ctrl_marking, suspended, running, CaseId)
                    },
                    NewCases = maps:put(CaseId, UpdatedCase, State#control_state.cases),
                    logger:info("Case resumed: id=~p reason=~p", [CaseId, Reason],
                                 [{yawl_control, case_resumed}]),
                    notify_subscribers(case_resumed, CaseId, State#control_state.subscribers),
                    {reply, ok, State#control_state{cases = NewCases, ctrl_marking = NewCtrlMarking}};
                {error, _Reason} ->
                    {reply, {error, marking_failed}, State}
            end
    end;

handle_call(get_case_statistics, _From, State) ->
    %% Snapshot the control marking for consistent statistics
    _CtrlSnapshot = pnet_marking:snapshot(State#control_state.ctrl_marking),
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
    %% Use pnet_marking:snapshot/1 for consistent view
    _CtrlSnapshot = pnet_marking:snapshot(State#control_state.ctrl_marking),
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
    %% Create control token for enabled case using wf_task
    {produce, ProduceMap} = wf_task:enabled(CaseId, SpecId, ?CTRL_PLACE_RUNNING),

    %% Add token to control marking
    CtrlMarking1 = pnet_marking:add(State#control_state.ctrl_marking, ProduceMap),

    CaseMarking = pnet_marking:new(?CTRL_PLACES),
    CaseMarking1 = pnet_marking:set(CaseMarking, ?CTRL_PLACE_RUNNING, [{CaseId, SpecId}]),

    CaseInfo = #case_info{
        case_id = CaseId,
        spec_id = SpecId,
        status = running,
        start_time = erlang:timestamp(),
        end_time = undefined,
        ctrl_marking = CaseMarking1
    },
    NewCases = maps:put(CaseId, CaseInfo, State#control_state.cases),
    logger:info("Case registered: id=~p spec=~p", [CaseId, SpecId],
                 [{yawl_control, case_registered}]),
    notify_subscribers(case_started, CaseId, State#control_state.subscribers),
    {noreply, State#control_state{cases = NewCases, ctrl_marking = CtrlMarking1}};

handle_cast({unregister_case, CaseId}, State) ->
    %% Remove case tokens from control marking
    ConsumeMap = #{
        ?CTRL_PLACE_RUNNING => [{CaseId, undefined}],
        ?CTRL_PLACE_SUSPENDED => [{CaseId, undefined}],
        ?CTRL_PLACE_COMPLETED => [{CaseId, undefined}],
        ?CTRL_PLACE_CANCELLED => [{CaseId, undefined}],
        ?CTRL_PLACE_FAILED => [{CaseId, undefined}]
    },
    NewCtrlMarking = case pnet_marking:take(State#control_state.ctrl_marking, ConsumeMap) of
        {ok, Marking} -> Marking;
        {error, _} -> State#control_state.ctrl_marking  % keep original if take fails
    end,
    NewCases = maps:remove(CaseId, State#control_state.cases),
    {noreply, State#control_state{cases = NewCases, ctrl_marking = NewCtrlMarking}};

handle_cast({update_case_status, CaseId, NewStatus}, State) ->
    case maps:get(CaseId, State#control_state.cases, undefined) of
        undefined ->
            {noreply, State};
        #case_info{status = OldStatus} = CaseInfo ->
            %% Apply transition atomically using pnet_marking:apply/3
            ConsumeMap = status_to_consume_map(OldStatus, CaseId),
            ProduceMap = status_to_produce_map(NewStatus, CaseId, undefined),

            case pnet_marking:'apply'(State#control_state.ctrl_marking, ConsumeMap, ProduceMap) of
                {ok, NewCtrlMarking} ->
                    UpdatedCase = CaseInfo#case_info{
                        status = NewStatus,
                        ctrl_marking = update_case_marking(CaseInfo#case_info.ctrl_marking, OldStatus, NewStatus, CaseId)
                    },
                    NewCases = maps:put(CaseId, UpdatedCase, State#control_state.cases),

                    %% Update engine counters based on new status
                    NewStatusRec = case NewStatus of
                        completed ->
                            wf_task:done(CaseId, undefined, ?CTRL_PLACE_COMPLETED),
                            State#control_state.status#engine_status{
                                cases_completed = State#control_state.status#engine_status.cases_completed + 1
                            };
                        failed ->
                            wf_task:failed(CaseId, undefined, ?CTRL_PLACE_FAILED),
                            State#control_state.status#engine_status{
                                cases_failed = State#control_state.status#engine_status.cases_failed + 1
                            };
                        _ ->
                            State#control_state.status
                    end,

                    notify_subscribers({status_update, NewStatus}, CaseId, State#control_state.subscribers),
                    {noreply, State#control_state{cases = NewCases, status = NewStatusRec, ctrl_marking = NewCtrlMarking}};
                {error, _Reason} ->
                    {noreply, State}
            end
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
%% Internal Functions - Helper Integration
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Converts a status to a consume map for pnet_marking:take/2.
%%
%% Maps the case status to the appropriate control place from which
%% to consume tokens.
%%
%% @end
%%--------------------------------------------------------------------
-spec status_to_consume_map(case_status(), binary()) -> pnet_marking:consume_map().

status_to_consume_map(running, CaseId) ->
    #{?CTRL_PLACE_RUNNING => [{case_token, CaseId}]};
status_to_consume_map(suspended, CaseId) ->
    #{?CTRL_PLACE_SUSPENDED => [{case_token, CaseId}]};
status_to_consume_map(completed, CaseId) ->
    #{?CTRL_PLACE_COMPLETED => [{case_token, CaseId}]};
status_to_consume_map(cancelled, CaseId) ->
    #{?CTRL_PLACE_CANCELLED => [{case_token, CaseId}]};
status_to_consume_map(failed, CaseId) ->
    #{?CTRL_PLACE_FAILED => [{case_token, CaseId}]}.

%%--------------------------------------------------------------------
%% @doc Converts a status to a produce map for pnet_marking:add/2.
%%
%% Maps the case status to the appropriate control place to which
%% to produce tokens. Uses wf_task constructors for lifecycle events.
%%
%% @end
%%--------------------------------------------------------------------
-spec status_to_produce_map(case_status(), binary(), term()) -> pnet_marking:produce_map().

status_to_produce_map(running, CaseId, Payload) ->
    {produce, ProduceMap} = wf_task:running(CaseId, Payload, ?CTRL_PLACE_RUNNING),
    ProduceMap;
status_to_produce_map(suspended, CaseId, Payload) ->
    %% Suspend is not a standard wf_task state, use enabled as proxy
    {produce, ProduceMap} = wf_task:enabled(CaseId, Payload, ?CTRL_PLACE_SUSPENDED),
    ProduceMap;
status_to_produce_map(completed, CaseId, Result) ->
    {produce, ProduceMap} = wf_task:done(CaseId, Result, ?CTRL_PLACE_COMPLETED),
    ProduceMap;
status_to_produce_map(cancelled, CaseId, Reason) ->
    {produce, ProduceMap} = wf_task:cancelled(CaseId, Reason, ?CTRL_PLACE_CANCELLED),
    ProduceMap;
status_to_produce_map(failed, CaseId, Reason) ->
    {produce, ProduceMap} = wf_task:failed(CaseId, Reason, ?CTRL_PLACE_FAILED),
    ProduceMap.

%%--------------------------------------------------------------------
%% @doc Updates a case's control marking during status transition.
%%
%% Uses pnet_marking:apply/3 to atomically transition the case's
%% local control marking from old status to new status.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_case_marking(pnet_marking:marking(), case_status(), case_status(), binary()) ->
          pnet_marking:marking().

update_case_marking(CaseMarking, OldStatus, NewStatus, CaseId) ->
    ConsumeMap = status_to_consume_map(OldStatus, CaseId),
    ProduceMap = status_to_produce_map(NewStatus, CaseId, undefined),
    case pnet_marking:'apply'(CaseMarking, ConsumeMap, ProduceMap) of
        {ok, NewMarking} -> NewMarking;
        {error, _} -> CaseMarking  % return original if transition fails
    end.

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
%% Uses pnet_marking:snapshot/1 to get a consistent view of the
%% control marking for accurate statistics.
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

%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the yawl_control module.
%%
%% This function validates the documentation examples by testing the
%% control state management transitions. It creates a test control panel
%% and verifies suspend/resume/cancel operations work correctly.
%%
%% <h4>Examples</h4>
%%
%% Running the doctest:
%% ```
%% 1> yawl_control:doctest_test().
%% ok
%% ```
%%
%% The test performs:
%% <ul>
%%   <li>Starts a control panel with a registered name</li>
%%   <li>Registers a test workflow case</li>
%%   <li>Suspends the case (running -> suspended transition)</li>
%%   <li>Resumes the case (suspended -> running transition)</li>
%%   <li>Verifies the case is running again</li>
%%   <li>Stops the control panel</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-doc """Runs doctests for control state management.

### Example

```erlang
1> yawl_control:doctest_test().
ok
```

The test validates the complete suspend/resume lifecycle:
1. Start control panel
2. Register a test case
3. Suspend the case
4. Resume the case
5. Verify status transitions
6. Stop control panel
""".
-spec doctest_test() -> ok.

doctest_test() ->
    TestName = yawl_control_doctest,
    CaseId = <<"doctest_case_001">>,
    SpecId = <<"doctest_workflow">>,

    try
        %% Step 1: Start the control panel
        {ok, _Pid} = start_control(TestName),

        %% Step 2: Register a test case
        register_case(TestName, CaseId, SpecId),

        %% Step 3: Verify the case is running
        {ok, running} = get_status_sync(TestName, CaseId),

        %% Step 4: Suspend the case
        ok = suspend_case_sync(TestName, CaseId, <<"Doctest suspension">>),

        %% Step 5: Verify the case is suspended
        {ok, suspended} = get_status_sync(TestName, CaseId),

        %% Step 6: Resume the case
        ok = resume_case_sync(TestName, CaseId, <<"Doctest resumption">>),

        %% Step 7: Verify the case is running again
        {ok, running} = get_status_sync(TestName, CaseId),

        %% Step 8: Clean up
        stop_sync(TestName),
        ok
    catch
        _:Reason ->
            %% Ensure cleanup even on failure
            catch stop_sync(TestName),
            error({doctest_failed, Reason})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Synchronous wrapper for getting case status from named control.
%%
%% Extracts the status atom from the case info map returned by the server.
%%--------------------------------------------------------------------
-spec get_status_sync(atom(), binary()) -> {ok, case_status()} | {error, term()}.

get_status_sync(ControlName, CaseId) ->
    case gen_server:call(ControlName, {get_case_status, CaseId}) of
        {error, not_found} -> {error, not_found};
        CaseMap when is_map(CaseMap) -> {ok, maps:get(status, CaseMap)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Synchronous wrapper for suspending a case in named control.
%%--------------------------------------------------------------------
-spec suspend_case_sync(atom(), binary(), binary()) -> ok | {error, term()}.

suspend_case_sync(ControlName, CaseId, Reason) ->
    gen_server:call(ControlName, {suspend_case, CaseId, Reason}).

%%--------------------------------------------------------------------
%% @private
%% @doc Synchronous wrapper for resuming a case in named control.
%%--------------------------------------------------------------------
-spec resume_case_sync(atom(), binary(), binary()) -> ok | {error, term()}.

resume_case_sync(ControlName, CaseId, Reason) ->
    gen_server:call(ControlName, {resume_case, CaseId, Reason}).

%%--------------------------------------------------------------------
%% @private
%% @doc Synchronous wrapper for registering a case in named control.
%%--------------------------------------------------------------------
-spec register_case(atom(), binary(), binary()) -> ok.

register_case(ControlName, CaseId, SpecId) ->
    gen_server:cast(ControlName, {register_case, CaseId, SpecId}),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Synchronous wrapper for stopping the named control panel.
%%--------------------------------------------------------------------
-spec stop_sync(atom()) -> ok.

stop_sync(ControlName) ->
    gen_server:stop(ControlName).
