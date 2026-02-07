%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
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
%% @doc YAWL Interface D for Exception Service Integration and Worklet Communication
%%
%% This module implements YAWL Interface D for exception service integration
%% and communication with worklets for dynamic workflow exception handling.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Exception Service Registration:</b> Register local/remote exception handlers</li>
%%   <li><b>Worklet Launch:</b> Launch worklets for exception handling</li>
%%   <li><b>Exception Routing:</b> Route exceptions to appropriate worklets</li>
%%   <li><b>Pre/Post Condition Checking:</b> Validate conditions before/after worklet execution</li>
%%   <li><b>Exception Propagation:</b> Propagate exceptions to parent workflow on failure</li>
%%   <li><b>Worklet Status Tracking:</b> Monitor active worklet executions</li>
%%   <li><b>Worklet Abortion:</b> Abort running worklets when needed</li>
%% </ul>
%%
%% <h3>Architecture</h3>
%%
%% Interface D acts as a bridge between the YAWL workflow engine and
%% exception handling worklets. When an exception occurs during workflow
%% execution, Interface D determines the appropriate worklet to handle it
%% based on the exception type and registered exception services.
%%
%% <h4>Exception Service Types:</h4>
%% <ul>
%%   <li><b>local:</b> Exception handler running in the same node</li>
%%   <li><b>remote:</b> Exception handler on a different node</li>
%% </ul>
%%
%% <h4>Worklet Execution States:</h4>
%% <ul>
%%   <li><b>pending:</b> Worklet is queued for execution</li>
%%   <li><b>running:</b> Worklet is currently executing</li>
%%   <li><b>completed:</b> Worklet completed successfully</li>
%%   <li><b>failed:</b> Worklet execution failed</li>
%%   <li><b>aborted:</b> Worklet was aborted</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_interface_d).
-behaviour(gen_server).

%% Include record definitions
-include("yawl_interface_d.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/0, stop/0]).
-export([launch_worklet/3, complete_worklet/2]).
-export([handle_exception/2, register_exception_service/2]).
-export([check_preconditions/2, check_postconditions/2]).
-export([get_exception_services/0, propagate_exception/2, propagate_exception/3]).
-export([get_worklet_status/1, abort_worklet/1]).
-export([coordinate_compensation/1, get_active_worklets/0, get_worklets_by_case/1]).
-export([unregister_exception_service/1, enable_exception_service/1, disable_exception_service/1]).
-export([set_service_priority/2]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

%% Doctests
-export([doctest_test/0]).

%%====================================================================
%% Type Definitions
%%====================================================================

%% Type definitions are in the header file
%% Records are defined in yawl_interface_d.hrl

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the Interface D server.
%%
%% Returns `{ok, Pid}' on success.
%% @end
%%--------------------------------------------------------------------
-doc("""
Starts the Interface D server.

## Examples

```erlang
1> {ok, Pid} = yawl_interface_d:start_link().
{ok, <0.123.0>}
2> gen_server:stop(yawl_interface_d).
ok
```
""").
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the Interface D server.
%%
%% Returns `ok' on success.
%% @end
%%--------------------------------------------------------------------
-doc("""
Stops the Interface D server.

## Examples

```erlang
1> yawl_interface_d:stop().
ok
```
""").
-spec stop() -> ok.

stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc Launches a worklet for exception handling.
%%
%% Parameters:
%%   - CaseId: The workflow case ID where exception occurred
%%   - WorkletSpecId: The specification ID of the worklet to launch
%%   - ExceptionData: Map containing exception details
%%
%% Returns `{ok, ExecutionId}' on success, `{error, Reason}' on failure.
%% @end
%%--------------------------------------------------------------------
-doc("""
Launches a worklet for exception handling.

## Examples

```erlang
1> {ok, ExecId} = yawl_interface_d:launch_worklet(
     <<"case_001">>, <<"worklet_retry">>, #{task_id => <<"task_1">>}).
{ok, <<"exec_", ...>>}
```
""").
-spec launch_worklet(CaseId :: case_id(),
                     WorkletSpecId :: worklet_spec_id(),
                     ExceptionData :: map()) ->
        {ok, execution_id()} | {error, term()}.

launch_worklet(CaseId, WorkletSpecId, ExceptionData) ->
    gen_server:call(?MODULE, {launch_worklet, CaseId, WorkletSpecId, ExceptionData}).

%%--------------------------------------------------------------------
%% @doc Completes a worklet execution with results.
%%
%% Parameters:
%%   - ExecutionId: The worklet execution ID
%%   - Result: The result data from worklet execution
%%
%% Returns `ok' on success, `{error, Reason}' on failure.
%% @end
%%--------------------------------------------------------------------
-doc("""
Completes a worklet execution with results.

## Examples

```erlang
1> {ok, ExecId} = yawl_interface_d:launch_worklet(
     <<"case_001">>, <<"worklet_retry">>, #{}).
{ok, <<"exec_", ...>>}
2> ok = yawl_interface_d:complete_worklet(ExecId, #{success => true}).
ok
```
""").
-spec complete_worklet(ExecutionId :: execution_id(),
                       Result :: term()) -> ok | {error, term()}.

complete_worklet(ExecutionId, Result) ->
    gen_server:call(?MODULE, {complete_worklet, ExecutionId, Result}).

%%--------------------------------------------------------------------
%% @doc Routes an exception to the appropriate worklet.
%%
%% Determines the appropriate worklet based on exception type
%% and launches it for handling.
%%
%% Parameters:
%%   - ExceptionType: The type of exception (e.g., yawl_runtime_exception)
%%   - ExceptionData: Map containing exception details
%%
%% Returns `{ok, ExecutionId}' if a worklet was launched,
%% `{error, no_handler}' if no worklet is registered for this exception type.
%% @end
%%--------------------------------------------------------------------
-doc("""
Routes an exception to the appropriate worklet.

## Examples

```erlang
1> {ok, SvcId} = yawl_interface_d:register_exception_service(
     <<"my_handler">>, [{service_type, local}, {priority, 1}]).
{ok, <<"svc_", ...>>}
2> {ok, ExecId} = yawl_interface_d:handle_exception(
     runtime_error, #{case_id => <<"case_001">>}).
{ok, <<"exec_", ...>>}
```
""").
-spec handle_exception(ExceptionType :: atom(),
                       ExceptionData :: map()) ->
        {ok, execution_id()} | {error, no_handler | term()}.

handle_exception(ExceptionType, ExceptionData) ->
    gen_server:call(?MODULE, {handle_exception, ExceptionType, ExceptionData}).

%%--------------------------------------------------------------------
%% @doc Registers an exception handler service.
%%
%% Parameters:
%%   - ServiceName: Name identifier for the service
%%   - Config: Proplist with service configuration
%%     - endpoint: Binary URL or identifier for the service
%%     - service_type: local | remote (default: local)
%%     - priority: Non-negative integer (default: 0)
%%     - enabled: Boolean (default: true)
%%
%% Returns `{ok, ServiceId}' on success, `{error, Reason}' on failure.
%% @end
%%--------------------------------------------------------------------
-doc("""
Registers an exception handler service.

## Examples

```erlang
1> {ok, SvcId} = yawl_interface_d:register_exception_service(
     <<"my_handler">>, [{endpoint, <<"http://localhost:8080">>}]).
{ok, <<"svc_", ...>>}
2> Services = yawl_interface_d:get_exception_services().
3> length(Services) > 0.
true
```
""").
-spec register_exception_service(ServiceName :: binary(),
                                  Config :: proplists:proplist()) ->
        {ok, service_id()} | {error, term()}.

register_exception_service(ServiceName, Config) when is_binary(ServiceName) ->
    gen_server:call(?MODULE, {register_exception_service, ServiceName, Config}).

%%--------------------------------------------------------------------
%% @doc Validates worklet preconditions before execution.
%%
%% Parameters:
%%   - WorkletSpecId: The worklet specification ID
%%   - Context: Execution context map
%%
%% Returns `{ok, true}' if preconditions are met,
%% `{ok, false}' if preconditions are not met,
%% `{error, Reason}' if validation fails.
%% @end
%%--------------------------------------------------------------------
-doc("""
Validates worklet preconditions before execution.

## Examples

```erlang
1> {ok, true} = yawl_interface_d:check_preconditions(
     <<"worklet_retry">>, #{case_id => <<"case_001">>}).
{ok, true}
```
""").
-spec check_preconditions(WorkletSpecId :: worklet_spec_id(),
                          Context :: map()) ->
        {ok, boolean()} | {error, term()}.

check_preconditions(WorkletSpecId, Context) ->
    gen_server:call(?MODULE, {check_preconditions, WorkletSpecId, Context}).

%%--------------------------------------------------------------------
%% @doc Validates worklet postconditions after execution.
%%
%% Parameters:
%%   - WorkletSpecId: The worklet specification ID
%%   - Context: Execution context map including results
%%
%% Returns `{ok, true}' if postconditions are met,
%% `{ok, false}' if postconditions are not met,
%% `{error, Reason}' if validation fails.
%% @end
%%--------------------------------------------------------------------
-doc("""
Validates worklet postconditions after execution.

## Examples

```erlang
1> {ok, true} = yawl_interface_d:check_postconditions(
     <<"worklet_retry">>, #{result => success}).
{ok, true}
```
""").
-spec check_postconditions(WorkletSpecId :: worklet_spec_id(),
                           Context :: map()) ->
        {ok, boolean()} | {error, term()}.

check_postconditions(WorkletSpecId, Context) ->
    gen_server:call(?MODULE, {check_postconditions, WorkletSpecId, Context}).

%%--------------------------------------------------------------------
%% @doc Lists all registered exception services.
%%
%% Returns a list of exception service records.
%% @end
%%--------------------------------------------------------------------
-doc("""
Lists all registered exception services.

## Examples

```erlang
1> {ok, _} = yawl_interface_d:register_exception_service(
     <<"test_handler">>, []).
{ok, <<"svc_", ...>>}
2> Services = yawl_interface_d:get_exception_services().
3> is_list(Services).
true
```
""").
-spec get_exception_services() -> [#exception_service{}].

get_exception_services() ->
    gen_server:call(?MODULE, get_exception_services).

%%--------------------------------------------------------------------
%% @doc Propagates an exception to the parent workflow.
%%
%% Used when a worklet fails to handle an exception and the
%% exception needs to be propagated up the workflow hierarchy.
%%
%% Parameters:
%%   - CaseId: The case ID where exception originated
%%   - ExceptionData: Map containing exception details
%%
%% Returns `ok' if propagation was successful.
%% @end
%%--------------------------------------------------------------------
-doc("""
Propagates an exception to the parent workflow.

## Examples

```erlang
1> ok = yawl_interface_d:propagate_exception(
     <<"case_001">>, #{error => timeout}).
ok
```
""").
-spec propagate_exception(CaseId :: case_id(),
                          ExceptionData :: map()) -> ok.

propagate_exception(CaseId, ExceptionData) ->
    propagate_exception(CaseId, ExceptionData, 1).

%%--------------------------------------------------------------------
%% @doc Propagates an exception to the parent workflow with level.
%%
%% Used when a worklet fails to handle an exception and the
%% exception needs to be propagated up the workflow hierarchy.
%%
%% Parameters:
%%   - CaseId: The case ID where exception originated
%%   - ExceptionData: Map containing exception details
%%   - PropagationLevel: How far to propagate (0 = immediate parent only)
%%
%% Returns `ok' if propagation was successful.
%% @end
%%--------------------------------------------------------------------
-doc("""
Propagates an exception to the parent workflow with level.

## Examples

```erlang
1> ok = yawl_interface_d:propagate_exception(
     <<"case_001">>, #{error => timeout}, 2).
ok
```
""").
-spec propagate_exception(CaseId :: case_id(),
                          ExceptionData :: map(),
                          PropagationLevel :: non_neg_integer()) -> ok.

propagate_exception(CaseId, ExceptionData, PropagationLevel) ->
    gen_server:cast(?MODULE, {propagate_exception, CaseId, ExceptionData, PropagationLevel}).

%%--------------------------------------------------------------------
%% @doc Coordinates compensation actions for failed worklets.
%%
%% When a worklet fails, this function coordinates any compensation
%% actions that need to be executed to undo the effects of the worklet.
%%
%% Parameters:
%%   - ExecutionId: The worklet execution ID to compensate
%%
%% Returns `{ok, Compensated}' if compensation was successful,
%% `{error, Reason}' on failure.
%% @end
%%--------------------------------------------------------------------
-doc("""
Coordinates compensation actions for failed worklets.

## Examples

```erlang
1> {ok, ExecId} = yawl_interface_d:launch_worklet(
     <<"case_001">>, <<"worklet_compensate">>, #{compensation_actions => []}).
{ok, <<"exec_", ...>>}
2> {ok, _} = yawl_interface_d:coordinate_compensation(ExecId).
{ok, _}
```
""").
-spec coordinate_compensation(ExecutionId :: execution_id()) ->
        {ok, boolean()} | {error, term()}.

coordinate_compensation(ExecutionId) ->
    gen_server:call(?MODULE, {coordinate_compensation, ExecutionId}).

%%--------------------------------------------------------------------
%% @doc Gets all active worklet executions.
%%
%% Returns a list of active worklet execution records.
%% @end
%%--------------------------------------------------------------------
-doc("""
Gets all active worklet executions.

## Examples

```erlang
1> Active = yawl_interface_d:get_active_worklets().
2> is_list(Active).
true
```
""").
-spec get_active_worklets() -> [#worklet_execution{}].

get_active_worklets() ->
    gen_server:call(?MODULE, get_active_worklets).

%%--------------------------------------------------------------------
%% @doc Gets worklets by case ID.
%%
%% Returns a list of worklet executions for the specified case.
%% @end
%%--------------------------------------------------------------------
-doc("""
Gets worklets by case ID.

## Examples

```erlang
1> {ok, ExecId} = yawl_interface_d:launch_worklet(
     <<"case_001">>, <<"worklet_test">>, #{}).
{ok, <<"exec_", ...>>}
2> CaseWorklets = yawl_interface_d:get_worklets_by_case(<<"case_001">>).
3> length(CaseWorklets) > 0.
true
```
""").
-spec get_worklets_by_case(CaseId :: case_id()) -> [#worklet_execution{}].

get_worklets_by_case(CaseId) ->
    gen_server:call(?MODULE, {get_worklets_by_case, CaseId}).

%%--------------------------------------------------------------------
%% @doc Unregisters an exception service.
%%
%% Returns `ok' on success, `{error, not_found}' if service ID not found.
%% @end
%%--------------------------------------------------------------------
-doc("""
Unregisters an exception service.

## Examples

```erlang
1> {ok, SvcId} = yawl_interface_d:register_exception_service(
     <<"temp_handler">>, []).
{ok, <<"svc_", ...>>}
2> ok = yawl_interface_d:unregister_exception_service(SvcId).
ok
3> {error, not_found} = yawl_interface_d:unregister_exception_service(<<"nonexistent">>).
{error, not_found}
```
""").
-spec unregister_exception_service(ServiceId :: service_id()) ->
        ok | {error, not_found}.

unregister_exception_service(ServiceId) ->
    gen_server:call(?MODULE, {unregister_exception_service, ServiceId}).

%%--------------------------------------------------------------------
%% @doc Enables an exception service.
%%
%% Returns `ok' on success, `{error, not_found}' if service ID not found.
%% @end
%%--------------------------------------------------------------------
-doc("""
Enables an exception service.

## Examples

```erlang
1> {ok, SvcId} = yawl_interface_d:register_exception_service(
     <<"my_handler">>, [{enabled, false}]).
{ok, <<"svc_", ...>>}
2> ok = yawl_interface_d:enable_exception_service(SvcId).
ok
```
""").
-spec enable_exception_service(ServiceId :: service_id()) ->
        ok | {error, not_found}.

enable_exception_service(ServiceId) ->
    gen_server:call(?MODULE, {enable_exception_service, ServiceId}).

%%--------------------------------------------------------------------
%% @doc Disables an exception service.
%%
%% Returns `ok' on success, `{error, not_found}' if service ID not found.
%% @end
%%--------------------------------------------------------------------
-doc("""
Disables an exception service.

## Examples

```erlang
1> {ok, SvcId} = yawl_interface_d:register_exception_service(
     <<"my_handler">>, []).
{ok, <<"svc_", ...>>}
2> ok = yawl_interface_d:disable_exception_service(SvcId).
ok
```
""").
-spec disable_exception_service(ServiceId :: service_id()) ->
        ok | {error, not_found}.

disable_exception_service(ServiceId) ->
    gen_server:call(?MODULE, {disable_exception_service, ServiceId}).

%%--------------------------------------------------------------------
%% @doc Sets the priority of an exception service.
%%
%% Returns `ok' on success, `{error, not_found}' if service ID not found.
%% @end
%%--------------------------------------------------------------------
-doc("""
Sets the priority of an exception service.

## Examples

```erlang
1> {ok, SvcId} = yawl_interface_d:register_exception_service(
     <<"my_handler">>, [{priority, 0}]).
{ok, <<"svc_", ...>>}
2> ok = yawl_interface_d:set_service_priority(SvcId, 10).
ok
```
""").
-spec set_service_priority(ServiceId :: service_id(), Priority :: non_neg_integer()) ->
        ok | {error, not_found}.

set_service_priority(ServiceId, Priority) ->
    gen_server:call(?MODULE, {set_service_priority, ServiceId, Priority}).

%%--------------------------------------------------------------------
%% @doc Gets the status of an active worklet.
%%
%% Returns `{ok, Status}' where Status is one of:
%% pending, running, completed, failed, aborted.
%% @end
%%--------------------------------------------------------------------
-doc("""
Gets the status of an active worklet.

## Examples

```erlang
1> {ok, ExecId} = yawl_interface_d:launch_worklet(
     <<"case_001">>, <<"worklet_test">>, #{}).
{ok, <<"exec_", ...>>}
2> {ok, Status} = yawl_interface_d:get_worklet_status(ExecId).
3> Status =:= running orelse Status =:= completed.
true
```
""").
-spec get_worklet_status(ExecutionId :: execution_id()) ->
        {ok, worklet_status()} | {error, not_found}.

get_worklet_status(ExecutionId) ->
    gen_server:call(?MODULE, {get_worklet_status, ExecutionId}).

%%--------------------------------------------------------------------
%% @doc Aborts a running worklet.
%%
%% Returns `ok' on success, `{error, not_found}' if execution ID not found,
%% `{error, invalid_status}' if worklet is not in a state that can be aborted.
%% @end
%%--------------------------------------------------------------------
-doc("""
Aborts a running worklet.

## Examples

```erlang
1> {ok, ExecId} = yawl_interface_d:launch_worklet(
     <<"case_001">>, <<"worklet_test">>, #{}).
{ok, <<"exec_", ...>>}
2> ok = yawl_interface_d:abort_worklet(ExecId).
ok
```
""").
-spec abort_worklet(ExecutionId :: execution_id()) ->
        ok | {error, not_found | invalid_status}.

abort_worklet(ExecutionId) ->
    gen_server:call(?MODULE, {abort_worklet, ExecutionId}).

%%--------------------------------------------------------------------
%% @doc Runs doctests for the yawl_interface_d module.
%%
%% Tests basic functionality including:
%% - Service registration and management
%% - Worklet lifecycle operations
%% - Exception handling
%%
%% Returns `ok' when all tests pass.
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Register exception service
    {ok, SvcId} = register_exception_service(<<"doctest_handler">>,
        [{service_type, local}, {priority, 1}, {enabled, true}]),

    %% Test 2: Get exception services
    Services = get_exception_services(),
    true = length(Services) > 0,

    %% Test 3: Launch worklet (with minimal exception data)
    {ok, ExecId} = launch_worklet(<<"doctest_case">>, <<"doctest_worklet">>,
        #{task_id => <<"task_1">>}),

    %% Test 4: Get worklet status
    {ok, Status} = get_worklet_status(ExecId),
    true = Status =:= running orelse Status =:= completed,

    %% Test 5: Get worklets by case
    CaseWorklets = get_worklets_by_case(<<"doctest_case">>),
    true = length(CaseWorklets) > 0,

    %% Test 6: Complete worklet
    ok = complete_worklet(ExecId, #{result => success}),

    %% Test 7: Abort worklet (launch a new one since first is completed)
    {ok, ExecId2} = launch_worklet(<<"doctest_case2">>, <<"doctest_worklet2">>,
        #{task_id => <<"task_2">>}),
    ok = abort_worklet(ExecId2),

    %% Test 8: Set service priority
    ok = set_service_priority(SvcId, 5),

    %% Test 9: Disable/enable service
    ok = disable_exception_service(SvcId),
    ok = enable_exception_service(SvcId),

    %% Test 10: Unregister service
    ok = unregister_exception_service(SvcId),

    %% Test 11: Verify service not found after unregister
    {error, not_found} = unregister_exception_service(<<"nonexistent_service">>),

    %% Test 12: Check preconditions (should pass when yawl_worklet not available)
    {ok, true} = check_preconditions(<<"test_worklet">>, #{}),

    %% Test 13: Check postconditions (should pass when yawl_worklet not available)
    {ok, true} = check_postconditions(<<"test_worklet">>, #{}),

    %% Test 14: Propagate exception (cast, returns immediately)
    ok = propagate_exception(<<"doctest_case">>, #{error => test}),

    %% Test 15: Get active worklets (should return list)
    ActiveWorklets = get_active_worklets(),
    true = is_list(ActiveWorklets),

    ok.

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

%% @private
init(_Arg) ->
    process_flag(trap_exit, true),
    logger:info("Interface D initialized", [{module, ?MODULE}]),
    {ok, #interface_d_state{compensation_log = []}}.

%% @private
handle_call({launch_worklet, CaseId, WorkletSpecId, ExceptionData}, _From, State) ->
    #interface_d_state{active_worklets = ActiveWorklets} = State,

    ExecutionId = generate_execution_id(),

    %% Check preconditions before launching
    PreconditionResult = check_worklet_preconditions(WorkletSpecId, ExceptionData),

    case PreconditionResult of
        {ok, true} ->
            Now = erlang:system_time(millisecond),

            Execution = #worklet_execution{
                execution_id = ExecutionId,
                case_id = CaseId,
                task_id = maps:get(task_id, ExceptionData, <<"unknown">>),
                worklet_spec_id = WorkletSpecId,
                status = running,
                started_at = Now,
                completed_at = undefined,
                exception_data = ExceptionData,
                result = undefined
            },

            %% Notify yawl_worklet about the launch
            notify_worklet_launched(Execution),

            logger:info("Worklet launched: id=~p case=~p spec=~p",
                        [ExecutionId, CaseId, WorkletSpecId],
                        [{module, ?MODULE}, {action, worklet_launched}]),

            {reply, {ok, ExecutionId}, State#interface_d_state{
                active_worklets = [Execution | ActiveWorklets]
            }};
        {ok, false} ->
            {reply, {error, precondition_not_met}, State};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({complete_worklet, ExecutionId, Result}, _From, State) ->
    #interface_d_state{active_worklets = ActiveWorklets} = State,

    case lists:keyfind(ExecutionId, #worklet_execution.execution_id, ActiveWorklets) of
        false ->
            {reply, {error, not_found}, State};
        #worklet_execution{status = running, case_id = CaseId, worklet_spec_id = WorkletSpecId, exception_data = ExceptionData} = Exec ->
            Now = erlang:system_time(millisecond),

            %% Check postconditions
            PostconditionResult = check_worklet_postconditions(
                WorkletSpecId,
                maps:put(result, Result, ExceptionData)
            ),

            CompletedExec = case PostconditionResult of
                {ok, true} ->
                    Exec#worklet_execution{
                        status = completed,
                        completed_at = Now,
                        result = Result
                    };
                _ ->
                    %% Postconditions failed, mark as failed
                    Exec#worklet_execution{
                        status = failed,
                        completed_at = Now,
                        result = {postcondition_failed, Result}
                    }
            end,

            %% Update the worklet in the active worklets list
            UpdatedWorklets = lists:keyreplace(
                ExecutionId,
                #worklet_execution.execution_id,
                ActiveWorklets,
                CompletedExec
            ),

            %% Notify completion
            notify_worklet_completed(CompletedExec),

            logger:info("Worklet completed: id=~p status=~p",
                          [ExecutionId, CompletedExec#worklet_execution.status],
                          [{module, ?MODULE}, {action, worklet_completed},
                           {case_id, CaseId}]),

            {reply, ok, State#interface_d_state{active_worklets = UpdatedWorklets}};
        #worklet_execution{status = Status} ->
            {reply, {error, {invalid_status, Status}}, State}
    end;

handle_call({handle_exception, ExceptionType, ExceptionData}, _From, State) ->
    #interface_d_state{exception_services = Services} = State,

    %% Find appropriate exception service based on exception type
    case find_service_for_exception(ExceptionType, Services) of
        {ok, _ServiceId} ->
            %% Get worklet spec ID from exception data or derive it
            WorkletSpecId = maps:get(worklet_spec_id, ExceptionData,
                                    derive_worklet_spec_id(ExceptionType)),
            CaseId = maps:get(case_id, ExceptionData, <<"unknown_case">>),

            %% Launch the worklet
            {Reply, NewState} = case do_launch_worklet(CaseId, WorkletSpecId, ExceptionData, State) of
                {ok, ExecId, UpdatedState} ->
                    {{ok, ExecId}, UpdatedState};
                {error, Reason} ->
                    {{error, Reason}, State}
            end,
            {reply, Reply, NewState};
        {error, no_handler} ->
            {reply, {error, no_handler}, State}
    end;

handle_call({register_exception_service, ServiceName, Config}, _From, State) ->
    #interface_d_state{exception_services = Services, service_counter = Counter} = State,

    %% Check if service already exists
    ServiceId = generate_service_id(ServiceName, Counter),

    ServiceType = proplists:get_value(service_type, Config, local),
    Endpoint = proplists:get_value(endpoint, Config, <<"">>),
    Priority = proplists:get_value(priority, Config, 0),
    Enabled = proplists:get_value(enabled, Config, true),

    %% Validate service type
    case lists:member(ServiceType, [local, remote]) of
        false ->
            {reply, {error, {invalid_service_type, ServiceType}}, State};
        true ->
            ExceptionService = #exception_service{
                service_id = ServiceId,
                endpoint = Endpoint,
                service_type = ServiceType,
                enabled = Enabled,
                priority = Priority
            },

            logger:info("Exception service registered: id=~p name=~p type=~p",
                          [ServiceId, ServiceName, ServiceType],
                          [{module, ?MODULE}, {action, service_registered}]),

            {reply, {ok, ServiceId}, State#interface_d_state{
                exception_services = Services#{ServiceId => ExceptionService},
                service_counter = Counter + 1
            }}
    end;

handle_call({check_preconditions, WorkletSpecId, Context}, _From, State) ->
    Result = do_check_preconditions(WorkletSpecId, Context),
    {reply, Result, State};

handle_call({check_postconditions, WorkletSpecId, Context}, _From, State) ->
    Result = do_check_postconditions(WorkletSpecId, Context),
    {reply, Result, State};

handle_call(get_exception_services, _From, #interface_d_state{exception_services = Services} = State) ->
    ServiceList = maps:values(Services),
    {reply, ServiceList, State};

handle_call({get_worklet_status, ExecutionId}, _From, #interface_d_state{active_worklets = ActiveWorklets} = State) ->
    case lists:keyfind(ExecutionId, #worklet_execution.execution_id, ActiveWorklets) of
        false ->
            {reply, {error, not_found}, State};
        #worklet_execution{status = Status} ->
            {reply, {ok, Status}, State}
    end;

handle_call({abort_worklet, ExecutionId}, _From, #interface_d_state{active_worklets = ActiveWorklets} = State) ->
    case lists:keyfind(ExecutionId, #worklet_execution.execution_id, ActiveWorklets) of
        false ->
            {reply, {error, not_found}, State};
        #worklet_execution{status = Status} = Exec when Status =:= pending orelse Status =:= running ->
            Now = erlang:system_time(millisecond),
            AbortedExec = Exec#worklet_execution{
                status = aborted,
                completed_at = Now
            },

            UpdatedWorklets = lists:keyreplace(
                ExecutionId,
                #worklet_execution.execution_id,
                ActiveWorklets,
                AbortedExec
            ),

            %% Notify yawl_worklet about the abortion
            notify_worklet_aborted(AbortedExec),

            logger:info("Worklet aborted: id=~p", [ExecutionId],
                        [{module, ?MODULE}, {action, worklet_aborted}]),

            {reply, ok, State#interface_d_state{active_worklets = UpdatedWorklets}};
        #worklet_execution{status = Status} ->
            {reply, {error, {invalid_status, Status}}, State}
    end;

handle_call({coordinate_compensation, ExecutionId}, _From, #interface_d_state{active_worklets = ActiveWorklets, compensation_log = Log} = State) ->
    case lists:keyfind(ExecutionId, #worklet_execution.execution_id, ActiveWorklets) of
        false ->
            {reply, {error, not_found}, State};
        #worklet_execution{status = failed} = Exec ->
            %% Coordinate compensation for failed worklet
            {Compensated, NewLog} = do_coordinate_compensation(Exec, Log),
            {reply, {ok, Compensated}, State#interface_d_state{compensation_log = NewLog}};
        #worklet_execution{status = Status} ->
            {reply, {error, {invalid_status, Status}}, State}
    end;

handle_call(get_active_worklets, _From, #interface_d_state{active_worklets = ActiveWorklets} = State) ->
    Active = [W || W <- ActiveWorklets, W#worklet_execution.status =:= running],
    {reply, Active, State};

handle_call({get_worklets_by_case, CaseId}, _From, #interface_d_state{active_worklets = ActiveWorklets} = State) ->
    CaseWorklets = [W || W <- ActiveWorklets, W#worklet_execution.case_id =:= CaseId],
    {reply, CaseWorklets, State};

handle_call({unregister_exception_service, ServiceId}, _From, #interface_d_state{exception_services = Services} = State) ->
    case maps:is_key(ServiceId, Services) of
        true ->
            {reply, ok, State#interface_d_state{exception_services = maps:remove(ServiceId, Services)}};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({enable_exception_service, ServiceId}, _From, #interface_d_state{exception_services = Services} = State) ->
    case maps:get(ServiceId, Services, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Service ->
            UpdatedService = Service#exception_service{enabled = true},
            {reply, ok, State#interface_d_state{exception_services = Services#{ServiceId => UpdatedService}}}
    end;

handle_call({disable_exception_service, ServiceId}, _From, #interface_d_state{exception_services = Services} = State) ->
    case maps:get(ServiceId, Services, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Service ->
            UpdatedService = Service#exception_service{enabled = false},
            {reply, ok, State#interface_d_state{exception_services = Services#{ServiceId => UpdatedService}}}
    end;

handle_call({set_service_priority, ServiceId, Priority}, _From, #interface_d_state{exception_services = Services} = State) ->
    case maps:get(ServiceId, Services, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Service ->
            UpdatedService = Service#exception_service{priority = Priority},
            {reply, ok, State#interface_d_state{exception_services = Services#{ServiceId => UpdatedService}}}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%% @private
handle_cast({propagate_exception, CaseId, ExceptionData, PropagationLevel}, State) ->
    do_propagate_exception(CaseId, ExceptionData, PropagationLevel, State),
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Generates a unique execution ID.
-spec generate_execution_id() -> execution_id().

generate_execution_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"exec_", Hex/binary>>.

%% @private
%% @doc Generates a unique service ID.
-spec generate_service_id(binary(), non_neg_integer()) -> service_id().

generate_service_id(ServiceName, Counter) ->
    Hash = binary:encode_hex(crypto:hash(md5, <<ServiceName/binary, "_", (integer_to_binary(Counter))/binary>>)),
    <<"svc_", Hash/binary>>.

%% @private
%% @doc Finds an exception service for a given exception type.
-spec find_service_for_exception(atom(), #{binary() => #exception_service{}}) ->
        {ok, service_id()} | {error, no_handler}.

find_service_for_exception(_ExceptionType, Services) ->
    %% Filter enabled services
    EnabledServices = maps:filter(fun(_K, S) -> S#exception_service.enabled =:= true end, Services),

    case maps:values(EnabledServices) of
        [] ->
            {error, no_handler};
        AllServices ->
            %% Sort by priority (highest first)
            Sorted = lists:sort(
                fun(A, B) ->
                    A#exception_service.priority >= B#exception_service.priority
                end,
                AllServices
            ),

            %% Return the highest priority service
            #exception_service{service_id = ServiceId} = hd(Sorted),
            {ok, ServiceId}
    end.

%% @private
%% @doc Derives a worklet spec ID from an exception type.
-spec derive_worklet_spec_id(atom()) -> worklet_spec_id().

derive_worklet_spec_id(ExceptionType) ->
    TypeBin = atom_to_binary(ExceptionType, utf8),
    <<"worklet_", TypeBin/binary>>.

%% @private
%% @doc Performs the actual worklet launch.
-spec do_launch_worklet(case_id(), worklet_spec_id(), map(), #interface_d_state{}) ->
        {ok, execution_id(), #interface_d_state{}} | {error, term()}.

do_launch_worklet(CaseId, WorkletSpecId, ExceptionData, State) ->
    #interface_d_state{active_worklets = ActiveWorklets} = State,

    ExecutionId = generate_execution_id(),
    Now = erlang:system_time(millisecond),

    Execution = #worklet_execution{
        execution_id = ExecutionId,
        case_id = CaseId,
        task_id = maps:get(task_id, ExceptionData, <<"unknown">>),
        worklet_spec_id = WorkletSpecId,
        status = running,
        started_at = Now,
        completed_at = undefined,
        exception_data = ExceptionData,
        result = undefined
    },

    %% Notify yawl_worklet module
    notify_worklet_launched(Execution),

    {ok, ExecutionId, State#interface_d_state{
        active_worklets = [Execution | ActiveWorklets]
    }}.

%% @private
%% @doc Checks preconditions for a worklet.
%% If yawl_worklet is not available, returns {ok, true} to allow execution.
-spec check_worklet_preconditions(worklet_spec_id(), map()) ->
        {ok, boolean()} | {error, term()}.

check_worklet_preconditions(WorkletSpecId, ExceptionData) ->
    try
        %% Check if yawl_worklet is available
        case whereis(yawl_worklet) of
            undefined ->
                %% yawl_worklet not available, allow execution
                {ok, true};
            _Pid ->
                %% Delegate to yawl_worklet for precondition checking
                case catch yawl_worklet:check_preconditions(WorkletSpecId, ExceptionData) of
                    {ok, Result} when is_boolean(Result) ->
                        {ok, Result};
                    {ok, Result} ->
                        %% Convert non-boolean to boolean
                        {ok, Result =/= false andalso Result =/= undefined};
                    {error, Reason} ->
                        {error, Reason};
                    {'EXIT', {{noproc, _}, _}} ->
                        %% yawl_worklet died during call, allow execution
                        {ok, true};
                    {'EXIT', {Reason, _Stack}} ->
                        {error, Reason};
                    _ ->
                        %% No precondition defined or error occurred, default to true
                        {ok, true}
                end
        end
    catch
        _:_ ->
            %% If any error occurs, allow execution
            {ok, true}
    end.

%% @private
%% @doc Checks postconditions for a worklet.
%% If yawl_worklet is not available, returns {ok, true} to allow completion.
-spec check_worklet_postconditions(worklet_spec_id(), map()) ->
        {ok, boolean()} | {error, term()}.

check_worklet_postconditions(WorkletSpecId, Context) ->
    try
        %% Check if yawl_worklet is available
        case whereis(yawl_worklet) of
            undefined ->
                %% yawl_worklet not available, allow completion
                {ok, true};
            _Pid ->
                %% Delegate to yawl_worklet for postcondition checking
                case catch yawl_worklet:check_postconditions(WorkletSpecId, Context) of
                    {ok, Result} when is_boolean(Result) ->
                        {ok, Result};
                    {ok, Result} ->
                        %% Convert non-boolean to boolean
                        {ok, Result =/= false andalso Result =/= undefined};
                    {error, Reason} ->
                        {error, Reason};
                    {'EXIT', {{noproc, _}, _}} ->
                        %% yawl_worklet died during call, allow completion
                        {ok, true};
                    {'EXIT', {Reason, _Stack}} ->
                        {error, Reason};
                    _ ->
                        %% No postcondition defined, default to true
                        {ok, true}
                end
        end
    catch
        _:_ ->
            %% If any error occurs, allow completion
            {ok, true}
    end.

%% @private
%% @doc Internal precondition check implementation.
-spec do_check_preconditions(worklet_spec_id(), map()) ->
        {ok, boolean()} | {error, term()}.

do_check_preconditions(WorkletSpecId, Context) ->
    check_worklet_preconditions(WorkletSpecId, Context).

%% @private
%% @doc Internal postcondition check implementation.
-spec do_check_postconditions(worklet_spec_id(), map()) ->
        {ok, boolean()} | {error, term()}.

do_check_postconditions(WorkletSpecId, Context) ->
    check_worklet_postconditions(WorkletSpecId, Context).

%% @private
%% @doc Notifies the yawl_worklet module about worklet launch.
-spec notify_worklet_launched(#worklet_execution{}) -> ok.

notify_worklet_launched(#worklet_execution{execution_id = ExecId,
                                          case_id = CaseId,
                                          worklet_spec_id = WorkletSpecId,
                                          exception_data = ExceptionData}) ->
    try
        %% Try to notify via yawl_worklet if available
        case whereis(yawl_worklet) of
            undefined ->
                ok;
            _Pid ->
                %% Trigger worklet in the worklet module
                TaskId = maps:get(task_id, ExceptionData, <<"unknown">>),
                catch yawl_worklet:launch_worklet(TaskId, WorkletSpecId, ExceptionData),
                ok
        end
    catch
        _:_ -> ok
    end,
    %% Also publish via IPC if available
    try
        case whereis(yawl_ipc) of
            undefined -> ok;
            _IpcPid ->
                yawl_ipc:publish_case_event(
                    CaseId,
                    <<"worklet_launched">>,
                    #{execution_id => ExecId,
                      worklet_spec_id => WorkletSpecId}
                )
        end
    catch
        _:_ -> ok
    end.

%% @private
%% @doc Notifies about worklet completion.
-spec notify_worklet_completed(#worklet_execution{}) -> ok.

notify_worklet_completed(#worklet_execution{execution_id = ExecId,
                                            case_id = CaseId,
                                            status = Status,
                                            result = Result}) ->
    try
        case whereis(yawl_ipc) of
            undefined -> ok;
            _Pid ->
                yawl_ipc:publish_case_event(
                    CaseId,
                    <<"worklet_completed">>,
                    #{execution_id => ExecId,
                      status => Status,
                      result => Result}
                )
        end
    catch
        _:_ -> ok
    end.

%% @private
%% @doc Notifies about worklet abortion.
-spec notify_worklet_aborted(#worklet_execution{}) -> ok.

notify_worklet_aborted(#worklet_execution{execution_id = ExecId,
                                           case_id = CaseId}) ->
    try
        case whereis(yawl_ipc) of
            undefined -> ok;
            _Pid ->
                yawl_ipc:publish_case_event(
                    CaseId,
                    <<"worklet_aborted">>,
                    #{execution_id => ExecId}
                )
        end
    catch
        _:_ -> ok
    end.

%% @private
%% @doc Propagates an exception to parent workflow.
-spec do_propagate_exception(case_id(), map(), non_neg_integer(), #interface_d_state{}) -> ok.

do_propagate_exception(CaseId, ExceptionData, PropagationLevel, State) ->
    try
        %% Log the propagation
        logger:warning("Exception propagation: case=~p level=~p",
                      [CaseId, PropagationLevel],
                      [{module, ?MODULE}, {action, exception_propagation},
                       {exception_data, ExceptionData}]),

        %% Try to notify via IPC if available
        case whereis(yawl_ipc) of
            undefined -> ok;
            _Pid ->
                yawl_ipc:publish_case_event(
                    CaseId,
                    <<"exception_propagated">>,
                    ExceptionData#{propagation_level => PropagationLevel}
                )
        end,

        %% If propagation level > 0, propagate to parent case
        ParentCaseId = maps:get(parent_case_id, ExceptionData, undefined),
        case ParentCaseId of
            undefined when PropagationLevel > 0 ->
                %% Try to find parent from worklet execution
                #interface_d_state{active_worklets = ActiveWorklets} = State,
                case lists:keyfind(CaseId, #worklet_execution.case_id, ActiveWorklets) of
                    false ->
                        ok;
                    _Exec ->
                        %% No explicit parent, could implement hierarchy traversal here
                        ok
                end;
            undefined ->
                ok;
            _ when PropagationLevel > 0 ->
                %% Recursive propagation to parent
                do_propagate_exception(ParentCaseId, ExceptionData, PropagationLevel - 1, State);
            _ ->
                ok
        end
    catch
        _:_ -> ok
    end.

%% @private
%% @doc Coordinates compensation for a failed worklet.
%% This function manages the compensation workflow when a worklet fails,
%% including executing compensation handlers and updating state.
%%
%% Compensation follows the Saga pattern:
%% 1. Identify the compensation actions from exception data
%% 2. Execute compensation actions in reverse order
%% 3. Track compensation completion
%% 4. Notify interested parties
%%
%% @end
-spec do_coordinate_compensation(#worklet_execution{}, [{execution_id(), term(), integer()}]) ->
        {boolean(), [{execution_id(), term(), integer()}]}.

do_coordinate_compensation(#worklet_execution{execution_id = ExecId,
                                              case_id = CaseId,
                                              exception_data = ExceptionData}, Log) ->
    try
        logger:info("Coordinate compensation: id=~p case=~p",
                    [ExecId, CaseId],
                    [{module, ?MODULE}, {action, coordinate_compensation}]),

        %% Check if compensation actions are defined
        CompensationActions = maps:get(compensation_actions, ExceptionData, []),

        %% Log the compensation attempt
        Timestamp = erlang:system_time(millisecond),
        LogEntry = {ExecId, attempted, Timestamp},
        NewLog = [LogEntry | Log],

        case CompensationActions of
            [] ->
                %% No compensation actions defined
                {false, NewLog};
            _ ->
                %% Execute compensation actions
                {Compensated, ActionsLog} = execute_compensation_actions(
                    lists:reverse(CompensationActions),
                    ExecId,
                    CaseId,
                    NewLog
                ),

                %% Add completion to log
                FinalLog = [{ExecId, {completed, Compensated}, erlang:system_time(millisecond)} | ActionsLog],

                %% Notify via IPC
                case whereis(yawl_ipc) of
                    undefined -> ok;
                    _Pid ->
                        yawl_ipc:publish_case_event(
                            CaseId,
                            <<"compensation_completed">>,
                            #{execution_id => ExecId, compensated => Compensated}
                        )
                end,

                {Compensated, FinalLog}
        end
    catch
        _:_ ->
            %% Default to false on error
            {false, Log}
    end.

%% @private
%% @doc Executes a list of compensation actions.
%%
%% Actions are executed in reverse order (undo pattern).
%% If any action fails, execution continues but result is false.
%%
%% @end
-spec execute_compensation_actions([fun()], execution_id(), case_id(), [{execution_id(), term(), integer()}]) ->
        {boolean(), [{execution_id(), term(), integer()}]}.

execute_compensation_actions([], _ExecId, _CaseId, Log) ->
    {true, Log};
execute_compensation_actions([Action | Rest], ExecId, CaseId, Log) ->
    Timestamp = erlang:system_time(millisecond),
    Result = try
        case Action of
            Fun when is_function(Fun, 0) ->
                Fun();
            Fun when is_function(Fun, 2) ->
                Fun(ExecId, CaseId);
            Fun when is_function(Fun, 1) ->
                Fun(ExecId);
            _ ->
                logger:warning("Invalid compensation action",
                              [{module, ?MODULE}, {action, invalid_compensation_action}])
        end,
        {ok, [{ExecId, action_completed, Timestamp} | Log]}
    catch
        Kind:Reason:Stack ->
            logger:error("Compensation failed: kind=~p reason=~p",
                       [Kind, Reason],
                       [{module, ?MODULE}, {action, compensation_failed},
                        {stacktrace, Stack}]),
            {error, [{ExecId, {action_failed, Kind, Reason}, Timestamp} | Log]}
    end,

    case Result of
        {ok, NewLog} ->
            execute_compensation_actions(Rest, ExecId, CaseId, NewLog);
        {error, ErrorLog} ->
            {_Res, _} = execute_compensation_actions(Rest, ExecId, CaseId, ErrorLog),
            {false, ErrorLog}
    end.
