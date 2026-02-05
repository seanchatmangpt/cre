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
%% @doc YAWL Stateless Execution Mode for CRE.
%%
%% This module implements stateless execution for YAWL workflows,
%% enabling cloud-native and serverless deployment patterns.
%%
%% <h3>Key Features</h3>
%%
%% <ul>
%%   <li><b>Stateless Execution:</b> Execute workflows without maintaining server state</li>
%%   <li><b>Checkpoint/Restore:</b> Save and restore execution state for resilience</li>
%%   <li><b>External System Integration:</b> REST-like interface for workflow execution</li>
%%   <li><b>Idempotent Operations:</b> Safe for retry and distributed execution</li>
%% </ul>
%%
%% <h3>Use Cases</h3>
%%
%% <ul>
%%   <li>Serverless function execution</li>
%%   <li>Workflow as a Service (WaaS)</li>
%%   <li>Multi-region deployment</li>
%%   <li>Event-driven processing</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_stateless).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Stateless execution API
-export([start_link/0, start_link/1, stop/0]).
-export([execute_stateless/2, validate_stateless/1]).
-export([get_execution_state/1, checkpoint/2, restore/2]).
-export([list_executions/0, get_execution_result/1]).
-export([cancel_execution/1, pause_execution/1, resume_execution/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Records
%%====================================================================

-record(execution_state, {
    execution_id :: binary(),
    workflow_spec :: term(),
    status :: pending | running | paused | completed | failed | cancelled,
    current_step :: binary() | undefined,
    steps_completed = [] :: [binary()],
    steps_remaining = [] :: [binary()],
    data = #{} :: map(),
    checkpoints = [] :: [{binary(), term(), erlang:timestamp()}],
    start_time :: erlang:timestamp(),
    end_time :: erlang:timestamp() | undefined,
    error_reason :: term() | undefined
}).

-record(stateless_state, {
    executions = #{} :: #{binary() => #execution_state{}},
    checkpoint_dir = <<"/tmp/yawl_checkpoints">> :: binary(),
    max_executions = 1000 :: non_neg_integer(),
    execution_ttl = 3600000 :: non_neg_integer()  % 1 hour
}).

%%====================================================================
%% Types
%%====================================================================

-type execution_id() :: binary().
-type execution_status() :: pending | running | paused | completed | failed | cancelled.
-type execution_state() :: #execution_state{}.
-type workflow_spec() :: term().

-export_type([execution_id/0, execution_status/0, execution_state/0, workflow_spec/0]).

%%====================================================================
%% Stateless Execution API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the stateless execution service with default name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    start_link(yawl_stateless).

%%--------------------------------------------------------------------
%% @doc Starts the stateless execution service with a registered name.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.

start_link(Name) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the stateless execution service.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.

stop() ->
    gen_server:stop(yawl_stateless).

%%--------------------------------------------------------------------
%% @doc Executes a workflow in stateless mode.
%%
%% @param WorkflowSpec The workflow specification to execute.
%% @param InputData Input data for the workflow.
%% @return {ok, ExecutionId} or {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_stateless(WorkflowSpec :: workflow_spec(), InputData :: map()) ->
          {ok, execution_id()} | {error, term()}.

execute_stateless(WorkflowSpec, InputData) when is_map(InputData) ->
    gen_server:call(yawl_stateless, {execute, WorkflowSpec, InputData}).

%%--------------------------------------------------------------------
%% @doc Validates a workflow specification for stateless execution.
%%
%% @param WorkflowSpec The workflow specification to validate.
%% @return ok | {error, [binary()]}.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_stateless(WorkflowSpec :: workflow_spec()) ->
          ok | {error, [binary()]}.

validate_stateless(WorkflowSpec) ->
    gen_server:call(yawl_stateless, {validate, WorkflowSpec}).

%%--------------------------------------------------------------------
%% @doc Gets the current state of an execution.
%%
%% @param ExecutionId The execution identifier.
%% @return Execution state or {error, not_found}.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_execution_state(execution_id()) -> execution_state() | {error, not_found}.

get_execution_state(ExecutionId) when is_binary(ExecutionId) ->
    gen_server:call(yawl_stateless, {get_state, ExecutionId}).

%%--------------------------------------------------------------------
%% @doc Creates a checkpoint for an execution.
%%
%% @param ExecutionId The execution identifier.
%% @param CheckpointName Name for the checkpoint.
%% @return ok | {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-spec checkpoint(execution_id(), binary()) -> ok | {error, term()}.

checkpoint(ExecutionId, CheckpointName) when is_binary(ExecutionId), is_binary(CheckpointName) ->
    gen_server:call(yawl_stateless, {checkpoint, ExecutionId, CheckpointName}).

%%--------------------------------------------------------------------
%% @doc Restores an execution to a checkpoint.
%%
%% @param ExecutionId The execution identifier.
%% @param CheckpointName Name of the checkpoint to restore.
%% @return ok | {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-spec restore(execution_id(), binary()) -> ok | {error, term()}.

restore(ExecutionId, CheckpointName) when is_binary(ExecutionId), is_binary(CheckpointName) ->
    gen_server:call(yawl_stateless, {restore, ExecutionId, CheckpointName}).

%%--------------------------------------------------------------------
%% @doc Lists all active executions.
%%
%% @return List of execution summaries.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_executions() -> [map()].

list_executions() ->
    gen_server:call(yawl_stateless, list_executions).

%%--------------------------------------------------------------------
%% @doc Gets the result of a completed execution.
%%
%% @param ExecutionId The execution identifier.
%% @return Result map or {error, term()}.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_execution_result(execution_id()) -> map() | {error, not_found | incomplete}.

get_execution_result(ExecutionId) when is_binary(ExecutionId) ->
    gen_server:call(yawl_stateless, {get_result, ExecutionId}).

%%--------------------------------------------------------------------
%% @doc Cancels a running execution.
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_execution(execution_id()) -> ok | {error, term()}.

cancel_execution(ExecutionId) when is_binary(ExecutionId) ->
    gen_server:call(yawl_stateless, {cancel, ExecutionId}).

%%--------------------------------------------------------------------
%% @doc Pauses a running execution.
%%
%% @end
%%--------------------------------------------------------------------
-spec pause_execution(execution_id()) -> ok | {error, term()}.

pause_execution(ExecutionId) when is_binary(ExecutionId) ->
    gen_server:call(yawl_stateless, {pause, ExecutionId}).

%%--------------------------------------------------------------------
%% @doc Resumes a paused execution.
%%
%% @end
%%--------------------------------------------------------------------
-spec resume_execution(execution_id()) -> ok | {error, term()}.

resume_execution(ExecutionId) when is_binary(ExecutionId) ->
    gen_server:call(yawl_stateless, {resume, ExecutionId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the stateless execution service.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, #stateless_state{}}.

init([]) ->
    logger:info("Starting stateless execution service",
                [{yawl_stateless, starting}, {pid, self()}]),
    %% Use persistent_term for O(1) access to checkpoint directory
    %% (OTP 21+ optimization)
    CheckpointDir = cre_config:get(yawl_stateless_checkpoint_dir,
                                   "priv/checkpoints"),
    ok = filelib:ensure_dir(CheckpointDir ++ "/"),
    %% Start TTL cleanup timer
    schedule_ttl_cleanup(),
    {ok, #stateless_state{
        checkpoint_dir = CheckpointDir
    }}.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, #stateless_state{}) ->
          {reply, term(), #stateless_state{}}.

handle_call({execute, WorkflowSpec, InputData}, _From, State) ->
    %% Check execution limit
    case maps:size(State#stateless_state.executions) >= State#stateless_state.max_executions of
        true ->
            {reply, {error, max_executions_reached}, State};
        false ->
            %% Validate workflow spec
            case validate_workflow_spec(WorkflowSpec) of
                {error, Errors} ->
                    {reply, {error, {validation_failed, Errors}}, State};
                ok ->
                    ExecutionId = generate_execution_id(),
                    Steps = extract_workflow_steps(WorkflowSpec),

                    ExecState = #execution_state{
                        execution_id = ExecutionId,
                        workflow_spec = WorkflowSpec,
                        status = running,
                        current_step = hd(Steps),
                        steps_completed = [],
                        steps_remaining = Steps,
                        data = InputData,
                        checkpoints = [],
                        start_time = erlang:timestamp(),
                        end_time = undefined
                    },

                    NewExecutions = maps:put(ExecutionId, ExecState, State#stateless_state.executions),

                    logger:info("Stateless execution started: ~p", [ExecutionId],
                                 [{yawl_stateless, execution_started}]),

                    %% Start execution in background
                    self() ! {execute_step, ExecutionId},

                    {reply, {ok, ExecutionId}, State#stateless_state{executions = NewExecutions}}
            end
    end;

handle_call({validate, WorkflowSpec}, _From, State) ->
    case validate_workflow_spec(WorkflowSpec) of
        {error, Errors} -> {reply, {error, Errors}, State};
        ok -> {reply, ok, State}
    end;

handle_call({get_state, ExecutionId}, _From, State) ->
    case maps:get(ExecutionId, State#stateless_state.executions, undefined) of
        undefined -> {reply, {error, not_found}, State};
        ExecState -> {reply, ExecState, State}
    end;

handle_call({checkpoint, ExecutionId, CheckpointName}, _From, State) ->
    case maps:get(ExecutionId, State#stateless_state.executions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ExecState ->
            CheckpointData = serialize_execution_state(ExecState),
            Timestamp = erlang:timestamp(),

            %% Save to file
            CheckpointFile = filename(State#stateless_state.checkpoint_dir,
                                       ExecutionId, CheckpointName),
            case save_checkpoint(CheckpointFile, CheckpointData) of
                ok ->
                    NewCheckpoints = [{CheckpointName, CheckpointData, Timestamp} |
                                     ExecState#execution_state.checkpoints],
                    NewExecState = ExecState#execution_state{checkpoints = NewCheckpoints},
                    NewExecutions = maps:put(ExecutionId, NewExecState, State#stateless_state.executions),
                    logger:info("Checkpoint created: ~p checkpoint=~p",
                                [ExecutionId, CheckpointName],
                                [{yawl_stateless, checkpoint_created}]),
                    {reply, ok, State#stateless_state{executions = NewExecutions}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({restore, ExecutionId, CheckpointName}, _From, State) ->
    case maps:get(ExecutionId, State#stateless_state.executions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ExecState ->
            case lists:keyfind(CheckpointName, 1, ExecState#execution_state.checkpoints) of
                false ->
                    {reply, {error, checkpoint_not_found}, State};
                {_Name, CheckpointData, _Timestamp} ->
                    RestoredState = deserialize_execution_state(CheckpointData),
                    NewExecutions = maps:put(ExecutionId, RestoredState, State#stateless_state.executions),
                    logger:info("Stateless execution restored: ~p checkpoint=~p",
                                [ExecutionId, CheckpointName],
                                [{yawl_stateless, execution_restored}]),
                    {reply, ok, State#stateless_state{executions = NewExecutions}}
            end
    end;

handle_call(list_executions, _From, State) ->
    Executions = maps:fold(
        fun(_Id, ExecState, Acc) ->
            [#{
                execution_id => ExecState#execution_state.execution_id,
                status => ExecState#execution_state.status,
                current_step => ExecState#execution_state.current_step,
                steps_completed => length(ExecState#execution_state.steps_completed),
                steps_remaining => length(ExecState#execution_state.steps_remaining),
                start_time => timestamp_to_binary(ExecState#execution_state.start_time),
                end_time => timestamp_to_binary(ExecState#execution_state.end_time)
            } | Acc]
        end,
        [],
        State#stateless_state.executions
    ),
    {reply, lists:reverse(Executions), State};

handle_call({get_result, ExecutionId}, _From, State) ->
    case maps:get(ExecutionId, State#stateless_state.executions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #execution_state{status = completed, data = Data} ->
            {reply, Data, State};
        #execution_state{status = Status} ->
            {reply, {error, {incomplete, Status}}, State}
    end;

handle_call({cancel, ExecutionId}, _From, State) ->
    case maps:get(ExecutionId, State#stateless_state.executions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        ExecState ->
            NewExecState = ExecState#execution_state{
                status = cancelled,
                end_time = erlang:timestamp()
            },
            NewExecutions = maps:put(ExecutionId, NewExecState, State#stateless_state.executions),
            logger:info("Execution cancelled: id=~p", [ExecutionId],
                        [{yawl_stateless, execution_cancelled}]),
            {reply, ok, State#stateless_state{executions = NewExecutions}}
    end;

handle_call({pause, ExecutionId}, _From, State) ->
    case maps:get(ExecutionId, State#stateless_state.executions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #execution_state{status = running} = ExecState ->
            NewExecState = ExecState#execution_state{status = paused},
            NewExecutions = maps:put(ExecutionId, NewExecState, State#stateless_state.executions),
            {reply, ok, State#stateless_state{executions = NewExecutions}};
        _ ->
            {reply, {error, not_running}, State}
    end;

handle_call({resume, ExecutionId}, _From, State) ->
    case maps:get(ExecutionId, State#stateless_state.executions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #execution_state{status = paused} = ExecState ->
            NewExecState = ExecState#execution_state{status = running},
            NewExecutions = maps:put(ExecutionId, NewExecState, State#stateless_state.executions),
            self() ! {execute_step, ExecutionId},
            {reply, ok, State#stateless_state{executions = NewExecutions}};
        _ ->
            {reply, {error, not_paused}, State}
    end;

handle_call({delete_completed_case, ExecutionId}, _From, State) ->
    case maps:get(ExecutionId, State#stateless_state.executions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #execution_state{status = completed} = ExecState ->
            %% Check if it's actually completed
            if ExecState#execution_state.end_time =/= undefined ->
                NewExecutions = maps:remove(ExecutionId, State#stateless_state.executions),
                {reply, ok, State#stateless_state{executions = NewExecutions}};
            true ->
                {reply, {error, not_completed}, State}
            end;
        _ ->
            {reply, {error, not_completed}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(term(), #stateless_state{}) -> {noreply, #stateless_state{}}.

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handles non-GEN_SERVER messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(term(), #stateless_state{}) -> {noreply, #stateless_state{}}.

handle_info({execute_step, ExecutionId}, State) ->
    case maps:get(ExecutionId, State#stateless_state.executions, undefined) of
        undefined ->
            {noreply, State};
        #execution_state{status = paused} ->
            %% Don't execute if paused
            {noreply, State};
        #execution_state{status = cancelled} ->
            %% Don't execute if cancelled
            {noreply, State};
        #execution_state{steps_remaining = []} = ExecState ->
            %% No more steps - complete execution
            NewExecState = ExecState#execution_state{
                status = completed,
                end_time = erlang:timestamp()
            },
            NewExecutions = maps:put(ExecutionId, NewExecState, State#stateless_state.executions),
            logger:info("Stateless execution completed: ~p", [ExecutionId],
                         [{yawl_stateless, execution_completed}]),
            {noreply, State#stateless_state{executions = NewExecutions}};
        #execution_state{steps_remaining = [CurrentStep | RemainingSteps]} = ExecState ->
            %% Execute current step
            case execute_workflow_step(CurrentStep, ExecState) of
                {ok, StepResult} ->
                    NextStep = case RemainingSteps of
                        [] -> CurrentStep;
                        [H | _] -> H
                    end,
                    NewExecState = ExecState#execution_state{
                        current_step = NextStep,
                        steps_completed = [CurrentStep | ExecState#execution_state.steps_completed],
                        steps_remaining = RemainingSteps,
                        data = maps:merge(ExecState#execution_state.data, StepResult)
                    },
                    NewExecutions = maps:put(ExecutionId, NewExecState, State#stateless_state.executions),
                    %% Schedule next step
                    self() ! {execute_step, ExecutionId},
                    {noreply, State#stateless_state{executions = NewExecutions}};
                {error, Reason} ->
                    NewExecState = ExecState#execution_state{
                        status = failed,
                        end_time = erlang:timestamp(),
                        error_reason = Reason
                    },
                    NewExecutions = maps:put(ExecutionId, NewExecState, State#stateless_state.executions),
                    logger:error("Stateless execution failed: ~p reason=~p",
                               [ExecutionId, Reason],
                               [{yawl_stateless, execution_failed}]),
                    {noreply, State#stateless_state{executions = NewExecutions}}
            end
    end;

handle_info(ttl_cleanup, State) ->
    %% Remove expired executions
    Now = erlang:timestamp(),
    TTL = State#stateless_state.execution_ttl,
    NewExecutions = maps:filter(
        fun(_Id, ExecState) ->
            case ExecState#execution_state.end_time of
                undefined -> true;  %% Still running, keep
                EndTime ->
                    Age = timer:now_diff(Now, EndTime) div 1000,
                    Age < TTL
            end
        end,
        State#stateless_state.executions
    ),
    schedule_ttl_cleanup(),
    {noreply, State#stateless_state{executions = NewExecutions}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @doc Handles code change.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(term(), #stateless_state{}, term()) -> {ok, #stateless_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Handles termination.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(term(), #stateless_state{}) -> ok.

terminate(_Reason, _State) ->
    logger:info("Stateless execution stopping", [{yawl_stateless, stopping}]),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a unique execution ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_execution_id() -> binary().

generate_execution_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"exec_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @doc Validates a workflow specification.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_workflow_spec(term()) -> ok | {error, [binary()]}.

validate_workflow_spec(Spec) when is_map(Spec) ->
    case maps:get(<<"tasks">>, Spec, undefined) of
        undefined -> {error, [<<"Missing tasks definition">>]};
        Tasks when is_list(Tasks) -> ok;
        _ -> {error, [<<"Tasks must be a list">>]}
    end;
validate_workflow_spec(_Spec) ->
    {error, [<<"Invalid workflow specification">>]}.

%%--------------------------------------------------------------------
%% @doc Extracts workflow steps from specification.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_workflow_steps(term()) -> [binary()].

extract_workflow_steps(Spec) when is_map(Spec) ->
    Tasks = maps:get(<<"tasks">>, Spec, []),
    [maps:get(<<"id">>, Task, <<"unknown">>) || Task <- Tasks];
extract_workflow_steps(_Spec) ->
    [].

%%--------------------------------------------------------------------
%% @doc Executes a single workflow step.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_workflow_step(binary(), #execution_state{}) ->
          {ok, map()} | {error, term()}.

execute_workflow_step(StepId, _ExecState) ->
    %% In a real implementation, this would execute the actual task
    %% For now, return a dummy result
    {ok, #{<<"step_result">> => <<"completed">>, <<"step_id">> => StepId}}.

%%--------------------------------------------------------------------
%% @doc Serializes execution state to a storable format.
%%
%% @end
%%--------------------------------------------------------------------
-spec serialize_execution_state(#execution_state{}) -> term().

serialize_execution_state(ExecState) ->
    term_to_binary(ExecState).

%%--------------------------------------------------------------------
%% @doc Deserializes execution state from stored format.
%%
%% @end
%%--------------------------------------------------------------------
-spec deserialize_execution_state(term()) -> #execution_state{}.

deserialize_execution_state(Binary) when is_binary(Binary) ->
    binary_to_term(Binary);
deserialize_execution_state(Term) ->
    Term.

%%--------------------------------------------------------------------
%% @doc Generates checkpoint file path.
%%
%% @end
%%--------------------------------------------------------------------
-spec filename(binary(), binary(), binary()) -> binary().

filename(BaseDir, ExecutionId, CheckpointName) ->
    <<BaseDir/binary, "/", ExecutionId/binary, "_", CheckpointName/binary, ".chk">>.

%%--------------------------------------------------------------------
%% @doc Saves checkpoint to file.
%%
%% @end
%%--------------------------------------------------------------------
-spec save_checkpoint(binary(), term()) -> ok | {error, term()}.

save_checkpoint(File, Data) ->
    FilePath = binary_to_list(File),
    case file:write_file(FilePath, Data) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

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
%% @doc Schedules TTL cleanup timer.
%%
%% @end
%%--------------------------------------------------------------------
-spec schedule_ttl_cleanup() -> reference().

schedule_ttl_cleanup() ->
    %% Use persistent_term for O(1) access to TTL cleanup interval
    %% (OTP 21+ optimization)
    Interval = cre_config:get(yawl_stateless_ttl_cleanup_interval, 60000),
    erlang:send_after(Interval, self(), ttl_cleanup).
