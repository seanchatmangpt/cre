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

-module(wf_task).
-moduledoc """
External task token constructors.

Token form is stable:
  {task, TaskId, State, Payload}

All functions return {produce, #{Place => [Token]}}.

```erlang
> wf_task:enabled(task42, #{kind => approval}, inbox).
{produce, #{inbox => [{task,task42,enabled,#{kind => approval}}]}}

> wf_task:running(task42, #{pct => 50}, inbox).
{produce, #{inbox => [{task,task42,running,#{pct => 50}}]}}

> wf_task:done(task42, #{ok => true}, inbox).
{produce, #{inbox => [{task,task42,done,#{ok => true}}]}}

> wf_task:failed(task42, timeout, inbox).
{produce, #{inbox => [{task,task42,failed,timeout}]}}

> wf_task:cancelled(task42, user_abort, inbox).
{produce, #{inbox => [{task,task42,cancelled,user_abort}]}}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Task lifecycle constructors
-export([enabled/3, running/3, done/3, failed/3, cancelled/3]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Unique identifier for a task instance.
%%
%% Can be any Erlang term - typically an integer, binary, or UUID.
%%--------------------------------------------------------------------
-type task_id() :: term().

%%--------------------------------------------------------------------
%% @doc Status atom representing the task's lifecycle state.
%%
%% Valid statuses:
%% <ul>
%%   <li><b>enabled:</b> Task is ready to begin execution</li>
%%   <li><b>running:</b> Task is currently executing</li>
%%   <li><b>done:</b> Task completed successfully</li>
%%   <li><b>failed:</b> Task failed with an error</li>
%%   <li><b>cancelled:</b> Task was cancelled before completion</li>
%% </ul>
%%--------------------------------------------------------------------
-type task_status() :: enabled | running | done | failed | cancelled.

%%--------------------------------------------------------------------
%% @doc A token representing a task lifecycle event.
%%
%% Tokens carry the task identifier, current status, and associated
%% payload (input data, result, error reason, etc.).
%%--------------------------------------------------------------------
-type task_token() :: {task, task_id(), task_status(), payload()}.

%%--------------------------------------------------------------------
%% @doc Payload carried by a task token.
%%
%% Can be any Erlang term:
%% <ul>
%%   <li>Input data for enabled/running</li>
%%   <li>Result value for done</li>
%%   <li>Error reason for failed</li>
%%   <li>Cancellation reason for cancelled</li>
%% </ul>
%%--------------------------------------------------------------------
-type payload() :: term().

%%--------------------------------------------------------------------
%% @doc A place in the Petri net where tokens are deposited.
%%
%% Must be an atom representing a valid place in the target net.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A produce map specifies tokens to be produced.
%%
%% Maps places to the list of tokens that will be added during
%% token injection. This is the standard gen_pnet produce map format.
%%--------------------------------------------------------------------
-type produce_map() :: #{place() => [task_token()]}.

%%--------------------------------------------------------------------
%% @doc The return type for all task constructors.
%%
%% A tuple containing the atom 'produce' and a map of places to tokens.
%% This format matches gen_pnet's expected injection format.
%%--------------------------------------------------------------------
-type produce_result() :: {produce, produce_map()}.

%% Export types
-export_type([task_id/0, task_status/0, task_token/0, payload/0]).
-export_type([place/0, produce_map/0, produce_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a produce map indicating a task is enabled.
%%
%% Use this when external work is ready to begin execution. The token
%% signals that the task has all prerequisites met and can start.
%%
%% @param TaskId Unique identifier for the task
%% @param Payload Input data or initial task context
%% @param Place The place where the enabled token should be deposited
%% @returns {produce, ProduceMap} with a single enabled token
%%
%% @end
%%--------------------------------------------------------------------
-spec enabled(TaskId :: task_id(), Payload :: payload(), Place :: place()) ->
          produce_result().

enabled(TaskId, Payload, Place) ->
    Token = {task, TaskId, enabled, Payload},
    {produce, #{Place => [Token]}}.

%%--------------------------------------------------------------------
%% @doc Creates a produce map indicating a task is running.
%%
%% Use this when external work has begun execution. The token signals
%% that the task is actively processing.
%%
%% @param TaskId Unique identifier for the task
%% @param Payload Execution context or tracking information
%% @param Place The place where the running token should be deposited
%% @returns {produce, ProduceMap} with a single running token
%%
%% @end
%%--------------------------------------------------------------------
-spec running(TaskId :: task_id(), Payload :: payload(), Place :: place()) ->
          produce_result().

running(TaskId, Payload, Place) ->
    Token = {task, TaskId, running, Payload},
    {produce, #{Place => [Token]}}.

%%--------------------------------------------------------------------
%% @doc Creates a produce map indicating a task completed successfully.
%%
%% Use this when external work finishes without error. The token carries
%% the result of the task execution.
%%
%% @param TaskId Unique identifier for the task
%% @param Output The result value from successful task execution
%% @param Place The place where the done token should be deposited
%% @returns {produce, ProduceMap} with a single done token
%%
%% @end
%%--------------------------------------------------------------------
-spec done(TaskId :: task_id(), Output :: payload(), Place :: place()) ->
          produce_result().

done(TaskId, Output, Place) ->
    Token = {task, TaskId, done, Output},
    {produce, #{Place => [Token]}}.

%%--------------------------------------------------------------------
%% @doc Creates a produce map indicating a task failed.
%%
%% Use this when external work encounters an error. The token carries
%% the reason for failure which can be used for error handling or retry logic.
%%
%% @param TaskId Unique identifier for the task
%% @param Reason Error reason indicating why the task failed
%% @param Place The place where the failed token should be deposited
%% @returns {produce, ProduceMap} with a single failed token
%%
%% @end
%%--------------------------------------------------------------------
-spec failed(TaskId :: task_id(), Reason :: payload(), Place :: place()) ->
          produce_result().

failed(TaskId, Reason, Place) ->
    Token = {task, TaskId, failed, Reason},
    {produce, #{Place => [Token]}}.

%%--------------------------------------------------------------------
%% @doc Creates a produce map indicating a task was cancelled.
%%
%% Use this when external work is cancelled before completion. The token
%% carries the cancellation reason (user_initiated, timeout, etc.).
%%
%% @param TaskId Unique identifier for the task
%% @param Reason Cancellation reason explaining why task was cancelled
%% @param Place The place where the cancelled token should be deposited
%% @returns {produce, ProduceMap} with a single cancelled token
%%
%% @end
%%--------------------------------------------------------------------
-spec cancelled(TaskId :: task_id(), Reason :: payload(), Place :: place()) ->
          produce_result().

cancelled(TaskId, Reason, Place) ->
    Token = {task, TaskId, cancelled, Reason},
    {produce, #{Place => [Token]}}.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).
-endif.
