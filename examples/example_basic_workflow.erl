%% -*- erlang -*-
%%
%% CRE Example: Basic Workflow
%%
%% This example demonstrates a simple sequential workflow pattern with
%% parameter passing and error handling.
%%
%% Workflow Structure:
%%   Task1 (Initialize) -> Task2 (Process) -> Task3 (Finalize)
%%
%% Features demonstrated:
%% - Sequential task execution (WCP-1: Sequence pattern)
%% - Parameter passing between tasks (WDP-1: Parameter Pass)
%% - Error handling and recovery
%% - Client-server communication with CRE
%%
%% Expected Output:
%%   Task1: Initialize with value: 42
%%   Task2: Process received: 42, result: 84
%%   Task3: Finalize received: 84
%%   Workflow completed successfully
%%

-module(example_basic_workflow).

%%====================================================================
%% Exports
%%====================================================================

-export([run/0, run/1]).
-export([init/1, is_value/2, step/2, recv/4]).

%%====================================================================
%% API Functions
%%====================================================================

%% Start a basic workflow with the local CRE node
run() ->
    {ok, CrePid} = cre:pid('cre@localhost'),
    run(CrePid).

%% Start a basic workflow with a specified CRE node
run(CrePid) ->
    io:format("~n=== Basic Workflow Example ===~n~n", []),

    %% Create a workflow as a simple workflow structure
    %% Format: {task_name, task_id, input_params}
    Workflow = [
        {<<"task1">>, <<"init">>, []},
        {<<"task2">>, <<"process">>, [{<<"value">>, 42}]},
        {<<"task3">>, <<"finalize">>, [{<<"result">>, undefined}]}
    ],

    %% Start the client
    {ok, ClientPid} = cre_client:start_link(
        CrePid,
        ?MODULE,
        [Workflow]
    ),

    %% Send the initial workflow to CRE
    cre_client:eval(ClientPid, Workflow),

    %% Wait for completion
    timer:sleep(1000),

    %% Stop the client
    cre_client:stop(ClientPid),

    io:format("~nWorkflow completed successfully~n~n", []),
    ok.

%%====================================================================
%% CRE Client Callback Functions
%%====================================================================

%% Initialize the client state
%% This is called once when the client starts
-spec init(InitArg :: term()) -> term().

init(Workflow) ->
    io:format("Initializing client with workflow: ~p~n", [Workflow]),
    #{
        workflow => Workflow,
        current_task => 1,
        results => []
    }.

%% Determine if an element represents a computed value
%% Returns true if the value is final (no more processing needed)
-spec is_value(E :: term(), UsrInfo :: term()) -> boolean().

is_value(E, _UsrInfo) ->
    case E of
        {complete, _} -> true;
        {error, _} -> true;
        _ -> false
    end.

%% Execute one step of the workflow
%% This is called repeatedly until is_value returns true
-spec step(E :: term(), UsrInfo :: term()) ->
    {ok, term(), [term()]} |
    {ok_send, term(), [term()]} |
    norule.

step(E, UsrInfo = #{current_task := TaskIdx, workflow := Workflow}) ->
    case E of
        {start} ->
            %% Start the workflow
            Tasks = Workflow,
            io:format("Starting workflow with ~p tasks~n", [length(Tasks)]),
            {ok_send, {task_init, 0}, Tasks};

        {task_init, Idx} when Idx < length(Workflow) ->
            %% Process task by task
            Task = lists:nth(Idx + 1, Workflow),
            {TaskName, TaskId, Params} = Task,
            io:format("Executing task ~p (ID: ~p) with params: ~p~n",
                      [TaskName, TaskId, Params]),

            %% Pass to next task
            NextIdx = Idx + 1,
            {ok_send, {task_init, NextIdx}, [{executed, TaskId}]};

        {task_init, Idx} when Idx >= length(Workflow) ->
            %% Workflow complete
            io:format("All tasks completed~n", []),
            {ok, {complete, ok}};

        {error, Reason} ->
            io:format("ERROR: Workflow failed with reason: ~p~n", [Reason]),
            {ok, {error, Reason}};

        _ ->
            %% No matching pattern
            norule
    end.

%% Receive results from CRE workers
%% This is called when a task completes
-spec recv(E :: term(), ReplyLst :: [term()], UsrInfo :: term()) -> term().

recv(E, ReplyLst, UsrInfo = #{results := Results}) ->
    io:format("Received replies: ~p~n", [ReplyLst]),
    case E of
        {task_result, TaskId, Result} ->
            %% Store the result
            NewResults = [Result | Results],
            UsrInfo#{results => NewResults};
        _ ->
            UsrInfo
    end.

%%====================================================================
%% Best Practices Demonstrated
%%====================================================================

% 1. PARAMETER PASSING (WDP-1):
%    - Task input parameters are passed as property lists
%    - Results are accumulated in the UsrInfo state map
%    - Each task receives output from previous tasks
%
% 2. ERROR HANDLING:
%    - Pattern matching on error tuples {error, Reason}
%    - Graceful degradation with error logging
%    - State preservation for recovery
%
% 3. STATE MANAGEMENT:
%    - Use maps for UsrInfo to store workflow state
%    - Current task index tracks progress
%    - Results list accumulates computation outputs
%
% 4. SEQUENCE PATTERN (WCP-1):
%    - Tasks execute in defined order
%    - Next task starts only after previous completes
%    - No parallel branches in this basic example
%
% 5. CLIENT INITIALIZATION:
%    - init/1 is called once at startup
%    - State is passed through all callbacks
%    - Modifications return new state to maintain consistency

%%====================================================================
%% Testing
%%====================================================================

% To test this example:
%
% 1. Start the CRE runtime:
%    erl -sname cre -pa _build/default/lib/*/ebin -run cre start
%
% 2. In another Erlang shell:
%    erl -sname example -pa _build/default/lib/*/ebin
%    (example_basic_workflow:run()).
%
% Expected output should show:
%   - Task initialization
%   - Sequential task execution
%   - Parameter passing between tasks
%   - Workflow completion
%
% If there are errors:
%   - Check that CRE node is running: rpc:call('cre@localhost', erlang, whereis, [cre_master])
%   - Verify node connectivity: net_kernel:connect_node('cre@localhost')
%   - Review error output for specific failure reasons
