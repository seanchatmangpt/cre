%% -*- erlang -*-
%%
%% CRE Test Approval Workflow
%%
%% This module defines a workflow for running tests with approval checkpoints.
%% Demonstrates human-in-the-loop workflow execution.
%%
%% @author CRE Team
%% @doc Test Approval Workflow Definition
%% @end

-module(test_approval_workflow).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

-export([
    create_workflow/0,
    create_workflow/1,
    execute_step/2,
    get_workflow_state/1,
    list_all_workflows/0
]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").
-include("cre_yawl_patterns.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(test_workflow, {
    id :: binary(),
    name :: binary(),
    steps :: [atom()],
    current_step :: atom() | undefined,
    checkpoints :: #{binary() => binary()},
    status :: created | running | paused | completed | failed,
    created_at :: integer(),
    started_at :: integer() | undefined,
    completed_at :: integer() | undefined,
    results :: map()
}).

-type workflow_id() :: binary().
-type workflow_status() :: created | running | paused | completed | failed.
-type step_name() :: atom().

-export_type([workflow_id/0, workflow_status/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new test approval workflow.
%%
%% @return {ok, WorkflowId}
%%
%% @end
%%--------------------------------------------------------------------
-spec create_workflow() -> {ok, workflow_id()}.

create_workflow() ->
    create_workflow(#{
        name => <<"Default Test Workflow">>,
        auto_approve_compile => true,
        require_test_approval => true,
        require_result_review => true
    }).

%%--------------------------------------------------------------------
%% @doc Creates a new test approval workflow with options.
%%
%% @param Options Workflow options map.
%% @return {ok, WorkflowId}
%%
%% @end
%%--------------------------------------------------------------------
-spec create_workflow(map()) -> {ok, workflow_id()}.

create_workflow(Options) ->
    WorkflowId = generate_workflow_id(),

    %% Define workflow steps
    Steps = [
        compile,
        approval_pre_test,
        run_tests,
        approval_review_results,
        complete
    ],

    Workflow = #test_workflow{
        id = WorkflowId,
        name = maps:get(name, Options, <<"Test Workflow">>),
        steps = Steps,
        current_step = undefined,
        checkpoints = #{},
        status = created,
        created_at = erlang:system_time(millisecond),
        started_at = undefined,
        completed_at = undefined,
        results = #{}
    },

    %% Store workflow in ETS
    store_workflow(Workflow),

    %% Log workflow creation
    ?LOG_INFO("Created test workflow: ~s", [WorkflowId]),

    {ok, WorkflowId}.

%%--------------------------------------------------------------------
%% @doc Executes a workflow step.
%%
%% @param WorkflowId The workflow ID.
%% @param StepName The step to execute.
%% @return {ok, Result} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_step(workflow_id(), step_name()) ->
    {ok, term()} | {error, term()}.

execute_step(WorkflowId, compile) ->
    ?LOG_INFO("Compiling workflow: ~s", [WorkflowId]),
    Result = do_compile(),
    update_workflow_result(WorkflowId, compile, Result),
    {ok, Result};

execute_step(WorkflowId, approval_pre_test) ->
    ?LOG_INFO("Pre-test approval checkpoint for workflow: ~s", [WorkflowId]),
    {ok, CheckpointId} = create_approval_checkpoint(
        WorkflowId,
        pre_test_checkpoint,
        #{required_approver => auto}
    ),
    {ok, Decision} = yawl_approval:request_approval(CheckpointId),
    update_workflow_checkpoint(WorkflowId, pre_test, CheckpointId),
    update_workflow_result(WorkflowId, approval_pre_test, Decision),
    {ok, Decision};

execute_step(WorkflowId, run_tests) ->
    ?LOG_INFO("Running tests for workflow: ~s", [WorkflowId]),
    Result = do_run_tests(),
    update_workflow_result(WorkflowId, run_tests, Result),
    {ok, Result};

execute_step(WorkflowId, approval_review_results) ->
    ?LOG_INFO("Review results approval for workflow: ~s", [WorkflowId]),
    {ok, CheckpointId} = create_approval_checkpoint(
        WorkflowId,
        post_test_review,
        #{required_approver => auto}
    ),
    {ok, Decision} = yawl_approval:request_approval(CheckpointId),
    update_workflow_checkpoint(WorkflowId, post_test, CheckpointId),
    update_workflow_result(WorkflowId, approval_review_results, Decision),
    {ok, Decision};

execute_step(WorkflowId, complete) ->
    ?LOG_INFO("Completing workflow: ~s", [WorkflowId]),
    mark_workflow_completed(WorkflowId),
    {ok, workflow_completed};

execute_step(_WorkflowId, Step) ->
    {error, {unknown_step, Step}}.

%%--------------------------------------------------------------------
%% @doc Gets the current state of a workflow.
%%
%% @param WorkflowId The workflow ID.
%% @return {ok, Workflow} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_workflow_state(workflow_id()) ->
    {ok, #test_workflow{}} | {error, not_found}.

get_workflow_state(WorkflowId) ->
    case ets:lookup(?MODULE, WorkflowId) of
        [{WorkflowId, Workflow}] -> {ok, Workflow};
        [] -> {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all workflows.
%%
%% @return List of workflow IDs.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_all_workflows() -> [workflow_id()].

list_all_workflows() ->
    WorkflowIds = ets:select(?MODULE, [{{'$1', '_'}, [], ['$1']}]),
    WorkflowIds.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Compiles the project.
%%
%% @end
%%--------------------------------------------------------------------
do_compile() ->
    Result = os:cmd("cd /Users/sac/cre && rebar3 compile 2>&1"),
    case string:str(Result, "Compiling cre") of
        0 -> {error, compilation_failed};
        _ -> {ok, compiled}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Runs the test suite.
%%
%% @end
%%--------------------------------------------------------------------
do_run_tests() ->
    Result = os:cmd("cd /Users/sac/cre && rebar3 eunit --module=yawl_approval_test 2>&1"),
    case string:str(Result, "passed") of
        0 -> {error, tests_failed};
        N when N > 0 -> {ok, tests_passed}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Creates an approval checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
create_approval_checkpoint(WorkflowId, StepName, Options) ->
    Context = #{
        workflow_id => WorkflowId,
        step => StepName,
        created_at => erlang:system_time(millisecond)
    },
    yawl_approval:create_checkpoint(
        <<WorkflowId/binary, "_", (atom_to_binary(StepName))/binary>>,
        StepName,
        maps:merge(Options, #{context => Context})
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Stores workflow in ETS.
%%
%% @end
%%--------------------------------------------------------------------
store_workflow(Workflow) ->
    ensure_ets_table(),
    ets:insert(?MODULE, {Workflow#test_workflow.id, Workflow}).

%%--------------------------------------------------------------------
%% @private
%% @doc Updates workflow result.
%%
%% @end
%%--------------------------------------------------------------------
update_workflow_result(WorkflowId, Step, Result) ->
    case get_workflow_state(WorkflowId) of
        {ok, Workflow} ->
            NewResults = maps:put(Step, Result, Workflow#test_workflow.results),
            NewWorkflow = Workflow#test_workflow{results = NewResults},
            store_workflow(NewWorkflow);
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Updates workflow checkpoint mapping.
%%
%% @end
%%--------------------------------------------------------------------
update_workflow_checkpoint(WorkflowId, Step, CheckpointId) ->
    case get_workflow_state(WorkflowId) of
        {ok, Workflow} ->
            NewCheckpoints = maps:put(atom_to_binary(Step), CheckpointId,
                                      Workflow#test_workflow.checkpoints),
            NewWorkflow = Workflow#test_workflow{checkpoints = NewCheckpoints},
            store_workflow(NewWorkflow);
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Marks workflow as completed.
%%
%% @end
%%--------------------------------------------------------------------
mark_workflow_completed(WorkflowId) ->
    case get_workflow_state(WorkflowId) of
        {ok, Workflow} ->
            NewWorkflow = Workflow#test_workflow{
                status = completed,
                completed_at = erlang:system_time(millisecond)
            },
            store_workflow(NewWorkflow);
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures ETS table exists.
%%
%% @end
%%--------------------------------------------------------------------
ensure_ets_table() ->
    case ets:whereis(?MODULE) of
        undefined ->
            ets:new(?MODULE, [named_table, public, {read_concurrency, true}]);
        _Table ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique workflow ID.
%%
%% @end
%%--------------------------------------------------------------------
generate_workflow_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"workflow_", Hex/binary>>.
