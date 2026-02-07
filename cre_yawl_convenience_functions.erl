%%====================================================================
%% Pattern Convenience Functions for cre_yawl
%%
;; This file contains the 5 pattern convenience functions that should be
;; added to /Users/sac/cre/src/cre_yawl.erl
%%
;; TO APPLY: Insert the functions below after the connect/3 function
;; and add the exports:
;; -export([add_atomic_task/3, add_approval_task/4, add_multi_instance_task/4,
;;          add_exclusive_gateway/2, add_parallel_gateway/2]).
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Adds an atomic task to the workflow (WCP-01).
%%
%% Creates a task with type=atomic that executes a single function.
%% The Module:Function tuple specifies the implementation to be called.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_atomic_task(Workflow :: #workflow{}, TaskName :: binary(),
                     {Module :: atom(), Function :: atom()}) ->
          {ok, #workflow{}} | {error, term()}.
add_atomic_task(Workflow, TaskName, {Module, Function})
  when is_binary(TaskName), is_atom(Module), is_atom(Function) ->
    TaskId = generate_id(<<"atomic">>),
    Task = #task{
        id = TaskId,
        name = TaskName,
        type = atomic,
        metadata = #{
            module => Module,
            function => Function
        }
    },
    {ok, add_task(Workflow, TaskId, Task)}.

%%--------------------------------------------------------------------
%% @doc Adds an approval task to the workflow.
%%
%% Creates a task with type=approval that requires human approval
%% before proceeding. The Approver specifies who can approve this task.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_approval_task(Workflow :: #workflow{}, TaskName :: binary(),
                       Approver :: binary() | [binary()],
                       Options :: map()) ->
          {ok, #workflow{}} | {error, term()}.
add_approval_task(Workflow, TaskName, Approver, Options)
  when is_binary(TaskName), is_map(Options) ->
    TaskId = generate_id(<<"approval">>),
    Metadata = #{
        approver => Approver,
        timeout => maps:get(timeout, Options, 86400000),
        notification => maps:get(notification, Options, undefined)
    },
    Task = #task{
        id = TaskId,
        name = TaskName,
        type = atomic,
        metadata = Metadata
    },
    {ok, add_task(Workflow, TaskId, Task)}.

%%--------------------------------------------------------------------
%% @doc Adds a multi-instance task to the workflow (WCP-08).
%%
%% Creates a task with type=multi_instance that executes multiple
%% parallel instances of the same task. Cardinality specifies how many.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_multi_instance_task(Workflow :: #workflow{}, TaskName :: binary(),
                              Cardinality :: pos_integer() | {atom(), pos_integer()},
                              {Module :: atom(), Function :: atom()}) ->
          {ok, #workflow{}} | {error, term()}.
add_multi_instance_task(Workflow, TaskName, Cardinality, {Module, Function})
  when is_binary(TaskName), is_atom(Module), is_atom(Function) ->
    TaskId = generate_id(<<"multi_instance">>),
    Metadata = #{
        module => Module,
        function => Function,
        cardinality => Cardinality
    },
    Task = #task{
        id = TaskId,
        name = TaskName,
        type = multi_instance,
        metadata = Metadata
    },
    {ok, add_task(Workflow, TaskId, Task)}.

%%--------------------------------------------------------------------
%% @doc Adds an exclusive (XOR) gateway to the workflow (WCP-04).
%%
%% Creates a gateway that routes to exactly one of multiple outgoing
%% branches based on condition evaluation.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_exclusive_gateway(Workflow :: #workflow{}, GatewayName :: binary()) ->
          {ok, #workflow{}} | {error, term()}.
add_exclusive_gateway(Workflow, GatewayName) when is_binary(GatewayName) ->
    GatewayId = generate_id(<<"xor_gateway">>),
    Gateway = #task{
        id = GatewayId,
        name = GatewayName,
        type = atomic,
        split_type = xor_split,
        join_type = xor_join
    },
    {ok, add_task(Workflow, GatewayId, Gateway)}.

%%--------------------------------------------------------------------
%% @doc Adds a parallel (AND) gateway to the workflow (WCP-02).
%%
%% Creates a gateway that splits execution to all outgoing branches
%% in parallel and waits for all to complete before proceeding.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_parallel_gateway(Workflow :: #workflow{}, GatewayName :: binary()) ->
          {ok, #workflow{}} | {error, term()}.
add_parallel_gateway(Workflow, GatewayName) when is_binary(GatewayName) ->
    GatewayId = generate_id(<<"and_gateway">>),
    Gateway = #task{
        id = GatewayId,
        name = GatewayName,
        type = atomic,
        split_type = and_split,
        join_type = and_join
    },
    {ok, add_task(Workflow, GatewayId, Gateway)}.
