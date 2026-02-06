%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%% Record definitions for YAWL workflow patterns
%%
%% This file contains record definitions extracted from cre_yawl.erl
%% for use by other modules that need to work with YAWL patterns.
%%

%%====================================================================
%% Basic Control Flow Records (WCP-01 to WCP-10)
%%====================================================================

-record(task, {
          id :: binary(),
          name :: binary(),
          type :: cre_yawl:task_type(),
          split_type :: cre_yawl:split_type() | undefined,
          join_type :: cre_yawl:join_type() | undefined,
          metadata = #{} :: #{atom() => term()}
         }).

-record(yawl_condition, {
          id :: binary(),
          expression :: cre_yawl:condition(),
          description :: binary() | undefined
         }).

-record(connection, {
          from_id :: binary(),
          to_id :: binary(),
          condition_id :: binary() | undefined
         }).

-record(workflow, {
          id :: binary(),
          name :: binary(),
          tasks = #{} :: #{binary() => #task{}},
          conditions = #{} :: #{binary() => #yawl_condition{}},
          connections = [] :: [#connection{}],
          start_task_id :: binary() | undefined,
          end_task_ids = [] :: [binary()]
         }).

-record(sequence, {task_ids :: [binary()]}).

-record(parallel_split, {split_task_id :: binary(), branch_task_ids :: [binary()]}).

-record(synchronization, {join_task_id :: binary(), incoming_task_ids :: [binary()]}).

-record(exclusive_choice, {choice_task_id :: binary(), branches :: [{binary(), term()}]}).

-record(simple_merge, {merge_task_id :: binary(), incoming_task_ids :: [binary()]}).

-record(multi_choice, {choice_task_id :: binary(), branches :: [{binary(), term()}]}).

-record(synchronizing_merge, {merge_task_id :: binary(), incoming_task_ids :: [binary()]}).

-record(multi_merge, {merge_task_id :: binary(), incoming_task_ids :: [binary()]}).

-record(discriminator, {merge_task_id :: binary(), incoming_task_ids :: [binary()]}).

-record(arbitration, {merge_task_id :: binary(), incoming_task_ids :: [binary()], required_count :: pos_integer()}).

%%====================================================================
%% Data Flow Pattern Records (WDP-1 to WDP-5)
%%====================================================================

-record(param_pass, {source_task_id :: binary(), target_task_id :: binary(), param_name :: atom(), transform_fn :: function() | undefined}).

-record(data_transform, {input_task_id :: binary(), output_task_id :: binary(), transform_fn :: function(), output_schema :: term() | undefined}).

-record(data_distribute, {source_task_id :: binary(), recipient_task_ids :: [binary()], distribution_type :: broadcast | round_robin | partitioned}).

-record(data_accumulate, {source_task_ids :: [binary()], target_task_id :: binary(), aggregation_fn :: function(), initial_value :: term()}).

-record(data_visibility, {data_task_id :: binary(), scope :: local | branch | global, access_list :: [binary()] | undefined}).

%%====================================================================
%% Resource Pattern Records (WRP-1 to WRP-5)
%%====================================================================

-record(resource_create, {resource_id :: binary(), resource_type :: atom(), init_params :: map()}).

-record(role_allocate, {role_id :: atom(), required_capability :: term(), allocation_strategy :: first_fit | best_fit | random}).

-record(resource_start, {resource_id :: binary(), start_params :: map()}).

-record(role_distribute, {work_item_ids :: [binary()], role_assignments :: map(), distribution_policy :: round_robin | least_loaded | affinity_based}).

-record(capability_allocate, {required_capabilities :: map(), resource_registry :: [term()], matching_strategy :: exact_match | minimum_met | best_effort}).

%%====================================================================
%% Publication-Level Pattern Reference Records (Adapted from A2A)
%%====================================================================

%% WCP Pattern Reference Record (Publication-Ready Implementation)
-record(wcp_pattern, {
    id :: binary(),
    name :: binary(),
    wcp_number :: {integer(), integer()},
    places :: [atom()],
    transitions :: [atom()],
    initial_marking :: map(),
    preset :: #{atom() => [atom()]},
    postset :: #{atom() => [atom()]},
    is_enabled :: fun(),
    fire :: fun(),
    soundness :: map()
}).

%%--------------------------------------------------------------------
%% WCP-01: Sequence Pattern
%%--------------------------------------------------------------------
-record(wcp_01_sequence, {
    task_ids :: [binary()]
}).

%%--------------------------------------------------------------------
%% WCP-02: Parallel Split Pattern
%%--------------------------------------------------------------------
-record(wcp_02_parallel_split, {
    branch_count :: pos_integer(),
    branch_tasks :: [binary()]
}).

%%--------------------------------------------------------------------
%% WCP-03: Synchronization Pattern
%%--------------------------------------------------------------------
-record(wcp_03_synchronization, {
    branch_count :: pos_integer(),
    branch_tasks :: [binary()]
}).

%%--------------------------------------------------------------------
%% WCP-04: Exclusive Choice Pattern
%%--------------------------------------------------------------------
-record(wcp_04_exclusive_choice, {
    condition_fun :: fun(),
    branch_tasks :: [binary()]
}).

%%--------------------------------------------------------------------
%% WCP-07: Structured Synchronization Merge Pattern
%%--------------------------------------------------------------------
-record(wcp_07_structured_sync_merge, {
    branch_count :: pos_integer(),
    branch_tasks :: [binary()]
}).

%%--------------------------------------------------------------------
%% WCP-09: Discriminator Pattern
%%--------------------------------------------------------------------
-record(wcp_09_discriminator, {
    branch_count :: pos_integer(),
    branch_tasks :: [binary()]
}).

%%--------------------------------------------------------------------
%% WCP-13: Multi-Instance (Static) Pattern
%%--------------------------------------------------------------------
-record(wcp_13_multi_instance_static, {
    instance_count :: pos_integer(),
    instance_fun :: fun((term()) -> term()),
    input_data :: [term()]
}).

%%--------------------------------------------------------------------
%% WCP-15: Multi-Instance (Dynamic) Pattern
%%--------------------------------------------------------------------
-record(wcp_15_multi_instance_dynamic, {
    data_fun :: fun(() -> {more, term()} | done),
    instance_fun :: fun((term()) -> term())
}).

%%--------------------------------------------------------------------
%% WCP-16: Deferred Choice Pattern
%%--------------------------------------------------------------------
-record(wcp_16_deferred_choice, {
    options :: [{binary(), term()}],
    condition_fun :: fun((term()) -> boolean()),
    initial_data :: term()
}).

%%--------------------------------------------------------------------
%% WCP-17: Interleaved Routing Pattern
%%--------------------------------------------------------------------
-record(wcp_17_interleaved_routing, {
    branches :: [fun((term()) -> term())],
    initial_data :: term()
}).

%%--------------------------------------------------------------------
%% WCP-18: Milestone Pattern
%%--------------------------------------------------------------------
-record(wcp_18_milestone, {
    activity :: fun((term()) -> term()),
    milestone_fun :: fun((term()) -> boolean()),
    initial_data :: term()
}).

%%--------------------------------------------------------------------
%% WCP-19: Cancel Activity Pattern
%%--------------------------------------------------------------------
-record(wcp_19_cancel_activity, {
    activity :: fun((term()) -> term()),
    cancel_fun :: fun((term()) -> boolean())
}).

%%--------------------------------------------------------------------
%% WCP-20: Cancel Case Pattern
%%--------------------------------------------------------------------
-record(wcp_20_cancel_case, {
    activities :: [fun((term()) -> term())],
    cancel_fun :: fun((term()) -> boolean())
}).

%%--------------------------------------------------------------------
%% WCP-25: Cancel Region Pattern
%%--------------------------------------------------------------------
-record(wcp_25_cancel_region, {
    region_activities :: [fun((term()) -> term())],
    cancel_fun :: fun((term()) -> boolean()),
    region_ids :: [atom()]
}).

%%--------------------------------------------------------------------
%% WCP-39: Critical Section Pattern
%%--------------------------------------------------------------------
-record(wcp_39_critical_section, {
    critical_activity :: fun((term()) -> term()),
    lock_id :: atom()
}).

%%====================================================================
%% Human-in-the-Loop Approval Pattern Records
%%====================================================================

%%--------------------------------------------------------------------
%% Approval Checkpoint Record
%%--------------------------------------------------------------------
-record(approval_checkpoint, {
    checkpoint_id :: binary(),
    pattern_id :: binary() | undefined,
    step_name :: atom(),
    context :: map(),
    required_approver :: human | simulated | auto,
    timeout :: non_neg_integer() | infinity,
    approval_schema :: map(),
    created_at :: integer(),
    expires_at :: integer() | undefined,
    metadata :: map()
}).

%%--------------------------------------------------------------------
%% Approval Decision Record
%%--------------------------------------------------------------------
-record(approval_decision, {
    checkpoint_id :: binary(),
    approved :: boolean(),
    decision_maker :: human | simulated | auto | {human, binary()} | cancelled | timeout,
    reason :: binary(),
    metadata :: map(),
    decided_at :: integer()
}).

%%--------------------------------------------------------------------
%% Approval Wrapped Pattern Record (Middleware)
%%--------------------------------------------------------------------
-record(approval_wrapped, {
    original_pattern :: term(),
    approval_config :: map(),
    middleware_chain :: [fun()]
}).

%%--------------------------------------------------------------------
%% Claude Bridge Session Record
%%--------------------------------------------------------------------
-record(claude_session, {
    session_id :: binary(),
    started_at :: integer(),
    last_activity :: integer(),
    message_count :: non_neg_integer(),
    context :: map()
}).
