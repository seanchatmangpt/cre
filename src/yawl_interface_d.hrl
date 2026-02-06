%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% YAWL Interface D Record Definitions
%%
%% This header file contains record definitions for Interface D
%% which handles exception service integration and worklet communication.
%%
%% @doc YAWL Interface D Header
%% @end

%%====================================================================
%% Types
%%====================================================================

-type service_id() :: binary().
-type execution_id() :: binary().
-type case_id() :: binary().
-type task_id() :: binary().
-type worklet_spec_id() :: binary().
-type service_type() :: local | remote.
-type worklet_status() :: pending | running | completed | failed | aborted.

%%====================================================================
%% Records
%%====================================================================

%% Exception Service Registration Record
-record(exception_service, {
    service_id :: service_id(),
    endpoint :: binary(),
    service_type :: service_type(),
    enabled :: boolean(),
    priority :: non_neg_integer()
}).

%% Worklet Execution Record
-record(worklet_execution, {
    execution_id :: execution_id(),
    case_id :: case_id(),
    task_id :: task_id(),
    worklet_spec_id :: worklet_spec_id(),
    status :: worklet_status(),
    started_at :: integer(),
    completed_at :: integer() | undefined,
    exception_data :: map(),
    result :: term() | undefined
}).

%% Interface D Server State Record
-record(interface_d_state, {
    exception_services = #{} :: #{service_id() => #exception_service{}},
    active_worklets = [] :: [#worklet_execution{}],
    service_counter = 0 :: non_neg_integer(),
    compensation_log = [] :: [{execution_id(), term(), integer()}]
}).
