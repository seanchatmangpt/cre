%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% OpenTelemetry Logger Record Definitions
%%
%% @doc YAWL OpenTelemetry Logger Records
%% @end

-record(otel_event, {
    id :: binary(),
    trace_id :: binary(),
    span_id :: binary() | undefined,
    parent_span_id :: binary() | undefined,
    timestamp :: integer(),
    event_type :: binary() | atom(),
    level :: atom(),
    user_id :: binary() | undefined,
    case_id :: binary() | undefined,
    task_id :: binary() | undefined,
    pattern_id :: binary() | undefined,
    message :: binary(),
    attributes :: map()
}).

-record(otel_trace, {
    trace_id :: binary(),
    case_id :: binary(),
    pattern_id :: binary(),
    start_time :: integer(),
    end_time :: integer() | undefined,
    status :: atom(),
    span_count :: non_neg_integer()
}).
