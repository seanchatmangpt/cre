%% -*- erlang -*-
%%
%% OpenTelemetry Logger Records for YAWL Workflows
%%
%% @doc Record definitions for YAWL OpenTelemetry logging
%% @end

-ifndef(YAWL_OTEL_LOGGER_HRL).
-define(YAWL_OTEL_LOGGER_HRL).

%%====================================================================
%% OpenTelemetry Event Record
%%====================================================================
-record(otel_event, {
    id :: binary(),
    trace_id :: binary(),
    span_id :: binary(),
    parent_span_id :: binary() | undefined,
    timestamp :: integer(),
    event_type :: binary() | atom(),
    level :: debug | info | warning | error,
    user_id :: term(),
    case_id :: term(),
    task_id :: term(),
    pattern_id :: term(),
    message :: binary(),
    attributes :: map()
}).

%%====================================================================
%% OpenTelemetry Trace Record
%%====================================================================
-record(otel_trace, {
    trace_id :: binary(),
    case_id :: binary(),
    pattern_id :: binary(),
    start_time :: integer(),
    end_time :: integer() | undefined,
    status :: term(),
    span_count :: non_neg_integer()
}).

-endif.
