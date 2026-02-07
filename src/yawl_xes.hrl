%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% XES Logging Records for YAWL Workflow Patterns
%% Based on IEEE 1849-2016 XES Standard for Event Logs
%%
%% -------------------------------------------------------------------

-ifndef(YAWL_XES_HRL).
-define(YAWL_XES_HRL, true).

%%====================================================================
%% Records
%%====================================================================

-record(xes_log, {
    log_id,
    trace_id,
    started_at,
    events = [],
    metadata = #{}
}).

-record(xes_event, {
    event_id,
    timestamp,
    case_id = undefined,
    concept = #{},
    lifecycle = #{},
    data = #{}
}).

-record(state, {
    logs = #{},
    traces = #{},
    next_event_id = 0
}).

-endif.
