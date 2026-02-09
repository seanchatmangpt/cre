%% -*- erlang -*-
%% @doc CRE-Specific Metrics Definitions
%%
%% Central registry of all CRE telemetry metrics with initialization helpers.
%%
%% @end

-module(cre_metrics).

%% API
-export([init/0]).
-export([register_all/0]).
-export([transition_fired/2, transition_fired/3]).
-export([pattern_executed/2]).
-export([mining_algorithm_started/1, mining_algorithm_completed/2]).
-export([yawl_compiled/1]).
-export([case_started/1, case_completed/2]).
-export([token_count/1]).
-export([pnet_throughput/1]).

%% Metric name exports for use in other modules
-export([
    %% Petri Net Metrics
    pnet_transitions_total/0,
    pnet_transition_duration_ms/0,
    pnet_tokens/0,
    pnet_throughput_fps/0,

    %% Pattern Metrics
    pattern_executions_total/0,
    pattern_execution_duration_ms/0,
    pattern_errors_total/0,

    %% Mining Metrics
    mining_algorithm_duration_ms/0,
    mining_algorithm_executions_total/0,
    mining_discovered_places/0,
    mining_discovered_transitions/0,

    %% YAWL Metrics
    yawl_compilations_total/0,
    yawl_compilation_duration_ms/0,
    yawl_cases_total/0,
    yawl_case_duration_ms/0,

    %% System Metrics
    cre_memory_bytes/0,
    cre_process_count/0
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Metric Name Constants
%%====================================================================

%% Petri Net Metrics
-define(PNET_TRANSITIONS_TOTAL, <<"cre_pnet_transitions_total">>).
-define(PNET_TRANSITION_DURATION_MS, <<"cre_pnet_transition_duration_ms">>).
-define(PNET_TOKENS, <<"cre_pnet_tokens">>).
-define(PNET_THROUGHPUT_FPS, <<"cre_pnet_throughput_fps">>).

%% Pattern Metrics
-define(PATTERN_EXECUTIONS_TOTAL, <<"cre_pattern_executions_total">>).
-define(PATTERN_EXECUTION_DURATION_MS, <<"cre_pattern_execution_duration_ms">>).
-define(PATTERN_ERRORS_TOTAL, <<"cre_pattern_errors_total">>).

%% Mining Metrics
-define(MINING_ALGORITHM_DURATION_MS, <<"cre_mining_algorithm_duration_ms">>).
-define(MINING_ALGORITHM_EXECUTIONS_TOTAL, <<"cre_mining_algorithm_executions_total">>).
-define(MINING_DISCOVERED_PLACES, <<"cre_mining_discovered_places">>).
-define(MINING_DISCOVERED_TRANSITIONS, <<"cre_mining_discovered_transitions">>).

%% YAWL Metrics
-define(YAWL_COMPILATIONS_TOTAL, <<"cre_yawl_compilations_total">>).
-define(YAWL_COMPILATION_DURATION_MS, <<"cre_yawl_compilation_duration_ms">>).
-define(YAWL_CASES_TOTAL, <<"cre_yawl_cases_total">>).
-define(YAWL_CASE_DURATION_MS, <<"cre_yawl_case_duration_ms">>).

%% System Metrics
-define(CRE_MEMORY_BYTES, <<"cre_memory_bytes">>).
-define(CRE_PROCESS_COUNT, <<"cre_process_count">>).

%%====================================================================
%% API
%%====================================================================

-spec init() -> ok.
init() ->
    application:set_env(cre, telemetry_enabled, true),
    register_all(),
    ok.

%% @doc Register all CRE metrics with OpenTelemetry.
-spec register_all() -> ok.
register_all() ->
    %% Petri Net Metrics
    ok = otel_metrics:register_counter(pnet_transitions_total(),
                                      <<"Total number of Petri net transitions fired">>),
    ok = otel_metrics:register_histogram(pnet_transition_duration_ms(),
                                        <<"Petri net transition firing duration in milliseconds">>),
    ok = otel_metrics:register_gauge(pnet_tokens(),
                                    <<"Current number of tokens in the Petri net">>),
    ok = otel_metrics:register_gauge(pnet_throughput_fps(),
                                    <<"Petri net throughput in firings per second">>),

    %% Pattern Metrics
    ok = otel_metrics:register_counter(pattern_executions_total(),
                                      <<"Total number of pattern executions">>),
    ok = otel_metrics:register_histogram(pattern_execution_duration_ms(),
                                        <<"Pattern execution duration in milliseconds">>),
    ok = otel_metrics:register_counter(pattern_errors_total(),
                                      <<"Total number of pattern execution errors">>),

    %% Mining Metrics
    ok = otel_metrics:register_histogram(mining_algorithm_duration_ms(),
                                        <<"Mining algorithm execution duration in milliseconds">>),
    ok = otel_metrics:register_counter(mining_algorithm_executions_total(),
                                      <<"Total number of mining algorithm executions">>),
    ok = otel_metrics:register_gauge(mining_discovered_places(),
                                    <<"Number of places discovered by mining">>),
    ok = otel_metrics:register_gauge(mining_discovered_transitions(),
                                    <<"Number of transitions discovered by mining">>),

    %% YAWL Metrics
    ok = otel_metrics:register_counter(yawl_compilations_total(),
                                      <<"Total number of YAWL compilations">>),
    ok = otel_metrics:register_histogram(yawl_compilation_duration_ms(),
                                        <<"YAWL compilation duration in milliseconds">>),
    ok = otel_metrics:register_counter(yawl_cases_total(),
                                      <<"Total number of YAWL cases started">>),
    ok = otel_metrics:register_histogram(yawl_case_duration_ms(),
                                        <<"YAWL case completion duration in milliseconds">>),

    %% System Metrics
    ok = otel_metrics:register_gauge(cre_memory_bytes(),
                                    <<"CRE memory usage in bytes">>),
    ok = otel_metrics:register_gauge(cre_process_count(),
                                    <<"Number of CRE processes">>),
    ok.

%%====================================================================
%% Metric Name Accessors
%%====================================================================

%% Petri Net Metrics
pnet_transitions_total() -> ?PNET_TRANSITIONS_TOTAL.
pnet_transition_duration_ms() -> ?PNET_TRANSITION_DURATION_MS.
pnet_tokens() -> ?PNET_TOKENS.
pnet_throughput_fps() -> ?PNET_THROUGHPUT_FPS.

%% Pattern Metrics
pattern_executions_total() -> ?PATTERN_EXECUTIONS_TOTAL.
pattern_execution_duration_ms() -> ?PATTERN_EXECUTION_DURATION_MS.
pattern_errors_total() -> ?PATTERN_ERRORS_TOTAL.

%% Mining Metrics
mining_algorithm_duration_ms() -> ?MINING_ALGORITHM_DURATION_MS.
mining_algorithm_executions_total() -> ?MINING_ALGORITHM_EXECUTIONS_TOTAL.
mining_discovered_places() -> ?MINING_DISCOVERED_PLACES.
mining_discovered_transitions() -> ?MINING_DISCOVERED_TRANSITIONS.

%% YAWL Metrics
yawl_compilations_total() -> ?YAWL_COMPILATIONS_TOTAL.
yawl_compilation_duration_ms() -> ?YAWL_COMPILATION_DURATION_MS.
yawl_cases_total() -> ?YAWL_CASES_TOTAL.
yawl_case_duration_ms() -> ?YAWL_CASE_DURATION_MS.

%% System Metrics
cre_memory_bytes() -> ?CRE_MEMORY_BYTES.
cre_process_count() -> ?CRE_PROCESS_COUNT.

%%====================================================================
%% Metric Recording Helpers
%%====================================================================

%% @doc Record a Petri net transition firing.
-spec transition_fired(atom(), atom()) -> ok.
transition_fired(NetMod, Transition) ->
    Labels = #{net_mod => NetMod, transition => Transition},
    otel_metrics:inc_counter(pnet_transitions_total(), Labels).

%% @doc Record a Petri net transition firing with duration.
-spec transition_fired(atom(), atom(), number()) -> ok.
transition_fired(NetMod, Transition, DurationMs) ->
    Labels = #{net_mod => NetMod, transition => Transition},
    otel_metrics:inc_counter(pnet_transitions_total(), Labels),
    otel_metrics:record_histogram(pnet_transition_duration_ms(), Labels, DurationMs).

%% @doc Record a pattern execution.
-spec pattern_executed(atom(), number()) -> ok.
pattern_executed(Pattern, DurationMs) ->
    Labels = #{pattern => Pattern},
    otel_metrics:inc_counter(pattern_executions_total(), Labels),
    otel_metrics:record_histogram(pattern_execution_duration_ms(), Labels, DurationMs).

%% @doc Record the start of a mining algorithm execution.
-spec mining_algorithm_started(atom()) -> ok.
mining_algorithm_started(Algorithm) ->
    Labels = #{algorithm => Algorithm},
    otel_metrics:inc_counter(mining_algorithm_executions_total(), Labels),
    %% Store start time in process dictionary
    put({mining_start, Algorithm}, erlang:monotonic_time(millisecond)),
    ok.

%% @doc Record the completion of a mining algorithm execution.
-spec mining_algorithm_completed(atom(), map()) -> ok.
mining_algorithm_completed(Algorithm, Result) ->
    Labels = #{algorithm => Algorithm},
    case get({mining_start, Algorithm}) of
        undefined ->
            otel_metrics:record_histogram(mining_algorithm_duration_ms(), Labels, 0);
        StartTime ->
            DurationMs = erlang:monotonic_time(millisecond) - StartTime,
            otel_metrics:record_histogram(mining_algorithm_duration_ms(), Labels, DurationMs),
            erase({mining_start, Algorithm}),
            ok
    end,
    %% Record discovered elements if available
    case maps:get(places, Result, undefined) of
        undefined -> ok;
        PlaceCount ->
            otel_metrics:set_gauge(mining_discovered_places(),
                                  #{algorithm => Algorithm},
                                  PlaceCount)
    end,
    case maps:get(transitions, Result, undefined) of
        undefined -> ok;
        TransitionCount ->
            otel_metrics:set_gauge(mining_discovered_transitions(),
                                  #{algorithm => Algorithm},
                                  TransitionCount)
    end,
    ok.

%% @doc Record a YAWL compilation.
-spec yawl_compiled(number()) -> ok.
yawl_compiled(DurationMs) ->
    Labels = #{},
    otel_metrics:inc_counter(yawl_compilations_total(), Labels),
    otel_metrics:record_histogram(yawl_compilation_duration_ms(), Labels, DurationMs).

%% @doc Record the start of a YAWL case.
-spec case_started(binary()) -> ok.
case_started(CaseId) ->
    Labels = #{case_id => CaseId},
    otel_metrics:inc_counter(yawl_cases_total(), Labels),
    put({case_start, CaseId}, erlang:monotonic_time(millisecond)),
    ok.

%% @doc Record the completion of a YAWL case.
-spec case_completed(binary(), atom()) -> ok.
case_completed(CaseId, Status) ->
    Labels = #{case_id => CaseId, status => Status},
    case get({case_start, CaseId}) of
        undefined ->
            otel_metrics:record_histogram(yawl_case_duration_ms(), Labels, 0);
        StartTime ->
            DurationMs = erlang:monotonic_time(millisecond) - StartTime,
            otel_metrics:record_histogram(yawl_case_duration_ms(), Labels, DurationMs),
            erase({case_start, CaseId}),
            ok
    end.

%% @doc Update the token count gauge.
-spec token_count(non_neg_integer()) -> ok.
token_count(Count) ->
    otel_metrics:set_gauge(pnet_tokens(), #{}, Count).

%% @doc Update the throughput gauge.
-spec pnet_throughput(float()) -> ok.
pnet_throughput(Fps) ->
    otel_metrics:set_gauge(pnet_throughput_fps(), #{}, Fps).
