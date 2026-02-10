%% -*- erlang -*-
%%%% @doc Workflow metrics collection and aggregation.
%%
%% Collects and exposes workflow metrics using ETS-based aggregation:
%% - Transition counts and latencies (min/max/avg)
%% - Pattern execution statistics
%% - Per-workflow metrics
%%
%% Metrics are stored in ETS tables for fast aggregation and querying.
%% All operations are non-blocking and use ets module directly.
%%
%% == Basic Usage ==
%%
%% ```erlang
%% > wf_metrics:start_link().
%% {ok, <0.123.0>}
%%
%% > wf_metrics:record_transition(my_workflow, t_start, 1000, 42).
%% ok
%%
%% > wf_metrics:get_transition_metrics(t_start).
%% #{count => 1, min_latency => 1000, max_latency => 1000,
%%   avg_latency => 1000.0, total_time => 1000}
%%
%% > wf_metrics:get_workflow_metrics(my_workflow).
%% #{workflows => 1, transitions => 1, total_time => 1000}
%% ```
%%
%% == Pattern Execution Stats ==
%%
%% ```erlang
%% > wf_metrics:record_pattern_execution(
%%     my_wf, arbitrary_cycles, 5000, 100, #{nodes => 3}).
%% ok
%%
%% > wf_metrics:get_pattern_metrics(arbitrary_cycles).
%% #{count => 1, executions => 1, total_time => 5000,
%%   avg_time => 5000.0, error_count => 0}
%% ```
%%
%% == Aggregation Operations ==
%%
%% All metrics support aggregation across:
%% - All transitions globally
%% - All patterns globally
%% - Per-workflow metrics
%% - Per-pattern metrics
%% - Per-transition metrics
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_metrics).
-behaviour(gen_server).

%%====================================================================
%% API Exports
%%====================================================================

-export([
    start_link/0,
    record_transition/4,
    record_pattern_execution/5,
    record_workflow_start/2,
    record_workflow_complete/3,
    get_transition_metrics/1,
    get_pattern_metrics/1,
    get_workflow_metrics/1,
    get_all_metrics/0,
    reset_metrics/0,
    dump_metrics/0,
    list_transitions/0,
    list_patterns/0,
    list_workflows/0
]).

%%====================================================================
%% gen_server Exports
%%====================================================================

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type workflow_id() :: term().
-type transition_id() :: atom().
-type pattern_id() :: atom().
-type latency_ms() :: non_neg_integer().
-type exec_time_ms() :: non_neg_integer().
-type token_count() :: non_neg_integer().

-type transition_metric() :: #{
    count => non_neg_integer(),
    min_latency => latency_ms(),
    max_latency => latency_ms(),
    avg_latency => float(),
    total_time => non_neg_integer()
}.

-type pattern_metric() :: #{
    count => non_neg_integer(),
    executions => non_neg_integer(),
    total_time => non_neg_integer(),
    avg_time => float(),
    error_count => non_neg_integer(),
    attributes => map()
}.

-type workflow_metric() :: #{
    workflows => non_neg_integer(),
    transitions => non_neg_integer(),
    patterns => non_neg_integer(),
    total_time => non_neg_integer(),
    start_time => integer() | undefined,
    end_time => integer() | undefined,
    duration => non_neg_integer() | undefined
}.

-export_type([
    workflow_id/0,
    transition_id/0,
    pattern_id/0,
    latency_ms/0,
    exec_time_ms/0,
    transition_metric/0,
    pattern_metric/0,
    workflow_metric/0
]).

%%====================================================================
%% Internal Records
%%====================================================================

-record(state, {
    transitions_table :: ets:table(),
    patterns_table :: ets:table(),
    workflows_table :: ets:table()
}).

-record(transition_stat, {
    key :: {transition_id(), workflow_id()},
    count = 0 :: non_neg_integer(),
    min_latency = infinity :: latency_ms() | infinity,
    max_latency = 0 :: latency_ms(),
    sum_latency = 0 :: non_neg_integer(),
    sum_tokens = 0 :: non_neg_integer()
}).

-record(pattern_stat, {
    key :: {pattern_id(), workflow_id()},
    count = 0 :: non_neg_integer(),
    executions = 0 :: non_neg_integer(),
    sum_time = 0 :: non_neg_integer(),
    min_time = infinity :: exec_time_ms() | infinity,
    max_time = 0 :: exec_time_ms(),
    error_count = 0 :: non_neg_integer(),
    attributes = #{} :: map()
}).

-record(workflow_stat, {
    key :: workflow_id(),
    transition_count = 0 :: non_neg_integer(),
    pattern_count = 0 :: non_neg_integer(),
    sum_time = 0 :: non_neg_integer(),
    start_time :: integer() | undefined,
    end_time :: integer() | undefined
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the metrics server.
%%
%% Creates ETS tables for storing metrics and registers the server
%% as `wf_metrics'. All metrics are stored persistently in ETS and
%% can be queried at any time.
%%
%% @returns `{ok, Pid}' on success, `{error, Reason}' on failure
%%
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Records a transition firing with latency metrics.
%%
%% Records when a transition fires, including the latency (time to execute)
%% and token count produced. Metrics are aggregated per transition per workflow.
%%
%% @param WorkflowId Identifier for the workflow instance
%% @param TransitionId Name of the transition that fired
%% @param LatencyMs Time in milliseconds for the transition to fire
%% @param TokenCount Number of tokens produced by this firing
%% @returns `ok'
%%
-spec record_transition(workflow_id(), transition_id(), latency_ms(), token_count()) -> ok.
record_transition(WorkflowId, TransitionId, LatencyMs, TokenCount) ->
    gen_server:cast(?MODULE, {record_transition, WorkflowId, TransitionId, LatencyMs, TokenCount}).

%% @doc Records pattern execution statistics.
%%
%% Tracks execution of a pattern within a workflow, including execution time,
%% token count, and optional attributes for detailed analysis.
%%
%% @param WorkflowId Identifier for the workflow instance
%% @param PatternId Name of the pattern executed
%% @param ExecTimeMs Total execution time in milliseconds
%% @param TokenCount Number of tokens involved in execution
%% @param Attributes Optional map with pattern-specific attributes
%% @returns `ok'
%%
-spec record_pattern_execution(workflow_id(), pattern_id(), exec_time_ms(), token_count(), map()) -> ok.
record_pattern_execution(WorkflowId, PatternId, ExecTimeMs, TokenCount, Attributes) ->
    gen_server:cast(?MODULE, {record_pattern_execution, WorkflowId, PatternId, ExecTimeMs, TokenCount, Attributes}).

%% @doc Records workflow start time.
%%
%% Marks the beginning of a workflow instance for duration tracking.
%%
%% @param WorkflowId Identifier for the workflow instance
%% @param Timestamp Timestamp in milliseconds (from erlang:system_time(millisecond))
%% @returns `ok'
%%
-spec record_workflow_start(workflow_id(), integer()) -> ok.
record_workflow_start(WorkflowId, Timestamp) ->
    gen_server:cast(?MODULE, {record_workflow_start, WorkflowId, Timestamp}).

%% @doc Records workflow completion.
%%
%% Marks the end of a workflow instance, allowing duration calculation.
%%
%% @param WorkflowId Identifier for the workflow instance
%% @param Timestamp Timestamp in milliseconds
%% @param Success Whether the workflow completed successfully
%% @returns `ok'
%%
-spec record_workflow_complete(workflow_id(), integer(), boolean()) -> ok.
record_workflow_complete(WorkflowId, Timestamp, Success) ->
    gen_server:cast(?MODULE, {record_workflow_complete, WorkflowId, Timestamp, Success}).

%% @doc Retrieves aggregated metrics for a specific transition.
%%
%% Returns count, latency statistics (min/max/avg), and total execution time
%% across all workflows where this transition executed.
%%
%% @param TransitionId Name of the transition
%% @returns Metric map with count, latency stats, and total time
%%
-spec get_transition_metrics(transition_id()) -> transition_metric().
get_transition_metrics(TransitionId) ->
    case ets:lookup(get_table_name(transitions), TransitionId) of
        [] ->
            #{count => 0, min_latency => 0, max_latency => 0, avg_latency => 0.0, total_time => 0};
        [Record] ->
            case Record#transition_stat.count of
                0 ->
                    #{count => 0, min_latency => 0, max_latency => 0, avg_latency => 0.0, total_time => 0};
                Count ->
                    AvgLatency = Record#transition_stat.sum_latency / Count,
                    MinLatency = case Record#transition_stat.min_latency of
                        infinity -> 0;
                        Min -> Min
                    end,
                    #{
                        count => Count,
                        min_latency => MinLatency,
                        max_latency => Record#transition_stat.max_latency,
                        avg_latency => AvgLatency,
                        total_time => Record#transition_stat.sum_latency
                    }
            end
    end.

%% @doc Retrieves aggregated metrics for a specific pattern.
%%
%% Returns execution count, timing statistics, error count, and aggregated
%% attributes across all workflow instances.
%%
%% @param PatternId Name of the pattern
%% @returns Metric map with execution stats, timings, and error count
%%
-spec get_pattern_metrics(pattern_id()) -> pattern_metric().
get_pattern_metrics(PatternId) ->
    case ets:lookup(get_table_name(patterns), PatternId) of
        [] ->
            #{count => 0, executions => 0, total_time => 0, avg_time => 0.0, error_count => 0};
        [Record] ->
            case Record#pattern_stat.count of
                0 ->
                    #{count => 0, executions => 0, total_time => 0, avg_time => 0.0, error_count => 0};
                Count ->
                    AvgTime = Record#pattern_stat.sum_time / Count,
                    #{
                        count => Count,
                        executions => Record#pattern_stat.executions,
                        total_time => Record#pattern_stat.sum_time,
                        avg_time => AvgTime,
                        error_count => Record#pattern_stat.error_count,
                        attributes => Record#pattern_stat.attributes
                    }
            end
    end.

%% @doc Retrieves aggregated metrics for a specific workflow.
%%
%% Returns summary statistics including transition count, pattern count,
%% total execution time, and workflow duration.
%%
%% @param WorkflowId Identifier for the workflow instance
%% @returns Metric map with workflow statistics and duration
%%
-spec get_workflow_metrics(workflow_id()) -> workflow_metric().
get_workflow_metrics(WorkflowId) ->
    case ets:lookup(get_table_name(workflows), WorkflowId) of
        [] ->
            #{workflows => 0, transitions => 0, patterns => 0, total_time => 0};
        [Record] ->
            Duration = case {Record#workflow_stat.start_time, Record#workflow_stat.end_time} of
                {undefined, _} -> undefined;
                {_, undefined} -> undefined;
                {Start, End} -> End - Start
            end,
            #{
                workflows => 1,
                transitions => Record#workflow_stat.transition_count,
                patterns => Record#workflow_stat.pattern_count,
                total_time => Record#workflow_stat.sum_time,
                start_time => Record#workflow_stat.start_time,
                end_time => Record#workflow_stat.end_time,
                duration => Duration
            }
    end.

%% @doc Retrieves all aggregated metrics.
%%
%% Returns a map containing all transitions, patterns, and workflows with
%% their respective metrics aggregated across all instances.
%%
%% @returns Map with keys for transitions, patterns, workflows
%%
-spec get_all_metrics() -> #{
    transitions => #{transition_id() => transition_metric()},
    patterns => #{pattern_id() => pattern_metric()},
    workflows => #{workflow_id() => workflow_metric()}
}.
get_all_metrics() ->
    TransTable = get_table_name(transitions),
    PatTable = get_table_name(patterns),
    WfTable = get_table_name(workflows),

    Transitions = aggregate_transition_metrics(ets:tab2list(TransTable), #{}),
    Patterns = aggregate_pattern_metrics(ets:tab2list(PatTable), #{}),
    Workflows = aggregate_workflow_metrics(ets:tab2list(WfTable), #{}),

    #{
        transitions => Transitions,
        patterns => Patterns,
        workflows => Workflows
    }.

%% @doc Lists all transitions that have been recorded.
%%
%% @returns List of transition identifiers
%%
-spec list_transitions() -> [transition_id()].
list_transitions() ->
    TransTable = get_table_name(transitions),
    [record_to_transition_id(R) || R <- ets:tab2list(TransTable)].

%% @doc Lists all patterns that have been recorded.
%%
%% @returns List of pattern identifiers
%%
-spec list_patterns() -> [pattern_id()].
list_patterns() ->
    PatTable = get_table_name(patterns),
    [record_to_pattern_id(R) || R <- ets:tab2list(PatTable)].

%% @doc Lists all workflows that have been recorded.
%%
%% @returns List of workflow identifiers
%%
-spec list_workflows() -> [workflow_id()].
list_workflows() ->
    WfTable = get_table_name(workflows),
    [R#workflow_stat.key || R <- ets:tab2list(WfTable)].

%% @doc Resets all metrics to empty state.
%%
%% Clears all ETS tables. Should be called before starting a new
%% metrics collection session.
%%
%% @returns `ok'
%%
-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

%% @doc Dumps all metrics to logger output.
%%
%% Pretty-prints all aggregated metrics using OTP logger.
%% Useful for debugging and monitoring.
%%
%% @returns `ok'
%%
-spec dump_metrics() -> ok.
dump_metrics() ->
    gen_server:call(?MODULE, dump_metrics).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init(list()) -> {ok, #state{}}.
init(_Args) ->
    TransTable = ets:new(wf_metrics_transitions, [
        {keypos, #transition_stat.key},
        public,
        named_table
    ]),
    PatTable = ets:new(wf_metrics_patterns, [
        {keypos, #pattern_stat.key},
        public,
        named_table
    ]),
    WfTable = ets:new(wf_metrics_workflows, [
        {keypos, #workflow_stat.key},
        public,
        named_table
    ]),
    {ok, #state{
        transitions_table = TransTable,
        patterns_table = PatTable,
        workflows_table = WfTable
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(reset_metrics, _From, State) ->
    ets:delete_all_objects(State#state.transitions_table),
    ets:delete_all_objects(State#state.patterns_table),
    ets:delete_all_objects(State#state.workflows_table),
    {reply, ok, State};

handle_call(dump_metrics, _From, State) ->
    Metrics = get_all_metrics(),
    logger:info("Workflow Metrics Dump:~n~p", [Metrics]),
    {reply, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({record_transition, WorkflowId, TransitionId, LatencyMs, TokenCount}, State) ->
    TransTable = State#state.transitions_table,

    %% Update aggregated transition metrics (key is just TransitionId)
    TransKey = TransitionId,
    case ets:lookup(TransTable, TransKey) of
        [] ->
            Stat = #transition_stat{
                key = TransKey,
                count = 1,
                min_latency = LatencyMs,
                max_latency = LatencyMs,
                sum_latency = LatencyMs,
                sum_tokens = TokenCount
            },
            ets:insert(TransTable, Stat);
        [OldStat] ->
            NewStat = OldStat#transition_stat{
                count = OldStat#transition_stat.count + 1,
                min_latency = min(OldStat#transition_stat.min_latency, LatencyMs),
                max_latency = max(OldStat#transition_stat.max_latency, LatencyMs),
                sum_latency = OldStat#transition_stat.sum_latency + LatencyMs,
                sum_tokens = OldStat#transition_stat.sum_tokens + TokenCount
            },
            ets:insert(TransTable, NewStat)
    end,

    %% Update workflow metrics
    update_workflow_metrics(State, WorkflowId, 1, 0, LatencyMs),

    {noreply, State};

handle_cast({record_pattern_execution, WorkflowId, PatternId, ExecTimeMs, TokenCount, Attributes}, State) ->
    PatTable = State#state.patterns_table,

    %% Update aggregated pattern metrics
    PatKey = PatternId,
    case ets:lookup(PatTable, PatKey) of
        [] ->
            Stat = #pattern_stat{
                key = PatKey,
                count = 1,
                executions = 1,
                sum_time = ExecTimeMs,
                min_time = ExecTimeMs,
                max_time = ExecTimeMs,
                attributes = Attributes
            },
            ets:insert(PatTable, Stat);
        [OldStat] ->
            NewAttrs = merge_attributes(OldStat#pattern_stat.attributes, Attributes),
            NewStat = OldStat#pattern_stat{
                count = OldStat#pattern_stat.count + 1,
                executions = OldStat#pattern_stat.executions + 1,
                sum_time = OldStat#pattern_stat.sum_time + ExecTimeMs,
                min_time = min(OldStat#pattern_stat.min_time, ExecTimeMs),
                max_time = max(OldStat#pattern_stat.max_time, ExecTimeMs),
                attributes = NewAttrs
            },
            ets:insert(PatTable, NewStat)
    end,

    %% Update workflow metrics
    update_workflow_metrics(State, WorkflowId, 0, 1, ExecTimeMs),

    {noreply, State};

handle_cast({record_workflow_start, WorkflowId, Timestamp}, State) ->
    WfTable = State#state.workflows_table,
    case ets:lookup(WfTable, WorkflowId) of
        [] ->
            Stat = #workflow_stat{
                key = WorkflowId,
                start_time = Timestamp
            },
            ets:insert(WfTable, Stat);
        [OldStat] ->
            NewStat = OldStat#workflow_stat{
                start_time = case OldStat#workflow_stat.start_time of
                    undefined -> Timestamp;
                    Existing -> Existing
                end
            },
            ets:insert(WfTable, NewStat)
    end,
    {noreply, State};

handle_cast({record_workflow_complete, WorkflowId, Timestamp, _Success}, State) ->
    WfTable = State#state.workflows_table,
    case ets:lookup(WfTable, WorkflowId) of
        [] ->
            Stat = #workflow_stat{
                key = WorkflowId,
                end_time = Timestamp
            },
            ets:insert(WfTable, Stat);
        [OldStat] ->
            NewStat = OldStat#workflow_stat{
                end_time = Timestamp
            },
            ets:insert(WfTable, NewStat)
    end,
    {noreply, State};

handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%% Get the actual ETS table name
-spec get_table_name(transitions | patterns | workflows) -> atom().
get_table_name(transitions) -> wf_metrics_transitions;
get_table_name(patterns) -> wf_metrics_patterns;
get_table_name(workflows) -> wf_metrics_workflows.

%% Update workflow-level metrics
-spec update_workflow_metrics(#state{}, workflow_id(), integer(), integer(), non_neg_integer()) -> ok.
update_workflow_metrics(State, WorkflowId, TransitionDelta, PatternDelta, TimeDelta) ->
    WfTable = State#state.workflows_table,
    case ets:lookup(WfTable, WorkflowId) of
        [] ->
            Stat = #workflow_stat{
                key = WorkflowId,
                transition_count = TransitionDelta,
                pattern_count = PatternDelta,
                sum_time = TimeDelta
            },
            ets:insert(WfTable, Stat);
        [OldStat] ->
            NewStat = OldStat#workflow_stat{
                transition_count = OldStat#workflow_stat.transition_count + TransitionDelta,
                pattern_count = OldStat#workflow_stat.pattern_count + PatternDelta,
                sum_time = OldStat#workflow_stat.sum_time + TimeDelta
            },
            ets:insert(WfTable, NewStat)
    end,
    ok.

%% Merge attributes from multiple pattern executions
-spec merge_attributes(map(), map()) -> map().
merge_attributes(Acc, New) ->
    maps:merge(Acc, New).

%% Extract transition ID from record
-spec record_to_transition_id(#transition_stat{}) -> transition_id().
record_to_transition_id(Record) ->
    Record#transition_stat.key.

%% Extract pattern ID from record
-spec record_to_pattern_id(#pattern_stat{}) -> pattern_id().
record_to_pattern_id(Record) ->
    Record#pattern_stat.key.

%% Aggregate transition metrics from list
-spec aggregate_transition_metrics(list(), map()) -> map().
aggregate_transition_metrics([], Acc) ->
    Acc;
aggregate_transition_metrics([Record | Rest], Acc) ->
    TransId = record_to_transition_id(Record),
    Count = Record#transition_stat.count,
    MinLatency = case Record#transition_stat.min_latency of
        infinity -> 0;
        Min -> Min
    end,
    AvgLatency = case Count of
        0 -> 0.0;
        _ -> Record#transition_stat.sum_latency / Count
    end,
    Metric = #{
        count => Count,
        min_latency => MinLatency,
        max_latency => Record#transition_stat.max_latency,
        avg_latency => AvgLatency,
        total_time => Record#transition_stat.sum_latency
    },
    aggregate_transition_metrics(Rest, Acc#{TransId => Metric}).

%% Aggregate pattern metrics from list
-spec aggregate_pattern_metrics(list(), map()) -> map().
aggregate_pattern_metrics([], Acc) ->
    Acc;
aggregate_pattern_metrics([Record | Rest], Acc) ->
    PatId = record_to_pattern_id(Record),
    Count = Record#pattern_stat.count,
    AvgTime = case Count of
        0 -> 0.0;
        _ -> Record#pattern_stat.sum_time / Count
    end,
    Metric = #{
        count => Count,
        executions => Record#pattern_stat.executions,
        total_time => Record#pattern_stat.sum_time,
        avg_time => AvgTime,
        error_count => Record#pattern_stat.error_count,
        attributes => Record#pattern_stat.attributes
    },
    aggregate_pattern_metrics(Rest, Acc#{PatId => Metric}).

%% Aggregate workflow metrics from list
-spec aggregate_workflow_metrics(list(), map()) -> map().
aggregate_workflow_metrics([], Acc) ->
    Acc;
aggregate_workflow_metrics([Record | Rest], Acc) ->
    WfId = Record#workflow_stat.key,
    Duration = case {Record#workflow_stat.start_time, Record#workflow_stat.end_time} of
        {undefined, _} -> undefined;
        {_, undefined} -> undefined;
        {Start, End} -> End - Start
    end,
    Metric = #{
        workflows => 1,
        transitions => Record#workflow_stat.transition_count,
        patterns => Record#workflow_stat.pattern_count,
        total_time => Record#workflow_stat.sum_time,
        start_time => Record#workflow_stat.start_time,
        end_time => Record#workflow_stat.end_time,
        duration => Duration
    },
    aggregate_workflow_metrics(Rest, Acc#{WfId => Metric}).
