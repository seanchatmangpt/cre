-module(yawl_telemetry).
-behaviour(gen_server).

%%====================================================================
%% API Exports
%%====================================================================
-export([
    start_link/0,
    start_span/4,
    end_span/2,
    emit_event/4,
    track_task/4,
    get_metrics/2,
    set_verbosity/1,
    flush/0,
    get_span_context/1
]).

%%====================================================================
%% gen_server Callbacks
%%====================================================================
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Type Definitions
%%====================================================================
-type spec_id() :: binary().
-type case_id() :: binary().
-type task_id() :: binary().
-type span_name() :: binary().
-type span_ctx() :: #{
    id => binary(),
    name => binary(),
    spec_id => spec_id(),
    case_id => case_id(),
    task_id => task_id() | undefined,
    start_time => integer(),
    attributes => map()
}.
-type event_type() :: task_start | task_complete | task_fail | task_cancel |
                      workflow_start | workflow_complete | workflow_fail |
                      checkpoint_created | checkpoint_restored.
-type status() :: success | failure | timeout | cancelled.
-type verbosity() :: silent | minimal | normal | verbose | debug.
-type metrics() :: #{
    spec_id => spec_id(),
    case_id => case_id(),
    total_tasks => non_neg_integer(),
    completed_tasks => non_neg_integer(),
    failed_tasks => non_neg_integer(),
    cancelled_tasks => non_neg_integer(),
    duration_ms => non_neg_integer(),
    start_time => integer(),
    end_time => integer() | undefined
}.

-type reason() :: term().

-export_type([
    spec_id/0,
    case_id/0,
    task_id/0,
    span_ctx/0,
    event_type/0,
    status/0,
    verbosity/0,
    metrics/0
]).

%%====================================================================
%% Records
%%====================================================================
-record(state, {
    verbosity = normal :: verbosity(),
    metrics = #{} :: #{spec_id() => #{case_id() => metrics()}},
    span_contexts = #{} :: #{binary() => span_ctx()},
    otel_available = false :: boolean(),
    event_queue = [] :: list()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the telemetry server.
-spec start_link() -> {ok, pid()} | {error, reason()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Starts a new OpenTelemetry span for workflow activity.
%%
%% Creates a span with attributes:
%%   - spec_id: Workflow specification identifier
%%   - case_id: Workflow case instance identifier
%%   - task_id: Task identifier (optional)
%%   - span_name: Human-readable span name
%%
%% Returns span context for linking events and ending the span.
%%
%% Example:
%% ```
%% {ok, SpanCtx} = yawl_telemetry:start_span(
%%     <<"order_fulfillment">>,
%%     <<"case-123">>,
%%     <<"payment_task">>,
%%     <<"Execute Payment">
%% ),
%% % ... do work ...
%% yawl_telemetry:end_span(SpanCtx, success).
%% ```
-spec start_span(SpecId, CaseId, TaskId, SpanName) ->
    {ok, span_ctx()} | {error, reason()} when
    SpecId :: spec_id(),
    CaseId :: case_id(),
    TaskId :: task_id() | undefined,
    SpanName :: span_name().
start_span(SpecId, CaseId, TaskId, SpanName) ->
    gen_server:call(?MODULE, {start_span, SpecId, CaseId, TaskId, SpanName}).

%% @doc Ends a span with completion status.
%%
%% Marks the span as complete with the given status:
%%   - success: Operation completed successfully
%%   - failure: Operation failed with error
%%   - timeout: Operation timed out
%%   - cancelled: Operation was cancelled
%%
%% Emits span end event and calculates duration.
%%
%% Example:
%% ```
%% yawl_telemetry:end_span(SpanCtx, success).
%% ```
-spec end_span(SpanCtx, Status) -> ok when
    SpanCtx :: span_ctx(),
    Status :: status().
end_span(SpanCtx, Status) ->
    gen_server:call(?MODULE, {end_span, SpanCtx, Status}).

%% @doc Emits a workflow event without creating a span.
%%
%% Use for lightweight events that don't require span tracking.
%% Event types:
%%   - task_start: Task execution started
%%   - task_complete: Task completed successfully
%%   - task_fail: Task failed with error
%%   - task_cancel: Task was cancelled
%%   - workflow_start: Workflow instance started
%%   - workflow_complete: Workflow completed successfully
%%   - workflow_fail: Workflow failed
%%   - checkpoint_created: Checkpoint was created
%%   - checkpoint_restored: Workflow was restored from checkpoint
%%
%% Example:
%% ```
%% yawl_telemetry:emit_event(
%%     <<"order_fulfillment">>,
%%     <<"case-123">>,
%%     task_start,
%%     #{task_id => <<"payment">>, worker_id => <<"worker-1">>}
%% ).
%% ```
-spec emit_event(SpecId, CaseId, EventType, Attributes) -> ok when
    SpecId :: spec_id(),
    CaseId :: case_id(),
    EventType :: event_type(),
    Attributes :: map().
emit_event(SpecId, CaseId, EventType, Attributes) ->
    gen_server:cast(?MODULE, {emit_event, SpecId, CaseId, EventType, Attributes}).

%% @doc Tracks task execution status with metrics.
%%
%% Updates metrics counters and emits telemetry event.
%% Tracks:
%%   - Total tasks executed
%%   - Completed tasks
%%   - Failed tasks
%%   - Cancelled tasks
%%
%% Example:
%% ```
%% yawl_telemetry:track_task(
%%     <<"order_fulfillment">>,
%%     <<"case-123">>,
%%     <<"payment_task">>,
%%     success
%% ).
%% ```
-spec track_task(SpecId, CaseId, TaskId, Status) -> ok when
    SpecId :: spec_id(),
    CaseId :: case_id(),
    TaskId :: task_id(),
    Status :: status().
track_task(SpecId, CaseId, TaskId, Status) ->
    gen_server:cast(?MODULE, {track_task, SpecId, CaseId, TaskId, Status}).

%% @doc Returns aggregated metrics for workflow instance.
%%
%% Provides comprehensive metrics including:
%%   - Total tasks executed
%%   - Completed tasks count
%%   - Failed tasks count
%%   - Cancelled tasks count
%%   - Duration in milliseconds
%%   - Start and end timestamps
%%
%% Example:
%% ```
%% {ok, Metrics} = yawl_telemetry:get_metrics(
%%     <<"order_fulfillment">>,
%%     <<"case-123">>
%% ),
%% io:format("Completed: ~p/~p~n", [
%%     maps:get(completed_tasks, Metrics),
%%     maps:get(total_tasks, Metrics)
%% ]).
%% ```
-spec get_metrics(SpecId, CaseId) -> {ok, metrics()} | {error, not_found} when
    SpecId :: spec_id(),
    CaseId :: case_id().
get_metrics(SpecId, CaseId) ->
    gen_server:call(?MODULE, {get_metrics, SpecId, CaseId}).

%% @doc Sets the telemetry verbosity level.
%%
%% Controls amount of telemetry emitted:
%%   - silent: No telemetry emitted
%%   - minimal: Only critical errors
%%   - normal: Errors and important events (default)
%%   - verbose: All events and warnings
%%   - debug: Everything including internal operations
%%
%% Example:
%% ```
%% yawl_telemetry:set_verbosity(debug).
%% ```
-spec set_verbosity(Verbosity) -> ok when
    Verbosity :: verbosity().
set_verbosity(Verbosity) ->
    gen_server:call(?MODULE, {set_verbosity, Verbosity}).

%% @doc Flushes any queued telemetry events.
%%
%% Forces immediate emission of queued events.
%% Useful for ensuring all telemetry is sent before shutdown.
%%
%% Example:
%% ```
%% yawl_telemetry:flush().
%% ```
-spec flush() -> ok.
flush() ->
    gen_server:call(?MODULE, flush).

%% @doc Extracts span context ID from span context map.
%%
%% Returns the unique identifier for the span.
%%
%% Example:
%% ```
%% SpanId = yawl_telemetry:get_span_context(SpanCtx).
%% ```
-spec get_span_context(SpanCtx) -> binary() when
    SpanCtx :: span_ctx().
get_span_context(#{id := Id}) -> Id;
get_span_context(_) -> <<>>.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init([]) ->
    %% Check if OpenTelemetry is available
    OtelAvailable = check_otel_available(),
    State = #state{
        otel_available = OtelAvailable
    },
    {ok, State}.

%% @private
handle_call({start_span, SpecId, CaseId, TaskId, SpanName}, _From, State) ->
    SpanId = generate_span_id(),
    StartTime = erlang:system_time(millisecond),

    SpanCtx = #{
        id => SpanId,
        name => SpanName,
        spec_id => SpecId,
        case_id => CaseId,
        task_id => TaskId,
        start_time => StartTime,
        attributes => #{
            spec_id => SpecId,
            case_id => CaseId,
            task_id => TaskId,
            span_name => SpanName
        }
    },

    %% Emit to OpenTelemetry if available
    State1 = maybe_emit_span_start(SpanCtx, State),

    %% Store span context
    Spans = maps:put(SpanId, SpanCtx, State#state.span_contexts),
    {reply, {ok, SpanCtx}, State1#state{span_contexts = Spans}};

handle_call({end_span, SpanCtx, Status}, _From, State) ->
    SpanId = maps:get(id, SpanCtx, <<>>),
    EndTime = erlang:system_time(millisecond),
    Duration = EndTime - maps:get(start_time, SpanCtx, EndTime),

    SpanCtx1 = SpanCtx#{
        end_time => EndTime,
        duration => Duration,
        status => Status
    },

    %% Emit to OpenTelemetry if available
    State1 = maybe_emit_span_end(SpanCtx1, State),

    %% Remove from active spans
    Spans = maps:remove(SpanId, State#state.span_contexts),
    {reply, ok, State1#state{span_contexts = Spans}};

handle_call({get_metrics, SpecId, CaseId}, _From, State) ->
    Result = case maps:find(SpecId, State#state.metrics) of
        {ok, Cases} ->
            case maps:find(CaseId, Cases) of
                {ok, Metrics} -> {ok, Metrics};
                error -> {error, not_found}
            end;
        error ->
            {error, not_found}
    end,
    {reply, Result, State};

handle_call({set_verbosity, Verbosity}, _From, State) ->
    {reply, ok, State#state{verbosity = Verbosity}};

handle_call(flush, _From, State) ->
    %% Process any queued events
    State1 = process_queue(State),
    {reply, ok, State1};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({emit_event, SpecId, CaseId, EventType, Attributes}, State) ->
    State1 = do_emit_event(SpecId, CaseId, EventType, Attributes, State),
    {noreply, State1};

handle_cast({track_task, SpecId, CaseId, TaskId, Status}, State) ->
    State1 = do_track_task(SpecId, CaseId, TaskId, Status, State),
    {noreply, State1};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Checks if OpenTelemetry is available in the system.
check_otel_available() ->
    case code:is_loaded(opentelemetry) of
        false ->
            %% Check if the module exists but isn't loaded
            case code:load_file(opentelemetry) of
                {module, _} -> true;
                {error, _} -> false
            end;
        _ -> true
    end.

%% @private
%% @doc Generates a unique span ID.
generate_span_id() ->
    Bin = term_to_binary({self(), erlang:unique_integer([positive])}),
    base64:encode(Bin).

%% @private
%% @doc Emits span start if OpenTelemetry is available and verbosity allows.
maybe_emit_span_start(SpanCtx, State) ->
    case should_emit(normal, State) of
        true ->
            otel_emit_span_start(SpanCtx, State);
        false ->
            State
    end.

%% @private
%% @doc Emits span end if OpenTelemetry is available and verbosity allows.
maybe_emit_span_end(SpanCtx, State) ->
    case should_emit(normal, State) of
        true ->
            otel_emit_span_end(SpanCtx, State);
        false ->
            State
    end.

%% @private
%% @doc Checks if event should be emitted based on verbosity.
should_emit(_Level, #state{verbosity = silent}) -> false;
should_emit(_Level, #state{verbosity = debug}) -> true;
should_emit(minimal, #state{verbosity = minimal}) -> true;
should_emit(minimal, #state{verbosity = normal}) -> true;
should_emit(minimal, #state{verbosity = verbose}) -> true;
should_emit(normal, #state{verbosity = normal}) -> true;
should_emit(normal, #state{verbosity = verbose}) -> true;
should_emit(verbose, #state{verbosity = verbose}) -> true;
should_emit(_, _) -> false.

%% @private
%% @doc Emits span start to OpenTelemetry (stub for actual implementation).
otel_emit_span_start(SpanCtx, State) ->
    case State#state.otel_available of
        true ->
            %% Stub: In real implementation, would call opentelemetry:start_span
            Attrs = maps:get(attributes, SpanCtx, #{}),
            logger:debug("Starting span: ~p with attributes: ~p", [
                maps:get(name, SpanCtx), Attrs
            ]);
        false ->
            ok
    end,
    State.

%% @private
%% @doc Emits span end to OpenTelemetry (stub for actual implementation).
otel_emit_span_end(SpanCtx, State) ->
    case State#state.otel_available of
        true ->
            %% Stub: In real implementation, would call opentelemetry:end_span
            Duration = maps:get(duration, SpanCtx, 0),
            Status = maps:get(status, SpanCtx, unknown),
            logger:debug("Ending span: ~p after ~pms with status: ~p", [
                maps:get(name, SpanCtx), Duration, Status
            ]);
        false ->
            ok
    end,
    State.

%% @private
%% @doc Emits a workflow event.
do_emit_event(SpecId, CaseId, EventType, Attributes, State) ->
    case should_emit_for_event(EventType, State) of
        true ->
            emit_to_otel(SpecId, CaseId, EventType, Attributes, State),
            log_event(EventType, SpecId, CaseId, Attributes);
        false ->
            ok
    end,
    State.

%% @private
%% @doc Checks if event should be emitted based on event type and verbosity.
should_emit_for_event(task_start, #state{verbosity = V}) ->
    V =:= verbose orelse V =:= debug;
should_emit_for_event(task_complete, #state{verbosity = V}) ->
    V =:= normal orelse V =:= verbose orelse V =:= debug;
should_emit_for_event(task_fail, #state{verbosity = V}) ->
    V =:= minimal orelse V =:= normal orelse V =:= verbose orelse V =:= debug;
should_emit_for_event(task_cancel, #state{verbosity = V}) ->
    V =:= normal orelse V =:= verbose orelse V =:= debug;
should_emit_for_event(workflow_start, #state{verbosity = V}) ->
    V =:= normal orelse V =:= verbose orelse V =:= debug;
should_emit_for_event(workflow_complete, #state{verbosity = V}) ->
    V =:= normal orelse V =:= verbose orelse V =:= debug;
should_emit_for_event(workflow_fail, #state{verbosity = V}) ->
    V =:= minimal orelse V =:= normal orelse V =:= verbose orelse V =:= debug;
should_emit_for_event(checkpoint_created, #state{verbosity = V}) ->
    V =:= verbose orelse V =:= debug;
should_emit_for_event(checkpoint_restored, #state{verbosity = V}) ->
    V =:= verbose orelse V =:= debug.

%% @private
%% @doc Emits event to OpenTelemetry (stub for actual implementation).
emit_to_otel(_SpecId, _CaseId, _EventType, _Attributes, _State) ->
    %% Stub: In real implementation, would emit to opentelemetry
    ok.

%% @private
%% @doc Logs event based on verbosity.
log_event(EventType, SpecId, CaseId, Attributes) ->
    case EventType of
        task_fail ->
            logger:error("Task failed [~p:~p]: ~p", [SpecId, CaseId, Attributes]);
        workflow_fail ->
            logger:error("Workflow failed [~p:~p]: ~p", [SpecId, CaseId, Attributes]);
        task_complete ->
            logger:info("Task completed [~p:~p]: ~p", [SpecId, CaseId, Attributes]);
        workflow_complete ->
            logger:info("Workflow completed [~p:~p]", [SpecId, CaseId]);
        _ ->
            logger:debug("Event [~p:~p]: ~p", [SpecId, CaseId, EventType])
    end.

%% @private
%% @doc Tracks task status and updates metrics.
do_track_task(SpecId, CaseId, _TaskId, Status, State) ->
    Metrics0 = maps:get(SpecId, State#state.metrics, #{}),
    Metrics1 = maps:get(CaseId, Metrics0, init_metrics(SpecId, CaseId)),

    Metrics2 = case Status of
        success ->
            maps:update_with(
                completed_tasks,
                fun(X) -> X + 1 end,
                1,
                Metrics1
            );
        failure ->
            maps:update_with(
                failed_tasks,
                fun(X) -> X + 1 end,
                1,
                Metrics1
            );
        timeout ->
            maps:update_with(
                failed_tasks,
                fun(X) -> X + 1 end,
                1,
                Metrics1
            );
        cancelled ->
            maps:update_with(
                cancelled_tasks,
                fun(X) -> X + 1 end,
                1,
                Metrics1
            )
    end,

    Metrics3 = maps:update_with(
        total_tasks,
        fun(X) -> X + 1 end,
        1,
        Metrics2
    ),

    MetricsMap = maps:put(CaseId, Metrics3, State#state.metrics),
    State#state{metrics = maps:put(SpecId, MetricsMap, MetricsMap)}.

%% @private
%% @doc Initializes metrics for a workflow instance.
init_metrics(SpecId, CaseId) ->
    #{
        spec_id => SpecId,
        case_id => CaseId,
        total_tasks => 0,
        completed_tasks => 0,
        failed_tasks => 0,
        cancelled_tasks => 0,
        duration_ms => 0,
        start_time => erlang:system_time(millisecond),
        end_time => undefined
    }.

%% @private
%% @doc Processes queued events.
process_queue(State) ->
    %% Stub: Would process any queued events
    State.
