%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% OpenTelemetry Logger for YAWL Workflows
%%
%% Captures and stores telemetry events for workflow execution traces.
%%
%% @doc YAWL OpenTelemetry Logger
%% @end

-module(yawl_otel_logger).
-author("CRE Team").

-include("yawl_otel_logger.hrl").

-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([
    start_link/0,
    start_link/1,
    log_event/3,
    log_event/4,
    log_approval/4,
    log_checkpoint/6,
    log_workflow_start/2,
    log_workflow_complete/2,
    log_workitem_start/3,
    log_workitem_complete/3,
    get_events/0,
    get_events/1,
    get_events_by_trace/1,
    get_recent_events/2,
    get_traces/0,
    clear_events/0,
    get_stats/0
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Types
%%====================================================================

-type timestamp() :: integer().
-type trace_id() :: binary().
-type span_id() :: binary().
-type event_type() :: binary() | atom().
-type event_level() :: debug | info | warning | error.

-record(otel_state, {
    events :: #{binary() => #otel_event{}},
    traces :: #{binary() => #otel_trace{}},
    trace_index :: #{binary() => binary()},
    event_counter :: non_neg_integer(),
    max_events :: non_neg_integer(),
    retention_ms :: non_neg_integer()
}).

-type state() :: #otel_state{}.

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_EVENTS, 10000).
-define(DEFAULT_RETENTION_MS, 24 * 60 * 60 * 1000).  % 24 hours
-define(DEFAULT_OTEL_TABLE, yawl_otel_events).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the OpenTelemetry logger with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc Start the OpenTelemetry logger with options.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Log a telemetry event.
-spec log_event(binary() | atom(), binary(), map()) -> ok.
log_event(EventType, Message, Attributes) ->
    log_event(EventType, Message, Attributes, info).

%% @doc Log a telemetry event with level.
-spec log_event(binary() | atom(), binary(), map(), event_level()) -> ok.
log_event(EventType, Message, Attributes, Level) ->
    gen_server:cast(?SERVER, {log_event, EventType, Message, Attributes, Level}).

%% @doc Log an approval decision event.
-spec log_approval(binary(), binary(), boolean(), map()) -> ok.
log_approval(CheckpointId, Approver, Approved, Attributes) ->
    EventType = case Approved of
        true -> checkpoint_approve;
        false -> checkpoint_deny
    end,
    Message = io_lib:format("Checkpoint ~s ~s by ~s", [
        CheckpointId,
        case Approved of true -> "approved"; false -> "denied" end,
        Approver
    ]),
    log_event(EventType, list_to_binary(Message), Attributes#{
        checkpoint_id => CheckpointId,
        approver => Approver,
        approved => Approved
    }, info).

%% @doc Log a checkpoint creation event.
-spec log_checkpoint(binary(), binary(), atom(), binary(), map(), map()) -> ok.
log_checkpoint(CheckpointId, PatternId, StepName, RequiredApprover, Context, Attributes) ->
    Message = io_lib:format("Checkpoint ~s created for ~p (approver: ~s)", [
        CheckpointId, StepName, RequiredApprover
    ]),
    log_event(checkpoint_create, list_to_binary(Message), Attributes#{
        checkpoint_id => CheckpointId,
        pattern_id => PatternId,
        step_name => StepName,
        required_approver => RequiredApprover,
        context => Context
    }, info).

%% @doc Log workflow start event.
-spec log_workflow_start(binary(), binary()) -> ok.
log_workflow_start(CaseId, PatternId) ->
    TraceId = generate_trace_id(),
    gen_server:cast(?SERVER, {workflow_start, TraceId, CaseId, PatternId}),
    log_event(workflow_start, <<"Workflow started">>, #{
        trace_id => TraceId,
        case_id => CaseId,
        pattern_id => PatternId
    }, info).

%% @doc Log workflow completion event.
-spec log_workflow_complete(binary(), binary()) -> ok.
log_workflow_complete(CaseId, Status) ->
    gen_server:cast(?SERVER, {workflow_complete, CaseId, Status}),
    Message = io_lib:format("Workflow ~s", [Status]),
    log_event(workflow_complete, list_to_binary(Message), #{
        case_id => CaseId,
        status => Status
    }, info).

%% @doc Log workitem start event.
-spec log_workitem_start(binary(), binary(), binary()) -> ok.
log_workitem_start(CaseId, TaskId, TaskName) ->
    Message = io_lib:format("Workitem ~s started", [TaskName]),
    log_event(workitem_start, list_to_binary(Message), #{
        case_id => CaseId,
        task_id => TaskId,
        task_name => TaskName
    }, info).

%% @doc Log workitem completion event.
-spec log_workitem_complete(binary(), binary(), binary()) -> ok.
log_workitem_complete(CaseId, TaskId, Result) ->
    Message = io_lib:format("Workitem ~s completed", [TaskId]),
    log_event(workitem_complete, list_to_binary(Message), #{
        case_id => CaseId,
        task_id => TaskId,
        result => Result
    }, info).

%% @doc Get all events.
-spec get_events() -> [#otel_event{}].
get_events() ->
    gen_server:call(?SERVER, get_events).

%% @doc Get events by type.
-spec get_events(binary() | atom()) -> [#otel_event{}].
get_events(EventType) ->
    gen_server:call(?SERVER, {get_events, EventType}).

%% @doc Get events by trace ID.
-spec get_events_by_trace(binary()) -> [#otel_event{}].
get_events_by_trace(TraceId) ->
    gen_server:call(?SERVER, {get_events_by_trace, TraceId}).

%% @doc Get recent events.
-spec get_recent_events(non_neg_integer(), event_level() | all) -> [#otel_event{}].
get_recent_events(Limit, Level) ->
    gen_server:call(?SERVER, {get_recent_events, Limit, Level}).

%% @doc Get all traces.
-spec get_traces() -> [#otel_trace{}].
get_traces() ->
    gen_server:call(?SERVER, get_traces).

%% @doc Clear all events.
-spec clear_events() -> ok.
clear_events() ->
    gen_server:call(?SERVER, clear_events).

%% @doc Get statistics.
-spec get_stats() -> map().
get_stats() ->
    gen_server:call(?SERVER, get_stats).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    MaxEvents = maps:get(max_events, Options, ?DEFAULT_MAX_EVENTS),
    RetentionMs = maps:get(retention_ms, Options, ?DEFAULT_RETENTION_MS),

    % Create ETS table for events
    TableId = ets:new(?DEFAULT_OTEL_TABLE, [
        named_table,
        public,
        ordered_set,
        {keypos, #otel_event.id}
    ]),

    {ok, #otel_state{
        events = #{},
        traces = #{},
        trace_index = #{},
        event_counter = 0,
        max_events = MaxEvents,
        retention_ms = RetentionMs
    }}.

handle_call(get_events, _From, State) ->
    Events = ets:tab2list(?DEFAULT_OTEL_TABLE),
    {reply, lists:sort(fun(A, B) ->
        A#otel_event.timestamp >= B#otel_event.timestamp
    end, Events), State};

handle_call({get_events, EventType}, _From, State) ->
    AllEvents = ets:tab2list(?DEFAULT_OTEL_TABLE),
    Events = lists:filter(fun(E) -> E#otel_event.event_type =:= EventType end, AllEvents),
    {reply, Events, State};

handle_call({get_events_by_trace, TraceId}, _From, State) ->
    AllEvents = ets:tab2list(?DEFAULT_OTEL_TABLE),
    Events = lists:filter(fun(E) -> E#otel_event.trace_id =:= TraceId end, AllEvents),
    {reply, Events, State};

handle_call({get_recent_events, Limit, Level}, _From, State) ->
    AllEvents = ets:tab2list(?DEFAULT_OTEL_TABLE),
    Filtered = case Level of
        all -> AllEvents;
        _ -> lists:filter(fun(E) -> E#otel_event.level =:= Level end, AllEvents)
    end,
    Sorted = lists:sort(fun(A, B) ->
        A#otel_event.timestamp >= B#otel_event.timestamp
    end, Filtered),
    Result = lists:sublist(Sorted, min(Limit, length(Sorted))),
    {reply, Result, State};

handle_call(get_traces, _From, State) ->
    Traces = maps:values(State#otel_state.traces),
    {reply, Traces, State};

handle_call(clear_events, _From, State) ->
    ets:delete_all_objects(?DEFAULT_OTEL_TABLE),
    {reply, ok, State#otel_state{
        events = #{},
        traces = #{},
        trace_index = #{}
    }};

handle_call(get_stats, _From, State) ->
    EventCount = ets:info(?DEFAULT_OTEL_TABLE, size),
    TraceCount = maps:size(State#otel_state.traces),
    Stats = #{
        event_count => EventCount,
        trace_count => TraceCount,
        max_events => State#otel_state.max_events,
        retention_ms => State#otel_state.retention_ms
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({log_event, EventType, Message, Attributes, Level}, State) ->
    Event = create_event(EventType, Message, Attributes, Level, State),
    ets:insert(?DEFAULT_OTEL_TABLE, Event),
    {noreply, maybe_cleanup_events(State)};

handle_cast({workflow_start, TraceId, CaseId, PatternId}, State) ->
    Trace = #otel_trace{
        trace_id = TraceId,
        case_id = CaseId,
        pattern_id = PatternId,
        start_time = erlang:system_time(millisecond),
        end_time = undefined,
        status = running,
        span_count = 1
    },
    {noreply, State#otel_state{
        traces = maps:put(TraceId, Trace, State#otel_state.traces),
        trace_index = maps:put(CaseId, TraceId, State#otel_state.trace_index)
    }};

handle_cast({workflow_complete, CaseId, Status}, State) ->
    TraceId = maps:get(CaseId, State#otel_state.trace_index, undefined),
    NewTraces = case TraceId of
        undefined -> State#otel_state.traces;
        _ ->
            maps:update_with(TraceId, fun(T) ->
                T#otel_trace{
                    end_time = erlang:system_time(millisecond),
                    status = Status
                }
            end, State#otel_state.traces)
    end,
    {noreply, State#otel_state{
        traces = NewTraces
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup_old_events, State) ->
    Now = erlang:system_time(millisecond),
    Cutoff = Now - State#otel_state.retention_ms,
    ets:select_delete(?DEFAULT_OTEL_TABLE, [{
        #otel_event{timestamp = '$1', _ = '_'},
        [{'<', '$1', Cutoff}],
        [true]
    }]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
create_event(EventType, Message, Attributes, Level, _State) ->
    TraceId = maps:get(trace_id, Attributes, generate_trace_id()),
    SpanId = maps:get(span_id, Attributes, generate_span_id()),
    ParentSpanId = maps:get(parent_span_id, Attributes, undefined),

    #otel_event{
        id = generate_event_id(),
        trace_id = TraceId,
        span_id = SpanId,
        parent_span_id = ParentSpanId,
        timestamp = erlang:system_time(millisecond),
        event_type = EventType,
        level = Level,
        user_id = maps:get(user_id, Attributes, undefined),
        case_id = maps:get(case_id, Attributes, undefined),
        task_id = maps:get(task_id, Attributes, undefined),
        pattern_id = maps:get(pattern_id, Attributes, undefined),
        message = to_binary(Message),
        attributes = Attributes
    }.

%% @private
maybe_cleanup_events(State) ->
    EventCount = ets:info(?DEFAULT_OTEL_TABLE, size),
    case EventCount > State#otel_state.max_events of
        true ->
            % Remove oldest events
            RemoveCount = EventCount - State#otel_state.max_events,
            Events = ets:tab2list(?DEFAULT_OTEL_TABLE),
            Sorted = lists:sort(fun(A, B) ->
                A#otel_event.timestamp < B#otel_event.timestamp
            end, Events),
            ToRemove = lists:sublist(Sorted, RemoveCount),
            lists:foreach(fun(E) ->
                ets:delete(?DEFAULT_OTEL_TABLE, E#otel_event.id)
            end, ToRemove),
            State;
        false ->
            State
    end.

%% @private
generate_event_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    <<Id:16/binary, _/binary>> = Unique,
    <<"event_", Id/binary>>.

%% @private
generate_trace_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"trace_", Hex/binary>>.

%% @private
generate_span_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    <<Id:8/binary, _/binary>> = Unique,
    <<"span_", Id/binary>>.

%% @private
to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(_) -> <<"">>.
