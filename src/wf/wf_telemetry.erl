-module(wf_telemetry).
-behaviour(gen_server).

-export([
    start_link/0,
    start_link/1,
    stop/0,
    instrument_transition/4,
    instrument_marking/3,
    record_place_change/4,
    span_transition_start/3,
    span_transition_end/2,
    span_pattern_execution/3,
    track_token_flow/4,
    emit_firing_event/3,
    get_transition_metrics/1,
    get_place_metrics/1,
    get_pattern_stats/1,
    reset_metrics/0,
    export_otel_spans/0,
    wrap_gen_pnet_callbacks/1,
    enable_auto_instrumentation/1,
    disable_auto_instrumentation/0
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("gen_pnet.hrl").

-record(transition_span, {
    id,
    trace_id,
    parent_id,
    net_mod,
    transition,
    mode,
    start_time,
    end_time,
    duration,
    status,
    produced,
    attributes
}).

-record(place_metric, {
    place,
    net_mod,
    token_count,
    high_water_mark,
    low_water_mark,
    total_additions,
    total_removals,
    last_update
}).

-record(transition_metric, {
    transition,
    net_mod,
    fire_count,
    abort_count,
    total_duration,
    min_duration,
    max_duration,
    avg_duration,
    last_fired
}).

-record(pattern_stat, {
    net_mod,
    total_transitions,
    total_places,
    active_tokens,
    throughput,
    start_time,
    last_activity
}).

-record(telemetry_state, {
    enabled = true,
    auto_instrument = false,
    active_spans = #{},
    completed_spans = [],
    place_metrics = #{},
    transition_metrics = #{},
    pattern_stats = #{},
    trace_context = undefined,
    otel_available = false,
    config = #{}
}).

-define(SPAN_CLEANUP_INTERVAL, 300000).
-define(MAX_COMPLETED_SPANS, 5000).

start_link() ->
    start_link(#{}).

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

stop() ->
    gen_server:stop(?MODULE).

instrument_transition(NetMod, Transition, Mode, ProduceMap) ->
    gen_server:cast(?MODULE, {instrument_transition, NetMod, Transition, Mode, ProduceMap}).

instrument_marking(NetMod, Place, Marking) ->
    gen_server:cast(?MODULE, {instrument_marking, NetMod, Place, Marking}).

record_place_change(NetMod, Place, Added, Removed) ->
    gen_server:cast(?MODULE, {record_place_change, NetMod, Place, Added, Removed}).

span_transition_start(NetMod, Transition, Mode) ->
    gen_server:call(?MODULE, {span_transition_start, NetMod, Transition, Mode}).

span_transition_end(SpanId, Result) ->
    gen_server:cast(?MODULE, {span_transition_end, SpanId, Result}).

span_pattern_execution(NetMod, PatternId, Attributes) ->
    gen_server:call(?MODULE, {span_pattern_execution, NetMod, PatternId, Attributes}).

track_token_flow(NetMod, FromPlace, ToPlace, Tokens) ->
    gen_server:cast(?MODULE, {track_token_flow, NetMod, FromPlace, ToPlace, Tokens}).

emit_firing_event(NetMod, Transition, Mode) ->
    gen_server:cast(?MODULE, {emit_firing_event, NetMod, Transition, Mode}).

get_transition_metrics(NetMod) ->
    gen_server:call(?MODULE, {get_transition_metrics, NetMod}).

get_place_metrics(NetMod) ->
    gen_server:call(?MODULE, {get_place_metrics, NetMod}).

get_pattern_stats(NetMod) ->
    gen_server:call(?MODULE, {get_pattern_stats, NetMod}).

reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

export_otel_spans() ->
    gen_server:call(?MODULE, export_otel_spans).

wrap_gen_pnet_callbacks(NetMod) ->
    gen_server:call(?MODULE, {wrap_gen_pnet_callbacks, NetMod}).

enable_auto_instrumentation(NetMod) ->
    gen_server:call(?MODULE, {enable_auto_instrumentation, NetMod}).

disable_auto_instrumentation() ->
    gen_server:call(?MODULE, disable_auto_instrumentation).

init(Config) ->
    OtelAvailable = check_otel_available(),
    EnableAutoInstrument = maps:get(auto_instrument, Config, false),

    process_flag(trap_exit, true),
    schedule_cleanup(),

    TraceId = generate_trace_id(),
    TraceContext = #{
        trace_id => TraceId,
        span_id => generate_span_id(),
        sampled => true
    },

    State = #telemetry_state{
        enabled = maps:get(enabled, Config, true),
        auto_instrument = EnableAutoInstrument,
        otel_available = OtelAvailable,
        trace_context = TraceContext,
        config = Config
    },

    logger:info("wf_telemetry started", #{
        otel_available => OtelAvailable,
        auto_instrument => EnableAutoInstrument
    }),

    {ok, State}.

handle_call({span_transition_start, NetMod, Transition, Mode}, _From, State) ->
    case State#telemetry_state.enabled of
        false ->
            {reply, {ok, undefined}, State};
        true ->
            SpanId = make_ref(),
            TraceId = maps:get(trace_id, State#telemetry_state.trace_context, generate_trace_id()),
            StartTime = erlang:system_time(microsecond),

            Span = #transition_span{
                id = SpanId,
                trace_id = TraceId,
                parent_id = undefined,
                net_mod = NetMod,
                transition = Transition,
                mode = Mode,
                start_time = StartTime,
                end_time = undefined,
                duration = undefined,
                status = in_progress,
                produced = undefined,
                attributes = #{
                    preset_places => maps:keys(Mode),
                    consumed_tokens => maps:fold(fun(_, V, Acc) -> Acc + length(V) end, 0, Mode)
                }
            },

            ActiveSpans = maps:put(SpanId, Span, State#telemetry_state.active_spans),

            maybe_emit_otel_span_start(Span, State),

            {reply, {ok, SpanId}, State#telemetry_state{active_spans = ActiveSpans}}
    end;

handle_call({span_pattern_execution, NetMod, PatternId, Attributes}, _From, State) ->
    case State#telemetry_state.enabled of
        false ->
            {reply, {ok, undefined}, State};
        true ->
            SpanId = make_ref(),
            TraceId = maps:get(trace_id, State#telemetry_state.trace_context, generate_trace_id()),
            StartTime = erlang:system_time(microsecond),

            Span = #transition_span{
                id = SpanId,
                trace_id = TraceId,
                parent_id = undefined,
                net_mod = NetMod,
                transition = pattern_execution,
                mode = #{},
                start_time = StartTime,
                end_time = undefined,
                duration = undefined,
                status = in_progress,
                produced = undefined,
                attributes = maps:merge(#{
                    pattern_id => PatternId,
                    pattern_type => pattern
                }, Attributes)
            },

            ActiveSpans = maps:put(SpanId, Span, State#telemetry_state.active_spans),

            {reply, {ok, SpanId}, State#telemetry_state{active_spans = ActiveSpans}}
    end;

handle_call({get_transition_metrics, NetMod}, _From, State) ->
    Metrics = maps:fold(
        fun({Mod, _Trsn}, Metric, Acc) when Mod =:= NetMod ->
                [transition_metric_to_map(Metric) | Acc];
           (_, _, Acc) ->
                Acc
        end,
        [],
        State#telemetry_state.transition_metrics
    ),
    {reply, {ok, Metrics}, State};

handle_call({get_place_metrics, NetMod}, _From, State) ->
    Metrics = maps:fold(
        fun({Mod, _Place}, Metric, Acc) when Mod =:= NetMod ->
                [place_metric_to_map(Metric) | Acc];
           (_, _, Acc) ->
                Acc
        end,
        [],
        State#telemetry_state.place_metrics
    ),
    {reply, {ok, Metrics}, State};

handle_call({get_pattern_stats, NetMod}, _From, State) ->
    Result = case maps:get(NetMod, State#telemetry_state.pattern_stats, undefined) of
        undefined ->
            {error, not_found};
        Stat ->
            {ok, pattern_stat_to_map(Stat)}
    end,
    {reply, Result, State};

handle_call(reset_metrics, _From, State) ->
    {reply, ok, State#telemetry_state{
        place_metrics = #{},
        transition_metrics = #{},
        pattern_stats = #{},
        completed_spans = []
    }};

handle_call(export_otel_spans, _From, State) ->
    AllSpans = maps:values(State#telemetry_state.active_spans) ++ State#telemetry_state.completed_spans,
    Exported = lists:map(fun span_to_otel_format/1, AllSpans),
    {reply, {ok, Exported}, State};

handle_call({wrap_gen_pnet_callbacks, NetMod}, _From, State) ->
    Result = create_instrumented_wrapper(NetMod, State),
    {reply, Result, State};

handle_call({enable_auto_instrumentation, NetMod}, _From, State) ->
    PatternStat = #pattern_stat{
        net_mod = NetMod,
        total_transitions = 0,
        total_places = 0,
        active_tokens = 0,
        throughput = 0.0,
        start_time = erlang:system_time(microsecond),
        last_activity = erlang:system_time(microsecond)
    },

    PatternStats = maps:put(NetMod, PatternStat, State#telemetry_state.pattern_stats),

    {reply, ok, State#telemetry_state{
        auto_instrument = true,
        pattern_stats = PatternStats
    }};

handle_call(disable_auto_instrumentation, _From, State) ->
    {reply, ok, State#telemetry_state{auto_instrument = false}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({span_transition_end, SpanId, Result}, State) ->
    case maps:get(SpanId, State#telemetry_state.active_spans, undefined) of
        undefined ->
            {noreply, State};
        Span ->
            EndTime = erlang:system_time(microsecond),
            Duration = EndTime - Span#transition_span.start_time,

            {Status, Produced} = case Result of
                {produce, ProdMap} -> {success, ProdMap};
                abort -> {abort, #{}};
                _ -> {unknown, #{}}
            end,

            UpdatedSpan = Span#transition_span{
                end_time = EndTime,
                duration = Duration,
                status = Status,
                produced = Produced
            },

            ActiveSpans = maps:remove(SpanId, State#telemetry_state.active_spans),
            CompletedSpans = [UpdatedSpan | State#telemetry_state.completed_spans],

            State1 = update_transition_metric(UpdatedSpan, State),
            State2 = update_pattern_stat(UpdatedSpan, State1),

            maybe_emit_otel_span_end(UpdatedSpan, State2),

            {noreply, State2#telemetry_state{
                active_spans = ActiveSpans,
                completed_spans = CompletedSpans
            }}
    end;

handle_cast({instrument_transition, NetMod, Transition, Mode, ProduceMap}, State) ->
    case State#telemetry_state.auto_instrument of
        false ->
            {noreply, State};
        true ->
            {ok, SpanId, State1} = do_span_transition_start(NetMod, Transition, Mode, State),
            Result = {produce, ProduceMap},
            State2 = do_span_transition_end(SpanId, Result, State1),
            {noreply, State2}
    end;

handle_cast({instrument_marking, NetMod, Place, Marking}, State) ->
    TokenCount = length(Marking),
    Now = erlang:system_time(microsecond),

    Key = {NetMod, Place},
    Metric = case maps:get(Key, State#telemetry_state.place_metrics, undefined) of
        undefined ->
            #place_metric{
                place = Place,
                net_mod = NetMod,
                token_count = TokenCount,
                high_water_mark = TokenCount,
                low_water_mark = TokenCount,
                total_additions = 0,
                total_removals = 0,
                last_update = Now
            };
        M ->
            M#place_metric{
                token_count = TokenCount,
                high_water_mark = max(M#place_metric.high_water_mark, TokenCount),
                low_water_mark = min(M#place_metric.low_water_mark, TokenCount),
                last_update = Now
            }
    end,

    PlaceMetrics = maps:put(Key, Metric, State#telemetry_state.place_metrics),
    {noreply, State#telemetry_state{place_metrics = PlaceMetrics}};

handle_cast({record_place_change, NetMod, Place, Added, Removed}, State) ->
    Key = {NetMod, Place},
    Now = erlang:system_time(microsecond),

    Metric = case maps:get(Key, State#telemetry_state.place_metrics, undefined) of
        undefined ->
            #place_metric{
                place = Place,
                net_mod = NetMod,
                token_count = length(Added) - length(Removed),
                high_water_mark = length(Added),
                low_water_mark = 0,
                total_additions = length(Added),
                total_removals = length(Removed),
                last_update = Now
            };
        M ->
            NewCount = M#place_metric.token_count + length(Added) - length(Removed),
            M#place_metric{
                token_count = NewCount,
                high_water_mark = max(M#place_metric.high_water_mark, NewCount),
                low_water_mark = min(M#place_metric.low_water_mark, NewCount),
                total_additions = M#place_metric.total_additions + length(Added),
                total_removals = M#place_metric.total_removals + length(Removed),
                last_update = Now
            }
    end,

    PlaceMetrics = maps:put(Key, Metric, State#telemetry_state.place_metrics),
    {noreply, State#telemetry_state{place_metrics = PlaceMetrics}};

handle_cast({track_token_flow, NetMod, FromPlace, ToPlace, Tokens}, State) ->
    case State#telemetry_state.otel_available of
        true ->
            logger:debug("Token flow: ~p -> ~p (~p tokens)", [FromPlace, ToPlace, length(Tokens)], #{
                net_mod => NetMod,
                trace_id => maps:get(trace_id, State#telemetry_state.trace_context, undefined)
            });
        false ->
            ok
    end,
    {noreply, State};

handle_cast({emit_firing_event, NetMod, Transition, Mode}, State) ->
    case State#telemetry_state.otel_available of
        true ->
            TraceId = maps:get(trace_id, State#telemetry_state.trace_context, undefined),
            logger:info("Transition fired", #{
                net_mod => NetMod,
                transition => Transition,
                mode_size => maps:size(Mode),
                trace_id => TraceId
            });
        false ->
            ok
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    CompletedSpans = lists:sublist(
        lists:sort(
            fun(A, B) ->
                (A#transition_span.end_time =/= undefined andalso
                 B#transition_span.end_time =/= undefined andalso
                 A#transition_span.end_time >= B#transition_span.end_time) orelse
                A#transition_span.start_time >= B#transition_span.start_time
            end,
            State#telemetry_state.completed_spans
        ),
        ?MAX_COMPLETED_SPANS
    ),

    schedule_cleanup(),

    {noreply, State#telemetry_state{completed_spans = CompletedSpans}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

check_otel_available() ->
    case code:is_loaded(opentelemetry) of
        false ->
            case code:load_file(opentelemetry) of
                {module, _} -> true;
                {error, _} -> false
            end;
        _ -> true
    end.

generate_trace_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    integer_to_binary(Id, 16).

generate_span_id() ->
    <<Id:64>> = crypto:strong_rand_bytes(8),
    integer_to_binary(Id, 16).

schedule_cleanup() ->
    erlang:send_after(?SPAN_CLEANUP_INTERVAL, self(), cleanup).

do_span_transition_start(NetMod, Transition, Mode, State) ->
    SpanId = make_ref(),
    TraceId = maps:get(trace_id, State#telemetry_state.trace_context, generate_trace_id()),
    StartTime = erlang:system_time(microsecond),

    Span = #transition_span{
        id = SpanId,
        trace_id = TraceId,
        parent_id = undefined,
        net_mod = NetMod,
        transition = Transition,
        mode = Mode,
        start_time = StartTime,
        end_time = undefined,
        duration = undefined,
        status = in_progress,
        produced = undefined,
        attributes = #{
            preset_places => maps:keys(Mode),
            consumed_tokens => maps:fold(fun(_, V, Acc) -> Acc + length(V) end, 0, Mode)
        }
    },

    ActiveSpans = maps:put(SpanId, Span, State#telemetry_state.active_spans),
    NewState = State#telemetry_state{active_spans = ActiveSpans},

    {ok, SpanId, NewState}.

do_span_transition_end(SpanId, Result, State) ->
    case maps:get(SpanId, State#telemetry_state.active_spans, undefined) of
        undefined ->
            State;
        Span ->
            EndTime = erlang:system_time(microsecond),
            Duration = EndTime - Span#transition_span.start_time,

            {Status, Produced} = case Result of
                {produce, ProdMap} -> {success, ProdMap};
                abort -> {abort, #{}};
                _ -> {unknown, #{}}
            end,

            UpdatedSpan = Span#transition_span{
                end_time = EndTime,
                duration = Duration,
                status = Status,
                produced = Produced
            },

            ActiveSpans = maps:remove(SpanId, State#telemetry_state.active_spans),
            CompletedSpans = [UpdatedSpan | State#telemetry_state.completed_spans],

            State1 = update_transition_metric(UpdatedSpan, State),

            State1#telemetry_state{
                active_spans = ActiveSpans,
                completed_spans = CompletedSpans
            }
    end.

update_transition_metric(Span, State) ->
    Key = {Span#transition_span.net_mod, Span#transition_span.transition},
    Duration = Span#transition_span.duration,
    Status = Span#transition_span.status,
    Now = erlang:system_time(microsecond),

    Metric = case maps:get(Key, State#telemetry_state.transition_metrics, undefined) of
        undefined ->
            #transition_metric{
                transition = Span#transition_span.transition,
                net_mod = Span#transition_span.net_mod,
                fire_count = if Status =:= success -> 1; true -> 0 end,
                abort_count = if Status =:= abort -> 1; true -> 0 end,
                total_duration = if Duration =/= undefined -> Duration; true -> 0 end,
                min_duration = if Duration =/= undefined -> Duration; true -> undefined end,
                max_duration = if Duration =/= undefined -> Duration; true -> undefined end,
                avg_duration = if Duration =/= undefined -> Duration; true -> 0 end,
                last_fired = Now
            };
        M ->
            FireCount = M#transition_metric.fire_count + if Status =:= success -> 1; true -> 0 end,
            AbortCount = M#transition_metric.abort_count + if Status =:= abort -> 1; true -> 0 end,
            TotalDuration = M#transition_metric.total_duration + if Duration =/= undefined -> Duration; true -> 0 end,
            MinDuration = if
                Duration =/= undefined andalso M#transition_metric.min_duration =/= undefined ->
                    min(M#transition_metric.min_duration, Duration);
                Duration =/= undefined andalso M#transition_metric.min_duration =:= undefined ->
                    Duration;
                true ->
                    M#transition_metric.min_duration
            end,
            MaxDuration = if
                Duration =/= undefined andalso M#transition_metric.max_duration =/= undefined ->
                    max(M#transition_metric.max_duration, Duration);
                Duration =/= undefined andalso M#transition_metric.max_duration =:= undefined ->
                    Duration;
                true ->
                    M#transition_metric.max_duration
            end,
            AvgDuration = if FireCount + AbortCount > 0 ->
                TotalDuration / (FireCount + AbortCount);
                true -> 0
            end,

            M#transition_metric{
                fire_count = FireCount,
                abort_count = AbortCount,
                total_duration = TotalDuration,
                min_duration = MinDuration,
                max_duration = MaxDuration,
                avg_duration = AvgDuration,
                last_fired = Now
            }
    end,

    TransitionMetrics = maps:put(Key, Metric, State#telemetry_state.transition_metrics),
    State#telemetry_state{transition_metrics = TransitionMetrics}.

update_pattern_stat(Span, State) ->
    NetMod = Span#transition_span.net_mod,
    Now = erlang:system_time(microsecond),

    Stat = case maps:get(NetMod, State#telemetry_state.pattern_stats, undefined) of
        undefined ->
            #pattern_stat{
                net_mod = NetMod,
                total_transitions = 1,
                total_places = 0,
                active_tokens = 0,
                throughput = 0.0,
                start_time = Now,
                last_activity = Now
            };
        S ->
            TimeDelta = Now - S#pattern_stat.start_time,
            Throughput = if TimeDelta > 0 ->
                (S#pattern_stat.total_transitions + 1) / (TimeDelta / 1000000);
                true -> 0.0
            end,

            S#pattern_stat{
                total_transitions = S#pattern_stat.total_transitions + 1,
                throughput = Throughput,
                last_activity = Now
            }
    end,

    PatternStats = maps:put(NetMod, Stat, State#telemetry_state.pattern_stats),
    State#telemetry_state{pattern_stats = PatternStats}.

maybe_emit_otel_span_start(Span, State) ->
    case State#telemetry_state.otel_available of
        true ->
            logger:debug("Span started", #{
                span_id => Span#transition_span.id,
                trace_id => Span#transition_span.trace_id,
                net_mod => Span#transition_span.net_mod,
                transition => Span#transition_span.transition
            });
        false ->
            ok
    end.

maybe_emit_otel_span_end(Span, State) ->
    case State#telemetry_state.otel_available of
        true ->
            logger:debug("Span ended", #{
                span_id => Span#transition_span.id,
                trace_id => Span#transition_span.trace_id,
                net_mod => Span#transition_span.net_mod,
                transition => Span#transition_span.transition,
                duration_us => Span#transition_span.duration,
                status => Span#transition_span.status
            });
        false ->
            ok
    end.

span_to_otel_format(Span) ->
    #{
        span_id => format_span_id(Span#transition_span.id),
        trace_id => Span#transition_span.trace_id,
        parent_span_id => format_span_id(Span#transition_span.parent_id),
        name => atom_to_binary(Span#transition_span.transition, utf8),
        start_time_unix_nano => Span#transition_span.start_time * 1000,
        end_time_unix_nano => if Span#transition_span.end_time =/= undefined ->
            Span#transition_span.end_time * 1000;
            true -> undefined
        end,
        attributes => maps:merge(#{
            'net.mod' => Span#transition_span.net_mod,
            'transition.name' => Span#transition_span.transition,
            'span.kind' => <<"INTERNAL">>
        }, Span#transition_span.attributes),
        status => #{
            code => case Span#transition_span.status of
                success -> <<"OK">>;
                abort -> <<"CANCELLED">>;
                in_progress -> <<"UNSET">>;
                _ -> <<"ERROR">>
            end
        }
    }.

format_span_id(undefined) -> undefined;
format_span_id(Ref) when is_reference(Ref) ->
    list_to_binary(ref_to_list(Ref));
format_span_id(Other) -> Other.

transition_metric_to_map(Metric) ->
    #{
        transition => Metric#transition_metric.transition,
        net_mod => Metric#transition_metric.net_mod,
        fire_count => Metric#transition_metric.fire_count,
        abort_count => Metric#transition_metric.abort_count,
        total_duration_us => Metric#transition_metric.total_duration,
        min_duration_us => Metric#transition_metric.min_duration,
        max_duration_us => Metric#transition_metric.max_duration,
        avg_duration_us => round(Metric#transition_metric.avg_duration),
        last_fired => Metric#transition_metric.last_fired
    }.

place_metric_to_map(Metric) ->
    #{
        place => Metric#place_metric.place,
        net_mod => Metric#place_metric.net_mod,
        token_count => Metric#place_metric.token_count,
        high_water_mark => Metric#place_metric.high_water_mark,
        low_water_mark => Metric#place_metric.low_water_mark,
        total_additions => Metric#place_metric.total_additions,
        total_removals => Metric#place_metric.total_removals,
        last_update => Metric#place_metric.last_update
    }.

pattern_stat_to_map(Stat) ->
    #{
        net_mod => Stat#pattern_stat.net_mod,
        total_transitions => Stat#pattern_stat.total_transitions,
        total_places => Stat#pattern_stat.total_places,
        active_tokens => Stat#pattern_stat.active_tokens,
        throughput => Stat#pattern_stat.throughput,
        start_time => Stat#pattern_stat.start_time,
        last_activity => Stat#pattern_stat.last_activity
    }.

create_instrumented_wrapper(NetMod, _State) ->
    WrapperModule = list_to_atom(atom_to_list(NetMod) ++ "_instrumented"),

    FireWrapper = fun(Trsn, Mode, UsrInfo) ->
        {ok, SpanId} = span_transition_start(NetMod, Trsn, Mode),
        try
            Result = NetMod:fire(Trsn, Mode, UsrInfo),
            span_transition_end(SpanId, Result),
            Result
        catch
            Class:Reason:Stack ->
                span_transition_end(SpanId, {error, {Class, Reason}}),
                erlang:raise(Class, Reason, Stack)
        end
    end,

    {ok, #{
        wrapper_module => WrapperModule,
        fire_wrapper => FireWrapper,
        original_module => NetMod
    }}.
