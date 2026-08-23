%% -*- erlang -*-
%% @doc Google Cloud Trace Exporter for CRE Distributed Tracing
%%
%% Exports OpenTelemetry spans to Google Cloud Trace API with:
%% - W3C trace context support
%% - Batch export for efficiency
%% - Sampling configuration
%% - Non-blocking async export
%% - Buffer spans during Cloud Trace unavailability
%% - Authentication via Application Default Credentials (ADC)
%%
%% @end

-module(cloud_trace_exporter).
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/0, stop/0]).
-export([export_span/1, export_batch/1]).
-export([set_sampler/1]).
-export([health/0]).
-export([flush_buffer/0]).
-export([get_buffer_size/0]).
-export([configure_project/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type span() :: map().
-type spans() :: [span()].
-type sampler() :: always | never | {probability, float()} | {module(), atom()}.
-type export_result() :: ok | {error, term()}.
-type project_id() :: binary().
-type state() :: #{project_id := project_id() | undefined,
                   buffer := queue:queue(span()),
                   buffer_size := non_neg_integer(),
                   max_buffer_size := pos_integer(),
                   batch_size := pos_integer(),
                   sampler := sampler(),
                   batch_timer := reference() | undefined,
                   http_client := pid() | undefined,
                   unavailable := boolean()}.

-export_type([span/0, sampler/0, export_result/0]).

-define(SERVER, ?MODULE).
-define(DEFAULT_MAX_BUFFER_SIZE, 10000).
-define(DEFAULT_BATCH_SIZE, 100).
-define(DEFAULT_BATCH_INTERVAL_MS, 5000).
-define(CLOUD_TRACE_API, <<"cloudtrace.googleapis.com">>).
-define(SPANS_PATH, <<":traces:batchWrite">>).

-define(DEFAULT_PROJECT_ID, application:get_env(cloud_trace_exporter, project_id, undefined)).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the Cloud Trace exporter with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the Cloud Trace exporter with options.
%% Options:
%%   - {project_id, binary()} - GCP project ID
%%   - {max_buffer_size, pos_integer()} - Max buffer size (default 10000)
%%   - {batch_size, pos_integer()} - Batch size for export (default 100)
%%   - {batch_interval_ms, pos_integer()} - Batch interval in ms (default 5000)
%%   - {sampler, sampler()} - Sampling strategy (default {probability, 0.1})
-spec start_link(proplists:proplist()) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Stop the exporter and flush remaining spans.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Export a single span to Cloud Trace.
%% Non-blocking: returns immediately, spans are buffered and sent in batches.
-spec export_span(span()) -> export_result().
export_span(Span) ->
    gen_server:cast(?SERVER, {export_span, Span}),
    ok.

%% @doc Export a batch of spans to Cloud Trace.
%% Non-blocking: adds to buffer for immediate batch processing.
-spec export_batch(spans()) -> export_result().
export_batch(Spans) when is_list(Spans) ->
    gen_server:cast(?SERVER, {export_batch, Spans}),
    ok.

%% @doc Set the sampling strategy.
-spec set_sampler(sampler()) -> ok.
set_sampler(Sampler) ->
    gen_server:cast(?SERVER, {set_sampler, Sampler}).

%% @doc Health check for the exporter.
-spec health() -> up | down.
health() ->
    case whereis(?SERVER) of
        undefined -> down;
        _Pid ->
            try gen_server:call(?SERVER, health) of
                ok -> up
            catch
                _:_ -> down
            end
    end.

%% @doc Manually flush the buffer to Cloud Trace.
-spec flush_buffer() -> {ok, non_neg_integer()}.
flush_buffer() ->
    gen_server:call(?SERVER, flush_buffer).

%% @doc Get the current buffer size.
-spec get_buffer_size() -> non_neg_integer().
get_buffer_size() ->
    gen_server:call(?SERVER, get_buffer_size).

%% @doc Configure the GCP project ID.
-spec configure_project(project_id()) -> ok.
configure_project(ProjectId) when is_binary(ProjectId) ->
    gen_server:cast(?SERVER, {configure_project, ProjectId}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Options) ->
    MaxBufferSize = proplists:get_value(max_buffer_size, Options, ?DEFAULT_MAX_BUFFER_SIZE),
    BatchSize = proplists:get_value(batch_size, Options, ?DEFAULT_BATCH_SIZE),
    BatchIntervalMs = proplists:get_value(batch_interval_ms, Options, ?DEFAULT_BATCH_INTERVAL_MS),
    InitialSampler = proplists:get_value(sampler, Options, {probability, 0.1}),
    ProjectId = proplists:get_value(project_id, Options, ?DEFAULT_PROJECT_ID),

    %% Try to get project ID from environment if not provided
    FinalProjectId = case ProjectId of
        undefined -> get_project_id_from_env();
        _ -> ProjectId
    end,

    %% Start batch timer
    TimerRef = case FinalProjectId of
        undefined -> undefined;
        _ -> erlang:send_after(BatchIntervalMs, self(), flush_batch)
    end,

    {ok, #{
        project_id => FinalProjectId,
        buffer => queue:new(),
        buffer_size => 0,
        max_buffer_size => MaxBufferSize,
        batch_size => BatchSize,
        sampler => InitialSampler,
        batch_timer => TimerRef,
        http_client => undefined,
        unavailable => false
    }}.

handle_call(health, _From, State) ->
    {reply, ok, State};

handle_call(get_buffer_size, _From, State = #{buffer_size := Size}) ->
    {reply, Size, State};

handle_call(flush_buffer, _From, State) ->
    {NewState, ExportedCount} = do_flush_with_count(State),
    {reply, {ok, ExportedCount}, NewState};

handle_call(get_project_id, _From, State = #{project_id := ProjectId}) ->
    {reply, ProjectId, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({export_span, Span}, State) ->
    NewState = handle_export_span(Span, State),
    {noreply, NewState};

handle_cast({export_batch, Spans}, State) ->
    NewState = lists:foldl(fun(Span, AccState) ->
        handle_export_span(Span, AccState)
    end, State, Spans),
    {noreply, NewState};

handle_cast({set_sampler, Sampler}, State) ->
    {noreply, State#{sampler => Sampler}};

handle_cast({configure_project, ProjectId}, State = #{batch_timer := OldTimer}) ->
    %% Cancel old timer if exists
    case OldTimer of
        undefined -> ok;
        _ -> erlang:cancel_timer(OldTimer)
    end,
    %% Start new timer with project
    TimerRef = erlang:send_after(?DEFAULT_BATCH_INTERVAL_MS, self(), flush_batch),
    {noreply, State#{project_id => ProjectId, batch_timer => TimerRef, unavailable => false}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush_batch, State = #{batch_timer := _OldTimer}) ->
    %% Restart timer
    TimerRef = erlang:send_after(?DEFAULT_BATCH_INTERVAL_MS, self(), flush_batch),
    NewState = do_flush(State),
    {noreply, NewState#{batch_timer => TimerRef}};

handle_info({http_response, Result}, State) ->
    handle_http_result(Result, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    %% Final flush on shutdown
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Handle a single span export.
-spec handle_export_span(span(), state()) -> state().
handle_export_span(Span, State = #{
    buffer := Buffer,
    buffer_size := BufferSize,
    max_buffer_size := MaxSize,
    sampler := Sampler,
    batch_size := BatchSize
}) ->
    case should_sample(Span, Sampler) of
        true ->
            NewBuffer = queue:in(Span, Buffer),
            NewBufferSize = BufferSize + 1,
            NewState = State#{buffer => NewBuffer, buffer_size => NewBufferSize},

            %% Flush if batch size reached
            case NewBufferSize >= BatchSize of
                true ->
                    do_flush(NewState);
                false ->
                    %% Check if buffer is full
                    case NewBufferSize >= MaxSize of
                        true ->
                            ?LOG(warning, "Cloud Trace buffer full, dropping oldest span", []),
                            %% Drop oldest to make room
                            {{value, _}, DroppedBuffer} = queue:out(NewBuffer),
                            NewState#{buffer => DroppedBuffer, buffer_size => NewBufferSize - 1};
                        false ->
                            NewState
                    end
            end;
        false ->
            State
    end.

%% @private Flush buffer to Cloud Trace.
-spec do_flush(state()) -> state().
do_flush(State = #{buffer := _Buffer, buffer_size := 0}) ->
    State;
do_flush(State = #{
    project_id := ProjectId,
    buffer := Buffer,
    buffer_size := BufferSize,
    unavailable := Unavailable
}) ->
    case ProjectId of
        undefined ->
            ?LOG(debug, "Cloud Trace project_id not configured, skipping export", []),
            State#{buffer => queue:new(), buffer_size => 0};
        _ when Unavailable ->
            ?LOG(debug, "Cloud Trace unavailable, buffering spans (~p in buffer)", [BufferSize]),
            State;
        _ ->
            SpanList = queue:to_list(Buffer),
            _Count = length(SpanList),
            %% Send to Cloud Trace API asynchronously
            spawn(fun() -> send_to_cloud_trace(ProjectId, SpanList, self()) end),
            State#{buffer => queue:new(), buffer_size => 0}
    end.

%% @private Flush buffer to Cloud Trace and return count.
-spec do_flush_with_count(state()) -> {state(), non_neg_integer()}.
do_flush_with_count(State = #{buffer := _Buffer, buffer_size := BufferSize}) ->
    case BufferSize of
        0 -> {State, 0};
        _ ->
            NewState = do_flush(State),
            {NewState, BufferSize}
    end.

%% @private Handle HTTP response result.
-spec handle_http_result(term(), state()) -> state().
handle_http_result({ok, _}, State) ->
    %% Success, reset unavailable flag
    {noreply, State#{unavailable => false}};
handle_http_result({error, Reason}, State = #{unavailable := false}) ->
    ?LOG(warning, "Cloud Trace export failed: ~p, buffering spans", [Reason]),
    {noreply, State#{unavailable => true}};
handle_http_result({error, Reason}, State = #{unavailable := true}) ->
    ?LOG(debug, "Cloud Trace still unavailable: ~p", [Reason]),
    {noreply, State}.

%% @private Determine if a span should be sampled.
-spec should_sample(span(), sampler()) -> boolean().
should_sample(_Span, always) ->
    true;
should_sample(_Span, never) ->
    false;
should_sample(_Span, {probability, P}) when P >= 1.0 ->
    true;
should_sample(_Span, {probability, P}) when P =< 0.0 ->
    false;
should_sample(_Span, {probability, P}) ->
    rand:uniform() < P;
should_sample(Span, {Module, Function}) ->
    try
        Module:Function(Span)
    catch
        _:_ -> false
    end.

%% @private Get project ID from environment variables.
-spec get_project_id_from_env() -> binary() | undefined.
get_project_id_from_env() ->
    case os:getenv("GOOGLE_CLOUD_PROJECT") of
        false ->
            case os:getenv("GCP_PROJECT") of
                false -> undefined;
                Val -> list_to_binary(Val)
            end;
        Val -> list_to_binary(Val)
    end.

%% @private Send spans to Cloud Trace API.
-spec send_to_cloud_trace(project_id(), spans(), pid()) -> ok.
send_to_cloud_trace(ProjectId, Spans, ReplyTo) ->
    Url = build_url(ProjectId),
    Body = encode_spans(Spans),
    Headers = build_headers(),

    Result = case get_access_token() of
        {ok, Token} ->
            AuthHeaders = Headers#{<<"authorization">> => <<"Bearer ", Token/binary>>},
            make_http_request(post, Url, AuthHeaders, Body);
        {error, Reason} ->
            {error, {auth_failure, Reason}}
    end,

    ReplyTo ! {http_response, Result},
    log_export_result(Result, length(Spans)).

%% @private Build the Cloud Trace API URL.
-spec build_url(project_id()) -> binary().
build_url(ProjectId) ->
    <<"https://", ?CLOUD_TRACE_API/binary,
      "/v2/projects/", ProjectId/binary,
      ?SPANS_PATH/binary>>.

%% @private Build HTTP headers.
-spec build_headers() -> map().
build_headers() ->
    #{
        <<"content-type">> => <<"application/json">>,
        <<"x-cloud-trace-context">> => get_trace_context()
    }.

%% @private Get current trace context from process dictionary.
-spec get_trace_context() -> binary().
get_trace_context() ->
    TraceId = case get('$trace_id') of
        undefined -> <<"00000000000000000000000000000000">>;
        T -> T
    end,
    SpanId = case get('$span_id') of
        undefined -> <<"0000000000000000">>;
        S -> S
    end,
    <<TraceId/binary, "/", SpanId/binary, ";o=1">>.

%% @private Encode a single span to Cloud Trace format.
-spec encode_span(span()) -> map().
encode_span(Span) ->
    Name = maps:get(name, Span, <<"unknown">>),
    SpanId = encode_span_id(maps:get(span_id, Span, <<>>)),
    TraceId = encode_trace_id(maps:get(trace_id, Span, <<>>)),
    ParentSpanId = case maps:get(parent_span_id, Span, undefined) of
        undefined -> undefined;
        PId -> encode_span_id(PId)
    end,

    StartTime = maps:get(start_time, Span, 0),
    EndTime = maps:get(end_time, Span, StartTime),

    Base = #{
        <<"name">> => <<"projects/", (get_project_id())/binary, "/traces/",
                        TraceId/binary, "/spans/", SpanId/binary>>,
        <<"spanId">> => SpanId,
        <<"displayName">> => #{<<"value">> => to_binary(Name)},
        <<"startTime">> => format_timestamp(StartTime),
        <<"endTime">> => format_timestamp(EndTime)
    },

    %% Add parent span ID if present
    WithParent = case ParentSpanId of
        undefined -> Base;
        _ -> Base#{<<"parentSpanId">> => ParentSpanId}
    end,

    %% Add attributes
    WithAttrs = case maps:get(attributes, Span, #{}) of
        Attrs when is_map(Attrs), map_size(Attrs) > 0 ->
            WithParent#{<<"attributes">> => #{<<"attributeMap">> => encode_attributes(Attrs)}};
        _ -> WithParent
    end,

    %% Add events
    WithEvents = case maps:get(events, Span, []) of
        [] -> WithAttrs;
        Events ->
            EncodedEvents = lists:map(fun encode_event/1, Events),
            WithAttrs#{<<"timeEvents">> => #{<<"timeEvent">> => EncodedEvents}}
    end,

    %% Add status
    case maps:get(status, Span, undefined) of
        undefined -> WithEvents;
        ok -> WithEvents#{<<"status">> => #{<<"code">> => 0}};
        {error, _, _} -> WithEvents#{<<"status">> => #{<<"code">> => 2}};
        _ -> WithEvents
    end.

%% @private Encode spans to Cloud Trace format.
-spec encode_spans(spans()) -> binary().
encode_spans(Spans) ->
    TraceSpans = lists:map(fun encode_span/1, Spans),
    Json = #{<<"traces">> => [#{<<"spans">> => TraceSpans}]},
    jsone:encode(Json).

%% @private Encode span ID to hex string.
-spec encode_span_id(binary()) -> binary().
encode_span_id(Binary) ->
    <<<<(integer_to_binary(N, 16))/binary>> || <<N:4>> <= Binary>>.

%% @private Encode trace ID to hex string.
-spec encode_trace_id(binary()) -> binary().
encode_trace_id(Binary) ->
    <<<<(integer_to_binary(N, 16))/binary>> || <<N:4>> <= Binary>>.

%% @private Format timestamp for Cloud Trace.
-spec format_timestamp(integer()) -> binary().
format_timestamp(Microseconds) ->
    %% Cloud Trace expects nanoseconds since Unix epoch
    Nanos = Microseconds * 1000,
    Sec = Nanos div 1_000_000_000,
    Rem = Nanos rem 1_000_000_000,
    io_lib:format("~b.~9.10.0b", [Sec, Rem]).

%% @private Encode attributes to Cloud Trace format.
-spec encode_attributes(map()) -> map().
encode_attributes(Attrs) ->
    maps:fold(fun(K, V, Acc) ->
        Key = to_binary(K),
        Value = encode_attribute_value(V),
        maps:put(Key, Value, Acc)
    end, #{}, Attrs).

%% @private Encode a single attribute value.
-spec encode_attribute_value(term()) -> map().
encode_attribute_value(V) when is_integer(V) ->
    #{<<"intValue">> => integer_to_binary(V)};
encode_attribute_value(V) when is_float(V) ->
    #{<<"doubleValue">> => V};
encode_attribute_value(V) when is_binary(V) ->
    #{<<"stringValue">> => #{<<"value">> => V}};
encode_attribute_value(V) when is_atom(V) ->
    #{<<"stringValue">> => #{<<"value">> => atom_to_binary(V, utf8)}};
encode_attribute_value(V) when is_list(V) ->
    #{<<"stringValue">> => #{<<"value">> => list_to_binary(V)}};
encode_attribute_value(V) when is_map(V) ->
    #{<<"stringValue">> => #{<<"value">> => jsone:encode(V)}};
encode_attribute_value(_V) ->
    #{<<"stringValue">> => #{<<"value">> => <<"">>}}.

%% @private Encode an event to Cloud Trace format.
-spec encode_event(map()) -> map().
encode_event(Event) ->
    Name = maps:get(name, Event, <<"unknown">>),
    Time = maps:get(timestamp, Event, erlang:monotonic_time(microsecond)),
    Attrs = maps:get(attributes, Event, #{}),

    #{
        <<"time">> => format_timestamp(Time),
        <<"annotation">> => #{
            <<"description">> => #{<<"value">> => Name},
            <<"attributes">> => #{<<"attributeMap">> => encode_attributes(Attrs)}
        }
    }.

%% @private Convert term to binary.
-spec to_binary(atom() | binary() | list() | integer()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V).

%% @private Get the current project ID from state or environment.
-spec get_project_id() -> binary().
get_project_id() ->
    case gen_server:call(?SERVER, get_project_id) of
        undefined -> <<"unknown-project">>;
        Pid -> Pid
    end.

%% @private Get access token from ADC (Application Default Credentials).
-spec get_access_token() -> {ok, binary()} | {error, term()}.
get_access_token() ->
    case os:getenv("GOOGLE_APPLICATION_CREDENTIALS") of
        false ->
            %% Try metadata server (GCE/GKE)
            get_token_from_metadata_server();
        CredFile ->
            %% Try to read from service account file
            get_token_from_service_account(CredFile)
    end.

%% @private Get token from GCP metadata server.
-spec get_token_from_metadata_server() -> {ok, binary()} | {error, term()}.
get_token_from_metadata_server() ->
    Url = "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token",
    Headers = [{"Metadata-Flavor", "Google"}],

    case httpc:request(get, {Url, Headers}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            case jsone:decode(Body) of
                #{<<"access_token">> := Token} -> {ok, Token};
                _ -> {error, invalid_response}
            end;
        {ok, {{_, Code, _}, _, _}} ->
            {error, {http_error, Code}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Get token from service account file.
-spec get_token_from_service_account(file:filename_all()) -> {ok, binary()} | {error, term()}.
get_token_from_service_account(_CredFile) ->
    %% In a full implementation, this would:
    %% 1. Read the service account JSON file
    %% 2. Create a JWT assertion
    %% 3. Exchange it for an access token
    %% For now, return error to encourage using metadata server
    {error, service_account_auth_not_implemented}.

%% @private Make HTTP request.
-spec make_http_request(post, binary(), map(), binary()) -> {ok, term()} | {error, term()}.
make_http_request(post, Url, Headers, Body) ->
    UrlStr = binary_to_list(Url),
    HeaderList = maps:to_list(Headers),

    case httpc:request(post, {UrlStr, HeaderList, <<"application/json">>, Body},
                       [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, _RespBody}} ->
            {ok, 200};
        {ok, {{_, Code, _}, _, RespBody}} ->
            {error, {http_error, Code, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Log export result.
-spec log_export_result(term(), non_neg_integer()) -> ok.
log_export_result({ok, _}, Count) ->
    ?LOG(debug, "Exported ~p spans to Cloud Trace", [Count]);
log_export_result({error, Reason}, Count) ->
    ?LOG(warning, "Failed to export ~p spans to Cloud Trace: ~p", [Count, Reason]).
