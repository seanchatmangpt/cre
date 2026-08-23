%% -*- erlang -*-
%% @doc Distributed Tracing Setup for CRE
%%
%% Provides span creation helpers and W3C trace context support.
%%
%% @end

-module(tracing).

%% API
-export([start_span/1, start_span/2]).
-export([end_span/0, end_span/1]).
-export([set_attribute/2, set_attributes/1]).
-export([add_event/1, add_event/2]).
-export([record_exception/1, record_exception/2]).
-export([get_current_span/0]).
-export([get_trace_id/0, get_span_id/0]).
-export([extract_context/1, inject_context/1]).
-export([child_context/0]).
-export([with_span/2]).
-export([export_span/1]).

%% Types
-type span_name() :: binary() | atom().
-type span() :: #{name := span_name(),
                  trace_id := binary(),
                  span_id := binary(),
                  parent_span_id => binary(),
                  start_time => integer(),
                  end_time => integer(),
                  attributes => map(),
                  events => list(),
                  status => term()}.
-type span_ctx() :: #{trace_id => binary(),
                     span_id => binary()}.
-type attribute_key() :: atom() | binary().
-type attribute_value() :: term().

-export_type([span/0, span_ctx/0, span_name/0]).

-define(SPAN_KEY, '$__current_span').
-define(TRACE_ID_VERSION, 0).
-define(TRACE_ID_BYTES, 16).
-define(SPAN_ID_BYTES, 8).

%%====================================================================
%% API
%%====================================================================

%% @doc Start a new root span.
-spec start_span(span_name()) -> span().
start_span(Name) ->
    start_span(Name, #{}).

%% @doc Start a new span with attributes.
-spec start_span(span_name(), map()) -> span().
start_span(Name, Attributes) ->
    Span = #{
        name => Name,
        trace_id => generate_trace_id(),
        span_id => generate_span_id(),
        parent_span_id => undefined,
        start_time => erlang:monotonic_time(microsecond),
        end_time => undefined,
        attributes => Attributes,
        events => [],
        status => undefined
    },
    put(?SPAN_KEY, Span),
    Span.

%% @doc End the current span.
-spec end_span() -> ok.
end_span() ->
    end_span(undefined).

%% @doc End the current span with a status.
-spec end_span(term()) -> ok.
end_span(Status) ->
    case get(?SPAN_KEY) of
        undefined ->
            ok;
        Span ->
            EndedSpan = Span#{
                end_time => erlang:monotonic_time(microsecond),
                status => Status
            },
            %% Export the span
            export_span(EndedSpan),
            %% Restore parent span if exists
            ParentSpanId = maps:get(parent_span_id, Span, undefined),
            erase(?SPAN_KEY),
            case ParentSpanId of
                undefined -> ok;
                _ ->
                    %% In a real implementation, we'd restore the parent span
                    %% from a span stack
                    ok
            end,
            ok
    end.

%% @doc Set a single attribute on the current span.
-spec set_attribute(attribute_key(), attribute_value()) -> ok.
set_attribute(Key, Value) ->
    case get(?SPAN_KEY) of
        undefined -> ok;
        Span ->
            Attributes = maps:put(Key, Value, maps:get(attributes, Span, #{})),
            NewSpan = Span#{attributes => Attributes},
            put(?SPAN_KEY, NewSpan),
            ok
    end.

%% @doc Set multiple attributes on the current span.
-spec set_attributes(map()) -> ok.
set_attributes(Attrs) when is_map(Attrs) ->
    case get(?SPAN_KEY) of
        undefined -> ok;
        Span ->
            CurrentAttrs = maps:get(attributes, Span, #{}),
            NewAttrs = maps:merge(CurrentAttrs, Attrs),
            NewSpan = Span#{attributes => NewAttrs},
            put(?SPAN_KEY, NewSpan),
            ok
    end.

%% @doc Add an event to the current span.
-spec add_event(binary()) -> ok.
add_event(Name) ->
    add_event(Name, #{}).

%% @doc Add an event with attributes to the current span.
-spec add_event(binary(), map()) -> ok.
add_event(Name, Attributes) ->
    case get(?SPAN_KEY) of
        undefined -> ok;
        Span ->
            Event = #{
                name => Name,
                timestamp => erlang:monotonic_time(microsecond),
                attributes => Attributes
            },
            Events = [Event | maps:get(events, Span, [])],
            NewSpan = Span#{events => Events},
            put(?SPAN_KEY, NewSpan),
            ok
    end.

%% @doc Record an exception in the current span.
-spec record_exception(term()) -> ok.
record_exception(Exception) ->
    record_exception(Exception, #{}).

%% @doc Record an exception with attributes.
-spec record_exception(term(), map()) -> ok.
record_exception(Exception, Attributes) ->
    ExcAttrs = Attributes#{
        exception => Exception,
        exception_type => element(1, Exception)
    },
    add_event(<<"exception">>, ExcAttrs),
    ok.

%% @doc Get the current span.
-spec get_current_span() -> span() | undefined.
get_current_span() ->
    get(?SPAN_KEY).

%% @doc Get the trace ID from the current span.
-spec get_trace_id() -> binary() | undefined.
get_trace_id() ->
    case get(?SPAN_KEY) of
        undefined -> undefined;
        Span -> maps:get(trace_id, Span)
    end.

%% @doc Get the span ID from the current span.
-spec get_span_id() -> binary() | undefined.
get_span_id() ->
    case get(?SPAN_KEY) of
        undefined -> undefined;
        Span -> maps:get(span_id, Span)
    end.

%% @doc Extract trace context from a carrier map.
-spec extract_context(map()) -> span_ctx().
extract_context(Carrier) ->
    TraceParent = maps:get(<<"traceparent">>, Carrier, <<>>),
    parse_trace_parent(TraceParent).

%% @doc Inject trace context into a carrier map.
-spec inject_context(map()) -> map().
inject_context(Carrier) ->
    case get(?SPAN_KEY) of
        undefined -> Carrier;
        Span ->
            TraceParent = format_trace_parent(Span),
            Carrier#{<<"traceparent">> => TraceParent}
    end.

%% @doc Create a child context from the current span.
-spec child_context() -> span_ctx().
child_context() ->
    case get(?SPAN_KEY) of
        undefined ->
            #{
                trace_id => generate_trace_id(),
                span_id => generate_span_id()
            };
        Span ->
            #{
                trace_id => maps:get(trace_id, Span),
                span_id => generate_span_id(),
                parent_span_id => maps:get(span_id, Span)
            }
    end.

%% @doc Execute a function within a new span.
-spec with_span(span_name(), fun(() -> A)) -> A.
with_span(Name, Fun) ->
    start_span(Name),
    try
        Result = Fun(),
        end_span(ok),
        Result
    catch
        Type:Error:Stack ->
            end_span({error, Type, Error}),
            erlang:raise(Type, Error, Stack)
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Generate a new trace ID.
-spec generate_trace_id() -> binary().
generate_trace_id() ->
    crypto:strong_rand_bytes(?TRACE_ID_BYTES).

%% @private Generate a new span ID.
-spec generate_span_id() -> binary().
generate_span_id() ->
    crypto:strong_rand_bytes(?SPAN_ID_BYTES).

%% @private Parse W3C traceparent header.
-spec parse_trace_parent(binary()) -> span_ctx().
parse_trace_parent(<<>>) ->
    %% No parent context, generate new
    #{trace_id => generate_trace_id(), span_id => generate_span_id()};
parse_trace_parent(TraceParent) ->
    %% traceparent: version-trace_id-span_id-trace_flags
    case binary:split(TraceParent, <<"-">>) of
        [<<"00">>, TraceId, SpanId, _Flags] ->
            #{
                trace_id => hex_to_bin(TraceId),
                span_id => hex_to_bin(SpanId)
            };
        _ ->
            #{trace_id => generate_trace_id(), span_id => generate_span_id()}
    end.

%% @private Format W3C traceparent header.
-spec format_trace_parent(span()) -> binary().
format_trace_parent(Span) ->
    TraceId = bin_to_hex(maps:get(trace_id, Span)),
    SpanId = bin_to_hex(maps:get(span_id, Span)),
    <<"00-", TraceId/binary, "-", SpanId/binary, "-01">>.

%% @private Convert binary to hex string.
-spec bin_to_hex(binary()) -> binary().
bin_to_hex(Bin) ->
    <<<<(integer_to_binary(N, 16))/binary>> || <<N:4>> <= Bin>>.

%% @private Convert hex string to binary.
-spec hex_to_bin(binary()) -> binary().
hex_to_bin(Hex) ->
    hex_to_bin(Hex, <<>>).

hex_to_bin(<<>>, Acc) ->
    Acc;
hex_to_bin(<<C:8, Rest/binary>>, Acc) when C >= $0, C =< $9 ->
    hex_to_bin(Rest, <<Acc/binary, (C - $0)>>);
hex_to_bin(<<C:8, Rest/binary>>, Acc) when C >= $a, C =< $f ->
    hex_to_bin(Rest, <<Acc/binary, (C - $a + 10)>>);
hex_to_bin(<<C:8, Rest/binary>>, Acc) when C >= $A, C =< $F ->
    hex_to_bin(Rest, <<Acc/binary, (C - $A + 10)>>).

%% @private Export a span for collection.
%% Sends the span to cloud_trace_exporter if available.
-spec export_span(span()) -> ok.
export_span(Span) ->
    Duration = maps:get(end_time, Span, 0) - maps:get(start_time, Span),
    case whereis(cloud_trace_exporter) of
        undefined ->
            %% Fallback to logging if exporter not available
            logger:debug("Span completed: ~p duration=~pus trace_id=~p",
                 [maps:get(name, Span), Duration,
                  bin_to_hex(maps:get(trace_id, Span))]);
        _Pid ->
            %% Export to Cloud Trace
            cloud_trace_exporter:export_span(Span)
    end,
    ok.
