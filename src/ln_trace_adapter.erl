%%%-------------------------------------------------------------------
%%% @doc ln_trace_adapter - Adapters for telemetry integration.
%%%
%%% Provides adapters to convert ln_trace events to formats used
%%% by yawl_telemetry and yawl_logging.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_trace_adapter).

%% API
-export([to_telemetry_span/1]).
-export([to_yawl_log/1]).
-export([export_to_telemetry/2]).
-export([export_to_yawl_logging/2]).

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Convert ln_trace event to yawl_telemetry span format.
-spec to_telemetry_span(ln_trace:event()) -> map().
to_telemetry_span(#{timestamp := TS, type := Type, seq := Seq, data := Data}) ->
    #{
        trace_id => generate_trace_id(),
        span_id => generate_span_id(Seq),
        parent_id => undefined,
        name => atom_to_list(Type),
        start_time => TS,
        end_time => TS,
        attributes => Data
    }.

%% @doc Convert ln_trace event to yawl_logging format.
-spec to_yawl_log(ln_trace:event()) -> map().
to_yawl_log(#{timestamp := TS, type := Type, seq := Seq, data := Data}) ->
    #{
        id => integer_to_binary(Seq),
        timestamp => TS,
        level => info,
        type => Type,
        case_id => maps:get(case_id, Data, undefined),
        workitem_id => maps:get(workitem_id, Data, undefined),
        message => format_message(Type, Data),
        data => Data
    }.

%% @doc Export ln_trace to yawl_telemetry.
-spec export_to_telemetry(ln_trace:state(), pid()) -> ok.
export_to_telemetry(TraceState, TelemetryPid) ->
    Events = ln_trace:get_all(TraceState),
    lists:foreach(fun(Event) ->
        Span = to_telemetry_span(Event),
        try
            gen_server:cast(TelemetryPid, {record_span, Span})
        catch
            _:_ ->
                %% Telemetry server might not be available or doesn't support this message
                ok
        end
    end, Events),
    ok.

%% @doc Export ln_trace to yawl_logging.
-spec export_to_yawl_logging(ln_trace:state(), pid()) -> ok.
export_to_yawl_logging(TraceState, LoggingPid) ->
    Events = ln_trace:get_all(TraceState),
    lists:foreach(fun(Event) ->
        LogEntry = to_yawl_log(Event),
        try
            gen_server:cast(LoggingPid, {log, LogEntry})
        catch
            _:_ ->
                %% Logging server might not be available or doesn't support this message
                ok
        end
    end, Events),
    ok.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @doc Generate a trace ID.
generate_trace_id() ->
    integer_to_list(erlang:unique_integer([positive])).

%% @doc Generate a span ID from sequence number.
generate_span_id(Seq) ->
    integer_to_list(Seq).

%% @doc Format event type and data into a message string.
format_message(Type, Data) ->
    TypeStr = atom_to_list(Type),
    case maps:size(Data) of
        0 ->
            TypeStr;
        _ ->
            DataStr = lists:flatten(io_lib:format("~p", [Data])),
            TypeStr ++ " " ++ DataStr
    end.
