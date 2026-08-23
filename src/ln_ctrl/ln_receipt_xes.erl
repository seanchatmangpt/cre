%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 Receipt System Contributors
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @module ln_receipt_xes
%% @doc XES (eXtensible Event Stream) export for receipts and traces.
%%
%% Converts ln_ctrl receipt format to XES 2.0 event format for process mining
%% and audit trail analysis. Supports:
%%
%% - Complete and partial receipt logs
%% - Execution traces with event attributes
%% - Standard XES extensions (Concept, Lifecycle, Organizational, Time)
%% - XML serialization with proper escaping
%%
%% <h3>Basic Usage</h3>
%%
%% Export receipts to XES:
%% ```erlang
%% > Receipts = [Receipt1, Receipt2],
%% > ok = ln_receipt_xes:export_receipts(Receipts, "/tmp/receipts.xes").
%% ```
%%
%% Export execution trace:
%% ```erlang
%% > TraceEvents = ln_ctrl:trace(CaseID, 0, 100),
%% > ok = ln_receipt_xes:export_trace(TraceEvents, "/tmp/trace.xes").
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_receipt_xes).

%% Export API
-export([
    export_receipts/2,
    export_receipts/3,
    export_trace/2,
    export_trace/3,
    to_xes_log/1,
    to_xes_trace/1,
    to_xes_event/1,
    create_xes_log/1,
    to_xml/1,
    to_xml_string/1
]).

%% Type exports
-export_type([
    xes_log/0,
    xes_trace/0,
    xes_event/0,
    xes_attribute/0,
    xes_extension/0,
    xes_classifier/0,
    export_option/0,
    export_options/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES =====================================================================

%% @doc XES log structure containing traces and metadata.
-record(xes_log, {
    attributes :: [xes_attribute()],
    traces :: [xes_trace()],
    extensions :: [xes_extension()],
    classifiers :: [xes_classifier()],
    global_trace :: [xes_attribute()],
    global_event :: [xes_attribute()]
}).

%% @doc XES trace representing a case/process instance.
-record(xes_trace, {
    attributes :: [xes_attribute()],
    events :: [xes_event()]
}).

%% @doc XES event representing an activity within a trace.
-record(xes_event, {
    attributes :: [xes_attribute()]
}).

%% @doc XES attribute with key, value, and type.
-record(xes_attribute, {
    key :: binary(),
    value :: term(),
    type :: string | date | id | float | int | boolean
}).

%% @doc XES extension definition.
-record(xes_extension, {
    name :: binary(),
    uri :: binary(),
    prefix :: binary()
}).

%% @doc XES classifier definition.
-record(xes_classifier, {
    name :: binary(),
    keys :: [binary()]
}).

-opaque xes_log() :: #xes_log{}.
-opaque xes_trace() :: #xes_trace{}.
-opaque xes_event() :: #xes_event{}.
-opaque xes_attribute() :: #xes_attribute{}.
-opaque xes_extension() :: #xes_extension{}.

-opaque xes_classifier() :: #xes_classifier{}.

-type export_option() :: #{
    include_extensions => boolean(),
    include_global_trace => boolean(),
    include_global_event => boolean(),
    include_classifiers => boolean(),
    trace_name => binary()
}.

-type export_options() :: [export_option()].

%%% CONSTANTS ==================================================================

-define(XES_VERSION, <<"1.0">>).
-define(XES_FEATURES, <<"nested-attributes">>).
-define(XES_XMLNS, <<"http://www.xes-standard.org/">>).

%% Extension URIs
-define(CONCEPT_URI, <<"http://www.xes-standard.org/concept.xesext">>).
-define(LIFECYCLE_URI, <<"http://www.xes-standard.org/lifecycle.xesext">>).
-define(ORGANIZATIONAL_URI, <<"http://www.xes-standard.org/org.xesext">>).
-define(TIME_URI, <<"http://www.xes-standard.org/time.xesext">>).

%%% API ======================================================================

%%--------------------------------------------------------------------
%% @doc Export multiple receipts to XES XML file.
%%
%% Converts a list of receipts to XES format and writes to file.
%% Uses default export options.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_receipts([term()], file:filename()) -> ok | {error, term()}.
export_receipts(Receipts, FilePath) ->
    export_receipts(Receipts, FilePath, #{}).

%%--------------------------------------------------------------------
%% @doc Export multiple receipts to XES XML file with options.
%%
%% Options:
%% - include_extensions: Include standard XES extensions (default: true)
%% - include_global_trace: Include global trace attributes (default: true)
%% - include_global_event: Include global event attributes (default: true)
%% - include_classifiers: Include event classifiers (default: false)
%% - trace_name: Name for the trace (default: <<"CRE Receipts">>)
%%
%% @end
%%--------------------------------------------------------------------
-spec export_receipts([term()], file:filename(), export_option()) ->
    ok | {error, term()}.
export_receipts(Receipts, FilePath, Options) ->
    case to_xes_log(Receipts) of
        XESLog when element(1, XESLog) =:= xes_log ->
            XESLogWithOpts = apply_options(XESLog, Options),
            XML = to_xml(XESLogWithOpts),
            case file:write_file(FilePath, XML) of
                ok -> ok;
                Error -> Error
            end;
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Export execution trace to XES XML file.
%%
%% Converts trace events (from ln_ctrl:trace/3) to XES format.
%% Uses default export options.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_trace([term()], file:filename()) -> ok | {error, term()}.
export_trace(TraceEvents, FilePath) ->
    export_trace(TraceEvents, FilePath, #{}).

%%--------------------------------------------------------------------
%% @doc Export execution trace to XES XML file with options.
%%
%% Converts trace events to XES format with configurable options.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_trace([term()], file:filename(), export_option()) ->
    ok | {error, term()}.
export_trace(TraceEvents, FilePath, Options) ->
    XESLog = trace_to_xes_log(TraceEvents, Options),
    XESLogWithOpts = apply_options(XESLog, Options),
    XML = to_xml(XESLogWithOpts),
    file:write_file(FilePath, XML).

%%--------------------------------------------------------------------
%% @doc Convert receipts to XES log structure.
%%
%% Creates a complete XES log with single trace containing all receipts.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_xes_log([term()]) -> xes_log().
to_xes_log(Receipts) ->
    Trace = to_xes_trace(Receipts),
    create_xes_log([Trace]).

%%--------------------------------------------------------------------
%% @doc Convert list of receipts to XES trace.
%%
%% Each receipt becomes an event within the trace.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_xes_trace([term()]) -> xes_trace().
to_xes_trace(Receipts) ->
    Events = [to_xes_event(R) || R <- Receipts],
    #xes_trace{
        attributes = [
            attr_string(<<"concept:name">>, <<"CRE Receipt Trace">>),
            attr_id(<<"id">>, generate_trace_id())
        ],
        events = Events
    }.

%%--------------------------------------------------------------------
%% @doc Convert single receipt to XES event.
%%
%% Handles all receipt types:
%% - ln_ctrl_receipt records (build_receipt, effect_receipt)
%% - ln_receipt_log receipt maps
%% - ln_receipt_effect maps
%% - ln_receipt #receipt{} records
%%
%% @end
%%--------------------------------------------------------------------
-spec to_xes_event(term()) -> xes_event().
to_xes_event(Receipt) ->
    case Receipt of
        {build_receipt, _, _, _, _, _, _, _} ->
            build_receipt_to_event(Receipt);
        {effect_receipt, _, _, _, _, _, _, _, _} ->
            effect_receipt_to_event(Receipt);
        {receipt, _, _, _, _, _, _} ->
            core_receipt_to_event(Receipt);
        _ when is_map(Receipt) ->
            map_receipt_to_event(Receipt);
        _ ->
            generic_receipt_to_event(Receipt)
    end.

%%--------------------------------------------------------------------
%% @doc Create XES log structure from traces.
%%
%% Adds default extensions and metadata.
%%
%% @end
%%--------------------------------------------------------------------
-spec create_xes_log([xes_trace()]) -> xes_log().
create_xes_log(Traces) ->
    Extensions = standard_extensions(),
    GlobalTrace = global_trace_attributes(),
    GlobalEvent = global_event_attributes(),

    #xes_log{
        attributes = log_attributes(Traces),
        traces = Traces,
        extensions = Extensions,
        classifiers = [],
        global_trace = GlobalTrace,
        global_event = GlobalEvent
    }.

%%--------------------------------------------------------------------
%% @doc Convert XES log structure to XML iolist.
%%
%% Generates valid XES 2.0 XML.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_xml(xes_log()) -> iolist().
to_xml(#xes_log{
    attributes = LogAttrs,
    traces = Traces,
    extensions = Extensions,
    classifiers = Classifiers,
    global_trace = GlobalTrace,
    global_event = GlobalEvent
}) ->
    ["<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
     "<log xmlns=\"", ?XES_XMLNS, "\" version=\"", ?XES_VERSION,
     "\" features=\"", ?XES_FEATURES, "\">\n",
     format_extensions(Extensions),
     format_classifiers(Classifiers),
     format_global_trace(GlobalTrace),
     format_global_event(GlobalEvent),
     format_log_attributes(LogAttrs),
     [format_trace(T) || T <- Traces],
     "</log>\n"].

%%--------------------------------------------------------------------
%% @doc Convert XES log to XML binary string.
%%
%% Same as to_xml/1 but returns binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_xml_string(xes_log()) -> binary().
to_xml_string(XESLog) ->
    iolist_to_binary(to_xml(XESLog)).

%%% INTERNAL FUNCTIONS =======================================================

%%--------------------------------------------------------------------
%% Receipt to Event Converters
%%--------------------------------------------------------------------

build_receipt_to_event({build_receipt, ReceiptID, _Type, InputHash, OutputHash,
                       PrevHash, Artifacts, Timestamp}) ->
    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, <<"build_receipt">>),
            attr_id(<<"id">>, receipt_id_to_binary(ReceiptID)),
            attr_date(<<"time:timestamp">>, Timestamp),
            attr_string(<<"lifecycle:transition">>, <<"complete">>),
            attr_string(<<"cre:type">>, <<"build">>),
            attr_string(<<"cre:input_hash">>, hash_to_hex(InputHash)),
            attr_string(<<"cre:output_hash">>, hash_to_hex(OutputHash)),
            attr_string(<<"cre:prev_hash">>, hash_to_hex(PrevHash)),
            attr_int(<<"cre:artifact_count">>, maps:size(Artifacts))
        ]
    }.

effect_receipt_to_event({effect_receipt, ReceiptID, _Type, EffectID, InputHash,
                         OutputHash, PrevHash, Result, Timestamp}) ->
    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, atom_to_binary(EffectID, utf8)),
            attr_id(<<"id">>, receipt_id_to_binary(ReceiptID)),
            attr_date(<<"time:timestamp">>, Timestamp),
            attr_string(<<"lifecycle:transition">>, <<"complete">>),
            attr_string(<<"cre:type">>, <<"effect">>),
            attr_string(<<"cre:input_hash">>, hash_to_hex(InputHash)),
            attr_string(<<"cre:output_hash">>, hash_to_hex(OutputHash)),
            attr_string(<<"cre:prev_hash">>, hash_to_hex(PrevHash)),
            attr_string(<<"cre:result_type">>, result_type(Result))
        ]
    }.

core_receipt_to_event({receipt, EffectID, SpecHash, CreatedAt,
                       CompletedAt, Summary, ScopeID}) ->
    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, <<"effect_execution">>),
            attr_id(<<"id">>, receipt_id_to_binary(EffectID)),
            attr_date(<<"time:timestamp">>, CompletedAt),
            attr_string(<<"lifecycle:transition">>, <<"complete">>),
            attr_string(<<"cre:type">>, <<"execution">>),
            attr_string(<<"cre:scope_id">>, scope_id_to_binary(ScopeID)),
            attr_string(<<"cre:spec_hash">>, hash_to_hex(SpecHash)),
            attr_int(<<"cre:duration_ms">>, CompletedAt - CreatedAt),
            attr_string(<<"cre:summary">>, summary_to_binary(Summary))
        ]
    }.

map_receipt_to_event(Receipt) when is_map(Receipt) ->
    %% Handle various map receipt formats from ln_receipt_log and ln_receipt_effect
    case maps:get(type, Receipt, undefined) of
        <<"build">> ->
            map_build_receipt_to_event(Receipt);
        <<"effect">> ->
            map_effect_receipt_to_event(Receipt);
        _ ->
            generic_map_to_event(Receipt)
    end;
map_receipt_to_event(Receipt) ->
    generic_receipt_to_event(Receipt).

map_build_receipt_to_event(Receipt) ->
    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, <<"build_receipt">>),
            attr_id(<<"id">>, mget_id(Receipt)),
            attr_date(<<"time:timestamp">>, mget_timestamp(Receipt)),
            attr_string(<<"lifecycle:transition">>, mget_status(Receipt)),
            attr_string(<<"cre:type">>, <<"build">>),
            attr_string(<<"cre:input_hash">>, mget_binary(Receipt, input_hash)),
            attr_string(<<"cre:output_hash">>, mget_binary(Receipt, output_hash))
        ]
    }.

map_effect_receipt_to_event(Receipt) ->
    EffectID = maps:get(effect_id, Receipt, <<"unknown">>),
    Connector = maps:get(connector, Receipt, <<"unknown">>),
    Status = maps:get(status, Receipt, <<"unknown">>),
    Latency = maps:get(latency_ms, Receipt, 0),
    StartTime = maps:get(start_time, Receipt, 0),
    EndTime = maps:get(end_time, Receipt, StartTime),

    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, to_binary(EffectID)),
            attr_id(<<"id">>, mget_id(Receipt)),
            attr_date(<<"time:timestamp">>, EndTime),
            attr_string(<<"lifecycle:transition">>, status_to_transition(Status)),
            attr_string(<<"org:resource">>, to_binary(Connector)),
            attr_string(<<"cre:type">>, <<"effect">>),
            attr_int(<<"cre:latency_ms">>, Latency),
            attr_string(<<"cre:status">>, to_binary(Status))
        ]
    }.

generic_map_to_event(Receipt) ->
    ConceptName = to_binary(maps:get(concept_name, Receipt, <<"receipt">>)),
    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, ConceptName),
            attr_id(<<"id">>, mget_id(Receipt)),
            attr_date(<<"time:timestamp">>, mget_timestamp(Receipt))
            | generic_map_attrs(Receipt)
        ]
    }.

generic_receipt_to_event(Receipt) when is_tuple(Receipt), tuple_size(Receipt) >= 2 ->
    ReceiptID = element(2, Receipt),
    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, <<"receipt">>),
            attr_id(<<"id">>, receipt_id_to_binary(ReceiptID)),
            attr_date(<<"time:timestamp">>, erlang:monotonic_time(millisecond)),
            attr_string(<<"lifecycle:transition">>, <<"complete">>)
        ]
    };
generic_receipt_to_event(_Receipt) ->
    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, <<"receipt">>),
            attr_id(<<"id">>, generate_event_id()),
            attr_date(<<"time:timestamp">>, erlang:monotonic_time(millisecond)),
            attr_string(<<"lifecycle:transition">>, <<"complete">>)
        ]
    }.

%%--------------------------------------------------------------------
%% Trace Event Conversion
%%--------------------------------------------------------------------

trace_to_xes_log(TraceEvents, Options) ->
    TraceName = maps:get(trace_name, Options, <<"CRE Execution Trace">>),
    Events = [trace_event_to_xes(E) || E <- TraceEvents],

    Trace = #xes_trace{
        attributes = [
            attr_string(<<"concept:name">>, TraceName),
            attr_id(<<"id">>, generate_trace_id())
        ],
        events = Events
    },

    create_xes_log([Trace]).

trace_event_to_xes({Seq, Type, Op, _Ctx, Time, Scope, Cancel}) ->
    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, atom_to_binary(Op, utf8)),
            attr_id(<<"id">>, list_to_binary(io_lib:format("evt_~p", [Seq]))),
            attr_date(<<"time:timestamp">>, Time),
            attr_string(<<"lifecycle:transition">>, atom_to_binary(Type, utf8)),
            attr_int(<<"cre:sequence">>, Seq),
            attr_string(<<"cre:scope">>, atom_to_binary(Scope, utf8)),
            attr_boolean(<<"cre:cancel">>, Cancel)
        ]
    };
trace_event_to_xes(TraceEvent) when is_tuple(TraceEvent) ->
    %% Generic tuple handler
    Size = tuple_size(TraceEvent),
    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, <<"trace_event">>),
            attr_id(<<"id">>, generate_event_id()),
            attr_date(<<"time:timestamp">>, erlang:monotonic_time(millisecond)),
            attr_int(<<"cre:tuple_size">>, Size)
        ]
    };
trace_event_to_xes(_TraceEvent) ->
    %% Fallback for unknown formats
    #xes_event{
        attributes = [
            attr_string(<<"concept:name">>, <<"trace_event">>),
            attr_id(<<"id">>, generate_event_id()),
            attr_date(<<"time:timestamp">>, erlang:monotonic_time(millisecond))
        ]
    }.

%%--------------------------------------------------------------------
%% Attribute Constructors
%%--------------------------------------------------------------------

attr_string(Key, Value) when is_binary(Value) ->
    #xes_attribute{key = Key, value = Value, type = string};
attr_string(Key, Value) when is_atom(Value) ->
    #xes_attribute{key = Key, value = atom_to_binary(Value, utf8), type = string};
attr_string(Key, Value) when is_list(Value) ->
    #xes_attribute{key = Key, value = list_to_binary(Value), type = string};
attr_string(Key, Value) when is_integer(Value) ->
    #xes_attribute{key = Key, value = integer_to_binary(Value), type = string}.

attr_date(Key, Timestamp) when is_integer(Timestamp) ->
    #xes_attribute{key = Key, value = format_timestamp(Timestamp), type = date}.

attr_id(Key, Value) when is_binary(Value) ->
    #xes_attribute{key = Key, value = Value, type = id};
attr_id(Key, Value) when is_atom(Value) ->
    #xes_attribute{key = Key, value = atom_to_binary(Value, utf8), type = id};
attr_id(Key, Value) when is_list(Value) ->
    #xes_attribute{key = Key, value = list_to_binary(Value), type = id};
attr_id(Key, Value) when is_reference(Value) ->
    #xes_attribute{key = Key, value = receipt_id_to_binary(Value), type = id}.

attr_int(Key, Value) when is_integer(Value) ->
    #xes_attribute{key = Key, value = Value, type = int}.

attr_boolean(Key, true) ->
    #xes_attribute{key = Key, value = true, type = boolean};
attr_boolean(Key, false) ->
    #xes_attribute{key = Key, value = false, type = boolean};
attr_boolean(Key, Value) ->
    #xes_attribute{key = Key, value = (Value =/= false andalso Value =/= 0), type = boolean}.

%%--------------------------------------------------------------------
%% XML Formatting
%%--------------------------------------------------------------------

format_extensions([]) ->
    [];
format_extensions(Extensions) ->
    ["  <extension>\n" || _ <- Extensions] ++
    lists:map(fun format_extension/1, Extensions) ++
    ["</extension>\n" || _ <- Extensions].

format_extension(#xes_extension{name = Name, uri = Uri, prefix = Prefix}) ->
    ["    <name=\"", escape_xml(Name), "\" ",
     "uri=\"", escape_xml(Uri), "\" ",
     "prefix=\"", escape_xml(Prefix), "\" />\n"].

format_classifiers([]) ->
    [];
format_classifiers(Classifiers) ->
    ["  <classifier>\n" || _ <- Classifiers] ++
    lists:map(fun format_classifier/1, Classifiers) ++
    ["</classifier>\n" || _ <- Classifiers].

format_classifier(#xes_classifier{name = Name, keys = Keys}) ->
    ["    <name=\"", escape_xml(Name), "\" ",
     "keys=\"", escape_xml(lists:join(<<" ">>, Keys)), "\" />\n"].

format_global_trace([]) ->
    [];
format_global_trace(Attrs) ->
    ["  <global trace=\"true\">\n",
     [format_attribute(A) || A <- Attrs],
     "  </global>\n"].

format_global_event([]) ->
    [];
format_global_event(Attrs) ->
    ["  <global event=\"true\">\n",
     [format_attribute(A) || A <- Attrs],
     "  </global>\n"].

format_log_attributes([]) ->
    [];
format_log_attributes(Attrs) ->
    ["  <trace>\n" || _ <- Attrs] ++
    [format_attribute(A) || A <- Attrs].

format_trace(#xes_trace{attributes = Attrs, events = Events}) ->
    ["  <trace>\n",
     [format_attribute(A) || A <- Attrs],
     [format_event(E) || E <- Events],
     "  </trace>\n"].

format_event(#xes_event{attributes = Attrs}) ->
    ["    <event>\n",
     [format_attribute(A) || A <- Attrs],
     "    </event>\n"].

format_attribute(#xes_attribute{key = Key, value = Value, type = Type}) ->
    ["      <", atom_to_list(Type), " key=\"", escape_xml(Key), "\"",
     case Type of
         string -> [" value=\"", escape_xml(to_binary(Value)), "\" />\n"];
         date -> [" value=\"", to_binary(Value), "\" />\n"];
         id -> [" value=\"", escape_xml(to_binary(Value)), "\" />\n"];
         int -> [" value=\"", integer_to_binary(Value), "\" />\n"];
         boolean -> [" value=\"", atom_to_binary(Value, utf8), "\" />\n"]
     end].

escape_xml(Binary) when is_binary(Binary) ->
    escape_xml_loop(Binary, <<>>).

escape_xml_loop(<<>>, Acc) ->
    Acc;
escape_xml_loop(<<"&">>, Acc) ->
    escape_xml_loop(<<>>, <<Acc/binary, "&amp;">>);
escape_xml_loop(<<"<">>, Acc) ->
    escape_xml_loop(<<>>, <<Acc/binary, "&lt;">>);
escape_xml_loop(<<">">>, Acc) ->
    escape_xml_loop(<<>>, <<Acc/binary, "&gt;">>);
escape_xml_loop(<<34>>, Acc) ->  %% double quote
    escape_xml_loop(<<>>, <<Acc/binary, "&quot;">>);
escape_xml_loop(<<39>>, Acc) ->  %% single quote
    escape_xml_loop(<<>>, <<Acc/binary, "&apos;">>);
escape_xml_loop(<<C:8, Rest/binary>>, Acc) ->
    escape_xml_loop(Rest, <<Acc/binary, C>>).

%%--------------------------------------------------------------------
%% Standard XES Components
%%--------------------------------------------------------------------

standard_extensions() ->
    [
        #xes_extension{
            name = <<"Concept">>,
            uri = ?CONCEPT_URI,
            prefix = <<"concept">>
        },
        #xes_extension{
            name = <<"Lifecycle">>,
            uri = ?LIFECYCLE_URI,
            prefix = <<"lifecycle">>
        },
        #xes_extension{
            name = <<"Organizational">>,
            uri = ?ORGANIZATIONAL_URI,
            prefix = <<"org">>
        },
        #xes_extension{
            name = <<"Time">>,
            uri = ?TIME_URI,
            prefix = <<"time">>
        }
    ].

global_trace_attributes() ->
    [
        attr_string(<<"concept:name">>, <<"CRE Trace">>)
    ].

global_event_attributes() ->
    [
        attr_string(<<"concept:name">>, <<"Receipt">>),
        attr_string(<<"lifecycle:transition">>, <<"complete">>)
    ].

log_attributes(Traces) ->
    [
        attr_string(<<"concept:name">>, <<"CRE Receipt Log">>),
        attr_string(<<"source">>, <<"CRE ln_ctrl">>),
        attr_date(<<"time:timestamp">>, erlang:system_time(millisecond)),
        attr_int(<<"trace_count">>, length(Traces))
    ].

%%--------------------------------------------------------------------
%% Helper Functions
%%--------------------------------------------------------------------

apply_options(Log, _Options) ->
    Log.

%% Receipt ID conversion
receipt_id_to_binary(Ref) when is_reference(Ref) ->
    list_to_binary(erlang:ref_to_list(Ref));
receipt_id_to_binary(ID) when is_integer(ID) ->
    integer_to_binary(ID);
receipt_id_to_binary(ID) when is_list(ID) ->
    list_to_binary(ID);
receipt_id_to_binary(ID) when is_binary(ID) ->
    ID.

%% Hash formatting
hash_to_hex(<<>>) ->
    <<"0000000000000000">>;
hash_to_hex(Binary) when is_binary(Binary) ->
    list_to_binary(lists:map(
        fun(B) ->
            list_to_binary(io_lib:format("~2.16.0b", [B]))
        end,
        binary_to_list(Binary)
    ));
hash_to_hex(Int) when is_integer(Int) ->
    list_to_binary(io_lib:format("~16.16.0b", [Int])).

%% Timestamp formatting (ISO 8601)
format_timestamp(Milliseconds) when is_integer(Milliseconds) ->
    %% Convert epoch ms to ISO 8601 date-time
    Seconds = Milliseconds div 1000,
    {{Year, Month, Day}, {Hour, Min, Sec}} =
        calendar:system_time_to_universal_time(Seconds, seconds),
    Ms = Milliseconds rem 1000,
    list_to_binary(io_lib:format("~4.10.0b-~2.10.0b-~2.10.0bT~2.10.0b:~2.10.0b:~2.10.0b.~3.10.0bZ",
        [Year, Month, Day, Hour, Min, Sec, Ms])).

%% ID generation
generate_trace_id() ->
    list_to_binary(io_lib:format("trace_~p", [erlang:unique_integer([positive])])).

generate_event_id() ->
    list_to_binary(io_lib:format("evt_~p", [erlang:unique_integer([positive])])).

%% Scope ID conversion
scope_id_to_binary({Tag, ID}) when is_atom(Tag) ->
    <<(atom_to_binary(Tag, utf8))/binary, ":", (to_binary(ID))/binary>>;
scope_id_to_binary(ID) ->
    to_binary(ID).

%% Result type detection
result_type(Result) when is_map(Result) -> <<"map">>;
result_type(Result) when is_list(Result) -> <<"list">>;
result_type(Result) when is_tuple(Result) -> <<"tuple">>;
result_type(Result) when is_pid(Result) -> <<"pid">>;
result_type(Result) when is_reference(Result) -> <<"reference">>;
result_type(Result) when is_port(Result) -> <<"port">>;
result_type(Result) when is_atom(Result) -> <<"atom">>;
result_type(Result) when is_integer(Result) -> <<"integer">>;
result_type(Result) when is_float(Result) -> <<"float">>;
result_type(Result) when is_binary(Result) -> <<"binary">>;
result_type(_) -> <<"unknown">>.

%% Summary to binary
summary_to_binary({Type, Value}) when is_atom(Type) ->
    <<(atom_to_binary(Type, utf8))/binary, ":", (to_binary(Value))/binary>>;
summary_to_binary(Summary) ->
    to_binary(Summary).

%% Generic map attributes
generic_map_attrs(Map) ->
    List = maps:to_list(Map),
    [attr_string(<<"cre:", K/binary>>, to_binary(V))
     || {K, V} <- List,
        K =/= type,
        K =/= id,
        K =/= timestamp,
        K =/= timestamp_ms,
        K =/= created_at,
        K =/= issued_at,
        K =/= concept_name
    ].

%% Map getters with defaults
mget_id(Map) ->
    to_binary(maps:get(id, Map, generate_event_id())).

mget_timestamp(Map) ->
    maps:get(timestamp, Map,
        maps:get(timestamp_ms, Map,
            maps:get(created_at, Map,
                maps:get(issued_at, Map, erlang:monotonic_time(millisecond))))).

mget_status(Map) ->
    to_binary(maps:get(status, Map, <<"complete">>)).

mget_binary(Map, Key) ->
    case maps:get(Key, Map, undefined) of
        undefined -> <<"">>;
        Value -> to_binary(Value)
    end.

%% Status to lifecycle transition
status_to_transition(<<"success">>) -> <<"complete">>;
status_to_transition(<<"failed">>) -> <<"abort">>;
status_to_transition(<<"pending">>) -> <<"start">>;
status_to_transition(<<"running">>) -> <<"start">>;
status_to_transition(Status) -> to_binary(Status).

%% Generic to_binary conversion
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(F) when is_float(F) -> float_to_binary(F, [{scientific, 10}]);
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(R) when is_reference(R) -> receipt_id_to_binary(R);
to_binary(P) when is_pid(P) -> list_to_binary(pid_to_list(P));
to_binary(_) -> <<"unknown">>.

%%% TESTS ====================================================================

attr_string_test() ->
    Attr = attr_string(<<"test">>, <<"value">>),
    ?assertEqual(<<"test">>, Attr#xes_attribute.key),
    ?assertEqual(<<"value">>, Attr#xes_attribute.value),
    ?assertEqual(string, Attr#xes_attribute.type).

attr_date_test() ->
    Timestamp = 1704067200000,  % 2024-01-01 00:00:00 UTC in ms
    Attr = attr_date(<<"time">>, Timestamp),
    ?assertEqual(<<"time">>, Attr#xes_attribute.key),
    ?assertEqual(date, Attr#xes_attribute.type),
    ?assert(is_binary(Attr#xes_attribute.value)).

attr_int_test() ->
    Attr = attr_int(<<"count">>, 42),
    ?assertEqual(<<"count">>, Attr#xes_attribute.key),
    ?assertEqual(42, Attr#xes_attribute.value),
    ?assertEqual(int, Attr#xes_attribute.type).

attr_boolean_test() ->
    ?assertEqual(true, (attr_boolean(<<"flag">>, true))#xes_attribute.value),
    ?assertEqual(false, (attr_boolean(<<"flag">>, false))#xes_attribute.value),
    ?assertEqual(true, (attr_boolean(<<"flag">>, 1))#xes_attribute.value),
    ?assertEqual(false, (attr_boolean(<<"flag">>, 0))#xes_attribute.value).

escape_xml_test() ->
    ?assertEqual(<<"&amp;">>, escape_xml(<<"&">>)),
    ?assertEqual(<<"&lt;">>, escape_xml(<<"<">>)),
    ?assertEqual(<<"&gt;">>, escape_xml(<<">">>)),
    ?assertEqual(<<"&quot;">>, escape_xml(<<34>>)),
    ?assertEqual(<<"&apos;">>, escape_xml(<<39>>)),
    ?assertEqual(<<"normal">>, escape_xml(<<"normal">>)).

format_timestamp_test() ->
    Ts = 1704067200000,  % 2024-01-01 00:00:00 UTC
    Formatted = format_timestamp(Ts),
    ?assert(is_binary(Formatted)),
    ?assertEqual(<<"2024-01-01T00:00:00.000Z">>, Formatted).

hash_to_hex_test() ->
    ?assertEqual(<<"0000000000000000">>, hash_to_hex(<<>>)),
    ?assertEqual(<<"ff">>, hash_to_hex(<<255>>)),
    ?assertEqual(<<"0a">>, hash_to_hex(<<10>>)).

to_xes_event_build_test() ->
    Receipt = {build_receipt, make_ref(), build, <<1:64>>, <<2:64>>,
                <<0:64>>, #{}, 1704067200000},
    Event = to_xes_event(Receipt),
    ?assertEqual(xes_event, element(1, Event)),
    ?assert(length(Event#xes_event.attributes) > 0).

to_xes_log_test() ->
    Receipt = {build_receipt, make_ref(), build, <<1:64>>, <<2:64>>,
                <<0:64>>, #{}, 1704067200000},
    Log = to_xes_log([Receipt]),
    ?assertEqual(xes_log, element(1, Log)),
    ?assert(length(Log#xes_log.traces) > 0),
    ?assert(length(Log#xes_log.extensions) > 0).

to_xml_test() ->
    Trace = #xes_trace{
        attributes = [attr_string(<<"concept:name">>, <<"Test">>)],
        events = []
    },
    Log = create_xes_log([Trace]),
    XML = to_xml_string(Log),
    ?assert(is_binary(XML)),
    ?assert(<<"<?xml" >> =< XML),
    ?assert(<<"<log">> =< XML),
    ?assert(<<"</log>">> =< XML).
