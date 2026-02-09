%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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
%% @doc XES-based Serialization for Event Logs
%%
%% This module implements reading and writing of XES (eXtensible Event
%% Stream) event logs, the standard XML format for process mining.
%%
%% XES Specification: https://xes-standard.org/
%%
%% <h3>Key Concepts</h3>
%%
%% <ul>
%%   <li><b>Log:</b> Root element containing traces</li>
%%   <li><b>Trace:</b> Single case execution with events</li>
%%   <li><b>Event:</b> Single activity occurrence with attributes</li>
%%   <li><b>Attributes:</b> Key-value pairs with type information</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(xes_serial).

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([read_xes/1, write_xes/2]).
-export([parse_xes_string/1, to_xes_string/1]).
-export([log_to_map/1, map_to_log/1]).

%% Utility functions
-export([get_traces/1, get_events/1]).
-export([add_trace/2, add_event/3]).
-export([filter_traces/2, filter_events/2]).

%%====================================================================
%% Types
%%====================================================================

-type attribute_key() :: binary().
-type attribute_value() :: binary() | integer() | float() | boolean() | timestamp().
-type timestamp() :: {integer(), integer(), integer()}.  %% {Megaseconds, Seconds, Microseconds}

-type attribute() :: #{
    key => attribute_key(),
    value => attribute_value(),
    type => string | integer | float | boolean | date | id
}.

-type event() :: #{
    attributes => [attribute()]
}.

-type trace() :: #{
    attributes => [attribute()],
    events => [event()]
}.

-type xes_log() :: #{
    attributes => [attribute()],
    traces => [trace()],
    extensions => [map()],
    classifiers => [map()],
    globals => map()
}.

-type event_log() :: #{
    traces => [trace()],
    metadata => map()
}.

-export_type([
    attribute/0, event/0, trace/0,
    xes_log/0, event_log/0,
    attribute_key/0, attribute_value/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Reads a XES file and returns an event log.
-spec read_xes(file:filename()) -> {ok, xes_log()} | {error, term()}.
read_xes(FilePath) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            String = binary_to_list(Binary),
            parse_xes_string(String);
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.

%% @doc Writes an event log to a XES file.
-spec write_xes(file:filename(), xes_log()) -> ok | {error, term()}.
write_xes(FilePath, Log) ->
    XESString = to_xes_string(Log),
    file:write_file(FilePath, list_to_binary(XESString)).

%% @doc Parses a XES string into an event log structure.
-spec parse_xes_string(string()) -> {ok, xes_log()} | {error, term()}.
parse_xes_string(XESString) ->
    try
        %% Parse XML using xmerl
        {XMLRoot, _} = xmerl_scan:string(XESString, [{space, normalize}]),
        Log = extract_log(XMLRoot),
        {ok, Log}
    catch
        _:Error ->
            {error, {parse_error, Error}}
    end.

%% @doc Converts an event log to a XES string.
-spec to_xes_string(xes_log()) -> string().
to_xes_string(Log) ->
    %% Build XML prolog and root element
    Header = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n",
    RootOpen = "<log xes.version=\"1.0\" xes.features=\"nested-attributes\" xmlns=\"http://www.xes-standard.org/\">\n",
    RootClose = "</log>\n",

    %% Build extensions
    Extensions = build_extensions(Log),

    %% Build globals
    Globals = build_globals(Log),

    %% Build classifiers
    Classifiers = build_classifiers(Log),

    %% Build traces
    Traces = build_traces(maps:get(traces, Log, [])),

    %% Build log attributes
    LogAttrs = build_attributes(maps:get(attributes, Log, [])),

    Header ++ RootOpen ++ Extensions ++ Globals ++ Classifiers ++ LogAttrs ++ Traces ++ RootClose.

%% @doc Converts XES log to simplified map format.
-spec log_to_map(xes_log()) -> event_log().
log_to_map(XESLog) ->
    Traces = maps:get(traces, XESLog, []),
    SimplifiedTraces = [simplify_trace(T) || T <- Traces],
    #{
        traces => SimplifiedTraces,
        metadata => extract_metadata(XESLog)
    }.

%% @doc Converts simplified map format to XES log.
-spec map_to_log(event_log()) -> xes_log().
map_to_log(EventLog) ->
    Traces = maps:get(traces, EventLog, []),
    XESTraces = [expand_trace(T) || T <- Traces],
    #{
        attributes => [],
        traces => XESTraces,
        extensions => [],
        classifiers => [],
        globals => #{}
    }.

%% @doc Gets all traces from a log.
-spec get_traces(xes_log()) -> [trace()].
get_traces(Log) ->
    maps:get(traces, Log, []).

%% @doc Gets all events from a trace.
-spec get_events(trace()) -> [event()].
get_events(Trace) ->
    maps:get(events, Trace, []).

%% @doc Adds a trace to the log.
-spec add_trace(xes_log(), trace()) -> xes_log().
add_trace(Log, Trace) ->
    Traces = maps:get(traces, Log, []),
    Log#{traces => Traces ++ [Trace]}.

%% @doc Adds an event to a trace.
-spec add_event(trace(), event(), [attribute()]) -> trace().
add_event(Trace, Event, Attrs) ->
    Events = maps:get(events, Trace, []),
    EventWithAttrs = Event#{attributes => Attrs},
    Trace#{events => Events ++ [EventWithAttrs]}.

%% @doc Filters traces based on a predicate function.
-spec filter_traces(xes_log(), fun((trace()) -> boolean())) -> xes_log().
filter_traces(Log, Pred) ->
    Traces = maps:get(traces, Log, []),
    Filtered = lists:filter(Pred, Traces),
    Log#{traces => Filtered}.

%% @doc Filters events based on a predicate function.
-spec filter_events(trace(), fun((event()) -> boolean())) -> trace().
filter_events(Trace, Pred) ->
    Events = maps:get(events, Trace, []),
    Filtered = lists:filter(Pred, Events),
    Trace#{events => Filtered}.

%%====================================================================
%% Internal Functions - Parsing
%%====================================================================

%% @private
extract_log(XMLRoot) ->
    %% Extract log-level information
    Attributes = extract_attributes(XMLRoot),

    %% Extract traces
    Traces = extract_traces(XMLRoot),

    %% Extract extensions
    Extensions = extract_extensions(XMLRoot),

    %% Extract classifiers
    Classifiers = extract_classifiers(XMLRoot),

    %% Extract globals
    Globals = extract_globals(XMLRoot),

    #{
        attributes => Attributes,
        traces => Traces,
        extensions => Extensions,
        classifiers => Classifiers,
        globals => Globals
    }.

%% @private
extract_attributes(Element) ->
    case xmerl_xpath:string("./attribute", Element) of
        [] -> [];
        AttrNodes ->
            [extract_attribute(AN) || AN <- AttrNodes]
    end.

%% @private
extract_attribute(AttrNode) ->
    Key = get_attribute_value(AttrNode, "key"),
    Value = get_attribute_value(AttrNode, "value"),
    Type = get_attribute_value(AttrNode, "type", "string"),

    ParsedValue = parse_value(Value, Type),

    #{
        key => list_to_binary(Key),
        value => ParsedValue,
        type => atomize_type(Type)
    }.

%% @private
parse_value(Value, "string") -> list_to_binary(Value);
parse_value(Value, "date") ->
    %% Parse ISO 8601 timestamp
    case string:split(Value, "T") of
        [Date, Time] ->
            [Y, Mo, D] = string:split(Date, "-", all),
            [H, Mi, S] = string:split(string:sub_string(Time, 1, 19), ":", all),
            %% Simplified timestamp
            {list_to_integer(Y), list_to_integer(Mo), list_to_integer(D)};
        _ ->
            {0, 0, 0}
    end;
parse_value(Value, "int") -> list_to_integer(Value);
parse_value(Value, "float") ->
    case Value of
        "INF" -> infinity;
        "-INF" -> neg_infinity;
        "NaN" -> nan;
        _ -> list_to_float(Value)
    end;
parse_value(Value, "boolean") ->
    case string:lowercase(Value) of
        "true" -> true;
        "false" -> false;
        _ -> false
    end;
parse_value(Value, "id") -> list_to_binary(Value);
parse_value(_, _) -> <<>>.

%% @private
atomize_type("string") -> string;
atomize_type("date") -> date;
atomize_type("int") -> integer;
atomize_type("float") -> float;
atomize_type("boolean") -> boolean;
atomize_type("id") -> id;
atomize_type(_) -> string.

%% @private
extract_traces(LogElement) ->
    TraceNodes = xmerl_xpath:string("./trace", LogElement),
    [extract_trace(TN) || TN <- TraceNodes].

%% @private
extract_trace(TraceNode) ->
    Attrs = extract_attributes(TraceNode),
    EventNodes = xmerl_xpath:string("./event", TraceNode),
    Events = [extract_event(EN) || EN <- EventNodes],

    #{
        attributes => Attrs,
        events => Events
    }.

%% @private
extract_event(EventNode) ->
    Attrs = extract_attributes(EventNode),
    #{attributes => Attrs}.

%% @private
extract_extensions(LogElement) ->
    ExtNodes = xmerl_xpath:string("./extension", LogElement),
    [extract_extension(EN) || EN <- ExtNodes].

%% @private
extract_extension(ExtNode) ->
    Name = get_attribute_value(ExtNode, "name"),
    Prefix = get_attribute_value(ExtNode, "prefix"),
    Uri = get_attribute_value(ExtNode, "uri"),
    #{
        name => list_to_binary(Name),
        prefix => list_to_binary(Prefix),
        uri => list_to_binary(Uri)
    }.

%% @private
extract_classifiers(LogElement) ->
    ClassNodes = xmerl_xpath:string("./classifier", LogElement),
    [extract_classifier(CN) || CN <- ClassNodes].

%% @private
extract_classifier(ClassNode) ->
    Name = get_attribute_value(ClassNode, "name"),
    Keys = get_attribute_value(ClassNode, "keys"),
    #{
        name => list_to_binary(Name),
        keys => string:split(Keys, " ", all)
    }.

%% @private
extract_globals(LogElement) ->
    Globals = #{
        trace => extract_global_scope(LogElement, "trace"),
        event => extract_global_scope(LogElement, "event")
    },
    Globals.

%% @private
extract_global_scope(LogElement, Scope) ->
    GlobalNodes = xmerl_xpath:string("./global[@scope='" ++ Scope ++ "']", LogElement),
    [extract_attribute(AN) || GN <- GlobalNodes, AN <- xmerl_xpath:string("./attribute", GN)].

%% @private
get_attribute_value(Element, AttrName) ->
    get_attribute_value(Element, AttrName, "").

%% @private
get_attribute_value(_Element, _AttrName, Default) ->
    %% Simplified for now - would use xmerl_xpath in production
    Default.

%%====================================================================
%% Internal Functions - Serialization
%%====================================================================

%% @private
build_extensions(Log) ->
    Extensions = maps:get(extensions, Log, []),
    lists:map(fun(Ext) ->
        Name = maps:get(name, Ext, <<>>),
        Prefix = maps:get(prefix, Ext, <<>>),
        Uri = maps:get(uri, Ext, <<>>),
        io_lib:format("  <extension name=\"~s\" prefix=\"~s\" uri=\"~s\"/>~n",
                     [Name, Prefix, Uri])
    end, Extensions).

%% @private
build_globals(Log) ->
    Globals = maps:get(globals, Log, #{}),
    TraceGlobal = maps:get(trace, Globals, []),
    EventGlobal = maps:get(event, Globals, []),

    TraceAttrs = build_global_attributes("trace", TraceGlobal),
    EventAttrs = build_global_attributes("event", EventGlobal),

    TraceAttrs ++ EventAttrs.

%% @private
build_global_attributes(Scope, Attrs) ->
    case Attrs of
        [] -> "";
        _ ->
            Inner = lists:map(fun(A) ->
                build_attribute_xml(A, "    ")
            end, Attrs),
            ["  <global scope=\"", Scope, "\">>\n", Inner, "  </global>\n"]
    end.

%% @private
build_classifiers(Log) ->
    Classifiers = maps:get(classifiers, Log, []),
    lists:map(fun(Class) ->
        Name = maps:get(name, Class, <<>>),
        Keys = string:join(maps:get(keys, Class, []), " "),
        io_lib:format("  <classifier name=\"~s\" keys=\"~s\"/>~n", [Name, Keys])
    end, Classifiers).

%% @private
build_traces(Traces) ->
    lists:map(fun(T) -> build_trace(T) end, Traces).

%% @private
build_trace(Trace) ->
    Attrs = build_attributes(maps:get(attributes, Trace, []), "  "),
    Events = build_events(maps:get(events, Trace, []), "    "),

    ["  <trace>\n", Attrs, Events, "  </trace>\n"].

%% @private
build_events(Events, Indent) ->
    lists:map(fun(E) -> build_event(E, Indent) end, Events).

%% @private
build_event(Event, Indent) ->
    Attrs = build_attributes(maps:get(attributes, Event, []), Indent ++ "  "),
    [Indent, "<event>\n", Attrs, Indent, "</event>\n"].

%% @private
build_attributes(Attrs) ->
    build_attributes(Attrs, "  ").

%% @private
build_attributes(Attrs, Indent) ->
    lists:map(fun(A) -> build_attribute_xml(A, Indent) end, Attrs).

%% @private
build_attribute_xml(Attr, Indent) ->
    Key = maps:get(key, Attr, <<>>),
    Value = format_value(maps:get(value, Attr, <<>>)),
    Type = maps:get(type, Attr, string),

    [Indent, "<string key=\"", Key, "\" value=\"", Value, "\"/>", "\n"].

%% @private
format_value(Value) when is_binary(Value) -> binary_to_list(Value);
format_value(Value) when is_integer(Value) -> integer_to_list(Value);
format_value(Value) when is_float(Value) ->
    case Value of
        infinity -> "INF";
        neg_infinity -> "-INF";
        nan -> "NaN";
        _ -> float_to_list(Value, [{decimals, 10}, compact])
    end;
format_value(true) -> "true";
format_value(false) -> "false";
format_value(_) -> "".

%% @private
simplify_trace(Trace) ->
    Events = maps:get(events, Trace, []),
    SimplifiedEvents = [simplify_event(E) || E <- Events],
    #{
        attributes => simplify_attributes(maps:get(attributes, Trace, [])),
        events => SimplifiedEvents
    }.

%% @private
simplify_event(Event) ->
    Attrs = maps:get(attributes, Event, []),
    simplify_attributes(Attrs).

%% @private
simplify_attributes(Attrs) ->
    maps:from_list([{maps:get(key, A), maps:get(value, A)} || A <- Attrs]).

%% @private
expand_trace(SimpleTrace) ->
    Attrs = expand_attributes(maps:get(attributes, SimpleTrace, #{})),
    Events = [expand_event(E) || E <- maps:get(events, SimpleTrace, [])],
    #{
        attributes => Attrs,
        events => Events
    }.

%% @private
expand_event(SimpleEvent) when is_map(SimpleEvent) ->
    Attrs = expand_attributes(SimpleEvent),
    #{attributes => Attrs}.

%% @private
expand_attributes(AttrMap) when is_map(AttrMap) ->
    maps:fold(fun(Key, Value, Acc) ->
        Type = infer_type(Value),
        [#{
            key => ensure_binary(Key),
            value => Value,
            type => Type
        } | Acc]
    end, [], AttrMap);
expand_attributes(AttrList) when is_list(AttrList) -> AttrList.

%% @private
infer_type(V) when is_binary(V) -> string;
infer_type(V) when is_integer(V) -> integer;
infer_type(V) when is_float(V) -> float;
infer_type(V) when is_boolean(V) -> boolean;
infer_type(_) -> string.

%% @private
ensure_binary(B) when is_binary(B) -> B;
ensure_binary(A) when is_atom(A) -> atom_to_binary(A);
ensure_binary(S) when is_list(S) -> list_to_binary(S);
ensure_binary(I) when is_integer(I) -> integer_to_binary(I).

%% @private
extract_metadata(Log) ->
    #{
        extensions => maps:get(extensions, Log, []),
        classifiers => maps:get(classifiers, Log, []),
        globals => maps:get(globals, Log, #{})
    }.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test helpers
%%--------------------------------------------------------------------

simple_xes_log() ->
    #{
        attributes => [],
        traces => [
            #{
                attributes => [#{key => <<"concept:name">>, value => <<"Case1">>, type => string}],
                events => [
                    #{attributes => [#{key => <<"concept:name">>, value => <<"A">>, type => string}]},
                    #{attributes => [#{key => <<"concept:name">>, value => <<"B">>, type => string}]}
                ]
            }
        ],
        extensions => [],
        classifiers => [],
        globals => #{}
    }.

simple_event_log() ->
    #{
        traces => [
            #{
                attributes => #{<<"concept:name">> => <<"Case1">>},
                events => [
                    #{<<"concept:name">> => <<"A">>},
                    #{<<"concept:name">> => <<"B">>}
                ]
            }
        ],
        metadata => #{}
    }.

%%--------------------------------------------------------------------
%% Serialization tests
%%--------------------------------------------------------------------

to_xes_string_test() ->
    Log = simple_xes_log(),
    XES = to_xes_string(Log),
    ?assert(is_list(XES)),
    ?assert(string:str(XES, "<log") > 0),
    ?assert(string:str(XES, "</log>") > 0),
    ?assert(string:str(XES, "<trace>") > 0),
    ?assert(string:str(XES, "<event>") > 0).

build_attributes_test() ->
    Attrs = [#{key => <<"test">>, value => <<"value">>, type => string}],
    Result = build_attributes(Attrs),
    ?assert(is_list(Result)),
    ?assert(string:str(Result, "test") > 0),
    ?assert(string:str(Result, "value") > 0).

%%--------------------------------------------------------------------
%% Conversion tests
%%--------------------------------------------------------------------

log_to_map_test() ->
    XESLog = simple_xes_log(),
    MapLog = log_to_map(XESLog),
    ?assert(maps:is_key(traces, MapLog)),
    ?assert(maps:is_key(metadata, MapLog)).

map_to_log_test() ->
    EventLog = simple_event_log(),
    XESLog = map_to_log(EventLog),
    ?assert(maps:is_key(traces, XESLog)),
    ?assert(maps:is_key(attributes, XESLog)).

simplify_trace_test() ->
    Trace = #{
        attributes => [#{key => <<"name">>, value => <<"T1">>, type => string}],
        events => [
            #{attributes => [#{key => <<"act">>, value => <<"A">>, type => string}]}
        ]
    },
    Simplified = simplify_trace(Trace),
    ?assert(is_map(Simplified)),
    ?assert(maps:is_key(attributes, Simplified)),
    ?assert(maps:is_key(events, Simplified)).

%%--------------------------------------------------------------------
%% Utility tests
%%--------------------------------------------------------------------

get_traces_test() ->
    Log = simple_xes_log(),
    Traces = get_traces(Log),
    ?assertEqual(1, length(Traces)).

get_events_test() ->
    Trace = #{
        attributes => [],
        events => [#{attributes => []}, #{attributes => []}]
    },
    Events = get_events(Trace),
    ?assertEqual(2, length(Events)).

add_trace_test() ->
    Log = simple_xes_log(),
    NewTrace = #{attributes => [], events => []},
    NewLog = add_trace(Log, NewTrace),
    ?assertEqual(2, length(get_traces(NewLog))).

add_event_test() ->
    Trace = #{attributes => [], events => []},
    Event = #{},
    Attrs = [#{key => <<"test">>, value => <<"v">>, type => string}],
    NewTrace = add_event(Trace, Event, Attrs),
    ?assertEqual(1, length(get_events(NewTrace))).

filter_traces_test() ->
    Log = simple_xes_log(),
    Filtered = filter_traces(Log, fun(_) -> true end),
    ?assertEqual(1, length(get_traces(Filtered))).

filter_events_test() ->
    Trace = #{attributes => [], events => [#{attributes => []}, #{attributes => []}]},
    Filtered = filter_events(Trace, fun(_) -> true end),
    ?assertEqual(2, length(get_events(Filtered))).

%%--------------------------------------------------------------------
%% Type inference tests
%%--------------------------------------------------------------------

infer_type_test() ->
    ?assertEqual(string, infer_type(<<"test">>)),
    ?assertEqual(integer, infer_type(42)),
    ?assertEqual(float, infer_type(3.14)),
    ?assertEqual(boolean, infer_type(true)).

ensure_binary_test() ->
    ?assert(is_binary(ensure_binary(<<"test">>))),
    ?assert(is_binary(ensure_binary(atom))),
    ?assert(is_binary(ensure_binary("string"))),
    ?assert(is_binary(ensure_binary(123))).

-endif.
