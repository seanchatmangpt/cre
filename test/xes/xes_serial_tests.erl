%% -*- erlang -*-
%% @doc XES Serialization Tests

-module(xes_serial_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Data
%%====================================================================

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

%%====================================================================
%% Test Cases
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Serialization tests
%%--------------------------------------------------------------------

to_xes_string_test() ->
    Log = simple_xes_log(),
    XES = xes_serial:to_xes_string(Log),
    ?assert(is_list(XES)),
    ?assert(string:str(XES, "<log") > 0),
    ?assert(string:str(XES, "</log>") > 0),
    ?assert(string:str(XES, "<trace>") > 0),
    ?assert(string:str(XES, "<event>") > 0),
    ?assert(string:str(XES, "concept:name") > 0).

to_xes_string_with_extensions_test() ->
    Log = simple_xes_log()#{
        extensions => [
            #{name => <<"Time">>, prefix => <<"time">>, uri => <<"http://example.com/time">>}
        ]
    },
    XES = xes_serial:to_xes_string(Log),
    ?assert(string:str(XES, "<extension") > 0),
    ?assert(string:str(XES, "Time") > 0).

to_xes_string_with_classifiers_test() ->
    Log = simple_xes_log()#{
        classifiers => [
            #{name => <<"Activity">>, keys => [<<"concept:name">>]}
        ]
    },
    XES = xes_serial:to_xes_string(Log),
    ?assert(string:str(XES, "<classifier") > 0).

to_xes_string_empty_log_test() ->
    Log = #{attributes => [], traces => [], extensions => [], classifiers => [], globals => #{}},
    XES = xes_serial:to_xes_string(Log),
    ?assert(string:str(XES, "<log") > 0),
    ?assert(string:str(XES, "</log>") > 0).

build_attributes_test() ->
    Attrs = [
        #{key => <<"test">>, value => <<"value">>, type => string},
        #{key => <<"number">>, value => 42, type => integer}
    ],
    Result = xes_serial:build_attributes(Attrs),
    ?assert(is_list(Result)),
    ?assert(string:str(Result, "test") > 0),
    ?assert(string:str(Result, "value") > 0),
    ?assert(string:str(Result, "42") > 0).

build_attributes_empty_test() ->
    Result = xes_serial:build_attributes([]),
    ?assertEqual([], Result).

%%--------------------------------------------------------------------
%% Conversion tests
%%--------------------------------------------------------------------

log_to_map_test() ->
    XESLog = simple_xes_log(),
    MapLog = xes_serial:log_to_map(XESLog),
    ?assert(maps:is_key(traces, MapLog)),
    ?assert(maps:is_key(metadata, MapLog)),
    ?assertEqual(1, length(maps:get(traces, MapLog))).

map_to_log_test() ->
    EventLog = simple_event_log(),
    XESLog = xes_serial:map_to_log(EventLog),
    ?assert(maps:is_key(traces, XESLog)),
    ?assert(maps:is_key(attributes, XESLog)),
    ?assert(maps:is_key(extensions, XESLog)).

simplify_trace_test() ->
    Trace = #{
        attributes => [#{key => <<"name">>, value => <<"T1">>, type => string}],
        events => [
            #{attributes => [#{key => <<"act">>, value => <<"A">>, type => string}]}
        ]
    },
    Simplified = xes_serial:simplify_trace(Trace),
    ?assert(is_map(Simplified)),
    ?assert(maps:is_key(attributes, Simplified)),
    ?assert(maps:is_key(events, Simplified)),
    ?assertEqual(1, length(maps:get(events, Simplified))).

simplify_trace_empty_test() ->
    Trace = #{attributes => [], events => []},
    Simplified = xes_serial:simplify_trace(Trace),
    ?assert(is_map(Simplified)),
    ?assertEqual([], maps:get(attributes, Simplified)),
    ?assertEqual([], maps:get(events, Simplified)).

expand_trace_test() ->
    SimpleTrace = #{
        attributes => #{<<"name">> => <<"T1">>},
        events => [#{<<"act">> => <<"A">>}]
    },
    Expanded = xes_serial:expand_trace(SimpleTrace),
    ?assert(is_map(Expanded)),
    ?assert(maps:is_key(attributes, Expanded)),
    ?assert(maps:is_key(events, Expanded)),
    ?assert(is_list(maps:get(attributes, Expanded))),
    ?assert(is_list(maps:get(events, Expanded))).

expand_trace_empty_test() ->
    SimpleTrace = #{attributes => #{}, events => []},
    Expanded = xes_serial:expand_trace(SimpleTrace),
    ?assert(is_map(Expanded)),
    ?assert(is_list(maps:get(attributes, Expanded))),
    ?assert(is_list(maps:get(events, Expanded))).

%%--------------------------------------------------------------------
%% Utility tests
%%--------------------------------------------------------------------

get_traces_test() ->
    Log = simple_xes_log(),
    Traces = xes_serial:get_traces(Log),
    ?assertEqual(1, length(Traces)),
    ?assert(is_map(hd(Traces))).

get_traces_empty_test() ->
    Log = #{attributes => [], traces => [], extensions => [], classifiers => [], globals => #{}},
    Traces = xes_serial:get_traces(Log),
    ?assertEqual([], Traces).

get_events_test() ->
    Trace = #{
        attributes => [],
        events => [
            #{attributes => [#{key => <<"act">>, value => <<"A">>, type => string}]},
            #{attributes => [#{key => <<"act">>, value => <<"B">>, type => string}]}
        ]
    },
    Events = xes_serial:get_events(Trace),
    ?assertEqual(2, length(Events)).

get_events_empty_test() ->
    Trace = #{attributes => [], events => []},
    Events = xes_serial:get_events(Trace),
    ?assertEqual([], Events).

add_trace_test() ->
    Log = simple_xes_log(),
    NewTrace = #{attributes => [], events => []},
    NewLog = xes_serial:add_trace(Log, NewTrace),
    ?assertEqual(2, length(xes_serial:get_traces(NewLog))).

add_event_test() ->
    Trace = #{attributes => [], events => []},
    Event = #{},
    Attrs = [#{key => <<"test">>, value => <<"v">>, type => string}],
    NewTrace = xes_serial:add_event(Trace, Event, Attrs),
    Events = xes_serial:get_events(NewTrace),
    ?assertEqual(1, length(Events)).

add_event_multiple_test() ->
    Trace = #{attributes => [], events => []},
    Event1 = #{},
    Attrs1 = [#{key => <<"test1">>, value => <<"v1">>, type => string}],
    Event2 = #{},
    Attrs2 = [#{key => <<"test2">>, value => <<"v2">>, type => string}],
    Trace1 = xes_serial:add_event(Trace, Event1, Attrs1),
    Trace2 = xes_serial:add_event(Trace1, Event2, Attrs2),
    Events = xes_serial:get_events(Trace2),
    ?assertEqual(2, length(Events)).

filter_traces_test() ->
    Log = simple_xes_log(),
    Filtered = xes_serial:filter_traces(Log, fun(_) -> true end),
    ?assertEqual(1, length(xes_serial:get_traces(Filtered))).

filter_traces_empty_test() ->
    Log = simple_xes_log(),
    Filtered = xes_serial:filter_traces(Log, fun(_) -> false end),
    ?assertEqual(0, length(xes_serial:get_traces(Filtered))).

filter_events_test() ->
    Trace = #{
        attributes => [],
        events => [
            #{attributes => [#{key => <<"act">>, value => <<"A">>, type => string}]},
            #{attributes => [#{key => <<"act">>, value => <<"B">>, type => string}]}
        ]
    },
    Filtered = xes_serial:filter_events(Trace, fun(E) ->
        Attrs = maps:get(attributes, E, []),
        case Attrs of
            [#{value := <<"A">>} | _] -> true;
            _ -> false
        end
    end),
    ?assertEqual(1, length(xes_serial:get_events(Filtered))).

filter_events_all_test() ->
    Trace = #{
        attributes => [],
        events => [
            #{attributes => []},
            #{attributes => []}
        ]
    },
    Filtered = xes_serial:filter_events(Trace, fun(_) -> true end),
    ?assertEqual(2, length(xes_serial:get_events(Filtered))).

%%--------------------------------------------------------------------
%% Type inference tests
%%--------------------------------------------------------------------

infer_type_test() ->
    ?assertEqual(string, xes_serial:infer_type(<<"test">>)),
    ?assertEqual(integer, xes_serial:infer_type(42)),
    ?assertEqual(float, xes_serial:infer_type(3.14)),
    ?assertEqual(boolean, xes_serial:infer_type(true)),
    ?assertEqual(boolean, xes_serial:infer_type(false)).

ensure_binary_test() ->
    ?assert(is_binary(xes_serial:ensure_binary(<<"test">>))),
    ?assert(is_binary(xes_serial:ensure_binary(atom))),
    ?assert(is_binary(xes_serial:ensure_binary("string"))),
    ?assert(is_binary(xes_serial:ensure_binary(123))).

format_value_test() ->
    ?assert(is_list(xes_serial:format_value(<<"test">>))),
    ?assert(is_list(xes_serial:format_value(42))),
    ?assert(is_list(xes_serial:format_value(3.14))),
    ?assertEqual("true", xes_serial:format_value(true)),
    ?assertEqual("false", xes_serial:format_value(false)).

%%--------------------------------------------------------------------
%% File I/O tests (mock)
%%--------------------------------------------------------------------

read_xes_test() ->
    %% Test with a simple XES string
    XESString = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<log xes.version=\"1.0\">
  <trace>
    <event>
      <string key=\"concept:name\" value=\"A\"/>
    </event>
  </trace>
</log>",
    {ok, Log} = xes_serial:parse_xes_string(XESString),
    ?assert(maps:is_key(traces, Log)).

write_xes_test() ->
    Log = simple_xes_log(),
    XES = xes_serial:to_xes_string(Log),
    ?assert(is_list(XES)),
    ?assert(string:str(XES, "<?xml") > 0).

parse_xes_string_test() ->
    XESString = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<log xes.version=\"1.0\">
  <trace>
    <event>
      <string key=\"concept:name\" value=\"A\"/>
    </event>
  </trace>
</log>",
    {ok, Log} = xes_serial:parse_xes_string(XESString),
    ?assert(is_map(Log)),
    ?assert(maps:is_key(traces, Log)).

parse_xes_string_empty_test() ->
    XESString = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>
<log xes.version=\"1.0\">
</log>",
    {ok, Log} = xes_serial:parse_xes_string(XESString),
    ?assert(is_map(Log)).

%%--------------------------------------------------------------------
%% Attribute tests
%%--------------------------------------------------------------------

attribute_record_test() ->
    Attr = #{key => <<"test">>, value => <<"value">>, type => string},
    ?assertEqual(<<"test">>, maps:get(key, Attr)),
    ?assertEqual(<<"value">>, maps:get(value, Attr)),
    ?assertEqual(string, maps:get(type, Attr)).

%%--------------------------------------------------------------------
%% Trace and event record tests
%%--------------------------------------------------------------------

trace_record_test() ->
    Trace = #{
        attributes => [#{key => <<"name">>, value => <<"T1">>, type => string}],
        events => [#{attributes => []}]
    },
    ?assert(maps:is_key(attributes, Trace)),
    ?assert(maps:is_key(events, Trace)).

event_record_test() ->
    Event = #{attributes => [#{key => <<"act">>, value => <<"A">>, type => string}]},
    ?assert(maps:is_key(attributes, Event)).

%%--------------------------------------------------------------------
%% Complex log tests
%%--------------------------------------------------------------------

log_with_multiple_traces_test() ->
    Log = #{
        attributes => [],
        traces => [
            #{
                attributes => [#{key => <<"concept:name">>, value => <<"Case1">>, type => string}],
                events => [#{attributes => [#{key => <<"concept:name">>, value => <<"A">>, type => string}]}]
            },
            #{
                attributes => [#{key => <<"concept:name">>, value => <<"Case2">>, type => string}],
                events => [#{attributes => [#{key => <<"concept:name">>, value => <<"B">>, type => string}]}]
            }
        ],
        extensions => [],
        classifiers => [],
        globals => #{}
    },
    XES = xes_serial:to_xes_string(Log),
    ?assert(string:str(XES, "<trace>") > 0),
    ?assert(string:str(XES, "Case1") > 0),
    ?assert(string:str(XES, "Case2") > 0).
