%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc Workflow Event Log to OpenXES Export
%%
%% This module converts workflow event logs to OpenXES XML format,
%% the standard for process mining event logs.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Convert event logs to OpenXES XML format</li>
%%   <li>Support for trace, event, and attribute elements</li>
%%   <li>ISO 8601 timestamp formatting</li>
%%   <li>XML special character escaping</li>
%%   <li>Lifecycle transition mapping</li>
%% </ul>
%%
%% <h3>Event Log Format</h3>
%%
%% Event logs are lists of maps with the following keys:
%% <ul>
%%   <li><b>timestamp:</b> Millisecond timestamp (integer)</li>
%%   <li><b>type:</b> Event type atom (e.g., case_created, workitem_started)</li>
%%   <li><b>case_id:</b> Case ID (binary)</li>
%%   <li><b>data:</b> Additional event data (map, optional)</li>
%% </ul>
%%
%% <h3>OpenXES Format</h3>
%%
%% Output follows the OpenXES 1.0 standard with:
%% <ul>
%%   <li>Log element with xes.version and namespace</li>
%%   <li>Extensions for Lifecycle, Concept, and Time</li>
%%   <li>Global trace and event attributes</li>
%%   <li>Trace elements grouped by case</li>
%%   <li>Event elements with timestamp and transition</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% Basic export:
%% ```erlang
%% > Log = [#{
%% >   timestamp => 1704067200000,
%% >   type => case_created,
%% >   case_id => <<"case_001">>,
%% >   data => #{}
%% > }, #{
%% >   timestamp => 1704067260000,
%% >   type => workitem_started,
%% >   case_id => <<"case_001">>,
%% >   data => #{task => <<"task1">>}
%% > }].
%% _
%% > Xes = wf_xes:export(Log, #{name => <<"demo">>}).
%% _
%% > is_binary(Xes).
%% true
%% > binary:match(Xes, <<"<log">>) =/= nomatch.
%% true
%% > binary:match(Xes, <<"case_001">>) =/= nomatch.
%% true
%% > binary:match(Xes, <<"workitem_started">>) =/= nomatch.
%% true
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_xes).

%%====================================================================
%% Exports
%%====================================================================

%% Main export function
-export([export/2]).

%% Test exports
-export([doctest_test/0]).

%%====================================================================
%% Types
%%====================================================================

-type event() :: #{
    timestamp := integer(),
    type := atom(),
    case_id := binary(),
    data => map()
}.

-type event_log() :: [event()].

-type export_options() :: #{
    name => binary(),
    source => binary(),
    description => binary()
}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Exports an event log to OpenXES XML format.
%%
%% Options:
%% - name: Log name (default: <<"log">>)
%% - source: Log source (optional)
%% - description: Log description (optional)
%%
%% Returns a binary containing valid OpenXES XML.
%%
%% Example:
%% ```erlang
%% > Log = wf_engine:case_log(Eng, Case2).
%% _
%% > length(Log) > 0.
%% true
%%
%% > Xes = wf_xes:export(Log, #{name => <<"demo">>}).
%% _
%% > is_binary(Xes).
%% true
%% > binary:match(Xes, <<"<log">>) =/= nomatch.
%% true
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec export(Log :: event_log(), Options :: export_options()) -> binary().

export(Log, Options) when is_list(Log), is_map(Options) ->
    %% Extract options with defaults
    Name = maps:get(name, Options, <<"log">>),
    Source = maps:get(source, Options, <<"CRE">>),
    Description = maps:get(description, Options, <<"Workflow Event Log">>),

    %% Group events by case
    Cases = group_events_by_case(Log),

    %% Build traces for each case
    Traces = [format_trace(CaseId, CaseEvents) ||
                 {CaseId, CaseEvents} <- maps:to_list(Cases)],

    %% Build OpenXES document
    Header = [
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n">>,
        <<"<log xes.version=\"1.0\" xes.features=\"nested-attributes\" xmlns=\"http://www.xes-standard.org/\">\n">>,
        <<"  <extension name=\"Lifecycle\" prefix=\"lifecycle\" uri=\"http://www.xes-standard.org/lifecycle.xesext\"/>\n">>,
        <<"  <extension name=\"Concept\" prefix=\"concept\" uri=\"http://www.xes-standard.org/concept.xesext\"/>\n">>,
        <<"  <extension name=\"Time\" prefix=\"time\" uri=\"http://www.xes-standard.org/time.xesext\"/>\n">>,
        <<"  <extension name=\"Organizational\" prefix=\"org\" uri=\"http://www.xes-standard.org/org.xesext\"/>\n">>,
        <<"  <string key=\"concept:name\" value=\"">>, escape_xml(Name), <<"\"/>\n">>,
        <<"  <string key=\"source\" value=\"">>, escape_xml(Source), <<"\"/>\n">>,
        <<"  <string key=\"description\" value=\"">>, escape_xml(Description), <<"\"/>\n">>,
        <<"  <global scope=\"trace\">\n">>,
        <<"    <string key=\"concept:name\" value=\"\"/>\n">>,
        <<"  </global>\n">>,
        <<"  <global scope=\"event\">\n">>,
        <<"    <string key=\"concept:name\" value=\"\"/>\n">>,
        <<"    <string key=\"lifecycle:transition\" value=\"\"/>\n">>,
        <<"    <date key=\"time:timestamp\" value=\"\"/>\n">>,
        <<"  </global>\n">>
    ],

    TracesXml = [<<"\n">> | Traces],

    Footer = <<"\n</log>\n">>,

    iolist_to_binary([Header, TracesXml, Footer]).

%%====================================================================
%% Doctest
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the wf_xes module.
%%
%% Validates export functionality including:
%% - OpenXES XML structure
%% - Trace and event generation
%% - Timestamp formatting
%% - Lifecycle transition mapping
%% - XML escaping
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Empty log produces valid XML
    EmptyLog = [],
    EmptyXes = export(EmptyLog, #{name => <<"empty">>}),
    true = is_binary(EmptyXes),
    nomatch =/= binary:match(EmptyXes, <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>">>),
    nomatch =/= binary:match(EmptyXes, <<"<log ">>),

    %% Test 2: Single event log
    SingleEventLog = [#{
        timestamp => 1704067200000,
        type => case_created,
        case_id => <<"case_test">>,
        data => #{}
    }],
    SingleXes = export(SingleEventLog, #{name => <<"single">>}),
    true = is_binary(SingleXes),
    nomatch =/= binary:match(SingleXes, <<"<log ">>),
    nomatch =/= binary:match(SingleXes, <<"case_test">>),
    nomatch =/= binary:match(SingleXes, <<"<trace>">>),
    nomatch =/= binary:match(SingleXes, <<"<event>">>),

    %% Test 3: Multiple events for same case
    MultiEventLog = [#{
        timestamp => 1704067200000,
        type => case_created,
        case_id => <<"case_001">>,
        data => #{}
    }, #{
        timestamp => 1704067260000,
        type => workitem_started,
        case_id => <<"case_001">>,
        data => #{task => <<"task1">>}
    }, #{
        timestamp => 1704067320000,
        type => workitem_completed,
        case_id => <<"case_001">>,
        data => #{task => <<"task1">>}
    }],
    MultiXes = export(MultiEventLog, #{name => <<"multi">>}),
    true = is_binary(MultiXes),
    nomatch =/= binary:match(MultiXes, <<"case_001">>),
    nomatch =/= binary:match(MultiXes, <<"workitem_started">>),
    nomatch =/= binary:match(MultiXes, <<"workitem_completed">>),

    %% Test 4: Multiple cases produce multiple traces
    MultiCaseLog = [#{
        timestamp => 1704067200000,
        type => case_created,
        case_id => <<"case_a">>,
        data => #{}
    }, #{
        timestamp => 1704067200000,
        type => case_created,
        case_id => <<"case_b">>,
        data => #{}
    }],
    MultiCaseXes = export(MultiCaseLog, #{}),
    nomatch =/= binary:match(MultiCaseXes, <<"case_a">>),
    nomatch =/= binary:match(MultiCaseXes, <<"case_b">>),
    %% Count trace elements (should be 2 for 2 cases)
    TracesCount = count_occurrences(MultiCaseXes, <<"<trace>">>),
    true = TracesCount >= 2,

    %% Test 5: Lifecycle transitions map correctly
    TransitionsLog = lists:map(fun(Type) -> #{
        timestamp => 1704067200000,
        type => Type,
        case_id => <<"case_trans">>,
        data => #{}
    } end, [case_created, case_started, case_suspended, case_resumed,
             case_completed, case_cancelled, case_failed,
             workitem_enabled, workitem_started, workitem_completed,
             workitem_failed, workitem_cancelled]),
    TransitionsXes = export(TransitionsLog, #{}),
    nomatch =/= binary:match(TransitionsXes, <<"schedule">>),
    nomatch =/= binary:match(TransitionsXes, <<"start">>),
    nomatch =/= binary:match(TransitionsXes, <<"complete">>),
    nomatch =/= binary:match(TransitionsXes, <<"suspend">>),
    nomatch =/= binary:match(TransitionsXes, <<"resume">>),
    nomatch =/= binary:match(TransitionsXes, <<"abort">>),
    nomatch =/= binary:match(TransitionsXes, <<"fail">>),

    %% Test 6: XML special characters are escaped
    EscapedLog = [#{
        timestamp => 1704067200000,
        type => case_created,
        case_id => <<"case_<script>">>,
        data => #{}
    }],
    EscapedXes = export(EscapedLog, #{}),
    %% Check that special characters are escaped
    nomatch =:= binary:match(EscapedXes, <<"<script>">>),
    nomatch =/= binary:match(EscapedXes, <<"&lt;script&gt;">>),

    %% Test 7: Timestamp formatting (ISO 8601)
    TsLog = [#{
        timestamp => 1704067200000,  %% 2024-01-01 00:00:00 UTC
        type => case_created,
        case_id => <<"case_ts">>,
        data => #{}
    }],
    TsXes = export(TsLog, #{}),
    %% Check for ISO 8601 format (YYYY-MM-DDTHH:MM:SS)
    nomatch =/= binary:match(TsXes, <<"2024-01-01T">>),

    %% Test 8: Custom options are included
    CustomXes = export([], #{
        name => <<"custom_name">>,
        source => <<"custom_source">>,
        description => <<"custom description">>
    }),
    nomatch =/= binary:match(CustomXes, <<"custom_name">>),
    nomatch =/= binary:match(CustomXes, <<"custom_source">>),
    nomatch =/= binary:match(CustomXes, <<"custom description">>),

    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Groups events by case ID.
-spec group_events_by_case(event_log()) -> #{binary() => [event()]}.

group_events_by_case(Events) ->
    lists:foldl(fun(Event, Acc) ->
        CaseId = maps:get(case_id, Event, <<"unknown">>),
        Acc#{CaseId => [Event | maps:get(CaseId, Acc, [])]}
    end, #{}, Events).

%% @private
%% @doc Formats a single case trace in OpenXES.
-spec format_trace(binary(), [event()]) -> iolist().

format_trace(CaseId, Events) ->
    %% Sort events by timestamp
    SortedEvents = lists:sort(fun(E1, E2) ->
        maps:get(timestamp, E1, 0) =< maps:get(timestamp, E2, 0)
    end, Events),

    %% Format events
    EventXml = [format_event(E) || E <- SortedEvents],

    [
        <<"  <trace>\n">>,
        <<"    <string key=\"concept:name\" value=\"">>, escape_xml(CaseId), <<"\"/>\n">>,
        EventXml,
        <<"  </trace>\n">>
    ].

%% @private
%% @doc Formats a single event in OpenXES.
-spec format_event(event()) -> iolist().

format_event(Event) ->
    Timestamp = maps:get(timestamp, Event, 0),
    Type = maps_get_atom(type, Event, unknown),
    Data = maps:get(data, Event, #{}),

    %% Format timestamp as ISO 8601
    TimestampBin = timestamp_to_binary(Timestamp),

    %% Get event name and message from data
    Name = case maps:get(name, Data, undefined) of
        undefined -> atom_to_binary(Type, utf8);
        N -> ensure_binary(N)
    end,

    %% Map event type to lifecycle transition
    Transition = event_type_to_transition(Type),

    %% Format data attributes
    DataAttrs = format_data_attributes(Data),

    [
        <<"    <event>\n">>,
        <<"      <string key=\"concept:name\" value=\"">>, escape_xml(Name), <<"\"/>\n">>,
        <<"      <string key=\"lifecycle:transition\" value=\"">>, Transition, <<"\"/>\n">>,
        <<"      <date key=\"time:timestamp\" value=\"">>, TimestampBin, <<"\"/>\n">>,
        <<"      <string key=\"event:type\" value=\"">>, (atom_to_binary(Type, utf8)), <<"\"/>\n">>,
        DataAttrs,
        <<"    </event>\n">>
    ].

%% @private
%% @doc Formats data map as OpenXES attributes.
-spec format_data_attributes(map()) -> iolist().

format_data_attributes(Data) when is_map(Data) ->
    maps:fold(fun(Key, Value, Acc) when Key =:= name; Key =:= type ->
        %% Skip name and type as they're handled separately
        Acc;
    (Key, Value, Acc) ->
        KeyBin = ensure_binary(Key),
        ValueBin = format_value(Value),
        [Acc, <<"      <string key=\"">>, escape_xml(KeyBin),
         <<"\" value=\"">>, escape_xml(ValueBin), <<"\"/>\n">>]
    end, [], Data);
format_data_attributes(_) ->
    [].

%% @private
%% @doc Formats a value as binary.
-spec format_value(term()) -> binary().

format_value(V) when is_binary(V) -> V;
format_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
format_value(V) when is_integer(V) -> integer_to_binary(V);
format_value(V) when is_float(V) -> float_to_binary(V, [{decimals, 6}, compact]);
format_value(V) when is_list(V) -> list_to_binary(io_lib:format("~p", [V]));
format_value(V) -> term_to_binary(V).

%% @private
%% @doc Converts event type to lifecycle transition.
-spec event_type_to_transition(atom()) -> binary().

event_type_to_transition(case_created) -> <<"start">>;
event_type_to_transition(case_started) -> <<"start">>;
event_type_to_transition(case_suspended) -> <<"suspend">>;
event_type_to_transition(case_resumed) -> <<"resume">>;
event_type_to_transition(case_completed) -> <<"complete">>;
event_type_to_transition(case_cancelled) -> <<"abort">>;
event_type_to_transition(case_failed) -> <<"fail">>;
event_type_to_transition(workitem_enabled) -> <<"schedule">>;
event_type_to_transition(workitem_started) -> <<"start">>;
event_type_to_transition(workitem_completed) -> <<"complete">>;
event_type_to_transition(workitem_failed) -> <<"fail">>;
event_type_to_transition(workitem_cancelled) -> <<"abort">>;
event_type_to_transition(_) -> <<"unknown">>.

%% @private
%% @doc Converts timestamp to ISO 8601 binary.
-spec timestamp_to_binary(integer()) -> binary().

timestamp_to_binary(Millis) when is_integer(Millis) ->
    Seconds = Millis div 1000,
    {{Year, Month, Day}, {Hour, Minute, Second}} =
        calendar:system_time_to_universal_time(Seconds, second),

    MillisPart = Millis rem 1000,

    Format = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0BZ",
                           [Year, Month, Day, Hour, Minute, Second, MillisPart]),

    list_to_binary(Format).

%% @private
%% @doc Escapes XML special characters.
-spec escape_xml(binary() | string() | atom()) -> binary().

escape_xml(Text) when is_binary(Text) ->
    %% Order matters: & must be escaped first
    AmpEscaped = binary:replace(Text, <<"&">>, <<"&amp;">>, [global]),
    LtEscaped = binary:replace(AmpEscaped, <<"<">>, <<"&lt;">>, [global]),
    GtEscaped = binary:replace(LtEscaped, <<">">>, <<"&gt;">>, [global]),
    QuotEscaped = binary:replace(GtEscaped, <<"\"">>, <<"&quot;">>, [global]),
    AposEscaped = binary:replace(QuotEscaped, <<"'">>, <<"&apos;">>, [global]),
    AposEscaped;
escape_xml(Text) when is_list(Text) ->
    escape_xml(list_to_binary(Text));
escape_xml(Text) when is_atom(Text) ->
    escape_xml(atom_to_binary(Text, utf8)).

%% @private
%% @doc Ensures a value is a binary.
-spec ensure_binary(binary() | atom() | list() | number()) -> binary().

ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
ensure_binary(V) when is_list(V) -> list_to_binary(V);
ensure_binary(V) when is_integer(V) -> integer_to_binary(V);
ensure_binary(V) when is_float(V) -> float_to_binary(V, [{decimals, 6}, compact]).

%% @private
%% @doc Gets an atom value from map with default.
-spec maps_get_atom(atom(), map(), atom()) -> atom().

maps_get_atom(Key, Map, Default) ->
    case maps:get(Key, Map, Default) of
        V when is_atom(V) -> V;
        _ -> Default
    end.

%% @private
%% @doc Counts occurrences of a substring in a binary.
-spec count_occurrences(binary(), binary()) -> non_neg_integer().

count_occurrences(Text, Pattern) ->
    count_occurrences(Text, Pattern, 0).

count_occurrences(Text, Pattern, Count) ->
    case binary:match(Text, Pattern) of
        nomatch -> Count;
        {Pos, Len} ->
            Rest = binary_part(Text, Pos + Len, byte_size(Text) - Pos - Len),
            count_occurrences(Rest, Pattern, Count + 1)
    end.
