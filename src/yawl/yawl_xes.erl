%% -*- erlang -*-
-module(yawl_xes).

-export([new_log/1, close_log/1, log_case_start/2, log_case_end/1, log_event/4, log_event/5, get_log/1, export_to_xml/1, export_to_file/2, replay_trace/2, replay_with_opts/2, replay_with_opts/5, query_traces/2, get_statistics/1]).

-on_load(init_ets/0).

%%====================================================================
%% Types
%%====================================================================

-type log_id() :: binary().
-type case_id() :: binary().
-type timestamp_ms() :: integer().
-type event_concept() :: binary().
-type event_lifecycle() :: binary().
-type event_data() :: map().
-type event() :: {timestamp_ms(), event_concept(), event_lifecycle(), event_data()}.
-type trace() :: {case_id(), timestamp_ms(), timestamp_ms() | undefined, [event()]}.
-type trace_filter() :: {concept, event_concept()} |
                      {lifecycle, event_lifecycle()} |
                      {time_range, {timestamp_ms(), timestamp_ms()}} |
                      {data_matches, event_data()}.
-type statistics() :: #{
    total_traces => non_neg_integer(),
    total_events => non_neg_integer(),
    avg_events_per_trace => float(),
    total_duration_ms => non_neg_integer()
}.
-type replay_options() :: [tuple() | atom()].

-export_type([log_id/0, case_id/0, timestamp_ms/0, event_concept/0,
               event_lifecycle/0, event_data/0, event/0, trace/0,
               trace_filter/0, statistics/0, replay_options/0]).

%%====================================================================
%% ETS Initialization
%%====================================================================

-spec init_ets() -> ok.
init_ets() ->
    case ets:info(log_state) of
        undefined -> ets:new(log_state, [named_table, public, set]);
        _ -> ok
    end.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

-spec generate_log_id() -> log_id().
generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    binary:encode_hex(Unique).

-spec format_timestamp_ms(timestamp_ms()) -> binary().
format_timestamp_ms(Ms) when is_integer(Ms) ->
    Seconds = Ms div 1000,
    DateTime = calendar:system_time_to_universal_time(Seconds),
    Fraction = Ms rem 1000,
    Year = element(1, DateTime),
    Formatted = io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B.~3..0B",
        [Year, element(2, DateTime), element(3, DateTime),
         element(4, DateTime), element(5, DateTime),
         Fraction div 100]),
    iolist_to_binary(Formatted).

-spec escape_xml_attr(binary() | string() | atom() | integer()) -> iolist().
escape_xml_attr(Text) when is_binary(Text) ->
    escape_xml_chars(binary_to_list(Text));
escape_xml_attr(Text) when is_list(Text) ->
    escape_xml_chars(Text);
escape_xml_attr(Text) when is_atom(Text) ->
    escape_xml_chars(atom_to_list(Text));
escape_xml_attr(Text) when is_integer(Text) ->
    escape_xml_chars(integer_to_list(Text)).

-spec escape_xml_chars([char()] | [byte()]) -> [char()] | [byte()].
escape_xml_chars([]) -> [];
escape_xml_chars([$< | Rest]) -> "&lt;" ++ escape_xml_chars(Rest);
escape_xml_chars([$> | Rest]) -> "&gt;" ++ escape_xml_chars(Rest);
escape_xml_chars([$& | Rest]) -> "&amp;" ++ escape_xml_chars(Rest);
escape_xml_chars([$" | Rest]) -> "&quot;" ++ escape_xml_chars(Rest);
escape_xml_chars([$' | Rest]) -> "&apos;" ++ escape_xml_chars(Rest);
escape_xml_chars([C | Rest]) -> [C | escape_xml_chars(Rest)].

%%====================================================================
%% API Functions
%%====================================================================

-spec new_log(map()) -> {ok, log_id()} | {error, term()}.
new_log(Metadata) when is_map(Metadata) ->
    LogId = generate_log_id(),
    ProcessName = case maps:get(<<"process">>, Metadata, undefined) of
        undefined -> <<"UnknownProcess">>;
        P -> P
    end,
    Log = {LogId, ProcessName, erlang:system_time(millisecond), [], undefined},
    case ets:insert(log_state, LogId, Log) of
        true ->
            logger:debug("Created new XES log: ~p for process: ~p", [LogId, ProcessName]),
            {ok, LogId};
        {error, Reason} ->
            logger:error("Failed to create XES log: ~p", [Reason]),
            {error, Reason}
    end.

-spec close_log(log_id()) -> ok | {error, term()}.
close_log(LogId) ->
    case ets:lookup(log_state, LogId) of
        [{_LogId, _Process, StartTime, Traces, _CurrentTrace}] when Traces =/= [] ->
            ReverseTraces = lists:reverse(Traces),
            LookupResult = ets:lookup(log_state, LogId),
            Process = element(2, hd(LookupResult)),
            UpdatedLog = {LogId, Process, StartTime, ReverseTraces, undefined},
            ets:insert(log_state, LogId, UpdatedLog),
            logger:debug("Closed XES log: ~p", [LogId]),
            ok;
        [] ->
            {error, not_found}
    end.

-spec log_case_start(log_id(), case_id()) -> ok | {error, term()}.
log_case_start(LogId, CaseId) ->
    case ets:lookup(log_state, LogId) of
        [{LogId, Process, StartTime, Traces, _CurrentTrace}] ->
            Trace = {CaseId, erlang:system_time(millisecond), undefined, []},
            UpdatedLog = {LogId, Process, StartTime, [Trace | Traces], Trace},
            ets:insert(log_state, LogId, UpdatedLog),
            logger:debug("Started trace case ~p in log ~p", [CaseId, LogId]),
            ok;
        [] ->
            {error, log_not_found}
    end.

-spec log_case_end(log_id()) -> ok | {error, term()}.
log_case_end(LogId) ->
    case ets:lookup(log_state, LogId) of
        [{LogId, Process, StartTime, Traces, CurrentTrace}] when CurrentTrace =/= undefined ->
            EndTime = erlang:system_time(millisecond),
            TraceStartTime = element(2, CurrentTrace),
            Duration = EndTime - TraceStartTime,
            FinalizedTrace = setelement(3, CurrentTrace, EndTime),
            logger:debug("Ended trace case ~p in log ~p duration: ~pms", [element(1, CurrentTrace), LogId, Duration]),
            UpdatedTraces = lists:reverse([FinalizedTrace | lists:reverse(Traces)]),
            UpdatedLog = {LogId, Process, StartTime, UpdatedTraces, undefined},
            ets:insert(log_state, LogId, UpdatedLog),
            ok;
        [] ->
            {error, no_active_trace}
    end.

-spec log_event(log_id(), event_concept(), event_lifecycle(), event_data()) -> ok | {error, term()}.
log_event(LogId, Concept, Lifecycle, Data) ->
    log_event(LogId, Concept, Lifecycle, erlang:system_time(millisecond), Data).

-spec log_event(log_id(), event_concept(), event_lifecycle(), timestamp_ms(), event_data()) -> ok | {error, term()}.
log_event(LogId, Concept, Lifecycle, Timestamp, Data) when is_map(Data) ->
    Event = {Timestamp, Concept, Lifecycle, Data},
    add_event_to_trace(LogId, Event);
log_event(_LogId, _Concept, _Lifecycle, _Timestamp, _Data) ->
    {error, invalid_data}.

-spec add_event_to_trace(log_id(), event()) -> ok | {error, term()}.
add_event_to_trace(LogId, Event) ->
    case ets:lookup(log_state, LogId) of
        [{LogId, Process, StartTime, Traces, CurrentTrace}] when CurrentTrace =/= undefined ->
            {_CaseId, _TraceStart, EndTime, Events} = CurrentTrace,
            UpdatedTrace = {element(1, CurrentTrace), element(2, CurrentTrace), EndTime, [Event | Events]},
            UpdatedLog = {LogId, Process, StartTime, Traces, UpdatedTrace},
            ets:insert(log_state, LogId, UpdatedLog),
            ok;
        [] ->
            {error, no_active_trace}
    end.

-spec get_log(log_id()) -> {ok, map()} | {error, term()}.
get_log(LogId) ->
    case ets:lookup(log_state, LogId) of
        [{_LogId, Process, _StartTime, Traces, _CurrentTrace}] ->
            TraceCount = length(Traces),
            EventCount = lists:foldl(fun({_, _, _, Evs}, Acc) -> Acc + length(Evs) end, 0, Traces),
            {ok, #{log_id => LogId, process => Process, trace_count => TraceCount, event_count => EventCount}};
        [] ->
            {error, not_found}
    end.

-spec export_to_xml(log_id()) -> {ok, binary()} | {error, term()}.
export_to_xml(LogId) ->
    case ets:lookup(log_state, LogId) of
        [{_LogId, _Process, _StartTime, Traces, _CurrentTrace}] ->
            ReversedTraces = lists:reverse(Traces),
            XML = build_xes_xml(ReversedTraces),
            {ok, XML};
        [] ->
            {error, log_not_found}
    end.

-spec export_to_file(log_id(), binary() | string()) -> {ok, string()} | {error, term()}.
export_to_file(LogId, FilePath) when is_binary(FilePath) ->
    export_to_file(LogId, binary_to_list(FilePath));
export_to_file(LogId, FilePath) when is_list(FilePath) ->
    case export_to_xml(LogId) of
        {ok, XML} ->
            Dirname = filename:dirname(FilePath),
            case filelib:ensure_dir(Dirname) of
                ok ->
                    case file:write_file(FilePath, XML) of
                        ok ->
                            logger:info("Exported XES log to ~s", [FilePath]),
                            {ok, FilePath};
                        {error, WriteReason} ->
                            {error, {write_failed, WriteReason}}
                    end;
                {error, DirReason} ->
                    {error, {directory_failed, DirReason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec replay_trace(trace(), function()) -> {ok, term()} | {error, term()}.
replay_trace({Events, _CaseId, _StartTime, _EndTime}, Callback) when is_function(Callback, 2) ->
    replay_loop(Events, Callback, 0, ok);
replay_trace(_Trace, _Callback) ->
    {error, invalid_trace}.

-spec replay_with_opts(trace(), replay_options()) -> {ok, term()} | {error, term()}.
replay_with_opts({Events, _CaseId, _StartTime, _EndTime}, Options) when is_list(Options) ->
    Strict = proplists:get_value(strict, Options, false),
    Timeout = proplists:get_value(timeout, Options, infinity),
    Callback = proplists:get_value(callback, Options, fun(_, A) -> {ok, A} end),
    case Timeout of
        infinity ->
            replay_with_opts(Events, Callback, Strict, 0, ok);
        _ ->
            StartTime = erlang:monotonic_time(millisecond),
            replay_with_timeout(Events, Callback, Strict, 0, ok, StartTime, Timeout)
    end;
replay_with_opts(_Trace, _Options) ->
    {error, invalid_trace}.

-spec query_traces(log_id(), [trace_filter()]) -> {ok, [trace()]} | {error, term()}.
query_traces(LogId, Filters) when is_list(Filters) ->
    case ets:lookup(log_state, LogId) of
        [{_LogId, _Process, _StartTime, Traces, _CurrentTrace}] ->
            Filtered = lists:filter(fun(Trace) -> trace_matches(Trace, Filters) end, Traces),
            {ok, Filtered};
        [] ->
            {error, log_not_found}
    end;
query_traces(_LogId, _Filters) ->
    {error, invalid_filters}.

-spec get_statistics(log_id()) -> {ok, statistics()} | {error, term()}.
get_statistics(LogId) ->
    case ets:lookup(log_state, LogId) of
        [{_LogId, _Process, _StartTime, Traces, _CurrentTrace}] ->
            Stats = calc_stats(Traces),
            {ok, Stats};
        [] ->
            {error, log_not_found}
    end.

%%====================================================================
%% XML Building Functions
%%====================================================================

-spec build_xes_xml([trace()]) -> binary().
build_xes_xml(Traces) ->
    TraceXmls = [build_trace_xml(Trace) || Trace <- Traces],
    iolist_to_binary([
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
        "<log xmlns=\"http://www.xes-standard.org/xes.xesext\"",
        " xmlns:xes=\"http://www.xes-standard.org/xes.xesext\"",
        " xmlns:time=\"http://www.xes-standard.org/xes.xesext/time.xesext\"",
        " xmlns:concept=\"http://www.xes-standard.org/xes.xesext/concept.xesext\"",
        " xmlns:lifecycle=\"http://www.xes-standard.org/xes.xesext/lifecycle.xesext\"",
        " xmlns:cre=\"http://cre.dev/xes\"",
        " xes:version=\"1.0\" xes:features=\"nested-attributes\">",
        "  <extension name=\"cre\" uri=\"http://cre.dev/xes\"/>",
        "  <extension name=\"concept\" uri=\"http://www.xes-standard.org/xes.xesext/concept.xesext\"/>",
        "  <extension name=\"lifecycle\" uri=\"http://www.xes-standard.org/xes.xesext/lifecycle.xesext\"/>",
        "  <extension name=\"time\" uri=\"http://www.xes-standard.org/xes.xesext/time.xesext\"/>",
        "  <global scope=\"trace\">",
        "    <string key=\"concept:name\" value=\"Case\"/>",
        "    <string key=\"cre:instance_id\" value=\"id\"/>",
        "  </global>",
        "  <global scope=\"event\">",
        "    <string key=\"concept:name\" value=\"string\"/>",
        "    <string key=\"lifecycle:transition\" value=\"string\"/>",
        "    <string key=\"time:timestamp\" value=\"date\"/>",
        "  </global>",
        "  <classifier name=\"cre:lifecycle\" keys=\"lifecycle:transition\"/>",
        "  <classifier name=\"cre:concept\" keys=\"concept:name\"/>",
        TraceXmls,
        "</log>"
    ]).

-spec build_trace_xml(trace()) -> binary().
build_trace_xml({CaseId, StartTime, EndTime, Events}) ->
    Duration = case EndTime of
        undefined -> 0;
        _ -> EndTime - StartTime
    end,
    EventsXmls = [build_event_xml(E) || E <- lists:reverse(Events)],
    iolist_to_binary([
        "  <trace>",
        "    <string key=\"cre:instance_id\" value=\"", escape_xml_attr(CaseId), "\"/>",
        "    <string key=\"concept:name\" value=\"Case\"/>",
        "    <date key=\"time:timestamp\" value=\"", format_timestamp_ms(StartTime), "\"/>",
        case EndTime of
            undefined -> [];
            _ -> ["    <long key=\"cre:duration_ms\" value=\"", integer_to_binary(Duration), "\"/>"]
        end,
        EventsXmls,
        "  </trace>"
    ]).

-spec build_event_xml(event()) -> binary().
build_event_xml({Timestamp, Concept, Lifecycle, Data}) ->
    Attrs = build_attrs(Data),
    iolist_to_binary([
        "    <event>",
        "      <string key=\"concept:name\" value=\"", escape_xml_attr(Concept), "\"/>",
        "      <string key=\"lifecycle:transition\" value=\"", escape_xml_attr(Lifecycle), "\"/>",
        "      <date key=\"time:timestamp\" value=\"", format_timestamp_ms(Timestamp), "\"/>",
        Attrs,
        "    </event>"
    ]).

-spec build_attrs(event_data()) -> iolist().
build_attrs(Data) ->
    lists:flatten([build_attr(K, V) || {K, V} <- maps:to_list(Data)]).

-spec build_attr(binary(), term()) -> iolist().
build_attr(Key, Value) when is_binary(Value); Value =:= <<>> ->
    iolist_to_binary(["      <string key=\"cre:", Key/binary, "\" value=\"", escape_xml_attr(Value), "\"/>"]);
build_attr(Key, Value) when is_integer(Value) ->
    iolist_to_binary(["      <long key=\"cre:", Key/binary, "\" value=\"", integer_to_binary(Value), "\"/>"]);
build_attr(Key, Value) when is_float(Value) ->
    iolist_to_binary(["      <float key=\"cre:", Key/binary, "\" value=\"", float_to_binary(Value, [{decimals, 6}, compact]), "\"/>"]);
build_attr(Key, Value) when is_boolean(Value) ->
    BoolStr = case Value of true -> "true"; false -> "false" end,
    iolist_to_binary(["      <boolean key=\"cre:", Key/binary, "\" value=\"", BoolStr, "\"/>"]);
build_attr(Key, Value) when is_atom(Value) ->
    iolist_to_binary(["      <string key=\"cre:", Key/binary, "\" value=\"", escape_xml_attr(atom_to_binary(Value)), "\"/>"]);
build_attr(Key, Value) when is_list(Value) ->
    Items = [escape_xml_attr(format_list_val(V)) || V <- Value],
    ListInner = [["        <value>", I, "</value>"] || I <- Items],
    iolist_to_binary(["      <list key=\"cre:", Key/binary, "\">", ListInner, "      </list>"]);
build_attr(_, _) ->
    [].

-spec format_list_val(term()) -> binary().
format_list_val(V) when is_binary(V) -> V;
format_list_val(V) when is_atom(V) -> atom_to_binary(V);
format_list_val(V) when is_integer(V) -> integer_to_binary(V);
format_list_val(V) when is_float(V) -> float_to_binary(V, [{decimals, 6}, compact]);
format_list_val(V) when is_list(V) ->
    iolist_to_binary([format_list_val(VV) || VV <- V]).

%%====================================================================
%% Trace Matching Functions
%%====================================================================

-spec trace_matches(trace(), [trace_filter()]) -> boolean().
trace_matches({CaseId, _StartTime, _EndTime, Events}, Filters) ->
    TraceTuple = {Events, CaseId},
    lists:all(fun(Filter) -> match_filter(Filter, TraceTuple) end, Filters).

-spec match_filter(trace_filter(), {[event()], case_id()}) -> boolean().
match_filter({concept, Concept}, {Events, _CaseId}) ->
    lists:any(fun({_Ts, _C, _L, _D}) -> _C =:= Concept end, Events);
match_filter({lifecycle, Lifecycle}, {Events, _CaseId}) ->
    lists:any(fun({_Ts, _C, _L, _D}) -> _L =:= Lifecycle end, Events);
match_filter({time_range, {Start, End}}, {Events, _CaseId}) ->
    lists:any(fun({Ts, _C, _L, _D}) -> Ts >= Start andalso Ts < End end, Events);
match_filter({data_matches, Data}, {Events, _CaseId}) ->
    lists:any(fun({_Ts, _C, _L, D}) -> maps:fold(fun(K, V, A) -> A andalso maps:get(K, D, undefined) =:= V end, true, Data) end, Events);
match_filter(_, _) ->
    true.

%%====================================================================
%% Replay Functions
%%====================================================================

-spec replay_loop([event()], function(), non_neg_integer(), term()) -> {ok, term()} | {error, term()}.
replay_loop([], _Callback, _Index, Acc) ->
    {ok, Acc};
replay_loop([Event | Rest], Callback, Index, Acc) ->
    case Callback(Event, Acc) of
        {ok, NewAcc} ->
            replay_loop(Rest, Callback, Index + 1, NewAcc);
        {error, Reason} ->
            {error, {replay_failed, Index, Reason}}
    end.

-spec replay_with_opts([event()], function(), boolean(), non_neg_integer(), term()) -> {ok, term()} | {error, term()}.
replay_with_opts([], _Callback, _Strict, _Index, Acc) ->
    {ok, Acc};
replay_with_opts([Event | Rest], Callback, Strict, Index, Acc) ->
    case Callback(Event, Acc) of
        {ok, NewAcc} ->
            replay_with_opts(Rest, Callback, Strict, Index + 1, NewAcc);
        {error, _Reason} ->
            case Strict of
                true -> {error, {replay_failed, Index, _Reason}};
                false -> replay_with_opts(Rest, Callback, Strict, Index + 1, Acc)
            end
    end.

-spec replay_with_timeout([event()], function(), boolean(), non_neg_integer(), term(), integer(), integer()) -> {ok, term()} | {error, term()}.
replay_with_timeout([], _Callback, _Strict, _Index, Acc, _StartTime, _Timeout) ->
    {ok, Acc};
replay_with_timeout([Event | Rest], Callback, Strict, Index, Acc, StartTime, Timeout) ->
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    case Elapsed > Timeout of
        true -> {error, timeout};
        _ ->
            case Callback(Event, Acc) of
                {ok, NewAcc} ->
                    replay_with_timeout(Rest, Callback, Strict, Index + 1, NewAcc, StartTime, Timeout);
                {error, Reason} ->
                    case Strict of
                        true -> {error, {replay_failed, Index, Reason}};
                        false -> replay_with_timeout(Rest, Callback, Strict, Index + 1, Acc, StartTime, Timeout)
                    end
            end
    end.

%%====================================================================
%% Statistics Functions
%%====================================================================

-spec calc_stats([trace()]) -> statistics().
calc_stats([]) ->
    #{total_traces => 0, total_events => 0, avg_events_per_trace => 0.0, total_duration_ms => 0};
calc_stats(Traces) ->
    TraceCount = length(Traces),
    EventCounts = [length(element(4, T)) || T <- Traces],
    TotalEvents = lists:sum(EventCounts),
    Durs = [calc_dur(T) || T <- Traces, element(3, T) =/= undefined],
    AvgEvents = safe_div(TotalEvents, TraceCount),
    #{total_traces => TraceCount, total_events => TotalEvents, avg_events_per_trace => AvgEvents, total_duration_ms => lists:sum(Durs)}.

-spec calc_dur(trace()) -> non_neg_integer().
calc_dur({_CaseId, StartTime, EndTime, _Events}) when EndTime =/= undefined ->
    EndTime - StartTime;
calc_dur(_) ->
    0.

-spec safe_div(number(), number()) -> float().
safe_div(_, 0) -> 0.0;
safe_div(N, D) -> N / D.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test ETS initialization
init_ets_test() ->
    ?assertNotEqual(undefined, ets:info(log_state)).

%% Test log ID generation
generate_log_id_test() ->
    LogId = generate_log_id(),
    ?assert(is_binary(LogId)),
    ?assertEqual(32, byte_size(LogId)).

%% Test timestamp formatting
format_timestamp_ms_test() ->
    ?assertEqual(<<"1970-01-01T00:00:00.000">>, format_timestamp_ms(0)),
    ?assertMatch(<<"20", _/binary>>, format_timestamp_ms(1000000000)).

%% Test XML attribute escaping
escape_xml_attr_test() ->
    ?assertEqual("&lt;", escape_xml_attr(<<"<">>)),
    ?assertEqual("&gt;", escape_xml_attr(<<">">>)),
    ?assertEqual("&amp;", escape_xml_attr(<<"&">>)),
    ?assertEqual("&quot;", escape_xml_attr(<<"\"">>)),
    ?assertEqual("&apos;", escape_xml_attr(<<"'">>)).

%% Test new_log/1
new_log_test() ->
    {ok, LogId} = new_log(#{<<"process">> => <<"TestProcess">>}),
    ?assert(is_binary(LogId)),
    ?assertMatch({_, <<"TestProcess">>, _, [], undefined}, ets:lookup(log_state, LogId)),
    ets:delete(log_state, LogId).

new_log_with_default_process_test() ->
    {ok, LogId} = new_log(#{}),
    [{_, Process, _, _, _}] = ets:lookup(log_state, LogId),
    ?assertEqual(<<"UnknownProcess">>, Process),
    ets:delete(log_state, LogId).

%% Test log_case_start/2
log_case_start_test() ->
    {ok, LogId} = new_log(#{<<"process">> => <<"TestCaseStart">>}),
    ok = log_case_start(LogId, <<"case123">>),
    [{_, _, _, [Trace | _], Trace}] = ets:lookup(log_state, LogId),
    ?assertEqual(<<"case123">>, element(1, Trace)),
    ets:delete(log_state, LogId).

log_case_start_not_found_test() ->
    ?assertEqual({error, log_not_found}, log_case_start(<<"nonexistent">>, <<"case">>)).

%% Test log_case_end/1
log_case_end_test() ->
    {ok, LogId} = new_log(#{<<"process">> => <<"TestCaseEnd">>}),
    ok = log_case_start(LogId, <<"case456">>),
    timer:sleep(10),
    ok = log_case_end(LogId),
    [{_, _, _, Traces, undefined}] = ets:lookup(log_state, LogId),
    ?assertEqual(1, length(Traces)),
    {_, _, EndTime, _, _} = lists:last(Traces),
    ?assert(EndTime > 0),
    ets:delete(log_state, LogId).

log_case_end_no_active_trace_test() ->
    ?assertEqual({error, no_active_trace}, log_case_end(<<"nonexistent">>)).

%% Test log_event/4
log_event_test() ->
    {ok, LogId} = new_log(#{<<"process">> => <<"TestEvent">>}),
    ok = log_case_start(LogId, <<"case789">>),
    ok = log_event(LogId, <<"TestConcept">>, <<"TestLifecycle">>, #{<<"key">> => <<"value">>}),
    [{_, _, _, [Trace], _}] = ets:lookup(log_state, LogId),
    {_, _, _, Events} = Trace,
    ?assertEqual(1, length(Events)),
    ets:delete(log_state, LogId).

log_event_3arity_test() ->
    {ok, LogId} = new_log(#{<<"process">> => <<"TestEvent3">>}),
    ok = log_case_start(LogId, <<"case999">>),
    ok = log_event(LogId, <<"Concept3">>, <<"Lifecycle3">>, #{<<"data">> => 123}),
    [{_, _, _, [Trace], _}] = ets:lookup(log_state, LogId),
    {_, _, _, Events} = Trace,
    ?assertEqual(1, length(Events)),
    ets:delete(log_state, LogId).

log_event_invalid_data_test() ->
    {ok, LogId} = new_log(#{<<"process">> => <<"TestInvalid">>}),
    ?assertEqual({error, invalid_data}, log_event(LogId, <<"C">>, <<"L">>, not_a_map)),
    ets:delete(log_state, LogId).

%% Test get_log/1
get_log_test() ->
    {ok, LogId} = new_log(#{<<"process">> => <<"GetLogTest">>}),
    ok = log_case_start(LogId, <<"case1">>),
    ok = log_event(LogId, <<"E1">>, <<"start">>, #{}),
    ok = log_event(LogId, <<"E2">>, <<"complete">>, #{}),
    {ok, Info} = get_log(LogId),
    ?assertEqual(LogId, maps:get(log_id, Info)),
    ?assertEqual(1, maps:get(trace_count, Info)),
    ?assertEqual(2, maps:get(event_count, Info)),
    ets:delete(log_state, LogId).

get_log_not_found_test() ->
    ?assertEqual({error, not_found}, get_log(<<"nonexistent">>)).

%% Test export_to_xml/1
export_to_xml_test() ->
    {ok, LogId} = new_log(#{<<"process">> => <<"ExportTest">>}),
    ok = log_case_start(LogId, <<"case_xml">>),
    ok = log_event(LogId, <<"Activity">>, <<"start">>, #{<<"action">> => <<"test">>}),
    ok = log_case_end(LogId),
    {ok, XML} = export_to_xml(LogId),
    ?assert(is_binary(XML)),
    ?assertNotEqual(<<>>, XML),
    ?assertMatch(<<"<?xml", _/binary>>, XML),
    ?assertMatch(<<"<log", _/binary>>, XML),
    ets:delete(log_state, LogId).

export_to_xml_not_found_test() ->
    ?assertEqual({error, log_not_found}, export_to_xml(<<"nonexistent">>)).

%% Test build_xes_xml/1
build_xes_xml_test() ->
    Trace = {<<"case1">>, 1000000000, 1000100, [
        {1000000000, <<"Activity">>, <<"start">>, #{<<"task">> => <<"A">>}},
        {1000000050, <<"Activity">>, <<"complete">>, #{<<"result">> => <<"done">>>}
    ]},
    XML = build_xes_xml([Trace]),
    ?assert(is_binary(XML)),
    ?assertMatch(<<"<?xml", _/binary>>, XML),
    ?assertMatch(<<"<log", _/binary>>, XML),
    ?assertMatch(<<"xmlns:cre=\"http://cre.dev/xes\"">>, XML).

%% Test build_trace_xml/1
build_trace_xml_test() ->
    Events = [
        {1000, <<"E1">>, <<"start">>, #{}},
        {2000, <<"E2">>, <<"complete">>, #{}}
    ],
    Trace = {<<"case1">>, 1000, 3000, Events},
    XML = build_trace_xml(Trace),
    ?assert(is_binary(XML)),
    ?assertMatch(<<"<trace>", _/binary>>, XML),
    ?assertMatch(<<"cre:instance_id\" value=\"case1\"">>, XML).

%% Test build_event_xml/1
build_event_xml_test() ->
    Event = {1234567890, <<"TestActivity">>, <<"complete">>, #{<<"result">> => <<"success">>, <<"count">> => 42}},
    XML = build_event_xml(Event),
    ?assert(is_binary(XML)),
    ?assertMatch(<<"<event>", _/binary>>, XML),
    ?assertMatch(<<"concept:name\" value=\"TestActivity\"">>, XML),
    ?assertMatch(<<"lifecycle:transition\" value=\"complete\"">>, XML),
    ?assertMatch(<<"cre:result\"/>", _/binary>>, XML),
    ?assertMatch(<<"cre:count\" value=\"42\"/>", _/binary>>, XML).

%% Test build_attr/2
build_attr_binary_test() ->
    XML = build_attr(<<"key">>, <<"value">>),
    ?assertMatch(<<"<string key=\"cre:key\" value=\"value\"/>">>, XML).

build_attr_integer_test() ->
    XML = build_attr(<<"count">>, 42),
    ?assertMatch(<<"<long key=\"cre:count\" value=\"42\"/>">>, XML).

build_attr_float_test() ->
    XML = build_attr(<<"ratio">>, 3.14159),
    ?assertMatch(<<"<float key=\"cre:ratio\" value=\"3.14159\"">>, XML).

build_attr_boolean_test() ->
    XMLTrue = build_attr(<<"flag">>, true),
    ?assertMatch(<<"<boolean key=\"cre:flag\" value=\"true\"/>">>, XMLTrue),
    XMLFalse = build_attr(<<"flag">>, false),
    ?assertMatch(<<"<boolean key=\"cre:flag\" value=\"false\"/>">>, XMLFalse).

build_attr_atom_test() ->
    XML = build_attr(<<"atom_key">>, <<"test_atom">>),
    ?assertMatch(<<"<string key=\"cre:atom_key\" value=\"test_atom\"/>">>, XML).

build_attr_list_test() ->
    XML = build_attr(<<"items">>, [<<"a">>, <<"b">>, 1]),
    ?assertMatch(<<"<list key=\"cre:items\">", _/binary>>, XML),
    ?assertMatch(<<"<value>a</value>">>, XML),
    ?assertMatch(<<"<value>b</value>">>, XML),
    ?assertMatch(<<"<value>1</value>">>, XML).

%% Test trace_matches/2
trace_matches_concept_test() ->
    Events = [{1000, <<"ConceptA">>, <<"start">>, #{}}],
    Trace = {<<"case1">>, 1000, 2000, Events},
    ?assert(trace_matches(Trace, [{concept, <<"ConceptA">>}])).

trace_matches_lifecycle_test() ->
    Events = [{1000, <<"C">>, <<"start">>, #{}}],
    Trace = {<<"case1">>, 1000, 2000, Events},
    ?assert(trace_matches(Trace, [{lifecycle, <<"start">>}])).

trace_matches_time_range_test() ->
    Events = [{1000, <<"C">>, <<"start">>, #{}}],
    Trace = {<<"case1">>, 1000, 2000, Events},
    ?assert(trace_matches(Trace, [{time_range, {500, 1500}}])).

trace_matches_data_test() ->
    Events = [{1000, <<"C">>, <<"start">>, #{<<"key">> => <<"value">>}],
    Trace = {<<"case1">>, 1000, 2000, Events},
    ?assert(trace_matches(Trace, [{data_matches, #{<<"key">> => <<"value">>}}])).

trace_matches_multiple_filters_test() ->
    Events = [{1000, <<"C">>, <<"start">>, #{<<"k">> => <<"v">>}],
    Trace = {<<"case1">>, 1000, 2000, Events},
    ?assert(trace_matches(Trace, [
        {concept, <<"C">>},
        {lifecycle, <<"start">>},
        {data_matches, #{<<"k">> => <<"v">>}}
    ])).

trace_matches_no_match_test() ->
    Events = [{1000, <<"C">>, <<"start">>, #{<<"k">> => <<"v">>}],
    Trace = {<<"case1">>, 1000, 2000, Events},
    ?assertNot(trace_matches(Trace, [{concept, <<"NonExistent">>}])).

%% Test match_filter/2
match_filter_concept_test() ->
    Events = [{1000, <<"MyConcept">>, <<"start">>, #{}}],
    ?assert(match_filter({concept, <<"MyConcept">>}, {Events, <<"case">>})).

match_filter_lifecycle_test() ->
    Events = [{1000, <<"C">>, <<"complete">>, #{}}],
    ?assert(match_filter({lifecycle, <<"complete">>}, {Events, <<"case">>})).

match_filter_time_range_test() ->
    Events = [{1000, <<"C">>, <<"start">>, #{}}],
    ?assert(match_filter({time_range, {500, 1500}}, {Events, <<"case">>})).

match_filter_data_matches_test() ->
    Events = [{1000, <<"C">>, <<"start">>, #{<<"key">> => <<"value">>}],
    ?assert(match_filter({data_matches, #{<<"key">> => <<"value">>}}, {Events, <<"case">>})).

match_filter_no_match_test() ->
    Events = [{1000, <<"C">>, <<"start">>, #{}}],
    ?assertNot(match_filter({concept, <<"Wrong">>}, {Events, <<"case">>})).

%% Test replay_trace/2
replay_trace_test() ->
    Events = [
        {1000, <<"E1">>, <<"start">>, #{}},
        {2000, <<"E2">>, <<"complete">>, #{}}
    ],
    Trace = {Events, <<"case1">>, 1000, 3000},
    Callback = fun(E, Acc) -> {ok, [E | Acc]} end,
    ?assertEqual({ok, [<<"E1">>, <<"E2">>]}, replay_trace(Trace, Callback)).

replay_trace_invalid_test() ->
    ?assertEqual({error, invalid_trace}, replay_trace(invalid, fun(_, A) -> {ok, A} end)).

replay_trace_error_test() ->
    Events = [{1000, <<"E">>, <<"start">>, #{}}],
    Trace = {Events, <<"case1">>, 1000, 2000},
    CallbackError = fun(_, _) -> {error, test_error} end,
    ?assertEqual({error, {replay_failed, 0, test_error}}, replay_trace(Trace, CallbackError)).

%% Test replay_with_opts/3
replay_with_opts_test() ->
    Events = [{1000, <<"E">>, <<"start">>, #{}}],
    Trace = {Events, <<"case1">>, 1000, 2000},
    ?assertEqual({ok, processed}, replay_with_opts(Trace, [{timeout, 5000}, callback, fun(_, A) -> {ok, processed} end])).

replay_with_strict_test() ->
    Events = [{1000, <<"E">>, <<"start">>, #{}}],
    Trace = {Events, <<"case1">>, 1000, 2000},
    CallbackError = fun(_, _) -> {error, strict_error} end,
    ?assertEqual({error, {replay_failed, 0, strict_error}}, replay_with_opts(Trace, [{strict, true}, callback, CallbackError])).

%% Test calc_stats/1
calc_stats_empty_test() ->
    Stats = calc_stats([]),
    ?assertEqual(0, maps:get(total_traces, Stats)),
    ?assertEqual(0, maps:get(total_events, Stats)),
    ?assertEqual(0.0, maps:get(avg_events_per_trace, Stats)),
    ?assertEqual(0, maps:get(total_duration_ms, Stats)).

calc_stats_test() ->
    Traces = [
        {<<"case1">>, 1000, 3000, [{1000, <<"E">>, <<"start">>, #{}}, {2000, <<"E">>, <<"end">>, #{}}]},
        {<<"case2">>, 4000, 6000, [{4000, <<"E">>, <<"start">>, #{}}]}
    ],
    Stats = calc_stats(Traces),
    ?assertEqual(2, maps:get(total_traces, Stats)),
    ?assertEqual(3, maps:get(total_events, Stats)),
    ?assertEqual(1.5, maps:get(avg_events_per_trace, Stats)),
    ?assertEqual(3000, maps:get(total_duration_ms, Stats)).

calc_stats_partial_duration_test() ->
    Traces = [
        {<<"case1">>, 1000, undefined, [{1000, <<"E">>, <<"start">>, #{}}]},
        {<<"case2">>, 2000, 4000, [{2000, <<"E">>, <<"start">>, #{}}]}
    ],
    Stats = calc_stats(Traces),
    ?assertEqual(2000, maps:get(total_duration_ms, Stats)).

%% Test safe_div/2
safe_div_test() ->
    ?assertEqual(2.0, safe_div(10, 5)),
    ?assertEqual(0.0, safe_div(10, 0)).

%% Integration tests
new_log_close_log_integration_test() ->
    {ok, LogId} = new_log(#{<<"process">> => <<"Integration">>}),
    ok = log_case_start(LogId, <<"case_integration">>),
    ok = log_event(LogId, <<"Activity">>, <<"start">>, #{}),
    ok = log_event(LogId, <<"Activity">>, <<"complete">>, #{}),
    ok = log_case_end(LogId),
    {ok, Info} = get_log(LogId),
    ?assertEqual(1, maps:get(trace_count, Info)),
    ?assertEqual(2, maps:get(event_count, Info)),
    ok = close_log(LogId),
    ?assertEqual({error, not_found}, get_log(LogId)).

-endif.
