%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% XES Logging Module for YAWL Workflow Patterns
%% Based on IEEE 1849-2016 XES Standard for Event Logs
%%
%% -------------------------------------------------------------------

-module(yawl_xes).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% XES Log Management
-export([start_link/0, start_link/1, stop/0]).
-export([new_log/0, new_log/1]).
-export([log_event/4, log_event/5]).
-export([export_xes/1, export_xes/2]).
-export([get_log/1, list_logs/0]).

%% Event Recording for Patterns
-export([log_pattern_start/3, log_pattern_complete/4]).
-export([log_token_move/4, log_transition_fire/4]).
-export([log_case_start/2, log_case_complete/3]).
-export([log_workitem_start/3, log_workitem_complete/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Types
%%====================================================================

-type log_id() :: binary().
-type trace_id() :: binary().
-type case_id() :: binary().
-type event_id() :: binary().
-type timestamp() :: integer().

-record(xes_log, {
    log_id :: log_id(),
    trace_id :: trace_id(),
    started_at :: timestamp(),
    events :: list(),
    metadata :: map()
}).

-record(xes_event, {
    event_id :: event_id(),
    timestamp :: timestamp(),
    case_id :: case_id() | undefined,
    concept :: map(),
    lifecycle :: map(),
    data :: map()
}).

-record(state, {
    logs :: #{log_id() => #xes_log{}},
    traces :: #{trace_id() => log_id()},
    next_event_id :: non_neg_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the XES logger with default configuration.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Starts the XES logger with a given name.
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Stops the XES logger.
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc Creates a new XES log.
%% @end
%%--------------------------------------------------------------------
-spec new_log() -> {ok, log_id()}.
new_log() ->
    new_log(#{}) .

-spec new_log(map()) -> {ok, log_id()}.
new_log(Metadata) ->
    gen_server:call(?MODULE, {new_log, Metadata}).

%%--------------------------------------------------------------------
%% @doc Logs a pattern execution event.
%% @end
%%--------------------------------------------------------------------
-spec log_pattern_start(log_id(), binary(), binary()) -> ok.
log_pattern_start(LogId, PatternType, PatternId) ->
    Timestamp = erlang:system_time(millisecond),
    EventId = generate_event_id(),
    Event = #xes_event{
        event_id = EventId,
        timestamp = Timestamp,
        concept = #{
            <<"concept:name">> => PatternType,
            <<"concept:instance">> => PatternId
        },
        lifecycle = #{<<"lifecycle:transition">> => <<"start">>},
        data = #{}
    },
    gen_server:cast(?MODULE, {add_event, LogId, Event}).

-spec log_pattern_complete(log_id(), binary(), binary(), term()) -> ok.
log_pattern_complete(LogId, PatternType, PatternId, Result) ->
    Timestamp = erlang:system_time(millisecond),
    EventId = generate_event_id(),
    Event = #xes_event{
        event_id = EventId,
        timestamp = Timestamp,
        concept = #{
            <<"concept:name">> => PatternType,
            <<"concept:instance">> => PatternId
        },
        lifecycle = #{<<"lifecycle:transition">> => <<"complete">>},
        data = #{<<"result">> => Result}
    },
    gen_server:cast(?MODULE, {add_event, LogId, Event}).

%%--------------------------------------------------------------------
%% @doc Logs a token move in Petri net.
%% @end
%%--------------------------------------------------------------------
-spec log_token_move(log_id(), binary(), binary(), binary()) -> ok.
log_token_move(LogId, Place, From, To) ->
    Timestamp = erlang:system_time(millisecond),
    EventId = generate_event_id(),
    Event = #xes_event{
        event_id = EventId,
        timestamp = Timestamp,
        concept = #{
            <<"concept:name">> => <<"TokenMove">>
        },
        lifecycle = #{<<"lifecycle:transition">> => <<"move">>},
        data = #{
            <<"place">> => Place,
            <<"from">> => From,
            <<"to">> => To
        }
    },
    gen_server:cast(?MODULE, {add_event, LogId, Event}).

%%--------------------------------------------------------------------
%% @doc Logs a transition firing.
%% @end
%%--------------------------------------------------------------------
-spec log_transition_fire(log_id(), binary(), binary(), list()) -> ok.
log_transition_fire(LogId, Transition, Inputs, Outputs) ->
    Timestamp = erlang:system_time(millisecond),
    EventId = generate_event_id(),
    Event = #xes_event{
        event_id = EventId,
        timestamp = Timestamp,
        concept = #{
            <<"concept:name">> => Transition
        },
        lifecycle = #{<<"lifecycle:transition">> => <<"fire">>},
        data = #{
            <<"inputs">> => Inputs,
            <<"outputs">> => Outputs
        }
    },
    gen_server:cast(?MODULE, {add_event, LogId, Event}).

%%--------------------------------------------------------------------
%% @doc Logs case start.
%% @end
%%--------------------------------------------------------------------
-spec log_case_start(log_id(), case_id()) -> ok.
log_case_start(LogId, CaseId) ->
    Timestamp = erlang:system_time(millisecond),
    EventId = generate_event_id(),
    Event = #xes_event{
        event_id = EventId,
        timestamp = Timestamp,
        case_id = CaseId,
        concept = #{
            <<"concept:name">> => <<"CaseStart">>
        },
        lifecycle = #{<<"lifecycle:transition">> => <<"start">>},
        data = #{}
    },
    gen_server:cast(?MODULE, {add_event, LogId, Event}).

%%--------------------------------------------------------------------
%% @doc Logs case completion.
%% @end
%%--------------------------------------------------------------------
-spec log_case_complete(log_id(), case_id(), map()) -> ok.
log_case_complete(LogId, CaseId, Stats) ->
    Timestamp = erlang:system_time(millisecond),
    EventId = generate_event_id(),
    Event = #xes_event{
        event_id = EventId,
        timestamp = Timestamp,
        case_id = CaseId,
        concept = #{
            <<"concept:name">> => <<"CaseComplete">>
        },
        lifecycle = #{<<"lifecycle:transition">> => <<"complete">>},
        data = Stats
    },
    gen_server:cast(?MODULE, {add_event, LogId, Event}).

%%--------------------------------------------------------------------
%% @doc Logs workitem start.
%% @end
%%--------------------------------------------------------------------
-spec log_workitem_start(log_id(), binary(), binary()) -> ok.
log_workitem_start(LogId, WorkitemId, TaskId) ->
    Timestamp = erlang:system_time(millisecond),
    EventId = generate_event_id(),
    Event = #xes_event{
        event_id = EventId,
        timestamp = Timestamp,
        concept = #{
            <<"concept:name">> => <<"Workitem">>,
            <<"concept:instance">> => WorkitemId
        },
        lifecycle = #{<<"lifecycle:transition">> => <<"start">>},
        data = #{<<"task">> => TaskId}
    },
    gen_server:cast(?MODULE, {add_event, LogId, Event}).

%%--------------------------------------------------------------------
%% @doc Logs workitem completion.
%% @end
%%--------------------------------------------------------------------
-spec log_workitem_complete(log_id(), binary(), binary(), term()) -> ok.
log_workitem_complete(LogId, WorkitemId, TaskId, Result) ->
    Timestamp = erlang:system_time(millisecond),
    EventId = generate_event_id(),
    Event = #xes_event{
        event_id = EventId,
        timestamp = Timestamp,
        concept = #{
            <<"concept:name">> => <<"Workitem">>,
            <<"concept:instance">> => WorkitemId
        },
        lifecycle = #{<<"lifecycle:transition">> => <<"complete">>},
        data = #{
            <<"task">> => TaskId,
            <<"result">> => Result
        }
    },
    gen_server:cast(?MODULE, {add_event, LogId, Event}).

%%--------------------------------------------------------------------
%% @doc Generic event logging.
%% @end
%%--------------------------------------------------------------------
-spec log_event(log_id(), binary(), binary(), map()) -> ok.
log_event(LogId, ConceptName, LifecycleTransition, Data) ->
    log_event(LogId, ConceptName, LifecycleTransition, Data, undefined).

-spec log_event(log_id(), binary(), binary(), map(), binary() | undefined) -> ok.
log_event(LogId, ConceptName, LifecycleTransition, Data, CaseId) ->
    Timestamp = erlang:system_time(millisecond),
    EventId = generate_event_id(),
    Event = #xes_event{
        event_id = EventId,
        timestamp = Timestamp,
        case_id = CaseId,
        concept = #{
            <<"concept:name">> => ConceptName
        },
        lifecycle = #{
            <<"lifecycle:transition">> => LifecycleTransition
        },
        data = Data
    },
    gen_server:cast(?MODULE, {add_event, LogId, Event}).

%%--------------------------------------------------------------------
%% @doc Exports log to XES XML format.
%% @end
%%--------------------------------------------------------------------
-spec export_xes(log_id()) -> {ok, iodata()}.
export_xes(LogId) ->
    export_xes(LogId, "xes_logs").

-spec export_xes(log_id(), string()) -> {ok, iodata()}.
export_xes(LogId, OutputDir) ->
    gen_server:call(?MODULE, {export_xes, LogId, OutputDir}).

%%--------------------------------------------------------------------
%% @doc Gets a log by ID.
%% @end
%%--------------------------------------------------------------------
-spec get_log(log_id()) -> {ok, #xes_log{}} | {error, not_found}.
get_log(LogId) ->
    gen_server:call(?MODULE, {get_log, LogId}).

%%--------------------------------------------------------------------
%% @doc Lists all logs.
%% @end
%%--------------------------------------------------------------------
-spec list_logs() -> [{log_id(), #xes_log{}}].
list_logs() ->
    gen_server:call(?MODULE, list_logs).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

init(_Args) ->
    {ok, #state{
        logs = #{},
        traces = #{},
        next_event_id = 1
    }}.

handle_call({new_log, Metadata}, _From, State) ->
    LogId = generate_log_id(),
    TraceId = generate_trace_id(),
    Log = #xes_log{
        log_id = LogId,
        trace_id = TraceId,
        started_at = erlang:system_time(millisecond),
        events = [],
        metadata = Metadata
    },
    State1 = State#state{
        logs = maps:put(LogId, Log, State#state.logs),
        traces = maps:put(TraceId, LogId, State#state.traces)
    },
    {reply, {ok, LogId}, State1};

handle_call({get_log, LogId}, _From, State) ->
    case maps:get(LogId, State#state.logs) of
        undefined -> {reply, {error, not_found}, State};
        Log -> {reply, {ok, Log}, State}
    end;

handle_call(list_logs, _From, State) ->
    LogsList = maps:to_list(State#state.logs),
    {reply, LogsList, State};

handle_call({export_xes, LogId, OutputDir}, _From, State) ->
    case maps:get(LogId, State#state.logs) of
        undefined ->
            {reply, {error, not_found}, State};
        Log ->
            XESContent = format_xes(Log),
            FileName = filename:join([OutputDir, binary_to_list(LogId) ++ ".xes"]),
            ok = filelib:ensure_dir(FileName),
            ok = file:write_file(FileName, XESContent),
            {reply, {ok, XESContent}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

handle_cast({add_event, LogId, Event}, State) ->
    case maps:get(LogId, State#state.logs) of
        undefined ->
            {noreply, State};
        Log ->
            Log1 = Log#xes_log{events = Log#xes_log.events ++ [Event]},
            State1 = State#state{logs = maps:put(LogId, Log1, State#state.logs)},
            {noreply, State1}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Formats XES log as XML.
%% @end
%%--------------------------------------------------------------------

format_xes(#xes_log{log_id = LogId, trace_id = TraceId, started_at = Started, events = Events, metadata = _Metadata}) ->
    StartTimeStr = format_timestamp(Started),
    EventsXML = lists:map(fun format_event/1, Events),
    EventsBin = iolist_to_binary(EventsXML),

    iolist_to_binary([
        <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n">>,
        <<"<log xes.version=\"1.0\" xes.features=\"nested-attributes\" xes.xmlns=\"http://www.xes-standard.org/\">\n">>,
        <<"  <trace xes:id=\"">>, TraceId, <<"\" xes:type=\"\">\n">>,
        <<"    <event xes:id=\"0\">\n">>,
        <<"      <string key=\"org:xes-standard:concept:name\" value=\"CRE YAWL Workflow\"/>\n">>,
        <<"      <date key=\"org:xes-standard:time:timestamp\" value=\"">>, StartTimeStr, <<"\"/>\n">>,
        <<"      <string key=\"log:id\" value=\"">>, LogId, <<"\"/>\n">>,
        <<"    </event>\n    ">>,
        EventsBin,
        <<"\n  </trace>\n</log>">>
    ]).

format_event(#xes_event{event_id = EventId, timestamp = Timestamp, case_id = CaseId, concept = Concept, lifecycle = Lifecycle, data = Data}) ->
    TimeStr = format_timestamp(Timestamp),
    ConceptXML = format_map(<<"concept">>, Concept),
    LifecycleXML = format_map(<<"lifecycle">>, Lifecycle),
    DataXML = format_map(<<"data">>, Data),

    CaseAttr = case CaseId of
        undefined -> <<"">>;
        _ -> <<" string key=\"case:id\" value=\"", CaseId/binary, "\"/>">>
    end,

    iolist_to_binary([
        <<"\n    <event id=\"">>, EventId, <<"\"">>, CaseAttr, <<">\n">>,
        <<"      <date key=\"time:timestamp\" value=\"">>, TimeStr, <<"\"/>\n">>,
        <<"      ", ConceptXML/binary, "\n">>,
        <<"      ", LifecycleXML/binary, "\n">>,
        <<"      ", DataXML/binary, "\n">>,
        <<"    </event>">>
    ]).

format_map(Prefix, Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        ValueStr = format_value(Value),
        <<"<string key=\"", Prefix/binary, ":", Key/binary, "\" value=\"", ValueStr/binary, "\"/>", Acc/binary>>
    end, <<"">>, Map).

format_value(Binary) when is_binary(Binary) -> Binary;
format_value(Integer) when is_integer(Integer) -> list_to_binary(integer_to_list(Integer));
format_value(Float) when is_float(Float) -> float_to_binary(Float, [{decimals, 6}, compact]);
format_value(Atom) when is_atom(Atom) -> atom_to_binary(Atom);
format_value(List) when is_list(List) ->
    Formatted = [format_value(V) || V <- List],
    Joined = join_binaries(Formatted, <<",">>),
    <<"[", Joined/binary, "]">>;
format_value(Map) when is_map(Map) ->
    <<"{", (format_map_inline(Map))/binary, "}">>;
format_value(Tuple) when is_tuple(Tuple) ->
    Formatted = [format_value(V) || V <- tuple_to_list(Tuple)],
    Joined = join_binaries(Formatted, <<",">>),
    <<"{", Joined/binary, "}">>.

%% Join a list of binaries with a separator
join_binaries([], _Sep) ->
    <<>>;
join_binaries([Bin], _Sep) ->
    Bin;
join_binaries([Bin | Rest], Sep) ->
    lists:foldl(fun(B, Acc) ->
        <<Acc/binary, Sep/binary, B/binary>>
    end, Bin, Rest).

format_map_inline(Map) ->
    maps:fold(fun(Key, Value, Acc) ->
        ValueStr = format_value(Value),
        <<Key/binary, "=>", ValueStr/binary, ", ", Acc/binary>>
    end, <<"">>, Map).

format_timestamp(Millis) ->
    %% Convert milliseconds to seconds for calendar functions
    Seconds = Millis div 1000,
    DateTime = calendar:gregorian_seconds_to_datetime(Seconds + calendar:datetime_to_gregorian_seconds({{1970,1,1},{0,0,0}})),
    {{Year, Month, Day}, {Hour, Minute, Second}} = DateTime,
    Format = "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
    list_to_binary(lists:flatten(io_lib:format(Format, [Year, Month, Day, Hour, Minute, Second, Millis rem 1000]))).

generate_log_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"log_", (integer_to_binary(Timestamp))/binary>>.

generate_trace_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"trace_", (integer_to_binary(Timestamp))/binary>>.

generate_event_id() ->
    Timestamp = erlang:unique_integer([positive, monotonic]),
    <<"event_", (integer_to_binary(Timestamp))/binary>>.
