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
%% @doc YAWL Logging and Audit Module
%%
%% This module provides comprehensive logging and audit trail functionality
%% for YAWL workflow execution in the CRE runtime environment.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Event logging for cases, work items, and engine events</li>
%%   <li>Audit trail compatible with YAWL logging format</li>
%%   <li>Export to OpenXES format for process mining</li>
%%   <li>Export to CSV for analysis</li>
%%   <li>Configurable log levels and filtering</li>
%% </ul>
%%
%% <h3>Event Types</h3>
%%
%% <ul>
%%   <li><b>case_created</b> - New workflow case initialized</li>
%%   <li><b>case_started</b> - Case execution started</li>
%%   <li><b>case_suspended</b> - Case suspended</li>
%%   <li><b>case_resumed</b> - Case resumed from suspension</li>
%%   <li><b>case_completed</b> - Case completed successfully</li>
%%   <li><b>case_cancelled</b> - Case cancelled</li>
%%   <li><b>case_failed</b> - Case failed with error</li>
%%   <li><b>workitem_enabled</b> - Work item enabled for execution</li>
%%   <li><b>workitem_started</b> - Work item started</li>
%%   <li><b>workitem_completed</b> - Work item completed</li>
%%   <li><b>workitem_failed</b> - Work item failed</li>
%%   <li><b>workitem_cancelled</b> - Work item cancelled</li>
%%   <li><b>engine_event</b> - General engine event</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_logging).

%%====================================================================
%% Exports
%%====================================================================

%% Logger management
-export([start_logger/0,
         start_logger/1,
         stop_logger/0,
         set_log_level/1,
         get_log_level/0]).

%% Event logging
-export([log_event/2,
         log_case_event/3,
         log_workitem_event/4,
         log_engine_event/2]).

%% Log retrieval
-export([get_case_log/1,
         get_engine_log/1,
         get_events_by_type/2,
         get_events_in_range/3]).

%% Export functions
-export([export_to_openxes/1,
         export_to_csv/1,
         export_to_json/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Types
%%====================================================================

-type log_level() :: debug | info | warning | error | critical.
-type event_type() :: case_created | case_started | case_suspended | case_resumed |
                      case_completed | case_cancelled | case_failed |
                      workitem_enabled | workitem_started | workitem_completed |
                      workitem_failed | workitem_cancelled |
                      engine_event.
-type event_id() :: binary().
-type case_id() :: binary().
-type workitem_id() :: binary().
-type timestamp() :: integer().

-record(log_entry, {
          id :: event_id(),
          timestamp :: timestamp(),
          level :: log_level(),
          type :: event_type(),
          case_id :: case_id() | undefined,
          workitem_id :: workitem_id() | undefined,
          message :: binary(),
          data :: map()
         }).

-record(logger_state, {
          level :: log_level(),
          events :: [#log_entry{}],
          max_events :: non_neg_integer() | unlimited,
          filters :: [fun((#log_entry{}) -> boolean())]
         }).

-type log_entry() :: #log_entry{}.

-export_type([log_level/0, event_type/0, log_entry/0]).

%%====================================================================
%% Logger Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the YAWL logger with default configuration.
%%
%% Returns {ok, Pid} on success.
%% @end
%%--------------------------------------------------------------------
-spec start_logger() -> {ok, pid()} | {error, term()}.

start_logger() ->
    start_logger(#{
        level => info,
        max_events => 10000,
        filters => []
    }).

%%--------------------------------------------------------------------
%% @doc Starts the YAWL logger with custom configuration.
%%
%% Options:
%% - level: Minimum log level (default: info)
%% - max_events: Maximum events to keep (default: 10000)
%% - filters: List of filter functions (default: [])
%%
%% @end
%%--------------------------------------------------------------------
-spec start_logger(Options :: map()) -> {ok, pid()} | {error, term()}.

start_logger(Options) ->
    Level = maps:get(level, Options, info),
    MaxEvents = maps:get(max_events, Options, 10000),
    Filters = maps:get(filters, Options, []),

    State = #logger_state{
        level = Level,
        events = [],
        max_events = MaxEvents,
        filters = Filters
    },

    case gen_server:start_link({local, ?MODULE}, ?MODULE, State, []) of
        {ok, Pid} -> {ok, Pid};
        {error, {already_started, Pid}} -> {ok, Pid};
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Stops the YAWL logger.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_logger() -> ok.

stop_logger() ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid)
    end.

%%--------------------------------------------------------------------
%% @doc Sets the current log level.
%%
%% Events below this level will be filtered out.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_log_level(log_level()) -> ok.

set_log_level(Level) ->
    case whereis(?MODULE) of
        undefined -> ok;
        Pid -> gen_server:call(Pid, {set_level, Level})
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current log level.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_log_level() -> log_level().

get_log_level() ->
    case whereis(?MODULE) of
        undefined -> info;
        Pid -> gen_server:call(Pid, get_level)
    end.

%%====================================================================
%% Event Logging
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Logs a generic event.
%%
%% @end
%%--------------------------------------------------------------------
-spec log_event(log_level(), binary()) -> ok.

log_event(Level, Message) when is_atom(Level), is_binary(Message) ->
    log_event(Level, engine_event, undefined, undefined, Message, #{});
log_event(Level, Message) when is_list(Message) ->
    log_event(Level, list_to_binary(Message));
log_event(Level, Message) ->
    log_event(Level, engine_event, undefined, undefined, yawl_util:to_binary(Message), #{}).

%%--------------------------------------------------------------------
%% @doc Logs a case-related event.
%%
%% @end
%%--------------------------------------------------------------------
-spec log_case_event(log_level(), case_id(), event_type()) -> ok;
                     (log_level(), case_id(), binary()) -> ok.

log_case_event(Level, CaseId, Type) when is_atom(Type) ->
    log_case_event(Level, CaseId, Type, <<>>);
log_case_event(Level, CaseId, Message) when is_binary(Message) ->
    log_event(Level, engine_event, CaseId, undefined, Message, #{}).

%%--------------------------------------------------------------------
%% @doc Logs a case event with custom message.
%%
%% @end
%%--------------------------------------------------------------------
-spec log_case_event(log_level(), case_id(), event_type(), binary()) -> ok.

log_case_event(Level, CaseId, Type, Message) when is_atom(Type) ->
    log_event(Level, Type, CaseId, undefined, Message, #{}).

%%--------------------------------------------------------------------
%% @doc Logs a work item event.
%%
%% @end
%%--------------------------------------------------------------------
-spec log_workitem_event(log_level(), case_id(), workitem_id(), event_type()) -> ok;
                         (log_level(), case_id(), workitem_id(), binary()) -> ok.

log_workitem_event(Level, CaseId, WorkItemId, Type) when is_atom(Type) ->
    log_workitem_event(Level, CaseId, WorkItemId, Type, <<>>);
log_workitem_event(Level, CaseId, WorkItemId, Message) when is_binary(Message) ->
    log_event(Level, engine_event, CaseId, WorkItemId, Message, #{}).

%%--------------------------------------------------------------------
%% @doc Logs a work item event with custom message.
%%
%% @end
%%--------------------------------------------------------------------
-spec log_workitem_event(log_level(), case_id(), workitem_id(), event_type(), binary()) -> ok.

log_workitem_event(Level, CaseId, WorkItemId, Type, Message) when is_atom(Type) ->
    log_event(Level, Type, CaseId, WorkItemId, Message, #{}).

%%--------------------------------------------------------------------
%% @doc Logs an engine event.
%%
%% @end
%%--------------------------------------------------------------------
-spec log_engine_event(log_level(), binary()) -> ok.

log_engine_event(Level, Message) ->
    log_event(Level, engine_event, undefined, undefined, Message, #{}).

%%--------------------------------------------------------------------
%% @doc Internal log event function.
%%
%% @end
%%--------------------------------------------------------------------
-spec log_event(log_level(), event_type(), case_id() | undefined,
                workitem_id() | undefined, binary(), map()) -> ok.

log_event(Level, Type, CaseId, WorkItemId, Message, Data) ->
    case whereis(?MODULE) of
        undefined ->
            %% Fallback to error_logger if logger not started
            error_logger:info_report([
                {level, Level},
                {type, Type},
                {case_id, CaseId},
                {workitem_id, WorkItemId},
                {message, Message},
                {data, Data}
            ]);
        Pid ->
            Entry = #log_entry{
                id = generate_event_id(),
                timestamp = yawl_util:timestamp(),
                level = Level,
                type = Type,
                case_id = CaseId,
                workitem_id = WorkItemId,
                message = Message,
                data = Data
            },
            gen_server:cast(Pid, {log, Entry})
    end.

%%====================================================================
%% Log Retrieval
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets all log entries for a specific case.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_log(case_id()) -> [log_entry()].

get_case_log(CaseId) ->
    case whereis(?MODULE) of
        undefined -> [];
        Pid ->
            case gen_server:call(Pid, {get_case_log, CaseId}) of
                {ok, Entries} -> Entries;
                _ -> []
            end
    end.

%%--------------------------------------------------------------------
%% @doc Gets all engine-level log entries.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_engine_log(non_neg_integer() | all) -> [log_entry()].

get_engine_log(Limit) ->
    case whereis(?MODULE) of
        undefined -> [];
        Pid ->
            case gen_server:call(Pid, {get_engine_log, Limit}) of
                {ok, Entries} -> Entries;
                _ -> []
            end
    end.

%%--------------------------------------------------------------------
%% @doc Gets log entries filtered by type.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_events_by_type(event_type(), non_neg_integer() | all) -> [log_entry()].

get_events_by_type(Type, Limit) ->
    case whereis(?MODULE) of
        undefined -> [];
        Pid ->
            case gen_server:call(Pid, {get_by_type, Type, Limit}) of
                {ok, Entries} -> Entries;
                _ -> []
            end
    end.

%%--------------------------------------------------------------------
%% @doc Gets log entries within a time range.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_events_in_range(integer(), integer(), non_neg_integer() | all) ->
          [log_entry()].

get_events_in_range(StartTime, EndTime, Limit) ->
    case whereis(?MODULE) of
        undefined -> [];
        Pid ->
            case gen_server:call(Pid, {get_in_range, StartTime, EndTime, Limit}) of
                {ok, Entries} -> Entries;
                _ -> []
            end
    end.

%%====================================================================
%% Export Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Exports log entries to OpenXES format.
%%
%% OpenXES is the standard XML format for process mining event logs.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_to_openxes(file:filename_all()) -> ok | {error, term()}.

export_to_openxes(Filename) ->
    case whereis(?MODULE) of
        undefined ->
            {error, logger_not_started};
        Pid ->
            case gen_server:call(Pid, get_all_events) of
                {ok, Events} ->
                    OpenXES = format_openxes(Events),
                    file:write_file(Filename, OpenXES);
                Error ->
                    Error
            end
    end.

%%--------------------------------------------------------------------
%% @doc Exports log entries to CSV format.
%%
%% CSV columns: timestamp, level, type, case_id, workitem_id, message
%%
%% @end
%%--------------------------------------------------------------------
-spec export_to_csv(file:filename_all()) -> ok | {error, term()}.

export_to_csv(Filename) ->
    case whereis(?MODULE) of
        undefined ->
            {error, logger_not_started};
        Pid ->
            case gen_server:call(Pid, get_all_events) of
                {ok, Events} ->
                    CSV = format_csv(Events),
                    file:write_file(Filename, CSV);
                Error ->
                    Error
            end
    end.

%%--------------------------------------------------------------------
%% @doc Exports log entries to JSON format.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_to_json(file:filename_all()) -> ok | {error, term()}.

export_to_json(Filename) ->
    case whereis(?MODULE) of
        undefined ->
            {error, logger_not_started};
        Pid ->
            case gen_server:call(Pid, get_all_events) of
                {ok, Events} ->
                    JSON = format_json(Events),
                    file:write_file(Filename, JSON);
                Error ->
                    Error
            end
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(State) ->
    {ok, State}.

%% @private
handle_call({set_level, Level}, _From, State) ->
    {reply, ok, State#logger_state{level = Level}};

handle_call(get_level, _From, State) ->
    {reply, State#logger_state.level, State};

handle_call({get_case_log, CaseId}, _From, State) ->
    Filtered = lists:filter(fun(#log_entry{case_id = CID}) ->
        CID =:= CaseId
    end, State#logger_state.events),
    {reply, {ok, Filtered}, State};

handle_call({get_engine_log, Limit}, _From, State) ->
    Filtered = lists:filter(fun(#log_entry{type = Type}) ->
        Type =:= engine_event
    end, State#logger_state.events),
    Result = limit_events(Filtered, Limit),
    {reply, {ok, Result}, State};

handle_call({get_by_type, Type, Limit}, _From, State) ->
    Filtered = lists:filter(fun(#log_entry{type = T}) ->
        T =:= Type
    end, State#logger_state.events),
    Result = limit_events(Filtered, Limit),
    {reply, {ok, Result}, State};

handle_call({get_in_range, Start, End, Limit}, _From, State) ->
    Filtered = lists:filter(fun(#log_entry{timestamp = TS}) ->
        TS >= Start andalso TS =< End
    end, State#logger_state.events),
    Result = limit_events(Filtered, Limit),
    {reply, {ok, Result}, State};

handle_call(get_all_events, _From, State) ->
    {reply, {ok, State#logger_state.events}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%% @private
handle_cast({log, Entry}, State) ->
    #logger_state{
        level = MinLevel,
        events = Events,
        max_events = MaxEvents,
        filters = Filters
    } = State,

    %% Check log level
    PassesLevel = level_ge(Entry#log_entry.level, MinLevel),

    %% Check filters
    PassesFilters = lists:all(fun(F) -> F(Entry) end, Filters),

    case PassesLevel andalso PassesFilters of
        true ->
            Events1 = [Entry | Events],
            Events2 = case MaxEvents of
                unlimited -> Events1;
                N when length(Events1) > N -> lists:sublist(Events1, N);
                _ -> Events1
            end,
            {noreply, State#logger_state{events = Events2}};
        false ->
            {noreply, State}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Generates a unique event ID.
-spec generate_event_id() -> event_id().

generate_event_id() ->
    yawl_util:generate_id(<<"evt">>).

%% @private
%% @doc Compares log levels (true if L1 >= L2).
-spec level_ge(log_level(), log_level()) -> boolean().

level_ge(debug, _) -> true;
level_ge(info, info) -> true;
level_ge(info, warning) -> true;
level_ge(info, error) -> true;
level_ge(info, critical) -> true;
level_ge(warning, warning) -> true;
level_ge(warning, error) -> true;
level_ge(warning, critical) -> true;
level_ge(error, error) -> true;
level_ge(error, critical) -> true;
level_ge(critical, critical) -> true;
level_ge(_, _) -> false.

%% @private
%% @doc Limits events list to N entries.
-spec limit_events([log_entry()], non_neg_integer() | all) -> [log_entry()].

limit_events(Events, all) ->
    Events;
limit_events(Events, N) when is_integer(N) ->
    lists:sublist(Events, N).

%% @private
%% @doc Formats events as OpenXES XML.
-spec format_openxes([log_entry()]) -> binary().

format_openxes(Events) ->
    %% Group events by case
    Cases = group_events_by_case(Events),

    %% Build traces for each case
    Traces = [format_trace(CaseId, CaseEvents) ||
                 {CaseId, CaseEvents} <- maps:to_list(Cases)],

    %% Build OpenXES document
    Header = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n",
               "<log xes.version=\"1.0\" xes.features=\"nested-attributes\" xmlns=\"http://www.xes-standard.org/\">\n",
               "  <extension name=\"Lifecycle\" prefix=\"lifecycle\" uri=\"http://www.xes-standard.org/lifecycle.xesext\"/>\n",
               "  <extension name=\"Concept\" prefix=\"concept\" uri=\"http://www.xes-standard.org/concept.xesext\"/>\n",
               "  <extension name=\"Time\" prefix=\"time\" uri=\"http://www.xes-standard.org/time.xesext\"/>\n",
               "  <global scope=\"trace\">\n",
               "    <string key=\"concept:name\" value=\"\"/>\n",
               "  </global>\n",
               "  <global scope=\"event\">\n",
               "    <string key=\"concept:name\" value=\"\"/>\n",
               "    <string key=\"lifecycle:transition\" value=\"\"/>\n",
               "    <date key=\"time:timestamp\" value=\"\"/>\n",
               "  </global>\n">>,

    NewLineBin = <<"\n  ">>,
    TracesXml = iolist_to_binary([NewLineBin | Traces]),

    Footer = <<"\n</log>\n">>,

    <<Header/binary, TracesXml/binary, Footer/binary>>.

%% @private
%% @doc Groups events by case ID.
-spec group_events_by_case([log_entry()]) ->
          #{case_id() => [log_entry()]}.

group_events_by_case(Events) ->
    lists:foldl(fun(#log_entry{case_id = CaseId} = Entry, Acc) ->
        Id = case CaseId of
            undefined -> <<"no_case">>;
            _ -> CaseId
        end,
        Acc#{Id => [Entry | maps:get(Id, Acc, [])]}
    end, #{}, Events).

%% @private
%% @doc Formats a single case trace in OpenXES.
-spec format_trace(case_id(), [log_entry()]) -> iolist().

format_trace(CaseId, Events) ->
    %% Sort events by timestamp
    SortedEvents = lists:sort(fun(#log_entry{timestamp = T1}, #log_entry{timestamp = T2}) ->
        T1 =< T2
    end, Events),

    %% Format events
    EventXml = [format_event_xml(E) || E <- SortedEvents],

    [
        <<"<trace>\n">>,
        <<"    <string key=\"concept:name\" value=\"">>, CaseId, <<"\"/>\n">>,
        EventXml,
        <<"  </trace>\n">>
    ].

%% @private
%% @doc Formats a single event in OpenXES.
-spec format_event_xml(log_entry()) -> iolist().

format_event_xml(#log_entry{type = Type, timestamp = TS, message = Message}) ->
    Timestamp = yawl_util:timestamp_to_binary(TS),
    TypeStr = atom_to_binary(Type, utf8),
    Transition = event_type_to_transition(Type),
    [
        <<"    <event>\n">>,
        <<"      <string key=\"concept:name\" value=\"">>, escape_xml(Message), <<"\"/>\n">>,
        <<"      <string key=\"lifecycle:transition\" value=\"">>, Transition, <<"\"/>\n">>,
        <<"      <date key=\"time:timestamp\" value=\"">>, Timestamp, <<"\"/>\n">>,
        <<"      <string key=\"type\" value=\"">>, TypeStr, <<"\"/>\n">>,
        <<"    </event>\n">>
    ].

%% @private
%% @doc Converts event type to lifecycle transition.
-spec event_type_to_transition(event_type()) -> binary().

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
event_type_to_transition(engine_event) -> <<"unknown">>.

%% @private
%% @doc Escapes XML special characters.
-spec escape_xml(binary()) -> binary().

escape_xml(Text) ->
    yawl_util:escape_xml(Text).

%% @private
%% @doc Formats events as CSV.
-spec format_csv([log_entry()]) -> binary().

format_csv([]) ->
    <<"timestamp,level,type,case_id,workitem_id,message\n">>;
format_csv(Events) ->
    Header = <<"timestamp,level,type,case_id,workitem_id,message\n">>,
    SortedEvents = lists:sort(fun(#log_entry{timestamp = T1}, #log_entry{timestamp = T2}) ->
        T1 =< T2
    end, Events),

    Rows = [format_csv_row(E) || E <- SortedEvents],
    iolist_to_binary([Header, Rows]).

%% @private
%% @doc Formats a single log entry as CSV.
-spec format_csv_row(log_entry()) -> binary().

format_csv_row(#log_entry{timestamp = TS, level = Level, type = Type,
                           case_id = CaseId, workitem_id = WorkItemId, message = Msg}) ->
    TSBin = yawl_util:timestamp_to_binary(TS),
    LevelBin = atom_to_binary(Level, utf8),
    TypeBin = atom_to_binary(Type, utf8),
    CaseIdBin = case CaseId of undefined -> <<>>; C -> C end,
    WorkItemIdBin = case WorkItemId of undefined -> <<>>; W -> W end,
    MsgEscaped = escape_csv(Msg),
    <<TSBin/binary, ",",
      LevelBin/binary, ",",
      TypeBin/binary, ",",
      CaseIdBin/binary, ",",
      WorkItemIdBin/binary, ",",
      MsgEscaped/binary, "\n">>.

%% @private
%% @doc Escapes a value for CSV.
-spec escape_csv(binary()) -> binary().

escape_csv(Text) ->
    NeedsEscape = case binary:match(Text, <<",">>) of
        nomatch -> false;
        _ -> true
    end orelse case binary:match(Text, <<"\"">>) of
        nomatch -> false;
        _ -> true
    end orelse case binary:match(Text, <<"\n">>) of
        nomatch -> false;
        _ -> true
    end,

    case NeedsEscape of
        true ->
            Escaped = binary:replace(Text, <<"\"">>, <<"\"\"">>),
            <<"\"", Escaped/binary, "\"">>;
        false ->
            Text
    end.

%% @private
%% @doc Formats events as JSON.
-spec format_json([log_entry()]) -> binary().

format_json(Events) ->
    SortedEvents = lists:sort(fun(#log_entry{timestamp = T1}, #log_entry{timestamp = T2}) ->
        T1 =< T2
    end, Events),

    JsonEvents = [format_json_event(E) || E <- SortedEvents],
    EventList = iolist_to_binary([<<"\n    ">> | lists:join(<<",\n    ">>, JsonEvents)]),

    CountBin = integer_to_binary(length(Events)),

    <<
        "{\n"
        "  \"events\": [", EventList/binary, "\n"
        "  ],\n"
        "  \"count\": ", CountBin/binary, "\n"
        "}\n"
    >>.

%% @private
%% @doc Formats a single event as JSON.
-spec format_json_event(log_entry()) -> iolist().

format_json_event(#log_entry{timestamp = TS, level = Level, type = Type,
                              case_id = CaseId, workitem_id = WorkItemId,
                              message = Message, data = Data}) ->
    TSBin = yawl_util:timestamp_to_binary(TS),
    LevelBin = atom_to_binary(Level, utf8),
    TypeBin = atom_to_binary(Type, utf8),

    CaseIdJson = case CaseId of
        undefined -> <<"null">>;
        _ -> <<"\"", CaseId/binary, "\"">>
    end,

    WorkItemIdJson = case WorkItemId of
        undefined -> <<"null">>;
        _ -> <<"\"", WorkItemId/binary, "\"">>
    end,

    MessageEscaped = jsone:encode(Message),
    DataJson = case maps:size(Data) of
        0 -> <<"null">>;
        _ -> jsone:encode(Data)
    end,

    [
        <<"{\n">>,
        <<"    \"timestamp\": \"">>, TSBin, <<"\",\n">>,
        <<"    \"level\": \"">>, LevelBin, <<"\",\n">>,
        <<"    \"type\": \"">>, TypeBin, <<"\",\n">>,
        <<"    \"case_id\": ">>, CaseIdJson, <<",\n">>,
        <<"    \"workitem_id\": ">>, WorkItemIdJson, <<",\n">>,
        <<"    \"message\": ">>, MessageEscaped, <<",\n">>,
        <<"    \"data\": ">>, DataJson, <<"\n">>,
        <<"  }">>
    ].
