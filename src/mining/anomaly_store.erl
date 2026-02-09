%% -*- erlang -*-
%% @doc Anomaly Store for Process Mining
%%
%% gen_server that manages anomaly data storage and frequency tracking.
%%
%% @end

-module(anomaly_store).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([update_frequency/2, get_trace_frequency/1]).
-export([store_anomaly/1, get_anomalies/1, get_all_anomalies/0]).
-export([create_alert/1, get_alerts/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Records
-record(state, {
    frequency_table :: ets:tid(),
    records_table :: ets:tid(),
    alerts_table :: ets:tid()
}).

-record(anomaly_record, {
    id :: reference(),
    case_id :: binary(),
    trace :: list(),
    anomaly_type :: atom(),
    severity :: critical | warning | info,
    confidence :: float(),
    description :: binary(),
    timestamp :: integer(),
    metadata :: map()
}).

-define(FREQUENCY_TABLE, anomaly_frequency).
-define(RECORDS_TABLE, anomaly_records).
-define(ALERTS_TABLE, anomaly_alerts).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

-spec update_frequency(binary(), integer()) -> ok.
update_frequency(CaseId, Delta) when is_binary(CaseId), is_integer(Delta) ->
    gen_server:call(?MODULE, {update_frequency, CaseId, Delta}).

-spec get_trace_frequency(binary()) -> integer() | undefined.
get_trace_frequency(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {get_trace_frequency, CaseId}).

-spec store_anomaly(#anomaly_record{}) -> reference().
store_anomaly(Record) when is_record(Record, anomaly_record) ->
    gen_server:call(?MODULE, {store_anomaly, Record}).

-spec get_anomalies(binary()) -> [#anomaly_record{}].
get_anomalies(CaseId) when is_binary(CaseId) ->
    gen_server:call(?MODULE, {get_anomalies, CaseId}).

-spec get_all_anomalies() -> [#anomaly_record{}].
get_all_anomalies() ->
    gen_server:call(?MODULE, get_all_anomalies).

-spec create_alert(#anomaly_record{}) -> reference().
create_alert(Record) when is_record(Record, anomaly_record) ->
    gen_server:call(?MODULE, {create_alert, Record}).

-spec get_alerts() -> [#anomaly_record{}].
get_alerts() ->
    gen_server:call(?MODULE, get_alerts).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    FreqTable = ets:new(?FREQUENCY_TABLE, [named_table, set, public]),
    RecordsTable = ets:new(?RECORDS_TABLE, [named_table, bag, public]),
    AlertsTable = ets:new(?ALERTS_TABLE, [named_table, set, public]),
    {ok, #state{
        frequency_table = FreqTable,
        records_table = RecordsTable,
        alerts_table = AlertsTable
    }}.

handle_call({update_frequency, CaseId, Delta}, _From, State) ->
    case ets:lookup(?FREQUENCY_TABLE, CaseId) of
        [{CaseId, Current}] ->
            ets:insert(?FREQUENCY_TABLE, {CaseId, Current + Delta});
        [] ->
            ets:insert(?FREQUENCY_TABLE, {CaseId, Delta})
    end,
    {reply, ok, State};

handle_call({get_trace_frequency, CaseId}, _From, State) ->
    case ets:lookup(?FREQUENCY_TABLE, CaseId) of
        [{CaseId, Freq}] -> {reply, Freq, State};
        [] -> {reply, undefined, State}
    end;

handle_call({store_anomaly, Record}, _From, State) ->
    Id = Record#anomaly_record.id,
    ets:insert(?RECORDS_TABLE, Record),
    {reply, Id, State};

handle_call({get_anomalies, CaseId}, _From, State) ->
    Anomalies = ets:match_object(?RECORDS_TABLE, #anomaly_record{case_id = CaseId, _ = '_'}),
    {reply, Anomalies, State};

handle_call(get_all_anomalies, _From, State) ->
    All = ets:tab2list(?RECORDS_TABLE),
    {reply, All, State};

handle_call({create_alert, Record}, _From, State) ->
    Id = make_ref(),
    Alert = #anomaly_record{
        id = Id,
        case_id = Record#anomaly_record.case_id,
        anomaly_type = Record#anomaly_record.anomaly_type,
        severity = Record#anomaly_record.severity,
        confidence = Record#anomaly_record.confidence,
        description = Record#anomaly_record.description,
        timestamp = erlang:system_time(millisecond),
        metadata = Record#anomaly_record.metadata
    },
    ets:insert(?ALERTS_TABLE, Alert),
    {reply, Id, State};

handle_call(get_alerts, _From, State) ->
    Alerts = ets:tab2list(?ALERTS_TABLE),
    {reply, Alerts, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
