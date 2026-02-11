%%% @doc SIEM Connector (Splunk mock for MVP)
%%% Handles: alert ingestion, enrichment, query
-module(incident_connector_siem).
-behaviour(gen_server).

-export([start_link/1, ingest/2, query/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    name = siem,
    events = [],
    receipt_log = []
}).

%% Public API

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

ingest(ConnName, Alert) ->
    gen_server:call(ConnName, {ingest, Alert}).

query(ConnName, Query) ->
    gen_server:call(ConnName, {query, Query}).

stop(ConnName) ->
    gen_server:call(ConnName, stop).

%% gen_server callbacks

init([Name]) ->
    {ok, #state{name = Name}}.

handle_call({ingest, Alert}, _From, State) ->
    %% Process alert: add to events, emit receipt
    ReceiptId = erlang:unique_integer([positive]),
    Receipt = {
        effect_receipt,
        {effect_id, ReceiptId},
        {type, siem_ingest},
        {alert_hash, erlang:phash2(Alert)},
        {timestamp, calendar:universal_time()}
    },

    NewEvents = [Alert | State#state.events],
    NewReceipts = [Receipt | State#state.receipt_log],
    NewState = State#state{events = NewEvents, receipt_log = NewReceipts},

    {reply, {ok, ReceiptId}, NewState};

handle_call({query, Query}, _From, State) ->
    %% Simple query: filter events by severity
    Results = case Query of
        {severity, high} ->
            [E || E <- State#state.events, severity_of(E) == high];
        {severity, critical} ->
            [E || E <- State#state.events, severity_of(E) == critical];
        _ ->
            State#state.events
    end,

    Receipt = {
        effect_receipt,
        {effect_id, erlang:unique_integer([positive])},
        {type, siem_query},
        {result_count, length(Results)},
        {timestamp, calendar:universal_time()}
    },

    NewReceipts = [Receipt | State#state.receipt_log],
    NewState = State#state{receipt_log = NewReceipts},

    {reply, {ok, Results}, NewState};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_, State) ->
    {noreply, State}.

terminate(_, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Helper functions

severity_of(Alert) when is_map(Alert) ->
    maps:get(severity, Alert, low);
severity_of(_) ->
    low.

