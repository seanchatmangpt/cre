%%% @doc EDR Connector (CrowdStrike mock)
-module(incident_connector_edr).
-behaviour(gen_server).
-export([start_link/1, quarantine/2, get_evidence/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name = edr, devices = [], receipt_log = []}).

start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [Name], []).
quarantine(ConnName, DeviceId) -> gen_server:call(ConnName, {quarantine, DeviceId}).
get_evidence(ConnName, IncidentId) -> gen_server:call(ConnName, {evidence, IncidentId}).
stop(ConnName) -> gen_server:call(ConnName, stop).

init([Name]) -> {ok, #state{name = Name}}.

handle_call({quarantine, DeviceId}, _From, State) ->
    Receipt = {effect_receipt, {device_id, DeviceId}, {action, quarantine}, {timestamp, calendar:universal_time()}},
    {reply, {ok, quarantine}, State#state{receipt_log = [Receipt | State#state.receipt_log]}};

handle_call({evidence, _IncidentId}, _From, State) ->
    Evidence = [{file_hashes, ["hash1", "hash2"]}, {process_tree, ["proc1", "proc2"]}, {network, ["192.168.1.1"]}],
    Receipt = {effect_receipt, {type, evidence_retrieval}, {count, length(Evidence)}, {timestamp, calendar:universal_time()}},
    {reply, {ok, Evidence}, State#state{receipt_log = [Receipt | State#state.receipt_log]}};

handle_call(stop, _From, State) -> {stop, normal, ok, State}.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
