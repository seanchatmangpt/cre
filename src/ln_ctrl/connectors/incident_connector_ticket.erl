%%% @doc Ticket Connector (ServiceNow mock)
-module(incident_connector_ticket).
-behaviour(gen_server).
-export([start_link/1, create_ticket/2, update_status/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name = ticket, tickets = [], receipt_log = []}).

start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [Name], []).
create_ticket(ConnName, TicketData) -> gen_server:call(ConnName, {create, TicketData}).
update_status(ConnName, TicketId, Status) -> gen_server:call(ConnName, {update, TicketId, Status}).
stop(ConnName) -> gen_server:call(ConnName, stop).

init([Name]) -> {ok, #state{name = Name}}.

handle_call({create, Data}, _From, State) ->
    TicketId = erlang:unique_integer([positive]),
    Receipt = {effect_receipt, {ticket_id, TicketId}, {action, create}, {data_hash, erlang:phash2(Data)}, {timestamp, calendar:universal_time()}},
    {reply, {ok, TicketId}, State#state{tickets = [{TicketId, Data} | State#state.tickets], receipt_log = [Receipt | State#state.receipt_log]}};

handle_call({update, TicketId, Status}, _From, State) ->
    Receipt = {effect_receipt, {ticket_id, TicketId}, {action, update}, {status, Status}, {timestamp, calendar:universal_time()}},
    {reply, {ok, updated}, State#state{receipt_log = [Receipt | State#state.receipt_log]}};

handle_call(stop, _From, State) -> {stop, normal, ok, State}.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
