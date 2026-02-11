%%% @doc Notification Connector (Slack mock)
-module(incident_connector_notify).
-behaviour(gen_server).
-export([start_link/1, notify/3, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {name = notify, messages = [], receipt_log = []}).

start_link(Name) -> gen_server:start_link({local, Name}, ?MODULE, [Name], []).
notify(ConnName, Recipient, Message) -> gen_server:call(ConnName, {notify, Recipient, Message}).
stop(ConnName) -> gen_server:call(ConnName, stop).

init([Name]) -> {ok, #state{name = Name}}.

handle_call({notify, Recipient, Message}, _From, State) ->
    Receipt = {effect_receipt, {recipient, Recipient}, {action, notify}, {message_hash, erlang:phash2(Message)}, {timestamp, calendar:universal_time()}},
    {reply, {ok, sent}, State#state{messages = [{Recipient, Message} | State#state.messages], receipt_log = [Receipt | State#state.receipt_log]}};

handle_call(stop, _From, State) -> {stop, normal, ok, State}.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, State) -> {noreply, State}.
terminate(_, _) -> ok.
code_change(_, State, _) -> {ok, State}.
