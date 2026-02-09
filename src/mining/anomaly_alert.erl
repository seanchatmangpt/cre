%% -*- erlang -*-
%% @doc Anomaly Alert System
%%
%% gen_server that manages subscriptions and notifications for anomaly alerts.
%%
%% @end

-module(anomaly_alert).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([subscribe/1, unsubscribe/1, notify/1]).
-export([list_subscriptions/0, get_subscriber_count/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Records
-record(state, {
    subscriptions :: map(),
    monitors :: map()
}).

-record(subscription, {
    id :: reference(),
    subscriber :: pid(),
    filter :: map(),
    notification_mode :: sync | async
}).

-record(anomaly_alert, {
    id :: reference(),
    severity :: critical | warning | info,
    anomaly_type :: atom(),
    case_id :: binary() | undefined,
    description :: binary(),
    confidence :: float(),
    timestamp :: integer()
}).

-define(SUBSCRIPTIONS_TABLE, anomaly_alert_subscriptions).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop).

-spec subscribe(map()) -> reference().
subscribe(Filter) when is_map(Filter) ->
    gen_server:call(?MODULE, {subscribe, Filter}).

-spec unsubscribe(reference()) -> ok | {error, not_found}.
unsubscribe(SubscriptionId) when is_reference(SubscriptionId) ->
    gen_server:call(?MODULE, {unsubscribe, SubscriptionId}).

-spec notify(#anomaly_alert{}) -> ok.
notify(Alert) when is_record(Alert, anomaly_alert) ->
    gen_server:cast(?MODULE, {notify, Alert}).

-spec list_subscriptions() -> [#subscription{}].
list_subscriptions() ->
    gen_server:call(?MODULE, list_subscriptions).

-spec get_subscriber_count() -> integer().
get_subscriber_count() ->
    gen_server:call(?MODULE, get_subscriber_count).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?SUBSCRIPTIONS_TABLE, [named_table, set, public]),
    {ok, #state{subscriptions = #{}, monitors = #{}}}.

handle_call({subscribe, Filter}, {Pid, _Ref}, State) ->
    SubscriptionId = make_ref(),
    Subscription = #subscription{
        id = SubscriptionId,
        subscriber = Pid,
        filter = Filter,
        notification_mode = maps:get(notification_mode, Filter, sync)
    },
    ets:insert(?SUBSCRIPTIONS_TABLE, Subscription),
    MonitorRef = erlang:monitor(process, Pid),
    NewState = State#state{
        subscriptions = maps:put(SubscriptionId, Subscription, State#state.subscriptions),
        monitors = maps:put(Pid, {SubscriptionId, MonitorRef}, State#state.monitors)
    },
    {reply, SubscriptionId, NewState};

handle_call({unsubscribe, SubscriptionId}, _From, State) ->
    case maps:get(SubscriptionId, State#state.subscriptions, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        _Subscription ->
            ets:delete(?SUBSCRIPTIONS_TABLE, SubscriptionId),
            NewSubscriptions = maps:remove(SubscriptionId, State#state.subscriptions),
            {reply, ok, State#state{subscriptions = NewSubscriptions}}
    end;

handle_call(list_subscriptions, _From, State) ->
    Subs = ets:tab2list(?SUBSCRIPTIONS_TABLE),
    {reply, Subs, State};

handle_call(get_subscriber_count, _From, State) ->
    Count = ets:info(?SUBSCRIPTIONS_TABLE, size),
    {reply, Count, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({notify, Alert}, State) ->
    Subs = ets:tab2list(?SUBSCRIPTIONS_TABLE),
    MatchingSubs = filter_matching_subscriptions(Alert, Subs),
    lists:foreach(fun(#subscription{subscriber = Pid, notification_mode = Mode, id = Id}) ->
        case Mode of
            sync -> Pid ! {anomaly_alert, Alert, Id};
            async -> spawn(fun() -> Pid ! {anomaly_alert, Alert, Id} end)
        end
    end, MatchingSubs),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    case maps:get(Pid, State#state.monitors, undefined) of
        undefined ->
            {noreply, State};
        {SubscriptionId, _MonitorRef} ->
            ets:delete(?SUBSCRIPTIONS_TABLE, SubscriptionId),
            NewSubscriptions = maps:remove(SubscriptionId, State#state.subscriptions),
            NewMonitors = maps:remove(Pid, State#state.monitors),
            {noreply, State#state{subscriptions = NewSubscriptions, monitors = NewMonitors}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ets:delete(?SUBSCRIPTIONS_TABLE),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
filter_matching_subscriptions(Alert, Subs) ->
    lists:filter(fun(#subscription{filter = Filter}) ->
        matches_filter(Alert, Filter)
    end, Subs).

%% @private
matches_filter(Alert, Filter) ->
    CheckSeverity = case maps:get(severity, Filter, undefined) of
        undefined -> true;
        Severity -> Alert#anomaly_alert.severity =:= Severity
    end,
    CheckType = case maps:get(anomaly_type, Filter, undefined) of
        undefined -> true;
        Type -> Alert#anomaly_alert.anomaly_type =:= Type
    end,
    CheckCaseId = case maps:get(case_id, Filter, undefined) of
        undefined -> true;
        CaseId -> Alert#anomaly_alert.case_id =:= CaseId
    end,
    CheckConfidence = case maps:get(min_confidence, Filter, undefined) of
        undefined -> true;
        MinConf -> Alert#anomaly_alert.confidence >= MinConf
    end,
    CheckSeverity and CheckType and CheckCaseId and CheckConfidence.
