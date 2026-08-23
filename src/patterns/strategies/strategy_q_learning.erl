%% -*- erlang -*-
%% @doc Q-Learning Strategy for Branch Selection
%%
%% Tabular Q-learning with epsilon-greedy exploration.
%%
%% @end

-module(strategy_q_learning).
-behaviour(gen_server).

%% API
-export([start_link/2, stop/1]).
-export([select_action/2, update_q_value/5, get_q_table/1]).
-export([set_epsilon/2, set_learning_rate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Records
-record(q_learning_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    q_table :: ets:tid(),
    state_encoder :: function(),
    learning_rate = 0.1 :: float(),
    discount_factor = 0.95 :: float(),
    epsilon = 1.0 :: float(),
    epsilon_min = 0.01 :: float(),
    epsilon_decay = 0.995 :: float()
}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

-spec start_link(pos_integer(), map()) -> {ok, pid()} | {error, term()}.
start_link(N, Options) when is_integer(N), N > 0 ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [N, Options], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc Select action using epsilon-greedy.
-spec select_action(pid(), term()) -> {ok, pos_integer()}.
select_action(Pid, State) ->
    gen_server:call(Pid, {select_action, State}).

%% @doc Update Q-value.
-spec update_q_value(pid(), term(), pos_integer(), float(), term()) -> ok.
update_q_value(Pid, State, Action, Reward, NextState) ->
    gen_server:call(Pid, {update_q_value, State, Action, Reward, NextState}).

%% @doc Get Q-table snapshot.
-spec get_q_table(pid()) -> map().
get_q_table(Pid) ->
    gen_server:call(Pid, get_q_table).

%% @doc Set exploration rate.
-spec set_epsilon(pid(), float()) -> ok.
set_epsilon(Pid, Epsilon) when is_float(Epsilon), Epsilon >= 0, Epsilon =< 1 ->
    gen_server:call(Pid, {set_epsilon, Epsilon}).

%% @doc Set learning rate.
-spec set_learning_rate(pid(), float()) -> ok.
set_learning_rate(Pid, Rate) when is_float(Rate), Rate > 0, Rate =< 1 ->
    gen_server:call(Pid, {set_learning_rate, Rate}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([N, Options]) ->
    QTable = ets:new(q_learning_table, [set, protected]),
    {ok, #q_learning_state{
        n = N,
        m = maps:get(m, Options, 1),
        q_table = QTable,
        learning_rate = maps:get(learning_rate, Options, 0.1),
        discount_factor = maps:get(discount_factor, Options, 0.95),
        epsilon = maps:get(epsilon, Options, 1.0),
        epsilon_min = maps:get(epsilon_min, Options, 0.01),
        epsilon_decay = maps:get(epsilon_decay, Options, 0.995)
    }}.

handle_call({select_action, State}, _From, #q_learning_state{epsilon = Epsilon, q_table = QTable, n = N} = S) ->
    Action = case rand:uniform() of
        X when X < Epsilon ->
            %% Explore: random action
            rand:uniform(N);
        _ ->
            %% Exploit: best Q-value
            Best = get_best_q(QTable, State, N),
            Best
    end,
    {reply, {ok, Action}, S};

handle_call({update_q_value, State, Action, Reward, NextState}, _From, #q_learning_state{
    q_table = QTable, learning_rate = Alpha, discount_factor = Gamma
} = S) ->
    %% Q-learning update: Q(s,a) += alpha * (reward + gamma * max(Q(s',a')) - Q(s,a))
    Key = {State, Action},
    CurrentQ = ets:lookup(QTable, Key),
    MaxNextQ = get_max_q(QTable, NextState, S#q_learning_state.n),
    NewQ = case CurrentQ of
        [{Key, QVal}] ->
            QVal + Alpha * (Reward + Gamma * MaxNextQ - QVal);
        [] ->
            Alpha * (Reward + Gamma * MaxNextQ)
    end,
    ets:insert(QTable, {Key, NewQ}),
    {reply, ok, S};

handle_call(get_q_table, _From, #q_learning_state{q_table = QTable} = S) ->
    QMap = maps:from_list(ets:tab2list(QTable)),
    {reply, QMap, S};

handle_call({set_epsilon, Epsilon}, _From, State) ->
    {reply, ok, State#q_learning_state{epsilon = Epsilon}};

handle_call({set_learning_rate, Rate}, _From, State) ->
    {reply, ok, State#q_learning_state{learning_rate = Rate}};

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

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec get_best_q(ets:tid(), term(), pos_integer()) -> pos_integer().
get_best_q(QTable, State, N) ->
    %% Find action with max Q-value for this state
    Actions = lists:seq(1, N),
    QValues = lists:map(fun(A) ->
        case ets:lookup(QTable, {State, A}) of
            [{_, QVal}] -> QVal;
            [] -> 0.0
        end
    end, Actions),
    %% Find argmax
    {_MaxQ, MaxIdx} = lists:foldl(fun({Q, Idx}, {MaxQAcc, MaxIdxAcc}) ->
        case Q > MaxQAcc of
            true -> {Q, Idx};
            false -> {MaxQAcc, MaxIdxAcc}
        end
    end, {-1.0, 1}, lists:zip(QValues, Actions)),
    MaxIdx.

%% @private
-spec get_max_q(ets:tid(), term(), pos_integer()) -> float().
get_max_q(QTable, State, N) ->
    Actions = lists:seq(1, N),
    QValues = lists:map(fun(A) ->
        case ets:lookup(QTable, {State, A}) of
            [{_, QVal}] -> QVal;
            [] -> 0.0
        end
    end, Actions),
    case QValues of
        [] -> 0.0;
        _ -> lists:max(QValues)
    end.
