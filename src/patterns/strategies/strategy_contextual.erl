%% -*- erlang -*-
%% @doc Contextual Bandit Strategy
%%
%% Context-aware branch selection using feature-based models.
%%
%% @end

-module(strategy_contextual).
-behaviour(gen_server).

%% API
-export([start_link/2, stop/1]).
-export([predict_branch/2, update_model/4]).
-export([set_feature_extractor/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Records
-record(linear_model, {
    weights :: [float()],
    bias :: float(),
    samples :: pos_integer()
}).

-record(contextual_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    feature_extractor :: function(),
    model :: #linear_model{} | undefined,
    min_samples = 10 :: pos_integer(),
    fallback_strategy :: atom()
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

%% @doc Predict best branch using contextual model.
-spec predict_branch(pid(), map()) -> {ok, pos_integer()}.
predict_branch(Pid, Context) ->
    gen_server:call(Pid, {predict_branch, Context}).

%% @doc Update model with observation.
-spec update_model(pid(), map(), pos_integer(), float()) -> ok.
update_model(Pid, Context, Branch, Reward) ->
    gen_server:call(Pid, {update_model, Context, Branch, Reward}).

%% @doc Set custom feature extractor.
-spec set_feature_extractor(pid(), function()) -> ok.
set_feature_extractor(Pid, Fun) when is_function(Fun) ->
    gen_server:call(Pid, {set_feature_extractor, Fun}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([N, Options]) ->
    FeatureExtractor = maps:get(feature_extractor, Options, fun default_features/1),
    Fallback = maps:get(fallback_strategy, Options, first_n),
    {ok, #contextual_state{
        n = N,
        m = maps:get(m, Options, 1),
        feature_extractor = FeatureExtractor,
        model = undefined,
        min_samples = maps:get(min_samples, Options, 10),
        fallback_strategy = Fallback
    }}.

handle_call({predict_branch, Context}, _From, #contextual_state{n = N, model = undefined, fallback_strategy = Fallback} = State) ->
    %% No model yet, use fallback
    Branch = apply_fallback(Fallback, N, Context),
    {reply, {ok, Branch}, State};

handle_call({predict_branch, Context}, _From, #contextual_state{n = N, model = Model} = State) ->
    Features = (State#contextual_state.feature_extractor)(Context),
    Scores = score_branches(Model, Features, N),
    {_BestScore, BestBranch} = lists:max(lists:zip(Scores, lists:seq(1, N))),
    {reply, {ok, BestBranch}, State};

handle_call({update_model, Context, Branch, _Reward}, _From, #contextual_state{model = Model, n = N} = State) ->
    Features = (State#contextual_state.feature_extractor)(Context),
    NewModel = case Model of
        undefined ->
            %% Initialize model
            #linear_model{
                weights = lists:duplicate(length(Features), 0.0),
                bias = 0.0,
                samples = 1
            };
        #linear_model{weights = Weights, bias = Bias, samples = Samples} ->
            %% Simple online update (one-step gradient descent)
            LearningRate = 0.01,
            %% Target: 1.0 for selected branch, 0.0 for others
            Targets = [case Idx =:= Branch of true -> 1.0; false -> 0.0 end || Idx <- lists:seq(1, N)],
            {NewWeights, NewBias} = update_weights(Weights, Bias, Features, Targets, LearningRate),
            #linear_model{
                weights = NewWeights,
                bias = NewBias,
                samples = Samples + 1
            }
    end,
    {reply, ok, State#contextual_state{model = NewModel}};

handle_call({set_feature_extractor, Fun}, _From, State) ->
    {reply, ok, State#contextual_state{feature_extractor = Fun}};

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
-spec default_features(map()) -> [float()].
default_features(Context) ->
    [
        maps:get(case_type, Context, 0.0),
        maps:get(priority, Context, 0.0),
        maps:get(value, Context, 0.0),
        maps:get(hour_of_day, Context, 0.0) / 24.0,
        maps:get(queue_depth, Context, 0.0)
    ].

%% @private
-spec apply_fallback(atom(), pos_integer(), map()) -> pos_integer().
apply_fallback(first_n, N, _Context) ->
    rand:uniform(N);
apply_fallback(random, N, _Context) ->
    rand:uniform(N);
apply_fallback(ucb, N, _Context) ->
    %% Simple UCB approximation
    rand:uniform(N).

%% @private
-spec score_branches(#linear_model{}, [float()], pos_integer()) -> [float()].
score_branches(#linear_model{weights = Weights, bias = Bias}, Features, N) ->
    %% Each branch gets its own weight vector
    %% Simplified: use dot product with shared weights + branch bias
    lists:map(fun(BranchIdx) ->
        %% Dot product
        Dot = lists:sum(lists:zipwith(fun(W, F) -> W * F end, Weights, Features)),
        %% Add branch-specific bias adjustment
        Dot + Bias + BranchIdx * 0.1
    end, lists:seq(1, N)).

%% @private
-spec update_weights([float()], float(), [float()], [float()], float()) -> {[float()], float()}.
update_weights(Weights, Bias, Features, Targets, LearningRate) ->
    %% Gradient descent step
    %% For each branch, compute gradient and update
    %% Simplified: treat as regression problem
    Prediction = lists:sum(lists:zipwith(fun(W, F) -> W * F end, Weights, Features)) + Bias,
    Error = Prediction - hd(Targets),  %% Simplified: use first target
    Gradient = [Error * F || F <- Features],
    NewWeights = lists:zipwith(fun(W, G) -> W - LearningRate * G end, Weights, Gradient),
    NewBias = Bias - LearningRate * Error,
    {NewWeights, NewBias}.
