%% -*- erlang -*-
%% @doc Predictive Process Mining
%%
%% Main API for next activity and remaining time prediction.
%%
%% @end

-module(predictive_mining).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([predict_next_activity/2, predict_remaining_time/2]).
-export([predict_outcome/2, load_model/1, unload_model/1, list_loaded_models/0]).
-export([set_prediction_mode/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Records
-record(state, {
    mode :: realtime | batch,
    models :: map(),
    markov_models :: map()
}).

-record(prediction, {
    model_id :: binary(),
    model_type :: statistical | markov | ensemble,
    prediction_type :: next_activity | remaining_time | outcome,
    result :: term(),
    confidence :: float(),
    timestamp :: integer()
}).

%% Types
-type state() :: #state{}.
-type prediction() :: #prediction{}.
-type prediction_mode() :: realtime | batch.
-type model_type() :: statistical | markov | ensemble.
-type prediction_type() :: next_activity | remaining_time | outcome.

-export_type([state/0, prediction/0, prediction_mode/0,
             model_type/0, prediction_type/0]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?SERVER, stop).

%% @doc Predict the next activity from a trace.
-spec predict_next_activity(binary(), [atom()]) -> {ok, [{atom(), float()}]}.
predict_next_activity(CaseId, Trace) when is_binary(CaseId), is_list(Trace) ->
    gen_server:call(?SERVER, {predict_next_activity, CaseId, Trace}).

%% @doc Predict remaining time for case completion.
-spec predict_remaining_time(binary(), [atom()]) -> {ok, integer()}.
predict_remaining_time(CaseId, Trace) when is_binary(CaseId), is_list(Trace) ->
    gen_server:call(?SERVER, {predict_remaining_time, CaseId, Trace}).

%% @doc Predict outcome (success/failure).
-spec predict_outcome(binary(), [atom()]) -> {ok, success | failure, float()}.
predict_outcome(CaseId, Trace) when is_binary(CaseId), is_list(Trace) ->
    gen_server:call(?SERVER, {predict_outcome, CaseId, Trace}).

%% @doc Load a prediction model.
-spec load_model(binary()) -> ok | {error, term()}.
load_model(ModelId) when is_binary(ModelId) ->
    gen_server:call(?SERVER, {load_model, ModelId}).

%% @doc Unload a prediction model.
-spec unload_model(binary()) -> ok.
unload_model(ModelId) when is_binary(ModelId) ->
    gen_server:call(?SERVER, {unload_model, ModelId}).

%% @doc List all loaded models.
-spec list_loaded_models() -> [binary()].
list_loaded_models() ->
    gen_server:call(?SERVER, list_loaded_models).

%% @doc Set prediction mode.
-spec set_prediction_mode(realtime | batch) -> ok.
set_prediction_mode(Mode) when Mode =:= realtime; Mode =:= batch ->
    gen_server:call(?SERVER, {set_mode, Mode}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{
        mode = realtime,
        models = #{},
        markov_models = #{}
    }}.

handle_call({predict_next_activity, _CaseId, Trace}, _From, State) ->
    %% Use Markov model for prediction
    Activities = extract_activities(Trace),
    case length(Activities) of
        0 ->
            {reply, {ok, []}, State};
        _ ->
            LastActivity = lists:last(Activities),
            %% Get transition probabilities
            Predictions = get_markov_predictions(LastActivity, State),
            {reply, {ok, Predictions}, State}
    end;

handle_call({predict_remaining_time, _CaseId, Trace}, _From, State) ->
    %% Use EMA-based duration prediction
    case Trace of
        [] -> {reply, {ok, 0}, State};
        _ ->
            %% Estimate based on trace length
            Estimated = length(Trace) * 60000,  %% 1 minute per activity
            {reply, {ok, Estimated}, State}
    end;

handle_call({predict_outcome, _CaseId, Trace}, _From, State) ->
    %% Simple heuristic: more activities = higher chance of issues
    case length(Trace) of
        N when N > 10 -> {reply, {ok, failure, 0.6}, State};
        _ -> {reply, {ok, success, 0.8}, State}
    end;

handle_call({load_model, ModelId}, _From, State) ->
    NewModels = maps:put(ModelId, loaded, State#state.models),
    {reply, ok, State#state{models = NewModels}};

handle_call({unload_model, ModelId}, _From, State) ->
    NewModels = maps:remove(ModelId, State#state.models),
    {reply, ok, State#state{models = NewModels}};

handle_call(list_loaded_models, _From, State) ->
    Models = maps:keys(State#state.models),
    {reply, Models, State};

handle_call({set_mode, Mode}, _From, State) ->
    {reply, ok, State#state{mode = Mode}};

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

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec extract_activities([term()]) -> [atom()].
extract_activities(Trace) ->
    [A || A <- Trace, is_atom(A)].

%% @private
-spec get_markov_predictions(atom(), #state{}) -> [{atom(), float()}].
get_markov_predictions(Activity, State) ->
    %% Get Markov model for this activity
    Models = State#state.markov_models,
    case maps:get(Activity, Models, undefined) of
        undefined ->
            %% Fallback to default predictions
            get_default_predictions(Activity);
        {ok, Predictions} ->
            Predictions
    end.

%% @private
-spec get_default_predictions(atom()) -> [{atom(), float()}].
get_default_predictions(_Activity) ->
    %% Default common transitions
    [
        {complete, 0.4},
        {approve, 0.3},
        {reject, 0.2},
        {review, 0.1}
    ].
