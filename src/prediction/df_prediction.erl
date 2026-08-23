%% -*- erlang -*-
%% @doc Directly-Follows Based Next Activity Prediction
%%
%% This module implements a simple next activity predictor based on
%% directly-follows relations in the event log.
%%
%% The algorithm:
%% 1. Builds a frequency matrix of directly-follows relations
%% 2. For a given trace, finds the last activity
%% 3. Returns probability distribution over possible next activities
%%
%% @end

-module(df_prediction).

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([predict_next_activity/1]).
-export([predict_next_activity/2]).
-export([train_from_log/1]).
-export([get_model/0]).

%% Utility exports for testing
-export([build_df_matrix/1]).
-export([find_followers/2]).
-export([calculate_probabilities/1]).

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type activity() :: atom().
-type trace() :: [activity()].
-type event_log() :: [trace()].
-type df_matrix() :: #{{activity(), activity()} => pos_integer()}.
-type probability() :: float().

-type model() :: #{
    df_matrix => df_matrix(),
    total_transitions => pos_integer(),
    activities => sets:set(activity())
}.

-export_type([model/0, probability/0]).

%% Server state
-record(state, {
    model :: model() | undefined
}).

%%====================================================================
%% gen_server API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Predicts the next activity given a trace.
%%
%% Uses the current model to predict probabilities of next activities.
%%
-spec predict_next_activity(trace()) -> [{activity(), probability()}].
predict_next_activity(Trace) ->
    predict_next_activity(Trace, get_model()).

%% @doc Predicts next activity using a specific model.
%%
-spec predict_next_activity(trace(), model() | undefined) ->
    [{activity(), probability()}].
predict_next_activity(_Trace, undefined) ->
    [];
predict_next_activity([], _Model) ->
    [];
predict_next_activity(Trace, #{df_matrix := DFMatrix}) ->
    %% Get the last activity in the trace
    LastActivity = lists:last(Trace),

    %% Find all activities that follow the last activity
    Followers = find_followers(LastActivity, DFMatrix),

    %% Calculate probabilities
    calculate_probabilities(Followers).

%% @doc Trains a prediction model from an event log.
%%
-spec train_from_log(event_log()) -> {ok, model()}.
train_from_log(Log) when is_list(Log) ->
    DFMatrix = build_df_matrix(Log),
    TotalTransitions = lists:sum(maps:values(DFMatrix)),
    Activities = extract_activities(Log),

    Model = #{
        df_matrix => DFMatrix,
        total_transitions => TotalTransitions,
        activities => Activities
    },

    %% Update server state if running
    case whereis(?MODULE) of
        undefined -> ok;
        _Pid -> gen_server:cast(?MODULE, {update_model, Model})
    end,

    {ok, Model}.

%% @doc Gets the current model from the server.
%%
-spec get_model() -> model() | undefined.
get_model() ->
    case whereis(?MODULE) of
        undefined -> undefined;
        _Pid -> gen_server:call(?MODULE, get_model)
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #state{model = undefined}}.

handle_call(get_model, _From, State = #state{model = Model}) ->
    {reply, Model, State};
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({update_model, Model}, State) ->
    {noreply, State#state{model = Model}};
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
-spec build_df_matrix(event_log()) -> df_matrix().
build_df_matrix(Log) ->
    lists:foldl(fun(Trace, Acc) ->
        extract_df_from_trace(Trace, Acc)
    end, #{}, Log).

%% @private
-spec extract_df_from_trace(trace(), df_matrix()) -> df_matrix().
extract_df_from_trace([], Acc) ->
    Acc;
extract_df_from_trace([_], Acc) ->
    Acc;
extract_df_from_trace([A, B | Rest], Acc) ->
    Key = {A, B},
    NewAcc = Acc#{Key => maps:get(Key, Acc, 0) + 1},
    extract_df_from_trace([B | Rest], NewAcc).

%% @private
-spec find_followers(activity(), df_matrix()) -> [{activity(), pos_integer()}].
find_followers(Activity, DFMatrix) ->
    maps:fold(fun({From, To}, Count, Acc) when From =:= Activity ->
        [{To, Count} | Acc];
       (_, _, Acc) ->
        Acc
    end, [], DFMatrix).

%% @private
-spec calculate_probabilities([{activity(), pos_integer()}]) ->
    [{activity(), probability()}].
calculate_probabilities(Followers) ->
    Total = lists:sum([Count || {_Activity, Count} <- Followers]),

    case Total of
        0 -> [];
        _ ->
            lists:map(fun({Activity, Count}) ->
                {Activity, Count / Total}
            end, Followers)
    end.

%% @private
-spec extract_activities(event_log()) -> sets:set(activity()).
extract_activities(Log) ->
    lists:foldl(fun(Trace, Acc) ->
        lists:foldl(fun(Activity, Set) ->
            sets:add_element(Activity, Set)
        end, Acc, Trace)
    end, sets:new(), Log).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test data
%%--------------------------------------------------------------------

simple_log() ->
    [[a, b, c],
     [a, b, c],
     [a, b, d]].

branching_log() ->
    [[a, b, c],
     [a, b, d],
     [a, b, c],
     [a, b, d],
     [a, b, e]].

complex_log() ->
    [[start, register, approve, complete],
     [start, register, reject, complete],
     [start, verify, approve, complete]].

%%--------------------------------------------------------------------
%% DF Matrix building tests
%%--------------------------------------------------------------------

build_df_matrix_test() ->
    Log = simple_log(),
    Matrix = build_df_matrix(Log),
    ?assertEqual(2, maps:get({a, b}, Matrix)),
    ?assertEqual(2, maps:get({b, c}, Matrix)),
    ?assertEqual(1, maps:get({b, d}, Matrix)).

build_df_matrix_empty_test() ->
    Log = [],
    Matrix = build_df_matrix(Log),
    ?assertEqual(0, map_size(Matrix)).

build_df_matrix_single_trace_test() ->
    Log = [[a, b, c]],
    Matrix = build_df_matrix(Log),
    ?assertEqual(1, maps:get({a, b}, Matrix)),
    ?assertEqual(1, maps:get({b, c}, Matrix)),
    ?assertEqual(undefined, maps:get({c, a}, Matrix, undefined)).

%%--------------------------------------------------------------------
%% Find followers tests
%%--------------------------------------------------------------------

find_followers_test() ->
    Matrix = #{{a, b} => 2, {a, c} => 1, {b, d} => 3},
    Followers = find_followers(a, Matrix),
    ?assertEqual(2, length(Followers)),
    ?assert(lists:keymember(b, 1, Followers)),
    ?assert(lists:keymember(c, 1, Followers)).

find_followers_none_test() ->
    Matrix = #{{a, b} => 2},
    Followers = find_followers(b, Matrix),
    ?assertEqual([], Followers).

%%--------------------------------------------------------------------
%% Calculate probabilities tests
%%--------------------------------------------------------------------

calculate_probabilities_test() ->
    Followers = [{b, 2}, {c, 1}, {d, 1}],
    Probs = calculate_probabilities(Followers),
    ?assertEqual(3, length(Probs)),
    ?assertEqual(0.5, proplists:get_value(b, Probs)),
    ?assertEqual(0.25, proplists:get_value(c, Probs)),
    ?assertEqual(0.25, proplists:get_value(d, Probs)).

calculate_probabilities_empty_test() ->
    Followers = [],
    Probs = calculate_probabilities(Followers),
    ?assertEqual([], Probs).

calculate_probabilities_single_test() ->
    Followers = [{b, 5}],
    Probs = calculate_probabilities(Followers),
    ?assertEqual(1.0, proplists:get_value(b, Probs)).

%%--------------------------------------------------------------------
%% Prediction tests
%%--------------------------------------------------------------------

predict_next_activity_test() ->
    Log = simple_log(),
    {ok, Model} = train_from_log(Log),
    Predictions = predict_next_activity([a, b], Model),
    ?assert(length(Predictions) > 0),
    {Activity, Prob} = hd(Predictions),
    ?assert(is_atom(Activity)),
    ?assert(is_float(Prob)),
    ?assert(Prob > 0.0).

predict_next_activity_branching_test() ->
    Log = branching_log(),
    {ok, Model} = train_from_log(Log),
    Predictions = predict_next_activity([a, b], Model),
    ?assertEqual(3, length(Predictions)),
    %% c and d should have equal probability (2/5 each)
    ?assert(lists:keymember(c, 1, Predictions)),
    ?assert(lists:keymember(d, 1, Predictions)),
    ?assert(lists:keymember(e, 1, Predictions)).

predict_next_activity_empty_trace_test() ->
    Log = simple_log(),
    {ok, Model} = train_from_log(Log),
    Predictions = predict_next_activity([], Model),
    ?assertEqual([], Predictions).

predict_next_activity_no_model_test() ->
    Predictions = predict_next_activity([a, b], undefined),
    ?assertEqual([], Predictions).

%%--------------------------------------------------------------------
%% Training tests
%%--------------------------------------------------------------------

train_from_log_test() ->
    Log = simple_log(),
    {ok, Model} = train_from_log(Log),
    ?assert(is_map(Model)),
    ?assert(maps:is_key(df_matrix, Model)),
    ?assert(maps:is_key(total_transitions, Model)),
    ?assert(maps:is_key(activities, Model)).

train_from_log_total_transitions_test() ->
    Log = simple_log(),
    {ok, #{total_transitions := Total}} = train_from_log(Log),
    ?assertEqual(5, Total).  %% a->b:3, b->c:2, b->d:1

train_from_log_activities_test() ->
    Log = complex_log(),
    {ok, #{activities := Activities}} = train_from_log(Log),
    ?assert(sets:is_element(start, Activities)),
    ?assert(sets:is_element(register, Activities)),
    ?assert(sets:is_element(approve, Activities)),
    ?assert(sets:is_element(complete, Activities)).

%%--------------------------------------------------------------------
%% Probability sum tests
%%--------------------------------------------------------------------

probabilities_sum_to_one_test() ->
    Log = branching_log(),
    {ok, Model} = train_from_log(Log),
    Predictions = predict_next_activity([a, b], Model),
    Total = lists:sum([P || {_A, P} <- Predictions]),
    ?assert(Total >= 0.99),
    ?assert(Total =< 1.01).

%%--------------------------------------------------------------------
%% Edge cases tests
%%--------------------------------------------------------------------

predict_with_unknown_activity_test() ->
    Log = [[a, b, c]],
    {ok, Model} = train_from_log(Log),
    Predictions = predict_next_activity([unknown], Model),
    ?assertEqual([], Predictions).

predict_with_partial_match_test() ->
    Log = [[a, b, c], [x, y, z]],
    {ok, Model} = train_from_log(Log),
    Predictions = predict_next_activity([a], Model),
    ?assert(length(Predictions) > 0).

-endif.
