%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc RNN-based Prediction for Process Mining
%%
%% This module implements an LSTM-style recurrent neural network for
%% sequential prediction in process mining. It maintains hidden state
%% across predictions and uses gen_server for state management.
%%
%% <h3>Key Concepts</h3>
%%
%% <ul>
%%   <li><b>Hidden State:</b> Internal representation maintained across predictions</li>
%%   <li><b>Cell State:</b> Long-term memory for LSTM-style gates</li>
%%   <li><b>Gates:</b> Input, forget, and output gates for information flow</li>
%%   <li><b>Sequence Prediction:</b> Predict next activity in trace</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(rnn_predict).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([stop/1]).
-export([predict_next/2, predict_sequence/3]).
-export([reset_state/1, get_state/1]).
-export([train_step/3, get_weights/1]).
-export([set_learning_rate/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% Records
-record(state, {
    agent_id :: binary(),
    hidden_state :: vector(),
    cell_state :: vector(),
    weights :: weights(),
    learning_rate :: float(),
    input_size :: pos_integer(),
    hidden_size :: pos_integer(),
    output_size :: pos_integer(),
    sequence_history :: queue:queue()
}).

-record(vector, {
    data :: [float()],
    size :: pos_integer()
}).

-record(weights, {
    wii :: matrix(),  %% Input to input gate
    whi :: matrix(),  %% Hidden to input gate
    wif :: matrix(),  %% Input to forget gate
    whf :: matrix(),  %% Hidden to forget gate
    wio :: matrix(),  %% Input to output gate
    who :: matrix(),  %% Hidden to output gate
    wig :: matrix(),  %% Input to cell
    whg :: matrix(),  %% Hidden to cell
    bi :: vector(),   %% Input gate bias
    bf :: vector(),   %% Forget gate bias
    bo :: vector(),   %% Output gate bias
    bg :: vector()    %% Cell bias
}).

-record(matrix, {
    data :: [float()],
    rows :: pos_integer(),
    cols :: pos_integer()
}).

%% Types
-type vector() :: #vector{}.
-type matrix() :: #matrix{}.
-type weights() :: #weights{}.
-type prediction_result() :: #{
    activity => atom() | undefined,
    probabilities => [{atom(), float()}],
    confidence => float()
}.

-export_type([vector/0, matrix/0, weights/0, prediction_result/0]).

%%====================================================================
%% API
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(<<"default_rnn">>).

-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(AgentId) ->
    gen_server:start_link(?MODULE, [AgentId], []).

-spec stop(binary()) -> ok.
stop(AgentId) ->
    gen_server:stop(AgentId).

%% @doc Predicts the next activity given current state and trace.
-spec predict_next(pid() | binary(), [atom()]) -> {ok, prediction_result()}.
predict_next(Pid, Trace) when is_pid(Pid) ->
    gen_server:call(Pid, {predict, Trace});
predict_next(AgentId, Trace) when is_binary(AgentId) ->
    case whereis(AgentId) of
        undefined -> {error, not_found};
        Pid -> gen_server:call(Pid, {predict, Trace})
    end.

%% @doc Predicts a sequence of future activities.
-spec predict_sequence(pid() | binary(), [atom()], pos_integer()) ->
    {ok, [prediction_result()]}.
predict_sequence(Pid, Trace, Length) when is_pid(Pid), is_integer(Length), Length > 0 ->
    gen_server:call(Pid, {predict_sequence, Trace, Length});
predict_sequence(AgentId, Trace, Length) when is_binary(AgentId) ->
    case whereis(AgentId) of
        undefined -> {error, not_found};
        Pid -> gen_server:call(Pid, {predict_sequence, Trace, Length})
    end.

%% @doc Resets the hidden and cell states.
-spec reset_state(pid() | binary()) -> ok.
reset_state(Pid) when is_pid(Pid) ->
    gen_server:cast(Pid, reset_state);
reset_state(AgentId) when is_binary(AgentId) ->
    case whereis(AgentId) of
        undefined -> {error, not_found};
        Pid -> gen_server:cast(Pid, reset_state)
    end.

%% @doc Gets the current state vectors.
-spec get_state(pid() | binary()) -> {ok, #{hidden => vector(), cell => vector()}}.
get_state(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_state);
get_state(AgentId) when is_binary(AgentId) ->
    case whereis(AgentId) of
        undefined -> {error, not_found};
        Pid -> gen_server:call(Pid, get_state)
    end.

%% @doc Performs a training step.
-spec train_step(pid() | binary(), [atom()], atom()) -> {ok, float()}.
train_step(Pid, Input, Target) when is_pid(Pid) ->
    gen_server:call(Pid, {train, Input, Target});
train_step(AgentId, Input, Target) when is_binary(AgentId) ->
    case whereis(AgentId) of
        undefined -> {error, not_found};
        Pid -> gen_server:call(Pid, {train, Input, Target})
    end.

%% @doc Gets the current weights.
-spec get_weights(pid() | binary()) -> {ok, weights()}.
get_weights(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_weights);
get_weights(AgentId) when is_binary(AgentId) ->
    case whereis(AgentId) of
        undefined -> {error, not_found};
        Pid -> gen_server:call(Pid, get_weights)
    end.

%% @doc Sets the learning rate.
-spec set_learning_rate(pid() | binary(), float()) -> ok | {error, term()}.
set_learning_rate(Pid, Rate) when is_pid(Pid), is_float(Rate), Rate > 0, Rate =< 1 ->
    gen_server:call(Pid, {set_learning_rate, Rate});
set_learning_rate(AgentId, Rate) when is_binary(AgentId) ->
    case whereis(AgentId) of
        undefined -> {error, not_found};
        Pid -> gen_server:call(AgentId, {set_learning_rate, Rate})
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([AgentId]) ->
    %% Initialize with default sizes
    InputSize = 50,   %% Vocabulary embedding size
    HiddenSize = 64,  %% Hidden state size
    OutputSize = 20,  %% Number of activities

    %% Initialize random weights
    Weights = init_weights(InputSize, HiddenSize, OutputSize),

    %% Initialize zero states
    HiddenState = zero_vector(HiddenSize),
    CellState = zero_vector(HiddenSize),

    State = #state{
        agent_id = AgentId,
        hidden_state = HiddenState,
        cell_state = CellState,
        weights = Weights,
        learning_rate = 0.01,
        input_size = InputSize,
        hidden_size = HiddenSize,
        output_size = OutputSize,
        sequence_history = queue:new()
    },
    {ok, State}.

handle_call({predict, Trace}, _From, State) ->
    %% Encode trace to embeddings
    Input = encode_trace(Trace, State#state.input_size),

    %% Process through LSTM
    {NewHidden, NewCell, Output} = lstm_step(Input, State#state.hidden_state,
                                              State#state.cell_state, State#state.weights),

    %% Decode output to probabilities
    Probs = softmax(Output),

    %% Get top predictions
    TopPredictions = get_top_predictions(Probs, 5),

    %% Update state
    NewState = State#state{
        hidden_state = NewHidden,
        cell_state = NewCell,
        sequence_history = queue_in(Trace, State#state.sequence_history, 10)
    },

    case TopPredictions of
        [{Activity, Conf} | _] ->
            Result = #{
                activity => Activity,
                probabilities => TopPredictions,
                confidence => Conf
            },
            {reply, {ok, Result}, NewState};
        [] ->
            Result = #{
                activity => undefined,
                probabilities => [],
                confidence => 0.0
            },
            {reply, {ok, Result}, NewState}
    end;

handle_call({predict_sequence, Trace, Length}, _From, State) ->
    %% Generate sequence iteratively
    {Results, FinalState} = generate_sequence(Trace, Length, State),

    {reply, {ok, Results}, FinalState};

handle_call(get_state, _From, State) ->
    {reply, {ok, #{hidden => State#state.hidden_state,
                   cell => State#state.cell_state}}, State};

handle_call({train, Input, Target}, _From, State) ->
    %% Encode input and target
    InputVec = encode_trace(Input, State#state.input_size),
    TargetIdx = activity_to_index(Target, State#state.output_size),

    %% Forward pass
    {NewHidden, NewCell, Output} = lstm_step(InputVec, State#state.hidden_state,
                                              State#state.cell_state, State#state.weights),

    %% Compute loss
    Loss = cross_entropy_loss(Output, TargetIdx),

    %% Simple gradient update (not full backprop)
    NewWeights = simple_update(State#state.weights, InputVec, TargetIdx, Output,
                                State#state.learning_rate),

    NewState = State#state{
        hidden_state = NewHidden,
        cell_state = NewCell,
        weights = NewWeights
    },

    {reply, {ok, Loss}, NewState};

handle_call(get_weights, _From, State) ->
    {reply, {ok, State#state.weights}, State};

handle_call({set_learning_rate, Rate}, _From, State) ->
    {reply, ok, State#state{learning_rate = Rate}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(reset_state, State) ->
    HiddenSize = State#state.hidden_size,
    NewState = State#state{
        hidden_state = zero_vector(HiddenSize),
        cell_state = zero_vector(HiddenSize),
        sequence_history = queue:new()
    },
    {noreply, NewState};

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
init_weights(InputSize, HiddenSize, OutputSize) ->
    %% Xavier initialization
    Scale = math:sqrt(2.0 / (InputSize + HiddenSize)),

    #weights{
        wii = random_matrix(HiddenSize, InputSize, Scale),
        whi = random_matrix(HiddenSize, HiddenSize, Scale),
        wif = random_matrix(HiddenSize, InputSize, Scale),
        whf = random_matrix(HiddenSize, HiddenSize, Scale),
        wio = random_matrix(HiddenSize, InputSize, Scale),
        who = random_matrix(HiddenSize, HiddenSize, Scale),
        wig = random_matrix(HiddenSize, InputSize, Scale),
        whg = random_matrix(HiddenSize, HiddenSize, Scale),
        bi = zero_vector(HiddenSize),
        bf = zero_vector(HiddenSize),
        bo = zero_vector(HiddenSize),
        bg = zero_vector(HiddenSize)
    }.

%% @private
zero_vector(Size) ->
    #vector{data = lists:duplicate(Size, 0.0), size = Size}.

%% @private
random_matrix(Rows, Cols, Scale) ->
    Data = [(rand:uniform() * 2.0 - 1.0) * Scale || _ <- lists:seq(1, Rows * Cols)],
    #matrix{data = Data, rows = Rows, cols = Cols}.

%% @private
encode_trace(Trace, Size) ->
    %% Simple hash-based encoding
    case Trace of
        [] -> zero_vector(Size);
        _ ->
            Hash = erlang:phash2(lists:reverse(Trace)),
            %% Convert hash to sparse vector
            Data = [case (Hash rem (I + 1)) of
                0 -> 1.0;
                _ -> 0.0
            end || I <- lists:seq(1, Size)],
            #vector{data = Data, size = Size}
    end.

%% @private
activity_to_index(Activity, OutputSize) ->
    %% Map activity to output index
    case is_atom(Activity) of
        true -> (erlang:phash2(Activity) rem OutputSize) + 1;
        false -> 1
    end.

%% @private
index_to_activity(Index) ->
    %% Map index back to activity (using atoms for demo)
    list_to_existing_atom("activity_" ++ integer_to_list(Index)).

%% @private
lstm_step(Input, Hidden, Cell, Weights) ->
    #weights{
        wii = Wii, whi = Whi, bi = Bi,
        wif = Wif, whf = Whf, bf = Bf,
        wio = Wio, who = Who, bo = Bo,
        wig = Wig, whg = Whg, bg = Bg
    } = Weights,

    %% Input gate: i = sigmoid(Wii * x + Whi * h + bi)
    InputGate = sigmoid(add_vec(add_matvec(matvec(Wii, Input), matvec(Whi, Hidden)), Bi)),

    %% Forget gate: f = sigmoid(Wif * x + Whf * h + bf)
    ForgetGate = sigmoid(add_vec(add_matvec(matvec(Wif, Input), matvec(Whf, Hidden)), Bf)),

    %% Output gate: o = sigmoid(Wio * x + Who * h + bo)
    OutputGate = sigmoid(add_vec(add_matvec(matvec(Wio, Input), matvec(Who, Hidden)), Bo)),

    %% Cell candidate: g = tanh(Wig * x + Whg * h + bg)
    CellCandidate = tanh_vec(add_vec(add_matvec(matvec(Wig, Input), matvec(Whg, Hidden)), Bg)),

    %% New cell state: c' = f * c + i * g
    NewCell = add_vec(mul_vec(ForgetGate, Cell), mul_vec(InputGate, CellCandidate)),

    %% New hidden state: h' = o * tanh(c')
    NewHidden = mul_vec(OutputGate, tanh_vec(NewCell)),

    %% Output (for prediction) is the hidden state
    {NewHidden, NewCell, NewHidden}.

%% @private
softmax(Vector) ->
    #vector{data = Data, size = Size} = Vector,
    %% Find max for numerical stability
    Max = lists:max(Data),
    %% Compute exp(x - max) and sum
    ExpData = [math:exp(X - Max) || X <- Data],
    Sum = lists:sum(ExpData),
    %% Normalize
    [E / Sum || E <- ExpData].

%% @private
get_top_predictions(Probabilities, N) ->
    %% Get top N activities with highest probabilities
    Indexed = lists:zip(lists:seq(1, length(Probabilities)), Probabilities),
    Sorted = lists:reverse(lists:keysort(2, Indexed)),
    Top = lists:sublist(Sorted, min(N, length(Sorted))),

    [{index_to_activity(Id), Prob} || {Id, Prob} <- Top, Prob > 0.01].

%% @private
generate_sequence(Trace, Length, State) ->
    InitialInput = encode_trace(Trace, State#state.input_size),

    {Results, FinalHidden, FinalCell} =
        lists:foldl(fun(_, {Acc, Hidden, Cell}) ->
            {NewHidden, NewCell, Output} = lstm_step(InitialInput, Hidden, Cell,
                                                      State#state.weights),
            Probs = softmax(Output),
            Top = get_top_predictions(Probs, 1),

            Prediction = case Top of
                [{Activity, Conf} | _] ->
                    #{activity => Activity, confidence => Conf, probabilities => Top};
                [] ->
                    #{activity => undefined, confidence => 0.0, probabilities => []}
            end,

            {[Prediction | Acc], NewHidden, NewCell}
        end, {[], State#state.hidden_state, State#state.cell_state}, lists:seq(1, Length)),

    {lists:reverse(Results), State#state{
        hidden_state = FinalHidden,
        cell_state = FinalCell
    }}.

%% @private
cross_entropy_loss(Output, TargetIdx) ->
    %% Simple loss calculation
    Probs = softmax(Output),
    TargetProb = case TargetIdx > length(Probs) of
        true -> 0.0;
        false -> lists:nth(TargetIdx, Probs)
    end,
    case TargetProb > 0.0 of
        true -> -math:log(TargetProb);
        false -> 10.0  %% Large penalty
    end.

%% @private
simple_update(Weights, _Input, _TargetIdx, _Output, LearningRate) ->
    %% Very simplified weight update (placeholder for full backprop)
    %% In production, implement full BPTT
    Weights.

%% @private
queue_in(Item, Queue, MaxLen) ->
    NewQueue = queue:in(Item, Queue),
    case queue:len(NewQueue) > MaxLen of
        true ->
            {_, Q} = queue:out(NewQueue),
            Q;
        false ->
            NewQueue
    end.

%%====================================================================
%% Vector/Matrix Operations
%%====================================================================

%% @private
add_vec(#vector{data = D1}, #vector{data = D2}) when length(D1) =:= length(D2) ->
    #vector{data = [A + B || {A, B} <- lists:zip(D1, D2)], size = length(D1)}.

%% @private
mul_vec(#vector{data = D1}, #vector{data = D2}) when length(D1) =:= length(D2) ->
    #vector{data = [A * B || {A, B} <- lists:zip(D1, D2)], size = length(D1)}.

%% @private
matvec(#matrix{data = Data, rows = Rows, cols = Cols}, #vector{data = Vec}) ->
    %% Matrix-vector multiplication
    [begin
        RowStart = (Row - 1) * Cols + 1,
        RowEnd = RowStart + Cols - 1,
        RowData = lists:sublist(Data, RowStart, Cols),
        lists:sum([A * B || {A, B} <- lists:zip(RowData, Vec)])
    end || Row <- lists:seq(1, Rows)].

%% @private
add_matvec(Vec1, Vec2) when is_list(Vec1), is_list(Vec2), length(Vec1) =:= length(Vec2) ->
    #vector{data = [A + B || {A, B} <- lists:zip(Vec1, Vec2)], size = length(Vec1)}.

%% @private
sigmoid(#vector{data = Data}) ->
    #vector{data = [1.0 / (1.0 + math:exp(-X)) || X <- Data], size = length(Data)}.

%% @private
tanh_vec(#vector{data = Data}) ->
    #vector{data = [math:tanh(X) || X <- Data], size = length(Data)}.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Initialization tests
%%--------------------------------------------------------------------

init_weights_test() ->
    Weights = init_weights(10, 20, 5),
    ?assert(is_record(Weights, weights)),
    ?assert(is_record(Weights#weights.wii, matrix)),
    ?assertEqual(20, (Weights#weights.wii)#matrix.rows),
    ?assertEqual(10, (Weights#weights.wii)#matrix.cols).

zero_vector_test() ->
    Vec = zero_vector(5),
    ?assertEqual(5, Vec#vector.size),
    ?assertEqual([0.0, 0.0, 0.0, 0.0, 0.0], Vec#vector.data).

%%--------------------------------------------------------------------
%% Vector operations tests
%%--------------------------------------------------------------------

add_vec_test() ->
    V1 = #vector{data = [1.0, 2.0, 3.0], size = 3},
    V2 = #vector{data = [0.5, 0.5, 0.5], size = 3},
    Result = add_vec(V1, V2),
    ?assertEqual([1.5, 2.5, 3.5], Result#vector.data).

mul_vec_test() ->
    V1 = #vector{data = [2.0, 3.0, 4.0], size = 3},
    V2 = #vector{data = [0.5, 2.0, 1.0], size = 3},
    Result = mul_vec(V1, V2),
    ?assertEqual([1.0, 6.0, 4.0], Result#vector.data).

sigmoid_test() ->
    Vec = #vector{data = [0.0, 1.0, -1.0], size = 3},
    Result = sigmoid(Vec),
    ?assert(length(Result#vector.data) =:= 3),
    [S1, S2, S3] = Result#vector.data,
    ?assert(S1 > 0.4 andalso S1 < 0.6),
    ?assert(S2 > 0.5),
    ?assert(S3 < 0.5).

tanh_vec_test() ->
    Vec = #vector{data = [0.0, 1.0, -1.0], size = 3},
    Result = tanh_vec(Vec),
    ?assert(length(Result#vector.data) =:= 3).

%%--------------------------------------------------------------------
%% Encoding tests
%%--------------------------------------------------------------------

encode_trace_empty_test() ->
    Vec = encode_trace([], 10),
    ?assertEqual(10, Vec#vector.size),
    ?assert(lists:all(fun(X) -> X =:= 0.0 end, Vec#vector.data)).

encode_trace_test() ->
    Vec = encode_trace([a, b, c], 10),
    ?assertEqual(10, Vec#vector.size),
    ?assert(lists:sum(Vec#vector.data) > 0).

activity_to_index_test() ->
    Idx1 = activity_to_index(activity_a, 20),
    Idx2 = activity_to_index(activity_b, 20),
    ?assert(Idx1 >= 1),
    ?assert(Idx1 =< 20),
    ?assert(Idx2 >= 1),
    ?assert(Idx2 =< 20).

%%--------------------------------------------------------------------
%% Softmax tests
%%--------------------------------------------------------------------

softmax_test() ->
    Vec = #vector{data = [1.0, 2.0, 3.0], size = 3},
    Result = softmax(Vec),
    ?assert(length(Result) =:= 3),
    Sum = lists:sum(Result),
    ?assert(Sum > 0.99 andalso Sum < 1.01).

%%--------------------------------------------------------------------
%% Loss tests
%%--------------------------------------------------------------------

cross_entropy_loss_test() ->
    Output = #vector{data = [0.1, 0.2, 0.7], size = 3},
    ?assert(cross_entropy_loss(Output, 3) < cross_entropy_loss(Output, 1)).

-endif.
