# Mining API Reference

This document provides comprehensive API documentation for the predictive process mining modules in CRE. The mining subsystem provides next activity prediction, remaining time estimation, outcome prediction, and anomaly detection capabilities.

## Table of Contents

1. [predictive_mining](#predictive_mining) - Main API for predictions
2. [pred_rnn](#pred_rnn) - Lightweight RNN for sequence prediction
3. [pred_stats](#pred_stats) - Statistical models (Markov, EMA, Linear Regression)
4. [pred_training](#pred_training) - Training data collection and preparation
5. [anomaly_detection](#anomaly_detection) - Real-time and batch anomaly detection

---

## predictive_mining

**Module:** `src/mining/predictive_mining.erl`

**Purpose:** Main gen_server API for next activity prediction, remaining time estimation, and outcome prediction. Acts as the primary interface for predictive process mining operations.

### Types

```erlang
-type state() :: #state{}.
-type prediction() :: #prediction{}.
-type prediction_mode() :: realtime | batch.
-type model_type() :: statistical | markov | ensemble.
-type prediction_type() :: next_activity | remaining_time | outcome.
```

### Records

```erlang
-record(prediction, {
    model_id       :: binary(),    % Model identifier
    model_type     :: model_type(), % statistical | markov | ensemble
    prediction_type :: prediction_type(), % next_activity | remaining_time | outcome
    result         :: term(),      % Prediction result
    confidence     :: float(),     % Confidence score (0.0-1.0)
    timestamp      :: integer()    % Unix timestamp in milliseconds
}).
```

### Exported Functions

#### start_link/0

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

**Description:** Starts the predictive mining server.

**Return Values:**
- `{ok, Pid}` - Server started successfully
- `{error, Reason}` - Failed to start (e.g., already started)

**Example:**
```erlang
{ok, Pid} = predictive_mining:start_link().
```

#### stop/0

```erlang
-spec stop() -> ok.
```

**Description:** Stops the predictive mining server gracefully.

**Example:**
```erlang
predictive_mining:stop().
```

#### predict_next_activity/2

```erlang
-spec predict_next_activity(binary(), [atom()]) -> {ok, [{atom(), float()}]}.
```

**Description:** Predicts the next activity in a trace using Markov chain models. Returns a list of possible next activities with their probabilities.

**Parameters:**
- `CaseId` - Binary identifier for the process case
- `Trace` - List of activity atoms representing the completed trace

**Return Values:**
- `{ok, [{Activity, Probability}]}` - List of activities with probabilities

**Example:**
```erlang
{ok, Predictions} = predictive_mining:predict_next_activity(
    <<"case-123">>,
    [submit, review, approve]
),
% Returns: [{complete, 0.4}, {approve, 0.3}, {reject, 0.2}, {review, 0.1}]
```

#### predict_remaining_time/2

```erlang
-spec predict_remaining_time(binary(), [atom()]) -> {ok, integer()}.
```

**Description:** Predicts the remaining time for case completion based on current trace.

**Parameters:**
- `CaseId` - Binary identifier for the process case
- `Trace` - List of activity atoms representing the completed trace

**Return Values:**
- `{ok, TimeMs}` - Estimated remaining time in milliseconds

**Example:**
```erlang
{ok, RemainingMs} = predictive_mining:predict_remaining_time(
    <<"case-456">>,
    [start, process]
).
```

#### predict_outcome/2

```erlang
-spec predict_outcome(binary(), [atom()]) -> {ok, success | failure, float()}.
```

**Description:** Predicts the likely outcome (success/failure) of a case with confidence score.

**Parameters:**
- `CaseId` - Binary identifier for the process case
- `Trace` - List of activity atoms representing the completed trace

**Return Values:**
- `{ok, Outcome, Confidence}` - Outcome (`success` or `failure`) and confidence (0.0-1.0)

**Example:**
```erlang
{ok, success, Confidence} = predictive_mining:predict_outcome(
    <<"case-789">>,
    [submit, review, approve]
).
```

#### load_model/1

```erlang
-spec load_model(binary()) -> ok | {error, term()}.
```

**Description:** Loads a prediction model into memory for use in predictions.

**Parameters:**
- `ModelId` - Binary identifier for the model to load

**Return Values:**
- `ok` - Model loaded successfully
- `{error, Reason}` - Model could not be loaded

**Example:**
```erlang
ok = predictive_mining:load_model(<<"markov-model-v1">>).
```

#### unload_model/1

```erlang
-spec unload_model(binary()) -> ok.
```

**Description:** Unloads a prediction model from memory.

**Parameters:**
- `ModelId` - Binary identifier for the model to unload

**Example:**
```erlang
ok = predictive_mining:unload_model(<<"markov-model-v1">>).
```

#### list_loaded_models/0

```erlang
-spec list_loaded_models() -> [binary()].
```

**Description:** Returns a list of all currently loaded model identifiers.

**Return Values:**
- `[ModelId]` - List of binary model identifiers

**Example:**
```erlang
Models = predictive_mining:list_loaded_models(),
% Returns: [<<"model-a">>, <<"model-b">>]
```

#### set_prediction_mode/1

```erlang
-spec set_prediction_mode(realtime | batch) -> ok.
```

**Description:** Sets the prediction mode for optimization.

**Parameters:**
- `Mode` - Either `realtime` for low-latency predictions or `batch` for higher accuracy

**Example:**
```erlang
ok = predictive_mining:set_prediction_mode(realtime).
```

---

## pred_rnn

**Module:** `src/mining/pred_rnn.erl`

**Purpose:** Lightweight Recurrent Neural Network implementation in pure Erlang for sequence prediction. No external dependencies required.

### Types

```erlang
-type rnn_cell() :: #rnn_cell{}.
-type input_vector() :: [float()].
-type hidden_state() :: [float()].
-type sequence() :: [input_vector()].
```

### Records

```erlang
-record(rnn_cell, {
    input_size  :: pos_integer(),  % Number of input features
    hidden_size :: pos_integer(),  % Size of hidden layer
    output_size :: pos_integer(),  % Number of output classes
    wxh         :: [[float()]],    % Input-to-hidden weights
    whh         :: [[float()]],    % Hidden-to-hidden weights
    why         :: [[float()]],    % Hidden-to-output weights
    bh          :: [float()],      % Hidden bias
    by          :: [float()]       % Output bias
}).
```

### Exported Functions

#### new_rnn/3

```erlang
-spec new_rnn(pos_integer(), pos_integer(), pos_integer()) -> rnn_cell().
```

**Description:** Creates a new RNN with random weight initialization.

**Parameters:**
- `InputSize` - Number of input features
- `HiddenSize` - Size of the hidden layer
- `OutputSize` - Number of output classes

**Return Values:**
- `#rnn_cell{}` - Initialized RNN cell

**Example:**
```erlang
RNN = pred_rnn:new_rnn(10, 32, 5).
% Creates: 10 inputs, 32 hidden units, 5 output classes
```

#### forward_step/2

```erlang
-spec forward_step({input_vector(), hidden_state()}, rnn_cell()) ->
    {hidden_state(), [float()]}.
```

**Description:** Performs a single forward step through the RNN.

**Parameters:**
- `{Input, Hidden}` - Tuple of input vector and current hidden state
- `RNN` - The RNN cell

**Return Values:**
- `{NewHidden, Output}` - New hidden state and output probabilities

**Example:**
```erlang
Input = [1.0, 0.5, 0.0],
Hidden = [0.0, 0.0, 0.0],
{NewHidden, Output} = pred_rnn:forward_step({Input, Hidden}, RNN).
```

#### forward_sequence/2

```erlang
-spec forward_sequence(sequence(), rnn_cell()) -> {hidden_state(), [[float()]]}.
```

**Description:** Performs a forward pass through an entire sequence.

**Parameters:**
- `Sequence` - List of input vectors
- `RNN` - The RNN cell

**Return Values:**
- `{FinalHidden, Outputs}` - Final hidden state and list of output vectors

**Example:**
```erlang
Sequence = [[1.0, 0.0], [0.5, 0.5], [0.0, 1.0]],
{FinalHidden, Outputs} = pred_rnn:forward_sequence(Sequence, RNN).
```

#### predict/2

```erlang
-spec predict(sequence(), rnn_cell()) -> [float()].
```

**Description:** Predicts the next value from a sequence (returns final output probabilities).

**Parameters:**
- `Sequence` - List of input vectors
- `RNN` - The RNN cell

**Return Values:**
- `[float()]` - Output probability distribution

**Example:**
```erlang
Sequence = [[0.5], [0.8], [0.3]],
Probs = pred_rnn:predict(Sequence, RNN).
```

#### get_weights/1

```erlang
-spec get_weights(rnn_cell()) -> map().
```

**Description:** Extracts all weights from an RNN cell.

**Parameters:**
- `RNN` - The RNN cell

**Return Values:**
- `map()` - Map with keys `wxh`, `whh`, `why`, `bh`, `by`

**Example:**
```erlang
Weights = pred_rnn:get_weights(RNN).
```

#### set_weights/2

```erlang
-spec set_weights(rnn_cell(), map()) -> rnn_cell().
```

**Description:** Updates weights in an RNN cell from a map.

**Parameters:**
- `RNN` - The RNN cell
- `Weights` - Map with weight matrices

**Return Values:**
- `#rnn_cell{}` - RNN cell with updated weights

**Example:**
```erlang
UpdatedRNN = pred_rnn:set_weights(RNN, Weights).
```

### Matrix Operations

#### matrix_mult/2

```erlang
-spec matrix_mult([[float()]], [[float()]]) -> [[float()]].
```

**Description:** Performs matrix multiplication.

#### matrix_add/2

```erlang
-spec matrix_add([[float()]], [[float()]]) -> [[float()]].
```

**Description:** Performs element-wise matrix addition.

#### tanh_list/1

```erlang
-spec tanh_list([float()]) -> [float()].
```

**Description:** Applies hyperbolic tangent activation to a list.

#### softmax/1

```erlang
-spec softmax([float()]) -> [float()].
```

**Description:** Applies softmax normalization to produce a probability distribution.

---

## pred_stats

**Module:** `src/mining/pred_stats.erl`

**Purpose:** Statistical fallback models for prediction including Markov chains, Exponential Moving Average (EMA), and Linear Regression.

### Types

```erlang
-type markov_model() :: #markov_model{}.
-type ema_model() :: #ema_model{}.
-type linear_model() :: #linear_model{}.
```

### Records

```erlang
-record(markov_model, {
    transitions :: map(),      % {From, To} -> Probability
    activities  :: [atom()]    % List of unique activities
}).

-record(ema_model, {
    alpha   :: float(),        % Smoothing factor (0-1)
    values  :: [float()],      % Historical values
    last_ema :: float()        % Last computed EMA value
}).

-record(linear_model, {
    slope     :: float(),      % Line slope
    intercept :: float(),      % Y-intercept
    r_squared :: float()       % Coefficient of determination
}).
```

### Markov Chain Functions

#### fit_markov/1

```erlang
-spec fit_markov([[atom()]]) -> #markov_model{}.
```

**Description:** Fits a Markov chain model from a list of activity traces.

**Parameters:**
- `Traces` - List of traces, where each trace is a list of activity atoms

**Return Values:**
- `#markov_model{}` - Fitted Markov model

**Example:**
```erlang
Traces = [
    [submit, review, approve],
    [submit, review, reject],
    [submit, approve]
],
Model = pred_stats:fit_markov(Traces).
```

#### predict_markov/2

```erlang
-spec predict_markov([[atom()]], atom()) -> [{atom(), float()}].
```

**Description:** Predicts next activities using a Markov model. Can take either pre-computed model or raw traces.

**Parameters:**
- `Traces` - List of traces OR a fitted `#markov_model{}`
- `LastActivity` - The last activity atom to predict from

**Return Values:**
- `[{Activity, Probability}]` - List of possible next activities with probabilities

**Example:**
```erlang
% Using traces
Predictions = pred_stats:predict_markov(Traces, review),
% Returns: [{approve, 0.67}, {reject, 0.33}]

% Using fitted model
Model = pred_stats:fit_markov(Traces),
Predictions = pred_stats:predict_markov(Model, review).
```

#### get_transitions/1

```erlang
-spec get_transitions(#markov_model{}) -> map().
```

**Description:** Returns the transition probability map from a Markov model.

**Parameters:**
- `Model` - Fitted Markov model

**Return Values:**
- `map()` - Transition probabilities

### Exponential Smoothing Functions

#### fit_ema/2

```erlang
-spec fit_ema([float()], float()) -> #ema_model{}.
```

**Description:** Fits an Exponential Moving Average model to time series data.

**Parameters:**
- `Values` - List of float values
- `Alpha` - Smoothing factor (0 < Alpha <= 1). Higher values weight recent data more heavily.

**Return Values:**
- `#ema_model{}` - Fitted EMA model

**Example:**
```erlang
Durations = [100.0, 120.0, 115.0, 130.0, 125.0],
Model = pred_stats:fit_ema(Durations, 0.3).
```

#### predict_ema/2

```erlang
-spec predict_ema(#ema_model{}, integer()) -> float().
```

**Description:** Predicts future value using EMA. For EMA, the prediction is the last EMA value (constant forecast).

**Parameters:**
- `Model` - Fitted EMA model
- `StepsAhead` - Number of steps to predict ahead (ignored for EMA)

**Return Values:**
- `float()` - Predicted value

**Example:**
```erlang
Model = pred_stats:fit_ema([100.0, 120.0], 0.3),
Prediction = pred_stats:predict_ema(Model, 1).
```

### Linear Regression Functions

#### fit_linear/1

```erlang
-spec fit_linear([float()]) -> #linear_model{}.
```

**Description:** Fits a linear regression model to time series data (sequential indices).

**Parameters:**
- `Values` - List of float values

**Return Values:**
- `#linear_model{}` - Fitted linear model with slope, intercept, and R-squared

**Example:**
```erlang
Values = [10.0, 12.0, 14.0, 16.0, 18.0],
Model = pred_stats:fit_linear(Values).
% Returns model with slope ~2.0, intercept ~8.0
```

#### predict_linear/2

```erlang
-spec predict_linear(#linear_model{}, integer()) -> float().
```

**Description:** Predicts value at a given index using the linear model.

**Parameters:**
- `Model` - Fitted linear model
- `X` - Index position to predict

**Return Values:**
- `float()` - Predicted value

**Example:**
```erlang
Model = pred_stats:fit_linear([10.0, 12.0, 14.0]),
Prediction = pred_stats:predict_linear(Model, 5).
% Returns: 18.0 (continuing the trend)
```

---

## pred_training

**Module:** `src/mining/pred_training.erl`

**Purpose:** Training data collection and management for predictive models. Handles event log extraction, feature engineering, and train/test splitting.

### Types

```erlang
-type training_example() :: #training_example{}.
-type split_mode() :: random | sequential.
```

### Records

```erlang
-record(training_example, {
    input    :: [float()],  % Feature vector
    target   :: float(),    % Target value
    metadata :: map()       % Additional metadata
}).
```

### Exported Functions

#### start_link/0

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

**Description:** Starts the training data collection server.

**Example:**
```erlang
{ok, Pid} = pred_training:start_link().
```

#### stop/0

```erlang
-spec stop() -> ok.
```

**Description:** Stops the training data collection server.

#### extract_sequences/1

```erlang
-spec extract_sequences(map()) -> [[atom()]].
```

**Description:** Extracts activity sequences from XES/OCEL formatted event logs.

**Parameters:**
- `EventLog` - Map with `cases` key containing case data

**Return Values:**
- `[[atom()]]` - List of activity sequences

**Example:**
```erlang
EventLog = #{
    cases => #{
        <<"case1">> => #{
            events => [
                #{activity => submit, timestamp => 1000},
                #{activity => review, timestamp => 2000}
            ]
        }
    }
},
Sequences = pred_training:extract_sequences(EventLog).
% Returns: [[submit, review]]
```

#### extract_features/1

```erlang
-spec extract_features(map()) -> [float()].
```

**Description:** Extracts numeric feature vector from an event map.

**Parameters:**
- `Event` - Event map containing activity, timestamp, resource, duration

**Return Values:**
- `[float()]` - Feature vector

**Example:**
```erlang
Event = #{
    activity => submit,
    timestamp => 1000,
    resource => <<"user1">>,
    duration => 500
},
Features = pred_training:extract_features(Event).
% Returns: [ActivityHash, 1000.0, ResourceHash, 500.0]
```

#### build_training_set/2

```erlang
-spec build_training_set([[atom()]], pos_integer()) -> {[#training_example{}], map()}.
```

**Description:** Builds a training set using sliding window over sequences.

**Parameters:**
- `Sequences` - List of activity sequences
- `WindowSize` - Size of the sliding window

**Return Values:**
- `{Examples, Config}` - List of training examples and configuration map

**Example:**
```erlang
Sequences = [[a, b, c, d, e]],
{Examples, Config} = pred_training:build_training_set(Sequences, 3).
% Creates examples: [a,b,c]->d, [b,c,d]->e
```

#### split_train_test/3

```erlang
-spec split_train_test([#training_example{}], float(), binary()) ->
    {[#training_example{}], [#training_example{}]}.
```

**Description:** Splits examples into training and test sets.

**Parameters:**
- `Examples` - List of training examples
- `TrainRatio` - Ratio for training set (0 < TrainRatio < 1)
- `Mode` - `random` for shuffled split, `sequential` for ordered split

**Return Values:**
- `{TrainSet, TestSet}` - Tuple of training and test examples

**Example:**
```erlang
{Train, Test} = pred_training:split_train_test(Examples, 0.8, random).
{Train, Test} = pred_training:split_train_test(Examples, 0.8, sequential).
```

#### record_training_event/4

```erlang
-spec record_training_event(binary(), term(), map(), integer()) -> ok.
```

**Description:** Records a training event from telemetry (async cast).

**Parameters:**
- `WorkflowId` - Binary workflow identifier
- `EventName` - Activity/event name (atom or binary)
- `Labels` - Map of event labels (outcome, resource, duration, etc.)
- `Timestamp` - Event timestamp in milliseconds

**Example:**
```erlang
ok = pred_training:record_training_event(
    <<"workflow-123">>,
    approve,
    #{outcome => success, resource => <<"user1">>, duration => 1000},
    erlang:system_time(millisecond)
).
```

#### get_training_data/1

```erlang
-spec get_training_data(binary()) -> {ok, [#training_example{}]} | {error, not_found}.
```

**Description:** Retrieves training data for a specific workflow.

**Parameters:**
- `WorkflowId` - Binary workflow identifier

**Return Values:**
- `{ok, Examples}` - List of training examples
- `{error, not_found}` - No data for this workflow

**Example:**
```erlang
{ok, Examples} = pred_training:get_training_data(<<"workflow-123">>).
```

---

## anomaly_detection

**Module:** `src/mining/anomaly_detection.erl`

**Purpose:** Real-time and batch anomaly detection for process mining. Detects statistical outliers, rare sequences, timing anomalies, and conformance issues.

### Types

```erlang
-type anomaly_type() :: statistical_outlier | sequence_rare | timing_anomaly | conformance_mismatch.
-type severity() :: critical | warning | info.
```

### Records

```erlang
-record(anomaly, {
    id         :: binary(),         % Unique anomaly identifier
    type       :: anomaly_type(),   % Type of anomaly
    severity   :: severity(),       % Severity level
    confidence :: float(),          % Confidence score (0.0-1.0)
    case_id    :: binary() | undefined,
    task       :: atom() | undefined,
    details    :: map(),            % Additional details
    timestamp  :: integer()         % Detection timestamp
}).

-record(anomaly_result, {
    anomalies          :: [#anomaly{}],
    statistics         :: map(),
    processing_time_ms :: integer()
}).
```

### Exported Functions

#### start_link/0

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
```

**Description:** Starts the anomaly detector with default name (`<<"default_anomaly_detector">>`).

**Example:**
```erlang
{ok, Pid} = anomaly_detection:start_link().
```

#### start_link/1

```erlang
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
```

**Description:** Starts the anomaly detector with a specific name.

**Parameters:**
- `Name` - Binary name for the detector instance

**Example:**
```erlang
{ok, Pid} = anomaly_detection:start_link(<<"production_detector">>).
```

#### stop/1

```erlang
-spec stop(binary()) -> ok.
```

**Description:** Stops the named anomaly detector.

**Parameters:**
- `Name` - Binary name of the detector

#### check_real_time/2

```erlang
-spec check_real_time(binary(), map()) -> {ok, #anomaly_result{}}.
```

**Description:** Performs real-time anomaly check on a workflow receipt/event.

**Parameters:**
- `Name` - Detector name
- `Receipt` - Map containing event data (case_id, task, timestamp, duration, etc.)

**Return Values:**
- `{ok, #anomaly_result{}}` - Result containing any detected anomalies

**Example:**
```erlang
Receipt = #{
    case_id => <<"case-123">>,
    task => approve,
    timestamp => erlang:system_time(millisecond),
    start_time => erlang:system_time(millisecond) - 5000,
    duration => 5000
},
{ok, Result} = anomaly_detection:check_real_time(<<"default">>, Receipt).
```

#### detect_batch/2

```erlang
-spec detect_batch(binary(), [map()]) -> {ok, #anomaly_result{}}.
```

**Description:** Performs batch anomaly detection on event logs. Builds frequency table for rare sequence detection.

**Parameters:**
- `Name` - Detector name
- `EventLogs` - List of event log maps, each containing trace and metadata

**Return Values:**
- `{ok, #anomaly_result{}}` - Result with all detected anomalies

**Example:**
```erlang
EventLogs = [
    #{
        trace => [submit, review, approve],
        case_id => <<"case-1">>,
        timestamp => 1000,
        duration => 5000
    },
    #{
        trace => [submit, review, reject],
        case_id => <<"case-2">>,
        timestamp => 2000,
        duration => 3000
    }
],
{ok, Result} = anomaly_detection:detect_batch(<<"default">>, EventLogs).
```

#### get_thresholds/1

```erlang
-spec get_thresholds(binary()) -> map().
```

**Description:** Gets current detection thresholds for a detector.

**Parameters:**
- `Name` - Detector name

**Return Values:**
- `map()` - Threshold map with keys:
    - `statistical_outlier` - Z-score threshold (default: 2.5)
    - `sequence_rare` - Frequency ratio threshold (default: 0.95)
    - `timing_anomaly` - Duration multiplier (default: 3.0)
    - `conformance_mismatch` - Fitness threshold (default: 0.7)

**Example:**
```erlang
Thresholds = anomaly_detection:get_thresholds(<<"default">>).
```

#### set_thresholds/2

```erlang
-spec set_thresholds(binary(), map()) -> ok.
```

**Description:** Updates detection thresholds for a detector. Merges with existing thresholds.

**Parameters:**
- `Name` - Detector name
- `Thresholds` - Map of threshold values to update

**Example:**
```erlang
ok = anomaly_detection:set_thresholds(<<"default">>, #{
    timing_anomaly => 5.0,
    sequence_rare => 0.99
}).
```

### Anomaly Types

1. **timing_anomaly** - Detected when activity duration exceeds threshold
2. **sequence_rare** - Detected when trace frequency is below threshold (batch only)
3. **conformance_mismatch** - Detected when fitness score is below threshold
4. **statistical_outlier** - Statistical deviation detection (reserved for future use)

---

## Configuration Notes

### Default Thresholds

The anomaly detection module uses these default thresholds:

| Threshold | Default | Description |
|-----------|---------|-------------|
| `statistical_outlier` | 2.5 | Z-score for statistical outlier detection |
| `sequence_rare` | 0.95 | Minimum frequency ratio for normal sequences |
| `timing_anomaly` | 3.0 | Duration multiplier (in seconds) |
| `conformance_mismatch` | 0.7 | Minimum fitness score |

### Prediction Modes

- **realtime** - Optimized for low-latency predictions (< 10ms)
- **batch** - Optimized for accuracy with higher latency tolerance

### Training Data Buffer

The training module maintains an in-memory buffer with:
- Default max size: 10,000 examples per workflow
- Automatic FIFO eviction when limit is reached
- Async recording via cast for non-blocking operation

---

## Usage Examples

### Complete Workflow: Activity Prediction

```erlang
% 1. Start servers
{ok, _} = predictive_mining:start_link(),
{ok, _} = pred_training:start_link(),

% 2. Load training data
EventLog = load_event_log(),
Sequences = pred_training:extract_sequences(EventLog),

% 3. Train statistical model
MarkovModel = pred_stats:fit_markov(Sequences),

% 4. Make predictions
Trace = [submit, review],
{ok, Predictions} = predictive_mining:predict_next_activity(<<"case-1">>, Trace).

% 5. Get remaining time estimate
{ok, TimeMs} = predictive_mining:predict_remaining_time(<<"case-1">>, Trace).

% 6. Predict outcome
{ok, Outcome, Confidence} = predictive_mining:predict_outcome(<<"case-1">>, Trace).
```

### Anomaly Detection Workflow

```erlang
% Start detector
{ok, _} = anomaly_detection:start_link(<<"production">>),

% Adjust thresholds
ok = anomaly_detection:set_thresholds(<<"production">>, #{
    timing_anomaly => 10.0,
    conformance_mismatch => 0.8
}),

% Real-time check
Receipt = #{
    case_id => <<"case-123">>,
    task => process_payment,
    timestamp => erlang:system_time(millisecond),
    start_time => erlang:system_time(millisecond) - 5000,
    duration => 5000,
    fitness => 0.95
},
{ok, Result} = anomaly_detection:check_real_time(<<"production">>, Receipt),
case Result#anomaly_result.anomalies of
    [] -> io:format("No anomalies detected~n");
    Anomalies -> io:format("Found ~p anomalies~n", [length(Anomalies)])
end.
```

### RNN Training and Prediction

```erlang
% Create RNN
RNN = pred_rnn:new_rnn(10, 32, 5),

% Forward pass
Sequence = [[0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0]],
{FinalHidden, Outputs} = pred_rnn:forward_sequence(Sequence, RNN),

% Get prediction
Prediction = pred_rnn:predict(Sequence, RNN),

% Save/load weights
Weights = pred_rnn:get_weights(RNN),
RestoredRNN = pred_rnn:set_weights(RNN, Weights).
```
