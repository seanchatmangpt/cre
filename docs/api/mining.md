# Mining API Reference

This document provides comprehensive API documentation for the predictive process mining modules in CRE. The mining subsystem provides process discovery, conformance checking, anomaly detection, and predictive monitoring capabilities.

## Table of Contents

1. [Process Discovery](#process-discovery)
2. [Conformance Checking](#conformance-checking)
3. [Anomaly Detection](#anomaly-detection)
4. [Predictive Mining](#predictive-mining)

---

## Process Discovery

### alpha_algorithm - Alpha Algorithm

**Module:** `src/mining/alpha_algorithm.erl`

Implements the Alpha algorithm for discovering workflow nets (WF-nets) from event logs.

#### Type Definitions

```erlang
-type activity() :: atom().
-type trace() :: [activity()].
-type event_log() :: [trace()].
-type direct_succession() :: sets:set({activity(), activity()}).
-type causality() :: sets:set({activity(), activity()}).
-type parallel() :: sets:set({activity(), activity()}).
-type wf_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [arc()],
    initial_place => place(),
    final_place => place()
}.
```

#### Exported Functions

```erlang
-spec mine_workflow_net(Log :: event_log()) -> wf_net().
```
Mines a workflow net from an event log using the Alpha algorithm.

```erlang
-spec extract_ordering_relations(Log :: event_log()) -> ordering_relations().
```
Extracts all ordering relations from an event log.

```erlang
-spec direct_succession(Log :: event_log()) -> direct_succession().
```
Extracts direct succession relations a > b from an event log.

```erlang
-spec causality(DirectSucc :: direct_succession()) -> causality().
```
Extracts causal relations from direct succession relations.

```erlang
-spec parallel(DirectSucc :: direct_succession()) -> parallel().
```
Extracts parallel relations from direct succession relations.

---

### process_discovery - Enhanced Process Discovery

**Module:** `src/mining/process_discovery.erl`

Implements advanced process mining algorithms with heuristics for handling noise, incomplete logs, loops, and non-free-choice constructs.

#### Type Definitions

```erlang
-type event() :: {case_id(), activity(), timestamp()}.
-type event_log() :: [event()].
-type dependency() :: float().
-type dependency_matrix() :: #{{activity(), activity()} => dependency()}.
-type petri_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [arc()],
    initial_place => place(),
    final_place => place(),
    metadata => map()
}.
```

#### Exported Functions

```erlang
-spec discover(Log :: event_log()) -> petri_net().
```
Main entry point for process discovery from event logs. Uses Heuristic Miner by default.

```erlang
-spec heuristic_miner(Log :: event_log()) -> petri_net().
```
Heuristic mining algorithm that handles noise better than Alpha.

```erlang
-spec frequency_based(Log :: event_log()) -> dependency_matrix().
```
Frequency-based dependency extraction. Formula: `dep(a,b) = (|a>b| - |b>a|) / (|a>b| + |b>a| + 1)`

```erlang
-spec discover_with_noise(Log :: event_log(), Options :: map()) -> petri_net().
```
Handle incomplete/noisy event logs with explicit parameters.

**Options:**
- `dependency_threshold` - Minimum dependency strength (0.0 to 1.0, default 0.6)
- `positive_observations` - Minimum observations to consider relation (default 1)
- `best_practice_threshold` - Minimum relative frequency (default 0.8)

```erlang
-spec discover_loops(Log :: event_log()) -> petri_net().
```
Properly handle short and long loops.

```erlang
-spec discover_non_free_choice(Log :: event_log()) -> petri_net().
```
Handle non-free-choice constructs.

```erlang
-spec calculate_dependencies([trace()]) -> dependency_matrix().
```
Calculates dependency matrix from traces.

```erlang
-spec calculate_frequency_matrix([trace()]) -> frequency_matrix().
```
Calculates frequency matrix of activity successions.

```erlang
-spec detect_loops([trace()]) -> [loop_info()].
```
Detects loops in the event log.

```erlang
-spec classify_loops([trace()]) -> [loop_info()].
```
Classifies loops by type with detailed analysis.

```erlang
-spec detect_non_free_choice([trace()]) -> [non_free_choice_info()].
```
Detects non-free-choice constructs.

---

## Conformance Checking

### conformance - Conformance Checking

**Module:** `src/mining/conformance.erl`

Implements conformance checking techniques that compare an event log against a process model.

#### Type Definitions

```erlang
-type replay_result() :: #{
    status => ok | partial | failed,
    consumed => non_neg_integer(),
    produced => non_neg_integer(),
    missing => non_neg_integer(),
    remaining => non_neg_integer(),
    marking => marking()
}.

-type replay_problems() :: #{
    missing => non_neg_integer(),
    remaining => non_neg_integer(),
    consumed => non_neg_integer(),
    produced => non_neg_integer()
}.

-type align_move() :: {log_move, activity()} |
                     {model_move, transition()} |
                     {sync_move, activity()} |
                     {no_move}.

-type alignment() :: [align_move()].

-type alignment_result() :: #{
    alignment => alignment(),
    cost => non_neg_integer(),
    trace => trace(),
    fitness => float()
}.

-type conformance_report() :: #{
    fitness => float(),
    precision => float(),
    generalization => float(),
    replay_problems => replay_problems(),
    trace_count => non_neg_integer(),
    event_count => non_neg_integer(),
    model_complexity => map()
}.
```

#### Exported Functions

```erlang
-spec token_replay(Log :: event_log(), Model :: wf_net()) -> replay_problems().
```
Replays an event log through a model using token replay.

```erlang
-spec fitness_score(Log :: event_log(), Model :: wf_net()) -> float().
```
Calculates fitness score (0-1) based on token replay.

Formula: `1/2 * (1 - missing/consumed) + 1/2 * (1 - remaining/produced)`

```erlang
-spec precision_score(Log :: event_log(), Model :: wf_net()) -> float().
```
Calculates precision score (0-1) based on escaping edges.

```erlang
-spec generalization_score(Log :: event_log(), Model :: wf_net()) -> float().
```
Calculates generalization score (0-1) for the model.

```erlang
-spec conformance_report(Log :: event_log(), Model :: wf_net()) -> conformance_report().
```
Generates a complete conformance report with all metrics.

```erlang
-spec align_trace(Trace :: trace(), Model :: wf_net()) -> alignment_result().
```
Finds optimal alignment between a trace and model using A* search.

**Move Types:**
- `log_move` - Activity in trace but not enabled in model
- `model_move` - Transition enabled but not in trace
- `sync_move` - Activity matches enabled transition

```erlang
-spec replay_trace(Trace :: trace(), Model :: wf_net()) -> replay_result().
```
Replays a single trace through the model.

```erlang
-spec count_problems(replay_problems()) -> non_neg_integer().
```
Counts problems from a replay_problems map.

```erlang
-spec alignment_cost(Alignment :: alignment()) -> non_neg_integer().
```
Calculates the cost of an alignment.

```erlang
-spec alignment_fitness(Alignment :: alignment(), Trace :: trace()) -> float().
```
Calculates fitness from an alignment.

---

## Anomaly Detection

### anomaly_detection - Anomaly Detection

**Module:** `src/mining/anomaly_detection.erl`

Main gen_server for real-time and batch anomaly detection.

#### Records

```erlang
-record(anomaly, {
    id :: binary(),
    type :: statistical_outlier | sequence_rare | timing_anomaly | conformance_mismatch,
    severity :: critical | warning | info,
    confidence :: float(),
    case_id :: binary() | undefined,
    task :: atom() | undefined,
    details :: map(),
    timestamp :: integer()
}).

-record(anomaly_result, {
    anomalies :: [#anomaly{}],
    statistics :: map(),
    processing_time_ms :: integer()
}).
```

#### Exported Functions

##### Lifecycle

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
```
Start the anomaly detector with default or specific name.

```erlang
-spec stop(binary()) -> ok.
```
Stop the anomaly detector.

##### Detection

```erlang
-spec check_real_time(binary(), map()) -> {ok, #anomaly_result{}}.
```
Perform real-time anomaly check on a workflow receipt.

**Parameters:**
- `Name` - Detector name
- `Receipt` - Map containing `timestamp`, `start_time`, `task`, `case_id`

```erlang
-spec detect_batch(binary(), [map()]) -> {ok, #anomaly_result{}}.
```
Perform batch anomaly detection on event logs.

**Parameters:**
- `Name` - Detector name
- `EventLogs` - List of event maps with `trace` keys

##### Configuration

```erlang
-spec get_thresholds(binary()) -> map().
```
Get current detection thresholds.

**Default thresholds:**
```erlang
#{
    statistical_outlier => 2.5,
    sequence_rare => 0.95,
    timing_anomaly => 3.0,
    conformance_mismatch => 0.7
}
```

```erlang
-spec set_thresholds(binary(), map()) -> ok.
```
Update detection thresholds dynamically.

---

### anomaly_classifier - Anomaly Classification

**Module:** `src/mining/anomaly_classifier.erl`

Classifies anomalies by type and severity.

#### Type Definitions

```erlang
-type anomaly_type() ::
    statistical_outlier | sequence_rare | timing_anomaly |
    conformance_mismatch | resource_exhaustion | ml_detected |
    cascade_risk | unknown_pattern.

-type severity() :: critical | warning | info.

-type anomaly() :: #{
    type => anomaly_type(),
    severity => severity(),
    confidence => float(),
    details => map()
}.
```

#### Exported Functions

```erlang
-spec classify(map()) -> anomaly().
```
Classify an anomaly into type and severity based on feature map.

**Features keys:**
- `conformance` - Map with `fitness` score
- `timing` - Map with `duration` in milliseconds
- `sequence` - Map with `frequency` score
- `statistical` - Map with `zscore` value

```erlang
-spec classify_batch([map()]) -> [anomaly()].
```
Classify a batch of anomalies.

```erlang
-spec calculate_severity(anomaly_type(), map()) -> severity().
```
Calculate severity based on anomaly type and context.

**Severity mapping:**
- `conformance_mismatch` -> critical
- `resource_exhaustion` -> critical
- `cascade_risk` -> critical
- `statistical_outlier` -> warning
- `timing_anomaly` -> warning
- `ml_detected` -> warning
- `sequence_rare` -> info
- `unknown_pattern` -> info

```erlang
-spec calculate_confidence({anomaly_type(), map()}) -> float().
```
Calculate confidence score (0.0 to 1.0) for anomaly type with features.

```erlang
-spec generate_reasoning(anomaly_type(), map(), float()) -> binary().
```
Generate human-readable reasoning for anomaly classification.

---

### anomaly_statistics - Statistical Analysis

**Module:** `src/mining/anomaly_statistics.erl`

Pure functional statistical calculations for anomaly detection.

#### Type Definitions

```erlang
-type numeric() :: number().
```

#### Exported Functions

##### Basic Statistics

```erlang
-spec mean([numeric()]) -> float().
```
Calculates the arithmetic mean. Returns 0.0 for empty lists.

```erlang
-spec median([numeric()]) -> float().
```
Calculates the median (50th percentile).

```erlang
-spec stddev([numeric()]) -> float().
```
Calculates population standard deviation.

```erlang
-spec variance([numeric()]) -> float().
```
Calculates population variance.

```erlang
-spec percentile([numeric()], integer()) -> float().
```
Calculates the Nth percentile of values (0-100).

##### Advanced Statistics

```erlang
-spec zscore(numeric(), [numeric()]) -> float().
```
Calculates Z-score for a value given population statistics.

Formula: `(value - mean) / stddev`

```erlang
-spec iqr_outliers([numeric()], float()) -> {ok, [{numeric(), pos_integer()}]}.
```
Identifies outliers using IQR method.

**Parameters:**
- Values - List of numeric values
- Multiplier - IQR multiplier (typically 1.5)

```erlang
-spec moving_average([numeric()], pos_integer()) -> [float()].
```
Calculates moving average over specified window size.

```erlang
-spec correlation([numeric()], [numeric()]) -> float() | undefined.
```
Calculates correlation coefficient between two lists.

```erlang
-spec covariance([numeric()], [numeric()]) -> float() | undefined.
```
Calculates covariance between two lists.

##### Trend Analysis

```erlang
-spec trend_analysis([numeric()]) -> map().
```
Analyzes trend in time series data.

**Returns:** Map with `trend` (rising/falling/stable), `slope`, and `r_squared`

```erlang
-spec detect_trend([numeric()], float()) -> rising | falling | stable.
```
Detects trend direction with specified threshold.

##### Anomaly Scoring

```erlang
-spec outlier_score(numeric(), [numeric()]) -> float().
```
Calculates composite outlier score (0-1, higher = more anomalous).

```erlang
-spec anomaly_probability(numeric(), [numeric()]) -> float().
```
Calculates probability that value belongs to population distribution.

---

### anomaly_store - Anomaly Storage

**Module:** `src/mining/anomaly_store.erl`

gen_server that manages anomaly data storage and frequency tracking.

#### Records

```erlang
-record(anomaly_record, {
    id :: reference(),
    case_id :: binary(),
    trace :: list(),
    anomaly_type :: atom(),
    severity :: critical | warning | info,
    confidence :: float(),
    description :: binary(),
    timestamp :: integer(),
    metadata :: map()
}).
```

#### Exported Functions

##### Lifecycle

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec stop() -> ok.
```

##### Frequency Tracking

```erlang
-spec update_frequency(binary(), integer()) -> ok.
```
Update trace frequency by delta.

```erlang
-spec get_trace_frequency(binary()) -> integer() | undefined.
```
Get current trace frequency for a case ID.

##### Anomaly Storage

```erlang
-spec store_anomaly(#anomaly_record{}) -> reference().
```
Store an anomaly record.

```erlang
-spec get_anomalies(binary()) -> [#anomaly_record{}].
```
Get all anomalies for a specific case ID.

```erlang
-spec get_all_anomalies() -> [#anomaly_record{}].
```
Get all stored anomalies.

##### Alert Management

```erlang
-spec create_alert(#anomaly_record{}) -> reference().
```
Create an alert from an anomaly record.

```erlang
-spec get_alerts() -> [#anomaly_record{}].
```
Get all alerts.

---

### anomaly_alert - Alert System

**Module:** `src/mining/anomaly_alert.erl`

gen_server that manages subscriptions and notifications for anomaly alerts.

#### Records

```erlang
-record(anomaly_alert, {
    id :: reference(),
    severity :: critical | warning | info,
    anomaly_type :: atom(),
    case_id :: binary() | undefined,
    description :: binary(),
    confidence :: float(),
    timestamp :: integer()
}).

-record(subscription, {
    id :: reference(),
    subscriber :: pid(),
    filter :: map(),
    notification_mode :: sync | async
}).
```

#### Exported Functions

##### Lifecycle

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec stop() -> ok.
```

##### Subscription Management

```erlang
-spec subscribe(map()) -> reference().
```
Subscribe to anomaly alerts with filter criteria.

**Filter keys:**
- `severity` - critical | warning | info
- `anomaly_type` - Atom type filter
- `case_id` - Binary case ID filter
- `min_confidence` - Minimum confidence threshold (0.0 to 1.0)
- `notification_mode` - sync | async

```erlang
-spec unsubscribe(reference()) -> ok | {error, not_found}.
```
Unsubscribe from alerts.

```erlang
-spec list_subscriptions() -> [#subscription{}].
```
List all active subscriptions.

```erlang
-spec get_subscriber_count() -> integer().
```
Get total number of active subscribers.

##### Notification

```erlang
-spec notify(#anomaly_alert{}) -> ok.
```
Send alert to all matching subscribers.

---

## Predictive Mining

### predictive_mining - Prediction API

**Module:** `src/mining/predictive_mining.erl`

Main API for next activity and remaining time prediction.

#### Records

```erlang
-record(prediction, {
    model_id :: binary(),
    model_type :: statistical | markov | ensemble,
    prediction_type :: next_activity | remaining_time | outcome,
    result :: term(),
    confidence :: float(),
    timestamp :: integer()
}).
```

#### Exported Functions

##### Lifecycle

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec stop() -> ok.
```

##### Predictions

```erlang
-spec predict_next_activity(binary(), [atom()]) -> {ok, [{atom(), float()}]}.
```
Predict the next activity from a trace using Markov model.

**Returns:** List of {Activity, Probability} tuples sorted by probability

```erlang
-spec predict_remaining_time(binary(), [atom()]) -> {ok, integer()}.
```
Predict remaining time for case completion in milliseconds.

```erlang
-spec predict_outcome(binary(), [atom()]) -> {ok, success | failure, float()}.
```
Predict outcome (success/failure) with confidence score.

##### Model Management

```erlang
-spec load_model(binary()) -> ok | {error, term()}.
```
Load a prediction model by ID.

```erlang
-spec unload_model(binary()) -> ok.
```
Unload a prediction model.

```erlang
-spec list_loaded_models() -> [binary()].
```
List all loaded models.

##### Configuration

```erlang
-spec set_prediction_mode(realtime | batch) -> ok.
```
Set prediction mode (realtime or batch processing).

---

### pred_training - Training Data

**Module:** `src/mining/pred_training.erl`

Collects and manages training data from event logs.

#### Records

```erlang
-record(training_example, {
    input :: [float()],
    target :: float(),
    metadata :: map()
}).
```

#### Type Definitions

```erlang
-type training_example() :: #training_example{}.
-type split_mode() :: random | sequential.
```

#### Exported Functions

##### Lifecycle

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec stop() -> ok.
```

##### Data Extraction

```erlang
-spec extract_sequences(map()) -> [[atom()]].
```
Extract sequences from XES/OCEL formatted event logs.

**Input format:** Map with `cases` key containing case data

```erlang
-spec extract_features(map()) -> [float()].
```
Extract features from events.

**Features extracted:**
- Activity encoding
- Timestamp (as float)
- Resource encoding
- Duration

##### Training Set Management

```erlang
-spec build_training_set([[atom()]], pos_integer()) -> {[#training_example{}], map()}.
```
Build training set with sliding window.

**Parameters:**
- `Sequences` - List of activity sequences
- `WindowSize` - Size of the sliding window for context

```erlang
-spec split_train_test([#training_example{}], float(), binary()) ->
    {[#training_example{}], [#training_example{}]}.
```
Split data into train/test sets.

**Parameters:**
- `Examples` - Training examples
- `TrainRatio` - Fraction for training (0.0 to 1.0)
- `Mode` - `random` for shuffled split, `sequential` for ordered split

##### Event Recording

```erlang
-spec record_training_event(binary(), term(), map(), integer()) -> ok.
```
Record a training event from telemetry.

**Parameters:**
- `WorkflowId` - Workflow identifier
- `EventName` - Name of the event
- `Labels` - Event labels/metadata
- `Timestamp` - Event timestamp

```erlang
-spec get_training_data(binary()) -> {ok, [#training_example{}]} | {error, not_found}.
```
Get training data for a specific workflow ID.

---

### pred_stats - Statistical Models

**Module:** `src/mining/pred_stats.erl`

Statistical fallback models for prediction: Markov chains, exponential smoothing, and linear regression.

#### Records

```erlang
-record(markov_model, {
    transitions :: map(),
    activities :: [atom()]
}).

-record(ema_model, {
    alpha :: float(),
    values :: [float()],
    last_ema :: float()
}).

-record(linear_model, {
    slope :: float(),
    intercept :: float(),
    r_squared :: float()
}).
```

#### Type Definitions

```erlang
-type markov_model() :: #markov_model{}.
-type ema_model() :: #ema_model{}.
-type linear_model() :: #linear_model{}.
```

#### Exported Functions

##### Markov Chain

```erlang
-spec fit_markov([[atom()]]) -> #markov_model{}.
```
Fit a Markov chain model from traces.

```erlang
-spec predict_markov([[atom()]], atom()) -> [{atom(), float()}].
```
Predict next activities using Markov model. Can be called with traces (fits model first) or pre-built model.

**Returns:** List of {Activity, Probability} tuples

```erlang
-spec get_transitions(#markov_model{}) -> map().
```
Get transition matrix from model.

##### Exponential Smoothing

```erlang
-spec fit_ema([float()], float()) -> #ema_model{}.
```
Fit EMA model to values.

**Parameters:**
- `Values` - Time series values
- `Alpha` - Smoothing factor (0.0 to 1.0, higher = more weight on recent values)

```erlang
-spec predict_ema(#ema_model{}, integer()) -> float().
```
Predict using EMA. Returns the last EMA value (constant prediction).

##### Linear Regression

```erlang
-spec fit_linear([float()]) -> #linear_model{}.
```
Fit linear regression to values.

**Returns:** Model with slope, intercept, and R-squared

```erlang
-spec predict_linear(#linear_model{}, integer()) -> float().
```
Predict using linear model.

**Parameters:**
- `Model` - Fitted linear model
- `X` - X value (index) for prediction

**Formula:** `y = slope * x + intercept`

---

### pred_rnn - RNN Implementation

**Module:** `src/mining/pred_rnn.erl`

Lightweight RNN in pure Erlang for sequence prediction.

#### Records

```erlang
-record(rnn_cell, {
    input_size :: pos_integer(),
    hidden_size :: pos_integer(),
    output_size :: pos_integer(),
    wxh :: [[float()]],
    whh :: [[float()]],
    why :: [[float()]],
    bh :: [float()],
    by :: [float()]
}).
```

#### Type Definitions

```erlang
-type rnn_cell() :: #rnn_cell{}.
-type input_vector() :: [float()].
-type hidden_state() :: [float()].
-type sequence() :: [input_vector()].
```

#### Exported Functions

##### Core API

```erlang
-spec new_rnn(pos_integer(), pos_integer(), pos_integer()) -> rnn_cell().
```
Create a new RNN with random initialization.

**Parameters:**
- `InputSize` - Number of input features
- `HiddenSize` - Number of hidden units
- `OutputSize` - Number of output units

```erlang
-spec forward_step({input_vector(), hidden_state()}, rnn_cell()) ->
    {hidden_state(), [float()]}.
```
Single forward step through RNN.

```erlang
-spec forward_sequence(sequence(), rnn_cell()) -> {hidden_state(), [[float()]]}.
```
Forward pass through complete sequence.

```erlang
-spec predict(sequence(), rnn_cell()) -> [float()].
```
Predict next activity from sequence.

##### Weight Management

```erlang
-spec get_weights(rnn_cell()) -> map().
```
Get weights as map: `#{wxh => [], whh => [], why => [], bh => [], by => []}`.

```erlang
-spec set_weights(rnn_cell(), map()) -> rnn_cell().
```
Set weights from map.

##### Matrix Operations

```erlang
-spec matrix_mult([[float()]], [[float()]]) -> [[float()]].
```
Matrix multiplication.

```erlang
-spec matrix_add([[float()]], [[float()]]) -> [[float()]].
```
Element-wise matrix addition.

```erlang
-spec tanh_list([float()]) -> [float()].
```
Hyperbolic tangent activation (applied to each element).

```erlang
-spec softmax([float()]) -> [float()].
```
Softmax normalization (results sum to 1.0).

---

## Usage Examples

### Complete Process Mining Workflow

```erlang
%% 1. Discover process model from event log
Log = [[a, b, c, d], [a, c, b, d], [a, b, c, e, d]],
Model = alpha_algorithm:mine_workflow_net(Log).

%% 2. Check conformance
Fitness = conformance:fitness_score(Log, Model),
Precision = conformance:precision_score(Log, Model),

%% 3. Detect anomalies in new trace
{ok, Result} = anomaly_detection:check_real_time(
    <<"detector1">>,
    #{
        timestamp => erlang:system_time(millisecond),
        start_time => erlang:system_time(millisecond) - 5000,
        task => b,
        case_id => <<"case123">>
    }
),

%% 4. Predict next activity
{ok, Predictions} = predictive_mining:predict_next_activity(
    <<"case123">>,
    [a, b]
).
```

### Heuristic Mining with Noise Handling

```erlang
%% Create event log with noise
Log = [
    {case1, a, 1}, {case1, b, 2}, {case1, c, 3},
    {case2, a, 4}, {case2, x, 5},  %% noise
    {case2, b, 6}, {case2, c, 7},
    {case3, a, 8}, {case3, b, 9}, {case3, c, 10}
],

%% Discover with strict threshold
Net1 = process_discovery:discover_with_noise(Log, #{
    dependency_threshold => 0.9,
    positive_observations => 3
}).

%% Discover with permissive threshold
Net2 = process_discovery:discover_with_noise(Log, #{
    dependency_threshold => 0.5,
    positive_observations => 1
}).
```

### Anomaly Detection Pipeline

```erlang
%% Start services
{ok, _} = anomaly_detection:start_link(<<"detector1">>),
{ok, _} = anomaly_alert:start_link(),

%% Subscribe to critical alerts
SubRef = anomaly_alert:subscribe(#{
    severity => critical,
    min_confidence => 0.8
}),

%% Set thresholds
anomaly_detection:set_thresholds(<<"detector1">>, #{
    timing_anomaly => 2.0,      %% 2 seconds
    sequence_rare => 0.90,
    statistical_outlier => 3.0,
    conformance_mismatch => 0.7
}),

%% Process events and detect anomalies
{ok, Result} = anomaly_detection:check_real_time(
    <<"detector1">>,
    #{
        timestamp => erlang:system_time(millisecond),
        start_time => erlang:system_time(millisecond) - 3000,
        task => approve,
        case_id => <<"order456">>
    }
),

%% Classify anomalies
Anomalies = maps:get(anomalies, Result, []),
Classified = [anomaly_classifier:classify(A) || A <- Anomalies].
```

### Statistical Analysis Example

```erlang
%% Calculate statistics for duration data
Durations = [100, 150, 200, 180, 220, 190, 210, 250, 300, 280],

Mean = anomaly_statistics:mean(Durations),
StdDev = anomaly_statistics:stddev(Durations),
Median = anomaly_statistics:median(Durations),

%% Find outliers
{ok, Outliers} = anomaly_statistics:iqr_outliers(Durations, 1.5),

%% Analyze trend
TrendInfo = anomaly_statistics:trend_analysis(Durations),

%% Calculate outlier score for new value
Score = anomaly_statistics:outlier_score(500, Durations).
```

### Predictive Monitoring Example

```erlang
%% Build training data from event log
EventLog = #{
    cases => #{
        <<"case1">> => #{
            events => [
                #{activity => a, timestamp => 1, resource => <<"user1">>, duration => 100},
                #{activity => b, timestamp => 2, resource => <<"user2">>, duration => 150},
                #{activity => c, timestamp => 3, resource => <<"user1">>, duration => 200}
            ]
        },
        <<"case2">> => #{
            events => [
                #{activity => a, timestamp => 4, resource => <<"user1">>, duration => 120},
                #{activity => c, timestamp => 5, resource => <<"user3">>, duration => 180},
                #{activity => b, timestamp => 6, resource => <<"user2">>, duration => 160}
            ]
        }
    }
},

Sequences = pred_training:extract_sequences(EventLog),
{Examples, Config} = pred_training:build_training_set(Sequences, 2),

%% Make prediction
{ok, Predictions} = predictive_mining:predict_next_activity(<<"case1">>, [a, b]),
{ok, RemainingTime} = predictive_mining:predict_remaining_time(<<"case1">>, [a, b]),
{ok, Outcome, Confidence} = predictive_mining:predict_outcome(<<"case1">>, [a, b, c]).
```

---

## Summary

The mining modules provide comprehensive process mining capabilities:

1. **Process Discovery** (`alpha_algorithm`, `process_discovery`) - Discover workflow models from event logs
2. **Conformance Checking** (`conformance`) - Compare observed behavior against models
3. **Anomaly Detection** (5 modules) - Real-time and batch anomaly detection with classification and alerting
4. **Predictive Mining** (4 modules) - Next activity prediction, remaining time estimation, and outcome prediction

All modules follow OTP design principles with proper gen_server behaviors where appropriate, comprehensive type specifications, and extensive documentation.

---

**Last Updated:** 2026-02-09
