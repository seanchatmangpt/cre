# Mining Modules API Reference

This document provides comprehensive API documentation for all process mining modules in the CRE system.

## Table of Contents

1. [alpha_algorithm](#alpha_algorithm) - Alpha algorithm for process discovery
2. [anomaly_detection](#anomaly_detection) - Real-time and batch anomaly detection
3. [anomaly_alert](#anomaly_alert) - Anomaly alert subscription system
4. [anomaly_classifier](#anomaly_classifier) - Anomaly type and severity classification
5. [anomaly_statistics](#anomaly_statistics) - Statistical calculations for anomaly detection
6. [anomaly_store](#anomaly_store) - Anomaly data storage and frequency tracking
7. [conformance](#conformance) - Conformance checking for process models
8. [predictive_mining](#predictive_mining) - Next activity and outcome prediction
9. [pred_rnn](#pred_rnn) - Lightweight RNN for sequence prediction
10. [pred_stats](#pred_stats) - Statistical fallback models for prediction
11. [pred_training](#pred_training) - Training data collection and management
12. [process_discovery](#process_discovery) - Enhanced process discovery algorithms

---

## alpha_algorithm

**Module:** `alpha_algorithm`

**Description:** Implements the Alpha algorithm for discovering workflow nets (WF-nets) from event logs, as described by van der Aalst et al. (2001).

### Types

```erlang
-type activity() :: atom().
-type trace() :: [activity()].
-type event_log() :: [trace()].
-type wf_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [arc()],
    initial_place => place(),
    final_place => place()
}.
```

### Functions

#### mine_workflow_net/1

**Specification:**
```erlang
-spec mine_workflow_net(Log :: event_log()) -> wf_net().
```

**Description:** Mines a workflow net from an event log using the Alpha algorithm.

**Parameters:**
- `Log` - Event log as list of traces (lists of activity atoms)

**Return Value:** Map containing places, transitions, arcs, and initial/final places

**Usage Example:**
```erlang
Log = [[a, b, c, d], [a, c, b, d]],
Net = alpha_algorithm:mine_workflow_net(Log),
maps:get(transitions, Net).  % [a,b,c,d]
```

**Error Conditions:**
- Empty log returns valid but minimal WF-net with only source/sink places

#### extract_ordering_relations/1

**Specification:**
```erlang
-spec extract_ordering_relations(Log :: event_log()) -> ordering_relations().
```

**Description:** Extracts all ordering relations (direct succession, causality, parallel, unrelated) from an event log.

**Parameters:**
- `Log` - Event log as list of traces

**Return Value:** Map with keys: `direct_succession`, `causality`, `parallel`, `unrelated`, `activities`

**Usage Example:**
```erlang
Log = [[a, b, c], [a, c, b]],
Relations = alpha_algorithm:extract_ordering_relations(Log),
sets:to_list(maps:get(parallel, Relations)).  % [{b, c}]
```

#### event_log_to_relations/1

**Specification:**
```erlang
-spec event_log_to_relations(Log :: event_log()) -> ordering_relations().
```

**Description:** Alias for `extract_ordering_relations/1`.

---

## anomaly_detection

**Module:** `anomaly_detection`

**Description:** gen_server for real-time and batch anomaly detection in process event logs.

### Types

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

### Functions

#### start_link/0, start_link/1

**Specification:**
```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
```

**Description:** Starts the anomaly detector server.

**Parameters:**
- `Name` (optional) - Binary name for the detector instance

**Return Value:** `{ok, Pid}` on success, `{error, Reason}` on failure

**Usage Example:**
```erlang
{ok, Pid} = anomaly_detection:start_link(<<"my_detector">>).
```

#### stop/1

**Specification:**
```erlang
-spec stop(binary()) -> ok.
```

**Description:** Stops the anomaly detector.

**Parameters:**
- `Name` - Binary name of the detector

#### check_real_time/2

**Specification:**
```erlang
-spec check_real_time(binary(), map()) -> {ok, #anomaly_result{}}.
```

**Description:** Performs real-time anomaly check on a workflow receipt.

**Parameters:**
- `Name` - Detector name
- `Receipt` - Map with keys: `timestamp`, `start_time`, `task`, `case_id`

**Return Value:** `{ok, AnomalyResult}` containing detected anomalies and statistics

**Usage Example:**
```erlang
Receipt = #{
    case_id => <<"case123">>,
    task => approve,
    start_time => 1704067200000,
    timestamp => 1704067260000
},
{ok, Result} = anomaly_detection:check_real_time(<<"default">>, Receipt).
```

**Error Conditions:**
- Invalid receipt format returns empty anomaly list

#### detect_batch/2

**Specification:**
```erlang
-spec detect_batch(binary(), [map()]) -> {ok, #anomaly_result{}}.
```

**Description:** Performs batch anomaly detection on event logs.

**Parameters:**
- `Name` - Detector name
- `EventLogs` - List of event maps, each containing `trace` key

**Return Value:** `{ok, AnomalyResult}` with all detected anomalies

**Usage Example:**
```erlang
Logs = [
    #{trace => [a, b, c], case_id => <<"1">>},
    #{trace => [a, b, c], case_id => <<"2">>}
],
{ok, Result} = anomaly_detection:detect_batch(<<"default">>, Logs).
```

#### get_thresholds/1, set_thresholds/2

**Specification:**
```erlang
-spec get_thresholds(binary()) -> map().
-spec set_thresholds(binary(), map()) -> ok.
```

**Description:** Gets or sets detection thresholds.

**Parameters:**
- `Name` - Detector name
- `Thresholds` - Map with keys: `statistical_outlier`, `sequence_rare`, `timing_anomaly`, `conformance_mismatch`

**Return Value:** Current thresholds map, or `ok`

**Usage Example:**
```erlang
anomaly_detection:set_thresholds(<<"default">>, #{
    statistical_outlier => 3.0,
    timing_anomaly => 100000
}).
```

---

## anomaly_alert

**Module:** `anomaly_alert`

**Description:** gen_server that manages subscriptions and notifications for anomaly alerts.

### Types

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
```

### Functions

#### start_link/0, stop/0

**Specification:**
```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec stop() -> ok.
```

**Description:** Starts or stops the alert server.

#### subscribe/1

**Specification:**
```erlang
-spec subscribe(map()) -> reference().
```

**Description:** Subscribes the calling process to anomaly alerts.

**Parameters:**
- `Filter` - Map with optional keys: `severity`, `anomaly_type`, `case_id`, `min_confidence`, `notification_mode`

**Return Value:** Subscription ID reference

**Usage Example:**
```erlang
SubId = anomaly_alert:subscribe(#{
    severity => critical,
    min_confidence => 0.8,
    notification_mode => async
}).
```

#### unsubscribe/1

**Specification:**
```erlang
-spec unsubscribe(reference()) -> ok | {error, not_found}.
```

**Description:** Unsubscribes from alerts.

**Parameters:**
- `SubscriptionId` - Reference returned by `subscribe/1`

**Return Value:** `ok` or `{error, not_found}`

**Error Conditions:**
- `not_found` - Subscription ID does not exist

#### notify/1

**Specification:**
```erlang
-spec notify(#anomaly_alert{}) -> ok.
```

**Description:** Sends an alert to all matching subscribers.

**Parameters:**
- `Alert` - Anomaly alert record

**Usage Example:**
```erlang
anomaly_alert:notify(#anomaly_alert{
    id = make_ref(),
    severity = critical,
    anomaly_type = conformance_mismatch,
    case_id = <<"case123">>,
    description = <<"Fitness below threshold">>,
    confidence = 0.95,
    timestamp = erlang:system_time(millisecond)
}).
```

#### list_subscriptions/0

**Specification:**
```erlang
-spec list_subscriptions() -> [#subscription{}].
```

**Description:** Lists all active subscriptions.

#### get_subscriber_count/0

**Specification:**
```erlang
-spec get_subscriber_count() -> integer().
```

**Description:** Returns the number of active subscribers.

---

## anomaly_classifier

**Module:** `anomaly_classifier`

**Description:** Classifies anomalies by type and severity.

### Types

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

### Functions

#### classify/1

**Specification:**
```erlang
-spec classify(map()) -> anomaly().
```

**Description:** Classifies an anomaly into type and severity.

**Parameters:**
- `Features` - Map containing: `conformance`, `timing`, `sequence`, `statistical` keys

**Return Value:** Anomaly map with type, severity, confidence, and details

**Usage Example:**
```erlang
Anomaly = anomaly_classifier:classify(#{
    conformance => #{fitness => 0.5},
    timing => #{duration => 2000000},
    sequence => #{frequency => 0.005},
    statistical => #{zscore => 3.5}
}).
```

#### classify_batch/1

**Specification:**
```erlang
-spec classify_batch([map()]) -> [anomaly()].
```

**Description:** Classifies a batch of anomalies.

**Parameters:**
- `FeaturesList` - List of feature maps

**Return Value:** List of anomaly maps

#### calculate_severity/2

**Specification:**
```erlang
-spec calculate_severity(anomaly_type(), map()) -> severity().
```

**Description:** Calculates severity based on anomaly type.

**Parameters:**
- `Type` - Anomaly type atom
- `Features` - Feature map (not used in current implementation)

**Return Value:** Severity level: `critical`, `warning`, or `info`

**Severity Mapping:**
- `conformance_mismatch` -> critical
- `resource_exhaustion` -> critical
- `cascade_risk` -> critical
- `statistical_outlier` -> warning
- `timing_anomaly` -> warning
- `ml_detected` -> warning
- `sequence_rare` -> info
- `unknown_pattern` -> info

#### calculate_confidence/1

**Specification:**
```erlang
-spec calculate_confidence({anomaly_type(), map()}) -> float().
```

**Description:** Calculates confidence score (0.0 to 1.0).

**Parameters:**
- `{Type, Features}` - Tuple of anomaly type and feature map

**Return Value:** Float between 0.0 and 1.0

**Usage Example:**
```erlang
Confidence = anomaly_classifier:calculate_confidence(
    {statistical_outlier, #{zscore => 3.0, indicator_count => 5}}
).  % 0.75
```

#### generate_reasoning/3

**Specification:**
```erlang
-spec generate_reasoning(anomaly_type(), map(), float()) -> binary().
```

**Description:** Generates human-readable reasoning for the anomaly.

**Parameters:**
- `Type` - Anomaly type
- `Features` - Feature map
- `Confidence` - Confidence score

**Return Value:** Binary description string

**Usage Example:**
```erlang
Reason = anomaly_classifier:generate_reasoning(
    statistical_outlier,
    #{statistical => #{zscore => 3.5}},
    0.95
).  % <<"Statistical outlier detected (Z-score: 3.50) (confidence: 0.95)">>
```

---

## anomaly_statistics

**Module:** `anomaly_statistics`

**Description:** Pure functional statistical calculations for anomaly detection.

### Types

```erlang
-type numeric() :: number().
```

### Functions

#### Basic Statistics

##### mean/1

**Specification:**
```erlang
-spec mean([numeric()]) -> float().
```

**Description:** Calculates arithmetic mean. Returns `0.0` for empty lists.

**Usage Example:**
```erlang
anomaly_statistics:mean([1, 2, 3, 4, 5]).  % 3.0
```

##### median/1

**Specification:**
```erlang
-spec median([numeric()]) -> float().
```

**Description:** Calculates median (50th percentile).

**Usage Example:**
```erlang
anomaly_statistics:median([1, 2, 3, 4, 5]).  % 3.0
anomaly_statistics:median([1, 2, 3, 4]).     % 2.5
```

##### stddev/1

**Specification:**
```erlang
-spec stddev([numeric()]) -> float().
```

**Description:** Calculates population standard deviation.

**Usage Example:**
```erlang
anomaly_statistics:stddev([1, 2, 3]).  % ~0.816
```

##### variance/1

**Specification:**
```erlang
-spec variance([numeric()]) -> float().
```

**Description:** Calculates population variance.

##### percentile/2

**Specification:**
```erlang
-spec percentile([numeric()], integer()) -> float().
```

**Description:** Calculates Nth percentile (0-100).

**Parameters:**
- `Values` - List of numeric values
- `Percentile` - Integer between 0 and 100

**Usage Example:**
```erlang
anomaly_statistics:percentile([1,2,3,4,5,6,7,8,9,10], 50).  % 5
```

#### Outlier Detection

##### zscore/2

**Specification:**
```erlang
-spec zscore(numeric(), [numeric()]) -> float().
```

**Description:** Calculates Z-score for a value given population statistics.

**Parameters:**
- `Value` - Value to score
- `Population` - Population data

**Return Value:** Z-score (number of standard deviations from mean)

**Usage Example:**
```erlang
anomaly_statistics:zscore(7, [3, 4, 5, 6, 7]).  % 1.0
```

##### iqr_outliers/2

**Specification:**
```erlang
-spec iqr_outliers([numeric()], float()) -> {ok, [{numeric(), pos_integer()}]}.
```

**Description:** Identifies outliers using IQR (Interquartile Range) method.

**Parameters:**
- `Values` - Numeric values
- `Multiplier` - IQR multiplier (typically 1.5)

**Return Value:** `{ok, [{Value, Index}]}` list of outliers and their positions

**Usage Example:**
```erlang
{ok, Outliers} = anomaly_statistics:iqr_outliers([1,1,1,1,100], 1.5).
```

##### outlier_score/2

**Specification:**
```erlang
-spec outlier_score(numeric(), [numeric()]) -> float().
```

**Description:** Calculates composite outlier score (0-1, higher = more anomalous).

**Parameters:**
- `Value` - Value to score
- `Population` - Population data

**Return Value:** Float between 0.0 and 1.0

##### anomaly_probability/2

**Specification:**
```erlang
-spec anomaly_probability(numeric(), [numeric()]) -> float().
```

**Description:** Calculates probability that value belongs to population distribution.

#### Time Series Analysis

##### moving_average/2

**Specification:**
```erlang
-spec moving_average([numeric()], pos_integer()) -> [float()].
```

**Description:** Calculates moving average over specified window size.

**Parameters:**
- `Values` - Numeric values
- `WindowSize` - Window size (positive integer)

**Usage Example:**
```erlang
anomaly_statistics:moving_average([1,2,3,4], 2).  % [1.5, 2.5, 3.5]
```

##### trend_analysis/1

**Specification:**
```erlang
-spec trend_analysis([numeric()]) -> map().
```

**Description:** Analyzes trend in time series data.

**Return Value:** Map with keys: `trend` (rising/falling/stable), `slope`, `r_squared`

**Usage Example:**
```erlang
anomaly_statistics:trend_analysis([1,2,3,4,5]).
% #{trend => rising, slope => 1.0, r_squared => 1.0}
```

##### detect_trend/2

**Specification:**
```erlang
-spec detect_trend([numeric()], float()) -> rising | falling | stable.
```

**Description:** Detects trend direction with threshold.

**Parameters:**
- `Values` - Numeric values
- `Threshold` - Minimum slope to consider as trend

#### Correlation

##### correlation/2, covariance/2

**Specification:**
```erlang
-spec correlation([numeric()], [numeric()]) -> float() | undefined.
-spec covariance([numeric()], [numeric()]) -> float() | undefined.
```

**Description:** Calculates correlation coefficient or covariance between two lists.

**Return Value:** Float between -1 and 1 for correlation, or `undefined` for invalid inputs

**Usage Example:**
```erlang
anomaly_statistics:correlation([1,2,3], [1,2,3]).  % 1.0
anomaly_statistics:correlation([1,2,3], [3,2,1]).  % -1.0
```

---

## anomaly_store

**Module:** `anomaly_store`

**Description:** gen_server that manages anomaly data storage and frequency tracking.

### Types

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

### Functions

#### start_link/0, stop/0

**Specification:**
```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec stop() -> ok.
```

**Description:** Starts or stops the store server.

#### Frequency Tracking

##### update_frequency/2

**Specification:**
```erlang
-spec update_frequency(binary(), integer()) -> ok.
```

**Description:** Updates the frequency counter for a case ID.

**Parameters:**
- `CaseId` - Binary case identifier
- `Delta` - Integer change (positive or negative)

**Usage Example:**
```erlang
anomaly_store:update_frequency(<<"case123">>, 1).
```

##### get_trace_frequency/1

**Specification:**
```erlang
-spec get_trace_frequency(binary()) -> integer() | undefined.
```

**Description:** Gets the frequency count for a case ID.

**Parameters:**
- `CaseId` - Binary case identifier

**Return Value:** Integer count or `undefined` if not found

#### Anomaly Storage

##### store_anomaly/1

**Specification:**
```erlang
-spec store_anomaly(#anomaly_record{}) -> reference().
```

**Description:** Stores an anomaly record.

**Parameters:**
- `Record` - Anomaly record

**Return Value:** Reference ID of stored record

**Usage Example:**
```erlang
anomaly_store:store_anomaly(#anomaly_record{
    id = make_ref(),
    case_id = <<"case123">>,
    trace = [a, b, c],
    anomaly_type = sequence_rare,
    severity = info,
    confidence = 0.8,
    description => <<"Rare sequence">>,
    timestamp = erlang:system_time(millisecond),
    metadata => #{}
}).
```

##### get_anomalies/1

**Specification:**
```erlang
-spec get_anomalies(binary()) -> [#anomaly_record{}].
```

**Description:** Gets all anomalies for a case ID.

**Parameters:**
- `CaseId` - Binary case identifier

**Return Value:** List of anomaly records

##### get_all_anomalies/0

**Specification:**
```erlang
-spec get_all_anomalies() -> [#anomaly_record{}].
```

**Description:** Gets all stored anomalies.

#### Alert Management

##### create_alert/1

**Specification:**
```erlang
-spec create_alert(#anomaly_record{}) -> reference().
```

**Description:** Creates an alert from an anomaly record.

**Parameters:**
- `Record` - Anomaly record to convert to alert

**Return Value:** Reference ID of alert

##### get_alerts/0

**Specification:**
```erlang
-spec get_alerts() -> [#anomaly_record{}].
```

**Description:** Gets all active alerts.

---

## conformance

**Module:** `conformance`

**Description:** Conformance checking for process mining - compares event logs against process models.

### Types

```erlang
-type marking() :: #{place() => non_neg_integer()}.

-type wf_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [arc()],
    initial_place => place(),
    final_place => place()
}.

-type replay_result() :: #{
    status => ok | partial | failed,
    consumed => non_neg_integer(),
    produced => non_neg_integer(),
    missing => non_neg_integer(),
    remaining => non_neg_integer(),
    marking => marking()
}.

-type alignment() :: [align_move()].

-type align_move() :: {log_move, activity()} |
                     {model_move, transition()} |
                     {sync_move, activity()} |
                     {no_move}.

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

### Functions

#### Token Replay

##### token_replay/2

**Specification:**
```erlang
-spec token_replay(Log :: event_log(), Model :: wf_net()) -> replay_problems().
```

**Description:** Replays an event log through a model using token replay.

**Parameters:**
- `Log` - Event log as list of traces
- `Model` - WF-net model from `alpha_algorithm:mine_workflow_net/1`

**Return Value:** Map with keys: `missing`, `remaining`, `consumed`, `produced`

**Usage Example:**
```erlang
Log = [[a, b, c]],
Model = alpha_algorithm:mine_workflow_net(Log),
Problems = conformance:token_replay(Log, Model).
% #{missing => 0, remaining => 0, consumed => 3, produced => 3}
```

##### replay_trace/2

**Specification:**
```erlang
-spec replay_trace(Trace :: trace(), Model :: wf_net()) -> replay_result().
```

**Description:** Replays a single trace through the model.

**Parameters:**
- `Trace` - Single trace (list of activities)
- `Model` - WF-net model

**Return Value:** Replay result with status and counts

#### Conformance Metrics

##### fitness_score/2

**Specification:**
```erlang
-spec fitness_score(Log :: event_log(), Model :: wf_net()) -> float().
```

**Description:** Calculates fitness score (0-1) based on token replay.

**Formula:** `1/2 * (1 - missing/consumed) + 1/2 * (1 - remaining/produced)`

**Return Value:** Float between 0.0 (no fit) and 1.0 (perfect fit)

**Usage Example:**
```erlang
Log = [[a, b, c]],
Model = alpha_algorithm:mine_workflow_net(Log),
Fitness = conformance:fitness_score(Log, Model).  % 1.0
```

##### precision_score/2

**Specification:**
```erlang
-spec precision_score(Log :: event_log(), Model :: wf_net()) -> float().
```

**Description:** Calculates precision score using escaping edges metric.

**Return Value:** Float between 0.0 and 1.0

##### generalization_score/2

**Specification:**
```erlang
-spec generalization_score(Log :: event_log(), Model :: wf_net()) -> float().
```

**Description:** Calculates generalization score (measures overfitting).

**Return Value:** Float between 0.0 (overfitted) and 1.0 (well-generalized)

##### conformance_report/2

**Specification:**
```erlang
-spec conformance_report(Log :: event_log(), Model :: wf_net()) -> conformance_report().
```

**Description:** Generates a complete conformance report.

**Return Value:** Map with all conformance metrics and diagnostic information

**Usage Example:**
```erlang
Report = conformance:conformance_report(Log, Model),
maps:get(fitness, Report).
```

#### Alignment

##### align_trace/2

**Specification:**
```erlang
-spec align_trace(Trace :: trace(), Model :: wf_net()) -> alignment_result().
```

**Description:** Finds optimal alignment between trace and model using A* search.

**Parameters:**
- `Trace` - Trace to align
- `Model` - WF-net model

**Return Value:** Map with `alignment`, `cost`, `trace`, `fitness` keys

**Usage Example:**
```erlang
Result = conformance:align_trace([a, b, c], Model),
maps:get(alignment, Result).
% [{sync_move, a}, {sync_move, b}, {sync_move, c}]
```

**Alignment Move Types:**
- `{sync_move, Activity}` - Activity matches enabled transition
- `{log_move, Activity}` - Activity in trace but not enabled
- `{model_move, Transition}` - Transition enabled but not in trace

#### Analysis Utilities

##### count_problems/1

**Specification:**
```erlang
-spec count_problems(replay_problems()) -> non_neg_integer().
```

**Description:** Counts total deviation count (missing + remaining).

**Usage Example:**
```erlang
conformance:count_problems(#{missing => 2, remaining => 1, consumed => 10, produced => 10}).
% 3
```

##### alignment_cost/1

**Specification:**
```erlang
-spec alignment_cost(alignment()) -> non_neg_integer().
```

**Description:** Calculates the cost of an alignment.

**Costs:** Log moves = 1, model moves = 1, sync moves = 0

**Usage Example:**
```erlang
conformance:alignment_cost([{sync_move, a}, {log_move, b}, {model_move, c}]).
% 2
```

---

## predictive_mining

**Module:** `predictive_mining`

**Description:** Main API for next activity and remaining time prediction.

### Types

```erlang
-type prediction_mode() :: realtime | batch.
-type model_type() :: statistical | markov | ensemble.
-type prediction_type() :: next_activity | remaining_time | outcome.

-record(prediction, {
    model_id :: binary(),
    model_type :: model_type(),
    prediction_type :: prediction_type(),
    result :: term(),
    confidence :: float(),
    timestamp :: integer()
}).
```

### Functions

#### Server Control

##### start_link/0, stop/0

**Specification:**
```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec stop() -> ok.
```

**Description:** Starts or stops the predictive mining server.

#### Predictions

##### predict_next_activity/2

**Specification:**
```erlang
-spec predict_next_activity(binary(), [atom()]) -> {ok, [{atom(), float()}]}.
```

**Description:** Predicts the next activity from a trace.

**Parameters:**
- `CaseId` - Binary case identifier
- `Trace` - List of activity atoms

**Return Value:** `{ok, [{Activity, Probability}]}` list of predictions

**Usage Example:**
```erlang
{ok, Predictions} = predictive_mining:predict_next_activity(
    <<"case123">>,
    [a, b, c]
).
% [{complete, 0.4}, {approve, 0.3}, {reject, 0.2}, {review, 0.1}]
```

##### predict_remaining_time/2

**Specification:**
```erlang
-spec predict_remaining_time(binary(), [atom()]) -> {ok, integer()}.
```

**Description:** Predicts remaining time for case completion (in milliseconds).

**Parameters:**
- `CaseId` - Binary case identifier
- `Trace` - List of activity atoms

**Return Value:** `{ok, Milliseconds}` estimated remaining time

**Usage Example:**
```erlang
{ok, Time} = predictive_mining:predict_remaining_time(<<"case123">>, [a, b, c]).
% {ok, 60000}  % 1 minute
```

##### predict_outcome/2

**Specification:**
```erlang
-spec predict_outcome(binary(), [atom()]) -> {ok, success | failure, float()}.
```

**Description:** Predicts outcome (success/failure) with confidence.

**Parameters:**
- `CaseId` - Binary case identifier
- `Trace` - List of activity atoms

**Return Value:** `{ok, Outcome, Confidence}`

**Usage Example:**
```erlang
{ok, Outcome, Confidence} = predictive_mining:predict_outcome(
    <<"case123">>,
    [a, b, c]
).
% {ok, success, 0.8}
```

#### Model Management

##### load_model/1, unload_model/1

**Specification:**
```erlang
-spec load_model(binary()) -> ok | {error, term()}.
-spec unload_model(binary()) -> ok.
```

**Description:** Loads or unloads a prediction model.

**Parameters:**
- `ModelId` - Binary model identifier

**Return Value:** `ok` or error tuple

##### list_loaded_models/0

**Specification:**
```erlang
-spec list_loaded_models() -> [binary()].
```

**Description:** Lists all loaded model IDs.

##### set_prediction_mode/1

**Specification:**
```erlang
-spec set_prediction_mode(realtime | batch) -> ok.
```

**Description:** Sets the prediction mode.

**Parameters:**
- `Mode` - `realtime` or `batch`

---

## pred_rnn

**Module:** `pred_rnn`

**Description:** Lightweight recurrent neural network for sequence prediction in pure Erlang.

### Types

```erlang
-record(rnn_cell, {
    input_size :: pos_integer(),
    hidden_size :: pos_integer(),
    output_size :: pos_integer(),
    wxh :: [[float()]],  %% Input-to-hidden weights
    whh :: [[float()]],  %% Hidden-to-hidden weights
    why :: [[float()]],  %% Hidden-to-output weights
    bh :: [float()],     %% Hidden bias
    by :: [float()]      %% Output bias
}).

-type input_vector() :: [float()].
-type hidden_state() :: [float()].
-type sequence() :: [input_vector()].
```

### Functions

#### Network Creation

##### new_rnn/3

**Specification:**
```erlang
-spec new_rnn(pos_integer(), pos_integer(), pos_integer()) -> rnn_cell().
```

**Description:** Creates a new RNN with random initialization.

**Parameters:**
- `InputSize` - Number of input features
- `HiddenSize` - Number of hidden units
- `OutputSize` - Number of output units

**Return Value:** RNN cell record

**Usage Example:**
```erlang
RNN = pred_rnn:new_rnn(4, 10, 3).
```

#### Forward Pass

##### forward_step/2

**Specification:**
```erlang
-spec forward_step({input_vector(), hidden_state()}, rnn_cell()) ->
    {hidden_state(), [float()]}.
```

**Description:** Performs single forward step.

**Parameters:**
- `{Input, Hidden}` - Tuple of input vector and previous hidden state

**Return Value:** `{NewHiddenState, Output}`

**Usage Example:**
```erlang
Input = [1.0, 0.5, 0.0, 1.0],
Hidden = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0],
{NewHidden, Output} = pred_rnn:forward_step({Input, Hidden}, RNN).
```

##### forward_sequence/2

**Specification:**
```erlang
-spec forward_sequence(sequence(), rnn_cell()) -> {hidden_state(), [[float()]]}.
```

**Description:** Performs forward pass through entire sequence.

**Parameters:**
- `Sequence` - List of input vectors

**Return Value:** `{FinalHiddenState, Outputs}`

**Usage Example:**
```erlang
Sequence = [[1.0, 0.0], [0.5, 0.5], [0.0, 1.0]],
{FinalHidden, Outputs} = pred_rnn:forward_sequence(Sequence, RNN).
```

##### predict/2

**Specification:**
```erlang
-spec predict(sequence(), rnn_cell()) -> [float()].
```

**Description:** Predicts next output from sequence.

**Parameters:**
- `Sequence` - Input sequence

**Return Value:** Output probability distribution

**Usage Example:**
```erlang
Probs = pred_rnn:predict([[0.5, 0.8]], RNN).
% [0.3, 0.5, 0.2]
```

#### Weight Management

##### get_weights/1, set_weights/2

**Specification:**
```erlang
-spec get_weights(rnn_cell()) -> map().
-spec set_weights(rnn_cell(), map()) -> rnn_cell().
```

**Description:** Gets or sets network weights.

**Usage Example:**
```erlang
Weights = pred_rnn:get_weights(RNN),
UpdatedRNN = pred_rnn:set_weights(RNN, Weights#{wxh => NewWxh}).
```

#### Matrix Operations

##### matrix_mult/2

**Specification:**
```erlang
-spec matrix_mult([[float()]], [[float()]]) -> [[float()]].
```

**Description:** Matrix multiplication.

##### matrix_add/2

**Specification:**
```erlang
-spec matrix_add([[float()]], [[float()]]) -> [[float()]].
```

**Description:** Element-wise matrix addition.

##### tanh_list/1

**Specification:**
```erlang
-spec tanh_list([float()]) -> [float()].
```

**Description:** Hyperbolic tangent activation.

##### softmax/1

**Specification:**
```erlang
-spec softmax([float()]) -> [float()].
```

**Description:** Softmax normalization (sums to 1.0).

**Usage Example:**
```erlang
pred_rnn:softmax([1.0, 2.0, 3.0]).
% [0.09, 0.24, 0.67]
```

---

## pred_stats

**Module:** `pred_stats`

**Description:** Statistical fallback models for prediction (Markov chains, exponential smoothing, linear regression).

### Types

```erlang
-record(markov_model, {
    transitions :: map(),  %% {From, To} -> Probability
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

### Functions

#### Markov Chain

##### fit_markov/1

**Specification:**
```erlang
-spec fit_markov([[atom()]]) -> #markov_model{}.
```

**Description:** Fits a Markov chain model from traces.

**Parameters:**
- `Traces` - List of traces (lists of activity atoms)

**Return Value:** Markov model with transition probabilities

**Usage Example:**
```erlang
Traces = [[a, b, c], [a, c, b], [a, b, c]],
Model = pred_stats:fit_markov(Traces).
```

##### predict_markov/2

**Specification:**
```erlang
-spec predict_markov([[atom()]], atom()) -> [{atom(), float()}].
-spec predict_markov(#markov_model{}, atom()) -> [{atom(), float()}].
```

**Description:** Predicts next activities using Markov model.

**Parameters:**
- `Traces` - Training traces, OR
- `Model` - Pre-fitted Markov model
- `LastActivity` - Last activity atom

**Return Value:** List of `{Activity, Probability}` tuples

**Usage Example:**
```erlang
Predictions = pred_stats:predict_markov(Traces, a).
% [{b, 0.67}, {c, 0.33}]
```

##### get_transitions/1

**Specification:**
```erlang
-spec get_transitions(#markov_model{}) -> map().
```

**Description:** Gets transition matrix from model.

#### Exponential Smoothing

##### fit_ema/2

**Specification:**
```erlang
-spec fit_ema([float()], float()) -> #ema_model{}.
```

**Description:** Fits EMA model to values.

**Parameters:**
- `Values` - List of float values
- `Alpha` - Smoothing factor (0 < Alpha <= 1)

**Return Value:** EMA model

**Usage Example:**
```erlang
Model = pred_stats:fit_ema([1.0, 2.0, 3.0, 4.0], 0.5).
```

##### predict_ema/2

**Specification:**
```erlang
-spec predict_ema(#ema_model{}, integer()) -> float().
```

**Description:** Predicts using EMA.

**Parameters:**
- `Model` - EMA model
- `StepsAhead` - Number of steps ahead (not used, returns last EMA)

**Return Value:** Predicted value

#### Linear Regression

##### fit_linear/1

**Specification:**
```erlang
-spec fit_linear([float()]) -> #linear_model{}.
```

**Description:** Fits linear regression to values.

**Parameters:**
- `Values` - List of float values

**Return Value:** Linear model with slope, intercept, and R-squared

**Usage Example:**
```erlang
Model = pred_stats:fit_linear([1.0, 2.0, 3.0, 4.0]),
Model#linear_model.slope.  % 1.0
```

##### predict_linear/2

**Specification:**
```erlang
-spec predict_linear(#linear_model{}, integer()) -> float().
```

**Description:** Predicts using linear model.

**Parameters:**
- `Model` - Linear model
- `X` - X value to predict

**Return Value:** Predicted Y value

**Usage Example:**
```erlang
pred_stats:predict_linear(Model, 5).  % 5.0
```

---

## pred_training

**Module:** `pred_training`

**Description:** Training data collection and management for predictive mining.

### Types

```erlang
-record(training_example, {
    input :: [float()],
    target :: float(),
    metadata :: map()
}).

-type split_mode() :: random | sequential.
```

### Functions

#### Server Control

##### start_link/0, stop/0

**Specification:**
```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec stop() -> ok.
```

**Description:** Starts or stops the training data server.

#### Data Extraction

##### extract_sequences/1

**Specification:**
```erlang
-spec extract_sequences(map()) -> [[atom()]].
```

**Description:** Extracts sequences from XES/OCEL-style event logs.

**Parameters:**
- `EventLog` - Map with `cases` key containing case data

**Return Value:** List of traces (activity sequences)

**Usage Example:**
```erlang
EventLog = #{
    cases => #{
        <<"case1">> => #{
            events => [
                #{activity => a, timestamp => 1},
                #{activity => b, timestamp => 2}
            ]
        }
    }
},
Sequences = pred_training:extract_sequences(EventLog).
% [[a, b]]
```

##### extract_features/1

**Specification:**
```erlang
-spec extract_features(map()) -> [float()].
```

**Description:** Extracts features from an event.

**Parameters:**
- `Event` - Map with `activity`, `timestamp`, `resource`, `duration` keys

**Return Value:** Feature vector (list of floats)

**Feature Encoding:**
- Activity -> `phash2(Activity)`
- Timestamp -> `float(Timestamp)`
- Resource -> `phash2(Resource)`
- Duration -> `float(Duration)`

#### Training Set Construction

##### build_training_set/2

**Specification:**
```erlang
-spec build_training_set([[atom()]], pos_integer()) -> {[#training_example{}], map()}.
```

**Description:** Builds training set with sliding window.

**Parameters:**
- `Sequences` - List of activity sequences
- `WindowSize` - Window size for input features

**Return Value:** `{Examples, Config}` where Examples is list of training examples

**Usage Example:**
```erlang
Sequences = [[a, b, c, d], [a, b, c, e]],
{Examples, Config} = pred_training:build_training_set(Sequences, 2).
```

##### split_train_test/3

**Specification:**
```erlang
-spec split_train_test([#training_example{}], float(), binary()) ->
    {[#training_example{}], [#training_example{}]}.
```

**Description:** Splits data into train/test sets.

**Parameters:**
- `Examples` - Training examples
- `TrainRatio` - Fraction for training (0 < TrainRatio < 1)
- `Mode` - `random` or `sequential`

**Return Value:** `{TrainSet, TestSet}`

**Usage Example:**
```erlang
{Train, Test} = pred_training:split_train_test(Examples, 0.8, random).
```

#### Event Recording

##### record_training_event/4

**Specification:**
```erlang
-spec record_training_event(binary(), term(), map(), integer()) -> ok.
```

**Description:** Records a training event from telemetry.

**Parameters:**
- `WorkflowId` - Binary workflow identifier
- `EventName` - Event name (atom or binary)
- `Labels` - Map of event labels (resource, duration, outcome, etc.)
- `Timestamp` - Integer timestamp

**Usage Example:**
```erlang
pred_training:record_training_event(
    <<"workflow123">>,
    approve,
    #{resource => <<"user1">>, duration => 5000, outcome => success},
    1704067200000
).
```

##### get_training_data/1

**Specification:**
```erlang
-spec get_training_data(binary()) -> {ok, [#training_example{}]} | {error, not_found}.
```

**Description:** Gets training data for a workflow.

**Parameters:**
- `WorkflowId` - Binary workflow identifier

**Return Value:** `{ok, Examples}` or `{error, not_found}`

**Error Conditions:**
- `not_found` - No training data exists for workflow

---

## process_discovery

**Module:** `process_discovery`

**Description:** Enhanced process discovery algorithms extending Alpha algorithm.

### Types

```erlang
-type event() :: {case_id(), activity(), timestamp()}.
-type event_log() :: [event()].
-type dependency_matrix() :: #{{activity(), activity()} => dependency()}.
-type frequency_matrix() :: #{{activity(), activity()} => frequency()}.

-type loop_type() :: short_loop_1 | short_loop_2 | long_loop.

-type loop_info() :: #{
    type => loop_type(),
    activities => [activity()],
    frequency => frequency()
}.

-type petri_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [arc()],
    initial_place => place(),
    final_place => place(),
    metadata => map()
}.
```

### Functions

#### Main Discovery

##### discover/1

**Specification:**
```erlang
-spec discover(event_log()) -> petri_net().
```

**Description:** Main entry point for process discovery. Uses Heuristic Miner by default.

**Parameters:**
- `Log` - Event log as list of `{CaseId, Activity, Timestamp}` tuples

**Return Value:** Petri net map

**Usage Example:**
```erlang
Log = [
    {case1, a, 1}, {case1, b, 2}, {case1, c, 3},
    {case2, a, 4}, {case2, c, 5}, {case2, b, 6}
],
Net = process_discovery:discover(Log).
```

##### heuristic_miner/1

**Specification:**
```erlang
-spec heuristic_miner(event_log()) -> petri_net().
```

**Description:** Heuristic mining algorithm with better noise tolerance than Alpha.

**Key Features:**
- Frequency-based instead of binary relations
- Configurable dependency thresholds (default 0.7)
- Better handling of infrequent paths

**Usage Example:**
```erlang
Net = process_discovery:heuristic_miner(Log).
```

##### frequency_based/1

**Specification:**
```erlang
-spec frequency_based(event_log()) -> dependency_matrix().
```

**Description:** Calculates frequency-based dependency matrix.

**Formula:** `dep(a,b) = (|a>b| - |b>a|) / (|a>b| + |b>a| + 1)`

**Return Value:** Dependency matrix with values from -1 to 1

#### Noise Handling

##### discover_with_noise/2

**Specification:**
```erlang
-spec discover_with_noise(event_log(), map()) -> petri_net().
```

**Description:** Handles incomplete/noisy event logs with explicit parameters.

**Options Map Keys:**
- `dependency_threshold` - Minimum dependency strength (default: 0.6)
- `positive_observations` - Minimum observations for relation (default: 1)
- `best_practice_threshold` - Minimum relative frequency (default: 0.8)

**Usage Example:**
```erlang
Net = process_discovery:discover_with_noise(Log, #{
    dependency_threshold => 0.9,
    positive_observations => 3
}).
```

#### Loop Discovery

##### discover_loops/1

**Specification:**
```erlang
-spec discover_loops(event_log()) -> petri_net().
```

**Description:** Properly handles short loops (length-1 and length-2) and long loops.

**Loop Types Detected:**
- `short_loop_1` - Self-loop (A -> A)
- `short_loop_2` - Two-activity loop (A -> B -> A)
- `long_loop` - Cycles through 3+ activities

##### detect_loops/1

**Specification:**
```erlang
-spec detect_loops([[atom()]]) -> [loop_info()].
```

**Description:** Detects loops in traces.

**Return Value:** List of loop information maps

**Usage Example:**
```erlang
Loops = process_discovery:detect_loops([[a, b, a, c]]).
% [#{type => short_loop_2, activities => [a, b], frequency => 1}]
```

##### classify_loops/1

**Specification:**
```erlang
-spec classify_loops([[atom()]]) -> [loop_info()].
```

**Description:** Classifies loops with detailed analysis (entry/exit points).

#### Non-Free-Choice Discovery

##### discover_non_free_choice/1

**Specification:**
```erlang
-spec discover_non_free_choice(event_log()) -> petri_net().
```

**Description:** Handles non-free-choice constructs (shared input/output places with different connectivity).

**Non-Free-Choice:** Transitions share input/output places but don't have identical connectivity

##### detect_non_free_choice/1

**Specification:**
```erlang
-spec detect_non_free_choice([[atom()]]) -> [non_free_choice_info()].
```

**Description:** Detects non-free-choice constructs in event log.

**Return Value:** List of non-free-choice pattern information

#### Analysis Functions

##### calculate_dependencies/1

**Specification:**
```erlang
-spec calculate_dependencies([[atom()]]) -> dependency_matrix().
```

**Description:** Calculates dependency matrix using heuristic measure.

##### calculate_frequency_matrix/1

**Specification:**
```erlang
-spec calculate_frequency_matrix([[atom()]]) -> frequency_matrix().
```

**Description:** Calculates frequency matrix of activity successions.

**Return Value:** Map of `{A, B}` -> occurrence count

**Usage Example:**
```erlang
Freq = process_discovery:calculate_frequency_matrix([[a, b, c]]),
maps:get({a, b}, Freq).  % 1
```

---

## Common Patterns and Error Handling

### Error Conditions Summary

| Module | Function | Error Condition | Return Value |
|--------|----------|-----------------|--------------|
| `anomaly_detection` | `check_real_time/2` | Invalid receipt | `{ok, #anomaly_result{anomalies = []}}` |
| `anomaly_alert` | `unsubscribe/1` | Not found | `{error, not_found}` |
| `pred_training` | `get_training_data/1` | Not found | `{error, not_found}` |
| `conformance` | `token_replay/2` | Empty log | `#{missing => 0, remaining => 0, ...}` |
| `alpha_algorithm` | `mine_workflow_net/1` | Empty log | Minimal WF-net |

### Typical Usage Pattern

```erlang
%% 1. Discover a process model
Log = [[a, b, c, d], [a, c, b, d]],
Model = alpha_algorithm:mine_workflow_net(Log).

%% 2. Check conformance
Fitness = conformance:fitness_score(Log, Model),

%% 3. Detect anomalies in real-time
{ok, Result} = anomaly_detection:check_real_time(<<"default">>, Receipt),

%% 4. Make predictions
{ok, NextActivities} = predictive_mining:predict_next_activity(CaseId, Trace).
```

### Integration Example

```erlang
%% Complete workflow: discovery -> conformance -> prediction
discover_and_analyze(Log) ->
    %% Discover model
    Model = process_discovery:heuristic_miner(Log),

    %% Check conformance
    Report = conformance:conformance_report(Log, Model),

    %% Setup prediction
    {ok, _} = predictive_mining:start_link(),

    %% Make predictions for first case
    [FirstTrace | _] = Log,
    {ok, Predictions} = predictive_mining:predict_next_activity(
        <<"case1">>,
        FirstTrace
    ),

    #{
        model => Model,
        conformance => Report,
        predictions => Predictions
    }.
```

---

## References

1. **Alpha Algorithm**: van der Aalst et al., "Workflow Mining: Discovering Process Models from Event Logs" (2001)
2. **Heuristic Miner**: Weijters & van der Aalst, "Rediscovering Process Mining" (2003)
3. **Conformance Checking**: "Process Mining Manifesto" (2011)
4. **Token Replay**: Basic technique for fitness computation
5. **Alignment**: A*-based optimal alignment for conformance checking

---

*Document Version: 1.0*
*Last Updated: 2025-02-08*
