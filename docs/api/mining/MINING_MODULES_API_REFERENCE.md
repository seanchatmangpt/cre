# Mining Modules API Reference

## Table of Contents

1. [Process Discovery](#process-discovery)
   - [alpha_algorithm - Alpha Algorithm](#1-alpha_algorithm---alpha-algorithm)
   - [process_discovery - Enhanced Process Discovery](#2-process_discovery---enhanced-process-discovery)
2. [Conformance Checking](#conformance-checking)
   - [conformance - Conformance Checking](#3-conformance---conformance-checking)
3. [Anomaly Detection](#anomaly-detection)
   - [anomaly_detection - Anomaly Detection](#4-anomaly_detection---anomaly-detection)
   - [anomaly_classifier - Anomaly Classification](#5-anomaly_classifier---anomaly-classification)
   - [anomaly_statistics - Statistical Analysis](#6-anomaly_statistics---statistical-analysis)
   - [anomaly_store - Anomaly Storage](#7-anomaly_store---anomaly-storage)
   - [anomaly_alert - Alert System](#8-anomaly_alert---alert-system)
4. [Predictive Mining](#predictive-mining)
   - [predictive_mining - Prediction API](#9-predictive_mining---prediction-api)
   - [pred_training - Training Data](#10-pred_training---training-data)
   - [pred_stats - Statistical Models](#11-pred_stats---statistical-models)
   - [pred_rnn - RNN Implementation](#12-pred_rnn---rnn-implementation)

---

## Process Discovery

### 1. alpha_algorithm - Alpha Algorithm

**File:** `src/mining/alpha_algorithm.erl`
**Lines:** 803
**Status:** Complete

Implements the Alpha algorithm for discovering workflow nets (WF-nets) from event logs, as described in "Workflow Mining: Discovering Process Models from Event Logs" by W.M.P. van der Aalst, et al. (2001).

#### Type Definitions

```erlang
-type activity() :: atom().
```
**Description:** An activity in the event log. Activities are represented as atoms for efficient pattern matching.

```erlang
-type trace() :: [activity()].
```
**Description:** A trace is a sequence of activities. Each trace represents one complete case execution.

```erlang
-type event_log() :: [trace()].
```
**Description:** An event log is a list of traces. The log may contain multiple traces of varying lengths.

```erlang
-type direct_succession() :: sets:set({activity(), activity()}).
```
**Description:** Direct succession relation a > b. A set of {A, B} tuples indicating activity A is immediately followed by activity B in at least one trace.

```erlang
-type causality() :: sets:set({activity(), activity()}).
```
**Description:** Causal relation a -> b. A set of {A, B} tuples indicating activity A causally precedes activity B (A > B and not B > A).

```erlang
-type parallel() :: sets:set({activity(), activity()}).
```
**Description:** Parallel relation a || b. A set of {A, B} tuples indicating activities A and B can execute in parallel.

```erlang
-type wf_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [arc()],
    initial_place => place(),
    final_place => place()
}.
```
**Description:** A workflow net (WF-net) discovered from an event log. Contains places, transitions, arcs, and the designated source/sink places.

#### Exported Functions

##### Main API

```erlang
-spec mine_workflow_net(Log :: event_log()) -> wf_net().
```
**Description:** Mines a workflow net from an event log using the Alpha algorithm.

**Parameters:**
- `Log` - Event log as list of traces

**Returns:** Complete WF-net structure with places, transitions, arcs, and initial/final places

**Example:**
```erlang
> Log = [[a, b, c, d], [a, c, b, d]],
> Net = alpha_algorithm:mine_workflow_net(Log),
> maps:get(transitions, Net).
[a,b,c,d]
> maps:get(places, Net).
[i_source,p_a_b,p_a_c,p_b_d,p_c_d,o_sink]
```

```erlang
-spec extract_ordering_relations(Log :: event_log()) -> ordering_relations().
```
**Description:** Extracts all ordering relations from an event log.

**Returns:** Map containing direct_succession, causality, parallel, unrelated relations and activities set

**Example:**
```erlang
> Log = [[a, b, c], [a, c, b]],
> Relations = alpha_algorithm:extract_ordering_relations(Log),
> sets:to_list(maps:get(causality, Relations)).
[{a,b},{a,c},{b,d},{c,d}]
```

##### Relation Extraction

```erlang
-spec direct_succession(Log :: event_log()) -> direct_succession().
```
**Description:** Extracts direct succession relations from an event log. Direct succession (a > b) exists if activity a is immediately followed by activity b in at least one trace.

**Example:**
```erlang
> Log = [[a, b, c, d], [a, c, b, d]],
> DS = alpha_algorithm:direct_succession(Log),
> sets:to_list(DS).
[{a,b},{b,c},{c,d},{a,c},{c,b},{b,d}]
```

```erlang
-spec causality(DirectSucc :: direct_succession()) -> causality().
```
**Description:** Extracts causal relations from direct succession relations. Causality (a -> b) exists when a > b and NOT b > a.

**Example:**
```erlang
> DS = sets:from_list([{a,b},{b,a},{b,c}]),
> Causal = alpha_algorithm:causality(DS),
> sets:to_list(Causal).
[{b,c}]
```

```erlang
-spec parallel(DirectSucc :: direct_succession()) -> parallel().
```
**Description:** Extracts parallel relations from direct succession relations. Parallel (a || b) exists when both a > b AND b > a.

**Example:**
```erlang
> DS = sets:from_list([{a,b},{b,a},{b,c}]),
> Par = alpha_algorithm:parallel(DS),
> sets:to_list(Par).
[{a,b}]
```

```erlang
-spec unrelated(DirectSucc :: direct_succession(), Activities :: sets:set(activity())) -> unrelated().
```
**Description:** Extracts unrelated relations from direct succession and activities. Unrelated (a # b) exists when NEITHER a > b NOR b > a.

---

### 2. process_discovery - Enhanced Process Discovery

**File:** `src/mining/process_discovery.erl`
**Lines:** 1,804
**Status:** Complete

Implements advanced process mining algorithms that extend the Alpha algorithm with heuristics for handling noise, incomplete logs, loops, and non-free-choice constructs.

#### Type Definitions

```erlang
-type event() :: {case_id(), activity(), timestamp()}.
```
**Description:** A single event in the event log. Tuple of {CaseID, Activity, Timestamp}.

```erlang
-type event_log() :: [event()].
```
**Description:** An event log as a list of events. Events may be unsorted; the algorithm groups by case and sorts.

```erlang
-type dependency() :: float().
```
**Description:** Dependency strength between two activities. Float value between 0.0 and 1.0.

```erlang
-type dependency_matrix() :: #{{activity(), activity()} => dependency()}.
```
**Description:** Dependency matrix mapping activity pairs to dependency values.

```erlang
-type petri_net() :: #{
    places => [place()],
    transitions => [transition()],
    arcs => [arc()],
    initial_place => place(),
    final_place => place(),
    metadata => map()
}.
```
**Description:** Complete Petri net model discovered from an event log.

#### Exported Functions

##### Main Discovery API

```erlang
-spec discover(Log :: event_log()) -> petri_net().
```
**Description:** Main entry point for process discovery from event logs. Uses Heuristic Miner by default for better noise tolerance.

**Example:**
```erlang
> Log = [{case1, a, 1}, {case1, b, 2}, {case1, c, 3},
          {case2, a, 4}, {case2, c, 5}, {case2, b, 6}],
> Net = process_discovery:discover(Log),
> maps:get(transitions, Net).
[a,b,c]
```

```erlang
-spec heuristic_miner(Log :: event_log()) -> petri_net().
```
**Description:** Heuristic mining algorithm that handles noise better than Alpha. Uses frequency-based dependency measures with configurable thresholds.

**Example:**
```erlang
> Log = [{c1, a, 1}, {c1, b, 2}, {c1, c, 3},
          {c2, a, 4}, {c2, c, 5}, {c2, b, 6}],
> Net = process_discovery:heuristic_miner(Log),
> maps:get(metadata, Net).
#{algorithm => heuristic_miner, cases_processed => 2, ...}
```

```erlang
-spec frequency_based(Log :: event_log()) -> dependency_matrix().
```
**Description:** Frequency-based dependency extraction. Calculates dependency strength based on frequency of succession patterns.

**Formula:** `dep(a,b) = (|a>b| - |b>a|) / (|a>b| + |b>a| + 1)`

```erlang
-spec discover_with_noise(Log :: event_log(), Options :: map()) -> petri_net().
```
**Description:** Handle incomplete/noisy event logs with explicit parameters.

**Options:**
- `dependency_threshold` - Minimum dependency strength (0.0 to 1.0, default 0.6)
- `positive_observations` - Minimum observations to consider relation (default 1)
- `best_practice_threshold` - Minimum relative frequency (default 0.8)

**Example:**
```erlang
> Log = noisy_log(),
> Net = process_discovery:discover_with_noise(Log, #{
>   dependency_threshold => 0.9,
>   positive_observations => 3
> }).
```

```erlang
-spec discover_loops(Log :: event_log()) -> petri_net().
```
**Description:** Properly handle short and long loops. Detects and constructs Petri net structures for:
- Short loops of length 1 (A -> A): Self-loop transitions
- Short loops of length 2 (A -> B -> A): Two-transition loops
- Long loops: Cycles through three or more activities

```erlang
-spec discover_non_free_choice(Log :: event_log()) -> petri_net().
```
**Description:** Handle non-free-choice constructs where multiple transitions share input or output places but have different connectivity patterns.

##### Analysis Functions

```erlang
-spec calculate_dependencies([trace()]) -> dependency_matrix().
```
**Description:** Calculates dependency matrix from traces using the heuristic dependency measure.

```erlang
-spec calculate_frequency_matrix([trace()]) -> frequency_matrix().
```
**Description:** Calculates frequency matrix of activity successions. Counts how often activity A is directly followed by activity B.

```erlang
-spec detect_loops([trace()]) -> [loop_info()].
```
**Description:** Detects loops in the event log. Returns list of loop information maps with type, activities, and frequency.

```erlang
-spec classify_loops([trace()]) -> [loop_info()].
```
**Description:** Classifies loops by type with detailed analysis including entry/exit points and nested loop structures.

```erlang
-spec detect_non_free_choice([trace()]) -> [non_free_choice_info()].
```
**Description:** Detects non-free-choice constructs in the event log. Returns list of shared input/output place information.

---

## Conformance Checking

### 3. conformance - Conformance Checking

**File:** `src/mining/conformance.erl`
**Lines:** 1,184
**Status:** Complete

Implements conformance checking techniques that compare an event log (observed behavior) against a process model (expected behavior).

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
```
**Description:** Token replay result for a single trace.

```erlang
-type replay_problems() :: #{
    missing => non_neg_integer(),
    remaining => non_neg_integer(),
    consumed => non_neg_integer(),
    produced => non_neg_integer()
}.
```
**Description:** Aggregated replay problems across all traces.

```erlang
-type align_move() :: {log_move, activity()} |
                     {model_move, transition()} |
                     {sync_move, activity()} |
                     {no_move}.
```
**Description:** Alignment move type.

```erlang
-type alignment() :: [align_move()].
```
**Description:** An alignment is a sequence of aligned moves.

```erlang
-type alignment_result() :: #{
    alignment => alignment(),
    cost => non_neg_integer(),
    trace => trace(),
    fitness => float()
}.
```
**Description:** Alignment result with cost.

```erlang
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
**Description:** Complete conformance report.

#### Exported Functions

##### Main Conformance API

```erlang
-spec token_replay(Log :: event_log(), Model :: wf_net()) -> replay_problems().
```
**Description:** Replays an event log through a model using token replay. Returns aggregated problem counts.

**Example:**
```erlang
> Log = [[a, b, c]],
> Model = alpha_algorithm:mine_workflow_net(Log),
> conformance:token_replay(Log, Model).
#{missing => 0, remaining => 0, consumed => 3, produced => 3}
```

```erlang
-spec fitness_score(Log :: event_log(), Model :: wf_net()) -> float().
```
**Description:** Calculates fitness score (0-1) based on token replay.

**Formula:** `1/2 * (1 - missing/consumed) + 1/2 * (1 - remaining/produced)`

Returns 1.0 for perfect fitness, 0.0 for complete mismatch.

**Example:**
```erlang
> Log = [[a, b, c]],
> Model = alpha_algorithm:mine_workflow_net(Log),
> conformance:fitness_score(Log, Model).
1.0
```

```erlang
-spec precision_score(Log :: event_log(), Model :: wf_net()) -> float().
```
**Description:** Calculates precision score (0-1) based on escaping edges. Higher precision means the model is more specific to the log.

```erlang
-spec generalization_score(Log :: event_log(), Model :: wf_net()) -> float().
```
**Description:** Calculates generalization score (0-1) for the model. Measures whether the model is overfitting.

```erlang
-spec conformance_report(Log :: event_log(), Model :: wf_net()) -> conformance_report().
```
**Description:** Generates a complete conformance report with all metrics and diagnostic information.

**Example:**
```erlang
> Log = [[a, b, c]],
> Model = alpha_algorithm:mine_workflow_net(Log),
> Report = conformance:conformance_report(Log, Model),
> maps:get(fitness, Report).
1.0
```

##### Alignment Functions

```erlang
-spec align_trace(Trace :: trace(), Model :: wf_net()) -> alignment_result().
```
**Description:** Finds optimal alignment between a trace and model using A* search.

**Move Types:**
- `log_move` - Activity in trace but not enabled in model
- `model_move` - Transition enabled but not in trace
- `sync_move` - Activity matches enabled transition

**Example:**
```erlang
> Trace = [a, b, c],
> Model = alpha_algorithm:mine_workflow_net([Trace]),
> Result = conformance:align_trace(Trace, Model),
> maps:get(cost, Result).
0
```

##### Analysis Utilities

```erlang
-spec replay_trace(Trace :: trace(), Model :: wf_net()) -> replay_result().
```
**Description:** Replays a single trace through the model.

```erlang
-spec count_problems(replay_problems()) -> non_neg_integer().
```
**Description:** Counts problems from a replay_problems map. Returns total deviation count (missing + remaining).

```erlang
-spec alignment_cost(Alignment :: alignment()) -> non_neg_integer().
```
**Description:** Calculates the cost of an alignment. Log moves and model moves cost 1, sync moves cost 0.

```erlang
-spec alignment_fitness(Alignment :: alignment(), Trace :: trace()) -> float().
```
**Description:** Calculates fitness from an alignment. Formula: `1 - (cost / (2 * trace_length))`.

---

## Anomaly Detection

### 4. anomaly_detection - Anomaly Detection

**File:** `src/mining/anomaly_detection.erl`
**Lines:** 315
**Status:** Complete

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
```

```erlang
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
**Description:** Start the anomaly detector with default or specific name.

```erlang
-spec stop(binary()) -> ok.
```
**Description:** Stop the anomaly detector.

##### Detection

```erlang
-spec check_real_time(binary(), map()) -> {ok, #anomaly_result{}}.
```
**Description:** Perform real-time anomaly check on a workflow receipt.

**Parameters:**
- `Name` - Detector name
- `Receipt` - Map containing `timestamp`, `start_time`, `task`, `case_id`

**Returns:** Anomaly result with detected anomalies and processing time

```erlang
-spec detect_batch(binary(), [map()]) -> {ok, #anomaly_result{}}.
```
**Description:** Perform batch anomaly detection on event logs.

**Parameters:**
- `Name` - Detector name
- `EventLogs` - List of event maps with `trace` keys

##### Configuration

```erlang
-spec get_thresholds(binary()) -> map().
```
**Description:** Get current detection thresholds.

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
**Description:** Update detection thresholds dynamically.

**Example:**
```erlang
> anomaly_detection:set_thresholds(<<"detector1">>, #{
>   statistical_outlier => 3.0,
>   timing_anomaly => 2.5
> }).
ok
```

---

### 5. anomaly_classifier - Anomaly Classification

**File:** `src/mining/anomaly_classifier.erl`
**Lines:** 121
**Status:** Complete

Classifies anomalies by type and severity.

#### Type Definitions

```erlang
-type anomaly_type() ::
    statistical_outlier | sequence_rare | timing_anomaly |
    conformance_mismatch | resource_exhaustion | ml_detected |
    cascade_risk | unknown_pattern.
```

```erlang
-type severity() :: critical | warning | info.
```

```erlang
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
**Description:** Classify an anomaly into type and severity based on feature map.

**Features keys:**
- `conformance` - Map with `fitness` score
- `timing` - Map with `duration` in milliseconds
- `sequence` - Map with `frequency` score
- `statistical` - Map with `zscore` value

**Example:**
```erlang
> Features = #{
>   conformance => #{fitness => 0.65},
>   timing => #{duration => 500},
>   statistical => #{zscore => 2.0}
> },
> anomaly_classifier:classify(Features).
#{
>   type => conformance_mismatch,
>   severity => critical,
>   confidence => 0.7,
>   details => Features
> }
```

```erlang
-spec classify_batch([map()]) -> [anomaly()].
```
**Description:** Classify a batch of anomalies.

```erlang
-spec calculate_severity(anomaly_type(), map()) -> severity().
```
**Description:** Calculate severity based on anomaly type and context.

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
**Description:** Calculate confidence score (0.0 to 1.0) for anomaly type with features. Based on indicator count in features.

```erlang
-spec generate_reasoning(anomaly_type(), map(), float()) -> binary().
```
**Description:** Generate human-readable reasoning for anomaly classification.

**Example:**
```erlang
> anomaly_classifier:generate_reasoning(
>   statistical_outlier,
>   #{statistical => #{zscore => 3.5}},
>   0.95
> ).
<<"Statistical outlier detected (Z-score: 3.50) (confidence: 0.95)">>
```

---

### 6. anomaly_statistics - Statistical Analysis

**File:** `src/mining/anomaly_statistics.erl`
**Lines:** 392
**Status:** Complete

Pure functional statistical calculations for anomaly detection. All functions are total (no crashes) and operate on lists.

#### Type Definitions

```erlang
-type numeric() :: number().
```

#### Exported Functions

##### Basic Statistics

```erlang
-spec mean([numeric()]) -> float().
```
**Description:** Calculates the arithmetic mean of a list of numbers. Returns 0.0 for empty lists.

**Example:**
```erlang
> anomaly_statistics:mean([1,2,3,4,5]).
3.0
> anomaly_statistics:mean([]).
0.0
```

```erlang
-spec median([numeric()]) -> float().
```
**Description:** Calculates the median (50th percentile).

```erlang
-spec stddev([numeric()]) -> float().
```
**Description:** Calculates population standard deviation.

```erlang
-spec variance([numeric()]) -> float().
```
**Description:** Calculates population variance.

```erlang
-spec percentile([numeric()], integer()) -> float().
```
**Description:** Calculates the Nth percentile of values (0-100).

**Example:**
```erlang
> anomaly_statistics:percentile([1,2,3,4,5,6,7,8,9,10], 90).
10
```

##### Advanced Statistics

```erlang
-spec zscore(numeric(), [numeric()]) -> float().
```
**Description:** Calculates Z-score for a value given population statistics.

**Formula:** `(value - mean) / stddev`

```erlang
-spec iqr_outliers([numeric()], float()) -> {ok, [{numeric(), pos_integer()}]}.
```
**Description:** Identifies outliers using IQR method.

**Parameters:**
- Values - List of numeric values
- Multiplier - IQR multiplier (typically 1.5)

**Returns:** List of {Value, Index} tuples for outliers

```erlang
-spec moving_average([numeric()], pos_integer()) -> [float()].
```
**Description:** Calculates moving average over specified window size.

```erlang
-spec correlation([numeric()], [numeric()]) -> float() | undefined.
```
**Description:** Calculates correlation coefficient between two lists. Returns `undefined` for invalid inputs.

```erlang
-spec covariance([numeric()], [numeric()]) -> float() | undefined.
```
**Description:** Calculates covariance between two lists.

##### Trend Analysis

```erlang
-spec trend_analysis([numeric()]) -> map().
```
**Description:** Analyzes trend in time series data.

**Returns:** Map with `trend` (rising/falling/stable), `slope`, and `r_squared`

**Example:**
```erlang
> anomaly_statistics:trend_analysis([1,2,3,4,5]).
#{trend => rising, slope => 1.0, r_squared => 1.0}
```

```erlang
-spec detect_trend([numeric()], float()) -> rising | falling | stable.
```
**Description:** Detects trend direction with specified threshold.

##### Anomaly Scoring

```erlang
-spec outlier_score(numeric(), [numeric()]) -> float().
```
**Description:** Calculates composite outlier score (0-1, higher = more anomalous). Combines Z-score, IQR, and percentile rank.

```erlang
-spec anomaly_probability(numeric(), [numeric()]) -> float().
```
**Description:** Calculates probability that value belongs to population distribution based on normal distribution PDF.

---

### 7. anomaly_store - Anomaly Storage

**File:** `src/mining/anomaly_store.erl`
**Lines:** 163
**Status:** Complete

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
**Description:** Update trace frequency by delta.

**Parameters:**
- `CaseId` - Case identifier
- `Delta` - Positive to increment, negative to decrement

```erlang
-spec get_trace_frequency(binary()) -> integer() | undefined.
```
**Description:** Get current trace frequency for a case ID.

##### Anomaly Storage

```erlang
-spec store_anomaly(#anomaly_record{}) -> reference().
```
**Description:** Store an anomaly record.

```erlang
-spec get_anomalies(binary()) -> [#anomaly_record{}].
```
**Description:** Get all anomalies for a specific case ID.

```erlang
-spec get_all_anomalies() -> [#anomaly_record{}].
```
**Description:** Get all stored anomalies.

##### Alert Management

```erlang
-spec create_alert(#anomaly_record{}) -> reference().
```
**Description:** Create an alert from an anomaly record.

```erlang
-spec get_alerts() -> [#anomaly_record{}].
```
**Description:** Get all alerts.

---

### 8. anomaly_alert - Alert System

**File:** `src/mining/anomaly_alert.erl`
**Lines:** 190
**Status:** Complete

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
```

```erlang
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
**Description:** Subscribe to anomaly alerts with filter criteria.

**Filter keys:**
- `severity` - critical | warning | info
- `anomaly_type` - Atom type filter
- `case_id` - Binary case ID filter
- `min_confidence` - Minimum confidence threshold (0.0 to 1.0)
- `notification_mode` - sync | async

**Example:**
```erlang
> Ref = anomaly_alert:subscribe(#{
>   severity => critical,
>   min_confidence => 0.8,
>   notification_mode => async
> }).
```

```erlang
-spec unsubscribe(reference()) -> ok | {error, not_found}.
```
**Description:** Unsubscribe from alerts.

```erlang
-spec list_subscriptions() -> [#subscription{}].
```
**Description:** List all active subscriptions.

```erlang
-spec get_subscriber_count() -> integer().
```
**Description:** Get total number of active subscribers.

##### Notification

```erlang
-spec notify(#anomaly_alert{}) -> ok.
```
**Description:** Send alert to all matching subscribers.

**Example:**
```erlang
> Alert = #anomaly_alert{
>   id = make_ref(),
>   severity = critical,
>   anomaly_type = conformance_mismatch,
>   case_id => <<"case123">>,
>   description => <<"Fitness below threshold">>,
>   confidence = 0.6,
>   timestamp = erlang:system_time(millisecond)
> },
> anomaly_alert:notify(Alert).
ok
```

---

## Predictive Mining

### 9. predictive_mining - Prediction API

**File:** `src/mining/predictive_mining.erl`
**Lines:** 178
**Status:** Complete

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
**Description:** Predict the next activity from a trace using Markov model.

**Returns:** List of {Activity, Probability} tuples sorted by probability

**Example:**
```erlang
> predictive_mining:predict_next_activity(<<"case1">>, [a, b]).
{ok, [{c, 0.4}, {d, 0.3}, {e, 0.2}, {review, 0.1}]}
```

```erlang
-spec predict_remaining_time(binary(), [atom()]) -> {ok, integer()}.
```
**Description:** Predict remaining time for case completion in milliseconds.

```erlang
-spec predict_outcome(binary(), [atom()]) -> {ok, success | failure, float()}.
```
**Description:** Predict outcome (success/failure) with confidence score.

**Example:**
```erlang
> predictive_mining:predict_outcome(<<"case1">>, [a, b, c]).
{ok, success, 0.8}
```

##### Model Management

```erlang
-spec load_model(binary()) -> ok | {error, term()}.
```
**Description:** Load a prediction model by ID.

```erlang
-spec unload_model(binary()) -> ok.
```
**Description:** Unload a prediction model.

```erlang
-spec list_loaded_models() -> [binary()].
```
**Description:** List all loaded models.

##### Configuration

```erlang
-spec set_prediction_mode(realtime | batch) -> ok.
```
**Description:** Set prediction mode (realtime or batch processing).

---

### 10. pred_training - Training Data

**File:** `src/mining/pred_training.erl`
**Lines:** 229
**Status:** Complete

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
**Description:** Extract sequences from XES/OCEL formatted event logs.

**Input format:** Map with `cases` key containing case data

```erlang
-spec extract_features(map()) -> [float()].
```
**Description:** Extract features from events.

**Features extracted:**
- Activity encoding
- Timestamp (as float)
- Resource encoding
- Duration

##### Training Set Management

```erlang
-spec build_training_set([[atom()]], pos_integer()) -> {[#training_example{}], map()}.
```
**Description:** Build training set with sliding window.

**Parameters:**
- `Sequences` - List of activity sequences
- `WindowSize` - Size of sliding window for context

**Returns:** Tuple of training examples and configuration map

```erlang
-spec split_train_test([#training_example{}], float(), binary()) ->
    {[#training_example{}], [#training_example{}]}.
```
**Description:** Split data into train/test sets.

**Parameters:**
- `Examples` - Training examples
- `TrainRatio` - Fraction for training (0.0 to 1.0)
- `Mode` - `random` for shuffled split, `sequential` for ordered split

**Example:**
```erlang
> {Train, Test} = pred_training:split_train_test(Examples, 0.8, random).
```

##### Event Recording

```erlang
-spec record_training_event(binary(), term(), map(), integer()) -> ok.
```
**Description:** Record a training event from telemetry.

**Parameters:**
- `WorkflowId` - Workflow identifier
- `EventName` - Name of the event
- `Labels` - Event labels/metadata
- `Timestamp` - Event timestamp

```erlang
-spec get_training_data(binary()) -> {ok, [#training_example{}]} | {error, not_found}.
```
**Description:** Get training data for a specific workflow ID.

---

### 11. pred_stats - Statistical Models

**File:** `src/mining/pred_stats.erl`
**Lines:** 185
**Status:** Complete

Statistical fallback models for prediction: Markov chains, exponential smoothing, and linear regression.

#### Records

```erlang
-record(markov_model, {
    transitions :: map(),
    activities :: [atom()]
}).
```

```erlang
-record(ema_model, {
    alpha :: float(),
    values :: [float()],
    last_ema :: float()
}).
```

```erlang
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
**Description:** Fit a Markov chain model from traces.

**Example:**
```erlang
> Traces = [[a,b,c], [a,c,b], [a,b,c]],
> Model = pred_stats:fit_markov(Traces),
> pred_stats:get_transitions(Model).
#{a => #{b => 0.67, c => 0.33}, b => #{c => 1.0}, ...}
```

```erlang
-spec predict_markov([[atom()]], atom()) -> [{atom(), float()}].
```
**Description:** Predict next activities using Markov model. Can be called with traces (fits model first) or pre-built model.

**Returns:** List of {Activity, Probability} tuples

```erlang
-spec get_transitions(#markov_model{}) -> map().
```
**Description:** Get transition matrix from model.

##### Exponential Smoothing

```erlang
-spec fit_ema([float()], float()) -> #ema_model{}.
```
**Description:** Fit EMA model to values.

**Parameters:**
- `Values` - Time series values
- `Alpha` - Smoothing factor (0.0 to 1.0, higher = more weight on recent values)

```erlang
-spec predict_ema(#ema_model{}, integer()) -> float().
```
**Description:** Predict using EMA. Returns the last EMA value (constant prediction).

##### Linear Regression

```erlang
-spec fit_linear([float()]) -> #linear_model{}.
```
**Description:** Fit linear regression to values.

**Returns:** Model with slope, intercept, and R-squared

```erlang
-spec predict_linear(#linear_model{}, integer()) -> float().
```
**Description:** Predict using linear model.

**Parameters:**
- `Model` - Fitted linear model
- `X` - X value (index) for prediction

**Formula:** `y = slope * x + intercept`

---

### 12. pred_rnn - RNN Implementation

**File:** `src/mining/pred_rnn.erl`
**Lines:** 218
**Status:** Complete

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
**Description:** Create a new RNN with random initialization.

**Parameters:**
- `InputSize` - Number of input features
- `HiddenSize` - Number of hidden units
- `OutputSize` - Number of output units

```erlang
-spec forward_step({input_vector(), hidden_state()}, rnn_cell()) ->
    {hidden_state(), [float()]}.
```
**Description:** Single forward step through RNN.

```erlang
-spec forward_sequence(sequence(), rnn_cell()) -> {hidden_state(), [[float()]]}.
```
**Description:** Forward pass through complete sequence.

```erlang
-spec predict(sequence(), rnn_cell()) -> [float()].
```
**Description:** Predict next activity from sequence.

##### Weight Management

```erlang
-spec get_weights(rnn_cell()) -> map().
```
**Description:** Get weights as map: `#{wxh => [], whh => [], why => [], bh => [], by => []}`.

```erlang
-spec set_weights(rnn_cell(), map()) -> rnn_cell().
```
**Description:** Set weights from map.

##### Matrix Operations

```erlang
-spec matrix_mult([[float()]], [[float()]]) -> [[float()]].
```
**Description:** Matrix multiplication.

```erlang
-spec matrix_add([[float()]], [[float()]]) -> [[float()]].
```
**Description:** Element-wise matrix addition.

```erlang
-spec tanh_list([float()]) -> [float()].
```
**Description:** Hyperbolic tangent activation (applied to each element).

```erlang
-spec softmax([float()]) -> [float()].
```
**Description:** Softmax normalization (results sum to 1.0).

**Example:**
```erlang
> pred_rnn:softmax([1.0, 2.0, 3.0]).
[0.09003057317038046, 0.24472847105479764, 0.6652409557748219]
```

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
