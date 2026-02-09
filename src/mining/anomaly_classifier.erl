%% -*- erlang -*-
%% @doc Anomaly Classification for Process Mining
%%
%% Classifies anomalies by type and severity.
%%
%% @end

-module(anomaly_classifier).

%% Classification API
-export([classify/1, classify_batch/1]).
-export([calculate_severity/2, calculate_confidence/1]).
-export([generate_reasoning/3]).

%% Types
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

-export_type([anomaly_type/0, severity/0, anomaly/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Classify an anomaly into type and severity.
-spec classify(map()) -> anomaly().
classify(Features) when is_map(Features) ->
    Type = classify_by_features(Features),
    Severity = calculate_severity(Type, Features),
    Confidence = calculate_confidence({Type, Features}),
    #{
        type => Type,
        severity => Severity,
        confidence => Confidence,
        details => Features
    }.

%% @doc Classify a batch of anomalies.
-spec classify_batch([map()]) -> [anomaly()].
classify_batch(FeaturesList) ->
    [classify(F) || F <- FeaturesList].

%% @doc Calculate severity based on anomaly type and context.
-spec calculate_severity(anomaly_type(), map()) -> severity().
calculate_severity(Type, _Features) ->
    base_severity(Type).

%% @doc Calculate confidence score (0.0 to 1.0) for anomaly type with features.
-spec calculate_confidence({anomaly_type(), map()}) -> float().
calculate_confidence({_Type, Features}) ->
    BaseConfidence = 0.5,
    IndicatorCount = maps:size(Features),
    Confidence = BaseConfidence + (IndicatorCount * 0.05),
    min(1.0, Confidence).

%% @doc Generate human-readable reasoning.
-spec generate_reasoning(anomaly_type(), map(), float()) -> binary().
generate_reasoning(Type, Features, Confidence) ->
    BaseReason = get_base_reason(Type, Features),
    ConfidenceNote = io_lib:format(" (confidence: ~.2f)", [Confidence]),
    list_to_binary([BaseReason, ConfidenceNote]).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
classify_by_features(Features) ->
    Conformance = maps:get(conformance, Features, undefined),
    Timing = maps:get(timing, Features, undefined),
    Sequence = maps:get(sequence, Features, undefined),
    Statistical = maps:get(statistical, Features, undefined),

    case {Conformance, Timing, Sequence, Statistical} of
        {#{fitness := F}, _, _, _} when F < 0.7 -> conformance_mismatch;
        {_, #{duration := D}, _, _} when D > 1000000 -> timing_anomaly;
        {_, _, #{frequency := F}, _} when F < 0.01 -> sequence_rare;
        {_, _, _, #{zscore := Z}} when abs(Z) > 3.0 -> statistical_outlier;
        _ -> unknown_pattern
    end.

%% @private
base_severity(statistical_outlier) -> warning;
base_severity(sequence_rare) -> info;
base_severity(timing_anomaly) -> warning;
base_severity(conformance_mismatch) -> critical;
base_severity(resource_exhaustion) -> critical;
base_severity(cascade_risk) -> critical;
base_severity(ml_detected) -> warning;
base_severity(unknown_pattern) -> info.

%% @private
get_base_reason(Type, Features) ->
    case Type of
        statistical_outlier ->
            Z = maps:get(zscore, maps:get(statistical, Features, #{}), 0.0),
            io_lib:format("Statistical outlier detected (Z-score: ~.2f)", [Z]);
        sequence_rare ->
            Freq = maps:get(frequency, maps:get(sequence, Features, #{}), 0.0),
            io_lib:format("Rare sequence detected (frequency: ~.4f)", [Freq]);
        timing_anomaly ->
            Duration = maps:get(duration, maps:get(timing, Features, #{}), 0),
            io_lib:format("Timing anomaly detected (duration: ~pms)", [Duration]);
        conformance_mismatch ->
            Fitness = maps:get(fitness, maps:get(conformance, Features, #{}), 0.0),
            io_lib:format("Conformance mismatch (fitness: ~.2f)", [Fitness]);
        _ ->
            io_lib:format("~s detected", [Type])
    end.
