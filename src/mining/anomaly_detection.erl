%% -*- erlang -*-
%% @doc Anomaly Detection for Process Mining
%%
%% Main gen_server for real-time and batch anomaly detection.
%%
%% @end

-module(anomaly_detection).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([stop/1]).
-export([check_real_time/2, detect_batch/2]).
-export([get_thresholds/1, set_thresholds/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%====================================================================
%% Records
%%====================================================================

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

-record(state, {
    name :: binary(),
    mode :: real_time | batch,
    thresholds :: map(),
    frequency_table :: ets:tid()
}).

-define(DEFAULT_THRESHOLDS, #{
    statistical_outlier => 2.5,
    sequence_rare => 0.95,
    timing_anomaly => 3.0,
    conformance_mismatch => 0.7
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the anomaly detector with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(<<"default_anomaly_detector">>).

%% @doc Start the anomaly detector with a specific name.
-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(Name) when is_binary(Name) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Name], []).

%% @doc Stop the anomaly detector.
-spec stop(binary()) -> ok.
stop(_Name) ->
    gen_server:stop(?MODULE).

%% @doc Perform real-time anomaly check on a workflow receipt.
-spec check_real_time(binary(), map()) -> {ok, #anomaly_result{}}.
check_real_time(Name, Receipt) when is_binary(Name), is_map(Receipt) ->
    gen_server:call(?MODULE, {check_real_time, Name, Receipt}).

%% @doc Perform batch anomaly detection on event logs.
-spec detect_batch(binary(), [map()]) -> {ok, #anomaly_result{}}.
detect_batch(Name, EventLogs) when is_binary(Name), is_list(EventLogs) ->
    gen_server:call(?MODULE, {detect_batch, Name, EventLogs}).

%% @doc Get current detection thresholds.
-spec get_thresholds(binary()) -> map().
get_thresholds(Name) ->
    gen_server:call(?MODULE, {get_thresholds, Name}).

%% @doc Update detection thresholds dynamically.
-spec set_thresholds(binary(), map()) -> ok.
set_thresholds(Name, Thresholds) when is_map(Thresholds) ->
    gen_server:call(?MODULE, {set_thresholds, Name, Thresholds}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Name]) ->
    %% Create ETS table for frequency tracking
    FreqTable = ets:new(anomaly_frequency, [set, public]),
    {ok, #state{
        name = Name,
        mode = real_time,
        thresholds = ?DEFAULT_THRESHOLDS,
        frequency_table = FreqTable
    }}.

handle_call({check_real_time, Name, Receipt}, _From, State) when State#state.name =:= Name ->
    Start = erlang:monotonic_time(millisecond),
    Anomalies = detect_real_time_anomalies(Receipt, State),
    ProcessingTime = erlang:monotonic_time(millisecond) - Start,
    Result = #anomaly_result{
        anomalies = Anomalies,
        statistics = calculate_statistics(Anomalies),
        processing_time_ms = ProcessingTime
    },
    {reply, {ok, Result}, State};

handle_call({detect_batch, Name, EventLogs}, _From, State) when State#state.name =:= Name ->
    Start = erlang:monotonic_time(millisecond),
    Anomalies = detect_batch_anomalies(EventLogs, State),
    ProcessingTime = erlang:monotonic_time(millisecond) - Start,
    Result = #anomaly_result{
        anomalies = Anomalies,
        statistics = calculate_statistics(Anomalies),
        processing_time_ms = ProcessingTime
    },
    {reply, {ok, Result}, State};

handle_call({get_thresholds, Name}, _From, State) when State#state.name =:= Name ->
    {reply, State#state.thresholds, State};

handle_call({set_thresholds, Name, NewThresholds}, _From, State) when State#state.name =:= Name ->
    Merged = maps:merge(State#state.thresholds, validate_thresholds(NewThresholds)),
    {reply, ok, State#state{thresholds = Merged}};

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
detect_real_time_anomalies(Receipt, State) ->
    Thresholds = State#state.thresholds,
    case extract_timing_features(Receipt) of
        {ok, Features} ->
            check_anomalies(Features, Thresholds, Receipt);
        {error, _Reason} ->
            []
    end.

%% @private
detect_batch_anomalies(EventLogs, State) ->
    Thresholds = State#state.thresholds,
    FreqTable = State#state.frequency_table,

    %% Build frequency table from all traces
    lists:foreach(fun(Log) ->
        Trace = maps:get(trace, Log, []),
        TraceKey = lists:flatten([atom_to_list(A) || A <- Trace]),
        ets:update_counter(FreqTable, TraceKey, {2, 1}, [{TraceKey, 0}])
    end, EventLogs),

    %% Check each log for anomalies
    lists:foldl(fun(Log, Acc) ->
        case extract_timing_features(Log) of
            {ok, Features} ->
                Trace = maps:get(trace, Log, []),
                TraceKey = lists:flatten([atom_to_list(A) || A <- Trace]),
                Freq = case ets:lookup(FreqTable, TraceKey) of
                    [{_, Count}] -> Count;
                    [] -> 0
                end,
                FeaturesWithFreq = Features#{frequency => Freq, total_logs => length(EventLogs)},
                Anomalies = check_anomalies(FeaturesWithFreq, Thresholds, Log),
                Acc ++ Anomalies;
            {error, _Reason} ->
                Acc
        end
    end, [], EventLogs).

%% @private
calculate_statistics(Anomalies) ->
    Total = length(Anomalies),
    ByType = lists:foldl(fun(A, Acc) ->
        Type = A#anomaly.type,
        Acc#{Type => maps:get(Type, Acc, 0) + 1}
    end, #{}, Anomalies),
    BySeverity = lists:foldl(fun(A, Acc) ->
        Severity = A#anomaly.severity,
        Acc#{Severity => maps:get(Severity, Acc, 0) + 1}
    end, #{}, Anomalies),
    #{
        total => Total,
        by_type => ByType,
        by_severity => BySeverity
    }.

%% @private
validate_thresholds(Thresholds) ->
    maps:filter(fun(_K, V) -> is_number(V) andalso V >= 0 end, Thresholds).

%% @private
generate_id() ->
    <<(erlang:unique_integer([positive, monotonic])):64>>.

%% @private
extract_timing_features(Receipt) ->
    try
        Timestamp = maps:get(timestamp, Receipt, erlang:system_time(millisecond)),
        StartTime = maps:get(start_time, Receipt, Timestamp),
        Duration = Timestamp - StartTime,
        Task = maps:get(task, Receipt, undefined),
        CaseId = maps:get(case_id, Receipt, <<>>),

        Features = #{
            duration => Duration,
            timestamp => Timestamp,
            start_time => StartTime,
            task => Task,
            case_id => CaseId
        },
        {ok, Features}
    catch
        error:_ -> {error, invalid_receipt}
    end.

%% @private
check_anomalies(Features, Thresholds, _Receipt) ->
    %% Check timing anomaly
    Duration = maps:get(duration, Features, 0),
    TimingThreshold = maps:get(timing_anomaly, Thresholds, 1000000) * 1000,
    Anomalies1 = case Duration > TimingThreshold of
        true ->
            [#anomaly{
                id = generate_id(),
                type = timing_anomaly,
                severity = warning,
                confidence = 0.8,
                case_id = maps:get(case_id, Features, <<>>),
                task = maps:get(task, Features, undefined),
                details = #{duration => Duration, threshold => TimingThreshold},
                timestamp = erlang:system_time(millisecond)
            }];
        false ->
            []
    end,

    %% Check sequence frequency anomaly (for batch mode)
    Anomalies2 = case maps:get(frequency, Features, undefined) of
        undefined ->
            [];
        Freq ->
            Total = maps:get(total_logs, Features, 1),
            FreqRatio = Freq / max(1, Total),
            RareThreshold = maps:get(sequence_rare, Thresholds, 0.95),
            case FreqRatio < (1.0 - RareThreshold) of
                true ->
                    [#anomaly{
                        id = generate_id(),
                        type = sequence_rare,
                        severity = info,
                        confidence = 1.0 - FreqRatio,
                        case_id = maps:get(case_id, Features, <<>>),
                        task = maps:get(task, Features, undefined),
                        details = #{frequency => Freq, ratio => FreqRatio},
                        timestamp = erlang:system_time(millisecond)
                    }];
                false ->
                    []
            end
    end,

    %% Check for conformance issues (if fitness score is provided)
    Anomalies3 = case maps:get(fitness, Features, undefined) of
        undefined ->
            [];
        Fitness ->
            ConformanceThreshold = maps:get(conformance_mismatch, Thresholds, 0.7),
            case Fitness < ConformanceThreshold of
                true ->
                    [#anomaly{
                        id = generate_id(),
                        type = conformance_mismatch,
                        severity = critical,
                        confidence = 1.0 - Fitness,
                        case_id = maps:get(case_id, Features, <<>>),
                        task = maps:get(task, Features, undefined),
                        details = #{fitness => Fitness},
                        timestamp = erlang:system_time(millisecond)
                    }];
                false ->
                    []
            end
    end,

    Anomalies1 ++ Anomalies2 ++ Anomalies3.
