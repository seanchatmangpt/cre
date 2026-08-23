%% -*- erlang -*-
%% @doc Adaptive Trace Sampling for CRE Distributed Tracing
%%
%% Implements intelligent sampling strategies for distributed tracing:
%% - High-value traces (errors, slow requests) sampled at 100%
%% - Background traffic sampled at lower rates (1-10%)
%% - Adaptive sampling based on traffic volume
%% - Per-service sampling configuration
%%
%% @end

-module(trace_sampling).

%% API
-export([should_sample/1]).
-export([should_sample/2]).
-export([get_sampling_rate/0]).
-export([set_sampling_rate/1]).
-export([set_strategy/1]).
-export([get_stats/0]).
-export([reset_stats/0]).

%% Sampling strategies
-export([adaptive_strategy/1]).
-export([priority_strategy/1]).
-export([error_focused_strategy/1]).
-export([probabilistic_strategy/1]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type span() :: map().
-type sampling_strategy() :: adaptive | priority | error_focused | probabilistic.
-type sampling_rate() :: float().
-type sampling_stats() :: #{total => non_neg_integer(),
                             sampled => non_neg_integer(),
                             dropped => non_neg_integer(),
                             rate => float()}.

-export_type([sampling_strategy/0, sampling_rate/0, sampling_stats/0]).

-define(SAMPLING_TABLE, trace_sampling_stats).
-define(DEFAULT_SAMPLING_RATE, 0.1).
-define(HIGH_VALUE_RATE, 1.0).
-define(BACKGROUND_RATE, 0.01).
-define(ERROR_THRESHOLD_MS, 1000).  % Spans over 1s are high-value
-define(ERROR_RATE_THRESHOLD, 0.05).  % 5% error rate triggers adaptive sampling

%%====================================================================
%% API
%%====================================================================

%% @doc Determine if a span should be sampled using current strategy.
-spec should_sample(span()) -> boolean().
should_sample(Span) ->
    Strategy = get_strategy(),
    should_sample(Span, Strategy).

%% @doc Determine if a span should be sampled using specific strategy.
-spec should_sample(span(), sampling_strategy() | {probability, float()}) -> boolean().
should_sample(_Span, {probability, Rate}) ->
    rand:uniform() < Rate;
should_sample(Span, adaptive) ->
    adaptive_strategy(Span);
should_sample(Span, priority) ->
    priority_strategy(Span);
should_sample(Span, error_focused) ->
    error_focused_strategy(Span);
should_sample(Span, probabilistic) ->
    probabilistic_strategy(Span).

%% @doc Get the current sampling rate.
-spec get_sampling_rate() -> float().
get_sampling_rate() ->
    case ets:lookup(?SAMPLING_TABLE, current_rate) of
        [{_, Rate}] -> Rate;
        [] -> ?DEFAULT_SAMPLING_RATE
    end.

%% @doc Set the sampling rate.
-spec set_sampling_rate(sampling_rate()) -> ok.
set_sampling_rate(Rate) when is_float(Rate), Rate >= 0.0, Rate =< 1.0 ->
    ensure_table(),
    ets:insert(?SAMPLING_TABLE, {current_rate, Rate}),
    ok.

%% @doc Set the sampling strategy.
-spec set_strategy(sampling_strategy()) -> ok.
set_strategy(Strategy) ->
    ensure_table(),
    ets:insert(?SAMPLING_TABLE, {current_strategy, Strategy}),
    ok.

%% @doc Get sampling statistics.
-spec get_stats() -> sampling_stats().
get_stats() ->
    ensure_table(),
    Total = case ets:lookup(?SAMPLING_TABLE, total_spans) of
        [{_, T}] -> T;
        [] -> 0
    end,
    Sampled = case ets:lookup(?SAMPLING_TABLE, sampled_spans) of
        [{_, S}] -> S;
        [] -> 0
    end,
    Dropped = Total - Sampled,
    Rate = case Total of
        0 -> 0.0;
        _ -> Sampled / Total
    end,
    #{total => Total, sampled => Sampled, dropped => Dropped, rate => Rate}.

%% @doc Reset sampling statistics.
-spec reset_stats() -> ok.
reset_stats() ->
    ensure_table(),
    ets:insert(?SAMPLING_TABLE, {total_spans, 0}),
    ets:insert(?SAMPLING_TABLE, {sampled_spans, 0}),
    ok.

%%====================================================================
%% Sampling Strategies
%%====================================================================

%% @private Adaptive sampling strategy.
%% Adjusts sampling rate based on traffic volume and error rate.
-spec adaptive_strategy(span()) -> boolean().
adaptive_strategy(Span) ->
    Stats = get_stats(),
    Total = maps:get(total, Stats, 0),

    case is_high_value_span(Span) of
        true ->
            record_sampled(),
            true;
        false ->
            %% Calculate adaptive rate based on volume
            Rate = calculate_adaptive_rate(Total),
            Decision = rand:uniform() < Rate,
            record_decision(Decision),
            Decision
    end.

%% @private Priority-based sampling strategy.
%% Prioritizes errors, slow requests, and specific services.
-spec priority_strategy(span()) -> boolean().
priority_strategy(Span) ->
    case classify_span_priority(Span) of
        high ->
            record_sampled(),
            true;
        medium ->
            Decision = rand:uniform() < 0.5,
            record_decision(Decision),
            Decision;
        low ->
            Decision = rand:uniform() < ?BACKGROUND_RATE,
            record_decision(Decision),
            Decision
    end.

%% @private Error-focused sampling strategy.
%% Samples 100% of error traces, low rate for success.
-spec error_focused_strategy(span()) -> boolean().
error_focused_strategy(Span) ->
    case has_error(Span) of
        true ->
            record_sampled(),
            true;
        false ->
            %% Check for slow request
            Duration = get_span_duration(Span),
            Decision = case Duration > ?ERROR_THRESHOLD_MS of
                true ->
                    rand:uniform() < 0.5;  % 50% for slow requests
                false ->
                    rand:uniform() < ?BACKGROUND_RATE  % 1% for normal
            end,
            record_decision(Decision),
            Decision
    end.

%% @private Simple probabilistic sampling strategy.
-spec probabilistic_strategy(span()) -> boolean().
probabilistic_strategy(_Span) ->
    Rate = get_sampling_rate(),
    Decision = rand:uniform() < Rate,
    record_decision(Decision),
    Decision.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Ensure the ETS table exists.
-spec ensure_table() -> ok.
ensure_table() ->
    case ets:whereis(?SAMPLING_TABLE) of
        undefined ->
            ets:new(?SAMPLING_TABLE, [named_table, set, public, {read_concurrency, true}]);
        _ ->
            ok
    end,
    ok.

%% @private Get the current strategy.
-spec get_strategy() -> sampling_strategy().
get_strategy() ->
    ensure_table(),
    case ets:lookup(?SAMPLING_TABLE, current_strategy) of
        [{_, Strategy}] -> Strategy;
        [] -> adaptive
    end.

%% @private Record a sampled span.
-spec record_sampled() -> ok.
record_sampled() ->
    ensure_table(),
    ets:update_counter(?SAMPLING_TABLE, total_spans, {2, 1}, {total_spans, 0}),
    ets:update_counter(?SAMPLING_TABLE, sampled_spans, {2, 1}, {sampled_spans, 0}),
    ok.

%% @private Record a sampling decision.
-spec record_decision(boolean()) -> ok.
record_decision(true) -> record_sampled();
record_decision(false) ->
    ensure_table(),
    ets:update_counter(?SAMPLING_TABLE, total_spans, {2, 1}, {total_spans, 0}),
    ok.

%% @private Determine if a span is high-value.
-spec is_high_value_span(span()) -> boolean().
is_high_value_span(Span) ->
    has_error(Span) orelse is_slow_request(Span) orelse is_critical_service(Span).

%% @private Check if span has an error.
-spec has_error(span()) -> boolean().
has_error(Span) ->
    case maps:get(status, Span, undefined) of
        {error, _, _} -> true;
        error -> true;
        undefined -> false;
        Status when Status =/= ok -> true;
        _ -> false
    end.

%% @private Check if span is a slow request.
-spec is_slow_request(span()) -> boolean().
is_slow_request(Span) ->
    Duration = get_span_duration(Span),
    Duration > ?ERROR_THRESHOLD_MS.

%% @private Get span duration in milliseconds.
-spec get_span_duration(span()) -> non_neg_integer().
get_span_duration(Span) ->
    StartTime = maps:get(start_time, Span, 0),
    EndTime = maps:get(end_time, Span, StartTime),
    max(0, EndTime - StartTime).

%% @private Check if span is from a critical service.
-spec is_critical_service(span()) -> boolean().
is_critical_service(Span) ->
    Name = maps:get(name, Span, <<>>),
    Attrs = maps:get(attributes, Span, #{}),
    ServiceName = maps:get(<<"service.name">>, Attrs, <<>>),

    %% Critical services list
    CriticalNames = [<<"payment">>, <<"auth">>, <<"transaction">>, <<"billing">>],
    CriticalServices = [<<"payment-service">>, <<"auth-service">>, <<"billing-service">>],

    lists:any(fun(C) -> binary:match(Name, C) =/= nomatch end, CriticalNames) orelse
    lists:any(fun(S) -> binary:match(ServiceName, S) =/= nomatch end, CriticalServices).

%% @private Classify span priority.
-spec classify_span_priority(span()) -> high | medium | low.
classify_span_priority(Span) ->
    case has_error(Span) of
        true -> high;
        false ->
            case is_critical_service(Span) of
                true -> medium;
                false ->
                    case is_slow_request(Span) of
                        true -> medium;
                        false -> low
                    end
            end
    end.

%% @private Calculate adaptive sampling rate based on traffic.
-spec calculate_adaptive_rate(non_neg_integer()) -> float().
calculate_adaptive_rate(Total) ->
    %% Adapt based on recent traffic volume
    case Total of
        N when N < 100 ->
            %% Low traffic: sample 100% for better observability
            1.0;
        N when N < 1000 ->
            %% Medium traffic: sample 50%
            0.5;
        N when N < 10000 ->
            %% High traffic: sample 10%
            0.1;
        _ ->
            %% Very high traffic: sample 1%
            ?BACKGROUND_RATE
    end.
