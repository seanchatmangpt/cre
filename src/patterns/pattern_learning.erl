%% -*- erlang -*-
%%%% @doc pattern_learning - Learning backend for adaptive patterns.
%%
%% This module provides the ETS backend and metrics recording for pattern
%% learning. Patterns can learn from execution history using exponential
%% moving averages (EMA) for performance metrics.
%%
%% <h3>Learning Algorithm: Exponential Moving Average</h3>
%%
%% <pre>
%% EMA_new = EMA_old + Alpha * (NewValue - EMA_old)
%% </pre>
%%
%% Where Alpha is the smoothing factor (default 0.2):
%% <ul>
%%   <li>Alpha = 0.1-0.3 (default 0.2)</li>
%%   <li>Gives 90% weight to history, 10% to new value</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(pattern_learning).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

%% Learning API
-export([start_link/0]).
-export([init_schema/0]).
-export([record_branch_metric/4]).
-export([get_branch_metrics/2]).
-export([update_branch_metrics/4]).
-export([reset_metrics/1]).
-export([get_all_metrics/0]).
-export([calculate_ema/3]).
-export([has_converged/3]).
-export([get_best_branch/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Branch Performance Metrics Record
%%
%% Tracks performance metrics for pattern branches using EMA.
%%--------------------------------------------------------------------
-record(branch_metrics, {
    key :: {binary(), binary(), term()},  %% {PatternId, WorkflowId, BranchId}
    pattern_id :: binary(),
    workflow_id :: binary(),
    branch_id :: term(),
    completion_count = 0 :: non_neg_integer(),
    total_duration_us = 0 :: non_neg_integer(),
    ema_duration_us = 0.0 :: float(),        %% Exponential moving average
    success_count = 0 :: non_neg_integer(),
    failure_count = 0 :: non_neg_integer(),
    last_updated :: integer()
}).

%%--------------------------------------------------------------------
%% @doc Pattern Learning Configuration
%%--------------------------------------------------------------------
-record(learning_config, {
    alpha = 0.2 :: float(),                  %% Smoothing factor (0.1-0.3)
    convergence_threshold = 0.01 :: float(), %% EMA change threshold
    min_samples = 10 :: pos_integer()        %% Minimum samples for convergence
}).

%%====================================================================
%% Types
%%====================================================================

-type branch_metrics() :: #branch_metrics{}.
-type learning_config() :: #learning_config{}.
-type pattern_id() :: binary().
-type workflow_id() :: binary().
-type branch_id() :: term().

-export_type([branch_metrics/0, learning_config/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the pattern learning service.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Initializes the ETS schema for pattern learning.
%%
%% Creates the branch_metrics table.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_schema() -> ok | {error, term()}.

init_schema() ->
    Node = node(),

    %% Start Mnesia if not running
    _ = case mnesia:start() of
        ok -> ok;
        {error, {already_started, _}} -> ok
    end,

    %% Create branch_metrics table
    MetricsAttrs = record_info(fields, branch_metrics),
    MetricsDef = [
        {attributes, MetricsAttrs},
        {ram_copies, [Node]},
        {type, set}
    ],

    case mnesia:create_table(branch_metrics, MetricsDef) of
        {atomic, ok} ->
            ok;
        {aborted, {already_exists, branch_metrics}} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Records branch execution metrics.
%%
%% @param PatternId Pattern identifier
%% @param WorkflowId Workflow instance identifier
%% @param BranchId Branch identifier
%% @param DurationUs Execution duration in microseconds
%%
%% @end
%%--------------------------------------------------------------------
-spec record_branch_metric(pattern_id(), workflow_id(), branch_id(), non_neg_integer()) ->
          ok | {error, term()}.

record_branch_metric(PatternId, WorkflowId, BranchId, DurationUs) ->
    gen_server:call(?MODULE, {record_metric, PatternId, WorkflowId, BranchId, DurationUs}).

%%--------------------------------------------------------------------
%% @doc Gets metrics for a specific branch.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_branch_metrics(pattern_id(), workflow_id()) ->
          {ok, [branch_metrics()]} | {error, term()}.

get_branch_metrics(PatternId, WorkflowId) ->
    Pattern = #branch_metrics{
        key = {PatternId, WorkflowId, '_'},
        pattern_id = PatternId,
        workflow_id = WorkflowId,
        branch_id = '_',
        completion_count = '_',
        total_duration_us = '_',
        ema_duration_us = '_',
        success_count = '_',
        failure_count = '_',
        last_updated = '_'
    },
    case mnesia:dirty_match_object(branch_metrics, Pattern) of
        [] -> {ok, []};
        Metrics -> {ok, Metrics}
    end.

%%--------------------------------------------------------------------
%% @doc Updates branch metrics with new execution data.
%%
%% Calculates and stores new EMA values.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_branch_metrics(pattern_id(), workflow_id(), branch_id(),
                           {success | failure, non_neg_integer()}) ->
          ok | {error, term()}.

update_branch_metrics(PatternId, WorkflowId, BranchId, {Result, DurationUs}) ->
    gen_server:call(?MODULE, {update_metrics, PatternId, WorkflowId, BranchId, Result, DurationUs}).

%%--------------------------------------------------------------------
%% @doc Resets all metrics for a pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_metrics(pattern_id()) -> ok.

reset_metrics(PatternId) ->
    gen_server:call(?MODULE, {reset_metrics, PatternId}).

%%--------------------------------------------------------------------
%% @doc Gets all metrics from the database.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_all_metrics() -> {ok, [branch_metrics()]}.

get_all_metrics() ->
    gen_server:call(?MODULE, get_all_metrics).

%%--------------------------------------------------------------------
%% @doc Calculates Exponential Moving Average.
%%
%% Formula: EMA_new = EMA_old + Alpha * (NewValue - EMA_old)
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_ema(float(), number(), float()) -> float().

calculate_ema(CurrentEMA, NewValue, Alpha) when is_float(CurrentEMA),
                                              is_number(NewValue),
                                              is_float(Alpha) ->
    CurrentEMA + Alpha * (NewValue - CurrentEMA).

%%--------------------------------------------------------------------
%% @doc Checks if EMA has converged.
%%
%% Convergence is reached when:
%% - Minimum samples have been collected
%% - Recent EMA change is below threshold
%%
%% @end
%%--------------------------------------------------------------------
-spec has_converged([number()], float(), pos_integer()) -> boolean().

has_converged(History, Threshold, MinSamples) when is_list(History),
                                                  length(History) >= MinSamples ->
    Recent = lists:sublist(History, 5),
    Max = lists:max(Recent),
    Min = lists:min(Recent),
    (Max - Min) / (Max + Min + 1.0e-10) < Threshold;
has_converged(_, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Gets the best performing branch for a pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_best_branch(pattern_id(), workflow_id()) ->
          {ok, branch_id()} | {error, term()}.

get_best_branch(PatternId, WorkflowId) ->
    case get_branch_metrics(PatternId, WorkflowId) of
        {ok, []} ->
            {error, no_metrics};
        {ok, Metrics} ->
            %% Find branch with lowest EMA duration
            Best = lists:foldl(
                fun(#branch_metrics{ema_duration_us = EMA, branch_id = Branch}, Acc) ->
                        case Acc of
                            undefined -> {Branch, EMA};
                            {BestBranch, BestEMA} when EMA < BestEMA -> {Branch, EMA};
                            _ -> Acc
                        end
                end,
                undefined,
                Metrics
            ),
            case Best of
                undefined -> {error, no_valid_branch};
                {BranchId, _} -> {ok, BranchId}
            end
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, #learning_config{}}.

init([]) ->
    init_schema(),
    {ok, #learning_config{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #learning_config{}) ->
          {reply, term(), #learning_config{}}.

handle_call({record_metric, PatternId, WorkflowId, BranchId, DurationUs}, _From, Config) ->
    Key = {PatternId, WorkflowId, BranchId},
    Now = erlang:system_time(millisecond),

    Transaction = fun() ->
        case mnesia:read(branch_metrics, Key) of
            [] ->
                %% First recording
                Record = #branch_metrics{
                    key = Key,
                    pattern_id = PatternId,
                    workflow_id = WorkflowId,
                    branch_id = BranchId,
                    completion_count = 1,
                    total_duration_us = DurationUs,
                    ema_duration_us = float(DurationUs),
                    success_count = 1,
                    failure_count = 0,
                    last_updated = Now
                },
                mnesia:write(Record);
            [Existing] ->
                %% Update with new EMA
                Alpha = Config#learning_config.alpha,
                NewEMA = calculate_ema(Existing#branch_metrics.ema_duration_us,
                                       DurationUs, Alpha),
                Record = Existing#branch_metrics{
                    completion_count = Existing#branch_metrics.completion_count + 1,
                    total_duration_us = Existing#branch_metrics.total_duration_us + DurationUs,
                    ema_duration_us = NewEMA,
                    success_count = Existing#branch_metrics.success_count + 1,
                    last_updated = Now
                },
                mnesia:write(Record)
        end,
        ok
    end,

    Result = case mnesia:transaction(Transaction) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end,
    {reply, Result, Config};

handle_call({update_metrics, PatternId, WorkflowId, BranchId, success, DurationUs}, _From, Config) ->
    record_branch_metric(PatternId, WorkflowId, BranchId, DurationUs),
    {reply, ok, Config};

handle_call({update_metrics, PatternId, WorkflowId, BranchId, failure, _DurationUs}, _From, Config) ->
    Key = {PatternId, WorkflowId, BranchId},
    Now = erlang:system_time(millisecond),

    Transaction = fun() ->
        case mnesia:read(branch_metrics, Key) of
            [] ->
                Record = #branch_metrics{
                    key = Key,
                    pattern_id = PatternId,
                    workflow_id = WorkflowId,
                    branch_id = BranchId,
                    completion_count = 0,
                    failure_count = 1,
                    last_updated = Now
                },
                mnesia:write(Record);
            [Existing] ->
                Record = Existing#branch_metrics{
                    failure_count = Existing#branch_metrics.failure_count + 1,
                    last_updated = Now
                },
                mnesia:write(Record)
        end,
        ok
    end,

    Result = case mnesia:transaction(Transaction) of
        {atomic, ok} -> ok;
        {aborted, Reason} -> {error, Reason}
    end,
    {reply, Result, Config};

handle_call({reset_metrics, PatternId}, _From, Config) ->
    Pattern = #branch_metrics{key = {PatternId, '_', '_'}, _ = '_'},
    lists:foreach(
        fun(Record) ->
            mnesia:dirty_delete_object(branch_metrics, Record)
        end,
        mnesia:dirty_match_object(branch_metrics, Pattern)
    ),
    {reply, ok, Config};

handle_call(get_all_metrics, _From, Config) ->
    Metrics = mnesia:dirty_match_object(branch_metrics, #branch_metrics{_ = '_'}),
    {reply, {ok, Metrics}, Config};

handle_call(_Request, _From, Config) ->
    {reply, {error, unknown_request}, Config}.

%% @private
-spec handle_cast(term(), #learning_config{}) -> {noreply, #learning_config{}}.

handle_cast(_Msg, Config) ->
    {noreply, Config}.

%% @private
-spec handle_info(term(), #learning_config{}) -> {noreply, #learning_config{}}.

handle_info(_Info, Config) ->
    {noreply, Config}.

%% @private
-spec code_change(term(), #learning_config{}, term()) -> {ok, #learning_config{}}.

code_change(_OldVsn, Config, _Extra) ->
    {ok, Config}.

%% @private
-spec terminate(term(), #learning_config{}) -> ok.

terminate(_Reason, _Config) ->
    ok.
