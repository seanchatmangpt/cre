%% -*- erlang -*-
%%%% @doc strategy_quality - Quality-based completion strategy.
%%
%% This strategy selects branches based on quality scores. Each branch
%% produces a result with an associated quality metric, and the strategy
%% selects the highest-quality completions.
%%
%% @end
%% -------------------------------------------------------------------

-module(strategy_quality).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

%% Strategy behavior
-export([init/2]).
-export([should_complete/2]).
-export([on_branch_complete/2]).
-export([get_result/1]).

%%====================================================================
==== Records
%%====================================================================

-record(quality_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    completed = [] :: [{pos_integer(), number(), term()}],  %% {Index, Score, Result}
    min_quality :: number() | undefined
}).

%%====================================================================
%% Types
%%====================================================================

-type quality_state() :: #quality_state{}.
-export_type([quality_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the quality-based strategy.
%%
%% Options can include:
%% - {min_quality, Number} - Minimum quality score to accept
%%
%% @end
%%--------------------------------------------------------------------
-spec init(pos_integer(), pos_integer()) -> {ok, quality_state()}.

init(N, M) when N =< M, N > 0, M > 0 ->
    {ok, #quality_state{
        n = N,
        m = M,
        min_quality = undefined
    }}.

%%--------------------------------------------------------------------
%% @doc Initializes with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(pos_integer(), pos_integer(), [{atom(), term()}]) -> {ok, quality_state()}.

init(N, M, Options) when N =< M, N > 0, M > 0 ->
    MinQuality = proplists:get_value(min_quality, Options),
    {ok, #quality_state{
        n = N,
        m = M,
        min_quality = MinQuality
    }}.

%%--------------------------------------------------------------------
%% @doc Determines if the pattern should complete.
%%
%% Completes when N branches with acceptable quality have finished.
%%
%% @end
%%--------------------------------------------------------------------
-spec should_complete(quality_state(), map()) -> boolean().

should_complete(#quality_state{
                    n = N,
                    completed = Completed,
                    min_quality = MinQuality}, _Context) ->
    Acceptable = case MinQuality of
        undefined ->
            Completed;
        Min ->
            [{I, S, R} || {I, S, R} <- Completed, S >= Min]
    end,
    length(Acceptable) >= N.

%%--------------------------------------------------------------------
%% @doc Called when a branch completes.
%%
%% Expects result format: {QualityScore, ActualResult}
%%
%% @end
%%--------------------------------------------------------------------
-spec on_branch_complete(quality_state(), {pos_integer(), term()}) ->
          quality_state().

on_branch_complete(State = #quality_state{completed = Completed},
                   {BranchIndex, {QualityScore, Result}})
  when is_number(QualityScore) ->
    State#quality_state{
        completed = [{BranchIndex, QualityScore, Result} | Completed]
    };
on_branch_complete(State, {BranchIndex, Result}) ->
    %% Default quality score of 0.5 for unqualified results
    on_branch_complete(State, {BranchIndex, {0.5, Result}}).

%%--------------------------------------------------------------------
%% @doc Gets the final result.
%%
%% Returns the N highest-quality results.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_result(quality_state()) -> {ok, [{pos_integer(), term()}]}.

get_result(#quality_state{completed = Completed, n = N, min_quality = MinQuality}) ->
    %% Sort by quality score descending
    Sorted = lists:reverse(lists:keysort(2, Completed)),

    %% Filter by minimum quality if set
    Filtered = case MinQuality of
        undefined -> Sorted;
        Min -> [{I, S, R} || {I, S, R} <- Sorted, S >= Min]
    end,

    %% Take N highest quality
    BestN = lists:sublist(Filtered, N),

    %% Extract {Index, Result} tuples
    Results = [{I, R} || {I, _S, R} <- BestN],
    {ok, Results}.
