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
%% Records
%%====================================================================

-record(quality_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    completed = [] :: [{pos_integer(), number(), term()}],
    min_quality :: number() | undefined
}).

%%====================================================================
%% Exports
%%====================================================================

-export([init/2]).
-export([init/3]).
-export([should_complete/2]).
-export([on_branch_complete/2]).
-export([get_result/1]).

%%====================================================================
%% Types
%%====================================================================

-type quality_state() :: #quality_state{}.
-export_type([quality_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec init(pos_integer(), pos_integer()) -> {ok, quality_state()}.
init(N, M) when N =< M, N > 0, M > 0 ->
    {ok, #quality_state{
        n = N,
        m = M,
        min_quality = undefined
    }}.

-spec init(pos_integer(), pos_integer(), [{atom(), term()}]) -> {ok, quality_state()}.
init(N, M, Options) when N =< M, N > 0, M > 0 ->
    MinQuality = proplists:get_value(min_quality, Options),
    {ok, #quality_state{
        n = N,
        m = M,
        min_quality = MinQuality
    }}.

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

-spec on_branch_complete(quality_state(), {pos_integer(), term()}) ->
          quality_state().
on_branch_complete(State = #quality_state{completed = Completed},
                   {BranchIndex, {QualityScore, Result}})
  when is_number(QualityScore) ->
    State#quality_state{
        completed = [{BranchIndex, QualityScore, Result} | Completed]
    };
on_branch_complete(State, {BranchIndex, Result}) ->
    on_branch_complete(State, {BranchIndex, {0.5, Result}}).

-spec get_result(quality_state()) -> {ok, [{pos_integer(), term()}]}.
get_result(#quality_state{completed = Completed, n = N, min_quality = MinQuality}) ->
    Sorted = lists:reverse(lists:keysort(2, Completed)),
    Filtered = case MinQuality of
        undefined -> Sorted;
        Min -> [{I, S, R} || {I, S, R} <- Sorted, S >= Min]
    end,
    BestN = lists:sublist(Filtered, N),
    Results = [{I, R} || {I, _S, R} <- BestN],
    {ok, Results}.
