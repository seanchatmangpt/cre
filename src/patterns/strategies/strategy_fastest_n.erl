%% -*- erlang -*-
%%%% @doc strategy_fastest_n - Fastest N completion strategy.
%%
%% This strategy waits for N branches to complete and selects the N
%% fastest based on completion time. Useful for competitive scenarios
%% where you want the best performers.
%%
%% @end
%% -------------------------------------------------------------------

-module(strategy_fastest_n).
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
%% Records
%%====================================================================

-record(fastest_n_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    completed = [] :: [{pos_integer(), integer(), term()}],  %% {Index, Time, Result}
    start_time :: integer()
}).

%%====================================================================
%% Types
%%====================================================================

-type fastest_n_state() :: #fastest_n_state{}.
-export_type([fastest_n_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Initializes the fastest-n strategy.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(pos_integer(), pos_integer()) -> {ok, fastest_n_state()}.

init(N, M) when N =< M, N > 0, M > 0 ->
    StartTime = erlang:monotonic_time(millisecond),
    {ok, #fastest_n_state{
        n = N,
        m = M,
        start_time = StartTime
    }}.

%%--------------------------------------------------------------------
%% @doc Determines if the pattern should complete.
%%
%% @end
%%--------------------------------------------------------------------
-spec should_complete(fastest_n_state(), map()) -> boolean().

should_complete(#fastest_n_state{n = N, completed = Completed}, _Context) ->
    length(Completed) >= N.

%%--------------------------------------------------------------------
%% @doc Called when a branch completes.
%%
%% Tracks completion time for ranking.
%%
%% @end
%%--------------------------------------------------------------------
-spec on_branch_complete(fastest_n_state(), {pos_integer(), term()}) ->
          fastest_n_state().

on_branch_complete(State = #fastest_n_state{
                    start_time = StartTime,
                    completed = Completed},
                   {BranchIndex, Result}) ->
    Time = erlang:monotonic_time(millisecond) - StartTime,
    State#fastest_n_state{
        completed = [{BranchIndex, Time, Result} | Completed]
    }.

%%--------------------------------------------------------------------
%% @doc Gets the final result.
%%
%% Returns results sorted by completion time (fastest first).
%%
%% @end
%%--------------------------------------------------------------------
-spec get_result(fastest_n_state()) -> {ok, [{pos_integer(), term()}]}.

get_result(#fastest_n_state{completed = Completed, n = N}) ->
    %% Sort by completion time
    Sorted = lists:keysort(2, Completed),
    %% Take N fastest
    FastestN = lists:sublist(Sorted, N),
    %% Extract {Index, Result} tuples
    Results = [{I, R} || {I, _T, R} <- FastestN],
    {ok, Results}.
