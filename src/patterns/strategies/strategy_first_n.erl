%% -*- erlang -*-
%%%% @doc strategy_first_n - First N completion strategy for N-of-M patterns.
%%
%% This strategy completes when the first N branches finish, regardless
%% of their outcome. It's the simplest and fastest strategy.
%%
%% @end
%% -------------------------------------------------------------------

-module(strategy_first_n).
-author("CRE Team").

%%====================================================================
%% Records
%%====================================================================

-record(first_n_state, {
    n :: pos_integer(),
    m :: pos_integer(),
    completed = [] :: [pos_integer()],
    results = #{} :: map()
}).

%%====================================================================
%% Exports
%%====================================================================

-export([init/2]).
-export([should_complete/2]).
-export([on_branch_complete/2]).
-export([get_result/1]).

%%====================================================================
%% Types
%%====================================================================

-type first_n_state() :: #first_n_state{}.
-export_type([first_n_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec init(pos_integer(), pos_integer()) -> {ok, first_n_state()}.
init(N, M) when N =< M, N > 0, M > 0 ->
    {ok, #first_n_state{n = N, m = M}}.

-spec should_complete(first_n_state(), map()) -> boolean().
should_complete(#first_n_state{n = N, completed = Completed}, _Context) ->
    length(Completed) >= N.

-spec on_branch_complete(first_n_state(), {pos_integer(), term()}) ->
          first_n_state().
on_branch_complete(State = #first_n_state{completed = Completed, results = Results},
                 {BranchIndex, Result}) ->
    State#first_n_state{
        completed = [BranchIndex | Completed],
        results = maps:put(BranchIndex, Result, Results)
    }.

-spec get_result(first_n_state()) -> {ok, map()}.
get_result(#first_n_state{results = Results}) ->
    {ok, Results}.
