%%%-------------------------------------------------------------------
%%% @doc ln_join - Join policy implementation for parallel branches.
%%%
%%% Handles different join policies: all, n_of_m, first, sync_merge.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_join).

%% API
-export([init/2]).
-export([branch_complete/3]).
-export([cancel/2]).
-export([handle_cancelled_branch/2]).
-export([is_complete/1]).
-export([pending_count/1]).
-export([completed_count/1]).

%% Types
-export_type([state/0, policy/0, outcome/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type join_id() :: reference().
-type branch_id() :: term().

-type policy() :: all
                 | {n_of_m, pos_integer()}
                 | first
                 | sync_merge.

-type cancelled_policy() :: cancelled_counts_as_complete
                           | cancelled_fails_join
                           | cancelled_ignored.

-type branch_status() :: completed | failed | cancelled.

-record(branch_result, {
    id :: branch_id(),
    status :: branch_status(),
    value :: term() | undefined
}).

-type branch_result() :: #branch_result{}.

-record(join_state, {
    id :: join_id(),
    policy :: policy(),
    cancelled_policy :: cancelled_policy(),
    total_branches :: non_neg_integer(),
    completed :: [branch_result()],
    failed :: [branch_result()],
    cancelled :: [branch_result()],
    start_time :: integer()
}).

-opaque state() :: #join_state{}.

-type outcome() :: {continue, [term()]}
                 | {wait, state()}
                 | {join_failed, term()}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Initialize a new join state.
-spec init(policy(), pos_integer()) -> state().
init(Policy, BranchCount) ->
    #join_state{
        id = make_ref(),
        policy = Policy,
        cancelled_policy = cancelled_fails_join,
        total_branches = BranchCount,
        completed = [],
        failed = [],
        cancelled = [],
        start_time = erlang:monotonic_time(millisecond)
    }.

%% @doc Register a branch completion.
-spec branch_complete(branch_id(), {ok, term()} | {error, term()}, state()) ->
    outcome().
branch_complete(BranchId, {ok, Value}, #join_state{policy = Policy, cancelled = Cancelled,
                                                   completed = Completed, failed = Failed,
                                                   total_branches = Total} = State) ->
    BranchRes = #branch_result{id = BranchId, status = completed, value = Value},
    NewCompleted = [BranchRes | Completed],
    check_join_complete(Policy, NewCompleted, Failed, Cancelled, Total, State, []);
branch_complete(BranchId, {error, Reason}, #join_state{policy = Policy, cancelled = Cancelled,
                                                        completed = Completed, failed = Failed,
                                                        total_branches = Total} = State) ->
    BranchRes = #branch_result{id = BranchId, status = failed, value = Reason},
    NewFailed = [BranchRes | Failed],
    %% For sync_merge, failures are collected, for others it's a failure
    case Policy of
        sync_merge ->
            check_join_complete(sync_merge, Completed, NewFailed, Cancelled, Total, State, []);
        _ ->
            {join_failed, {branch_failed, BranchId, Reason}}
    end.

%% @doc Cancel pending branches.
-spec cancel([branch_id()], state()) -> {cancel_pending, [branch_id()], state()}
                                      | {join_complete, [term()]}
                                      | {join_failed, term()}.
cancel(BranchIds, #join_state{policy = Policy, completed = Completed, failed = Failed,
                               cancelled = Cancelled, total_branches = Total} = State) ->
    NewCancelled = lists:map(fun(Id) -> #branch_result{id = Id, status = cancelled, value = undefined} end, BranchIds),
    AllCancelled = Cancelled ++ NewCancelled,
    %% Check if join can complete with cancelled branches
    case Policy of
        sync_merge ->
            {join_complete, extract_results(Completed ++ AllCancelled)};
        _ ->
            NewState = State#join_state{cancelled = AllCancelled},
            check_join_complete(Policy, Completed, Failed, AllCancelled, Total, NewState, [])
    end.

%% @doc Handle a cancelled branch notification.
-spec handle_cancelled_branch(branch_id(), state()) -> state().
handle_cancelled_branch(BranchId, #join_state{cancelled = Cancelled} = State) ->
    BranchRes = #branch_result{id = BranchId, status = cancelled, value = undefined},
    State#join_state{cancelled = [BranchRes | Cancelled]}.

%% @doc Check if join is complete.
-spec is_complete(state()) -> boolean().
is_complete(#join_state{policy = Policy, completed = Completed, failed = Failed,
                         cancelled = Cancelled, total_branches = Total}) ->
    TotalDone = length(Completed) + length(Failed) + length(Cancelled),
    case Policy of
        all -> length(Completed) =:= Total;
        {n_of_m, N} -> length(Completed) >= N;
        first -> length(Completed) >= 1;
        sync_merge -> TotalDone =:= Total
    end.

%% @doc Get pending branch count.
-spec pending_count(state()) -> non_neg_integer().
pending_count(#join_state{completed = Completed, failed = Failed, cancelled = Cancelled, total_branches = Total}) ->
    Total - length(Completed) - length(Failed) - length(Cancelled).

%% @doc Get completed branch count.
-spec completed_count(state()) -> non_neg_integer().
completed_count(#join_state{completed = Completed}) ->
    length(Completed).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @doc Check if join is complete based on policy.
check_join_complete(all, Completed, [], [], Total, State, _Extra) when length(Completed) =:= Total ->
    {continue, extract_results(Completed)};
check_join_complete(all, _Completed, _Failed, [_|_], _Total, _State, _Extra) ->
    {join_failed, branch_cancelled};
check_join_complete(all, _, _, _, _, State, _Extra) ->
    {wait, State};

check_join_complete({n_of_m, N}, Completed, [], [], Total, State, _Extra) when length(Completed) >= N ->
    {continue, lists:sublist(extract_results(Completed), N)};
check_join_complete({n_of_m, N}, Completed, [], Cancelled, Total, State, _Extra) ->
    %% With cancelled, check if we can still reach N
    Possible = length(Completed) + (Total - length(Completed) - length(Cancelled)),
    case Possible >= N of
        true -> {wait, State};
        false -> {join_failed, cannot_reach_quorum}
    end;
check_join_complete({n_of_m, _}, _, _, _, _, State, _Extra) ->
    {wait, State};

check_join_complete(first, Completed, [], [], _Total, State, _Extra) when length(Completed) >= 1 ->
    {continue, [hd(Completed)]};
check_join_complete(first, _Completed, _Failed, _Cancelled, _Total, State, _Extra) ->
    {wait, State};

check_join_complete(sync_merge, Completed, Failed, Cancelled, Total, State, _Extra) ->
    TotalDone = length(Completed) + length(Failed) + length(Cancelled),
    case TotalDone =:= Total of
        true ->
            {continue, #{
                success => extract_results(Completed),
                failure => Failed,
                cancelled => Cancelled
            }};
        false ->
            {wait, State}
    end.

%% @doc Extract result values from branch results.
extract_results(BranchResults) ->
    [Value || #branch_result{value = Value} <- BranchResults].

%% @doc Extract failure reasons.
extract_failures(BranchResults) ->
    [{Id, Value} || #branch_result{id = Id, value = Value} <- BranchResults].
