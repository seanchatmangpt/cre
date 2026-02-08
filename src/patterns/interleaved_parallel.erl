%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc Interleaved Parallel Routing Pattern (WCP-17) for YAWL.
%%
%% This module implements the Interleaved Parallel Routing pattern
%% using a mutex token pattern for managing fair round-robin execution
%% of multiple branches where only one branch can be active at a time.
%%
%% The pattern ensures:
%% - Only one branch has the "token" (is active) at any time
%% - Branches take turns in a fair, round-robin manner
%% - All branches complete before synchronization
%%
%% Reference: workflow_patterns_2003.pdf, section 6.5 (page 28)
%%
%% @end
%% -------------------------------------------------------------------

-module(interleaved_parallel).
-moduledoc """
Interleaved Parallel Routing Pattern (WCP-17) using mutex token pattern.

This module provides a functional API for managing interleaved parallel
routing where N parallel branches execute but only one can be active at a time.
Branches take turns holding the "mutex token" ensuring fair execution.

## Pattern Description

The Interleaved Parallel Routing pattern enables fair execution of multiple
concurrent branches by processing them in a round-robin fashion. Only one
branch holds the "mutex token" at any given time, ensuring mutual exclusion
while maintaining controlled progress.

## API Functions

### interleave_start/2
Initializes N parallel branches with the first branch holding the token.

### interleave_next/2
Switches the token from the current active branch to the next branch
in round-robin order.

### interleave_complete/3
Marks the current branch as complete and passes the token to the next
pending branch.

### interleave_sync/1
Synchronizes when all interleaved branches have completed.

### active_branch/2
Queries which branch currently holds the mutex token.

## Examples

```erlang
%% Start 3 interleaved branches
State0 = interleaved_parallel:interleave_start(#{}, 3).

%% Check which branch is active (should be branch 1)
{ok, Branch1} = interleaved_parallel:active_branch(State0, #{}).

%% Complete branch 1, pass token to branch 2
State1 = interleaved_parallel:interleave_complete(State0, #{}, 1).

%% Check active branch now
{ok, Branch2} = interleaved_parallel:active_branch(State1, #{}).

%% Switch to next branch explicitly
State2 = interleaved_parallel:interleave_next(State1, #{}).

%% Complete remaining branches
State3 = interleaved_parallel:interleave_complete(State2, #{}, 2),
State4 = interleaved_parallel:interleave_complete(State3, #{}, 3).

%% Synchronize when all complete
ok = interleaved_parallel:interleave_sync(State4).
```

## Mutex Token Pattern

The state contains:
- `active_branch`: Which branch currently holds the token
- `pending_branches`: List of branches waiting for the token
- `completed_branches`: List of branches that have completed
- `total_branches`: Total number of branches

Only the active branch can execute; others wait their turn.
""".

%%====================================================================
%% API Exports
%%====================================================================

-export([
    interleave_start/2,
    interleave_next/2,
    interleave_complete/2,
    interleave_complete/3,
    interleave_sync/1,
    active_branch/2
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-record(interleave_state, {
    active_branch :: undefined | pos_integer(),
    pending_branches :: [pos_integer()],
    completed_branches :: [pos_integer()],
    total_branches :: pos_integer(),
    cycle_count = 0 :: non_neg_integer()
}).

-type interleave_state() :: #interleave_state{}.
-type context() :: map().

%% Export types
-export_type([interleave_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts N parallel branches with only one active at a time.
%%
%% Initializes the interleaved routing with N branches. Branch 1 receives
%% the initial mutex token, while other branches wait in the pending queue.
%%
%% @param Context Execution context map (can be empty for basic usage).
%% @param N Number of parallel branches (minimum 2).
%% @return New interleave_state with branch 1 active.
%%
%% @end
%%--------------------------------------------------------------------
-spec interleave_start(Context :: context(), N :: pos_integer()) -> interleave_state().

interleave_start(_Context, N) when is_integer(N), N >= 2 ->
    #interleave_state{
        active_branch = 1,
        pending_branches = lists:seq(2, N),
        completed_branches = [],
        total_branches = N,
        cycle_count = 0
    };
interleave_start(_Context, N) when is_integer(N) ->
    error({invalid_branch_count, N, minimum_is_2}).

%%--------------------------------------------------------------------
%% @doc Switches the mutex token to the next branch.
%%
%% Moves the token from the current active branch to the next branch
%% in round-robin order. Unlike interleave_complete/3, this does NOT
%% mark the current branch as complete - it merely passes the token.
%%
%% @param State Current interleave_state.
%% @param Context Execution context map.
%% @return Updated interleave_state with next branch active.
%%
%% @end
%%--------------------------------------------------------------------
-spec interleave_next(State :: interleave_state(), Context :: context()) -> interleave_state().

interleave_next(#interleave_state{active_branch = undefined} = State, _Context) ->
    State;
interleave_next(#interleave_state{active_branch = Active,
                                   pending_branches = [],
                                   completed_branches = Completed,
                                   total_branches = Total,
                                   cycle_count = Cycle} = State, _Context) ->
    %% No pending branches - check if all are complete
    case lists:usort(Completed) of
        Sorted when length(Sorted) =:= Total ->
            %% All branches complete
            State#interleave_state{
                completed_branches = Sorted,
                active_branch = undefined
            };
        _ ->
            %% Cycle back to incomplete branches
            Incomplete = [B || B <- lists:seq(1, Total),
                              not lists:member(B, Completed)],
            case Incomplete of
                [Next | _Rest] when Next =:= Active ->
                    %% Current branch is the only incomplete one, stay there
                    State;
                [Next | Rest] ->
                    %% Move to next incomplete branch
                    State#interleave_state{
                        active_branch = Next,
                        pending_branches = Rest,
                        cycle_count = Cycle + 1
                    };
                [] ->
                    State
            end
    end;
interleave_next(#interleave_state{active_branch = _Active,
                                   pending_branches = [Next | Rest],
                                   cycle_count = Cycle} = State, _Context) ->
    %% Move to next pending branch (current active not added back to pending)
    State#interleave_state{
        active_branch = Next,
        pending_branches = Rest,
        cycle_count = Cycle + 1
    }.

%%--------------------------------------------------------------------
%% @doc Marks the currently active branch as complete and passes token to next branch.
%%
%% Completes the active branch and passes the mutex token to the next
%% pending branch in round-robin order.
%%
%% @param State Current interleave_state.
%% @param Context Execution context map.
%% @return Updated interleave_state with next branch active.
%%
%% @end
%%--------------------------------------------------------------------
-spec interleave_complete(State :: interleave_state(), Context :: context()) ->
          interleave_state().

interleave_complete(#interleave_state{active_branch = undefined} = State, _Context) ->
    State;
interleave_complete(#interleave_state{active_branch = Active} = State, Context) ->
    %% Mark the active branch as complete and move to next
    interleave_complete(State, Context, Active).

%%--------------------------------------------------------------------
%% @doc Marks a branch as complete and passes token to next branch.
%%
%% The specified branch is marked as completed (if not already) and
%% the mutex token is passed to the next pending branch. This is the
%% primary way to advance through interleaved branches.
%%
%% @param State Current interleave_state.
%% @param Context Execution context map.
%% @param BranchId Branch to mark as complete (must be active branch).
%% @return Updated interleave_state with next branch active.
%%
%% @end
%%--------------------------------------------------------------------
-spec interleave_complete(State :: interleave_state(), Context :: context(), BranchId :: pos_integer()) ->
          interleave_state().

interleave_complete(#interleave_state{active_branch = Active,
                                     completed_branches = Completed} = State, Context, BranchId)
    when Active =:= BranchId ->
    %% Mark active branch as complete and move to next
    NewState = State#interleave_state{
        completed_branches = [BranchId | Completed]
    },
    interleave_next(NewState, Context);
interleave_complete(#interleave_state{active_branch = Active}, _Context, BranchId) ->
    error({branch_not_active, BranchId, active_is, Active}).

%%--------------------------------------------------------------------
%% @doc Synchronizes when all interleaved branches are complete.
%%
%% Blocks until all branches have been marked as complete.
%% Returns `ok` when synchronization is successful.
%% Throws an error if not all branches are complete.
%%
%% @param State Current interleave_state.
%% @return ok if all branches complete, error otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec interleave_sync(State :: interleave_state()) -> ok | {error, {incomplete, map()}}.

interleave_sync(#interleave_state{completed_branches = Completed,
                                  total_branches = Total}) ->
    case length(Completed) of
        Total -> ok;
        _ ->
            {error, {incomplete, #{
                <<"completed">> => length(Completed),
                <<"total">> => Total
            }}}
    end.

%%--------------------------------------------------------------------
%% @doc Queries which branch currently holds the mutex token.
%%
%% Returns the branch ID that is currently active (holds the token).
%% Returns `undefined` if no branch is active or all are complete.
%%
%% @param State Current interleave_state.
%% @param Context Execution context map.
%% @return {ok, BranchId} or {ok, undefined} if no active branch.
%%
%% @end
%%--------------------------------------------------------------------
-spec active_branch(State :: interleave_state(), Context :: context()) ->
          {ok, pos_integer() | undefined}.

active_branch(#interleave_state{active_branch = Active}, _Context) ->
    {ok, Active}.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets the current state as a map for inspection.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec get_info(State :: interleave_state()) -> map().

get_info(#interleave_state{active_branch = Active,
                            pending_branches = Pending,
                            completed_branches = Completed,
                            total_branches = Total,
                            cycle_count = Cycle}) ->
    #{
        active_branch => Active,
        pending_branches => Pending,
        completed_branches => Completed,
        total_branches => Total,
        cycle_count => Cycle,
        is_complete => length(Completed) =:= Total
    }.

%%--------------------------------------------------------------------
%% @doc Resets the state for reuse.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec reset(State :: interleave_state()) -> interleave_state().

reset(#interleave_state{total_branches = Total}) ->
    interleave_start(#{}, Total).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test basic interleave_start
interleave_start_test() ->
    State = interleave_start(#{}, 3),
    ?assertEqual(1, State#interleave_state.active_branch),
    ?assertEqual([2, 3], State#interleave_state.pending_branches),
    ?assertEqual([], State#interleave_state.completed_branches),
    ?assertEqual(3, State#interleave_state.total_branches).

%% Test invalid branch count
interleave_start_invalid_test() ->
    ?assertError({invalid_branch_count, 1, minimum_is_2}, interleave_start(#{}, 1)).

%% Test active_branch query
active_branch_test() ->
    State = interleave_start(#{}, 3),
    ?assertEqual({ok, 1}, active_branch(State, #{})).

%% Test interleave_next
interleave_next_test() ->
    State0 = interleave_start(#{}, 3),
    ?assertEqual(1, State0#interleave_state.active_branch),

    State1 = interleave_next(State0, #{}),
    ?assertEqual(2, State1#interleave_state.active_branch),
    ?assertEqual([3], State1#interleave_state.pending_branches),

    State2 = interleave_next(State1, #{}),
    ?assertEqual(3, State2#interleave_state.active_branch),
    ?assertEqual([], State2#interleave_state.pending_branches).

%% Test interleave_complete
interleave_complete_test() ->
    State0 = interleave_start(#{}, 3),
    ?assertEqual(1, State0#interleave_state.active_branch),

    State1 = interleave_complete(State0, #{}, 1),
    ?assertEqual(2, State1#interleave_state.active_branch),
    ?assertEqual([1], lists:sort(State1#interleave_state.completed_branches)),

    State2 = interleave_complete(State1, #{}, 2),
    ?assertEqual(3, State2#interleave_state.active_branch),
    ?assertEqual([1, 2], lists:sort(State2#interleave_state.completed_branches)),

    State3 = interleave_complete(State2, #{}, 3),
    ?assertEqual(undefined, State3#interleave_state.active_branch),
    ?assertEqual([1, 2, 3], lists:sort(State3#interleave_state.completed_branches)).

%% Test interleave_sync
interleave_sync_test() ->
    State0 = interleave_start(#{}, 3),
    ?assertMatch({error, {incomplete, #{<<"completed">> := 0, <<"total">> := 3}}}, interleave_sync(State0)),

    State1 = interleave_complete(State0, #{}, 1),
    ?assertMatch({error, {incomplete, #{<<"completed">> := 1, <<"total">> := 3}}}, interleave_sync(State1)),

    State2 = interleave_complete(State1, #{}, 2),
    ?assertMatch({error, {incomplete, #{<<"completed">> := 2, <<"total">> := 3}}}, interleave_sync(State2)),

    State3 = interleave_complete(State2, #{}, 3),
    ?assertEqual(ok, interleave_sync(State3)).

%% Test complete wrong branch error
interleave_complete_wrong_branch_test() ->
    State = interleave_start(#{}, 3),
    ?assertError({branch_not_active, 2, active_is, 1}, interleave_complete(State, #{}, 2)).

%% Test round-robin cycling
interleave_cycle_test() ->
    State0 = interleave_start(#{}, 3),

    %% Complete branch 1, move to 2
    State1 = interleave_complete(State0, #{}, 1),
    ?assertEqual(2, State1#interleave_state.active_branch),

    %% Move to 3 (without completing 2)
    State2 = interleave_next(State1, #{}),
    ?assertEqual(3, State2#interleave_state.active_branch),

    %% Move to 2 (since 1 is pending completion, 2 is not complete)
    State3 = interleave_next(State2, #{}),
    ?assertEqual(2, State3#interleave_state.active_branch).

%% Test get_info
get_info_test() ->
    State = interleave_start(#{}, 3),
    Info = get_info(State),
    ?assertEqual(1, maps:get(active_branch, Info)),
    ?assertEqual([2, 3], maps:get(pending_branches, Info)),
    ?assertEqual(false, maps:get(is_complete, Info)).

%% Test reset
reset_test() ->
    State0 = interleave_start(#{}, 3),
    State1 = interleave_complete(State0, #{}, 1),
    State2 = reset(State1),
    ?assertEqual(1, State2#interleave_state.active_branch),
    ?assertEqual([2, 3], State2#interleave_state.pending_branches),
    ?assertEqual([], State2#interleave_state.completed_branches).

-endif.
