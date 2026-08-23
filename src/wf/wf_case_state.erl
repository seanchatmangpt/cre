%%%-------------------------------------------------------------------
%%% @doc wf_case_state - Per-case state management with atomic commits.
%%%
%%% This module provides atomic state commit/revert mechanisms for workflow
%%% case state management. It supports transaction-like semantics for
%%% state mutations with rollback capability on failed operations.
%%%
%%% <h3>Features</h3>
%%% <ul>
%%%   <li>Atomic state commits with automatic rollback on failure</li>
%%%   <li>State versioning with undo/redo capability</li>
%%%   <li>State persistence integration with Mnesia</li>
%%%   <li>Observable state transitions</li>
%%%   <li>Multi-operation transactions</li>
%%% </ul>
%%%
%%% <h3>Basic Usage</h3>
%%%
%%% Creating a new case state:
%%% ```erlang
%%% > State0 = wf_case_state:new(<<"case-123">>).
%%% > wf_case_state:set_data(State0, #{amount => 100}).
%%% '''
%%%
%%% Atomic commit with rollback:
%%% ```erlang
%%% > {ok, State1} = wf_case_state:atomic_update(
%%%     State0,
%%%     fun(S) -> {ok, wf_case_state:set_data(S, #{amount => 100})} end
%%% ).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(wf_case_state).

%%====================================================================
%% Exports
%%====================================================================

%% State construction
-export([new/1, new/2]).
-export([from_yawl_state/1, to_yawl_state/1]).

%% State accessors
-export([get_case_id/1, get_status/1, get_data/1]).
-export([get_version/1, get_parent/1, get_mutations/1]).

%% State mutations
-export([set_data/2, update_data/2]).
-export([set_status/2]).

%% Atomic operations
-export([atomic_update/2, atomic_transaction/3]).
-export([commit/2, rollback/2]).

%% History management
-export([history/1, history_count/1]).
-export([undo/1, redo/1, can_undo/1, can_redo/1]).

%% Persistence integration
-export([persist/2, restore/1]).
-export([checkpoint/1, restore_checkpoint/1]).

%% Export types
-export_type([
    state/0,
    mutation/0,
    transaction_result/0,
    commit_result/0,
    checkpoint/0
]).

%%====================================================================
%% Types
%%====================================================================

-type case_id() :: binary().

-type status() :: created | running | suspended | completed | cancelled | failed.

-type mutation() :: #{
    type := atom(),
    key := term(),
    old_value => term(),
    new_value => term(),
    timestamp => integer()
}.

-type state() :: #{
    case_id := case_id(),
    status := status(),
    data := map(),
    version := non_neg_integer(),
    parent => state(),
    mutations := [mutation()],
    history := [state()],
    committed := boolean(),
    dirty := boolean()
}.

-type transaction_fun() :: fun((state()) -> {ok, state()} | {error, term()}).

-type transaction_result() :: {ok, state()} | {error, term()}.

-type commit_result() :: {ok, state()} | {error, term()}.

-type checkpoint() :: #{
    state := state(),
    timestamp := integer(),
    version := non_neg_integer()
}.

%%====================================================================
%% Records
%%====================================================================

-record(persistent_state, {
    case_id :: case_id(),
    status :: status(),
    data :: map(),
    version :: non_neg_integer(),
    mutations :: [mutation()],
    checkpoint :: checkpoint() | undefined,
    updated_at :: integer()
}).

%%====================================================================
%% State Construction
%%====================================================================

%% @doc Creates a new case state with default values.
-spec new(CaseId :: case_id()) -> state().
new(CaseId) when is_binary(CaseId) ->
    #{
        case_id => CaseId,
        status => created,
        data => #{},
        version => 0,
        parent => undefined,
        mutations => [],
        history => [],
        committed => true,
        dirty => false
    }.

%% @doc Creates a new case state with initial data.
-spec new(CaseId :: case_id(), InitialData :: map()) -> state().
new(CaseId, InitialData) when is_binary(CaseId), is_map(InitialData) ->
    (new(CaseId))#{data => InitialData}.

%% @doc Converts a yawl_state to wf_case_state.
-spec from_yawl_state(yawl_state:t()) -> state().
from_yawl_state(YawlState) ->
    maps:fold(
        fun(K, V, Acc) -> Acc#{K => V} end,
        new(maps:get(case_id, YawlState, <<>>)),
        YawlState
    ).

%% @doc Converts wf_case_state to yawl_state format.
-spec to_yawl_state(state()) -> yawl_state:t().
to_yawl_state(State) ->
    YawlState = yawl_state:new(maps:get(case_id, State, <<>>)),
    YawlState#{
        status => maps:get(status, State, created),
        data => maps:get(data, State, #{}),
        timestamps => #{
            created_at => undefined,
            started_at => undefined,
            suspended_at => undefined,
            resumed_at => undefined,
            completed_at => undefined,
            cancelled_at => undefined,
            updated_at => undefined
        }
    }.

%%====================================================================
%% State Accessors
%%====================================================================

-spec get_case_id(state()) -> case_id().
get_case_id(#{case_id := CaseId}) -> CaseId.

-spec get_status(state()) -> status().
get_status(#{status := Status}) -> Status.

-spec get_data(state()) -> map().
get_data(#{data := Data}) -> Data.

-spec get_version(state()) -> non_neg_integer().
get_version(#{version := Version}) -> Version.

-spec get_parent(state()) -> state() | undefined.
get_parent(#{parent := Parent}) -> Parent.

-spec get_mutations(state()) -> [mutation()].
get_mutations(#{mutations := Mutations}) -> Mutations.

%%====================================================================
%% State Mutations
%%====================================================================

%% @doc Sets the entire data map atomically.
-spec set_data(state(), map()) -> commit_result().
set_data(State, Data) when is_map(Data) ->
    atomic_update(State, fun(S) ->
        {ok, S#{data => Data}}
    end).

%% @doc Updates data map by merging with provided map.
-spec update_data(state(), map()) -> commit_result().
update_data(State, Updates) when is_map(Updates) ->
    atomic_update(State, fun(S) ->
        #{data := CurrentData} = S,
        {ok, S#{data => maps:merge(CurrentData, Updates)}}
    end).

%% @doc Sets the status atomically.
-spec set_status(state(), status()) -> commit_result().
set_status(State, Status) when is_atom(Status) ->
    atomic_update(State, fun(S) ->
        {ok, S#{status => Status}}
    end).

%%====================================================================
%% Atomic Operations
%%====================================================================

%% @doc Performs an atomic update with automatic rollback on failure.
%%
%% Creates a new version of the state with tracking for rollback.
%% If the update function returns {error, Reason}, the state
%% is rolled back to its previous version.
%%
%% ```erlang
%% > State = wf_case_state:new(<<"case-1">>),
%% > {ok, Updated} = wf_case_state:atomic_update(State, fun(S) ->
%%     {ok, S#{data => #{x => 1}}}
%% end).
%% '''
%%
-spec atomic_update(state(), transaction_fun()) -> transaction_result().
atomic_update(State, TransactionFun) when is_function(TransactionFun, 1) ->
    Timestamp = erlang:system_time(millisecond),

    case TransactionFun(State) of
        {ok, NewState} ->
            Mutation = #{
                type => set,
                key => undefined,
                old_value => State,
                new_value => NewState,
                timestamp => Timestamp
            },
            {
                ok,
                NewState#{
                    version => maps:get(version, State, 0) + 1,
                    parent => State,
                    mutations => [Mutation | maps:get(mutations, State, [])],
                    history => [State | maps:get(history, State, [])],
                    committed => true,
                    dirty => false
                }
            };
        {error, Reason} ->
            {error, {transaction_failed, Reason, State}}
    end.

%% @doc Executes a multi-operation transaction atomically.
%%
%% All operations in the transaction function must succeed for the
%% entire transaction to commit. If any operation fails,
%% the entire state is rolled back.
%%
%% ```erlang
%% > State = wf_case_state:new(<<"case-1">>),
%% > {ok, State2} = wf_case_state:atomic_transaction(
%%     State,
%%     fun(S) ->
%%         S1 = S#{data => #{step1 => done}},
%%         S2 = S1#{data => maps:merge(maps:get(data, S1), #{step2 => done})},
%%         {ok, S2}
%%     end,
%%     #{timeout => 5000}
%% ).
%% '''
%%
-spec atomic_transaction(state(), transaction_fun(), map()) -> transaction_result().
atomic_transaction(State, TransactionFun, Options) when is_function(TransactionFun, 1) ->
    Timeout = maps:get(timeout, Options, infinity),

    % Create a mutable context for the transaction
    TransactionCtx = #{
        original => State,
        current => State,
        operations => [],
        start_time => erlang:monotonic_time(millisecond)
    },

    try
        case execute_transaction(TransactionCtx, TransactionFun, Timeout) of
            {ok, FinalState} ->
                % Commit the transaction
                commit(State, FinalState);
            {error, Reason} ->
                % Rollback on error
                rollback(State, Reason)
        end
    catch
        _:Exception:Stack ->
            logger:error("Transaction exception: ~p~nStack: ~p", [Exception, Stack]),
            rollback(State, {exception, Exception})
    end.

%% @private
execute_transaction(Ctx, TransactionFun, Timeout) ->
    StartTime = maps:get(start_time, Ctx),

    % Execute transaction with timeout
    Result = try
        case TransactionFun(maps:get(current, Ctx)) of
            {ok, NewState} ->
                {ok, NewState};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Exception:Stack ->
            logger:error("Transaction exception: ~p~nStack: ~p", [Exception, Stack]),
            {error, {exception, Exception, Stack}}
    end,

    % Check if timeout occurred
    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
    if
        Elapsed >= Timeout andalso Timeout =/= infinity ->
            {error, timeout};
        true ->
            Result
    end.

%% @doc Commits a state transition atomically.
%%
%% Creates a new committed version with mutation tracking.
-spec commit(state(), state()) -> commit_result().
commit(FromState, ToState) ->
    case maps_get_equal(FromState, ToState, [case_id, version]) of
        true ->
            % Same state - no-op
            {ok, FromState};
        false ->
            Timestamp = erlang:system_time(millisecond),

            % Create mutation record
            Mutations = case maps:get(version, ToState, 0) > maps:get(version, FromState, 0) of
                true ->
                    [#{
                        type => commit,
                        key => version,
                        old_value => maps:get(version, FromState, 0),
                        new_value => maps:get(version, ToState, 0),
                        timestamp => Timestamp
                    }];
                false ->
                    []
            end,

            CommittedState = ToState#{
                version => maps:get(version, FromState, 0) + 1,
                parent => FromState,
                mutations => Mutations ++ maps:get(mutations, ToState, []),
                history => [FromState | maps:get(history, ToState, [])],
                committed => true,
                dirty => false
            },

            {ok, CommittedState}
    end.

%% @doc Rolls back to a previous state version.
%%
%% Reverts the state to the parent version, undoing all
%% mutations since that point.
-spec rollback(state(), term()) -> {error, term()}.
rollback(State, Reason) ->
    Parent = maps:get(parent, State, undefined),
    case Parent of
        undefined ->
            {error, {no_parent, Reason}};
        _ ->
            logger:info("Rolling back state ~p to version ~p, reason: ~p",
                      [maps:get(version, State, 0),
                       maps:get(version, Parent, 0),
                       Reason]),
            {error, {rolled_back, Parent, Reason}}
    end.

%%====================================================================
%% History Management
%%====================================================================

%% @doc Returns the complete history of state versions.
-spec history(state()) -> [state()].
history(#{history := History}) ->
    lists:reverse(History).

%% @doc Returns the number of state versions in history.
-spec history_count(state()) -> non_neg_integer().
history_count(#{history := History}) ->
    length(History).

%% @doc Undoes the last state transition.
%%
%% Reverts to the parent state if available.
-spec undo(state()) -> {ok, state()} | {error, term()}.
undo(State) ->
    Parent = maps:get(parent, State, undefined),
    case Parent of
        undefined ->
            {error, no_parent};
        _ ->
            % Restore parent state with incremented version
            Restored = Parent#{
                history => lists:delete(Parent, maps:get(history, State, [])),
                committed => true,
                dirty => false
            },
            {ok, Restored}
    end.

%% @doc Redoes a previously undone transition.
%%
-spec redo(state()) -> {ok, state()} | {error, term()}.
redo(State) ->
    History = maps:get(history, State, []),
    case History of
        [] ->
            {error, no_redo_available};
        [NextState | _] ->
            {ok, NextState#{committed => true, dirty => false}}
    end.

%% @doc Checks if undo is available.
-spec can_undo(state()) -> boolean().
can_undo(#{parent := Parent}) ->
    Parent =/= undefined.

%% @doc Checks if redo is available.
-spec can_redo(state()) -> boolean().
can_redo(#{history := History}) when is_list(History) ->
    length(History) > 0;
can_redo(_) ->
    false.

%%====================================================================
%% Persistence Integration
%%====================================================================

%% @doc Persists state to Mnesia storage.
%%
%% Creates a persistent record with all state information
%% for recovery after restart.
-spec persist(state(), module()) -> {ok, checkpoint()} | {error, term()}.
persist(State, PersistenceModule) ->
    try
        case PersistenceModule:save_case(to_persistent_record(State)) of
            ok ->
                Checkpoint = #{
                    state => State,
                    timestamp => erlang:system_time(millisecond),
                    version => maps:get(version, State, 0)
                },
                {ok, Checkpoint};
            {error, Reason} ->
                {error, {persistence_failed, Reason}}
        end
    catch
        _:Exception:Stack ->
            logger:error("Persist exception: ~p~nStack: ~p",
                         [Exception, Stack]),
            {error, {persist_exception, Exception}}
    end.

%% @doc Restores state from persistence.
%%
%% Loads the most recent state from storage.
-spec restore(module()) -> {ok, state()} | {error, term()}.
restore(PersistenceModule) ->
    try
        case PersistenceModule:list_active_cases() of
            {ok, []} ->
                {error, no_active_cases};
            {ok, Cases} ->
                case Cases of
                    [] ->
                        {error, no_active_cases};
                    _ ->
                        % Get the most recent case (first in list)
                        [CaseMap | _] = Cases,
                        CaseId = maps:get(case_id, CaseMap, <<>>),

                        case PersistenceModule:load_case(CaseId) of
                            {ok, PersistentCase} ->
                                State = from_persistent_record(PersistentCase),
                                {ok, State};
                            {error, Reason} ->
                                {error, {load_failed, Reason}}
                        end
                end
        end
    catch
        _:Exception:Stack ->
            logger:error("Restore exception: ~p~nStack: ~p",
                         [Exception, Stack]),
            {error, {restore_exception, Exception}}
    end.

%% @doc Creates an in-memory checkpoint.
-spec checkpoint(state()) -> {ok, checkpoint()}.
checkpoint(State) ->
    Checkpoint = #{
        state => State,
        timestamp => erlang:system_time(millisecond),
        version => maps:get(version, State, 0)
    },
    {ok, Checkpoint}.

%% @doc Restores state from a checkpoint.
-spec restore_checkpoint(checkpoint()) -> {ok, state()}.
restore_checkpoint(#{state := State} = Checkpoint) ->
    logger:info("Restoring checkpoint version ~p from ~p",
                  [maps:get(version, State, 0),
                   maps:get(timestamp, Checkpoint, 0)]),
    {ok, State#{dirty => false, committed => true}}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Converts state to persistent record format.
to_persistent_record(State) ->
    #persistent_state{
        case_id = maps:get(case_id, State, <<>>),
        status = maps:get(status, State, created),
        data = maps:get(data, State, #{}),
        version = maps:get(version, State, 0),
        mutations = maps:get(mutations, State, []),
        checkpoint = undefined,
        updated_at = erlang:system_time(millisecond)
    }.

%% @private
%% @doc Converts persistent record to state format.
from_persistent_record(#persistent_state{} = Rec) ->
    #{
        case_id => Rec#persistent_state.case_id,
        status => Rec#persistent_state.status,
        data => Rec#persistent_state.data,
        version => Rec#persistent_state.version,
        parent => undefined,
        mutations => Rec#persistent_state.mutations,
        history => [],
        committed => true,
        dirty => false
    };
from_persistent_record(Map) when is_map(Map) ->
    #{
        case_id => maps:get(case_id, Map, <<>>),
        status => maps:get(status, Map, created),
        data => maps:get(data, Map, #{}),
        version => maps:get(version, Map, 0),
        parent => undefined,
        mutations => maps:get(mutations, Map, []),
        history => [],
        committed => true,
        dirty => false
    }.

%% @private
%% @doc Checks if specified keys are equal between two maps.
maps_get_equal(MapA, MapB, Keys) when is_list(Keys) ->
    lists:all(fun(K) ->
        maps:get(K, MapA, undefined) =:= maps:get(K, MapB, undefined)
    end, Keys).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% State construction tests
new_creates_default_state_test() ->
    S = new(<<"case-1">>),
    ?assertEqual(<<"case-1">>, get_case_id(S)),
    ?assertEqual(created, get_status(S)),
    ?assertEqual(0, get_version(S)),
    ?assertEqual(undefined, get_parent(S)),
    ?assertEqual([], get_mutations(S)).

new_with_initial_data_test() ->
    S = new(<<"case-2">>, #{x => 1}),
    ?assertEqual(<<"case-2">>, get_case_id(S)),
    ?assertEqual(#{x => 1}, get_data(S)).

%% Atomic update tests
atomic_update_success_test() ->
    S0 = new(<<"case-1">>),
    {ok, S1} = atomic_update(S0, fun(S) ->
        {ok, S#{data => #{test => 1}}}
    end),
    ?assertEqual(#{test => 1}, get_data(S1)),
    ?assertEqual(1, get_version(S1)),
    ?assertEqual(S0, get_parent(S1)).

atomic_update_failure_test() ->
    S0 = new(<<"case-1">>),
    Result = atomic_update(S0, fun(_S) ->
        {error, test_failure}
    end),
    ?assertMatch({error, {transaction_failed, test_failure, _}}, Result).

set_data_atomic_test() ->
    S0 = new(<<"case-1">>),
    {ok, S1} = set_data(S0, #{x => 1}),
    ?assertEqual(#{x => 1}, get_data(S1)).

update_data_atomic_test() ->
    S0 = new(<<"case-1">>, #{a => 1}),
    {ok, S1} = update_data(S0, #{b => 2}),
    ?assertEqual(#{a => 1, b => 2}, get_data(S1)).

%% Commit tests
commit_increments_version_test() ->
    S0 = new(<<"case-1">>),
    {ok, S1} = commit(S0, S0#{data => #{x => 1}}),
    ?assertEqual(1, get_version(S1)).

rollback_no_parent_test() ->
    S0 = new(<<"case-1">>),
    Result = rollback(S0, test_reason),
    ?assertMatch({error, {no_parent, test_reason}}, Result).

undo_redo_test() ->
    S0 = new(<<"case-1">>),
    {ok, S1} = commit(S0, S0#{data => #{x => 1}}),

    ?assert(can_undo(S1)),
    {ok, S0_restored} = undo(S1),
    ?assertEqual(0, get_version(S0_restored)),

    ?assertNot(can_undo(S0_restored)),
    ?assert(can_redo(S0_restored)),
    {ok, S1_restored} = redo(S0_restored),
    ?assertEqual(1, get_version(S1_restored)).

checkpoint_test() ->
    S0 = new(<<"case-1">>),
    {ok, Ckpt} = checkpoint(S0),
    ?assertEqual(S0, maps:get(state, Ckpt)),
    {ok, S1} = restore_checkpoint(Ckpt),
    ?assertEqual(<<"case-1">>, get_case_id(S1)).

-endif.
