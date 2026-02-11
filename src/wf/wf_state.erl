%%% @doc WF State Management - Atomic State Store with Commit Protocol
%%%
%%% This module implements atomic state management for workflow case execution.
%%% It provides:
%%% - case_state() record with exec, user_ctx, tokens, effects, committed
%%% - Atomic commit protocol for state mutations
%%% - State checkpoint and rollback
%%% - Effect staging and commit boundaries
%%% - Token management
%%%
%%% All state mutations are staged in exec_state and only committed at
%%% effect boundaries, ensuring atomic state updates and rollback capability.
%%%
%%% @end
-module(wf_state).

-export([
    %% Case state constructors
    case_state/5,
    case_state_new/2,

    %% Accessors
    case_id/1,
    case_exec/1,
    case_user_ctx/1,
    case_tokens/1,
    case_effects/1,
    case_committed/1,

    %% Setters
    case_set_exec/2,
    case_set_user_ctx/2,
    case_set_tokens/2,
    case_set_effects/2,
    case_set_committed/2,

    %% Token operations
    token_add/3,
    token_get/2,
    token_remove/2,
    token_list/1,

    %% Effect operations
    effect_add/3,
    effect_get/2,
    effect_update/3,
    effect_list/1,
    effect_pending/1,

    %% Commit protocol
    checkpoint/1,
    rollback/2,
    commit/1,
    commit_with_exec/2,

    %% State validation
    is_committed/1,
    validate/1
]).

-export_type([
    case_state/0,
    token_id/0,
    token_value/0,
    effect_id/0,
    effect_status/0,
    effect_result/0,
    checkpoint_data/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

%% Case state record (the atomic state container)
-type case_state() :: {
    case_id(),              % Case identifier
    wf_vm:exec_state(),     % Execution machine state
    wf_term:context(),      % User-provided context
    token_store(),          % Per-activity tokens
    effect_store(),         % Effect staging and results
    non_neg_integer()       % Last committed step number
}.

-type case_id() :: atom() | binary() | {atom(), term()}.

-type token_id() :: atom() | {atom(), term()}.
-type token_value() :: any().
-type token_store() :: #{token_id() => token_value()}.

-type effect_id() :: atom() | binary() | {atom(), term()}.
-type effect_status() :: pending | completed | failed | cancelled.
-type effect_result() :: {effect_status(), any()}.
-type effect_store() :: #{effect_id() => effect_result()}.

%% Checkpoint data for rollback
-type checkpoint_data() :: {
    wf_vm:exec_state(),     % Execution state snapshot
    wf_term:context(),      % Context snapshot
    token_store(),          % Token store snapshot
    effect_store(),         % Effect store snapshot
    non_neg_integer()       % Committed step number
}.

%%% CONSTRUCTORS ============================================================

%% @doc Create a new case_state.
-spec case_state(
    CaseId :: case_id(),
    ExecState :: wf_vm:exec_state(),
    UserCtx :: wf_term:context(),
    Tokens :: token_store(),
    Effects :: effect_store()
) -> case_state().
case_state(CaseId, ExecState, UserCtx, Tokens, Effects) ->
    Committed = length(wf_vm:exec_trace(ExecState)),
    {CaseId, ExecState, UserCtx, Tokens, Effects, Committed}.

%% @doc Create a new case_state with initial exec state and context.
-spec case_state_new(
    CaseId :: case_id(),
    ExecState :: wf_vm:exec_state()
) -> case_state().
case_state_new(CaseId, ExecState) ->
    InitCtx = wf_vm:exec_ctx(ExecState),
    {CaseId, ExecState, InitCtx, #{}, #{}, 0}.

%%% ACCESSORS ===============================================================

%% @doc Get case ID.
-spec case_id(State :: case_state()) -> case_id().
case_id({CaseId, _, _, _, _, _}) -> CaseId.

%% @doc Get execution state.
-spec case_exec(State :: case_state()) -> wf_vm:exec_state().
case_exec({_, ExecState, _, _, _, _}) -> ExecState.

%% @doc Get user context.
-spec case_user_ctx(State :: case_state()) -> wf_term:context().
case_user_ctx({_, _, UserCtx, _, _, _}) -> UserCtx.

%% @doc Get token store.
-spec case_tokens(State :: case_state()) -> token_store().
case_tokens({_, _, _, Tokens, _, _}) -> Tokens.

%% @doc Get effect store.
-spec case_effects(State :: case_state()) -> effect_store().
case_effects({_, _, _, _, Effects, _}) -> Effects.

%% @doc Get last committed step number.
-spec case_committed(State :: case_state()) -> non_neg_integer().
case_committed({_, _, _, _, _, Committed}) -> Committed.

%%% SETTERS =================================================================

%% @doc Set execution state.
-spec case_set_exec(State :: case_state(), ExecState :: wf_vm:exec_state()) -> case_state().
case_set_exec({CId, _, Ctx, Tok, Eff, Comm}, ExecState) ->
    {CId, ExecState, Ctx, Tok, Eff, Comm}.

%% @doc Set user context.
-spec case_set_user_ctx(State :: case_state(), UserCtx :: wf_term:context()) -> case_state().
case_set_user_ctx({CId, Exec, _, Tok, Eff, Comm}, UserCtx) ->
    {CId, Exec, UserCtx, Tok, Eff, Comm}.

%% @doc Set token store.
-spec case_set_tokens(State :: case_state(), Tokens :: token_store()) -> case_state().
case_set_tokens({CId, Exec, Ctx, _, Eff, Comm}, Tokens) ->
    {CId, Exec, Ctx, Tokens, Eff, Comm}.

%% @doc Set effect store.
-spec case_set_effects(State :: case_state(), Effects :: effect_store()) -> case_state().
case_set_effects({CId, Exec, Ctx, Tok, _, Comm}, Effects) ->
    {CId, Exec, Ctx, Tok, Effects, Comm}.

%% @doc Set committed step number.
-spec case_set_committed(State :: case_state(), Committed :: non_neg_integer()) -> case_state().
case_set_committed({CId, Exec, Ctx, Tok, Eff, _}, Committed) ->
    {CId, Exec, Ctx, Tok, Eff, Committed}.

%%% TOKEN OPERATIONS ========================================================

%% @doc Add or update a token in the token store.
-spec token_add(State :: case_state(), TokenId :: token_id(), Value :: token_value()) -> case_state().
token_add(State, TokenId, Value) ->
    Tokens = case_tokens(State),
    NewTokens = maps:put(TokenId, Value, Tokens),
    case_set_tokens(State, NewTokens).

%% @doc Get a token value.
-spec token_get(State :: case_state(), TokenId :: token_id()) -> {ok, token_value()} | error.
token_get(State, TokenId) ->
    Tokens = case_tokens(State),
    case maps:get(TokenId, Tokens, undefined) of
        undefined -> error;
        Value -> {ok, Value}
    end.

%% @doc Remove a token from the token store.
-spec token_remove(State :: case_state(), TokenId :: token_id()) -> case_state().
token_remove(State, TokenId) ->
    Tokens = case_tokens(State),
    NewTokens = maps:remove(TokenId, Tokens),
    case_set_tokens(State, NewTokens).

%% @doc List all token IDs.
-spec token_list(State :: case_state()) -> [token_id()].
token_list(State) ->
    Tokens = case_tokens(State),
    maps:keys(Tokens).

%%% EFFECT OPERATIONS =======================================================

%% @doc Add an effect to the effect store (initially pending).
-spec effect_add(State :: case_state(), EffectId :: effect_id(), Spec :: term()) -> case_state().
effect_add(State, EffectId, Spec) ->
    Effects = case_effects(State),
    NewEffects = maps:put(EffectId, {pending, Spec}, Effects),
    case_set_effects(State, NewEffects).

%% @doc Get an effect result.
-spec effect_get(State :: case_state(), EffectId :: effect_id()) -> {ok, effect_result()} | error.
effect_get(State, EffectId) ->
    Effects = case_effects(State),
    case maps:get(EffectId, Effects, undefined) of
        undefined -> error;
        Result -> {ok, Result}
    end.

%% @doc Update an effect with a new status and result.
-spec effect_update(
    State :: case_state(),
    EffectId :: effect_id(),
    Result :: effect_result()
) -> case_state().
effect_update(State, EffectId, Result) ->
    Effects = case_effects(State),
    NewEffects = maps:put(EffectId, Result, Effects),
    case_set_effects(State, NewEffects).

%% @doc List all effect IDs.
-spec effect_list(State :: case_state()) -> [effect_id()].
effect_list(State) ->
    Effects = case_effects(State),
    maps:keys(Effects).

%% @doc List all pending effects.
-spec effect_pending(State :: case_state()) -> [effect_id()].
effect_pending(State) ->
    Effects = case_effects(State),
    [EffectId || {EffectId, {Status, _}} <- maps:to_list(Effects), Status == pending].

%%% COMMIT PROTOCOL =========================================================

%% @doc Create a checkpoint of the current state for rollback.
%%
%% Checkpoints capture the entire state at a commit boundary, allowing
%% rollback on effect failure or error.
%%
%% @end
-spec checkpoint(State :: case_state()) -> checkpoint_data().
checkpoint(State) ->
    ExecState = case_exec(State),
    UserCtx = case_user_ctx(State),
    Tokens = case_tokens(State),
    Effects = case_effects(State),
    Committed = case_committed(State),
    {ExecState, UserCtx, Tokens, Effects, Committed}.

%% @doc Rollback to a previous checkpoint.
%%
%% Restores the state to a previous checkpoint, discarding all mutations
%% since that checkpoint was taken.
%%
%% @end
-spec rollback(State :: case_state(), Checkpoint :: checkpoint_data()) -> case_state().
rollback({CaseId, _, _, _, _, _}, {ExecState, UserCtx, Tokens, Effects, Committed}) ->
    {CaseId, ExecState, UserCtx, Tokens, Effects, Committed}.

%% @doc Commit the current execution state.
%%
%% Commits all staged mutations in exec_state to the case state. This is
%% called at effect boundaries after successful effect completion.
%%
%% The commit updates:
%% - User context from exec_state context
%% - Committed step counter to current trace length
%%
%% @end
-spec commit(State :: case_state()) -> case_state().
commit(State) ->
    ExecState = case_exec(State),

    % Extract current context from exec state
    ExecCtx = wf_vm:exec_ctx(ExecState),

    % Update user context with exec context
    State1 = case_set_user_ctx(State, ExecCtx),

    % Update committed step counter
    TraceLength = length(wf_vm:exec_trace(ExecState)),
    State2 = case_set_committed(State1, TraceLength),

    State2.

%% @doc Commit with a new exec state.
%%
%% Convenience function to set the exec state and commit in one operation.
%%
%% @end
-spec commit_with_exec(State :: case_state(), ExecState :: wf_vm:exec_state()) -> case_state().
commit_with_exec(State, ExecState) ->
    State1 = case_set_exec(State, ExecState),
    commit(State1).

%%% STATE VALIDATION ========================================================

%% @doc Check if all mutations are committed (no pending changes).
-spec is_committed(State :: case_state()) -> boolean().
is_committed(State) ->
    ExecState = case_exec(State),
    TraceLength = length(wf_vm:exec_trace(ExecState)),
    Committed = case_committed(State),
    TraceLength == Committed.

%% @doc Validate the case state structure.
-spec validate(State :: case_state()) -> ok | {error, term()}.
validate({CaseId, ExecState, UserCtx, Tokens, Effects, Committed})
    when is_tuple(ExecState),
         is_map(UserCtx),
         is_map(Tokens),
         is_map(Effects),
         is_integer(Committed),
         Committed >= 0 ->
    % Validate that CaseId is valid
    case is_valid_case_id(CaseId) of
        true ->
            % Validate committed <= trace length
            TraceLength = length(wf_vm:exec_trace(ExecState)),
            if
                Committed =< TraceLength ->
                    ok;
                true ->
                    {error, {invalid_committed_counter, Committed, TraceLength}}
            end;
        false ->
            {error, {invalid_case_id, CaseId}}
    end;
validate(State) ->
    {error, {invalid_state_structure, State}}.

%%% INTERNAL HELPERS ========================================================

-spec is_valid_case_id(CaseId :: term()) -> boolean().
is_valid_case_id(CaseId) when is_atom(CaseId) -> true;
is_valid_case_id(CaseId) when is_binary(CaseId) -> true;
is_valid_case_id(CaseId) when is_tuple(CaseId), size(CaseId) >= 1 ->
    is_atom(element(1, CaseId));
is_valid_case_id(_) -> false.

%%% TESTS ===================================================================

%% Test case state construction
case_state_construction_test_() ->
    InitCtx = #{data => <<"test">>},
    Program = [wf_vm:op_halt()],
    ExecState = wf_vm:exec_state(Program, 0, [], InitCtx, #{}, #{}, []),
    [
        ?_test(begin
            State = case_state_new(my_case, ExecState),
            ?assertEqual(my_case, case_id(State)),
            ?assertEqual(ExecState, case_exec(State)),
            ?assertEqual(InitCtx, case_user_ctx(State)),
            ?assertEqual(#{}, case_tokens(State)),
            ?assertEqual(#{}, case_effects(State)),
            ?assertEqual(0, case_committed(State))
        end)
    ].

%% Test token operations
token_operations_test_() ->
    InitCtx = #{data => <<"test">>},
    Program = [wf_vm:op_halt()],
    ExecState = wf_vm:exec_state(Program, 0, [], InitCtx, #{}, #{}, []),
    State0 = case_state_new(test_case, ExecState),
    [
        ?_test(begin
            % Add a token
            State1 = token_add(State0, token1, <<"value1">>),
            ?assertEqual({ok, <<"value1">>}, token_get(State1, token1)),

            % Add another token
            State2 = token_add(State1, token2, <<"value2">>),
            ?assertEqual([token2, token1], lists:sort(token_list(State2))),

            % Remove a token
            State3 = token_remove(State2, token1),
            ?assertEqual(error, token_get(State3, token1)),
            ?assertEqual([token2], token_list(State3))
        end)
    ].

%% Test effect operations
effect_operations_test_() ->
    InitCtx = #{data => <<"test">>},
    Program = [wf_vm:op_halt()],
    ExecState = wf_vm:exec_state(Program, 0, [], InitCtx, #{}, #{}, []),
    State0 = case_state_new(test_case, ExecState),
    [
        ?_test(begin
            % Add an effect
            State1 = effect_add(State0, effect1, {call, my_tool, []}),
            ?assertMatch({ok, {pending, _}}, effect_get(State1, effect1)),

            % Update effect to completed
            State2 = effect_update(State1, effect1, {completed, <<"result">>}),
            ?assertEqual({ok, {completed, <<"result">>}}, effect_get(State2, effect1)),

            % Add another pending effect
            State3 = effect_add(State2, effect2, {call, other_tool, []}),
            Pending = effect_pending(State3),
            ?assertEqual([effect2], Pending)
        end)
    ].

%% Test checkpoint and rollback
checkpoint_rollback_test_() ->
    InitCtx = #{data => <<"test">>},
    Program = [wf_vm:op_halt()],
    ExecState = wf_vm:exec_state(Program, 0, [], InitCtx, #{}, #{}, []),
    State0 = case_state_new(test_case, ExecState),
    [
        ?_test(begin
            % Create checkpoint
            Checkpoint = checkpoint(State0),

            % Make changes
            State1 = token_add(State0, token1, <<"value1">>),
            State2 = effect_add(State1, effect1, {call, tool, []}),

            % Verify changes
            ?assertEqual({ok, <<"value1">>}, token_get(State2, token1)),
            ?assertMatch({ok, {pending, _}}, effect_get(State2, effect1)),

            % Rollback
            State3 = rollback(State2, Checkpoint),

            % Verify rollback
            ?assertEqual(error, token_get(State3, token1)),
            ?assertEqual(error, effect_get(State3, effect1))
        end)
    ].

%% Test commit protocol
commit_test_() ->
    InitCtx = #{data => <<"initial">>},
    Program = [wf_vm:op_halt()],
    ExecState = wf_vm:exec_state(Program, 0, [], InitCtx, #{}, #{}, []),
    State0 = case_state_new(test_case, ExecState),
    [
        ?_test(begin
            % Initial state is committed
            ?assert(is_committed(State0)),

            % Update exec state with a trace event
            ExecState1 = wf_vm:exec_add_trace_event(ExecState, test_event, test_op),
            State1 = case_set_exec(State0, ExecState1),

            % Not committed yet
            ?assertNot(is_committed(State1)),

            % Commit
            State2 = commit(State1),

            % Now committed
            ?assert(is_committed(State2)),
            ?assertEqual(1, case_committed(State2))
        end)
    ].

%% Test state validation
validation_test_() ->
    InitCtx = #{data => <<"test">>},
    Program = [wf_vm:op_halt()],
    ExecState = wf_vm:exec_state(Program, 0, [], InitCtx, #{}, #{}, []),
    State = case_state_new(test_case, ExecState),
    [
        ?_assertEqual(ok, validate(State)),
        ?_assertMatch({error, {invalid_state_structure, _}}, validate({invalid})),
        ?_assertMatch({error, {invalid_case_id, _}}, validate({123, ExecState, #{}, #{}, #{}, 0}))
    ].
