%%% @doc ln_ctrl_cancel: Cancellation semantics
%%%
%%% Mark scope as cancelled (set flag in exec_state).
%%% Jump PC to scope exit.
%%% Filter effect queue: drop effects initiated after cancel signal.
%%%
%%% @end
-module(ln_ctrl_cancel).

-export([
    new_cancel_signal/1,
    is_cancelled/2,
    propagate_cancel/2,
    stop_effects_in_scope/2
]).

-export_type([
    cancel_signal/0
]).

-include_lib("eunit/include/eunit.hrl").

%%% TYPES ===================================================================

-record(cancel_signal, {
    scope_id :: atom(),
    timestamp_ms :: non_neg_integer()
}).

-type cancel_signal() :: #cancel_signal{}.

%%% API =====================================================================

%% @doc Create a new cancel signal for a scope.
%%
%% Signals are timestamped to allow filtering effects issued before cancellation.
%%
%% @end
-spec new_cancel_signal(ScopeID :: atom()) -> cancel_signal().
new_cancel_signal(ScopeID) when is_atom(ScopeID) ->
    #cancel_signal{
        scope_id = ScopeID,
        timestamp_ms = erlang:monotonic_time(millisecond)
    }.

%% @doc Check if a scope is cancelled.
%%
%% Examines the cancellation flags in the execution state to determine
%% if the given scope is marked as cancelled.
%%
%% @end
-spec is_cancelled(ScopeID :: atom(), ExecState :: wf_vm:exec_state()) -> boolean().
is_cancelled(ScopeID, ExecState) when is_atom(ScopeID) ->
    CancelFlags = wf_vm:exec_cancel(ExecState),
    maps:get(ScopeID, CancelFlags, false).

%% @doc Propagate cancellation to an execution state.
%%
%% Sets the cancellation flag for the scope and optionally jumps the PC
%% to the scope exit (if exit PC is known).
%%
%% @end
-spec propagate_cancel(
    ExecState :: wf_vm:exec_state(),
    ScopeID :: atom()
) -> wf_vm:exec_state().
propagate_cancel(ExecState, ScopeID) when is_atom(ScopeID) ->
    CancelFlags = wf_vm:exec_cancel(ExecState),
    NewCancelFlags = maps:put(ScopeID, true, CancelFlags),
    wf_vm:exec_set_cancel(ExecState, NewCancelFlags).

%% @doc Stop effects initiated within a scope after cancellation.
%%
%% Filters the effect log to remove effects that were initiated after
%% the cancel signal. This ensures no effects post-cancel are issued.
%%
%% Effects are represented as {effect_id, timestamp_ms, spec, ...}
%%
%% @end
-spec stop_effects_in_scope(
    EffectLog :: [any()],
    CancelSignal :: cancel_signal()
) -> [any()].
stop_effects_in_scope(EffectLog, CancelSignal) when is_list(EffectLog) ->
    CancelTime = CancelSignal#cancel_signal.timestamp_ms,

    lists:filter(
        fun(Effect) ->
            case Effect of
                {_EffectID, Timestamp, _Spec, _} when is_integer(Timestamp) ->
                    Timestamp =< CancelTime;
                _ ->
                    true
            end
        end,
        EffectLog
    ).

%%% TESTS ===================================================================

new_cancel_signal_test_() ->
    Signal = new_cancel_signal(my_scope),
    [
        ?_assertEqual(my_scope, Signal#cancel_signal.scope_id),
        ?_assert(Signal#cancel_signal.timestamp_ms > 0)
    ].

is_cancelled_test_() ->
    ExecState = wf_vm:exec_state([], 0, [], #{}, #{}, #{}, []),
    [
        ?_assertNot(is_cancelled(my_scope, ExecState))
    ].

propagate_cancel_test_() ->
    ExecState = wf_vm:exec_state([], 0, [], #{}, #{}, #{}, []),
    NewExecState = propagate_cancel(ExecState, my_scope),
    [
        ?_assert(is_cancelled(my_scope, NewExecState))
    ].

stop_effects_in_scope_test_() ->
    NowMs = erlang:monotonic_time(millisecond),
    OldEffect = {eff1, NowMs - 1000, {effect, test, payload}},
    NewEffect = {eff2, NowMs + 1000, {effect, test, payload}},
    EffectLog = [OldEffect, NewEffect],

    Signal = #cancel_signal{scope_id = my_scope, timestamp_ms = NowMs},
    FilteredLog = stop_effects_in_scope(EffectLog, Signal),

    [
        ?_assertEqual(1, length(FilteredLog)),
        ?_assertEqual(OldEffect, hd(FilteredLog))
    ].
