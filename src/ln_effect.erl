%%%-------------------------------------------------------------------
%%% @doc ln_effect - Effect boundary for external side effects.
%%%
%%% Manages effect requests, completion tracking, and cancellation
%%% with receipt generation.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_effect).

%% API
-export([init/1]).
-export([request/4]).
-export([complete/3]).
-export([cancel_effects/2]).
-export([get_pending/1]).
-export([get_receipts/1]).
-export([default_handler/1]).

%% Types
-export_type([state/0, effect_spec/0, effect_id/0, receipt/0, handler/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type effect_id() :: reference().
-type scope_id() :: term().

-type effect_spec() :: #{
    module => module(),
    function => atom(),
    args => [term()],
    options => map()
}.

-type effect_status() :: requested | in_flight | completed | failed | cancelled.

-type receipt() :: #{
    effect_id => effect_id(),
    spec_hash => binary(),
    spec => effect_spec(),
    scope_id => scope_id(),
    started_at => integer(),
    completed_at => integer() | undefined,
    result => term() | undefined
}.

-record(pending_effect, {
    effect_id :: effect_id(),
    spec :: effect_spec(),
    scope_id :: scope_id(),
    callback_mod :: module(),
    continuation :: term(),
    status :: effect_status(),
    started_at :: integer()
}).

-type pending() :: #{effect_id() => #pending_effect{}}.

-record(effect_state, {
    pending :: pending(),
    receipts :: [receipt()],
    handler :: handler()
}).

-type handler() :: module() | fun((effect_spec()) -> {ok, term()} | {error, term()}).

-opaque state() :: #effect_state{}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Initialize effect state with handler.
-spec init(handler()) -> state().
init(Handler) ->
    #effect_state{
        pending = #{},
        receipts = [],
        handler = Handler
    }.

%% @doc Request a new effect.
-spec request(effect_spec(), scope_id(), module(), term()) ->
    {ok, effect_id(), state()}.
request(Spec, ScopeId, CallbackMod, Cont) ->
    EffectId = make_ref(),
    PendingEffect = #pending_effect{
        effect_id = EffectId,
        spec = Spec,
        scope_id = ScopeId,
        callback_mod = CallbackMod,
        continuation = Cont,
        status = requested,
        started_at = erlang:monotonic_time(millisecond)
    },
    State = #effect_state{
        pending = #{EffectId => PendingEffect},
        receipts = [],
        handler = default_handler
    },
    {ok, EffectId, State}.

%% @doc Complete an effect with result.
-spec complete(effect_id(), {ok, term()} | {error, term()}, state()) ->
    {ok, state()} | {error, term()}.
complete(EffectId, Result, #effect_state{pending = Pending, receipts = Receipts} = State) ->
    case maps:find(EffectId, Pending) of
        {ok, #pending_effect{spec = Spec, scope_id = ScopeId, started_at = StartedAt}} ->
            Receipt = #{
                effect_id => EffectId,
                spec_hash => ln_receipt:hash(Spec),
                spec => Spec,
                scope_id => ScopeId,
                started_at => StartedAt,
                completed_at => erlang:monotonic_time(millisecond),
                result => Result
            },
            NewState = State#effect_state{
                pending = maps:remove(EffectId, Pending),
                receipts = [Receipt | Receipts]
            },
            {ok, NewState};
        error ->
            {error, effect_not_found}
    end.

%% @doc Cancel all effects in a scope.
-spec cancel_effects(scope_id(), state()) -> {[effect_id()], state()}.
cancel_effects(ScopeId, #effect_state{pending = Pending, receipts = Receipts} = State) ->
    {ToCancel, Remaining} = maps:fold(fun
        (EffectId, #pending_effect{scope_id = S} = P, {Cancel, Rest}) ->
            case S =:= ScopeId of
                true ->
                    Receipt = #{
                        effect_id => EffectId,
                        spec_hash => ln_receipt:hash(P#pending_effect.spec),
                        spec => P#pending_effect.spec,
                        scope_id => ScopeId,
                        started_at => P#pending_effect.started_at,
                        completed_at => erlang:monotonic_time(millisecond),
                        result => cancelled
                    },
                    {[EffectId | Cancel], Rest, [Receipt | Receipts]};
                false ->
                    {Cancel, maps:put(EffectId, P, Rest), Receipts}
            end
    end, {[], #{} , Receipts}, Pending),
    NewState = State#effect_state{
        pending = Remaining,
        receipts = ToCancel ++ Receipts
    },
    {lists:reverse(ToCancel), NewState}.

%% @doc Get pending effects.
-spec get_pending(state()) -> [{effect_id(), effect_spec(), scope_id()}].
get_pending(#effect_state{pending = Pending}) ->
    maps:fold(fun(EffectId, #pending_effect{spec = Spec, scope_id = ScopeId}, Acc) ->
        [{EffectId, Spec, ScopeId} | Acc]
    end, [], Pending).

%% @doc Get all receipts.
-spec get_receipts(state()) -> [receipt()].
get_receipts(#effect_state{receipts = Receipts}) ->
    lists:reverse(Receipts).

%% @doc Default effect handler (mock in-process).
-spec default_handler(effect_spec()) -> {ok, term()} | {error, term()}.
default_handler(#{module := Mod, function := Fun, args := Args}) ->
    try
        {ok, apply(Mod, Fun, Args)}
    catch
        _:Reason ->
            {error, Reason}
    end.
