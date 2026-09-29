%% @doc Pure idempotent effect-claim state machine (payments falsifiers F2, F3, F4, F6).
%%
%% Model: OBLIGATION -> PREPARED -> SUBMITTED -> {UNKNOWN | ACCEPTED | REJECTED}
%%        -> {SETTLED | RETURNED | REVERSED}.
%% A timeout yields UNKNOWN; UNKNOWN never permits blind resubmission. Only a
%% reconciliation that proves non-acceptance releases the obligation for a NEW effect.
%% Effect identity is a hash over the economic fields, so any change to
%% beneficiary/amount/obligation/authority yields a different effect_id and a
%% prior authorization does not transfer (REFUSED_EFFECT_IDENTITY_MISMATCH).
%% Non-normative: no ISO 20022 / FIBO conformance is claimed; fields are a CASTLE
%% operational extension.
-module(cre_effect_claim).

-export([effect_id/1, new_store/0, prepare/2, submit/2, observe/3,
         authorize_check/3, state/2, settlement_count/2, replay/2,
         identity_fields/0]).

-type effect() :: #{atom() => term()}.
-type state() :: prepared | submitted | unknown | accepted | rejected
               | settled | returned | reversed.
-type store() :: #{claims := #{binary() => map()}, obligations := #{term() => [binary()]}}.
-export_type([effect/0, state/0, store/0]).

identity_fields() ->
    [principal_id, beneficiary_id, monetary_amount, currency_or_asset, purpose,
     authority_grant_id, obligation_id, rail_profile_id, expires_at,
     resource_reservation_id].

%% @doc Deterministic effect identity (sha256 hex over ordered field tuple).
-spec effect_id(effect()) -> binary().
effect_id(Effect) ->
    Terms = [{F, maps:get(F, Effect, undefined)} || F <- identity_fields()],
    Bin = term_to_binary(Terms, [{minor_version, 2}, deterministic]),
    binary:encode_hex(crypto:hash(sha256, Bin), lowercase).

new_store() -> #{claims => #{}, obligations => #{}}.

%% @doc Prepare an effect. Refuses a second live effect for one obligation.
-spec prepare(effect(), store()) -> {ok, binary(), store()} | {refused, term()}.
prepare(Effect, Store = #{claims := Cs, obligations := Os}) ->
    Ob = maps:get(obligation_id, Effect, undefined),
    Id = effect_id(Effect),
    case maps:is_key(Id, Cs) of
        true -> {ok, Id, Store};   % idempotent re-prepare of the identical effect
        false ->
            Live = [E || E <- maps:get(Ob, Os, []), is_live(maps:get(state, maps:get(E, Cs)))],
            case Live of
                [] ->
                    Claim = #{effect => Effect, state => prepared, log => [prepared]},
                    {ok, Id, Store#{claims := Cs#{Id => Claim},
                                    obligations := Os#{Ob => maps:get(Ob, Os, []) ++ [Id]}}};
                [Other | _] ->
                    {refused, {obligation_has_live_effect, Other}}
            end
    end.

%% @doc Submit. Only PREPARED may be submitted; UNKNOWN is a refusal (no blind retry).
-spec submit(binary(), store()) -> {ok, store()} | {refused, term()}.
submit(Id, Store) ->
    case state(Id, Store) of
        prepared -> {ok, put_state(Id, submitted, Store)};
        unknown  -> {refused, blind_retry_on_unknown};
        not_found -> {refused, unknown_effect};
        S -> {refused, {not_submittable, S}}
    end.

%% @doc Record a rail/reconciliation observation.
%% timeout -> unknown; accepted|rejected from submitted or unknown;
%% settled|returned|reversed require prior acceptance (ledger observes, never fabricates).
-spec observe(binary(), atom(), store()) -> {ok, store()} | {refused, term()}.
observe(Id, Obs, Store) ->
    S = state(Id, Store),
    case {S, Obs} of
        {not_found, _} -> {refused, unknown_effect};
        {submitted, timeout} -> {ok, put_state(Id, unknown, Store)};
        {submitted, accepted} -> {ok, put_state(Id, accepted, Store)};
        {submitted, rejected} -> {ok, put_state(Id, rejected, Store)};
        {unknown, accepted} -> {ok, put_state(Id, accepted, Store)};
        {unknown, rejected} -> {ok, put_state(Id, rejected, Store)};
        {unknown, timeout} -> {ok, Store};
        {accepted, settled} -> {ok, put_state(Id, settled, Store)};
        {settled, returned} -> {ok, put_state(Id, returned, Store)};
        {settled, reversed} -> {ok, put_state(Id, reversed, Store)};
        {_, settled} -> {refused, settlement_without_acceptance};
        _ -> {refused, {illegal_transition, S, Obs}}
    end.

%% @doc F2: an authorization for effect id AuthId only covers an effect whose id matches.
-spec authorize_check(binary(), effect(), store()) -> ok | {refused, term()}.
authorize_check(AuthId, Effect, _Store) ->
    case effect_id(Effect) of
        AuthId -> ok;
        _ -> {refused, 'REFUSED_EFFECT_IDENTITY_MISMATCH'}
    end.

-spec state(binary(), store()) -> state() | not_found.
state(Id, #{claims := Cs}) ->
    case Cs of
        #{Id := #{state := S}} -> S;
        _ -> not_found
    end.

%% @doc Valid settlements per obligation; invariant: =< 1.
-spec settlement_count(term(), store()) -> non_neg_integer().
settlement_count(Ob, #{claims := Cs, obligations := Os}) ->
    length([E || E <- maps:get(Ob, Os, []),
                 lists:member(maps:get(state, maps:get(E, Cs)), [settled])]).

%% @doc F6: replay folds recorded observations into state and performs NO external effect.
%% Returns the final store and an empty list of external actions by construction.
-spec replay([{binary(), atom()}], store()) -> {store(), []}.
replay(Observations, Store) ->
    Final = lists:foldl(fun({Id, Obs}, Acc) ->
                                case observe(Id, Obs, Acc) of
                                    {ok, N} -> N;
                                    {refused, _} -> Acc
                                end
                        end, Store, Observations),
    {Final, []}.

is_live(S) -> not lists:member(S, [rejected, returned, reversed]).

put_state(Id, S, Store = #{claims := Cs}) ->
    C = #{log := L} = maps:get(Id, Cs),
    Store#{claims := Cs#{Id := C#{state := S, log := L ++ [S]}}}.
