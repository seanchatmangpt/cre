-module(cre_effect_claim_tests).
-include_lib("eunit/include/eunit.hrl").

eff() ->
    #{principal_id => <<"p1">>, beneficiary_id => <<"b1">>, monetary_amount => 1000,
      currency_or_asset => <<"USD">>, purpose => <<"invoice">>, authority_grant_id => <<"g1">>,
      obligation_id => <<"o1">>, rail_profile_id => <<"r1">>, expires_at => 99,
      resource_reservation_id => <<"res1">>,
      beneficiary_account_identity => <<"acct-1">>, authority_digest => <<"d1">>}.

prepared_submitted() ->
    {ok, Id, S0} = cre_effect_claim:prepare(eff(), cre_effect_claim:new_store()),
    {ok, S1} = cre_effect_claim:submit(Id, S0),
    {Id, S1}.

f2_identity_changes_test_() ->
    Id = cre_effect_claim:effect_id(eff()),
    [?_assertEqual(ok, cre_effect_claim:authorize_check(Id, eff(), cre_effect_claim:new_store()))
     | [?_assertEqual({refused, 'REFUSED_EFFECT_IDENTITY_MISMATCH'},
                      cre_effect_claim:authorize_check(Id, (eff())#{F => V},
                                                       cre_effect_claim:new_store()))
        || {F, V} <- [{beneficiary_id, <<"b2">>}, {monetary_amount, 1001},
                      {obligation_id, <<"o2">>}, {authority_grant_id, <<"g2">>}]]].

f3_timeout_forbids_blind_retry_test() ->
    {Id, S1} = prepared_submitted(),
    {ok, S2} = cre_effect_claim:observe(Id, timeout, S1),
    ?assertEqual(unknown, cre_effect_claim:state(Id, S2)),
    ?assertEqual({refused, blind_retry_on_unknown}, cre_effect_claim:submit(Id, S2)),
    %% a second effect for the same obligation is refused while UNKNOWN is live
    E2 = (eff())#{expires_at => 100},
    ?assertMatch({refused, {obligation_has_live_effect, Id}},
                 cre_effect_claim:prepare(E2, S2)).

reconcile_non_acceptance_releases_obligation_test() ->
    {Id, S1} = prepared_submitted(),
    {ok, S2} = cre_effect_claim:observe(Id, timeout, S1),
    {ok, S3} = cre_effect_claim:observe(Id, rejected, S2),
    ?assertMatch({ok, _, _}, cre_effect_claim:prepare((eff())#{expires_at => 100}, S3)).

accept_then_dropped_ack_settles_once_test() ->
    {Id, S1} = prepared_submitted(),
    {ok, S2} = cre_effect_claim:observe(Id, timeout, S1),   % ACK dropped
    {ok, S3} = cre_effect_claim:observe(Id, accepted, S2),  % reconciliation finds acceptance
    {ok, S4} = cre_effect_claim:observe(Id, settled, S3),
    ?assertEqual(1, cre_effect_claim:settlement_count(<<"o1">>, S4)),
    ?assertMatch({refused, _}, cre_effect_claim:prepare((eff())#{expires_at => 100}, S4)).

f5_no_settlement_without_acceptance_test() ->
    {Id, S1} = prepared_submitted(),
    ?assertEqual({refused, settlement_without_acceptance},
                 cre_effect_claim:observe(Id, settled, S1)).

f6_replay_performs_no_external_action_test() ->
    {Id, S1} = prepared_submitted(),
    {S2, Actions} = cre_effect_claim:replay([{Id, timeout}, {Id, accepted}, {Id, settled}], S1),
    ?assertEqual([], Actions),
    ?assertEqual(settled, cre_effect_claim:state(Id, S2)),
    %% replay is deterministic
    ?assertEqual({S2, []},
                 cre_effect_claim:replay([{Id, timeout}, {Id, accepted}, {Id, settled}], S1)).

idempotent_prepare_test() ->
    {ok, Id, S0} = cre_effect_claim:prepare(eff(), cre_effect_claim:new_store()),
    ?assertEqual({ok, Id, S0}, cre_effect_claim:prepare(eff(), S0)).

%% Regression: every identity field must move effect_id (audit mutation: a field dropped from
%% the identity, e.g. beneficiary_account_identity / authority_digest, silently transfers
%% an old authorization to a redirected payment).
every_identity_field_changes_effect_id_test_() ->
    Base = cre_effect_claim:effect_id(eff()),
    [{atom_to_list(F),
      ?_assertNotEqual(Base, cre_effect_claim:effect_id((eff())#{F => changed_value}))}
     || F <- cre_effect_claim:identity_fields()].

identity_fields_cover_spec_test() ->
    Spec = [principal_id, beneficiary_id, beneficiary_account_identity, monetary_amount,
            currency_or_asset, purpose, authority_grant_id, authority_digest, obligation_id,
            rail_profile_id, expires_at, resource_reservation_id],
    ?assertEqual(lists:sort(Spec), lists:sort(cre_effect_claim:identity_fields())).

redirected_account_not_authorized_test() ->
    Id = cre_effect_claim:effect_id(eff()),
    ?assertEqual({refused, 'REFUSED_EFFECT_IDENTITY_MISMATCH'},
                 cre_effect_claim:authorize_check(
                   Id, (eff())#{beneficiary_account_identity => <<"acct-2">>},
                   cre_effect_claim:new_store())).

non_identity_fields_do_not_change_effect_id_test() ->
    ?assertEqual(cre_effect_claim:effect_id(eff()),
                 cre_effect_claim:effect_id((eff())#{nonce => <<"n">>, created_at => 5})).

%% Pinned vector: effect_id is a stable 64-char lowercase sha256 hex (deterministic across runs).
effect_id_shape_and_stability_test() ->
    Id = cre_effect_claim:effect_id(eff()),
    ?assertEqual(64, byte_size(Id)),
    ?assertMatch({match, _}, re:run(Id, "^[0-9a-f]{64}$")),
    ?assertEqual(Id, cre_effect_claim:effect_id(maps:from_list(lists:reverse(maps:to_list(eff()))))),
    ?assertEqual(<<"b29328f8b130eea57d4e2d53aed0f3fbccf056c3f66a52f1905bdf7202cbb9b4">>, Id).

second_settle_is_illegal_transition_test() ->
    {Id, S1} = prepared_submitted(),
    {ok, S2} = cre_effect_claim:observe(Id, accepted, S1),
    {ok, S3} = cre_effect_claim:observe(Id, settled, S2),
    ?assertEqual({refused, {illegal_transition, settled, settled}},
                 cre_effect_claim:observe(Id, settled, S3)).

%% Second live effect for one obligation is refused in every live state.
second_live_effect_refused_in_each_live_state_test_() ->
    Paths = [{[], prepared}, {[], submitted}, {[timeout], unknown},
             {[accepted], accepted}, {[accepted, settled], settled}],
    [?_assertMatch({refused, {obligation_has_live_effect, _}}, second_effect_after(Obs, Want))
     || {Obs, Want} <- Paths].

second_effect_after(Obs, Want) ->
    {ok, Id, S0} = cre_effect_claim:prepare(eff(), cre_effect_claim:new_store()),
    S1 = case Want of
             prepared -> S0;
             _ -> {ok, X} = cre_effect_claim:submit(Id, S0), X
         end,
    S2 = lists:foldl(fun(O, A) -> {ok, N} = cre_effect_claim:observe(Id, O, A), N end, S1, Obs),
    ?assertEqual(Want, cre_effect_claim:state(Id, S2)),
    cre_effect_claim:prepare((eff())#{expires_at => 100}, S2).

%% Property over MULTIPLE effects for one obligation: random prepare/submit/observe steps;
%% invariants: settled count =< 1 and live effects =< 1.
one_obligation_invariants_test() ->
    Obs = [timeout, accepted, rejected, settled, returned, reversed],
    rand:seed(exsss, {1, 2, 3}),
    lists:foreach(fun(_) ->
        Final = lists:foldl(fun(N, S) -> step(N, Obs, S) end,
                            cre_effect_claim:new_store(), lists:seq(1, 40)),
        ?assert(cre_effect_claim:settlement_count(<<"o1">>, Final) =< 1),
        ?assert(live_count(Final) =< 1)
    end, lists:seq(1, 200)).

step(N, Obs, S) ->
    case rand:uniform(3) of
        1 -> case cre_effect_claim:prepare((eff())#{expires_at => N}, S) of
                 {ok, _, S2} -> S2;
                 {refused, _} -> S
             end;
        2 -> case ids(S) of
                 [] -> S;
                 Ids -> Id = lists:nth(rand:uniform(length(Ids)), Ids),
                        case cre_effect_claim:submit(Id, S) of
                            {ok, S2} -> S2;
                            {refused, _} -> S
                        end
             end;
        3 -> case ids(S) of
                 [] -> S;
                 Ids -> Id = lists:nth(rand:uniform(length(Ids)), Ids),
                        O = lists:nth(rand:uniform(length(Obs)), Obs),
                        case cre_effect_claim:observe(Id, O, S) of
                            {ok, S2} -> S2;
                            {refused, _} -> S
                        end
             end
    end.

ids(#{claims := Cs}) -> maps:keys(Cs).

live_count(S = #{claims := Cs}) ->
    length([I || I <- maps:keys(Cs),
                 not lists:member(cre_effect_claim:state(I, S), [rejected, returned, reversed])]).
