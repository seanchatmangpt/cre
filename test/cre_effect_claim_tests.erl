-module(cre_effect_claim_tests).
-include_lib("eunit/include/eunit.hrl").

eff() ->
    #{principal_id => <<"p1">>, beneficiary_id => <<"b1">>, monetary_amount => 1000,
      currency_or_asset => <<"USD">>, purpose => <<"invoice">>, authority_grant_id => <<"g1">>,
      obligation_id => <<"o1">>, rail_profile_id => <<"r1">>, expires_at => 99,
      resource_reservation_id => <<"res1">>}.

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

%% Property-style: for any observation sequence, settled count per obligation =< 1.
settlement_at_most_once_test() ->
    Obs = [timeout, accepted, rejected, settled, returned, reversed, submitted],
    rand:seed(exsss, {1, 2, 3}),
    lists:foreach(fun(_) ->
        {Id, S1} = prepared_submitted(),
        Seq = [lists:nth(rand:uniform(length(Obs)), Obs) || _ <- lists:seq(1, 12)],
        {S2, _} = cre_effect_claim:replay([{Id, O} || O <- Seq], S1),
        ?assert(cre_effect_claim:settlement_count(<<"o1">>, S2) =< 1)
    end, lists:seq(1, 200)).
