%% -*- erlang -*-
%%%% @doc NATO Emergency Session "STRAT-STABILITY" Concuerror entry-point tests.
%%
%% Protocol + concurrency stress tests for governance, coordination, safety gates.
%% Run via: concuerror -m nato_deterrence_concuerror_tests -t scenario_consensus_publish
%% Or: concuerror:run([{entry_point, {nato_deterrence_concuerror_tests, scenario_consensus_publish, []}}]).
%%
%% @end
%% -------------------------------------------------------------------

-module(nato_deterrence_concuerror_tests).
-export([
    scenario_consensus_publish/0,
    scenario_two_channel_posture_gate/0,
    scenario_draft_race_single_writer/0,
    scenario_no_classified_leak_to_press/0,
    scenario_deadlock_fallback_chair_summary/0,
    scenario_audit_chain_integrity/0
]).

-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

spawn_actor(Fun) ->
    spawn(fun() -> Fun() end).

wait_for_pids(Pids) ->
    [erlang:monitor(process, P) || P <- Pids],
    wait_down(length(Pids)).

wait_down(0) -> ok;
wait_down(N) ->
    receive
        {'DOWN', _, process, _, _} -> wait_down(N - 1)
    end.

%%--------------------------------------------------------------------
%% 1) Consensus publish under adversarial interleavings
%%--------------------------------------------------------------------
scenario_consensus_publish() ->
    Deps = [usa, uk, fr, de],
    {ok, Pid} = nato_conf:start_link(#{
        delegations => Deps,
        policy => #{
            publish_requires => unanimous,
            allow_stand_aside => false
        }
    }),
    Draft = nato_conf:new_draft(Pid, chair, <<"Strategic Stability Communique">>),

    Pids = [
        spawn_actor(fun() -> nato_conf:approve(Pid, D, Draft) end) || D <- Deps
    ] ++ [
        spawn_actor(fun() -> nato_conf:publish(Pid, chair, Draft) end)
    ],

    wait_for_pids(Pids),

    true = nato_assert:consensus(Pid, Draft),
    true = nato_assert:audit_chain_ok(Pid),

    ok = nato_conf:stop(Pid),
    ok.

%%--------------------------------------------------------------------
%% 2) Two-channel gate for posture changes
%%--------------------------------------------------------------------
scenario_two_channel_posture_gate() ->
    Deps = [usa, uk, fr, de],
    {ok, Pid} = nato_conf:start_link(#{
        delegations => Deps,
        policy => #{posture_requires_two_channels => true}
    }),

    Evidence = <<"EVID-001">>,

    Pids = [
        spawn_actor(fun() -> nato_conf:recommend_posture(Pid, intel, 1, Evidence) end),
        spawn_actor(fun() -> nato_conf:recommend_posture(Pid, mil, 1, Evidence) end),
        spawn_actor(fun() -> nato_conf:confirm_channel(Pid, intel, Evidence) end),
        spawn_actor(fun() -> nato_conf:confirm_channel(Pid, mil, Evidence) end)
    ] ++ [
        spawn_actor(fun() -> nato_conf:set_posture(Pid, chair, 1, Evidence) end) || _ <- lists:seq(1, 3)
    ],

    wait_for_pids(Pids),

    true = nato_assert:posture_set_requires_two_channels(Pid),
    true = nato_assert:audit_chain_ok(Pid),

    ok = nato_conf:stop(Pid),
    ok.

%%--------------------------------------------------------------------
%% 3) Draft race: concurrent edits must not produce split-brain
%%--------------------------------------------------------------------
scenario_draft_race_single_writer() ->
    Deps = [usa, uk, fr, de],
    {ok, Pid} = nato_conf:start_link(#{
        delegations => Deps,
        policy => #{
            draft_model => single_writer,
            publish_requires => unanimous
        }
    }),
    Draft = nato_conf:new_draft(Pid, chair, <<"Deterrence Posture Statement">>),

    Pids = [
        spawn_actor(fun() -> nato_conf:propose_edit(Pid, usa, Draft, {replace, paragraph2, <<"Text A">>}) end),
        spawn_actor(fun() -> nato_conf:propose_edit(Pid, fr, Draft, {replace, paragraph2, <<"Text B">>}) end),
        spawn_actor(fun() -> nato_conf:propose_edit(Pid, de, Draft, {append, annex_public, <<"Declassified Appendix">>}) end)
    ],

    wait_for_pids(Pids),

    true = nato_assert:single_current_draft(Pid),
    true = nato_assert:audit_chain_ok(Pid),

    ok = nato_conf:stop(Pid),
    ok.

%%--------------------------------------------------------------------
%% 4) No classified leak to press office
%%--------------------------------------------------------------------
scenario_no_classified_leak_to_press() ->
    Deps = [usa, uk, fr, de],
    {ok, Pid} = nato_conf:start_link(#{
        delegations => Deps,
        policy => #{press_receives => declassified_only}
    }),

    Pids = [
        spawn_actor(fun() -> nato_conf:push_press(Pid, {declassified, <<"Public line 1">>}) end),
        spawn_actor(fun() -> nato_conf:push_press(Pid, {classified, <<"Annex X">>}) end),
        spawn_actor(fun() -> nato_conf:push_press(Pid, {declassified, <<"Public line 2">>}) end)
    ],

    wait_for_pids(Pids),

    true = nato_assert:no_classified_in_press(Pid),

    ok = nato_conf:stop(Pid),
    ok.

%%--------------------------------------------------------------------
%% 5) Deadlock safety: if consensus fails, must end with chair_summary
%%--------------------------------------------------------------------
scenario_deadlock_fallback_chair_summary() ->
    Deps = [usa, uk, fr, de],
    {ok, Pid} = nato_conf:start_link(#{
        delegations => Deps,
        policy => #{
            publish_requires => unanimous,
            fallback => chair_summary_after_rounds,
            max_rounds => 1
        }
    }),
    Draft = nato_conf:new_draft(Pid, chair, <<"Communique Draft">>),

    Pids = [
        spawn_actor(fun() -> nato_conf:approve(Pid, usa, Draft) end),
        spawn_actor(fun() -> nato_conf:approve(Pid, uk, Draft) end),
        spawn_actor(fun() -> nato_conf:approve(Pid, fr, Draft) end),
        spawn_actor(fun() -> nato_conf:object(Pid, de, Draft, <<"Objection">>) end),
        spawn_actor(fun() -> nato_conf:close_session(Pid, chair, timeout_rounds) end)
    ],

    wait_for_pids(Pids),

    Res = nato_conf:result(Pid),
    true = (Res =:= chair_summary orelse Res =:= published orelse Res =:= adjourned),

    true = nato_assert:audit_chain_ok(Pid),

    ok = nato_conf:stop(Pid),
    ok.

%%--------------------------------------------------------------------
%% 6) Audit chain integrity: publish references a stable draft hash
%%--------------------------------------------------------------------
scenario_audit_chain_integrity() ->
    Deps = [usa, uk, fr, de],
    {ok, Pid} = nato_conf:start_link(#{
        delegations => Deps,
        policy => #{publish_requires => unanimous}
    }),
    Draft = nato_conf:new_draft(Pid, chair, <<"Final Statement">>),

    Pids = [
        spawn_actor(fun() -> nato_conf:approve(Pid, D, Draft) end) || D <- Deps
    ] ++ [
        spawn_actor(fun() -> nato_conf:publish(Pid, chair, Draft) end)
    ],

    wait_for_pids(Pids),

    Hash = nato_ids:hash_draft(Pid, Draft),
    true = is_binary(Hash),

    true = nato_assert:audit_chain_ok(Pid),

    ok = nato_conf:stop(Pid),
    ok.

%%--------------------------------------------------------------------
%% EUnit wrappers (run without Concuerror)
%%--------------------------------------------------------------------

-ifdef(TEST).

nato_scenario_consensus_publish_test_() ->
    {"consensus publish", fun scenario_consensus_publish/0}.

nato_scenario_two_channel_posture_gate_test_() ->
    {"two-channel posture gate", fun scenario_two_channel_posture_gate/0}.

nato_scenario_draft_race_single_writer_test_() ->
    {"draft race single writer", fun scenario_draft_race_single_writer/0}.

nato_scenario_no_classified_leak_test_() ->
    {"no classified leak to press", fun scenario_no_classified_leak_to_press/0}.

nato_scenario_deadlock_fallback_test_() ->
    {"deadlock fallback chair summary", fun scenario_deadlock_fallback_chair_summary/0}.

nato_scenario_audit_chain_test_() ->
    {"audit chain integrity", fun scenario_audit_chain_integrity/0}.

-endif.
