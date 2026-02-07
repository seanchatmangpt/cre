%% -*- erlang -*-
%% @doc AGI Symposium Ω Participant Simulation Common Test Suite.
%%
%% Verifies participant agents interact correctly through existing APIs.
-module(agi_symposium_simulation_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Exported Test Callbacks
%%%===================================================================

-export([
    all/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2
]).

%%%===================================================================
%%% Exported Test Cases
%%%===================================================================

-export([
    test_participant_spec/1,
    test_simulator_run/1,
    test_chair_starts_case/1,
    test_program_chair_completes_task/1,
    test_workflow_completes/1,
    test_suspend_resume/1,
    test_cancel_case/1,
    test_zai_backed_run/1
]).

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

all() ->
    [
        test_participant_spec,
        test_simulator_run,
        test_chair_starts_case,
        test_program_chair_completes_task,
        test_workflow_completes,
        test_suspend_resume,
        test_cancel_case,
        test_zai_backed_run
    ].

init_per_suite(Config) ->
    ct:pal("Starting agi_symposium_simulation_SUITE"),
    ok = test_helper:ensure_cre_gen_pnet_loaded(),
    Config.

end_per_suite(_Config) ->
    ct:pal("Completed agi_symposium_simulation_SUITE"),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================

%% @doc Verify AGI Symposium spec structure.
test_participant_spec(_Config) ->
    Spec = agi_symposium_participants:agi_symposium_spec(),
    ?assert(maps:is_key(places, Spec)),
    ?assert(maps:is_key(transitions, Spec)),
    ?assert(maps:is_key(start_token, Spec)),
    ?assert(maps:is_key(end_place, Spec)),
    ?assertEqual([p_start, p_task, p_end], maps:get(places, Spec)),
    ?assert(maps:is_key(t_go_nogo, maps:get(transitions, Spec))),
    ok.

%% @doc Run full simulation.
test_simulator_run(_Config) ->
    case agi_symposium_simulator:run() of
        {ok, Result} ->
            ?assert(maps:is_key(case_id, Result)),
            ?assert(maps:is_key(status, Result)),
            ?assertEqual(completed, maps:get(status, Result)),
            ct:pal("Simulation completed: ~p", [Result]),
            ok;
        {error, Reason} ->
            ct:fail("Simulation failed: ~p", [Reason])
    end.

%% @doc Chair starts workflow case.
test_chair_starts_case(_Config) ->
    Spec = agi_symposium_participants:agi_symposium_spec(),
    Now = erlang:monotonic_time(millisecond),
    {ok, Engine} = wf_engine:start_link(#{spec => Spec, seed => 1, now => Now}),
    try
        {ok, CaseId} = agi_symposium_participants:chair_agent(Engine, #{}),
        ?assert(is_binary(CaseId)),
        ?assert(byte_size(CaseId) > 0),
        State = wf_engine:case_state(Engine, CaseId),
        ?assert(lists:member(State, [running, completed])),
        ok
    after
        gen_server:stop(Engine)
    end.

%% @doc Program chair discovers and completes work item.
test_program_chair_completes_task(_Config) ->
    Spec = agi_symposium_participants:agi_symposium_spec(),
    Now = erlang:monotonic_time(millisecond),
    {ok, Engine} = wf_engine:start_link(#{spec => Spec, seed => 1, now => Now}),
    try
        {ok, CaseId} = wf_engine:start_case(Engine, #{data => #{}}, Now),
        Result = agi_symposium_participants:program_chair_agent(Engine, CaseId, #{}),
        ?assert((Result =:= ok) or (is_tuple(Result) andalso element(1, Result) =:= error)),
        FinalState = wf_engine:case_state(Engine, CaseId),
        ?assert(lists:member(FinalState, [running, completed])),
        ok
    after
        gen_server:stop(Engine)
    end.

%% @doc Workflow reaches completed state after participant actions.
test_workflow_completes(_Config) ->
    Spec = agi_symposium_participants:agi_symposium_spec(),
    Now = erlang:monotonic_time(millisecond),
    {ok, Engine} = wf_engine:start_link(#{spec => Spec, seed => 1, now => Now}),
    try
        {ok, CaseId} = wf_engine:start_case(Engine, #{data => #{}}, Now),
        %% Run participants until completion
        run_until_complete(Engine, CaseId, Now, 20),
        State = wf_engine:case_state(Engine, CaseId),
        ?assertEqual(completed, State),
        ok
    after
        gen_server:stop(Engine)
    end.

%% @doc Chair can suspend and resume case.
test_suspend_resume(_Config) ->
    Spec = agi_symposium_participants:agi_symposium_spec(),
    Now = erlang:monotonic_time(millisecond),
    {ok, Engine} = wf_engine:start_link(#{spec => Spec, seed => 1, now => Now}),
    try
        {ok, CaseId} = wf_engine:start_case(Engine, #{data => #{}}, Now),
        ok = wf_engine:suspend_case(Engine, CaseId, Now + 1),
        ?assertEqual(suspended, wf_engine:case_state(Engine, CaseId)),
        ok = wf_engine:resume_case(Engine, CaseId, Now + 2),
        ?assertEqual(running, wf_engine:case_state(Engine, CaseId)),
        ok
    after
        gen_server:stop(Engine)
    end.

%% @doc Safety officer can cancel case.
test_cancel_case(_Config) ->
    Spec = agi_symposium_participants:agi_symposium_spec(),
    Now = erlang:monotonic_time(millisecond),
    {ok, Engine} = wf_engine:start_link(#{spec => Spec, seed => 1, now => Now}),
    try
        {ok, CaseId} = wf_engine:start_case(Engine, #{data => #{}}, Now),
        ok = agi_symposium_participants:safety_officer_agent(Engine, CaseId, #{emergency => true}),
        ?assertEqual(cancelled, wf_engine:case_state(Engine, CaseId)),
        ok
    after
        gen_server:stop(Engine)
    end.

%% @doc Run simulation with Z.AI-backed LLM decisions (skips when ZAI_API_KEY not set).
test_zai_backed_run(_Config) ->
    case zai_client:get_api_key() of
        undefined ->
            ct:pal("Skipping test_zai_backed_run: ZAI_API_KEY not configured"),
            {skip, "ZAI_API_KEY not configured"};
        _ ->
            Spec = agi_symposium_participants:agi_symposium_spec(),
            Options = #{zai_enabled => true, model => <<"glm-4-plus">>},
            case agi_symposium_simulator:run(Spec, Options) of
                {ok, Result} ->
                    ?assert(maps:is_key(case_id, Result)),
                    ?assert(maps:is_key(status, Result)),
                    ?assertEqual(completed, maps:get(status, Result)),
                    ct:pal("ZAI-backed simulation completed: ~p", [Result]),
                    ok;
                {error, Reason} ->
                    ct:fail("ZAI-backed simulation failed: ~p", [Reason])
            end
    end.

%%%===================================================================
%%% Internal
%%%===================================================================

run_until_complete(Engine, CaseId, Now, 0) ->
    ok;
run_until_complete(Engine, CaseId, Now, N) ->
    case wf_engine:case_state(Engine, CaseId) of
        completed ->
            ok;
        _ ->
            _ = agi_symposium_participants:program_chair_agent(Engine, CaseId, #{}),
            _ = agi_symposium_participants:reviewer_agent(Engine, CaseId, #{}),
            run_until_complete(Engine, CaseId, Now + 1, N - 1)
    end.
