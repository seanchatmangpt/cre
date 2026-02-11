%% -*- erlang -*-
%%%% @doc Test suite for Mnesia checkpoint + resume functionality.
%%
%% This test suite validates the complete checkpoint and resume flow:
%% <ul>
%%   <li>gen_pnet:marking/1 returns the current marking</li>
%%   <li>gen_pnet:usr_info/1 returns the usr_info state</li>
%%   <li>wf_store:put_instance/4 saves workflow state</li>
%%   <li>wf_store:get_instance/2 loads workflow state</li>
%%   <li>wf_test_net_resume resumes from saved state</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_checkpoint_resume_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test slice 6 of compound doctests - Mnesia checkpoint + resume.
%%
%% Verifies:
%% 1. gen_pnet:marking/1 returns marking
%% 2. gen_pnet:usr_info/1 returns usr_info
%% 3. wf_store:put_instance/4 saves state
%% 4. wf_store:get_instance/2 loads state
%% 5. wf_test_net_resume resumes from saved state
%%--------------------------------------------------------------------
checkpoint_resume_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(Store) ->
         [
          {"marking returns current marking", fun() -> marking_returns_current_marking() end},
          {"usr_info returns usr_info", fun() -> usr_info_returns_usr_info() end},
          {"put_instance saves state", fun() -> put_instance_saves_state(Store) end},
          {"get_instance loads state", fun() -> get_instance_loads_state(Store) end},
          {"resume restores marking", fun() -> resume_restores_marking() end},
          {"full checkpoint resume flow", fun() -> full_checkpoint_resume_flow(Store) end}
         ]
     end}.

%%--------------------------------------------------------------------
%% @doc Setup function - opens the wf_store.
%%--------------------------------------------------------------------
setup() ->
    {ok, Store} = wf_store:open(#{backend => mnesia, dir => tmp}),
    Store.

%%--------------------------------------------------------------------
%% @doc Cleanup function - closes the wf_store.
%%--------------------------------------------------------------------
cleanup(Store) ->
    ok = wf_store:close(Store).

%%====================================================================
%% Individual Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test that gen_pnet:marking/1 returns the current marking.
%%--------------------------------------------------------------------
marking_returns_current_marking() ->
    % Start a gen_pnet instance
    {ok, P} = gen_pnet:start_link(wf_test_net_resume, #{}, []),

    % Initially, marking should be empty (no tokens)
    Mark0 = gen_pnet:marking(P),
    ?assertEqual(#{p1 => [], p2 => []}, Mark0),

    % Inject tokens into place p1
    {ok, _} = gen_pnet:inject(P, #{p1 => [a, b]}),

    % After injection, marking should reflect the injected tokens
    Mark1 = gen_pnet:marking(P),
    ?assertEqual([a, b], lists:sort(maps:get(p1, Mark1))),
    ?assertEqual([], maps:get(p2, Mark1)),

    % Clean up
    ok = gen_pnet:stop(P).

%%--------------------------------------------------------------------
%% @doc Test that gen_pnet:usr_info/1 returns the usr_info.
%%--------------------------------------------------------------------
usr_info_returns_usr_info() ->
    % Start a gen_pnet instance with init arg
    InitArg = #{test => data},
    {ok, P} = gen_pnet:start_link(wf_test_net_resume, InitArg, []),

    % usr_info returns whatever the module's init/1 returns
    UsrInfo = gen_pnet:usr_info(P),
    % For wf_test_net_resume with no resume, init returns empty map
    ?assert(is_map(UsrInfo)),
    % The actual content depends on the module's init implementation
    % Just verify it's a map

    % Clean up
    ok = gen_pnet:stop(P).

%%--------------------------------------------------------------------
%% @doc Test that wf_store:put_instance/4 saves state.
%%--------------------------------------------------------------------
put_instance_saves_state(Store) ->
    InstId = <<"wf-test-instance">>,

    % Save instance data - data should be in the 'data' field
    InstanceData = #{data => #{marking => #{p1 => [x, y], p2 => [z]},
                            usr_info => #{key => value}}},
    ok = wf_store:put_instance(Store, InstId, InstanceData, undefined),

    % Verify it was saved - data is in the 'data' field
    {ok, Loaded} = wf_store:get_instance(Store, InstId),
    Data = maps:get(data, Loaded),
    ?assertEqual(#{p1 => [x, y], p2 => [z]}, maps:get(marking, Data)),
    ?assertEqual(#{key => value}, maps:get(usr_info, Data)).

%%--------------------------------------------------------------------
%% @doc Test that wf_store:get_instance/2 loads state.
%%--------------------------------------------------------------------
get_instance_loads_state(Store) ->
    InstId = <<"wf-test-load">>,

    % Try loading non-existent instance
    not_found = wf_store:get_instance(Store, InstId),

    % Save an instance - data should be in the 'data' field
    InstanceData = #{data => #{marking => #{p1 => [test]},
                            usr_info => #{context => test_context}}},
    ok = wf_store:put_instance(Store, InstId, InstanceData, undefined),

    % Load it back
    {ok, Loaded} = wf_store:get_instance(Store, InstId),
    Data = maps:get(data, Loaded),
    ?assertEqual(#{p1 => [test]}, maps:get(marking, Data)),
    ?assertEqual(#{context => test_context}, maps:get(usr_info, Data)).

%%--------------------------------------------------------------------
%% @doc Test that wf_test_net_resume resumes from saved state.
%%--------------------------------------------------------------------
resume_restores_marking() ->
    % Create a saved state with specific marking
    SavedMarking = #{p1 => [token1, token2], p2 => [token3]},
    SavedUsrInfo = #{user => test_user, context => recovery_test},

    % Start a new instance with resume data
    ResumeArg = #{resume => #{marking => SavedMarking, usr_info => SavedUsrInfo}},
    {ok, P} = gen_pnet:start_link(wf_test_net_resume, ResumeArg, []),

    % The marking should be restored
    RestoredMarking = gen_pnet:marking(P),
    ?assertEqual([token1, token2], lists:sort(maps:get(p1, RestoredMarking))),
    ?assertEqual([token3], maps:get(p2, RestoredMarking)),

    % Clean up
    ok = gen_pnet:stop(P).

%%====================================================================
%% Complete Flow Test
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test the complete checkpoint + resume flow.
%%
%% This test implements the full flow from the user specification:
%% 1. Open wf_store
%% 2. Start gen_pnet instance
%% 3. Inject tokens
%% 4. Drain (process transitions)
%% 5. Get marking and usr_info
%% 6. Save to wf_store
%% 7. Stop instance
%% 8. Load from wf_store
%% 9. Resume new instance with saved state
%% 10. Verify marking is restored
%%--------------------------------------------------------------------
full_checkpoint_resume_flow(Store) ->
    InstId = <<"wf-checkpoint-test">>,

    % Step 1: Start a gen_pnet instance
    {ok, P0} = gen_pnet:start_link(wf_test_net_resume, #{seed => 3}, []),

    % Step 2: Inject tokens
    {ok, _} = gen_pnet:inject(P0, #{p1 => [a, b]}),

    % Step 3: Drain (no transitions in wf_test_net_resume, so should complete quickly)
    {ok, _} = gen_pnet:drain(P0, 10),

    % Step 4: Get marking and usr_info
    Mark0 = gen_pnet:marking(P0),
    Usr0 = gen_pnet:usr_info(P0),

    % Verify we got the expected marking
    ?assertEqual([a, b], lists:sort(maps:get(p1, Mark0))),
    ?assertEqual([], maps:get(p2, Mark0)),

    % Step 5: Save to wf_store
    Snapshot = #{marking => Mark0, usr_info => Usr0},
    ok = wf_store:put_instance(Store, InstId, #{data => Snapshot}, undefined),

    % Step 6: Stop the instance
    ok = gen_pnet:stop(P0),

    % Step 7: Load the saved state
    {ok, LoadedSnap} = wf_store:get_instance(Store, InstId),
    LoadedData = maps:get(data, LoadedSnap),
    LoadedMarking = maps:get(marking, LoadedData),

    % Verify loaded marking matches saved marking
    ?assertEqual([a, b], lists:sort(maps:get(p1, LoadedMarking))),
    ?assertEqual([], maps:get(p2, LoadedMarking)),

    % Step 8: Resume with the saved state
    ResumeArg = #{resume => LoadedData},
    {ok, P1} = gen_pnet:start_link(wf_test_net_resume, ResumeArg, []),

    % Step 9: Verify the marking is restored
    RestoredMarking = gen_pnet:marking(P1),
    ?assertEqual([a, b], lists:sort(maps:get(p1, RestoredMarking))),
    ?assertEqual([], maps:get(p2, RestoredMarking)),

    % Step 10: Verify the restored marking matches the snapshot
    ?assertEqual(LoadedMarking, RestoredMarking),

    % Clean up
    ok = gen_pnet:stop(P1).

%%====================================================================
%% Edge Case Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test resume with empty marking.
%%--------------------------------------------------------------------
resume_empty_marking_test() ->
    SavedMarking = #{p1 => [], p2 => []},
    SavedUsrInfo = #{},

    ResumeArg = #{resume => #{marking => SavedMarking, usr_info => SavedUsrInfo}},
    {ok, P} = gen_pnet:start_link(wf_test_net_resume, ResumeArg, []),

    RestoredMarking = gen_pnet:marking(P),
    ?assertEqual([], maps:get(p1, RestoredMarking)),
    ?assertEqual([], maps:get(p2, RestoredMarking)),

    ok = gen_pnet:stop(P).

%%--------------------------------------------------------------------
%% @doc Test multiple checkpoint cycles.
%%--------------------------------------------------------------------
multiple_checkpoint_cycles_test_() ->
    {setup,
     fun() -> {ok, Store} = wf_store:open(#{backend => mnesia, dir => tmp}), Store end,
     fun(Store) -> wf_store:close(Store) end,
     fun(Store) ->
         [
          {"first checkpoint", fun() -> checkpoint_cycle(Store, 1) end},
          {"second checkpoint", fun() -> checkpoint_cycle(Store, 2) end},
          {"third checkpoint", fun() -> checkpoint_cycle(Store, 3) end}
         ]
     end}.

checkpoint_cycle(Store, CycleNum) ->
    InstId = list_to_binary("wf-cycle-" ++ integer_to_list(CycleNum)),

    % Create and save
    {ok, P0} = gen_pnet:start_link(wf_test_net_resume, #{}, []),
    Tokens = [t1, t2, t3],
    {ok, _} = gen_pnet:inject(P0, #{p1 => Tokens}),
    Mark0 = gen_pnet:marking(P0),
    Usr0 = gen_pnet:usr_info(P0),

    ok = wf_store:put_instance(Store, InstId,
                                #{data => #{marking => Mark0, usr_info => Usr0}},
                                undefined),
    ok = gen_pnet:stop(P0),

    % Resume and verify
    {ok, Snap} = wf_store:get_instance(Store, InstId),
    Data = maps:get(data, Snap),
    {ok, P1} = gen_pnet:start_link(wf_test_net_resume, #{resume => Data}, []),
    Mark1 = gen_pnet:marking(P1),

    ?assertEqual(Tokens, lists:sort(maps:get(p1, Mark1))),
    ok = gen_pnet:stop(P1).
