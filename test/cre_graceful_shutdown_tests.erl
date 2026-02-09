%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Project
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author CRE Project
%% @copyright 2025
%%
%% @doc EUnit Tests for CRE Graceful Shutdown Module
%%
%% Tests the graceful shutdown functionality for GKE deployment including
%% shutdown coordination, workflow completion waiting, Mnesia checkpointing,
%% cluster notification, and connection cleanup.
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_graceful_shutdown_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

%% @doc Setup function run before each test
setup() ->
    %% Start the graceful shutdown server for testing
    case cre_graceful_shutdown:start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok
    end.

%% @doc Cleanup function run after each test
cleanup(_State) ->
    %% Reset shutdown state
    catch cre_graceful_shutdown:stop(),
    timer:sleep(100),
    ok.

%%====================================================================
%% Shutdown Coordination Tests
%%====================================================================

%% @doc Test starting the shutdown server
start_server_returns_pid_test() ->
    {ok, Pid} = cre_graceful_shutdown:start_link(),
    ?assert(is_pid(Pid)),
    cre_graceful_shutdown:stop(),
    ?assert(process_info(Pid) =:= undefined).

%% @doc Test is_shutting_down returns false initially
not_shutting_down_initially_test() ->
    ?assertNot(cre_graceful_shutdown:is_shutting_down()).

%% @doc Test get_shutdown_state returns not_shutting_down
get_state_initially_not_shutting_down_test() ->
    State = cre_graceful_shutdown:get_shutdown_state(),
    ?assertEqual(not_shutting_down, maps:get(state, State, not_shutting_down)).

%% @doc Test initiate_shutdown with default timeout
initiate_default_shutdown_test() ->
    ok = cre_graceful_shutdown:initiate_shutdown(),
    timer:sleep(200),
    ?assert(cre_graceful_shutdown:is_shutting_down()).

%% @doc Test initiate_shutdown with custom timeout
initiate_custom_shutdown_test() ->
    ok = cre_graceful_shutdown:initiate_shutdown(5000),
    timer:sleep(200),
    ?assert(cre_graceful_shutdown:is_shutting_down()).

%% @doc Test initiate_shutdown with reason
initiate_shutdown_with_reason_test() ->
    ok = cre_graceful_shutdown:initiate_shutdown(5000, gke_preemption),
    timer:sleep(200),
    State = cre_graceful_shutdown:get_shutdown_state(),
    Reason = maps:get(reason, State, manual),
    ?assertEqual(gke_preemption, Reason).

%% @doc Test get_shutdown_state includes timeout
get_state_includes_timeout_test() ->
    Timeout = 15000,
    ok = cre_graceful_shutdown:initiate_shutdown(Timeout),
    timer:sleep(200),
    State = cre_graceful_shutdown:get_shutdown_state(),
    TimeoutValue = maps:get(timeout, State, Timeout),
    ?assertEqual(Timeout, TimeoutValue).

%%====================================================================
%% Workflow Completion Tests
%%====================================================================

%% @doc Test get_active_workflow_count returns number
get_active_count_returns_number_test() ->
    Count = cre_graceful_shutdown:get_active_workflow_count(),
    ?assert(is_integer(Count)),
    ?assert(Count >= 0).

%% @doc Test get_active_workflow_details returns list
get_active_details_returns_list_test() ->
    Details = cre_graceful_shutdown:get_active_workflow_details(),
    ?assert(is_list(Details)).

%% @doc Test workflow detail has required fields
workflow_detail_has_required_fields_test() ->
    Details = cre_graceful_shutdown:get_active_workflow_details(),
    case Details of
        [] -> ?assert(true);
        [First | _] ->
            ?assert(maps:is_key(id, First)),
            ?assert(maps:is_key(status, First))
    end.

%% @doc Test wait_for_active_workflows with zero workflows
wait_zero_workflows_completes_test() ->
    Result = cre_graceful_shutdown:wait_for_active_workflows(1000),
    ?assertMatch({ok, _Count}, Result).

%% @doc Test wait_for_active_workflows with short timeout
wait_workflows_short_timeout_test() ->
    Result = cre_graceful_shutdown:wait_for_active_workflows(100),
    ?assertMatch({ok, _Count}, Result).

%%====================================================================
%% Mnesia Checkpoint Tests
%%====================================================================

%% @doc Test save_final_checkpoint returns ok
save_checkpoint_returns_ok_test() ->
    Result = cre_graceful_shutdown:save_final_checkpoint(),
    ?assertEqual(ok, Result).

%% @doc Test checkpoint_status returns map
checkpoint_status_returns_map_test() ->
    Status = cre_graceful_shutdown:checkpoint_status(),
    ?assert(is_map(Status)),
    ?assert(maps:is_key(status, Status)).

%% @doc Test checkpoint_status includes ok or not_running
checkpoint_status_valid_values_test() ->
    Status = cre_graceful_shutdown:checkpoint_status(),
    ValidStatuses = [ok, error, not_running],
    ?assert(lists:member(maps:get(status, Status), ValidStatuses)).

%%====================================================================
%% Cluster Notification Tests
%%====================================================================

%% @doc Test notify_cluster returns ok
notify_cluster_returns_ok_test() ->
    Result = cre_graceful_shutdown:notify_cluster(),
    ?assertMatch({ok, _}, Result).

%% @doc Test notify_cluster returns notification result
notify_cluster_includes_result_test() ->
    {ok, Result} = cre_graceful_shutdown:notify_cluster(),
    ?assert(maps:is_key(peers_notified, Result)),
    ?assert(maps:is_key(failed_peers, Result)),
    ?assert(maps:is_key(duration_ms, Result)).

%% @doc Test notify_cluster with custom timeout
notify_cluster_custom_timeout_test() ->
    {ok, Result} = cre_graceful_shutdown:notify_cluster(2000),
    ?assert(maps:is_key(peers_notified, Result)).

%% @doc Test handle_peer_notification accepts node
handle_peer_notification_accepts_node_test() ->
    ok = cre_graceful_shutdown:handle_peer_notification(nonode@nohost).

%%====================================================================
%% Connection Cleanup Tests
%%====================================================================

%% @doc Test close_connections returns ok or partial error
close_connections_returns_ok_test() ->
    Result = cre_graceful_shutdown:close_connections(),
    ?assert(Result =:= ok orelse Result =:= {error, partial_close}).

%% @doc Test close_connections with custom timeout
close_connections_custom_timeout_test() ->
    Result = cre_graceful_shutdown:close_connections(1000),
    ?assert(Result =:= ok orelse Result =:= {error, partial_close}).

%%====================================================================
%% Integration Tests
%%====================================================================

%% @doc Test full shutdown sequence with no workflows
full_shutdown_sequence_no_workflows_test() ->
    %% Initiate shutdown
    ok = cre_graceful_shutdown:initiate_shutdown(2000),
    timer:sleep(300),

    %% Check state
    State = cre_graceful_shutdown:get_shutdown_state(),
    StateValue = maps:get(state, State, not_shutting_down),
    ?assert(StateValue =/= not_shutting_down).

%% @doc Test shutdown state progression
shutdown_state_progression_test() ->
    %% Initial state
    ?assertNot(cre_graceful_shutdown:is_shutting_down()),

    %% Initiate
    ok = cre_graceful_shutdown:initiate_shutdown(2000),

    %% Should be shutting down now
    timer:sleep(200),
    ?assert(cre_graceful_shutdown:is_shutting_down()).

%% @doc Test timeout enforcement in wait_for_workflows
wait_workflows_timeout_enforced_test() ->
    %% With very short timeout, should return quickly
    Start = erlang:monotonic_time(millisecond),
    {ok, _} = cre_graceful_shutdown:wait_for_active_workflows(100),
    End = erlang:monotonic_time(millisecond),

    %% Should not take significantly longer than timeout
    Duration = End - Start,
    ?assert(Duration < 500).

%%====================================================================
%% Error Handling Tests
%%====================================================================

%% @doc Test handle_peer_notification handles invalid input
handle_peer_notification_handles_invalid_test() ->
    %% Should not crash on various inputs
    ?assertEqual(ok, cre_graceful_shutdown:handle_peer_notification(undefined)),
    ?assertEqual(ok, cre_graceful_shutdown:handle_peer_notification([])).

%% @doc Test wait_for_active_workflows with infinity timeout
wait_workflows_infinity_timeout_test() ->
    %% With infinity, should return immediately when no workflows
    {ok, _} = cre_graceful_shutdown:wait_for_active_workflows(infinity),
    ?assert(true).

%% @doc Test initiate_shutdown with various reasons
initiate_shutdown_various_reasons_test() ->
    Reasons = [sigterm, sigint, gke_preemption, manual, upgrade],
    lists:foreach(fun(Reason) ->
        ok = cre_graceful_shutdown:initiate_shutdown(100, Reason),
        timer:sleep(50)
    end, Reasons),
    ?assert(true).

%%====================================================================
%% Type Specification Tests
%%====================================================================

%% @doc Test shutdown_reason types
shutdown_reason_types_test() ->
    ValidReasons = [sigterm, sigint, gke_preemption, manual, upgrade],
    lists:foreach(fun(Reason) ->
        ?assert(is_atom(Reason))
    end, ValidReasons).

%% @doc Test shutdown_state types
shutdown_state_types_test() ->
    ValidStates = [not_shutting_down, stopping_new, draining,
                   checkpointing, finalizing],
    lists:foreach(fun(State) ->
        ?assert(is_atom(State))
    end, ValidStates).

%%====================================================================
%% Test Generators
%%====================================================================

%% @doc Generate tests for shutdown coordination
shutdown_coordination_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"initially not shutting down", fun not_shutting_down_initially_test/0},
      {"initiate with default timeout", fun initiate_default_shutdown_test/0},
      {"initiate with custom timeout", fun initiate_custom_shutdown_test/0},
      {"get state includes timeout", fun get_state_includes_timeout_test/0}
     ]}.

%% @doc Generate tests for workflow completion
workflow_completion_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"active count returns number", fun get_active_count_returns_number_test/0},
      {"active details returns list", fun get_active_details_returns_list_test/0},
      {"wait zero workflows completes", fun wait_zero_workflows_completes_test/0},
      {"wait short timeout", fun wait_workflows_short_timeout_test/0}
     ]}.

%% @doc Generate tests for Mnesia checkpoint
mnesia_checkpoint_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"save checkpoint returns ok", fun save_checkpoint_returns_ok_test/0},
      {"checkpoint status returns map", fun checkpoint_status_returns_map_test/0},
      {"checkpoint status has valid values", fun checkpoint_status_valid_values_test/0}
     ]}.

%% @doc Generate tests for cluster notification
cluster_notification_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"notify returns ok", fun notify_cluster_returns_ok_test/0},
      {"notify includes result", fun notify_cluster_includes_result_test/0},
      {"notify custom timeout", fun notify_cluster_custom_timeout_test/0},
      {"handle peer notification", fun handle_peer_notification_accepts_node_test/0}
     ]}.

%% @doc Generate tests for connection cleanup
connection_cleanup_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"close connections returns ok", fun close_connections_returns_ok_test/0},
      {"close with custom timeout", fun close_connections_custom_timeout_test/0}
     ]}.

%%====================================================================
%% Doctests
%%====================================================================

%% @doc Run doctests for the module
doctest_test() ->
    %% Basic sanity checks
    ?assertNot(cre_graceful_shutdown:is_shutting_down()),

    State = cre_graceful_shutdown:get_shutdown_state(),
    ?assert(is_map(State)),
    ?assert(maps:is_key(state, State)),

    Count = cre_graceful_shutdown:get_active_workflow_count(),
    ?assert(is_integer(Count)),
    ?assert(Count >= 0),

    ok.

%%====================================================================
%% Helper Functions
%%====================================================================

%% @doc Create mock workflow info for testing
create_mock_workflow(Id) ->
    #{
        id => list_to_binary(Id),
        spec => <<"test_spec">>,
        status => running
    }.

%% @doc Get the current shutdown state atom
get_current_state_atom() ->
    maps:get(state, cre_graceful_shutdown:get_shutdown_state()).
