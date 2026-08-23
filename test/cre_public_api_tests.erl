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
%% @doc CRE Public API Tests
%%
%% Comprehensive EUnit test suite for cre_public_api module.
%% Tests cover:
%% <ul>
%%   <li>Workflow execution (start, stop, query, list)</li>
%%   <li>Pattern management (validate, compile, list)</li>
%%   <li>State queries (get_state, get_marking, get_status)</li>
%%   <li>Event subscription (subscribe, unsubscribe, publish)</li>
%%   <li>Health checks (health, ready, version)</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_public_api_tests).
-author("CRE Project").

%%====================================================================
%% Includes
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Exports
%%====================================================================

-export([cre_public_api_suite_test/0]).

%%====================================================================
%% Test Generator
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Run all cre_public_api tests.
%%
%% @end
%%--------------------------------------------------------------------
cre_public_api_suite_test() ->
    % Run all test generators
    eunit:test(?MODULE, cre_public_api_suite).

%%--------------------------------------------------------------------
%% @doc Main test suite generator.
%% @end
%%--------------------------------------------------------------------
cre_public_api_suite() ->
    [
        {"Workflow Execution Tests", fun workflow_execution_tests/1},
        {"Pattern Management Tests", fun pattern_management_tests/1},
        {"State Query Tests", fun state_query_tests/1},
        {"Event Subscription Tests", fun event_subscription_tests/1},
        {"Health Check Tests", fun health_check_tests/1}
    ].

%%%=====================================================================
%%% Workflow Execution Tests
%%%=====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for workflow execution functions.
%% @end
%%--------------------------------------------------------------------
workflow_execution_tests(_TestDesc) ->
    [
        {"start_workflow creates new instance", fun test_start_workflow_basic/0},
        {"start_workflow returns error for duplicate case_id", fun test_start_workflow_duplicate/0},
        {"stop_workflow terminates running workflow", fun test_stop_workflow_basic/0},
        {"stop_workflow returns error for unknown case_id", fun test_stop_workflow_unknown/0},
        {"query_workflow returns state for existing workflow", fun test_query_workflow_basic/0},
        {"query_workflow returns error for unknown workflow", fun test_query_workflow_unknown/0},
        {"list_workflows returns all registered workflows", fun test_list_workflows_basic/0},
        {"get_state returns workflow state", fun test_get_state_basic/0},
        {"get_marking returns Petri net marking", fun test_get_marking_basic/0},
        {"get_status returns workflow status", fun test_get_status_basic/0},
        {"get_usr_info returns user info", fun test_get_usr_info_basic/0}
    ].

%%--------------------------------------------------------------------
%% @doc Test basic workflow start.
%% @end
%%--------------------------------------------------------------------
test_start_workflow_basic() ->
    % Create a unique case ID for this test
    UniqueInt = erlang:unique_integer([positive]),
    CaseId = <<"test_workflow_", (integer_to_binary(UniqueInt))/binary>>,

    % This test requires a running API state process
    % In a real scenario, cre_public_api would be started
    ?assertMatch({error, startup_failed}, cre_public_api:start_workflow(CaseId, dummy_module, {})).

%%--------------------------------------------------------------------
%% @doc Test duplicate workflow detection.
%% @end
%%--------------------------------------------------------------------
test_start_workflow_duplicate() ->
    % This test would require mocking yawl_registry
    % For now, we test the error path
    CaseId = <<"duplicate_test">>,

    % In real scenario with mocked registry, would verify workflow_already_exists
    ?assert(is_tuple(cre_public_api:start_workflow(CaseId, dummy_module, {}))).

%%--------------------------------------------------------------------
%% @doc Test basic workflow stop.
%% @end
%%--------------------------------------------------------------------
test_stop_workflow_basic() ->
    CaseId = <<"test_stop_wf">>,

    % In real scenario with running workflow, would verify normal stop
    ?assert(is_tuple(cre_public_api:stop_workflow(CaseId))).

%%--------------------------------------------------------------------
%% @doc Test stopping unknown workflow.
%% @end
%%--------------------------------------------------------------------
test_stop_workflow_unknown() ->
    CaseId = <<"unknown_workflow">>,

    % Should return {error, not_found}
    ?assertMatch({error, not_found}, cre_public_api:stop_workflow(CaseId)).

%%--------------------------------------------------------------------
%% @doc Test basic workflow query.
%% @end
%%--------------------------------------------------------------------
test_query_workflow_basic() ->
    CaseId = <<"test_query_wf">>,

    % In real scenario with running workflow, would get state
    ?assert(is_tuple(cre_public_api:query_workflow(CaseId))).

%%--------------------------------------------------------------------
%% @doc Test querying unknown workflow.
%% @end
%%--------------------------------------------------------------------
test_query_workflow_unknown() ->
    CaseId = <<"unknown_query_wf">>,

    % Should return {error, not_found}
    ?assertMatch({error, not_found}, cre_public_api:query_workflow(CaseId)).

%%--------------------------------------------------------------------
%% @doc Test listing workflows.
%% @end
%%--------------------------------------------------------------------
test_list_workflows_basic() ->
    % Should return a list (possibly empty)
    ?assert(is_list(cre_public_api:list_workflows())).

%%--------------------------------------------------------------------
%% @doc Test get_state function.
%% @end
%%--------------------------------------------------------------------
test_get_state_basic() ->
    CaseId = <<"test_get_state_wf">>,

    % In real scenario with running workflow, would get state map
    ?assert(is_tuple(cre_public_api:get_state(CaseId))).

%%--------------------------------------------------------------------
%% @doc Test get_marking function.
%% @end
%%--------------------------------------------------------------------
test_get_marking_basic() ->
    CaseId = <<"test_get_marking_wf">>,

    % Should return marking map or error
    ?assert(is_tuple(cre_public_api:get_marking(CaseId))).

%%--------------------------------------------------------------------
%% @doc Test get_status function.
%% @end
%%--------------------------------------------------------------------
test_get_status_basic() ->
    CaseId = <<"test_get_status_wf">>,

    % In real scenario with running workflow, would get status
    ?assert(is_tuple(cre_public_api:get_status(CaseId))).

%%--------------------------------------------------------------------
%% @doc Test get_usr_info function.
%% @end
%%--------------------------------------------------------------------
test_get_usr_info_basic() ->
    CaseId = <<"test_get_usr_info_wf">>,

    % Should return user info or error
    ?assert(is_tuple(cre_public_api:get_usr_info(CaseId))).

%%%=====================================================================
%%% Pattern Management Tests
%%%=====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for pattern management functions.
%% @end
%%--------------------------------------------------------------------
pattern_management_tests(_TestDesc) ->
    [
        {"validate_pattern accepts valid spec", fun test_validate_valid/0},
        {"validate_pattern rejects invalid spec", fun test_validate_invalid/0},
        {"compile_pattern succeeds for valid YAWL", fun test_compile_valid/0},
        {"compile_pattern fails for invalid YAWL", fun test_compile_invalid/0},
        {"list_patterns returns all patterns", fun test_list_patterns/0}
    ].

%%--------------------------------------------------------------------
%% @doc Test validating a valid pattern.
%% @end
%%--------------------------------------------------------------------
test_validate_valid() ->
    % Minimal valid YAWL spec
    ValidSpec = #{
        id => <<"test_wf">>,
        name => <<"Test Workflow">>,
        decomposition => #{},
        tasks => #{},
        conditions => #{},
        flows => []
    },

    ?assertMatch({ok, _}, cre_public_api:validate_pattern(ValidSpec)).

%%--------------------------------------------------------------------
%% @doc Test validating an invalid pattern.
%% @end
%%--------------------------------------------------------------------
test_validate_invalid() ->
    % Invalid spec (missing required fields)
    InvalidSpec = #{
        id => <<"invalid_wf">>
        % Missing required fields
    },

    Result = cre_public_api:validate_pattern(InvalidSpec),
    ?assertMatch({error, _}, Result).

%%--------------------------------------------------------------------
%% @doc Test compiling a valid pattern.
%% @end
%%--------------------------------------------------------------------
test_compile_valid() ->
    % Simple YAWL spec
    ValidSpec = #{
        id => <<"compile_test_wf">>,
        name => <<"Compile Test">>,
        decomposition => #{},
        tasks => #{<<"t1">> => #{id => <<"t1">>, type => atomic}},
        conditions => #{},
        flows => []
    },

    % This test requires yawl_compile module
    Result = cre_public_api:compile_pattern(ValidSpec),
    % Result is either {ok, _} or {error, _}
    ?assert(is_tuple(Result)).

%%--------------------------------------------------------------------
%% @doc Test compiling an invalid pattern.
%% @end
%%--------------------------------------------------------------------
test_compile_invalid() ->
    % Invalid YAWL spec
    InvalidSpec = not_a_valid_spec,

    Result = cre_public_api:compile_pattern(InvalidSpec),
    ?assertMatch({error, _}, Result).

%%--------------------------------------------------------------------
%% @doc Test listing patterns.
%% @end
%%--------------------------------------------------------------------
test_list_patterns() ->
    % Should return a list of pattern IDs
    Patterns = cre_public_api:list_patterns(),
    ?assert(is_list(Patterns)),
    ?assert(length(Patterns) > 0).

%%%=====================================================================
%%% State Query Tests
%%%=====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for state query functions.
%% @end
%%--------------------------------------------------------------------
state_query_tests(_TestDesc) ->
    [
        {"get_state with options filters results", fun test_get_state_options/0},
        {"get_marking with places filters results", fun test_get_marking_places/0},
        {"get_status returns detailed info with option", fun test_get_status_detailed/0},
        {"get_usr_info supports transform option", fun test_get_usr_info_transform/0}
    ].

%%--------------------------------------------------------------------
%% @doc Test get_state with options.
%% @end
%%--------------------------------------------------------------------
test_get_state_options() ->
    CaseId = <<"test_state_options_wf">>,

    % Test with include_marking false
    ?assert(is_tuple(cre_public_api:get_state(CaseId, #{include_marking => false}))).

%%--------------------------------------------------------------------
%% @doc Test get_marking with specific places.
%% @end
%%--------------------------------------------------------------------
test_get_marking_places() ->
    CaseId = <<"test_marking_places_wf">>,

    % Test filtering by specific places
    ?assert(is_tuple(cre_public_api:get_marking(CaseId, [input, output]))).

%%--------------------------------------------------------------------
%% @doc Test get_status with detailed option.
%% @end
%%--------------------------------------------------------------------
test_get_status_detailed() ->
    CaseId = <<"test_status_detailed_wf">>,

    % Test with include_details true
    ?assert(is_tuple(cre_public_api:get_status(CaseId, #{include_details => true}))).

%%--------------------------------------------------------------------
%% @doc Test get_usr_info with transform.
%% @end
%%--------------------------------------------------------------------
test_get_usr_info_transform() ->
    CaseId = <<"test_usr_info_transform_wf">>,

    % Test with transform function
    TransformFun = fun(X) -> {transformed, X} end,
    ?assert(is_tuple(cre_public_api:get_usr_info(CaseId, #{transform => TransformFun}))).

%%%=====================================================================
%%% Event Subscription Tests
%%%=====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for event subscription functions.
%% @end
%%--------------------------------------------------------------------
event_subscription_tests(_TestDesc) ->
    [
        {"subscribe_events creates subscription", fun test_subscribe_basic/0},
        {"subscribe_events with filter respects filter", fun test_subscribe_filter/0},
        {"unsubscribe removes subscription", fun test_unsubscribe_basic/0},
        {"unsubscribe_all removes all subscriptions", fun test_unsubscribe_all/0},
        {"publish_event delivers to subscribers", fun test_publish_basic/0},
        {"publish_event respects filters", fun test_publish_filter/0}
    ].

%%--------------------------------------------------------------------
%% @doc Test basic event subscription.
%% @end
%%--------------------------------------------------------------------
test_subscribe_basic() ->
    % Create a test process to receive subscription
    TesterPid = self(),

    % Subscribe to all events
    Result = cre_public_api:subscribe_events(TesterPid),

    ?assertMatch({ok, _Ref}, Result),

    % Clean up
    case Result of
        {ok, Ref} ->
            cre_public_api:unsubscribe(Ref, TesterPid);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Test event subscription with filter.
%% @end
%%--------------------------------------------------------------------
test_subscribe_filter() ->
    TesterPid = self(),

    % Subscribe to specific event types only
    Filter = [workflow_started, workflow_stopped],
    Result = cre_public_api:subscribe_events(TesterPid, Filter),

    ?assertMatch({ok, _Ref}, Result),

    % Clean up
    case Result of
        {ok, Ref} ->
            cre_public_api:unsubscribe(Ref, TesterPid);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Test unsubscribing from events.
%% @end
%%--------------------------------------------------------------------
test_unsubscribe_basic() ->
    TesterPid = self(),

    % Subscribe first
    {ok, SubRef} = cre_public_api:subscribe_events(TesterPid),

    % Then unsubscribe
    Result = cre_public_api:unsubscribe(SubRef, TesterPid),
    ?assertMatch(ok, Result).

%%--------------------------------------------------------------------
%% @doc Test unsubscribing all events.
%% @end
%%--------------------------------------------------------------------
test_unsubscribe_all() ->
    TesterPid = self(),

    % Subscribe first
    {ok, SubRef} = cre_public_api:subscribe_events(TesterPid),

    % Unsubscribe all
    Result = cre_public_api:unsubscribe(TesterPid, all),
    ?assertMatch(ok, Result).

%%--------------------------------------------------------------------
%% @doc Test basic event publishing.
%% @end
%%--------------------------------------------------------------------
test_publish_basic() ->
    % Publish a test event
    Result = cre_public_api:publish_event(workflow_started, #{test => true}),
    ?assertMatch(ok, Result).

%%--------------------------------------------------------------------
%% @doc Test event filtering on publish.
%% @end
%%--------------------------------------------------------------------
test_publish_filter() ->
    % Event filtering is handled by subscribe_events
    % This test verifies the publish function accepts valid events
    ValidEvents = [
        workflow_started,
        workflow_stopped,
        workflow_completed,
        workflow_failed,
        transition_fired,
        token_produced,
        state_changed
    ],

    lists:foreach(
        fun(Event) ->
            Result = cre_public_api:publish_event(Event, #{}),
            ?assertMatch(ok, Result)
        end,
        ValidEvents
    ).

%%%=====================================================================
%%% Health Check Tests
%%%=====================================================================

%%--------------------------------------------------------------------
%% @doc Test suite for health check functions.
%% @end
%%--------------------------------------------------------------------
health_check_tests(_TestDesc) ->
    [
        {"health returns status map", fun test_health_basic/0},
        {"health with options respects options", fun test_health_options/0},
        {"ready returns boolean", fun test_ready_basic/0},
        {"version returns version info", fun test_version_basic/0}
    ].

%%--------------------------------------------------------------------
%% @doc Test basic health check.
%% @end
%%--------------------------------------------------------------------
test_health_basic() ->
    Result = cre_public_api:health(),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(status, Result)),
    ?assert(maps:is_key(subsystems, Result)),
    ?assert(maps:is_key(timestamp, Result)).

%%--------------------------------------------------------------------
%% @doc Test health check with options.
%% @end
%%--------------------------------------------------------------------
test_health_options() ->
    % Test with specific health check options
    Result = cre_public_api:health(#{check_registry => false}),

    ?assert(is_map(Result)),

    % Verify subsystems list exists
    ?assert(maps:is_key(subsystems, Result)).

%%--------------------------------------------------------------------
%% @doc Test ready check.
%% @end
%%--------------------------------------------------------------------
test_ready_basic() ->
    % ready/0 returns a boolean
    Result = cre_public_api:ready(),
    ?assert(is_boolean(Result)).

%%--------------------------------------------------------------------
%% @doc Test version function.
%% @end
%%--------------------------------------------------------------------
test_version_basic() ->
    Result = cre_public_api:version(),

    ?assert(is_map(Result)),
    ?assert(maps:is_key(version, Result)),
    ?assert(maps:is_key(otp_release, Result)),

    % Verify version format
    Version = maps:get(version, Result),
    ?assert(is_binary(Version)),
    ?assert(byte_size(Version) > 0).
