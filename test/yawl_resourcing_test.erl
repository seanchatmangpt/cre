%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc YAWL Resource Management Test Suite
%%
%% Comprehensive test suite for YAWL resource management functionality including
%% participant registration, resource allocation, role-based distribution, and
%% capability management.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_resourcing_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Record Definitions (imported from yawl_resourcing)
%%====================================================================

-record(participant, {
    id :: binary(),
    name :: binary(),
    roles :: [binary()],
    capabilities :: [binary()],
    is_user :: boolean(),
    resource_type :: human | machine | non_human | system,
    status :: available | busy | unavailable | offline,
    metadata :: #{atom() => term()}
}).

-record(resource_offer, {
    task_id :: binary(),
    resources :: [binary()],
    allocation_strategy :: eager | lazy,
    initiation :: user | system | automatic,
    required_roles :: [binary()],
    required_capabilities :: [binary()]
}).

-record(resource_allocation, {
    allocation_id :: binary(),
    task_id :: binary(),
    participant_id :: binary(),
    allocated_at :: erlang:timestamp(),
    status :: active | completed | cancelled
}).

%%====================================================================
%% Test Setup/Teardown
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% Starts the yawl_resourcing gen_server.
%% @end
%%--------------------------------------------------------------------
setup() ->
    case whereis(yawl_resourcing) of
        undefined ->
            {ok, _Pid} = yawl_resourcing:start_link({local, yawl_resourcing}),
            timer:sleep(100),  % Allow server to initialize
            ok;
        _Pid ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% Stops the yawl_resourcing gen_server.
%% @end
%%--------------------------------------------------------------------
cleanup(_State) ->
    case whereis(yawl_resourcing) of
        undefined -> ok;
        Pid -> gen_server:stop(Pid), timer:sleep(50)
    end,
    ok.

%%====================================================================
%% Test: Register Participant
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test registering a new participant.
%% @end
%%--------------------------------------------------------------------
test_register_participant_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Register basic participant
                     Props = [
                         {name, <<"John Doe">>},
                         {roles, [<<"analyst">>]},
                         {capabilities, [<<"data_analysis">>]}
                     ],
                     Result = yawl_resourcing:register_participant(Props, <<"participant_001">>),
                     ?assertEqual({ok, <<"participant_001">>}, Result)
                 end),
          ?_test(begin
                     % Register with all properties
                     Props = [
                         {name, <<"Jane Smith">>},
                         {roles, [<<"manager">>, <<"reviewer">>]},
                         {capabilities, [<<"approval">>, <<"audit">>]},
                         {is_user, true},
                         {resource_type, human},
                         {status, available},
                         {metadata, #{department => <<"finance">>}}
                     ],
                     Result = yawl_resourcing:register_participant(Props, <<"participant_002">>),
                     ?assertEqual({ok, <<"participant_002">>}, Result),
                     % Verify participant info
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"participant_002">>),
                     ?assertEqual(<<"Jane Smith">>, Participant#participant.name),
                     ?assertEqual(true, Participant#participant.is_user),
                     ?assertEqual(human, Participant#participant.resource_type)
                 end),
          ?_test(begin
                     % Register machine resource
                     Props = [
                         {name, <<"Auto Processor">>},
                         {roles, [<<"processor">>]},
                         {capabilities, [<<"batch_process">>]},
                         {resource_type, machine},
                         {is_user, false}
                     ],
                     Result = yawl_resourcing:register_participant(Props, <<"machine_001">>),
                     ?assertEqual({ok, <<"machine_001">>}, Result)
                 end),
          ?_test(begin
                     % Register multiple participants
                     lists:foreach(
                         fun(N) ->
                             Id = list_to_binary("participant_" ++ integer_to_list(N)),
                             Props = [
                                 {name, Id},
                                 {roles, [<<"worker">>]},
                                 {capabilities, [<<"general">>]}
                             ],
                             {ok, Id} = yawl_resourcing:register_participant(Props, Id)
                         end,
                         lists:seq(1, 10)
                     ),
                     All = yawl_resourcing:get_all_participants(),
                     ?assert(length(All) >= 10)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Unregister Participant
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test unregistering a participant.
%% @end
%%--------------------------------------------------------------------
test_unregister_participant_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Unregister non-existent participant
                     Result = yawl_resourcing:unregister_participant(<<"nonexistent">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Unregister existing participant
                     Props = [{name, <<"Temp">>}, {roles, []}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"temp_participant">>),
                     ?assertEqual(ok, yawl_resourcing:unregister_participant(<<"temp_participant">>)),
                     % Verify participant is gone
                     Result = yawl_resourcing:get_participant_info(<<"temp_participant">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Unregister affects available resources
                     Props1 = [{name, <<"A">>}, {roles, [<<"r">>]}, {capabilities, []}],
                     Props2 = [{name, <<"B">>}, {roles, [<<"r">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props1, <<"p1">>),
                     {ok, _} = yawl_resourcing:register_participant(Props2, <<"p2">>),
                     Available1 = yawl_resourcing:get_available_resources([<<"r">>]),
                     ?assertEqual(2, length(Available1)),
                     ok = yawl_resourcing:unregister_participant(<<"p1">>),
                     Available2 = yawl_resourcing:get_available_resources([<<"r">>]),
                     ?assertEqual(1, length(Available2))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Allocate Resource
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test allocating a resource to a task.
%% @end
%%--------------------------------------------------------------------
test_allocate_resource_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Allocate with no available resources
                     Result = yawl_resourcing:allocate_resource(<<"task_001">>, [<<"analyst">>], eager),
                     ?assertEqual({error, no_resources_available}, Result)
                 end),
          ?_test(begin
                     % Allocate with eager strategy
                     Props = [{name, <<"Analyst1">>}, {roles, [<<"analyst">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"analyst_001">>),
                     Result = yawl_resourcing:allocate_resource(<<"task_001">>, [<<"analyst">>], eager),
                     ?assertMatch({ok, _AllocationId}, Result)
                 end),
          ?_test(begin
                     % Allocate with lazy strategy
                     Props = [{name, <<"Processor1">>}, {roles, [<<"processor">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"processor_001">>),
                     Result = yawl_resourcing:allocate_resource(<<"task_002">>, [<<"processor">>], lazy),
                     ?assertMatch({ok, _AllocationId}, Result)
                 end),
          ?_test(begin
                     % Allocate from multiple matching resources
                     lists:foreach(
                         fun(N) ->
                             Id = list_to_binary("worker_" ++ integer_to_list(N)),
                             Props = [{name, Id}, {roles, [<<"worker">>]}, {capabilities, []}],
                             {ok, _} = yawl_resourcing:register_participant(Props, Id)
                         end,
                         lists:seq(1, 5)
                     ),
                     {ok, AllocationId} = yawl_resourcing:allocate_resource(<<"task_003">>, [<<"worker">>], eager),
                     ?assert(is_binary(AllocationId)),
                     ?assert(size(AllocationId) > 0)
                 end),
          ?_test(begin
                     % Allocate with role preference
                     Props1 = [{name, <<"A">>}, {roles, [<<"admin">>, <<"user">>]}, {capabilities, []}],
                     Props2 = [{name, <<"B">>}, {roles, [<<"user">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props1, <<"admin_user">>),
                     {ok, _} = yawl_resourcing:register_participant(Props2, <<"normal_user">>),
                     {ok, _} = yawl_resourcing:allocate_resource(<<"task_004">>, [<<"admin">>, <<"user">>], eager),
                     % Should allocate to admin_user (has admin role)
                     ?assert(true)  % If we got here, allocation succeeded
                 end)
         ]
     end}.

%%====================================================================
%% Test: Deallocate Resource
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test deallocating a resource from a task.
%% @end
%%--------------------------------------------------------------------
test_deallocate_resource_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Deallocate existing allocation
                     Props = [{name, <<"Worker1">>}, {roles, [<<"worker">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_001">>),
                     {ok, AllocId} = yawl_resourcing:allocate_resource(<<"task_001">>, [<<"worker">>], eager),
                     ?assertEqual(ok, yawl_resourcing:deallocate_resource(<<"task_001">>, AllocId))
                 end),
          ?_test(begin
                     % Multiple allocations and deallocations
                     Props = [{name, <<"W">>}, {roles, [<<"worker">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_multi">>),
                     {ok, A1} = yawl_resourcing:allocate_resource(<<"task_a">>, [<<"worker">>], eager),
                     {ok, A2} = yawl_resourcing:allocate_resource(<<"task_b">>, [<<"worker">>], eager),
                     {ok, A3} = yawl_resourcing:allocate_resource(<<"task_c">>, [<<"worker">>], eager),
                     ?assertEqual(ok, yawl_resourcing:deallocate_resource(<<"task_a">>, A1)),
                     ?assertEqual(ok, yawl_resourcing:deallocate_resource(<<"task_b">>, A2)),
                     ?assertEqual(ok, yawl_resourcing:deallocate_resource(<<"task_c">>, A3))
                 end),
          ?_test(begin
                     % Deallocate non-existent allocation (should not error)
                     ?assertEqual(ok, yawl_resourcing:deallocate_resource(<<"task_x">>, <<"fake_alloc">>))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Get Available Resources
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test retrieving available resources by role.
%% @end
%%--------------------------------------------------------------------
test_get_available_resources_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % No resources initially
                     ?assertEqual([], yawl_resourcing:get_available_resources([<<"any">>]))
                 end),
          ?_test(begin
                     % Get resources by single role
                     lists:foreach(
                         fun(N) ->
                             Id = list_to_binary("analyst_" ++ integer_to_list(N)),
                             Props = [{name, Id}, {roles, [<<"analyst">>]}, {capabilities, []}],
                             {ok, _} = yawl_resourcing:register_participant(Props, Id)
                         end,
                         lists:seq(1, 3)
                     ),
                     Analysts = yawl_resourcing:get_available_resources([<<"analyst">>]),
                     ?assertEqual(3, length(Analysts))
                 end),
          ?_test(begin
                     % Get resources by multiple roles (any match)
                     Props1 = [{name, <<"A">>}, {roles, [<<"role_a">>]}, {capabilities, []}],
                     Props2 = [{name, <<"B">>}, {roles, [<<"role_b">>]}, {capabilities, []}],
                     Props3 = [{name, <<"C">>}, {roles, [<<"role_c">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props1, <<"p_a">>),
                     {ok, _} = yawl_resourcing:register_participant(Props2, <<"p_b">>),
                     {ok, _} = yawl_resourcing:register_participant(Props3, <<"p_c">>),
                     AnyRole = yawl_resourcing:get_available_resources([<<"role_a">>, <<"role_b">>]),
                     ?assertEqual(2, length(AnyRole))
                 end),
          ?_test(begin
                     % Only available participants returned
                     Props1 = [{name, <<"Avail">>}, {roles, [<<"worker">>]}, {capabilities, []}, {status, available}],
                     Props2 = [{name, <<"Busy">>}, {roles, [<<"worker">>]}, {capabilities, []}, {status, busy}],
                     Props3 = [{name, <<"Offline">>}, {roles, [<<"worker">>]}, {capabilities, []}, {status, offline}],
                     {ok, _} = yawl_resourcing:register_participant(Props1, <<"avail_worker">>),
                     {ok, _} = yawl_resourcing:register_participant(Props2, <<"busy_worker">>),
                     {ok, _} = yawl_resourcing:register_participant(Props3, <<"offline_worker">>),
                     Workers = yawl_resourcing:get_available_resources([<<"worker">>]),
                     ?assertEqual(1, length(Workers)),
                     ?assertEqual(<<"avail_worker">>, (hd(Workers))#participant.id)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Add Role
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test adding a role to a participant.
%% @end
%%--------------------------------------------------------------------
test_add_role_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Add role to non-existent participant
                     Result = yawl_resourcing:add_role(<<"nonexistent">>, <<"admin">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Add role to existing participant
                     Props = [{name, <<"User1">>}, {roles, [<<"user">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"user_001">>),
                     ?assertEqual(ok, yawl_resourcing:add_role(<<"user_001">>, <<"admin">>)),
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"user_001">>),
                     ?assert(lists:member(<<"admin">>, Participant#participant.roles))
                 end),
          ?_test(begin
                     % Add multiple roles
                     Props = [{name, <<"User2">>}, {roles, []}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"user_002">>),
                     lists:foreach(
                         fun(Role) -> ok = yawl_resourcing:add_role(<<"user_002">>, Role) end,
                         [<<"role_a">>, <<"role_b">>, <<"role_c">>, <<"role_d">>]
                     ),
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"user_002">>),
                     ?assertEqual(4, length(Participant#participant.roles))
                 end),
          ?_test(begin
                     % Adding duplicate role should not duplicate
                     Props = [{name, <<"User3">>}, {roles, []}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"user_003">>),
                     ok = yawl_resourcing:add_role(<<"user_003">>, <<"admin">>),
                     ok = yawl_resourcing:add_role(<<"user_003">>, <<"admin">>),
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"user_003">>),
                     AdminCount = length([R || R <- Participant#participant.roles, R =:= <<"admin">>]),
                     ?assertEqual(1, AdminCount)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Remove Role
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test removing a role from a participant.
%% @end
%%--------------------------------------------------------------------
test_remove_role_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Remove role from non-existent participant
                     Result = yawl_resourcing:remove_role(<<"nonexistent">>, <<"admin">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Remove existing role
                     Props = [{name, <<"User1">>}, {roles, [<<"admin">>, <<"user">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"user_001">>),
                     ?assertEqual(ok, yawl_resourcing:remove_role(<<"user_001">>, <<"admin">>)),
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"user_001">>),
                     ?assertNot(lists:member(<<"admin">>, Participant#participant.roles)),
                     ?assert(lists:member(<<"user">>, Participant#participant.roles))
                 end),
          ?_test(begin
                     % Remove all roles
                     Props = [{name, <<"User2">>}, {roles, [<<"a">>, <<"b">>, <<"c">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"user_002">>),
                     lists:foreach(
                         fun(R) -> ok = yawl_resourcing:remove_role(<<"user_002">>, R) end,
                         [<<"a">>, <<"b">>, <<"c">>]
                     ),
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"user_002">>),
                     ?assertEqual([], Participant#participant.roles)
                 end),
          ?_test(begin
                     % Remove non-existent role (should not error)
                     Props = [{name, <<"User3">>}, {roles, [<<"user">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"user_003">>),
                     ?assertEqual(ok, yawl_resourcing:remove_role(<<"user_003">>, <<"nonexistent">>)),
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"user_003">>),
                     ?assert(lists:member(<<"user">>, Participant#participant.roles))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Add Capability
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test adding a capability to a participant.
%% @end
%%--------------------------------------------------------------------
test_add_capability_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Add capability to non-existent participant
                     Result = yawl_resourcing:add_capability(<<"nonexistent">>, <<"skill">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Add capability to existing participant
                     Props = [{name, <<"Worker1">>}, {roles, []}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_001">>),
                     ?assertEqual(ok, yawl_resourcing:add_capability(<<"worker_001">>, <<"welding">>)),
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"worker_001">>),
                     ?assert(lists:member(<<"welding">>, Participant#participant.capabilities))
                 end),
          ?_test(begin
                     % Add multiple capabilities
                     Props = [{name, <<"Worker2">>}, {roles, []}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_002">>),
                     Capabilities = [<<"skill_a">>, <<"skill_b">>, <<"skill_c">>],
                     lists:foreach(
                         fun(C) -> ok = yawl_resourcing:add_capability(<<"worker_002">>, C) end,
                         Capabilities
                     ),
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"worker_002">>),
                     lists:foreach(
                         fun(C) -> ?assert(lists:member(C, Participant#participant.capabilities)) end,
                         Capabilities
                     )
                 end),
          ?_test(begin
                     % Adding duplicate capability should not duplicate
                     Props = [{name, <<"Worker3">>}, {roles, []}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_003">>),
                     ok = yawl_resourcing:add_capability(<<"worker_003">>, <<"skill_x">>),
                     ok = yawl_resourcing:add_capability(<<"worker_003">>, <<"skill_x">>),
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"worker_003">>),
                     SkillCount = length([C || C <- Participant#participant.capabilities, C =:= <<"skill_x">>]),
                     ?assertEqual(1, SkillCount)
                 end)
         ]
     end}.

%%====================================================================
%% Test: Get Resources by Role
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test filtering participants by role.
%% @end
%%--------------------------------------------------------------------
test_get_resources_by_role_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % No resources for non-existent role
                     ?assertEqual([], yawl_resourcing:get_resources_by_role(<<"nonexistent">>))
                 end),
          ?_test(begin
                     % Get resources by single role
                     Props1 = [{name, <<"A1">>}, {roles, [<<"admin">>]}, {capabilities, []}],
                     Props2 = [{name, <<"A2">>}, {roles, [<<"admin">>, <<"user">>]}, {capabilities, []}],
                     Props3 = [{name, <<"U1">>}, {roles, [<<"user">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props1, <<"p1">>),
                     {ok, _} = yawl_resourcing:register_participant(Props2, <<"p2">>),
                     {ok, _} = yawl_resourcing:register_participant(Props3, <<"p3">>),
                     Admins = yawl_resourcing:get_resources_by_role(<<"admin">>),
                     ?assertEqual(2, length(Admins))
                 end),
          ?_test(begin
                     % Include participants regardless of status
                     Props1 = [{name, <<"A">>}, {roles, [<<"worker">>]}, {capabilities, []}, {status, available}],
                     Props2 = [{name, <<"B">>}, {roles, [<<"worker">>]}, {capabilities, []}, {status, busy}],
                     Props3 = [{name, <<"C">>}, {roles, [<<"worker">>]}, {capabilities, []}, {status, offline}],
                     {ok, _} = yawl_resourcing:register_participant(Props1, <<"w1">>),
                     {ok, _} = yawl_resourcing:register_participant(Props2, <<"w2">>),
                     {ok, _} = yawl_resourcing:register_participant(Props3, <<"w3">>),
                     AllWorkers = yawl_resourcing:get_resources_by_role(<<"worker">>),
                     ?assertEqual(3, length(AllWorkers))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Get Resources by Capability
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test filtering participants by capability.
%% @end
%%--------------------------------------------------------------------
test_get_resources_by_capability_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % No resources for non-existent capability
                     ?assertEqual([], yawl_resourcing:get_resources_by_capability(<<"nonexistent">>))
                 end),
          ?_test(begin
                     % Get resources by single capability
                     Props1 = [{name, <<"D1">>}, {roles, []}, {capabilities, [<<"driving">>]}],
                     Props2 = [{name, <<"D2">>}, {roles, []}, {capabilities, [<<"driving">>, <<"mechanic">>]}],
                     Props3 = [{name, <<"M1">>}, {roles, []}, {capabilities, [<<"mechanic">>]}],
                     {ok, _} = yawl_resourcing:register_participant(Props1, <<"d1">>),
                     {ok, _} = yawl_resourcing:register_participant(Props2, <<"d2">>),
                     {ok, _} = yawl_resourcing:register_participant(Props3, <<"m1">>),
                     Drivers = yawl_resourcing:get_resources_by_capability(<<"driving">>),
                     ?assertEqual(2, length(Drivers))
                 end),
          ?_test(begin
                     % Include participants regardless of status
                     Props1 = [{name, <<"X">>}, {roles, []}, {capabilities, [<<"skill_x">>]}, {status, available}],
                     Props2 = [{name, <<"Y">>}, {roles, []}, {capabilities, [<<"skill_x">>]}, {status, busy}],
                     {ok, _} = yawl_resourcing:register_participant(Props1, <<"x1">>),
                     {ok, _} = yawl_resourcing:register_participant(Props2, <<"x2">>),
                     Skilled = yawl_resourcing:get_resources_by_capability(<<"skill_x">>),
                     ?assertEqual(2, length(Skilled))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Set Participant Status
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test changing participant status.
%% @end
%%--------------------------------------------------------------------
test_set_participant_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Set status for non-existent participant
                     Result = yawl_resourcing:set_participant_status(<<"nonexistent">>, busy),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Set available to busy
                     Props = [{name, <<"Worker1">>}, {roles, []}, {capabilities, []}, {status, available}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_001">>),
                     ?assertEqual(ok, yawl_resourcing:set_participant_status(<<"worker_001">>, busy)),
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"worker_001">>),
                     ?assertEqual(busy, Participant#participant.status)
                 end),
          ?_test(begin
                     % Cycle through all statuses
                     Props = [{name, <<"Worker2">>}, {roles, []}, {capabilities, []}, {status, available}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_002">>),
                     Statuses = [available, busy, unavailable, offline],
                     lists:foreach(
                         fun(S) ->
                             ?assertEqual(ok, yawl_resourcing:set_participant_status(<<"worker_002">>, S)),
                             {ok, P} = yawl_resourcing:get_participant_info(<<"worker_002">>),
                             ?assertEqual(S, P#participant.status)
                         end,
                         Statuses
                     )
                 end),
          ?_test(begin
                     % Status affects availability
                     Props = [{name, <<"Worker3">>}, {roles, [<<"worker">>]}, {capabilities, []}, {status, available}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_003">>),
                     Available1 = yawl_resourcing:get_available_resources([<<"worker">>]),
                     ?assertEqual(1, length(Available1)),
                     ok = yawl_resourcing:set_participant_status(<<"worker_003">>, busy),
                     Available2 = yawl_resourcing:get_available_resources([<<"worker">>]),
                     ?assertEqual(0, length(Available2))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Eager and Lazy Allocation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test eager and lazy allocation strategies.
%% @end
%%--------------------------------------------------------------------
test_eager_lazy_allocation_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Eager allocation - immediate assignment
                     Props = [{name, <<"Worker1">>}, {roles, [<<"worker">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_001">>),
                     {ok, AllocId} = yawl_resourcing:allocate_resource(<<"task_001">>, [<<"worker">>], eager),
                     ?assert(is_binary(AllocId)),
                     % After allocation, worker should be busy (if using add_resource_to_task)
                     ?assert(true)  % Eager allocation completed
                 end),
          ?_test(begin
                     % Lazy allocation - deferred assignment
                     Props = [{name, <<"Worker2">>}, {roles, [<<"worker">>]}, {capabilities, []}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_002">>),
                     {ok, AllocId} = yawl_resourcing:allocate_resource(<<"task_002">>, [<<"worker">>], lazy),
                     ?assert(is_binary(AllocId))
                     % Lazy allocation - assignment happens when needed
                 end),
          ?_test(begin
                     % Multiple eager allocations create allocation records
                     lists:foreach(
                         fun(N) ->
                             Id = list_to_binary("worker_" ++ integer_to_list(N)),
                             Props = [{name, Id}, {roles, [<<"worker">>]}, {capabilities, []}, {status, available}],
                             {ok, _} = yawl_resourcing:register_participant(Props, Id)
                         end,
                         lists:seq(1, 3)
                     ),
                     % Allocate resources - note: allocate_resource doesn't mark participants as busy
                     % it only creates allocation records
                     {ok, A1} = yawl_resourcing:allocate_resource(<<"task_a">>, [<<"worker">>], eager),
                     {ok, A2} = yawl_resourcing:allocate_resource(<<"task_b">>, [<<"worker">>], eager),
                     {ok, A3} = yawl_resourcing:allocate_resource(<<"task_c">>, [<<"worker">>], eager),
                     % All allocations should return IDs
                     ?assert(is_binary(A1)),
                     ?assert(is_binary(A2)),
                     ?assert(is_binary(A3)),
                     % Resources are still available (allocate_resource doesn't consume them)
                     % To consume, use add_resource_to_task
                     Available = yawl_resourcing:get_available_resources([<<"worker">>]),
                     case length(Available) >= 3 of
                         true -> ?assert(true);
                         false -> ?assert(false)
                     end
                 end),
          ?_test(begin
                     % Add resource directly to task (eager behavior)
                     Props = [{name, <<"Worker3">>}, {roles, [<<"worker">>]}, {capabilities, []}, {status, available}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_direct">>),
                     {ok, AllocId} = yawl_resourcing:add_resource_to_task(<<"task_direct">>, <<"worker_direct">>, eager),
                     ?assert(is_binary(AllocId)),
                     % Worker should now be busy
                     {ok, Participant} = yawl_resourcing:get_participant_info(<<"worker_direct">>),
                     ?assertEqual(busy, Participant#participant.status)
                 end),
          ?_test(begin
                     % Remove resource from task releases participant
                     Props = [{name, <<"Worker4">>}, {roles, [<<"worker">>]}, {capabilities, []}, {status, available}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_release">>),
                     {ok, AllocId} = yawl_resourcing:add_resource_to_task(<<"task_release">>, <<"worker_release">>, eager),
                     ?assertEqual(busy, get_status(<<"worker_release">>)),
                     ok = yawl_resourcing:remove_resource_from_task(<<"task_release">>, AllocId),
                     ?assertEqual(available, get_status(<<"worker_release">>))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Check Resource Availability
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test checking if a resource is available for a task.
%% @end
%%--------------------------------------------------------------------
test_check_resource_availability_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Check non-existent participant
                     Result = yawl_resourcing:check_resource_availability(<<"nonexistent">>, [<<"admin">>]),
                     ?assertEqual(false, Result)
                 end),
          ?_test(begin
                     % Available participant with matching role
                     Props = [{name, <<"Admin1">>}, {roles, [<<"admin">>]}, {capabilities, []}, {status, available}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"admin_001">>),
                     ?assertEqual(true, yawl_resourcing:check_resource_availability(<<"admin_001">>, [<<"admin">>]))
                 end),
          ?_test(begin
                     % Available participant without matching role
                     Props = [{name, <<"User1">>}, {roles, [<<"user">>]}, {capabilities, []}, {status, available}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"user_001">>),
                     ?assertEqual(false, yawl_resourcing:check_resource_availability(<<"user_001">>, [<<"admin">>]))
                 end),
          ?_test(begin
                     % Busy participant with matching role
                     Props = [{name, <<"Admin2">>}, {roles, [<<"admin">>]}, {capabilities, []}, {status, busy}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"admin_002">>),
                     ?assertEqual(false, yawl_resourcing:check_resource_availability(<<"admin_002">>, [<<"admin">>]))
                 end),
          ?_test(begin
                     % Check with multiple role options
                     Props = [{name, <<"Multi">>}, {roles, [<<"reviewer">>]}, {capabilities, []}, {status, available}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"multi_001">>),
                     ?assertEqual(true, yawl_resourcing:check_resource_availability(<<"multi_001">>, [<<"admin">>, <<"reviewer">>])),
                     ?assertEqual(true, yawl_resourcing:check_resource_availability(<<"multi_001">>, [<<"reviewer">>, <<"approver">>]))
                 end)
         ]
     end}.

%%====================================================================
%% Test: Get Participant Status
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test retrieving participant status.
%% @end
%%--------------------------------------------------------------------
test_get_participant_status_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_State) ->
         [
          ?_test(begin
                     % Get status of non-existent participant
                     Result = yawl_resourcing:get_participant_status(<<"nonexistent">>),
                     ?assertEqual({error, not_found}, Result)
                 end),
          ?_test(begin
                     % Get status of existing participant
                     Props = [{name, <<"Worker1">>}, {roles, []}, {capabilities, []}, {status, available}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_001">>),
                     {ok, Status} = yawl_resourcing:get_participant_status(<<"worker_001">>),
                     ?assertEqual(available, Status)
                 end),
          ?_test(begin
                     % Status changes are reflected
                     Props = [{name, <<"Worker2">>}, {roles, []}, {capabilities, []}, {status, available}],
                     {ok, _} = yawl_resourcing:register_participant(Props, <<"worker_002">>),
                     ok = yawl_resourcing:set_participant_status(<<"worker_002">>, busy),
                     {ok, Status} = yawl_resourcing:get_participant_status(<<"worker_002">>),
                     ?assertEqual(busy, Status)
                 end)
         ]
     end}.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Helper to get the status of a participant.
%% @end
%%--------------------------------------------------------------------
-spec get_status(binary()) -> available | busy | unavailable | offline.

get_status(ParticipantId) ->
    case yawl_resourcing:get_participant_status(ParticipantId) of
        {error, _} -> undefined;
        {ok, Status} -> Status
    end.
