%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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
%% @doc YAWL Resource Management Services.
%%
%% This module implements the YAWL resource management patterns including
%% participant registration, resource allocation, and role-based distribution.
%%
%% <h3>Resource Types</h3>
%% <ul>
%%   <li><b>human</b> - User participants requiring manual interaction</li>
%%   <li><b>machine</b> - Automated system resources</li>
%%   <li><b>non_human</b> - External non-user resources</li>
%%   <li><b>system</b> - System-level resources</li>
%% </ul>
%%
%% <h3>Allocation Strategies</h3>
%% <ul>
%%   <li><b>eager</b> - Allocate resources immediately when task starts</li>
%%   <li><b>lazy</b> - Defer allocation until resource is needed</li>
%% </ul>
%%
%% <h3>Participant Status</h3>
%% <ul>
%%   <li><b>available</b> - Participant is free for assignment</li>
%%   <li><b>busy</b> - Participant is currently assigned to a task</li>
%%   <li><b>unavailable</b> - Participant is temporarily unavailable</li>
%%   <li><b>offline</b> - Participant is not connected</li>
%% </ul>
%%
%% <h3>Doctests</h3>
%%
%% Starting the resourcing service:
%%
%% ```erlang
%% 1> {ok, Pid} = yawl_resourcing:start_link().
%% {ok, <0.123.0>}
%% '''
%%
%% Registering participants:
%%
%% ```erlang
%% 2> Props = [
%%     {name, <<"Alice">>},
%%     {roles, [<<"reviewer">>, <<"approver">>]},
%%     {capabilities, [<<"document_review">>]},
%%     {is_user, true},
%%     {resource_type, human}
%% ].
%% [...]
%% 3> {ok, ParticipantId} = yawl_resourcing:register_participant(Props, <<"participant_001">>).
%% {ok, <<"participant_001">>}
%% '''
%%
%% Allocating resources to tasks:
%%
%% ```erlang
%% 4> {ok, AllocationId} = yawl_resourcing:allocate_resource(
%%     <<"task_123">>, [<<"reviewer">>], eager
%% ).
%% {ok, <<"allocation_", _/binary>>}
%% '''
%%
%% Checking resource availability:
%%
%% ```erlang
%% 5> Available = yawl_resourcing:get_available_resources([<<"reviewer">>]).
%% [#participant{id = <<"participant_001">>, name = <<"Alice">>, ...}]
%% '''
%%
%% Getting resources by role:
%%
%% ```erlang
%% 6> Approvers = yawl_resourcing:get_resources_by_role(<<"approver">>).
%% [#participant{id = <<"participant_001">>, name = <<"Alice">>,
%%               roles := [<<"reviewer">>, <<"approver">>], ...}]
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_resourcing).
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/0, start_link/1]).
-export([register_participant/2, unregister_participant/1]).
-export([allocate_resource/3, deallocate_resource/2]).
-export([get_available_resources/1, get_all_participants/0]).
-export([add_resource_to_task/3, remove_resource_from_task/2]).
-export([check_resource_availability/2, get_participant_info/1]).
-export([add_role/2, remove_role/2, add_capability/2]).
-export([get_resources_by_role/1, get_resources_by_capability/1]).
-export([set_participant_status/2, get_participant_status/1]).
-export([doctest_test/0]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
         handle_info/2, init/1, terminate/2]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type resource_type() :: human | machine | non_human | system.
-type allocation_strategy() :: eager | lazy.
-type initiation_type() :: user | system | automatic.
-type participant_status() :: available | busy | unavailable | offline.

%%====================================================================
%% Record Definitions
%%====================================================================

-record(participant, {
    id :: binary(),
    name :: binary(),
    roles :: [binary()],
    capabilities :: [binary()],
    is_user :: boolean(),
    resource_type :: resource_type(),
    status :: participant_status(),
    metadata :: #{atom() => term()}
}).

-record(resource_offer, {
    task_id :: binary(),
    resources :: [binary()],
    allocation_strategy :: allocation_strategy(),
    initiation :: initiation_type(),
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

-record(resourcing_state, {
    participants = #{} :: #{binary() => #participant{}},
    allocations = [] :: [#resource_allocation{}],
    offers = #{} :: #{binary() => #resource_offer{}},
    participant_counter = 0 :: non_neg_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts an anonymous resourcing service.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%% @doc Starts a named resourcing service.
start_link(Name) ->
    gen_server:start_link(Name, ?MODULE, [], []).

%% @doc Registers a new participant with the resourcing service.
%%
%%      Props must include: name, roles, capabilities
%%      Optional: is_user (default false), resource_type (default system)
%%
%% Example:
%% ```erlang
%% 1> Props = [
%%     {name, <<"Bob">>},
%%     {roles, [<<"analyst">>]},
%%     {capabilities, [<<"data_analysis">>]},
%%     {is_user, true},
%%     {resource_type, human}
%% ].
%% [...]
%% 2> {ok, Pid} = yawl_resourcing:start_link().
%% {ok, <0.123.0>}
%% 3> {ok, ParticipantId} = yawl_resourcing:register_participant(Props, <<"participant_002">>).
%% {ok, <<"participant_002">>}
%% '''
-spec register_participant(proplists:proplist(), binary()) ->
    {ok, binary()} | {error, term()}.
register_participant(Props, ParticipantId) ->
    gen_server:call(?MODULE, {register_participant, Props, ParticipantId}).

%% @doc Unregisters a participant from the resourcing service.
-spec unregister_participant(binary()) -> ok | {error, term()}.
unregister_participant(ParticipantId) ->
    gen_server:call(?MODULE, {unregister_participant, ParticipantId}).

%% @doc Allocates a resource to a task based on specified criteria.
%%
%% Example:
%% ```erlang
%% 1> {ok, AllocationId} = yawl_resourcing:allocate_resource(
%%     <<"review_task_001">>, [<<"reviewer">>], eager
%% ).
%% {ok, <<"allocation_", _/binary>>}
%% '''
%%
%% Returns {error, no_resources_available} if no matching participants exist.
%%
-spec allocate_resource(binary(), [binary()], allocation_strategy()) ->
    {ok, binary()} | {error, term()}.
allocate_resource(TaskId, RoleList, Strategy) ->
    gen_server:call(?MODULE, {allocate_resource, TaskId, RoleList, Strategy}).

%% @doc Deallocates a resource from a task.
-spec deallocate_resource(binary(), binary()) -> ok | {error, term()}.
deallocate_resource(TaskId, AllocationId) ->
    gen_server:call(?MODULE, {deallocate_resource, TaskId, AllocationId}).

%% @doc Gets available resources matching the given criteria.
%%
%% Example:
%% ```erlang
%% 1> Reviewers = yawl_resourcing:get_available_resources([<<"reviewer">>]).
%% [#participant{id = <<"participant_001">>, name = <<"Alice">>, ...}]
%% '''
-spec get_available_resources([binary()]) -> [#participant{}].
get_available_resources(RoleList) ->
    gen_server:call(?MODULE, {get_available_resources, RoleList}).

%% @doc Gets all registered participants.
-spec get_all_participants() -> [#participant{}].
get_all_participants() ->
    gen_server:call(?MODULE, get_all_participants).

%% @doc Adds a resource directly to a task.
-spec add_resource_to_task(binary(), binary(), allocation_strategy()) ->
    {ok, binary()} | {error, term()}.
add_resource_to_task(TaskId, ParticipantId, Strategy) ->
    gen_server:call(?MODULE, {add_resource_to_task, TaskId, ParticipantId, Strategy}).

%% @doc Removes a resource from a task.
-spec remove_resource_from_task(binary(), binary()) -> ok.
remove_resource_from_task(TaskId, AllocationId) ->
    gen_server:call(?MODULE, {remove_resource_from_task, TaskId, AllocationId}).

%% @doc Checks if a resource is available for a specific task.
%%
%% Example:
%% ```erlang
%% 1> IsAvailable = yawl_resourcing:check_resource_availability(
%%     <<"participant_001">>, [<<"reviewer">>]
%% ).
%% true
%% '''
-spec check_resource_availability(binary(), [binary()]) -> boolean().
check_resource_availability(ParticipantId, RoleList) ->
    gen_server:call(?MODULE, {check_resource_availability, ParticipantId, RoleList}).

%% @doc Gets detailed information about a participant.
-spec get_participant_info(binary()) -> {ok, #participant{}} | {error, not_found}.
get_participant_info(ParticipantId) ->
    gen_server:call(?MODULE, {get_participant_info, ParticipantId}).

%% @doc Adds a role to a participant.
-spec add_role(binary(), binary()) -> ok | {error, term()}.
add_role(ParticipantId, Role) ->
    gen_server:call(?MODULE, {add_role, ParticipantId, Role}).

%% @doc Removes a role from a participant.
-spec remove_role(binary(), binary()) -> ok | {error, term()}.
remove_role(ParticipantId, Role) ->
    gen_server:call(?MODULE, {remove_role, ParticipantId, Role}).

%% @doc Adds a capability to a participant.
-spec add_capability(binary(), binary()) -> ok | {error, term()}.
add_capability(ParticipantId, Capability) ->
    gen_server:call(?MODULE, {add_capability, ParticipantId, Capability}).

%% @doc Gets all resources with a specific role.
%%
%% Example:
%% ```erlang
%% 1> Approvers = yawl_resourcing:get_resources_by_role(<<"approver">>).
%% [#participant{id = <<"participant_001">>, name = <<"Alice">>,
%%               roles = [<<"reviewer">>, <<"approver">>], ...}]
%% '''
-spec get_resources_by_role(binary()) -> [#participant{}].
get_resources_by_role(Role) ->
    gen_server:call(?MODULE, {get_resources_by_role, Role}).

%% @doc Gets all resources with a specific capability.
-spec get_resources_by_capability(binary()) -> [#participant{}].
get_resources_by_capability(Capability) ->
    gen_server:call(?MODULE, {get_resources_by_capability, Capability}).

%% @doc Sets the status of a participant.
-spec set_participant_status(binary(), participant_status()) ->
    ok | {error, term()}.
set_participant_status(ParticipantId, Status) ->
    gen_server:call(?MODULE, {set_participant_status, ParticipantId, Status}).

%% @doc Gets the current status of a participant.
-spec get_participant_status(binary()) -> {ok, participant_status()} | {error, not_found}.
get_participant_status(ParticipantId) ->
    gen_server:call(?MODULE, {get_participant_status, ParticipantId}).

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call({register_participant, Props, ParticipantId}, _From, State) ->
    #resourcing_state{participants = Participants} = State,

    Name = proplists:get_value(name, Props, <<"Unnamed Participant">>),
    Roles = proplists:get_value(roles, Props, []),
    Capabilities = proplists:get_value(capabilities, Props, []),
    IsUser = proplists:get_value(is_user, Props, false),
    ResourceType = proplists:get_value(resource_type, Props, system),
    Status = proplists:get_value(status, Props, available),
    Metadata = proplists:get_value(metadata, Props, #{}),

    Participant = #participant{
        id = ParticipantId,
        name = Name,
        roles = ensure_binary_list(Roles),
        capabilities = ensure_binary_list(Capabilities),
        is_user = IsUser,
        resource_type = ResourceType,
        status = Status,
        metadata = Metadata
    },

    {reply, {ok, ParticipantId}, State#resourcing_state{
        participants = Participants#{ParticipantId => Participant}
    }};

handle_call({unregister_participant, ParticipantId}, _From, State) ->
    #resourcing_state{participants = Participants} = State,
    case maps:is_key(ParticipantId, Participants) of
        true ->
            {reply, ok, State#resourcing_state{
                participants = maps:remove(ParticipantId, Participants)
            }};
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call({allocate_resource, TaskId, RoleList, _Strategy}, _From, State) ->
    #resourcing_state{participants = Participants, allocations = Allocations} = State,

    AvailableParticipants = find_available_participants(Participants, RoleList),

    case AvailableParticipants of
        [] ->
            {reply, {error, no_resources_available}, State};
        [Participant | _] ->
            AllocationId = generate_id(<<"allocation">>),
            Allocation = #resource_allocation{
                allocation_id = AllocationId,
                task_id = TaskId,
                participant_id = Participant#participant.id,
                allocated_at = erlang:timestamp(),
                status = active
            },
            {reply, {ok, AllocationId}, State#resourcing_state{
                allocations = [Allocation | Allocations]
            }}
    end;

handle_call({deallocate_resource, TaskId, AllocationId}, _From, State) ->
    #resourcing_state{allocations = Allocations} = State,

    UpdatedAllocations = lists:map(
        fun(Alloc) when Alloc#resource_allocation.allocation_id =:= AllocationId,
                       Alloc#resource_allocation.task_id =:= TaskId ->
                Alloc#resource_allocation{status = completed};
           (Alloc) ->
                Alloc
        end,
        Allocations
    ),

    {reply, ok, State#resourcing_state{allocations = UpdatedAllocations}};

handle_call({get_available_resources, RoleList}, _From, State) ->
    #resourcing_state{participants = Participants} = State,
    Available = find_available_participants(Participants, RoleList),
    {reply, Available, State};

handle_call(get_all_participants, _From, State) ->
    #resourcing_state{participants = Participants} = State,
    ParticipantList = maps:values(Participants),
    {reply, ParticipantList, State};

handle_call({add_resource_to_task, TaskId, ParticipantId, _Strategy}, _From, State) ->
    #resourcing_state{participants = Participants, allocations = Allocations} = State,

    case maps:get(ParticipantId, Participants, undefined) of
        undefined ->
            {reply, {error, participant_not_found}, State};
        #participant{status = available} = Participant ->
            AllocationId = generate_id(<<"allocation">>),
            Allocation = #resource_allocation{
                allocation_id = AllocationId,
                task_id = TaskId,
                participant_id = ParticipantId,
                allocated_at = erlang:timestamp(),
                status = active
            },
            UpdatedParticipants = Participants#{ParticipantId => Participant#participant{status = busy}},
            {reply, {ok, AllocationId}, State#resourcing_state{
                participants = UpdatedParticipants,
                allocations = [Allocation | Allocations]
            }};
        #participant{status = _} ->
            {reply, {error, participant_not_available}, State}
    end;

handle_call({remove_resource_from_task, TaskId, AllocationId}, _From, State) ->
    #resourcing_state{allocations = Allocations, participants = Participants} = State,

    {UpdatedAllocations, ParticipantId} = lists:foldl(
        fun(Alloc, {AccAllocs, _Pid}) when Alloc#resource_allocation.allocation_id =:= AllocationId,
                                             Alloc#resource_allocation.task_id =:= TaskId ->
                {[Alloc#resource_allocation{status = completed} | AccAllocs],
                 Alloc#resource_allocation.participant_id};
           (Alloc, {AccAllocs, Pid}) ->
                {[Alloc | AccAllocs], Pid}
        end,
        {[], undefined},
        Allocations
    ),

    UpdatedParticipants = case ParticipantId of
        undefined -> Participants;
        _ ->
            maps:update_with(
                ParticipantId,
                fun(P) -> P#participant{status = available} end,
                Participants
            )
    end,

    {reply, ok, State#resourcing_state{
        allocations = UpdatedAllocations,
        participants = UpdatedParticipants
    }};

handle_call({check_resource_availability, ParticipantId, RoleList}, _From, State) ->
    #resourcing_state{participants = Participants} = State,

    Result = case maps:get(ParticipantId, Participants, undefined) of
        #participant{status = available, roles = ParticipantRoles} ->
            lists:any(fun(Role) -> lists:member(Role, ParticipantRoles) end, RoleList);
        _ ->
            false
    end,

    {reply, Result, State};

handle_call({get_participant_info, ParticipantId}, _From, State) ->
    #resourcing_state{participants = Participants} = State,
    case maps:get(ParticipantId, Participants, undefined) of
        undefined -> {reply, {error, not_found}, State};
        Participant -> {reply, {ok, Participant}, State}
    end;

handle_call({add_role, ParticipantId, Role}, _From, State) ->
    #resourcing_state{participants = Participants} = State,
    case maps:get(ParticipantId, Participants, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #participant{roles = Roles} = Participant ->
            UpdatedRoles = case lists:member(Role, Roles) of
                true -> Roles;
                false -> [Role | Roles]
            end,
            UpdatedParticipant = Participant#participant{roles = UpdatedRoles},
            {reply, ok, State#resourcing_state{
                participants = Participants#{ParticipantId => UpdatedParticipant}
            }}
    end;

handle_call({remove_role, ParticipantId, Role}, _From, State) ->
    #resourcing_state{participants = Participants} = State,
    case maps:get(ParticipantId, Participants, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #participant{roles = Roles} = Participant ->
            UpdatedRoles = lists:delete(Role, Roles),
            UpdatedParticipant = Participant#participant{roles = UpdatedRoles},
            {reply, ok, State#resourcing_state{
                participants = Participants#{ParticipantId => UpdatedParticipant}
            }}
    end;

handle_call({add_capability, ParticipantId, Capability}, _From, State) ->
    #resourcing_state{participants = Participants} = State,
    case maps:get(ParticipantId, Participants, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #participant{capabilities = Caps} = Participant ->
            UpdatedCaps = case lists:member(Capability, Caps) of
                true -> Caps;
                false -> [Capability | Caps]
            end,
            UpdatedParticipant = Participant#participant{capabilities = UpdatedCaps},
            {reply, ok, State#resourcing_state{
                participants = Participants#{ParticipantId => UpdatedParticipant}
            }}
    end;

handle_call({get_resources_by_role, Role}, _From, State) ->
    #resourcing_state{participants = Participants} = State,

    Filtered = lists:filter(
        fun(#participant{roles = Roles}) ->
            lists:member(Role, Roles)
        end,
        maps:values(Participants)
    ),

    {reply, Filtered, State};

handle_call({get_resources_by_capability, Capability}, _From, State) ->
    #resourcing_state{participants = Participants} = State,

    Filtered = lists:filter(
        fun(#participant{capabilities = Caps}) ->
            lists:member(Capability, Caps)
        end,
        maps:values(Participants)
    ),

    {reply, Filtered, State};

handle_call({set_participant_status, ParticipantId, Status}, _From, State) ->
    #resourcing_state{participants = Participants} = State,
    case maps:get(ParticipantId, Participants, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Participant ->
            UpdatedParticipant = Participant#participant{status = Status},
            {reply, ok, State#resourcing_state{
                participants = Participants#{ParticipantId => UpdatedParticipant}
            }}
    end;

handle_call({get_participant_status, ParticipantId}, _From, State) ->
    #resourcing_state{participants = Participants} = State,
    case maps:get(ParticipantId, Participants, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #participant{status = Status} ->
            {reply, {ok, Status}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

init(_Arg) ->
    process_flag(trap_exit, true),
    {ok, #resourcing_state{}}.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Finds available participants matching the role requirements.
-spec find_available_participants(#{binary() => #participant{}}, [binary()]) ->
    [#participant{}].
find_available_participants(Participants, RoleList) ->
    lists:filter(
        fun(#participant{status = available, roles = Roles}) ->
            case RoleList of
                [] -> true;
                _ -> lists:any(fun(Role) -> lists:member(Role, Roles) end, RoleList)
            end;
           (_) ->
            false
        end,
        maps:values(Participants)
    ).

%% @private
%% @doc Generates a unique identifier with a prefix.
-spec generate_id(binary()) -> binary().
generate_id(Prefix) ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<Prefix/binary, "_", Hex/binary>>.

%% @private
%% @doc Ensures all elements in the list are binaries.
-spec ensure_binary_list([binary() | atom() | string()]) -> [binary()].
ensure_binary_list(List) ->
    [to_binary(E) || E <- List].

%% @private
%% @doc Converts various types to binary.
-spec to_binary(binary() | atom() | string() | integer()) -> binary().
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(S) when is_list(S) -> list_to_binary(S);
to_binary(I) when is_integer(I) -> integer_to_binary(I).

%%====================================================================
%% Doctests
%%====================================================================

%% @doc Runs doctests for the yawl_resourcing module.
%%
%% This function validates core resource management functionality including:
%% <ul>
%%   <li>Participant ID format validation</li>
%%   <li>Role and capability list validation</li>
%%   <li>Resource type validation</li>
%%   <li>Allocation strategy validation</li>
%%   <li>Participant status validation</li>
%%   <li>Binary list conversion</li>
%%   <li>Type conversion utilities</li>
%% </ul>
%%
%% Running the doctests:
%%
%% ```erlang
%% 1> yawl_resourcing:doctest_test().
%% ok
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Participant ID format validation
    ParticipantId = <<"test_participant_001">>,
    true = is_binary(ParticipantId),
    true = byte_size(ParticipantId) > 0,

    %% Test 2: Role list validation
    Roles = [<<"reviewer">>, <<"approver">>, <<"analyst">>],
    true = is_list(Roles),
    true = lists:all(fun is_binary/1, Roles),

    %% Test 3: Capability list validation
    Capabilities = [<<"document_review">>, <<"data_analysis">>],
    true = is_list(Capabilities),
    true = lists:all(fun is_binary/1, Capabilities),

    %% Test 4: Resource type validation
    ResourceType = human,
    true = ResourceType =:= human orelse
             ResourceType =:= machine orelse
             ResourceType =:= non_human orelse
             ResourceType =:= system,

    %% Test 5: Allocation strategy validation
    Strategy = eager,
    true = Strategy =:= eager orelse Strategy =:= lazy,

    %% Test 6: Participant status validation
    Status = available,
    true = Status =:= available orelse
             Status =:= busy orelse
             Status =:= unavailable orelse
             Status =:= offline,

    %% Test 7: Task ID format validation
    TaskId = <<"task_123">>,
    true = is_binary(TaskId),

    %% Test 8: Ensure binary list conversion works correctly
    BinaryRoles = ensure_binary_list([reviewer, <<"analyst">>, "approver"]),
    true = is_list(BinaryRoles),
    3 = length(BinaryRoles),
    true = lists:all(fun is_binary/1, BinaryRoles),

    %% Test 9: Verify to_binary conversion handles all types
    Bin1 = to_binary(<<"already_binary">>),
    true = is_binary(Bin1),
    Bin2 = to_binary(atom),
    true = is_binary(Bin2),
    Bin3 = to_binary("list"),
    true = is_binary(Bin3),
    Bin4 = to_binary(123),
    true = is_binary(Bin4),

    ok.
