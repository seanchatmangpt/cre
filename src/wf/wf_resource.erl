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

-module(wf_resource).
-moduledoc """
Organization model for users, roles, and non-human resources.

Provides a pure functional interface for managing organizational resources
including human users, role assignments, and non-human resources (robots,
services, automated systems).

## Four-Eyes Principle (Separation of Duties)

This module implements the four-eyes principle requiring certain tasks to be
performed by different users to prevent fraud and ensure proper oversight.

```erlang
> Org = wf_resource:org(#{
..   users => [alice, bob],
..   roles => #{clerk => [alice], manager => [bob]},
..   separation_rules => [{approve_request, authorize_payment}]
.. }).
_

> %% Alice cannot both approve and authorize - blocked
> wf_resource:check_separation(Org, <<"case1">>, alice, authorize_payment,
..                             [{task, approve_request, alice}]).
{error, separation_violation}

> %% Bob can authorize since Alice approved - allowed
> wf_resource:check_separation(Org, <<"case1">>, bob, authorize_payment,
..                             [{task, approve_request, alice}]).
ok
```

## Basic Usage

```erlang
> Org = wf_resource:org(#{
..   users => [alice, bob],
..   roles => #{clerk => [alice], manager => [bob]},
..   non_human => #{robot1 => #{roles => [clerk]}}
.. }).
_

> wf_resource:has_role(Org, alice, clerk).
true

> wf_resource:has_role(Org, robot1, clerk).
true

> wf_resource:has_role(Org, alice, manager).
false
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Org construction
-export([org/1]).

%% Role queries
-export([has_role/3]).

%% Separation of duties (four-eyes principle)
-export([separation_of_duty/2, check_separation/5, record_completion/3,
         get_completion_history/2, who_completed/3]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Unique identifier for a user.
%%
%% Can be any Erlang term - typically an atom or binary representing
%% a person's username or ID.
%%--------------------------------------------------------------------
-type user_id() :: term().

%%--------------------------------------------------------------------
%% @doc Unique identifier for a role.
%%
%% Roles represent job functions, permissions, or organizational groups.
%% Typically an atom like 'clerk', 'manager', 'admin', etc.
%%--------------------------------------------------------------------
-type role_id() :: term().

%%--------------------------------------------------------------------
%% @doc Unique identifier for a non-human resource.
%%
%% Non-human resources include robots, automated services, systems,
%% or any entity that is not a person but can have roles assigned.
%%--------------------------------------------------------------------
-type resource_id() :: term().

%%--------------------------------------------------------------------
%% @doc A list of users in the organization.
%%
%% Simple list representation for human user membership.
%%--------------------------------------------------------------------
-type user_list() :: [user_id()].

%%--------------------------------------------------------------------
%% @doc A role mapping assigns a role to a list of users.
%%
%% Maps role IDs to the list of users who have that role.
%%--------------------------------------------------------------------
-type role_map() :: #{role_id() => [user_id()]}.

%%--------------------------------------------------------------------
%% @doc Configuration for a non-human resource.
%%
%% Contains the roles assigned to this non-human resource.
%% Additional attributes may be added in future extensions.
%%--------------------------------------------------------------------
-type resource_config() :: #{roles => [role_id()]}.

%%--------------------------------------------------------------------
%% @doc A mapping of non-human resource IDs to their configurations.
%%
%% Each non-human resource has its own role assignment independent
%% of the human role mappings.
%%--------------------------------------------------------------------
-type non_human_map() :: #{resource_id() => resource_config()}.

%%--------------------------------------------------------------------
%% @doc Unique identifier for a workflow task.
%%
%% Tasks represent discrete units of work that may require separation
%% of duties enforcement.
%%--------------------------------------------------------------------
-type task_id() :: term().

%%--------------------------------------------------------------------
%% @doc Unique identifier for a workflow case/instance.
%%
%% Each case tracks its own completion history for separation enforcement.
%%--------------------------------------------------------------------
-type case_id() :: term().

%%--------------------------------------------------------------------
%% @doc A separation rule defining two tasks requiring different users.
%%
%% The tuple {TaskA, TaskB} means TaskA and TaskB must be completed
%% by different users within the same case. The relationship is
%% symmetric - if {prepare, approve} exists, the same person cannot
%% do both tasks regardless of order.
%%--------------------------------------------------------------------
-type separation_rule() :: {task_id(), task_id()}.

%%--------------------------------------------------------------------
%% @doc A list of separation rules for an organization.
%%
%% Defines all task pairs that require four-eyes principle enforcement.
%%--------------------------------------------------------------------
-type separation_rules() :: [separation_rule()].

%%--------------------------------------------------------------------
%% @doc A single completion record tracking who did what task.
%%
%% Timestamp is an integer representing epoch milliseconds.
%%--------------------------------------------------------------------
-type completion_record() :: #{
    task => task_id(),
    user => user_id(),
    timestamp => integer()
}.

%%--------------------------------------------------------------------
%% @doc Completion history per case.
%%
%% Maps case_id to a list of completion records in chronological order.
%% This history is used to enforce separation of duties.
%%--------------------------------------------------------------------
-type completion_history() :: #{case_id() => [completion_record()]}.

%%--------------------------------------------------------------------
%% @doc Input specification for creating an organization.
%%
%% All fields are optional:
%% <ul>
%%   <li><b>users:</b> List of human users (optional, default: [])</li>
%%   <li><b>roles:</b> Map of roles to user assignments (optional, default: #{})</li>
%%   <li><b>non_human:</b> Map of non-human resources (optional, default: #{})</li>
%%   <li><b>separation_rules:</b> Task pairs requiring different users (optional, default: [])</li>
%%   <li><b>completion_history:</b> Existing completion records (optional, default: #{})</li>
%% </ul>
%%--------------------------------------------------------------------
-type org_spec() :: #{
    users => user_list(),
    roles => role_map(),
    non_human => non_human_map(),
    separation_rules => separation_rules(),
    completion_history => completion_history()
}.

%%--------------------------------------------------------------------
%% @doc An organization model containing all resource information.
%%
%% Internal representation created by org/1. Stores users, role mappings,
%% non-human resource configurations, separation of duties rules, and
%% completion history with normalized indices for efficient lookups.
%%--------------------------------------------------------------------
-opaque org() :: #{
    type => org,
    users => user_list(),
    roles => role_map(),
    non_human => non_human_map(),
    separation_rules => separation_rules(),
    completion_history => completion_history(),
    %% Index: user_id -> [role_id()] for O(1) lookup
    user_role_index => #{user_id() => [role_id()]},
    %% Index: resource_id -> [role_id()] for O(1) lookup
    resource_role_index => #{resource_id() => [role_id()]},
    %% Index: {task_id(), task_id()} for O(1) rule lookup
    separation_index => #{task_id() => [task_id()]}
}.

%% Export types
-export_type([user_id/0, role_id/0, resource_id/0]).
-export_type([user_list/0, role_map/0, resource_config/0, non_human_map/0]).
-export_type([task_id/0, case_id/0, separation_rule/0, separation_rules/0]).
-export_type([completion_record/0, completion_history/0]).
-export_type([org_spec/0]).
-export_type([org/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an organization model from a specification map.
%%
%% The organization model normalizes the input data into indexed structures
%% for efficient role membership queries and separation of duties enforcement.
%% Both human users and non-human resources are supported with role assignments.
%%
%% @param Spec Map with optional keys: users, roles, non_human, separation_rules, completion_history
%% @returns Organization model with indexed lookups
%%
%% @end
%%--------------------------------------------------------------------
-spec org(Spec :: org_spec()) -> org().

org(Spec) when is_map(Spec) ->
    %% Extract components from spec with defaults
    Users = maps:get(users, Spec, []),
    Roles = maps:get(roles, Spec, #{}),
    NonHuman = maps:get(non_human, Spec, #{}),
    SeparationRules = maps:get(separation_rules, Spec, []),
    CompletionHistory = maps:get(completion_history, Spec, #{}),

    %% Build user -> roles index for O(1) lookups
    UserRoleIndex = build_user_role_index(Roles),

    %% Build non-human resource -> roles index for O(1) lookups
    ResourceRoleIndex = build_resource_role_index(NonHuman),

    %% Build separation index for O(1) rule lookups
    SeparationIndex = build_separation_index(SeparationRules),

    #{
        type => org,
        users => normalize_list(Users),
        roles => normalize_role_map(Roles),
        non_human => NonHuman,
        separation_rules => normalize_separation_rules(SeparationRules),
        completion_history => CompletionHistory,
        user_role_index => UserRoleIndex,
        resource_role_index => ResourceRoleIndex,
        separation_index => SeparationIndex
    }.

%%--------------------------------------------------------------------
%% @doc Checks if a user or resource has a specific role.
%%
%% Performs role membership lookup for both human users and non-human
%% resources. Returns true if the entity has been assigned the role,
%% false otherwise.
%%
%% @param Org Organization model created by org/1
%% @param Entity User ID (for humans) or Resource ID (for non-human)
%% @param Role Role ID to check membership for
%% @returns true if entity has the role, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec has_role(Org :: org(), Entity :: user_id() | resource_id(), Role :: role_id()) ->
          boolean().

has_role(#{type := org, user_role_index := UserIdx, resource_role_index := ResIdx},
         Entity, Role)
  when is_map(UserIdx), is_map(ResIdx) ->
    %% Check user role index first, then resource index
    case maps:get(Entity, UserIdx, undefined) of
        undefined ->
            %% Not a user with roles, check non-human resources
            case maps:get(Entity, ResIdx, undefined) of
                undefined -> false;
                RoleList when is_list(RoleList) -> lists:member(Role, RoleList)
            end;
        RoleList when is_list(RoleList) ->
            lists:member(Role, RoleList)
    end;
has_role(_, _, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc Adds a separation of duty rule to an organization.
%%
%% Creates a new organization with an additional rule requiring the
%% two specified tasks to be performed by different users. The rule
%% is symmetric - {A, B} means the same person cannot do both tasks.
%%
%% ```erlang
%% > Org0 = wf_resource:org(#{}).
%% > Org1 = wf_resource:separation_of_duty(Org0, {prepare_invoice, approve_payment}).
%% > wf_resource:check_separation(Org1, <<"case1">>, alice, approve_payment,
%% ..                             [{task, prepare_invoice, alice}]).
%% {error, separation_violation}
%%
%% > %% But Bob can approve since Alice prepared
%% > wf_resource:check_separation(Org1, <<"case1">>, bob, approve_payment,
%% ..                             [{task, prepare_invoice, alice}]).
%% ok
%% ```
%%
%% @param Org Organization model to update
%% @param Rule Pair of task IDs requiring different users
%% @returns Updated organization with new rule
%%
%% @end
%%--------------------------------------------------------------------
-spec separation_of_duty(Org :: org(), Rule :: separation_rule()) -> org().

separation_of_duty(#{type := org, separation_rules := Rules} = Org, {TaskA, TaskB} = Rule)
  when TaskA =/= TaskB ->
    %% Avoid duplicate rules
    NewRules = case lists:member(Rule, Rules) of
        true  -> Rules;
        false -> [Rule | Rules]
    end,
    SeparationIndex = build_separation_index(NewRules),
    Org#{separation_rules => NewRules, separation_index => SeparationIndex};
separation_of_duty(Org, _) ->
    %% Ignore invalid rules where TaskA == TaskB
    Org.

%%--------------------------------------------------------------------
%% @doc Checks if a user can perform a task based on separation rules.
%%
%% Verifies that the user hasn't already completed a conflicting task
%% in the same case. A conflict exists when:
%% <ul>
%%   <li>A separation rule exists between the proposed task and a prior task</li>
%%   <li>The same user completed the prior task in this case</li>
%% </ul>
%%
%% Returns ok if the user can proceed, or {error, separation_violation}
%% if the four-eyes principle would be violated.
%%
%% ```erlang
%% > Org = wf_resource:org(#{
%% ..   separation_rules => [{submit_expense, approve_expense}]
%% .. }).
%%
%% > %% Same user blocked from doing both tasks
%% > wf_resource:check_separation(Org, <<"case1">>, alice, approve_expense,
%% ..                             [{task, submit_expense, alice}]).
%% {error, separation_violation}
%%
%% > %% Different users allowed for the pair
%% > wf_resource:check_separation(Org, <<"case1">>, bob, approve_expense,
%% ..                             [{task, submit_expense, alice}]).
%% ok
%%
%% > %% No rule means anyone can do it
%% > wf_resource:check_separation(Org, <<"case1">>, alice, unrelated_task, []).
%% ok
%% ```
%%
%% @param Org Organization with separation rules
%% @param CaseId Workflow case instance ID
%% @param User User attempting the task
%% @param Task Task being attempted
%% @param PriorCompletions List of {task, TaskId, User} tuples for this case
%% @returns ok or {error, separation_violation}
%%
%% @end
%%--------------------------------------------------------------------
-spec check_separation(Org :: org(), CaseId :: case_id(), User :: user_id(), Task :: task_id(),
                       PriorCompletions :: [{task, task_id(), user_id()}]) ->
          ok | {error, separation_violation}.

check_separation(#{type := org, separation_index := SepIndex, completion_history := History},
                 CaseId, User, Task, PriorCompletions) ->
    %% Combine prior completions with stored history for this case
    CaseHistory = maps:get(CaseId, History, []),
    AllCompletions = PriorCompletions ++ [
        {task, RTask, RUser} || #{task := RTask, user := RUser} <- CaseHistory
    ],
    check_separation_internal(SepIndex, User, Task, AllCompletions).

%%--------------------------------------------------------------------
%% @doc Records a task completion for separation of duties tracking.
%%
%% Creates a new organization with an updated completion history.
%% Each completion is timestamped and stored per case for enforcement
%% of separation rules. Pure functional - returns a new org map.
%%
%% ```erlang
%% > Org0 = wf_resource:org(#{
%% ..   separation_rules => [{create_user, approve_user}]
%% .. }).
%%
%% > %% Record alice creating the user
%% > Org1 = wf_resource:record_completion(Org0, <<"case1">>,
%% ..                                     {task, create_user, alice}).
%%
%% > %% Now alice cannot approve - bob must do it
%% > wf_resource:check_separation(Org1, <<"case1">>, alice, approve_user, []).
%% {error, separation_violation}
%% ```
%%
%% @param Org Organization to update
%% @param CaseId Workflow case instance ID
%% @param Completion {task, TaskId, User} tuple to record
%% @returns Updated organization with new completion record
%%
%% @end
%%--------------------------------------------------------------------
-spec record_completion(Org :: org(), CaseId :: case_id(),
                        Completion :: {task, task_id(), user_id()}) -> org().

record_completion(#{type := org, completion_history := History} = Org,
                  CaseId, {task, Task, User}) ->
    Timestamp = erlang:system_time(millisecond),
    Record = #{task => Task, user => User, timestamp => Timestamp},
    CaseHistory = maps:get(CaseId, History, []),
    Org#{completion_history => History#{CaseId => [Record | CaseHistory]}}.

%%--------------------------------------------------------------------
%% @doc Gets the completion history for a specific case.
%%
%% Returns all task completion records for the given case in
%% chronological order (oldest first).
%%
%% @param Org Organization with completion history
%% @param CaseId Workflow case instance ID
%% @returns List of completion records or empty list if none exist
%%
%% @end
%%--------------------------------------------------------------------
-spec get_completion_history(Org :: org(), CaseId :: case_id()) -> [completion_record()].

get_completion_history(#{type := org, completion_history := History}, CaseId) ->
    case maps:get(CaseId, History, undefined) of
        undefined -> [];
        List when is_list(List) -> lists:reverse(List)
    end.

%%--------------------------------------------------------------------
%% @doc Finds who completed a specific task in a case.
%%
%% Returns the user who completed the given task, or undefined if
%% the task was not completed or completed by multiple users.
%%
%% @param Org Organization with completion history
%% @param CaseId Workflow case instance ID
%% @param Task Task to look up
%% @returns User ID or undefined
%%
%% @end
%%--------------------------------------------------------------------
-spec who_completed(Org :: org(), CaseId :: case_id(), Task :: task_id()) ->
          user_id() | undefined.

who_completed(#{type := org} = Org, CaseId, Task) ->
    History = get_completion_history(Org, CaseId),
    case [R || #{task := T, user := _U} = R <- History, T =:= Task] of
        [#{user := User}] -> User;
        _ -> undefined
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Builds an index from users to their assigned roles.
%%
%% Inverts the role_map (role -> [users]) into a user_role_index
%% (user -> [roles]) for O(1) membership testing.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_user_role_index(role_map()) -> #{user_id() => [role_id()]}.

build_user_role_index(RoleMap) when is_map(RoleMap) ->
    maps:fold(fun(Role, Users, Acc) when is_list(Users) ->
        lists:foldl(fun(User, InnerAcc) ->
            ExistingRoles = maps:get(User, InnerAcc, []),
            InnerAcc#{User => [Role | ExistingRoles]}
        end, Acc, Users)
    end, #{}, RoleMap).

%%--------------------------------------------------------------------
%% @private
%% @doc Builds an index from non-human resources to their assigned roles.
%%
%% Extracts the roles list from each non-human resource configuration.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_resource_role_index(non_human_map()) -> #{resource_id() => [role_id()]}.

build_resource_role_index(NonHumanMap) when is_map(NonHumanMap) ->
    maps:fold(fun(ResourceId, Config, Acc) when is_map(Config) ->
        Roles = maps:get(roles, Config, []),
        Acc#{ResourceId => Roles}
    end, #{}, NonHumanMap).

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes a value to a list.
%%
%% Ensures list type, handling edge cases where input might not be
%% a proper list.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_list(term()) -> list().

normalize_list(List) when is_list(List) -> List;
normalize_list(_) -> [].

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes a role map structure.
%%
%% Ensures proper map format for role assignments.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_role_map(term()) -> role_map().

normalize_role_map(Map) when is_map(Map) -> Map;
normalize_role_map(_) -> #{}.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes separation rules list.
%%
%% Ensures proper list format for separation rules.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_separation_rules(term()) -> separation_rules().

normalize_separation_rules(List) when is_list(List) -> List;
normalize_separation_rules(_) -> [].

%%--------------------------------------------------------------------
%% @private
%% @doc Builds an index for separation rule lookups.
%%
%% Creates a map from task_id to list of tasks it cannot be paired with.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_separation_index(separation_rules()) -> #{task_id() => [task_id()]}.

build_separation_index(Rules) when is_list(Rules) ->
    lists:foldl(fun({TaskA, TaskB}, Acc) ->
        Acc1 = maps:update_with(TaskA, fun(V) -> [TaskB | V] end, [TaskB], Acc),
        maps:update_with(TaskB, fun(V) -> [TaskA | V] end, [TaskA], Acc1)
    end, #{}, Rules).

%%--------------------------------------------------------------------
%% @private
%% @doc Internal check for separation of duties violation.
%%
%% Checks if the given user completed any task that has a separation
%% rule with the proposed task.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_separation_internal(#{task_id() => [task_id()]}, user_id(), task_id(),
                                 [{task, task_id(), user_id()}]) ->
          ok | {error, separation_violation}.

check_separation_internal(SepIndex, User, Task, Completions) ->
    %% Get list of tasks that cannot be done with this task
    ForbiddenTasks = maps:get(Task, SepIndex, []),
    check_completions(User, ForbiddenTasks, Completions).

%%--------------------------------------------------------------------
%% @private
%% @doc Checks completion list for violations.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_completions(user_id(), [task_id()], [{task, task_id(), user_id()}]) ->
          ok | {error, separation_violation}.

check_completions(_User, [], _Completions) ->
    %% No forbidden tasks, no violation possible
    ok;
check_completions(User, ForbiddenTasks, Completions) ->
    %% Check if user completed any forbidden task
    case lists:any(fun({task, Task, CompletedBy}) ->
        lists:member(Task, ForbiddenTasks) andalso CompletedBy =:= User
    end, Completions) of
        true  -> {error, separation_violation};
        false -> ok
    end.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Note: doctests disabled - moduledoc examples use shell-specific syntax
%% that doesn't work with EUnit's doctest parser.

%%====================================================================
%% Additional Unit Tests
%%====================================================================

org_creation_test() ->
    %% Test basic org creation
    Org = wf_resource:org(#{
        users => [alice, bob],
        roles => #{clerk => [alice], manager => [bob]},
        non_human => #{robot1 => #{roles => [clerk]}}
    }),
    ?assertMatch(#{type := org}, Org).

has_role_human_test() ->
    %% Test role checking for human users
    Org = wf_resource:org(#{
        users => [alice, bob],
        roles => #{clerk => [alice], manager => [bob]}
    }),
    ?assertEqual(true, wf_resource:has_role(Org, alice, clerk)),
    ?assertEqual(false, wf_resource:has_role(Org, alice, manager)),
    ?assertEqual(false, wf_resource:has_role(Org, bob, clerk)),
    ?assertEqual(true, wf_resource:has_role(Org, bob, manager)).

has_role_non_human_test() ->
    %% Test role checking for non-human resources
    Org = wf_resource:org(#{
        non_human => #{
            robot1 => #{roles => [clerk]},
            robot2 => #{roles => [manager, clerk]}
        }
    }),
    ?assertEqual(true, wf_resource:has_role(Org, robot1, clerk)),
    ?assertEqual(false, wf_resource:has_role(Org, robot1, manager)),
    ?assertEqual(true, wf_resource:has_role(Org, robot2, clerk)),
    ?assertEqual(true, wf_resource:has_role(Org, robot2, manager)).

has_role_unknown_entity_test() ->
    %% Test role checking for unknown entities
    Org = wf_resource:org(#{}),
    ?assertEqual(false, wf_resource:has_role(Org, unknown_user, clerk)),
    ?assertEqual(false, wf_resource:has_role(Org, unknown_robot, manager)).

empty_org_test() ->
    %% Test empty org creation and queries
    Org = wf_resource:org(#{}),
    ?assertMatch(#{type := org}, Org),
    ?assertEqual(false, wf_resource:has_role(Org, anyone, any_role)).

org_with_defaults_test() ->
    %% Test org with partial spec (missing keys)
    Org1 = wf_resource:org(#{users => [alice]}),
    ?assertEqual(false, wf_resource:has_role(Org1, alice, clerk)),

    Org2 = wf_resource:org(#{roles => #{admin => [bob]}}),
    ?assertEqual(true, wf_resource:has_role(Org2, bob, admin)).

multiple_roles_test() ->
    %% Test users/resources with multiple roles
    Org = wf_resource:org(#{
        users => [alice],
        roles => #{clerk => [alice], manager => [alice], admin => [alice]}
    }),
    ?assertEqual(true, wf_resource:has_role(Org, alice, clerk)),
    ?assertEqual(true, wf_resource:has_role(Org, alice, manager)),
    ?assertEqual(true, wf_resource:has_role(Org, alice, admin)),
    ?assertEqual(false, wf_resource:has_role(Org, alice, auditor)).

non_human_multiple_roles_test() ->
    %% Test non-human resources with multiple roles
    Org = wf_resource:org(#{
        non_human => #{robot1 => #{roles => [clerk, manager, admin]}}
    }),
    ?assertEqual(true, wf_resource:has_role(Org, robot1, clerk)),
    ?assertEqual(true, wf_resource:has_role(Org, robot1, manager)),
    ?assertEqual(true, wf_resource:has_role(Org, robot1, admin)).

mixed_resources_test() ->
    %% Test both human and non-human resources together
    Org = wf_resource:org(#{
        users => [alice, bob],
        roles => #{clerk => [alice, bob], manager => [alice]},
        non_human => #{robot1 => #{roles => [clerk]}}
    }),
    ?assertEqual(true, wf_resource:has_role(Org, alice, clerk)),
    ?assertEqual(true, wf_resource:has_role(Org, alice, manager)),
    ?assertEqual(true, wf_resource:has_role(Org, bob, clerk)),
    ?assertEqual(false, wf_resource:has_role(Org, bob, manager)),
    ?assertEqual(true, wf_resource:has_role(Org, robot1, clerk)),
    ?assertEqual(false, wf_resource:has_role(Org, robot1, manager)).

%%====================================================================
%% Separation of Duties Tests
%%====================================================================

separation_of_duty_adds_rule_test() ->
    %% Test adding a separation rule
    Org0 = wf_resource:org(#{}),
    Org1 = wf_resource:separation_of_duty(Org0, {prepare_invoice, approve_payment}),
    ?assertEqual([{prepare_invoice, approve_payment}],
                 maps:get(separation_rules, Org1)),
    %% Check the index was built correctly
    ?assertEqual([approve_payment],
                 maps:get(prepare_invoice, maps:get(separation_index, Org1))),
    ?assertEqual([prepare_invoice],
                 maps:get(approve_payment, maps:get(separation_index, Org1))).

separation_of_duty_duplicate_test() ->
    %% Test that duplicate rules are ignored
    Org0 = wf_resource:org(#{
        separation_rules => [{task_a, task_b}]
    }),
    Org1 = wf_resource:separation_of_duty(Org0, {task_a, task_b}),
    ?assertEqual(1, length(maps:get(separation_rules, Org1))).

separation_of_duty_invalid_test() ->
    %% Test that invalid rules (same task twice) are ignored
    Org0 = wf_resource:org(#{}),
    Org1 = wf_resource:separation_of_duty(Org0, {task_a, task_a}),
    ?assertEqual([], maps:get(separation_rules, Org1)).

check_separation_same_user_blocked_test() ->
    %% Test same user is blocked from doing both tasks in a pair
    Org = wf_resource:org(#{
        separation_rules => [{submit_expense, approve_expense}]
    }),
    ?assertEqual({error, separation_violation},
                 wf_resource:check_separation(Org, <<"case1">>, alice,
                                              approve_expense,
                                              [{task, submit_expense, alice}])).

check_separation_different_user_allowed_test() ->
    %% Test different users are allowed for the pair
    Org = wf_resource:org(#{
        separation_rules => [{submit_expense, approve_expense}]
    }),
    ?assertEqual(ok,
                 wf_resource:check_separation(Org, <<"case1">>, bob,
                                              approve_expense,
                                              [{task, submit_expense, alice}])).

check_separation_no_rule_allowed_test() ->
    %% Test no rule means anyone can do it
    Org = wf_resource:org(#{
        separation_rules => [{submit_expense, approve_expense}]
    }),
    ?assertEqual(ok,
                 wf_resource:check_separation(Org, <<"case1">>, alice,
                                              unrelated_task, [])),
    ?assertEqual(ok,
                 wf_resource:check_separation(Org, <<"case1">>, bob,
                                              unrelated_task,
                                              [{task, submit_expense, alice}])).

check_separation_empty_history_test() ->
    %% Test with empty completion history
    Org = wf_resource:org(#{
        separation_rules => [{task_a, task_b}]
    }),
    ?assertEqual(ok,
                 wf_resource:check_separation(Org, <<"case1">>, alice,
                                              task_a, [])).

check_separation_symmetric_test() ->
    %% Test that rules are symmetric (order doesn't matter)
    Org = wf_resource:org(#{
        separation_rules => [{task_a, task_b}]
    }),
    %% Task B first, then A - same user blocked
    ?assertEqual({error, separation_violation},
                 wf_resource:check_separation(Org, <<"case1">>, alice,
                                              task_a,
                                              [{task, task_b, alice}])),
    %% Task A first, then B - same user blocked
    ?assertEqual({error, separation_violation},
                 wf_resource:check_separation(Org, <<"case1">>, alice,
                                              task_b,
                                              [{task, task_a, alice}])).

check_separation_multiple_rules_test() ->
    %% Test multiple separation rules
    Org = wf_resource:org(#{
        separation_rules => [
            {prepare, approve},
            {create, delete},
            {prepare, authorize}
        ]
    }),
    %% Alice prepared, can't approve
    ?assertEqual({error, separation_violation},
                 wf_resource:check_separation(Org, <<"case1">>, alice,
                                              approve,
                                              [{task, prepare, alice}])),
    %% Alice prepared, can't authorize either
    ?assertEqual({error, separation_violation},
                 wf_resource:check_separation(Org, <<"case1">>, alice,
                                              authorize,
                                              [{task, prepare, alice}])),
    %% But alice can delete (no rule with prepare)
    ?assertEqual(ok,
                 wf_resource:check_separation(Org, <<"case1">>, alice,
                                              delete,
                                              [{task, prepare, alice}])).

record_completion_test() ->
    %% Test recording a completion
    Org0 = wf_resource:org(#{
        separation_rules => [{prepare, approve}]
    }),
    Org1 = wf_resource:record_completion(Org0, <<"case1">>,
                                          {task, prepare, alice}),
    %% Check history was updated
    History = wf_resource:get_completion_history(Org1, <<"case1">>),
    ?assertEqual(1, length(History)),
    [Record] = History,
    ?assertEqual(prepare, maps:get(task, Record)),
    ?assertEqual(alice, maps:get(user, Record)),
    ?assert(is_integer(maps:get(timestamp, Record))).

record_completion_multiple_test() ->
    %% Test recording multiple completions
    Org0 = wf_resource:org(#{}),
    Org1 = wf_resource:record_completion(Org0, <<"case1">>,
                                          {task, task_a, alice}),
    Org2 = wf_resource:record_completion(Org1, <<"case1">>,
                                          {task, task_b, bob}),
    Org3 = wf_resource:record_completion(Org2, <<"case1">>,
                                          {task, task_c, alice}),
    History = wf_resource:get_completion_history(Org3, <<"case1">>),
    ?assertEqual(3, length(History)),
    %% Check chronological order
    [First, Second, Third] = History,
    ?assertEqual(task_a, maps:get(task, First)),
    ?assertEqual(task_b, maps:get(task, Second)),
    ?assertEqual(task_c, maps:get(task, Third)).

record_completion_different_cases_test() ->
    %% Test completions in different cases don't interfere
    Org0 = wf_resource:org(#{
        separation_rules => [{prepare, approve}]
    }),
    Org1 = wf_resource:record_completion(Org0, <<"case1">>,
                                          {task, prepare, alice}),
    Org2 = wf_resource:record_completion(Org1, <<"case2">>,
                                          {task, prepare, bob}),
    %% Bob can approve in case1 (he didn't prepare there)
    ?assertEqual(ok,
                 wf_resource:check_separation(Org2, <<"case1">>, bob,
                                              approve, [])),
    %% Alice can approve in case2 (she didn't prepare there)
    ?assertEqual(ok,
                 wf_resource:check_separation(Org2, <<"case2">>, alice,
                                              approve, [])),
    %% But alice still can't approve in case1 (she prepared there)
    ?assertEqual({error, separation_violation},
                 wf_resource:check_separation(Org2, <<"case1">>, alice,
                                              approve, [])),
    %% And bob can't approve in case2 (he prepared there)
    ?assertEqual({error, separation_violation},
                 wf_resource:check_separation(Org2, <<"case2">>, bob,
                                              approve, [])).

get_completion_history_empty_test() ->
    %% Test getting history for case with no completions
    Org = wf_resource:org(#{}),
    ?assertEqual([],
                 wf_resource:get_completion_history(Org, <<"nonexistent">>)).

who_completed_test() ->
    %% Test finding who completed a task
    Org0 = wf_resource:org(#{}),
    Org1 = wf_resource:record_completion(Org0, <<"case1">>,
                                          {task, prepare, alice}),
    ?assertEqual(alice,
                 wf_resource:who_completed(Org1, <<"case1">>, prepare)),
    ?assertEqual(undefined,
                 wf_resource:who_completed(Org1, <<"case1">>, approve)).

who_completed_multiple_completions_test() ->
    %% Test with multiple completions of same task returns undefined
    Org0 = wf_resource:org(#{}),
    Org1 = wf_resource:record_completion(Org0, <<"case1">>,
                                          {task, prepare, alice}),
    Org2 = wf_resource:record_completion(Org1, <<"case1">>,
                                          {task, prepare, bob}),
    %% Multiple users completed same task - return undefined
    ?assertEqual(undefined,
                 wf_resource:who_completed(Org2, <<"case1">>, prepare)).

integration_full_workflow_test() ->
    %% Test complete four-eyes workflow
    Org0 = wf_resource:org(#{
        separation_rules => [
            {create_invoice, approve_invoice},
            {approve_invoice, pay_invoice}
        ]
    }),

    %% Alice creates invoice
    Org1 = wf_resource:record_completion(Org0, <<"case1">>,
                                          {task, create_invoice, alice}),
    ?assertEqual(alice,
                 wf_resource:who_completed(Org1, <<"case1">>, create_invoice)),

    %% Alice cannot approve her own invoice
    ?assertEqual({error, separation_violation},
                 wf_resource:check_separation(Org1, <<"case1">>, alice,
                                              approve_invoice, [])),

    %% Bob approves
    Org2 = wf_resource:record_completion(Org1, <<"case1">>,
                                          {task, approve_invoice, bob}),

    %% Bob cannot pay (he approved)
    ?assertEqual({error, separation_violation},
                 wf_resource:check_separation(Org2, <<"case1">>, bob,
                                              pay_invoice, [])),

    %% Alice can pay (she only created)
    ?assertEqual(ok,
                 wf_resource:check_separation(Org2, <<"case1">>, alice,
                                              pay_invoice, [])),

    %% Charlie can also pay (third person)
    ?assertEqual(ok,
                 wf_resource:check_separation(Org2, <<"case1">>, charlie,
                                              pay_invoice, [])).

-endif.
