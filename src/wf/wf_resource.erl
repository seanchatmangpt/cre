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
%% @doc Input specification for creating an organization.
%%
%% All fields are optional:
%% <ul>
%%   <li><b>users:</b> List of human users (optional, default: [])</li>
%%   <li><b>roles:</b> Map of roles to user assignments (optional, default: #{})</li>
%%   <li><b>non_human:</b> Map of non-human resources (optional, default: #{})</li>
%% </ul>
%%--------------------------------------------------------------------
-type org_spec() :: #{
    users => user_list(),
    roles => role_map(),
    non_human => non_human_map()
}.

%%--------------------------------------------------------------------
%% @doc An organization model containing all resource information.
%%
%% Internal representation created by org/1. Stores users, role mappings,
%% and non-human resource configurations with normalized indices for
%% efficient role lookup.
%%--------------------------------------------------------------------
-opaque org() :: #{
    type => org,
    users => user_list(),
    roles => role_map(),
    non_human => non_human_map(),
    %% Index: user_id -> [role_id()] for O(1) lookup
    user_role_index => #{user_id() => [role_id()]},
    %% Index: resource_id -> [role_id()] for O(1) lookup
    resource_role_index => #{resource_id() => [role_id()]}
}.

%% Export types
-export_type([user_id/0, role_id/0, resource_id/0]).
-export_type([user_list/0, role_map/0, resource_config/0, non_human_map/0]).
-export_type([org_spec/0, org/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an organization model from a specification map.
%%
%% The organization model normalizes the input data into indexed structures
%% for efficient role membership queries. Both human users and non-human
%% resources are supported with role assignments.
%%
%% @param Spec Map with optional keys: users, roles, non_human
%% @returns Organization model with indexed role lookups
%%
%% @end
%%--------------------------------------------------------------------
-spec org(Spec :: org_spec()) -> org().

org(Spec) when is_map(Spec) ->
    %% Extract components from spec with defaults
    Users = maps:get(users, Spec, []),
    Roles = maps:get(roles, Spec, #{}),
    NonHuman = maps:get(non_human, Spec, #{}),

    %% Build user -> roles index for O(1) lookups
    UserRoleIndex = build_user_role_index(Roles),

    %% Build non-human resource -> roles index for O(1) lookups
    ResourceRoleIndex = build_resource_role_index(NonHuman),

    #{
        type => org,
        users => normalize_list(Users),
        roles => normalize_role_map(Roles),
        non_human => NonHuman,
        user_role_index => UserRoleIndex,
        resource_role_index => ResourceRoleIndex
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

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

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

-endif.
