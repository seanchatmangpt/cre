%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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

-module(role_based_allocation).
-moduledoc """
Role-Based Allocation Pattern (WRP-02) for YAWL.

This module implements the Role-Based Allocation pattern (WRP-02) as a
gen_yawl behaviour.

## Pattern Description

The Role-Based Allocation pattern (WRP-02) allocates resources to tasks
based on role definitions. Resources with matching roles are automatically
assigned to tasks requiring those roles.

## Petri Net Structure

Places:
- `p_start` - Start of allocation
- `p_checking` - Checking role match
- `p_allocated` - Resource allocated
- `p_failed` - Allocation failed
- `p_end` - Pattern completed

Transitions:
- `t_check_role` - Check if role matches
- `t_allocate` - Allocate resource to role
- `t_fail` - Allocation failed
- `t_finish` - Complete the pattern

## Examples

Get the list of places in the Petri net:

```erlang
> role_based_allocation:place_lst().
[p_start,p_checking,p_allocated,p_failed,p_end]
```

Get the list of transitions:

```erlang
> role_based_allocation:trsn_lst().
[t_check_role,t_allocate,t_fail,t_finish]
```

Create a new role-based allocation state:

```erlang
> State = role_based_allocation:new(admin, #{admin => [user1, user2]}),
> State#role_based_allocation_state.required_role.
admin
```
""".
-behaviour(gen_yawl).

%% gen_pnet callbacks
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2,
    trigger/3
]).

-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3
]).

%% API exports
-export([
    new/2,
    start/2,
    get_state/1
]).

%%====================================================================
%% Records
%%====================================================================

-record(role_based_allocation_state, {
    required_role :: atom(),
    role_map :: #{atom() => [term()]},
    allocated :: undefined | term(),
    status = pending :: pending | checking | allocated | failed | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).

-type role_based_allocation_state() :: #role_based_allocation_state{}.
-export_type([role_based_allocation_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Role-Based Allocation pattern state.

```erlang
> State = role_based_allocation:new(admin, #{admin => [user1, user2]}),
> State#role_based_allocation_state.required_role.
admin
```
""".
-spec new(RequiredRole :: atom(), RoleMap :: map()) -> role_based_allocation_state().

new(RequiredRole, RoleMap) when is_atom(RequiredRole), is_map(RoleMap) ->
    #role_based_allocation_state{
        required_role = RequiredRole,
        role_map = RoleMap,
        start_time = erlang:system_time(millisecond),
        log_id = generate_log_id()
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Role-Based Allocation workflow as a gen_pnet process.
%% @end
%%--------------------------------------------------------------------
-spec start(RequiredRole :: atom(), RoleMap :: map()) -> {ok, pid()} | {error, term()}.

start(RequiredRole, RoleMap) when is_atom(RequiredRole), is_map(RoleMap) ->
    State = new(RequiredRole, RoleMap),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Role-Based Allocation workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, role_based_allocation_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Role-Based Allocation Petri net.

```erlang
> role_based_allocation:place_lst().
[p_start,p_checking,p_allocated,p_failed,p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    ['p_start', 'p_checking', 'p_allocated', 'p_failed', 'p_end'].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Role-Based Allocation Petri net.

```erlang
> role_based_allocation:trsn_lst().
[t_check_role,t_allocate,t_fail,t_finish]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    ['t_check_role', 't_allocate', 't_fail', 't_finish'].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: role_based_allocation_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> role_based_allocation:preset(t_check_role).
[p_start]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_check_role') -> ['p_start'];
preset('t_allocate') -> ['p_checking'];
preset('t_fail') -> ['p_checking'];
preset('t_finish') -> ['p_allocated'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: role_based_allocation_state()) ->
          boolean().

is_enabled('t_check_role', _Mode, _UsrInfo) ->
    true;
is_enabled('t_allocate', #{'p_checking' := [{role_available, _}]}, _UsrInfo) ->
    true;
is_enabled('t_fail', #{'p_checking' := [{role_unavailable, _}]}, _UsrInfo) ->
    true;
is_enabled('t_finish', #{'p_allocated' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: role_based_allocation_state()) ->
          {produce, map()} | abort.

fire('t_check_role', #{'p_start' := [start]}, #role_based_allocation_state{required_role = Role, role_map = RoleMap}) ->
    case maps:get(Role, RoleMap, undefined) of
        undefined ->
            {produce, #{
                'p_start' => [],
                'p_checking' => [{role_unavailable, Role}]
            }};
        [_|_] = Resources ->
            {produce, #{
                'p_start' => [],
                'p_checking' => [{role_available, hd(Resources)}]
            }};
        [] ->
            {produce, #{
                'p_start' => [],
                'p_checking' => [{role_unavailable, Role}]
            }}
    end;

fire('t_allocate', #{'p_checking' := [{role_available, Resource}]}, State) ->
    NewState = State#role_based_allocation_state{
        allocated = Resource,
        status = allocated
    },
    {produce, #{
        'p_checking' => [],
        'p_allocated' => [{allocated, Resource}]
    }, NewState};

fire('t_fail', #{'p_checking' := [{role_unavailable, Role}]}, State) ->
    NewState = State#role_based_allocation_state{status = failed},
    {produce, #{
        'p_checking' => [],
        'p_failed' => [{allocation_failed, Role}]
    }, NewState};

fire('t_finish', #{'p_allocated' := [{allocated, _Resource}]}, State) ->
    NewState = State#role_based_allocation_state{status = completed},
    {produce, #{
        'p_allocated' => [],
        'p_end' => [done]
    }, NewState};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: role_based_allocation_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: role_based_allocation_state()) ->
          {ok, role_based_allocation_state()}.

init(State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {ok, UsrInfo}, NetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles non-gen_pnet messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_info(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles code changes.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @doc Cleanup on termination.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), NetState :: term()) ->
          ok.

terminate(_Reason, _NetState) ->
    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a unique log ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().

generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"role_based_allocation_", Hex/binary>>.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test place_lst callback
place_lst_test() ->
    ?assertEqual([p_start, p_checking, p_allocated, p_failed, p_end], place_lst()).

%% Test trsn_lst callback
trsn_lst_test() ->
    ?assertEqual([t_check_role, t_allocate, t_fail, t_finish], trsn_lst()).

%% Test preset callback
preset_t_check_role_test() ->
    ?assertEqual([p_start], preset(t_check_role)).

preset_t_allocate_test() ->
    ?assertEqual([p_checking], preset(t_allocate)).

%% Test new/2 constructor
new_test() ->
    State = new(admin, #{admin => [user1]}),
    ?assertEqual(admin, State#role_based_allocation_state.required_role),
    ?assertEqual(pending, State#role_based_allocation_state.status).

%% Test init_marking callback
init_marking_p_start_test() ->
    State = new(admin, #{}),
    ?assertEqual([start], init_marking(p_start, State)).

-endif.
