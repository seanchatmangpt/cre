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

-module(resource_allocation).
-moduledoc """
Resource Allocation Pattern (WRP-04) for YAWL.

This module implements the Resource Allocation pattern (WRP-04) as a
gen_yawl behaviour.

## Pattern Description

The Resource Allocation pattern (WRP-04) manages the allocation of
resources to tasks. It ensures resources are properly assigned and
tracks which resources are available or in use.

## Petri Net Structure

Places:
- `p_start` - Start of allocation
- `p_available` - Available resources
- `p_allocating` - Allocation in progress
- `p_allocated` - Resource allocated to task
- `p_busy` - Resource busy/in use
- `p_end` - Pattern completed

Transitions:
- `t_request` - Request resource allocation
- `t_allocate` - Allocate resource
- `t_release` - Release allocated resource
- `t_finish` - Complete the pattern

## Examples

Get the list of places in the Petri net:

```erlang
> resource_allocation:place_lst().
[p_start,p_available,p_allocating,p_allocated,p_busy,p_end]
```

Get the list of transitions:

```erlang
> resource_allocation:trsn_lst().
[t_request,t_allocate,t_release,t_finish]
```

Create a new resource allocation state:

```erlang
> Resources = [res1, res2, res3],
> State = resource_allocation:new(Resources, task1),
> State#resource_allocation_state.task_id.
task1
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

-record(resource_allocation_state, {
    resources :: [term()],
    available :: [term()],
    allocated :: undefined | term(),
    task_id :: term(),
    status = pending :: pending | requesting | allocated | busy | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).

-type resource_allocation_state() :: #resource_allocation_state{}.
-export_type([resource_allocation_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Resource Allocation pattern state.

```erlang
> State = resource_allocation:new([res1, res2], task1),
> State#resource_allocation_state.task_id.
task1
```
""".
-spec new(Resources :: [term()], TaskId :: term()) -> resource_allocation_state().

new(Resources, TaskId) when is_list(Resources) ->
    #resource_allocation_state{
        resources = Resources,
        available = Resources,
        task_id = TaskId,
        start_time = erlang:system_time(millisecond),
        log_id = generate_log_id()
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Resource Allocation workflow as a gen_pnet process.
%% @end
%%--------------------------------------------------------------------
-spec start(Resources :: [term()], TaskId :: term()) -> {ok, pid()} | {error, term()}.

start(Resources, TaskId) when is_list(Resources) ->
    State = new(Resources, TaskId),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Resource Allocation workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, resource_allocation_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Resource Allocation Petri net.

```erlang
> resource_allocation:place_lst().
[p_start,p_available,p_allocating,p_allocated,p_busy,p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    ['p_start', 'p_available', 'p_allocating', 'p_allocated', 'p_busy', 'p_end'].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Resource Allocation Petri net.

```erlang
> resource_allocation:trsn_lst().
[t_request,t_allocate,t_release,t_finish]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    ['t_request', 't_allocate', 't_release', 't_finish'].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: resource_allocation_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking('p_available', #resource_allocation_state{available = Available}) ->
    Available;
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> resource_allocation:preset(t_request).
[p_start]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_request') -> ['p_start'];
preset('t_allocate') -> ['p_allocating'];
preset('t_release') -> ['p_busy'];
preset('t_finish') -> ['p_allocated'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: resource_allocation_state()) ->
          boolean().

is_enabled('t_request', _Mode, _UsrInfo) ->
    true;
is_enabled('t_allocate', #{'p_allocating' := [_], 'p_available' := [_ | _]}, _UsrInfo) ->
    true;
is_enabled('t_release', #{'p_busy' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_finish', #{'p_allocated' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: resource_allocation_state()) ->
          {produce, map()} | abort.

fire('t_request', #{'p_start' := [start]}, #resource_allocation_state{task_id = TaskId} = State) ->
    NewState = State#resource_allocation_state{status = requesting},
    {produce, #{
        'p_start' => [],
        'p_allocating' => [{request, TaskId}]
    }, NewState};

fire('t_allocate', #{'p_allocating' := [{request, _TaskId}], 'p_available' := [Resource | Remaining]}, State) ->
    NewState = State#resource_allocation_state{
        available = Remaining,
        allocated = Resource,
        status = allocated
    },
    {produce, #{
        'p_allocating' => [],
        'p_available' => Remaining,
        'p_allocated' => [{allocated, Resource}]
    }, NewState};

fire('t_release', #{'p_busy' := [{allocated, Resource}]}, #resource_allocation_state{available = Available} = State) ->
    NewState = State#resource_allocation_state{
        available = [Resource | Available],
        status = completed
    },
    {produce, #{
        'p_busy' => [],
        'p_available' => [Resource | Available],
        'p_allocated' => [{released, Resource}]
    }, NewState};

fire('t_finish', #{'p_allocated' := [{released, _Resource}]}, State) ->
    NewState = State#resource_allocation_state{status = completed},
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
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: resource_allocation_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: resource_allocation_state()) ->
          {ok, resource_allocation_state()}.

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
    <<"resource_allocation_", Hex/binary>>.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test place_lst callback
place_lst_test() ->
    ?assertEqual([p_start, p_available, p_allocating, p_allocated, p_busy, p_end], place_lst()).

%% Test trsn_lst callback
trsn_lst_test() ->
    ?assertEqual([t_request, t_allocate, t_release, t_finish], trsn_lst()).

%% Test preset callback
preset_t_request_test() ->
    ?assertEqual([p_start], preset(t_request)).

preset_t_allocate_test() ->
    ?assertEqual([p_allocating], preset(t_allocate)).

%% Test new/2 constructor
new_test() ->
    State = new([res1, res2], task1),
    ?assertEqual(task1, State#resource_allocation_state.task_id),
    ?assertEqual([res1, res2], State#resource_allocation_state.available),
    ?assertEqual(pending, State#resource_allocation_state.status).

%% Test init_marking callback
init_marking_p_start_test() ->
    State = new([res1], task1),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_p_available_test() ->
    State = new([res1, res2], task1),
    ?assertEqual([res1, res2], init_marking(p_available, State)).

-endif.
