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

-module(resource_deallocation).
-moduledoc """
Resource Deallocation Pattern (WRP-05) for YAWL.

This module implements the Resource Deallocation pattern (WRP-05) as a
gen_yawl behaviour.

## Pattern Description

The Resource Deallocation pattern (WRP-05) manages the release of
resources after task completion. It ensures proper cleanup and
makes resources available for future allocations.

## Petri Net Structure

Places:
- `p_start` - Start of deallocation
- `p_releasing` - Resource being released
- `p_cleanup` - Cleanup in progress
- `p_available` - Resource available again
- `p_deallocated` - Resource deallocated
- `p_end` - Pattern completed

Transitions:
- `t_start_release` - Initiate resource release
- `t_cleanup` - Perform cleanup operations
- `t_make_available` - Mark resource as available
- `t_finish` - Complete the pattern

## Examples

Get the list of places in the Petri net:

```erlang
> resource_deallocation:place_lst().
[p_start,p_releasing,p_cleanup,p_available,p_deallocated,p_end]
```

Get the list of transitions:

```erlang
> resource_deallocation:trsn_lst().
[t_start_release,t_cleanup,t_make_available,t_finish]
```

Create a new resource deallocation state:

```erlang
> CleanupFun = fun(R) -> maps:put(cleaned, true, R) end,
> State = resource_deallocation:new(CleanupFun, res1),
> State#resource_deallocation_state.resource.
res1
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

-record(resource_deallocation_state, {
    cleanup_fun :: function(),
    resource :: term(),
    status = pending :: pending | releasing | cleanup | available | deallocated | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).

-type resource_deallocation_state() :: #resource_deallocation_state{}.
-export_type([resource_deallocation_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Resource Deallocation pattern state.

```erlang
> CleanupFun = fun(R) -> maps:put(cleaned, true, R) end,
> State = resource_deallocation:new(CleanupFun, res1),
> State#resource_deallocation_state.resource.
res1
```
""".
-spec new(CleanupFun :: function(), Resource :: term()) -> resource_deallocation_state().

new(CleanupFun, Resource) when is_function(CleanupFun, 1) ->
    #resource_deallocation_state{
        cleanup_fun = CleanupFun,
        resource = Resource,
        start_time = erlang:system_time(millisecond),
        log_id = generate_log_id()
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Resource Deallocation workflow as a gen_pnet process.
%% @end
%%--------------------------------------------------------------------
-spec start(CleanupFun :: function(), Resource :: term()) -> {ok, pid()} | {error, term()}.

start(CleanupFun, Resource) when is_function(CleanupFun, 1) ->
    State = new(CleanupFun, Resource),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Resource Deallocation workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, resource_deallocation_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Resource Deallocation Petri net.

```erlang
> resource_deallocation:place_lst().
[p_start,p_releasing,p_cleanup,p_available,p_deallocated,p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    ['p_start', 'p_releasing', 'p_cleanup', 'p_available', 'p_deallocated', 'p_end'].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Resource Deallocation Petri net.

```erlang
> resource_deallocation:trsn_lst().
[t_start_release,t_cleanup,t_make_available,t_finish]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    ['t_start_release', 't_cleanup', 't_make_available', 't_finish'].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: resource_deallocation_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> resource_deallocation:preset(t_start_release).
[p_start]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_start_release') -> ['p_start'];
preset('t_cleanup') -> ['p_releasing'];
preset('t_make_available') -> ['p_cleanup'];
preset('t_finish') -> ['p_deallocated'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: resource_deallocation_state()) ->
          boolean().

is_enabled('t_start_release', _Mode, _UsrInfo) ->
    true;
is_enabled('t_cleanup', #{'p_releasing' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_make_available', #{'p_cleanup' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_finish', #{'p_deallocated' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: resource_deallocation_state()) ->
          {produce, map()} | abort.

fire('t_start_release', #{'p_start' := [start]}, #resource_deallocation_state{resource = Resource} = State) ->
    NewState = State#resource_deallocation_state{status = releasing},
    {produce, #{
        'p_start' => [],
        'p_releasing' => [{releasing, Resource}]
    }, NewState};

fire('t_cleanup', #{'p_releasing' := [{releasing, Resource}]}, #resource_deallocation_state{cleanup_fun = CleanupFun} = State) ->
    CleanedResource = CleanupFun(Resource),
    NewState = State#resource_deallocation_state{
        resource = CleanedResource,
        status = available
    },
    {produce, #{
        'p_releasing' => [],
        'p_cleanup' => [{cleanup_done, CleanedResource}]
    }, NewState};

fire('t_make_available', #{'p_cleanup' := [{cleanup_done, Resource}]}, State) ->
    NewState = State#resource_deallocation_state{status = deallocated},
    {produce, #{
        'p_cleanup' => [],
        'p_available' => [{available, Resource}],
        'p_deallocated' => [{deallocated, Resource}]
    }, NewState};

fire('t_finish', #{'p_deallocated' := [{deallocated, Resource}]}, State) ->
    NewState = State#resource_deallocation_state{
        resource = Resource,
        status = completed
    },
    {produce, #{
        'p_deallocated' => [],
        'p_end' => [{done, Resource}]
    }, NewState};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: resource_deallocation_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: resource_deallocation_state()) ->
          {ok, resource_deallocation_state()}.

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
    <<"resource_deallocation_", Hex/binary>>.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test place_lst callback
place_lst_test() ->
    ?assertEqual([p_start, p_releasing, p_cleanup, p_available, p_deallocated, p_end], place_lst()).

%% Test trsn_lst callback
trsn_lst_test() ->
    ?assertEqual([t_start_release, t_cleanup, t_make_available, t_finish], trsn_lst()).

%% Test preset callback
preset_t_start_release_test() ->
    ?assertEqual([p_start], preset(t_start_release)).

preset_t_cleanup_test() ->
    ?assertEqual([p_releasing], preset(t_cleanup)).

%% Test new/2 constructor
new_test() ->
    CleanupFun = fun(R) -> R end,
    State = new(CleanupFun, res1),
    ?assertEqual(res1, State#resource_deallocation_state.resource),
    ?assertEqual(pending, State#resource_deallocation_state.status).

%% Test init_marking callback
init_marking_p_start_test() ->
    State = new(fun(R) -> R end, res1),
    ?assertEqual([start], init_marking(p_start, State)).

-endif.
