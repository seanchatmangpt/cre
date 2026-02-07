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

-module(resource_initialization).
-moduledoc """
Resource Initialization Pattern (WRP-03) for YAWL.

This module implements the Resource Initialization pattern (WRP-03) as a
gen_yawl behaviour.

## Pattern Description

The Resource Initialization pattern (WRP-03) handles the initialization
phase of resources, ensuring they are properly configured before use.
This may include loading configurations, establishing connections, or
performing startup validation.

## Petri Net Structure

Places:
- `p_start` - Start of initialization
- `p_initializing` - Resource being initialized
- `p_validated` - Resource validated
- `p_ready` - Resource ready for use
- `p_end` - Pattern completed

Transitions:
- `t_init` - Initialize the resource
- `t_validate` - Validate the initialized resource
- `t_finish` - Complete the pattern

## Examples

Get the list of places in the Petri net:

```erlang
> resource_initialization:place_lst().
[p_start,p_initializing,p_validated,p_ready,p_end]
```

Get the list of transitions:

```erlang
> resource_initialization:trsn_lst().
[t_init,t_validate,t_finish]
```

Create a new resource initialization state:

```erlang
> InitFun = fun(R) -> maps:put(initialized, true, R) end,
> State = resource_initialization:new(InitFun, #{id => 1}),
> State#resource_initialization_state.status.
pending
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

-record(resource_initialization_state, {
    init_fun :: function(),
    resource :: term(),
    status = pending :: pending | initializing | validated | ready | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).

-type resource_initialization_state() :: #resource_initialization_state{}.
-export_type([resource_initialization_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Resource Initialization pattern state.

```erlang
> InitFun = fun(R) -> maps:put(initialized, true, R) end,
> State = resource_initialization:new(InitFun, #{id => 1}),
> State#resource_initialization_state.resource.
#{'id' => 1}
```
""".
-spec new(InitFun :: function(), Resource :: term()) -> resource_initialization_state().

new(InitFun, Resource) when is_function(InitFun, 1) ->
    #resource_initialization_state{
        init_fun = InitFun,
        resource = Resource,
        start_time = erlang:system_time(millisecond),
        log_id = generate_log_id()
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Resource Initialization workflow as a gen_pnet process.
%% @end
%%--------------------------------------------------------------------
-spec start(InitFun :: function(), Resource :: term()) -> {ok, pid()} | {error, term()}.

start(InitFun, Resource) when is_function(InitFun, 1) ->
    State = new(InitFun, Resource),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Resource Initialization workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, resource_initialization_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Resource Initialization Petri net.

```erlang
> resource_initialization:place_lst().
[p_start,p_initializing,p_validated,p_ready,p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    ['p_start', 'p_initializing', 'p_validated', 'p_ready', 'p_end'].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Resource Initialization Petri net.

```erlang
> resource_initialization:trsn_lst().
[t_init,t_validate,t_finish]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    ['t_init', 't_validate', 't_finish'].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: resource_initialization_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> resource_initialization:preset(t_init).
[p_start]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_init') -> ['p_start'];
preset('t_validate') -> ['p_initializing'];
preset('t_finish') -> ['p_ready'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: resource_initialization_state()) ->
          boolean().

is_enabled('t_init', _Mode, _UsrInfo) ->
    true;
is_enabled('t_validate', #{'p_initializing' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_finish', #{'p_ready' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: resource_initialization_state()) ->
          {produce, map()} | abort.

fire('t_init', #{'p_start' := [start]}, #resource_initialization_state{init_fun = InitFun, resource = Resource} = State) ->
    InitializedResource = InitFun(Resource),
    NewState = State#resource_initialization_state{
        resource = InitializedResource,
        status = initializing
    },
    {produce, #{
        'p_start' => [],
        'p_initializing' => [{initializing, InitializedResource}]
    }, NewState};

fire('t_validate', #{'p_initializing' := [{initializing, Resource}]}, State) ->
    NewState = State#resource_initialization_state{
        status = validated
    },
    {produce, #{
        'p_initializing' => [],
        'p_validated' => [{validated, Resource}]
    }, NewState};

fire('t_finish', #{'p_validated' := [{validated, Resource}]}, State) ->
    NewState = State#resource_initialization_state{
        resource = Resource,
        status = completed
    },
    {produce, #{
        'p_validated' => [],
        'p_ready' => [],
        'p_end' => [{ready, Resource}]
    }, NewState};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: resource_initialization_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: resource_initialization_state()) ->
          {ok, resource_initialization_state()}.

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
    <<"resource_initialization_", Hex/binary>>.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test place_lst callback
place_lst_test() ->
    ?assertEqual([p_start, p_initializing, p_validated, p_ready, p_end], place_lst()).

%% Test trsn_lst callback
trsn_lst_test() ->
    ?assertEqual([t_init, t_validate, t_finish], trsn_lst()).

%% Test preset callback
preset_t_init_test() ->
    ?assertEqual([p_start], preset(t_init)).

preset_t_validate_test() ->
    ?assertEqual([p_initializing], preset(t_validate)).

%% Test new/2 constructor
new_test() ->
    InitFun = fun(R) -> maps:put(initialized, true, R) end,
    State = new(InitFun, #{id => 1}),
    ?assertEqual(#{id => 1}, State#resource_initialization_state.resource),
    ?assertEqual(pending, State#resource_initialization_state.status).

%% Test init_marking callback
init_marking_p_start_test() ->
    State = new(fun(R) -> R end, #{}),
    ?assertEqual([start], init_marking(p_start, State)).

-endif.
