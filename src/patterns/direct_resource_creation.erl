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

-module(direct_resource_creation).
-moduledoc """
Direct Resource Creation Pattern (WRP-01) for YAWL.

This module implements the Direct Resource Creation pattern (WRP-01) as a
gen_yawl behaviour.

## Pattern Description

The Direct Resource Creation pattern (WRP-01) enables direct creation and
initialization of resources within a workflow. Resources are created on-demand
and made available for subsequent tasks.

## Petri Net Structure

Places:
- `p_start` - Start of resource creation
- `p_creating` - Resource in creation
- `p_ready` - Resource ready for use
- `p_end` - Pattern completed

Transitions:
- `t_create` - Create the resource
- `t_finish` - Complete the pattern

## Examples

Get the list of places in the Petri net:

```erlang
> direct_resource_creation:place_lst().
[p_start,p_creating,p_ready,p_end]
```

Get the list of transitions:

```erlang
> direct_resource_creation:trsn_lst().
[t_create,t_finish]
```

Create a new resource creation state:

```erlang
> State = direct_resource_creation:new(fun() -> #{id => 1} end).
{direct_resource_creation_state,_,_,_,_,_}
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
    new/1,
    start/1,
    get_state/1
]).

%%====================================================================
%% Records
%%====================================================================

-record(direct_resource_creation_state, {
    create_fun :: function(),
    resource :: undefined | term(),
    status = pending :: pending | creating | ready | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).

-type direct_resource_creation_state() :: #direct_resource_creation_state{}.
-export_type([direct_resource_creation_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Direct Resource Creation pattern state.

```erlang
> Fun = fun() -> #{id => 1, type => disk} end,
> State = direct_resource_creation:new(Fun),
> State#direct_resource_creation_state.status.
pending
```
""".
-spec new(CreateFun :: function()) -> direct_resource_creation_state().

new(CreateFun) when is_function(CreateFun, 0) ->
    #direct_resource_creation_state{
        create_fun = CreateFun,
        start_time = erlang:system_time(millisecond),
        log_id = generate_log_id()
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Direct Resource Creation workflow as a gen_pnet process.
%% @end
%%--------------------------------------------------------------------
-spec start(CreateFun :: function()) -> {ok, pid()} | {error, term()}.

start(CreateFun) when is_function(CreateFun, 0) ->
    State = new(CreateFun),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Direct Resource Creation workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, direct_resource_creation_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Direct Resource Creation Petri net.

```erlang
> direct_resource_creation:place_lst().
[p_start,p_creating,p_ready,p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    ['p_start', 'p_creating', 'p_ready', 'p_end'].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Direct Resource Creation Petri net.

```erlang
> direct_resource_creation:trsn_lst().
[t_create,t_finish]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    ['t_create', 't_finish'].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: direct_resource_creation_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> direct_resource_creation:preset(t_create).
[p_start]
```

```erlang
> direct_resource_creation:preset(t_finish).
[p_ready]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_create') -> ['p_start'];
preset('t_finish') -> ['p_ready'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: direct_resource_creation_state()) ->
          boolean().

is_enabled('t_create', _Mode, _UsrInfo) ->
    true;
is_enabled('t_finish', _Mode, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: direct_resource_creation_state()) ->
          {produce, map()} | abort.

fire('t_create', #{'p_start' := [start]}, #direct_resource_creation_state{create_fun = Fun} = State) ->
    Resource = Fun(),
    NewState = State#direct_resource_creation_state{
        resource = Resource,
        status = ready
    },
    {produce, #{
        'p_start' => [],
        'p_creating' => [],
        'p_ready' => [{resource_ready, Resource}]
    }, NewState};

fire('t_finish', #{'p_ready' := [{resource_ready, _Resource}]}, State) ->
    NewState = State#direct_resource_creation_state{status = completed},
    {produce, #{
        'p_ready' => [],
        'p_end' => [done]
    }, NewState};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: direct_resource_creation_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: direct_resource_creation_state()) ->
          {ok, direct_resource_creation_state()}.

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
    <<"direct_resource_creation_", Hex/binary>>.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test place_lst callback
place_lst_test() ->
    ?assertEqual([p_start, p_creating, p_ready, p_end], place_lst()).

%% Test trsn_lst callback
trsn_lst_test() ->
    ?assertEqual([t_create, t_finish], trsn_lst()).

%% Test preset callback
preset_t_create_test() ->
    ?assertEqual([p_start], preset(t_create)).

preset_t_finish_test() ->
    ?assertEqual([p_ready], preset(t_finish)).

%% Test new/1 constructor
new_test() ->
    Fun = fun() -> #{id => 1} end,
    State = new(Fun),
    ?assertEqual(pending, State#direct_resource_creation_state.status),
    ?assert(is_function(State#direct_resource_creation_state.create_fun, 0)).

%% Test init_marking callback
init_marking_p_start_test() ->
    State = new(fun() -> ok end),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_other_place_test() ->
    State = new(fun() -> ok end),
    ?assertEqual([], init_marking(p_ready, State)).

-endif.
