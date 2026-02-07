%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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

-module(param_pass).
-moduledoc """
Parameter Passing Pattern (WDP-01) for YAWL.

This module implements the Parameter Passing pattern as a gen_yawl behaviour.

## Pattern Description

The Parameter Passing pattern (WDP-01) enables data to be passed between
workflow activities. It defines how input data is provided to tasks and
how output data is made available to subsequent tasks.

## Petri Net Structure

Places:
- `p_start` - Start of parameter passing
- `p_param_hold` - Holds parameters during transfer
- `p_end` - End of the pattern

Transitions:
- `t_pass` - Pass parameters to target
- `t_finish` - Complete the pattern

## Examples

Get the list of places in the Petri net:

```erlang
> param_pass:place_lst().
[p_start,p_param_hold,p_end]
```

Get the list of transitions:

```erlang
> param_pass:trsn_lst().
[t_pass,t_finish]
```

Get the preset (input places) for a transition:

```erlang
> param_pass:preset(t_pass).
[p_start]
```

```erlang
> param_pass:preset(t_finish).
[p_param_hold]
```

```erlang
> param_pass:preset(unknown).
[]
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
    get_state/1,
    pass/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(param_pass_state, {
    params :: map(),
    target :: undefined | pid(),
    start_time :: integer()
}).

-type param_pass_state() :: #param_pass_state{}.
-export_type([param_pass_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Parameter Passing pattern state.
%% @end
%%--------------------------------------------------------------------
-spec new(Params :: map()) -> param_pass_state().

new(Params) when is_map(Params) ->
    #param_pass_state{
        params = Params,
        target = undefined,
        start_time = erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Parameter Passing workflow as a gen_pnet process.
%%
%% @param Params Map of parameters to pass.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Params :: map()) -> {ok, pid()} | {error, term()}.

start(Params) when is_map(Params) ->
    PassState = new(Params),
    gen_yawl:start_link(?MODULE, PassState, []).

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Parameter Passing workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, param_pass_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Passes parameters to a target process/activity.
%%
%% @param Params Map of parameters to pass.
%% @param Target Target pid or identifier.
%% @return {ok, PassedParams}
%%
%% @end
%%--------------------------------------------------------------------
-spec pass(Params :: map(), Target :: term()) -> {ok, map()}.

pass(Params, Target) when is_map(Params) ->
    {ok, Params#{target => Target}}.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Parameter Passing Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_param_hold',
        'p_end'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Parameter Passing Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_pass',
        't_finish'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: param_pass_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_pass') -> ['p_start'];
preset('t_finish') -> ['p_param_hold'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: param_pass_state()) ->
          boolean().

is_enabled('t_pass', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_finish', #{'p_param_hold' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: param_pass_state()) ->
          {produce, map()} | abort.

fire('t_pass', #{'p_start' := [start]}, #param_pass_state{params = Params}) ->
    {produce, #{
        'p_start' => [],
        'p_param_hold' => [Params]
    }};

fire('t_finish', #{'p_param_hold' := [Params]}, _State) ->
    {produce, #{
        'p_param_hold' => [],
        'p_end' => [complete, Params]
    }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: param_pass_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: param_pass_state()) ->
          {ok, param_pass_state()}.

init(ParamPassState) ->
    {ok, ParamPassState}.

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
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test basic place_lst callback
place_lst_test() ->
    Expected = [p_start, p_param_hold, p_end],
    ?assertEqual(Expected, place_lst()).

%% Test basic trsn_lst callback
trsn_lst_test() ->
    Expected = [t_pass, t_finish],
    ?assertEqual(Expected, trsn_lst()).

%% Test preset for various transitions
preset_t_pass_test() ->
    ?assertEqual([p_start], preset(t_pass)).

preset_t_finish_test() ->
    ?assertEqual([p_param_hold], preset(t_finish)).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

%% Test new/1 constructor
new_test() ->
    Params = #{key => value},
    State = new(Params),
    ?assertEqual(Params, State#param_pass_state.params),
    ?assertEqual(undefined, State#param_pass_state.target).

%% Test init_marking callback
init_marking_p_start_test() ->
    State = new(#{}),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_other_place_test() ->
    State = new(#{}),
    ?assertEqual([], init_marking(p_param_hold, State)),
    ?assertEqual([], init_marking(p_end, State)).

-endif.
