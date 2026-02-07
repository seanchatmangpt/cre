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

-module(data_transform).
-moduledoc """
Data Transformation Pattern (WDP-02) for YAWL.

This module implements the Data Transformation pattern as a gen_yawl behaviour.

## Pattern Description

The Data Transformation pattern (WDP-02) enables data to be transformed
between different formats or representations as it flows through the workflow.
This includes format conversion, data mapping, and value transformations.

## Petri Net Structure

Places:
- `p_start` - Start of transformation
- `p_transforming` - Data being transformed
- `p_end` - End of the pattern

Transitions:
- `t_transform` - Apply transformation function
- `t_finish` - Complete the pattern

## Examples

Get the list of places in the Petri net:

```erlang
> data_transform:place_lst().
[p_start,p_transforming,p_end]
```

Get the list of transitions:

```erlang
> data_transform:trsn_lst().
[t_transform,t_finish]
```

Get the preset (input places) for a transition:

```erlang
> data_transform:preset(t_transform).
[p_start]
```

```erlang
> data_transform:preset(t_finish).
[p_transforming]
```

```erlang
> data_transform:preset(unknown).
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
    new/2,
    start/2,
    get_state/1,
    transform/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(data_transform_state, {
    transform_fun :: function(),
    input_data :: term(),
    output_data :: undefined | term(),
    start_time :: integer()
}).

-type data_transform_state() :: #data_transform_state{}.
-export_type([data_transform_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Data Transformation pattern state.
%% @end
%%--------------------------------------------------------------------
-spec new(TransformFun :: function(), InputData :: term()) -> data_transform_state().

new(TransformFun, InputData) when is_function(TransformFun, 1) ->
    #data_transform_state{
        transform_fun = TransformFun,
        input_data = InputData,
        output_data = undefined,
        start_time = erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Data Transformation workflow as a gen_pnet process.
%%
%% @param TransformFun Function to apply for transformation.
%% @param InputData Input data to transform.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(TransformFun :: function(), InputData :: term()) ->
          {ok, pid()} | {error, term()}.

start(TransformFun, InputData) when is_function(TransformFun, 1) ->
    TransformState = new(TransformFun, InputData),
    gen_yawl:start_link(?MODULE, TransformState, []).

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Data Transformation workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, data_transform_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Transforms input data using the provided function.
%%
%% @param TransformFun Function to apply for transformation.
%% @param InputData Input data to transform.
%% @return {ok, TransformedData}
%%
%% @end
%%--------------------------------------------------------------------
-spec transform(TransformFun :: function(), InputData :: term()) ->
          {ok, term()}.

transform(TransformFun, InputData) when is_function(TransformFun, 1) ->
    try
        Result = TransformFun(InputData),
        {ok, Result}
    catch
        Error:Reason ->
            {error, {transform_error, Error, Reason}}
    end.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Data Transformation Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_transforming',
        'p_end'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Data Transformation Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_transform',
        't_finish'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: data_transform_state()) ->
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

preset('t_transform') -> ['p_start'];
preset('t_finish') -> ['p_transforming'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: data_transform_state()) ->
          boolean().

is_enabled('t_transform', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_finish', #{'p_transforming' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: data_transform_state()) ->
          {produce, map()} | abort.

fire('t_transform', #{'p_start' := [start]}, #data_transform_state{
    transform_fun = TransformFun,
    input_data = InputData
}) ->
    try
        Transformed = TransformFun(InputData),
        {produce, #{
            'p_start' => [],
            'p_transforming' => [Transformed]
        }}
    catch
        _:_ ->
            abort
    end;

fire('t_finish', #{'p_transforming' := [Transformed]}, _State) ->
    {produce, #{
        'p_transforming' => [],
        'p_end' => [complete, Transformed]
    }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: data_transform_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: data_transform_state()) ->
          {ok, data_transform_state()}.

init(DataTransformState) ->
    {ok, DataTransformState}.

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
    Expected = [p_start, p_transforming, p_end],
    ?assertEqual(Expected, place_lst()).

%% Test basic trsn_lst callback
trsn_lst_test() ->
    Expected = [t_transform, t_finish],
    ?assertEqual(Expected, trsn_lst()).

%% Test preset for various transitions
preset_t_transform_test() ->
    ?assertEqual([p_start], preset(t_transform)).

preset_t_finish_test() ->
    ?assertEqual([p_transforming], preset(t_finish)).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

%% Test new/2 constructor
new_test() ->
    Fun = fun(X) -> X * 2 end,
    State = new(Fun, 5),
    ?assertEqual(Fun, State#data_transform_state.transform_fun),
    ?assertEqual(5, State#data_transform_state.input_data),
    ?assertEqual(undefined, State#data_transform_state.output_data).

%% Test transform/2
transform_test() ->
    Fun = fun(X) -> X * 2 end,
    ?assertEqual({ok, 10}, transform(Fun, 5)).

%% Test init_marking callback
init_marking_p_start_test() ->
    State = new(fun(X) -> X end, test),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_other_place_test() ->
    State = new(fun(X) -> X end, test),
    ?assertEqual([], init_marking(p_transforming, State)),
    ?assertEqual([], init_marking(p_end, State)).

-endif.
