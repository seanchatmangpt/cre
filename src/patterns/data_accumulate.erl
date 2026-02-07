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

-module(data_accumulate).
-moduledoc """
Data Accumulation Pattern (WDP-04) for YAWL.

This module implements the Data Accumulation pattern as a gen_yawl behaviour.

## Pattern Description

The Data Accumulation pattern (WDP-04) enables data from multiple sources
to be collected and aggregated into a single result. This includes many-to-one
accumulation, reduction, and aggregation operations.

## Petri Net Structure

Places:
- `p_start` - Start of accumulation
- `p_collecting` - Collecting data from sources
- `p_accumulated` - Data has been accumulated
- `p_end` - End of the pattern

Transitions:
- `t_accumulate` - Accumulate data from sources
- `t_finish` - Complete the pattern

## Examples

Get the list of places in the Petri net:

```erlang
> data_accumulate:place_lst().
[p_start,p_collecting,p_accumulated,p_end]
```

Get the list of transitions:

```erlang
> data_accumulate:trsn_lst().
[t_accumulate,t_finish]
```

Get the preset (input places) for a transition:

```erlang
> data_accumulate:preset(t_accumulate).
[p_start]
```

```erlang
> data_accumulate:preset(t_finish).
[p_accumulated]
```

```erlang
> data_accumulate:preset(unknown).
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
    new/3,
    start/2,
    get_state/1,
    accumulate/3
]).

%%====================================================================
%% Records
%%====================================================================

-record(data_accumulate_state, {
    sources :: [term()],
    accumulator_fun :: function(),
    collected_data = [] :: [term()],
    source_count :: pos_integer(),
    start_time :: integer()
}).

-type data_accumulate_state() :: #data_accumulate_state{}.
-export_type([data_accumulate_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Data Accumulation pattern state.
%% @end
%%--------------------------------------------------------------------
-spec new(Sources :: [term()], AccumulatorFun :: function(), SourceCount :: pos_integer()) ->
          data_accumulate_state().

new(Sources, AccumulatorFun, SourceCount) when is_list(Sources),
                                                is_function(AccumulatorFun, 2),
                                                length(Sources) =:= SourceCount,
                                                SourceCount >= 1 ->
    #data_accumulate_state{
        sources = Sources,
        accumulator_fun = AccumulatorFun,
        collected_data = [],
        source_count = SourceCount,
        start_time = erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Data Accumulation workflow as a gen_pnet process.
%%
%% @param Sources List of data sources.
%% @param AccumulatorFun Function to accumulate data (Accumulator, Item -> NewAccumulator).
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Sources :: [term()], AccumulatorFun :: function()) ->
          {ok, pid()} | {error, term()}.

start(Sources, AccumulatorFun) when is_list(Sources),
                                     is_function(AccumulatorFun, 2),
                                     length(Sources) >= 1 ->
    SourceCount = length(Sources),
    AccumulateState = new(Sources, AccumulatorFun, SourceCount),
    gen_yawl:start_link(?MODULE, AccumulateState, []).

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Data Accumulation workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, data_accumulate_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Accumulates data from multiple sources using the provided function.
%%
%% @param Sources List of data sources or values.
%% @param AccumulatorFun Function to accumulate (Accumulator, Item -> NewAccumulator).
%% @param Initial Initial accumulator value.
%% @return {ok, AccumulatedResult}
%%
%% @end
%%--------------------------------------------------------------------
-spec accumulate(Sources :: [term()], AccumulatorFun :: function(), Initial :: term()) ->
          {ok, term()}.

accumulate(Sources, AccumulatorFun, Initial) when is_list(Sources),
                                                  is_function(AccumulatorFun, 2) ->
    Result = lists:foldl(AccumulatorFun, Initial, Sources),
    {ok, Result}.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Data Accumulation Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_collecting',
        'p_accumulated',
        'p_end'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Data Accumulation Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_accumulate',
        't_finish'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: data_accumulate_state()) ->
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

preset('t_accumulate') -> ['p_start'];
preset('t_finish') -> ['p_accumulated'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: data_accumulate_state()) ->
          boolean().

is_enabled('t_accumulate', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_finish', #{'p_accumulated' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: data_accumulate_state()) ->
          {produce, map()} | abort.

fire('t_accumulate', #{'p_start' := [start]}, #data_accumulate_state{
    sources = Sources,
    accumulator_fun = AccumFun
}) ->
    %% Accumulate all sources into a single result
    Result = lists:foldl(AccumFun, [], Sources),
    {produce, #{
        'p_start' => [],
        'p_collecting' => Sources,
        'p_accumulated' => [Result]
    }};

fire('t_finish', #{'p_accumulated' := [Accumulated]}, _State) ->
    {produce, #{
        'p_accumulated' => [],
        'p_end' => [complete, Accumulated]
    }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: data_accumulate_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: data_accumulate_state()) ->
          {ok, data_accumulate_state()}.

init(DataAccumulateState) ->
    {ok, DataAccumulateState}.

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
    Expected = [p_start, p_collecting, p_accumulated, p_end],
    ?assertEqual(Expected, place_lst()).

%% Test basic trsn_lst callback
trsn_lst_test() ->
    Expected = [t_accumulate, t_finish],
    ?assertEqual(Expected, trsn_lst()).

%% Test preset for various transitions
preset_t_accumulate_test() ->
    ?assertEqual([p_start], preset(t_accumulate)).

preset_t_finish_test() ->
    ?assertEqual([p_accumulated], preset(t_finish)).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

%% Test new/3 constructor
new_test() ->
    Sources = [1, 2, 3],
    Fun = fun(Acc, X) -> Acc + X end,
    State = new(Sources, Fun, 3),
    ?assertEqual(Sources, State#data_accumulate_state.sources),
    ?assertEqual(Fun, State#data_accumulate_state.accumulator_fun),
    ?assertEqual(3, State#data_accumulate_state.source_count).

%% Test accumulate/3
accumulate_test() ->
    Fun = fun(Acc, X) -> Acc + X end,
    ?assertEqual({ok, 6}, accumulate([1, 2, 3], Fun, 0)).

%% Test init_marking callback
init_marking_p_start_test() ->
    State = new([1], fun(A, X) -> A + X end, 1),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_other_place_test() ->
    State = new([1], fun(A, X) -> A + X end, 1),
    ?assertEqual([], init_marking(p_collecting, State)),
    ?assertEqual([], init_marking(p_end, State)).

-endif.
