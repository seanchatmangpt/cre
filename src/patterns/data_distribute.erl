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

-module(data_distribute).
-moduledoc """
Data Distribution Pattern (WDP-03) for YAWL.

This module implements the Data Distribution pattern as a gen_yawl behaviour.

## Pattern Description

The Data Distribution pattern (WDP-03) enables data to be distributed to
multiple recipients or targets in the workflow. This includes one-to-many
distribution, broadcast, and selective routing based on data content.

## Petri Net Structure

Places:
- `p_start` - Start of distribution
- `p_targets` - Targets to receive data
- `p_distributed` - Data has been distributed
- `p_end` - End of the pattern

Transitions:
- `t_distribute` - Distribute data to targets
- `t_finish` - Complete the pattern

## Examples

Get the list of places in the Petri net:

```erlang
> data_distribute:place_lst().
[p_start,p_targets,p_distributed,p_end]
```

Get the list of transitions:

```erlang
> data_distribute:trsn_lst().
[t_distribute,t_finish]
```

Get the preset (input places) for a transition:

```erlang
> data_distribute:preset(t_distribute).
[p_start]
```

```erlang
> data_distribute:preset(t_finish).
[p_distributed]
```

```erlang
> data_distribute:preset(unknown).
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
    distribute/3
]).

%%====================================================================
%% Records
%%====================================================================

-record(data_distribute_state, {
    data :: term(),
    targets :: [term()],
    distributed_count = 0 :: non_neg_integer(),
    start_time :: integer()
}).

-type data_distribute_state() :: #data_distribute_state{}.
-export_type([data_distribute_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Data Distribution pattern state.
%% @end
%%--------------------------------------------------------------------
-spec new(Data :: term(), Targets :: [term()], TargetCount :: pos_integer()) ->
          data_distribute_state().

new(Data, Targets, TargetCount) when is_list(Targets),
                                      length(Targets) =:= TargetCount,
                                      TargetCount >= 1 ->
    #data_distribute_state{
        data = Data,
        targets = Targets,
        distributed_count = 0,
        start_time = erlang:system_time(millisecond)
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Data Distribution workflow as a gen_pnet process.
%%
%% @param Data Data to distribute.
%% @param Targets List of target identifiers.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Data :: term(), Targets :: [term()]) ->
          {ok, pid()} | {error, term()}.

start(Data, Targets) when is_list(Targets), length(Targets) >= 1 ->
    TargetCount = length(Targets),
    DistributeState = new(Data, Targets, TargetCount),
    gen_yawl:start_link(?MODULE, DistributeState, []).

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Data Distribution workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, data_distribute_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Distributes data to all targets.
%%
%% @param Data Data to distribute.
%% @param Targets List of target identifiers.
%% @param DistributionFun Function to handle distribution to each target.
%% @return {ok, DistributionResults}
%%
%% @end
%%--------------------------------------------------------------------
-spec distribute(Data :: term(), Targets :: [term()], DistributionFun :: function()) ->
          {ok, [term()]}.

distribute(Data, Targets, DistributionFun) when is_list(Targets),
                                                is_function(DistributionFun, 2) ->
    Results = lists:map(fun(Target) ->
        DistributionFun(Data, Target)
    end, Targets),
    {ok, Results}.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Data Distribution Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_targets',
        'p_distributed',
        'p_end'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Data Distribution Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_distribute',
        't_finish'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: data_distribute_state()) ->
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

preset('t_distribute') -> ['p_start'];
preset('t_finish') -> ['p_distributed'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: data_distribute_state()) ->
          boolean().

is_enabled('t_distribute', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_finish', #{'p_distributed' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: data_distribute_state()) ->
          {produce, map()} | abort.

fire('t_distribute', #{'p_start' := [start]}, #data_distribute_state{
    data = Data,
    targets = Targets
}) ->
    {produce, #{
        'p_start' => [],
        'p_targets' => [{Data, Target} || Target <- Targets],
        'p_distributed' => [{distributed, length(Targets)}]
    }};

fire('t_finish', #{'p_distributed' := [DistInfo]}, _State) ->
    {produce, #{
        'p_distributed' => [],
        'p_end' => [complete, DistInfo]
    }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: data_distribute_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: data_distribute_state()) ->
          {ok, data_distribute_state()}.

init(DataDistributeState) ->
    {ok, DataDistributeState}.

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
    Expected = [p_start, p_targets, p_distributed, p_end],
    ?assertEqual(Expected, place_lst()).

%% Test basic trsn_lst callback
trsn_lst_test() ->
    Expected = [t_distribute, t_finish],
    ?assertEqual(Expected, trsn_lst()).

%% Test preset for various transitions
preset_t_distribute_test() ->
    ?assertEqual([p_start], preset(t_distribute)).

preset_t_finish_test() ->
    ?assertEqual([p_distributed], preset(t_finish)).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

%% Test new/3 constructor
new_test() ->
    Targets = [target1, target2],
    State = new(test_data, Targets, 2),
    ?assertEqual(test_data, State#data_distribute_state.data),
    ?assertEqual(Targets, State#data_distribute_state.targets),
    ?assertEqual(0, State#data_distribute_state.distributed_count).

%% Test distribute/3
distribute_test() ->
    Fun = fun(Data, Target) -> {Data, Target} end,
    ?assertEqual({ok, [{data, a}, {data, b}]}, distribute(data, [a, b], Fun)).

%% Test init_marking callback
init_marking_p_start_test() ->
    State = new(data, [t1], 1),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_other_place_test() ->
    State = new(data, [t1], 1),
    ?assertEqual([], init_marking(p_targets, State)),
    ?assertEqual([], init_marking(p_end, State)).

-endif.
