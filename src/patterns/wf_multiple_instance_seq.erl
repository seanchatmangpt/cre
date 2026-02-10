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
%% @doc Multiple Instances Sequential Pattern for YAWL
%%
%% This module implements the Sequential Multiple Instances pattern
%% as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Sequential Multiple Instances pattern creates multiple instances
%% of a subprocess and executes them sequentially, one at a time, waiting
%% for each to complete before starting the next.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start          - Start of the workflow
%%     p_spawn          - Spawn trigger
%%     p_active         - Current instance active
%%     p_completed      - Completed instances
%%     p_sync_point     - Synchronization point
%%     p_end            - Workflow complete
%%
%%   Transitions:
%%     t_spawn          - Initialize instances
%%     t_exec           - Execute current instance
%%     t_next           - Move to next instance
%%     t_finish         - Complete workflow
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always true (all instances execute)</li>
%%   <li><b>Proper completion:</b> All instances complete sequentially</li>
%%   <li><b>No dead transitions:</b> All instances execute in order</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_multiple_instance_seq).
-moduledoc """
Multiple Instances Sequential Pattern for YAWL.

This module implements the Sequential Multiple Instances pattern
as a gen_yawl behaviour. Multiple instances are spawned but executed
sequentially, one at a time.

## Example: Execute Sequential Instances

```erlang
> Fun = fun(X) -> X * 2 end,
> DataList = [1, 2, 3, 4],
> wf_multiple_instance_seq:execute(Fun, DataList).
{ok, [2,4,6,8]}
```

## Example: Place List

```erlang
> wf_multiple_instance_seq:place_lst().
['p_start','p_spawn','p_active','p_completed','p_sync_point','p_end']
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
    run/2,
    get_state/1,
    execute/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(seq_instance_state, {
    subprocess :: function(),
    instance_count :: pos_integer(),
    input_data :: list(),
    current_index = 0 :: non_neg_integer(),
    completed = [] :: [pos_integer()],
    results = #{} :: #{pos_integer() => term()},
    log_id :: binary() | undefined
}).

-type seq_instance_state() :: #seq_instance_state{}.
-export_type([seq_instance_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Sequential Multiple Instances pattern state.

## Example

```erlang
> Fun = fun(X) -> X * 2 end,
> State = wf_multiple_instance_seq:new(Fun, 3).
{seq_instance_state,_,3,[],0,[],#{},_}
```
""".
-spec new(Subprocess :: function(), InstanceCount :: pos_integer()) ->
          seq_instance_state().

new(Subprocess, InstanceCount) when is_function(Subprocess), is_integer(InstanceCount), InstanceCount > 0 ->
    LogId = generate_log_id(),
    #seq_instance_state{
        subprocess = Subprocess,
        instance_count = InstanceCount,
        input_data = [],
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Sequential Multiple Instances workflow as a gen_yawl process.
%% @end
%%--------------------------------------------------------------------
-spec start(Subprocess :: function(), InstanceCount :: pos_integer()) ->
          {ok, pid()} | {error, term()}.

start(Subprocess, InstanceCount) when is_function(Subprocess), is_integer(InstanceCount), InstanceCount > 0 ->
    State = new(Subprocess, InstanceCount),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Runs the Sequential Multiple Instances workflow synchronously.
%% @end
%%--------------------------------------------------------------------
-spec run(Subprocess :: function(), InputData :: list()) ->
          {ok, list()} | {error, term()}.

run(Subprocess, InputData) when is_function(Subprocess), is_list(InputData) ->
    InstanceCount = length(InputData),
    case start(Subprocess, InstanceCount) of
        {ok, Pid} ->
            gen_yawl:cast(Pid, {input_data, InputData}),
            case wait_for_completion(Pid, 30000) of
                {ok, Results} ->
                    gen_yawl:stop(Pid),
                    {ok, Results};
                {error, Reason} ->
                    gen_yawl:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Sequential Multiple Instances workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, seq_instance_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
-doc """
Executes the Sequential Multiple Instances pattern.

Spawns multiple instances but executes them sequentially, waiting for
each to complete before starting the next.

## Example

```erlang
> Fun = fun(X) -> X * 2 end,
> wf_multiple_instance_seq:execute(Fun, [1,2,3,4]).
{ok, [2,4,6,8]}
```

Parameters:
- `Subprocess` - Function to execute for each instance
- `InputData` - List of data items, one per instance

Returns `{ok, Results}` ordered list or `{error, Reason}`.
""".
-spec execute(Subprocess :: function(), InputData :: list()) ->
          {ok, list()} | {error, term()}.

execute(Subprocess, InputData) when is_function(Subprocess), is_list(InputData) ->
    InstanceCount = length(InputData),
    Ref = make_ref(),
    Parent = self(),

    %% Execute instances sequentially
    execute_sequential(Subprocess, InputData, 1, InstanceCount, Ref, Parent, 30000, []).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Sequential Multiple Instances Petri net.

```erlang
> wf_multiple_instance_seq:place_lst().
['p_start','p_spawn','p_active','p_completed','p_sync_point','p_end']
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_spawn',
        'p_active',
        'p_completed',
        'p_sync_point',
        'p_end'
    ].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Sequential Multiple Instances Petri net.

```erlang
> wf_multiple_instance_seq:trsn_lst().
['t_spawn','t_exec','t_next','t_finish']
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_spawn',
        't_exec',
        't_next',
        't_finish'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: seq_instance_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> wf_multiple_instance_seq:preset('t_spawn').
['p_start']
> wf_multiple_instance_seq:preset('t_next').
['p_active']
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_spawn') -> ['p_start'];
preset('t_exec') -> ['p_active'];
preset('t_next') -> ['p_completed'];
preset('t_finish') -> ['p_sync_point'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: seq_instance_state()) ->
          boolean().

is_enabled('t_spawn', _Mode, _UsrInfo) ->
    true;
is_enabled('t_exec', #{'p_active' := [{instance, _Index}]}, _UsrInfo) ->
    true;
is_enabled('t_next', #{'p_completed' := [{complete, _Index}]}, #seq_instance_state{current_index = CurrentIndex, instance_count = Count}) ->
    CurrentIndex < Count;
is_enabled('t_finish', #{'p_sync_point' := [_]}, #seq_instance_state{completed = Completed, instance_count = Count}) ->
    length(Completed) =:= Count;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: seq_instance_state()) ->
          {produce, map()} | {produce, map(), seq_instance_state()} | abort.

fire('t_spawn', #{'p_start' := [start]}, #seq_instance_state{instance_count = Count} = State) ->
    log_event(State, <<"SequentialMultipleInstances">>, <<"Spawn">>, #{<<"count">> => Count}),
    {produce, #{
        'p_start' => [],
        'p_active' => [{instance, 1}]
    }, State#seq_instance_state{current_index = 1}};

fire('t_exec', #{'p_active' := [{instance, Index}]}, #seq_instance_state{subprocess = Subprocess, input_data = DataList} = State) ->
    Result = execute_instance(Subprocess, get_instance_data(Index, DataList)),
    log_event(State, <<"SequentialMultipleInstances">>, <<"Exec">>, #{<<"index">> => Index}),
    {produce, #{
        'p_active' => [],
        'p_completed' => [{complete, Index, Result}]
    }, State};

fire('t_next', #{'p_completed' := [{complete, Index, Result}]}, #seq_instance_state{current_index = CurrentIndex, instance_count = Count, completed = Completed, results = Results} = State) ->
    NewCompleted = [Index | Completed],
    NewResults = maps:put(Index, Result, Results),
    NewIndex = CurrentIndex + 1,
    log_event(State, <<"SequentialMultipleInstances">>, <<"Next">>, #{<<"from">> => Index, <<"to">> => NewIndex}),
    case NewIndex =< Count of
        true ->
            {produce, #{
                'p_completed' => [],
                'p_active' => [{instance, NewIndex}]
            }, State#seq_instance_state{current_index = NewIndex, completed = NewCompleted, results = NewResults}};
        false ->
            {produce, #{
                'p_completed' => [],
                'p_sync_point' => [{done}]
            }, State#seq_instance_state{completed = NewCompleted, results = NewResults}}
    end;

fire('t_finish', #{'p_sync_point' := [done]}, #seq_instance_state{results = Results, instance_count = Count} = State) ->
    ResultList = [maps:get(I, Results) || I <- lists:seq(1, Count)],
    log_event(State, <<"SequentialMultipleInstances">>, <<"Complete">>, #{}),
    {produce, #{
        'p_sync_point' => [],
        'p_end' => [{complete, ResultList}]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: seq_instance_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: seq_instance_state()) ->
          {ok, seq_instance_state()}.

init(SeqInstanceState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"SequentialMultipleInstances">>}) of
        {ok, LogId} ->
            State1 = SeqInstanceState#seq_instance_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, SeqInstanceState}
    end.

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

handle_cast({input_data, InputData}, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #seq_instance_state{} = State ->
            NewState = State#seq_instance_state{input_data = InputData},
            NewUsrInfo = gen_yawl:set_usr_info(NetState, NewState),
            {noreply, NewUsrInfo};
        _ ->
            {noreply, NetState}
    end;
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

terminate(_Reason, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #seq_instance_state{log_id = LogId} when LogId =/= undefined ->
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        _ ->
            ok
    end,
    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes instances sequentially.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec execute_sequential(Subprocess :: function(), InputData :: list(),
                         Index :: pos_integer(), Count :: pos_integer(),
                         Ref :: reference(), Parent :: pid(),
                         Timeout :: timeout(), Acc :: list()) ->
          {ok, list()} | {error, term()}.

execute_sequential(_Subprocess, _InputData, Index, Count, _Ref, _Parent, _Timeout, Acc)
  when Index > Count ->
    {ok, lists:reverse(Acc)};

execute_sequential(Subprocess, InputData, Index, Count, Ref, Parent, Timeout, Acc) ->
    Data = lists:nth(Index, InputData),
    try
        Result = Subprocess(Data),
        execute_sequential(Subprocess, InputData, Index + 1, Count, Ref, Parent, Timeout, [Result | Acc])
    catch
        Error:Reason:_Stack ->
            {error, {instance_error, Index, Error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Executes an instance with given data.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec execute_instance(Subprocess :: function(), Data :: term()) -> term().

execute_instance(Subprocess, Data) ->
    try
        Subprocess(Data)
    catch
        _:_ -> {error, instance_failed}
    end.

%%--------------------------------------------------------------------
%% @doc Gets data for an instance by index.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec get_instance_data(Index :: pos_integer(), DataList :: list()) -> term().

get_instance_data(Index, DataList) when Index > 0, Index =< length(DataList) ->
    lists:nth(Index, DataList);
get_instance_data(_Index, _DataList) ->
    undefined.

%%--------------------------------------------------------------------
%% @doc Waits for workflow completion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_for_completion(Pid :: pid(), Timeout :: timeout()) ->
          {ok, list()} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_end', Ref},
    receive
        {trigger, 'p_end', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #seq_instance_state{results = Results, instance_count = Count} ->
                            ResultList = [maps:get(I, Results) || I <- lists:seq(1, Count)],
                            {ok, ResultList};
                        _ ->
                            {error, no_result}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    after Timeout ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Generates a unique log ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().

generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"seq_instance_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @doc Generates a unique case ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id() -> binary().

generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @doc Logs an XES event.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec log_event(State :: seq_instance_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#seq_instance_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Test execute/2
execute_test() ->
    Fun = fun(X) -> X * 2 end,
    ?assertEqual({ok, [2,4,6]}, execute(Fun, [1,2,3])).

-endif.
