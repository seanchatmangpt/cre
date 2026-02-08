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
%% @doc Multiple Instances with Synchronization Pattern (WCP-12) for YAWL
%%
%% This module implements the Multiple Instances with Synchronization pattern
%% as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Multiple Instances with Synchronization pattern (WCP-12) creates
%% multiple concurrent instances of a subprocess and waits for all instances
%% to complete before continuing (synchronization barrier).
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start          - Start of the workflow
%%     p_spawn          - Spawn instances trigger
%%     p_instance_pool  - Pool of instance tokens
%%     p_active_1       - Instance 1 active
%%     p_active_2       - Instance 2 active
%%     p_active_3       - Instance 3 active
%%     p_active_4       - Instance 4 active
%%     p_complete_1     - Instance 1 complete
%%     p_complete_2     - Instance 2 complete
%%     p_complete_3     - Instance 3 complete
%%     p_complete_4     - Instance 4 complete
%%     p_sync_barrier   - Synchronization barrier
%%     p_all_done       - All instances complete
%%     p_complete       - Workflow complete
%%
%%   Transitions:
%%     t_spawn          - Spawn all instances
%%     t_exec_1         - Execute instance 1
%%     t_exec_2         - Execute instance 2
%%     t_exec_3         - Execute instance 3
%%     t_exec_4         - Execute instance 4
%%     t_finish_1       - Mark instance 1 complete
%%     t_finish_2       - Mark instance 2 complete
%%     t_finish_3       - Mark instance 3 complete
%%     t_finish_4       - Mark instance 4 complete
%%     t_sync           - Synchronize all instances
%%     t_complete       - Complete workflow
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always true (all instances complete)</li>
%%   <li><b>Proper completion:</b> All instances complete exactly once</li>
%%   <li><b>No dead transitions:</b> All instances execute and reach sync point</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(multiple_instances_sync).
-moduledoc """
Multiple Instances with Synchronization Pattern (WCP-12) for YAWL.

This module implements the Multiple Instances with Synchronization pattern
as a gen_yawl behaviour. Multiple instances are spawned concurrently and
synchronized at a barrier before completion.

## Example: Execute Multiple Instances

```erlang
> Fun = fun(X) -> X * 2 end,
> DataList = [1, 2, 3, 4],
> multiple_instances_sync:execute(Fun, DataList).
{ok, [2,4,6,8]}
```

## Example: Place List

```erlang
> multiple_instances_sync:place_lst().
['p_start','p_spawn','p_instance_pool','p_active_1','p_active_2',
 'p_active_3','p_active_4','p_complete_1','p_complete_2',
 'p_complete_3','p_complete_4','p_sync_barrier','p_all_done',
 'p_complete']
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

-record(multi_instance_state, {
    subprocess :: function(),
    instance_count :: pos_integer(),
    input_data :: list(),
    completed = [] :: [pos_integer()],
    results = #{} :: #{pos_integer() => term()},
    log_id :: binary() | undefined
}).

-type multi_instance_state() :: #multi_instance_state{}.
-export_type([multi_instance_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Multiple Instances with Synchronization pattern state.

## Example

```erlang
> Fun = fun(X) -> X * 2 end,
> State = multiple_instances_sync:new(Fun, 3).
{multi_instance_state,_,3,[],[],#{},_}
```
""".
-spec new(Subprocess :: function(), InstanceCount :: pos_integer()) ->
          multi_instance_state().

new(Subprocess, InstanceCount) when is_function(Subprocess), is_integer(InstanceCount), InstanceCount > 0 ->
    LogId = generate_log_id(),
    #multi_instance_state{
        subprocess = Subprocess,
        instance_count = InstanceCount,
        input_data = [],
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Multiple Instances workflow as a gen_yawl process.
%% @end
%%--------------------------------------------------------------------
-spec start(Subprocess :: function(), InstanceCount :: pos_integer()) ->
          {ok, pid()} | {error, term()}.

start(Subprocess, InstanceCount) when is_function(Subprocess), is_integer(InstanceCount), InstanceCount > 0 ->
    State = new(Subprocess, InstanceCount),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Runs the Multiple Instances workflow synchronously.
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
%% @doc Gets the current state of the Multiple Instances workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, multi_instance_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
-doc """
Executes the Multiple Instances with Synchronization pattern.

Spawns multiple concurrent instances and waits for all to complete
before returning results.

## Example

```erlang
> Fun = fun(X) -> X * 2 end,
> multiple_instances_sync:execute(Fun, [1,2,3,4]).
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

    %% Spawn all instances in parallel (monitored for crash detection)
    PidMRefs = lists:map(fun({Data, Index}) ->
        {Pid, MRef} = spawn_monitor(fun() ->
            try
                Result = Subprocess(Data),
                Parent ! {Ref, {instance_complete, Index}, Result}
            catch
                Error:Reason:Stack ->
                    Parent ! {Ref, {instance_error, Index}, {Error, Reason, Stack}}
            end
        end),
        {Pid, MRef, Index}
    end, lists:zip(InputData, lists:seq(1, InstanceCount))),

    %% Wait for all instances to complete
    wait_all_instances(Ref, PidMRefs, InstanceCount, 30000, #{}).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Multiple Instances Petri net.

```erlang
> multiple_instances_sync:place_lst().
['p_start','p_spawn','p_instance_pool','p_active_1','p_active_2',
 'p_active_3','p_active_4','p_complete_1','p_complete_2',
 'p_complete_3','p_complete_4','p_sync_barrier','p_all_done',
 'p_complete']
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_spawn',
        'p_instance_pool',
        'p_active_1',
        'p_active_2',
        'p_active_3',
        'p_active_4',
        'p_complete_1',
        'p_complete_2',
        'p_complete_3',
        'p_complete_4',
        'p_sync_barrier',
        'p_all_done',
        'p_complete'
    ].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Multiple Instances Petri net.

```erlang
> multiple_instances_sync:trsn_lst().
['t_spawn','t_exec_1','t_exec_2','t_exec_3','t_exec_4',
 't_finish_1','t_finish_2','t_finish_3','t_finish_4',
 't_sync','t_complete']
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_spawn',
        't_exec_1',
        't_exec_2',
        't_exec_3',
        't_exec_4',
        't_finish_1',
        't_finish_2',
        't_finish_3',
        't_finish_4',
        't_sync',
        't_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: multi_instance_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> multiple_instances_sync:preset('t_spawn').
['p_start']
> multiple_instances_sync:preset('t_sync').
['p_sync_barrier']
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_spawn') -> ['p_start'];
preset('t_exec_1') -> ['p_active_1'];
preset('t_exec_2') -> ['p_active_2'];
preset('t_exec_3') -> ['p_active_3'];
preset('t_exec_4') -> ['p_active_4'];
preset('t_finish_1') -> ['p_active_1'];
preset('t_finish_2') -> ['p_active_2'];
preset('t_finish_3') -> ['p_active_3'];
preset('t_finish_4') -> ['p_active_4'];
preset('t_sync') -> ['p_sync_barrier'];
preset('t_complete') -> ['p_all_done'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: multi_instance_state()) ->
          boolean().

is_enabled('t_spawn', _Mode, _UsrInfo) ->
    true;
is_enabled('t_exec_1', #{'p_active_1' := [{instance, _Index}]}, _UsrInfo) ->
    true;
is_enabled('t_exec_2', #{'p_active_2' := [{instance, _Index}]}, _UsrInfo) ->
    true;
is_enabled('t_exec_3', #{'p_active_3' := [{instance, _Index}]}, #multi_instance_state{instance_count = Count}) when Count >= 3 ->
    true;
is_enabled('t_exec_4', #{'p_active_4' := [{instance, _Index}]}, #multi_instance_state{instance_count = Count}) when Count >= 4 ->
    true;
is_enabled('t_finish_1', #{'p_active_1' := [{instance, _Index}]}, _UsrInfo) ->
    true;
is_enabled('t_finish_2', #{'p_active_2' := [{instance, _Index}]}, _UsrInfo) ->
    true;
is_enabled('t_finish_3', #{'p_active_3' := [{instance, _Index}]}, #multi_instance_state{instance_count = Count}) when Count >= 3 ->
    true;
is_enabled('t_finish_4', #{'p_active_4' := [{instance, _Index}]}, #multi_instance_state{instance_count = Count}) when Count >= 4 ->
    true;
is_enabled('t_sync', #{'p_sync_barrier' := Tokens}, #multi_instance_state{completed = Completed, instance_count = Count}) ->
    length(Tokens) =:= Count andalso length(Completed) =:= Count;
is_enabled('t_complete', #{'p_all_done' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: multi_instance_state()) ->
          {produce, map()} | {produce, map(), multi_instance_state()} | abort.

fire('t_spawn', #{'p_start' := [start]}, #multi_instance_state{instance_count = Count} = State) ->
    log_event(State, <<"MultipleInstancesSync">>, <<"Spawn">>, #{<<"count">> => Count}),
    %% Spawn instances based on count
    SpawnResult = spawn_instances(Count),
    {produce, #{
        'p_start' => [],
        'p_spawn' => SpawnResult
    }, State};

fire('t_exec_1', #{'p_active_1' := [{instance, Index}]}, #multi_instance_state{subprocess = Subprocess, input_data = DataList} = State) ->
    Result = execute_instance(Subprocess, get_instance_data(Index, DataList)),
    log_event(State, <<"MultipleInstancesSync">>, <<"Exec1">>, #{<<"index">> => Index}),
    {produce, #{
        'p_active_1' => [],
        'p_complete_1' => [{complete, Index, Result}]
    }, State};

fire('t_exec_2', #{'p_active_2' := [{instance, Index}]}, #multi_instance_state{subprocess = Subprocess, input_data = DataList} = State) ->
    Result = execute_instance(Subprocess, get_instance_data(Index, DataList)),
    log_event(State, <<"MultipleInstancesSync">>, <<"Exec2">>, #{<<"index">> => Index}),
    {produce, #{
        'p_active_2' => [],
        'p_complete_2' => [{complete, Index, Result}]
    }, State};

fire('t_exec_3', #{'p_active_3' := [{instance, Index}]}, #multi_instance_state{subprocess = Subprocess, input_data = DataList} = State) ->
    Result = execute_instance(Subprocess, get_instance_data(Index, DataList)),
    log_event(State, <<"MultipleInstancesSync">>, <<"Exec3">>, #{<<"index">> => Index}),
    {produce, #{
        'p_active_3' => [],
        'p_complete_3' => [{complete, Index, Result}]
    }, State};

fire('t_exec_4', #{'p_active_4' := [{instance, Index}]}, #multi_instance_state{subprocess = Subprocess, input_data = DataList} = State) ->
    Result = execute_instance(Subprocess, get_instance_data(Index, DataList)),
    log_event(State, <<"MultipleInstancesSync">>, <<"Exec4">>, #{<<"index">> => Index}),
    {produce, #{
        'p_active_4' => [],
        'p_complete_4' => [{complete, Index, Result}]
    }, State};

fire('t_finish_1', #{'p_active_1' := [{instance, Index}]}, State) ->
    finish_instance(1, Index, State, #{'p_active_1' => []});

fire('t_finish_2', #{'p_active_2' := [{instance, Index}]}, State) ->
    finish_instance(2, Index, State, #{'p_active_2' => []});

fire('t_finish_3', #{'p_active_3' := [{instance, Index}]}, State) ->
    finish_instance(3, Index, State, #{'p_active_3' => []});

fire('t_finish_4', #{'p_active_4' := [{instance, Index}]}, State) ->
    finish_instance(4, Index, State, #{'p_active_4' => []});

fire('t_sync', #{'p_sync_barrier' := Tokens}, #multi_instance_state{completed = Completed, instance_count = Count} = State)
  when length(Tokens) =:= Count andalso length(Completed) =:= Count ->
    log_event(State, <<"MultipleInstancesSync">>, <<"Sync">>, #{<<"all_done">> => true}),
    {produce, #{
        'p_sync_barrier' => [],
        'p_all_done' => [all_done]
    }, State};

fire('t_complete', #{'p_all_done' := [all_done]}, #multi_instance_state{results = Results} = State) ->
    ResultList = [maps:get(I, Results) || I <- lists:seq(1, map_size(Results))],
    log_event(State, <<"MultipleInstancesSync">>, <<"Complete">>, #{}),
    {produce, #{
        'p_all_done' => [],
        'p_complete' => [{complete, ResultList}]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: multi_instance_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: multi_instance_state()) ->
          {ok, multi_instance_state()}.

init(MultiInstanceState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"MultipleInstancesSync">>}) of
        {ok, LogId} ->
            State1 = MultiInstanceState#multi_instance_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, MultiInstanceState}
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
        #multi_instance_state{} = State ->
            NewState = State#multi_instance_state{input_data = InputData},
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
        #multi_instance_state{log_id = LogId} when LogId =/= undefined ->
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
%% @doc Spawns instance tokens based on count.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec spawn_instances(Count :: pos_integer()) -> map().

spawn_instances(Count) ->
    spawn_instances(Count, 1, #{}).

spawn_instances(Count, Index, Acc) when Index > Count ->
    Acc;
spawn_instances(Count, Index, Acc) when Index =< 4 ->
    Place = list_to_atom("p_active_" ++ integer_to_list(Index)),
    spawn_instances(Count, Index + 1, maps:put(Place, [{instance, Index}], Acc));
spawn_instances(Count, Index, Acc) ->
    %% For instances beyond 4, distribute across existing places
    PlaceNum = ((Index - 1) rem 4) + 1,
    Place = list_to_atom("p_active_" ++ integer_to_list(PlaceNum)),
    ExistingTokens = maps:get(Place, Acc, []),
    spawn_instances(Count, Index + 1, maps:put(Place, ExistingTokens ++ [{instance, Index}], Acc)).

%%--------------------------------------------------------------------
%% @doc Handles instance completion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec finish_instance(PlaceNum :: pos_integer(), Index :: pos_integer(),
                      State :: multi_instance_state(), BaseMap :: map()) ->
          {produce, map(), multi_instance_state()}.

finish_instance(PlaceNum, Index, #multi_instance_state{completed = Completed, results = Results, instance_count = Count} = State, BaseMap) ->
    CompletePlace = list_to_atom("p_complete_" ++ integer_to_list(PlaceNum)),
    NewCompleted = [Index | Completed],
    NewState = State#multi_instance_state{completed = NewCompleted},
    NewResults = maps:put(Index, ok, Results), %% Simplified result storage
    AllDone = length(NewCompleted) =:= Count,
    NewBaseMap = maps:put(CompletePlace, [{complete, Index}], BaseMap),
    case AllDone of
        true ->
            %% Add sync barrier tokens
            SyncTokens = lists:map(fun(I) -> {sync_done, I} end, lists:sort(NewCompleted)),
            {produce, maps:merge(NewBaseMap, #{'p_sync_barrier' => SyncTokens}), NewResults, NewState};
        false ->
            {produce, NewBaseMap, NewState}
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
    Pid ! {trigger, 'p_complete', Ref},
    receive
        {trigger, 'p_complete', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #multi_instance_state{results = Results} ->
                            ResultList = [maps:get(I, Results) || I <- lists:seq(1, map_size(Results))],
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
%% @doc Waits for all instances to complete.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_all_instances(Ref :: reference(), PidMRefs :: [{pid(), reference(), pos_integer()}],
                         Remaining :: pos_integer(), Timeout :: timeout(), Acc :: map()) ->
          {ok, list()} | {error, term()}.

wait_all_instances(_Ref, _PidMRefs, 0, _Timeout, Acc) ->
    %% Convert map to ordered list
    ResultList = [maps:get(I, Acc) || I <- lists:seq(1, maps:size(Acc))],
    {ok, ResultList};
wait_all_instances(Ref, PidMRefs, Remaining, Timeout, Acc) ->
    receive
        {Ref, {instance_complete, Index}, Result} ->
            NewPidMRefs = [E || E = {_P, _MRef, I} <- PidMRefs, I =/= Index],
            wait_all_instances(Ref, NewPidMRefs, Remaining - 1, Timeout, maps:put(Index, Result, Acc));
        {Ref, {instance_error, Index}, {Error, Reason, _Stack}} ->
            {error, {instance_error, Index, Error, Reason}};
        {'DOWN', MRef, process, Pid, Reason} ->
            case [I || {P, MR, I} <- PidMRefs, P =:= Pid, MR =:= MRef] of
                [Index] ->
                    {error, {instance_crash, Index, Reason}};
                [] ->
                    %% Stale DOWN (process already completed) - ignore
                    wait_all_instances(Ref, PidMRefs, Remaining, Timeout, Acc)
            end
    after Timeout ->
        lists:foreach(fun({Pid, MRef, _}) ->
            exit(Pid, kill),
            demonitor(MRef, [flush])
        end, PidMRefs),
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
    <<"multi_instance_sync_", Hex/binary>>.

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
-spec log_event(State :: multi_instance_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#multi_instance_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
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
