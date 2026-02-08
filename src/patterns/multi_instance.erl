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
%% @doc Multiple Instance Workflow Patterns for YAWL
%%
%% This module implements the Multiple Instance workflow patterns from
%% the Workflow Patterns Initiative (2003). These patterns handle
%% creating multiple concurrent instances of a task.
%%
%% <h3>Patterns Implemented</h3>
%% <ul>
%%   <li><b>WCP12:</b> Multiple Instances without Synchronization</li>
%%   <li><b>WCP13:</b> Multiple Instances with Design Time Knowledge</li>
%%   <li><b>WCP14:</b> Multiple Instances with Runtime Knowledge</li>
%% </ul>
%%
%% <h3>Pattern Differences</h3>
%% <ul>
%%   <li><b>No synchronization:</b> Spawn instances and continue immediately</li>
%%   <li><b>Design time knowledge:</b> Fixed N known at design time, sync on completion</li>
%%   <li><b>Runtime knowledge:</b> N determined at runtime, sync on completion</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(multi_instance).
-moduledoc """
Multiple Instance Workflow Patterns for YAWL.

This module implements the Multiple Instance workflow patterns from
the Workflow Patterns Initiative (2003).

## Patterns

- `multiple_instances_no_sync/2` - Create N parallel instances with no synchronization
- `multiple_instances_design_time/3` - Fixed number of instances known at design time
- `multiple_instances_runtime/3` - Number determined at runtime
- `create_instances/3` - Spawn N instance tokens from a single task
- `collect_instances/2` - Gather results when all instances complete
- `instance_counter/1` - Track active instance count

## Examples

Create instances without synchronization:

```erlang
> Fun = fun(X) -> X * 2 end,
> multi_instance:multiple_instances_no_sync(Fun, 5).
{ok, [pids...]}```

Create instances with design-time knowledge:

```erlang
> Fun = fun(X) -> X * 2 end,
> Data = [1,2,3],
> multi_instance:multiple_instances_design_time(Fun, Data, 3).
{ok, [2,4,6]}```

Create instances with runtime knowledge:

```erlang
> Fun = fun(X) -> X * 2 end,
> Data = [1,2,3,4,5],
> NFun = fun(List) -> length(List) end,
> multi_instance:multiple_instances_runtime(Fun, Data, NFun).
{ok, [2,4,6,8,10]}```
""".

%% API exports
-export([
    multiple_instances_no_sync/2,
    multiple_instances_design_time/3,
    multiple_instances_runtime/3,
    create_instances/3,
    collect_instances/2,
    instance_counter/1
]).

%%====================================================================
%% Types
%%====================================================================

-type instance_id() :: pos_integer().
-type instance_token() :: {instance, instance_id(), term()}.
-type instance_result() :: {instance_id(), term()}.
-type counter_state() :: #{
    total => pos_integer(),
    active => non_neg_integer(),
    completed => non_neg_integer()
}.
-type sync_state() :: #{
    expected => pos_integer(),
    results => [instance_result()],
    pids => [pid()]
}.

-export_type([
    instance_id/0,
    instance_token/0,
    instance_result/0,
    counter_state/0,
    sync_state/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Multiple Instances without Synchronization (WCP12).

Creates N parallel instances of a subprocess without waiting for
completion. The function spawns all instances and returns immediately
with the list of instance PIDs.

## Example

```erlang
> Fun = fun(X) -> timer:sleep(100), X * 2 end,
> {ok, Pids} = multi_instance:multiple_instances_no_sync(Fun, 3),
> length(Pids).
3```

Parameters:
- `Subprocess` - Function to execute for each instance (receives index as argument)
- `Count` - Number of instances to create

Returns `{ok, Pids}` where Pids is a list of process identifiers.
""".
-spec multiple_instances_no_sync(Subprocess :: function(), Count :: pos_integer()) ->
          {ok, [pid()]} | {error, term()}.

multiple_instances_no_sync(Subprocess, Count) when is_function(Subprocess), is_integer(Count), Count > 0 ->
    Parent = self(),
    Ref = make_ref(),

    Pids = lists:map(fun(Index) ->
        spawn(fun() ->
            try
                Result = Subprocess(Index),
                Parent ! {Ref, {instance_complete, Index}, Result}
            catch
                Error:Reason:Stack ->
                    Parent ! {Ref, {instance_error, Index}, {Error, Reason, Stack}}
            end
        end)
    end, lists:seq(1, Count)),

    {ok, Pids};
multiple_instances_no_sync(_Subprocess, _Count) ->
    {error, invalid_arguments}.

%%--------------------------------------------------------------------
-doc """
Multiple Instances with Design Time Knowledge (WCP13).

Creates a fixed number of instances known at design time and
synchronizes on completion of all instances.

## Example

```erlang
> Fun = fun(X) -> X * 2 end,
> Data = [1,2,3,4],
> {ok, Results} = multi_instance:multiple_instances_design_time(Fun, Data, 4),
> Results.
[2,4,6,8]```

Parameters:
- `Subprocess` - Function to execute for each instance
- `InputData` - List of input data, one per instance
- `InstanceCount` - Fixed number of instances (must equal length of InputData)

Returns `{ok, Results}` ordered list or `{error, Reason}`.
""".
-spec multiple_instances_design_time(
    Subprocess :: function(),
    InputData :: list(),
    InstanceCount :: pos_integer()
) -> {ok, [term()]} | {error, term()}.

multiple_instances_design_time(Subprocess, InputData, InstanceCount)
  when is_function(Subprocess), is_list(InputData), is_integer(InstanceCount) ->
    ActualCount = length(InputData),
    if
        ActualCount =/= InstanceCount ->
            {error, {count_mismatch, ActualCount, InstanceCount}};
        InstanceCount < 1 ->
            {error, invalid_count};
        true ->
            execute_with_sync(Subprocess, InputData, InstanceCount)
    end;
multiple_instances_design_time(_Subprocess, _InputData, _InstanceCount) ->
    {error, invalid_arguments}.

%%--------------------------------------------------------------------
-doc """
Multiple Instances with Runtime Knowledge (WCP14).

Creates instances where the number is determined at runtime via a
calculation function, then synchronizes on completion.

## Example

```erlang
> Fun = fun(X) -> X * 2 end,
> Data = [1,2,3,4,5],
> CountFun = fun(List) -> length(List) end,
> {ok, Results} = multi_instance:multiple_instances_runtime(Fun, Data, CountFun),
> Results.
[2,4,6,8,10]```

Parameters:
- `Subprocess` - Function to execute for each instance
- `InputData` - List of input data
- `CountFun` - Function that calculates instance count from InputData

Returns `{ok, Results}` ordered list or `{error, Reason}`.
""".
-spec multiple_instances_runtime(
    Subprocess :: function(),
    InputData :: list(),
    CountFun :: function()
) -> {ok, [term()]} | {error, term()}.

multiple_instances_runtime(Subprocess, InputData, CountFun)
  when is_function(Subprocess), is_list(InputData), is_function(CountFun) ->
    try
        InstanceCount = CountFun(InputData),
        if
            InstanceCount < 1 ->
                {error, invalid_count};
            InstanceCount > length(InputData) ->
                {error, {insufficient_data, length(InputData), InstanceCount}};
            true ->
                execute_with_sync(Subprocess, InputData, InstanceCount)
        end
    catch
        _:_ ->
            {error, count_function_failed}
    end;
multiple_instances_runtime(_Subprocess, _InputData, _CountFun) ->
    {error, invalid_arguments}.

%%--------------------------------------------------------------------
-doc """
Create instance tokens from a single task.

Spawns N instance tokens from a single task token, distributing
the input data across instances.

## Example

```erlang
> Data = [a,b,c],
> Tokens = multi_instance:create_instances(Data, 3, fun(X) -> X end),
> length(Tokens).
3```

Parameters:
- `InputData` - List of data items to distribute
- `Count` - Number of instance tokens to create
- `TransformFun` - Optional function to transform data before wrapping

Returns list of instance tokens.
""".
-spec create_instances(InputData :: list(), Count :: pos_integer(), TransformFun :: function()) ->
          [instance_token()].

create_instances(InputData, Count, TransformFun) when is_list(InputData), is_integer(Count), Count > 0 ->
    lists:map(fun(Index) ->
        Data = if
            Index > length(InputData) -> undefined;
            true -> lists:nth(Index, InputData)
        end,
        Transformed = try TransformFun(Data) catch _:_ -> Data end,
        {instance, Index, Transformed}
    end, lists:seq(1, Count)).

%%--------------------------------------------------------------------
-doc """
Collect results when all instances complete.

Waits for all spawned instances to complete and collects their
results in order.

## Example

```erlang
> Fun = fun(X) -> X * 2 end,
> {ok, Results} = multi_instance:multiple_instances_design_time(Fun, [1,2,3], 3),
> Results.
[2,4,6]```

Parameters:
- `Ref` - Reference for matching instance messages
- `ExpectedCount` - Number of instances to wait for

Returns `{ok, Results}` ordered list or `{error, Reason}`.
""".
-spec collect_instances(Ref :: reference(), ExpectedCount :: pos_integer()) ->
          {ok, [term()]} | {error, term()}.

collect_instances(_Ref, 0) ->
    {ok, []};
collect_instances(Ref, ExpectedCount) when ExpectedCount > 0 ->
    collect_instances_loop(Ref, ExpectedCount, #{}).

%%--------------------------------------------------------------------
-doc """
Track active instance count.

Creates or updates a counter state for tracking active instances.

## Example

```erlang
> Counter0 = multi_instance:instance_counter(new),
> Counter1 = multi_instance:instance_counter({spawn, 5, Counter0}),
> Counter2 = multi_instance:instance_counter({complete, 1, Counter1}),
> maps:get(active, Counter2).
4```

Parameters:
- `Input` - Either `new`, `{spawn, Count, State}`, or `{complete, Count, State}`

Returns counter state map with keys: `total`, `active`, `completed`.
""".
-spec instance_counter(Input :: new | {spawn, pos_integer(), counter_state()} |
                                   {complete, pos_integer(), counter_state()}) ->
          counter_state().

instance_counter(new) ->
    #{total => 0, active => 0, completed => 0};
instance_counter({spawn, Count, State}) when is_map(State) ->
    Active = maps:get(active, State, 0) + Count,
    Total = maps:get(total, State, 0) + Count,
    State#{active => Active, total => Total};
instance_counter({complete, Count, State}) when is_map(State) ->
    Active = max(0, maps:get(active, State, 0) - Count),
    Completed = maps:get(completed, State, 0) + Count,
    State#{active => Active, completed => Completed}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes instances with synchronization barrier.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec execute_with_sync(Subprocess :: function(), InputData :: list(), Count :: pos_integer()) ->
          {ok, [term()]} | {error, term()}.

execute_with_sync(Subprocess, InputData, Count) ->
    Parent = self(),
    Ref = make_ref(),

    %% Create monitored instance processes
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
    end, lists:zip(InputData, lists:seq(1, Count))),

    %% Wait for all instances with timeout
    wait_all_instances(Ref, PidMRefs, Count, 30000, #{}).

%%--------------------------------------------------------------------
%% @doc Wait for all instances to complete.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_all_instances(
    Ref :: reference(),
    PidMRefs :: [{pid(), reference(), pos_integer()}],
    Remaining :: pos_integer(),
    Timeout :: timeout(),
    Acc :: map()
) -> {ok, [term()]} | {error, term()}.

wait_all_instances(_Ref, _PidMRefs, 0, _Timeout, Acc) ->
    %% Convert map to ordered list
    ResultList = [maps:get(I, Acc) || I <- lists:seq(1, maps:size(Acc))],
    {ok, ResultList};
wait_all_instances(Ref, PidMRefs, Remaining, Timeout, Acc) ->
    receive
        {Ref, {instance_complete, Index}, Result} ->
            %% Remove completed instance from tracking
            NewPidMRefs = [E || E = {_P, _MRef, I} <- PidMRefs, I =/= Index],
            wait_all_instances(Ref, NewPidMRefs, Remaining - 1, Timeout, maps:put(Index, Result, Acc));
        {Ref, {instance_error, Index}, {Error, Reason, _Stack}} ->
            %% Cleanup on error
            cleanup_remaining(PidMRefs),
            {error, {instance_error, Index, Error, Reason}};
        {'DOWN', MRef, process, Pid, Reason} ->
            %% Process died - check if it was one of ours
            case [I || {P, MR, I} <- PidMRefs, P =:= Pid, MR =:= MRef] of
                [Index] ->
                    cleanup_remaining(PidMRefs),
                    {error, {instance_crash, Index, Reason}};
                [] ->
                    %% Stale DOWN (process already completed) - ignore
                    wait_all_instances(Ref, PidMRefs, Remaining, Timeout, Acc)
            end
    after Timeout ->
        cleanup_remaining(PidMRefs),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Collect instance results in loop.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec collect_instances_loop(Ref :: reference(), Remaining :: pos_integer(), Acc :: map()) ->
          {ok, [term()]} | {error, term()}.

collect_instances_loop(_Ref, 0, Acc) ->
    ResultList = [maps:get(I, Acc) || I <- lists:seq(1, maps:size(Acc))],
    {ok, ResultList};
collect_instances_loop(Ref, Remaining, Acc) ->
    receive
        {Ref, {instance_complete, Index}, Result} ->
            collect_instances_loop(Ref, Remaining - 1, maps:put(Index, Result, Acc));
        {Ref, {instance_error, _Index}, {_Error, _Reason, _Stack}} ->
            {error, instance_failed}
    after 5000 ->
        {error, collect_timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Cleanup remaining processes.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec cleanup_remaining([{pid(), reference(), pos_integer()}]) -> ok.

cleanup_remaining(PidMRefs) ->
    lists:foreach(fun({Pid, MRef, _}) ->
        exit(Pid, kill),
        demonitor(MRef, [flush])
    end, PidMRefs),
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test multiple_instances_no_sync/2
no_sync_test() ->
    Fun = fun(X) -> X * 2 end,
    {ok, Pids} = multiple_instances_no_sync(Fun, 3),
    ?assertEqual(3, length(Pids)).

no_sync_invalid_count_test() ->
    Fun = fun(X) -> X * 2 end,
    ?assertEqual({error, invalid_arguments}, multiple_instances_no_sync(Fun, 0)).

%% Test multiple_instances_design_time/3
design_time_test() ->
    Fun = fun(X) -> X * 2 end,
    Data = [1, 2, 3],
    {ok, Results} = multiple_instances_design_time(Fun, Data, 3),
    ?assertEqual([2, 4, 6], Results).

design_time_count_mismatch_test() ->
    Fun = fun(X) -> X * 2 end,
    Data = [1, 2, 3],
    ?assertEqual({error, {count_mismatch, 3, 5}}, multiple_instances_design_time(Fun, Data, 5)).

%% Test multiple_instances_runtime/3
runtime_test() ->
    Fun = fun(X) -> X * 2 end,
    Data = [1, 2, 3, 4],
    CountFun = fun(L) -> length(L) end,
    {ok, Results} = multiple_instances_runtime(Fun, Data, CountFun),
    ?assertEqual([2, 4, 6, 8], Results).

runtime_insufficient_data_test() ->
    Fun = fun(X) -> X * 2 end,
    Data = [1, 2],
    CountFun = fun(_L) -> 5 end,
    ?assertEqual({error, {insufficient_data, 2, 5}}, multiple_instances_runtime(Fun, Data, CountFun)).

%% Test create_instances/3
create_instances_test() ->
    Data = [a, b, c],
    Tokens = create_instances(Data, 3, fun(X) -> X end),
    ?assertEqual(3, length(Tokens)),
    ?assertEqual([{instance, 1, a}, {instance, 2, b}, {instance, 3, c}], Tokens).

create_instances_transform_test() ->
    Data = [1, 2, 3],
    Tokens = create_instances(Data, 3, fun(X) -> X * 10 end),
    ?assertEqual([{instance, 1, 10}, {instance, 2, 20}, {instance, 3, 30}], Tokens).

create_instances_undefined_data_test() ->
    Data = [a],
    Tokens = create_instances(Data, 3, fun(X) -> X end),
    ?assertEqual([{instance, 1, a}, {instance, 2, undefined}, {instance, 3, undefined}], Tokens).

%% Test instance_counter/1
instance_counter_new_test() ->
    State = instance_counter(new),
    ?assertEqual(#{total => 0, active => 0, completed => 0}, State).

instance_counter_spawn_test() ->
    State0 = instance_counter(new),
    State1 = instance_counter({spawn, 5, State0}),
    ?assertEqual(5, maps:get(total, State1)),
    ?assertEqual(5, maps:get(active, State1)),
    ?assertEqual(0, maps:get(completed, State1)).

instance_counter_complete_test() ->
    State0 = instance_counter(new),
    State1 = instance_counter({spawn, 5, State0}),
    State2 = instance_counter({complete, 2, State1}),
    ?assertEqual(5, maps:get(total, State2)),
    ?assertEqual(3, maps:get(active, State2)),
    ?assertEqual(2, maps:get(completed, State2)).

%% Test collect_instances/2
collect_instances_empty_test() ->
    ?assertEqual({ok, []}, collect_instances(make_ref(), 0)).

-endif.
