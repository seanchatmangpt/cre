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
%% @doc Multiple Choice Pattern (WCP-06) for YAWL
%%
%% This module implements the Multiple Choice pattern as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Multiple Choice pattern (WCP-06) splits the workflow into multiple
%% concurrent branches based on runtime data evaluation. Multiple branches
%% can be selected simultaneously, and all selected branches execute in parallel.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Start of the workflow
%%     p_eval          - Evaluate choice conditions
%%     p_branch1       - Branch 1 selected
%%     p_branch2       - Branch 2 selected
%%     p_branch3       - Branch 3 selected
%%     p_branch4       - Branch 4 selected
%%     p_active_1      - Branch 1 executing
%%     p_active_2      - Branch 2 executing
%%     p_active_3      - Branch 3 executing
%%     p_active_4      - Branch 4 executing
%%     p_done_1        - Branch 1 complete
%%     p_done_2        - Branch 2 complete
%%     p_done_3        - Branch 3 complete
%%     p_done_4        - Branch 4 complete
%%     p_sync          - Synchronization point
%%     p_complete      - Workflow complete
%%
%%   Transitions:
%%     t_start         - Start the workflow
%%     t_eval          - Evaluate branch conditions
%%     t_select_1      - Select branch 1
%%     t_select_2      - Select branch 2
%%     t_select_3      - Select branch 3
%%     t_select_4      - Select branch 4
%%     t_exec_1        - Execute branch 1
%%     t_exec_2        - Execute branch 2
%%     t_exec_3        - Execute branch 3
%%     t_exec_4        - Execute branch 4
%%     t_finish_1      - Finish branch 1
%%     t_finish_2      - Finish branch 2
%%     t_finish_3      - Finish branch 3
%%     t_finish_4      - Finish branch 4
%%     t_sync          - Synchronize selected branches
%%     t_complete      - Complete workflow
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always true (all selected branches complete)</li>
%%   <li><b>Proper completion:</b> All selected branches complete exactly once</li>
%%   <li><b>No dead transitions:</b> All selected branches execute and reach sync</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(multiple_choice).
-moduledoc """
Multiple Choice Pattern (WCP-06) for YAWL.

This module implements the Multiple Choice pattern as a gen_yawl behaviour.
Multiple branches can be selected simultaneously based on runtime conditions.

## Example: Execute Multiple Choice

```erlang
> Options = #{
>   branch1 => {fun(X) -> X > 0 end, fun(X) -> X * 2 end},
>   branch2 => {fun(X) -> X < 10 end, fun(X) -> X + 1 end}
> },
> multiple_choice:execute(Options, 5).
{ok, #{branch1 => 10, branch2 => 6}}
```

## Example: Place List

```erlang
> multiple_choice:place_lst().
['p_start','p_eval','p_branch1','p_branch2','p_branch3','p_branch4',
 'p_active_1','p_active_2','p_active_3','p_active_4',
 'p_done_1','p_done_2','p_done_3','p_done_4',
 'p_sync','p_complete']
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
    run/2,
    get_state/1,
    execute/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(multiple_choice_state, {
    branches :: #{atom() => {function(), function()}},  %% {ConditionFun, BranchFun}
    input_data :: term() | undefined,
    selected = [] :: [atom()],
    completed = [] :: [atom()],
    results = #{} :: #{atom() => term()},
    log_id :: binary() | undefined
}).

-type multiple_choice_state() :: #multiple_choice_state{}.
-export_type([multiple_choice_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Multiple Choice pattern state.

The branches map contains:
- Key: Branch name (atom)
- Value: {ConditionFun, BranchFun} tuple
  - ConditionFun: Takes input data, returns boolean()
  - BranchFun: Takes input data, returns result

## Example

```erlang
> Options = #{
>   branch1 => {fun(X) -> X > 0 end, fun(X) -> X * 2 end},
>   branch2 => {fun(X) -> X < 10 end, fun(X) -> X + 1 end}
> },
> State = multiple_choice:new(Options).
{multiple_choice_state,_,_,[],[],#{},_}
```
""".
-spec new(Branches :: #{atom() => {function(), function()}}) -> multiple_choice_state().

new(Branches) when is_map(Branches) ->
    LogId = generate_log_id(),
    #multiple_choice_state{
        branches = Branches,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Multiple Choice workflow as a gen_yawl process.
%% @end
%%--------------------------------------------------------------------
-spec start(Branches :: #{atom() => {function(), function()}}) ->
          {ok, pid()} | {error, term()}.

start(Branches) when is_map(Branches) ->
    State = new(Branches),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Runs the Multiple Choice workflow synchronously.
%% @end
%%--------------------------------------------------------------------
-spec run(Branches :: #{atom() => {function(), function()}}, InputData :: term()) ->
          {ok, #{atom() => term()}} | {error, term()}.

run(Branches, InputData) when is_map(Branches) ->
    case start(Branches) of
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
%% @doc Gets the current state of the Multiple Choice workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, multiple_choice_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
-doc """
Executes the Multiple Choice pattern with given input data.

Multiple branches are selected based on their condition functions,
and all selected branches execute in parallel.

## Example

```erlang
> Options = #{
>   branch1 => {fun(X) -> X > 0 end, fun(X) -> X * 2 end},
>   branch2 => {fun(X) -> X < 10 end, fun(X) -> X + 1 end}
> },
> multiple_choice:execute(Options, 5).
{ok, #{branch1 => 10, branch2 => 6}}
```

Parameters:
- `Branches` - Map of branch_name => {ConditionFun, BranchFun}
- `InputData` - Data passed to condition and branch functions

Returns `{ok, Results}` map with branch names as keys, or `{error, Reason}`.
""".
-spec execute(Branches :: #{atom() => {function(), function()}}, InputData :: term()) ->
          {ok, #{atom() => term()}} | {error, term()}.

execute(Branches, InputData) when is_map(Branches) ->
    Ref = make_ref(),
    Parent = self(),

    %% Evaluate which branches to execute
    Selected = maps:fold(fun(BranchName, {ConditionFun, _BranchFun}, Acc) ->
        case ConditionFun(InputData) of
            true -> [BranchName | Acc];
            false -> Acc
        end
    end, [], Branches),

    %% Spawn selected branches
    Pids = lists:map(fun(BranchName) ->
        {_ConditionFun, BranchFun} = maps:get(BranchName, Branches),
        spawn(fun() ->
            try
                Result = BranchFun(InputData),
                Parent ! {Ref, {branch_complete, BranchName}, Result}
            catch
                Error:Reason:Stack ->
                    Parent ! {Ref, {branch_error, BranchName}, {Error, Reason, Stack}}
            end
        end)
    end, Selected),

    %% Wait for all selected branches to complete
    wait_all_branches(Ref, Pids, length(Selected), 30000, #{}).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Multiple Choice Petri net.

```erlang
> multiple_choice:place_lst().
['p_start','p_eval','p_branch1','p_branch2','p_branch3','p_branch4',
 'p_active_1','p_active_2','p_active_3','p_active_4',
 'p_done_1','p_done_2','p_done_3','p_done_4',
 'p_sync','p_complete']
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_eval',
        'p_branch1',
        'p_branch2',
        'p_branch3',
        'p_branch4',
        'p_active_1',
        'p_active_2',
        'p_active_3',
        'p_active_4',
        'p_done_1',
        'p_done_2',
        'p_done_3',
        'p_done_4',
        'p_sync',
        'p_complete'
    ].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Multiple Choice Petri net.

```erlang
> multiple_choice:trsn_lst().
['t_start','t_eval','t_select_1','t_select_2','t_select_3','t_select_4',
 't_exec_1','t_exec_2','t_exec_3','t_exec_4',
 't_finish_1','t_finish_2','t_finish_3','t_finish_4',
 't_sync','t_complete']
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_start',
        't_eval',
        't_select_1',
        't_select_2',
        't_select_3',
        't_select_4',
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
-spec init_marking(Place :: atom(), UsrInfo :: multiple_choice_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> multiple_choice:preset('t_start').
['p_start']
> multiple_choice:preset('t_eval').
['p_eval']
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_start') -> ['p_start'];
preset('t_eval') -> ['p_eval'];
preset('t_select_1') -> ['p_branch1'];
preset('t_select_2') -> ['p_branch2'];
preset('t_select_3') -> ['p_branch3'];
preset('t_select_4') -> ['p_branch4'];
preset('t_exec_1') -> ['p_active_1'];
preset('t_exec_2') -> ['p_active_2'];
preset('t_exec_3') -> ['p_active_3'];
preset('t_exec_4') -> ['p_active_4'];
preset('t_finish_1') -> ['p_active_1'];
preset('t_finish_2') -> ['p_active_2'];
preset('t_finish_3') -> ['p_active_3'];
preset('t_finish_4') -> ['p_active_4'];
preset('t_sync') -> ['p_sync'];
preset('t_complete') -> ['p_complete'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: multiple_choice_state()) ->
          boolean().

is_enabled('t_start', _Mode, _UsrInfo) ->
    true;
is_enabled('t_eval', #{'p_eval' := [evaluate]}, _UsrInfo) ->
    true;
is_enabled('t_select_1', #{'p_branch1' := [selected]}, _UsrInfo) ->
    true;
is_enabled('t_select_2', #{'p_branch2' := [selected]}, _UsrInfo) ->
    true;
is_enabled('t_select_3', #{'p_branch3' := [selected]}, _UsrInfo) ->
    true;
is_enabled('t_select_4', #{'p_branch4' := [selected]}, _UsrInfo) ->
    true;
is_enabled('t_exec_1', #{'p_active_1' := [{branch, branch1}]}, _UsrInfo) ->
    true;
is_enabled('t_exec_2', #{'p_active_2' := [{branch, branch2}]}, _UsrInfo) ->
    true;
is_enabled('t_exec_3', #{'p_active_3' := [{branch, branch3}]}, _UsrInfo) ->
    true;
is_enabled('t_exec_4', #{'p_active_4' := [{branch, branch4}]}, _UsrInfo) ->
    true;
is_enabled('t_finish_1', #{'p_active_1' := [{branch, branch1}]}, _UsrInfo) ->
    true;
is_enabled('t_finish_2', #{'p_active_2' := [{branch, branch2}]}, _UsrInfo) ->
    true;
is_enabled('t_finish_3', #{'p_active_3' := [{branch, branch3}]}, _UsrInfo) ->
    true;
is_enabled('t_finish_4', #{'p_active_4' := [{branch, branch4}]}, _UsrInfo) ->
    true;
is_enabled('t_sync', #{'p_sync' := Tokens}, #multiple_choice_state{selected = Selected, completed = Completed}) ->
    length(Tokens) =:= length(Selected) andalso length(Completed) =:= length(Selected);
is_enabled('t_complete', #{'p_complete' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: multiple_choice_state()) ->
          {produce, map()} | {produce, map(), multiple_choice_state()} | abort.

fire('t_start', #{'p_start' := [start]}, State) ->
    log_event(State, <<"MultipleChoice">>, <<"Start">>, #{}),
    {produce, #{
        'p_start' => [],
        'p_eval' => [evaluate]
    }, State};

fire('t_eval', #{'p_eval' := [evaluate]}, #multiple_choice_state{branches = Branches, input_data = InputData} = State) ->
    %% Evaluate branch conditions
    Selected = lists:filter(fun(BranchName) ->
        case maps:get(BranchName, Branches) of
            {ConditionFun, _BranchFun} when is_function(ConditionFun) ->
                case ConditionFun(InputData) of
                    true -> true;
                    _ -> false
                end;
            _ ->
                false
        end
    end, maps:keys(Branches)),

    log_event(State, <<"MultipleChoice">>, <<"Evaluated">>, #{<<"selected">> => Selected}),

    %% Create tokens for selected branches
    ProduceMap = lists:foldl(fun(BranchName, Acc) ->
        PlaceIndex = get_branch_place_index(BranchName),
        PlaceName = list_to_atom("p_branch" ++ integer_to_list(PlaceIndex)),
        maps:put(PlaceName, [selected], Acc)
    end, #{'p_eval' => []}, Selected),

    {produce, ProduceMap, State#multiple_choice_state{selected = Selected}};

fire('t_select_1', #{'p_branch1' := [selected]}, State) ->
    enter_branch(1, State, #{'p_branch1' => []});

fire('t_select_2', #{'p_branch2' := [selected]}, State) ->
    enter_branch(2, State, #{'p_branch2' => []});

fire('t_select_3', #{'p_branch3' := [selected]}, State) ->
    enter_branch(3, State, #{'p_branch3' => []});

fire('t_select_4', #{'p_branch4' := [selected]}, State) ->
    enter_branch(4, State, #{'p_branch4' => []});

fire('t_exec_1', #{'p_active_1' := [{branch, branch1}]}, State) ->
    execute_branch(1, branch1, State);

fire('t_exec_2', #{'p_active_2' := [{branch, branch2}]}, State) ->
    execute_branch(2, branch2, State);

fire('t_exec_3', #{'p_active_3' := [{branch, branch3}]}, State) ->
    execute_branch(3, branch3, State);

fire('t_exec_4', #{'p_active_4' := [{branch, branch4}]}, State) ->
    execute_branch(4, branch4, State);

fire('t_finish_1', #{'p_active_1' := [{branch, branch1}]}, State) ->
    finish_branch(1, branch1, State, #{'p_active_1' => []});

fire('t_finish_2', #{'p_active_2' := [{branch, branch2}]}, State) ->
    finish_branch(2, branch2, State, #{'p_active_2' => []});

fire('t_finish_3', #{'p_active_3' := [{branch, branch3}]}, State) ->
    finish_branch(3, branch3, State, #{'p_active_3' => []});

fire('t_finish_4', #{'p_active_4' := [{branch, branch4}]}, State) ->
    finish_branch(4, branch4, State, #{'p_active_4' => []});

fire('t_sync', #{'p_sync' := _Tokens}, #multiple_choice_state{results = Results, selected = Selected} = State) ->
    log_event(State, <<"MultipleChoice">>, <<"Sync">>, #{<<"selected_count">> => length(Selected)}),
    {produce, #{
        'p_sync' => [],
        'p_complete' => [{complete, Results}]
    }, State};

fire('t_complete', #{'p_complete' := [{complete, Results}]}, State) ->
    log_event(State, <<"MultipleChoice">>, <<"Complete">>, #{<<"results">> => Results}),
    {produce, #{
        'p_complete' => [],
        'p_complete' => [{complete, Results}]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: multiple_choice_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: multiple_choice_state()) ->
          {ok, multiple_choice_state()}.

init(MultipleChoiceState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"MultipleChoice">>}) of
        {ok, LogId} ->
            State1 = MultipleChoiceState#multiple_choice_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, MultipleChoiceState}
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
        #multiple_choice_state{} = State ->
            NewState = State#multiple_choice_state{input_data = InputData},
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
        #multiple_choice_state{log_id = LogId} when LogId =/= undefined ->
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
%% @doc Gets the place index for a branch name.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec get_branch_place_index(BranchName :: atom()) -> pos_integer().

get_branch_place_index(branch1) -> 1;
get_branch_place_index(branch2) -> 2;
get_branch_place_index(branch3) -> 3;
get_branch_place_index(branch4) -> 4;
get_branch_place_index(_) -> 1.

%%--------------------------------------------------------------------
%% @doc Enters a branch for execution.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec enter_branch(Index :: pos_integer(), State :: multiple_choice_state(), BaseMap :: map()) ->
          {produce, map(), multiple_choice_state()}.

enter_branch(Index, State, BaseMap) ->
    ActivePlace = list_to_atom("p_active_" ++ integer_to_list(Index)),
    BranchName = list_to_atom("branch" ++ integer_to_list(Index)),
    {produce, maps:put(ActivePlace, [{branch, BranchName}], BaseMap), State}.

%%--------------------------------------------------------------------
%% @doc Executes a branch.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec execute_branch(Index :: pos_integer(), BranchName :: atom(), State :: multiple_choice_state()) ->
          {produce, map(), multiple_choice_state()}.

execute_branch(_Index, BranchName, #multiple_choice_state{branches = Branches, input_data = InputData} = State) ->
    Result = case maps:get(BranchName, Branches, {undefined, undefined}) of
        {_ConditionFun, BranchFun} when is_function(BranchFun) ->
            try BranchFun(InputData)
            catch _:_ -> {error, branch_failed} end;
        _ ->
            {error, no_branch_fun}
    end,
    log_event(State, <<"MultipleChoice">>, <<"BranchExec">>, #{<<"branch">> => BranchName, <<"result">> => Result}),
    {produce, #{}, State}.

%%--------------------------------------------------------------------
%% @doc Finishes a branch execution.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec finish_branch(Index :: pos_integer(), BranchName :: atom(), State :: multiple_choice_state(), BaseMap :: map()) ->
          {produce, map(), multiple_choice_state()}.

finish_branch(Index, BranchName, #multiple_choice_state{completed = Completed, results = Results, branches = Branches, input_data = InputData} = State, BaseMap) ->
    DonePlace = list_to_atom("p_done_" ++ integer_to_list(Index)),

    %% Get the result from branch execution
    Result = case maps:get(BranchName, Branches, {undefined, undefined}) of
        {_ConditionFun, BranchFun} when is_function(BranchFun) ->
            try BranchFun(InputData)
            catch _:_ -> {error, branch_failed} end;
        _ ->
            {error, no_branch_fun}
    end,

    NewCompleted = [BranchName | Completed],
    NewResults = maps:put(BranchName, Result, Results),
    NewState = State#multiple_choice_state{completed = NewCompleted, results = NewResults},

    %% Add sync tokens when all selected branches are done
    NewBaseMap = case length(NewCompleted) =:= length(State#multiple_choice_state.selected) of
        true ->
            SyncTokens = [{branch_done, B} || B <- lists:sort(NewCompleted)],
            maps:put('p_sync', SyncTokens, maps:put(DonePlace, [done], BaseMap));
        false ->
            maps:put(DonePlace, [done], BaseMap)
    end,

    log_event(State, <<"MultipleChoice">>, <<"BranchDone">>, #{<<"branch">> => BranchName}),

    {produce, NewBaseMap, NewState}.

%%--------------------------------------------------------------------
%% @doc Waits for workflow completion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_for_completion(Pid :: pid(), Timeout :: timeout()) ->
          {ok, #{atom() => term()}} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_complete', Ref},
    receive
        {trigger, 'p_complete', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #multiple_choice_state{results = Results} ->
                            {ok, Results};
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
%% @doc Waits for all branches to complete.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_all_branches(Ref :: reference(), Pids :: [pid()], Remaining :: pos_integer(),
                        Timeout :: timeout(), Acc :: map()) ->
          {ok, map()} | {error, term()}.

wait_all_branches(_Ref, _Pids, 0, _Timeout, Acc) ->
    {ok, Acc};
wait_all_branches(Ref, Pids, Remaining, Timeout, Acc) ->
    receive
        {Ref, {branch_complete, BranchName}, Result} ->
            wait_all_branches(Ref, Pids, Remaining - 1, Timeout, maps:put(BranchName, Result, Acc));
        {Ref, {branch_error, BranchName}, {Error, Reason, _Stack}} ->
            {error, {branch_error, BranchName, Error, Reason}}
    after Timeout ->
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
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
    <<"multiple_choice_", Hex/binary>>.

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
-spec log_event(State :: multiple_choice_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#multiple_choice_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
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

%% Test execute/2 with multiple branches selected
execute_multiple_test() ->
    Options = #{
        branch1 => {fun(X) -> X > 0 end, fun(X) -> X * 2 end},
        branch2 => {fun(X) -> X < 10 end, fun(X) -> X + 1 end}
    },
    ?assertMatch({ok, #{branch1 := 10, branch2 := 6}}, execute(Options, 5)).

%% Test execute/2 with single branch selected
execute_single_test() ->
    Options = #{
        branch1 => {fun(X) -> X > 100 end, fun(X) -> X * 2 end},
        branch2 => {fun(X) -> X < 10 end, fun(X) -> X + 1 end}
    },
    ?assertMatch({ok, #{branch2 := 6}}, execute(Options, 5)).

%% Test execute/2 with no branches selected
execute_none_test() ->
    Options = #{
        branch1 => {fun(X) -> X > 100 end, fun(X) -> X * 2 end}
    },
    ?assertMatch({ok, #{}}, execute(Options, 5)).

-endif.
