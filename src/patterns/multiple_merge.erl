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

-module(multiple_merge).
-moduledoc """
Multiple Merge Pattern (WCP-07) for YAWL.

This module implements the Multiple Merge pattern as a gen_pnet behaviour.

## Pattern Description

The Multiple Merge pattern (WCP-07) merges multiple concurrent paths
without requiring all paths to complete. Unlike the Discriminator (WCP-09)
which triggers only once, Multiple Merge allows multiple triggers while
maintaining merge semantics.

## Petri Net Structure

```
Places:
  p_input        - Initial input place
  p_path_pool    - Pool of active path tokens
  p_merge_pending- Pending merge requests
  p_merge_active - Active merge state
  p_output       - Output place (can have multiple tokens)

Transitions:
  t_split          - Split into paths
  t_complete_path  - Complete a path
  t_merge          - Merge completed path
  t_forward        - Forward merged result
```

## Soundness Properties

- **Option to complete**: Always true (no cycles)
- **Proper completion**: One output token per path completion
- **No dead transitions**: All transitions fire appropriately

## Examples

Get the list of places in the Petri net:

```erlang
> multiple_merge:place_lst().
['p_input','p_path_pool','p_merge_pending','p_merge_active','p_output']
```

Get the list of transitions in the Petri net:

```erlang
> multiple_merge:trsn_lst().
['t_split','t_complete_path','t_merge','t_forward']
```

Get the preset (input places) for a transition:

```erlang
> multiple_merge:preset('t_split').
['p_input']

> multiple_merge:preset('t_merge').
['p_merge_pending']

> multiple_merge:preset('t_forward').
['p_merge_active']

> multiple_merge:preset(unknown).
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
    start/1,
    run/1,
    get_state/1,
    execute/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(multiple_merge_state, {
    path_count :: pos_integer(),
    path_funs :: [function()],
    completed = [] :: [pos_integer()],
    output_count = 0 :: non_neg_integer(),
    log_id :: binary() | undefined
}).

-type multiple_merge_state() :: #multiple_merge_state{}.
-export_type([multiple_merge_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-doc """
Creates a new Multiple Merge pattern state.

Parameters:
- PathFuns: List of functions to execute for each path
- PathCount: Number of paths (must match length of PathFuns and be >= 2)

Returns a new multiple_merge_state record with the given path functions.
""".
-spec new(PathFuns :: [function()], PathCount :: pos_integer()) ->
          multiple_merge_state().

new(PathFuns, PathCount) when is_list(PathFuns),
                                length(PathFuns) =:= PathCount,
                                PathCount >= 2 ->
    LogId = generate_log_id(),
    #multiple_merge_state{
        path_count = PathCount,
        path_funs = PathFuns,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Multiple Merge workflow as a gen_pnet process.
%%
%% @param PathFuns List of functions to execute for each path.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(PathFuns :: [function()]) ->
          {ok, pid()} | {error, term()}.

start(PathFuns) when is_list(PathFuns), length(PathFuns) >= 2 ->
    PathCount = length(PathFuns),
    MergeState = new(PathFuns, PathCount),
    gen_yawl:start_link(?MODULE, MergeState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Multiple Merge workflow synchronously.
%%
%% @param PathFuns List of functions to execute for each path.
%% @return {ok, Results} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec run(PathFuns :: [function()]) ->
          {ok, [term()]} | {error, term()}.

run(PathFuns) when is_list(PathFuns), length(PathFuns) >= 2 ->
    case start(PathFuns) of
        {ok, Pid} ->
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
%% @doc Gets the current state of the Multiple Merge workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, multiple_merge_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes the Multiple Merge pattern with given input data.
%%
%% @param PathFuns List of functions to execute for each path.
%% @param InputData Input data to pass to each path.
%% @return {ok, Results} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(PathFuns :: [function()], InputData :: term()) ->
          {ok, [term()]} | {error, term()}.

execute(PathFuns, InputData) when is_list(PathFuns), length(PathFuns) >= 2 ->
    PathCount = length(PathFuns),
    Ref = make_ref(),
    Parent = self(),

    %% Spawn all paths
    Pids = lists:map(fun({Fun, Index}) ->
        spawn(fun() ->
            try
                Result = Fun(InputData),
                Parent ! {Ref, {path_complete, Index}, Result}
            catch
                Error:Reason:Stack ->
                    Parent ! {Ref, {path_error, Index}, {Error, Reason, Stack}}
            end
        end)
    end, lists:zip(PathFuns, lists:seq(1, PathCount))),

    %% Collect all results (multiple merge - all results are collected)
    collect_all_results(Ref, Pids, PathCount, 30000, []).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

-doc """
Returns the list of places for the Multiple Merge Petri net.

## Examples

```erlang
> multiple_merge:place_lst().
['p_input','p_path_pool','p_merge_pending','p_merge_active','p_output']
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_input',
        'p_path_pool',
        'p_merge_pending',
        'p_merge_active',
        'p_output'
    ].

-doc """
Returns the list of transitions for the Multiple Merge Petri net.

## Examples

```erlang
> multiple_merge:trsn_lst().
['t_split','t_complete_path','t_merge','t_forward']
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_split',
        't_complete_path',
        't_merge',
        't_forward'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: multiple_merge_state()) ->
          [term()].

init_marking('p_input', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

-doc """
Returns the preset (input places) for each transition.

## Examples

```erlang
> multiple_merge:preset('t_split').
['p_input']

> multiple_merge:preset('t_merge').
['p_merge_pending']

> multiple_merge:preset('t_forward').
['p_merge_active']

> multiple_merge:preset(unknown).
[]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_split') -> ['p_input'];
preset('t_complete_path') -> ['p_path_pool'];
preset('t_merge') -> ['p_merge_pending'];
preset('t_forward') -> ['p_merge_active'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: multiple_merge_state()) ->
          boolean().

is_enabled('t_split', _Mode, _UsrInfo) ->
    true;
is_enabled('t_complete_path', #{'p_path_pool' := Tokens}, _UsrInfo) ->
    length(Tokens) > 0;
is_enabled('t_merge', #{'p_merge_pending' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_forward', #{'p_merge_active' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: multiple_merge_state()) ->
          {produce, map()} | {produce, map(), multiple_merge_state()} | abort.

fire('t_split', #{'p_input' := [start]}, #multiple_merge_state{path_count = Count, path_funs = Funs} = State) ->
    %% Create path tokens
    PathTokens = [{{path, I}, Fun} || {I, Fun} <- lists:zip(lists:seq(1, Count), Funs)],
    log_event(State, <<"MultipleMerge">>, <<"Split">>, #{<<"path_count">> => Count}),
    {produce, #{
        'p_path_pool' => PathTokens
    }, State};

fire('t_complete_path', #{'p_path_pool' := [Token | Rest]}, State) ->
    %% Complete a path
    case Token of
        {{path, Index}, _Fun} ->
            NewState = State#multiple_merge_state{completed = [Index | State#multiple_merge_state.completed]},
            log_event(State, <<"MultipleMerge">>, <<"PathComplete">>, #{<<"path">> => Index}),
            {produce, #{
                'p_path_pool' => Rest,
                'p_merge_pending' => [{path_complete, Index}]
            }, NewState}
    end;

fire('t_merge', #{'p_merge_pending' := [{path_complete, Index}]}, State) ->
    %% Merge this path
    NewState = State#multiple_merge_state{output_count = State#multiple_merge_state.output_count + 1},
    log_event(State, <<"MultipleMerge">>, <<"Merged">>, #{<<"path">> => Index}),
    {produce, #{
        'p_merge_pending' => [],
        'p_merge_active' => [{merged, Index}]
    }, NewState};

fire('t_forward', #{'p_merge_active' := [{merged, Index}]}, State) ->
    %% Forward the merged result
    OutputToken = {output, Index},
    log_event(State, <<"MultipleMerge">>, <<"Output">>, #{<<"path">> => Index}),
    {produce, #{
        'p_merge_active' => [],
        'p_output' => [OutputToken]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: multiple_merge_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: multiple_merge_state()) ->
          {ok, multiple_merge_state()}.

init(MultipleMergeState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"MultipleMerge">>}) of
        {ok, LogId} ->
            State1 = MultipleMergeState#multiple_merge_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, MultipleMergeState}
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
        #multiple_merge_state{log_id = LogId} when LogId =/= undefined ->
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
%% @doc Waits for workflow completion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_for_completion(Pid :: pid(), Timeout :: timeout()) ->
          {ok, [term()]} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_output', Ref},
    receive
        {trigger, 'p_output', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #multiple_merge_state{completed = Completed} ->
                            {ok, lists:reverse(Completed)};
                        _ ->
                            {ok, []}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    after Timeout ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Collects all results from spawned processes.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec collect_all_results(Ref :: reference(), Pids :: [pid()], Count :: pos_integer(),
                          Timeout :: timeout(), Acc :: [term()]) ->
          {ok, [term()]} | {error, term()}.

collect_all_results(_Ref, _Pids, 0, _Timeout, Acc) ->
    {ok, lists:reverse(Acc)};
collect_all_results(Ref, Pids, Count, Timeout, Acc) ->
    receive
        {Ref, {path_complete, _Index}, Result} ->
            collect_all_results(Ref, Pids, Count - 1, Timeout, [Result | Acc]);
        {Ref, {path_error, Index}, {Error, Reason, _Stack}} ->
            {error, {path_error, Index, Error, Reason}}
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
    <<"multiple_merge_", Hex/binary>>.

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
-spec log_event(State :: multiple_merge_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#multiple_merge_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Doctest test runner
doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%% Unit tests for core pure functions

place_lst_test() ->
    Expected = ['p_input', 'p_path_pool', 'p_merge_pending',
                'p_merge_active', 'p_output'],
    ?assertEqual(Expected, place_lst()).

trsn_lst_test() ->
    Expected = ['t_split', 't_complete_path', 't_merge', 't_forward'],
    ?assertEqual(Expected, trsn_lst()).

preset_t_split_test() ->
    ?assertEqual(['p_input'], preset('t_split')).

preset_t_complete_path_test() ->
    ?assertEqual(['p_path_pool'], preset('t_complete_path')).

preset_t_merge_test() ->
    ?assertEqual(['p_merge_pending'], preset('t_merge')).

preset_t_forward_test() ->
    ?assertEqual(['p_merge_active'], preset('t_forward')).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

new_state_test() ->
    Fun1 = fun(X) -> X * 2 end,
    Fun2 = fun(X) -> X + 10 end,
    State = new([Fun1, Fun2], 2),
    ?assertEqual(2, State#multiple_merge_state.path_count),
    ?assertEqual(2, length(State#multiple_merge_state.path_funs)),
    ?assertEqual([], State#multiple_merge_state.completed),
    ?assertEqual(0, State#multiple_merge_state.output_count).

init_marking_p_input_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    ?assertEqual([start], init_marking('p_input', State)).

init_marking_other_place_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    ?assertEqual([], init_marking('p_output', State)),
    ?assertEqual([], init_marking('p_path_pool', State)).

-endif.
