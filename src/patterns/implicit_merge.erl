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

-module(implicit_merge).
-behaviour(gen_yawl).

-moduledoc """
Implicit Merge Pattern (WCP-06) for YAWL.

This module implements the Implicit Merge pattern as a gen_yawl behaviour.

<h3>Pattern Description</h3>
The Implicit Merge pattern (WCP-06) merges multiple concurrent branches
without requiring explicit synchronization. The merge occurs when the
first branch completes, and subsequent branches are consumed without
triggering additional forward flow.

<h3>Petri Net Structure</h3>
<pre>
  Places:
    p_input        - Initial input place
    p_branch_pool  - Branch execution token pool
    p_merge_ready  - Merge is ready to receive
    p_merge_done   - Merge completed
    p_output       - Final output place

  Transitions:
    t_split              - Split into branches
    t_complete_branch    - Complete a branch
    t_merge_trigger      - Trigger on first completion
    t_consume_remaining  - Consume remaining completions
    t_complete           - Final completion
</pre>

<h3>Soundness Properties</h3>
<ul>
  <li><b>Option to complete:</b> Always true (no cycles)</li>
  <li><b>Proper completion:</b> Exactly one output token per N input tokens</li>
  <li><b>No dead transitions:</b> All transitions fire exactly once per branch</li>
</ul>

```erlang
%% Create a new implicit merge state with 2 branches
> State = implicit_merge:new([fun() -> ok end, fun() -> result end], 2).
{implicit_merge_state,2,[#Fun<...>,#Fun<...>],[],undefined,<<"implicit_merge_",...>>}

%% Get the list of places
> implicit_merge:place_lst().
[p_input,p_branch_pool,p_merge_ready,p_merge_done,p_output]

%% Get the list of transitions
> implicit_merge:trsn_lst().
[t_split,t_complete_branch,t_merge_trigger,t_consume_remaining,t_complete]

%% Get preset for a transition
> implicit_merge:preset('t_split').
[p_input]

> implicit_merge:preset('t_complete').
[p_merge_done]
```
""".
%% -------------------------------------------------------------------

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

-record(implicit_merge_state, {
    branch_count :: pos_integer(),
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    triggered_by :: undefined | pos_integer(),
    log_id :: binary() | undefined
}).

-type implicit_merge_state() :: #implicit_merge_state{}.
-export_type([implicit_merge_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-doc """
Creates a new Implicit Merge pattern state.

```erlang
> State = implicit_merge:new([fun() -> a end, fun() -> b end], 2).
{implicit_merge_state,2,[#Fun<...>,#Fun<...>],[],undefined,<<"implicit_merge_",...>>}
```
""".
-spec new(BranchFuns :: [function()], BranchCount :: pos_integer()) ->
          implicit_merge_state().

new(BranchFuns, BranchCount) when is_list(BranchFuns),
                                  length(BranchFuns) =:= BranchCount,
                                  BranchCount >= 2 ->
    LogId = generate_log_id(),
    #implicit_merge_state{
        branch_count = BranchCount,
        branch_funs = BranchFuns,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Implicit Merge workflow as a gen_yawl process.
%%
%% @param BranchFuns List of functions to execute for each branch.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(BranchFuns :: [function()]) ->
          {ok, pid()} | {error, term()}.

start(BranchFuns) when is_list(BranchFuns), length(BranchFuns) >= 2 ->
    BranchCount = length(BranchFuns),
    MergeState = new(BranchFuns, BranchCount),
    gen_yawl:start_link(?MODULE, MergeState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Implicit Merge workflow synchronously.
%%
%% @param BranchFuns List of functions to execute for each branch.
%% @return {ok, Result} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec run(BranchFuns :: [function()]) ->
          {ok, term()} | {error, term()}.

run(BranchFuns) when is_list(BranchFuns), length(BranchFuns) >= 2 ->
    case start(BranchFuns) of
        {ok, Pid} ->
            case wait_for_completion(Pid, 30000) of
                {ok, Result} ->
                    gen_yawl:stop(Pid),
                    {ok, Result};
                {error, Reason} ->
                    gen_yawl:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Implicit Merge workflow.
%%
%% @param Pid The pid of the gen_yawl process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, implicit_merge_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes the Implicit Merge pattern with given input data.
%%
%% @param BranchFuns List of functions to execute for each branch.
%% @param InputData Input data to pass to each branch.
%% @return {ok, Result} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(BranchFuns :: [function()], InputData :: term()) ->
          {ok, term()} | {error, term()}.

execute(BranchFuns, InputData) when is_list(BranchFuns), length(BranchFuns) >= 2 ->
    BranchCount = length(BranchFuns),
    Ref = make_ref(),
    Parent = self(),

    %% Spawn all branches
    Pids = lists:map(fun({Fun, Index}) ->
        spawn(fun() ->
            try
                Result = Fun(InputData),
                Parent ! {Ref, {branch_complete, Index}, Result}
            catch
                Error:Reason:Stack ->
                    Parent ! {Ref, {branch_error, Index}, {Error, Reason, Stack}}
            end
        end)
    end, lists:zip(BranchFuns, lists:seq(1, BranchCount))),

    %% Wait for first completion (implicit merge)
    case wait_for_first_completion(Ref, Pids, 30000) of
        {ok, {_Index, Result}} ->
            %% Consume remaining results without processing
            consume_remaining(Ref, Pids, 5000),
            {ok, Result};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

-doc """
Returns the list of places for the Implicit Merge Petri net.

```erlang
> implicit_merge:place_lst().
[p_input,p_branch_pool,p_merge_ready,p_merge_done,p_output]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_input',
        'p_branch_pool',
        'p_merge_ready',
        'p_merge_done',
        'p_output'
    ].

-doc """
Returns the list of transitions for the Implicit Merge Petri net.

```erlang
> implicit_merge:trsn_lst().
[t_split,t_complete_branch,t_merge_trigger,t_consume_remaining,t_complete]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_split',
        't_complete_branch',
        't_merge_trigger',
        't_consume_remaining',
        't_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: implicit_merge_state()) ->
          [term()].

init_marking('p_input', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

-doc """
Returns the preset (input places) for each transition.

```erlang
> implicit_merge:preset('t_split').
[p_input]

> implicit_merge:preset('t_merge_trigger').
[p_merge_ready]

> implicit_merge:preset(unknown).
[]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_split') -> ['p_input'];
preset('t_complete_branch') -> ['p_branch_pool'];
preset('t_merge_trigger') -> ['p_merge_ready'];
preset('t_consume_remaining') -> ['p_merge_done'];
preset('t_complete') -> ['p_merge_done'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled in the given mode.
%%
%% @param Trsn The transition to check.
%% @param Mode The execution mode map.
%% @param UsrInfo The implicit_merge_state.
%% @return true if enabled, false otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: implicit_merge_state()) ->
          boolean().

is_enabled('t_split', _Mode, _UsrInfo) ->
    true;
is_enabled('t_complete_branch', #{'p_branch_pool' := Tokens}, _UsrInfo) ->
    length(Tokens) > 0;
is_enabled('t_merge_trigger', #{'p_merge_ready' := [_]}, #implicit_merge_state{triggered_by = undefined}) ->
    true;
is_enabled('t_consume_remaining', #{'p_merge_done' := [_]}, #implicit_merge_state{triggered_by = TriggeredBy}) when TriggeredBy =/= undefined ->
    true;
is_enabled('t_complete', #{'p_merge_done' := [_]}, #implicit_merge_state{completed = Completed, branch_count = Count}) when length(Completed) =:= Count ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%%
%% @param Trsn The transition to fire.
%% @param Mode The current mode (marking).
%% @param UsrInfo The implicit_merge_state.
%% @return {produce, NewMode} | {produce, NewMode, NewUsrInfo} | abort.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: implicit_merge_state()) ->
          {produce, map()} | {produce, map(), implicit_merge_state()} | abort.

fire('t_split', #{'p_input' := [start]}, #implicit_merge_state{branch_count = Count, branch_funs = Funs} = State) ->
    %% Create branch tokens
    BranchTokens = [{{branch, I}, Fun} || {I, Fun} <- lists:zip(lists:seq(1, Count), Funs)],
    log_event(State, <<"ImplicitMerge">>, <<"Split">>, #{<<"branch_count">> => Count}),
    {produce, #{
        'p_branch_pool' => BranchTokens
    }, State};

fire('t_complete_branch', #{'p_branch_pool' := [Token | Rest]}, State) ->
    %% Complete a branch
    case Token of
        {{branch, Index}, _Fun} ->
            NewState = State#implicit_merge_state{completed = [Index | State#implicit_merge_state.completed]},
            log_event(State, <<"ImplicitMerge">>, <<"BranchComplete">>, #{<<"branch">> => Index}),
            case State#implicit_merge_state.triggered_by of
                undefined ->
                    %% First completion - trigger merge
                    {produce, #{
                        'p_branch_pool' => Rest,
                        'p_merge_ready' => [first_complete, Index]
                    }, NewState#implicit_merge_state{triggered_by = Index}};
                _ ->
                    %% Subsequent completion - just add to pool
                    {produce, #{
                        'p_branch_pool' => Rest,
                        'p_merge_ready' => [subsequent_complete, Index]
                    }, NewState}
            end
    end;

fire('t_merge_trigger', #{'p_merge_ready' := [first_complete, Index]}, State) ->
    %% Trigger on first completion
    log_event(State, <<"ImplicitMerge">>, <<"MergeTriggered">>, #{<<"triggered_by">> => Index}),
    {produce, #{
        'p_merge_ready' => [],
        'p_merge_done' => [merged, Index]
    }, State};

fire('t_consume_remaining', #{'p_merge_ready' := Remaining, 'p_merge_done' := [merged, FirstIndex]}, State) ->
    %% Consume remaining completions
    Completions = [Index || [subsequent_complete, Index] <- Remaining],
    log_event(State, <<"ImplicitMerge">>, <<"ConsumedRemaining">>, #{
        <<"first">> => FirstIndex,
        <<"consumed_count">> => length(Completions)
    }),
    AllCompleted = [FirstIndex | Completions],
    {produce, #{
        'p_merge_ready' => [],
        'p_merge_done' => [all_complete, AllCompleted]
    }, State#implicit_merge_state{completed = AllCompleted}};

fire('t_complete', #{'p_merge_done' := [all_complete, AllIndexes]}, State) ->
    %% Final completion
    log_event(State, <<"ImplicitMerge">>, <<"Complete">>, #{
        <<"all_branches">> => AllIndexes
    }),
    {produce, #{
        'p_merge_done' => [],
        'p_output' => [complete, AllIndexes]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: implicit_merge_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: implicit_merge_state()) ->
          {ok, implicit_merge_state()}.

init(ImplicitMergeState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"ImplicitMerge">>}) of
        {ok, LogId} ->
            State1 = ImplicitMergeState#implicit_merge_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, ImplicitMergeState}
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
        #implicit_merge_state{log_id = LogId} when LogId =/= undefined ->
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
          {ok, term()} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_output', Ref},
    receive
        {trigger, 'p_output', Ref, pass} ->
            %% Check the marking for output
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #implicit_merge_state{completed = Completed} ->
                            {ok, {completed, Completed}};
                        _ ->
                            {ok, completed}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    after Timeout ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Waits for first branch completion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_for_first_completion(Ref :: reference(), Pids :: [pid()], Timeout :: timeout()) ->
          {ok, {pos_integer(), term()}} | {error, term()}.

wait_for_first_completion(Ref, Pids, Timeout) ->
    receive
        {Ref, {branch_complete, Index}, Result} ->
            {ok, {Index, Result}};
        {Ref, {branch_error, Index}, {Error, Reason, _Stack}} ->
            {error, {branch_error, Index, Error, Reason}}
    after Timeout ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Consumes remaining branch results.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec consume_remaining(Ref :: reference(), Pids :: [pid()], Timeout :: timeout()) ->
          ok.

consume_remaining(_Ref, _Pids, Timeout) ->
    %% Drain any remaining messages
    receive
        _ ->
            consume_remaining(_Ref, _Pids, Timeout - 100)
    after Timeout ->
        ok
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
    <<"implicit_merge_", Hex/binary>>.

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
-spec log_event(State :: implicit_merge_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#implicit_merge_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.
-endif.
