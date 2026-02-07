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

-module(simple_merge).
-moduledoc """
Simple Merge Pattern (WCP-05) for YAWL.

This module implements the Simple Merge pattern as a gen_pnet behaviour.

The Simple Merge pattern (WCP-05) merges multiple incoming branches
without requiring synchronization. Unlike Synchronization (WCP-03) which
waits for ALL branches to complete, Simple Merge allows ANY incoming
branch to proceed through the merge point immediately.

This is the XOR join counterpart to Exclusive Choice (WCP-04), providing
a merge point for mutually exclusive branches.

## Petri Net Structure

**Places:**
- `p_start` - Initial input place
- `p_branch_a` - Branch A execution place
- `p_branch_b` - Branch B execution place
- `p_merge_ready` - Merge point is ready to receive
- `p_merged` - Branch has been merged
- `p_end` - Final output place

**Transitions:**
- `t_split_a` - Split into branch A
- `t_split_b` - Split into branch B
- `t_merge_a` - Merge branch A (XOR semantics)
- `t_merge_b` - Merge branch B (XOR semantics)
- `t_finish` - Complete the workflow

## Examples

```erlang
> simple_merge:place_lst().
[p_start,p_branch_a,p_branch_b,p_merge_ready,p_merged,p_end]

> simple_merge:trsn_lst().
[t_split_a,t_split_b,t_merge_a,t_merge_b,t_finish]

> simple_merge:preset(t_split_a).
[p_start]

> simple_merge:preset(t_merge_a).
[p_branch_a,p_merge_ready]

> simple_merge:preset(t_finish).
[p_merged]
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
    start_link/1,
    run/2,
    get_state/1,
    execute_branch/3,
    execute/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(simple_merge_state, {
    branch_a_fun :: function(),
    branch_b_fun :: function(),
    merged_by :: undefined | branch_a | branch_b,
    result :: undefined | term(),
    log_id :: binary() | undefined
}).

-type simple_merge_state() :: #simple_merge_state{}.
-export_type([simple_merge_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-doc """
Creates a new Simple Merge pattern state.

Creates a state record containing the two branch functions. Each function
should take a single input argument and return a result.

The log_id field is automatically generated using crypto:hash/2 for XES
event tracing purposes.
""".
-spec new(BranchAFun :: function(), BranchBFun :: function()) ->
          simple_merge_state().

new(BranchAFun, BranchBFun) when is_function(BranchAFun), is_function(BranchBFun) ->
    LogId = generate_log_id(),
    #simple_merge_state{
        branch_a_fun = BranchAFun,
        branch_b_fun = BranchBFun,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Simple Merge workflow as a gen_pnet process.
%%
%% @param SimpleMergeState A simple_merge_state record from new/2.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(SimpleMergeState :: simple_merge_state()) ->
          {ok, pid()} | {error, term()}.

start(SimpleMergeState) ->
    gen_yawl:start(?MODULE, SimpleMergeState, []).

%%--------------------------------------------------------------------
%% @doc Starts the Simple Merge workflow as a gen_pnet process (linked).
%%
%% @param SimpleMergeState A simple_merge_state record from new/2.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(SimpleMergeState :: simple_merge_state()) ->
          {ok, pid()} | {error, term()}.

start_link(SimpleMergeState) ->
    gen_yawl:start_link(?MODULE, SimpleMergeState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Simple Merge workflow synchronously.
%%
%% @param BranchAFun Function for branch A.
%% @param BranchBFun Function for branch B.
%% @return {ok, Result} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec run(BranchAFun :: function(), BranchBFun :: function()) ->
          {ok, term()} | {error, term()}.

run(BranchAFun, BranchBFun) ->
    MergeState = new(BranchAFun, BranchBFun),
    case start_link(MergeState) of
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
%% @doc Gets the current state of the Simple Merge workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, simple_merge_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes a specific branch with input data.
%%
%% @param Pid The pid of the gen_pnet process.
%% @param Branch Which branch to execute (branch_a or branch_b).
%% @param InputData Input data to pass to the branch.
%% @return {ok, Result} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_branch(Pid :: pid(), Branch :: branch_a | branch_b, InputData :: term()) ->
          {ok, term()} | {error, term()}.

execute_branch(Pid, Branch, InputData) ->
    gen_yawl:call(Pid, {execute_branch, Branch, InputData}).

%%--------------------------------------------------------------------
%% @doc Executes the Simple Merge pattern by selecting a branch based on condition.
%%
%% @param SimpleMergeState A simple_merge_state record.
%% @param {Branch, InputData} Tuple of branch selector and input data.
%% @return {ok, Result} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(SimpleMergeState :: simple_merge_state(),
              {Branch :: branch_a | branch_b, InputData :: term()}) ->
          {ok, term()} | {error, term()}.

execute(SimpleMergeState, {Branch, InputData}) ->
    case start_link(SimpleMergeState) of
        {ok, Pid} ->
            case execute_branch(Pid, Branch, InputData) of
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

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

-doc """
Returns the list of places for the Simple Merge Petri net.

## Examples

```erlang
> simple_merge:place_lst().
[p_start,p_branch_a,p_branch_b,p_merge_ready,p_merged,p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_branch_a',
        'p_branch_b',
        'p_merge_ready',
        'p_merged',
        'p_end'
    ].

-doc """
Returns the list of transitions for the Simple Merge Petri net.

## Examples

```erlang
> simple_merge:trsn_lst().
[t_split_a,t_split_b,t_merge_a,t_merge_b,t_finish]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_split_a',
        't_split_b',
        't_merge_a',
        't_merge_b',
        't_finish'
    ].

-doc """
Returns the initial marking for a given place.

The initial marking provides tokens at startup to begin the workflow.
Only `p_start` and `p_merge_ready` have initial tokens. All other places
return an empty marking.

The second argument (UsrInfo) is a simple_merge_state record, typically
created via new/2, but its contents are not used by init_marking/2.

## Examples

```erlang
> simple_merge:init_marking(p_start, undefined).
[start]

> simple_merge:init_marking(p_merge_ready, undefined).
[ready]

> simple_merge:init_marking(p_branch_a, undefined).
[]

> simple_merge:init_marking(p_end, undefined).
[]
```
""".
-spec init_marking(Place :: atom(), UsrInfo :: simple_merge_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking('p_merge_ready', _UsrInfo) ->
    [ready];
init_marking(_, _UsrInfo) ->
    [].

-doc """
Returns the preset (input places) for each transition.

## Examples

```erlang
> simple_merge:preset(t_split_a).
[p_start]

> simple_merge:preset(t_split_b).
[p_start]

> simple_merge:preset(t_merge_a).
[p_branch_a,p_merge_ready]

> simple_merge:preset(t_merge_b).
[p_branch_b,p_merge_ready]

> simple_merge:preset(t_finish).
[p_merged]

> simple_merge:preset(unknown).
[]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_split_a') -> ['p_start'];
preset('t_split_b') -> ['p_start'];
preset('t_merge_a') -> ['p_branch_a', 'p_merge_ready'];
preset('t_merge_b') -> ['p_branch_b', 'p_merge_ready'];
preset('t_finish') -> ['p_merged'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled in the given mode.
%%
%% @param Trsn The transition to check.
%% @param Mode The execution mode map.
%% @param UsrInfo The simple_merge_state.
%% @return true if enabled, false otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: simple_merge_state()) ->
          boolean().

is_enabled('t_split_a', #{'p_start' := [start]}, #simple_merge_state{merged_by = undefined}) ->
    true;
is_enabled('t_split_b', #{'p_start' := [start]}, #simple_merge_state{merged_by = undefined}) ->
    true;
is_enabled('t_merge_a', #{'p_branch_a' := [_], 'p_merge_ready' := [ready]}, #simple_merge_state{merged_by = undefined}) ->
    true;
is_enabled('t_merge_b', #{'p_branch_b' := [_], 'p_merge_ready' := [ready]}, #simple_merge_state{merged_by = undefined}) ->
    true;
is_enabled('t_finish', #{'p_merged' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%%
%% @param Trsn The transition to fire.
%% @param Mode The current mode (marking).
%% @param UsrInfo The simple_merge_state.
%% @return {produce, NewMode} | {produce, NewMode, NewUsrInfo} | abort.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: simple_merge_state()) ->
          {produce, map()} | {produce, map(), simple_merge_state()} | abort.

fire('t_split_a', #{'p_start' := [start]}, State) ->
    %% Split into branch A
    log_event(State, <<"SimpleMerge">>, <<"SplitToA">>, #{}),
    {produce, #{
        'p_start' => [],
        'p_branch_a' => [{branch_token, branch_a}]
    }, State};

fire('t_split_b', #{'p_start' := [start]}, State) ->
    %% Split into branch B
    log_event(State, <<"SimpleMerge">>, <<"SplitToB">>, #{}),
    {produce, #{
        'p_start' => [],
        'p_branch_b' => [{branch_token, branch_b}]
    }, State};

fire('t_merge_a', #{'p_branch_a' := [{branch_token, branch_a}], 'p_merge_ready' := [ready]}, State) ->
    %% Merge branch A - no waiting for branch B (XOR semantics)
    NewState = State#simple_merge_state{merged_by = branch_a},
    log_event(State, <<"SimpleMerge">>, <<"MergedA">>, #{}),
    {produce, #{
        'p_branch_a' => [],
        'p_merge_ready' => [],
        'p_merged' => [merged, branch_a]
    }, NewState};

fire('t_merge_b', #{'p_branch_b' := [{branch_token, branch_b}], 'p_merge_ready' := [ready]}, State) ->
    %% Merge branch B - no waiting for branch A (XOR semantics)
    NewState = State#simple_merge_state{merged_by = branch_b},
    log_event(State, <<"SimpleMerge">>, <<"MergedB">>, #{}),
    {produce, #{
        'p_branch_b' => [],
        'p_merge_ready' => [],
        'p_merged' => [merged, branch_b]
    }, NewState};

fire('t_finish', #{'p_merged' := [merged, Branch]}, State) ->
    %% Complete the workflow
    Result = case Branch of
        branch_a -> {merged, branch_a};
        branch_b -> {merged, branch_b}
    end,
    NewState = State#simple_merge_state{result = Result},
    log_event(State, <<"SimpleMerge">>, <<"Complete">>, #{<<"branch">> => Branch}),
    {produce, #{
        'p_merged' => [],
        'p_end' => [complete, Result]
    }, NewState};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: simple_merge_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: simple_merge_state()) ->
          {ok, simple_merge_state()}.

init(SimpleMergeState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"SimpleMerge">>}) of
        {ok, LogId} ->
            State1 = SimpleMergeState#simple_merge_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, SimpleMergeState}
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

handle_call({execute_branch, Branch, InputData}, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Result = case Branch of
        branch_a ->
            Fun = UsrInfo#simple_merge_state.branch_a_fun,
            try
                Result1 = Fun(InputData),
                %% Trigger the merge
                gen_yawl:sync(NetState, 1000),
                NewState = UsrInfo#simple_merge_state{
                    merged_by = branch_a,
                    result = {ok, Result1}
                },
                NewUsrInfo = gen_yawl:set_usr_info(NetState, NewState),
                {reply, {ok, Result1}, NewUsrInfo}
            catch
                Error:Reason:Stack ->
                    {reply, {error, {branch_a_error, Error, Reason, Stack}}, NetState}
            end;
        branch_b ->
            Fun = UsrInfo#simple_merge_state.branch_b_fun,
            try
                Result1 = Fun(InputData),
                %% Trigger the merge
                gen_yawl:sync(NetState, 1000),
                NewState = UsrInfo#simple_merge_state{
                    merged_by = branch_b,
                    result = {ok, Result1}
                },
                NewUsrInfo = gen_yawl:set_usr_info(NetState, NewState),
                {reply, {ok, Result1}, NewUsrInfo}
            catch
                Error:Reason:Stack ->
                    {reply, {error, {branch_b_error, Error, Reason, Stack}}, NetState}
            end
    end,
    Result;

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
        #simple_merge_state{log_id = LogId} when LogId =/= undefined ->
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
    Pid ! {trigger, 'p_end', Ref},
    receive
        {trigger, 'p_end', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #simple_merge_state{result = Result} when Result =/= undefined ->
                            {ok, Result};
                        #simple_merge_state{merged_by = MergedBy} when MergedBy =/= undefined ->
                            {ok, {merged, MergedBy}};
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
%% @doc Generates a unique log ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().

generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"simple_merge_", Hex/binary>>.

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
-spec log_event(State :: simple_merge_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#simple_merge_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
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
-endif.
