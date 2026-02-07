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
-module(discriminator).
-behaviour(gen_yawl).

-moduledoc """
Discriminator Pattern (WCP-09) for YAWL.

This module implements the Discriminator pattern as a gen_pnet behaviour.

## Pattern Description

The Discriminator pattern (WCP-09) merges multiple concurrent branches,
triggering on the FIRST completion and accepting remaining branches
without re-triggering. After all branches complete, it resets to allow
another cycle.

## Petri Net Structure

```
Places:
  p_input         - Initial input place
  p_branch_pool   - Pool of branch execution tokens
  p_trigger_ready - Trigger is ready
  p_triggered     - Trigger has fired (first complete)
  p_consume       - Consuming remaining completions
  p_reset         - Reset for next cycle
  p_output        - Output place

Transitions:
  t_split              - Split into branches
  t_complete_branch    - Complete a branch
  t_trigger            - Trigger on first completion
  t_consume_remaining  - Consume remaining completions
  t_reset              - Reset for next cycle
  t_output             - Produce output
```

## Examples

Get the list of places in the discriminator Petri net:

```erlang
> discriminator:place_lst().
['p_input','p_branch_pool','p_trigger_ready','p_triggered',
 'p_consume','p_reset','p_output']
```

Get the list of transitions in the discriminator Petri net:

```erlang
> discriminator:trsn_lst().
['t_split','t_complete_branch','t_trigger','t_consume_remaining',
 't_reset','t_output']
```

Get the preset (input places) for the split transition:

```erlang
> discriminator:preset('t_split').
['p_input']
```

Get the preset for the trigger transition:

```erlang
> discriminator:preset('t_trigger').
['p_trigger_ready']
```

## Soundness Properties

- **Option to complete:** Always true (trigger fires on first completion)
- **Proper completion:** Exactly one output token per N input tokens
- **No dead transitions:** All branches complete and are consumed
""".

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
    execute/2,
    reset/1
]).

%%====================================================================
%% Records
%%====================================================================

-record(discriminator_state, {
    branch_count :: pos_integer(),
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    triggered_by :: undefined | pos_integer(),
    cycle_count = 0 :: non_neg_integer(),
    log_id :: binary() | undefined
}).

-type discriminator_state() :: #discriminator_state{}.
-export_type([discriminator_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-doc """
Creates a new Discriminator pattern state.
""".
-spec new(BranchFuns :: [function()], BranchCount :: pos_integer()) ->
          discriminator_state().

new(BranchFuns, BranchCount) when is_list(BranchFuns),
                                  length(BranchFuns) =:= BranchCount,
                                  BranchCount >= 2 ->
    LogId = generate_log_id(),
    #discriminator_state{
        branch_count = BranchCount,
        branch_funs = BranchFuns,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Discriminator workflow as a gen_yawl process.
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
    DiscriminatorState = new(BranchFuns, BranchCount),
    gen_yawl:start_link(?MODULE, DiscriminatorState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Discriminator workflow synchronously.
%%
%% @param BranchFuns List of functions to execute for each branch.
%% @return {ok, {TriggerBranch, Result}} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec run(BranchFuns :: [function()]) ->
          {ok, {pos_integer(), term()}} | {error, term()}.

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
%% @doc Gets the current state of the Discriminator workflow.
%%
%% @param Pid The pid of the gen_yawl process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, discriminator_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes the Discriminator pattern with given input data.
%%
%% @param BranchFuns List of functions to execute for each branch.
%% @param InputData Input data to pass to each branch.
%% @return {ok, {TriggerBranch, Result}} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(BranchFuns :: [function()], InputData :: term()) ->
          {ok, {pos_integer(), term()}} | {error, term()}.

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

    %% Wait for first completion (discriminator semantics)
    case wait_first_complete(Ref, Pids, 30000) of
        {ok, {Index, Result}} ->
            %% Consume remaining results without triggering
            consume_remaining(Ref, 5000),
            {ok, {Index, Result}};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Resets the discriminator for another cycle.
%%
%% @param Pid The pid of the gen_yawl process.
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(Pid :: pid()) -> ok | {error, term()}.

reset(Pid) ->
    gen_yawl:cast(Pid, reset_discriminator).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

-doc """
Returns the list of places for the Discriminator Petri net.

## Examples

```erlang
> discriminator:place_lst().
['p_input','p_branch_pool','p_trigger_ready','p_triggered',
 'p_consume','p_reset','p_output']
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_input',
        'p_branch_pool',
        'p_trigger_ready',
        'p_triggered',
        'p_consume',
        'p_reset',
        'p_output'
    ].

-doc """
Returns the list of transitions for the Discriminator Petri net.

## Examples

```erlang
> discriminator:trsn_lst().
['t_split','t_complete_branch','t_trigger','t_consume_remaining',
 't_reset','t_output']
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_split',
        't_complete_branch',
        't_trigger',
        't_consume_remaining',
        't_reset',
        't_output'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: discriminator_state()) ->
          [term()].

init_marking('p_input', _UsrInfo) ->
    [start];
init_marking('p_trigger_ready', _UsrInfo) ->
    [ready];
init_marking(_, _UsrInfo) ->
    [].

-doc """
Returns the preset (input places) for each transition.

## Examples

```erlang
> discriminator:preset('t_split').
['p_input']
```

```erlang
> discriminator:preset('t_trigger').
['p_trigger_ready']
```

```erlang
> discriminator:preset('t_reset').
['p_reset']
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_split') -> ['p_input'];
preset('t_complete_branch') -> ['p_branch_pool'];
preset('t_trigger') -> ['p_trigger_ready'];
preset('t_consume_remaining') -> ['p_consume'];
preset('t_reset') -> ['p_reset'];
preset('t_output') -> ['p_triggered'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: discriminator_state()) ->
          boolean().

is_enabled('t_split', _Mode, _UsrInfo) ->
    true;
is_enabled('t_complete_branch', #{'p_branch_pool' := Tokens}, _UsrInfo) ->
    length(Tokens) > 0;
is_enabled('t_trigger', #{'p_trigger_ready' := [ready]}, #discriminator_state{triggered_by = undefined}) ->
    true;
is_enabled('t_consume_remaining', #{'p_branch_pool' := Pool}, #discriminator_state{triggered_by = TriggeredBy, branch_count = Count}) when TriggeredBy =/= undefined ->
    length(Pool) < Count;  %% Some still remaining to consume
is_enabled('t_reset', #{'p_reset' := [reset_req]}, #discriminator_state{completed = Completed, branch_count = Count}) when length(Completed) =:= Count ->
    true;
is_enabled('t_output', #{'p_triggered' := [first_complete, _Index]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: discriminator_state()) ->
          {produce, map()} | abort.

fire('t_split', #{'p_input' := [start]}, #discriminator_state{branch_count = Count, branch_funs = Funs} = State) ->
    %% Create branch tokens
    BranchTokens = [{{branch, I}, Fun} || {I, Fun} <- lists:zip(lists:seq(1, Count), Funs)],
    log_event(State, <<"Discriminator">>, <<"Split">>, #{<<"branch_count">> => Count}),
    {produce, #{
        'p_input' => [],
        'p_branch_pool' => BranchTokens
    }};

fire('t_complete_branch', #{'p_branch_pool' := [Token | Rest]}, #discriminator_state{triggered_by = TriggeredBy} = State) ->
    %% Complete a branch
    case Token of
        {{branch, Index}, _Fun} when TriggeredBy =:= undefined ->
            %% First completion - trigger the discriminator
            log_event(State, <<"Discriminator">>, <<"FirstComplete">>, #{<<"branch">> => Index}),
            {produce, #{
                'p_branch_pool' => Rest,
                'p_trigger_ready' => [],
                'p_triggered' => [first_complete, Index]
            }};
        {{branch, Index}, _Fun} ->
            %% Subsequent completion - just track it
            log_event(State, <<"Discriminator">>, <<"BranchComplete">>, #{<<"branch">> => Index}),
            case length(Rest) of
                0 ->
                    %% All done, request reset
                    {produce, #{
                        'p_branch_pool' => [],
                        'p_consume' => [all_done],
                        'p_reset' => [reset_req]
                    }};
                _ ->
                    {produce, #{
                        'p_branch_pool' => Rest,
                        'p_consume' => [consumed, Index]
                    }}
            end
    end;

fire('t_trigger', #{'p_trigger_ready' := [ready]}, _State) ->
    %% This shouldn't fire directly - triggered by t_complete_branch
    {produce, #{}};

fire('t_consume_remaining', #{'p_consume' := [all_done]}, _State) ->
    %% All branches consumed
    {produce, #{
        'p_consume' => [],
        'p_reset' => [reset_req]
    }};

fire('t_reset', #{'p_reset' := [reset_req]}, #discriminator_state{cycle_count = Cycle} = State) ->
    %% Reset for next cycle
    log_event(State, <<"Discriminator">>, <<"Reset">>, #{<<"cycle">> => Cycle + 1}),
    {produce, #{
        'p_reset' => [],
        'p_trigger_ready' => [ready]
    }};

fire('t_output', #{'p_triggered' := [first_complete, Index]}, State) ->
    %% Produce output
    log_event(State, <<"Discriminator">>, <<"Output">>, #{<<"triggered_by">> => Index}),
    {produce, #{
        'p_triggered' => [],
        'p_output' => [{discriminated, Index}]
    }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: discriminator_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: discriminator_state()) ->
          discriminator_state().

init(DiscriminatorState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"Discriminator">>}) of
        {ok, LogId} ->
            State1 = DiscriminatorState#discriminator_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            State1;
        _ ->
            DiscriminatorState
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term()} | noreply.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {ok, UsrInfo}};
handle_call(_Request, _From, _NetState) ->
    {reply, {error, bad_msg}}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          noreply.

handle_cast(reset_discriminator, _NetState) ->
    noreply;
handle_cast(_Request, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles non-gen_pnet messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), NetState :: term()) ->
          noreply.

handle_info(_Request, _NetState) ->
    noreply.

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
        #discriminator_state{log_id = LogId} when LogId =/= undefined ->
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
          {ok, {pos_integer(), term()}} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_output', Ref},
    receive
        {trigger, 'p_output', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #discriminator_state{triggered_by = TriggeredBy} when TriggeredBy =/= undefined ->
                            {ok, {TriggeredBy, triggered}};
                        _ ->
                            {error, no_trigger}
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
-spec wait_first_complete(Ref :: reference(), Pids :: [pid()], Timeout :: timeout()) ->
          {ok, {pos_integer(), term()}} | {error, term()}.

wait_first_complete(Ref, Pids, Timeout) ->
    receive
        {Ref, {branch_complete, Index}, Result} ->
            {ok, {Index, Result}};
        {Ref, {branch_error, Index}, {Error, Reason, _Stack}} ->
            {error, {branch_error, Index, Error, Reason}}
    after Timeout ->
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Consumes remaining branch results.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec consume_remaining(Ref :: reference(), Timeout :: timeout()) -> ok.

consume_remaining(_Ref, Timeout) when Timeout =< 0 ->
    ok;
consume_remaining(Ref, Timeout) ->
    receive
        {_Ref, _, _} ->
            consume_remaining(Ref, Timeout - 100)
    after 100 ->
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
    <<"discriminator_", Hex/binary>>.

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
-spec log_event(State :: discriminator_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#discriminator_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
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
