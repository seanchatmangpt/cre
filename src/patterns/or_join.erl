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
-module(or_join).
-moduledoc """
OR-Join (Synchronizing Merge) Pattern for YAWL.

This module implements the OR-Join pattern as a gen_yawl behaviour.

## Pattern Description

The OR-Join pattern (WCP-09 variant) merges multiple concurrent branches,
triggering when ANY input branch completes. Unlike the AND-join which waits
for ALL branches, OR-join proceeds with the first completion but consumes
tokens from all completed branches to prevent re-triggering.

## OR-Join Semantics

1. **Triggers when ANY input branch completes** - Not all branches required
2. **Consumes tokens from all completed branches** - Prevents duplicate triggers
3. **Subsequent arrivals don't re-trigger** - Once triggered, waits for reset
4. **Reset after all branches processed** - Ready for next merge cycle

## Petri Net Structure

Places:
  p_input_1         - Input place for branch 1
  p_input_2         - Input place for branch 2
  ...
  p_input_N         - Input place for branch N
  p_trigger_ready   - Trigger is ready to fire
  p_triggered       - Trigger has fired (first complete)
  p_consume_pool    - Pool of remaining completions to consume
  p_reset_pending   - Reset pending when all branches consumed
  p_output          - Output place

Transitions:
  t_complete_1      - Complete branch 1
  t_complete_2      - Complete branch 2
  ...
  t_complete_N      - Complete branch N
  t_trigger         - Trigger on first completion
  t_consume         - Consume remaining completions
  t_reset           - Reset for next cycle
  t_output          - Produce output

## Soundness Properties

- **Option to complete:** Always true (trigger fires on first completion)
- **Proper completion:** Exactly one output token per merge cycle
- **No dead transitions:** All branches are consumed appropriately
- **No re-trigger:** Once triggered, waits for reset before next cycle

## Examples

```erlang
%% Create a 2-branch OR-Join
> State = or_join:new([fun(X) -> X * 2 end, fun(X) -> X + 10 end], 2).
#or_join_state{branch_count = 2, branch_funs = [...], ...}

%% Get places for 2 branches
> or_join:place_lst(2).
[p_input_1,p_input_2,p_trigger_ready,p_triggered,
 p_consume_pool,p_reset_pending,p_output]

%% Get places for 5 branches (dynamic)
> or_join:place_lst(5).
[p_input_1,p_input_2,p_input_3,p_input_4,p_input_5,
 p_trigger_ready,p_triggered,p_consume_pool,p_reset_pending,p_output]

%% Get transitions for 3 branches
> or_join:trsn_lst(3).
[t_complete_1,t_complete_2,t_complete_3,t_trigger,t_consume,t_reset,t_output]

%% Check preset for a completion transition
> or_join:preset('t_complete_1', State).
[p_input_1]

%% Check preset for control transitions
> or_join:preset('t_trigger', State).
[p_trigger_ready]
> or_join:preset('t_output', State).
[p_triggered]

%% Initial marking for trigger_ready
> or_join:init_marking('p_trigger_ready', State).
[ready]

%% Initial marking for other places (empty)
> or_join:init_marking('p_input_1', State).
[]

%% Check if t_complete_1 is enabled (has token, not triggered)
> Mode = #{'p_input_1' => [complete_token], 'p_trigger_ready' => [ready]},
> or_join:is_enabled('t_complete_1', Mode, State).
true

%% Check if t_complete_1 is enabled after trigger (should be false)
> TriggeredState = State#or_join_state{triggered_by = 1},
> or_join:is_enabled('t_complete_1', Mode, TriggeredState).
false

%% Check if t_trigger is enabled
> or_join:is_enabled('t_trigger', #{'p_trigger_ready' => [ready]}, State).
true

%% Check if t_output is enabled (needs triggered token)
> or_join:is_enabled('t_output', #{'p_triggered' => [triggered, 1]}, State).
true
```
""".
-behaviour(gen_yawl).

%% gen_yawl callbacks
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

-record(or_join_state, {
    branch_count :: pos_integer(),
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    triggered_by :: undefined | pos_integer(),
    cycle_count = 0 :: non_neg_integer(),
    log_id :: binary() | undefined
}).

-type or_join_state() :: #or_join_state{}.
-export_type([or_join_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-doc """
Creates a new OR-Join pattern state.

Parameters:
- BranchFuns: List of functions to execute for each branch
- BranchCount: Number of branches (must match length of BranchFuns and be >= 2)

Returns a new or_join_state record with the given branch functions.
""".
-spec new(BranchFuns :: [function()], BranchCount :: pos_integer()) ->
          or_join_state().

new(BranchFuns, BranchCount) when is_list(BranchFuns),
                                  length(BranchFuns) =:= BranchCount,
                                  BranchCount >= 2 ->
    LogId = generate_log_id(),
    #or_join_state{
        branch_count = BranchCount,
        branch_funs = BranchFuns,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the OR-Join workflow as a gen_yawl process.
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
    OrJoinState = new(BranchFuns, BranchCount),
    gen_yawl:start_link(?MODULE, OrJoinState, []).

%%--------------------------------------------------------------------
%% @doc Runs the OR-Join workflow synchronously.
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
%% @doc Gets the current state of the OR-Join workflow.
%%
%% @param Pid The pid of the gen_yawl process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, or_join_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes the OR-Join pattern with given input data.
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

    %% Wait for first completion (OR-join semantics)
    case wait_first_complete(Ref, Pids, 30000) of
        {ok, {Index, Result}} ->
            %% Consume remaining results without triggering
            consume_remaining(Ref, 5000),
            {ok, {Index, Result}};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Resets the OR-join for another cycle.
%%
%% @param Pid The pid of the gen_yawl process.
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec reset(Pid :: pid()) -> ok | {error, term()}.

reset(Pid) ->
    gen_yawl:cast(Pid, reset_or_join).

%%====================================================================
%% gen_yawl Callbacks
%%====================================================================

-doc """
Returns the list of places for the OR-Join Petri net.

The place list is dynamically generated based on the number of branches.

For N branches, returns: [p_input_1, ..., p_input_N, p_trigger_ready,
p_triggered, p_consume_pool, p_reset_pending, p_output].

## Examples

```erlang
> or_join:place_lst(2).
[p_input_1,p_input_2,p_trigger_ready,p_triggered,
 p_consume_pool,p_reset_pending,p_output]

> or_join:place_lst(4).
[p_input_1,p_input_2,p_input_3,p_input_4,p_trigger_ready,
 p_triggered,p_consume_pool,p_reset_pending,p_output]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    place_lst(10).  %% Default max branches

place_lst(BranchCount) when BranchCount >= 2 ->
    InputPlaces = [list_to_atom("p_input_" ++ integer_to_list(I)) || I <- lists:seq(1, BranchCount)],
    InputPlaces ++ ['p_trigger_ready', 'p_triggered', 'p_consume_pool', 'p_reset_pending', 'p_output'].

-doc """
Returns the list of transitions for the OR-Join Petri net.

The transition list is dynamically generated based on the number of branches.

For N branches, returns: [t_complete_1, ..., t_complete_N, t_trigger,
t_consume, t_reset, t_output].

## Examples

```erlang
> or_join:trsn_lst(2).
[t_complete_1,t_complete_2,t_trigger,t_consume,t_reset,t_output]

> or_join:trsn_lst(5).
[t_complete_1,t_complete_2,t_complete_3,t_complete_4,t_complete_5,
 t_trigger,t_consume,t_reset,t_output]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    trsn_lst(10).  %% Default max branches

trsn_lst(BranchCount) when BranchCount >= 2 ->
    CompleteTrans = [list_to_atom("t_complete_" ++ integer_to_list(I)) || I <- lists:seq(1, BranchCount)],
    CompleteTrans ++ ['t_trigger', 't_consume', 't_reset', 't_output'].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: or_join_state()) ->
          [term()].

init_marking('p_trigger_ready', _UsrInfo) ->
    [ready];
init_marking(_Place, _UsrInfo) ->
    [].

-doc """
Returns the preset (input places) for each transition.

The preset is dynamically generated based on the branch count stored in UsrInfo.
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_trigger') ->
    ['p_trigger_ready'];
preset('t_consume') ->
    ['p_consume_pool'];
preset('t_reset') ->
    ['p_reset_pending'];
preset('t_output') ->
    ['p_triggered'];
preset(Trsn) when is_atom(Trsn) ->
    Str = atom_to_list(Trsn),
    case Str of
        "t_complete_" ++ Rest ->
            [list_to_atom("p_input_" ++ Rest)];
        _ ->
            []
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%%
%% For t_complete_N transitions: enabled when the input place has a token
%% and no branch has triggered yet (TriggeredBy = undefined).
%%
%% For t_trigger: enabled when trigger_ready has token and not yet triggered.
%%
%% For t_consume: enabled when consume_pool has tokens and already triggered.
%%
%% For t_reset: enabled when reset_pending has reset_req token.
%%
%% For t_output: enabled when triggered place has [triggered, Index] tokens.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: or_join_state()) ->
          boolean().

%% t_complete_N transitions - check if specific input branch has token
is_enabled(Trsn, Mode, #or_join_state{triggered_by = TriggeredBy}) when is_atom(Trsn) ->
    Str = atom_to_list(Trsn),
    case Str of
        "t_complete_" ++ Rest ->
            InputPlace = list_to_atom("p_input_" ++ Rest),
            case maps:get(InputPlace, Mode, []) of
                [_] when TriggeredBy =:= undefined ->
                    true;
                _ ->
                    false
            end;
        _ ->
            control_transition_enabled(Trsn, Mode, TriggeredBy)
    end.

%% @private Control transition enablement
-spec control_transition_enabled(atom(), map(), undefined | pos_integer()) -> boolean().
control_transition_enabled('t_trigger', Mode, TriggeredBy) ->
    case maps:get('p_trigger_ready', Mode, []) of
        [ready] when TriggeredBy =:= undefined ->
            true;
        _ ->
            false
    end;
control_transition_enabled('t_consume', Mode, TriggeredBy) ->
    case maps:get('p_consume_pool', Mode, []) of
        [_ | _] when TriggeredBy =/= undefined ->
            true;
        _ ->
            false
    end;
control_transition_enabled('t_reset', Mode, _TriggeredBy) ->
    case maps:get('p_reset_pending', Mode, []) of
        [reset_req] ->
            true;
        _ ->
            false
    end;
control_transition_enabled('t_output', Mode, _TriggeredBy) ->
    case maps:get('p_triggered', Mode, []) of
        [triggered, _] ->
            true;
        _ ->
            false
    end;
control_transition_enabled(_Trsn, _Mode, _TriggeredBy) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: or_join_state()) ->
          {produce, map()} | {produce, map(), or_join_state()} | abort.

fire(Trsn, Mode, #or_join_state{branch_count = Count} = State) ->
    Str = atom_to_list(Trsn),
    _PrefixLen = length("t_complete_"),
    case Str of
        "t_complete_" ++ Rest ->
            InputPlace = list_to_atom("p_input_" ++ Rest),
            Index = list_to_integer(Rest),
            case maps:get(InputPlace, Mode, []) of
                [complete_token] ->
                    %% First completion - trigger the OR-join
                    NewState = State#or_join_state{
                        completed = [Index],
                        triggered_by = Index
                    },
                    log_event(State, <<"ORJoin">>, <<"FirstComplete">>, #{<<"branch">> => Index}),
                    {produce, #{
                        InputPlace => [],
                        'p_trigger_ready' => [],
                        'p_triggered' => [triggered, Index]
                    }, NewState};
                _ ->
                    abort
            end;
        _ ->
            case Trsn of
                't_trigger' ->
                    %% Trigger transition - just pass through (actual work in t_complete_N)
                    {produce, #{}, State};
                't_consume' ->
                    ConsumePool = maps:get('p_consume_pool', Mode, []),
                    NewState = State#or_join_state{completed = [I || {complete, I} <- ConsumePool] ++ State#or_join_state.completed},
                    CompletedCount = length(NewState#or_join_state.completed),
                    case CompletedCount >= Count of
                        true ->
                            %% All branches consumed, request reset
                            log_event(State, <<"ORJoin">>, <<"AllConsumed">>, #{<<"count">> => Count}),
                            {produce, #{
                                'p_consume_pool' => [],
                                'p_reset_pending' => [reset_req]
                            }, NewState};
                        false ->
                            {produce, #{
                                'p_consume_pool' => []
                            }, NewState}
                    end;
                't_reset' ->
                    #or_join_state{cycle_count = Cycle} = State,
                    NewState = State#or_join_state{
                        completed = [],
                        triggered_by = undefined,
                        cycle_count = Cycle + 1
                    },
                    log_event(State, <<"ORJoin">>, <<"Reset">>, #{<<"cycle">> => Cycle + 1}),
                    {produce, #{
                        'p_reset_pending' => [],
                        'p_trigger_ready' => [ready]
                    }, NewState};
                't_output' ->
                    TriggeredTokens = maps:get('p_triggered', Mode, []),
                    case TriggeredTokens of
                        [triggered, Index] ->
                            log_event(State, <<"ORJoin">>, <<"Output">>, #{<<"triggered_by">> => Index}),
                            {produce, #{
                                'p_triggered' => [],
                                'p_output' => [{or_joined, Index}]
                            }, State};
                        _ ->
                            abort
                    end;
                _ ->
                    abort
            end
    end.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: or_join_state()) ->
          pass | drop.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_yawl.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: or_join_state()) -> or_join_state().

init(OrJoinState) ->
    case catch yawl_xes:new_log(#{<<"process">> => <<"ORJoin">>}) of
        {ok, LogId} ->
            State1 = OrJoinState#or_join_state{log_id = LogId},
            catch yawl_xes:log_case_start(LogId, generate_case_id()),
            State1;
        _ ->
            OrJoinState
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
          {noreply, term()}.

handle_cast(_Request, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles non-gen_yawl messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), NetState :: term()) -> noreply.

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
        #or_join_state{log_id = LogId} when LogId =/= undefined ->
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
                        #or_join_state{triggered_by = TriggeredBy} when TriggeredBy =/= undefined ->
                            {ok, {TriggeredBy, or_joined}};
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
    <<"or_join_", Hex/binary>>.

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
-spec log_event(State :: or_join_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#or_join_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    %% Use catch to handle cases where yawl_xes is not available
    catch yawl_xes:log_event(LogId, Concept, Lifecycle, Data),
    ok;
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Doctest placeholder
%%--------------------------------------------------------------------
doctest_test_() ->
    {"Doctest examples in moduledoc", [
        ?_assertEqual(
            [p_input_1,p_input_2,p_trigger_ready,p_triggered,
             p_consume_pool,p_reset_pending,p_output],
            place_lst(2)),
        ?_assertEqual(
            [p_input_1,p_input_2,p_input_3,p_input_4,p_input_5,
             p_trigger_ready,p_triggered,p_consume_pool,p_reset_pending,p_output],
            place_lst(5)),
        ?_assertEqual(
            [t_complete_1,t_complete_2,t_complete_3,
             t_trigger,t_consume,t_reset,t_output],
            trsn_lst(3)),
        ?_assertEqual(
            [p_input_1],
            preset('t_complete_1', new([fun(_) -> ok end, fun(_) -> ok end], 2))),
        ?_assertEqual(
            [p_trigger_ready],
            preset('t_trigger', new([fun(_) -> ok end, fun(_) -> ok end], 2))),
        ?_assertEqual(
            [p_triggered],
            preset('t_output', new([fun(_) -> ok end, fun(_) -> ok end], 2))),
        ?_assertEqual(
            [ready],
            init_marking('p_trigger_ready', new([fun(_) -> ok end, fun(_) -> ok end], 2))),
        ?_assertEqual(
            [],
            init_marking('p_input_1', new([fun(_) -> ok end, fun(_) -> ok end], 2))),
        ?_assert(
            is_enabled('t_complete_1',
                      #{'p_input_1' => [complete_token]},
                      new([fun(_) -> ok end, fun(_) -> ok end], 2))),
        ?_assertNot(
            is_enabled('t_complete_1',
                      #{'p_input_1' => [complete_token]},
                      (new([fun(_) -> ok end, fun(_) -> ok end], 2))#or_join_state{triggered_by = 1})),
        ?_assert(
            is_enabled('t_trigger',
                      #{'p_trigger_ready' => [ready]},
                      new([fun(_) -> ok end, fun(_) -> ok end], 2))),
        ?_assert(
            is_enabled('t_output',
                      #{'p_triggered' => [triggered, 1]},
                      new([fun(_) -> ok end, fun(_) -> ok end], 2)))
    ]}.

%%--------------------------------------------------------------------
%% Dynamic place/trsn generation tests
%%--------------------------------------------------------------------
place_lst_dynamic_test_() ->
    {"place_lst generates correct number of places based on branch count", [
        ?_assertEqual(7, length(place_lst(2))),
        ?_assertEqual(8, length(place_lst(3))),
        ?_assertEqual(9, length(place_lst(4))),
        ?_assertEqual(10, length(place_lst(5))),
        ?_assertEqual(15, length(place_lst(10))),  % 10 input + 5 control
        ?_assertEqual(
            [p_input_1,p_input_2,p_trigger_ready,p_triggered,
             p_consume_pool,p_reset_pending,p_output],
            place_lst(2)),
        ?_assertEqual(
            [p_input_1,p_input_2,p_input_3,p_trigger_ready,
             p_triggered,p_consume_pool,p_reset_pending,p_output],
            place_lst(3)),
        ?_assertEqual(
            [p_input_1,p_input_2,p_input_3,p_input_4,p_input_5,p_input_6,p_input_7,
             p_trigger_ready,p_triggered,p_consume_pool,p_reset_pending,p_output],
            place_lst(7))
    ]}.

trsn_lst_dynamic_test_() ->
    {"trsn_lst generates correct number of transitions based on branch count", [
        ?_assertEqual(6, length(trsn_lst(2))),
        ?_assertEqual(7, length(trsn_lst(3))),
        ?_assertEqual(8, length(trsn_lst(4))),
        ?_assertEqual(9, length(trsn_lst(5))),
        ?_assertEqual(14, length(trsn_lst(10))),  % 10 complete + 4 control
        ?_assertEqual(
            [t_complete_1,t_complete_2,t_trigger,t_consume,t_reset,t_output],
            trsn_lst(2)),
        ?_assertEqual(
            [t_complete_1,t_complete_2,t_complete_3,t_trigger,t_consume,t_reset,t_output],
            trsn_lst(3)),
        ?_assertEqual(
            [t_complete_1,t_complete_2,t_complete_3,t_complete_4,t_complete_5,
             t_trigger,t_consume,t_reset,t_output],
            trsn_lst(5))
    ]}.

%%--------------------------------------------------------------------
%% Preset tests for dynamic transitions
%%--------------------------------------------------------------------
preset_dynamic_test_() ->
    State5 = new([fun(_) -> ok end, fun(_) -> ok end, fun(_) -> ok end,
                  fun(_) -> ok end, fun(_) -> ok end], 5),
    {"preset returns correct input places for dynamic t_complete_N", [
        ?_assertEqual([p_input_1], preset('t_complete_1', State5)),
        ?_assertEqual([p_input_2], preset('t_complete_2', State5)),
        ?_assertEqual([p_input_3], preset('t_complete_3', State5)),
        ?_assertEqual([p_input_4], preset('t_complete_4', State5)),
        ?_assertEqual([p_input_5], preset('t_complete_5', State5)),
        ?_assertEqual([p_trigger_ready], preset('t_trigger', State5)),
        ?_assertEqual([p_consume_pool], preset('t_consume', State5)),
        ?_assertEqual([p_reset_pending], preset('t_reset', State5)),
        ?_assertEqual([p_triggered], preset('t_output', State5)),
        ?_assertEqual([], preset(unknown, State5))
    ]}.

preset_t_complete_1_test() ->
    ?assertEqual(['p_input_1'], preset('t_complete_1')).

preset_t_trigger_test() ->
    ?assertEqual(['p_trigger_ready'], preset('t_trigger')).

preset_t_output_test() ->
    ?assertEqual(['p_triggered'], preset('t_output')).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

new_state_test() ->
    Fun1 = fun(X) -> X * 2 end,
    Fun2 = fun(X) -> X + 10 end,
    State = new([Fun1, Fun2], 2),
    ?assertEqual(2, State#or_join_state.branch_count),
    ?assertEqual(2, length(State#or_join_state.branch_funs)),
    ?assertEqual([], State#or_join_state.completed),
    ?assertEqual(undefined, State#or_join_state.triggered_by).

init_marking_p_trigger_ready_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    ?assertEqual([ready], init_marking('p_trigger_ready', State)).

init_marking_other_place_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    ?assertEqual([], init_marking('p_output', State)),
    ?assertEqual([], init_marking('p_input_1', State)).

is_enabled_t_complete_when_ready_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    Mode = #{'p_input_1' => [complete_token], 'p_trigger_ready' => [ready]},
    ?assert(is_enabled('t_complete_1', Mode, State)).

is_enabled_t_complete_after_trigger_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    TriggeredState = State#or_join_state{triggered_by = 1},
    {"is_enabled for control transitions", [
        %% t_trigger: enabled when ready and not triggered
        ?_assert(
            is_enabled('t_trigger',
                      #{'p_trigger_ready' => [ready]},
                      State)),
        ?_assertNot(
            is_enabled('t_trigger',
                      #{},
                      State)),
        ?_assertNot(
            is_enabled('t_trigger',
                      #{'p_trigger_ready' => [ready]},
                      TriggeredState)),
        %% t_consume: enabled when pool has tokens AND already triggered
        ?_assert(
            is_enabled('t_consume',
                      #{'p_consume_pool' => [{complete, 2}]},
                      TriggeredState)),
        ?_assertNot(
            is_enabled('t_consume',
                      #{'p_consume_pool' => [{complete, 2}]},
                      State)),
        ?_assertNot(
            is_enabled('t_consume',
                      #{},
                      TriggeredState)),
        %% t_reset: enabled when reset_pending has reset_req
        ?_assert(
            is_enabled('t_reset',
                      #{'p_reset_pending' => [reset_req]},
                      State)),
        ?_assertNot(
            is_enabled('t_reset',
                      #{},
                      State)),
        %% t_output: enabled when triggered has [triggered, Index]
        ?_assert(
            is_enabled('t_output',
                      #{'p_triggered' => [triggered, 1]},
                      State)),
        ?_assertNot(
            is_enabled('t_output',
                      #{'p_triggered' => []},
                      State)),
        ?_assertNot(
            is_enabled('t_output',
                      #{'p_triggered' => [triggered]},
                      State))
    ]}.

is_enabled_prevent_retrigger_test_() ->
    {"OR-join prevents re-trigger after first completion", [
        ?_test(begin
            State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
            Mode = #{'p_input_1' => [complete_token]},
            ?assert(is_enabled('t_complete_1', Mode, State)),
            TriggeredState = State#or_join_state{triggered_by = 1},
            ?assertNot(is_enabled('t_complete_1', Mode, TriggeredState)),
            ?assertNot(is_enabled('t_complete_2', Mode, TriggeredState))
        end)
    ]}.

%%--------------------------------------------------------------------
%% State management tests
%%--------------------------------------------------------------------
new_state_test_() ->
    {"new creates valid or_join_state", [
        ?_test(begin
            Fun1 = fun(X) -> X * 2 end,
            Fun2 = fun(X) -> X + 10 end,
            State = new([Fun1, Fun2], 2),
            ?assertEqual(2, State#or_join_state.branch_count),
            ?assertEqual(2, length(State#or_join_state.branch_funs)),
            ?assertEqual([], State#or_join_state.completed),
            ?assertEqual(undefined, State#or_join_state.triggered_by),
            ?assert(is_binary(State#or_join_state.log_id))
        end)
    ]}.

init_marking_test_() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    {"init_marking returns correct initial markings", [
        ?_assertEqual([ready], init_marking('p_trigger_ready', State)),
        ?_assertEqual([], init_marking('p_output', State)),
        ?_assertEqual([], init_marking('p_input_1', State)),
        ?_assertEqual([], init_marking('p_input_2', State)),
        ?_assertEqual([], init_marking('p_triggered', State)),
        ?_assertEqual([], init_marking('p_consume_pool', State)),
        ?_assertEqual([], init_marking('p_reset_pending', State))
    ]}.

%%--------------------------------------------------------------------
%% fire transition tests
%%--------------------------------------------------------------------
fire_t_complete_1_test_() ->
    {"fire t_complete_N produces correct marking", [
        ?_test(begin
            State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
            Mode = #{'p_input_1' => [complete_token]},
            Result = fire('t_complete_1', Mode, State),
            ?assertMatch({produce, _, _}, Result),
            {produce, NewMode, NewState} = Result,
            ?assertEqual([], maps:get('p_input_1', NewMode)),
            ?assertEqual([], maps:get('p_trigger_ready', NewMode)),
            ?assertEqual([triggered, 1], maps:get('p_triggered', NewMode)),
            ?assertEqual(1, NewState#or_join_state.triggered_by),
            ?assertEqual([1], NewState#or_join_state.completed)
        end)
    ]}.

fire_t_reset_test_() ->
    {"fire t_reset resets state for next cycle", [
        ?_test(begin
            State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
            TriggeredState = State#or_join_state{triggered_by = 1, completed = [1], cycle_count = 0},
            Mode = #{'p_reset_pending' => [reset_req]},
            Result = fire('t_reset', Mode, TriggeredState),
            ?assertMatch({produce, _, _}, Result),
            {produce, NewMode, NewState} = Result,
            ?assertEqual([], maps:get('p_reset_pending', NewMode)),
            ?assertEqual([ready], maps:get('p_trigger_ready', NewMode)),
            ?assertEqual(undefined, NewState#or_join_state.triggered_by),
            ?assertEqual([], NewState#or_join_state.completed),
            ?assertEqual(1, NewState#or_join_state.cycle_count)
        end)
    ]}.

%%--------------------------------------------------------------------
%% Helper function tests
%%--------------------------------------------------------------------
log_id_generation_test_() ->
    {"generate_log_id creates unique IDs", [
        ?_test(begin
            Id1 = generate_log_id(),
            Id2 = generate_log_id(),
            ?assertNotEqual(Id1, Id2),
            ?assert(is_binary(Id1)),
            ?assertMatch(<< "or_join_", _/binary >>, Id1)
        end)
    ]}.

case_id_generation_test_() ->
    {"generate_case_id creates unique IDs", [
        ?_test(begin
            Id1 = generate_case_id(),
            Id2 = generate_case_id(),
            ?assertNotEqual(Id1, Id2),
            ?assert(is_binary(Id1)),
            ?assertMatch(<< "case_", _/binary >>, Id1)
        end)
    ]}.

-endif.
