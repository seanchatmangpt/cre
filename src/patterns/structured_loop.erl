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
%% @doc Structured Loop Pattern (WCP-22/WCP-23) for YAWL
%%
%% This module implements the Structured Loop pattern as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Structured Loop pattern (WCP-23) provides while/until loop constructs
%% for repeated execution of a subprocess based on a condition. The loop body
%% is executed while (while) or until (until) a condition becomes false/true.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Start of the loop
%%     p_check         - Check loop condition
%%     p_body_ready    - Loop body ready to execute
%%     p_body_active   - Loop body executing
%%     p_body_done     - Loop body completed
%%     p_condition_met - Condition satisfied (exit for while, continue for until)
%%     p_loop_back     - Loop back to condition check
%%     p_complete      - Loop completed
%%
%%   Transitions:
%%     t_start         - Start the loop
%%     t_check_cond    - Check the loop condition
%%     t_enter_body    - Enter loop body (condition allows iteration)
%%     t_execute_body  - Execute the loop body
%%     t_finish_body   - Mark body execution complete
%%     t_loop_back     - Loop back to condition check
%%     t_exit          - Exit the loop
%%     t_complete      - Mark loop complete
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always true (condition eventually fails)</li>
%%   <li><b>Proper completion:</b> Loop exits when condition no longer satisfied</li>
%%   <li><b>No dead transitions:</b> All transitions fire when conditions met</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(structured_loop).
-moduledoc """
Structured Loop Pattern (WCP-23) for YAWL.

This module implements the Structured Loop pattern as a gen_yawl behaviour.
Provides while/until loop constructs for repeated execution.

## Example: While Loop

```erlang
> Body = fun(X) -> X + 1 end,
> Cond = fun(X) -> X < 5 end,
> structured_loop:execute(Body, while, Cond, 0).
{ok, 5}
```

## Example: Until Loop

```erlang
> Body = fun(X) -> X + 1 end,
> Cond = fun(X) -> X >= 5 end,
> structured_loop:execute(Body, until, Cond, 0).
{ok, 5}
```

## Example: Place List

```erlang
> structured_loop:place_lst().
['p_start','p_check','p_body_ready','p_body_active',
 'p_body_done','p_condition_met','p_loop_back','p_complete']
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
    start/3,
    run/3,
    get_state/1,
    execute/4
]).

%%====================================================================
%% Records
%%====================================================================

-record(loop_state, {
    body_fun,
    loop_type,
    condition_fun,
    current_state,
    iteration_count = 0,
    max_iterations = 1000,
    log_id
}).

-type loop_state() :: #loop_state{}.
-export_type([loop_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Structured Loop pattern state.

## Example

```erlang
> Body = fun(X) -> X + 1 end,
> Cond = fun(X) -> X < 5 end,
> State = structured_loop:new(Body, while, Cond).
{loop_state,_,while,_,undefined,0,1000,_}
```
""".
-spec new(BodyFun :: function(), LoopType :: while | until, ConditionFun :: function()) ->
          loop_state().

new(BodyFun, LoopType, ConditionFun) when is_function(BodyFun),
                                          is_atom(LoopType),
                                          LoopType =:= while orelse LoopType =:= until,
                                          is_function(ConditionFun) ->
    LogId = generate_log_id(),
    #loop_state{
        body_fun = BodyFun,
        loop_type = LoopType,
        condition_fun = ConditionFun,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Structured Loop workflow as a gen_yawl process.
%% @end
%%--------------------------------------------------------------------
-spec start(BodyFun :: function(), LoopType :: while | until, ConditionFun :: function()) ->
          {ok, pid()} | {error, term()}.

start(BodyFun, LoopType, ConditionFun) when is_function(BodyFun),
                                            is_atom(LoopType),
                                            LoopType =:= while orelse LoopType =:= until,
                                            is_function(ConditionFun) ->
    State = new(BodyFun, LoopType, ConditionFun),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Runs the Structured Loop workflow synchronously.
%% @end
%%--------------------------------------------------------------------
-spec run(BodyFun :: function(), LoopType :: while | until, InitialState :: term()) ->
          {ok, term()} | {error, term()}.

run(BodyFun, LoopType, InitialState) when is_function(BodyFun), is_atom(LoopType) ->
    ConditionFun = case LoopType of
        while -> fun(State) -> State =/= undefined andalso State < 10 end;
        until -> fun(State) -> State =:= undefined orelse State >= 10 end
    end,
    case start(BodyFun, LoopType, ConditionFun) of
        {ok, Pid} ->
            gen_yawl:cast(Pid, {initial_state, InitialState}),
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
%% @doc Gets the current state of the Structured Loop workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, loop_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
-doc """
Executes the Structured Loop pattern with given parameters.

The loop body is executed repeatedly while (while) or until (until)
the condition is satisfied.

## Examples

While loop - execute while condition is true:

```erlang
> Body = fun(X) -> X + 1 end,
> Cond = fun(X) -> X < 5 end,
> structured_loop:execute(Body, while, Cond, 0).
{ok, 5}
```

Until loop - execute until condition is true:

```erlang
> Body = fun(X) -> X + 1 end,
> Cond = fun(X) -> X >= 5 end,
> structured_loop:execute(Body, until, Cond, 0).
{ok, 5}
```

Parameters:
- `BodyFun` - Function to execute on each iteration
- `LoopType` - `while` (loop while true) or `until` (loop until true)
- `ConditionFun` - Function that takes current state and returns boolean()
- `InitialState` - Initial state value

Returns `{ok, FinalState}` or `{error, Reason}`.
""".
-spec execute(BodyFun :: function(), LoopType :: while | until, ConditionFun :: function(), InitialState :: term()) ->
          {ok, term()} | {error, term()}.

execute(BodyFun, while, ConditionFun, InitialState) when is_function(BodyFun), is_function(ConditionFun) ->
    execute_while(BodyFun, ConditionFun, InitialState, 0);

execute(BodyFun, until, ConditionFun, InitialState) when is_function(BodyFun), is_function(ConditionFun) ->
    execute_until(BodyFun, ConditionFun, InitialState, 0).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Structured Loop Petri net.

```erlang
> structured_loop:place_lst().
['p_start','p_check','p_body_ready','p_body_active',
 'p_body_done','p_condition_met','p_loop_back','p_complete']
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_check',
        'p_body_ready',
        'p_body_active',
        'p_body_done',
        'p_condition_met',
        'p_loop_back',
        'p_complete'
    ].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Structured Loop Petri net.

```erlang
> structured_loop:trsn_lst().
['t_start','t_check_cond','t_enter_body','t_execute_body',
 't_finish_body','t_loop_back','t_exit','t_complete']
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_start',
        't_check_cond',
        't_enter_body',
        't_execute_body',
        't_finish_body',
        't_loop_back',
        't_exit',
        't_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: loop_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> structured_loop:preset('t_start').
['p_start']
> structured_loop:preset('t_check_cond').
['p_check']
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_start') -> ['p_start'];
preset('t_check_cond') -> ['p_check'];
preset('t_enter_body') -> ['p_body_ready'];
preset('t_execute_body') -> ['p_body_active'];
preset('t_finish_body') -> ['p_body_active'];
preset('t_loop_back') -> ['p_body_done'];
preset('t_exit') -> ['p_condition_met'];
preset('t_complete') -> ['p_complete'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: loop_state()) ->
          boolean().

is_enabled('t_start', _Mode, _UsrInfo) ->
    true;
is_enabled('t_check_cond', #{'p_check' := [check]}, #loop_state{iteration_count = Count, max_iterations = Max}) when Count < Max ->
    true;
is_enabled('t_enter_body', #{'p_body_ready' := [ready]}, _UsrInfo) ->
    true;
is_enabled('t_execute_body', #{'p_body_active' := [{state, _}]}, _UsrInfo) ->
    true;
is_enabled('t_finish_body', #{'p_body_active' := [{state, _}]}, _UsrInfo) ->
    true;
is_enabled('t_loop_back', #{'p_body_done' := [{state, _}]}, #loop_state{iteration_count = Count, max_iterations = Max}) when Count < Max ->
    true;
is_enabled('t_exit', #{'p_condition_met' := [exit_signal]}, _UsrInfo) ->
    true;
is_enabled('t_complete', #{'p_complete' := [done]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: loop_state()) ->
          {produce, map()} | {produce, map(), loop_state()} | abort.

fire('t_start', #{'p_start' := [start]}, State) ->
    log_event(State, <<"StructuredLoop">>, <<"Start">>, #{}),
    {produce, #{
        'p_start' => [],
        'p_check' => [check]
    }, State};

fire('t_check_cond', #{'p_check' := [check]}, #loop_state{loop_type = LoopType, condition_fun = ConditionFun, current_state = CurrentState} = State) ->
    %% Check loop condition
    ShouldContinue = evaluate_condition(LoopType, ConditionFun, CurrentState),
    log_event(State, <<"StructuredLoop">>, <<"ConditionCheck">>, #{<<"result">> => ShouldContinue}),
    case ShouldContinue of
        true when LoopType =:= while ->
            %% Continue with loop body
            {produce, #{
                'p_check' => [],
                'p_body_ready' => [ready]
            }, State};
        true when LoopType =:= until ->
            %% Condition met - exit loop
            {produce, #{
                'p_check' => [],
                'p_condition_met' => [exit_signal]
            }, State};
        false when LoopType =:= while ->
            %% Condition failed - exit loop
            {produce, #{
                'p_check' => [],
                'p_condition_met' => [exit_signal]
            }, State};
        false when LoopType =:= until ->
            %% Condition not met - continue loop body
            {produce, #{
                'p_check' => [],
                'p_body_ready' => [ready]
            }, State}
    end;

fire('t_enter_body', #{'p_body_ready' := [ready]}, #loop_state{current_state = CurrentState} = State) ->
    {produce, #{
        'p_body_ready' => [],
        'p_body_active' => [{state, CurrentState}]
    }, State};

fire('t_execute_body', #{'p_body_active' := [{state, CurrentState}]}, #loop_state{body_fun = BodyFun} = State) ->
    %% Execute loop body
    NewState = try
        BodyFun(CurrentState)
    catch
        _:_ -> CurrentState
    end,
    log_event(State, <<"StructuredLoop">>, <<"BodyExecuted">>, #{<<"new_state">> => NewState}),
    {produce, #{
        'p_body_active' => [{state, NewState}]
    }, State#loop_state{current_state = NewState}};

fire('t_finish_body', #{'p_body_active' := [{state, NewState}]}, #loop_state{iteration_count = Count} = State) ->
    log_event(State, <<"StructuredLoop">>, <<"BodyDone">>, #{<<"iteration">> => Count + 1}),
    {produce, #{
        'p_body_active' => [],
        'p_body_done' => [{state, NewState}]
    }, State#loop_state{iteration_count = Count + 1}};

fire('t_loop_back', #{'p_body_done' := [{state, NewState}]}, State) ->
    %% Loop back to condition check
    {produce, #{
        'p_body_done' => [],
        'p_check' => [check]
    }, State};

fire('t_exit', #{'p_condition_met' := [exit_signal]}, #loop_state{current_state = CurrentState} = State) ->
    log_event(State, <<"StructuredLoop">>, <<"Exit">>, #{<<"final_state">> => CurrentState}),
    {produce, #{
        'p_condition_met' => [],
        'p_complete' => [done]
    }, State};

fire('t_complete', #{'p_complete' := [done]}, #loop_state{current_state = CurrentState} = State) ->
    log_event(State, <<"StructuredLoop">>, <<"Complete">>, #{<<"result">> => CurrentState}),
    {produce, #{
        'p_complete' => [],
        'p_complete' => [{complete, CurrentState}]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: loop_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: loop_state()) ->
          {ok, loop_state()}.

init(LoopState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"StructuredLoop">>}) of
        {ok, LogId} ->
            State1 = LoopState#loop_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, LoopState}
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

handle_cast({initial_state, InitialState}, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #loop_state{} = State ->
            NewState = State#loop_state{current_state = InitialState},
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
        #loop_state{log_id = LogId} when LogId =/= undefined ->
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
%% @doc Evaluates the loop condition based on loop type.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec evaluate_condition(LoopType :: while | until, ConditionFun :: function(), CurrentState :: term()) ->
          boolean().

evaluate_condition(while, ConditionFun, CurrentState) ->
    try
        ConditionFun(CurrentState)
    catch
        _:_ -> false
    end;
evaluate_condition(until, ConditionFun, CurrentState) ->
    try
        ConditionFun(CurrentState)
    catch
        _:_ -> true
    end.

%%--------------------------------------------------------------------
%% @doc Executes a while loop.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec execute_while(BodyFun :: function(), ConditionFun :: function(), State :: term(), Iteration :: non_neg_integer()) ->
          {ok, term()} | {error, term()}.

execute_while(BodyFun, ConditionFun, State, Iteration) when Iteration < 1000 ->
    case ConditionFun(State) of
        true ->
            try
                NewState = BodyFun(State),
                execute_while(BodyFun, ConditionFun, NewState, Iteration + 1)
            catch
                Error:Reason:Stack ->
                    {error, {Error, Reason, Stack}}
            end;
        false ->
            {ok, State}
    end;
execute_while(_BodyFun, _ConditionFun, State, _Iteration) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Executes an until loop.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec execute_until(BodyFun :: function(), ConditionFun :: function(), State :: term(), Iteration :: non_neg_integer()) ->
          {ok, term()} | {error, term()}.

execute_until(BodyFun, ConditionFun, State, Iteration) when Iteration < 1000 ->
    case ConditionFun(State) of
        true ->
            {ok, State};
        false ->
            try
                NewState = BodyFun(State),
                execute_until(BodyFun, ConditionFun, NewState, Iteration + 1)
            catch
                Error:Reason:Stack ->
                    {error, {Error, Reason, Stack}}
            end
    end;
execute_until(_BodyFun, _ConditionFun, State, _Iteration) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Waits for workflow completion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_for_completion(Pid :: pid(), Timeout :: timeout()) ->
          {ok, term()} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_complete', Ref},
    receive
        {trigger, 'p_complete', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #loop_state{current_state = Result} ->
                            {ok, Result};
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
    <<"structured_loop_", Hex/binary>>.

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
-spec log_event(State :: loop_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#loop_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
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

%% Test while loop
execute_while_test() ->
    Body = fun(X) -> X + 1 end,
    Cond = fun(X) -> X < 5 end,
    ?assertEqual({ok, 5}, execute(Body, while, Cond, 0)).

%% Test until loop
execute_until_test() ->
    Body = fun(X) -> X + 1 end,
    Cond = fun(X) -> X >= 5 end,
    ?assertEqual({ok, 5}, execute(Body, until, Cond, 0)).

-endif.
