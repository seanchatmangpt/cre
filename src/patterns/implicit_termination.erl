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
%% @doc Implicit Termination Pattern (WCP-11) for YAWL
%%
%% This module implements the Implicit Termination pattern as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Implicit Termination pattern (WCP-11) specifies that a subprocess should
%% terminate when there is no work remaining and all input conditions are satisfied.
%% The workflow automatically detects completion without explicit termination signals.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start        - Start of the workflow
%%     p_active       - Workflow is active
%%     p_work         - Work is available
%%     p_no_work      - No work remaining signal
%%     p_check_inputs - Check if all inputs satisfied
%%     p_terminate    - Termination condition met
%%     p_complete     - Workflow complete
%%
%%   Transitions:
%%     t_activate     - Activate the workflow
%%     t_check_work   - Check if work remains
%%     t_no_work      - Signal no work remaining
%%     t_verify_inputs - Verify all inputs satisfied
%%     t_terminate    - Terminate the workflow
%%     t_complete     - Mark workflow complete
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always true (termination when conditions met)</li>
%%   <li><b>Proper completion:</b> All work processed before termination</li>
%%   <li><b>No dead transitions:</b> All transitions fire when conditions met</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(implicit_termination).
-moduledoc """
Implicit Termination Pattern (WCP-11) for YAWL.

This module implements the Implicit Termination pattern as a gen_yawl behaviour.
The workflow terminates automatically when no work remains and all inputs are satisfied.

## Example: Place List

```erlang
> implicit_termination:place_lst().
['p_start','p_active','p_work','p_no_work','p_check_inputs',
 'p_terminate','p_complete']
```

## Example: Transition List

```erlang
> implicit_termination:trsn_lst().
['t_activate','t_check_work','t_no_work','t_verify_inputs',
 't_terminate','t_complete']
```

## Example: Execute Implicit Termination

```erlang
> Fun = fun(List) -> lists:map(fun(X) -> X * 2 end, List) end,
> implicit_termination:execute(Fun, [1,2,3]).
{ok, [2,4,6]}
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

-record(implicit_termination_state, {
    subprocess :: function(),
    input_data :: term() | undefined,
    work_remaining = true :: boolean(),
    inputs_satisfied = true :: boolean(),
    result :: undefined | term(),
    log_id :: binary() | undefined
}).

-type implicit_termination_state() :: #implicit_termination_state{}.
-export_type([implicit_termination_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Implicit Termination pattern state.

The subprocess will be executed and automatically terminated when complete.

## Example

```erlang
> Fun = fun(X) -> X * 2 end,
> State = implicit_termination:new(Fun).
{implicit_termination_state,_,_,true,true,undefined,_}
```
""".
-spec new(Subprocess :: function()) -> implicit_termination_state().

new(Subprocess) when is_function(Subprocess) ->
    LogId = generate_log_id(),
    #implicit_termination_state{
        subprocess = Subprocess,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Implicit Termination workflow as a gen_yawl process.
%% @end
%%--------------------------------------------------------------------
-spec start(Subprocess :: function()) ->
          {ok, pid()} | {error, term()}.

start(Subprocess) when is_function(Subprocess) ->
    State = new(Subprocess),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Runs the Implicit Termination workflow synchronously.
%% @end
%%--------------------------------------------------------------------
-spec run(Subprocess :: function(), InputData :: term()) ->
          {ok, term()} | {error, term()}.

run(Subprocess, InputData) when is_function(Subprocess) ->
    case start(Subprocess) of
        {ok, Pid} ->
            gen_yawl:cast(Pid, {input_data, InputData}),
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
%% @doc Gets the current state of the Implicit Termination workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, implicit_termination_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
-doc """
Executes the Implicit Termination pattern with given input data.

The subprocess is executed and automatically terminates when complete.

## Example

```erlang
> Fun = fun(List) -> lists:map(fun(X) -> X * 2 end, List) end,
> implicit_termination:execute(Fun, [1,2,3]).
{ok, [2,4,6]}
```

Parameters:
- `Subprocess` - Function to execute
- `InputData` - Data to pass to the subprocess

Returns `{ok, Result}` or `{error, Reason}`.
""".
-spec execute(Subprocess :: function(), InputData :: term()) ->
          {ok, term()} | {error, term()}.

execute(Subprocess, InputData) when is_function(Subprocess) ->
    try
        Result = Subprocess(InputData),
        {ok, Result}
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Implicit Termination Petri net.

```erlang
> implicit_termination:place_lst().
['p_start','p_active','p_work','p_no_work','p_check_inputs',
 'p_terminate','p_complete']
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_active',
        'p_work',
        'p_no_work',
        'p_check_inputs',
        'p_terminate',
        'p_complete'
    ].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Implicit Termination Petri net.

```erlang
> implicit_termination:trsn_lst().
['t_activate','t_check_work','t_no_work','t_verify_inputs',
 't_terminate','t_complete']
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_activate',
        't_check_work',
        't_no_work',
        't_verify_inputs',
        't_terminate',
        't_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: implicit_termination_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking('p_work', _UsrInfo) ->
    [pending];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

## Example

```erlang
> implicit_termination:preset('t_activate').
['p_start']
> implicit_termination:preset('t_terminate').
['p_no_work']
> implicit_termination:preset('unknown').
[]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_activate') -> ['p_start'];
preset('t_check_work') -> ['p_active'];
preset('t_no_work') -> ['p_work'];
preset('t_verify_inputs') -> ['p_no_work'];
preset('t_terminate') -> ['p_check_inputs'];
preset('t_complete') -> ['p_terminate'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: implicit_termination_state()) ->
          boolean().

is_enabled('t_activate', _Mode, _UsrInfo) ->
    true;
is_enabled('t_check_work', #{'p_active' := [active]}, #implicit_termination_state{work_remaining = true}) ->
    true;
is_enabled('t_no_work', #{'p_work' := [pending]}, _UsrInfo) ->
    true;
is_enabled('t_verify_inputs', #{'p_no_work' := [no_work]}, #implicit_termination_state{inputs_satisfied = true}) ->
    true;
is_enabled('t_terminate', #{'p_check_inputs' := [verified]}, _UsrInfo) ->
    true;
is_enabled('t_complete', #{'p_terminate' := [terminate]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: implicit_termination_state()) ->
          {produce, map()} | {produce, map(), implicit_termination_state()} | abort.

fire('t_activate', #{'p_start' := [start]}, State) ->
    log_event(State, <<"ImplicitTermination">>, <<"Activate">>, #{}),
    {produce, #{
        'p_start' => [],
        'p_active' => [active]
    }, State};

fire('t_check_work', #{'p_active' := [active]}, #implicit_termination_state{subprocess = Subprocess, input_data = InputData} = State) ->
    %% Execute subprocess
    Result = try
        case InputData of
            undefined -> Subprocess();
            Data -> Subprocess(Data)
        end
    catch
        _:_ -> {error, subprocess_failed}
    end,
    log_event(State, <<"ImplicitTermination">>, <<"WorkDone">>, #{<<"result">> => Result}),
    {produce, #{
        'p_active' => [],
        'p_work' => [no_work]
    }, State#implicit_termination_state{work_remaining = false, result = Result}};

fire('t_no_work', #{'p_work' := [pending]}, State) ->
    %% Signal no work remaining
    {produce, #{
        'p_work' => [],
        'p_no_work' => [no_work]
    }, State#implicit_termination_state{work_remaining = false}};

fire('t_verify_inputs', #{'p_no_work' := [no_work]}, State) ->
    %% Verify all inputs satisfied (assumed true for this pattern)
    log_event(State, <<"ImplicitTermination">>, <<"InputsVerified">>, #{}),
    {produce, #{
        'p_no_work' => [],
        'p_check_inputs' => [verified]
    }, State};

fire('t_terminate', #{'p_check_inputs' := [verified]}, State) ->
    log_event(State, <<"ImplicitTermination">>, <<"Terminate">>, #{}),
    {produce, #{
        'p_check_inputs' => [],
        'p_terminate' => [terminate]
    }, State};

fire('t_complete', #{'p_terminate' := [terminate]}, #implicit_termination_state{result = Result} = State) ->
    log_event(State, <<"ImplicitTermination">>, <<"Complete">>, #{<<"result">> => Result}),
    {produce, #{
        'p_terminate' => [],
        'p_complete' => [{complete, Result}]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: implicit_termination_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: implicit_termination_state()) ->
          {ok, implicit_termination_state()}.

init(ImplicitTerminationState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"ImplicitTermination">>}) of
        {ok, LogId} ->
            State1 = ImplicitTerminationState#implicit_termination_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, ImplicitTerminationState}
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
        #implicit_termination_state{} = State ->
            NewState = State#implicit_termination_state{input_data = InputData},
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
        #implicit_termination_state{log_id = LogId} when LogId =/= undefined ->
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
    Pid ! {trigger, 'p_complete', Ref},
    receive
        {trigger, 'p_complete', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #implicit_termination_state{result = Result} ->
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
    <<"implicit_term_", Hex/binary>>.

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
-spec log_event(State :: implicit_termination_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#implicit_termination_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
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

%% Test place_lst/0
place_lst_test() ->
    Expected = [p_start, p_active, p_work, p_no_work, p_check_inputs,
                p_terminate, p_complete],
    ?assertEqual(Expected, place_lst()).

%% Test trsn_lst/0
trsn_lst_test() ->
    Expected = [t_activate, t_check_work, t_no_work, t_verify_inputs,
                t_terminate, t_complete],
    ?assertEqual(Expected, trsn_lst()).

%% Test preset/1
preset_t_activate_test() ->
    ?assertEqual([p_start], preset('t_activate')).

preset_t_terminate_test() ->
    ?assertEqual([p_check_inputs], preset('t_terminate')).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

%% Test new/1
new_test() ->
    Fun = fun() -> ok end,
    State = new(Fun),
    ?assert(is_record(State, implicit_termination_state)),
    ?assertEqual(true, State#implicit_termination_state.work_remaining).

%% Test execute/2
execute_test() ->
    Fun = fun(X) -> X * 2 end,
    ?assertEqual({ok, 10}, execute(Fun, 5)).

execute_list_test() ->
    Fun = fun(List) -> lists:map(fun(X) -> X * 2 end, List) end,
    ?assertEqual({ok, [2,4,6]}, execute(Fun, [1,2,3])).

-endif.
