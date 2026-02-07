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
%% @doc Critical Section Pattern (WCP-26) for YAWL
%%
%% This module implements the Critical Section pattern as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Critical Section pattern (WCP-26) provides mutual exclusion for shared
%% resources. Only one process can execute within the critical section at a time,
%% ensuring that concurrent access to shared resources is properly serialized.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Start of the workflow
%%     p_lock_request  - Request for lock
%%     p_lock_wait     - Waiting for lock
%%     p_lock_acquired - Lock acquired
%%     p_critical      - Inside critical section
%%     p_critical_done - Critical section complete
%%     p_lock_release  - Release lock
%%     p_complete      - Workflow complete
%%
%%   Transitions:
%%     t_request_lock  - Request the lock
%%     t_acquire_lock  - Acquire the lock (if available)
%%     t_enter_critical - Enter critical section
%%     t_execute       - Execute critical section activity
%%     t_exit_critical - Exit critical section
%%     t_release_lock  - Release the lock
%%     t_complete      - Complete workflow
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always true (lock eventually acquired)</li>
%%   <li><b>Proper completion:</b> Critical section executes exactly once</li>
%%   <li><b>Mutual exclusion:</b> Only one process in critical section at a time</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(critical_section).
-moduledoc """
Critical Section Pattern (WCP-26) for YAWL.

This module implements the Critical Section pattern as a gen_yawl behaviour.
Provides mutual exclusion for shared resources using lock-based synchronization.

## Example: Execute Critical Section

```erlang
> CriticalFun = fun(X) -> X * 2 end,
> critical_section:execute(CriticalFun, my_lock, 5).
{ok, 10}
```

## Example: Place List

```erlang
> critical_section:place_lst().
['p_start','p_lock_request','p_lock_wait','p_lock_acquired',
 'p_critical','p_critical_done','p_lock_release','p_complete']
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
    execute/3
]).

%%====================================================================
%% Records
%%====================================================================

-record(critical_section_state, {
    critical_fun :: function(),
    lock_id :: term(),
    input_data :: term() | undefined,
    result :: undefined | term(),
    lock_acquired = false :: boolean(),
    log_id :: binary() | undefined
}).

-type critical_section_state() :: #critical_section_state{}.
-export_type([critical_section_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Critical Section pattern state.

## Example

```erlang
> Fun = fun(X) -> X * 2 end,
> State = critical_section:new(Fun, my_lock).
{critical_section_state,_,my_lock,_,undefined,false,_}
```
""".
-spec new(CriticalFun :: function(), LockId :: term()) -> critical_section_state().

new(CriticalFun, LockId) when is_function(CriticalFun) ->
    LogId = generate_log_id(),
    #critical_section_state{
        critical_fun = CriticalFun,
        lock_id = LockId,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Critical Section workflow as a gen_yawl process.
%% @end
%%--------------------------------------------------------------------
-spec start(CriticalFun :: function(), LockId :: term()) ->
          {ok, pid()} | {error, term()}.

start(CriticalFun, LockId) when is_function(CriticalFun) ->
    State = new(CriticalFun, LockId),
    gen_yawl:start_link(?MODULE, State, []).

%%--------------------------------------------------------------------
%% @doc Runs the Critical Section workflow synchronously.
%% @end
%%--------------------------------------------------------------------
-spec run(CriticalFun :: function(), InputData :: term()) ->
          {ok, term()} | {error, term()}.

run(CriticalFun, InputData) when is_function(CriticalFun) ->
    LockId = make_ref(),
    case start(CriticalFun, LockId) of
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
%% @doc Gets the current state of the Critical Section workflow.
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, critical_section_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
-doc """
Executes the Critical Section pattern with given input data.

Acquires a lock, executes the critical function, and releases the lock.

## Example

```erlang
> Fun = fun(X) -> X * 2 end,
> critical_section:execute(Fun, my_lock, 5).
{ok, 10}
```

Parameters:
- `CriticalFun` - Function to execute within critical section
- `LockId` - Identifier for the lock (used for mutual exclusion)
- `InputData` - Data to pass to the critical function

Returns `{ok, Result}` or `{error, Reason}`.
""".
-spec execute(CriticalFun :: function(), LockId :: term(), InputData :: term()) ->
          {ok, term()} | {error, term()}.

execute(CriticalFun, LockId, InputData) when is_function(CriticalFun) ->
    LockName = get_lock_name(LockId),
    try
        %% Acquire lock using global lock table
        acquire_lock(LockName),
        %% Execute critical section
        Result = try
            CriticalFun(InputData)
        catch
            InnerError:InnerReason:InnerStack ->
                release_lock(LockName),
                erlang:raise(InnerError, InnerReason, InnerStack)
        end,
        %% Release lock
        release_lock(LockName),
        {ok, Result}
    catch
        OuterError:OuterReason ->
            {error, {OuterError, OuterReason}}
    end.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Critical Section Petri net.

```erlang
> critical_section:place_lst().
['p_start','p_lock_request','p_lock_wait','p_lock_acquired',
 'p_critical','p_critical_done','p_lock_release','p_complete']
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_lock_request',
        'p_lock_wait',
        'p_lock_acquired',
        'p_critical',
        'p_critical_done',
        'p_lock_release',
        'p_complete'
    ].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Critical Section Petri net.

```erlang
> critical_section:trsn_lst().
['t_request_lock','t_acquire_lock','t_enter_critical',
 't_execute','t_exit_critical','t_release_lock','t_complete']
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_request_lock',
        't_acquire_lock',
        't_enter_critical',
        't_execute',
        't_exit_critical',
        't_release_lock',
        't_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: critical_section_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> critical_section:preset('t_request_lock').
['p_start']
> critical_section:preset('t_acquire_lock').
['p_lock_wait']
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_request_lock') -> ['p_start'];
preset('t_acquire_lock') -> ['p_lock_wait'];
preset('t_enter_critical') -> ['p_lock_acquired'];
preset('t_execute') -> ['p_critical'];
preset('t_exit_critical') -> ['p_critical'];
preset('t_release_lock') -> ['p_critical_done'];
preset('t_complete') -> ['p_lock_release'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: critical_section_state()) ->
          boolean().

is_enabled('t_request_lock', _Mode, _UsrInfo) ->
    true;
is_enabled('t_acquire_lock', #{'p_lock_wait' := [request]}, #critical_section_state{lock_id = LockId}) ->
    %% Check if lock is available
    is_lock_available(get_lock_name(LockId));
is_enabled('t_enter_critical', #{'p_lock_acquired' := [acquired]}, _UsrInfo) ->
    true;
is_enabled('t_execute', #{'p_critical' := [{input, _}]}, _UsrInfo) ->
    true;
is_enabled('t_exit_critical', #{'p_critical' := [{result, _}]}, _UsrInfo) ->
    true;
is_enabled('t_release_lock', #{'p_critical_done' := [done]}, _UsrInfo) ->
    true;
is_enabled('t_complete', #{'p_lock_release' := [released]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: critical_section_state()) ->
          {produce, map()} | {produce, map(), critical_section_state()} | abort.

fire('t_request_lock', #{'p_start' := [start]}, State) ->
    log_event(State, <<"CriticalSection">>, <<"RequestLock">>, #{}),
    {produce, #{
        'p_start' => [],
        'p_lock_request' => [request]
    }, State};

fire('t_acquire_lock', #{'p_lock_wait' := [request]}, #critical_section_state{lock_id = LockId} = State) ->
    LockName = get_lock_name(LockId),
    case acquire_lock(LockName) of
        ok ->
            log_event(State, <<"CriticalSection">>, <<"LockAcquired">>, #{<<"lock">> => LockName}),
            {produce, #{
                'p_lock_wait' => [],
                'p_lock_acquired' => [acquired]
            }, State#critical_section_state{lock_acquired = true}};
        {error, _} ->
            %% Wait and retry
            {produce, #{
                'p_lock_request' => [],
                'p_lock_wait' => [request]
            }, State}
    end;

fire('t_enter_critical', #{'p_lock_acquired' := [acquired]}, #critical_section_state{input_data = InputData} = State) ->
    log_event(State, <<"CriticalSection">>, <<"Enter">>, #{}),
    {produce, #{
        'p_lock_acquired' => [],
        'p_critical' => [{input, InputData}]
    }, State};

fire('t_execute', #{'p_critical' := [{input, InputData}]}, #critical_section_state{critical_fun = CriticalFun} = State) ->
    %% Execute critical section
    Result = try
        CriticalFun(InputData)
    catch
        _:_ -> {error, critical_failed}
    end,
    log_event(State, <<"CriticalSection">>, <<"Execute">>, #{<<"result">> => Result}),
    {produce, #{
        'p_critical' => [{result, Result}]
    }, State};

fire('t_exit_critical', #{'p_critical' := [{result, Result}]}, #critical_section_state{lock_id = LockId} = State) ->
    %% Exit critical section and release lock
    LockName = get_lock_name(LockId),
    release_lock(LockName),
    log_event(State, <<"CriticalSection">>, <<"Exit">>, #{<<"result">> => Result}),
    {produce, #{
        'p_critical' => [],
        'p_critical_done' => [done]
    }, State#critical_section_state{result = Result, lock_acquired = false}};

fire('t_release_lock', #{'p_critical_done' := [done]}, State) ->
    {produce, #{
        'p_critical_done' => [],
        'p_lock_release' => [released]
    }, State};

fire('t_complete', #{'p_lock_release' := [released]}, #critical_section_state{result = Result} = State) ->
    log_event(State, <<"CriticalSection">>, <<"Complete">>, #{<<"result">> => Result}),
    {produce, #{
        'p_lock_release' => [],
        'p_complete' => [{complete, Result}]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: critical_section_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: critical_section_state()) ->
          {ok, critical_section_state()}.

init(CriticalSectionState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"CriticalSection">>}) of
        {ok, LogId} ->
            State1 = CriticalSectionState#critical_section_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, CriticalSectionState}
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
        #critical_section_state{} = State ->
            NewState = State#critical_section_state{input_data = InputData},
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
        #critical_section_state{log_id = LogId, lock_id = LockId, lock_acquired = true} when LogId =/= undefined ->
            %% Release lock if still held
            release_lock(get_lock_name(LockId)),
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        #critical_section_state{log_id = LogId} when LogId =/= undefined ->
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
%% @doc Gets the lock name for a lock ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec get_lock_name(LockId :: term()) -> atom().

get_lock_name(LockId) when is_atom(LockId) ->
    LockId;
get_lock_name(LockId) ->
    list_to_atom("critical_section_lock_" ++ atom_to_list(LockId)).

%%--------------------------------------------------------------------
%% @doc Acquires a lock using ETS table for locking.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec acquire_lock(LockName :: atom()) -> ok | {error, term()}.

acquire_lock(LockName) ->
    case get_lock_table() of
        {ok, Tid} ->
            try
                ets:insert_new(Tid, {LockName, self()}),
                ok
            catch
                error:_ ->
                    {error, lock_failed}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Releases a lock.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec release_lock(LockName :: atom()) -> ok.

release_lock(LockName) ->
    case get_lock_table() of
        {ok, Tid} ->
            ets:delete(Tid, LockName);
        {error, _} ->
            ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc Checks if a lock is available.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec is_lock_available(LockName :: atom()) -> boolean().

is_lock_available(LockName) ->
    case get_lock_table() of
        {ok, Tid} ->
            case ets:lookup(Tid, LockName) of
                [] -> true;
                [{LockName, Owner}] when Owner =:= self() -> true;
                _ -> false
            end;
        _ ->
            true
    end.

%%--------------------------------------------------------------------
%% @doc Gets or creates the lock table.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec get_lock_table() -> {ok, ets:tid()} | {error, term()}.

get_lock_table() ->
    case whereis(critical_section_lock_table) of
        undefined ->
            Tid = try
                ets:new(critical_section_locks, [set, public, named_table])
            catch
                error:_ ->
                    undefined
            end,
            case Tid of
                undefined ->
                    case ets:whereis(critical_section_locks) of
                        undefined -> {error, no_table};
                        ExistingTid -> {ok, ExistingTid}
                    end;
                _ ->
                    try register(critical_section_lock_table, self()) of
                        true -> {ok, Tid}
                    catch
                        error:_ ->
                            case ets:whereis(critical_section_locks) of
                                undefined -> {error, no_table};
                                ExistingTid -> {ok, ExistingTid}
                            end
                    end
            end;
        _Pid ->
            case ets:whereis(critical_section_locks) of
                undefined -> {error, no_table};
                Tid -> {ok, Tid}
            end
    end.

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
                        #critical_section_state{result = Result} ->
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
    <<"critical_section_", Hex/binary>>.

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
-spec log_event(State :: critical_section_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#critical_section_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
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

%% Test execute/3
execute_test() ->
    Fun = fun(X) -> X * 2 end,
    ?assertEqual({ok, 10}, execute(Fun, test_lock, 5)).

%% Test lock behavior
lock_test() ->
    Fun = fun(X) -> timer:sleep(10), X * 2 end,
    ?assertEqual({ok, 10}, execute(Fun, lock_test, 5)).

-endif.
