# YAWL Pattern Implementation Guide

This guide provides comprehensive documentation for implementing new YAWL (Yet Another Workflow Language) workflow patterns in the CRE (Common Runtime Environment) system.

## Table of Contents

1. [Pattern Module Structure](#pattern-module-structure)
2. [gen_yawl Behavior Callbacks](#gen_yawl-behavior-callbacks)
3. [Petri Net Design](#petri-net-design)
4. [XES Logging Integration](#xes-logging-integration)
5. [EUnit Doctest Requirements](#eunit-doctest-requirements)
6. [Code Templates](#code-templates)
7. [Common Pitfalls](#common-pitfalls)
8. [Testing Patterns](#testing-patterns)

---

## Pattern Module Structure

Every YAWL pattern module follows a consistent structure:

```
-module(pattern_name).
-moduledoc """Pattern description...""".
-behaviour(gen_yawl).

%%====================================================================
%% Includes
%%====================================================================
-include("gen_pnet.hrl").
-include("cre.hrl").
-include("yawl_otel_logger.hrl").

%%====================================================================
%% Records
%%====================================================================
-record(pattern_state, {
    field1 :: type1(),
    field2 :: type2(),
    log_id :: binary() | undefined
}).

%%====================================================================
%% Exports
%%====================================================================

%% gen_yawl callbacks (required)
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3, init/1, handle_call/3, handle_cast/2,
         handle_info/2, code_change/3, terminate/2, trigger/3]).

%% API exports (pattern-specific)
-export([new/1, start/1, run/1, execute/2, get_state/1]).
```

### Module Organization

1. **Header Section** - Module declaration, license, moduledoc
2. **Include Directives** - Required header files
3. **Record Definitions** - State records with type exports
4. **Export Declarations** - Callbacks and public API
5. **Type Specifications** - Exported types for state records
6. **API Functions** - Public interface for pattern usage
7. **gen_yawl Callbacks** - Implementation of behavior callbacks
8. **Internal Helpers** - Private utility functions
9. **Doctests Section** - EUnit tests under `-ifdef(TEST)`

---

## gen_yawl Behavior Callbacks

The `gen_yawl` behavior extends `gen_pnet` with support for 3-tuple returns in `fire/3` for automatic user info updates.

### Structure Callbacks (Required)

These define the Petri net topology:

#### `place_lst/0`
```erlang
-spec place_lst() -> [atom()].
```
Returns list of all place names in the Petri net.

```erlang
place_lst() ->
    ['p_start', 'p_active', 'p_done', 'p_complete'].
```

#### `trsn_lst/0`
```erlang
-spec trsn_lst() -> [atom()].
```
Returns list of all transition names in the Petri net.

```erlang
trsn_lst() ->
    ['t_activate', 't_execute', 't_complete'].
```

#### `init_marking/2`
```erlang
-spec init_marking(Place :: atom(), UsrInfo :: term()) -> [term()].
```
Returns initial token list for a given place. Only the start place typically has tokens.

```erlang
init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].
```

#### `preset/1`
```erlang
-spec preset(Trsn :: atom()) -> [atom()].
```
Returns input places (preset) for each transition.

```erlang
preset('t_activate') -> ['p_start'];
preset('t_execute') -> ['p_active'];
preset('t_complete') -> ['p_done'];
preset(_) -> [].
```

#### `is_enabled/3`
```erlang
-spec is_enabled(Trsn :: atom(), Mode :: #{atom() => [term()]}, UsrInfo :: term()) ->
          boolean().
```
Determines if a transition can fire in the given mode. The mode maps places to the tokens that will be consumed.

```erlang
is_enabled('t_activate', #{'p_start' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_execute', #{'p_active' := [work]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.
```

#### `fire/3`
```erlang
-spec fire(Trsn :: atom(), Mode :: #{atom() => [term()]}, UsrInfo :: term()) ->
          abort | {produce, #{atom() => [term()]}} | {produce, #{atom() => [term()]}, NewUsrInfo :: term()}.
```
Fires a transition, consuming tokens from the mode and producing new tokens.

**Return values:**
- `abort` - Transition cannot fire (cancel the firing)
- `{produce, ProduceMap}` - Produce tokens, no state change
- `{produce, ProduceMap, NewUsrInfo}` - Produce tokens and update user info

```erlang
fire('t_activate', #{'p_start' := [start]}, State) ->
    log_event(State, <<"MyPattern">>, <<"Activate">>, #{}),
    {produce, #{
        'p_start' => [],
        'p_active' => [work]
    }, State};

fire('t_complete', #{'p_done' := [result]}, #pattern_state{log_id = LogId} = State) ->
    log_event(State, <<"MyPattern">>, <<"Complete">>, #{}),
    {produce, #{
        'p_done' => [],
        'p_complete' => [done]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.
```

### Interface Callbacks (Required)

These handle process interaction:

#### `init/1`
```erlang
-spec init(UsrInfo :: term()) -> {ok, UsrInfo :: term()}.
```
Initialize the pattern state and create XES log.

```erlang
init(PatternState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"MyPattern">>}) of
        {ok, LogId} ->
            State1 = PatternState#pattern_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, PatternState}
    end.
```

#### `handle_call/3`
```erlang
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.
```
Handle synchronous gen_server calls.

```erlang
handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {ok, UsrInfo}, NetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.
```

#### `handle_cast/2`
```erlang
-spec handle_cast(Request :: term(), NetState :: term()) ->
          noreply | {noreply, #{atom() => [term()]}}.
```
Handle asynchronous messages.

```erlang
handle_cast({input_data, InputData}, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #pattern_state{} = State ->
            NewState = State#pattern_state{data = InputData},
            NewUsrInfo = gen_yawl:set_usr_info(NetState, NewState),
            {noreply, NewUsrInfo};
        _ ->
            {noreply, NetState}
    end;
handle_cast(_Request, NetState) ->
    {noreply, NetState}.
```

#### `handle_info/2`
```erlang
-spec handle_info(Info :: term(), NetState :: term()) ->
          noreply | {noreply, #{atom() => [term()]}}.
```
Handle out-of-band messages.

```erlang
handle_info(_Request, NetState) ->
    {noreply, NetState}.
```

#### `code_change/3`
```erlang
-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.
```
Handle hot code reloading.

```erlang
code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.
```

#### `terminate/2`
```erlang
-spec terminate(Reason :: term(), NetState :: term()) -> ok.
```
Cleanup on process termination.

```erlang
terminate(_Reason, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #pattern_state{log_id = LogId} when LogId =/= undefined ->
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        _ ->
            ok
    end,
    ok.
```

#### `trigger/3`
```erlang
-spec trigger(Place :: atom(), Token :: term(), NetState :: term()) ->
          pass | drop.
```
Filter tokens as they are produced. Return `pass` to keep the token or `drop` to discard it.

```erlang
trigger(_Place, _Token, _UsrInfo) ->
    pass.
```

---

## Petri Net Design

YAWL patterns are implemented as Petri nets with places (holding tokens) and transitions (consuming and producing tokens).

### Place Naming Convention

Use lowercase with `p_` prefix:

```erlang
% Input/output places
'p_start', 'p_end', 'p_input', 'p_output'

% State places
'p_active', 'p_waiting', 'p_done', 'p_complete'

% Synchronization places
'p_sync', 'p_barrier', 'p_join_ready'

% Branch places
'p_branch_1', 'p_branch_2', 'p_branch_a', 'p_branch_b'
```

### Transition Naming Convention

Use lowercase with `t_` prefix:

```erlang
% Control flow
't_start', 't_activate', 't_complete', 't_finish'

% Branching
't_split', 't_merge', 't_join'

% Decision points
't_select', 't_choose', 't_route'
```

### Token Design

Tokens represent workflow state and can be any Erlang term:

```erlang
% Simple control tokens
[start, done, complete]

% Data tokens with structure
{data, Value}
{work_item, Id, Payload}

% State tracking tokens
{branch, Id}
{iteration, Count}
```

### Marking Evolution

The marking (token distribution) evolves through transition firings:

```
Initial:  #{p_start => [start], p_active => [], p_done => []}
           |
           v (t_activate fires)
           |
Step 1:   #{p_start => [], p_active => [work], p_done => []}
           |
           v (t_execute fires)
           |
Step 2:   #{p_start => [], p_active => [], p_done => [result]}
```

---

## XES Logging Integration

XES (eXtensible Event Stream) logging provides IEEE 1849-2016 compliant event logs for process mining.

### Log Creation

Create a log in `init/1`:

```erlang
init(PatternState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"MyPattern">>}) of
        {ok, LogId} ->
            State1 = PatternState#pattern_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, PatternState}
    end.
```

### Event Logging

Log events during transition firing:

```erlang
fire('t_execute', Mode, State) ->
    %% Log transition start
    log_event(State, <<"MyPattern">>, <<"ExecuteStart">>, #{
        <<"input">> => format_mode(Mode)
    }),

    %% Perform work...
    Result = do_work(),

    %% Log transition complete
    log_event(State, <<"MyPattern">>, <<"ExecuteComplete">>, #{
        <<"result">> => Result
    }),

    {produce, #{
        'p_active' => [],
        'p_done' => [Result]
    }, State}.
```

### Log Cleanup

Close the log in `terminate/2`:

```erlang
terminate(_Reason, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #pattern_state{log_id = LogId} when LogId =/= undefined ->
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        _ ->
            ok
    end,
    ok.
```

### Helper Functions

Standard logging helpers:

```erlang
%% Generate unique log ID
-spec generate_log_id() -> binary().
generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"my_pattern_", Hex/binary>>.

%% Generate unique case ID
-spec generate_case_id() -> binary().
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

%% Log XES event
-spec log_event(State :: pattern_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) -> ok.
log_event(#pattern_state{log_id = LogId}, Concept, Lifecycle, Data)
  when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.
```

---

## EUnit Doctest Requirements

All pattern modules must include doctests for validation.

### Test Section Structure

```erlang
%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Module doctest runner
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% place_lst test
place_lst_test() ->
    Expected = [p_start, p_active, p_done, p_complete],
    ?assertEqual(Expected, place_lst()).

%% trsn_lst test
trsn_lst_test() ->
    Expected = [t_activate, t_execute, t_complete],
    ?assertEqual(Expected, trsn_lst()).

%% preset tests
preset_t_activate_test() ->
    ?assertEqual([p_start], preset('t_activate')).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

%% init_marking tests
init_marking_p_start_test() ->
    State = new(fun() -> ok end),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_other_place_test() ->
    State = new(fun() -> ok end),
    ?assertEqual([], init_marking(p_done, State)).

%% API function tests
new_test() ->
    Fun = fun() -> ok end,
    State = new(Fun),
    ?assert(is_record(State, pattern_state)).

execute_test() ->
    Fun = fun(X) -> X * 2 end,
    ?assertEqual({ok, 10}, execute(Fun, 5)).

-endif.
```

### Doctest Best Practices

1. **Cover all callbacks** - Test each callback function
2. **Test edge cases** - Include unknown transitions/places
3. **Validate state records** - Verify record creation
4. **Test public API** - All exported functions should have tests
5. **Use ?assertEqual** - Prefer over ?assert for clearer failure messages
6. **Keep tests isolated** - Each test should be independent

---

## Code Templates

### Minimal Pattern Template

```erlang
%% -*- erlang -*-
-module(my_pattern).
-moduledoc """
My Pattern (WCP-XX) for YAWL.

Pattern description here...
""".
-behaviour(gen_yawl).

%% gen_yawl callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2, trigger/3]).
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3]).

%% API exports
-export([new/1, start/1, run/1, execute/2, get_state/1]).

%%====================================================================
%% Records
%%====================================================================

-record(my_pattern_state, {
    work_fun :: function(),
    result :: undefined | term(),
    log_id :: binary() | undefined
}).

-type my_pattern_state() :: #my_pattern_state{}.
-export_type([my_pattern_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec new(WorkFun :: function()) -> my_pattern_state().
new(WorkFun) when is_function(WorkFun) ->
    LogId = generate_log_id(),
    #my_pattern_state{
        work_fun = WorkFun,
        log_id = LogId
    }.

-spec start(WorkFun :: function()) -> {ok, pid()} | {error, term()}.
start(WorkFun) when is_function(WorkFun) ->
    State = new(WorkFun),
    gen_yawl:start_link(?MODULE, State, []).

-spec run(WorkFun :: function()) -> {ok, term()} | {error, term()}.
run(WorkFun) when is_function(WorkFun) ->
    case start(WorkFun) of
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

-spec get_state(Pid :: pid()) -> {ok, my_pattern_state()} | {error, term()}.
get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

-spec execute(WorkFun :: function(), Input :: term()) ->
          {ok, term()} | {error, term()}.
execute(WorkFun, Input) when is_function(WorkFun) ->
    try
        Result = WorkFun(Input),
        {ok, Result}
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%====================================================================
%% gen_yawl Callbacks
%%====================================================================

-spec place_lst() -> [atom()].
place_lst() ->
    ['p_start', 'p_active', 'p_done', 'p_complete'].

-spec trsn_lst() -> [atom()].
trsn_lst() ->
    ['t_start', 't_work', 't_complete'].

-spec init_marking(Place :: atom(), UsrInfo :: my_pattern_state()) -> [term()].
init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

-spec preset(Trsn :: atom()) -> [atom()].
preset('t_start') -> ['p_start'];
preset('t_work') -> ['p_active'];
preset('t_complete') -> ['p_done'];
preset(_) -> [].

-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: my_pattern_state()) ->
          boolean().
is_enabled('t_start', _Mode, _UsrInfo) ->
    true;
is_enabled('t_work', #{'p_active' := [work]}, _UsrInfo) ->
    true;
is_enabled('t_complete', #{'p_done' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: my_pattern_state()) ->
          abort | {produce, map()} | {produce, map(), my_pattern_state()}.
fire('t_start', #{'p_start' := [start]}, State) ->
    log_event(State, <<"MyPattern">>, <<"Start">>, #{}),
    {produce, #{
        'p_start' => [],
        'p_active' => [work]
    }, State};

fire('t_work', #{'p_active' := [work]}, #my_pattern_state{work_fun = Fun} = State) ->
    Result = try Fun() catch _:_ -> {error, failed} end,
    log_event(State, <<"MyPattern">>, <<"Work">>, #{<<"result">> => Result}),
    {produce, #{
        'p_active' => [],
        'p_done' => [Result]
    }, State#my_pattern_state{result = Result}};

fire('t_complete', #{'p_done' := [Result]}, State) ->
    log_event(State, <<"MyPattern">>, <<"Complete">>, #{}),
    {produce, #{
        'p_done' => [],
        'p_complete' => [done]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: my_pattern_state()) ->
          pass | drop.
trigger(_Place, _Token, _UsrInfo) ->
    pass.

-spec init(UsrInfo :: my_pattern_state()) -> {ok, my_pattern_state()}.
init(MyPatternState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"MyPattern">>}) of
        {ok, LogId} ->
            State1 = MyPatternState#my_pattern_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, MyPatternState}
    end.

-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.
handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {ok, UsrInfo}, NetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

-spec handle_cast(Request :: term(), NetState :: term()) ->
          noreply | {noreply, map()}.
handle_cast(_Request, NetState) ->
    {noreply, NetState}.

-spec handle_info(Info :: term(), NetState :: term()) ->
          noreply | {noreply, map()}.
handle_info(_Request, NetState) ->
    {noreply, NetState}.

-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.
code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

-spec terminate(Reason :: term(), NetState :: term()) -> ok.
terminate(_Reason, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #my_pattern_state{log_id = LogId} when LogId =/= undefined ->
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        _ ->
            ok
    end,
    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

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
                        #my_pattern_state{result = Result} ->
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

-spec generate_log_id() -> binary().
generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"my_pattern_", Hex/binary>>.

-spec generate_case_id() -> binary().
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

-spec log_event(State :: my_pattern_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) -> ok.
log_event(#my_pattern_state{log_id = LogId}, Concept, Lifecycle, Data)
  when LogId =/= undefined ->
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

place_lst_test() ->
    Expected = [p_start, p_active, p_done, p_complete],
    ?assertEqual(Expected, place_lst()).

trsn_lst_test() ->
    Expected = [t_start, t_work, t_complete],
    ?assertEqual(Expected, trsn_lst()).

preset_t_start_test() ->
    ?assertEqual([p_start], preset('t_start')).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

init_marking_p_start_test() ->
    State = new(fun() -> ok end),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_other_place_test() ->
    State = new(fun() -> ok end),
    ?assertEqual([], init_marking(p_done, State)).

new_test() ->
    Fun = fun() -> ok end,
    State = new(Fun),
    ?assert(is_record(State, my_pattern_state)).

execute_test() ->
    Fun = fun(X) -> X * 2 end,
    ?assertEqual({ok, 10}, execute(Fun, 5)).

-endif.
```

---

## Common Pitfalls and How to Avoid Them

### 1. Missing Place/Transition in Callbacks

**Problem:** A place or transition is added to `place_lst()` or `trsn_lst()` but missing from other callbacks.

**Solution:** Always update all callbacks together:
- When adding a place: add to `place_lst()`, `init_marking/2`, and at least one `preset()`
- When adding a transition: add to `trsn_lst()`, `preset()`, `is_enabled/3`, and `fire/3`

### 2. Forgetting to Empty Places in `fire/3`

**Problem:** Tokens are not removed from input places, causing infinite loops.

**Solution:** Always include consumed places in the produce map with empty lists:

```erlang
% WRONG - tokens remain in p_start
fire('t_start', #{'p_start' := [start]}, State) ->
    {produce, #{
        'p_active' => [work]
    }, State}.

% CORRECT - tokens consumed from p_start
fire('t_start', #{'p_start' := [start]}, State) ->
    {produce, #{
        'p_start' => [],   % Consume the token
        'p_active' => [work]
    }, State}.
```

### 3. Incorrect Mode Matching

**Problem:** `is_enabled/3` and `fire/3` use different patterns for the same mode.

**Solution:** Use identical mode patterns in both callbacks:

```erlang
% Both must match the same mode structure
is_enabled('t_work', #{'p_active' := [work]}, _UsrInfo) -> true;
fire('t_work', #{'p_active' := [work]}, State) -> ...
```

### 4. Missing XES Log Cleanup

**Problem:** XES logs are not closed on termination, causing memory leaks.

**Solution:** Always close logs in `terminate/2`:

```erlang
terminate(_Reason, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #pattern_state{log_id = LogId} when LogId =/= undefined ->
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        _ ->
            ok
    end,
    ok.
```

### 5. Not Using 3-Tuple Return for State Updates

**Problem:** State updates are lost because `fire/3` only returns 2-tuple.

**Solution:** Use 3-tuple return when state needs to change:

```erlang
% For state updates
fire('t_work', Mode, #pattern_state{count = Count} = State) ->
    NewState = State#pattern_state{count = Count + 1},
    {produce, ProduceMap, NewState}.
```

### 6. Assuming Token Order

**Problem:** Code assumes tokens are in a specific order in a place.

**Solution:** Tokens are a multiset - order is not guaranteed. Always match by pattern:

```erlang
% WRONG - assumes order
fire('t_join', #{'p_a' := [TokenA], 'p_b' := [TokenB]}, State) -> ...

% CORRECT - matches regardless of order
fire('t_join', #{'p_a' := [TokenA], 'p_b' := [TokenB]}, State) when
    is_tuple(TokenA), is_tuple(TokenB) -> ...
```

### 7. Forgetting Doctests

**Problem:** New functions don't have corresponding tests.

**Solution:** Add tests for all exported functions in the `-ifdef(TEST)` section.

### 8. Infinite Loops in Petri Nets

**Problem:** A transition always produces tokens that enable it again.

**Solution:** Ensure progress - tokens should move toward completion:

```erlang
% WRONG - infinite loop
fire('t_step', #{'p_ready' := [_]}, State) ->
    {produce, #{
        'p_ready' => [step]  % Produces what it consumes
    }, State}.

% CORRECT - progress toward completion
fire('t_step', #{'p_ready' := [_]}, State) ->
    {produce, #{
        'p_ready' => [],
        'p_done' => [step]
    }, State}.
```

---

## Testing Patterns

### Running Unit Tests

```bash
# Run all tests
rebar3 eunit

# Run specific pattern tests
rebar3 eunit --module=my_pattern

# Run with coverage
rebar3 cover
```

### Manual Testing with the Erlang Shell

```erlang
# Start the application
rebar3 shell

# Create and start a pattern
State = my_pattern:new(fun(X) -> X * 2 end),
{ok, Pid} = gen_yawl:start_link(my_pattern, State, []).

# Query the state
{ok, CurrentState} = gen_yawl:call(Pid, get_state).

# Query the marking
Marking = gen_yawl:marking(Pid).

# Query a specific place
{ok, Tokens} = gen_yawl:ls(Pid, 'p_active').

# Stop the pattern
gen_yawl:stop(Pid).
```

### Integration Testing

Test patterns composed together:

```erlang
% Test pattern composition
SplitState = parallel_split:new([fun() -> a end, fun() -> b end], 2),
{ok, SplitPid} = gen_yawl:start_link(parallel_split, SplitState, []),

% Wait for completion and verify
{ok, FinalMarking} = gen_yawl:sync(SplitPid, 5000),
% Verify expected marking...
```

---

## References

- **gen_pnet User Guide**: `/Users/sac/cre/docs/GEN_PNET_USER_GUIDE.md`
- **YAWL Patterns Reference**: `/Users/sac/cre/docs/YAWL_PATTERNS_REFERENCE.md`
- **Source Examples**: `/Users/sac/cre/src/patterns/*.erl`
- **XES Module**: `/Users/sac/cre/src/yawl_xes.erl`
- **gen_yawl Behavior**: `/Users/sac/cre/src/core/gen_yawl.erl`
