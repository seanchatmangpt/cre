%% -*- erlang -*-
%% @doc Recursion Pattern (WCP-22) for YAWL.
%%
%% This module implements Recursion pattern (WCP-22) as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Recursion pattern enables a workflow to call itself recursively with new parameters.
%% This allows solving complex problems through divide-and-conquer approaches.
%% Depth tracking prevents infinite recursion.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Initial place, starts recursion
%%     p_call_ready     - Ready to make recursive call
%%     p_recursing      - Recursion in progress
%%     p_returned       - Returned from recursive call
%%     p_end           - Final place, recursion complete
%%
%%   Transitions:
%%     t_start         - Initialize recursion
%%     t_call          - Make recursive call
%%     t_return        - Return from recursive call
%%     t_finish        - Complete recursion
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always reachable via base case</li>
%%   <li><b>Proper completion:</b> No pending recursive calls on exit</li>
%%   <li><b>No dead transitions:</b> All transitions fireable when conditions met</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(recursion).
-moduledoc """
Recursion Pattern (WCP-22) for YAWL.

This pattern enables a workflow to call itself recursively with new parameters,
similar to function recursion in programming languages.

## Example

```erlang
%% Create a recursion pattern with max depth of 10
State = recursion:new(factorial, 10),
{ok, Pid} = gen_yawl:start_link(recursion, State, []).

%% Make a recursive call with new arguments
ok = gen_yawl:cast(Pid, {recurse, 5}).

%% Get current recursion depth
{ok, Depth} = gen_yawl:call(Pid, get_depth).
```

## API

- `new/2` - Create new recursion state with workflow and max depth
- `get_depth/1` - Get current recursion depth
- `get_max_depth/1` - Get maximum allowed depth
- `set_max_depth/2` - Set new maximum depth
- `can_recurse/1` - Check if another recursion is allowed
- `increment_depth/1` - Increment recursion depth
- `decrement_depth/1` - Decrement recursion depth

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

%% gen_yawl callbacks
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
    get_depth/1,
    get_max_depth/1,
    set_max_depth/2,
    can_recurse/1,
    increment_depth/1,
    decrement_depth/1,
    reset_depth/1
]).

%%====================================================================
%% Records
%%====================================================================

-record(recursion_state, {
    call :: atom(),
    depth = 0 :: non_neg_integer(),
    max_depth = 100 :: pos_integer(),
    base_case = false :: boolean(),
    log_id :: binary() | undefined
}).

-type recursion_state() :: #recursion_state{}.
-export_type([recursion_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Recursion pattern state.

## Example

```erlang
> State = recursion:new(factorial, 100).
#recursion_state{call = factorial, max_depth = 100, depth = 0, ...}
```
""".
-spec new(Call :: atom(), MaxDepth :: pos_integer()) -> recursion_state().
new(Call, MaxDepth) when is_atom(Call), is_integer(MaxDepth), MaxDepth > 0 ->
    LogId = generate_log_id(),
    #recursion_state{
        call = Call,
        max_depth = MaxDepth,
        depth = 0,
        base_case = false,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Gets the current recursion depth.

## Example

```erlang
> State = #recursion_state{depth = 5},
> recursion:get_depth(State).
5
```
""".
-spec get_depth(State :: recursion_state()) -> non_neg_integer().
get_depth(#recursion_state{depth = Depth}) ->
    Depth.

%%--------------------------------------------------------------------
-doc """
Gets the maximum allowed recursion depth.

## Example

```erlang
> State = #recursion_state{max_depth = 100},
> recursion:get_max_depth(State).
100
```
""".
-spec get_max_depth(State :: recursion_state()) -> pos_integer().
get_max_depth(#recursion_state{max_depth = MaxDepth}) ->
    MaxDepth.

%%--------------------------------------------------------------------
-doc """
Sets a new maximum recursion depth.

## Example

```erlang
> State0 = recursion:new(factorial, 100),
> State1 = recursion:set_max_depth(200, State0).
#recursion_state{max_depth = 200, ...}
```
""".
-spec set_max_depth(MaxDepth :: pos_integer(), State :: recursion_state()) -> recursion_state().
set_max_depth(MaxDepth, State) when is_integer(MaxDepth), MaxDepth > 0 ->
    State#recursion_state{max_depth = MaxDepth}.

%%--------------------------------------------------------------------
-doc """
Checks if another recursion level is allowed (depth < max_depth).

## Example

```erlang
> State0 = recursion:new(factorial, 10),
> recursion:can_recurse(State0).
true
> State1 = State0#recursion_state{depth = 10},
> recursion:can_recurse(State1).
false
```
""".
-spec can_recurse(State :: recursion_state()) -> boolean().
can_recurse(#recursion_state{depth = Depth, max_depth = MaxDepth}) ->
    Depth < MaxDepth.

%%--------------------------------------------------------------------
-doc """
Increments the recursion depth by 1.

## Example

```erlang
> State0 = #recursion_state{depth = 5},
> State1 = recursion:increment_depth(State0).
#recursion_state{depth = 6, ...}
```
""".
-spec increment_depth(State :: recursion_state()) -> recursion_state().
increment_depth(#recursion_state{depth = Depth} = State) ->
    State#recursion_state{depth = Depth + 1}.

%%--------------------------------------------------------------------
-doc """
Decrements the recursion depth by 1 (minimum 0).

## Example

```erlang
> State0 = #recursion_state{depth = 5},
> State1 = recursion:decrement_depth(State0).
#recursion_state{depth = 4, ...}
```
""".
-spec decrement_depth(State :: recursion_state()) -> recursion_state().
decrement_depth(#recursion_state{depth = Depth} = State) ->
    State#recursion_state{depth = max(0, Depth - 1)}.

%%--------------------------------------------------------------------
-doc """
Resets the recursion depth to 0.

## Example

```erlang
> State0 = #recursion_state{depth = 5},
> State1 = recursion:reset_depth(State0).
#recursion_state{depth = 0, ...}
```
""".
-spec reset_depth(State :: recursion_state()) -> recursion_state().
reset_depth(State) ->
    State#recursion_state{depth = 0}.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Recursion Petri net.

```erlang
> recursion:place_lst().
[p_start, p_call_ready, p_recursing, p_returned, p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [p_start, p_call_ready, p_recursing, p_returned, p_end].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Recursion Petri net.

```erlang
> recursion:trsn_lst().
[t_start, t_call, t_return, t_finish]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [t_start, t_call, t_return, t_finish].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: recursion_state()) ->
          [term()].

init_marking(p_start, _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> recursion:preset(t_start).
[p_start]
> recursion:preset(t_call).
[p_call_ready]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset(t_start) -> [p_start];
preset(t_call) -> [p_call_ready];
preset(t_return) -> [p_recursing];
preset(t_finish) -> [p_returned];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: recursion_state()) ->
          boolean().

is_enabled(t_start, _Mode, _UsrInfo) ->
    true;
is_enabled(t_call, #{p_call_ready := [ready]}, #recursion_state{depth = Depth, max_depth = MaxDepth}) when Depth < MaxDepth ->
    true;
is_enabled(t_return, #{p_recursing := [_Token]}, _UsrInfo) ->
    true;
is_enabled(t_finish, #{p_returned := [returned]}, #recursion_state{depth = 0}) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: recursion_state()) ->
          {produce, map()} | {produce, map(), recursion_state()} | abort.

fire(t_start, #{p_start := [start]}, #recursion_state{log_id = LogId} = State) ->
    log_event(LogId, <<"Recursion">>, <<"Start">>, #{
        <<"call">> => atom_to_binary(State#recursion_state.call)
    }),
    {produce, #{
        p_start => [],
        p_call_ready => [ready]
    }, State};

fire(t_call, #{p_call_ready := [ready]}, #recursion_state{depth = Depth, log_id = LogId} = State) ->
    NewDepth = Depth + 1,
    log_event(LogId, <<"Recursion">>, <<"Call">>, #{
        <<"depth">> => NewDepth
    }),
    {produce, #{
        p_call_ready => [],
        p_recursing => [{recursing, NewDepth}]
    }, State#recursion_state{depth = NewDepth}};

fire(t_return, #{p_recursing := [{recursing, _CurDepth}]}, #recursion_state{depth = Depth, log_id = LogId} = State) ->
    NewDepth = max(0, Depth - 1),
    log_event(LogId, <<"Recursion">>, <<"Return">>, #{
        <<"depth">> => NewDepth
    }),
    BaseCase = NewDepth =:= 0,
    {produce, #{
        p_recursing => [],
        p_returned => [returned]
    }, State#recursion_state{depth = NewDepth, base_case = BaseCase}};

fire(t_finish, #{p_returned := [returned]}, #recursion_state{call = Call, depth = 0, log_id = LogId} = State) ->
    log_event(LogId, <<"Recursion">>, <<"Complete">>, #{
        <<"call">> => atom_to_binary(Call),
        <<"final_depth">> => 0
    }),
    {produce, #{
        p_returned => [],
        p_end => [done]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), NetState :: recursion_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: recursion_state()) ->
          {ok, recursion_state()}.

init(RecursionState) ->
    LogId = RecursionState#recursion_state.log_id,
    case yawl_xes:new_log(#{<<"process">> => <<"Recursion">>}) of
        {ok, XesLogId} ->
            State1 = RecursionState#recursion_state{log_id = XesLogId},
            yawl_xes:log_case_start(XesLogId, generate_case_id()),
            {ok, State1};
        _ when LogId =/= undefined ->
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, RecursionState};
        _ ->
            {ok, RecursionState}
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_depth, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Depth = get_depth(UsrInfo),
    {reply, {ok, Depth}, NetState};
handle_call(get_max_depth, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    MaxDepth = get_max_depth(UsrInfo),
    {reply, {ok, MaxDepth}, NetState};
handle_call({set_max_depth, MaxDepth}, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    NewUsrInfo = set_max_depth(MaxDepth, UsrInfo),
    NewNetState = gen_yawl:set_usr_info(NetState, NewUsrInfo),
    {reply, ok, NewNetState};
handle_call(can_recurse, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    CanRecurse = can_recurse(UsrInfo),
    {reply, {ok, CanRecurse}, NetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_cast({recurse, _Args}, NetState) ->
    %% Trigger recursive call transition
    {noreply, NetState};
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
        #recursion_state{log_id = LogId} when LogId =/= undefined ->
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
%% @private
%% @doc Generates a unique log ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().
generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"recursion_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique case ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id() -> binary().
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Logs an XES event.
%% @end
%%--------------------------------------------------------------------
-spec log_event(LogId :: binary() | undefined, Concept :: binary(),
                Lifecycle :: binary(), Data :: map()) -> ok.
log_event(undefined, _Concept, _Lifecycle, _Data) ->
    ok;
log_event(LogId, Concept, Lifecycle, Data) ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test new/2
%%--------------------------------------------------------------------
new_test() ->
    State = new(factorial, 100),
    ?assert(is_record(State, recursion_state)),
    ?assertEqual(factorial, State#recursion_state.call),
    ?assertEqual(100, State#recursion_state.max_depth),
    ?assertEqual(0, State#recursion_state.depth).

%%--------------------------------------------------------------------
%% Test get_depth/1
%%--------------------------------------------------------------------
get_depth_test() ->
    State = #recursion_state{depth = 5},
    ?assertEqual(5, get_depth(State)).

%%--------------------------------------------------------------------
%% Test get_max_depth/1
%%--------------------------------------------------------------------
get_max_depth_test() ->
    State = #recursion_state{max_depth = 100},
    ?assertEqual(100, get_max_depth(State)).

%%--------------------------------------------------------------------
%% Test set_max_depth/2
%%--------------------------------------------------------------------
set_max_depth_test() ->
    State0 = #recursion_state{max_depth = 100},
    State1 = set_max_depth(200, State0),
    ?assertEqual(200, State1#recursion_state.max_depth).

%%--------------------------------------------------------------------
%% Test can_recurse/1
%%--------------------------------------------------------------------
can_recurse_test() ->
    State0 = #recursion_state{depth = 5, max_depth = 10},
    ?assert(can_recurse(State0)),
    State1 = #recursion_state{depth = 10, max_depth = 10},
    ?assertNot(can_recurse(State1)),
    State2 = #recursion_state{depth = 11, max_depth = 10},
    ?assertNot(can_recurse(State2)).

%%--------------------------------------------------------------------
%% Test increment_depth/1
%%--------------------------------------------------------------------
increment_depth_test() ->
    State0 = #recursion_state{depth = 5},
    State1 = increment_depth(State0),
    ?assertEqual(6, State1#recursion_state.depth).

%%--------------------------------------------------------------------
%% Test decrement_depth/1
%%--------------------------------------------------------------------
decrement_depth_test() ->
    State0 = #recursion_state{depth = 5},
    State1 = decrement_depth(State0),
    ?assertEqual(4, State1#recursion_state.depth),
    %% Test floor at 0
    State2 = decrement_depth(#recursion_state{depth = 0}),
    ?assertEqual(0, State2#recursion_state.depth).

%%--------------------------------------------------------------------
%% Test reset_depth/1
%%--------------------------------------------------------------------
reset_depth_test() ->
    State0 = #recursion_state{depth = 5},
    State1 = reset_depth(State0),
    ?assertEqual(0, State1#recursion_state.depth).

%%--------------------------------------------------------------------
%% Test place_lst/0
%%--------------------------------------------------------------------
place_lst_test() ->
    Places = place_lst(),
    ?assert(lists:member(p_start, Places)),
    ?assert(lists:member(p_call_ready, Places)),
    ?assert(lists:member(p_recursing, Places)),
    ?assert(lists:member(p_returned, Places)),
    ?assert(lists:member(p_end, Places)).

%%--------------------------------------------------------------------
%% Test trsn_lst/0
%%--------------------------------------------------------------------
trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assert(lists:member(t_start, Transitions)),
    ?assert(lists:member(t_call, Transitions)),
    ?assert(lists:member(t_return, Transitions)),
    ?assert(lists:member(t_finish, Transitions)).

%%--------------------------------------------------------------------
%% Test preset/1
%%--------------------------------------------------------------------
preset_test() ->
    ?assertEqual([p_start], preset(t_start)),
    ?assertEqual([p_call_ready], preset(t_call)),
    ?assertEqual([p_recursing], preset(t_return)),
    ?assertEqual([p_returned], preset(t_finish)),
    ?assertEqual([], preset(undefined)).

-endif.
