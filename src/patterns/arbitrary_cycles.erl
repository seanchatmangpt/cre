%% -*- erlang -*-
%% @doc Arbitrary Cycles Pattern (WCP-10) for YAWL.
%%
%% This module implements the Arbitrary Cycles pattern (WCP-10) as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Arbitrary Cycles pattern allows a token to be routed back to any previous
%% place in the workflow, not just the immediately preceding one. This enables
%% complex loop structures where multiple cycle paths exist.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Initial place, starts the cycle
%%     p_cycle_pool     - Pool of available cycle targets
%%     p_cycle_active    - Active cycle execution
%%     p_end           - Final place, workflow complete
%%
%%   Transitions:
%%     t_start         - Initialize the cycle pool
%%     t_cycle         - Execute a cycle to a target node
%%     t_exit          - Exit the cycle structure
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always reachable via t_exit</li>
%%   <li><b>Proper completion:</b> No tokens remain in cycle pool on exit</li>
%%   <li><b>No dead transitions:</b> All transitions fireable when conditions met</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(arbitrary_cycles).
-moduledoc """
Arbitrary Cycles Pattern (WCP-10) for YAWL.

This pattern enables tokens to cycle back to any previous node in the workflow,
not just the immediately preceding one.

## Example

```erlang
%% Create an arbitrary cycles pattern with specific cycle targets
State = arbitrary_cycles:new([node_a, node_b, node_c]),
{ok, Pid} = gen_yawl:start_link(arbitrary_cycles, State, []).

%% Execute a cycle back to node_a
ok = gen_yawl:cast(Pid, {cycle_to, node_a}).

%% Exit the cycle structure
ok = gen_yawl:cast(Pid, exit).
```

## API

- `new/1` - Create new arbitrary cycles state with target nodes
- `add_cycle_target/2` - Add a new cycle target
- `remove_cycle_target/2` - Remove a cycle target
- `get_cycle_targets/1` - Get list of available cycle targets
- `get_cycle_count/1` - Get number of cycles executed

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
    new/1,
    add_cycle_target/2,
    remove_cycle_target/2,
    get_cycle_targets/1,
    get_cycle_count/1,
    can_cycle_to/2,
    cycle_to/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(cycle_state, {
    nodes :: [atom()],
    cycles :: #{atom() => [atom()]},
    cycle_count = 0 :: non_neg_integer(),
    current_cycle :: atom() | undefined,
    log_id :: binary() | undefined
}).

-type cycle_state() :: #cycle_state{}.
-export_type([cycle_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Arbitrary Cycles pattern state.

## Example

```erlang
> State = arbitrary_cycles:new([node_a, node_b, node_c]).
#cycle_state{nodes = [node_a, node_b, node_c], cycle_count = 0, ...}
```
""".
-spec new(Nodes :: [atom()]) -> cycle_state().
new(Nodes) when is_list(Nodes) ->
    LogId = generate_log_id(),
    #cycle_state{
        nodes = Nodes,
        cycles = build_cycle_map(Nodes),
        cycle_count = 0,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Adds a new cycle target to the pattern.

## Example

```erlang
> State0 = arbitrary_cycles:new([node_a]),
> State1 = arbitrary_cycles:add_cycle_target(node_b, State0).
#cycle_state{nodes = [node_a, node_b], ...}
```
""".
-spec add_cycle_target(Node :: atom(), State :: cycle_state()) -> cycle_state().
add_cycle_target(Node, #cycle_state{nodes = Nodes, cycles = Cycles} = State) ->
    State#cycle_state{
        nodes = [Node | Nodes],
        cycles = maps:put(Node, Nodes -- [Node], Cycles)
    }.

%%--------------------------------------------------------------------
-doc """
Removes a cycle target from the pattern.

## Example

```erlang
> State0 = arbitrary_cycles:new([node_a, node_b]),
> State1 = arbitrary_cycles:remove_cycle_target(node_b, State0).
#cycle_state{nodes = [node_a], ...}
```
""".
-spec remove_cycle_target(Node :: atom(), State :: cycle_state()) -> cycle_state().
remove_cycle_target(Node, #cycle_state{nodes = Nodes, cycles = Cycles} = State) ->
    State#cycle_state{
        nodes = lists:delete(Node, Nodes),
        cycles = maps:map(fun(_K, V) -> lists:delete(Node, V) end, Cycles)
    }.

%%--------------------------------------------------------------------
-doc """
Gets the list of available cycle targets.

## Example

```erlang
> State = arbitrary_cycles:new([node_a, node_b, node_c]),
> arbitrary_cycles:get_cycle_targets(State).
[node_a, node_b, node_c]
```
""".
-spec get_cycle_targets(State :: cycle_state()) -> [atom()].
get_cycle_targets(#cycle_state{nodes = Nodes}) ->
    Nodes.

%%--------------------------------------------------------------------
-doc """
Gets the number of cycles executed so far.

## Example

```erlang
> State = #cycle_state{cycle_count = 5},
> arbitrary_cycles:get_cycle_count(State).
5
```
""".
-spec get_cycle_count(State :: cycle_state()) -> non_neg_integer().
get_cycle_count(#cycle_state{cycle_count = Count}) ->
    Count.

%%--------------------------------------------------------------------
-doc """
Checks if a cycle to the specified node is possible.

## Example

```erlang
> State = arbitrary_cycles:new([node_a, node_b]),
> arbitrary_cycles:can_cycle_to(node_a, State).
true
> arbitrary_cycles:can_cycle_to(node_c, State).
false
```
""".
-spec can_cycle_to(Node :: atom(), State :: cycle_state()) -> boolean().
can_cycle_to(Node, #cycle_state{nodes = Nodes}) ->
    lists:member(Node, Nodes).

%%--------------------------------------------------------------------
-doc """
Executes a cycle to the specified target node.

Returns an updated state with incremented cycle count.

## Example

```erlang
> State0 = arbitrary_cycles:new([node_a, node_b]),
> State1 = arbitrary_cycles:cycle_to(node_a, State0).
#cycle_state{cycle_count = 1, current_cycle = node_a, ...}
```
""".
-spec cycle_to(Node :: atom(), State :: cycle_state()) -> cycle_state().
cycle_to(Node, #cycle_state{cycle_count = Count, log_id = LogId} = State) ->
    log_event(LogId, <<"ArbitraryCycles">>, <<"Cycle">>, #{
        <<"target">> => atom_to_binary(Node),
        <<"cycle_count">> => Count + 1
    }),
    State#cycle_state{
        cycle_count = Count + 1,
        current_cycle = Node
    }.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Arbitrary Cycles Petri net.

```erlang
> arbitrary_cycles:place_lst().
[p_start, p_cycle_pool, p_cycle_active, p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [p_start, p_cycle_pool, p_cycle_active, p_end].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Arbitrary Cycles Petri net.

```erlang
> arbitrary_cycles:trsn_lst().
[t_start, t_cycle, t_exit]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [t_start, t_cycle, t_exit].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: cycle_state()) ->
          [term()].

init_marking(p_start, _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> arbitrary_cycles:preset(t_start).
[p_start]
> arbitrary_cycles:preset(t_cycle).
[p_cycle_pool]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset(t_start) -> [p_start];
preset(t_cycle) -> [p_cycle_pool];
preset(t_exit) -> [p_cycle_pool];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: cycle_state()) ->
          boolean().

is_enabled(t_start, _Mode, _UsrInfo) ->
    true;
is_enabled(t_cycle, #{p_cycle_pool := [_Token]}, #cycle_state{nodes = Nodes}) when length(Nodes) > 0 ->
    true;
is_enabled(t_exit, #{p_cycle_pool := [_Token]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: cycle_state()) ->
          {produce, map()} | {produce, map(), cycle_state()} | abort.

fire(t_start, #{p_start := [start]}, #cycle_state{log_id = LogId} = State) ->
    log_event(LogId, <<"ArbitraryCycles">>, <<"Start">>, #{}),
    {produce, #{
        p_start => [],
        p_cycle_pool => [cycle_token]
    }, State};

fire(t_cycle, #{p_cycle_pool := [cycle_token]}, #cycle_state{current_cycle = Current, log_id = LogId} = State) ->
    Target = case Current of
        undefined -> hd(State#cycle_state.nodes);
        _ -> Current
    end,
    log_event(LogId, <<"ArbitraryCycles">>, <<"CycleExecute">>, #{
        <<"target">> => atom_to_binary(Target)
    }),
    {produce, #{
        p_cycle_pool => [cycle_token],
        p_cycle_active => [{cycle, Target}]
    }, State};

fire(t_exit, #{p_cycle_pool := [cycle_token]}, #cycle_state{cycle_count = Count, log_id = LogId} = State) ->
    log_event(LogId, <<"ArbitraryCycles">>, <<"Exit">>, #{
        <<"total_cycles">> => Count
    }),
    {produce, #{
        p_cycle_pool => [],
        p_end => [done]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), NetState :: cycle_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: cycle_state()) ->
          {ok, cycle_state()}.

init(CycleState) ->
    LogId = CycleState#cycle_state.log_id,
    case yawl_xes:new_log(#{<<"process">> => <<"ArbitraryCycles">>}) of
        {ok, XesLogId} ->
            State1 = CycleState#cycle_state{log_id = XesLogId},
            yawl_xes:log_case_start(XesLogId, generate_case_id()),
            {ok, State1};
        _ when LogId =/= undefined ->
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, CycleState};
        _ ->
            {ok, CycleState}
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_cycle_targets, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Targets = get_cycle_targets(UsrInfo),
    {reply, {ok, Targets}, NetState};
handle_call(get_cycle_count, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Count = get_cycle_count(UsrInfo),
    {reply, {ok, Count}, NetState};
handle_call({cycle_to, Node}, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case can_cycle_to(Node, UsrInfo) of
        true ->
            NewUsrInfo = cycle_to(Node, UsrInfo),
            NewNetState = gen_yawl:set_usr_info(NetState, NewUsrInfo),
            {reply, ok, NewNetState};
        false ->
            {reply, {error, invalid_target}, NetState}
    end;
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_cast({cycle_to, Node}, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case can_cycle_to(Node, UsrInfo) of
        true ->
            NewUsrInfo = cycle_to(Node, UsrInfo),
            NewNetState = gen_yawl:set_usr_info(NetState, NewUsrInfo),
            {noreply, NewNetState};
        false ->
            {noreply, NetState}
    end;
handle_cast(exit, NetState) ->
    %% Trigger exit transition
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
        #cycle_state{log_id = LogId} when LogId =/= undefined ->
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
%% @doc Builds a cycle map from a list of nodes.
%% Each node can cycle to any other node.
%% @end
%%--------------------------------------------------------------------
-spec build_cycle_map([atom()]) -> #{atom() => [atom()]}.
build_cycle_map(Nodes) ->
    lists:foldl(fun(Node, Acc) ->
        Acc#{Node => lists:delete(Node, Nodes)}
    end, #{}, Nodes).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique log ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().
generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"arbitrary_cycles_", Hex/binary>>.

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
%% Test new/1
%%--------------------------------------------------------------------
new_test() ->
    State = new([node_a, node_b]),
    ?assert(is_record(State, cycle_state)),
    ?assertEqual([node_a, node_b], State#cycle_state.nodes),
    ?assertEqual(0, State#cycle_state.cycle_count).

%%--------------------------------------------------------------------
%% Test add_cycle_target/2
%%--------------------------------------------------------------------
add_cycle_target_test() ->
    State0 = new([node_a]),
    State1 = add_cycle_target(node_b, State0),
    ?assert(lists:member(node_b, State1#cycle_state.nodes)),
    ?assertEqual(2, length(State1#cycle_state.nodes)).

%%--------------------------------------------------------------------
%% Test remove_cycle_target/2
%%--------------------------------------------------------------------
remove_cycle_target_test() ->
    State0 = new([node_a, node_b]),
    State1 = remove_cycle_target(node_b, State0),
    ?assertNot(lists:member(node_b, State1#cycle_state.nodes)),
    ?assertEqual(1, length(State1#cycle_state.nodes)).

%%--------------------------------------------------------------------
%% Test get_cycle_targets/1
%%--------------------------------------------------------------------
get_cycle_targets_test() ->
    State = new([node_a, node_b, node_c]),
    ?assertEqual([node_a, node_b, node_c], get_cycle_targets(State)).

%%--------------------------------------------------------------------
%% Test get_cycle_count/1
%%--------------------------------------------------------------------
get_cycle_count_test() ->
    State = #cycle_state{cycle_count = 5},
    ?assertEqual(5, get_cycle_count(State)).

%%--------------------------------------------------------------------
%% Test can_cycle_to/2
%%--------------------------------------------------------------------
can_cycle_to_test() ->
    State = new([node_a, node_b]),
    ?assert(can_cycle_to(node_a, State)),
    ?assert(can_cycle_to(node_b, State)),
    ?assertNot(can_cycle_to(node_c, State)).

%%--------------------------------------------------------------------
%% Test cycle_to/2
%%--------------------------------------------------------------------
cycle_to_test() ->
    State0 = #cycle_state{cycle_count = 0, log_id = undefined},
    State1 = cycle_to(node_a, State0),
    ?assertEqual(1, State1#cycle_state.cycle_count),
    ?assertEqual(node_a, State1#cycle_state.current_cycle).

%%--------------------------------------------------------------------
%% Test place_lst/0
%%--------------------------------------------------------------------
place_lst_test() ->
    Places = place_lst(),
    ?assert(lists:member(p_start, Places)),
    ?assert(lists:member(p_cycle_pool, Places)),
    ?assert(lists:member(p_cycle_active, Places)),
    ?assert(lists:member(p_end, Places)).

%%--------------------------------------------------------------------
%% Test trsn_lst/0
%%--------------------------------------------------------------------
trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assert(lists:member(t_start, Transitions)),
    ?assert(lists:member(t_cycle, Transitions)),
    ?assert(lists:member(t_exit, Transitions)).

%%--------------------------------------------------------------------
%% Test preset/1
%%--------------------------------------------------------------------
preset_test() ->
    ?assertEqual([p_start], preset(t_start)),
    ?assertEqual([p_cycle_pool], preset(t_cycle)),
    ?assertEqual([p_cycle_pool], preset(t_exit)),
    ?assertEqual([], preset(undefined)).

%%--------------------------------------------------------------------
%% Test build_cycle_map/1
%%--------------------------------------------------------------------
build_cycle_map_test() ->
    Nodes = [a, b, c],
    Map = build_cycle_map(Nodes),
    ?assertEqual(3, map_size(Map)),
    ?assert(lists:member(b, maps:get(a, Map))),
    ?assert(lists:member(c, maps:get(a, Map))),
    ?assertNot(lists:member(a, maps:get(a, Map))).

-endif.
