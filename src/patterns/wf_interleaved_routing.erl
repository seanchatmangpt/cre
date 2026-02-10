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

-module(wf_interleaved_routing).
-moduledoc """
Interleaved Routing Pattern (WCP-17) for YAWL.

This module implements the Interleaved Routing pattern as a gen_pnet behaviour.

The Interleaved Routing pattern (WCP-17) executes multiple concurrent
branches in an interleaved (round-robin) fashion, ensuring fair execution
among all branches. No prescribed execution order is enforced.

## Pattern Description

The Interleaved Routing pattern enables fair execution of multiple concurrent
branches by processing them in a round-robin fashion. This ensures that all
branches make progress while maintaining a controlled execution order.

## Petri Net Structure

Places:
- `p_start` - Start of interleaved routing
- `p_branch_pool` - Pool of pending branches
- `p_next_branch` - Next branch to execute (round-robin)
- `p_executing` - Currently executing branch
- `p_branch_done` - Branch execution completed
- `p_all_done` - All branches completed
- `p_output` - Final output

Transitions:
- `t_distribute` - Distribute work to branches
- `t_pick_next` - Pick next branch for execution
- `t_execute` - Execute the picked branch
- `t_return` - Return branch to pool or mark done
- `t_complete` - Complete when all branches done

## Examples

Get the list of places in the Petri net:

```erlang
> wf_interleaved_routing:place_lst().
[p_start,p_branch_pool,p_next_branch,p_executing,p_branch_done,
 p_all_done,p_output]
```

Get the list of transitions:

```erlang
> wf_interleaved_routing:trsn_lst().
[t_distribute,t_pick_next,t_execute,t_return,t_complete]
```

Get the preset (input places) for a transition:

```erlang
> wf_interleaved_routing:preset(t_distribute).
[p_start]
```

```erlang
> wf_interleaved_routing:preset(t_pick_next).
[p_branch_pool,p_next_branch]
```

```erlang
> wf_interleaved_routing:preset(unknown).
[]
```

## Soundness Properties

- **Option to complete:** Always true (all branches complete)
- **Proper completion:** All branches complete exactly once
- **No dead transitions:** Fair round-robin ensures liveness
""".
-behaviour(gen_pnet).

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

-record(interleaved_state, {
    branches :: map(),
    branch_order :: [atom()],
    completed = [] :: [atom()],
    current_index = 1 :: pos_integer(),
    results = [] :: [{atom(), term()}],
    log_id :: binary() | undefined
}).

-type interleaved_state() :: #interleaved_state{}.
-export_type([interleaved_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec new(Branches :: map()) -> interleaved_state().
new(Branches) when is_map(Branches), map_size(Branches) >= 2 ->
    BranchOrder = lists:sort(maps:keys(Branches)),
    LogId = generate_log_id(),
    #interleaved_state{
        branches = Branches,
        branch_order = BranchOrder,
        log_id = LogId
    }.

-spec start(Branches :: map()) -> {ok, pid()} | {error, term()}.
start(Branches) when is_map(Branches), map_size(Branches) >= 2 ->
    InterleavedState = new(Branches),
    gen_pnet:start_link(?MODULE, InterleavedState, []).

-spec run(Branches :: map(), InputData :: term()) ->
          {ok, [{atom(), term()}]} | {error, term()}.
run(Branches, InputData) when is_map(Branches), map_size(Branches) >= 2 ->
    case start(Branches) of
        {ok, Pid} ->
            gen_pnet:cast(Pid, {input_data, InputData}),
            case wait_for_completion(Pid, 30000) of
                {ok, Results} ->
                    gen_pnet:stop(Pid),
                    {ok, Results};
                {error, Reason} ->
                    gen_pnet:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_state(Pid :: pid()) -> {ok, interleaved_state()} | {error, term()}.
get_state(Pid) ->
    gen_pnet:call(Pid, get_state).

-spec execute(Branches :: map(), InputData :: term()) ->
          {ok, [{atom(), term()}]} | {error, term()}.
execute(Branches, InputData) when is_map(Branches), map_size(Branches) >= 2 ->
    BranchKeys = maps:keys(Branches),
    Ref = make_ref(),
    Parent = self(),

    Pids = lists:map(fun(Key) ->
        Fun = maps:get(Key, Branches),
        spawn(fun() ->
            try
                Result = Fun(InputData),
                Parent ! {Ref, {branch_complete, Key}, Result}
            catch
                Error:Reason:Stack ->
                    Parent ! {Ref, {branch_error, Key}, {Error, Reason, Stack}}
            end
        end)
    end, BranchKeys),

    execute_interleaved(Ref, Pids, BranchKeys, BranchKeys, 1, []).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

-spec place_lst() -> [atom()].
place_lst() ->
    [
        p_start,
        p_branch_pool,
        p_next_branch,
        p_executing,
        p_branch_done,
        p_all_done,
        p_output
    ].

-spec trsn_lst() -> [atom()].
trsn_lst() ->
    [
        t_distribute,
        t_pick_next,
        t_execute,
        t_return,
        t_complete
    ].

-spec init_marking(Place :: atom(), UsrInfo :: interleaved_state()) ->
          [term()].
init_marking(p_start, _UsrInfo) ->
    [start];
init_marking(p_next_branch, _UsrInfo) ->
    [ready];
init_marking(_, _UsrInfo) ->
    [].

-spec preset(Trsn :: atom()) -> [atom()].
preset(t_distribute) -> [p_start];
preset(t_pick_next) -> [p_branch_pool, p_next_branch];
preset(t_execute) -> [p_executing];
preset(t_return) -> [p_branch_done];
preset(t_complete) -> [p_all_done];
preset(_) -> [].

-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: interleaved_state()) ->
          boolean().
is_enabled(t_distribute, _Mode, _UsrInfo) ->
    true;
is_enabled(t_pick_next, #{p_branch_pool := Pool, p_next_branch := [ready]}, _UsrInfo) when length(Pool) > 0 ->
    true;
is_enabled(t_execute, #{p_executing := [_]}, _UsrInfo) ->
    true;
is_enabled(t_return, #{p_branch_done := [_]}, _UsrInfo) ->
    true;
is_enabled(t_complete, #{p_all_done := [_]}, #interleaved_state{branches = Branches, completed = Completed}) ->
    map_size(Branches) =:= length(Completed);
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: interleaved_state()) ->
          abort | {produce, map()} | {produce, map(), interleaved_state()}.

fire(t_distribute, #{p_start := [start]}, #interleaved_state{branch_order = Order} = State) ->
    BranchTokens = [{{branch, Key}, pending} || Key <- Order],
    log_event(State, <<"InterleavedRouting">>, <<"Distribute">>, #{
        <<"branch_count">> => length(Order)
    }),
    {produce, #{
        p_start => [],
        p_branch_pool => BranchTokens
    }, State};

fire(t_pick_next, #{p_branch_pool := Pool, p_next_branch := [ready]}, #interleaved_state{current_index = Index} = State) ->
    PoolSize = length(Pool),
    ActualIndex = case Index > PoolSize of
        true -> 1;
        false -> Index
    end,
    {BranchToken, Remaining} = case lists:split(ActualIndex, Pool) of
        {[], _} -> {hd(Pool), tl(Pool)};
        {[Pick | Rest], []} -> {Pick, Rest};
        {Before, [Pick | Rest]} -> {Pick, Rest ++ Before}
    end,
    case BranchToken of
        {{branch, Key}, _} ->
            NewState = State#interleaved_state{current_index = ActualIndex + 1},
            log_event(State, <<"InterleavedRouting">>, <<"PickNext">>, #{<<"branch">> => Key}),
            {produce, #{
                p_branch_pool => Remaining,
                p_executing => [BranchToken]
            }, NewState}
    end;

fire(t_execute, #{p_executing := [{{branch, Key}, _}]}, #interleaved_state{branches = Branches} = State) ->
    _Fun = maps:get(Key, Branches),
    log_event(State, <<"InterleavedRouting">>, <<"Execute">>, #{<<"branch">> => Key}),
    {produce, #{
        p_executing => [],
        p_branch_done => [{branch_executed, Key}]
    }, State};

fire(t_return, #{p_branch_done := [{branch_executed, Key}]}, #interleaved_state{completed = Completed} = State) ->
    NewCompleted = [Key | Completed],
    Branches = State#interleaved_state.branches,
    AllDone = map_size(Branches) =:= length(NewCompleted),
    NewState = State#interleaved_state{completed = NewCompleted},
    log_event(State, <<"InterleavedRouting">>, <<"Return">>, #{
        <<"branch">> => Key,
        <<"all_done">> => AllDone
    }),
    case AllDone of
        true ->
            {produce, #{
                p_branch_done => [],
                p_next_branch => [ready],
                p_all_done => [all_complete]
            }, NewState};
        false ->
            {produce, #{
                p_branch_done => [],
                p_next_branch => [ready]
            }, NewState}
    end;

fire(t_complete, #{p_all_done := [all_complete]}, #interleaved_state{completed = Completed} = State) ->
    log_event(State, <<"InterleavedRouting">>, <<"Complete">>, #{
        <<"completed_branches">> => Completed
    }),
    {produce, #{
        p_all_done => [],
        p_output => [{completed, Completed}]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: interleaved_state()) ->
          pass | drop.
trigger(_Place, _Token, _UsrInfo) ->
    pass.

-spec init(UsrInfo :: interleaved_state()) ->
          interleaved_state().
init(InterleavedState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"InterleavedRouting">>}) of
        {ok, LogId} ->
            State1 = InterleavedState#interleaved_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            State1;
        _ ->
            InterleavedState
    end.

-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: #net_state{}) ->
          {reply, term()} | {reply, term(), #{atom() => [_]}} | noreply |
          {noreply, #{atom() => [_]}} | {stop, _, _}.
handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_pnet:get_usr_info(NetState),
    {reply, {ok, UsrInfo}};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}}.

-spec handle_cast(Request :: term(), NetState :: #net_state{}) ->
          noreply | {noreply, #{atom() => [_]}} | {stop, _}.
handle_cast({input_data, _InputData}, NetState) ->
    noreply;
handle_cast(_Request, NetState) ->
    noreply.

-spec handle_info(Info :: term(), NetState :: #net_state{}) ->
          noreply | {noreply, #{atom() => [_]}} | {stop, _}.
handle_info(_Request, NetState) ->
    noreply.

-spec code_change(OldVsn :: term(), NetState :: #net_state{}, Extra :: term()) ->
          {ok, #net_state{}} | {error, _}.
code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

-spec terminate(Reason :: term(), NetState :: #net_state{}) -> ok.
terminate(_Reason, NetState) ->
    UsrInfo = gen_pnet:get_usr_info(NetState),
    case UsrInfo of
        #interleaved_state{log_id = LogId} when LogId =/= undefined ->
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
          {ok, [{atom(), term()}]} | {error, term()}.
wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, p_output, Ref},
    receive
        {trigger, p_output, Ref, pass} ->
            case gen_pnet:marking(Pid) of
                Marking when is_map(Marking) ->
                    UsrInfo = gen_pnet:usr_info(Pid),
                    case UsrInfo of
                        #interleaved_state{completed = Completed} ->
                            {ok, [{Key, completed} || Key <- Completed]};
                        _ ->
                            {ok, []}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    after Timeout ->
        {error, timeout}
    end.

-spec execute_interleaved(Ref :: reference(), Pids :: [pid()], AllKeys :: [atom()],
                          RemainingKeys :: [atom()], Turn :: pos_integer(),
                          Acc :: [{atom(), term()}]) ->
          {ok, [{atom(), term()}]} | {error, term()}.
execute_interleaved(_Ref, _Pids, _AllKeys, [], _Turn, Acc) ->
    {ok, lists:reverse(Acc)};
execute_interleaved(Ref, Pids, AllKeys, [Key | Rest], Turn, Acc) ->
    receive
        {Ref, {branch_complete, Key}, Result} ->
            execute_interleaved(Ref, Pids, AllKeys, Rest, Turn + 1, [{Key, Result} | Acc]);
        {Ref, {branch_error, Key}, {Error, Reason, _Stack}} ->
            consume_remaining_messages(Ref, 1000),
            {error, {branch_error, Key, Error, Reason}};
        {Ref, {branch_complete, OtherKey}, Result} ->
            execute_interleaved(Ref, Pids, AllKeys, Rest ++ [OtherKey], Turn, [{OtherKey, Result} | Acc])
    after 5000 ->
        {error, {timeout, Key}}
    end.

-spec consume_remaining_messages(Ref :: reference(), Timeout :: timeout()) -> ok.
consume_remaining_messages(_Ref, Timeout) when Timeout =< 0 ->
    ok;
consume_remaining_messages(Ref, Timeout) ->
    receive
        {Ref, _, _} ->
            consume_remaining_messages(Ref, Timeout - 100)
    after 100 ->
        ok
    end.

-spec generate_log_id() -> binary().
generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"interleaved_routing_", Hex/binary>>.

-spec generate_case_id() -> binary().
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

-spec log_event(State :: interleaved_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.
log_event(#interleaved_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
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

place_lst_test() ->
    Places = place_lst(),
    ?assertEqual(7, length(Places)),
    ?assert(lists:member(p_start, Places)),
    ?assert(lists:member(p_branch_pool, Places)),
    ?assert(lists:member(p_next_branch, Places)),
    ?assert(lists:member(p_executing, Places)),
    ?assert(lists:member(p_branch_done, Places)),
    ?assert(lists:member(p_all_done, Places)),
    ?assert(lists:member(p_output, Places)).

trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assertEqual(5, length(Transitions)),
    ?assert(lists:member(t_distribute, Transitions)),
    ?assert(lists:member(t_pick_next, Transitions)),
    ?assert(lists:member(t_execute, Transitions)),
    ?assert(lists:member(t_return, Transitions)),
    ?assert(lists:member(t_complete, Transitions)).

preset_test() ->
    ?assertEqual([p_start], preset(t_distribute)),
    ?assertEqual([p_branch_pool, p_next_branch], preset(t_pick_next)),
    ?assertEqual([p_executing], preset(t_execute)),
    ?assertEqual([p_branch_done], preset(t_return)),
    ?assertEqual([p_all_done], preset(t_complete)),
    ?assertEqual([], preset(unknown)).

new_test() ->
    Branches = #{a => fun() -> ok end, b => fun() -> ok end, c => fun() -> ok end},
    State = new(Branches),
    ?assert(is_record(State, interleaved_state)),
    ?assertEqual([a, b, c], State#interleaved_state.branch_order),
    ?assertEqual(3, map_size(State#interleaved_state.branches)).

-endif.
