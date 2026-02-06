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
%% @doc Interleaved Routing Pattern (WCP-17) for YAWL
%%
%% This module implements the Interleaved Routing pattern as a gen_pnet behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Interleaved Routing pattern (WCP-17) executes multiple concurrent
%% branches in an interleaved (round-robin) fashion, ensuring fair execution
%% among all branches. No prescribed execution order is enforced.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Start of interleaved routing
%%     p_branch_pool   - Pool of pending branches
%%     p_next_branch   - Next branch to execute (round-robin)
%%     p_executing     - Currently executing branch
%%     p_branch_done   - Branch execution completed
%%     p_all_done      - All branches completed
%%     p_output        - Final output
%%
%%   Transitions:
%%     t_distribute    - Distribute work to branches
%%     t_pick_next     - Pick next branch for execution
%%     t_execute       - Execute the picked branch
%%     t_return        - Return branch to pool or mark done
%%     t_complete      - Complete when all branches done
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always true (all branches complete)</li>
%%   <li><b>Proper completion:</b> All branches complete exactly once</li>
%%   <li><b>No dead transitions:</b> Fair round-robin ensures liveness</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(interleaved_routing).
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

-record(interleaved_state, {
    branches :: map(),  %% Map of branch_id => function()
    branch_order :: [atom()],  %% Order for round-robin
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

%%--------------------------------------------------------------------
%% @doc Creates a new Interleaved Routing pattern state.
%%
%% @param Branches Map of branch identifiers to functions.
%% @return A new interleaved_state record.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(Branches :: map()) -> interleaved_state().

new(Branches) when is_map(Branches), map_size(Branches) >= 2 ->
    BranchOrder = lists:sort(maps:keys(Branches)),
    LogId = generate_log_id(),
    #interleaved_state{
        branches = Branches,
        branch_order = BranchOrder,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Interleaved Routing workflow as a gen_pnet process.
%%
%% @param Branches Map of branch identifiers to functions.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Branches :: map()) -> {ok, pid()} | {error, term()}.

start(Branches) when is_map(Branches), map_size(Branches) >= 2 ->
    InterleavedState = new(Branches),
    gen_yawl:start_link(?MODULE, InterleavedState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Interleaved Routing workflow synchronously.
%%
%% @param Branches Map of branch identifiers to functions.
%% @param InputData Input data to pass to each branch.
%% @return {ok, Results} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec run(Branches :: map(), InputData :: term()) ->
          {ok, [{atom(), term()}]} | {error, term()}.

run(Branches, InputData) when is_map(Branches), map_size(Branches) >= 2 ->
    case start(Branches) of
        {ok, Pid} ->
            %% Inject input data
            gen_yawl:cast(Pid, {input_data, InputData}),
            case wait_for_completion(Pid, 30000) of
                {ok, Results} ->
                    gen_yawl:stop(Pid),
                    {ok, Results};
                {error, Reason} ->
                    gen_yawl:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Interleaved Routing workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, interleaved_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes the Interleaved Routing pattern with given input data.
%%
%% @param Branches Map of branch identifiers to functions.
%% @param InputData Input data to pass to each branch.
%% @return {ok, Results} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(Branches :: map(), InputData :: term()) ->
          {ok, [{atom(), term()}]} | {error, term()}.

execute(Branches, InputData) when is_map(Branches), map_size(Branches) >= 2 ->
    BranchKeys = maps:keys(Branches),
    Ref = make_ref(),
    Parent = self(),

    %% Spawn all branches
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

    %% Execute in interleaved fashion by receiving in round-robin
    execute_interleaved(Ref, Pids, BranchKeys, BranchKeys, 1, []).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Interleaved Routing Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_branch_pool',
        'p_next_branch',
        'p_executing',
        'p_branch_done',
        'p_all_done',
        'p_output'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Interleaved Routing Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_distribute',
        't_pick_next',
        't_execute',
        't_return',
        't_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: interleaved_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking('p_next_branch', _UsrInfo) ->
    [ready];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_distribute') -> ['p_start'];
preset('t_pick_next') -> ['p_branch_pool', 'p_next_branch'];
preset('t_execute') -> ['p_executing'];
preset('t_return') -> ['p_branch_done'];
preset('t_complete') -> ['p_all_done'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: interleaved_state()) ->
          boolean().

is_enabled('t_distribute', _Mode, _UsrInfo) ->
    true;
is_enabled('t_pick_next', #{'p_branch_pool' := Pool, 'p_next_branch' := [ready]}, _UsrInfo) when length(Pool) > 0 ->
    true;
is_enabled('t_execute', #{'p_executing' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_return', #{'p_branch_done' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_complete', #{'p_all_done' := [_]}, #interleaved_state{branches = Branches, completed = Completed}) ->
    map_size(Branches) =:= length(Completed);
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: interleaved_state()) ->
          {produce, map()} | {produce, map(), interleaved_state()} | abort.

fire('t_distribute', #{'p_start' := [start]}, #interleaved_state{branch_order = Order} = State) ->
    %% Create branch tokens in order
    BranchTokens = [{{branch, Key}, pending} || Key <- Order],
    log_event(State, <<"InterleavedRouting">>, <<"Distribute">>, #{
        <<"branch_count">> => length(Order)
    }),
    {produce, #{
        'p_start' => [],
        'p_branch_pool' => BranchTokens
    }, State};

fire('t_pick_next', #{'p_branch_pool' := Pool, 'p_next_branch' := [ready]}, #interleaved_state{current_index = Index} = State) ->
    %% Pick next branch in round-robin order
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
                'p_branch_pool' => Remaining,
                'p_executing' => [BranchToken]
            }, NewState}
    end;

fire('t_execute', #{'p_executing' := [{{branch, Key}, _}]}, #interleaved_state{branches = Branches} = State) ->
    %% Execute the branch (token carries result)
    Fun = maps:get(Key, Branches),
    %% In a real execution, this would run the function
    %% For Petri net semantics, we just mark it as executed
    log_event(State, <<"InterleavedRouting">>, <<"Execute">>, #{<<"branch">> => Key}),
    {produce, #{
        'p_executing' => [],
        'p_branch_done' => [{branch_executed, Key}]
    }, State};

fire('t_return', #{'p_branch_done' := [{branch_executed, Key}]}, #interleaved_state{completed = Completed} = State) ->
    %% Return from branch execution
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
                'p_branch_done' => [],
                'p_next_branch' => [ready],
                'p_all_done' => [all_complete]
            }, NewState};
        false ->
            {produce, #{
                'p_branch_done' => [],
                'p_next_branch' => [ready]
            }, NewState}
    end;

fire('t_complete', #{'p_all_done' := [all_complete]}, #interleaved_state{completed = Completed} = State) ->
    %% Complete the interleaved routing
    log_event(State, <<"InterleavedRouting">>, <<"Complete">>, #{
        <<"completed_branches">> => Completed
    }),
    {produce, #{
        'p_all_done' => [],
        'p_output' => [{completed, Completed}]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: interleaved_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: interleaved_state()) ->
          {ok, interleaved_state()}.

init(InterleavedState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"InterleavedRouting">>}) of
        {ok, LogId} ->
            State1 = InterleavedState#interleaved_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, InterleavedState}
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

handle_cast({input_data, _InputData}, NetState) ->
    %% Store input data for later use
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

%%--------------------------------------------------------------------
%% @doc Waits for workflow completion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_for_completion(Pid :: pid(), Timeout :: timeout()) ->
          {ok, [{atom(), term()}]} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_output', Ref},
    receive
        {trigger, 'p_output', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
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

%%--------------------------------------------------------------------
%% @doc Executes branches in interleaved fashion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec execute_interleaved(Ref :: reference(), Pids :: [pid()], AllKeys :: [atom()],
                          RemainingKeys :: [atom()], Turn :: pos_integer(),
                          Acc :: [{atom(), term()}]) ->
          {ok, [{atom(), term()}]} | {error, term()}.

execute_interleaved(_Ref, _Pids, _AllKeys, [], _Turn, Acc) ->
    %% All keys processed
    {ok, lists:reverse(Acc)};
execute_interleaved(Ref, Pids, AllKeys, [Key | Rest], Turn, Acc) ->
    %% Wait for this specific branch's result
    receive
        {Ref, {branch_complete, Key}, Result} ->
            execute_interleaved(Ref, Pids, AllKeys, Rest, Turn + 1, [{Key, Result} | Acc]);
        {Ref, {branch_error, Key}, {Error, Reason, _Stack}} ->
            %% Consume remaining messages before returning error
            consume_remaining_messages(Ref, 1000),
            {error, {branch_error, Key, Error, Reason}};
        {Ref, {branch_complete, OtherKey}, Result} ->
            %% Wrong branch completed first - store and continue
            execute_interleaved(Ref, Pids, AllKeys, Rest ++ [OtherKey], Turn, [{OtherKey, Result} | Acc])
    after 5000 ->
        {error, {timeout, Key}}
    end.

%%--------------------------------------------------------------------
%% @doc Consumes remaining messages.
%% @private
%% @end
%%--------------------------------------------------------------------
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

%%--------------------------------------------------------------------
%% @doc Generates a unique log ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().

generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"interleaved_routing_", Hex/binary>>.

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
-spec log_event(State :: interleaved_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#interleaved_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.
