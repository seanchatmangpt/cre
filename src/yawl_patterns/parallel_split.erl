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
%% @doc Parallel Split Pattern (WCP-02) for YAWL
%%
%% This module implements the Parallel Split pattern as a gen_pnet behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Parallel Split pattern (WCP-02) splits a single thread of execution
%% into multiple concurrent branches that execute in parallel. All branches
%% are activated simultaneously from the same starting point.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Start of the parallel split
%%     p_branch1       - Branch 1 execution place
%%     p_branch2       - Branch 2 execution place
%%     p_branchN       - Branch N execution place
%%     p_join_ready    - Join synchronization point
%%     p_all_done      - All branches completed
%%     p_end           - Final output place
%%
%%   Transitions:
%%     t_split         - Split into parallel branches
%%     t_join_branch1  - Join branch 1 completion
%%     t_join_branch2  - Join branch 2 completion
%%     t_join_branchN  - Join branch N completion
%%     t_finish        - Complete when all branches done
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always true (all branches complete)</li>
%%   <li><b>Proper completion:</b> All branches complete exactly once</li>
%%   <li><b>No dead transitions:</b> All branches execute and reach join</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(parallel_split).
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
    new/2,
    start/1,
    run/2,
    get_state/1,
    execute/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(parallel_split_state, {
    branch_count :: pos_integer(),
    branch_funs :: [function()],
    completed = [] :: [pos_integer()],
    results = #{} :: #{pos_integer() => term()},
    start_time :: integer(),
    log_id :: binary() | undefined
}).

-type parallel_split_state() :: #parallel_split_state{}.
-export_type([parallel_split_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Parallel Split pattern state.
%%
%% @param BranchFuns List of functions to execute for each branch.
%% @param BranchCount Number of branches (must match length of BranchFuns).
%% @return A new parallel_split_state record.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(BranchFuns :: [function()], BranchCount :: pos_integer()) ->
          parallel_split_state().

new(BranchFuns, BranchCount) when is_list(BranchFuns),
                                  length(BranchFuns) =:= BranchCount,
                                  BranchCount >= 2 ->
    LogId = generate_log_id(),
    #parallel_split_state{
        branch_count = BranchCount,
        branch_funs = BranchFuns,
        start_time = erlang:system_time(millisecond),
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Parallel Split workflow as a gen_pnet process.
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
    SplitState = new(BranchFuns, BranchCount),
    gen_pnet:start_link(?MODULE, SplitState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Parallel Split workflow synchronously.
%%
%% @param BranchFuns List of functions to execute for each branch.
%% @param InputData Input data to pass to each branch.
%% @return {ok, Results} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec run(BranchFuns :: [function()], InputData :: term()) ->
          {ok, #{pos_integer() => term()}} | {error, term()}.

run(BranchFuns, InputData) when is_list(BranchFuns), length(BranchFuns) >= 2 ->
    case start(BranchFuns) of
        {ok, Pid} ->
            %% Inject input data
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

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Parallel Split workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) ->
          {ok, parallel_split_state()} | {error, term()}.

get_state(Pid) ->
    gen_pnet:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes the Parallel Split pattern with given input data.
%%
%% @param BranchFuns List of functions to execute for each branch.
%% @param InputData Input data to pass to each branch.
%% @return {ok, Results} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(BranchFuns :: [function()], InputData :: term()) ->
          {ok, #{pos_integer() => term()}} | {error, term()}.

execute(BranchFuns, InputData) when is_list(BranchFuns), length(BranchFuns) >= 2 ->
    BranchCount = length(BranchFuns),
    Ref = make_ref(),
    Parent = self(),

    %% Spawn all branches in parallel
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

    %% Wait for all branches to complete
    wait_all_branches(Ref, Pids, BranchCount, 30000, #{}).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Parallel Split Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_branch1',
        'p_branch2',
        'p_branch3',
        'p_branch4',
        'p_join_ready',
        'p_all_done',
        'p_end'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Parallel Split Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_split',
        't_join_branch1',
        't_join_branch2',
        't_join_branch3',
        't_join_branch4',
        't_finish'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: parallel_split_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_split') -> ['p_start'];
preset('t_join_branch1') -> ['p_branch1'];
preset('t_join_branch2') -> ['p_branch2'];
preset('t_join_branch3') -> ['p_branch3'];
preset('t_join_branch4') -> ['p_branch4'];
preset('t_finish') -> ['p_join_ready'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled in the given mode.
%%
%% @param Trsn The transition to check.
%% @param Mode The execution mode map.
%% @param UsrInfo The parallel_split_state.
%% @return true if enabled, false otherwise.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: parallel_split_state()) ->
          boolean().

is_enabled('t_split', _Mode, _UsrInfo) ->
    true;
is_enabled('t_join_branch1', #{'p_branch1' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_join_branch2', #{'p_branch2' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_join_branch3', #{'p_branch3' := [_]}, #parallel_split_state{branch_count = Count}) when Count >= 3 ->
    true;
is_enabled('t_join_branch4', #{'p_branch4' := [_]}, #parallel_split_state{branch_count = Count}) when Count >= 4 ->
    true;
is_enabled('t_finish', #{'p_join_ready' := Tokens}, #parallel_split_state{completed = Completed, branch_count = Count}) ->
    %% Enable when we have completion tokens for all branches
    length(Completed) =:= Count andalso length(Tokens) =:= Count;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%%
%% @param Trsn The transition to fire.
%% @param Mode The current mode (marking).
%% @param UsrInfo The parallel_split_state.
%% @return {produce, NewMode} | {produce, NewMode, NewUsrInfo} | abort.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: parallel_split_state()) ->
          {produce, map()} | {produce, map(), parallel_split_state()} | abort.

fire('t_split', #{'p_start' := [start]}, #parallel_split_state{branch_count = Count} = State) ->
    %% Split into parallel branches
    log_event(State, <<"ParallelSplit">>, <<"Split">>, #{<<"branch_count">> => Count}),
    case Count of
        2 ->
            {produce, #{
                'p_start' => [],
                'p_branch1' => [{branch, 1}],
                'p_branch2' => [{branch, 2}]
            }, State};
        3 ->
            {produce, #{
                'p_start' => [],
                'p_branch1' => [{branch, 1}],
                'p_branch2' => [{branch, 2}],
                'p_branch3' => [{branch, 3}]
            }, State};
        4 ->
            {produce, #{
                'p_start' => [],
                'p_branch1' => [{branch, 1}],
                'p_branch2' => [{branch, 2}],
                'p_branch3' => [{branch, 3}],
                'p_branch4' => [{branch, 4}]
            }, State};
        _ when Count > 4 ->
            %% For more than 4 branches, distribute tokens across first 4 places
            %% Multiple branches can share a place
            BranchTokens = distribute_branches(Count, 4),
            {produce, #{
                'p_start' => [],
                'p_branch1' => BranchTokens#{1 => [{branch, 1}]},
                'p_branch2' => BranchTokens#{2 => [{branch, 2}]},
                'p_branch3' => maps:get(3, BranchTokens, [{branch, 3}]),
                'p_branch4' => maps:get(4, BranchTokens, [{branch, 4}])
            }, State}
    end;

fire('t_join_branch1', #{'p_branch1' := [{branch, Index}]}, State) ->
    %% Branch 1 completed
    join_branch(Index, State, #{'p_branch1' => []});

fire('t_join_branch2', #{'p_branch2' := [{branch, Index}]}, State) ->
    %% Branch 2 completed
    join_branch(Index, State, #{'p_branch2' => []});

fire('t_join_branch3', #{'p_branch3' := [{branch, Index}]}, State) ->
    %% Branch 3 completed
    join_branch(Index, State, #{'p_branch3' => []});

fire('t_join_branch4', #{'p_branch4' := [{branch, Index}]}, State) ->
    %% Branch 4 completed
    join_branch(Index, State, #{'p_branch4' => []});

fire('t_finish', _Mode, #parallel_split_state{completed = Completed, results = Results, start_time = StartTime} = State) ->
    %% All branches completed - finish
    Elapsed = erlang:system_time(millisecond) - StartTime,
    log_event(State, <<"ParallelSplit">>, <<"Complete">>, #{
        <<"all_branches">> => Completed,
        <<"duration_ms">> => Elapsed
    }),
    {produce, #{
        'p_join_ready' => [],
        'p_end' => [{complete, maps:merge(Results, #{completed => Completed})}]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: parallel_split_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: parallel_split_state()) ->
          {ok, parallel_split_state()}.

init(ParallelSplitState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"ParallelSplit">>}) of
        {ok, LogId} ->
            State1 = ParallelSplitState#parallel_split_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, ParallelSplitState}
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_pnet:get_usr_info(NetState),
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
    UsrInfo = gen_pnet:get_usr_info(NetState),
    case UsrInfo of
        #parallel_split_state{log_id = LogId} when LogId =/= undefined ->
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
%% @doc Handles branch completion at the join.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec join_branch(Index :: pos_integer(), State :: parallel_split_state(), PlaceUpdates :: map()) ->
          {produce, map(), parallel_split_state()}.

join_branch(Index, #parallel_split_state{completed = Completed, branch_count = Count} = State, PlaceUpdates) ->
    NewCompleted = [Index | Completed],
    NewState = State#parallel_split_state{completed = NewCompleted},
    log_event(State, <<"ParallelSplit">>, <<"BranchJoin">>, #{<<"branch">> => Index}),
    AllDone = length(NewCompleted) =:= Count,
    BaseProduce = PlaceUpdates,
    case AllDone of
        true ->
            %% All branches completed - add completion tokens to join_ready
            JoinTokens = [{branch_complete, I} || I <- lists:sort(NewCompleted)],
            {produce, maps:merge(BaseProduce, #{'p_join_ready' => JoinTokens}), NewState};
        false ->
            %% Still waiting for more branches
            {produce, BaseProduce, NewState}
    end.

%%--------------------------------------------------------------------
%% @doc Distributes branches across places for more than 4 branches.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec distribute_branches(BranchCount :: pos_integer(), PlaceCount :: pos_integer()) ->
          map().

distribute_branches(BranchCount, PlaceCount) ->
    lists:foldl(fun(Index, Acc) ->
        PlaceIndex = ((Index - 1) rem PlaceCount) + 1,
        ExistingTokens = maps:get(PlaceIndex, Acc, []),
        maps:put(PlaceIndex, ExistingTokens ++ [{branch, Index}], Acc)
    end, #{}, lists:seq(1, BranchCount)).

%%--------------------------------------------------------------------
%% @doc Waits for workflow completion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_for_completion(Pid :: pid(), Timeout :: timeout()) ->
          {ok, #{pos_integer() => term()}} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_end', Ref},
    receive
        {trigger, 'p_end', Ref, pass} ->
            case gen_pnet:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_pnet:get_usr_info(Pid),
                    case UsrInfo of
                        #parallel_split_state{results = Results} ->
                            {ok, Results};
                        _ ->
                            {ok, #{}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    after Timeout ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Waits for all branches to complete.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_all_branches(Ref :: reference(), Pids :: [pid()], Remaining :: pos_integer(),
                        Timeout :: timeout(), Acc :: map()) ->
          {ok, map()} | {error, term()}.

wait_all_branches(_Ref, _Pids, 0, _Timeout, Acc) ->
    {ok, Acc};
wait_all_branches(Ref, Pids, Remaining, Timeout, Acc) ->
    receive
        {Ref, {branch_complete, Index}, Result} ->
            wait_all_branches(Ref, Pids, Remaining - 1, Timeout, maps:put(Index, Result, Acc));
        {Ref, {branch_error, Index}, {Error, Reason, _Stack}} ->
            {error, {branch_error, Index, Error, Reason}}
    after Timeout ->
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
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
    <<"parallel_split_", Hex/binary>>.

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
-spec log_event(State :: parallel_split_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#parallel_split_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.
