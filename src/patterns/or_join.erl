%% -*- erlang %%
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
OR-Join Synchronization Pattern for YAWL.

This module implements the OR-Join pattern (WCP-09 variant) which is the most
complex synchronization pattern in YAWL. Unlike simple discriminator or AND-join,
OR-join must wait for ALL active threads that CAN reach the join, but proceed
if a thread is stuck elsewhere (cannot reach the join).

## OR-Join Semantics

The OR-join semantics are defined as:
1. **Wait for all active threads that CAN reach the join** - Not just those
   that have arrived, but all threads that could potentially arrive
2. **Proceed if a thread cannot reach the join** - If structural analysis
   shows a thread is stuck (no path to join), don't wait for it
3. **Trigger on first arrival** - Once the waiting set is determined, the
   join triggers when the first thread from that set arrives

## Key Challenge

The key challenge is determining the "active threads" set. A thread is:
- **Active**: Has a token in some place that can reach the join
- **Can reach join**: There exists a path in the net structure from the
  thread's current place to the join transition's input places

## Petri Net Structure

```
Places:
  p_start          - Initial input place
  p_branch1        - Branch 1 place
  p_branch2        - Branch 2 place
  p_branch3        - Branch 3 place
  p_arrived        - Track which branches have arrived
  p_joined         - Join has fired
  p_end            - Final output place

Transitions:
  t_split          - Split into branches
  t_complete1      - Complete branch 1 (arrive at join)
  t_complete2      - Complete branch 2 (arrive at join)
  t_complete3      - Complete branch 3 (arrive at join)
  t_join           - OR-join (trigger on arrival)
  t_finish         - Produce output
```

## Examples

```erlang
> % Check if OR-join should fire
> Mode = #{p_branch1 => [token], p_branch2 => [token], p_branch3 => []},
> or_join:or_join_trigger(t_join, Mode, #{branch_count => 3}).
true

> % Find active threads
> Net = #{places => [p_branch1, p_branch2, p_branch3, p_end],
>         transitions => [t_join, t_finish],
>         arcs => [{p_branch1, t_join}, {p_branch2, t_join},
>                  {p_branch3, t_join}, {t_join, p_end}]},
> Mode = #{p_branch1 => [token], p_branch2 => []},
> or_join:active_threads(Mode, Net, [p_branch1, p_branch2, p_branch3]).
[p_branch1]

> % Check reachability
> Net = #{arcs => [{p_branch1, t_join}, {t_join, p_end}]},
> or_join:can_reach_join(p_branch1, t_join, Net).
true
```

## Soundness Properties

- **Option to complete:** Join fires when all active threads that can reach
  it have arrived
- **No deadlock:** Join proceeds even if some threads are structurally
  unable to reach it
- **Proper completion:** Exactly one output token per merge cycle

## Reference

This implementation follows the YAWL OR-join semantics as described in:
"Workflow Patterns: The OR-Join Pattern" by van der Aalst et al.
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

%% OR-join analysis functions - main exports
-export([
    or_join_trigger/2,
    active_threads/3,
    can_reach_join/2,
    or_join_semantics/1
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

%% Internal analysis exports
-export([
    compute_reachable/3,
    find_potential_threads/2,
    is_thread_active/3
]).

%%====================================================================
%% Records
%%====================================================================

-record(or_join_state, {
    branch_count = 3 :: pos_integer(),
    branch_funs = [] :: [function()],
    %% Tracks which branches have completed
    completed = [] :: [pos_integer()],
    %% Which branch triggered the join
    triggered_by :: undefined | pos_integer(),
    %% Net structure for reachability analysis
    net_structure = #{} :: map(),
    %% Which branches are considered "active" (can reach join)
    active_branches = [] :: [pos_integer()],
    %% Cycle count for reset
    cycle_count = 0 :: non_neg_integer(),
    %% Logging
    log_id :: binary() | undefined
}).

-record(reachability_cache, {
    from_place :: atom(),
    to_transition :: atom(),
    can_reach :: boolean(),
    computed_at :: integer()
}).

-type or_join_state() :: #or_join_state{}.
-type reachability_cache() :: #reachability_cache{}.
-type marking() :: #{atom() => [term()]}.
-type net_structure() :: #{
    places => [atom()],
    transitions => [atom()],
    arcs => [{atom(), atom()}]
}.

-export_type([or_join_state/0, reachability_cache/0, net_structure/0]).

%%====================================================================
%% OR-Join Analysis Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Determine if OR-join should fire based on current marking.
%%
%% This is the core OR-join trigger function. It returns true if:
%% 1. All threads that CAN reach the join have arrived, OR
%% 2. No more threads can reach the join (they're stuck elsewhere)
%%
%% @param JoinTransition The OR-join transition atom
%% @param Marking The current marking of the net
%% @return true if join should fire, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec or_join_trigger(JoinTransition :: atom(), Marking :: marking()) ->
          boolean().

or_join_trigger(JoinTransition, Marking) when is_atom(JoinTransition), is_map(Marking) ->
    %% Extract net structure from marking metadata or use default
    NetStructure = get_net_structure_from_marking(Marking),

    %% Find all potential thread places (input places to the join)
    %% For OR-join, these are the branch places (p_branch1, p_branch2, etc.)
    PotentialThreads = find_potential_threads(JoinTransition, NetStructure),

    %% If no potential threads found, use the preset of the join
    ActualPotentialThreads = case PotentialThreads of
        [] -> [p_branch1, p_branch2, p_branch3];
        _ -> PotentialThreads
    end,

    %% Determine which threads are active (have tokens and can reach join)
    ActiveThreads = active_threads(Marking, NetStructure, ActualPotentialThreads),

    %% Find which threads have arrived (have tokens in arrived places)
    ArrivedThreads = find_arrived_threads(Marking, ActualPotentialThreads),

    %% Join triggers if:
    %% 1. All active threads have arrived, OR
    %% 2. No active threads exist (all stuck or none started)
    should_join_fire(ActiveThreads, ArrivedThreads, Marking).

%%--------------------------------------------------------------------
%% @doc Identify which threads are still active that could reach the join.
%%
%% A thread is "active" if:
%% 1. It has a token somewhere in the net, AND
%% 2. That token is in a place that can reach the join transition
%%
%% @param Marking The current marking of the net
%% @param NetStructure The Petri net structure
%% @param PotentialThreads List of potential thread places to check
%% @return List of active thread place atoms
%%
%% @end
%%--------------------------------------------------------------------
-spec active_threads(Marking :: marking(),
                     NetStructure :: net_structure(),
                     PotentialThreads :: [atom()]) ->
          [atom()].

active_threads(Marking, NetStructure, PotentialThreads)
  when is_map(Marking), is_map(NetStructure), is_list(PotentialThreads) ->
    %% For each potential thread place, check if it's active
    lists:filter(fun(Place) ->
        is_thread_active(Place, Marking, NetStructure)
    end, PotentialThreads).

%%--------------------------------------------------------------------
%% @doc Check if a place/transition can reach the OR-join in the net.
%%
%% This performs structural reachability analysis by examining the net's
%% arc structure. Returns true if there exists a path from Source to
%% any input place of the OR-join transition.
%%
%% @param Source The place or transition to check
%% @param NetStructure The Petri net structure containing arcs
%% @return true if Source can reach the join, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec can_reach_join(Source :: atom(), NetStructure :: net_structure()) ->
          boolean().

can_reach_join(Source, NetStructure) when is_atom(Source), is_map(NetStructure) ->
    %% Get the join transition from net structure
    JoinTransition = maps:get(join_transition, NetStructure, t_join),

    %% Build adjacency map from arcs
    Arcs = maps:get(arcs, NetStructure, []),
    Adj = build_adjacency_map(Arcs),

    %% Find all places that are input to the join (preset of join)
    JoinInputPlaces = find_join_input_places(JoinTransition, Arcs),

    %% Check if Source can reach any of the join input places
    can_reach_any_place(Source, JoinInputPlaces, Adj, sets:new()).

%%--------------------------------------------------------------------
%% @doc Apply OR-join firing rule to marking.
%%
%% When OR-join fires, it consumes one token from each arrived branch
%% and produces a single joined token. This function computes the new
%% marking after the join fires.
%%
%% @param Marking The current marking
%% @return {ok, NewMarking} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec or_join_semantics(Marking :: marking()) ->
          {ok, marking()} | {error, term()}.

or_join_semantics(Marking) when is_map(Marking) ->
    try
        %% Find all arrived places (places with 'arrived' tokens)
        ArrivedPlaces = find_places_with_token_type(Marking, arrived),

        case ArrivedPlaces of
            [] ->
                {error, no_arrived_threads};
            _ ->
                %% Remove arrived tokens from all input places
                Marking1 = lists:foldl(fun(Place, Acc) ->
                    Acc#{Place => []}
                end, Marking, ArrivedPlaces),

                %% Add joined token to output place
                Marking2 = Marking1#{p_joined => [joined]},
                {ok, Marking2}
        end
    catch
        Type:Reason:Stack ->
            logger:error("or_join_semantics failed: ~p:~p~n~p", [Type, Reason, Stack]),
            {error, {failed, Type, Reason}}
    end.

%%====================================================================
%% Reachability Analysis Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compute all places reachable from a source place.
%%
%% Performs BFS traversal through the net structure to find all places
%% that can be reached from Source. This is used for determining if
%% a thread can potentially reach the OR-join.
%%
%% @param Source The starting place
%% @param NetStructure The Petri net structure
%% @param JoinTransition The OR-join transition to check reachability to
%% @return {ok, [Place]} list of reachable places, or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec compute_reachable(Source :: atom(),
                        NetStructure :: net_structure(),
                        JoinTransition :: atom()) ->
          {ok, [atom()]} | {error, term()}.

compute_reachable(Source, NetStructure, JoinTransition)
  when is_atom(Source), is_map(NetStructure), is_atom(JoinTransition) ->
    try
        Arcs = maps:get(arcs, NetStructure, []),
        Adj = build_adjacency_map(Arcs),

        %% Find join input places
        JoinInputs = find_join_input_places(JoinTransition, Arcs),

        %% BFS to find reachable places
        Visited = bfs_reachable(Source, Adj, sets:new()),

        %% Filter to only places that can reach the join
        CanReachJoin = lists:filter(fun(P) ->
            sets:is_element(P, JoinInputs) andalso sets:is_element(P, Visited)
        end, maps:get(places, NetStructure, [])),

        {ok, CanReachJoin}
    catch
        Type:Reason ->
            {error, {compute_reachable_failed, Type, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Find all potential thread places for a join.
%%
%% Potential thread places are places that could hold thread tokens
%% that need to arrive at the join. These are the input places to
%% the join transition.
%%
%% @param JoinTransition The OR-join transition
%% @param NetStructure The Petri net structure
%% @return List of potential thread place atoms
%%
%% @end
%%--------------------------------------------------------------------
-spec find_potential_threads(JoinTransition :: atom(),
                             NetStructure :: net_structure()) ->
          [atom()].

find_potential_threads(_JoinTransition, NetStructure) ->
    %% Use the preset of t_join from the module's preset/1 function
    %% For OR-join pattern, the input places are the branch places
    maps:get(places, NetStructure, [p_branch1, p_branch2, p_branch3]).

%%--------------------------------------------------------------------
%% @doc Check if a specific thread (place) is active.
%%
%% A thread is active if it has tokens and those tokens can reach
%% the join transition through the net structure.
%%
%% @param Place The place representing the thread
%% @param Marking The current marking
%% @param NetStructure The Petri net structure
%% @return true if the thread is active, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_thread_active(Place :: atom(),
                       Marking :: marking(),
                       NetStructure :: net_structure()) ->
          boolean().

is_thread_active(Place, Marking, NetStructure) ->
    %% Check if place has tokens
    case maps:get(Place, Marking, []) of
        [] ->
            false;
        _Tokens ->
            %% Check if this place can reach the join
            can_reach_join(Place, NetStructure)
    end.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new OR-Join pattern state.
%%
%% @param BranchFuns List of functions to execute for each branch
%% @param BranchCount Number of branches
%% @return A new or_join_state record
%%
%% @end
%%--------------------------------------------------------------------
-spec new(BranchFuns :: [function()], BranchCount :: pos_integer()) ->
          or_join_state().

new(BranchFuns, BranchCount) when is_list(BranchFuns),
                                  length(BranchFuns) =:= BranchCount,
                                  BranchCount >= 2 ->
    LogId = generate_log_id(),
    %% Build default net structure
    NetStructure = build_default_net_structure(BranchCount),

    #or_join_state{
        branch_count = BranchCount,
        branch_funs = BranchFuns,
        net_structure = NetStructure,
        active_branches = lists:seq(1, BranchCount),
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the OR-Join workflow as a gen_yawl process.
%%
%% @param BranchFuns List of functions to execute for each branch
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
%% @param BranchFuns List of functions to execute for each branch
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
%% @param Pid The pid of the gen_yawl process
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
%% @param BranchFuns List of functions to execute for each branch
%% @param InputData Input data to pass to each branch
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
%% @param Pid The pid of the gen_yawl process
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

-spec place_lst() -> [atom()].

place_lst() ->
    [p_start, p_branch1, p_branch2, p_branch3, p_arrived, p_joined, p_end].

-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [t_split, t_complete1, t_complete2, t_complete3, t_join, t_finish].

-spec init_marking(Place :: atom(), UsrInfo :: or_join_state()) ->
          [term()].

init_marking(p_start, _UsrInfo) ->
    [start];
init_marking(_Place, _UsrInfo) ->
    [].

-spec preset(Trsn :: atom()) -> [atom()].

preset(t_split) -> [p_start];
preset(t_complete1) -> [p_branch1];
preset(t_complete2) -> [p_branch2];
preset(t_complete3) -> [p_branch3];
preset(t_join) -> [p_branch1, p_branch2, p_branch3];
preset(t_finish) -> [p_joined];
preset(_) -> [].

-spec is_enabled(Trsn :: atom(), Mode :: marking(), UsrInfo :: or_join_state()) ->
          boolean().

is_enabled(t_split, _Mode, _UsrInfo) ->
    true;
is_enabled(t_complete1, Mode, _UsrInfo) ->
    maps:is_key(p_branch1, Mode) andalso length(maps:get(p_branch1, Mode, [])) > 0;
is_enabled(t_complete2, Mode, _UsrInfo) ->
    maps:is_key(p_branch2, Mode) andalso length(maps:get(p_branch2, Mode, [])) > 0;
is_enabled(t_complete3, Mode, _UsrInfo) ->
    maps:is_key(p_branch3, Mode) andalso length(maps:get(p_branch3, Mode, [])) > 0;
is_enabled(t_join, Mode, #or_join_state{triggered_by = undefined} = State) ->
    %% OR-join trigger logic: fire if all active threads have arrived
    or_join_trigger(t_join, Mode#{net_structure => State#or_join_state.net_structure});
is_enabled(t_join, _Mode, _UsrInfo) ->
    %% Already triggered, don't fire again
    false;
is_enabled(t_finish, Mode, _UsrInfo) ->
    maps:is_key(p_joined, Mode) andalso length(maps:get(p_joined, Mode, [])) > 0;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

-spec fire(Trsn :: atom(), Mode :: marking(), UsrInfo :: or_join_state()) ->
          {produce, marking()} | {produce, marking(), or_join_state()} | abort.

fire(t_split, _Mode, State) ->
    BranchCount = State#or_join_state.branch_count,
    %% Create tokens for each branch
    Produce = #{
        p_start => [],
        p_branch1 => [token],
        p_branch2 => [token],
        p_branch3 => [token]
    },
    log_event(State, <<"ORJoin">>, <<"Split">>, #{<<"branch_count">> => BranchCount}),
    {produce, Produce, State};

fire(t_complete1, _Mode, State) ->
    %% Branch 1 arrives at join
    log_event(State, <<"ORJoin">>, <<"Branch1Arrive">>, #{}),
    {produce, #{
        p_branch1 => [],
        p_arrived => [arrived, 1]
    }, State};

fire(t_complete2, _Mode, State) ->
    %% Branch 2 arrives at join
    log_event(State, <<"ORJoin">>, <<"Branch2Arrive">>, #{}),
    {produce, #{
        p_branch2 => [],
        p_arrived => [arrived, 2]
    }, State};

fire(t_complete3, _Mode, State) ->
    %% Branch 3 arrives at join
    log_event(State, <<"ORJoin">>, <<"Branch3Arrive">>, #{}),
    {produce, #{
        p_branch3 => [],
        p_arrived => [arrived, 3]
    }, State};

fire(t_join, Mode, State) ->
    %% Determine which branch triggered
    ArrivedTokens = maps:get(p_arrived, Mode, []),
    TriggeredBy = case ArrivedTokens of
        [arrived, N | _] -> N;
        [arrived, N] -> N;
        _ -> 1
    end,

    NewState = State#or_join_state{
        triggered_by = TriggeredBy,
        completed = [TriggeredBy]
    },

    log_event(State, <<"ORJoin">>, <<"Triggered">>, #{<<"by">> => TriggeredBy}),

    {produce, #{
        p_branch1 => [],
        p_branch2 => [],
        p_branch3 => [],
        p_arrived => [],
        p_joined => [joined]
    }, NewState};

fire(t_finish, _Mode, State) ->
    #or_join_state{triggered_by = TriggeredBy} = State,
    log_event(State, <<"ORJoin">>, <<"Complete">>, #{<<"triggered_by">> => TriggeredBy}),
    {produce, #{
        p_joined => [],
        p_end => [done]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: or_join_state()) ->
          pass | drop.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

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

-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term()} | noreply.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {ok, UsrInfo}};
handle_call(_Request, _From, _NetState) ->
    {reply, {error, bad_msg}}.

-spec handle_cast(Request :: term(), NetState :: term()) ->
          noreply.

handle_cast(_Request, _NetState) ->
    noreply.

-spec handle_info(Request :: term(), NetState :: term()) -> noreply.

handle_info(_Request, _NetState) ->
    noreply.

-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

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
%% @private
%% @doc Check if join should fire based on active and arrived threads.
%%
%% @end
%%--------------------------------------------------------------------
-spec should_join_fire(ActiveThreads :: [atom()],
                       ArrivedThreads :: [atom()],
                       Marking :: marking()) ->
          boolean().

should_join_fire(ActiveThreads, ArrivedThreads, Marking) ->
    case ActiveThreads of
        %% No active threads - join can fire
        [] ->
            true;
        _ ->
            %% Check if all active threads have arrived
            %% A thread has "arrived" if its token is consumed (empty in marking)
            AllArrived = lists:all(fun(ThreadPlace) ->
                %% Thread has arrived if its place is empty (token consumed)
                %% OR if it's in the arrived list
                case maps:get(ThreadPlace, Marking, []) of
                    [] -> true;
                    _ -> lists:member(ThreadPlace, ArrivedThreads)
                end
            end, ActiveThreads),
            AllArrived
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Find which threads have arrived at the join.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_arrived_threads(Marking :: marking(), PotentialThreads :: [atom()]) ->
          [atom()].

find_arrived_threads(Marking, PotentialThreads) ->
    %% A thread has arrived if its place has been consumed (empty)
    %% or if there's an arrived token tracking it
    lists:filter(fun(Place) ->
        case maps:get(Place, Marking, []) of
            [] -> true;
            _ -> false
        end
    end, PotentialThreads).

%%--------------------------------------------------------------------
%% @private
%% @doc Find all places with a specific token type.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_places_with_token_type(Marking :: marking(), TokenType :: term()) ->
          [atom()].

find_places_with_token_type(Marking, TokenType) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        case lists:member(TokenType, Tokens) of
            true -> [Place | Acc];
            false -> Acc
        end
    end, [], Marking).

%%--------------------------------------------------------------------
%% @private
%% @doc Get net structure from marking metadata.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_net_structure_from_marking(marking()) -> net_structure().

get_net_structure_from_marking(Marking) ->
    maps:get(net_structure, Marking, build_default_net_structure(3)).

%%--------------------------------------------------------------------
%% @private
%% @doc Build default net structure for given branch count.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_default_net_structure(pos_integer()) -> net_structure().

build_default_net_structure(BranchCount) ->
    Places = [p_start, p_end, p_arrived, p_joined] ++
             [list_to_atom("p_branch" ++ integer_to_list(I)) || I <- lists:seq(1, BranchCount)],

    Transitions = [t_split, t_join, t_finish] ++
                   [list_to_atom("t_complete" ++ integer_to_list(I)) || I <- lists:seq(1, BranchCount)],

    %% Build arcs: split -> branches, branches -> join, join -> end
    SplitArcs = [{p_start, t_split} | [begin
        BP = list_to_atom("p_branch" ++ integer_to_list(I)),
        {t_split, BP}
    end || I <- lists:seq(1, BranchCount)]],

    CompleteArcs = lists:flatmap(fun(I) ->
        BP = list_to_atom("p_branch" ++ integer_to_list(I)),
        CT = list_to_atom("t_complete" ++ integer_to_list(I)),
        [{BP, CT}, {CT, p_arrived}]
    end, lists:seq(1, BranchCount)),

    JoinArcs = [{p_arrived, t_join}, {t_join, p_joined}, {p_joined, t_finish}, {t_finish, p_end}],

    AllArcs = SplitArcs ++ CompleteArcs ++ JoinArcs,

    #{
        places => Places,
        transitions => Transitions,
        arcs => AllArcs,
        join_transition => t_join
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Build adjacency map from arcs list.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_adjacency_map([{atom(), atom()}]) -> #{atom() => [atom()]}.

build_adjacency_map(Arcs) ->
    lists:foldl(fun({From, To}, Acc) ->
        Acc#{From => [To | maps:get(From, Acc, [])]}
    end, #{}, Arcs).

%%--------------------------------------------------------------------
%% @private
%% @doc Find input places to a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec find_join_input_places(JoinTransition :: atom(), Arcs :: [{atom(), atom()}]) ->
          sets:set(atom()).

find_join_input_places(JoinTransition, Arcs) ->
    InputPlaces = lists:filtermap(fun({Place, Trsn}) ->
        case Trsn =:= JoinTransition of
            true -> {true, Place};
            false -> false
        end
    end, Arcs),
    sets:from_list(InputPlaces).

%%--------------------------------------------------------------------
%% @private
%% @doc Check if source can reach any of the target places.
%%
%% @end
%%--------------------------------------------------------------------
-spec can_reach_any_place(Source :: atom(),
                          TargetPlaces :: sets:set(atom()),
                          Adj :: #{atom() => [atom()]},
                          Visited :: sets:set(atom())) ->
          boolean().

can_reach_any_place(Source, TargetPlaces, Adj, Visited) ->
    case sets:is_element(Source, TargetPlaces) of
        true ->
            true;
        false ->
            case sets:is_element(Source, Visited) of
                true ->
                    false;
                false ->
                    Visited1 = sets:add_element(Source, Visited),
                    Neighbors = maps:get(Source, Adj, []),
                    lists:any(fun(N) ->
                        can_reach_any_place(N, TargetPlaces, Adj, Visited1)
                    end, Neighbors)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc BFS to find all reachable places from source.
%%
%% @end
%%--------------------------------------------------------------------
-spec bfs_reachable(Source :: atom(),
                    Adj :: #{atom() => [atom()]},
                    Visited :: sets:set(atom())) ->
          sets:set(atom()).

bfs_reachable(Source, Adj, Visited) ->
    case sets:is_element(Source, Visited) of
        true ->
            Visited;
        false ->
            Visited1 = sets:add_element(Source, Visited),
            Neighbors = maps:get(Source, Adj, []),
            lists:foldl(fun(N, Acc) ->
                bfs_reachable(N, Adj, Acc)
            end, Visited1, Neighbors)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Wait for workflow completion.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_for_completion(Pid :: pid(), Timeout :: timeout()) ->
          {ok, {pos_integer(), term()}} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, p_end, Ref},
    receive
        {trigger, p_end, Ref, pass} ->
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
%% @private
%% @doc Wait for first branch completion.
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_first_complete(reference(), [pid()], timeout()) ->
          {ok, {pos_integer(), term()}} | {error, term()}.

wait_first_complete(Ref, Pids, Timeout) ->
    receive
        {Ref, {branch_complete, Index}, Result} ->
            {ok, {Index, Result}};
        {Ref, {branch_error, Index}, {Error, Reason, _Stack}} ->
            {error, {branch_error, Index, Error, Reason}}
    after Timeout ->
        lists:foreach(fun(P) -> exit(P, kill) end, Pids),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Consume remaining branch results.
%%
%% @end
%%--------------------------------------------------------------------
-spec consume_remaining(reference(), timeout()) -> ok.

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
%% @private
%% @doc Generate a unique log ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().

generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"or_join_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generate a unique case ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id() -> binary().

generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Log an XES event.
%%
%% @end
%%--------------------------------------------------------------------
-spec log_event(State :: or_join_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#or_join_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% OR-Join Analysis Tests
%%--------------------------------------------------------------------

or_join_trigger_no_active_threads_test() ->
    Marking = #{p_branch1 => [], p_branch2 => [], p_branch3 => []},
    ?assertEqual(true, or_join_trigger(t_join, Marking)).

or_join_trigger_all_arrived_test() ->
    %% All active threads have arrived (places empty)
    Marking = #{p_branch1 => [], p_branch2 => [], p_branch3 => []},
    ?assertEqual(true, or_join_trigger(t_join, Marking)).

or_join_trigger_active_not_arrived_test() ->
    %% Branch 1 still active (has token)
    NetStructure = build_default_net_structure(3),
    Marking = #{
        p_branch1 => [token],
        p_branch2 => [],
        p_branch3 => [],
        net_structure => NetStructure
    },
    ?assertEqual(false, or_join_trigger(t_join, Marking)).

active_threads_one_active_test() ->
    NetStructure = build_default_net_structure(3),
    Marking = #{
        p_branch1 => [token],
        p_branch2 => [],
        p_branch3 => []
    },
    PotentialThreads = [p_branch1, p_branch2, p_branch3],
    Active = active_threads(Marking, NetStructure, PotentialThreads),
    ?assertEqual([p_branch1], lists:sort(Active)).

active_threads_all_active_test() ->
    NetStructure = build_default_net_structure(3),
    Marking = #{
        p_branch1 => [token],
        p_branch2 => [token],
        p_branch3 => [token]
    },
    PotentialThreads = [p_branch1, p_branch2, p_branch3],
    Active = active_threads(Marking, NetStructure, PotentialThreads),
    ?assertEqual([p_branch1, p_branch2, p_branch3], lists:sort(Active)).

can_reach_join_direct_path_test() ->
    NetStructure = #{
        arcs => [{p_branch1, t_join}],
        join_transition => t_join
    },
    ?assertEqual(true, can_reach_join(p_branch1, NetStructure)).

can_reach_join_no_path_test() ->
    NetStructure = #{
        arcs => [{p_branch1, t_other}, {t_other, p_nowhere}],
        join_transition => t_join
    },
    ?assertEqual(false, can_reach_join(p_branch1, NetStructure)).

or_join_semantics_basic_test() ->
    Marking = #{p_arrived => [arrived, 1], p_branch1 => [], p_branch2 => []},
    {ok, NewMarking} = or_join_semantics(Marking),
    ?assertEqual([joined], maps:get(p_joined, NewMarking, [])).

or_join_semantics_no_arrived_test() ->
    Marking = #{p_branch1 => [token], p_branch2 => []},
    ?assertEqual({error, no_arrived_threads}, or_join_semantics(Marking)).

compute_reachable_basic_test() ->
    NetStructure = #{
        arcs => [{p_branch1, t_complete1}, {t_complete1, p_arrived}, {p_arrived, t_join}],
        join_transition => t_join
    },
    {ok, Reachable} = compute_reachable(p_branch1, NetStructure, t_join),
    ?assert(lists:member(p_arrived, Reachable)).

is_thread_active_with_token_test() ->
    NetStructure = build_default_net_structure(3),
    Marking = #{p_branch1 => [token]},
    ?assertEqual(true, is_thread_active(p_branch1, Marking, NetStructure)).

is_thread_active_no_token_test() ->
    NetStructure = build_default_net_structure(3),
    Marking = #{p_branch1 => []},
    ?assertEqual(false, is_thread_active(p_branch1, Marking, NetStructure)).

find_potential_threads_test() ->
    NetStructure = build_default_net_structure(3),
    Threads = find_potential_threads(t_join, NetStructure),
    ?assert(lists:member(p_branch1, Threads)),
    ?assert(lists:member(p_branch2, Threads)),
    ?assert(lists:member(p_branch3, Threads)).

%%--------------------------------------------------------------------
%% gen_yawl Callback Tests
%%--------------------------------------------------------------------

place_lst_test() ->
    ?assertEqual([p_start, p_branch1, p_branch2, p_branch3, p_arrived, p_joined, p_end],
                 place_lst()).

trsn_lst_test() ->
    ?assertEqual([t_split, t_complete1, t_complete2, t_complete3, t_join, t_finish],
                 trsn_lst()).

preset_t_split_test() ->
    ?assertEqual([p_start], preset(t_split)).

preset_t_complete1_test() ->
    ?assertEqual([p_branch1], preset(t_complete1)).

preset_t_join_test() ->
    ?assertEqual([p_branch1, p_branch2, p_branch3], preset(t_join)).

init_marking_p_start_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_other_place_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    ?assertEqual([], init_marking(p_branch1, State)).

is_enabled_t_split_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    Mode = #{},
    ?assertEqual(true, is_enabled(t_split, Mode, State)).

is_enabled_t_complete1_with_token_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    Mode = #{p_branch1 => [token]},
    ?assertEqual(true, is_enabled(t_complete1, Mode, State)).

is_enabled_t_complete1_no_token_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    Mode = #{p_branch1 => []},
    ?assertEqual(false, is_enabled(t_complete1, Mode, State)).

fire_t_split_test() ->
    State = new([fun(_) -> ok end, fun(_) -> ok end], 2),
    Mode = #{p_start => [start]},
    Result = fire(t_split, Mode, State),
    ?assertMatch({produce, _, _}, Result).

new_state_test() ->
    Fun1 = fun(X) -> X * 2 end,
    Fun2 = fun(X) -> X + 10 end,
    State = new([Fun1, Fun2], 2),
    ?assertEqual(2, State#or_join_state.branch_count),
    ?assertEqual([], State#or_join_state.completed),
    ?assertEqual(undefined, State#or_join_state.triggered_by).

-endif.
