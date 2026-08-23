%% -*- erlang -*-
%% @author CRE Team
%% @version 0.3.0
%% @doc Bounded State Space Explorer for Petri Net Validation
%%
%% Performs depth-limited DFS with token bounds to explore reachable
%% states in a Petri net.
%%
%% <h3>Exploration Strategy</h3>
%% <ul>
%%   <li>Depth-limited DFS with configurable maximum depth</li>
%%   <li>Token bound per place to prevent state explosion</li>
%%   <li>State hashing for cycle detection</li>
%%   <li>Complete path tracking for error reporting</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% {ok, Traces} = yawl_explorer:explore(InitialMarking, Transitions,
%%     #{depth => 15, token_bound => 10}).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_explorer).

%%====================================================================
%% Exports
%%====================================================================

-export([explore/3, fire_transition/2]).

%%====================================================================
%% Types
%%====================================================================

%% A trace step records the transition fired and resulting marking
-type trace_step() :: {pnet_types:trsn(), pnet_marking:marking()}.

%% A trace is a sequence of steps from initial state
-type trace() :: [trace_step()].

-export_type([trace/0, trace_step/0]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Explores the state space with bounds.
%%
%% Returns all execution traces found within bounds.
%%
%% @end
%%--------------------------------------------------------------------
-spec explore(InitialMarking :: pnet_marking:marking(),
             Transitions :: [yawl_pnet_compiler:transition()],
             Bounds :: map()) ->
    {ok, [trace()]} | {error, term()}.

explore(InitialMarking, Transitions, Bounds) ->
    try
        Visited = sets:new(),
        MaxDepth = maps:get(depth, Bounds, 15),
        TokenBound = maps:get(token_bound, Bounds, 10),

        %% Start exploration with empty path
        Traces = dfs([], InitialMarking, Transitions, Visited, 0, MaxDepth, TokenBound),

        {ok, Traces}
    catch
        _:Error:StackTrace ->
            io:format("Exploration error: ~p~n~p~n", [Error, StackTrace]),
            {error, Error}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Depth-limited DFS with token bound checking.
%%
%% @end
%%--------------------------------------------------------------------
-spec dfs(Path :: trace_step(),
           CurrentMarking :: pnet_marking:marking(),
           Transitions :: [yawl_pnet_compiler:transition()],
           Visited :: sets:set(binary()),
           Depth :: non_neg_integer(),
           MaxDepth :: non_neg_integer(),
           TokenBound :: non_neg_integer()) ->
    [trace()].

dfs(Path, CurrentMarking, Transitions, Visited, Depth, MaxDepth, TokenBound) ->
    %% Check token bound first
    case check_token_bound(CurrentMarking, TokenBound) of
        exceed ->
            %% Token bound exceeded - stop this path
            [lists:reverse(Path)];
        ok ->
            %% Get enabled transitions
            Enabled = get_enabled_transitions(CurrentMarking, Transitions),

            case Enabled of
                [] ->
                    %% Deadlock or final state - end of path
                    [lists:reverse(Path)];
                _ when Depth >= MaxDepth ->
                    %% Depth limit reached - stop exploring
                    [lists:reverse(Path)];
                _ ->
                    %% Check if we've visited this state before
                    StateHash = pnet_marking:hash(CurrentMarking),
                    case sets:is_element(StateHash, Visited) of
                        true ->
                            %% Already visited - avoid cycles
                            [lists:reverse(Path)];
                        false ->
                            %% New state - explore successors
                            Visited1 = sets:add_element(StateHash, Visited),
                            lists:flatmap(fun(Transition) ->
                                fire_and_explore(Transition, CurrentMarking, Transitions,
                                                 Path, Visited1, Depth, MaxDepth, TokenBound)
                            end, Enabled)
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc Fires a transition and continues exploration.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire_and_explore(Transition :: yawl_pnet_compiler:transition(),
                       CurrentMarking :: pnet_marking:marking(),
                       Transitions :: [yawl_pnet_compiler:transition()],
                       Path :: trace_step(),
                       Visited :: sets:set(binary()),
                       Depth :: non_neg_integer(),
                       MaxDepth :: non_neg_integer(),
                       TokenBound :: non_neg_integer()) ->
    [trace()].

fire_and_explore(Transition, CurrentMarking, Transitions, Path, Visited, Depth, MaxDepth, TokenBound) ->
    case fire_transition(Transition, CurrentMarking) of
        {ok, NextMarking} ->
            %% Record this step and continue
            Step = {maps:get(id, Transition), NextMarking},
            dfs([Step | Path], NextMarking, Transitions, Visited, Depth + 1, MaxDepth, TokenBound);
        {error, _Reason} ->
            %% Transition failed to fire - skip this path
            []
    end.

%%--------------------------------------------------------------------
%% @doc Gets all transitions enabled in the current marking.
%%
%% A transition is enabled if all places in its preset have at least one token.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_enabled_transitions(pnet_marking:marking(),
                              [yawl_pnet_compiler:transition()]) ->
    [yawl_pnet_compiler:transition()].

get_enabled_transitions(Marking, Transitions) ->
    lists:filter(fun(Transition) ->
        is_enabled(Marking, Transition)
    end, Transitions).

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled in the given marking.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(pnet_marking:marking(), yawl_pnet_compiler:transition()) ->
    boolean().

is_enabled(Marking, #{preset := Preset}) ->
    lists:all(fun(Place) ->
        case pnet_marking:get(Marking, Place) of
            {ok, Tokens} when length(Tokens) > 0 -> true;
            _ -> false
        end
    end, Preset).

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming from preset and producing to postset.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire_transition(yawl_pnet_compiler:transition(), pnet_marking:marking()) ->
    {ok, pnet_marking:marking()} | {error, term()}.

fire_transition(#{preset := Preset, postset := Postset}, Marking) ->
    %% Consume one token from each place in preset
    %% Take the actual tokens that are present (first token from each place)
    ConsumeList = lists:map(fun(P) ->
        {ok, Tokens} = pnet_marking:get(Marking, P),
        case Tokens of
            [] -> error({no_tokens, P});
            [FirstToken | _] -> {P, [FirstToken]}
        end
    end, Preset),

    try
        ConsumeMap = maps:from_list(ConsumeList),
        case pnet_marking:take(Marking, ConsumeMap) of
            {ok, Marking1} ->
                %% Produce one token to each place in postset
                ProduceMap = maps:from_list([{P, [token]} || P <- Postset]),
                {ok, pnet_marking:add(Marking1, ProduceMap)};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error -> {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if any place exceeds the token bound.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_token_bound(pnet_marking:marking(), non_neg_integer()) ->
    ok | exceed.

check_token_bound(Marking, Bound) ->
    Exceeds = lists:any(fun(Place) ->
        {ok, Tokens} = pnet_marking:get(Marking, Place),
        length(Tokens) > Bound
    end, maps:keys(Marking)),

    case Exceeds of
        true -> exceed;
        false -> ok
    end.
