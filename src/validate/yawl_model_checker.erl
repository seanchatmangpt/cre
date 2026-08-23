%% -*- erlang -*-
%% @author CRE Team
%% @version 0.3.0
%% @doc Bounded Model Checking Validation Backend for YAWL Workflows
%%
%% This module provides formal verification of YAWL workflows by compiling
%% them to Petri nets and performing bounded state space exploration.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Detects deadlock states (no enabled transitions, not final)</li>
%%   <li>Detects dead transitions (unreachable tasks)</li>
%%   <li>Detects completion problems (workflow cannot finish)</li>
%%   <li>Bounded exploration for performance (depth D, token bound K)</li>
%%   <li>Integration with existing yawl_validate infrastructure</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Validate a workflow with default bounds (depth=15, token_bound=10)
%% {ok, Warnings} = yawl_model_checker:validate(Spec).
%%
%% %% Validate with custom bounds
%% {error, Errors} = yawl_model_checker:validate(Spec,
%%     #{depth => 20, token_bound => 15}).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_model_checker).

%%====================================================================
%% Exports
%%====================================================================

%% Main validation API
-export([validate/1, validate/2]).

%% Individual property checks
-export([check_deadlock/1, check_dead_transitions/2, check_completion/1]).

%%====================================================================
%% Types
%%====================================================================

%% Validation result
-type validation_result() :: {ok, [yawl_validate:validation_error()]} |
                             {error, term()}.

%% Exploration bounds
-type bounds() :: #{
    depth => pos_integer(),      %% Maximum exploration depth (default 15)
    token_bound => pos_integer() %% Maximum tokens per place (default 10)
}.

-export_type([validation_result/0, bounds/0]).

%%====================================================================
%% Main Validation API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validates a YAWL workflow specification using bounded model checking.
%%
%% Uses default bounds: depth=15, token_bound=10
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(Spec :: yawl_validate:specification()) -> validation_result().

validate(Spec) ->
    validate(Spec, #{depth => 15, token_bound => 10}).

%%--------------------------------------------------------------------
%% @doc Validates a YAWL workflow with custom exploration bounds.
%%
%% Bounds control the state space exploration:
%% <ul>
%%   <li><strong>depth:</strong> Maximum path length from initial state</li>
%%   <li><strong>token_bound:</strong> Maximum tokens allowed per place</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(Spec :: yawl_validate:specification(),
              Options :: bounds()) -> validation_result().

validate(Spec, Options) when is_map(Spec), is_map(Options) ->
    StartTime = erlang:monotonic_time(millisecond),
    try
        %% Compile to Petri net
        {ok, InitialMarking, Transitions} = yawl_pnet_compiler:compile(Spec),

        %% Explore bounded state space
        {ok, Traces} = yawl_explorer:explore(InitialMarking, Transitions, Options),

        %% Check properties
        Deadlocks = check_deadlock(Traces),
        DeadTransitions = check_dead_transitions(Traces, Transitions),
        Completion = check_completion(Traces),

        %% Combine results
        AllIssues = Deadlocks ++ DeadTransitions ++ Completion,

        EndTime = erlang:monotonic_time(millisecond),
        Duration = EndTime - StartTime,

        %% Log performance metrics
        io:format("Model checking completed in ~pms~n", [Duration]),
        io:format("Explored ~p traces~n", [length(Traces)]),

        {Errors, Warnings} = lists:partition(
            fun(#{severity := Sev}) -> Sev =:= error end,
            AllIssues
        ),

        case Errors of
            [] -> {ok, Warnings};
            _ -> {error, Errors ++ Warnings}
        end
    catch
        _:Error:StackTrace ->
            io:format("Model checking error: ~p~n~p~n", [Error, StackTrace]),
            {error, Error}
    end.

%%====================================================================
%% Property Checking Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Detects deadlock states in exploration traces.
%%
%% A deadlock is a state with no enabled transitions that is not
%% a final marking (all tokens at output conditions).
%%
%% @end
%%--------------------------------------------------------------------
-spec check_deadlock(Traces :: [yawl_explorer:trace()]) ->
    [yawl_validate:validation_error()].

check_deadlock(Traces) ->
    lists:filtermap(fun(Trace) ->
        case is_deadlock(Trace) of
            {true, LastStep} ->
                {_Transition, Marking} = LastStep,
                Error = #{
                    type => semantic,
                    severity => error,
                    message => <<"Deadlock detected: no enabled transitions but not in final state">>,
                    location => format_marking_location(Marking),
                    code => deadlock_detected
                },
                {true, Error};
            false ->
                false
        end
    end, Traces).

%%--------------------------------------------------------------------
%% @doc Detects dead (unreachable) transitions.
%%
%% A dead transition is one that never fired during exploration,
%% indicating an unreachable task or condition.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_dead_transitions(Traces :: [yawl_explorer:trace()],
                             AllTransitions :: [yawl_pnet_compiler:transition()]) ->
    [yawl_validate:validation_error()].

check_dead_transitions(Traces, AllTransitions) ->
    %% Collect all fired transitions from traces
    FiredTransitions = lists:usort(lists:flatmap(fun(Trace) ->
        extract_fired_transitions(Trace)
    end, Traces)),

    %% Find transitions that never fired
    AllTransitionIds = [Id || #{id := Id} <- AllTransitions],
    DeadTransitions = lists:filter(fun(Id) ->
        not lists:member(Id, FiredTransitions)
    end, AllTransitionIds),

    %% Generate warnings for dead transitions
    [begin
        #{
            type => semantic,
            severity => warning,
            message => iolist_to_binary([<<"Unreachable transition: '">>,
                                        atom_to_binary(Id, utf8), <<"'">>]),
            location => atom_to_binary(Id, utf8),
            code => dead_transition
        }
    end || Id <- DeadTransitions].

%%--------------------------------------------------------------------
%% @doc Checks if workflow can reach completion.
%%
%% Completion is possible if at least one trace reaches a final
%% marking (all tokens at output conditions).
%%
%% @end
%%--------------------------------------------------------------------
-spec check_completion(Traces :: [yawl_explorer:trace()]) ->
    [yawl_validate:validation_error()].

check_completion(Traces) ->
    %% Check if any trace reaches a final state
    HasCompletion = lists:any(fun(Trace) ->
        case Trace of
            [] -> false;
            _ ->
                {_, LastMarking} = lists:last(Trace),
                is_final_marking(LastMarking)
        end
    end, Traces),

    case HasCompletion of
        true -> [];
        false ->
            [#{
                type => semantic,
                severity => error,
                message => <<"Workflow cannot reach completion state">>,
                location => undefined,
                code => no_completion_path
            }]
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a trace ends in a deadlock state.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_deadlock(Trace :: yawl_explorer:trace()) ->
    {true, yawl_explorer:trace_step()} | false.

is_deadlock([]) ->
    false;
is_deadlock(Trace) ->
    LastStep = lists:last(Trace),
    {_, LastMarking} = LastStep,

    %% Check if this is a final state (all tokens at output conditions)
    IsFinal = is_final_marking(LastMarking),

    case IsFinal of
        true -> false;
        false ->
            %% Not final, but trace ended = deadlock
            {true, LastStep}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a marking is a final state.
%%
%% Final if all tokens are at output conditions.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_final_marking(pnet_marking:marking()) -> boolean().

is_final_marking(Marking) ->
    %% Final if all tokens are at output conditions (places starting with 'output')
    maps:fold(fun(Place, Tokens, Acc) ->
        PlaceStr = atom_to_list(Place),
        HasTokens = length(Tokens) > 0,
        case {PlaceStr, HasTokens} of
            {"output" ++ _, true} -> Acc andalso true;  %% Output condition with tokens - good
            {_, true} -> false;  %% Non-output place with tokens - not final
            {_, false} -> Acc  %% Empty place - don't affect result
        end
    end, true, Marking).

%%--------------------------------------------------------------------
%% @doc Extracts transition IDs from a trace.
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_fired_transitions(Trace :: yawl_explorer:trace()) ->
    [pnet_types:trsn()].

extract_fired_transitions(Trace) ->
    [TransitionId || {TransitionId, _Marking} <- Trace].

%%--------------------------------------------------------------------
%% @doc Formats a marking for error location reporting.
%%
%% @end
%%--------------------------------------------------------------------
-spec format_marking_location(pnet_marking:marking()) -> binary().

format_marking_location(Marking) ->
    %% Create a readable representation of the marking
    Places = lists:sort(maps:keys(Marking)),
    PlaceStrs = lists:map(fun(Place) ->
        {ok, Tokens} = pnet_marking:get(Marking, Place),
        TokenCount = length(Tokens),
        io_lib:format("~s:~p", [Place, TokenCount])
    end, Places),
    iolist_to_binary(["{", lists:join(<<", ">>, PlaceStrs), "}"]).
