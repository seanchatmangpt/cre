%% -*- erlang -*-
%% @author CRE Team
%% @version 0.3.0
%% @doc YAWL to Petri Net Compiler
%%
%% Compiles YAWL workflow specifications to Petri net representations
%% for formal verification.
%%
%% <h3>Compilation Strategy</h3>
%% <ul>
%%   <li>Tasks → Places</li>
%%   <li>Conditions → Places</li>
%%   <li>Flows → Transitions</li>
%%   <li>Initial marking → Tokens at input conditions</li>
%% </ul>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% {ok, InitialMarking, Transitions} = yawl_pnet_compiler:compile(Spec).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_pnet_compiler).

%%====================================================================
%% Exports
%%====================================================================

-export([compile/1]).

%%====================================================================
%% Types
%%====================================================================

%% Transition representation
-type transition() :: #{
    id => pnet_types:trsn(),
    preset => [pnet_types:place()],
    postset => [pnet_types:place()]
}.

-export_type([transition/0]).

%%====================================================================
%% API
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Compiles a YAWL specification to a Petri net.
%%
%% Returns the initial marking and list of transitions.
%%
%% @end
%%--------------------------------------------------------------------
-spec compile(Spec :: yawl_validate:specification()) ->
    {ok, pnet_marking:marking(), [transition()]} | {error, term()}.

compile(Spec) ->
    try
        %% Extract specification components
        Tasks = maps:get(tasks, Spec, #{}),
        Conditions = maps:get(conditions, Spec, #{}),
        Flows = maps:get(flows, Spec, []),

        %% Build place set (tasks and conditions)
        Places = build_places(Tasks, Conditions),

        %% Build initial marking with tokens at input conditions
        InitialMarking = build_initial_marking(Tasks, Conditions),

        %% Build transitions from flows
        Transitions = build_transitions(Tasks, Conditions, Flows),

        %% Verify compilation
        case verify_compilation(Places, InitialMarking, Transitions) of
            ok ->
                {ok, InitialMarking, Transitions};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Error:StackTrace ->
            io:format("Compilation error: ~p~n~p~n", [Error, StackTrace]),
            {error, Error}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Builds the set of places from tasks and conditions.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_places(Tasks :: map(), Conditions :: map()) -> [pnet_types:place()].

build_places(Tasks, Conditions) ->
    %% Convert task IDs to atoms
    TaskPlaces = [binary_to_atom(Id, utf8) || Id <- maps:keys(Tasks)],

    %% Convert condition IDs to atoms
    ConditionPlaces = [binary_to_atom(Id, utf8) || Id <- maps:keys(Conditions)],

    TaskPlaces ++ ConditionPlaces.

%%--------------------------------------------------------------------
%% @doc Builds the initial marking with tokens at input conditions.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_initial_marking(Tasks :: map(), Conditions :: map()) -> pnet_marking:marking().

build_initial_marking(Tasks, Conditions) ->
    %% Get all task and condition IDs
    TaskIds = maps:keys(Tasks),
    ConditionIds = maps:keys(Conditions),

    %% Create marking with all tasks and conditions as places
    TaskAtoms = [binary_to_atom(Id, utf8) || Id <- TaskIds],
    ConditionAtoms = [binary_to_atom(Id, utf8) || Id <- ConditionIds],
    Marking = pnet_marking:new(TaskAtoms ++ ConditionAtoms),

    %% Find all input conditions and place tokens there
    InputConditions = [Id || #{id := Id, type := Type} <- maps:values(Conditions),
                             Type =:= input_condition],

    %% Add initial tokens to input conditions
    lists:foldl(fun(Id, Acc) ->
        PlaceId = binary_to_atom(Id, utf8),
        pnet_marking:set(Acc, PlaceId, [start])
    end, Marking, InputConditions).

%%--------------------------------------------------------------------
%% @doc Builds transitions from flow definitions.
%%
%% Each flow becomes a transition that consumes from source and produces to target.
%%
%% @end
%%--------------------------------------------------------------------
-spec build_transitions(Tasks :: map(), Conditions :: map(), Flows :: [map()]) ->
    [transition()].

build_transitions(Tasks, Conditions, Flows) ->
    %% Build list of all valid places
    TaskIds = maps:keys(Tasks),
    ConditionIds = maps:keys(Conditions),
    AllIds = TaskIds ++ ConditionIds,

    %% Create a transition for each flow
    lists:filtermap(fun(Flow) ->
        try
            Id = maps:get(id, Flow),
            Source = maps:get(source, Flow),
            Target = maps:get(target, Flow),

            %% Verify source and target exist
            case lists:member(Source, AllIds) andalso lists:member(Target, AllIds) of
                true ->
                    Transition = #{
                        id => binary_to_atom(Id, utf8),
                        preset => [binary_to_atom(Source, utf8)],
                        postset => [binary_to_atom(Target, utf8)]
                    },
                    {true, Transition};
                false ->
                    %% Skip invalid flows (should be caught by structural validation)
                    false
            end
        catch
            _:_:_ -> false
        end
    end, Flows).

%%--------------------------------------------------------------------
%% @doc Verifies the compiled Petri net is well-formed.
%%
%% @end
%%--------------------------------------------------------------------
-spec verify_compilation(Places :: [pnet_types:place()],
                         Marking :: pnet_marking:marking(),
                         Transitions :: [transition()]) ->
    ok | {error, term()}.

verify_compilation(Places, Marking, Transitions) ->
    %% Check that initial marking places exist in place set
    MarkingPlaces = maps:keys(Marking),
    InvalidMarkingPlaces = lists:filter(fun(P) ->
        not lists:member(P, Places)
    end, MarkingPlaces),

    case InvalidMarkingPlaces of
        [] ->
            %% Check that transition presets/postsets reference valid places
            AllPlaces = sets:from_list(Places),
            InvalidTransitions = lists:filter(fun(#{preset := Preset, postset := Postset}) ->
                PresetSet = sets:from_list(Preset),
                PostsetSet = sets:from_list(Postset),
                not (sets:is_subset(PresetSet, AllPlaces) andalso
                     sets:is_subset(PostsetSet, AllPlaces))
            end, Transitions),

            case InvalidTransitions of
                [] -> ok;
                _ -> {error, {invalid_transition_places, InvalidTransitions}}
            end;
        _ ->
            {error, {invalid_marking_places, InvalidMarkingPlaces}}
    end.
