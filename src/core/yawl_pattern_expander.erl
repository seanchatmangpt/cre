%% -*- erlang -*-
%% @doc Pattern Expander - expands pattern instances into Petri net structures.
%%
%% **Joe Armstrong Design: Pure Helper Module (Stateless)**
%%
%% This module provides pure functional transformations from pattern instances
%% to Petri net structures. No state is maintained - all functions are
%% stateless data transformations.
%%
%% Takes a pattern_instance from YAML spec and generates the corresponding
%% Petri net structure (places, transitions, flows) for compilation.
-module(yawl_pattern_expander).
-export([
    expand_pattern/2,
    expand_all_patterns/2,
    expand_patterns_for_net/3,
    generate_places/2,
    generate_transitions/2,
    generate_flows/2
]).

-type pattern_instance() :: map().
-type net_structure() :: #{
    places => [atom()],
    transitions => [atom()],
    flows => [{atom(), atom()}],
    preset => #{atom() => [atom()]},
    postset => #{atom() => [atom()]}
}.

%% Expand a single pattern instance into net structure
-spec expand_pattern(pattern_instance(), map()) -> net_structure().

expand_pattern(PatternInstance, Context) ->
    PatternId = maps:get(pattern, PatternInstance, undefined),
    Module = yawl_pattern_registry:pattern_module(PatternId),
    case Module of
        undefined ->
            #{places => [], transitions => [], flows => [], preset => #{}, postset => #{}};
        _ ->
            expand_pattern_impl(Module, PatternInstance, Context)
    end.

%% Expand all pattern instances for a net
-spec expand_all_patterns([pattern_instance()], map()) -> net_structure().

expand_all_patterns(PatternInstances, Context) ->
    lists:foldl(fun(Instance, Acc) ->
        Expanded = expand_pattern(Instance, Context),
        merge_net_structures(Acc, Expanded)
    end, #{places => [], transitions => [], flows => [], preset => #{}, postset => #{}}, PatternInstances).

%% Expand pattern instances for a specific net (filters by net field).
-spec expand_patterns_for_net([pattern_instance()], NetId :: binary(), map()) -> net_structure().

expand_patterns_for_net(PatternInstances, NetId, Context) ->
    Filtered = [I || I <- PatternInstances, net_matches(I, NetId)],
    expand_all_patterns(Filtered, Context).

%% @private
net_matches(Instance, NetId) when is_binary(NetId) ->
    case maps:get(net, Instance, undefined) of
        N when is_binary(N) -> N =:= NetId;
        N when is_atom(N) -> atom_to_binary(N, utf8) =:= NetId;
        _ -> false
    end.

%% Generate places for a pattern
-spec generate_places(pattern_instance(), map()) -> [atom()].

generate_places(PatternInstance, _Context) ->
    PatternId = maps:get(pattern, PatternInstance, undefined),
    Module = yawl_pattern_registry:pattern_module(PatternId),
    case Module of
        undefined -> [];
        _ ->
            try
                Module:place_lst()
            catch
                _:_ -> []
            end
    end.

%% Generate transitions for a pattern
-spec generate_transitions(pattern_instance(), map()) -> [atom()].

generate_transitions(PatternInstance, _Context) ->
    PatternId = maps:get(pattern, PatternInstance, undefined),
    Module = yawl_pattern_registry:pattern_module(PatternId),
    case Module of
        undefined -> [];
        _ ->
            try
                Module:trsn_lst()
            catch
                _:_ -> []
            end
    end.

%% Generate flows for a pattern
-spec generate_flows(pattern_instance(), map()) -> [{atom(), atom()}].

generate_flows(PatternInstance, Context) ->
    PatternId = maps:get(pattern, PatternInstance, undefined),
    Module = yawl_pattern_registry:pattern_module(PatternId),
    case Module of
        undefined -> [];
        _ ->
            try
                Transitions = Module:trsn_lst(),
                lists:foldl(fun(Trsn, Acc) ->
                    Preset = Module:preset(Trsn),
                    Flows = [{P, Trsn} || P <- Preset],
                    Acc ++ Flows
                end, [], Transitions)
            catch
                _:_ -> []
            end
    end.

%% Internal: Expand pattern implementation
expand_pattern_impl(Module, PatternInstance, Context) ->
    Places = generate_places(PatternInstance, Context),
    Transitions = generate_transitions(PatternInstance, Context),
    Flows = generate_flows(PatternInstance, Context),
    
    %% Build preset/postset maps
    Preset = build_preset(Module, Transitions),
    Postset = build_postset(Module, Transitions),
    
    #{
        places => Places,
        transitions => Transitions,
        flows => Flows,
        preset => Preset,
        postset => Postset
    }.

%% Build preset map
build_preset(Module, Transitions) ->
    lists:foldl(fun(Trsn, Acc) ->
        try
            Preset = Module:preset(Trsn),
            Acc#{Trsn => Preset}
        catch
            _:_ -> Acc
        end
    end, #{}, Transitions).

%% Build postset map
build_postset(Module, Transitions) ->
    lists:foldl(fun(Trsn, Acc) ->
        try
            Preset = Module:preset(Trsn),
            %% For now, infer postset from flows
            %% In a full implementation, we'd call a postset/1 function
            Acc#{Trsn => []}
        catch
            _:_ -> Acc
        end
    end, #{}, Transitions).

%% Merge two net structures
merge_net_structures(Struct1, Struct2) ->
    #{
        places => lists:usort(maps:get(places, Struct1, []) ++ maps:get(places, Struct2, [])),
        transitions => lists:usort(maps:get(transitions, Struct1, []) ++ maps:get(transitions, Struct2, [])),
        flows => lists:usort(maps:get(flows, Struct1, []) ++ maps:get(flows, Struct2, [])),
        preset => maps:merge(maps:get(preset, Struct1, #{}), maps:get(preset, Struct2, #{})),
        postset => maps:merge(maps:get(postset, Struct1, #{}), maps:get(postset, Struct2, #{}))
    }.
