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

%% Expand a single pattern instance into net structure.
%% Uses YAML params (split_task, branches, waits_for, merge_task) when present
%% to map pattern places (p_thread1..4) to YAML branch names (ProgramThread, etc.).
-spec expand_pattern(pattern_instance(), map()) -> net_structure().

expand_pattern(PatternInstance, Context) ->
    PatternId = pi_get(pattern, PatternInstance),
    Module = yawl_pattern_registry:pattern_module(PatternId),
    case Module of
        undefined ->
            #{places => [], transitions => [], flows => [], preset => #{}, postset => #{}};
        _ ->
            expand_pattern_impl(Module, PatternInstance, Context)
    end.

%% @private Get value from pattern instance (handles both atom and binary keys)
pi_get(Key, Instance) when is_map(Instance) ->
    KeyBin = case Key of
        K when is_atom(K) -> atom_to_binary(K, utf8);
        K when is_binary(K) -> K
    end,
    case maps:get(Key, Instance, undefined) of
        undefined -> maps:get(KeyBin, Instance, undefined);
        V -> V
    end.

%% Expand all pattern instances for a net
-spec expand_all_patterns([pattern_instance()], map()) -> net_structure().

expand_all_patterns(PatternInstances, Context) ->
    lists:foldl(fun(Instance, Acc) ->
        Expanded = expand_pattern(Instance, Context),
        merge_net_structures(Acc, Expanded)
    end, #{places => [], transitions => [], flows => [], preset => #{}, postset => #{}}, PatternInstances).

%% Expand pattern instances for a specific net (filters by net field).
-spec expand_patterns_for_net([pattern_instance()], NetId :: binary() | atom(), map()) -> net_structure().

expand_patterns_for_net(PatternInstances, NetId, Context) ->
    NetIdBin = case NetId of
        B when is_binary(B) -> B;
        A when is_atom(A) -> atom_to_binary(A, utf8);
        _ -> <<"">>
    end,
    Filtered = [I || I <- PatternInstances, net_matches(I, NetIdBin)],
    expand_all_patterns(Filtered, Context).

%% @private
net_matches(Instance, NetId) when is_binary(NetId) ->
    N = pi_get(net, Instance),
    case N of
        B when is_binary(B) -> B =:= NetId;
        A when is_atom(A) -> atom_to_binary(A, utf8) =:= NetId;
        _ -> false
    end.

%% Generate places for a pattern
-spec generate_places(pattern_instance(), map()) -> [atom()].

generate_places(PatternInstance, _Context) ->
    PatternId = pi_get(pattern, PatternInstance),
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
    PatternId = pi_get(pattern, PatternInstance),
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
    PatternId = pi_get(pattern, PatternInstance),
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

%% Internal: Expand pattern implementation.
%% Uses branches/waits_for from PatternInstance to map p_thread1..4 to YAML names.
%% Renames transitions when join_task/join/split_task/merge_task present (avoids merge collision,
%% enables human task discovery by task name).
%% Namespaces internal transitions and places per instance to avoid collisions when merging.
expand_pattern_impl(Module, PatternInstance, Context) ->
    BasePlaces = generate_places(PatternInstance, Context),
    BaseTransitions = generate_transitions(PatternInstance, Context),
    BaseFlows = generate_flows(PatternInstance, Context),
    BasePreset = build_preset(Module, BaseTransitions),
    BasePostset = build_postset(Module, BaseTransitions),

    %% Build place mapping: p_thread1..4 -> YAML branch names when present
    PlaceMap = build_place_mapping(Module, PatternInstance, Context),
    {Places, Transitions, Preset, Postset, Flows} = apply_place_mapping(
        PlaceMap, BasePlaces, BaseTransitions, BasePreset, BasePostset, BaseFlows),

    %% Rename transitions for human tasks (t_join -> t_GoNoGo, etc.); namespace internal ones
    TrsnMap = build_transition_mapping(Module, PatternInstance),
    TrsnMap2 = add_internal_transition_namespace(Transitions, TrsnMap, PatternInstance),
    {FinalTransitions, FinalPreset, FinalPostset, FinalFlows} = apply_transition_mapping(
        TrsnMap2, Transitions, Preset, Postset, Flows),

    %% Namespace internal places (not in PlaceMap values); entry owner keeps p_start
    PlaceMap2 = add_internal_place_namespace(Places, PlaceMap, PatternInstance, Context),
    MapPlace = fun(P) -> maps:get(P, PlaceMap2, P) end,
    MapPlaces = fun(Ps) -> [MapPlace(Pl) || Pl <- Ps] end,
    FinalPlaces = lists:usort([MapPlace(P) || P <- Places]),
    FinalPreset2 = maps:fold(fun(Trsn, PresetList, Acc) ->
        Acc#{Trsn => MapPlaces(PresetList)}
    end, #{}, FinalPreset),
    FinalPostset2 = maps:fold(fun(Trsn, PostsetList, Acc) ->
        Acc#{Trsn => MapPlaces(PostsetList)}
    end, #{}, FinalPostset),
    FinalFlows2 = lists:usort([{MapPlace(From), To} || {From, To} <- FinalFlows]),

    #{
        places => FinalPlaces,
        transitions => FinalTransitions,
        flows => FinalFlows2,
        preset => FinalPreset2,
        postset => FinalPostset2
    }.

%% @private Build mapping from pattern place names (p_thread1..4) to YAML names.
%% P42: branches=[ProgramThread, OpsThread, ...] -> p_thread1->ProgramThread, etc.
%% P41: waits_for or branches; else infer from spec subnets (Context)
build_place_mapping(sequence, PatternInstance, _Context) ->
    From = pi_get(from, PatternInstance),
    To = pi_get(to, PatternInstance),
    FromA = to_atom(From),
    ToA = to_atom(To),
    case {FromA, ToA} of
        {A, B} when A =/= undefined, B =/= undefined ->
            #{p_task1 => A, p_task2 => B};
        _ -> #{}
    end;
build_place_mapping(parallel_split, PatternInstance, _Context) ->
    Branches = pi_get(branches, PatternInstance),
    build_branch_place_mapping_4(Branches);
build_place_mapping(simple_merge, PatternInstance, _Context) ->
    Froms = pi_get(froms, PatternInstance),
    case Froms of
        L when is_list(L), length(L) >= 2 ->
            [A, B | _] = L,
            #{p_branch_a => to_atom(A), p_branch_b => to_atom(B)};
        _ -> #{}
    end;
build_place_mapping(multiple_choice, PatternInstance, _Context) ->
    Branches = pi_get(branches, PatternInstance),
    build_branch_place_mapping_4(Branches);
build_place_mapping(structured_sync_merge, PatternInstance, _Context) ->
    Froms = pi_get(froms, PatternInstance),
    build_branch_place_mapping_4(Froms);
build_place_mapping(multiple_merge, PatternInstance, _Context) ->
    To = pi_get(to, PatternInstance),
    ToA = to_atom(To),
    case ToA of
        undefined -> #{};
        _ ->
            %% Map p_output to 'to' for output place; froms map to path places if available
            #{p_output => ToA}
    end;
build_place_mapping(discriminator, _PatternInstance, _Context) ->
    %% P9 discriminator has p_branch_pool, not p_race; structure differs
    #{};
build_place_mapping(blocking_discriminator, PatternInstance, _Context) ->
    BlocksUntil = pi_get(blocks_until, PatternInstance),
    build_branch_place_mapping(BlocksUntil, undefined);
build_place_mapping(cancelling_discriminator, PatternInstance, _Context) ->
    Race = pi_get(race, PatternInstance),
    build_race_place_mapping(Race);
build_place_mapping(structured_partial_join, _PatternInstance, _Context) -> #{};
build_place_mapping(blocking_partial_join, _PatternInstance, _Context) -> #{};
build_place_mapping(cancelling_partial_join, _PatternInstance, _Context) -> #{};
build_place_mapping(generalized_and_join, PatternInstance, _Context) ->
    Froms = pi_get(froms, PatternInstance),
    case Froms of
        L when is_list(L), L =/= [] -> build_branch_place_mapping(Froms, undefined);
        _ -> #{}
    end;
build_place_mapping(multiple_instances_sync, _PatternInstance, _Context) -> #{};
build_place_mapping(cancel_mi_activity, _PatternInstance, _Context) -> #{};
build_place_mapping(complete_mi_activity, _PatternInstance, _Context) -> #{};
build_place_mapping(static_partial_join_mi, _PatternInstance, _Context) -> #{};
build_place_mapping(cancelling_partial_join_mi, _PatternInstance, _Context) -> #{};
build_place_mapping(dynamic_partial_join_mi, _PatternInstance, _Context) -> #{};
build_place_mapping(deferred_choice, _PatternInstance, _Context) -> #{};
build_place_mapping(interleaved_routing, PatternInstance, _Context) ->
    Tasks = pi_get(tasks, PatternInstance),
    build_branch_place_mapping_4(Tasks);
build_place_mapping(milestone, PatternInstance, _Context) ->
    case task_ref(pi_get(gate_task, PatternInstance)) of
        T when T =/= undefined -> #{p_milestone_guard => to_atom(T)};
        _ -> #{}
    end;
build_place_mapping(transient_trigger, PatternInstance, _Context) ->
    case task_ref(pi_get(enabled_only_in, PatternInstance)) of
        T when T =/= undefined -> #{p_enabled => to_atom(T)};
        _ -> #{}
    end;
build_place_mapping(persistent_trigger, PatternInstance, _Context) ->
    case task_ref(pi_get(consumed_in, PatternInstance)) of
        T when T =/= undefined -> #{p_consume_ready => to_atom(T)};
        _ -> #{}
    end;
build_place_mapping(local_sync_merge, _PatternInstance, _Context) -> #{};
build_place_mapping(critical_section, PatternInstance, _Context) ->
    Mutex = task_ref(pi_get(mutex, PatternInstance)),
    Protected = task_ref(pi_get(protected, PatternInstance)),
    M = case Mutex of T when T =/= undefined -> #{p_lock_request => to_atom(T)}; _ -> #{} end,
    case Protected of P when P =/= undefined -> maps:put(p_critical, to_atom(P), M); _ -> M end;
build_place_mapping(cancel_activity, PatternInstance, _Context) ->
    case task_ref(pi_get(target, PatternInstance)) of
        T when T =/= undefined -> #{p_active => to_atom(T)};
        _ -> #{}
    end;
build_place_mapping(cancel_case, PatternInstance, _Context) ->
    case task_ref(pi_get(cancel_event, PatternInstance)) of
        T when T =/= undefined -> #{p_cancel_event => to_atom(T)};
        _ -> #{}
    end;
build_place_mapping(cancel_region, PatternInstance, _Context) ->
    case task_ref(pi_get(region, PatternInstance)) of
        T when T =/= undefined -> #{p_region_active => to_atom(T)};
        _ -> #{}
    end;
build_place_mapping(explicit_termination, PatternInstance, _Context) ->
    case task_ref(pi_get(terminator, PatternInstance)) of
        T when T =/= undefined -> #{p_terminate_event => to_atom(T)};
        _ -> #{}
    end;
build_place_mapping(structured_loop, PatternInstance, _Context) ->
    Entry = task_ref(pi_get(entry, PatternInstance)),
    case Entry of T when T =/= undefined -> #{p_body_ready => to_atom(T)}; _ -> #{} end;
build_place_mapping(arbitrary_cycles, PatternInstance, _Context) ->
    Nodes = pi_get(nodes, PatternInstance),
    build_branch_place_mapping_4(Nodes);
build_place_mapping(recursion, PatternInstance, _Context) ->
    case task_ref(pi_get(call, PatternInstance)) of
        T when T =/= undefined -> #{p_call_ready => to_atom(T)};
        _ -> #{}
    end;
build_place_mapping(thread_split, PatternInstance, _Context) ->
    Branches = pi_get(branches, PatternInstance),
    build_thread_place_mapping(Branches);
build_place_mapping(synchronization, PatternInstance, _Context) ->
    WaitsFor = pi_get(waits_for, PatternInstance),
    JoinTask = pi_get(join_task, PatternInstance),
    build_branch_place_mapping(WaitsFor, join_task_to_suffix(JoinTask));
build_place_mapping(general_sync_merge, PatternInstance, Context) ->
    Froms = pi_get(froms, PatternInstance),
    Join = pi_get(join, PatternInstance),
    Inferred = infer_froms_for_general_sync(Froms, PatternInstance, Context),
    build_branch_place_mapping(Inferred, join_to_suffix(Join));
build_place_mapping(thread_merge, PatternInstance, Context) ->
    WaitsFor = pi_get(waits_for, PatternInstance),
    Branches = pi_get(branches, PatternInstance),
    case WaitsFor of
        L when is_list(L), L =/= [] -> build_thread_place_mapping(L);
        _ ->
            case Branches of
                B when is_list(B), B =/= [] -> build_thread_place_mapping(B);
                _ ->
                    %% Infer from net subnets (e.g. Symposium has ProgramThread, OpsThread, ...)
                    NetIdRaw = pi_get(net, PatternInstance),
                    NetId = case NetIdRaw of
                        B when is_binary(B) -> B;
                        A when is_atom(A) -> atom_to_binary(A, utf8);
                        _ -> <<"">>
                    end,
                    Subnets = case maps:get(spec, Context, undefined) of
                        Spec when is_tuple(Spec), element(1, Spec) =:= yawl_yaml_spec ->
                            wf_yaml_spec:net_subnets(Spec, NetId);
                        _ -> []
                    end,
                    SubnetIds = [subnet_id(S) || S <- Subnets, subnet_id(S) =/= undefined],
                    build_thread_place_mapping(SubnetIds)
            end
    end;
build_place_mapping(_Module, _PatternInstance, _Context) ->
    #{}.

%% @private Map pattern transitions to YAML task names when join_task/join/split_task/merge_task.
%% Enables human task discovery (find_inject_place returns task name) and avoids merge collision.
build_transition_mapping(sequence, _PatternInstance) -> #{};
build_transition_mapping(parallel_split, PatternInstance) ->
    case task_ref(pi_get(split_task, PatternInstance)) of
        T when T =/= undefined -> #{t_split => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(exclusive_choice, PatternInstance) ->
    At = task_ref(pi_get(at, PatternInstance)),
    Choices = pi_get(choices, PatternInstance),
    M = case At of
        T when T =/= undefined -> #{t_finish => trsn_atom(T)};
        _ -> #{}
    end,
    case is_list(Choices) andalso length(Choices) >= 2 of
        true ->
            [C1, C2 | _] = Choices,
            M#{t_select_a => trsn_atom(C1), t_select_b => trsn_atom(C2)};
        false -> M
    end;
build_transition_mapping(simple_merge, PatternInstance) ->
    case task_ref(pi_get(to, PatternInstance)) of
        T when T =/= undefined -> #{t_finish => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(multiple_choice, PatternInstance) ->
    case task_ref(pi_get(at, PatternInstance)) of
        T when T =/= undefined -> #{t_start => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(structured_sync_merge, PatternInstance) ->
    case task_ref(pi_get(join, PatternInstance)) of
        T when T =/= undefined -> #{t_join => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(multiple_merge, PatternInstance) ->
    case task_ref(pi_get(to, PatternInstance)) of
        T when T =/= undefined -> #{t_forward => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(discriminator, PatternInstance) ->
    case task_ref(pi_get(winner_to, PatternInstance)) of
        T when T =/= undefined -> #{t_output => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(blocking_discriminator, PatternInstance) ->
    case task_ref(pi_get(trigger, PatternInstance)) of
        T when T =/= undefined -> #{t_trigger => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(cancelling_discriminator, _PatternInstance) -> #{};
build_transition_mapping(structured_partial_join, PatternInstance) ->
    case task_ref(pi_get(join, PatternInstance)) of
        T when T =/= undefined -> #{t_partial_join => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(blocking_partial_join, PatternInstance) ->
    case task_ref(pi_get(partial_out, PatternInstance)) of
        T when T =/= undefined -> #{t_partial => trsn_atom(T)};
        _ ->
            case task_ref(pi_get(final_out, PatternInstance)) of
                F when F =/= undefined -> #{t_final => trsn_atom(F)};
                _ -> #{}
            end
    end;
build_transition_mapping(cancelling_partial_join, _PatternInstance) -> #{};
build_transition_mapping(generalized_and_join, PatternInstance) ->
    case task_ref(pi_get(join, PatternInstance)) of
        T when T =/= undefined -> #{t_join => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(synchronization, PatternInstance) ->
    case task_ref(pi_get(join_task, PatternInstance)) of
        T when T =/= undefined -> #{t_join => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(general_sync_merge, PatternInstance) ->
    case task_ref(pi_get(join, PatternInstance)) of
        T when T =/= undefined -> #{t_join => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(thread_merge, PatternInstance) ->
    case task_ref(pi_get(merge_task, PatternInstance)) of
        T when T =/= undefined -> #{t_merge => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(thread_split, PatternInstance) ->
    case task_ref(pi_get(split_task, PatternInstance)) of
        T when T =/= undefined -> #{t_split => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(multiple_instances_sync, PatternInstance) ->
    case task_ref(pi_get(task, PatternInstance)) of
        T when T =/= undefined -> #{t_spawn => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(cancel_mi_activity, PatternInstance) ->
    case task_ref(pi_get(mi_task, PatternInstance)) of
        T when T =/= undefined -> #{t_create_instances => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(complete_mi_activity, PatternInstance) ->
    case task_ref(pi_get(mi_task, PatternInstance)) of
        T when T =/= undefined -> #{t_create_instances => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(static_partial_join_mi, _PatternInstance) -> #{};
build_transition_mapping(cancelling_partial_join_mi, _PatternInstance) -> #{};
build_transition_mapping(dynamic_partial_join_mi, _PatternInstance) -> #{};
build_transition_mapping(deferred_choice, _PatternInstance) -> #{};
build_transition_mapping(interleaved_routing, _PatternInstance) -> #{};
build_transition_mapping(milestone, PatternInstance) ->
    case task_ref(pi_get(gate_task, PatternInstance)) of
        T when T =/= undefined -> #{t_check_milestone => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(transient_trigger, PatternInstance) ->
    case task_ref(pi_get(event, PatternInstance)) of
        T when T =/= undefined -> #{t_event => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(persistent_trigger, PatternInstance) ->
    case task_ref(pi_get(consumed_in, PatternInstance)) of
        T when T =/= undefined -> #{t_consume => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(local_sync_merge, PatternInstance) ->
    case task_ref(pi_get(join, PatternInstance)) of
        T when T =/= undefined -> #{t_join => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(critical_section, _PatternInstance) -> #{};
build_transition_mapping(cancel_activity, PatternInstance) ->
    case task_ref(pi_get(target, PatternInstance)) of
        T when T =/= undefined -> #{t_start => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(cancel_case, PatternInstance) ->
    case task_ref(pi_get(cancel_event, PatternInstance)) of
        T when T =/= undefined -> #{t_cancel => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(cancel_region, PatternInstance) ->
    case task_ref(pi_get(cancel_event, PatternInstance)) of
        T when T =/= undefined -> #{t_cancel_region => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(explicit_termination, PatternInstance) ->
    case task_ref(pi_get(terminator, PatternInstance)) of
        T when T =/= undefined -> #{t_terminate => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(structured_loop, PatternInstance) ->
    case task_ref(pi_get(entry, PatternInstance)) of
        T when T =/= undefined -> #{t_start => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(arbitrary_cycles, _PatternInstance) -> #{};
build_transition_mapping(recursion, PatternInstance) ->
    case task_ref(pi_get(call, PatternInstance)) of
        T when T =/= undefined -> #{t_call => trsn_atom(T)};
        _ -> #{}
    end;
build_transition_mapping(_Module, _PatternInstance) ->
    #{}.

task_ref(V) when is_atom(V), V =/= undefined -> V;
task_ref(V) when is_binary(V), V =/= <<>> -> binary_to_atom(V, utf8);
task_ref(_) -> undefined.

trsn_atom(A) when is_atom(A) -> list_to_atom("t_" ++ atom_to_list(A));
trsn_atom(B) when is_binary(B) -> list_to_atom("t_" ++ binary_to_list(B)).

%% @private Build Internal TrsnMap: prefix transitions not renamed by YAML TrsnMap.
%% Prevents collisions when merging multiple patterns (e.g. t_split, t_complete1..3) on same net.
add_internal_transition_namespace(Transitions, TrsnMap, PatternInstance) ->
    %% Only namespace transitions that are NOT explicitly renamed by TrsnMap
    Prefix = namespace_prefix(PatternInstance),
    Internal = lists:foldl(fun(T, Acc) ->
        case maps:is_key(T, TrsnMap) of
            true -> Acc;
            false ->
                TStr = atom_to_list(T),
                Base = case string:prefix(TStr, "t_") of nomatch -> TStr; Rest -> Rest end,
                Acc#{T => list_to_atom("t_" ++ atom_to_list(Prefix) ++ "_" ++ Base)}
        end
    end, #{}, Transitions),
    maps:merge(Internal, TrsnMap).

%% @private Build Internal PlaceMap: prefix places not renamed by YAML PlaceMap.
%% Entry owner keeps p_start unnamespaced so init_marking finds it.
add_internal_place_namespace(Places, PlaceMap, PatternInstance, Context) ->
    IsEntryOwner = is_entry_owner(PatternInstance, Context),
    Prefix = namespace_prefix(PatternInstance),
    lists:foldl(fun(P, Acc) ->
        case {P, maps:is_key(P, PlaceMap), IsEntryOwner} of
            {p_start, false, true} -> Acc;
            {_, true, _} -> Acc;
            {_, false, _} ->
                PStr = atom_to_list(P),
                Base = case string:prefix(PStr, "p_") of nomatch -> PStr; Rest -> Rest end,
                Acc#{P => list_to_atom("p_" ++ atom_to_list(Prefix) ++ "_" ++ Base)}
        end
    end, PlaceMap, Places).

is_entry_owner(PatternInstance, Context) ->
    EntryId = maps:get(entry_owner_id, Context, undefined),
    case EntryId of
        undefined -> false;
        _ ->
            InstId = pi_get(id, PatternInstance),
            (InstId =:= EntryId) orelse
                (is_binary(InstId) andalso is_binary(EntryId) andalso InstId =:= EntryId)
    end.

namespace_prefix(PatternInstance) ->
    Id = pi_get(id, PatternInstance),
    IdStr = case Id of
        B when is_binary(B) -> binary_to_list(B);
        A when is_atom(A) -> atom_to_list(A);
        L when is_list(L) -> L;
        _ -> "unknown"
    end,
    list_to_atom("pi" ++ integer_to_list(erlang:phash2(IdStr))).

%% @private Rename transitions in preset, postset, flows.
%% Note: #{} matches any map in Erlang; use map_size guard for empty check.
apply_transition_mapping(TrsnMap, Transitions, Preset, Postset, Flows) when map_size(TrsnMap) =:= 0 ->
    {Transitions, Preset, Postset, Flows};
apply_transition_mapping(TrsnMap, Transitions, Preset, Postset, Flows) ->
    MapTrsn = fun(T) -> maps:get(T, TrsnMap, T) end,
    NewTransitions = [MapTrsn(T) || T <- Transitions],
    NewPreset = maps:fold(fun(Trsn, V, Acc) -> Acc#{MapTrsn(Trsn) => V} end, #{}, Preset),
    NewPostset = maps:fold(fun(Trsn, V, Acc) -> Acc#{MapTrsn(Trsn) => V} end, #{}, Postset),
    NewFlows = [{From, MapTrsn(To)} || {From, To} <- Flows],
    {NewTransitions, NewPreset, NewPostset, NewFlows}.

subnet_id(#{<<"id">> := Id}) -> Id;
subnet_id(#{id := Id}) -> Id;
subnet_id(_) -> undefined.

build_thread_place_mapping(Branches) when is_list(Branches), Branches =/= [] ->
    Defaults = [p_thread1, p_thread2, p_thread3, p_thread4],
    %% Build pairs manually to avoid zip/2 function_clause when lists have different structure
    Pairs = build_zip_safe(Defaults, Branches),
    maps:from_list([{P, to_atom(B)} || {P, B} <- Pairs, B =/= undefined, is_valid_branch_ref(B)]);
build_thread_place_mapping(_) ->
    #{}.

%% @private Map p_race1..3 to race list (discriminator, cancelling_discriminator).
build_race_place_mapping(Race) when is_list(Race), Race =/= [] ->
    Defaults = [p_race1, p_race2, p_race3],
    Pairs = build_zip_safe(Defaults, Race),
    maps:from_list([{P, to_atom(B)} || {P, B} <- Pairs, B =/= undefined, is_valid_branch_ref(B)]);
build_race_place_mapping(_) -> #{}.

%% @private Like build_thread_place_mapping but for p_branch1..4.
build_branch_place_mapping_4(Branches) ->
    build_branch_place_mapping_4(Branches, undefined).
build_branch_place_mapping_4(Branches, undefined) when is_list(Branches), Branches =/= [] ->
    Defaults = [p_branch1, p_branch2, p_branch3, p_branch4],
    Pairs = build_zip_safe(Defaults, Branches),
    maps:from_list([{P, to_atom(B)} || {P, B} <- Pairs, B =/= undefined, is_valid_branch_ref(B)]);
build_branch_place_mapping_4(Branches, Suffix) when is_list(Branches), Branches =/= [], Suffix =/= undefined ->
    Defaults = [p_branch1, p_branch2, p_branch3, p_branch4],
    SuffixAtom = suffix_to_atom(Suffix),
    Targets = [list_to_atom("p_" ++ atom_to_list(SuffixAtom) ++ "_branch" ++ integer_to_list(I)) || I <- [1, 2, 3, 4]],
    Pairs = build_zip_safe(Defaults, Targets),
    maps:from_list([{P, T} || {P, T} <- Pairs]);
build_branch_place_mapping_4(_, _) -> #{}.

build_branch_place_mapping(Branches, undefined) when is_list(Branches), Branches =/= [] ->
    Defaults = [p_branch1, p_branch2, p_branch3],
    Pairs = build_zip_safe(Defaults, Branches),
    maps:from_list([{P, to_atom(B)} || {P, B} <- Pairs, B =/= undefined, is_valid_branch_ref(B)]);
build_branch_place_mapping(Branches, Suffix) when is_list(Branches), Branches =/= [], Suffix =/= undefined ->
    Defaults = [p_branch1, p_branch2, p_branch3],
    SuffixAtom = suffix_to_atom(Suffix),
    Targets = [list_to_atom("p_" ++ atom_to_list(SuffixAtom) ++ "_branch" ++ integer_to_list(I)) || I <- [1, 2, 3]],
    Pairs = build_zip_safe(Defaults, Targets),
    maps:from_list([{P, T} || {P, T} <- Pairs]);
build_branch_place_mapping(Branches, Suffix) when (Branches =:= undefined orelse not is_list(Branches) orelse Branches =:= []), Suffix =/= undefined ->
    Defaults = [p_branch1, p_branch2, p_branch3],
    SuffixAtom = suffix_to_atom(Suffix),
    Targets = [list_to_atom("p_" ++ atom_to_list(SuffixAtom) ++ "_branch" ++ integer_to_list(I)) || I <- [1, 2, 3]],
    maps:from_list(lists:zip(Defaults, Targets));
build_branch_place_mapping(_, _) ->
    #{}.

%% @private Derive place suffix from join_task (e.g. GoNoGo -> gonogo)
%% Must match omega_demo_runner p_branch_place_for_subnet (p_gonogo_branch1..3).
join_task_to_suffix(undefined) -> undefined;
join_task_to_suffix(<<"GoNoGo">>) -> <<"gonogo">>;
join_task_to_suffix('GoNoGo') -> <<"gonogo">>;
join_task_to_suffix(JoinTask) -> task_ref_to_suffix(JoinTask).

%% @private Derive place suffix from join (e.g. CloseSymposium -> close)
join_to_suffix(undefined) -> undefined;
join_to_suffix(<<"CloseSymposium">>) -> <<"close">>;
join_to_suffix(CloseSymposium) when CloseSymposium =:= 'CloseSymposium' -> <<"close">>;
join_to_suffix(Join) -> task_ref_to_suffix(Join).

task_ref_to_suffix(A) when is_atom(A) -> atom_to_binary(A, utf8);
task_ref_to_suffix(B) when is_binary(B) -> B;
task_ref_to_suffix(_) -> undefined.

suffix_to_atom(<<>>) -> undefined;
suffix_to_atom(B) when is_binary(B) -> binary_to_atom(B, utf8);
suffix_to_atom(A) when is_atom(A) -> A;
suffix_to_atom(_) -> undefined.

%% @private Infer froms for P38 when not specified (e.g. from net subnets)
infer_froms_for_general_sync(Froms, _PatternInstance, _Context) when is_list(Froms), Froms =/= [] ->
    Froms;
infer_froms_for_general_sync(_, PatternInstance, Context) ->
    NetIdRaw = pi_get(net, PatternInstance),
    NetId = case NetIdRaw of
        B when is_binary(B) -> B;
        A when is_atom(A) -> atom_to_binary(A, utf8);
        _ -> <<>>
    end,
    case maps:get(spec, Context, undefined) of
        Spec when is_tuple(Spec), element(1, Spec) =:= yawl_yaml_spec ->
            Subnets = wf_yaml_spec:net_subnets(Spec, NetId),
            Exits = [subnet_exit(S) || S <- Subnets, subnet_exit(S) =/= undefined],
            case Exits of
                [_, _, _ | _] -> Exits;
                _ -> [1, 2, 3]
            end;
        _ -> [1, 2, 3]
    end.

subnet_exit(#{<<"exit">> := E}) -> E;
subnet_exit(#{exit := E}) -> E;
subnet_exit(_) -> undefined.

%% @private Safe zip: handles any list lengths, never causes function_clause
build_zip_safe([], _) -> [];
build_zip_safe(_, []) -> [];
build_zip_safe([P | Ps], [B | Bs]) ->
    [{P, B} | build_zip_safe(Ps, Bs)].

is_valid_branch_ref(B) when is_binary(B) -> true;
is_valid_branch_ref(A) when is_atom(A) -> true;
is_valid_branch_ref(_) -> false.

to_atom(B) when is_binary(B) -> binary_to_atom(B, utf8);
to_atom(A) when is_atom(A) -> A;
to_atom(L) when is_list(L) -> list_to_atom(L);
to_atom(_) -> undefined.

%% @private Apply place mapping to all structures.
%% Replaces p_thread1..4 with YAML branch names (ProgramThread, etc.)
apply_place_mapping(#{}, Places, Transitions, Preset, Postset, Flows) ->
    {Places, Transitions, Preset, Postset, Flows};
apply_place_mapping(PlaceMap, BasePlaces, Transitions, BasePreset, BasePostset, BaseFlows) ->
    MapPlace = fun(P) -> maps:get(P, PlaceMap, P) end,
    MapPlaces = fun(Ps) -> [MapPlace(P) || P <- Ps] end,

    Places = lists:usort([MapPlace(P) || P <- BasePlaces]),
    Preset = maps:fold(fun(Trsn, PresetList, Acc) ->
        Acc#{Trsn => MapPlaces(PresetList)}
    end, #{}, BasePreset),
    Postset = maps:fold(fun(Trsn, PostsetList, Acc) ->
        Acc#{Trsn => MapPlaces(PostsetList)}
    end, #{}, BasePostset),
    Flows = lists:usort([{MapPlace(From), MapPlace(To)} || {From, To} <- BaseFlows]),

    {Places, Transitions, Preset, Postset, Flows}.

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

%% Build postset map. For thread_split/thread_merge, known postset from fire/3 behavior.
build_postset(sequence, Transitions) ->
    PostsetMap = #{
        t_start => [p_task1],
        t_complete1 => [p_task2],
        t_complete2 => [p_task2],
        t_finish => [p_end]
    },
    lists:foldl(fun(Trsn, Acc) ->
        Acc#{Trsn => maps:get(Trsn, PostsetMap, [])}
    end, #{}, Transitions);
build_postset(thread_split, Transitions) ->
    PostsetMap = #{
        t_split => [p_thread1, p_thread2, p_thread3, p_thread4],
        t_finish1 => [p_end],
        t_finish2 => [p_end],
        t_finish3 => [p_end],
        t_finish4 => [p_end]
    },
    lists:foldl(fun(Trsn, Acc) ->
        Acc#{Trsn => maps:get(Trsn, PostsetMap, [])}
    end, #{}, Transitions);
build_postset(thread_merge, Transitions) ->
    PostsetMap = #{
        t_split => [p_thread1, p_thread2, p_thread3, p_thread4],
        t_complete1 => [p_thread1],
        t_complete2 => [p_thread2],
        t_complete3 => [p_thread3],
        t_complete4 => [p_thread4],
        t_merge => [p_merged],
        t_finish => [p_end]
    },
    lists:foldl(fun(Trsn, Acc) ->
        Acc#{Trsn => maps:get(Trsn, PostsetMap, [])}
    end, #{}, Transitions);
build_postset(synchronization, Transitions) ->
    PostsetMap = #{
        t_split => [p_branch1, p_branch2, p_branch3],
        t_complete1 => [p_branch1],
        t_complete2 => [p_branch2],
        t_complete3 => [p_branch3],
        t_join => [p_joined],
        t_finish => [p_end]
    },
    lists:foldl(fun(Trsn, Acc) ->
        Acc#{Trsn => maps:get(Trsn, PostsetMap, [])}
    end, #{}, Transitions);
build_postset(general_sync_merge, Transitions) ->
    PostsetMap = #{
        t_split => [p_branch1, p_branch2, p_branch3],
        t_complete1 => [p_branch1],
        t_complete2 => [p_branch2],
        t_complete3 => [p_branch3],
        t_join => [p_joined],
        t_finish => [p_end]
    },
    lists:foldl(fun(Trsn, Acc) ->
        Acc#{Trsn => maps:get(Trsn, PostsetMap, [])}
    end, #{}, Transitions);
build_postset(Module, Transitions) ->
    lists:foldl(fun(Trsn, Acc) ->
        try
            case erlang:function_exported(Module, postset, 1) of
                true -> Acc#{Trsn => Module:postset(Trsn)};
                false -> Acc#{Trsn => []}
            end
        catch
            _:_ -> Acc#{Trsn => []}
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
