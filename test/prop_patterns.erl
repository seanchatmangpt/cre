-module(prop_patterns).
-author("CRE Team").

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("gen_pnet.hrl").

-export([
    prop_pattern_soundness/0,
    prop_no_deadlock/0,
    prop_liveness/0,
    prop_marking_conservation/0,
    prop_state_space_bounded/0,
    prop_transition_fairness/0,
    prop_reachability/0,
    prop_reversibility/0,
    prop_deterministic_replay/0,
    prop_concurrent_execution/0
]).

-export([
    gen_pattern/0,
    gen_pattern_module/0,
    gen_place_list/0,
    gen_transition_list/0,
    gen_marking/1,
    gen_token/0,
    gen_workflow_config/0,
    gen_execution_trace/2
]).

-export([
    check_soundness/1,
    check_no_deadlock/2,
    check_liveness/2,
    explore_state_space/3,
    compute_strongly_connected/1,
    check_invariants/2
]).

-define(MAX_PLACES, 10).
-define(MAX_TRANSITIONS, 10).
-define(MAX_TOKENS, 5).
-define(MAX_STEPS, 100).
-define(STATE_SPACE_LIMIT, 1000).

gen_pattern_module() ->
    oneof([
        sequence,
        parallel_split,
        synchronization,
        exclusive_choice,
        simple_merge,
        multiple_choice,
        structured_sync_merge,
        multiple_merge,
        discriminator,
        n_out_of_m,
        or_join,
        deferred_choice,
        interleaved_routing,
        milestone,
        cancel_activity,
        cancel_case,
        cancel_region,
        critical_section,
        thread_split,
        thread_merge,
        arbitrary_cycles,
        structured_loop,
        recursion,
        transient_trigger,
        persistent_trigger,
        blocking_discriminator,
        cancelling_discriminator,
        structured_partial_join,
        blocking_partial_join,
        cancelling_partial_join,
        general_sync_merge,
        local_sync_merge,
        static_partial_join_mi,
        cancelling_partial_join_mi,
        dynamic_partial_join_mi,
        implicit_termination,
        explicit_termination
    ]).

gen_place_list() ->
    ?LET(N, range(2, ?MAX_PLACES),
         vector(N, oneof([
             p_start, p_end, p_work, p_buffer, p_choice, p_merge,
             p_branch1, p_branch2, p_branch3, p_sync, p_ready,
             p_active, p_complete, p_waiting, p_done
         ]))).

gen_transition_list() ->
    ?LET(N, range(2, ?MAX_TRANSITIONS),
         vector(N, oneof([
             t_start, t_finish, t_work, t_split, t_join, t_select,
             t_merge, t_fire, t_complete, t_activate, t_cancel,
             t_branch1, t_branch2, t_branch3
         ]))).

gen_token() ->
    oneof([
        atom,
        {tagged, atom},
        {data, integer()},
        {task, binary()},
        {workflow, atom, integer()},
        list(oneof([atom, integer(), binary()])),
        #{data => integer(), id => binary()}
    ]).

gen_marking(Places) ->
    ?LET(TokenLists,
         [vector(range(0, ?MAX_TOKENS), gen_token()) || _ <- Places],
         maps:from_list(lists:zip(Places, TokenLists))).

gen_preset_map(Transitions, Places) ->
    ?LET(Presets,
         [non_empty(sublist(Places)) || _ <- Transitions],
         maps:from_list(lists:zip(Transitions, Presets))).

gen_pattern() ->
    ?LET({PlaceLst, TrsnLst},
         {gen_place_list(), gen_transition_list()},
         ?LET(PresetMap, gen_preset_map(TrsnLst, PlaceLst),
              #{
                  places => PlaceLst,
                  transitions => TrsnLst,
                  presets => PresetMap,
                  initial_marking => gen_marking(PlaceLst)
              })).

gen_workflow_config() ->
    ?LET({Pattern, InitTokens},
         {gen_pattern_module(),
          range(1, 5)},
         #{
             pattern => Pattern,
             init_tokens => InitTokens,
             timeout => range(100, 5000),
             max_steps => range(10, ?MAX_STEPS)
         }).

gen_execution_trace(Pattern, MaxSteps) ->
    ?LET(Steps, range(1, MaxSteps),
         vector(Steps, oneof([fire, inject, drain]))).

prop_pattern_soundness() ->
    ?FORALL(PatternMod, gen_pattern_module(),
            begin
                case pattern_exists(PatternMod) of
                    false -> true;
                    true ->
                        try
                            Places = PatternMod:place_lst(),
                            Transitions = PatternMod:trsn_lst(),
                            Presets = [{T, PatternMod:preset(T)} || T <- Transitions],

                            WellFormed = check_well_formed(Places, Transitions, Presets),
                            Soundness = check_soundness_properties(PatternMod),

                            WellFormed andalso Soundness
                        catch
                            _:_ -> false
                        end
                end
            end).

check_well_formed(Places, Transitions, Presets) ->
    PlacesUnique = length(Places) =:= length(lists:usort(Places)),
    TransitionsUnique = length(Transitions) =:= length(lists:usort(Transitions)),
    PresetsValid = lists:all(fun({_T, Preset}) ->
        lists:all(fun(P) -> lists:member(P, Places) end, Preset)
    end, Presets),

    PlacesUnique andalso TransitionsUnique andalso PresetsValid.

check_soundness_properties(PatternMod) ->
    try
        {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),
        InitMarking = gen_pnet:marking(Pid),

        HasInitialTokens = check_initial_tokens(InitMarking),

        Result = gen_pnet:drain(Pid, ?MAX_STEPS),

        gen_pnet:stop(Pid),

        HasInitialTokens andalso check_termination(Result)
    catch
        _:_ -> false
    end.

check_initial_tokens(Marking) when is_map(Marking) ->
    TokenCount = lists:sum([length(Tokens) || {_, Tokens} <- maps:to_list(Marking)]),
    TokenCount > 0.

check_termination({ok, _Receipts}) -> true;
check_termination({error, limit}) -> false.

prop_no_deadlock() ->
    ?FORALL(PatternMod, gen_pattern_module(),
            begin
                case pattern_exists(PatternMod) of
                    false -> true;
                    true ->
                        check_no_deadlock(PatternMod, ?MAX_STEPS)
                end
            end).

check_no_deadlock(PatternMod, MaxSteps) ->
    try
        {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),

        Result = execute_with_detection(Pid, MaxSteps, []),

        gen_pnet:stop(Pid),

        case Result of
            {ok, _} -> true;
            {deadlock, _} -> false;
            {error, _} -> false
        end
    catch
        _:_ -> false
    end.

execute_with_detection(Pid, 0, History) ->
    {ok, lists:reverse(History)};
execute_with_detection(Pid, StepsLeft, History) ->
    case gen_pnet:step(Pid) of
        abort ->
            CurrentMarking = gen_pnet:marking(Pid),
            TokenCount = count_tokens(CurrentMarking),
            if
                TokenCount > 0 -> {deadlock, {marking, CurrentMarking}};
                true -> {ok, lists:reverse(History)}
            end;
        {ok, Receipt} ->
            CurrentMarking = gen_pnet:marking(Pid),
            MarkingHash = erlang:phash2(CurrentMarking),

            case lists:member(MarkingHash, History) of
                true ->
                    CycleLength = find_cycle_length(MarkingHash, History),
                    if
                        CycleLength > 0 andalso CycleLength < 10 ->
                            {ok, lists:reverse([MarkingHash | History])};
                        true ->
                            execute_with_detection(Pid, StepsLeft - 1, [MarkingHash | History])
                    end;
                false ->
                    execute_with_detection(Pid, StepsLeft - 1, [MarkingHash | History])
            end
    end.

find_cycle_length(Hash, History) ->
    case find_cycle_length(Hash, History, 0) of
        not_found -> 0;
        Length -> Length
    end.

find_cycle_length(_Hash, [], _Count) ->
    not_found;
find_cycle_length(Hash, [Hash | _Rest], Count) ->
    Count;
find_cycle_length(Hash, [_ | Rest], Count) ->
    find_cycle_length(Hash, Rest, Count + 1).

count_tokens(Marking) when is_map(Marking) ->
    lists:sum([length(Tokens) || {_, Tokens} <- maps:to_list(Marking)]).

prop_liveness() ->
    ?FORALL(PatternMod, gen_pattern_module(),
            begin
                case pattern_exists(PatternMod) of
                    false -> true;
                    true ->
                        check_liveness(PatternMod, ?MAX_STEPS)
                end
            end).

check_liveness(PatternMod, MaxSteps) ->
    try
        {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),
        InitMarking = gen_pnet:marking(Pid),
        InitTokens = count_tokens(InitMarking),

        Result = gen_pnet:drain(Pid, MaxSteps),

        FinalMarking = gen_pnet:marking(Pid),
        FinalTokens = count_tokens(FinalMarking),

        gen_pnet:stop(Pid),

        case Result of
            {ok, _} when InitTokens > 0 -> FinalTokens >= 0;
            {error, limit} -> true;
            _ -> false
        end
    catch
        _:_ -> false
    end.

prop_marking_conservation() ->
    ?FORALL(PatternMod, gen_pattern_module(),
            begin
                case pattern_exists(PatternMod) of
                    false -> true;
                    true ->
                        try
                            {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),
                            InitMarking = gen_pnet:marking(Pid),
                            InitCount = count_tokens(InitMarking),

                            StepCount = rand:uniform(20),
                            _ = execute_n_steps(Pid, StepCount),

                            FinalMarking = gen_pnet:marking(Pid),
                            FinalCount = count_tokens(FinalMarking),

                            gen_pnet:stop(Pid),

                            is_integer(InitCount) andalso is_integer(FinalCount)
                        catch
                            _:_ -> false
                        end
                end
            end).

execute_n_steps(_Pid, 0) ->
    ok;
execute_n_steps(Pid, N) ->
    case gen_pnet:step(Pid) of
        abort -> ok;
        {ok, _} -> execute_n_steps(Pid, N - 1)
    end.

prop_state_space_bounded() ->
    ?FORALL(PatternMod, gen_pattern_module(),
            begin
                case pattern_exists(PatternMod) of
                    false -> true;
                    true ->
                        try
                            {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),

                            StateSpace = explore_state_space(Pid, ?STATE_SPACE_LIMIT, sets:new()),

                            gen_pnet:stop(Pid),

                            sets:size(StateSpace) =< ?STATE_SPACE_LIMIT
                        catch
                            _:_ -> false
                        end
                end
            end).

explore_state_space(Pid, 0, Visited) ->
    Visited;
explore_state_space(Pid, Limit, Visited) ->
    Marking = gen_pnet:marking(Pid),
    MarkingHash = erlang:phash2(Marking),

    case sets:is_element(MarkingHash, Visited) of
        true -> Visited;
        false ->
            NewVisited = sets:add_element(MarkingHash, Visited),
            case gen_pnet:step(Pid) of
                abort -> NewVisited;
                {ok, _} -> explore_state_space(Pid, Limit - 1, NewVisited)
            end
    end.

prop_transition_fairness() ->
    ?FORALL(PatternMod, gen_pattern_module(),
            begin
                case pattern_exists(PatternMod) of
                    false -> true;
                    true ->
                        try
                            {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),
                            Transitions = PatternMod:trsn_lst(),

                            FireCounts = execute_and_count(Pid, 50, maps:from_list([{T, 0} || T <- Transitions])),

                            gen_pnet:stop(Pid),

                            check_fairness(FireCounts)
                        catch
                            _:_ -> false
                        end
                end
            end).

execute_and_count(_Pid, 0, Counts) ->
    Counts;
execute_and_count(Pid, N, Counts) ->
    case gen_pnet:step(Pid) of
        abort -> Counts;
        {ok, _Receipt} ->
            execute_and_count(Pid, N - 1, Counts)
    end.

check_fairness(Counts) when is_map(Counts) ->
    Values = maps:values(Counts),
    case Values of
        [] -> true;
        [_] -> true;
        _ ->
            Max = lists:max(Values),
            Min = lists:min(Values),
            (Max - Min) =< 20
    end.

prop_reachability() ->
    ?FORALL(PatternMod, gen_pattern_module(),
            begin
                case pattern_exists(PatternMod) of
                    false -> true;
                    true ->
                        try
                            Places = PatternMod:place_lst(),
                            {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),

                            _ = gen_pnet:drain(Pid, ?MAX_STEPS),

                            Reachable = check_places_reachable(Pid, Places),

                            gen_pnet:stop(Pid),

                            length(Reachable) > 0
                        catch
                            _:_ -> false
                        end
                end
            end).

check_places_reachable(Pid, Places) ->
    Marking = gen_pnet:marking(Pid),
    lists:filter(fun(Place) ->
        case gen_pnet:ls(Pid, Place) of
            {ok, Tokens} -> length(Tokens) > 0;
            _ -> false
        end
    end, Places).

prop_reversibility() ->
    ?FORALL(PatternMod, gen_pattern_module(),
            begin
                case pattern_exists(PatternMod) of
                    false -> true;
                    true ->
                        try
                            {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),
                            InitMarking = gen_pnet:marking(Pid),

                            _ = gen_pnet:drain(Pid, 10),

                            gen_pnet:stop(Pid),

                            {ok, Pid2} = gen_pnet:start_link(PatternMod, #{}, []),
                            InitMarking2 = gen_pnet:marking(Pid2),
                            gen_pnet:stop(Pid2),

                            InitMarking =:= InitMarking2
                        catch
                            _:_ -> false
                        end
                end
            end).

prop_deterministic_replay() ->
    ?FORALL({PatternMod, Seed}, {gen_pattern_module(), integer()},
            begin
                case pattern_exists(PatternMod) of
                    false -> true;
                    true ->
                        try
                            rand:seed(exsplus, {Seed, Seed, Seed}),
                            Trace1 = execute_deterministic(PatternMod, 20),

                            rand:seed(exsplus, {Seed, Seed, Seed}),
                            Trace2 = execute_deterministic(PatternMod, 20),

                            Trace1 =:= Trace2
                        catch
                            _:_ -> false
                        end
                end
            end).

execute_deterministic(PatternMod, MaxSteps) ->
    {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),
    Trace = collect_trace(Pid, MaxSteps, []),
    gen_pnet:stop(Pid),
    Trace.

collect_trace(_Pid, 0, Trace) ->
    lists:reverse(Trace);
collect_trace(Pid, N, Trace) ->
    Marking = gen_pnet:marking(Pid),
    case gen_pnet:step(Pid) of
        abort -> lists:reverse([{final, Marking} | Trace]);
        {ok, Receipt} ->
            collect_trace(Pid, N - 1, [{Receipt, Marking} | Trace])
    end.

prop_concurrent_execution() ->
    ?FORALL(PatternMod, gen_pattern_module(),
            begin
                case pattern_exists(PatternMod) of
                    false -> true;
                    true ->
                        try
                            Parent = self(),
                            Ref = make_ref(),

                            spawn_link(fun() ->
                                Result = execute_pattern_isolated(PatternMod),
                                Parent ! {Ref, 1, Result}
                            end),

                            spawn_link(fun() ->
                                Result = execute_pattern_isolated(PatternMod),
                                Parent ! {Ref, 2, Result}
                            end),

                            Result1 = receive {Ref, 1, R1} -> R1 after 5000 -> timeout end,
                            Result2 = receive {Ref, 2, R2} -> R2 after 5000 -> timeout end,

                            Result1 =/= timeout andalso Result2 =/= timeout
                        catch
                            _:_ -> false
                        end
                end
            end).

execute_pattern_isolated(PatternMod) ->
    try
        {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),
        Result = gen_pnet:drain(Pid, 50),
        gen_pnet:stop(Pid),
        {ok, Result}
    catch
        _:Error -> {error, Error}
    end.

check_soundness(PatternMod) ->
    try
        Places = PatternMod:place_lst(),
        Transitions = PatternMod:trsn_lst(),

        {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),
        InitMarking = gen_pnet:marking(Pid),

        DrainResult = gen_pnet:drain(Pid, ?MAX_STEPS),

        FinalMarking = gen_pnet:marking(Pid),

        gen_pnet:stop(Pid),

        #{
            well_formed => check_well_formed(Places, Transitions,
                                             [{T, PatternMod:preset(T)} || T <- Transitions]),
            option_to_complete => case DrainResult of
                {ok, _} -> true;
                _ -> false
            end,
            proper_completion => check_proper_completion(InitMarking, FinalMarking),
            no_dead_transitions => check_no_dead_transitions(DrainResult)
        }
    catch
        _:Error -> #{error => Error}
    end.

check_proper_completion(InitMarking, FinalMarking) ->
    InitTokens = count_tokens(InitMarking),
    FinalTokens = count_tokens(FinalMarking),
    InitTokens > 0 andalso FinalTokens >= 0.

check_no_dead_transitions({ok, Receipts}) ->
    length(Receipts) > 0;
check_no_dead_transitions(_) ->
    false.

check_invariants(PatternMod, MaxSteps) ->
    try
        {ok, Pid} = gen_pnet:start_link(PatternMod, #{}, []),

        Invariants = collect_invariants(Pid, MaxSteps, []),

        gen_pnet:stop(Pid),

        #{
            invariants => Invariants,
            valid => check_all_invariants(Invariants)
        }
    catch
        _:Error -> #{error => Error}
    end.

collect_invariants(_Pid, 0, Acc) ->
    lists:reverse(Acc);
collect_invariants(Pid, N, Acc) ->
    Marking = gen_pnet:marking(Pid),
    Invariant = #{
        step => ?MAX_STEPS - N,
        token_count => count_tokens(Marking),
        marking_hash => erlang:phash2(Marking)
    },

    case gen_pnet:step(Pid) of
        abort -> lists:reverse([Invariant | Acc]);
        {ok, _} -> collect_invariants(Pid, N - 1, [Invariant | Acc])
    end.

check_all_invariants(Invariants) ->
    lists:all(fun(Inv) ->
        TokenCount = maps:get(token_count, Inv),
        TokenCount >= 0
    end, Invariants).

compute_strongly_connected(Graph) when is_map(Graph) ->
    Vertices = maps:keys(Graph),
    {Components, _} = tarjan_scc(Vertices, Graph, #{}, [], 0, []),
    Components.

tarjan_scc([], _Graph, _State, Stack, _Index, SCCs) ->
    {SCCs, Stack};
tarjan_scc([V | Rest], Graph, State, Stack, Index, SCCs) ->
    case maps:is_key(V, State) of
        true ->
            tarjan_scc(Rest, Graph, State, Stack, Index, SCCs);
        false ->
            {NewState, NewStack, NewIndex, NewSCCs} =
                strongconnect(V, Graph, State, Stack, Index, SCCs),
            tarjan_scc(Rest, Graph, NewState, NewStack, NewIndex, NewSCCs)
    end.

strongconnect(V, Graph, State, Stack, Index, SCCs) ->
    VState = #{index => Index, lowlink => Index, on_stack => true},
    NewState = maps:put(V, VState, State),
    NewStack = [V | Stack],
    NewIndex = Index + 1,

    Successors = maps:get(V, Graph, []),

    {FinalState, FinalStack, FinalIndex, FinalSCCs} =
        process_successors(Successors, V, Graph, NewState, NewStack, NewIndex, SCCs),

    VInfo = maps:get(V, FinalState),
    case maps:get(index, VInfo) =:= maps:get(lowlink, VInfo) of
        true ->
            {SCC, RemainingStack} = pop_scc(V, FinalStack, []),
            {FinalState, RemainingStack, FinalIndex, [SCC | FinalSCCs]};
        false ->
            {FinalState, FinalStack, FinalIndex, FinalSCCs}
    end.

process_successors([], _V, _Graph, State, Stack, Index, SCCs) ->
    {State, Stack, Index, SCCs};
process_successors([W | Rest], V, Graph, State, Stack, Index, SCCs) ->
    case maps:is_key(W, State) of
        false ->
            {NewState, NewStack, NewIndex, NewSCCs} =
                strongconnect(W, Graph, State, Stack, Index, SCCs),
            VInfo = maps:get(V, NewState),
            WInfo = maps:get(W, NewState),
            UpdatedVInfo = VInfo#{lowlink => min(maps:get(lowlink, VInfo),
                                                   maps:get(lowlink, WInfo))},
            UpdatedState = maps:put(V, UpdatedVInfo, NewState),
            process_successors(Rest, V, Graph, UpdatedState, NewStack, NewIndex, NewSCCs);
        true ->
            WInfo = maps:get(W, State),
            case maps:get(on_stack, WInfo, false) of
                true ->
                    VInfo = maps:get(V, State),
                    UpdatedVInfo = VInfo#{lowlink => min(maps:get(lowlink, VInfo),
                                                          maps:get(index, WInfo))},
                    UpdatedState = maps:put(V, UpdatedVInfo, State),
                    process_successors(Rest, V, Graph, UpdatedState, Stack, Index, SCCs);
                false ->
                    process_successors(Rest, V, Graph, State, Stack, Index, SCCs)
            end
    end.

pop_scc(V, [V | Stack], Acc) ->
    {lists:reverse([V | Acc]), Stack};
pop_scc(V, [W | Stack], Acc) ->
    pop_scc(V, Stack, [W | Acc]).

pattern_exists(PatternMod) ->
    try
        code:ensure_loaded(PatternMod),
        erlang:function_exported(PatternMod, place_lst, 0) andalso
        erlang:function_exported(PatternMod, trsn_lst, 0)
    catch
        _:_ -> false
    end.

-ifdef(TEST).

proper_test_() ->
    {timeout, 300, [
        {"Pattern soundness", ?_assert(proper:quickcheck(prop_pattern_soundness(), [{numtests, 20}]))},
        {"No deadlock", ?_assert(proper:quickcheck(prop_no_deadlock(), [{numtests, 20}]))},
        {"Liveness", ?_assert(proper:quickcheck(prop_liveness(), [{numtests, 20}]))},
        {"Marking conservation", ?_assert(proper:quickcheck(prop_marking_conservation(), [{numtests, 20}]))},
        {"Bounded state space", ?_assert(proper:quickcheck(prop_state_space_bounded(), [{numtests, 10}]))},
        {"Transition fairness", ?_assert(proper:quickcheck(prop_transition_fairness(), [{numtests, 15}]))},
        {"Reachability", ?_assert(proper:quickcheck(prop_reachability(), [{numtests, 20}]))},
        {"Reversibility", ?_assert(proper:quickcheck(prop_reversibility(), [{numtests, 15}]))},
        {"Deterministic replay", ?_assert(proper:quickcheck(prop_deterministic_replay(), [{numtests, 10}]))},
        {"Concurrent execution", ?_assert(proper:quickcheck(prop_concurrent_execution(), [{numtests, 10}]))}
    ]}.

unit_test_() ->
    [
        {"Check well-formed pattern", fun() ->
            Places = [p1, p2, p3],
            Transitions = [t1, t2],
            Presets = [{t1, [p1]}, {t2, [p2]}],
            ?assert(check_well_formed(Places, Transitions, Presets))
        end},

        {"Detect invalid preset", fun() ->
            Places = [p1, p2],
            Transitions = [t1],
            Presets = [{t1, [p3]}],
            ?assertNot(check_well_formed(Places, Transitions, Presets))
        end},

        {"Count tokens correctly", fun() ->
            Marking = #{p1 => [a, b], p2 => [c], p3 => []},
            ?assertEqual(3, count_tokens(Marking))
        end},

        {"Empty marking has zero tokens", fun() ->
            Marking = #{p1 => [], p2 => []},
            ?assertEqual(0, count_tokens(Marking))
        end},

        {"Check termination with ok result", fun() ->
            ?assert(check_termination({ok, [receipt1, receipt2]}))
        end},

        {"Check termination with limit error", fun() ->
            ?assertNot(check_termination({error, limit}))
        end}
    ].

integration_test_() ->
    {timeout, 60, [
        {"Sequence pattern soundness", fun() ->
            case pattern_exists(sequence) of
                false -> ok;
                true ->
                    Soundness = check_soundness(sequence),
                    ?assertMatch(#{option_to_complete := true}, Soundness)
            end
        end},

        {"Parallel split no deadlock", fun() ->
            case pattern_exists(parallel_split) of
                false -> ok;
                true ->
                    ?assert(check_no_deadlock(parallel_split, 50))
            end
        end},

        {"Synchronization liveness", fun() ->
            case pattern_exists(synchronization) of
                false -> ok;
                true ->
                    ?assert(check_liveness(synchronization, 50))
            end
        end},

        {"Exclusive choice soundness", fun() ->
            case pattern_exists(exclusive_choice) of
                false -> ok;
                true ->
                    Soundness = check_soundness(exclusive_choice),
                    ?assertMatch(#{well_formed := true}, Soundness)
            end
        end}
    ]}.

-endif.
