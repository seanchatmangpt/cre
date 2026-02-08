%% -*- erlang -*-
%% @doc Debugging helpers for workflow block diagnostics.
%%
%% Dump marking, enabled transitions, and block analysis when a workflow
%% is blocked (step/1 returns abort with no human task to complete).
%%
%% @end
-module(cre_debug).
-export([
    dump_blocked_state/3,
    format_marking/1,
    format_marking/2,
    analyze_block_reason/3
]).

%%====================================================================
%% API
%%====================================================================

%% @doc Dump marking, enabled transitions, and block analysis to stderr.
-spec dump_blocked_state(pid(), wf_yawl_executor:executor(), term()) -> ok.
dump_blocked_state(Pid, Executor, _Spec) ->
    Analysis = analyze_block_reason(Pid, Executor, gen_yawl:marking(Pid)),
    io:format(standard_error, "~n========== BLOCKED STATE DIAGNOSTIC ==========~n", []),
    io:format(standard_error, "Marking (non-empty places):~n~s~n",
        [format_marking(maps:get(marking, Analysis, #{}), #{include_empty => false})]),
    io:format(standard_error, "Enabled transitions: ~p~n", [maps:get(enabled, Analysis, [])]),
    case maps:get(find_inject_place, Analysis, undefined) of
        undefined ->
            io:format(standard_error, "find_inject_place: undefined (no human task ready)~n", []);
        {Place, Task} ->
            io:format(standard_error, "find_inject_place: ~p -> ~p~n", [Place, Task])
    end,
    io:format(standard_error, "run_subnets would run: ~p~n",
        [maps:get(run_subnets_has_tokens, Analysis, false)]),
    case maps:get(preset_analysis, Analysis, []) of
        [] -> ok;
        Presets ->
            io:format(standard_error, "Preset token counts (human tasks):~n", []),
            lists:foreach(fun({Trsn, Counts}) ->
                io:format(standard_error, "  ~p: ~p~n", [Trsn, Counts])
            end, Presets)
    end,
    io:format(standard_error, "==============================================~n~n", []),
    ok.

%% @doc Format marking as human-readable string. Excludes empty places by default.
-spec format_marking(#{atom() => [term()]}) -> iolist().
format_marking(Marking) ->
    format_marking(Marking, #{include_empty => false}).

%% @doc Format marking with options.
-spec format_marking(#{atom() => [term()]}, #{include_empty => boolean()}) -> iolist().
format_marking(Marking, Opts) ->
    IncludeEmpty = maps:get(include_empty, Opts, false),
    Pairs = maps:to_list(Marking),
    Filtered = case IncludeEmpty of
        true -> Pairs;
        false -> [{P, L} || {P, L} <- Pairs, L =/= []]
    end,
    [["  ", atom_to_list(P), " => ", integer_to_list(length(L)), " token(s)\n"]
     || {P, L} <- lists:sort(Filtered)].

%% @doc Analyze why a workflow is blocked. Returns a map with enabled, marking,
%% find_inject_place, run_subnets_has_tokens, preset_analysis.
-spec analyze_block_reason(pid(), wf_yawl_executor:executor(), #{atom() => [term()]}) ->
          #{atom() => term()}.
analyze_block_reason(Pid, Executor, Marking) ->
    RootMod = wf_yawl_executor:get_root_module(Executor),
    Transitions = RootMod:trsn_lst(),
    PresetMap = maps:from_list([{T, RootMod:preset(T)} || T <- Transitions]),
    FindResult = find_inject_place_analyze(Transitions, PresetMap, Marking),
    SubnetTokens = has_subnet_tokens(Executor, Marking),
    PresetAnalysis = preset_analysis(Transitions, PresetMap, Marking,
        [t_GoNoGo, t_CloseSymposium]),
    Enabled = try gen_yawl:enabled_transitions(Pid) of
        L when is_list(L) -> L
    catch _:_ -> []
    end,
    #{marking => Marking,
      enabled => Enabled,
      find_inject_place => FindResult,
      run_subnets_has_tokens => SubnetTokens,
      preset_analysis => PresetAnalysis}.

%%====================================================================
%% Internal
%%====================================================================

find_inject_place_analyze(Transitions, PresetMap, Marking) ->
    Ordered = case lists:member(t_GoNoGo, Transitions) andalso lists:member(t_CloseSymposium, Transitions) of
        true ->
            ClosePreset = maps:get(t_CloseSymposium, PresetMap, []),
            InClosePhase = has_tokens_in_any(ClosePreset, Marking)
                orelse maps:get(p_joined, Marking, []) =/= [],
            if InClosePhase -> [t_CloseSymposium, t_GoNoGo | lists:subtract(Transitions, [t_GoNoGo, t_CloseSymposium])];
               true -> [t_GoNoGo, t_CloseSymposium | lists:subtract(Transitions, [t_GoNoGo, t_CloseSymposium])]
            end;
        false -> Transitions
    end,
    lists:foldl(fun(Trsn, Acc) ->
        case Acc of
            undefined ->
                Preset = maps:get(Trsn, PresetMap, []),
                TaskName = task_name_from_transition(Trsn),
                case find_inject_place_for_preset(Preset, Marking, TaskName) of
                    undefined -> undefined;
                    Result -> Result
                end;
            _ -> Acc
        end
    end, undefined, Ordered).

find_inject_place_for_preset([P1, P2], Marking, TaskName) ->
    T1 = length(maps:get(P1, Marking, [])),
    T2 = length(maps:get(P2, Marking, [])),
    if T1 > 0, T2 =:= 0 -> {P2, TaskName};
       T2 > 0, T1 =:= 0 -> {P1, TaskName};
       true -> undefined
    end;
find_inject_place_for_preset([_], _Marking, _TaskName) ->
    undefined;
find_inject_place_for_preset(Preset, Marking, TaskName) when is_list(Preset), length(Preset) > 2 ->
    TokenCounts = [{Pl, length(maps:get(Pl, Marking, []))} || Pl <- Preset],
    {Full, Empty} = lists:partition(fun({_, C}) -> C > 0 end, TokenCounts),
    case {length(Full), length(Empty)} of
        {N, 1} when N >= 1 ->
            [{EmptyPl, _} | _] = Empty,
            {EmptyPl, TaskName};
        {1, 2} ->
            [{EmptyPl, _} | _] = Empty,
            {EmptyPl, TaskName};
        _ ->
            undefined
    end;
find_inject_place_for_preset(_, _, _) -> undefined.

task_name_from_transition(Trsn) ->
    Bin = atom_to_binary(Trsn, utf8),
    case binary:split(Bin, <<"_">>) of
        [<<"t">>, Rest] when byte_size(Rest) > 0 -> Rest;
        _ -> Bin
    end.

has_tokens_in_any([], _Marking) -> false;
has_tokens_in_any([P | Ps], Marking) ->
    case maps:get(P, Marking, []) of
        [] -> has_tokens_in_any(Ps, Marking);
        [_ | _] -> true
    end.

has_subnet_tokens(Executor, Marking) ->
    try
        SubnetModules = wf_yawl_executor:get_subnet_modules(Executor),
        lists:any(fun({NetId, _Mod}) ->
            BranchPlace = binary_to_atom(NetId, utf8),
            Index = subnet_index(NetId, SubnetModules),
            P42Place = list_to_atom("p_thread" ++ integer_to_list(Index)),
            BranchTokens = maps:get(BranchPlace, Marking, []) ++ maps:get(P42Place, Marking, []),
            BranchTokens =/= []
        end, SubnetModules)
    catch _:_ -> false
    end.

subnet_index(NetId, SubnetModules) ->
    I = find_index(NetId, SubnetModules, 1),
    min(I, 4).

find_index(NetId, [{NetId, _} | _], N) -> N;
find_index(NetId, [_ | Rest], N) -> find_index(NetId, Rest, N + 1);
find_index(_, [], N) -> N.

preset_analysis(Transitions, PresetMap, Marking, HumanTasks) ->
    Filtered = [T || T <- HumanTasks, lists:member(T, Transitions)],
    [{T, [{P, length(maps:get(P, Marking, []))} || P <- maps:get(T, PresetMap, [])]}
     || T <- Filtered].
