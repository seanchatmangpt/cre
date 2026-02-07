%% -*- erlang -*-
%% @doc Run AGI Symposium Ω simulation with Z.AI LLM-backed human task decisions.
%%
%% Requires ZAI_API_KEY or cre.zai_api_key. Loads omega YAML, compiles,
%% loads modules, starts gen_yawl, and runs participant loop with Z.AI.
%%
%% Usage: rebar3 shell -eval "run_omega_with_zai:main([]), halt(0)."
-module(run_omega_with_zai).
-export([main/1]).

main(Args) ->
    try
        do_main(Args)
    catch
        Type:Reason:Stack ->
            io:format("ERROR: ~p:~p~n~p~n", [Type, Reason, Stack]),
            halt(1)
    end.

do_main(_Args) ->
    io:format("=== AGI Symposium Ω + Z.AI LLM Simulation ===~n~n"),

    %% Step 1: Check ZAI_API_KEY
    case zai_client:get_api_key() of
        undefined ->
            io:format("ERROR: ZAI_API_KEY not configured.~n"),
            io:format("  Set ZAI_API_KEY environment variable or cre.zai_api_key app env.~n"),
            halt(1);
        _ ->
            io:format("Step 1: Z.AI API key configured~n")
    end,

    %% Step 2: Start applications (yamerl for YAML, inets for HTTP)
    io:format("Step 2: Starting applications...~n"),
    _ = application:load(cre),
    case application:ensure_all_started(yamerl) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, R2} ->
            io:format("  ERROR: Failed to start yamerl: ~p~n", [R2]),
            halt(1)
    end,
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, _} -> ok
    end,
    io:format("  ✓ Applications started~n~n"),

    %% Step 3: Load YAML
    YamlFile = "test/fixtures/agi_symposium_omega.yaml",
    io:format("Step 3: Loading YAML from ~s~n", [YamlFile]),
    Spec = case wf_yaml_spec:from_yaml_file(YamlFile) of
        {ok, S} ->
            io:format("  ✓ Parsed: ~p, root=~p, nets=~p~n",
                [wf_yaml_spec:id(S), wf_yaml_spec:root_net(S), length(wf_yaml_spec:nets(S))]),
            S;
        {error, R3} ->
            io:format("  ERROR: Parse failed: ~p~n", [R3]),
            halt(1)
    end,

    %% Step 4: Compile
    io:format("Step 4: Compiling specification...~n"),
    Executor = case wf_yawl_executor:compile_workflow(Spec, #{}) of
        {ok, E} ->
            io:format("  ✓ Compiled, root_module=~p~n", [wf_yawl_executor:get_root_module(E)]),
            E;
        {error, R4} ->
            io:format("  ERROR: Compile failed: ~p~n", [R4]),
            halt(1)
    end,

    %% Step 5: Load modules
    io:format("Step 5: Loading compiled modules...~n"),
    case wf_yawl_executor:ensure_modules_loaded(Executor) of
        {ok, _} -> io:format("  ✓ Modules loaded~n");
        {error, R5} ->
            io:format("  ERROR: Module load failed: ~p~n", [R5]),
            halt(1)
    end,

    %% Step 6: Start workflow
    io:format("Step 6: Starting workflow (gen_yawl)...~n"),
    {ok, Pid, CaseId} = case wf_yawl_executor:start_workflow(Executor, #{}) of
        {ok, P, C} ->
            io:format("  ✓ Started: pid=~p, case_id=~p~n", [P, C]),
            {ok, P, C};
        {error, R6} ->
            io:format("  ERROR: Start failed: ~p~n", [R6]),
            halt(1)
    end,

    %% Step 7: Run participant loop with Z.AI
    io:format("Step 7: Running participant loop (Z.AI-backed)...~n"),
    Result = run_omega_loop(Pid, Executor, CaseId, 0, 100),
    wf_yawl_executor:stop_workflow(Pid),

    io:format("~n=== Simulation Complete ===~n"),
    io:format("  Case ID: ~p~n", [CaseId]),
    io:format("  Rounds: ~p~n", [maps:get(rounds, Result, 0)]),
    io:format("  Status: ~p~n", [maps:get(status, Result, unknown)]),
    io:format("~n✓ Omega + Z.AI LLM simulation finished.~n"),
    io:format("~n", []),
    ok.

run_omega_loop(Pid, Executor, CaseId, Round, MaxRounds) when Round < MaxRounds ->
    case gen_yawl:step(Pid) of
        {ok, _Receipt} ->
            run_omega_loop(Pid, Executor, CaseId, Round + 1, MaxRounds);
        abort ->
            Marking = gen_yawl:marking(Pid),
            case find_and_complete_human_task(Pid, Executor, Marking) of
                ok ->
                    run_omega_loop(Pid, Executor, CaseId, Round + 1, MaxRounds);
                done ->
                    #{status => completed, rounds => Round};
                none ->
                    case check_completed(Marking, Executor) of
                        true -> #{status => completed, rounds => Round};
                        false -> #{status => blocked, rounds => Round}
                    end
            end
    end;
run_omega_loop(_Pid, _Executor, _CaseId, Round, _MaxRounds) ->
    #{status => timeout, rounds => Round}.

find_and_complete_human_task(Pid, Executor, Marking) ->
    RootMod = wf_yawl_executor:get_root_module(Executor),
    try
        Transitions = RootMod:trsn_lst(),
        PresetMap = maps:from_list([{T, RootMod:preset(T)} || T <- Transitions]),
        InjectPlace = find_inject_place(Transitions, PresetMap, Marking),
        case InjectPlace of
            undefined -> none;
            Place ->
                Data = case zai_client:chat_json(
                    [#{role => <<"user">>, content => <<"Complete this workflow task. Reply JSON: {\"decision\":\"accept\"}">>}],
                    #{model => zai_client:get_model(), temperature => 0.3, max_tokens => 64}
                ) of
                    {ok, Json} -> maps:get(<<"decision">>, Json, <<"accept">>);
                    _ -> <<"accept">>
                end,
                gen_yawl:inject(Pid, #{Place => [Data]}),
                ok
        end
    catch
        _:_ -> none
    end.

find_inject_place(Transitions, PresetMap, Marking) ->
    lists:foldl(fun(Trsn, Acc) ->
        case Acc of
            undefined ->
                Preset = maps:get(Trsn, PresetMap, []),
                case Preset of
                    [P1, P2] ->
                        T1 = length(maps:get(P1, Marking, [])),
                        T2 = length(maps:get(P2, Marking, [])),
                        if T1 > 0, T2 =:= 0 -> P2;
                           T2 > 0, T1 =:= 0 -> P1;
                           true -> undefined
                        end;
                    _ -> undefined
                end;
            _ -> Acc
        end
    end, undefined, Transitions).

check_completed(Marking, Executor) ->
    RootMod = wf_yawl_executor:get_root_module(Executor),
    try
        Places = RootMod:place_lst(),
        case Places of
            [] -> false;
            _ ->
                EndPlace = lists:last(Places),
                length(maps:get(EndPlace, Marking, [])) > 0
        end
    catch _:_ -> false
    end.
