%% -*- erlang -*-
%% @doc Run AGI Symposium Ω demo with 20 LLM agents and full content visibility.
%%
%% Loads omega YAML, compiles via wf_yawl_executor, runs gen_yawl workflow
%% with 20 role-specific agents. Displays full prompts, JSON responses,
%% pattern execution, and workflow context.
%%
%% Usage: ./demo --omega
%%        ./demo --omega --dry-run  (no Z.AI, curated responses)
-module(omega_demo_runner).
-export([run/0]).

%%====================================================================
%% API
%%====================================================================

-spec run() -> ok | {error, term()}.
run() ->
    try
        do_run()
    catch
        throw:{fallback, ok} ->
            ok;
        Type:Reason:Stack ->
            io:format(standard_error, "ERROR: ~p:~p~n~p~n", [Type, Reason, Stack]),
            {error, {Type, Reason}}
    end.

%%====================================================================
%% Main
%%====================================================================

do_run() ->
    DryRun = os:getenv("DEMO_DRY_RUN") =/= false andalso os:getenv("DEMO_DRY_RUN") =/= "",
    ZaiEnabled = case os:getenv("ZAI_API_KEY") of
        false -> false;
        "" -> false;
        _ -> true
    end,
    TranscriptMode = os:getenv("DEMO_TRANSCRIPT") =/= false andalso os:getenv("DEMO_TRANSCRIPT") =/= "",

    case TranscriptMode of
        true -> agi_symposium_otel:print_transcript_banner();
        false -> agi_symposium_otel:print_omega_banner()
    end,

    run_omega_main(DryRun, ZaiEnabled, TranscriptMode).

run_omega_main(DryRun, ZaiEnabled, TranscriptMode) ->
    %% Ensure apps
    _ = application:load(cre),
    _ = application:ensure_all_started(yamerl),
    _ = application:ensure_all_started(inets),

    %% Load omega YAML
    YamlFile = "test/fixtures/agi_symposium_omega.yaml",
    Spec = case wf_yaml_spec:from_yaml_file(YamlFile) of
        {ok, S} -> S;
        {error, ParseErr} ->
            io:format(standard_error, "ERROR: Parse failed: ~p~n", [ParseErr]),
            throw({parse_error, ParseErr})
    end,

    %% Compile and load (fallback to 5-agent demo if omega fails)
    Executor = try
        case wf_yawl_executor:compile_workflow(Spec, #{}) of
            {ok, Exec} ->
                case wf_yawl_executor:ensure_modules_loaded(Exec) of
                    {ok, _} -> Exec;
                    _ -> throw(compile_fallback)
                end;
            _ -> throw(compile_fallback)
        end
    catch
        throw:compile_fallback ->
            io:format(standard_error, "Omega compile not yet supported, falling back to 5-agent demo~n", []),
            run_omega_fallback(DryRun, ZaiEnabled, TranscriptMode),
            throw({fallback, ok});
        error:_Reason ->
            io:format(standard_error, "Omega compile not yet supported, falling back to 5-agent demo~n", []),
            run_omega_fallback(DryRun, ZaiEnabled, TranscriptMode),
            throw({fallback, ok})
    end,

    %% Start workflow
    {ok, Pid, CaseId} = case wf_yawl_executor:start_workflow(Executor, #{}) of
        {ok, P, C} -> {ok, P, C};
        {error, Err3} ->
            io:format(standard_error, "ERROR: Start failed: ~p~n", [Err3]),
            throw({start_error, Err3})
    end,

    %% Init OTEL trace
    SpecId = wf_yaml_spec:id(Spec),
    agi_symposium_otel:log_workflow_start(CaseId, SpecId),

    %% Run loop with 20 agents
    Agents = agi_symposium_omega_agents:map_roles_to_agents(Spec, #{dry_run => DryRun, zai_enabled => ZaiEnabled, transcript_mode => TranscriptMode}),
    Result = run_omega_loop(Pid, Executor, Spec, CaseId, Agents, TranscriptMode, 0, 500),

    wf_yawl_executor:stop_workflow(Pid),
    agi_symposium_otel:log_workflow_complete(CaseId, maps:get(status, Result, unknown)),

    case TranscriptMode of
        true -> agi_symposium_otel:print_transcript_end();
        false -> agi_symposium_otel:print_omega_summary(Result, Spec)
    end,
    io:format(standard_error, "~nDemo complete.~n", []),
    ok.

%% When omega YAML does not compile, run 5-agent demo with omega-style output.
run_omega_fallback(DryRun, ZaiEnabled, TranscriptMode) ->
    Spec = agi_symposium_participants:agi_symposium_spec_full(),
    Opts = case ZaiEnabled of
        true -> #{zai_enabled => true, model => zai_client:get_model()};
        _ -> #{zai_enabled => false}
    end,
    RunOpts = maps:merge(Opts, #{live_stream => true, dry_run => DryRun}),
    case agi_symposium_simulator:run(Spec, RunOpts) of
        {ok, Result} ->
            case TranscriptMode of
                true -> agi_symposium_otel:print_transcript_end();
                false -> agi_symposium_otel:print_omega_summary(Result, undefined)
            end,
            io:format(standard_error, "~nDemo complete (5-agent fallback).~n", []);
        {error, Reason} ->
            io:format(standard_error, "ERROR: Demo failed: ~p~n", [Reason]),
            throw({error, Reason})
    end.

%%====================================================================
%% Loop
%%====================================================================

run_omega_loop(Pid, Executor, Spec, CaseId, Agents, TranscriptMode, Round, MaxRounds) when Round < MaxRounds ->
    MarkingBefore = gen_yawl:marking(Pid),
    case gen_yawl:step(Pid) of
        {ok, Receipt} ->
            PatternInfo = detect_pattern_execution(MarkingBefore, Receipt, Executor, Spec),
            track_pattern_execution(CaseId, PatternInfo, Spec),
            run_omega_loop(Pid, Executor, Spec, CaseId, Agents, TranscriptMode, Round + 1, MaxRounds);
        abort ->
            Marking = gen_yawl:marking(Pid),
            Context = extract_workflow_context(Pid, Executor, Spec),
            case TranscriptMode of
                false -> agi_symposium_otel:print_workflow_context(Context);
                true -> ok  %% Skip workflow context in transcript mode
            end,
            case find_and_complete_human_task(Pid, Executor, Spec, Marking, Agents, CaseId) of
                ok ->
                    run_omega_loop(Pid, Executor, Spec, CaseId, Agents, TranscriptMode, Round + 1, MaxRounds);
                done ->
                    #{status => completed, rounds => Round};
                none ->
                    case check_completed(Marking, Executor) of
                        true -> #{status => completed, rounds => Round};
                        false -> #{status => blocked, rounds => Round}
                    end
            end
    end;
run_omega_loop(_Pid, _Executor, _Spec, _CaseId, _Agents, _TranscriptMode, Round, _MaxRounds) ->
    #{status => timeout, rounds => Round}.

%%====================================================================
%% Human Task Completion
%%====================================================================

find_and_complete_human_task(Pid, Executor, Spec, Marking, Agents, CaseId) ->
    RootMod = wf_yawl_executor:get_root_module(Executor),
    try
        Transitions = RootMod:trsn_lst(),
        PresetMap = maps:from_list([{T, RootMod:preset(T)} || T <- Transitions]),
        case find_inject_place(Transitions, PresetMap, Marking) of
            undefined ->
                case run_subnets_if_needed(Pid, Executor, Spec, Marking) of
                    ok -> ok;
                    none -> none
                end;
            {Place, TaskName} ->
                Role = task_to_role(TaskName),
                Agent = maps:get(Role, Agents, fun default_agent/2),
                Context = extract_workflow_context(Pid, Executor, Spec),
                PatternLabel = get_current_pattern_label(Spec),
                Ctx = Context#{pattern_label => PatternLabel, case_id => CaseId},
                Decision = Agent(TaskName, Ctx),
                Data = maps:get(decision, Decision, <<"accept">>),
                gen_yawl:inject(Pid, #{Place => [Data]}),
                ok
        end
    catch
        _:_ -> none
    end.

%% Run subnets that have tokens in their entry places. Injects subnet outputs into root.
%% When subnets run, steps until no more automated transitions fire (Phase 1.4).
run_subnets_if_needed(Pid, Executor, Spec, Marking) ->
    case is_yaml_spec(Spec) of
        false -> none;
        true ->
            SubnetModules = wf_yawl_executor:get_subnet_modules(Executor),
            RootNet = wf_yaml_spec:root_net(Spec),
            SubnetDefs = wf_yaml_spec:net_subnets(Spec, RootNet),
            Ran = run_ready_subnets(Pid, Marking, SubnetModules, SubnetDefs, Executor),
            case Ran of
                true ->
                    %% Step until abort (drain automated transitions)
                    _ = step_until_abort(Pid, 100),
                    ok;
                false -> none
            end
    end.

step_until_abort(_Pid, 0) -> ok;
step_until_abort(Pid, N) ->
    case gen_yawl:step(Pid) of
        {ok, _} -> step_until_abort(Pid, N - 1);
        abort -> ok;
        {error, _} -> ok
    end.

is_yaml_spec(Spec) when is_tuple(Spec) -> element(1, Spec) =:= yawl_yaml_spec;
is_yaml_spec(_) -> false.

run_ready_subnets(_Pid, _Marking, [], _SubnetDefs, _Executor) -> false;
run_ready_subnets(Pid, Marking, SubnetModules, SubnetDefs, Executor) ->
    %% Run ALL subnets that have tokens (P3 Sync needs ProgramExit, OpsExit, CommsExit)
    %% Try both YAML names (ProgramThread) and P42 pattern names (p_thread1, p_thread2, ...)
    Ran = lists:foldl(
        fun({NetId, Mod}, Acc) ->
            case get_subnet_entry_exit(NetId, SubnetDefs, Executor) of
                {ok, Entry, Exit} ->
                    EntryAtom = to_place_atom(Entry),
                    ExitAtom = to_place_atom(Exit),
                    BranchPlace = binary_to_atom(NetId, utf8),
                    %% P42 pattern uses p_thread1..p_thread4; YAML uses ProgramThread, etc.
                    Index = subnet_index(NetId, SubnetModules),
                    P42Place = list_to_atom("p_thread" ++ integer_to_list(Index)),
                    BranchTokens = maps:get(BranchPlace, Marking, []) ++ maps:get(P42Place, Marking, []),
                    EntryTokens = maps:get(EntryAtom, Marking, []),
                    Tokens = BranchTokens ++ EntryTokens,
                    case Tokens of
                        [] -> Acc;
                        [T | _] ->
                            %% Consume token from root before passing to subnet
                            WithdrawPlace = case maps:get(BranchPlace, Marking, []) of
                                [_ | _] -> BranchPlace;
                                [] -> case maps:get(P42Place, Marking, []) of
                                    [_ | _] -> P42Place;
                                    [] -> EntryAtom
                                end
                            end,
                            _ = gen_yawl:withdraw(Pid, #{WithdrawPlace => [T]}),
                            %% Inject into P3 sync place (p_gonogo_branch1..3) when subnet completes.
                            %% P3 uses separate namespace to avoid collision with P38 (p_close_branch1..3).
                            InjectPlace = p_branch_place_for_subnet(Index),
                            run_one_subnet(Pid, Mod, EntryAtom, ExitAtom, InjectPlace, T),
                            true
                    end;
                undefined -> Acc
            end
        end,
        false,
        SubnetModules
    ),
    Ran.

subnet_index(NetId, SubnetModules) ->
    I = find_index(NetId, SubnetModules, 1),
    min(I, 4).  %% thread_split has at most 4 branch places

%% P3 sync preset uses p_gonogo_branch1..3 (separate from P38 p_close_branch1..3).
p_branch_place_for_subnet(1) -> p_gonogo_branch1;
p_branch_place_for_subnet(2) -> p_gonogo_branch2;
p_branch_place_for_subnet(3) -> p_gonogo_branch3;
p_branch_place_for_subnet(_) -> p_gonogo_branch3.  %% fallback for IncidentThread, etc.

find_index(NetId, [{NetId, _} | _], N) -> N;
find_index(NetId, [_ | Rest], N) -> find_index(NetId, Rest, N + 1);
find_index(_, [], N) -> N.

get_subnet_entry_exit(NetId, SubnetDefs, Executor) ->
    case lists:search(fun(S) -> subnet_id_match(S, NetId) end, SubnetDefs) of
        {value, Sub} ->
            Entry = maps:get(<<"entry">>, Sub, maps:get(entry, Sub, undefined)),
            Exit = maps:get(<<"exit">>, Sub, maps:get(exit, Sub, undefined)),
            case {Entry, Exit} of
                {E, X} when E =/= undefined, X =/= undefined -> {ok, E, X};
                _ -> undefined
            end;
        false ->
            case wf_yawl_executor:get_subnet_info(Executor, NetId) of
                #{entry := E, exit := X} -> {ok, E, X};
                _ -> undefined
            end
    end.

to_place_atom(B) when is_binary(B) -> binary_to_atom(B, utf8);
to_place_atom(A) when is_atom(A) -> A;
to_place_atom(_) -> undefined.

subnet_id_match(#{<<"id">> := Id}, NetId) when is_binary(NetId) -> Id =:= NetId;
subnet_id_match(#{id := Id}, NetId) when is_binary(NetId) -> Id =:= NetId;
subnet_id_match(_, _) -> false.

%% EntryPlace: subnet entry. ExitPlace: subnet exit (check completion). BranchPlace: root place to inject (F-003).
run_one_subnet(RootPid, SubnetMod, EntryPlace, ExitPlace, BranchPlace, Token) ->
    try
        case gen_yawl:start_link(SubnetMod, #{}, []) of
            {ok, SubPid} ->
                try
                    _ = gen_yawl:inject(SubPid, #{EntryPlace => [Token]}),
                    _ = gen_yawl:drain(SubPid, 500),
                    SubMarking = gen_yawl:marking(SubPid),
                    ExitTokens = maps:get(ExitPlace, SubMarking, []),
                    case ExitTokens of
                        [] -> ok;
                        [T | _] -> gen_yawl:inject(RootPid, #{BranchPlace => [T]})
                    end
                after
                    gen_yawl:stop(SubPid)
                end;
            {error, StartErr} ->
                io:format(standard_error, "Subnet ~p start failed: ~p~n", [SubnetMod, StartErr]),
                ok
        end
    catch
        Class:Err:Stack ->
            io:format(standard_error, "Subnet ~p error: ~p:~p~n~p~n", [SubnetMod, Class, Err, Stack]),
            ok
    end.

find_inject_place(Transitions, PresetMap, Marking) ->
    %% Prefer GoNoGo before CloseSymposium (flow order: Split->GoNoGo->OpenDoors->Close).
    %% Use P3 place namespace (p_gonogo_branch*) and P38 (p_close_branch*) for phase detection.
    Ordered = case lists:member(t_GoNoGo, Transitions) andalso lists:member(t_CloseSymposium, Transitions) of
        true ->
            %% Close phase: tokens in p_close_branch* or past GoNoGo (p_joined)
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

%% Strip t_ prefix so t_GoNoGo -> <<"GoNoGo">> for task_to_role and agent lookup.
task_name_from_transition(Trsn) ->
    Bin = atom_to_binary(Trsn, utf8),
    case binary:split(Bin, <<"_">>) of
        [<<"t">>, Rest] when byte_size(Rest) > 0 -> Rest;
        _ -> Bin
    end.

%% Helper: true if any of Places has tokens in Marking.
has_tokens_in_any([], _Marking) -> false;
has_tokens_in_any([P | Ps], Marking) ->
    case maps:get(P, Marking, []) of
        [] -> has_tokens_in_any(Ps, Marking);
        [_ | _] -> true
    end.

%% Handle [P1,P2] (one full, one empty), [P1] (single), or [P1,P2,...,Pn] (all but one full).
%% Never inject when all preset places are empty ({0,K}) - that causes premature injection.
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
            %% One full, two empty: agent provides second decision
            [{EmptyPl, _} | _] = Empty,
            {EmptyPl, TaskName};
        _ ->
            %% {0, K} excluded: all empty = no injection (prevents I-001 premature injection)
            undefined
    end;
find_inject_place_for_preset(_, _, _) -> undefined.

default_agent(_TaskName, _Context) ->
    #{decision => <<"accept">>}.

%%====================================================================
%% Task-to-Role Mapping (20 roles from omega YAML)
%%====================================================================

task_to_role(<<"GoNoGo">>) -> <<"Chair">>;
task_to_role(<<"CloseSymposium">>) -> <<"Chair">>;
task_to_role(<<"DeskTriage">>) -> <<"EthicsChair">>;
task_to_role(<<"ReviewCycle">>) -> <<"Reviewer">>;
task_to_role(<<"AuthorRebuttal">>) -> <<"AreaChair">>;
task_to_role(<<"MetaReview">>) -> <<"AreaChair">>;
task_to_role(<<"Decision">>) -> <<"ProgramChair">>;
task_to_role(<<"ProgramBuild">>) -> <<"ProgramChair">>;
task_to_role(<<"VenueRace">>) -> <<"VenueLead">>;
task_to_role(<<"SecurityClear">>) -> <<"SafetyOfficer">>;
task_to_role(<<"AVPrep">>) -> <<"AVLead">>;
task_to_role(<<"NetworkPrep">>) -> <<"OpsLead">>;
task_to_role(<<"TravelGrants">>) -> <<"TravelGrantsLead">>;
task_to_role(<<"VisaLetters">>) -> <<"FinanceLead">>;
task_to_role(<<"SpeakerOnboarding">>) -> <<"OpsLead">>;
task_to_role(<<"SponsorLiaison">>) -> <<"SponsorshipLead">>;
task_to_role(<<"IncidentIntake">>) -> <<"SafetyOfficer">>;
task_to_role(<<"IncidentResponse">>) -> <<"SafetyOfficer">>;
task_to_role(TaskName) when is_binary(TaskName) ->
    %% Fallback: use first role that could handle
    <<"Chair">>;
task_to_role(TaskName) when is_atom(TaskName) ->
    task_to_role(atom_to_binary(TaskName, utf8)).

%%====================================================================
%% Pattern Tracking
%%====================================================================

detect_pattern_execution(_MarkingBefore, Receipt, _Executor, _Spec) ->
    Trsn = maps:get(trsn, Receipt, undefined),
    #{trsn => Trsn}.

track_pattern_execution(CaseId, #{trsn := Trsn}, Spec) when Trsn =/= undefined ->
    Label = get_pattern_label_for_transition(Spec, Trsn),
    agi_symposium_otel:print_pattern_execution(CaseId, Trsn, Label);
track_pattern_execution(_CaseId, _, _Spec) ->
    ok.

get_current_pattern_label(Spec) ->
    Instances = wf_yaml_spec:pattern_instances(Spec),
    case Instances of
        [] -> <<"P1 Sequence">>;
        [First | _] ->
            to_binary(maps:get(<<"label">>, First, maps:get(label, First, <<"P1 Sequence">>)))
    end.

get_pattern_label_for_transition(Spec, Trsn) ->
    TrsnAtom = if is_atom(Trsn) -> Trsn; true -> binary_to_atom(Trsn, utf8) end,
    TrsnStr = atom_to_list(TrsnAtom),
    %% Strip t_ prefix if present
    TaskName = case string:prefix(TrsnStr, "t_") of
        nomatch -> TrsnStr;
        Rest -> Rest
    end,
    TaskAtom = list_to_atom(TaskName),
    Instances = wf_yaml_spec:pattern_instances(Spec),
    case lists:search(fun(I) ->
        match_task(TrsnAtom, TaskAtom, I)
    end, Instances) of
        {value, Inst} ->
            to_binary(maps:get(<<"label">>, Inst, maps:get(label, Inst, <<"P1 Sequence">>)));
        false -> atom_to_binary(Trsn, utf8)
    end.

match_task(_Trsn, Task, Inst) ->
    Keys = [split_task, join_task, merge_task, at, task],
    lists:any(fun(K) ->
        V = maps:get(K, Inst, maps:get(atom_to_binary(K, utf8), Inst, undefined)),
        V =:= Task orelse (is_atom(V) andalso atom_to_binary(V, utf8) =:= atom_to_binary(Task, utf8))
    end, Keys).

to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> iolist_to_binary(L).

%%====================================================================
%% Workflow Context
%%====================================================================

extract_workflow_context(Pid, Executor, Spec) ->
    Marking = gen_yawl:marking(Pid),
    RootMod = wf_yawl_executor:get_root_module(Executor),
    RootNet = wf_yaml_spec:root_net(Spec),
    VarDefs = wf_yaml_spec:net_variables(Spec, RootNet),
    Vars = lists:foldl(fun(V, Acc) ->
        Name = var_name(V),
        Initial = var_initial(V),
        Acc#{Name => Initial}
    end, #{}, VarDefs),
    #{marking => Marking, variables => Vars, root_module => RootMod}.

var_name(#{<<"name">> := N}) -> N;
var_name(#{name := N}) -> N;
var_name(_) -> <<"unknown">>.

var_initial(#{<<"initial">> := V}) -> V;
var_initial(#{initial := V}) -> V;
var_initial(_) -> undefined.

%%====================================================================
%% Completion Check
%%====================================================================

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
