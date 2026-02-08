# Executor Pattern Analysis: wf_yawl_executor.erl

## Introduction

The `wf_yawl_executor.erl` module implements the Executor Pattern from Generative Analysis, providing a complete end-to-end execution API for YAWL (Yet Another Workflow Language) workflows. This analysis examines how the executor demonstrates the five key dimensions of the pattern: YAML-to-execution pipeline, human task injection/extraction, transcript mode vs debug mode, swarm coordination through workflow topology, and admissibility boundary enforcement.

## 1. The YAML-to-Execution Pipeline

### 1.1 Loading and Parsing

The executor provides multiple entry points for loading workflows, supporting both XML and YAML specifications:

```erlang
%% File-based loading (lines 266-275)
load_workflow(FilePath) when is_list(FilePath); is_binary(FilePath) ->
    case wf_spec:from_xml_file(FilePath) of
        {ok, Spec} ->
            load_workflow_from_spec(Spec);
        {error, Reason} ->
            {error, {load_error, Reason}}
    end.

%% YAML support (lines 316-325)
load_workflow_from_yaml(FilePath) when is_list(FilePath); is_binary(FilePath) ->
    case wf_yaml_spec:from_yaml_file(FilePath) of
        {ok, Spec} ->
            load_workflow_from_yaml_spec(Spec);
        {error, Reason} ->
            {error, {load_error, Reason}}
    end.
```

### 1.2 Compilation to Petri Net Modules

The compilation process, handled by `yawl_compile:compile/2`, transforms YAWL specifications into gen_pnet-compatible Petri net modules. Each decomposition becomes a standalone Erlang module implementing the gen_pnet behavior:

```erlang
%% From yawl_compile.erl (lines 491-512)
do_compile_workflow(Spec, Options, RootNetFun) ->
    case yawl_compile:compile(Spec, Options) of
        {ok, Compiled} ->
            RootNetRaw = RootNetFun(Spec),
            RootNet = case RootNetRaw of
                B when is_binary(B) -> B;
                A when is_atom(A) -> atom_to_binary(A, utf8);
                _ -> <<"main">>
            end,
            Prefix = maps:get(module_prefix, Options, <<"yawl_">>),
            RootModule = list_to_atom(binary_to_list(<<Prefix/binary, RootNet/binary>>)),
            Modules = maps:keys(maps:get(modules, Compiled, #{})),
            Executor = #yawl_executor{
                spec = Spec,
                compiled = Compiled,
                root_module = RootModule,
                modules = [RootModule | Modules]
            },
            {ok, Executor};
        {error, Reason} ->
            {error, {compile_error, Reason}}
    end.
```

### 1.3 Module Loading and Execution

For YAML-compiled workflows that haven't been written to disk, the executor provides `ensure_modules_loaded/1` (lines 339-357) which:

1. Creates a temporary directory
2. Writes generated module source to files
3. Compiles each module using `compile:file/2`
4. Loads the compiled bytecode via `code:load_binary/2`
5. Cleans up temporary files

### 1.4 Starting Workflow Instances

The executor uses `gen_yawl:start_link/3` to instantiate workflow processes:

```erlang
%% Starting workflow instances (lines 543-556)
start_workflow(#yawl_executor{root_module = RootMod, spec = Spec, compiled = Compiled}, InitialData) when is_map(InitialData) ->
    CaseId = generate_case_id(),
    Regions = case root_net_info(Spec, Compiled) of
        #{regions := R} when is_map(R) -> R;
        _ -> #{}
    end,
    NetArg = #{initial_data => InitialData, case_id => CaseId, regions => Regions},

    case gen_yawl:start_link(RootMod, NetArg, []) of
        {ok, Pid} ->
            {ok, Pid, CaseId};
        {error, Reason} ->
            {error, {start_error, Reason}}
    end.
```

The `generate_case_id/0` function (lines 1099-1104) creates unique identifiers using system timestamps and Erlang's unique integer counter:

```erlang
generate_case_id() ->
    Timestamp = erlang:system_time(microsecond),
    Unique = erlang:unique_integer(),
    BinId = integer_to_binary(Timestamp),
    BinUnique = integer_to_binary(Unique),
    <<"case_", BinId/binary, "_", BinUnique/binary>>.
```

## 2. Human Task Injection and Extraction

### 2.1 The Injection Mechanism

The primary mechanism for human task interaction is `inject_token/3` (lines 734-749):

```erlang
-spec inject_token(Pid :: pid(), Place :: atom(), Token :: term()) ->
          {ok, receipt()} | {error, term()}.

inject_token(Pid, Place, Token) when is_pid(Pid), is_atom(Place) ->
    try
        case gen_yawl:inject(Pid, #{Place => [Token]}) of
            {ok, _} ->
                %% Return receipt of injection
                {ok, #{place => Place, token => Token}};
            {error, InjectError} ->
                {error, {inject_error, InjectError}}
        end
    catch
        exit:{noproc, _} -> {error, no_process};
        _:_Reason:_Stacktrace -> {error, catch_error}
    end.
```

This function wraps `gen_yawl:inject/2`, which ultimately calls into the gen_server to add tokens to specific places in the Petri net marking.

### 2.2 Human Task Discovery

The `yawl_pattern_expander` module enables human task discovery through transition renaming. When a pattern instance specifies `join_task`, `split_task`, or `merge_task`, the generic pattern transitions (e.g., `t_join`, `t_split`) are renamed to match the YAML task names:

```erlang
%% From yawl_pattern_expander.erl (lines 400-403)
build_transition_mapping(synchronization, PatternInstance) ->
    case task_ref(pi_get(join_task, PatternInstance)) of
        T when T =/= undefined -> #{t_join => trsn_atom(T)};
        _ -> #{}
    end.
```

This allows external agents to discover which places correspond to which human tasks by examining the transition list and preset maps. The `run_omega_with_zai.erl` module demonstrates this pattern:

```erlang
%% From run_omega_with_zai.erl (lines 128-133)
find_and_complete_human_task(Pid, Executor, Marking) ->
    RootMod = wf_yawl_executor:get_root_module(Executor),
    try
        Transitions = RootMod:trsn_lst(),
        PresetMap = maps:from_list([{T, RootMod:preset(T)} || T <- Transitions]),
        InjectPlace = find_inject_place(Transitions, PresetMap, Marking),
        %% ... complete task via Z.AI LLM
```

### 2.3 Task Completion Flow

When a workflow becomes quiescent (no automatic transitions enabled), it typically means it's waiting for human input. The execution flow is:

1. **Quiescence Detection**: `is_quiescent/1` (lines 823-832) checks if no transitions are enabled
2. **Task Discovery**: Examine the marking to find places awaiting tokens
3. **Token Injection**: Use `inject_token/3` to provide completion tokens
4. **Continuation**: Call `execute_step/2` to resume execution

## 3. Transcript Mode vs Debug Mode

### 3.1 Receipt-Based Execution Transcript

The executor maintains a complete execution transcript through receipts returned by each transition firing:

```erlang
%% Receipt type definition (lines 208-212)
-type receipt() :: #{
    trsn := atom(),
    mode => #{atom() => [term()]},
    produce => #{atom() => [term()]}
}.
```

Each receipt records:
- `trsn`: The transition that fired
- `mode`: The tokens consumed (firing mode)
- `produce`: The tokens produced

The `normalize_receipt/1` function (lines 1108-1125) ensures all receipts have a consistent structure:

```erlang
normalize_receipt(Receipt) when is_map(Receipt) ->
    Base = #{
        trsn => maps:get(trsn, Receipt, undefined)
    },
    Mode = case maps:get(mode, Receipt, undefined) of
        undefined -> [];
        M -> M
    end,
    Produce = case maps:get(produce, Receipt, undefined) of
        undefined -> #{};
        P -> P
    end,
    Base#{
        mode => Mode,
        produce => Produce
    }.
```

### 3.2 Step Execution with Receipt Collection

The `execute_step/2` function (lines 698-710) collects all receipts during execution:

```erlang
execute_step(Pid, MaxSteps) when is_pid(Pid), is_integer(MaxSteps), MaxSteps >= 0 ->
    try
        case gen_yawl:drain(Pid, MaxSteps) of
            {ok, Receipts} ->
                Normalized = [normalize_receipt(R) || R <- Receipts],
                {ok, Normalized};
            {error, limit} ->
                {error, limit}
        end
    catch
        exit:{noproc, _} -> {error, no_process};
        _:_:_ -> {error, drain_exception}
    end.
```

This receipt stream serves as a **transcript mode** execution log, capturing every transition firing for audit trails, replay, or analysis.

### 3.3 Debug Mode Through State Inspection

Debug mode functionality is provided through state inspection functions:

```erlang
%% State inspection (lines 777-801)
get_workflow_state(Pid) when is_pid(Pid) ->
    try
        Marking = gen_yawl:marking(Pid),

        %% Determine status from marking
        Status = determine_status(Marking),

        %% Extract case_id from usr_info
        UsrInfo = gen_yawl:usr_info(Pid),
        CaseId = maps:get(case_id, UsrInfo, <<"unknown">>),
        SpecId = maps:get(spec_id, UsrInfo, <<"unknown">>),

        State = #{
            marking => Marking,
            status => Status,
            case_id => CaseId,
            spec_id => SpecId
        },
        {ok, State}
    catch
        exit:{noproc, _} -> {error, no_process};
        _:_:_ -> {error, state_exception}
    end.
```

The `executor_info/1` function (lines 1050-1062) provides additional metadata including places, transitions, and module information for debugging.

### 3.4 Telemetry Integration

While not explicitly shown in `wf_yawl_executor.erl`, the underlying `gen_yawl` module integrates with telemetry systems. The `terminate/2` function in `gen_yawl` (lines 1041-1082) emits telemetry events:

```erlang
%% Emit shutdown telemetry event
_ = case catch yawl_telemetry:emit(shutdown, #{reason => Reason,
                                              active_fires => ActiveFires}) of
    ok -> ok;
    _ -> ok
end,
```

This enables observability for both transcript mode (receipts) and debug mode (telemetry/logging).

## 4. Swarm Coordination Through Workflow Topology

### 4.1 Workflow Topology as Swarm Structure

The YAWL workflow specification defines a topology that maps naturally to swarm coordination patterns:

- **Nets/Decompositions**: Represent swarm subteams or agent groups
- **Tasks**: Represent individual agent roles or capabilities
- **Flows**: Represent communication channels between agents
- **Patterns**: Represent coordination protocols (e.g., synchronization, discrimination)

### 4.2 Pattern-Based Coordination

The `yawl_pattern_expander` module expands YAWL pattern instances into Petri net structures that implement specific coordination patterns:

```erlang
%% Pattern expansion (lines 34-44)
expand_pattern(PatternInstance, Context) ->
    PatternId = pi_get(pattern, PatternInstance),
    Module = yawl_pattern_registry:pattern_module(PatternId),
    case Module of
        undefined ->
            #{places => [], transitions => [], flows => [], preset => #{}, postset => #{}};
        _ ->
            expand_pattern_impl(Module, PatternInstance, Context)
    end.
```

Patterns such as:
- **P42 Parallel Split**: Spawns multiple concurrent agent threads
- **P41 Synchronization**: Joins multiple threads
- **P4 Discriminator**: Races multiple branches and forwards the winner
- **P25 Cancel Region**: Implements cancellation domains

### 4.3 Subnet Coordination

The executor supports subnet execution through region management:

```erlang
%% Region extraction for cancellation (lines 545-548)
Regions = case root_net_info(Spec, Compiled) of
    #{regions := R} when is_map(R) -> R;
    _ -> #{}
end,
```

The `cancel_region/2` function in `gen_yawl` (lines 541-545) enables coordinated cancellation:

```erlang
cancel_region(Name, RegionId) when is_binary(RegionId); is_atom(RegionId) ->
    gen_server:call(Name, {cancel_region, RegionId}).
```

This implements a structured form of swarm coordination where groups of agents can be collectively cancelled through region-based control flow.

### 4.4 Thread Split and Merge

The thread split and merge patterns (P42 and P41) provide the fundamental coordination primitives for multi-agent swarms:

```erlang
%% From yawl_pattern_expander.erl (lines 288-294)
build_place_mapping(thread_split, PatternInstance, _Context) ->
    Branches = pi_get(branches, PatternInstance),
    build_thread_place_mapping(Branches);
build_place_mapping(synchronization, PatternInstance, _Context) ->
    WaitsFor = pi_get(waits_for, PatternInstance),
    JoinTask = pi_get(join_task, PatternInstance),
    build_branch_place_mapping(WaitsFor, join_task_to_suffix(JoinTask)).
```

When combined with YAML subnet definitions, this enables hierarchical swarm coordination where subnets represent specialized agent teams that synchronize at specific coordination points.

## 5. Admissibility Boundary Enforcement

### 5.1 Validation at Load Time

The executor enforces admissibility boundaries through validation during workflow loading:

```erlang
%% From load_workflow_from_spec (lines 1070-1081)
load_workflow_from_spec(Spec) ->
    case wf_spec:validate(Spec) of
        ok ->
            case compile_workflow(Spec) of
                {ok, Executor} ->
                    {ok, Executor};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Errors} ->
            {error, {validation_error, Errors}}
    end.
```

The `wf_spec:validate/1` function checks structural constraints such as:
- Valid task references
- Proper decomposition hierarchies
- Consistent flow connections

### 5.2 Cycle Detection

The `gen_yawl` wrapper implements cycle detection to prevent infinite loops:

```erlang
%% From gen_yawl.erl (lines 926-936)
#net_state{marking = NewMarking} = NetState3,
Fingerprint = erlang:phash2(term_to_binary(NewMarking)),
SeenBefore = lists:member(Fingerprint, History),
NewHistory = case SeenBefore of
    true ->
        logger:warning("gen_yawl halt: marking cycle detected (fingerprint=~p)", [Fingerprint]),
        [];
    false ->
        Hist1 = [Fingerprint | History],
        lists:sublist(Hist1, MaxHistory)
end,
```

This detects when the workflow enters a cycle in its marking (state) space and halts execution.

### 5.3 Step Limits

The executor enforces admissibility through step limits in `execute_step/2`:

```erlang
%% Step limit enforcement (lines 819-820, 825-827)
handle_call({drain, MaxSteps, _Acc}, _From, WrapperState) when MaxSteps =< 0 ->
    {reply, {error, limit}, WrapperState};

handle_call({drain, MaxSteps, Acc}, _From, WrapperState) ->
    case progress(NetState0, FireTimeout) of
        abort ->
            {reply, {ok, lists:reverse(Acc)}, WrapperState};
        %% ... decrement MaxSteps on each successful fire
```

This prevents runaway execution by limiting the number of transition firings.

### 5.4 Region-Based Isolation

The cancel region pattern (P25) provides boundary enforcement through region isolation:

```erlang
%% From yawl_pattern_reference.erl (lines 933-937)
Places = [p_start, p_region_boundary | [list_to_atom("p_region_" ++ integer_to_list(I))
                                          || I <- lists:seq(1, RegionCount)]] ++
          [p_cancel_req, p_cancelling, p_cancelled, p_outside, p_end],
```

The `p_region_boundary` place represents the admissibility boundary between inside and outside the region. Tokens crossing this boundary are subject to cancellation semantics.

### 5.5 Scope Boundary Mapping

The `wf_scope` module provides scope boundary mapping for nested workflows, enforcing lexical scoping of place names between parent and child nets:

```erlang
%% From wf_scope.erl (lines 24-28)
-moduledoc """
Scope boundary mapping (parent places <-> child places).

enter/3 renames parent keys to child keys using BindingTable[ScopeId].
leave/3 renames child keys back to parent keys.
bindings/2 returns mapping or {error, unknown_scope}.
"""
```

This ensures that subnet execution respects admissibility boundaries by preventing name collisions and enforcing proper token translation across scope boundaries.

## 6. Connection to "Swarm Roles and Human Tasks"

### 6.1 Role-Based Task Assignment

The executor pattern implements role-based task assignment through:

1. **Task Type Declaration**: YAWL specs declare tasks as `human` or `automated`
2. **Place Naming**: Pattern expansion maps abstract places (e.g., `p_thread1`) to concrete task names (e.g., `ProgramThread`)
3. **Transition Renaming**: Generic transitions (`t_join`) become task-specific (`t_GoNoGo`)

### 6.2 Human Task as Swarm Coordination Point

Human tasks in the executor serve as coordination points where:
- The workflow becomes quiescent (waiting state)
- External agents inspect the marking to determine pending tasks
- Agents inject completion tokens to advance the workflow
- The receipt stream records all human interactions

### 6.3 Multi-Agent Coordination

The thread split/merge patterns enable multiple agents to work in parallel:

```erlang
%% Example: Symposium workflow spawns multiple participant threads
%% - ProgramThread runs agenda items
%% - OpsThread manages logistics
%% - FinanceThread handles budget
%% t_GoNoGo synchronizes before proceeding
```

The executor ensures:
- All threads complete before synchronization (P41)
- Race conditions are handled by discriminator patterns (P9)
- Cancellation propagates through regions (P25)

## Conclusion

The `wf_yawl_executor.erl` module demonstrates a sophisticated implementation of the Executor Pattern that:

1. **Pipeline**: Provides a complete YAML-to-execution pipeline with validation, compilation, and runtime execution
2. **Human Tasks**: Implements token-based injection/extraction for human task coordination
3. **Transcript/Debug**: Maintains execution receipts for transcripts and provides state inspection for debugging
4. **Swarm Coordination**: Uses workflow topology and pattern-based coordination for multi-agent orchestration
5. **Admissibility**: Enforces boundaries through validation, cycle detection, step limits, and region isolation

The executor serves as the bridge between declarative workflow specifications (YAML/YAWL) and imperative execution (gen_pnet), enabling both automated and human-in-the-loop processes within a unified framework.
