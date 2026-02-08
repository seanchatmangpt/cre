# CRE Compilation Pipeline Diagrams

This document contains comprehensive Mermaid diagrams showing the complete YAWL workflow compilation pipeline from YAML specifications to executable Petri net code.

---

## Table of Contents

1. [YAML Compilation Pipeline Overview](#yaml-compilation-pipeline-overview)
2. [Stage 1: YAML Parsing](#stage-1-yaml-parsing)
3. [Stage 2: Pattern Expansion](#stage-2-pattern-expansion)
4. [Stage 3: Net Structure Generation](#stage-3-net-structure-generation)
5. [Stage 4: Module Code Generation](#stage-4-module-code-generation)
6. [Stage 5: Runtime Execution](#stage-5-runtime-execution)
7. [Pattern Expansion Detail](#pattern-expansion-detail)
8. [Receipt Generation and Verification](#receipt-generation-and-verification)
9. [Complete End-to-End Flow](#complete-end-to-end-flow)

---

## YAML Compilation Pipeline Overview

The compilation pipeline transforms YAWL YAML specifications into executable gen_pnet modules through five stages:

```mermaid
flowchart TD
    subgraph Input["Input"]
        YAML["YAML File<br/>(.yaml)"]
    end

    subgraph Stage1["Stage 1: YAML Parsing"]
        Parser["wf_yaml_spec:from_yaml_file/1<br/>(yamerl)"]
        Spec1["yawl_yaml_spec record"]
    end

    subgraph Stage2["Stage 2: Pattern Expansion"]
        Expander["yawl_pattern_expander:<br/>expand_patterns_for_net/3"]
        Registry["yawl_pattern_registry:<br/>pattern_module/1"]
        Patterns["43 Pattern Modules<br/>(P1-P43)"]
        Expanded["Expanded Net Structure"]
    end

    subgraph Stage3["Stage 3: Net Structure Generation"]
        Builder["yawl_compile:build_yaml_net_infos/4"]
        NetInfos["NetInfo Maps<br/>(places, transitions, flows)"]
    end

    subgraph Stage4["Stage 4: Module Code Generation"]
        Generator["yawl_compile:generate_module/3"]
        Modules["Erlang Source Code<br/>(yawl_*.erl)"]
    end

    subgraph Stage5["Stage 5: Runtime Execution"]
        Compiler["erlc:compile/2"]
        Beam["BEAM Modules<br/>(.beam)"]
        Runner["gen_yawl:start_link/3"]
        Process["Workflow Process"]
    end

    YAML --> Parser
    Parser --> Spec1
    Spec1 --> Expander
    Expander --> Registry
    Registry --> Patterns
    Patterns --> Expander
    Expander --> Expanded
    Expanded --> Builder
    Builder --> NetInfos
    NetInfos --> Generator
    Generator --> Modules
    Modules --> Compiler
    Compiler --> Beam
    Beam --> Runner
    Runner --> Process

    style Stage1 fill:#e1f5fe
    style Stage2 fill:#f3e5f5
    style Stage3 fill:#e8f5e9
    style Stage4 fill:#fff3e0
    style Stage5 fill:#fce4ec
```

---

## Stage 1: YAML Parsing

The YAML parsing stage converts raw YAML into the internal `yawl_yaml_spec` record format using the `yamerl` parser.

```mermaid
sequenceDiagram
    participant File as YAML File
    participant Yamerl as yamerl_constr
    participant Parser as wf_yaml_spec
    participant Validator as yawl_validate

    File->>Yamerl: Read file contents
    Yamerl->>Yamerl: Parse YAML structure
    Yamerl-->>Parser: Erlang terms (maps/lists)

    Parser->>Parser: normalize_yaml_data/1
    Parser->>Parser: parse_specification_set/1

    Note over Parser
        Extract:
        - yawl_yaml_version (must be "0.2")
        - specificationSet data
        - nets (nodes, flows, variables)
        - pattern_instances
        - pattern_registry
    end

    Parser->>Parser: parse_nets/1
    Note over Parser
        For each net:
        - Extract task IDs
        - Find input/output conditions
        - Build decomposition_info
    end

    Parser->>Parser: extract_tasks_from_nets/1
    Parser->>Parser: extract_flows_from_nets/1
    Parser->>Parser: extract_conditions_from_nets/1

    Parser->>Validator: validate/1
    Validator-->>Parser: ok | {error, [Reason]}

    Parser-->>Actor: {ok, #yawl_yaml_spec{}}
```

### YAML Specification Structure

```mermaid
flowchart LR
    Root["yawl_yaml_version: 0.2"]

    SpecSet["specificationSet"]
    Meta["metaData<br/>title, version"]
    RootNet["rootNet"]
    Roles["roles"]
    Nets["nets[]"]
    Patterns["pattern_instances[]"]
    Registry["pattern_registry"]
    Usage["pattern_usage_index"]

    Root --> SpecSet
    SpecSet --> Meta
    SpecSet --> RootNet
    SpecSet --> Roles
    SpecSet --> Nets
    SpecSet --> Patterns
    SpecSet --> Registry
    SpecSet --> Usage

    Net["net"]
    Nodes["nodes[]<br/>(tasks, conditions)"]
    Flows["flows[]"]
    Vars["variables[]"]
    Regions["regions[]"]
    Subnets["subnets[]"]

    Nets --> Net
    Net --> Nodes
    Net --> Flows
    Net --> Vars
    Net --> Regions
    Net --> Subnets

    Pattern["pattern_instance"]
    PID["id"]
    PType["pattern<br/>(P1-P43)"]
    PNet["net"]
    PParams["pattern params<br/>(branches, waits_for, etc.)"]

    Patterns --> Pattern
    Pattern --> PID
    Pattern --> PType
    Pattern --> PNet
    Pattern --> PParams
```

---

## Stage 2: Pattern Expansion

Pattern expansion converts pattern instances into concrete Petri net structures (places, transitions, flows).

```mermaid
flowchart TD
    subgraph Input["Pattern Expansion Input"]
        Instances["pattern_instances[]"]
        Context["Context<br/>(spec, entry_owner_id)"]
    end

    subgraph Process["Expansion Process"]
        Filter["Filter by net ID"]
        Lookup["Registry Lookup<br/>pattern_module/1"]
        Expand["expand_pattern_impl/3"]
        PlaceMap["build_place_mapping/3"]
        TrsnMap["build_transition_mapping/3"]
        ApplyPlace["apply_place_mapping/6"]
        ApplyTrsn["apply_transition_mapping/5"]
    end

    subgraph Output["Expanded Structure"]
        Places["places: [atom()]"]
        Transitions["transitions: [atom()]"]
        Flows["flows: [{atom(), atom()}]"]
        Preset["preset: #{atom() => [atom()]}"]
        Postset["postset: #{atom() => [atom()]}"]
    end

    subgraph Modules["Pattern Modules (43 total)"]
        P1["sequence<br/>(P1)"]
        P2["parallel_split<br/>(P2)"]
        P3["synchronization<br/>(P3)"]
        P4["exclusive_choice<br/>(P4)"]
        P42["thread_split<br/>(P42)"]
        P41["thread_merge<br/>(P41)"]
        P43["explicit_termination<br/>(P43)"]
        Dots["... 36 more patterns ..."]
    end

    Instances --> Filter
    Context --> Filter
    Filter --> Lookup
    Lookup --> Expand
    Expand --> PlaceMap
    Expand --> TrsnMap
    PlaceMap --> ApplyPlace
    TrsnMap --> ApplyTrsn
    ApplyPlace --> Output
    ApplyTrsn --> Output

    Lookup --> Modules

    style Process fill:#e1f5fe
    style Output fill:#c8e6c9
```

### Pattern Expansion Sequence

```mermaid
sequenceDiagram
    participant Exp as yawl_pattern_expander
    participant Reg as yawl_pattern_registry
    participant Pat as Pattern Module
    participant Net as Net Structure

    Exp->>Exp: expand_patterns_for_net/3<br/>(Instances, NetId, Context)

    loop For each pattern instance
        Exp->>Exp: net_matches/2<br/>(filter by net ID)

        Exp->>Reg: pattern_module/1<br/>(PatternId)
        Reg-->>Exp: ModuleName | undefined

        alt Module found
            Exp->>Pat: place_lst/0
            Pat-->>Exp: [p_start, p_task1, ...]

            Exp->>Pat: trsn_lst/0
            Pat-->>Exp: [t_start, t_complete1, ...]

            Exp->>Pat: preset/1 (per transition)
            Pat-->>Exp: [atom()]

            Exp->>Exp: build_place_mapping/3<br/>(map p_thread1->YAML names)
            Exp->>Exp: build_transition_mapping/3<br/>(rename transitions)

            Exp->>Net: merge_net_structures/2
        end
    end

    Exp-->>Actor: #{
        places => [...],
        transitions => [...],
        flows => [...],
        preset => #{},
        postset => #{}
    }
```

---

## Stage 3: Net Structure Generation

Net structure generation builds complete NetInfo maps for each decomposition net.

```mermaid
flowchart TD
    subgraph Inputs["Inputs"]
        Spec["yawl_yaml_spec"]
        Nets["net IDs"]
        Expanded["Expanded patterns"]
    end

    subgraph Build["build_yaml_net_infos/4"]
        GetRoot["Get root_net"]
        loop For each net
            GetTasks["wf_yaml_spec:tasks/2"]
            GetEntry["compute_entry_owner_id/3"]
            Expand["yawl_pattern_expander:<br/>expand_patterns_for_net/3"]
            GetIO["net_input/output_condition/2"]
            GetVars["wf_yaml_spec:variables/2"]
            GetRegions["wf_yaml_spec:net_regions/2"]
            BuildRegion["build_region_places_map/5"]
        end
    end

    subgraph NetInfo["NetInfo Map"]
        ID["id: binary()"]
        SpecID["spec_id: binary()"]
        IsRoot["is_root: boolean()"]
        Tasks["tasks: [atom()]"]
        Places["places: [atom()]<br/>(expanded + all referenced)"]
        Transitions["transitions: [atom()]"]
        Preset["preset: #{atom() => [atom()]}"]
        Postset["postset: #{atom() => [atom()]}"]
        Flows["flows: [{atom(), atom()}]"]
        IOCond["input/output_condition"]
        Vars["variables: [variable_def()]"]
        Regions["regions: #{binary() => [atom()]}"]
    end

    Spec --> Build
    Nets --> Build
    Expanded --> Build
    Build --> NetInfo

    style Build fill:#e8f5e9
    style NetInfo fill:#c8e6c9
```

### Place Resolution Strategy

```mermaid
flowchart TD
    Expanded["Expanded Places<br/>(from patterns)"]
    Preset["Preset Places"]
    Postset["Postset Places"]
    AllRef["All Referenced Places<br/>(Preset + Postset)"]
    FinalPlaces["Final place_lst/0"]

    Expanded --> FinalPlaces
    AllRef --> FinalPlaces

    Note["Note: Ensures all places in<br/>preset/postset are included<br/>in place_lst to prevent<br/>enum_mode badmatch"]

    style FinalPlaces fill:#ffe082
```

---

## Stage 4: Module Code Generation

Module code generation creates complete Erlang source files implementing the gen_pnet behavior.

```mermaid
flowchart TD
    subgraph Input["Module Generation Input"]
        NetId["NetId: binary()"]
        NetInfo["NetInfo map"]
        Options["Compile options"]
    end

    subgraph Gen["generate_module/3"]
        ModName["module_name/2<br/>(prefix + NetId)"]

        subgraph Headers["Module Headers"]
            Behavior["-behaviour(gen_pnet)"]
            ModDoc["-moduledoc"]
            Includes["-include_lib"]
        end

        subgraph Callbacks["gen_pnet Callbacks"]
            PlaceLst["place_lst/0"]
            TrsnLst["trsn_lst/0"]
            InitMarking["init_marking/2"]
            Preset["preset/1"]
            IsEnabled["is_enabled/3"]
            Fire["fire/3"]
        end

        subgraph Interface["gen_pnet Interface"]
            Init["init/1"]
            HandleCall["handle_call/3"]
            HandleCast["handle_cast/2"]
            HandleInfo["handle_info/2"]
            CodeChange["code_change/3"]
            Terminate["terminate/2"]
            Trigger["trigger/3"]
        end
    end

    subgraph Output["Generated Module"]
        Module["yawl_<NetId>.erl"]
    end

    Input --> Gen
    Gen --> Output

    style Gen fill:#fff3e0
    style Output fill:#ffcc80
```

### Generated Module Structure

```mermaid
flowchart TB
    Module["-module(yawl_NetId).<br/>-behaviour(gen_pnet)."]

    Module --> Struct["Structure Callbacks"]
    Module --> User["User Callbacks"]

    Struct --> PL["place_lst/0<br/>-> [atom()]"]
    Struct --> TL["trsn_lst/0<br/>-> [atom()]"]
    Struct --> IM["init_marking/2<br/>-> Place -> [term()]"]
    Struct --> PR["preset/1<br/>-> Trsn -> [atom()]"]
    Struct --> IE["is_enabled/3<br/>-> (Trsn, Mode, UsrInfo) -> boolean()"]
    Struct --> FR["fire/3<br/>-> (Trsn, Mode, UsrInfo) -><br/>{produce, Map} | abort"]

    User --> UI["init/1<br/>-> UsrInfo"]
    User --> HC["handle_call/3"]
    User --> HS["handle_cast/2"]
    User --> HI["handle_info/2"]
    User --> CC["code_change/3"]
    User --> TR["terminate/2"]
    User --> TG["trigger/3<br/>-> (Place, Token, NetState) -> pass | drop"]

    style Module fill:#e3f2fd
    style Struct fill:#c8e6c9
    style User fill:#ffccbc
```

### Fire/3 Clause Generation

```mermaid
flowchart LR
    Input["Transition + Mode"]
    Match["Match preset places<br/>with token consumption"]
    Produce["Generate produce map<br/>(postset places)"]
    Output["fire/3 clause"]

    Input --> Match
    Match --> Produce
    Produce --> Output

    MatchNote["<b>Match Pattern:</b><br/>t_split -><br/>  #{p_start := [init]}"]
    ProduceNote["<b>Produce:</b><br/>#{p_thread1 => [token],<br/>  p_thread2 => [token]}"]

    OutputNote["<b>Output:</b><br/>{t_split, #{p_start := [init]}, _UsrInfo} -><br/>  {produce, #{<br/>    p_thread1 => [token],<br/>    p_thread2 => [token]<br/>  }}"]

    style Match fill:#e1f5fe
    style Produce fill:#f3e5f5
```

---

## Stage 5: Runtime Execution

Runtime execution manages workflow instances through the gen_yawl behavior.

```mermaid
sequenceDiagram
    participant Client as Client Code
    participant Exec as wf_yawl_executor
    participant Yawl as gen_yawl
    participant Pnet as gen_pnet
    participant Net as yawl_<NetId>

    Client->>Exec: start_workflow/2<br/>(Executor, InitialData)
    Exec->>Exec: generate_case_id/0
    Exec->>Yawl: start_link/3<br/>(NetMod, NetArg, [])

    Yawl->>Net: init/1<br/>(NetArg)
    Net-->>Yawl: UsrInfo

    Yawl->>Yawl: Initialize marking<br/>(init_marking for each place)
    Yawl->>Yawl: continue(self())

    loop Continue loop
        Yawl->>Yawl: progress/2<br/>(NetState, FireTimeout)

        alt Transition enabled
            Yawl->>Net: is_enabled/3
            Net-->>Yawl: true

            Yawl->>Net: fire/3<br/>(with timeout protection)
            Net-->>Yawl: {produce, ProdMap}<br/>{produce, ProdMap, NewUsrInfo}<br/>abort

            Yawl->>Yawl: cns/2 (consume tokens)
            Yawl->>Net: trigger/3 (for each produced token)
            Yawl->>Yawl: prd/2 (produce tokens)
            Yawl->>Yawl: update_stats/4
            Yawl->>Yawl: continue(self())
        else No transitions enabled
            Yawl-->>Yawl: abort (quiescent)
        end
    end

    Client->>Exec: execute_step/1
    Exec->>Yawl: step/1
    Yawl-->>Exec: {ok, Receipt} | abort

    Client->>Exec: get_workflow_state/1
    Exec->>Yawl: marking/1
    Exec->>Yawl: usr_info/1
    Yawl-->>Exec: State data
    Exec-->>Client: {ok, WorkflowState}
```

### gen_yawl State Management

```mermaid
stateDiagram-v2
    [*] --> Initialized: init/1

    Initialized --> Executing: continue cast

    Executing --> Executing: progress returns delta

    Executing --> Quiescent: progress returns abort

    Executing --> Error: progress returns error

    Quiescent --> Executing: inject/2 adds tokens

    Quiescent --> [*]: stop/1

    Error --> [*]: terminate/2

    note right of Executing
        Cycle Detection:
        - Track marking fingerprints
        - Halt on repeat (max 10 history)
        - Max continue limit (1000 steps)
    end note
```

---

## Pattern Expansion Detail

Detailed view of how pattern instances are expanded into concrete structures.

```mermaid
flowchart TD
    subgraph Instance["Pattern Instance"]
        IID["id: instance_id"]
        IType["pattern: P42_ThreadSplit"]
        INet["net: main"]
        IBranches["branches: [A, B, C]"]
    end

    subgraph Lookup["Registry Lookup"]
        Reg["yawl_pattern_registry:<br/>pattern_module/1"]
        Mod["thread_split module"]
    end

    subgraph Query["Pattern Module Queries"]
        Q1["place_lst/0<br/>-> [p_start, p_thread1, p_thread2,<br/>      p_thread3, p_thread4, p_end]"]
        Q2["trsn_lst/0<br/>-> [t_split, t_finish1, t_finish2,<br/>       t_finish3, t_finish4]"]
        Q3["preset/1<br/>-> t_split: [p_start]<br/>   t_finish1: [p_thread1]<br/>   ..."]
        Q4["postset/1<br/>-> t_split: [p_thread1, p_thread2,<br/>            p_thread3, p_thread4]<br/>   t_finish1: [p_end]<br/>   ..."]
    end

    subgraph Mapping["Place Mapping"]
        PM1["build_place_mapping/3"]
        PM2["p_thread1 -> A"]
        PM3["p_thread2 -> B"]
        PM4["p_thread3 -> C"]
        PM5["p_thread4 -> undefined (dropped)"]
    end

    subgraph Transition["Transition Mapping"]
        TM1["build_transition_mapping/3"]
        TM2["split_task: SplitTask"]
        TM3["t_split -> t_SplitTask"]
    end

    subgraph Result["Expanded Structure"]
        R1["places: [p_start, A, B, C, p_end]"]
        R2["transitions: [t_SplitTask, t_finish1,<br/>            t_finish2, t_finish3]"]
        R3["preset: #{t_SplitTask => [p_start],<br/>         t_finish1 => [A], ...}"]
        R4["postset: #{t_SplitTask => [A, B, C],<br/>          t_finish1 => [p_end], ...}"]
    end

    Instance --> Lookup
    Lookup --> Mod
    Mod --> Query
    Query --> Mapping
    Mapping --> Result

    style Mapping fill:#e1f5fe
    style Result fill:#c8e6c9
```

### All 43 Workflow Patterns

```mermaid
flowchart LR
    subgraph Basic["Basic Control Flow (P1-P11)"]
        P1["P1_Sequence"]
        P2["P2_ParallelSplit"]
        P3["P3_Synchronization"]
        P4["P4_ExclusiveChoice"]
        P5["P5_SimpleMerge"]
        P6["P6_MultipleChoice"]
        P7["P7_StructuredSyncMerge"]
        P8["P8_MultipleMerge"]
        P9["P9_Discriminator"]
        P10["P10_ArbitraryCycles"]
        P11["P11_ImplicitTermination"]
    end

    subgraph MI["Multiple Instances (P12-P15)"]
        P12["P12_MI_NoSync"]
        P13["P13_MI_DesignTime"]
        P14["P14_MI_RuntimeKnown"]
        P15["P15_MI_RuntimeUnknown"]
    end

    subgraph Advanced["Advanced (P16-P25)"]
        P16["P16_DeferredChoice"]
        P17["P17_InterleavedParallelRouting"]
        P18["P18_Milestone"]
        P19["P19_CancelActivity"]
        P20["P20_CancelCase"]
        P21["P21_StructuredLoop"]
        P22["P22_Recursion"]
        P23["P23_TransientTrigger"]
        P24["P24_PersistentTrigger"]
        P25["P25_CancelRegion"]
    end

    subgraph Discriminators["Discriminators (P28-P29)"]
        P28["P28_BlockingDiscriminator"]
        P29["P29_CancellingDiscriminator"]
    end

    subgraph Partial["Partial Joins (P30-P36)"]
        P30["P30_StructuredPartialJoin"]
        P31["P31_BlockingPartialJoin"]
        P32["P32_CancellingPartialJoin"]
        P33["P33_GeneralizedANDJoin"]
        P34["P34_StaticPartialJoinMI"]
        P35["P35_CancellingPartialJoinMI"]
        P36["P36_DynamicPartialJoinMI"]
    end

    subgraph Extended["Extended (P37-P43)"]
        P37["P37_LocalSyncMerge"]
        P38["P38_GeneralSyncMerge"]
        P39["P39_CriticalSection"]
        P40["P40_InterleavedRouting"]
        P41["P41_ThreadMerge"]
        P42["P42_ThreadSplit"]
        P43["P43_ExplicitTermination"]
    end

    Basic --> MI
    MI --> Advanced
    Advanced --> Discriminators
    Discriminators --> Partial
    Partial --> Extended

    style Basic fill:#e3f2fd
    style MI fill:#f3e5f5
    style Advanced fill:#e8f5e9
    style Discriminators fill:#fff3e0
    style Partial fill:#fce4ec
    style Extended fill:#e0f2f1
```

---

## Receipt Generation and Verification

Receipts provide immutable audit records for state transitions in the Petri net.

```mermaid
flowchart TD
    subgraph State["State Before"]
        Marking1["Marking: #{p1 => [a], p2 => []}"]
        Hash1["Hash: hash(Marking)"]
    end

    subgraph Fire["Fire Transition"]
        Trsn["t1"]
        Mode["#{p1 := [a]}"]
        Call["NetMod:fire/3"]
    end

    subgraph Produce["Produce"]
        ProdMap["#{p2 => [b]}"]
    end

    subgraph State2["State After"]
        Consume["cns/2: consume from Mode"]
        Produce2["prd/2: produce ProdMap"]
        Marking2["Marking: #{p1 => [], p2 => [b]}"]
        Hash2["Hash: hash(Marking2)"]
    end

    subgraph Receipt["Receipt"]
        Before["before_hash: Hash1"]
        After["after_hash: Hash2"]
        Move["move: #{trsn, mode, produce}"]
        TS["ts: monotonic_timestamp()"]
    end

    Marking1 --> Hash1
    Hash1 --> Fire
    Fire --> Call
    Call --> Produce
    Produce --> State2
    State2 --> Marking2
    Marking2 --> Hash2

    Hash1 --> Receipt
    Hash2 --> Receipt
    Fire --> Receipt
    Produce --> Receipt

    style Receipt fill:#c8e6c9
```

### Receipt Structure and Classification

```mermaid
flowchart LR
    Receipt["pnet_receipt:make/3"]
    ReceiptFields["#{<br/>  before_hash: binary(),<br/>  after_hash: binary(),<br/>  move: #{<br/>    trsn: atom(),<br/>    mode: #{},<br/>    produce: #{}<br/>  },<br/>  ts: integer()<br/>}"]

    Effects["pnet_receipt:effects/1"]

    Silent["{silent, Receipt}<br/>Produce map is empty"]
    Single["{single_production, Receipt}<br/>One place in produce map"]
    Multiple["{multiple_production, Receipt}<br/>Multiple places in produce map"]

    Receipt --> ReceiptFields
    ReceiptFields --> Effects
    Effects --> Silent
    Effects --> Single
    Effects --> Multiple

    style Silent fill:#ffebee
    style Single fill:#e8f5e9
    style Multiple fill:#e3f2fd
```

### Receipt Verification Chain

```mermaid
sequenceDiagram
    participant Fire as fire/3
    participant Hash as hash/1
    participant Receipt as pnet_receipt
    participant Verify as Verification

    Fire->>Hash: Hash state before (Marking1)
    Hash-->>Fire: BeforeHash

    Fire->>Fire: Execute transition logic
    Fire->>Fire: Build produce map

    Fire->>Hash: Hash state after (Marking2)
    Hash-->>Fire: AfterHash

    Fire->>Receipt: make/3<br/>(BeforeHash, AfterHash, Move)
    Receipt->>Receipt: timestamp/0
    Receipt-->>Fire: Receipt

    Fire-->>Verify: Receipt

    Note over Verify
        Verification:
        1. Recompute hash of current marking
        2. Match against receipt.after_hash
        3. Verify transition was valid
        4. Check timestamp ordering
    end

    Verify->>Verify: validate_receipt/1
```

---

## Complete End-to-End Flow

The complete workflow from YAML file to executing process with receipts.

```mermaid
flowchart TD
    subgraph Stage1["Stage 1: Parse YAML"]
        YAML["workflow.yaml"]
        Parser["wf_yaml_spec:<br/>from_yaml_file/1"]
        Spec["#yawl_yaml_spec{}"]
    end

    subgraph Stage2["Stage 2: Expand Patterns"]
        Filter["Filter patterns by net"]
        Expander["yawl_pattern_expander:<br/>expand_patterns_for_net/3"]
        Expanded["Expanded net structures<br/>(places, transitions, preset/postset)"]
    end

    subgraph Stage3["Stage 3: Generate NetInfo"]
        Builder["yawl_compile:<br/>build_yaml_net_infos/4"]
        NetInfo["NetInfo maps<br/>per decomposition"]
    end

    subgraph Stage4["Stage 4: Generate Code"]
        Generator["yawl_compile:<br/>generate_module/3"]
        Source["yawl_main.erl<br/>yawl_subnet1.erl<br/>..."]
    end

    subgraph Stage5["Stage 5: Compile & Load"]
        Compiler["erlc / code:load_binary"]
        Beam["yawl_main.beam<br/>yawl_subnet1.beam<br/>..."]
    end

    subgraph Stage6["Stage 6: Start Workflow"]
        Executor["wf_yawl_executor:<br/>start_workflow/2"]
        Starter["gen_yawl:start_link/3"]
        Process["Workflow Pid"]
    end

    subgraph Stage7["Stage 7: Execute"]
        Exec["execute_step/2"]
        Progress["progress/2"]
        Fire["fire/3"]
        Receipts["Receipts"]
    end

    subgraph Stage8["Stage 8: Query State"]
        Query["get_workflow_state/1"]
        Marking["Current Marking"]
        Status["Status: completed|running"]
    end

    YAML --> Parser
    Parser --> Spec
    Spec --> Filter
    Filter --> Expander
    Expander --> Expanded
    Expanded --> Builder
    Builder --> NetInfo
    NetInfo --> Generator
    Generator --> Source
    Source --> Compiler
    Compiler --> Beam
    Beam --> Starter
    Starter --> Process
    Process --> Exec
    Exec --> Progress
    Progress --> Fire
    Fire --> Receipts
    Receipts --> Query
    Query --> Marking
    Query --> Status

    style Stage1 fill:#e1f5fe
    style Stage2 fill:#f3e5f5
    style Stage3 fill:#e8f5e9
    style Stage4 fill:#fff3e0
    style Stage5 fill:#ffe0b2
    style Stage6 fill:#f8bbd0
    style Stage7 fill:#e1bee7
    style Stage8 fill:#b2dfdb
```

### One-Shot Execution Flow

```mermaid
sequenceDiagram
    participant Client
    participant Exec as wf_yawl_executor
    participant Pid as Workflow Pid

    Client->>Exec: execute_workflow/3<br/>(FilePath, InitialData, Options)

    Exec->>Exec: load_workflow/1
    Exec->>Exec: compile_workflow/1

    Exec->>Exec: start_workflow/2
    Exec->>Pid: gen_yawl:start_link

    Exec->>Pid: execute_step/2<br/>(MaxSteps)

    loop Until quiescent or MaxSteps
        Pid->>Pid: progress/2
        Pid->>Pid: fire/3
        Pid-->>Exec: Receipt
    end

    Exec->>Pid: get_workflow_state/1
    Pid-->>Exec: {ok, WorkflowState}

    Exec->>Pid: stop_workflow/1

    Exec-->>Client: {ok, #{
        receipts := [...],
        final_state := WorkflowState,
        case_id := ...
    }}
```

---

## Key Module Interactions

### Module Dependency Graph

```mermaid
flowchart LR
    subgraph Parsing["Parsing Layer"]
        YAML["wf_yaml_spec"]
    end

    subgraph Patterns["Pattern Layer"]
        Registry["yawl_pattern_registry"]
        Expander["yawl_pattern_expander"]
        Patterns["43 Pattern Modules"]
    end

    subgraph Compilation["Compilation Layer"]
        Compile["yawl_compile"]
    end

    subgraph Execution["Execution Layer"]
        Executor["wf_yawl_executor"]
        GenYawl["gen_yawl"]
        GenPnet["gen_pnet"]
    end

    subgraph Runtime["Runtime Layer"]
        Receipt["pnet_receipt"]
    end

    YAML --> Expander
    Registry --> Expander
    Patterns --> Expander
    Expander --> Compile
    Compile --> Executor
    Executor --> GenYawl
    GenYawl --> GenPnet
    GenYawl --> Receipt

    style Parsing fill:#e1f5fe
    style Patterns fill:#f3e5f5
    style Compilation fill:#e8f5e9
    style Execution fill:#fff3e0
    style Runtime fill:#ffe0b2
```

### Data Flow Through Compilation

```mermaid
flowchart LR
    subgraph Input["Input Data"]
        YAML["YAML File<br/>(text)"]
    end

    subgraph P1["Parse: wf_yaml_spec"]
        S1["yawl_yaml_spec record<br/>(spec, patterns, nets)"]
    end

    subgraph P2["Expand: yawl_pattern_expander"]
        S2["net_structure map<br/>(places, transitions, flows)"]
    end

    subgraph P3["Build: yawl_compile"]
        S3["NetInfo map<br/>(+ regions, variables)"]
    end

    subgraph P4["Generate: yawl_compile"]
        S4["Erlang source<br/>(binary)"]
    end

    subgraph P5["Compile: erlc"]
        S5["BEAM code<br/>(.beam file)"]
    end

    subgraph P6["Run: gen_yawl"]
        S6["Workflow state<br/>(marking, usr_info)"]
    end

    subgraph P7["Receipt: pnet_receipt"]
        S7["Receipt record<br/>(before_hash, after_hash, move, ts)"]
    end

    YAML --> P1
    P1 --> P2
    P2 --> P3
    P3 --> P4
    P4 --> P5
    P5 --> P6
    P6 --> P7

    style S1 fill:#e1f5fe
    style S2 fill:#f3e5f5
    style S3 fill:#e8f5e9
    style S4 fill:#fff3e0
    style S5 fill:#ffe0b2
    style S6 fill:#f8bbd0
    style S7 fill:#e1bee7
```

---

## Appendix: Module Reference

### Core Modules

| Module | Purpose | Key Functions |
|--------|---------|---------------|
| `wf_yaml_spec` | Parse YAML 0.2 specs | `from_yaml_file/1`, `validate/1` |
| `yawl_pattern_registry` | Map patterns to modules | `pattern_module/1`, `all_patterns/0` |
| `yawl_pattern_expander` | Expand patterns to net structures | `expand_pattern/2`, `expand_patterns_for_net/3` |
| `yawl_compile` | Generate gen_pnet modules | `compile/2`, `generate_module/3` |
| `gen_yawl` | Workflow runtime (gen_pnet wrapper) | `start_link/3`, `step/1`, `drain/2` |
| `wf_yawl_executor` | High-level execution API | `load_workflow/1`, `execute_workflow/3` |
| `pnet_receipt` | Immutable audit records | `make/3`, `effects/1` |

### Pattern Modules (P1-P43)

| Code | Pattern | Module |
|------|---------|--------|
| P1 | Sequence | `sequence` |
| P2 | Parallel Split | `parallel_split` |
| P3 | Synchronization | `synchronization` |
| P4 | Exclusive Choice | `exclusive_choice` |
| P5 | Simple Merge | `simple_merge` |
| P6 | Multiple Choice | `multiple_choice` |
| P7 | Structured Sync Merge | `structured_sync_merge` |
| P8 | Multiple Merge | `multiple_merge` |
| P9 | Discriminator | `discriminator` |
| P10 | Arbitrary Cycles | `arbitrary_cycles` |
| P11 | Implicit Termination | `implicit_termination` |
| P12-P15 | Multiple Instances | `multiple_instances_sync` |
| P16 | Deferred Choice | `deferred_choice` |
| P17 | Interleaved Routing | `interleaved_routing` |
| P18 | Milestone | `milestone` |
| P19 | Cancel Activity | `cancel_activity` |
| P20 | Cancel Case | `cancel_case` |
| P21 | Structured Loop | `structured_loop` |
| P22 | Recursion | `recursion` |
| P23 | Transient Trigger | `transient_trigger` |
| P24 | Persistent Trigger | `persistent_trigger` |
| P25 | Cancel Region | `cancel_region` |
| P26 | Cancel MI Activity | `cancel_mi_activity` |
| P27 | Complete MI Activity | `complete_mi_activity` |
| P28 | Blocking Discriminator | `blocking_discriminator` |
| P29 | Cancelling Discriminator | `cancelling_discriminator` |
| P30 | Structured Partial Join | `structured_partial_join` |
| P31 | Blocking Partial Join | `blocking_partial_join` |
| P32 | Cancelling Partial Join | `cancelling_partial_join` |
| P33 | Generalized AND Join | `generalized_and_join` |
| P34 | Static Partial Join MI | `static_partial_join_mi` |
| P35 | Cancelling Partial Join MI | `cancelling_partial_join_mi` |
| P36 | Dynamic Partial Join MI | `dynamic_partial_join_mi` |
| P37 | Local Sync Merge | `local_sync_merge` |
| P38 | General Sync Merge | `general_sync_merge` |
| P39 | Critical Section | `critical_section` |
| P40 | Interleaved Routing | `interleaved_routing` |
| P41 | Thread Merge | `thread_merge` |
| P42 | Thread Split | `thread_split` |
| P43 | Explicit Termination | `explicit_termination` |
