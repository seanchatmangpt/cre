# AGI Symposium Ω Workflow

## High-Level Flow

```mermaid
flowchart LR
    Start([Start]) --> Split[SplitMegaThreads]
    Split --> Program[ProgramThread]
    Split --> Ops[OpsThread]
    Split --> Comms[CommsThread]
    Split --> Incident[IncidentThread]
    Program --> Merge[MergeMegaThreads]
    Ops --> Merge
    Comms --> Merge
    Incident --> Merge
    Merge --> GoNoGo[GoNoGo]
    GoNoGo --> OpenDoors[OpenDoors]
    OpenDoors --> Close[CloseSymposium]
    Close --> Publish[PublishProceedings]
    Publish --> End([End])
```

## Sync Place Namespaces

```mermaid
flowchart TD
    subgraph P3[P3 Synchronization]
        p_g1((p_gonogo_branch1))
        p_g2((p_gonogo_branch2))
        p_g3((p_gonogo_branch3))
        p_g1 --> t_gn[t_GoNoGo]
        p_g2 --> t_gn
        p_g3 --> t_gn
    end

    subgraph P38[P38 General Sync Merge]
        p_c1((p_close_branch1))
        p_c2((p_close_branch2))
        p_c3((p_close_branch3))
        p_c1 --> t_cs[t_CloseSymposium]
        p_c2 --> t_cs
        p_c3 --> t_cs
    end
```

## Pattern Instances (43)

```mermaid
flowchart TD
    subgraph Symposium[Symposium Net]
        P42[P42 Thread Split]
        P41[P41 Thread Merge]
        P3[P3 Synchronization]
        P38[P38 General Sync Merge]
        P32[P32 Cancelling Partial Join]
        P22[P22 Recursion]
        P25[P25 Cancel Region]
        P20[P20 Cancel Case]
        P43[P43 Explicit Termination]
        P11[P11 Implicit Termination]
        P8[P8 Multiple Merge]
    end

    subgraph ProgramThread[ProgramThread Net]
        P1[P1 Sequence]
        P2[P2 Parallel Split]
        P4[P4 Exclusive Choice]
        P5[P5 Simple Merge]
        P6[P6 Multiple Choice]
        P7[P7 Structured Sync Merge]
        P9[P9 Discriminator]
        P10[P10 Arbitrary Cycles]
        P21[P21 Structured Loop]
        P19[P19 Cancel Activity]
        P25p[P25 Cancel Region]
        P30[P30 Structured Partial Join]
        P31[P31 Blocking Partial Join]
        P33[P33 Generalized AND Join]
        P39[P39 Critical Section]
    end
```

## Execution Flow and Blocking Points

Shows where the Omega demo can stop and why.

```mermaid
flowchart TD
    subgraph OmegaLoop [Omega Demo Run Loop]
        Step[gen_yawl:step]
        Step -->|ok| NextRound[Round+1, continue]
        Step -->|abort| FindTask[find_and_complete_human_task]
        FindTask -->|ok| NextRound
        FindTask -->|done| Completed[status: completed]
        FindTask -->|none| RunSubnets[run_subnets_if_needed]
        RunSubnets -->|ok| StepUntilAbort[step_until_abort]
        StepUntilAbort --> NextRound
        RunSubnets -->|none| CheckComplete[check_completed]
        CheckComplete -->|true| Completed
        CheckComplete -->|false| Blocked[status: blocked]
    end
```

## Token Flow and Subnet Wiring

Shows where tokens must flow for GoNoGo to fire.

```mermaid
flowchart LR
    subgraph RootNet [Root Net]
        Start((p_start)) --> Split[t_SplitMegaThreads]
        Split --> T1[p_thread1]
        Split --> T2[p_thread2]
        Split --> T3[p_thread3]
        Split --> T4[p_thread4]
        T1 --> P1[ProgramThread]
        T2 --> O1[OpsThread]
        T3 --> C1[CommsThread]
        T4 --> I1[IncidentThread]
    end

    subgraph P3Sync [P3 GoNoGo Sync]
        B1((p_gonogo_branch1))
        B2((p_gonogo_branch2))
        B3((p_gonogo_branch3))
        B1 --> GoNoGo[t_GoNoGo]
        B2 --> GoNoGo
        B3 --> GoNoGo
    end

    P1 -->|Inject on exit| B1
    O1 -->|Inject on exit| B2
    C1 -->|Inject on exit| B3
```

## Cycle Detection (when enabled)

When `max_marking_history > 0`, this path can halt execution.

```mermaid
flowchart TD
    subgraph ContinueHandler [handle_cast continue]
        Progress[progress: fire transition]
        Progress -->|delta| UpdateState[Update marking]
        UpdateState --> Fingerprint[phash2 term_to_binary Marking]
        Fingerprint --> Seen{In History?}
        Seen -->|yes| Halt[halt: cycle detected]
        Seen -->|no| AddHist[Add to history, continue]
        AddHist --> Continue[continue self]
    end
```

## Issue Identification

```mermaid
flowchart TD
    Blocked[Demo reports blocked]
    Blocked --> Q1{Rounds > 0?}
    Q1 -->|no| A1[Pattern collision: transitions/places overwritten]
    Q1 -->|yes| Q2{Cycle detection log?}
    Q2 -->|yes| A2[Marking cycle: enable max_marking_history=0]
    Q2 -->|no| Q3{find_inject_place undefined?}
    Q3 -->|yes| Q4{run_subnets_if_needed returns none?}
    Q4 -->|yes| A3[Subnet wiring: tokens not in p_threadN or Entry place]
    Q4 -->|no| A4[Subnet human task: drain_subnet_with_human_tasks or inject place]
    Q3 -->|no| A5[Root human task: Agent or inject logic]
```

## Subnet Run Flow

```mermaid
flowchart TD
    subgraph RunReady [run_ready_subnets]
        CheckToken{Tokens in BranchPlace or p_threadN or Entry?}
        CheckToken -->|no| Skip[Skip subnet]
        CheckToken -->|yes| StartSubnet[gen_yawl:start_link subnet]
        StartSubnet --> InjectEntry[Inject token to Entry]
        InjectEntry --> Drain[drain_subnet_with_human_tasks]
        Drain -->|abort| FindInject[subnet_find_inject_place]
        FindInject -->|found| InjectDefault[Inject default_agent]
        InjectDefault --> Drain
        Drain -->|complete| GetExit[Check Exit place]
        GetExit --> InjectRoot[Inject to p_gonogo_branchN]
        InjectRoot --> StopSubnet[gen_yawl:stop subnet]
    end
```

## Place Name Mapping (P42 vs YAML)

```mermaid
flowchart LR
    subgraph P42Pattern [P42 ThreadSplit Output]
        pt1[p_thread1]
        pt2[p_thread2]
        pt3[p_thread3]
        pt4[p_thread4]
    end

    subgraph YAMLSubnets [YAML Subnet IDs]
        prog[ProgramThread]
        ops[OpsThread]
        comms[CommsThread]
        incident[IncidentThread]
        sat[SatelliteSymposium]
    end

    prog -->|index 1| pt1
    ops -->|index 2| pt2
    comms -->|index 3| pt3
    incident -->|index 4| pt4
    sat -->|index 5| pt4
```

## 7. Code-Level Call Chain (Entry to Blocked)

Trace the execution path from `run_omega_loop` to `blocked` with modules and functions. Includes return paths and `step_until_abort`.

```mermaid
flowchart TD
    subgraph omega_demo_runner [omega_demo_runner]
        Loop[run_omega_loop/8]
        Find[find_and_complete_human_task/6]
        RunSub[run_subnets_if_needed/4]
        StepUntil[step_until_abort/2]
        Check[check_completed/2]
        Dump[maybe_dump_blocked_state/3]
    end
    subgraph gen_yawl [gen_yawl]
        Step[step/1]
    end
    subgraph omega_internals [omega_demo_runner internals]
        FindInject[find_inject_place/3]
        RunReady[run_ready_subnets/5]
    end

    Loop --> Step
    Step -->|"ok Receipt"| Loop
    Step -->|abort| Find
    Find --> FindInject
    FindInject -->|undefined| RunSub
    Find -->|ok| Loop
    Find -->|done| Completed["status: completed"]
    RunSub --> RunReady
    RunReady -->|true| StepUntil
    StepUntil --> Loop
    RunReady -->|false| Check
    Check -->|false| Dump
    Check -->|true| Completed
    Dump --> Blocked["Result: blocked"]
```

## 8. Root Human Task Resolution (find_inject_place)

```mermaid
flowchart TD
    subgraph omega_demo_runner [omega_demo_runner]
        FIP[find_inject_place/3]
        FIPP[find_inject_place_for_preset]
        TTR[task_to_role/1]
        AgentLookup["maps:get Role Agents default_agent"]
        ExtractCtx[extract_workflow_context]
        PatternLabel[get_current_pattern_label]
        AgentCall["Agent TaskName Ctx"]
        InjectRoot[gen_yawl:inject]
    end
    subgraph RootMod [RootMod compiled]
        Trsn[trsn_lst/0]
        Preset[preset/1]
    end

    FIP --> Trsn
    FIP --> Preset
    FIP --> FIPP
    FIPP -->|"P1 P2 one full one empty"| Ret1["{Place, TaskName}"]
    FIPP -->|"P1 single"| Ret2["undefined"]
    FIPP -->|"length gt 2 N full 1 empty"| Ret1
    FIPP -->|"else"| Ret2

    Ret1 --> TTR
    TTR --> AgentLookup
    AgentLookup --> ExtractCtx
    ExtractCtx --> PatternLabel
    PatternLabel --> AgentCall
    AgentCall --> InjectRoot
    InjectRoot --> ReturnOk["return ok"]

    Ret2 --> Breakdown1["Breakdown: no root task enabled"]
```

## 9. Subnet Run Breakdown (run_ready_subnets)

```mermaid
flowchart TD
    subgraph omega_demo_runner [omega_demo_runner]
        RRS[run_ready_subnets/5]
        GSE[get_subnet_entry_exit/3]
        SI[subnet_index/2]
        PB[p_branch_place_for_subnet/1]
        ROS[run_one_subnet/6]
        Drain[drain_subnet_with_human_tasks/3]
    end
    subgraph gen_yawl [gen_yawl]
        Withdraw[withdraw/2]
        StartLink[start_link/3]
        Inject[inject/2]
        Marking[marking/1]
        InjectRoot[inject/2 to root]
        Stop[stop/1]
    end

    RRS -->|"for each NetId Mod"| GSE
    GSE -->|undefined| Skip1["Skip subnet"]
    GSE -->|"ok Entry Exit"| SI
    SI --> PB
    PB -->|"Index 1 to 3"| B1["p_gonogo_branch1..3"]
    PB -->|"Index 4+"| B2["p_gonogo_branch3"]
    RRS -->|"BranchTokens ++ EntryTokens = []"| Skip2["Acc no run"]
    RRS -->|"Tokens /= []"| Withdraw
    Withdraw --> ROS
    ROS --> StartLink
    StartLink --> Inject
    Inject --> Drain
    Drain --> Marking
    Marking --> CheckExit["ExitPlace tokens?"]
    CheckExit -->|"[]"| Stop
    CheckExit -->|"T present"| InjectRoot
    InjectRoot --> Stop
    Stop --> RanTrue["RRS returns true"]
    RanTrue --> StepUntil["step_until_abort in run_subnets_if_needed"]
```

## 10. Subnet Drain Breakdown (drain_subnet_with_human_tasks)

```mermaid
flowchart TD
    subgraph omega_demo_runner [omega_demo_runner]
        Drain[drain_subnet_with_human_tasks/3]
        DrainRec[drain_subnet_with_human_tasks N-1]
        SFIP[subnet_find_inject_place/2]
        FIPP[find_inject_place_for_preset]
    end
    subgraph gen_yawl [gen_yawl]
        Step[step/1]
        Marking[marking/1]
        Inject[inject/2]
    end
    subgraph SubnetMod [SubnetMod compiled]
        Trsn[trsn_lst/0]
        Preset[preset/1]
    end

    Drain --> NZero{N eq 0?}
    NZero -->|yes| ReturnOk["return ok to run_one_subnet"]
    NZero -->|no| Step
    Step -->|"ok Receipt"| DrainRec
    DrainRec --> Drain
    Step -->|abort| Marking
    Marking --> SFIP
    SFIP --> Trsn
    SFIP --> Preset
    SFIP --> FIPP
    FIPP -->|found| Inject
    Inject --> DrainRec
    FIPP -->|undefined| Break["Breakdown: return ok stop draining"]
    Break --> ReturnOk
```

## 11. Place/Token Data Flow

```mermaid
flowchart TD
    subgraph root_marking [root_marking]
        PT1[p_thread1]
        PT2[p_thread2]
        PT3[p_thread3]
        PT4[p_thread4]
        Prog[ProgramThread]
        Ops[OpsThread]
        Comms[CommsThread]
        Inc[IncidentThread]
        Entry[EntryPlace from YAML]
    end

    subgraph run_ready_subnets_logic [run_ready_subnets logic]
        BranchPlace["BranchPlace = binary_to_atom NetId"]
        P42Place["P42Place = p_threadN N = subnet_index"]
        BranchTokens["BranchTokens = BranchPlace ++ P42Place"]
        EntryTokens["EntryTokens = maps:get EntryPlace Marking"]
        Tokens["Tokens = BranchTokens ++ EntryTokens"]
        WithdrawPlace["WithdrawPlace: prefer BranchPlace then P42Place then EntryAtom"]
    end

    PT1 --> BranchPlace
    Prog --> BranchPlace
    PT1 --> P42Place
    BranchPlace --> BranchTokens
    P42Place --> BranchTokens
    Entry --> EntryTokens
    BranchTokens --> Tokens
    EntryTokens --> Tokens
    Tokens -->|"[]"| None["run_subnets_if_needed returns none"]
    Tokens -->|"T present"| WithdrawPlace
    WithdrawPlace --> Withdraw["gen_yawl:withdraw"]
```
