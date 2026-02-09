# AGI Symposium Omega: Issue Identification Guide

This document provides mermaid diagrams and explanations to diagnose issues when the AGI Symposium Omega demo blocks or fails.

---

## 1. Execution Flow and Blocking Points

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

**Issue:** When `step()` returns `abort`, `find_inject_place` returns `undefined` (no root human task enabled). Then `run_subnets_if_needed` may return `none` if no subnets have tokens in entry places. Result: **blocked** at ~3 rounds.

---

## 2. Token Flow: Root to Subnets to P3 Sync

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

**Issue:** Subnets must complete and `run_one_subnet` must inject into `p_gonogo_branch1..3`. If subnets never receive tokens (P42 place mapping mismatch) or inject to wrong place, P3 never fires.

---

## 3. Cycle Detection Logic (gen_yawl)

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

**Note:** Omega uses `max_marking_history = 0` so cycle detection is disabled. If someone runs without that option, cycle detection would halt after a repeated marking.

---

## 4. Issue Identification Decision Tree

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

---

## 5. Subnet Run Flow (run_ready_subnets)

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

**Issue:** `p_branch_place_for_subnet(4)` and `p_branch_place_for_subnet(_)` both map to `p_gonogo_branch3`. IncidentThread and SatelliteSymposium (indices 4,5) would both inject into branch3, potentially causing P3 sync to see duplicate tokens or wrong count.

---

## 6. Place Name Mapping (P42 vs YAML)

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

**Issue:** 5 subnets but P42 has 4 branches. `subnet_index` and `p_branch_place_for_subnet` may misalign for IncidentThread and SatelliteSymposium.

---

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

**Breakdown points:**
- `find_inject_place` → `undefined`: no preset (P1,P2) has exactly one full + one empty
- `run_ready_subnets` → `false`: `Tokens = []` for every subnet (BranchPlace + P42Place + EntryPlace all empty)
- `check_completed` → `false`: end place has no tokens

---

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

**Key:** `find_inject_place_for_preset` returns `undefined` when:
- `[P1,P2]`: both empty, or both full (T1>0,T2>0)
- `[P1]`: always undefined (single-place preset)
- `[P1..Pn]`: not exactly (N full, 1 empty) or (1 full, 2 empty)

---

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

**Breakdown points:**
- `get_subnet_entry_exit` → `undefined`: subnet not found in SubnetDefs or Executor
- `Tokens = []`: `BranchPlace` and `P42Place` and `EntryPlace` all empty
- `p_branch_place_for_subnet(4)` and `(_)` both map to `p_gonogo_branch3` (5 subnets, 4 P42 branches)

---

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

**Breakdown:** When subnet blocks on a human task whose preset doesn't match `find_inject_place_for_preset` (e.g. single-place preset, or all-empty / all-full), `subnet_find_inject_place` returns `undefined` → drain returns `ok` without injecting → subnet never completes → exit place empty → no token injected to root.

---

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

---

## Quick Reference

### Key Functions and Return Values

| Module | Function | Returns | Breakdown when |
|--------|----------|---------|----------------|
| omega_demo_runner | find_inject_place/3 | undefined \| {Place, TaskName} | undefined → no root task |
| omega_demo_runner | find_inject_place_for_preset | undefined \| {Place, TaskName} | Preset structure doesn't match |
| omega_demo_runner | run_subnets_if_needed/4 | ok \| none | none when run_ready_subnets false |
| omega_demo_runner | run_ready_subnets/5 | true \| false | false when no subnet has tokens |
| omega_demo_runner | subnet_find_inject_place/2 | undefined \| {Place, TaskName} | undefined → subnet human task unknown |
| omega_demo_runner | drain_subnet_with_human_tasks/3 | ok | Returns ok when subnet_find_inject_place undefined |
| omega_demo_runner | check_completed/2 | true \| false | false → end place empty |
| gen_yawl | step/1 | ok \| abort | abort → human task or no enabled auto |

### Symptom to Cause

| Symptom | Likely Cause |
|---------|--------------|
| Rounds = 0 | Pattern collision (namespacing fix applied) |
| "marking cycle detected" log | Set `max_marking_history = 0` in gen_yawl_options |
| Blocked at ~3 rounds | Subnet wiring or token injection; check p_threadN and Entry places |
| find_inject_place undefined | No root human task enabled; run_subnets_if_needed should run |
| run_subnets_if_needed returns none | No tokens in p_threadN or subnet Entry; P42 split may not have produced |

## Related Files

- `test/omega_demo_runner.erl` - Main run loop, find_and_complete_human_task, run_subnets_if_needed
- `src/core/gen_yawl.erl` - Cycle detection (marking_history)
- `docs/mermaid/14-omega-symposium.md` - High-level workflow diagrams
