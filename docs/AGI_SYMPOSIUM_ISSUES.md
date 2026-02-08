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

## Quick Reference

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
