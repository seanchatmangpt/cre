# CRE C4 Architecture Diagrams

**Purpose:** Complete C4 model documentation to prevent repeated architectural mistakes. All diagrams enforce **gen_yawl as the single workflow runtime**.

**Last Updated:** 2026-02-06

---

## Diagram Index

| Level | Diagram | File | Purpose |
|-------|---------|------|---------|
| 0 | System Context | [level0-context-diagram.puml](level0-context-diagram.puml) | CRE system, users, external systems |
| 0 | System Context + Z.AI | [level0-context-diagram-zai.puml](level0-context-diagram-zai.puml) | CRE with Z.AI for LLM-backed human tasks |
| 1 | Container | [level1-container-diagram-v2.puml](level1-container-diagram-v2.puml) | Deployable containers, gen_yawl-centric |
| 1 | Container + Z.AI | [level1-container-diagram-zai.puml](level1-container-diagram-zai.puml) | Containers with Z.AI, simulation orchestrator |
| 2 | Component | [level2-yawl-engine-component.puml](level2-yawl-engine-component.puml) | YAWL Engine internals, execution flow |
| 3 | Code/Flow | [level3-pattern-execution.puml](level3-pattern-execution.puml) | 43 patterns execution pipeline |
| Seq | Omega + Z.AI | [sequence-omega-zai-execution.puml](sequence-omega-zai-execution.puml) | Omega simulation with gen_yawl + Z.AI |

**Legacy (superseded):** `level1-container-diagram.puml`, `level2-component-diagram.puml` — use v2 diagrams.

---

## Rendering Diagrams

### PlantUML

```bash
# Using PlantUML jar
java -jar plantuml.jar docs/diagrams/c4/*.puml

# Or with rebar3 doc
rebar3 doc
```

### VS Code

- Extension: PlantUML (jebbs.plantuml)
- Alt+D to preview

---

## Architectural Rules (Must Follow)

### 1. gen_yawl is the Only Workflow Runtime

- **All** workflow execution uses `gen_yawl:start_link`, `gen_yawl:step`, `gen_yawl:drain`, etc.
- **Never** call `gen_pnet` directly for workflow execution from `yawl_execution` or `wf_yawl_executor`.
- `gen_pnet` is internal to gen_yawl; workflows see only gen_yawl.

### 2. Each Pattern = One gen_yawl Module

- Location: `src/patterns/*.erl`
- Behaviour: `-behaviour(gen_yawl)`
- Callbacks: `place_lst`, `trsn_lst`, `init_marking`, `preset`, `is_enabled`, `fire`
- Optional: 3-tuple `{produce, Map, NewUsrInfo}` in fire/3 for usr_info updates

### 3. No Scattered Implementations

| Wrong | Correct |
|-------|---------|
| `yawl_executor:execute_pattern(Record)` | `gen_yawl:start_link(PatternMod, InitArg, [])` |
| `cre_yawl_patterns` monolithic gen_pnet | Individual `src/patterns/P*.erl` gen_yawl modules |
| `yawl_pattern_reference` pure records | Reference only; implement as gen_yawl modules |
| `gen_pnet:start_link` in yawl_execution | `gen_yawl:start_link` in yawl_execution |

### 4. Specification Pipeline

- **XML:** `wf_spec` → `yawl_validate` → `yawl_compile`
- **YAML 0.2:** `wf_yaml_spec` → `yawl_validate` → `yawl_compile`
- **Pattern expansion:** `yawl_pattern_registry` maps YAML macros to gen_yawl modules
- **Output:** Compiled modules implementing gen_yawl callbacks

### 5. 43 Pattern Registry

| YAML Macro | gen_yawl Module |
|------------|-----------------|
| P1_Sequence | sequence |
| P2_ParallelSplit | parallel_split |
| P3_Synchronization | and_join / synchronization |
| P4_ExclusiveChoice | exclusive_choice |
| P5_SimpleMerge | simple_merge |
| P6_MultipleChoice | multiple_choice |
| P7_StructuredSyncMerge | or_join |
| P8_MultipleMerge | multiple_merge |
| P9_Discriminator | discriminator |
| P10_ArbitraryCycles | arbitrary_cycles |
| P11-P43 | See plan: 43 patterns |

---

## Mistakes to Avoid

### Anti-Pattern 1: Direct gen_pnet for Workflows

```erlang
%% WRONG
gen_pnet:start_link(NetMod, InitArg, []).

%% CORRECT
gen_yawl:start_link(NetMod, InitArg, []).
```

### Anti-Pattern 2: Record-Based Pattern Dispatch

```erlang
%% WRONG (yawl_executor.erl style)
execute_pattern(#sequence{task_ids = Ids}, Input, _) -> ...
execute_pattern(#parallel_split{...}, Input, _) -> ...

%% CORRECT
%% Each pattern is a module; start it via gen_yawl
gen_yawl:start_link(parallel_split, #{branches => [...]}, []).
```

### Anti-Pattern 3: Monolithic Pattern Module

```erlang
%% WRONG (cre_yawl_patterns.erl)
-behaviour(gen_pnet).
%% One module with trigger/3 dispatching to many patterns

%% CORRECT
%% One module per pattern
-module(parallel_split).
-behaviour(gen_yawl).
```

### Anti-Pattern 4: Mixed Runtime

- Do not have some callers use gen_pnet and others gen_yawl for the same workflow.
- Standardize on gen_yawl everywhere.

### Anti-Pattern 5: wf_engine Without gen_yawl

- **wf_engine** is the high-level workflow API (worklist, allocate, complete, case_state).
- **wf_engine** is supposed to use gen_yawl internally. Currently it does not.
- Participants use wf_engine API — that is correct. The fix: make wf_engine delegate to gen_yawl.

---

## Migration Checklist

When implementing or refactoring:

- [ ] `yawl_execution.erl` uses `gen_yawl` (not gen_pnet)
- [ ] `wf_yawl_executor.erl` uses gen_yawl
- [ ] New patterns in `src/patterns/` have `-behaviour(gen_yawl)`
- [ ] `yawl_pattern_registry` maps all 43 macros to modules
- [ ] `yawl_compile` emits gen_yawl-compatible modules
- [ ] No `yawl_executor:execute_pattern` for workflow execution
- [ ] `cre_yawl_patterns` refactored or deprecated in favor of individual modules

---

## Z.AI Integration

For LLM-backed human task decisions in simulation:

- **zai_client** — HTTP client for Z.AI Chat API (`chat_json/2`)
- **Participant agents** — Call `zai_client:chat_json` when `zai_enabled => true`
- **gen_yawl:inject** — Participant completes human task by injecting decision token
- **Flow:** gen_yawl quiescent → discover pending tasks → Z.AI decision → inject → execute_step

See [sequence-omega-zai-execution.puml](sequence-omega-zai-execution.puml).

## References

- [ARCHITECTURE.md](../../ARCHITECTURE.md) — Joe Armstrong design, gen_pnet/gen_yawl
- [43 Patterns Plan](../../REMOTE_BRANCH_MERGE_SUMMARY.md) — Implementation context; see plan file for full details
- [gen_yawl.erl](../../../src/core/gen_yawl.erl) — Runtime implementation
- [zai_client.erl](../../../src/api/zai_client.erl) — Z.AI Chat API client
