# Plan Gap Analysis - AGI Symposium Ω Implementation

**Date:** 2026-02-06  
**Purpose:** Verify plan completeness against AGI Symposium Ω YAML spec

---

## Critical Gaps Identified

### 1. YAML Spec Structure Not Fully Handled

**YAML Features Missing from Plan:**

| Feature | YAML Example | Plan Coverage | Gap |
|---------|-------------|---------------|-----|
| `subnets` with `entry:`/`exit:` | `subnets: [{id: ProgramThread, entry: ProgramEntry, exit: ProgramExit}]` | Partial - decompositions exist but entry/exit not explicit | **GAP** |
| `variables` in nets | `variables: [{name: symposium_state, type: string, initial: "planning"}]` | Mentioned but no implementation detail | **GAP** |
| `regions` with `cancel_region` | `regions: [{id: Region_HighRisk_Demos, cancel_region: true}]` | Not mentioned | **GAP** |
| `nodes` with `kind` | `kind: task|condition|inputCondition|outputCondition` | Not handled | **GAP** |
| `taskType` | `taskType: human|automated|service` | Not handled | **GAP** |
| `pattern_instances` expansion | Complex parameters (split_task, merge_task, branches, etc.) | Mentioned but no expansion algorithm | **GAP** |

### 2. Pattern Instance Expansion Missing

**YAML Pattern Instance Structure:**
```yaml
pattern_instances:
  - {id: P42_split_megathreads, pattern: P42_ThreadSplit, net: Symposium,
     split_task: SplitMegaThreads, branches: [ProgramThread, OpsThread, CommsThread]}
```

**What's Needed:**
- Parse `pattern_instances` from YAML
- Look up pattern macro in `pattern_registry`
- Expand pattern into places/transitions/flows
- Wire into parent net structure
- Handle pattern-specific parameters (split_task, merge_task, branches, waits_for, etc.)

**Current Plan:** Says "expand macro into places/transitions/flows" but no algorithm.

### 3. Subnet Entry/Exit Points

**YAML:**
```yaml
subnets:
  - {id: ProgramThread, entry: ProgramEntry, exit: ProgramExit}
```

**What's Needed:**
- Map subnet `entry:` to input condition place
- Map subnet `exit:` to output condition place
- Generate transitions that connect parent net → entry, exit → parent net
- Handle thread split/merge patterns (P41, P42) correctly

**Current Plan:** Mentions subnets but doesn't detail entry/exit handling.

### 4. Variables in usr_info

**YAML:**
```yaml
variables:
  - {name: symposium_state, type: string, initial: "planning"}
  - {name: emergency_stop, type: boolean, initial: false}
```

**What's Needed:**
- Store variables in `usr_info` map
- Initialize from `initial` values in `init/1`
- Update via 3-tuple `{produce, Map, NewUsrInfo}` in fire/3
- Use in predicates (e.g., `risk_to_N`, `attendance_estimate*0.08`)

**Current Plan:** Mentions usr_info but no variable initialization/update logic.

### 5. Regions for Cancellation

**YAML:**
```yaml
regions:
  - {id: Region_HighRisk_Demos, cancel_region: true, description: "High-risk demo expo zone"}
```

**What's Needed:**
- Track which places/nodes belong to which region
- On cancel event, cancel all tokens in region places
- Integrate with `yawl_cancellation.erl` / `wf_cancel.erl`

**Current Plan:** Not mentioned.

### 6. Node Types and Task Types

**YAML:**
```yaml
nodes:
  - {id: Start, kind: inputCondition}
  - {id: CFP, kind: task, taskType: service}
  - {id: GoNoGo, kind: task, taskType: human}
```

**What's Needed:**
- `inputCondition` / `outputCondition` → places (not transitions)
- `task` with `taskType: human` → may need worker assignment
- `task` with `taskType: automated` → auto-fire
- `task` with `taskType: service` → external service call

**Current Plan:** Doesn't distinguish node kinds or task types.

### 7. Pattern-Specific Parameter Handling

**Examples from YAML:**

| Pattern | Parameters | Expansion Needed |
|---------|------------|------------------|
| P42_ThreadSplit | `split_task`, `branches` | Create AND-split transition, spawn subnet instances |
| P41_ThreadMerge | `merge_task`, `waits_for` | Create AND-join transition, wait for subnet exits |
| P2_ParallelSplit | `split_task`, `branches` | Create parallel branches |
| P3_Synchronization | `join_task`, `waits_for` | Create AND-join |
| P21_StructuredLoop | `entry`, `body`, `exit_condition` | Create loop structure |
| P30_StructuredPartialJoin | `m`, `n` | Create N-of-M join |
| P39_CriticalSection | `mutex`, `protected` | Create mutex places |

**Current Plan:** No expansion algorithm for these parameters.

---

## Missing Implementation Details

### Phase 1: YAML Parsing

**Gap:** `wf_yaml_spec.erl` needs to parse:
- `subnets` array with entry/exit
- `variables` array with name/type/initial
- `regions` array with cancel_region flag
- `nodes` array with kind/taskType
- `pattern_instances` array with all parameters
- `pattern_registry` map
- `pattern_usage_index` map

**Action:** Add detailed parsing functions for each structure.

### Phase 3: Compiler Integration

**Gap:** Pattern expansion algorithm missing:

1. **Pattern Instance → Net Structure:**
   ```
   For each pattern_instance:
     - Look up pattern macro in pattern_registry
     - Get pattern module (e.g., thread_split.erl)
     - Call pattern expansion function with parameters
     - Generate places/transitions/flows
     - Wire into parent net
   ```

2. **Subnet Handling:**
   ```
   For each subnet:
     - Generate subnet module (or reference existing)
     - Create entry transition: parent → subnet_entry
     - Create exit transition: subnet_exit → parent
     - Handle thread split/merge patterns
   ```

3. **Variable Initialization:**
   ```
   In init/1:
     - Build usr_info map from variables initial values
     - Store variable names/types for type checking
   ```

4. **Region Tracking:**
   ```
   - Build region → [places] map
   - Store in net metadata
   - Use in cancellation logic
   ```

**Action:** Add `yawl_pattern_expander.erl` module with expansion functions.

---

## Updated Plan Additions

### New Phase: Pattern Expansion Module

**Add `src/core/yawl_pattern_expander.erl`:**

- `expand_pattern_instance/3` - Expand single pattern instance
- `expand_subnets/2` - Handle subnet entry/exit
- `initialize_variables/1` - Build usr_info from variables
- `build_regions/1` - Track region → places mapping
- `wire_pattern_into_net/4` - Connect pattern to parent net

### Enhanced Phase 3: Compiler Integration

**3.4 Pattern Expansion:**

- Parse `pattern_instances` from spec
- For each instance:
  - Look up pattern in registry
  - Call `yawl_pattern_expander:expand_pattern_instance/3`
  - Generate places/transitions/flows
  - Wire into net structure

**3.5 Subnet Handling:**

- Parse `subnets` from spec
- For each subnet:
  - Generate entry/exit transitions
  - Connect to parent net
  - Handle thread patterns (P41, P42)

**3.6 Variable Management:**

- Parse `variables` from spec
- Initialize in `init/1` callback
- Update via 3-tuple fire/3
- Use in predicates/conditions

**3.7 Region Tracking:**

- Parse `regions` from spec
- Build region → places map
- Integrate with cancellation

---

## Verification Checklist

Before running AGI Symposium Ω:

- [ ] YAML parser handles all structures (subnets, variables, regions, nodes, pattern_instances)
- [ ] Pattern expansion generates correct net structure for all 43 patterns
- [ ] Subnet entry/exit points connect correctly
- [ ] Variables initialize and update via usr_info
- [ ] Regions tracked for cancellation
- [ ] Node kinds (inputCondition, outputCondition, task) handled correctly
- [ ] Task types (human, automated, service) handled correctly
- [ ] Pattern instances expand with all parameters
- [ ] Thread split/merge (P41, P42) spawns subnets correctly
- [ ] All 26 implemented patterns verified in pattern_usage_index

---

## Risk Assessment

**High Risk:**

1. **Pattern Expansion Complexity** - 43 patterns × many parameters = complex expansion logic
   - **Mitigation:** Start with simple patterns (P1-P10), add complex ones incrementally

2. **Subnet Threading** - P41/P42 need to spawn actual subnet processes
   - **Mitigation:** Use gen_yawl:start_link for each subnet, track in usr_info

3. **Variable Predicates** - Expressions like `risk_to_N`, `attendance_estimate*0.08` need evaluation
   - **Mitigation:** Use `erlang:apply/3` or compile expressions to functions

**Medium Risk:**

1. **YAML Parsing** - Complex nested structures
   - **Mitigation:** Use yamerl, parse incrementally, validate

2. **Region Cancellation** - Need to track which places belong to regions
   - **Mitigation:** Build region map during compilation, use in cancellation

---

## Conclusion

**Plan Status:** Good foundation but missing critical implementation details for:
- Pattern expansion algorithm
- Subnet entry/exit handling
- Variable initialization/updates
- Region tracking
- Node/task type handling

**Recommendation:** Add Phase 3.4-3.7 and new `yawl_pattern_expander.erl` module before implementation.
