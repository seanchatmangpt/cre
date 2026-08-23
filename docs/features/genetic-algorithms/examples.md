# GA Constitution Examples

This document provides practical examples of GA Constitutions for various use cases.

---

## Table of Contents

1. [Minimal Testing Constitution](#1-minimal-testing-constitution)
2. [Standard Workflow Constitution](#2-standard-workflow-constitution)
3. [Critical Safety Constitution](#3-critical-safety-constitution)
4. [Multi-Agent Swarm Constitution](#4-multi-agent-swarm-constitution)
5. [Hybrid Human-AI Constitution](#5-hybrid-human-ai-constitution)

---

## 1. Minimal Testing Constitution

For unit tests and development environments where rapid iteration is prioritized.

```yaml
ga_constitution:
  version: "1.0"
  constitution_id: "CON_TEST_MINIMAL_001"
  name: "Minimal Test Constitution"
  description: "Lightweight constitution for testing and development"

  profile:
    domain: testing
    safety_level: permissive
    verification_mode: none

  sigma:
    # Single primitive type for all tokens
    types:
      - name: TestToken
        base: term

    token_contracts:
      - name: AnyToken
        type: TestToken
        constraints: []

    place_types:
      # Accept any token, unlimited cardinality
      - {place: p_any, accepts: [TestToken], cardinality: [0, unlimited]}

    transition_contracts: []

  eta:
    # Only basic cycle detection
    refusals:
      - id: REF_NO_CYCLES
        name: "No Deadlock Cycles"
        category: liveness
        severity: warning
        guard:
          type: cycle_detection
        remediation:
          auto: false
          strategy: ignore

  kappa:
    invariants: []

    receipts:
      enabled: false
      hash_algorithm: none

    gates: []

    verification:
      static:
        enabled: false
      runtime:
        enabled: false
      periodic:
        enabled: false

  lambda:
    instances: []
    topology:
      nodes: []
      edges: []
```

---

## 2. Standard Workflow Constitution

For production workflows requiring standard safety guarantees.

```yaml
ga_constitution:
  version: "1.0"
  constitution_id: "CON_STANDARD_WORKFLOW_001"
  name: "Standard Workflow Constitution"
  description: "Production-ready constitution for business workflows"

  profile:
    domain: workflow
    safety_level: standard
    verification_mode: runtime

  sigma:
    types:
      # Standard workflow token
      - name: WorkflowToken
        base: record
        fields:
          - {name: case_id, type: binary, required: true}
          - {name: timestamp, type: datetime, required: true}
          - {name: payload, type: term, required: false}

      # Approval decision type
      - name: ApprovalDecision
        base: union
        variants: [approve, reject, request_changes]
        discriminant: decision

    token_contracts:
      - name: StandardWorkflowToken
        type: WorkflowToken
        constraints:
          - {field: case_id, constraint: non_empty}
          - {field: timestamp, constraint: recent, args: {max_age_ms: 60000}}

      - name: ApprovalWithReason
        type: ApprovalDecision
        constraints:
          - {variant: request_changes, requires: [comment]}

    place_types:
      - {place: p_start, accepts: [WorkflowToken], cardinality: [0, 1]}
      - {place: p_processing, accepts: [WorkflowToken], cardinality: [0, 10]}
      - {place: p_approval, accepts: [ApprovalDecision], cardinality: [0, 1]}
      - {place: p_end, accepts: [WorkflowToken, ApprovalDecision], cardinality: [0, unlimited]}

    transition_contracts:
      - transition: t_process
        consumes:
          - {place: p_start, type: WorkflowToken}
        produces:
          - {place: p_processing, type: WorkflowToken}

      - transition: t_approve
        consumes:
          - {place: p_processing, type: WorkflowToken}
        produces:
          - {place: p_approval, type: ApprovalDecision}

  eta:
    refusals:
      # Soundness: no orphan tokens
      - id: REF_NO_ORPHANS
        name: "No Orphan Tokens"
        category: soundness
        severity: error
        guard:
          type: marking_invariant
          check: "forall p in non_output_places: marking[p] = []"
        remediation:
          auto: false
          strategy: rollback_to_checkpoint
          recovery_steps:
            - {action: dump_marking, target: file}
            - {action: suspend_workflow}

      # Liveness: no cycles
      - id: REF_NO_CYCLES
        name: "No Marking Cycles"
        category: liveness
        severity: error
        guard:
          type: cycle_detection
        remediation:
          auto: false
          strategy: notify_operator

      # Safety: max tokens per place
      - id: REF_MAX_TOKENS
        name: "Max Token Limit"
        category: safety
        severity: warning
        guard:
          type: place_cardinality
          check: "forall p: length(marking[p]) <= 100"
        remediation:
          auto: false
          strategy: notify_operator

    handlers:
      - refusal: REF_NO_ORPHANS
        handler_module: ga_refusal_handler
        handler_function: handle_orphan_tokens
        priority: 1

  kappa:
    invariants:
      - id: INV_TERMINAL_INITIALLY_EMPTY
        name: "Terminal Places Initially Empty"
        check_points: [init]
        predicate:
          type: marking_check
          places: [p_end]
          condition: empty
        failure_action: raise_error

      - id: INV_NO_TOKEN_LEAKS
        name: "No Token Leaks"
        check_points: [always]
        predicate:
          type: conservation
          property: "sum(marking) = initial_count"
        failure_action: log_and_continue

    receipts:
      enabled: true
      hash_algorithm: sha256
      compression: true
      fields:
        - {name: timestamp, include: true}
        - {name: transition, include: true}
        - {name: marking_delta, include: true}
        - {name: usr_info_delta, include: false}
      retention:
        policy: duration_based
        duration_ms: 604800000  # 7 days

    replay:
      enabled: true
      deterministic: true
      max_replay_speed: 1000
      checkpoints:
        strategy: periodic
        interval_ms: 60000
      validation:
        check_markings: true
        check_receipts: true
        stop_on_mismatch: true

    gates:
      - id: GATE_MAX_ROUNDS
        name: "Max Execution Rounds"
        condition:
          type: threshold
          metric: rounds
          operator: "<="
          value: 1000
        on_fail: abort

      - id: GATE_EXECUTION_TIME
        name: "Max Execution Time"
        condition:
          type: threshold
          metric: execution_duration_ms
          operator: "<="
          value: 300000  # 5 minutes
        on_fail: warn

      - id: GATE_TOKEN_COUNT
        name: "Max Token Count"
        condition:
          type: threshold
          metric: total_tokens
          operator: "<="
          value: 1000
        on_fail: warn

    verification:
      static:
        enabled: true
        checks:
          - type_checking
          - well_formedness

      runtime:
        enabled: true
        checks:
          - invariant_monitoring
          - refusal_evaluation

      periodic:
        enabled: true
        interval_ms: 1000
        checks:
          - resource_limits
          - memory_usage

  lambda:
    instances:
      - id: P1_main_sequence
        pattern: P1_Sequence
        net: MainNet
        config:
          from: t_start
          to: t_end
        label: "Main Sequence"

      - id: P2_parallel_split
        pattern: P2_ParallelSplit
        net: MainNet
        config:
          split_task: t_split
          branches: [branch_a, branch_b]

      - id: P3_synchronization
        pattern: P3_Synchronization
        net: MainNet
        config:
          join_task: t_join
          waits_for: [branch_a, branch_b]

    topology:
      nodes: [P1_main_sequence, P2_parallel_split, P3_synchronization]
      edges:
        - {from: P1_main_sequence, to: P2_parallel_split}
        - {from: P2_parallel_split, to: P3_synchronization}

    compositions:
      - id: COMP_STANDARD_FLOW
        type: sequential
        order: [P1_main_sequence, P2_parallel_split, P3_synchronization]

    constraints: []
    macros: []
```

---

## 3. Critical Safety Constitution

For critical systems requiring maximum safety guarantees (medical, avionics, financial).

```yaml
ga_constitution:
  version: "1.0"
  constitution_id: "CON_CRITICAL_SAFETY_001"
  name: "Critical Safety Constitution"
  description: "Maximum safety for critical workflows"

  profile:
    domain: workflow
    safety_level: critical
    verification_mode: full

  sigma:
    types:
      - name: CriticalToken
        base: record
        fields:
          - {name: case_id, type: binary, required: true}
          - {name: timestamp, type: datetime, required: true}
          - {name: provenance, type: binary, required: true}
          - {name: checksum, type: binary, required: true}
          - {name: priority, type: enum, required: true}

    token_contracts:
      - name: CriticalWorkflowToken
        type: CriticalToken
        constraints:
          - {field: provenance, constraint: signed}
          - {field: checksum, constraint: valid_sha256}
          - {field: priority, constraint: in_range, args: {min: 1, max: 5}}

    place_types:
      - {place: p_critical_entry, accepts: [CriticalToken], cardinality: [1, 1]}
      - {place: p_critical_section, accepts: [CriticalToken], cardinality: [1, 1]}

    transition_contracts:
      - transition: t_critical_operation
        consumes:
          - {place: p_critical_entry, type: CriticalToken}
        produces:
          - {place: p_critical_section, type: CriticalToken}
        side_effects:
          - {action: audit_log, severity: critical}

  eta:
    refusals:
      - id: REF_MUTEX_VIOLATION
        name: "Critical Section Mutex Violation"
        category: mutual_exclusion
        severity: critical
        guard:
          type: place_cardinality
          place: p_critical_section
          max_tokens: 1
        remediation:
          auto: true
          strategy: rollback_and_retry
          recovery_steps:
            - {action: emit_alert, target: safety_monitor}
            - {action: checkpoint_rollback}

      - id: REF_PROVENANCE_BREACH
        name: "Provenance Verification Failed"
        category: safety
        severity: critical
        guard:
          type: type_check
        remediation:
          auto: true
          strategy: terminate

      - id: REF_PRIORITY_INVERSION
        name: "Priority Inversion Detected"
        category: safety
        severity: error
        guard:
          type: temporal
          check: "low_priority_not_blocking_high_priority"
        remediation:
          auto: true
          strategy: rollback_and_retry

    compositions:
      - id: REF_CRITICAL_COMPOSITE
        combines: [REF_MUTEX_VIOLATION, REF_PROVENANCE_BREACH]
        logic: all_of

  kappa:
    invariants:
      - id: INV_MUTEX_INVARIANT
        name: "Mutex Always Held"
        check_points: [always]
        predicate:
          type: marking_check
          places: [p_critical_section]
          condition: cardinality <= 1
        failure_action: abort

      - id: INV_TOKEN_CONSERVATION
        name: "Token Conservation"
        check_points: [always]
        predicate:
          type: conservation
          property: "sum(marking) = initial_count"
        failure_action: abort

      - id: INV_PRIORITY_ORDERING
        name: "Priority Ordering"
        check_points: [on_transition]
        predicate:
          type: marking_check
          condition: "higher_priority_preferred"
        failure_action: abort

    receipts:
      enabled: true
      hash_algorithm: sha512
      compression: false
      fields:
        - {name: timestamp, include: true}
        - {name: transition, include: true}
        - {name: marking_delta, include: true}
        - {name: usr_info_delta, include: true}
        - {name: provenance, include: true}
      retention:
        policy: indefinite

    replay:
      enabled: true
      deterministic: true
      max_replay_speed: 1  # Real-time replay
      checkpoints:
        strategy: on_event
        triggers: [before_critical_section]
      validation:
        check_markings: true
        check_receipts: true
        stop_on_mismatch: true

    gates:
      - id: GATE_EXECUTION_TIME
        name: "Max Execution Time"
        condition:
          type: threshold
          metric: execution_duration_ms
          operator: "<="
          value: 5000
        on_fail: abort

      - id: GATE_EXTERNAL_CALLS
        name: "Max External Calls"
        condition:
          type: threshold
          metric: external_call_count
          operator: "<="
          value: 10
        on_fail: abort

      - id: GATE_MEMORY_USAGE
        name: "Max Memory Usage"
        condition:
          type: threshold
          metric: memory_mb
          operator: "<="
          value: 1024
        on_fail: abort

    verification:
      static:
        enabled: true
        checks:
          - type_checking
          - well_formedness
          - soundness_proof

      runtime:
        enabled: true
        checks:
          - invariant_monitoring
          - refusal_evaluation
          - receipt_verification

      periodic:
        enabled: true
        interval_ms: 100
        checks:
          - resource_limits
          - memory_usage
          - throughput_check

  lambda:
    instances:
      - id: P39_critical_mutex
        pattern: P39_CriticalSection
        net: CriticalNet
        config:
          mutex: p_critical_mutex
          protected: t_critical_operation

      - id: P43_termination
        pattern: P43_ExplicitTermination
        net: CriticalNet
        config:
          terminator: t_emergency_stop

    topology:
      nodes: [P39_critical_mutex, P43_termination]
      edges:
        - {from: P39_critical_mutex, to: P43_termination}

    constraints:
      - applies_to: P39_CriticalSection
        constraint:
          type: mutual_exclusion
          scope: global
```

---

## 4. Multi-Agent Swarm Constitution

For coordinating multiple AI agents in a swarm workflow.

```yaml
ga_constitution:
  version: "1.0"
  constitution_id: "CON_SWARM_COORD_001"
  name: "Multi-Agent Swarm Constitution"
  description: "Constitution for AI swarm coordination"

  profile:
    domain: agent
    safety_level: standard
    verification_mode: runtime

  sigma:
    types:
      - name: AgentMessage
        base: record
        fields:
          - {name: from, type: binary, required: true}
          - {name: to, type: binary, required: true}
          - {name: content, type: term, required: true}
          - {name: message_id, type: binary, required: true}
          - {name: timestamp, type: datetime, required: true}

      - name: AgentState
        base: record
        fields:
          - {name: agent_id, type: binary, required: true}
          - {name: status, type: enum, required: true}
          - {name: assigned_task, type: binary, required: false}

    token_contracts:
      - name: AgentMessageContract
        type: AgentMessage
        constraints:
          - {field: message_id, constraint: unique}
          - {field: from, constraint: registered_agent}
          - {field: to, constraint: registered_agent}

    place_types:
      - {place: p_agent_inbox, accepts: [AgentMessage], cardinality: [0, unlimited]}
      - {place: p_agent_outbox, accepts: [AgentMessage], cardinality: [0, unlimited]}
      - {place: p_agent_state, accepts: [AgentState], cardinality: [1, 1]}

  eta:
    refusals:
      - id: REF_NO_MESSAGE_LOSS
        name: "No Message Loss"
        category: soundness
        severity: error
        guard:
          type: marking_invariant
          check: "sent_messages = delivered_messages"
        remediation:
          auto: true
          strategy: compensate

      - id: REF_NO_DUPLICATE_MESSAGES
        name: "No Duplicate Messages"
        category: safety
        severity: warning
        guard:
          type: marking_invariant
          check: "forall m: message_ids_unique(m)"
        remediation:
          auto: true
          strategy: ignore

      - id: REF_AGENT_NOT_BLOCKED
        name: "Agent Not Blocked"
        category: liveness
        severity: warning
        guard:
          type: temporal
          check: "agent_max_wait_time < timeout"
        remediation:
          auto: false
          strategy: notify_operator

    handlers:
      - refusal: REF_NO_MESSAGE_LOSS
        handler_module: swarm_message_handler
        handler_function: handle_message_loss
        priority: 1

  kappa:
    invariants:
      - id: INV_MESSAGE_ORDERING
        name: "Message Ordering Within Channel"
        check_points: [on_transition]
        predicate:
          type: custom_expression
          property: "messages_ordered_by_timestamp"
        failure_action: log_and_continue

    receipts:
      enabled: true
      hash_algorithm: sha256
      compression: true
      retention:
        policy: duration_based
        duration_ms: 86400000

    gates:
      - id: GATE_MAX_AGENTS
        name: "Max Concurrent Agents"
        condition:
          type: threshold
          metric: active_agents
          operator: "<="
          value: 100
        on_fail: warn

      - id: GATE_MAX_MESSAGES
        name: "Max Pending Messages"
        condition:
          type: threshold
          metric: pending_messages
          operator: "<="
          value: 10000
        on_fail: warn

    verification:
      runtime:
        enabled: true
        checks:
          - invariant_monitoring
          - refusal_evaluation

  lambda:
    instances:
      - id: P9_discriminator_agent_selection
        pattern: P9_Discriminator
        net: SwarmNet
        config:
          race: [agent_a, agent_b, agent_c]
          winner_to: p_selected_agent

      - id: P17_interleaved_agent_tasks
        pattern: P17_InterleavedParallelRouting
        net: SwarmNet
        config:
          tasks: [task_a, task_b, task_c]

    topology:
      nodes: [P9_discriminator_agent_selection, P17_interleaved_agent_tasks]
      edges:
        - {from: P9_discriminator_agent_selection, to: P17_interleaved_agent_tasks}

    constraints:
      - applies_to: P9_Discriminator
        constraint:
          type: cardinality
          min_branches: 2
          max_branches: 10
```

---

## 5. Hybrid Human-AI Constitution

For workflows involving both human and AI agents.

```yaml
ga_constitution:
  version: "1.0"
  constitution_id: "CON_HUMAN_AI_001"
  name: "Hybrid Human-AI Constitution"
  description: "Constitution for human-in-the-loop workflows with AI assistance"

  profile:
    domain: hybrid
    safety_level: standard
    verification_mode: runtime

  sigma:
    types:
      - name: HumanTask
        base: record
        fields:
          - {name: task_id, type: binary, required: true}
          - {name: assigned_to, type: binary, required: true}
          - {name: created_at, type: datetime, required: true}
          - {name: deadline, type: datetime, required: false}
          - {name: status, type: enum, required: true}

      - name: AIAssist
        base: record
        fields:
          - {name: suggestion_id, type: binary, required: true}
          - {name: confidence, type: float, required: true}
          - {name: rationale, type: binary, required: false}

      - name: HumanDecision
        base: union
        variants: [accept, reject, modify]
        discriminant: decision

    token_contracts:
      - name: HumanTaskContract
        type: HumanTask
        constraints:
          - {field: assigned_to, constraint: registered_human}
          - {field: status, constraint: valid_status}

      - name: AIAssistContract
        type: AIAssist
        constraints:
          - {field: confidence, constraint: in_range, args: {min: 0.0, max: 1.0}}

    place_types:
      - {place: p_human_task_queue, accepts: [HumanTask], cardinality: [0, unlimited]}
      - {place: p_ai_suggestions, accepts: [AIAssist], cardinality: [0, unlimited]}
      - {place: p_human_decision, accepts: [HumanDecision], cardinality: [0, 1]}

  eta:
    refusals:
      - id: REF_NO_STALE_AI_SUGGESTIONS
        name: "No Stale AI Suggestions"
        category: safety
        severity: warning
        guard:
          type: temporal
          check: "suggestion_age < task_duration"
        remediation:
          auto: true
          strategy: ignore

      - id: REF_HUMAN_ALWAYS_DECIDES
        name: "Human Must Decide"
        category: safety
        severity: critical
        guard:
          type: marking_invariant
          check: "ai_suggestion_only_with_human_decision"
        remediation:
          auto: true
          strategy: rollback_and_retry

      - id: REF_NO_DUPLICATE_TASKS
        name: "No Duplicate Human Tasks"
        category: soundness
        severity: error
        guard:
          type: marking_invariant
          check: "unique_task_ids"
        remediation:
          auto: true
          strategy: compensate

  kappa:
    invariants:
      - id: INV_AI_NOT_AUTONOMOUS
        name: "AI Cannot Execute Without Human"
        check_points: [on_transition]
        predicate:
          type: marking_check
          condition: "ai_output_needs_human_approval"
        failure_action: abort

    receipts:
      enabled: true
      hash_algorithm: sha256
      compression: true
      retention:
        policy: duration_based
        duration_ms: 2592000000  # 30 days

    gates:
      - id: GATE_HUMAN_RESPONSE_TIME
        name: "Max Human Response Time"
        condition:
          type: threshold
          metric: human_response_time_ms
          operator: "<="
          value: 86400000  # 1 day
        on_fail: warn

      - id: GATE_AI_CONFIDENCE_THRESHOLD
        name: "Min AI Confidence for Auto-Apply"
        condition:
          type: threshold
          metric: ai_confidence
          operator: ">="
          value: 0.95
        on_fail: require_human

  lambda:
    instances:
      - id: P16_deferred_choice
        pattern: P16_DeferredChoice
        net: HybridNet
        config:
          events: [human_accepts, human_rejects, ai_suggestion_provided]

      - id: P21_review_loop
        pattern: P21_StructuredLoop
        net: HybridNet
        config:
          entry: human_review
          body: [ai_assist, human_decision]
          exit_condition: "accept || final_reject"

    topology:
      nodes: [P16_deferred_choice, P21_review_loop]
      edges:
        - {from: P16_deferred_choice, to: P21_review_loop}

    constraints:
      - applies_to: P21_StructuredLoop
        constraint:
          type: cardinality
          max_branches: 3
```

---

## Appendix: Constitution Selection Guide

| Use Case | Recommended Constitution | Safety Level | Verification Mode |
|----------|-------------------------|--------------|-------------------|
| Unit Tests | Minimal | permissive | none |
| Development | Standard | permissive | static |
| Production Business | Standard | standard | runtime |
| Financial | Standard | strict | runtime |
| Medical/Aerospace | Critical | critical | full |
| AI Swarm | Multi-Agent | standard | runtime |
| Human-in-the-Loop | Hybrid | standard | runtime |
| Research | Standard | permissive | static |
| Demo/Prototype | Minimal | permissive | none |

---

**End of Examples**
