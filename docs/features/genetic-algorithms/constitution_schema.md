# GA Constitution Schema: Complete Specification

**Version:** 1.0
**Date:** 2026-02-07
**Status:** Design Specification

---

## Table of Contents

1. [Overview](#1-overview)
2. [Constitution Structure](#2-constitution-structure)
3. [Sigma (Σ): Typing Profile](#3-sigma-typing-profile)
4. [Eta (H): Refusal Catalog](#4-eta-h-refusal-catalog)
5. [Kappa (Q): Quality Gates](#5-kappa-q-quality-gates)
6. [Lambda (Λ): Pattern Composition](#6-lambda-pattern-composition)
7. [JSON Schema](#7-json-schema)
8. [Example Constitutions](#8-example-constitutions)
9. [Erlang Records](#9-erlang-record-definitions)
10. [Validation Rules](#10-validation-rules)

---

## 1. Overview

A GA Constitution is a declarative specification that governs how a workflow system operates. It combines four complementary aspects:

```
┌─────────────────────────────────────────────────────────────────┐
│                    GA Constitution                              │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  Σ (Sigma)    ───►  Typing: What data flows through the system  │
│  H (Eta)      ───►  Refusals: What MUST NOT happen              │
│  Q (Kappa)    ───►  Quality: What MUST be true                  │
│  Λ (Lambda)   ───►  Composition: How patterns connect           │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### Key Concepts

| Symbol | Name | Purpose |
|--------|------|---------|
| **Σ** | Sigma | Type system for tokens, places, and transitions |
| **H** | Eta | Refusal conditions and remediation strategies |
| **Q** | Kappa | Quality gates, invariants, and verification |
| **Λ** | Lambda | Pattern topology and composition rules |

---

## 2. Constitution Structure

The root YAML structure:

```yaml
ga_constitution:
  version: "1.0"
  constitution_id: "CON_XXXXXXXX"
  name: "Constitution Name"
  description: "Human-readable description"

  # Import other constitutions
  imports:
    - id: "BASE_CONSTITUTION"
      version: ">=1.0"
      alias: "base"

  # Profile: What kind of constitution?
  profile:
    domain: workflow | agent | hybrid | testing
    safety_level: permissive | standard | strict | critical
    verification_mode: none | static | runtime | full

  # The four aspects
  sigma: !sigma
    # Typing profile
    ...

  eta: !eta
    # Refusal catalog
    ...

  kappa: !kappa
    # Quality gates
    ...

  lambda: !lambda
    # Pattern composition
    ...
```

### Root Record Definition

```erlang
-record(ga_constitution, {
    id :: binary(),
    version :: binary(),
    name :: binary(),
    description :: binary() | undefined,
    imports :: [#constitution_import{}],
    profile :: #constitution_profile{},
    sigma :: #sigma{},
    eta :: #eta{},
    kappa :: #kappa{},
    lambda :: #lambda{},
    metadata :: map()
}).

-record(constitution_import, {
    id :: binary(),
    version :: binary() | undefined,
    alias :: atom()
}).

-record(constitution_profile, {
    domain :: constitution_domain(),
    safety_level :: safety_level(),
    verification_mode :: verification_mode()
}).

-type constitution_domain() :: workflow | agent | hybrid | testing.
-type safety_level() :: permissive | standard | strict | critical.
-type verification_mode() :: none | static | runtime | full.
```

---

## 3. Sigma (Σ): Typing Profile

Sigma defines the type system for tokens, places, and transitions.

### 3.1 YAML Structure

```yaml
sigma:
  # Type registry
  types:
    - name: WorkflowToken
      base: record
      fields:
        - {name: case_id, type: binary, required: true}
        - {name: timestamp, type: datetime, required: true}
        - {name: payload, type: term, required: false}

    - name: ApprovalDecision
      base: union
      variants: [approve, reject, request_changes]
      discriminant: decision

  # Token contracts
  token_contracts:
    - name: StandardToken
      type: WorkflowToken
      constraints:
        - {field: case_id, constraint: non_empty}
        - {field: timestamp, constraint: recent, max_age_ms: 60000}

    - name: ApprovalToken
      type: ApprovalDecision
      constraints:
        - {variant: request_changes, requires: [comment]}

  # Place typing
  place_types:
    - place: p_start
      accepts: [WorkflowToken]
      cardinality: [0, 1]  # min, max

    - place: p_approval
      accepts: [ApprovalToken]
      cardinality: [1, 1]  # exactly one

  # Transition contracts
  transition_contracts:
    - transition: t_review
      consumes:
        - {place: p_input, type: WorkflowToken}
      produces:
        - {place: p_output, type: ApprovalToken}
      side_effects:
        - {action: log_decision, severity: info}

  # Type narrowing (runtime type refinement)
  narrowing_rules:
    - {place: p_reviewed, narrows: WorkflowToken, to: ReviewedToken}
```

### 3.2 Sigma Records

```erlang
-record(sigma, {
    types :: #{binary() => #type_def{}},
    token_contracts :: [#token_contract{}],
    place_types :: [#place_type{}],
    transition_contracts :: [#transition_contract{}],
    narrowing_rules :: [#narrowing_rule{}]
}).

-record(type_def, {
    name :: binary(),
    base :: type_base(),
    fields :: [#field_def{}] | undefined,
    variants :: [atom()] | undefined,
    discriminant :: atom() | undefined
}).

-type type_base() :: record | union | enum | primitive | term.

-record(field_def, {
    name :: atom(),
    type :: atom(),
    required :: boolean(),
    constraints :: [#field_constraint{}]
}).

-record(field_constraint, {
    name :: atom(),
    args :: map()
}).

-record(token_contract, {
    name :: binary(),
    type :: binary(),
    constraints :: [#token_constraint{}]
}).

-record(token_constraint, {
    field :: atom() | undefined,
    variant :: atom() | undefined,
    constraint :: atom(),
    args :: map()
}).

-record(place_type, {
    place :: atom(),
    accepts :: [binary()],
    cardinality :: {non_neg_integer(), pos_integer() | unlimited}
}).

-record(transition_contract, {
    transition :: atom(),
    consumes :: [#place_consumption{}],
    produces :: [#place_production{}],
    side_effects :: [#side_effect{}]
}).

-record(place_consumption, {
    place :: atom(),
    type :: binary()
}).

-record(place_production, {
    place :: atom(),
    type :: binary()
}).

-record(side_effect, {
    action :: atom(),
    severity :: atom(),
    args :: map()
}).

-record(narrowing_rule, {
    place :: atom(),
    narrows :: binary(),
    to :: binary()
}).
```

---

## 4. Eta (H): Refusal Catalog

Eta defines what MUST NOT happen and how to remediate violations.

### 4.1 YAML Structure

```yaml
eta:
  version: "1.0"

  # Refusal conditions (guards)
  refusals:
    - id: REF_NO_ORPHAN_TOKENS
      name: "No Orphan Tokens"
      description: "Tokens must not be left in non-output places at termination"
      category: soundness
      severity: error
      guard:
        type: marking_invariant
        check: "forall p in non_output_places: marking[p] = []"
      remediation:
        auto: false
        strategy: notify_operator
        recovery_steps:
          - {action: dump_marking, target: file}
          - {action: suspend_workflow}

    - id: REF_NO_CYCLES_WITHOUT_PROGRESS
      name: "No Cycles Without Progress"
      description: "Detect marking cycles that make no forward progress"
      category: liveness
      severity: error
      guard:
        type: cycle_detection
        check: "marking_history_contains_cycle"
      remediation:
        auto: false
        strategy: rollback_to_checkpoint

    - id: REF_CRITICAL_SECTION_VIOLATION
      name: "Critical Section Violation"
      description: "Multiple processes in critical section simultaneously"
      category: mutual_exclusion
      severity: error
      guard:
        type: place_cardinality
        place: p_critical_section
        max_tokens: 1
      remediation:
        auto: true
        strategy: rollback_and_retry

  # Refusal patterns (parameterized)
  refusal_patterns:
    - name: MaxTokens
      params: [place, max]
      guard:
        type: place_cardinality
        max_tokens: "$max"
      remediation:
        auto: false

    - name: TypeMismatch
      params: [place, expected_type]
      guard:
        type: type_check
        place: "$place"
        type: "$expected_type"
      remediation:
        auto: false

  # Refusal composition
  compositions:
    - id: REF_SOUNDNESS_COMPOSITE
      combines: [REF_NO_ORPHAN_TOKENS, REF_PROPER_COMPLETION]
      logic: all_of  # all must be satisfied

    - id: REF_SAFETY_NET
      combines: [REF_NO_CYCLES_WITHOUT_PROGRESS, REF_MAX_ROUNDS]
      logic: any_of  # at least one must be satisfied

  # Refusal handlers
  handlers:
    - refusal: REF_NO_ORPHAN_TOKENS
      handler_module: refusal_soundness
      handler_function: handle_orphan_tokens
      priority: 1
```

### 4.2 Eta Records

```erlang
-record(eta, {
    version :: binary(),
    refusals :: [#refusal{}],
    refusal_patterns :: [#refusal_pattern{}],
    compositions :: [#refusal_composition{}],
    handlers :: [#refusal_handler{}]
}).

-record(refusal, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    category :: refusal_category(),
    severity :: severity_level(),
    guard :: #guard{},
    remediation :: #remediation{}
}).

-type refusal_category() ::
    soundness | liveness | mutual_exclusion | safety | type_checking | custom.
-type severity_level() :: warning | error | critical.

-record(guard, {
    type :: guard_type(),
    check :: binary() | undefined,
    args :: map()
}).

-type guard_type() ::
    marking_invariant | cycle_detection | place_cardinality |
    type_check | custom_predicate | temporal.

-record(remediation, {
    auto :: boolean(),
    strategy :: remediation_strategy(),
    recovery_steps :: [#recovery_step{}] | undefined
}).

-type remediation_strategy() ::
    notify_operator | rollback_to_checkpoint | rollback_and_retry |
    ignore | compensate | suspend_workflow | terminate.

-record(recovery_step, {
    action :: atom(),
    target :: atom() | binary(),
    args :: map()
}).

-record(refusal_pattern, {
    name :: binary(),
    params :: [atom()],
    guard :: #guard{},
    remediation :: #remediation{}
}).

-record(refusal_composition, {
    id :: binary(),
    combines :: [binary()],
    logic :: composition_logic()
}).

-type composition_logic() :: all_of | any_of | exactly_one | none_of.

-record(refusal_handler, {
    refusal :: binary(),
    handler_module :: atom(),
    handler_function :: atom(),
    priority :: integer()
}).
```

---

## 5. Kappa (Q): Quality Gates

Kappa defines quality gates, invariants, and verification configuration.

### 5.1 YAML Structure

```yaml
kappa:
  version: "1.0"

  # Invariants (must hold at all times)
  invariants:
    - id: INV_TERMINAL_PLACES_EMPTY
      name: "Terminal Places Initially Empty"
      check_points: [init]
      predicate:
        type: marking_check
        places: [p_end, p_error]
        condition: empty
      failure_action: raise_error

    - id: INV_NO_TOKEN_LEAKS
      name: "No Token Leaks"
      check_points: [always]
      predicate:
        type: conservation
        property: "sum(marking) = constant"
      failure_action: log_and_continue

  # Receipts (verification trail)
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
      max_receipts: 1000000

  # Replay configuration
  replay:
    enabled: true
    deterministic: true
    max_replay_speed: 1000  # max steps per second

    checkpoints:
      strategy: periodic
      interval_ms: 60000

    validation:
      check_markings: true
      check_receipts: true
      stop_on_mismatch: true

  # Quality gates
  gates:
    - id: GATE_PATTERN_COVERAGE
      name: "Pattern Coverage Gate"
      condition:
        type: threshold
        metric: patterns_used
        operator: ">="
        value: 10
      on_fail: warn

    - id: GATE_EXECUTION_TIME
      name: "Max Execution Time"
      condition:
        type: threshold
        metric: execution_duration_ms
        operator: "<="
        value: 300000
      on_fail: abort

    - id: GATE_TOKEN_COUNT
      name: "Max Token Count"
      condition:
        type: threshold
        metric: total_tokens
        operator: "<="
        value: 1000
      on_fail: warn

  # Verification
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
      interval_ms: 1000
      checks:
        - resource_limits
        - memory_usage
```

### 5.2 Kappa Records

```erlang
-record(kappa, {
    version :: binary(),
    invariants :: [#invariant{}],
    receipts :: #receipt_config{},
    replay :: #replay_config{},
    gates :: [#quality_gate{}],
    verification :: #verification_config{}
}).

-record(invariant, {
    id :: binary(),
    name :: binary(),
    check_points :: [check_point()],
    predicate :: #invariant_predicate{},
    failure_action :: failure_action()
}).

-type check_point() :: init | always | on_transition | on_completion | custom.

-record(invariant_predicate, {
    type :: predicate_type(),
    places :: [atom()] | undefined,
    condition :: atom(),
    property :: binary() | undefined
}).

-type predicate_type() ::
    marking_check | conservation | custom_expression | temporal.

-type failure_action() :: raise_error | log_and_continue | rollback | abort.

-record(receipt_config, {
    enabled :: boolean(),
    hash_algorithm :: hash_algorithm(),
    compression :: boolean(),
    fields :: [#receipt_field{}],
    retention :: #retention_policy{}
}).

-type hash_algorithm() :: sha256 | sha512 | blake2b | none.

-record(receipt_field, {
    name :: atom(),
    include :: boolean()
}).

-record(retention_policy, {
    policy :: retention_type(),
    duration_ms :: integer() | undefined,
    max_receipts :: integer() | undefined
}).

-type retention_type() :: duration_based | count_based | indefinite.

-record(replay_config, {
    enabled :: boolean(),
    deterministic :: boolean(),
    max_replay_speed :: pos_integer(),
    checkpoints :: #checkpoint_config{},
    validation :: #replay_validation{}
}).

-record(checkpoint_config, {
    strategy :: checkpoint_strategy(),
    interval_ms :: integer() | undefined,
    triggers :: [atom()] | undefined
}).

-type checkpoint_strategy() :: periodic | on_demand | on_event.

-record(replay_validation, {
    check_markings :: boolean(),
    check_receipts :: boolean(),
    stop_on_mismatch :: boolean()
}).

-record(quality_gate, {
    id :: binary(),
    name :: binary(),
    condition :: #gate_condition{},
    on_fail :: gate_action()
}).

-record(gate_condition, {
    type :: gate_condition_type(),
    metric :: atom(),
    operator :: comparison_operator(),
    value :: number()
}).

-type gate_condition_type() :: threshold | range | expression.
-type comparison_operator() :: '=:=' | '=/=' | '<' | '=<' | '>' | '>='.
-type gate_action() :: warn | abort | rollback | ignore.

-record(verification_config, {
    static :: #static_verification{},
    runtime :: #runtime_verification{},
    periodic :: #periodic_verification{}
}).

-record(static_verification, {
    enabled :: boolean(),
    checks :: [static_check()]
}).

-type static_check() :: type_checking | well_formedness | soundness_proof.

-record(runtime_verification, {
    enabled :: boolean(),
    checks :: [runtime_check()]
}).

-type runtime_check() :: invariant_monitoring | refusal_evaluation | receipt_verification.

-record(periodic_verification, {
    enabled :: boolean(),
    interval_ms :: integer(),
    checks :: [periodic_check()]
}).

-type periodic_check() :: resource_limits | memory_usage | throughput_check.
```

---

## 6. Lambda (Λ): Pattern Composition

Lambda defines the pattern topology and composition rules.

### 6.1 YAML Structure

```yaml
lambda:
  version: "1.0"

  # Pattern instances
  instances:
    - id: P1_sequence_main
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
        branches: [branch_a, branch_b, branch_c]

  # Pattern topology
  topology:
    nodes: [P1_sequence_main, P2_parallel_split, P3_sync]
    edges:
      - {from: P1_sequence_main, to: P2_parallel_split}
      - {from: P2_parallel_split, to: P3_sync}

  # Composition rules
  compositions:
    - id: COMP_SEQUENTIAL_FLOW
      type: sequential
      order: [P1_sequence_main, P2_parallel_split]

    - id: COMP_PARALLEL_BRANCHES
      type: parallel
      branches: [P2_parallel_split.branch_a, P2_parallel_split.branch_b]

    - id: COMP_NESTED_PATTERN
      type: nested
      outer: P22_Recursion
      inner: P1_sequence_main

  # Pattern constraints
  constraints:
    - applies_to: P9_Discriminator
      constraint:
        type: cardinality
        min_branches: 2
        max_branches: 10

    - applies_to: P39_CriticalSection
      constraint:
        type: mutual_exclusion
        scope: global

  # Pattern macros
  macros:
    - name: standard_workflow
      expands_to:
        - P1_Sequence
        - P2_ParallelSplit
        - P3_Synchronization
```

### 6.2 Lambda Records

```erlang
-record(lambda, {
    version :: binary(),
    instances :: [#pattern_instance{}],
    topology :: #pattern_topology{},
    compositions :: [#pattern_composition{}],
    constraints :: [#pattern_constraint{}],
    macros :: [#pattern_macro{}]
}).

-record(pattern_instance, {
    id :: binary(),
    pattern :: binary(),
    net :: binary(),
    config :: map(),
    label :: binary() | undefined
}).

-record(pattern_topology, {
    nodes :: [binary()],
    edges :: [#topology_edge{}]
}).

-record(topology_edge, {
    from :: binary(),
    to :: binary(),
    condition :: binary() | undefined
}).

-record(pattern_composition, {
    id :: binary(),
    type :: composition_type(),
    order :: [binary()] | undefined,
    branches :: [binary()] | undefined,
    outer :: binary() | undefined,
    inner :: binary() | undefined
}).

-type composition_type() :: sequential | parallel | nested | conditional | recursive.

-record(pattern_constraint, {
    applies_to :: binary(),
    constraint :: #pattern_constraint_def{}
}).

-record(pattern_constraint_def, {
    type :: constraint_type(),
    min_branches :: pos_integer() | undefined,
    max_branches :: pos_integer() | undefined,
    scope :: atom() | undefined
}).

-type constraint_type() :: cardinality | mutual_exclusion | temporal | custom.

-record(pattern_macro, {
    name :: binary(),
    expands_to :: [binary()],
    params :: [atom()] | undefined
}).
```

---

## 7. JSON Schema

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "$id": "https://cre.workflow/ga-constitution-schema-v1.json",
  "title": "GA Constitution",
  "description": "Complete schema for Generative Architecture Constitutions",
  "type": "object",
  "required": ["ga_constitution"],
  "properties": {
    "ga_constitution": {
      "type": "object",
      "required": ["version", "constitution_id", "name", "sigma", "eta", "kappa", "lambda"],
      "properties": {
        "version": {
          "type": "string",
          "pattern": "^\\d+\\.\\d+$"
        },
        "constitution_id": {
          "type": "string",
          "pattern": "^CON_[A-F0-9]+$"
        },
        "name": {
          "type": "string",
          "minLength": 1,
          "maxLength": 256
        },
        "description": {
          "type": "string"
        },
        "imports": {
          "type": "array",
          "items": {
            "$ref": "#/definitions/import"
          }
        },
        "profile": {
          "$ref": "#/definitions/profile"
        },
        "sigma": {
          "$ref": "#/definitions/sigma"
        },
        "eta": {
          "$ref": "#/definitions/eta"
        },
        "kappa": {
          "$ref": "#/definitions/kappa"
        },
        "lambda": {
          "$ref": "#/definitions/lambda"
        }
      }
    }
  },
  "definitions": {
    "import": {
      "type": "object",
      "required": ["id", "alias"],
      "properties": {
        "id": {"type": "string"},
        "version": {"type": "string"},
        "alias": {"type": "string"}
      }
    },
    "profile": {
      "type": "object",
      "required": ["domain", "safety_level", "verification_mode"],
      "properties": {
        "domain": {
          "type": "string",
          "enum": ["workflow", "agent", "hybrid", "testing"]
        },
        "safety_level": {
          "type": "string",
          "enum": ["permissive", "standard", "strict", "critical"]
        },
        "verification_mode": {
          "type": "string",
          "enum": ["none", "static", "runtime", "full"]
        }
      }
    },
    "sigma": {
      "type": "object",
      "required": ["types", "token_contracts", "place_types"],
      "properties": {
        "types": {
          "type": "array",
          "items": {"$ref": "#/definitions/type_def"}
        },
        "token_contracts": {
          "type": "array",
          "items": {"$ref": "#/definitions/token_contract"}
        },
        "place_types": {
          "type": "array",
          "items": {"$ref": "#/definitions/place_type"}
        },
        "transition_contracts": {
          "type": "array",
          "items": {"$ref": "#/definitions/transition_contract"}
        },
        "narrowing_rules": {
          "type": "array",
          "items": {"$ref": "#/definitions/narrowing_rule"}
        }
      }
    },
    "type_def": {
      "type": "object",
      "required": ["name", "base"],
      "properties": {
        "name": {"type": "string"},
        "base": {
          "type": "string",
          "enum": ["record", "union", "enum", "primitive", "term"]
        },
        "fields": {
          "type": "array",
          "items": {"$ref": "#/definitions/field_def"}
        },
        "variants": {
          "type": "array",
          "items": {"type": "string"}
        },
        "discriminant": {"type": "string"}
      }
    },
    "field_def": {
      "type": "object",
      "required": ["name", "type"],
      "properties": {
        "name": {"type": "string"},
        "type": {"type": "string"},
        "required": {"type": "boolean"},
        "constraints": {
          "type": "array",
          "items": {"$ref": "#/definitions/field_constraint"}
        }
      }
    },
    "field_constraint": {
      "type": "object",
      "required": ["name"],
      "properties": {
        "name": {"type": "string"},
        "args": {"type": "object"}
      }
    },
    "token_contract": {
      "type": "object",
      "required": ["name", "type"],
      "properties": {
        "name": {"type": "string"},
        "type": {"type": "string"},
        "constraints": {
          "type": "array",
          "items": {"$ref": "#/definitions/token_constraint"}
        }
      }
    },
    "token_constraint": {
      "type": "object",
      "properties": {
        "field": {"type": "string"},
        "variant": {"type": "string"},
        "constraint": {"type": "string"},
        "args": {"type": "object"}
      }
    },
    "place_type": {
      "type": "object",
      "required": ["place", "accepts"],
      "properties": {
        "place": {"type": "string"},
        "accepts": {
          "type": "array",
          "items": {"type": "string"}
        },
        "cardinality": {
          "type": "array",
          "minItems": 2,
          "maxItems": 2,
          "items": {"type": "integer"}
        }
      }
    },
    "transition_contract": {
      "type": "object",
      "required": ["transition"],
      "properties": {
        "transition": {"type": "string"},
        "consumes": {
          "type": "array",
          "items": {"$ref": "#/definitions/place_consumption"}
        },
        "produces": {
          "type": "array",
          "items": {"$ref": "#/definitions/place_production"}
        },
        "side_effects": {
          "type": "array",
          "items": {"$ref": "#/definitions/side_effect"}
        }
      }
    },
    "place_consumption": {
      "type": "object",
      "required": ["place", "type"],
      "properties": {
        "place": {"type": "string"},
        "type": {"type": "string"}
      }
    },
    "place_production": {
      "type": "object",
      "required": ["place", "type"],
      "properties": {
        "place": {"type": "string"},
        "type": {"type": "string"}
      }
    },
    "side_effect": {
      "type": "object",
      "required": ["action"],
      "properties": {
        "action": {"type": "string"},
        "severity": {"type": "string"},
        "args": {"type": "object"}
      }
    },
    "narrowing_rule": {
      "type": "object",
      "required": ["place", "narrows", "to"],
      "properties": {
        "place": {"type": "string"},
        "narrows": {"type": "string"},
        "to": {"type": "string"}
      }
    },
    "eta": {
      "type": "object",
      "required": ["refusals"],
      "properties": {
        "version": {"type": "string"},
        "refusals": {
          "type": "array",
          "items": {"$ref": "#/definitions/refusal"}
        },
        "refusal_patterns": {
          "type": "array",
          "items": {"$ref": "#/definitions/refusal_pattern"}
        },
        "compositions": {
          "type": "array",
          "items": {"$ref": "#/definitions/refusal_composition"}
        },
        "handlers": {
          "type": "array",
          "items": {"$ref": "#/definitions/refusal_handler"}
        }
      }
    },
    "refusal": {
      "type": "object",
      "required": ["id", "name", "category", "severity", "guard", "remediation"],
      "properties": {
        "id": {"type": "string"},
        "name": {"type": "string"},
        "description": {"type": "string"},
        "category": {
          "type": "string",
          "enum": ["soundness", "liveness", "mutual_exclusion", "safety", "type_checking", "custom"]
        },
        "severity": {
          "type": "string",
          "enum": ["warning", "error", "critical"]
        },
        "guard": {"$ref": "#/definitions/guard"},
        "remediation": {"$ref": "#/definitions/remediation"}
      }
    },
    "guard": {
      "type": "object",
      "required": ["type"],
      "properties": {
        "type": {
          "type": "string",
          "enum": ["marking_invariant", "cycle_detection", "place_cardinality", "type_check", "custom_predicate", "temporal"]
        },
        "check": {"type": "string"},
        "args": {"type": "object"}
      }
    },
    "remediation": {
      "type": "object",
      "required": ["auto", "strategy"],
      "properties": {
        "auto": {"type": "boolean"},
        "strategy": {
          "type": "string",
          "enum": ["notify_operator", "rollback_to_checkpoint", "rollback_and_retry", "ignore", "compensate", "suspend_workflow", "terminate"]
        },
        "recovery_steps": {
          "type": "array",
          "items": {"$ref": "#/definitions/recovery_step"}
        }
      }
    },
    "recovery_step": {
      "type": "object",
      "required": ["action"],
      "properties": {
        "action": {"type": "string"},
        "target": {"type": "string"},
        "args": {"type": "object"}
      }
    },
    "refusal_pattern": {
      "type": "object",
      "required": ["name", "params", "guard"],
      "properties": {
        "name": {"type": "string"},
        "params": {
          "type": "array",
          "items": {"type": "string"}
        },
        "guard": {"$ref": "#/definitions/guard"},
        "remediation": {"$ref": "#/definitions/remediation"}
      }
    },
    "refusal_composition": {
      "type": "object",
      "required": ["id", "combines", "logic"],
      "properties": {
        "id": {"type": "string"},
        "combines": {
          "type": "array",
          "items": {"type": "string"}
        },
        "logic": {
          "type": "string",
          "enum": ["all_of", "any_of", "exactly_one", "none_of"]
        }
      }
    },
    "refusal_handler": {
      "type": "object",
      "required": ["refusal", "handler_module", "handler_function"],
      "properties": {
        "refusal": {"type": "string"},
        "handler_module": {"type": "string"},
        "handler_function": {"type": "string"},
        "priority": {"type": "integer"}
      }
    },
    "kappa": {
      "type": "object",
      "required": ["invariants", "receipts", "gates"],
      "properties": {
        "version": {"type": "string"},
        "invariants": {
          "type": "array",
          "items": {"$ref": "#/definitions/invariant"}
        },
        "receipts": {"$ref": "#/definitions/receipt_config"},
        "replay": {"$ref": "#/definitions/replay_config"},
        "gates": {
          "type": "array",
          "items": {"$ref": "#/definitions/quality_gate"}
        },
        "verification": {"$ref": "#/definitions/verification_config"}
      }
    },
    "invariant": {
      "type": "object",
      "required": ["id", "predicate", "failure_action"],
      "properties": {
        "id": {"type": "string"},
        "name": {"type": "string"},
        "check_points": {
          "type": "array",
          "items": {"type": "string", "enum": ["init", "always", "on_transition", "on_completion", "custom"]}
        },
        "predicate": {"$ref": "#/definitions/invariant_predicate"},
        "failure_action": {
          "type": "string",
          "enum": ["raise_error", "log_and_continue", "rollback", "abort"]
        }
      }
    },
    "invariant_predicate": {
      "type": "object",
      "required": ["type", "condition"],
      "properties": {
        "type": {
          "type": "string",
          "enum": ["marking_check", "conservation", "custom_expression", "temporal"]
        },
        "places": {
          "type": "array",
          "items": {"type": "string"}
        },
        "condition": {"type": "string"},
        "property": {"type": "string"}
      }
    },
    "receipt_config": {
      "type": "object",
      "required": ["enabled", "hash_algorithm"],
      "properties": {
        "enabled": {"type": "boolean"},
        "hash_algorithm": {
          "type": "string",
          "enum": ["sha256", "sha512", "blake2b", "none"]
        },
        "compression": {"type": "boolean"},
        "fields": {
          "type": "array",
          "items": {"$ref": "#/definitions/receipt_field"}
        },
        "retention": {"$ref": "#/definitions/retention_policy"}
      }
    },
    "receipt_field": {
      "type": "object",
      "required": ["name", "include"],
      "properties": {
        "name": {"type": "string"},
        "include": {"type": "boolean"}
      }
    },
    "retention_policy": {
      "type": "object",
      "required": ["policy"],
      "properties": {
        "policy": {
          "type": "string",
          "enum": ["duration_based", "count_based", "indefinite"]
        },
        "duration_ms": {"type": "integer"},
        "max_receipts": {"type": "integer"}
      }
    },
    "replay_config": {
      "type": "object",
      "required": ["enabled"],
      "properties": {
        "enabled": {"type": "boolean"},
        "deterministic": {"type": "boolean"},
        "max_replay_speed": {"type": "integer"},
        "checkpoints": {"$ref": "#/definitions/checkpoint_config"},
        "validation": {"$ref": "#/definitions/replay_validation"}
      }
    },
    "checkpoint_config": {
      "type": "object",
      "required": ["strategy"],
      "properties": {
        "strategy": {
          "type": "string",
          "enum": ["periodic", "on_demand", "on_event"]
        },
        "interval_ms": {"type": "integer"},
        "triggers": {
          "type": "array",
          "items": {"type": "string"}
        }
      }
    },
    "replay_validation": {
      "type": "object",
      "properties": {
        "check_markings": {"type": "boolean"},
        "check_receipts": {"type": "boolean"},
        "stop_on_mismatch": {"type": "boolean"}
      }
    },
    "quality_gate": {
      "type": "object",
      "required": ["id", "condition", "on_fail"],
      "properties": {
        "id": {"type": "string"},
        "name": {"type": "string"},
        "condition": {"$ref": "#/definitions/gate_condition"},
        "on_fail": {
          "type": "string",
          "enum": ["warn", "abort", "rollback", "ignore"]
        }
      }
    },
    "gate_condition": {
      "type": "object",
      "required": ["type", "metric", "operator", "value"],
      "properties": {
        "type": {
          "type": "string",
          "enum": ["threshold", "range", "expression"]
        },
        "metric": {"type": "string"},
        "operator": {"type": "string"},
        "value": {"type": "number"}
      }
    },
    "verification_config": {
      "type": "object",
      "properties": {
        "static": {"$ref": "#/definitions/static_verification"},
        "runtime": {"$ref": "#/definitions/runtime_verification"},
        "periodic": {"$ref": "#/definitions/periodic_verification"}
      }
    },
    "static_verification": {
      "type": "object",
      "properties": {
        "enabled": {"type": "boolean"},
        "checks": {
          "type": "array",
          "items": {"type": "string", "enum": ["type_checking", "well_formedness", "soundness_proof"]}
        }
      }
    },
    "runtime_verification": {
      "type": "object",
      "properties": {
        "enabled": {"type": "boolean"},
        "checks": {
          "type": "array",
          "items": {"type": "string", "enum": ["invariant_monitoring", "refusal_evaluation", "receipt_verification"]}
        }
      }
    },
    "periodic_verification": {
      "type": "object",
      "properties": {
        "enabled": {"type": "boolean"},
        "interval_ms": {"type": "integer"},
        "checks": {
          "type": "array",
          "items": {"type": "string", "enum": ["resource_limits", "memory_usage", "throughput_check"]}
        }
      }
    },
    "lambda": {
      "type": "object",
      "required": ["instances", "topology"],
      "properties": {
        "version": {"type": "string"},
        "instances": {
          "type": "array",
          "items": {"$ref": "#/definitions/pattern_instance"}
        },
        "topology": {"$ref": "#/definitions/pattern_topology"},
        "compositions": {
          "type": "array",
          "items": {"$ref": "#/definitions/pattern_composition"}
        },
        "constraints": {
          "type": "array",
          "items": {"$ref": "#/definitions/pattern_constraint"}
        },
        "macros": {
          "type": "array",
          "items": {"$ref": "#/definitions/pattern_macro"}
        }
      }
    },
    "pattern_instance": {
      "type": "object",
      "required": ["id", "pattern", "net", "config"],
      "properties": {
        "id": {"type": "string"},
        "pattern": {"type": "string"},
        "net": {"type": "string"},
        "config": {"type": "object"},
        "label": {"type": "string"}
      }
    },
    "pattern_topology": {
      "type": "object",
      "required": ["nodes", "edges"],
      "properties": {
        "nodes": {
          "type": "array",
          "items": {"type": "string"}
        },
        "edges": {
          "type": "array",
          "items": {"$ref": "#/definitions/topology_edge"}
        }
      }
    },
    "topology_edge": {
      "type": "object",
      "required": ["from", "to"],
      "properties": {
        "from": {"type": "string"},
        "to": {"type": "string"},
        "condition": {"type": "string"}
      }
    },
    "pattern_composition": {
      "type": "object",
      "required": ["id", "type"],
      "properties": {
        "id": {"type": "string"},
        "type": {
          "type": "string",
          "enum": ["sequential", "parallel", "nested", "conditional", "recursive"]
        },
        "order": {
          "type": "array",
          "items": {"type": "string"}
        },
        "branches": {
          "type": "array",
          "items": {"type": "string"}
        },
        "outer": {"type": "string"},
        "inner": {"type": "string"}
      }
    },
    "pattern_constraint": {
      "type": "object",
      "required": ["applies_to", "constraint"],
      "properties": {
        "applies_to": {"type": "string"},
        "constraint": {"$ref": "#/definitions/pattern_constraint_def"}
      }
    },
    "pattern_constraint_def": {
      "type": "object",
      "required": ["type"],
      "properties": {
        "type": {
          "type": "string",
          "enum": ["cardinality", "mutual_exclusion", "temporal", "custom"]
        },
        "min_branches": {"type": "integer"},
        "max_branches": {"type": "integer"},
        "scope": {"type": "string"}
      }
    },
    "pattern_macro": {
      "type": "object",
      "required": ["name", "expands_to"],
      "properties": {
        "name": {"type": "string"},
        "expands_to": {
          "type": "array",
          "items": {"type": "string"}
        },
        "params": {
          "type": "array",
          "items": {"type": "string"}
        }
      }
    }
  }
}
```

---

## 8. Example Constitutions

### 8.1 Minimal Constitution (Testing)

```yaml
ga_constitution:
  version: "1.0"
  constitution_id: "CON_TEST_MINIMAL_001"
  name: "Minimal Test Constitution"
  profile:
    domain: testing
    safety_level: permissive
    verification_mode: none

  sigma:
    types:
      - name: SimpleToken
        base: term
    token_contracts: []
    place_types: []
    transition_contracts: []

  eta:
    refusals:
      - id: REF_NO_DEADLOCK
        name: "No Deadlock"
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

  lambda:
    instances: []
    topology:
      nodes: []
      edges: []
```

### 8.2 Standard Workflow Constitution

```yaml
ga_constitution:
  version: "1.0"
  constitution_id: "CON_STANDARD_WORKFLOW_001"
  name: "Standard Workflow Constitution"
  description: "Production-ready constitution for standard workflows"
  profile:
    domain: workflow
    safety_level: standard
    verification_mode: runtime

  imports:
    - id: "BASE_SOUNDNESS"
      alias: base_soundness

  sigma:
    types:
      - name: WorkflowToken
        base: record
        fields:
          - {name: case_id, type: binary, required: true}
          - {name: timestamp, type: datetime, required: true}

      - name: ApprovalToken
        base: union
        variants: [approve, reject]
        discriminant: decision

    token_contracts:
      - name: StandardWorkflowToken
        type: WorkflowToken
        constraints:
          - {field: case_id, constraint: non_empty}

    place_types:
      - {place: p_start, accepts: [WorkflowToken], cardinality: [0, 1]}
      - {place: p_end, accepts: [WorkflowToken], cardinality: [0, unlimited]}

    transition_contracts:
      - transition: t_process
        consumes:
          - {place: p_start, type: WorkflowToken}
        produces:
          - {place: p_end, type: WorkflowToken}

  eta:
    refusals:
      - id: REF_SOUNDNESS_ORPHANS
        name: "No Orphan Tokens"
        category: soundness
        severity: error
        guard:
          type: marking_invariant
          check: "forall p in non_output: marking[p] = []"
        remediation:
          auto: false
          strategy: rollback_to_checkpoint

      - id: REF_NO_CYCLES
        name: "No Marking Cycles"
        category: liveness
        severity: error
        guard:
          type: cycle_detection
        remediation:
          auto: false
          strategy: notify_operator

  kappa:
    invariants:
      - id: INV_TERMINAL_INITIALLY_EMPTY
        name: "Terminal Initially Empty"
        check_points: [init]
        predicate:
          type: marking_check
          places: [p_end]
          condition: empty
        failure_action: raise_error

    receipts:
      enabled: true
      hash_algorithm: sha256
      compression: true
      fields:
        - {name: timestamp, include: true}
        - {name: transition, include: true}
        - {name: marking_delta, include: true}
      retention:
        policy: duration_based
        duration_ms: 86400000

    gates:
      - id: GATE_MAX_ROUNDS
        name: "Max Execution Rounds"
        condition:
          type: threshold
          metric: rounds
          operator: "<="
          value: 1000
        on_fail: abort

    verification:
      runtime:
        enabled: true
        checks: [invariant_monitoring, refusal_evaluation]

  lambda:
    instances:
      - id: P1_main_sequence
        pattern: P1_Sequence
        net: MainNet
        config:
          from: t_start
          to: t_end

    topology:
      nodes: [P1_main_sequence]
      edges: []
```

### 8.3 Critical Safety Constitution

```yaml
ga_constitution:
  version: "1.0"
  constitution_id: "CON_CRITICAL_SAFETY_001"
  name: "Critical Safety Constitution"
  description: "Maximum safety for critical workflows (medical, avionics, etc.)"
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

    token_contracts:
      - name: CriticalWorkflowToken
        type: CriticalToken
        constraints:
          - {field: provenance, constraint: signed}
          - {field: checksum, constraint: valid_sha256}

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
      max_replay_speed: 1
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

    verification:
      static:
        enabled: true
        checks: [type_checking, well_formedness, soundness_proof]
      runtime:
        enabled: true
        checks: [invariant_monitoring, refusal_evaluation, receipt_verification]
      periodic:
        enabled: true
        interval_ms: 100
        checks: [resource_limits, memory_usage]

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

## 9. Erlang Record Definitions

Complete header file for GA Constitutions.

```erlang
%%-------------------------------------------------------------------
%% @doc GA Constitution Record Definitions
%%
%% This header file defines all records used for parsing and
%% processing GA (Generative Architecture) Constitutions.
%%
%% A GA Constitution combines four aspects:
%% - Sigma (Σ): Typing profile
%% - Eta (H): Refusal catalog
%% - Kappa (Q): Quality gates
%% - Lambda (Λ): Pattern composition
%% @end
%%-------------------------------------------------------------------

-ifndef(GA_CONSTITUTION_HRL).
-define(GA_CONSTITUTION_HRL).

%%====================================================================
%% Root Constitution Records
%%====================================================================

-record(ga_constitution, {
    id :: binary(),
    version :: binary(),
    name :: binary(),
    description :: binary() | undefined,
    imports :: [#constitution_import{}] | [],
    profile :: #constitution_profile{},
    sigma :: #sigma{},
    eta :: #eta{},
    kappa :: #kappa{},
    lambda :: #lambda{},
    metadata :: map()
}).

-record(constitution_import, {
    id :: binary(),
    version :: binary() | undefined,
    alias :: atom()
}).

-record(constitution_profile, {
    domain :: constitution_domain(),
    safety_level :: safety_level(),
    verification_mode :: verification_mode()
}).

-type constitution_domain() :: workflow | agent | hybrid | testing.
-type safety_level() :: permissive | standard | strict | critical.
-type verification_mode() :: none | static | runtime | full.

%%====================================================================
%% Sigma (Σ): Typing Profile Records
%%====================================================================

-record(sigma, {
    types :: #{binary() => #type_def{}},
    token_contracts :: [#token_contract{}] | [],
    place_types :: [#place_type{}] | [],
    transition_contracts :: [#transition_contract{}] | [],
    narrowing_rules :: [#narrowing_rule{}] | []
}).

-record(type_def, {
    name :: binary(),
    base :: type_base(),
    fields :: [#field_def{}] | undefined,
    variants :: [atom()] | undefined,
    discriminant :: atom() | undefined,
    metadata :: map()
}).

-type type_base() :: record | union | enum | primitive | term.

-record(field_def, {
    name :: atom(),
    type :: atom(),
    required :: boolean(),
    constraints :: [#field_constraint{}] | []
}).

-record(field_constraint, {
    name :: atom(),
    args :: map()
}).

-record(token_contract, {
    name :: binary(),
    type :: binary(),
    constraints :: [#token_constraint{}] | []
}).

-record(token_constraint, {
    field :: atom() | undefined,
    variant :: atom() | undefined,
    constraint :: atom(),
    args :: map()
}).

-record(place_type, {
    place :: atom(),
    accepts :: [binary()],
    cardinality :: {non_neg_integer(), pos_integer() | unlimited},
    metadata :: map()
}).

-record(transition_contract, {
    transition :: atom(),
    consumes :: [#place_consumption{}] | [],
    produces :: [#place_production{}] | [],
    side_effects :: [#side_effect{}] | []
}).

-record(place_consumption, {
    place :: atom(),
    type :: binary()
}).

-record(place_production, {
    place :: atom(),
    type :: binary()
}).

-record(side_effect, {
    action :: atom(),
    severity :: atom(),
    args :: map()
}).

-record(narrowing_rule, {
    place :: atom(),
    narrows :: binary(),
    to :: binary()
}).

%%====================================================================
%% Eta (H): Refusal Catalog Records
%%====================================================================

-record(eta, {
    version :: binary(),
    refusals :: [#refusal{}] | [],
    refusal_patterns :: [#refusal_pattern{}] | [],
    compositions :: [#refusal_composition{}] | [],
    handlers :: [#refusal_handler{}] | []
}).

-record(refusal, {
    id :: binary(),
    name :: binary(),
    description :: binary(),
    category :: refusal_category(),
    severity :: severity_level(),
    guard :: #guard{},
    remediation :: #remediation{},
    metadata :: map()
}).

-type refusal_category() ::
    soundness | liveness | mutual_exclusion | safety | type_checking | custom.
-type severity_level() :: warning | error | critical.

-record(guard, {
    type :: guard_type(),
    check :: binary() | undefined,
    args :: map()
}).

-type guard_type() ::
    marking_invariant | cycle_detection | place_cardinality |
    type_check | custom_predicate | temporal.

-record(remediation, {
    auto :: boolean(),
    strategy :: remediation_strategy(),
    recovery_steps :: [#recovery_step{}] | undefined
}).

-type remediation_strategy() ::
    notify_operator | rollback_to_checkpoint | rollback_and_retry |
    ignore | compensate | suspend_workflow | terminate.

-record(recovery_step, {
    action :: atom(),
    target :: atom() | binary(),
    args :: map()
}).

-record(refusal_pattern, {
    name :: binary(),
    params :: [atom()],
    guard :: #guard{},
    remediation :: #remediation{},
    metadata :: map()
}).

-record(refusal_composition, {
    id :: binary(),
    combines :: [binary()],
    logic :: composition_logic(),
    metadata :: map()
}).

-type composition_logic() :: all_of | any_of | exactly_one | none_of.

-record(refusal_handler, {
    refusal :: binary(),
    handler_module :: atom(),
    handler_function :: atom(),
    priority :: integer()
}).

%%====================================================================
%% Kappa (Q): Quality Gates Records
%%====================================================================

-record(kappa, {
    version :: binary(),
    invariants :: [#invariant{}] | [],
    receipts :: #receipt_config{},
    replay :: #replay_config{},
    gates :: [#quality_gate{}] | [],
    verification :: #verification_config{}
}).

-record(invariant, {
    id :: binary(),
    name :: binary(),
    check_points :: [check_point()],
    predicate :: #invariant_predicate{},
    failure_action :: failure_action(),
    metadata :: map()
}).

-type check_point() :: init | always | on_transition | on_completion | custom.

-record(invariant_predicate, {
    type :: predicate_type(),
    places :: [atom()] | undefined,
    condition :: atom(),
    property :: binary() | undefined,
    args :: map()
}).

-type predicate_type() ::
    marking_check | conservation | custom_expression | temporal.

-type failure_action() :: raise_error | log_and_continue | rollback | abort.

-record(receipt_config, {
    enabled :: boolean(),
    hash_algorithm :: hash_algorithm(),
    compression :: boolean(),
    fields :: [#receipt_field{}] | [],
    retention :: #retention_policy{}
}).

-type hash_algorithm() :: sha256 | sha512 | blake2b | none.

-record(receipt_field, {
    name :: atom(),
    include :: boolean()
}).

-record(retention_policy, {
    policy :: retention_type(),
    duration_ms :: integer() | undefined,
    max_receipts :: integer() | undefined
}).

-type retention_type() :: duration_based | count_based | indefinite.

-record(replay_config, {
    enabled :: boolean(),
    deterministic :: boolean(),
    max_replay_speed :: pos_integer(),
    checkpoints :: #checkpoint_config{},
    validation :: #replay_validation{}
}).

-record(checkpoint_config, {
    strategy :: checkpoint_strategy(),
    interval_ms :: integer() | undefined,
    triggers :: [atom()] | undefined
}).

-type checkpoint_strategy() :: periodic | on_demand | on_event.

-record(replay_validation, {
    check_markings :: boolean(),
    check_receipts :: boolean(),
    stop_on_mismatch :: boolean()
}).

-record(quality_gate, {
    id :: binary(),
    name :: binary(),
    condition :: #gate_condition{},
    on_fail :: gate_action(),
    metadata :: map()
}).

-record(gate_condition, {
    type :: gate_condition_type(),
    metric :: atom(),
    operator :: comparison_operator(),
    value :: number()
}).

-type gate_condition_type() :: threshold | range | expression.
-type comparison_operator() :: '=:=' | '=/=' | '<' | '=<' | '>' | '>='.
-type gate_action() :: warn | abort | rollback | ignore.

-record(verification_config, {
    static :: #static_verification{},
    runtime :: #runtime_verification{},
    periodic :: #periodic_verification{}
}).

-record(static_verification, {
    enabled :: boolean(),
    checks :: [static_check()]
}).

-type static_check() :: type_checking | well_formedness | soundness_proof.

-record(runtime_verification, {
    enabled :: boolean(),
    checks :: [runtime_check()]
}).

-type runtime_check() :: invariant_monitoring | refusal_evaluation | receipt_verification.

-record(periodic_verification, {
    enabled :: boolean(),
    interval_ms :: integer(),
    checks :: [periodic_check()]
}).

-type periodic_check() :: resource_limits | memory_usage | throughput_check.

%%====================================================================
%% Lambda (Λ): Pattern Composition Records
%%====================================================================

-record(lambda, {
    version :: binary(),
    instances :: [#pattern_instance{}] | [],
    topology :: #pattern_topology{},
    compositions :: [#pattern_composition{}] | [],
    constraints :: [#pattern_constraint{}] | [],
    macros :: [#pattern_macro{}] | []
}).

-record(pattern_instance, {
    id :: binary(),
    pattern :: binary(),
    net :: binary(),
    config :: map(),
    label :: binary() | undefined,
    metadata :: map()
}).

-record(pattern_topology, {
    nodes :: [binary()],
    edges :: [#topology_edge{}]
}).

-record(topology_edge, {
    from :: binary(),
    to :: binary(),
    condition :: binary() | undefined
}).

-record(pattern_composition, {
    id :: binary(),
    type :: composition_type(),
    order :: [binary()] | undefined,
    branches :: [binary()] | undefined,
    outer :: binary() | undefined,
    inner :: binary() | undefined,
    metadata :: map()
}).

-type composition_type() :: sequential | parallel | nested | conditional | recursive.

-record(pattern_constraint, {
    applies_to :: binary(),
    constraint :: #pattern_constraint_def{},
    metadata :: map()
}).

-record(pattern_constraint_def, {
    type :: constraint_type(),
    min_branches :: pos_integer() | undefined,
    max_branches :: pos_integer() | undefined,
    scope :: atom() | undefined,
    args :: map()
}).

-type constraint_type() :: cardinality | mutual_exclusion | temporal | custom.

-record(pattern_macro, {
    name :: binary(),
    expands_to :: [binary()],
    params :: [atom()] | undefined,
    metadata :: map()
}).

%%====================================================================
%% Utility Types
%%====================================================================

-type constitution_result() :: {ok, #ga_constitution{}} | {error, constitution_error()}.
-type constitution_error() ::
    {invalid_id, binary()} |
    {invalid_version, binary()} |
    {missing_required_field, binary()} |
    {type_mismatch, binary(), term(), term()} |
    {constraint_violation, binary(), term()} |
    {validation_failed, [binary()]}.

-define(GA_CONSTITUTION_VERSION, "1.0").

-endif.
```

---

## 10. Validation Rules

### 10.1 Structural Validation

| Rule | Description |
|------|-------------|
| `REQ-001` | Constitution must have valid `constitution_id` format: `CON_[A-F0-9]+` |
| `REQ-002` | Version must follow semantic versioning: `^\d+\.\d+$` |
| `REQ-003` | All four aspects (Σ, H, Q, Λ) must be present |
| `REQ-004` | Import aliases must be unique |
| `REQ-005` | Profile domain, safety_level, and verification_mode must be valid enum values |

### 10.2 Sigma Validation

| Rule | Description |
|------|-------------|
| `SIG-001` | Type names must be unique |
| `SIG-002` | Union types must have discriminant field |
| `SIG-003` | Token contract types must reference defined types |
| `SIG-004` | Place cardinality: max >= min |
| `SIG-005` | Transition consumes must reference valid places |
| `SIG-006` | Transition produces must reference valid places |
| `SIG-007` | Narrowing rules must reference valid types |

### 10.3 Eta Validation

| Rule | Description |
|------|-------------|
| `ETA-001` | Refusal IDs must be unique |
| `ETA-002` | Refusal pattern params must match guard args |
| `ETA-003` | Composition references must exist in refusals |
| `ETA-004` | Handler modules must be loadable |
| `ETA-005` | Critical severity refusals must have remediation |

### 10.4 Kappa Validation

| Rule | Description |
|------|-------------|
| `KAP-001` | Invariant IDs must be unique |
| `KAP-002` | Receipt retention must have at least one constraint |
| `KAP-003` | Replay interval must be > 0 for periodic strategy |
| `KAP-004` | Gate metrics must be valid |
| `KAP-005` | Critical safety profiles must have receipt verification enabled |

### 10.5 Lambda Validation

| Rule | Description |
|------|-------------|
| `LAM-001` | Pattern instance IDs must be unique |
| `LAM-002` | Pattern references must be in registry |
| `LAM-003` | Topology edges must reference valid nodes |
| `LAM-004` | Composition must not create cycles (unless recursive) |
| `LAM-005` | Macro params must be valid atoms |

### 10.6 Cross-Aspect Validation

| Rule | Description |
|------|-------------|
| `CROSS-001` | Eta refusals must not contradict Sigma type contracts |
| `CROSS-002` | Kappa invariants must be compatible with Lambda patterns |
| `CROSS-003` | Place types in Sigma must match Lambda instances |
| `CROSS-004` | Critical safety profile requires comprehensive verification |

### 10.7 Safety Level Constraints

| Safety Level | Required Checks |
|--------------|-----------------|
| `permissive` | REQ-001 through REQ-005 only |
| `standard` | All SIG, ETA, KAP, LAM rules |
| `strict` | All standard + cross-aspect validation |
| `critical` | All strict + additional proofs required |

---

## Appendix A: Quick Reference

### Constitution Template

```yaml
ga_constitution:
  version: "1.0"
  constitution_id: "CON_XXXXXXXX"
  name: "My Constitution"
  profile:
    domain: workflow
    safety_level: standard
    verification_mode: runtime

  sigma:
    types: []
    token_contracts: []
    place_types: []
    transition_contracts: []

  eta:
    refusals: []

  kappa:
    invariants: []
    receipts: {enabled: true, hash_algorithm: sha256}
    gates: []

  lambda:
    instances: []
    topology: {nodes: [], edges: []}
```

### Default Values

| Field | Default | Description |
|-------|---------|-------------|
| `description` | `undefined` | Optional description |
| `imports` | `[]` | No imports |
| `sigma.narrowing_rules` | `[]` | No type narrowing |
| `eta.handlers` | `[]` | No custom handlers |
| `kappa.receipts.compression` | `true` | Compress receipts |
| `kappa.replay.deterministic` | `true` | Deterministic replay |
| `lambda.macros` | `[]` | No macros |

---

**End of Specification**
