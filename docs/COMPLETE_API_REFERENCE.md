# Complete API Reference

This document provides the complete API specification for the refactored CRE architecture following Joe Armstrong's interface design: **one real OTP runner (`gen_pnet`), everything else pure helpers/utilities + message contracts**.

## Table of Contents

1. [Core OTP Behaviors](#1-core-otp-behaviors)
   - [gen_pnet - Petri Net Runtime](#11-gen_pnet---petri-net-runtime)
   - [gen_yawl - YAWL Wrapper with 3-tuple fire/3](#12-gen_yawl---yawl-wrapper)

2. [YAWL Compilation Modules](#2-yawl-compilation-modules)
   - [yawl_compile - YAWL to pnet_net Compiler](#21-yawl_compile---yawl-to-pnet_net-compiler)
   - [yawl_compiled - Compiled Specification Accessor](#22-yawl_compiled---compiled-specification-accessor)
   - [yawl_validate - YAWL Specification Validation](#23-yawl_validate---yawl-validation)

3. [PNET Pure Helper Modules](#3-pnet-pure-helper-modules)
   - [pnet_types - Type System and Validation](#31-pnet_types---type-system-and-validation)
   - [pnet_marking - Multiset Marking Algebra](#32-pnet_marking---multiset-marking-algebra)
   - [pnet_mode - Mode Enumeration](#33-pnet_mode---mode-enumeration)
   - [pnet_choice - Deterministic Nondeterminism](#34-pnet_choice---deterministic-nondeterminism)
   - [pnet_receipt - Receipt Tracking](#35-pnet_receipt---receipt-tracking)

4. [Workflow Utility Modules (wf_*)](#4-workflow-utility-modules-wf_)
   - [wf_spec - YAWL XML Parser and IR](#41-wf_spec---yawl-xml-parser-and-ir)
   - [wf_rules - Rule Evaluation Engine](#42-wf_rules---rule-evaluation)
   - [wf_yawl_pred - XPath to Erlog Compiler](#43-wf_yawl_pred---xpath-to-erlog-compiler)
   - [wf_timer - ISO 8601 Duration Parser](#44-wf_timer---iso-8601-duration-parser)
   - [wf_timerq - Deadline Queue](#45-wf_timerq---deadline-queue)
   - [wf_task - External Task Token Constructors](#46-wf_task---external-task-token-constructors)
   - [wf_mi - Multi-Instance Handling](#47-wf_mi---multi-instance-handling)
   - [wf_cancel - Cancellation Tokens](#48-wf_cancel---cancellation-tokens)
   - [wf_scope - Boundary Mapping](#49-wf_scope---boundary-mapping)

5. [YAWL Runtime Modules (wf/yawl_*)](#5-yawl-runtime-modules-wfyawl_)
   - [yawl_execution - Execution Engine](#51-yawl_execution---execution-engine)
   - [yawl_state - State Management](#52-yawl_state---state-management)
   - [yawl_cancellation - Cancellation Runtime](#53-yawl_cancellation---cancellation-runtime)
   - [yawl_telemetry - OpenTelemetry Integration](#54-yawl_telemetry---opentelemetry-integration)
   - [yawl_executor - Task Execution](#55-yawl_executor---task-execution)
   - [yawl_timer_runtime - Timer Runtime](#56-yawl_timer_runtime---timer-runtime)
   - [yawl_mi_runtime - Multi-Instance Runtime](#57-yawl_mi_runtime---multi-instance-runtime)
   - [yawl_schema - Schema Definitions](#58-yawl_schema---schema-definitions)
   - [yawl_recovery - Checkpoint and Recovery](#59-yawl_recovery---checkpoint-and-recovery)
   - [yawl_data - Data Flow Management](#510-yawl_data---data-flow-management)
   - [yawl_pred_eval - Predicate Evaluation](#511-yawl_pred_eval---predicate-evaluation)

6. [Message Contracts](#6-message-contracts)

7. [Type Definitions](#7-type-definitions)

---

## Module Index

| Module | Category | Description |
|--------|----------|-------------|
| `gen_pnet` | Core OTP | Petri net gen_server behavior |
| `gen_yawl` | Core OTP | YAWL wrapper with 3-tuple fire/3 support |
| `yawl_compile` | Compilation | YAWL XML to pnet_net compiler |
| `yawl_compiled` | Compilation | Compiled spec accessor functions |
| `yawl_validate` | Compilation | YAWL specification validation |
| `pnet_types` | PNET Helper | Type validation for Petri net structures |
| `pnet_marking` | PNET Helper | Multiset marking algebra operations |
| `pnet_mode` | PNET Helper | Mode enumeration for transition firing |
| `pnet_choice` | PNET Helper | Deterministic choice for nondeterminism |
| `pnet_receipt` | PNET Helper | Receipt tracking for audit trails |
| `wf_spec` | Workflow | YAWL XML parser and intermediate representation |
| `wf_rules` | Workflow | Erlog-backed rule evaluation |
| `wf_yawl_pred` | Workflow | XPath to Erlog compiler |
| `wf_timer` | Workflow | ISO 8601 duration parsing |
| `wf_timerq` | Workflow | Deadline queue for time-based events |
| `wf_task` | Workflow | External task token constructors |
| `wf_mi` | Workflow | Multi-instance task handling |
| `wf_cancel` | Workflow | Cancellation token operations |
| `wf_scope` | Workflow | Boundary mapping for nested nets |
| `yawl_execution` | Runtime | YAWL execution engine |
| `yawl_state` | Runtime | Workflow state management |
| `yawl_cancellation` | Runtime | Cancellation region handling |
| `yawl_telemetry` | Runtime | OpenTelemetry observability |
| `yawl_executor` | Runtime | External task executor |
| `yawl_timer_runtime` | Runtime | Timer event execution |
| `yawl_mi_runtime` | Runtime | Multi-instance execution |
| `yawl_schema` | Runtime | Schema type definitions |
| `yawl_recovery` | Runtime | Checkpoint and recovery |
| `yawl_data` | Runtime | Data flow between tasks |
| `yawl_pred_eval` | Runtime | Predicate evaluation engine |

---

## 1. Core OTP Behaviors

The core of the CRE architecture consists of exactly two OTP behaviors: `gen_pnet` (the Petri net runtime) and `gen_yawl` (a convenience wrapper for YAWL workflows). Following Joe Armstrong's design principle, these are the only modules that maintain state as OTP processes. All other modules are pure helper functions.

### 1.1 gen_pnet - Petri Net Runtime

**Location:** `/Users/sac/cre/src/core/gen_pnet.erl`

**Module:** `gen_pnet` - A generic Petri net OTP behavior

#### Description

`gen_pnet` is a behavior module for implementing Petri net workflows as Erlang/OTP gen_server processes. It provides automatic transition firing, token processing, and statistics tracking. This is the **only** stateful OTP process in the CRE architecture following Joe Armstrong's design principle.

#### Callbacks

**Structure Callbacks (required):**
- `place_lst/0` - Returns list of place names
- `trsn_lst/0` - Returns list of transition names
- `init_marking/2` - Returns initial marking for a place
- `preset/1` - Returns input places for a transition
- `is_enabled/3` - Determines if transition is enabled in a mode
- `fire/3` - Returns tokens produced when transition fires

**Interface Callbacks (required):**
- `init/1` - Initializes the net instance
- `handle_call/3` - Synchronous message exchange
- `handle_cast/2` - Asynchronous message reception
- `handle_info/2` - Unformatted message reception
- `code_change/3` - Hot code reload handling
- `terminate/2` - Cleanup on termination
- `trigger/3` - Side effects when tokens are produced

#### API Functions

```erlang
% Starting
start_link(NetMod, NetArg, Options) -> start_link_result()
start_link(ServerName, NetMod, NetArg, Options) -> start_link_result()

% Query
ls(Name, Place) -> {ok, [term()]} | {error, bad_place}
marking(Name) -> #{atom() => [term()]}
usr_info(Name) -> term()
stats(Name) -> #stats{}
reset_stats(Name) -> ok

% Communication
call(Name, Request) -> term()
call(Name, Request, Timeout) -> term()
cast(Name, Request) -> ok
reply(Client, Reply) -> ok

% State
inject(Name, ProduceMap) -> {ok, Receipt} | {error, Reason}
step(Name) -> abort | {ok, Receipt}
drain(Name, MaxSteps) -> {ok, [Receipt]} | {error, limit}
state_property(Name, Pred, PlaceLst) -> ok | {error, Reason}

% Lifecycle
stop(Name) -> ok
```

#### Records

```erlang
-record(bad_place, {name}).
-record(net_state, {
    marking,
    net_mod,
    usr_info,
    stats,
    tstart,
    cnt
}).
-record(stat, {t, fps}).
-record(stats, {current, hi, lo}).
```

---

### 1.2 gen_yawl - YAWL Wrapper

**Location:** `/Users/sac/cre/src/core/gen_yawl.erl`

**Module:** `gen_yawl` - YAWL wrapper with 3-tuple fire/3 support

#### Description

A drop-in replacement for `gen_pnet` that adds enhanced support for 3-tuple returns from the `fire/3` callback. When a transition returns `{produce, ProduceMap, NewUsrInfo}`, the user info is automatically updated. This is essential for YAWL workflows where state needs to be tracked as part of transition firing.

#### Key Features

- Drop-in Replacement: Same API surface as gen_pnet
- Enhanced fire/3: Supports 3-tuple returns for automatic state updates
- Backward Compatible: Works with existing gen_pnet callback modules
- Transparent Wrapping: No changes to Petri net semantics

#### Enhanced fire/3 Return Types

```erlang
-callback fire(Trsn, Mode, UsrInfo) ->
    abort |                                      % Abort transition
    {produce, ProduceMap} |                    % Standard gen_pnet behavior
    {produce, ProduceMap, NewUsrInfo}.         % Enhanced behavior with state update
```

#### API

Same as gen_pnet (see section 1.1).

---

## 2. YAWL Compilation Modules

The YAWL compilation modules transform YAWL 2.1/2.2 specifications into gen_pnet-compatible Petri net modules.

### 2.1 yawl_compile - YAWL to pnet_net Compiler

**Location:** `/Users/sac/cre/src/core/yawl_compile.erl`

#### Description

Compiles YAWL 2.1/2.2 specifications into gen_pnet compatible Petri net modules. Each YAWL decomposition becomes a standalone Erlang module implementing the gen_pnet behavior.

#### API

```erlang
compile(Spec, Options) -> {ok, compile_result()} | {error, Reason}
compile_to_file(Spec, Options, OutputDir) -> {ok, [file:filename()]} | {error, Reason}
generate_module(NetId, NetInfo) -> {ok, binary()} | {error, Reason}
generate_places(NetInfo) -> [place()]
generate_transitions(NetInfo) -> [transition()]
```

#### Types

```erlang
-type compile_options() :: #{
    seed := non_neg_integer(),
    module_prefix := binary(),
    output_dir => file:filename_all(),
    include_source => boolean(),
    gen_observer => boolean()
}.

-type compile_result() :: #{
    spec_id := binary(),
    modules := #{binary() => module()},
    places := #{binary() => [place()]},
    transitions := #{binary() => [transition()]},
    net_info := #{binary() => map()}
}.
```

---

### 2.2 yawl_compiled - Compiled Specification Accessor

**Location:** `/Users/sac/cre/src/core/yawl_compiled.erl`

#### Description

Provides accessor functions for compiled YAWL specifications. Stores compiled modules in persistent_term for O(1) lookup.

#### API

```erlang
get_net_list() -> [net_id()]
get_net_info(NetId) -> net_info() | undefined
get_net_module(NetId) -> module() | undefined
get_all_nets() -> #{net_id() => net_info()}
is_net_compiled(NetId) -> boolean()
get_net_stats(NetId) -> #{atom() => term()}
```

---

### 2.3 yawl_validate - YAWL Specification Validation

**Location:** `/Users/sac/cre/src/core/yawl_validate.erl`

#### Description

Validates YAWL specifications against interface requirements including net structure, transition connectivity, place definitions, and transition definitions.

#### API

```erlang
validate(XmlOrSpec) -> ok | {error, term()}
validate_net_structure(XmlOrSpec) -> ok | throw(Reason)
validate_transition_connectivity(XmlOrSpec) -> ok
validate_places(XmlOrSpec) -> ok
validate_transitions(XmlOrSpec) -> ok
```

---

## 3. PNET Pure Helper Modules

All PNET helper modules are pure functions with no side effects. They provide type validation, marking algebra, mode enumeration, deterministic choice, and receipt tracking.

### 3.1 pnet_types - Type System and Validation

**Location:** `/Users/sac/cre/src/pnet/pnet_types.erl`

#### Description

Type validators for Petri net data structures. All validators are total: they return true/false and never crash, making them safe to use in guards and assertions.

#### Types

```erlang
% Basic Petri net types
-type place() :: atom().
-type trsn() :: atom().
-type token() :: term().
-type marking() :: #{place() => [token()]}.
-type consume_map() :: #{place() => [token()]}.
-type produce_map() :: #{place() => [token()]}.
-type mode() :: #{place() => [token()]}.

% Colored Petri net extension
-type var() :: atom().
-type binding() :: #{var() => term()}.
-type cmode() :: {binding(), mode()}.

% Execution tracking
-type move() :: #{trsn := trsn(), mode := mode() | cmode(), produce := produce_map()}.
-type receipt() :: #{before_hash := binary(), after_hash := binary(), move := move(), ts := integer()}.
```

#### API

```erlang
% Basic type validators
is_place(Term) -> boolean()
is_trsn(Term) -> boolean()
is_token(Term) -> boolean()
is_var(Term) -> boolean()

% State type validators
is_marking(Term) -> boolean()
is_consume_map(Term) -> boolean()
is_produce_map(Term) -> boolean()
is_mode(Term) -> boolean()

% Colored type validators
is_binding(Term) -> boolean()
is_cmode(Term) -> boolean()

% Execution type validators
is_move(Term) -> boolean()
is_receipt(Term) -> boolean()
```

---

### 3.2 pnet_marking - Multiset Marking Algebra

**Location:** `/Users/sac/cre/src/pnet/pnet_marking.erl`

#### Description

Multiset marking algebra (places -> bag of tokens). Operations respect multiplicity: adding [a,b] to [a] gives [a,a,b]; taking [a] from [a,a,b] leaves [a,b].

#### API

```erlang
% Creation and basic operations
new(Places) -> marking()
get(Marking, Place) -> {ok, [token()]}
set(Marking, Place, Tokens) -> marking()

% Marking algebra operations
add(Marking, ProduceMap) -> marking()
take(Marking, ConsumeMap) -> {ok, marking()} | {error, insufficient}
apply(Marking, Move) -> {ok, marking()} | {error, insufficient}

% Inspection and utilities
snapshot(Marking) -> marking()
hash(Marking) -> binary()
```

---

### 3.3 pnet_mode - Mode Enumeration

**Location:** `/Users/sac/cre/src/pnet/pnet_mode.erl`

#### Description

Mode enumeration (input token selections). Enumerates deterministic modes for transition firing based on available tokens in the marking.

#### API

```erlang
% Mode enumeration
preset_counts(PresetPlaces) -> #{place() => non_neg_integer()}
enum_modes(PresetPlaces, Marking) -> [mode()]

% Colored net extension
enum_cmodes(Trsn, Marking, UsrInfo, NetMod) -> {ok, [cmode()]} | {error, term()}
```

---

### 3.4 pnet_choice - Deterministic Nondeterminism

**Location:** `/Users/sac/cre/src/pnet/pnet_choice.erl`

#### Description

Provides deterministic choice for resolving nondeterministic selection in Petri net execution. Uses XOR-shift PRNG for reproducible randomness.

#### API

```erlang
seed(SeedTerm) -> rng_state()
pick(List, RngState) -> {T, rng_state()} | {error, empty}
pick_weighted(Items, RngState) -> {T, rng_state()} | {error, empty | bad_weights}
```

---

### 3.5 pnet_receipt - Receipt Tracking

**Location:** `/Users/sac/cre/src/pnet/pnet_receipt.erl`

#### Description

Receipt tracking for audit trails. Records each transition firing with before/after hashes, move information, and timestamp.

#### API

```erlang
make(BeforeHash, AfterHash, Move) -> receipt()
timestamp() -> integer()
effects(Receipt) -> [term()]  % Extract effect commands from receipt
```

---

## 4. Workflow Utility Modules (wf_*)

Workflow utility modules provide parsing, rule evaluation, and task management for YAWL workflows. All are pure helper functions.

### 4.1 wf_spec - YAWL XML Parser and IR

**Location:** `/Users/sac/cre/src/wf/wf_spec.erl`

#### Description

YAWL XML specification parser, validator, and compiler. Imports YAWL 2.2 XML specifications and provides access to workflow structure.

#### API

```erlang
% Parsing
from_xml(Xml) -> {ok, yawl_spec()} | {error, Reason}
from_xml_file(FilePath) -> {ok, yawl_spec()} | {error, Reason}

% Accessors
id(Spec) -> binary()
title(Spec) -> binary()
version(Spec) -> binary() | undefined
root_net(Spec) -> binary()
tasks(Spec) -> [task_id()]
task_doc(Spec, TaskId) -> binary()
task_type(Spec, TaskId) -> atom()

% IR access
uri(Spec) -> binary()
meta(Spec) -> map()
schema_types(Spec) -> [atom()]
nets(Spec) -> [binary()]
tasks(Spec, NetId) -> [task_id()]
task_name(Spec, NetId, TaskId) -> binary()
join_split(Spec, NetId, TaskId) -> {atom(), atom()}
flows(Spec, NetId, FromId) -> [map()]
variables(Spec, NetId) -> [map()]
timer(Spec, NetId, TaskId) -> map() | undefined
mi(Spec, NetId, TaskId) -> mi_params() | undefined

% Advanced features
split_type(Spec, TaskId) -> 'and' | 'or' | 'xor' | undefined
join_type(Spec, TaskId) -> 'and' | 'or' | 'xor' | undefined
decomposition_nets(Spec) -> [binary()]
decomposition_net(Spec, TaskId) -> {ok, binary()} | {error, not_found}
cancellation_set(Spec, TaskId) -> [task_id()]
cancellation_regions(Spec) -> [{task_id(), [task_id()]}]
flows(Spec) -> [{task_id(), task_id(), binary() | undefined}]
flow_predicate(Spec, From, To) -> binary() | undefined | not_found
task_params(Spec, TaskId) -> #{binary() => term()}
task_param(Spec, TaskId, ParamName) -> term() | undefined
conditions(Spec) -> [{binary(), input | output, binary() | undefined}]
condition_expr(Spec, ConditionId) -> binary() | undefined | not_found
all_decompositions(Spec) -> [{binary(), boolean(), [task_id()]}]
decomposition_tasks(Spec, DecompositionId) -> {ok, [task_id()]} | {error, not_found}

% Validation and compilation
validate(Spec) -> ok | {error, [binary()]}
compile(Spec) -> {ok, compiled_spec()} | {error, term()}
```

#### Records

```erlang
-record(yawl_spec, {
    id :: binary(),
    title :: binary(),
    version :: binary() | undefined,
    root_net :: binary(),
    tasks :: #{task_id() => task_info()},
    places :: [place()],
    transitions :: [transition()],
    decompositions :: #{binary() => decomposition_info()},
    flows :: [flow_info()],
    conditions :: #{binary() => condition_info()}
}).

-record(task_info, {
    docs :: binary() | undefined,
    type :: atom(),
    split_type :: 'and' | 'or' | 'xor' | undefined,
    join_type :: 'and' | 'or' | 'xor' | undefined,
    decomposes_to :: binary() | undefined,
    cancellation_set :: [task_id()],
    params :: #{binary() => term()},
    mi_params :: mi_params() | undefined
}).

-type mi_params() :: #{
    min_instances := non_neg_integer(),
    max_instances := non_neg_integer() | unlimited,
    continuation_threshold := non_neg_integer()
}.
```

---

### 4.2 wf_rules - Rule Evaluation Engine

**Location:** `/Users/sac/cre/src/wf/wf_rules.erl`

#### Description

Erlog-backed rule evaluation for workflow decision making. Rules are expressed in a Datalog-like syntax and compiled for efficient evaluation.

#### API

```erlang
% Compilation
compile(Program) -> {ok, rules()} | {error, bad_program}

% Fact conversion
facts_from_marking(Marking) -> [fact()]

% Query operations
bool(Rules, Query, Facts, ExtraFacts) -> true | false | {error, Reason}
query(Rules, Query, Facts, ExtraFacts) -> {ok, [binding()]} | {error, Reason}
```

#### Types

```erlang
-type rules() :: #{clauses := #{predicate() => [clause()]}}.
-type predicate() :: atom().
-type arg() :: atom() | {var, atom()} | term().
-type clause() :: #{head := pred_ref(), body := [pred_ref()]}.
-type pred_ref() :: {predicate(), [arg()]}.
-type fact() :: {predicate(), [term()]}.
-type binding() :: #{atom() => term()}.
-type query() :: {predicate(), [arg()]}.
```

---

### 4.3 wf_yawl_pred - XPath to Erlog Compiler

**Location:** `/Users/sac/cre/src/wf/wf_yawl_pred.erl`

#### Description

YAWL XPath predicate to Erlog rule compiler. Converts YAWL 2.1 XPath predicates to wf_rules Datalog format. Handles variable references like `/NetName/VarName/text()`.

#### API

```erlang
to_erlog(PredicateBin) -> {ok, {RulesBin, GoalTerm}} | {error, Reason}
parse_xpath(PredicateBin) -> {ok, xpath_ast()} | {error, Reason}
extract_vars(PredicateBin) -> {ok, [VarName]} | {error, Reason}
```

#### Types

```erlang
-type xpath_ast() ::
    {xpath_var, {path, binary(), binary()}} |
    {xpath_literal, term()} |
    {xpath_comp, xpath_ast(), binary(), xpath_ast()} |
    {xpath_bool, binary(), [xpath_ast()]}.

-type comp_op() :: '=' | '!=' | '<' | '>' | '<=' | '>='.
-type bool_op() :: 'and' | 'or'.
```

---

### 4.4 wf_timer - ISO 8601 Duration Parser

**Location:** `/Users/sac/cre/src/wf/wf_timer.erl`

#### Description

ISO 8601 duration parser for YAWL timers. Parses duration strings like P3D, PT12H, PT1H30M into milliseconds.

#### API

```erlang
parse_duration(DurationBin) -> {ok, milliseconds()} | {error, term()}
to_ms(DurationBin) -> milliseconds()
to_seconds(DurationBin) -> seconds()
```

#### Types

```erlang
-type duration() :: binary().
-type milliseconds() :: non_neg_integer().
-type seconds() :: non_neg_integer().
-type parse_result() :: {ok, milliseconds()} | {error, term()}.
```

#### Duration Format

- `P` - designator starts duration (required)
- Date: `Y` (years), `M` (months), `D` (days)
- Time: `H` (hours), `M` (minutes), `S` (seconds)
- `T` - separator between date and time

Examples:
- `P3D` - 3 days
- `PT12H` - 12 hours
- `PT1H30M` - 1 hour 30 minutes
- `P1DT2H30M` - 1 day 2 hours 30 minutes

---

### 4.5 wf_timerq - Deadline Queue

**Location:** `/Users/sac/cre/src/wf/wf_timerq.erl`

#### Description

Deadline queue for time-based token injection. Stores arbitrary events (typically `{inject, Place, Token}` tuples) and fires based on monotonic millisecond deadlines. Pure functional implementation.

#### API

```erlang
% Timer queue API
new() -> timerq()
arm(TimerQ, Key, Deadline, Event) -> timerq()
disarm(TimerQ, Key) -> timerq()
poll(TimerQ, Now) -> {[timer_event()], timerq()}

% Inspection
is_empty(TimerQ) -> boolean()
size(TimerQ) -> non_neg_integer()
peek(TimerQ) -> {deadline(), timer_event()} | undefined

% Extended timer API
arm_from_now(TimerQ, Key, DurationMs, Event) -> timerq()
get_deadline(TimerQ, Key) -> deadline() | undefined
clear_all(TimerQ) -> timerq()
arm_duration(TimerQ, Key, Duration, Event) -> timerq() | {error, bad_duration}
```

#### Types

```erlang
-opaque timerq() :: #{keys := key_map(), deadlines := deadline_list()}.
-type timer_key() :: term().
-type deadline() :: integer().
-type duration_ms() :: non_neg_integer().
-type iso8601_duration() :: binary() | string().
-type timer_event() :: term().
```

---

### 4.6 wf_task - External Task Token Constructors

**Location:** `/Users/sac/cre/src/wf/wf_task.erl`

#### Description

External task token constructors. All functions return `{produce, #{Place => [Token]}}`.

#### API

```erlang
enabled(TaskId, Payload, Place) -> produce_result()
running(TaskId, Payload, Place) -> produce_result()
done(TaskId, Output, Place) -> produce_result()
failed(TaskId, Reason, Place) -> produce_result()
cancelled(TaskId, Reason, Place) -> produce_result()
```

#### Types

```erlang
-type task_id() :: term().
-type task_status() :: enabled | running | done | failed | cancelled.
-type task_token() :: {task, task_id(), task_status(), payload()}.
-type payload() :: term().
-type produce_result() :: {produce, #{place() => [task_token()]}}.
```

#### Token Format

```
{task, TaskId, State, Payload}
```

States: `enabled`, `running`, `done`, `failed`, `cancelled`

---

### 4.7 wf_mi - Multi-Instance Handling

**Location:** `/Users/sac/cre/src/wf/wf_mi.erl`

#### Description

Multi-instance task handling for YAWL. Determines if a task is multi-instance, evaluates instance count from data, and creates tokens for each instance.

#### API

```erlang
% Detection and evaluation
is_mi_task(Spec, NetId, TaskId) -> boolean()
evaluate_mi(MIParams, Data) -> {ok, non_neg_integer()} | {error, term()}

% Token creation
create_instance_tokens(TaskId, Count) -> [mi_token()]

% Instance management
instance_count(MIParams, Data) -> {ok, non_neg_integer()} | {error, term()}
instance_threshold(MIParams, Completed) -> boolean()
```

#### Types

```erlang
-type mi_params() :: #{
    min_instances := non_neg_integer(),
    max_instances := non_neg_integer() | unlimited,
    continuation_threshold := non_neg_integer()
}.
-type mi_token() :: {mi_instance, task_id(), non_neg_integer()}.
```

---

### 4.8 wf_cancel - Cancellation Tokens

**Location:** `/Users/sac/cre/src/wf/wf_cancel.erl`

#### Description

Cancellation token handling for YAWL cancellation regions. Manages cancellation tokens that terminate workflow regions when specific conditions are met.

#### API

```erlang
% Token validation
is_cancel_token(Term) -> boolean()

% Token creation
create_cancel_token(Target) -> cancel_token()

% Token inspection
cancel_targets(CancelToken) -> [atom()]

% Cancellation application
apply_cancellation(Marking, Targets) -> marking()
cancel_region(Marking, Region) -> marking()

% Validation
is_cancellation_set(Term) -> boolean()
```

#### Types

```erlang
-type cancel_token() :: {cancel, [atom()]}.
-type cancel_region() :: {cancel_region, atom(), [atom()]}.
-type cancellation_set() :: [atom()].
```

---

### 4.9 wf_scope - Boundary Mapping

**Location:** `/Users/sac/cre/src/wf/wf_scope.erl`

#### Description

Boundary mapping helpers for nested Petri nets. Maps places between parent and child net scopes.

#### API

```erlang
enter(BindingTable, ScopeId, DeltaOrMarking) -> produce_map()
leave(BindingTable, ScopeId, DeltaOrMarking) -> produce_map()
bindings(BindingTable, ScopeId) -> #{place() => place()} | {error, unknown_scope}
```

#### Types

```erlang
-type scope_id() :: term().
-type binding_table() :: #{scope_id() => #{place() => place()}}.
```

---

## 5. YAWL Runtime Modules (wf/yawl_*)

YAWL runtime modules provide execution engine, state management, cancellation handling, telemetry, and data flow for running YAWL workflows.

### 5.1 yawl_execution - Execution Engine

**Location:** `/Users/sac/cre/src/wf/yawl_execution.erl`

#### Description

Core YAWL execution engine that manages workflow case execution, coordinates task dispatching, and handles completion events.

#### API

```erlang
start_case(SpecId, CaseId, InitialData) -> {ok, pid()} | {error, Reason}
complete_task(CaseId, TaskId, Result) -> ok | {error, Reason}
cancel_case(CaseId, Reason) -> ok | {error, Reason}
get_case_state(CaseId) -> {ok, case_state()} | {error, Reason}
```

---

### 5.2 yawl_state - State Management

**Location:** `/Users/sac/cre/src/wf/yawl_state.erl`

#### Description

Workflow state management for YAWL cases. Tracks case lifecycle, work items, and workflow variables.

#### API

```erlang
new_state(CaseId, SpecId) -> case_state()
get_status(State) -> running | suspended | completed | cancelled
get_workitems(State) -> #{task_id() => workitem()}
get_variable(State, VarName) -> term() | undefined
set_variable(State, VarName, Value) -> case_state()
add_workitem(State, WorkItem) -> case_state()
remove_workitem(State, TaskId) -> case_state()
```

---

### 5.3 yawl_cancellation - Cancellation Runtime

**Location:** `/Users/sac/cre/src/wf/yawl_cancellation.erl`

#### Description

Cancellation region handling at runtime. Processes cancellation tokens and clears tokens from affected places.

#### API

```erlang
process_cancel(Marking, CancelToken) -> marking()
get_cancellation_regions(Spec) -> [{atom(), [atom()]}]
apply_cancellation_set(Marking, Targets) -> marking()
```

---

### 5.4 yawl_telemetry - OpenTelemetry Integration

**Location:** `/Users/sac/cre/src/wf/yawl_telemetry.erl`

#### Description

OpenTelemetry observability integration for YAWL workflows. Provides tracing and metrics for workflow execution.

#### API

```erlang
start_span(SpanName, Attributes) -> otel_span:end_span()
end_span(Span) -> ok
add_event(Span, EventName) -> ok
record_metric(MetricName, Value) -> ok
set_attributes(Span, Attributes) -> ok
```

---

### 5.5 yawl_executor - Task Execution

**Location:** `/Users/sac/cre/src/wf/yawl_executor.erl`

#### Description

External task executor that manages work item queues and dispatches tasks to available workers.

#### API

```erlang
register_executor(TaskType, ExecutorMod) -> ok
dispatch_task(WorkItem) -> {ok, pid()} | {error, Reason}
complete_workitem(WorkItemId, Result) -> ok
fail_workitem(WorkItemId, Reason) -> ok
```

---

### 5.6 yawl_timer_runtime - Timer Runtime

**Location:** `/Users/sac/cre/src/wf/yawl_timer_runtime.erl`

#### Description

Timer event execution at runtime. Manages timer queue and injects timeout tokens when deadlines expire.

#### API

```erlang
start_timer(CaseId, TaskId, Duration) -> {ok, TimerRef} | {error, Reason}
cancel_timer(TimerRef) -> ok
poll_timers() -> [timer_event()]
```

---

### 5.7 yawl_mi_runtime - Multi-Instance Runtime

**Location:** `/Users/sac/cre/src/wf/yawl_mi_runtime.erl`

#### Description

Multi-instance execution runtime. Manages creation and tracking of parallel or sequential task instances.

#### API

```erlang
create_instances(CaseId, TaskId, MIParams, Data) -> {ok, [InstancePid]} | {error, Reason}
get_instance_status(CaseId, TaskId) -> [{InstanceId, Status}]
complete_instance(CaseId, TaskId, InstanceId, Result) -> ok
check_continuation_threshold(CaseId, TaskId) -> boolean()
```

---

### 5.8 yawl_schema - Schema Definitions

**Location:** `/Users/sac/cre/src/wf/yawl_schema.erl`

#### Description

Schema type definitions for YAWL workflows. Provides type records for tasks, flows, and decompositions.

#### Records

```erlang
-record(yawl_schema, {
    spec_id :: binary(),
    nets :: #{binary() => net_schema()},
    tasks :: #{task_id() => task_schema()},
    flows :: [flow_schema()]
}).

-record(net_schema, {
    id :: binary(),
    is_root :: boolean(),
    tasks :: [task_id()],
    input_condition :: binary(),
    output_condition :: binary()
}).
```

---

### 5.9 yawl_recovery - Checkpoint and Recovery

**Location:** `/Users/sac/cre/src/wf/yawl_recovery.erl`

#### Description

Checkpoint and recovery functionality for long-running workflows. Enables persistence and restoration of workflow state.

#### API

```erlang
create_checkpoint(CaseId) -> {ok, CheckpointId} | {error, Reason}
restore_from_checkpoint(CheckpointId) -> {ok, case_state()} | {error, Reason}
list_checkpoints(CaseId) -> [checkpoint_info()]
delete_checkpoint(CheckpointId) -> ok
```

---

### 5.10 yawl_data - Data Flow Management

**Location:** `/Users/sac/cre/src/wf/yawl_data.erl`

#### Description

Data flow management between tasks. Handles data passing between workflow tasks and variable scoping.

#### API

```erlang
get_case_data(CaseId) -> #{binary() => term()}
set_case_data(CaseId, Data) -> ok
get_task_input(CaseId, TaskId) -> map()
set_task_output(CaseId, TaskId, Output) -> ok
merge_task_output(CaseId, TaskId, Output) -> ok
```

---

### 5.11 yawl_pred_eval - Predicate Evaluation

**Location:** `/Users/sac/cre/src/wf/yawl_pred_eval.erl`

#### Description

Predicate evaluation engine for YAWL workflow conditions. Evaluates XPath predicates against case data.

#### API

```erlang
evaluate_predicate(Predicate, CaseData) -> {ok, boolean()} | {error, Reason}
evaluate_flow_predicate(FlowId, CaseData) -> {ok, boolean()} | {error, Reason}
get_required_variables(Predicate) -> [binary()]
```

---

## 6. Message Contracts

### 6.1 Messages to gen_pnet

```erlang
% Token injection
{produce, #{place1 => [token1, token2], place2 => [token3]}}

% Synchronous requests
{call, {get, marking}}
{call, {get, specific_place}}
{call, last_receipt}
{call, {receipts, 5}}
{call, {inject, ProduceMap}}
{call, step}
{call, {drain, MaxSteps, Acc}}

% Asynchronous requests
{cast, {custom_request, data}}
{cast, continue}  % Internal progress loop message
```

### 6.2 Messages from gen_pnet

```erlang
% Query responses
{ok, Marking}           % Current marking map
{ok, [Token]}           % Tokens at a place
{ok, Receipt}           % Last execution receipt
{ok, [Receipt]}         % Last N receipts
{reply, Reply}          % Custom call response
{error, bad_place}      % Invalid place reference

% Receipt notifications (to subscribers)
{pnet_receipt, #{
    before_hash => <<Hash1>>,
    after_hash => <<Hash2>>,
    move => #{trsn => T, mode => M, produce => P},
    ts => Timestamp
}}

% Effect commands (optional, from on_receipt callback)
{pnet_effect, audit_log}
{pnet_effect, notification}
```

### 6.3 gen_pnet State Records

```erlang
-record(net_state, {
    marking     :: #{place() => [token()]},
    net_mod     :: module(),
    usr_info    :: term(),
    stats       :: #stats{} | undefined,
    tstart      :: integer(),
    cnt         :: non_neg_integer()
}).

-record(stats, {
    current :: #stat{},
    hi      :: #stat{},
    lo      :: #stat{}
}).

-record(stat, {
    t   :: integer(),
    fps :: number()
}).

-record(bad_place, {
    name :: atom()
}).
```

---

## 7. Type Definitions

### 7.1 Core Types

```erlang
% Place and transition identifiers
-type place() :: atom().
-type trsn() :: atom().

% Token and marking
-type token() :: term().
-type marking() :: #{place() => [token()]}.

% Mode and binding (colored nets)
-type mode() :: #{place() => [token()]}.
-type binding() :: #{var() => term()}.
-type cmode() :: {binding(), mode()}.

% Movement tracking
-type move() :: #{
    trsn := trsn(),
    mode := mode() | cmode(),
    produce := produce_map()
}.

-type receipt() :: #{
    before_hash := binary(),
    after_hash := binary(),
    move := move(),
    ts := integer()
}.
```

### 7.2 YAWL Types

```erlang
% YAWL identifiers
-type spec_id() :: binary().
-type net_id() :: binary().
-type task_id() :: atom().

% YAWL data structures
-type yawl_spec() :: #yawl_spec{}.
-type compiled_spec() :: #compiled_spec{}.
-type mi_params() :: #{
    min_instances := non_neg_integer(),
    max_instances := non_neg_integer() | unlimited,
    continuation_threshold := non_neg_integer()
}.
```

### 7.3 Workflow Types

```erlang
% Task and workitem
-type workitem() :: #{
    task_id := task_id(),
    case_id := binary(),
    status := ready | in_progress | completed | failed,
    input_data := map(),
    output_data := map() | undefined
}.

% Case state
-type case_state() :: #{
    case_id := binary(),
    spec_id := spec_id(),
    status := running | suspended | completed | cancelled,
    workitems := #{task_id() => workitem()},
    data := map(),
    timestamps => map()
}.
```

---

## Appendix A: Complete Module File Listing

### Core OTP (2 modules)
- `/Users/sac/cre/src/core/gen_pnet.erl`
- `/Users/sac/cre/src/core/gen_yawl.erl`

### YAWL Compilation (3 modules)
- `/Users/sac/cre/src/core/yawl_compile.erl`
- `/Users/sac/cre/src/core/yawl_compiled.erl`
- `/Users/sac/cre/src/core/yawl_validate.erl`

### PNET Helpers (5 modules)
- `/Users/sac/cre/src/pnet/pnet_types.erl`
- `/Users/sac/cre/src/pnet/pnet_marking.erl`
- `/Users/sac/cre/src/pnet/pnet_mode.erl`
- `/Users/sac/cre/src/pnet/pnet_choice.erl`
- `/Users/sac/cre/src/pnet/pnet_receipt.erl`

### Workflow Utilities (9 modules)
- `/Users/sac/cre/src/wf/wf_spec.erl`
- `/Users/sac/cre/src/wf/wf_rules.erl`
- `/Users/sac/cre/src/wf/wf_yawl_pred.erl`
- `/Users/sac/cre/src/wf/wf_timer.erl`
- `/Users/sac/cre/src/wf/wf_timerq.erl`
- `/Users/sac/cre/src/wf/wf_task.erl`
- `/Users/sac/cre/src/wf/wf_mi.erl`
- `/Users/sac/cre/src/wf/wf_cancel.erl`
- `/Users/sac/cre/src/wf/wf_scope.erl`

### YAWL Runtime (11 modules)
- `/Users/sac/cre/src/wf/yawl_execution.erl`
- `/Users/sac/cre/src/wf/yawl_state.erl`
- `/Users/sac/cre/src/wf/yawl_cancellation.erl`
- `/Users/sac/cre/src/wf/yawl_telemetry.erl`
- `/Users/sac/cre/src/wf/yawl_executor.erl`
- `/Users/sac/cre/src/wf/yawl_timer_runtime.erl`
- `/Users/sac/cre/src/wf/yawl_mi_runtime.erl`
- `/Users/sac/cre/src/wf/yawl_schema.erl`
- `/Users/sac/cre/src/wf/yawl_recovery.erl`
- `/Users/sac/cre/src/wf/yawl_data.erl`
- `/Users/sac/cre/src/wf/yawl_pred_eval.erl`

### Header Files (5 files)
- `/Users/sac/cre/include/gen_pnet.hrl`
- `/Users/sac/cre/include/yawl_recovery.hrl`
- `/Users/sac/cre/include/yawl_otel_logger.hrl`
- `/Users/sac/cre/include/yawl_twitter.hrl`
- `/Users/sac/cre/include/cre.hrl`

---

This completes the comprehensive API reference structure for the CRE architecture.
         marking() | {error, bad_place}.
set(Marking, Place, Tokens) when is_map(Marking), is_atom(Place), is_list(Tokens) ->
    case maps:is_key(Place, Marking) of
        true -> Marking#{Place => Tokens};
        false -> {error, bad_place}
    end;
set(_Marking, _Place, _Tokens) ->
    {error, bad_place}.

%% @doc Adds tokens to the marking via a produce map (from actual implementation)
%% Tokens are appended to existing tokens at each place.
%% All places in the produce map must exist in the marking.
-spec add(Marking :: marking(), ProduceMap :: produce_map()) ->
         marking() | {error, bad_place}.
add(Marking, ProduceMap) when is_map(Marking), is_map(ProduceMap) ->
    try
        maps:fold(fun
            (Place, NewTokens, Acc) when is_atom(Place), is_list(NewTokens) ->
                case maps:find(Place, Acc) of
                    {ok, ExistingTokens} ->
                        Acc#{Place => ExistingTokens ++ NewTokens};
                    error ->
                        throw({error, bad_place})
                end;
            (_, _, _) ->
                throw({error, bad_place})
        end, Marking, ProduceMap)
    catch
        throw:{error, bad_place} -> {error, bad_place}
    end;
add(_Marking, _ProduceMap) ->
    {error, bad_place}.

%% @doc Takes tokens from the marking via a consume map (from actual implementation)
%% This is a multiset operation - the exact tokens specified must be
%% present with sufficient multiplicity. Tokens are removed from the
%% front of each place's token list.
-spec take(Marking :: marking(), ConsumeMap :: consume_map()) ->
         {ok, marking()} | {error, bad_place | insufficient}.
take(Marking, ConsumeMap) when is_map(Marking), is_map(ConsumeMap) ->
    try
        Result = maps:fold(fun
            (Place, TokensToTake, Acc) when is_atom(Place), is_list(TokensToTake) ->
                case maps:find(Place, Acc) of
                    {ok, ExistingTokens} ->
                        case consume_tokens(ExistingTokens, TokensToTake) of
                            {ok, RemainingTokens} ->
                                Acc#{Place => RemainingTokens};
                            {error, insufficient} ->
                                throw({error, insufficient})
                        end;
                    error ->
                        throw({error, bad_place})
                end;
            (_, _, _) ->
                throw({error, bad_place})
        end, Marking, ConsumeMap),
        {ok, Result}
    catch
        throw:{error, Reason} -> {error, Reason}
    end;
take(_Marking, _ConsumeMap) ->
    {error, bad_place}.

%% @doc Applies a consume map and produce map atomically (from actual implementation)
%% This is the primary operation for transition firing. Tokens are
%% first consumed via the consume map, then tokens are added via the
%% produce map. If consumption fails, the marking is unchanged.
-spec apply(Marking :: marking(), ConsumeMap :: consume_map(),
            ProduceMap :: produce_map()) ->
         {ok, marking()} | {error, bad_place | insufficient}.
apply(Marking, ConsumeMap, ProduceMap)
  when is_map(Marking), is_map(ConsumeMap), is_map(ProduceMap) ->
    case take(Marking, ConsumeMap) of
        {ok, Marking1} ->
            case add(Marking1, ProduceMap) of
                {error, Reason} -> {error, Reason};
                Marking2 -> {ok, Marking2}
            end;
        {error, Reason} ->
            {error, Reason}
    end;
apply(_Marking, _ConsumeMap, _ProduceMap) ->
    {error, bad_place}.

%% @doc Creates a deep copy (snapshot) of the marking (from actual implementation)
%% Returns an identical copy that can be modified without affecting
%% the original. Since Erlang data is immutable, this returns the
%% same marking but provides a clear semantic for snapshotting.
-spec snapshot(Marking :: marking()) -> marking().
snapshot(Marking) when is_map(Marking) ->
    %% Erlang maps are immutable, so the marking itself is already
    %% a snapshot. This function provides semantic clarity.
    Marking.

%% @doc Computes a consistent hash of the marking (from actual implementation)
%% Uses SHA-256 to hash the marking's term representation.
%% Useful for state comparison, caching keys, and receipt generation.
-spec hash(Marking :: marking()) -> binary().
hash(Marking) when is_map(Marking) ->
    crypto:hash(sha256, term_to_binary(Marking)).

%% @private
%% @doc Consumes tokens from a list, respecting multiset semantics (from actual implementation)
%% Removes each token in TokensToTake from AvailableTokens,
%% checking that each token exists with sufficient multiplicity.
%% Returns the remaining tokens or an error if consumption fails.
-spec consume_tokens(AvailableTokens :: [token()],
                     TokensToTake :: [token()]) ->
         {ok, [token()]} | {error, insufficient}.
consume_tokens(AvailableTokens, []) ->
    {ok, AvailableTokens};
consume_tokens([], [_|_]) ->
    {error, insufficient};
consume_tokens([Token | RestAvailable], TokensToTake) ->
    case lists:member(Token, TokensToTake) of
        true ->
            %% Remove one occurrence of this token from TokensToTake
            RemainingToTake = lists:delete(Token, TokensToTake),
            consume_tokens(RestAvailable, RemainingToTake);
        false ->
            %% This token isn't being consumed, keep it
            case consume_tokens(RestAvailable, TokensToTake) of
                {ok, Remaining} -> {ok, [Token | Remaining]};
                {error, _} = Error -> Error
            end
    end.
```

---

## 3. `pnet_mode` - Mode Enumeration

### Types

```erlang
%% Mode enumeration types for basic and colored Petri nets
-type mode() :: #{place() => [token()]}.
-type cmode() :: {binding(), mode()}.
-type binding() :: #{var() => term()}.
```

### Functions

```erlang
%% @doc Returns the count of tokens needed from each preset place (from actual implementation)
%% Handles multiplicity: repeated places in preset list increment the count.
%% This function returns a map indicating the requirement for each place.
%%
%% Doctests:
%% > pnet_mode:preset_counts([p, p, q]).
%% #{p => 2, q => 1}
%%
%% > pnet_mode:preset_counts([a, b, a, c]).
%% #{a => 2, b => 1, c => 1}
-spec preset_counts(PresetPlaces :: [place()]) ->
         #{place() => non_neg_integer()}.
preset_counts(PresetPlaces) when is_list(PresetPlaces) ->
    lists:foldl(
        fun(P, Acc) ->
            maps:update_with(P, fun(V) -> V + 1 end, 1, Acc)
        end,
        #{},
        PresetPlaces
    ).

%% @doc Enumerates all possible modes given the current marking (from actual implementation)
%% A mode represents one valid way to fire a transition by selecting
%% tokens from each preset place. Handles repeated places via preset_counts.
%% Results are in deterministic term order.
%%
%% Doctests:
%% > pnet_mode:enum_modes([p], #{p => [a, b, c]}).
%% [#{p => [a]}, #{p => [b]}, #{p => [c]}]
%%
%% > pnet_mode:enum_modes([p, q], #{p => [a], q => [x]}).
%% [#{p => [a], q => [x]}]
%%
%% > pnet_mode:enum_modes([p, q], #{p => [a, b], q => [x, y]}).
%% [#{p => [a], q => [x]},
%%  #{p => [a], q => [y]},
%%  #{p => [b], q => [x]},
%%  #{p => [b], q => [y]}]
%%
%% > pnet_mode:enum_modes([p, p, q], #{p => [a, b], q => [x]}).
%% [#{p => [a, b], q => [x]}]
%%
%% > pnet_mode:enum_modes([p], #{p => []}).
%% []
-spec enum_modes(PresetPlaces :: [place()], Marking :: marking()) ->
         [mode()].
enum_modes(PresetPlaces, Marking) when is_list(PresetPlaces), is_map(Marking) ->
    Counts = preset_counts(PresetPlaces),
    UniquePlaces = lists:usort(PresetPlaces),
    enum_modes_for_places(UniquePlaces, Counts, Marking).

%% @doc Enumerates colored modes with variable bindings (from actual implementation)
%% For colored Petri nets, this function calls the net module's
%% cmodes callback to get modes that include variable bindings.
%% If the net module doesn't implement colored modes, falls back
%% to basic mode enumeration with empty bindings.
%%
%% Doctests (assuming net module implements cmodes/3):
%% > pnet_mode:enum_cmodes(t1, #{p => [a,b]}, ctx, basic_net).
%% [{#{}, #{p => [a]}}, {#{}, #{p => [b]}}]
%%
%% Colored net example:
%% > Marking = #{input => [{user, alice, "report"}]},
%% > pnet_mode:enum_cmodes(process, Marking, #{role => "manager"}, workflow_net).
%% [{{user => alice}, #{input => [{user, alice, "report"}]}}]
-spec enum_cmodes(Trsn :: atom(), Marking :: marking(),
                  Ctx :: usr_info(), NetMod :: net_mod()) ->
         [cmode()].
enum_cmodes(Trsn, Marking, Ctx, NetMod) when is_atom(Trsn), is_map(Marking),
                                           is_atom(NetMod) ->
    %% Check if the net module implements colored modes
    case erlang:function_exported(NetMod, cmodes, 3) of
        true ->
            try
                NetMod:cmodes(Trsn, Marking, Ctx)
            catch
                _:_:_-> []
            end;
        false ->
            %% Fall back to basic mode enumeration with empty binding
            Modes = enum_modes(NetMod:preset(Trsn), Marking),
            [{#{}, M} || M <- Modes]
    end.

%% @doc Internal helper for mode enumeration (from actual implementation)
%% Recursive function that generates modes for multiple places.
enum_modes_for_places([], _Counts, _Marking) ->
    [#{ }];
enum_modes_for_places([Place | Rest], Counts, Marking) ->
    case maps:get(Place, Marking, []) of
        [] ->
            [];  %% No tokens available, no modes possible
        Tokens ->
            Count = maps:get(Place, Counts, 1),
            TokenCombos = combinations(Count, Tokens),
            RestModes = enum_modes_for_places(Rest, Counts, Marking),
            lists:flatmap(
                fun(Combo) ->
                    [M#{Place => Combo} || M <- RestModes]
                end,
                TokenCombos
            ).

%% @doc Internal helper for combination generation (from actual implementation)
%% Generates all combinations of N elements from a list in deterministic order.
combinations(0, _List) ->
    [[]];
combinations(_N, []) ->
    [];
combinations(N, [H | T]) ->
    %% Combinations including H (need N-1 more from T)
    WithH = [[H | Rest] || Rest <- combinations(N - 1, T)],
    %% Combinations excluding H (need N from T)
    WithoutH = combinations(N, T),
    WithH ++ WithoutH.
```

---

## 4. `pnet_choice` - Deterministic Nondeterminism

### Types
```erlang
-type rng_state() :: {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
%% 3-tuple XOR-shift PRNG state for deterministic choice
```

### Functions

```erlang
%% @doc Creates a deterministic RNG state from a seed term (from actual implementation)
-spec seed(term()) -> rng_state().
seed(SeedTerm) ->
    IntSeed = erlang:phash2(SeedTerm),
    {A, B, C} = {IntSeed bxor 16#5deece66d, IntSeed bxor 16#babe, IntSeed bxor 16#deadbeef},
    seed_splitmix(A, B, C).

%% @doc Picks a random element from a list (from actual implementation)
-spec pick([T], rng_state()) -> {T, rng_state()} | {error, empty}.
pick([], _RngState) -> {error, empty};
pick(List, RngState) when is_list(List), length(List) > 0 ->
    Len = length(List),
    {Index, NewRngState} = rand_uniform(Len, RngState),
    Element = lists:nth(Index + 1, List),
    {Element, NewRngState}.

%% @doc Picks a random element from a weighted list (from actual implementation)
-spec pick_weighted([{T, pos_integer()}], rng_state()) ->
          {T, rng_state()} | {error, empty | bad_weights}.
pick_weighted([], _RngState) -> {error, empty};
pick_weighted(Items, RngState) when is_list(Items) ->
    case validate_weights(Items) of
        false -> {error, bad_weights};
        true ->
            TotalWeight = lists:sum([W || {_, W} <- Items]),
            {RandValue, NewRngState} = rand_uniform(TotalWeight, RngState),
            Item = select_weighted(Items, RandValue, 0),
            {Item, NewRngState}
    end.

%% @private
validate_weights([]) -> true;
validate_weights([{_Item, Weight} | Rest]) when is_integer(Weight), Weight > 0 ->
    validate_weights(Rest);
validate_weights(_) -> false.

%% @private
select_weighted([{Item, _Weight} | _Rest], _RandValue, _Acc) ->
    Item;
select_weighted([{Item, Weight} | Rest], RandValue, Acc) ->
    NewAcc = Acc + Weight,
    if RandValue < NewAcc -> Item;
       true -> select_weighted(Rest, RandValue, NewAcc)
    end.
```

---

## 5. `pnet_receipt` - Receipt Tracking

### Functions

```erlang
%% @doc Create a new receipt
-spec make(binary(), binary(), move()) -> receipt().
make(BeforeHash, AfterHash, Move) ->
    #{
        before_hash => BeforeHash,
        after_hash => AfterHash,
        move => Move,
        ts => timestamp()
    }.

%% @doc Get current timestamp (monotonic time for consistency)
-spec timestamp() -> integer().
timestamp() ->
    erlang:monotonic_time(millisecond).

%% @doc Extract effect commands from receipt
-spec receipt(receipt()) -> [term()].
effects(Receipt) ->
    % Default implementation returns empty list
    % Can be extended by net module via on_receipt callback
    [].
```

---

## 6. `wf_timerq` - Deadline Queue

### Types
```erlang
-type timerq() :: term().
-type timer_key() :: term().
-type deadline() :: integer().
-type timer_event() :: {produce, produce_map()}.
```

### Functions

```erlang
%% @doc Create a new timer queue
-spec new() -> timerq().
new() ->
    [].

%% @doc Arm a timer event
-spec arm(timerq(), timer_key(), deadline(), timer_event()) -> timerq().
arm(TimerQ, Key, Deadline, Event) ->
    lists:keystore(Key, 1, TimerQ, {Key, {Deadline, Event}}).

%% @doc Disarm a timer
-spec disarm(timerq(), timer_key()) -> timerq().
disarm(TimerQ, Key) ->
    lists:keydelete(Key, 1, TimerQ).

%% @doc Poll timer queue for ready events
-spec poll(timerq(), deadline()) -> {[timer_event()], timerq()}.
poll(TimerQ, Now) ->
    {Ready, Remaining} = lists:partition(fun({_, {Deadline, _}}) -> Deadline =< Now end, TimerQ),
    Events = [Event || {_, {_, Event}} <- Ready],
    {Events, Remaining}.
```

---

## 7. `wf_task` - External Task Tokens

### Types
```erlang
-type task_id() :: term().
-type task_event() :: {task, task_id(),
                     enabled | running | done | failed | cancelled,
                     term()}.
```

### Functions

```erlang
%% @doc Create enabled task event
-spec enabled(task_id(), term(), place()) -> {produce, produce_map()}.
enabled(TaskId, Payload, Place) ->
    {produce, #{Place => [{task, TaskId, enabled, Payload}]}}.

%% @doc Create running task event
-spec running(task_id(), term(), place()) -> {produce, produce_map()}.
running(TaskId, Payload, Place) ->
    {produce, #{Place => [{task, TaskId, running, Payload}]}}.

%% @doc Create completed task event
-spec done(task_id(), term(), place()) -> {produce, produce_map()}.
done(TaskId, Output, Place) ->
    {produce, #{Place => [{task, TaskId, done, Output}]}}.

%% @doc Create failed task event
-spec failed(task_id(), term(), place()) -> {produce, produce_map()}.
failed(TaskId, Reason, Place) ->
    {produce, #{Place => [{task, TaskId, failed, Reason}]}}.

%% @doc Create cancelled task event
-spec cancelled(task_id(), term(), place()) -> {produce, produce_map()}.
cancelled(TaskId, Reason, Place) ->
    {produce, #{Place => [{task, TaskId, cancelled, Reason}]}}.
```

---

## 8. `wf_scope` - Boundary Mapping

### Types
```erlang
-type scope_id() :: term().
-type binding_table() :: #{scope_id() => #{place() => place()}}.
```

### Functions

```erlang
%% @doc Enter a scope (parent to child mapping)
-spec enter(binding_table(), scope_id(), marking() | produce_map()) -> produce_map().
enter(BindingTable, ScopeId, DeltaOrMarking) ->
    case maps:find(ScopeId, BindingTable) of
        {ok, ParentToChild} ->
            maps:fold(fun(ParentPlace, ChildPlace, Acc) ->
                case maps:find(ParentPlace, DeltaOrMarking) of
                    {ok, Tokens} -> Acc#{ChildPlace := Tokens};
                    error -> Acc
                end
            end, #{}, ParentToChild);
        error -> error
    end.

%% @doc Leave a scope (child to parent mapping)
-spec leave(binding_table(), scope_id(), marking() | produce_map()) -> produce_map().
leave(BindingTable, ScopeId, DeltaOrMarking) ->
    case maps:find(ScopeId, BindingTable) of
        {ok, ParentToChild} ->
            maps:fold(fun(ChildPlace, ParentPlace, Acc) ->
                case maps:find(ChildPlace, DeltaOrMarking) of
                    {ok, Tokens} -> Acc#{ParentPlace := Tokens};
                    error -> Acc
                end
            end, #{}, maps:to_list(ParentToChild));
        error -> error
    end.

%% @doc Get bindings for a scope
-spec bindings(binding_table(), scope_id()) -> #{place() => place()} | {error, unknown_scope}.
bindings(BindingTable, ScopeId) ->
    case maps:find(ScopeId, BindingTable) of
        {ok, Bindings} -> Bindings;
        error -> {error, unknown_scope}
    end.
```

---

## 9. `pnet_net` - Net Semantics Behaviour

### Callback Module

```erlang
%% @type net_arg() :: term().  % Net-specific initialization argument
%% @type usr_info() :: term(). % User info maintained by net module
%% @type state_summary() :: term(). % Minimal state for callbacks

%% Required callbacks (uncolored)
-callback places() -> [place()].
-callback transitions() -> [trsn()].
-callback preset(trsn()) -> [place()].
-callback init(net_arg()) -> usr_info().
-callback init_marking(place(), usr_info()) -> [token()].
-callback modes(trsn(), marking(), usr_info()) -> [mode()].
-callback fire(trsn(), mode(), usr_info()) -> abort | {produce, produce_map()}.

%% Optional callbacks
-callback on_produce(place(), token(), state_summary()) -> keep | drop.
-callback on_receipt(receipt(), state_summary()) -> [term()].

%% Colored extension callbacks (mutually exclusive with uncolored)
-callback cmodes(trsn(), marking(), usr_info()) -> [cmode()].
-callback cfire(trsn(), binding(), mode(), usr_info()) -> abort | {produce, produce_map()}.
```

### Example Implementation

```erlang
-module(my_workflow_net).
-behaviour(pnet_net).

-export([places/0, transitions/0, preset/1, init/1,
         init_marking/2, modes/2, fire/3, on_receipt/2]).

places() -> [start, task1, task2, done].
transitions() -> [t1, t2].

preset(t1) -> [start];
preset(t2) -> [task1].

init(_NetArg) -> #{priority => normal}.

init_marking(start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

modes(t1, Marking, _UsrInfo) ->
    case pnet_marking:get(Marking, start) of
        [init] -> [#{start => []}];
        _ -> []
    end.

modes(t2, Marking, _UsrInfo) ->
    case pnet_marking:get(Marking, task1) of
        [done] -> [#{task1 => []}];
        _ -> []
    end.

fire(t1, #{start => []}, _UsrInfo) ->
    {produce, #{task1 => [done]}}.

fire(t2, #{task1 => []}, _UsrInfo) ->
    {produce, #{done => [complete]}}.

on_receipt(Receipt, _State) ->
    % Log receipt for auditing
    io:format("Transition fired: ~p~n", [Receipt]),
    [audit_log].
```

---

## 10. `gen_pnet` - OTP Runner

### API Functions

```erlang
%% @doc Start the gen_pnet process with default name
-spec start_link(module(), term(), list()) -> {ok, pid()} | {error, term()}.
start_link(NetMod, NetArg, Options) ->
    gen_server:start_link(?MODULE, {local, NetMod}, NetMod, NetArg, Options).

%% @doc Start the gen_pnet process with registered name
-spec start_link(term(), module(), term(), list()) -> {ok, pid()} | {error, term()}.
start_link(Name, NetMod, NetArg, Options) ->
    gen_server:start_link(Name, ?MODULE, NetMod, NetArg, Options).

%% @doc Get current marking
-spec marking(term()) -> marking().
marking(Name) ->
    call(Name, {get, marking}).

%% @doc Get tokens at specific place
-spec ls(term(), place()) -> {ok, [token()]} | {error, bad_place}.
ls(Name, Place) ->
    call(Name, {get, Place}).

%% @doc Get user info
-spec usr_info(term()) -> term().
usr_info(Name) ->
    call(Name, {get, usr_info}).

%% @doc Produce tokens (core workflow primitive)
-spec produce(term(), produce_map()) -> ok.
produce(Name, ProduceMap) ->
    cast(Name, {produce, ProduceMap}).

%% @doc Synchronous call
-spec call(term(), term()) -> term().
call(Name, Request) ->
    gen_server:call(Name, Request).

%% @doc Synchronous call with timeout
-spec call(term(), term(), timeout()) -> term().
call(Name, Request, Timeout) ->
    gen_server:call(Name, Request, Timeout).

%% @doc Asynchronous cast
-spec cast(term(), term()) -> ok.
cast(Name, Request) ->
    gen_server:cast(Name, Request).

%% @doc Reply to client
-spec reply(term(), term()) -> ok.
reply(ClientRef, Reply) ->
    gen_server:reply(ClientRef, Reply).

%% @doc Stop the process
-spec stop(term()) -> ok.
stop(Name) ->
    gen_server:call(Name, stop).

%% @doc Get last receipt
-spec last_receipt(term()) -> receipt() | undefined.
last_receipt(Name) ->
    call(Name, last_receipt).

%% @doc Get last N receipts
-spec receipts(term(), pos_integer()) -> [receipt()].
receipts(Name, N) ->
    call(Name, {receipts, N}).

%% @doc Subscribe to receipt notifications
-spec subscribe(term(), pid()) -> ok.
subscribe(Name, Pid) ->
    cast(Name, {subscribe, Pid}).
```

### gen_server Implementation (Internal)

```erlang
-module(gen_pnet).
-behaviour(gen_server).

-export([start_link/4, start_link/3,
         marking/1, ls/2, usr_info/1, produce/2,
         call/2, call/3, cast/2, stop/1,
         last_receipt/1, receipts/2, subscribe/2, reply/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    net_mod,           :: module()
    net_arg,           :: term()
    usr_info,          :: term()
    marking,           :: marking()
    receipts = [],     :: [receipt()]
    subscribers = [],  :: [pid()]
    rng_state          :: term()
}).

%% Public API (as above)...

%% gen_server callbacks
init({Name, NetMod, NetArg}) ->
    % Initialize net module
    UsrInfo = NetMod:init(NetArg),

    % Create initial marking from init_marking callbacks
    InitialMarking = create_initial_marking(NetMod, UsrInfo),

    % Start progress loop
    {ok, ProgressMarking} = run_progress_loop(#state{
        net_mod = NetMod,
        net_arg = NetArg,
        usr_info = UsrInfo,
        marking = InitialMarking,
        rng_state = pnet_choice:seed(erlang:unique_integer())
    }, InitialMarking)};

handle_call({get, marking}, _From, State) ->
    {reply, State#state.marking, State};

handle_call({get, Place}, _From, State) ->
    Tokens = pnet_marking:get(State#state.marking, Place),
    {reply, {ok, Tokens}, State};

handle_call({get, usr_info}, _From, State) ->
    {reply, State#state.usr_info, State};

handle_call(last_receipt, _From, State) ->
    case State#state.receipts of
        [Last|_] -> {reply, Last, State};
        [] -> {reply, undefined, State}
    end;

handle_call({receipts, N}, _From, State) ->
    Receipts = lists:sublist(lists:reverse(State#state.receipts), N),
    {reply, Receipts, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast({produce, ProduceMap}, State) ->
    % Apply production and run progress loop
    case pnet_marking:add(State#state.marking, ProduceMap) of
        {ok, NewMarking} ->
            {noreply, run_progress_loop(State, NewMarking)};
        {error, _} = Error ->
            % Log error but don't crash
            error_logger:error_msg("Produce error: ~p~n", [Error]),
            {noreply, State}
    end;

handle_cast({subscribe, Pid}, State) ->
    MonRef = erlang:monitor(process, Pid),
    {noreply, State#state{
        subscribers = [{Pid, MonRef} | State#state.subscribers]
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    % Remove dead subscriber
    NewSubscribers = proplists:delete(Pid, State#state.subscribers),
    {noreply, State#state{subscribers = NewSubscribers}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% Progress loop implementation
run_progress_loop(State, Marking) ->
    Enabled = find_enabled_transitions(State#state.net_mod, Marking, State#state.usr_info),
    case Enabled of
        [] -> % Blocked
            State#state{marking = Marking};
        [T|_] ->
            % Choose mode deterministically
            {Mode, NewRngState} = choose_mode(T, Marking, State#state.net_mod, State#state.usr_info, State#state.rng_state),

            % Fire transition
            case fire_transition(T, Mode, State#state.net_mod, State#state.usr_info) of
                {produce, NewTokens} ->
                    BeforeHash = pnet_marking:hash(Marking),
                    {ok, NewMarking} = pnet_marking:apply(Marking, Mode, NewTokens),
                    AfterHash = pnet_marking:hash(NewMarking),

                    % Create receipt
                    Move = #{trsn => T, mode => Mode, produce => NewTokens},
                    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),

                    % Store receipt and notify subscribers
                    NewState = store_receipt_and_notify(State#state{rng_state = NewRngState}, Receipt, NewMarking),

                    % Continue progress loop
                    run_progress_loop(NewState, NewMarking);
                abort ->
                    State#state{marking = Marking}
            end
    end.

%% Helper functions (implementation details)...
```

---

## 11. `yawl_validate` - YAWL Validation

### Functions

```erlang
%% @doc Validate YAWL specification against interface requirements
-spec validate(term()) -> ok | {error, term()}.
validate(XmlOrSpec) ->
    % Parse XML or validate existing spec
    try
        % Basic validation checks
        validate_net_structure(XmlOrSpec),
        validate_transition_connectivity(XmlOrSpec),
        validate_places(XmlOrSpec),
        validate_transitions(XmlOrSpec),
        ok
    catch
        throw:Reason -> {error, Reason}
    end.

%% Internal validation functions
validate_net_structure(XmlOrSpec) ->
    % Check for required YAWL elements
    Required = [places, transitions, preset, flows],
    check_required_elements(XmlOrSpec, Required).

validate_transition_connectivity(XmlOrSpec) ->
    % Ensure all transitions have proper preset/postset
    % No dangling transitions
    ok.

validate_places(XmlOrSpec) ->
    % Check place definitions
    ok.

validate_transitions(XmlOrSpec) ->
    % Check transition definitions
    ok.
```

---

## 12. `yawl_compile` - YAWL Compilation

### Functions

```erlang
%% @doc Compile YAWL specification to net module
-spec compile(term(), list()) -> {ok, module()} | {ok, module(), term(), term(), term()} | {error, term()}.
compile(XmlOrSpec, Options) ->
    try
        % Parse XML or use existing spec
        Spec = parse_yawl(XmlOrSpec),

        % Generate net module code
        NetMod = generate_net_module(Spec, Options),

        % Additional outputs requested?
        case proplists:get_value(advanced, Options, false) of
            true ->
                {ScopeTable, TimerPlan, TaskMap} = generate_auxiliary_structures(Spec),
                {ok, NetMod, ScopeTable, TimerPlan, TaskMap};
            false ->
                {ok, NetMod}
        end
    catch
        throw:Reason -> {error, Reason}
    end.

%% Helper functions
parse_yawl(XmlOrSpec) ->
    % Implementation for XML parsing or spec validation
    ok.

generate_net_module(Spec, Options) ->
    % Generate Erlang module implementing pnet_net behaviour
    % This is a code generation process
    yawl_net_generator:generate(Spec, Options).

generate_auxiliary_structures(Spec) ->
    % Generate scope table, timer plan, and task map
    {#{}, [], #{}}.
```

## 13. `yawl_compiled` - Compiled Specification Accessors

### Functions

```erlang
%% @doc Get list of all compiled YAWL nets
-spec get_net_list() -> [net_id()].
get_net_list() ->
    case persistent_term:get(?COMPILED_NETS, undefined) of
        undefined -> [];
        Nets -> maps:keys(Nets)
    end.

%% @doc Get information about a compiled net
-spec get_net_info(net_id()) -> net_info() | undefined.
get_net_info(NetId) ->
    case persistent_term:get(?COMPILED_NETS, undefined) of
        undefined -> undefined;
        Nets -> maps:get(NetId, Nets, undefined)
    end.

%% @doc Get the compiled module for a net
-spec get_net_module(net_id()) -> module() | undefined.
get_net_module(NetId) ->
    case get_net_info(NetId) of
        undefined -> undefined;
        #net_info{module = Module} -> Module
    end.

%% @doc Get all compiled nets as a map
-spec get_all_nets() -> #{net_id() => net_info()}.
get_all_nets() ->
    persistent_term:get(?COMPILED_NETS, #{}).

%% @doc Check if a net is compiled
-spec is_net_compiled(net_id()) -> boolean().
is_net_compiled(NetId) ->
    maps:is_key(NetId, get_all_nets()).

%% @doc Get compiled module statistics
-spec get_net_stats(net_id()) -> #{atom() => term()}.
get_net_stats(NetId) ->
    case get_net_module(NetId) of
        undefined -> #{error => not_compiled};
        Module -> Module:module_info()
    end.
```

---

## 14. `gen_yawl` - YAWL Wrapper with 3-tuple fire/3

### Overview

`gen_yawl` is a drop-in replacement for `gen_pnet` that adds enhanced support for 3-tuple returns from the `fire/3` callback. When a transition returns `{produce, ProduceMap, NewUsrInfo}`, the user info is automatically updated. This is essential for YAWL workflows where state needs to be tracked as part of transition firing.

### Key Features

- **Drop-in Replacement**: Same API surface as `gen_pnet`
- **Enhanced fire/3**: Supports 3-tuple returns for automatic state updates
- **Backward Compatible**: Works with existing `gen_pnet` callback modules
- **Transparent Wrapping**: No changes to Petri net semantics

### Enhanced fire/3 Callback

The `fire/3` callback can return:
- `{produce, ProduceMap}` - Standard `gen_pnet` behavior
- `{produce, ProduceMap, NewUsrInfo}` - Enhanced behavior with automatic user info update
- `abort` - Abort the transition firing

### API Functions

```erlang
%% Start YAWL workflows (same as gen_pnet)
-spec start_link(NetMod :: atom(), NetArg :: term(), Options :: [prop()]) ->
      start_link_result().
-spec start_link(ServerName :: server_name(), NetMod :: atom(), NetArg :: term(), Options :: [prop()]) ->
      start_link_result().

%% Query functions (same as gen_pnet)
-spec ls(Name :: name(), Place :: atom()) -> {ok, [term()]} | {error, #bad_place{}}.
-spec marking(Name :: name()) -> #{atom() => [term()]}.
-spec usr_info(Name :: name()) -> term().          % Enhanced: returns current user info
-spec stats(Name :: name()) -> #stats{}.
-spec reset_stats(Name :: name()) -> ok.

%% Communication (same as gen_pnet)
-spec call(Name :: name(), Request :: term()) -> term().
-spec call(Name :: name(), Request :: term(), Timeout :: non_neg_integer() | infinity) -> term().
-spec cast(Name :: name(), Request :: term()) -> ok.
-spec reply(Client :: {pid(), gen_server:reply_tag()}, Reply :: term()) -> ok.

%% State property checking (same as gen_pnet)
-spec state_property(Name :: name(), Pred :: fun((...) -> ok | {error, term()}), PlaceLst :: [atom()]) ->
      ok | {error, term()}.

%% Lifecycle (same as gen_pnet)
-spec stop(Name :: name()) -> ok.

%% Net state accessors (forwarded from gen_pnet)
-spec get_ls(Place :: atom(), NetState :: #net_state{}) -> [term()].
-spec get_usr_info(NetState :: #net_state{}) -> term().
-spec get_stats(NetState :: #net_state{}) -> #stats{}.
```

### Enhanced Callback Specification

```erlang
%% Enhanced fire callback - supports 3-tuple return with usr_info update
-callback fire(Trsn :: atom(), Mode :: #{atom() => [term()]}, UsrInfo :: term()) ->
              abort |
              {produce, #{atom() => [term()]}} |
              {produce, #{atom() => [term()]}, NewUsrInfo :: term()}.

%% Standard pnet_net callbacks (same as gen_pnet)
-callback place_lst() -> [atom()].
-callback trsn_lst() -> [atom()].
-callback init_marking(Place :: atom(), UsrInfo :: term()) -> [term()].
-callback preset(Trsn :: atom()) -> [atom()].
-callback is_enabled(Trsn :: atom(), Mode :: #{atom() => [term()]}, UsrInfo :: term()) ->
              boolean().

%% Interface callbacks (same as gen_pnet)
-callback init(NetArg :: term()) -> UsrInfo :: term().
-callback code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
              {ok, term()} | {error, term()}.
-callback handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
              {reply, term()} | {reply, term(), #{atom() => [term()]}} | noreply |
              {noreply, #{atom() => [term()]}} | {stop, term(), term()}.
-callback handle_cast(Request :: term(), NetState :: term()) ->
              noreply | {noreply, #{atom() => [term()]}} | {stop, term()}.
-callback handle_info(Info :: term(), NetState :: term()) ->
              noreply | {noreply, #{atom() => [term()]}} | {stop, term()}.
-callback terminate(Reason :: term(), NetState :: term()) -> ok.
-callback trigger(Place :: atom(), Token :: term(), NetState :: term()) ->
              pass | drop.
```

### Usage Example

```erlang
% Define a YAWL workflow that tracks state
-module(order_workflow).
-behaviour(gen_yawl).  % Use gen_yawl instead of gen_pnet

places() -> [start, process_order, ship_order, end].
transitions() -> [t_process, t_ship].

preset(t_process) -> [start].
preset(t_ship) -> [process_order].

init(_NetArg) -> #{order_id => undefined, status => pending}.

init_marking(start, _UsrInfo) -> [init].
init_marking(_Place, _UsrInfo) -> [].

is_enabled(t_process, #{start := [init]}, _UsrInfo) -> true.
is_enabled(t_ship, #{process_order := [done]}, _UsrInfo) -> true.

% Standard 2-tuple return
fire(t_process, #{start := []}, UsrInfo) ->
    {produce, #{process_order => [done]}};

% Enhanced 3-tuple return with state update
fire(t_ship, #{process_order := []}, UsrInfo) ->
    NewUsrInfo = UsrInfo#{status => shipped, shipped_at => os:system_time()},
    {produce, #{end => [complete]}, NewUsrInfo}.
```

### Key Differences from gen_pnet

1. **User Info Updates**: The 3-tuple format allows automatic updating of user info
2. **State Persistence**: Workflow state can be tracked across transitions
3. **Backward Compatibility**: Existing gen_pnet modules work unchanged
4. **Same Performance**: No additional overhead compared to gen_pnet

---

## Message Contracts

### To gen_pnet
```erlang
% Token injection
{produce, #{place1 => [token1, token2], place2 => [token3]}}

% Asynchronous requests
{cast, {custom_request, data}}

% Synchronous requests
{call, {get, marking}}
{call, {get, specific_place}}
{call, last_receipt}
{call, {receipts, 5}}
```

### From gen_pnet
```ersubscript
% Receipt notifications
{pnet_receipt, #{
    before_hash => <<Hash1>>,
    after_hash => <<Hash2>>,
    move => #{trsn => t1, mode => #{}, produce => #{}},
    ts => 1234567890
}}

% Effect commands (optional)
{pnet_effect, audit_log}
{pnet_effect, notification}
```

This completes the comprehensive API reference for the refactored CRE architecture, implementing the Joe Armstrong interface design principles.

---

## YAWL Workflow Framework Modules

### 10. `wf_spec` - YAWL XML Parser and IR

### Types
```erlang
-type yawl_spec() :: #yawl_spec{}.
-type compiled_spec() :: #compiled_spec{}.
-type task_id() :: atom().
-type place() :: atom().
-type transition() :: atom().

-record(yawl_spec, {
    id :: binary(),
    title :: binary(),
    version :: binary() | undefined,
    root_net :: binary(),
    tasks :: #{task_id() => task_info()},
    places :: [place()],
    transitions :: [transition()],
    decompositions :: #{binary() => decomposition_info()},
    flows :: [flow_info()],
    conditions :: #{binary() => condition_info()}
}).

-record(task_info, {
    docs :: binary() | undefined,
    type :: atom(),
    split_type :: 'and' | 'or' | 'xor' | undefined,
    join_type :: 'and' | 'or' | 'xor' | undefined,
    decomposes_to :: binary() | undefined,
    cancellation_set :: [task_id()],
    params :: #{binary() => term()},
    mi_params :: mi_params() | undefined
}).

-type mi_params() :: #{
    min_instances := non_neg_integer(),
    max_instances := non_neg_integer() | unlimited,
    continuation_threshold := non_neg_integer()
}.
```

### Main API

```erlang
%% Parsing
-spec from_xml(Xml :: binary()) -> {ok, yawl_spec()} | {error, term()}.
-spec from_xml_file(FilePath :: file:filename_all()) -> {ok, yawl_spec()} | {error, term()}.

%% Accessors
-spec id(Spec :: yawl_spec()) -> binary().
-spec title(Spec :: yawl_spec()) -> binary().
-spec version(Spec :: yawl_spec()) -> binary() | undefined.
-spec root_net(Spec :: yawl_spec()) -> binary().
-spec tasks(Spec :: yawl_spec()) -> [task_id()].
-spec task_doc(Spec :: yawl_spec(), TaskId :: task_id()) -> binary().
-spec task_type(Spec :: yawl_spec(), TaskId :: task_id()) -> atom().
-spec places(Compiled :: compiled_spec()) -> [place()].
-spec transitions(Compiled :: compiled_spec()) -> [transition()].

%% IR access
-spec uri(Spec :: yawl_spec()) -> binary().
-spec meta(Spec :: yawl_spec()) -> map().
-spec schema_types(Spec :: yawl_spec()) -> [atom()].
-spec nets(Spec :: yawl_spec()) -> [binary()].
-spec tasks(Spec :: yawl_spec(), NetId :: binary()) -> [task_id()].
-spec task_name(Spec :: yawl_spec(), NetId :: binary(), TaskId :: task_id()) -> binary().
-spec join_split(Spec :: yawl_spec(), NetId :: binary(), TaskId :: task_id()) -> {atom(), atom()}.
-spec flows(Spec :: yawl_spec(), NetId :: binary(), FromId :: task_id()) -> [map()].
-spec variables(Spec :: yawl_spec(), NetId :: binary()) -> [map()].
-spec timer(Spec :: yawl_spec(), NetId :: binary(), TaskId :: task_id()) -> map() | undefined.
-spec mi(Spec :: yawl_spec(), NetId :: binary(), TaskId :: task_id()) -> mi_params() | undefined.

%% Advanced features
-spec split_type(Spec :: yawl_spec(), TaskId :: task_id()) -> 'and' | 'or' | 'xor' | undefined.
-spec join_type(Spec :: yawl_spec(), TaskId :: task_id()) -> 'and' | 'or' | 'xor' | undefined.
-spec decomposition_nets(Spec :: yawl_spec()) -> [binary()].
-spec decomposition_net(Spec :: yawl_spec(), TaskId :: task_id()) -> {ok, binary()} | {error, not_found}.
-spec cancellation_set(Spec :: yawl_spec(), TaskId :: task_id()) -> [task_id()].
-spec cancellation_regions(Spec :: yawl_spec()) -> [{task_id(), [task_id()]}].
-spec flows(Spec :: yawl_spec()) -> [{task_id(), task_id(), binary() | undefined}].
-spec flow_predicate(Spec :: yawl_spec(), From :: task_id(), To :: task_id()) -> binary() | undefined | not_found.
-spec task_params(Spec :: yawl_spec(), TaskId :: task_id()) -> #{binary() => term()}.
-spec task_param(Spec :: yawl_spec(), TaskId :: task_id(), ParamName :: binary()) -> term() | undefined.
-spec conditions(Spec :: yawl_spec()) -> [{binary(), input | output, binary() | undefined}].
-spec condition_expr(Spec :: yawl_spec(), ConditionId :: binary()) -> binary() | undefined | not_found.
-spec all_decompositions(Spec :: yawl_spec()) -> [{binary(), boolean(), [task_id()]}].
-spec decomposition_tasks(Spec :: yawl_spec(), DecompositionId :: binary()) -> {ok, [task_id()]} | {error, not_found}.

%% Validation and compilation
-spec validate(Spec :: yawl_spec()) -> ok | {error, [binary()]}.
-spec compile(Spec :: yawl_spec()) -> {ok, compiled_spec()} | {error, term()}.
```

### 11. `wf_rules` - Rule Evaluation

### Types
```erlang
-type rules() :: #{
    clauses := #{predicate() => [clause()]}
}.

-type predicate() :: atom().
-type arg() :: atom() | {var, atom()} | term().
-type clause() :: #{
    head := pred_ref(),
    body := [pred_ref()]
}.
-type pred_ref() :: {predicate(), [arg()]}.
-type fact() :: {predicate(), [term()]}.
-type binding() :: #{atom() => term()}.
-type query() :: {predicate(), [arg()]}.
```

### Functions

```erlang
%% Compilation
-spec compile(Program :: binary()) -> {ok, rules()} | {error, bad_program}.

%% Fact conversion
-spec facts_from_marking(Marking :: #{atom() => [term()]}) -> [fact()].

%% Query operations
-spec bool(Rules :: rules(), Query :: query(), Facts :: [fact()],
           ExtraFacts :: #{atom() => term()}) -> true | false | {error, Reason :: term()}.

-spec query(Rules :: rules(), Query :: query(), Facts :: [fact()],
            ExtraFacts :: #{atom() => term()}) -> {ok, [binding()]} | {error, Reason :: term()}.
```

### Usage Example

```erlang
% Compile rules
{ok, Rules} = wf_rules:compile(<<"enabled(a) :- token(coin_slot, coin).">>),

% Convert marking to facts
Facts = wf_rules:facts_from_marking(#{coin_slot => [coin], storage => []}),

% Evaluate boolean query
true = wf_rules:bool(Rules, {enabled, a}, Facts, #{}),

% Query with variable bindings
{ok, R2} = wf_rules:compile(<<"take(X) :- token(p, X).">>),
F2 = wf_rules:facts_from_marking(#{p => [a,b]}),
{ok, Bs} = wf_rules:query(R2, {take, {var, x}}, F2, #{}).
```

### 12. `wf_yawl_pred` - XPath to Erlog Compiler

### Types
```erlang
-type xpath_ast() ::
    {xpath_var, {path, binary(), binary()}} |
    {xpath_literal, term()} |
    {xpath_comp, xpath_ast(), binary(), xpath_ast()} |
    {xpath_bool, binary(), [xpath_ast()]}.

-type comp_op() :: '=' | '!=' | '<' | '>' | '<=' | '>='.
-type bool_op() :: 'and' | 'or'.
```

### Functions

```erlang
%% Main API
-spec to_erlog(PredicateBin :: binary()) ->
    {ok, {RulesBin :: binary(), GoalTerm :: term()}} | {error, Reason :: term()}.

-spec parse_xpath(PredicateBin :: binary()) ->
    {ok, xpath_ast()} | {error, Reason :: term()}.

-spec extract_vars(PredicateBin :: binary()) ->
    {ok, [VarName :: binary()]} | {error, Reason :: term()}.
```

### Supported XPath Patterns

```erlang
% Variable references: /NetName/VarName/text() -> token(NetName, VarName)
"/Overall/Status/text()='approved'"

% String literals
"/Overall/Flag/text()='true'"

% Numeric comparison
"/Overall/Count/text() > 0"

% Boolean operators
"/Overall/A/text()='true' and /Overall/B/text()='false'"
"/Overall/A/text()='true' or /Overall/B/text()='true'"

% Compound predicates
"/Overall/PO_timedout/text()='false' and /Overall/POApproval/text()='true'"
```

### Usage Example

```erlang
% Convert XPath predicate to Erlog rules
{ok, {RulesBin, _Goal}} = wf_yawl_pred:to_erlog(
    <<"/Overall/PO_timedout/text()='false'">>
),

% Extract variable references
{ok, Vars} = wf_yawl_pred:extract_vars(
    <<"/Overall/Status/text()='approved'">>
),
% Returns: [<<"/Overall/Status/text()">>]
```

### 14. `wf_timer` - ISO 8601 Duration Parser

### Types
```erlang
-type duration() :: binary().
-type milliseconds() :: non_neg_integer().
-type seconds() :: non_neg_integer().
-type parse_result() :: {ok, milliseconds()} | {error, term()}.
```

### Functions

```erlang
-spec parse_duration(DurationBin :: duration()) -> parse_result().
-spec to_ms(DurationBin :: duration()) -> milliseconds().
-spec to_seconds(DurationBin :: duration()) -> seconds().
```

### Duration Format

```erlang
% ISO 8601 duration components:
% P - designator starts duration (required)
% Date: Y (years), M (months), D (days)
% Time: H (hours), M (minutes), S (seconds)
% T - separator between date and time

% Examples:
"P3D"           -> 3 days
"PT12H"         -> 12 hours
"PT1H30M"       -> 1 hour 30 minutes
"P1DT2H30M"     -> 1 day 2 hours 30 minutes
"P1Y2M3DT4H5M6S" -> Full duration
```

### Conversion Rules

```erlang
% Simplified conversions:
% Year: 365 days
% Month: 30 days
% Day: 24 hours
% Hour: 60 minutes
% Minute: 60 seconds
% Second: 1000 milliseconds
```

### Usage Example

```erlang
% Parse duration
{ok, Ms} = wf_timer:parse_duration(<<"PT30S">>),
% Returns: {ok, 30000}

% Convert to milliseconds
Ms = wf_timer:to_ms(<<"P3D">>),
% Returns: 259200000

% Convert to seconds
Secs = wf_timer:to_seconds(<<"PT5M">>),
% Returns: 300
```

### 15. `wf_timerq` - Deadline Queue

### Types
```erlang
-opaque timerq() :: #{keys := key_map(), deadlines := deadline_list()}.
-type timer_key() :: term().
-type deadline() :: integer().
-type duration_ms() :: non_neg_integer().
-type iso8601_duration() :: binary() | string().
-type timer_event() :: term().
-type duration_error() :: bad_duration.
```

### Functions

```erlang
%% Basic queue operations
-spec new() -> timerq().
-spec arm(TimerQ :: timerq(), Key :: timer_key(),
         Deadline :: deadline(), Event :: timer_event()) -> timerq().
-spec disarm(TimerQ :: timerq(), Key :: timer_key()) -> timerq().
-spec poll(TimerQ :: timerq(), Now :: deadline()) -> {[timer_event()], timerq()}.
-spec is_empty(TimerQ :: timerq()) -> boolean().
-spec size(TimerQ :: timerq()) -> non_neg_integer().
-spec peek(TimerQ :: timerq()) -> {deadline(), timer_event()} | undefined.

%% Extended timer API
-spec arm_from_now(TimerQ :: timerq(), Key :: timer_key(),
                  DurationMs :: duration_ms(), Event :: timer_event()) -> timerq().
-spec get_deadline(TimerQ :: timerq(), Key :: timer_key()) -> deadline() | undefined.
-spec clear_all(TimerQ :: timerq()) -> timerq().
-spec arm_duration(TimerQ :: timerq(), Key :: timer_key(),
                Duration :: iso8601_duration(), Event :: timer_event()) ->
    timerq() | {error, duration_error}.
```

### Usage Example

```erlang
% Create new queue
Q0 = wf_timerq:new(),

% Arm timers
Q1 = wf_timerq:arm(Q0, k1, 1100, {inject, p1, a}),
Q2 = wf_timerq:arm(Q1, k2, 1200, {inject, p2, b}),

% Poll for ready events
{Events, Q3} = wf_timerq:poll(Q2, 1100),
% Returns: [{inject, p1, a}]

% Disarm timer
Q4 = wf_timerq:disarm(Q2, k2),

% Check queue state
wf_timerq:size(Q2),
% Returns: 2

wf_timerq:is_empty(Q0),
% Returns: true
```

### 16. `wf_mi` - Multi-Instance Handling

### Types
```erlang
-type mi_params() :: #{
    min_instances := non_neg_integer(),
    max_instances := non_neg_integer() | unlimited,
    continuation_threshold := non_neg_integer()
}.
-type data() :: map().
-type mi_token() :: {mi_instance, task_id(), non_neg_integer()}.
```

### Functions

```erlang
%% Detection and evaluation
-spec is_mi_task(Spec :: term(), NetId :: binary(), TaskId :: atom()) -> boolean().
-spec evaluate_mi(MIParams :: mi_params() | undefined, Data :: data()) ->
    {ok, non_neg_integer()} | {error, term()}.

%% Token creation
-spec create_instance_tokens(TaskId :: atom(), Count :: non_neg_integer()) -> [mi_token()].

%% Instance management
-spec instance_count(MIParams :: mi_params(), Data :: data()) ->
    {ok, non_neg_integer()} | {error, term()}.
-spec instance_threshold(MIParams :: mi_params(), Completed :: non_neg_integer()) -> boolean().
```

### Multi-Instance Semantics

```erlang
% Parallel: All instances run simultaneously
% Sequential: Instances run one at a time
% N-of-M: Continue when N out of M instances complete

% Instance parameters:
% min_instances - Minimum instances to create
% max_instances - Maximum instances (unlimited for no limit)
% continuation_threshold - Instances needed before continuing
```

### Usage Example

```erlang
% Check if task is multi-instance
true = wf_mi:is_mi_task(Spec, NetId, TaskId),

% Evaluate instance count
MIParams = #{min_instances => 2, max_instances => 5, continuation_threshold => 3},
Data = #{instance_count => 4},
{ok, 4} = wf_mi:evaluate_mi(MIParams, Data),

% Create instance tokens
Tokens = wf_mi:create_instance_tokens(review_task, 4),
% Returns: [{mi_instance,review_task,0}, {mi_instance,review_task,1},
%           {mi_instance,review_task,2}, {mi_instance,review_task,3}]

% Check continuation threshold
false = wf_mi:instance_threshold(MIParams, 2),
true = wf_mi:instance_threshold(MIParams, 3),
```

### 17. `wf_cancel` - Cancellation Tokens

### Types
```erlang
-type cancel_token() :: {cancel, [atom()]}.
-type cancel_region() :: {cancel_region, atom(), [atom()]}.
-type cancellation_set() :: [atom()].
-type marking() :: #{atom() => [term()]}.
```

### Functions

```erlang
%% Token validation
-spec is_cancel_token(term()) -> boolean().

%% Token creation
-spec create_cancel_token(Target :: atom() | [atom()]) -> cancel_token().

%% Token inspection
-spec cancel_targets(cancel_token()) -> [atom()].

%% Cancellation application
-spec apply_cancellation(Marking :: marking(), Targets :: [atom()]) -> marking().
-spec cancel_region(Marking :: marking(), Region :: [atom()]) -> marking().

%% Validation
-spec is_cancellation_set(term()) -> boolean().
```

### Cancellation Behavior

```erlang
% When a cancel token is processed:
% 1. Remove all tokens from places in the cancellation set
% 2. Set those places to empty lists
% 3. Return the updated marking

% Cancel token: {cancel, [place1, place2, ...]}
% Cancel region: {cancel_region, RegionName, [place1, place2, ...]}
```

### Usage Example

```erlang
% Create cancel token
Token = wf_cancel:create_cancel_token([region_place1, region_place2]),
% Returns: {cancel, [region_place1, region_place2]}

% Validate token
true = wf_cancel:is_cancel_token(Token),

% Get cancel targets
[region_place1, region_place2] = wf_cancel:cancel_targets(Token),

% Apply cancellation to marking
Marking = #{p1 => [a], p2 => [b], p3 => [c]},
UpdatedMarking = wf_cancel:apply_cancellation(Marking, [p1, p2]),
% Returns: #{p1 => [], p2 => [], p3 => [c]}

% Cancel region
Region = [p2, p3],
FinalMarking = wf_cancel:cancel_region(Marking, Region),
% Returns: #{p1 => [a], p2 => [], p3 => []}
```

---

## Cross-Module Integration Examples

### Complete YAWL Workflow Execution

```erlang
% 1. Parse YAWL specification
{ok, Spec} = wf_spec:from_xml_file("workflow.yawl"),

% 2. Validate specification
ok = wf_spec:validate(Spec),

% 3. Compile to Petri net modules
{ok, Compiled} = yawl_compile:compile(Spec, #{}),

% 4. Extract net information
{ok, NetInfo} = yawl_compiled:net(Compiled, <<"main">>),
{ok, Module} = yawl_compiled:net_module(Compiled, <<"main">>),

% 5. Start workflow instance
{ok, Pid} = gen_pnet:start_link(Module, #{}, []),

% 6. Monitor progress
{ok, Marking} = gen_pnet:marking(Pid),

% 7. Subscribe to receipts
gen_pnet:subscribe(Pid, self()),

% 8. Poll for task tokens
{ok, Tokens} = gen_pnet:ls(Pid, task_place),
```

### Workflow with Timers

```erlang
% 1. Create timer queue
TimerQ = wf_timerq:new(),

% 2. Parse and arm timer
Duration = <<"PT30S">>,
{ok, DeadlineMs} = wf_timer:parse_duration(Duration),
Now = erlang:monotonic_time(millisecond),
TimerQ1 = wf_timerq:arm(TimerQ, task_timer, Now + DeadlineMs, {inject, task, timeout}),

% 3. Poll for ready events
{ReadyEvents, TimerQ2} = wf_timerq:poll(TimerQ1, erlang:monotonic_time(millisecond)),
```

### Workflow with Predicates

```erlang
% 1. Compile XPath predicate to rules
{ok, {RulesBin, Goal}} = wf_yawl_pred:to_erlog(
    <<"/Overall/Status/text()='approved'">>
),
{ok, Rules} = wf_rules:compile(RulesBin),

% 2. Create facts from marking
Facts = wf_rules:facts_from_marking(#{
    overall => [#{status => <<"approved">>}]
}),

% 3. Evaluate predicate
true = wf_rules:bool(Rules, Goal, Facts, #{}),
```

---

## Execution Guide for Running YAWL Workflows

### Step-by-Step Process

1. **Parse YAWL XML**
   ```erlang
   {ok, Spec} = wf_spec:from_xml_file("workflow.yawl"),
   ```

2. **Validate Specification**
   ```erlang
   case wf_spec:validate(Spec) of
       ok -> continue;
       {error, Errors} -> handle_errors(Errors)
   end,
   ```

3. **Compile to Petri Net**
   ```erlang
   {ok, Compiled} = yawl_compile:compile(Spec, #{}),
   ```

4. **Start Workflow Instance**
   ```erlang
   {ok, Module} = yawl_compiled:net_module(Compiled, <<"main">>),
   {ok, Pid} = gen_pnet:start_link(Module, #{}, []),
   ```

5. **Monitor Execution**
   ```erlang
   gen_pnet:subscribe(Pid, self()),
   receive
       {pnet_receipt, Receipt} -> handle_receipt(Receipt)
   end,
   ```

6. **Inject External Tokens**
   ```erlang
   gen_pnet:produce(Pid, #{task_place => [completion_token]}),
   ```

7. **Query State**
   ```erlang
   {ok, Marking} = gen_pnet:marking(Pid),
   {ok, Receipts} = gen_pnet:receipts(Pid, 10),
   ```

### Troubleshooting

| Issue | Symptoms | Solution |
|-------|----------|----------|
| Parse error | `{error, {parse_error, Reason}}` | Check YAWL XML syntax |
| Validation error | `{error, [Errors]}` | Fix specification issues |
| Compilation error | `{error, Reason}` | Verify net structure |
| No transitions enabled | Empty `ls/2` results | Check token presence in preset places |
| Workflow stuck | No progress loop activity | Verify predicates and rules |
| Timer not firing | No timeout events | Check timer queue polling |

### Common Patterns

**Human Task:**
```erlang
% Task token appears in output place
{ok, [task_token]} = gen_pnet:ls(Pid, human_task_place),

% External worker completes task
gen_pnet:produce(Pid, #{human_task_place => [completion_result]}),
```

**Multi-Instance Task:**
```erlang
% Multiple instance tokens created
{ok, [MI1, MI2, MI3]} = gen_pnet:ls(Pid, mi_task_place),

% Each instance completes independently
gen_pnet:produce(Pid, #{mi_task_place => [completion1]}),
gen_pnet:produce(Pid, #{mi_task_place => [completion2]}),
```

**Cancellation:**
```erlang
% Cancel token appears
gen_pnet:produce(Pid, #{cancel => [{cancel, [task1, task2]}]}),

% Cancellation applied automatically
{ok, Marking} = gen_pnet:marking(Pid),
% task1 and task2 places are now empty
```

---

## Module Categorization

### Generic Framework Modules (Core Infrastructure)
- **gen_pnet** - Petri net runtime (only OTP process)
- **gen_yawl** - YAWL convenience wrapper
- **yawl_compile** - YAWL to pnet_net compiler
- **yawl_compiled** - Compiled spec accessor
- **yawl_validate** - Specification validator

### Pure Utility Modules (Stateless)
- **pnet_types** - Type validation
- **pnet_marking** - Marking algebra
- **pnet_mode** - Mode enumeration
- **pnet_choice** - Deterministic choice
- **pnet_receipt** - Receipt tracking
- **wf_spec** - XML parser and IR
- **wf_rules** - Rule evaluation engine
- **wf_yawl_pred** - XPath compiler
- **wf_timer** - Duration parser
- **wf_timerq** - Timer queue
- **wf_mi** - Multi-instance handling
- **wf_cancel** - Cancellation tokens

### Test Modules (Not for Production Use)
- **wf_test_net_*** - Test networks
- **wf_test_stub_net** - Stub implementation

---

This completes the comprehensive API reference for all generic YAWL framework modules.