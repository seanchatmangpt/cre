# Quick Reference Cards - Generative Analysis

**One-Page Reference Cards for YAWL Workflow Engine**

---

## 1. The 43 Workflow Patterns

### Overview

All 43 YAWL workflow control patterns implemented as `gen_yawl` behavior modules following Joe Armstrong's design principle: one real OTP runner (`gen_pnet`), everything else pure helpers/utilities.

### Pattern Registry Quick Lookup

| ID | Pattern Name | Module | Category |
|----|--------------|--------|----------|
| **P1** | Sequence | `sequence` | Basic Control |
| **P2** | Parallel Split | `parallel_split` | Basic Control |
| **P3** | Synchronization | `synchronization` | Basic Control |
| **P4** | Exclusive Choice | `exclusive_choice` | Basic Control |
| **P5** | Simple Merge | `simple_merge` | Basic Control |
| **P6** | Multiple Choice | `multiple_choice` | Advanced Control |
| **P7** | Structured Sync Merge | `structured_sync_merge` | Advanced Control |
| **P8** | Multiple Merge | `multiple_merge` | Advanced Control |
| **P9** | Discriminator | `discriminator` | Advanced Control |
| **P10** | Arbitrary Cycles | `arbitrary_cycles` | Advanced Control |
| **P11** | Implicit Termination | `implicit_termination` | Termination |
| **P12-P15** | Multi-Instance (4 variants) | `multiple_instances_sync` | Multi-Instance |
| **P16** | Deferred Choice | `deferred_choice` | Advanced Routing |
| **P17** | Interleaved Routing | `interleaved_routing` | Advanced Routing |
| **P18** | Milestone | `milestone` | State-Based |
| **P19** | Cancel Activity | `cancel_activity` | Cancellation |
| **P20** | Cancel Case | `cancel_case` | Cancellation |
| **P21** | Structured Loop | `structured_loop` | Iteration |
| **P22** | Recursion | `recursion` | Iteration |
| **P23** | Transient Trigger | `transient_trigger` | Event-Based |
| **P24** | Persistent Trigger | `persistent_trigger` | Event-Based |
| **P25** | Cancel Region | `cancel_region` | Cancellation |
| **P26** | Cancel MI Activity | `cancel_mi_activity` | Cancellation |
| **P27** | Complete MI Activity | `complete_mi_activity` | Multi-Instance |
| **P28** | Blocking Discriminator | `blocking_discriminator` | Advanced Control |
| **P29** | Cancelling Discriminator | `cancelling_discriminator` | Cancellation |
| **P30** | Structured Partial Join | `structured_partial_join` | Multi-Instance |
| **P31** | Blocking Partial Join | `blocking_partial_join` | Multi-Instance |
| **P32** | Cancelling Partial Join | `cancelling_partial_join` | Cancellation |
| **P33** | Generalized AND Join | `generalized_and_join` | Advanced Control |
| **P34** | Static Partial Join MI | `static_partial_join_mi` | Multi-Instance |
| **P35** | Cancelling Partial Join MI | `cancelling_partial_join_mi` | Cancellation |
| **P36** | Dynamic Partial Join MI | `dynamic_partial_join_mi` | Multi-Instance |
| **P37** | Local Sync Merge | `local_sync_merge` | Advanced Control |
| **P38** | General Sync Merge | `general_sync_merge` | Advanced Control |
| **P39** | Critical Section | `critical_section` | Concurrency |
| **P40** | Interleaved Routing | `interleaved_routing` | Advanced Routing |
| **P41** | Thread Merge | `thread_merge` | Thread-Based |
| **P42** | Thread Split | `thread_split` | Thread-Based |
| **P43** | Explicit Termination | `explicit_termination` | Termination |

### Usage API

```erlang
% Look up pattern module
yawl_pattern_registry:pattern_module(<<"P1_Sequence">>).
% => sequence

% Get all patterns
yawl_pattern_registry:all_patterns().
% => [<<"P1_Sequence">>, <<"P2_ParallelSplit">>, ...]

% Validate pattern
yawl_pattern_registry:validate_pattern(<<"P43_ExplicitTermination">>).
% => true
```

### Pattern Categories Summary

| Category | Patterns | Description |
|----------|----------|-------------|
| **Basic Control** | P1-P5 | Sequence, parallel, choice |
| **Advanced Control** | P6-P10, P28, P33, P37-P38 | Complex merge/split |
| **Termination** | P11, P43 | Implicit/explicit end |
| **Multi-Instance** | P12-P15, P27, P30-P31, P34, P36 | Parallel execution |
| **Advanced Routing** | P16-P17, P40 | Deferred, interleaved |
| **State-Based** | P18 | Milestone guards |
| **Cancellation** | P19-P20, P25-P26, P29, P32, P35 | Activity/region cancel |
| **Iteration** | P21-P22 | Loops, recursion |
| **Event-Based** | P23-P24 | Triggers |
| **Concurrency** | P39 | Critical sections |
| **Thread-Based** | P41-P42 | Thread split/merge |

---

## 2. BCD Template (Behavior-Condition-Data)

### Template Structure

```erlang
%%--------------------------------------------------------------------
%% @doc PATTERN_NAME Pattern
%%
%% Petri Net Structure:
%%   p_start --t_init--> p_ready --t_process--> p_done
%%
%% Soundness Properties:
%%   - Option to complete: [true/false]
%%   - Proper completion: [true/false]
%%   - No dead transitions: [true/false]
%%
%% @end
%%--------------------------------------------------------------------

-module(pattern_name).
-behavior(gen_yawl).

%% Exports
-export([places/0, transitions/0, init_marking/2,
         preset/1, postset/1, is_enabled/3, fire/3]).

%%====================================================================
%% Types
%%====================================================================

-type usr_info() :: #{
    user_id := binary(),
    case_id := binary(),
    timestamp := integer(),
    data => map()
}.

-type mode() :: #{atom() => [term()]}.

%%====================================================================
%% gen_yawl Callbacks
%%====================================================================

%% @doc Returns all place identifiers
-spec places() -> [atom()].
places() ->
    [p_start, p_ready, p_done, p_end].

%% @doc Returns all transition identifiers
-spec transitions() -> [atom()].
transitions() ->
    [t_init, t_process, t_complete].

%% @doc Initial marking for each place
-spec init_marking(atom(), usr_info()) -> [term()].
init_marking(p_start, _UsrInfo) -> [init];
init_marking(_Place, _UsrInfo) -> [].

%% @doc Input places for each transition
-spec preset(atom()) -> [atom()].
preset(t_init) -> [p_start];
preset(t_process) -> [p_ready];
preset(t_complete) -> [p_done].

%% @doc Output places for each transition
-spec postset(atom()) -> [atom()].
postset(t_init) -> [p_ready];
postset(t_process) -> [p_done];
postset(t_complete) -> [p_end].

%% @doc Check if transition is enabled
-spec is_enabled(atom(), mode(), usr_info()) -> boolean().
is_enabled(t_init, #{p_start := [_]}, _UsrInfo) -> true;
is_enabled(t_process, #{p_ready := [Token]}, _UsrInfo) ->
    %% Add guard condition here
    is_valid(Token);
is_enabled(t_complete, #{p_done := [_]}, _UsrInfo) -> true;
is_enabled(_Trsn, _Mode, _UsrInfo) -> false.

%% @doc Fire transition
-spec fire(atom(), mode(), usr_info()) ->
    {produce, #{atom() => [term()]}}.
fire(t_init, #{p_start := []}, _UsrInfo) ->
    {produce, #{p_ready => [ready]}};
fire(t_process, #{p_ready := []}, UsrInfo) ->
    Result = do_processing(UsrInfo),
    {produce, #{p_done => [Result]}};
fire(t_complete, #{p_done := []}, _UsrInfo) ->
    {produce, #{p_end => [complete]}}.

%%====================================================================
%% Internal Functions
%%====================================================================

is_valid(Token) ->
    %% Validation logic
    true.

do_processing(UsrInfo) ->
    %% Processing logic
    processed.
```

### Pattern Implementation Checklist

- [ ] Define `places()` - all place atoms
- [ ] Define `transitions()` - all transition atoms
- [ ] Define `init_marking/2` - initial tokens
- [ ] Define `preset/1` - input arcs
- [ ] Define `postset/1` - output arcs
- [ ] Define `is_enabled/3` - guard conditions
- [ ] Define `fire/3` - transition effects
- [ ] Add `@moduledoc` with Petri net diagram
- [ ] Document soundness properties
- [ ] Export all callback functions

---

## 3. YAML Syntax Quick Reference (YAML 0.2)

### Document Structure

```yaml
yawl_yaml_version: "0.2"

specificationSet:
  yawl_schema_version: "2.1"
  uri: "workflow_id"
  metaData:
    title: "Workflow Title"
    version: "1.0"
    author: "Author Name"
    description: "Workflow description"

  rootNet: "main_net"

  roles:
    - "role1"
    - "role2"

  nets:
    - id: "main_net"
      type: "NetFacts"

      variables:
        - name: "var1"
          type: "string"
          initial: "default_value"

      nodes:
        - id: "Start"
          kind: "inputCondition"
        - id: "Task1"
          kind: "task"
          name: "First Task"
          taskType: "human"  # human | automated | service
        - id: "End"
          kind: "outputCondition"

      flows:
        - from: "Start"
          to: "Task1"
        - from: "Task1"
          to: "End"

      subnets: []
      regions: []

  pattern_instances:
    - id: "pi_1"
      pattern: "P1_Sequence"
      net: "main_net"
      label: "Main Sequence"
      split_task: "Task1"
      merge_task: "Task2"

  pattern_registry:
    P1_Sequence:
      macro: "sequence"

  pattern_usage_index: {}
```

### Node Types

| Kind | Description | Required Fields |
|------|-------------|-----------------|
| `inputCondition` | Entry point for net | `id` |
| `outputCondition` | Exit point for net | `id` |
| `condition` | Generic condition | `id` |
| `task` | Task node | `id`, `taskType` |

### Task Types

| Type | Description |
|------|-------------|
| `human` | Human task (manual) |
| `automated` | Automated task |
| `service` | Service invocation |

### Flow Predicates

```yaml
flows:
  - from: "Choice"
    to: "BranchA"
    predicate: "amount > 1000"
  - from: "Choice"
    to: "BranchB"
    predicate: "amount <= 1000"
```

### Multi-Instance Parameters

```yaml
nodes:
  - id: "MITask"
    kind: "task"
    mi_params:
      min_instances: 1
      max_instances: unlimited  # or integer
      continuation_threshold: 1  # N of M completion
```

### Cancellation Regions

```yaml
regions:
  - id: "region1"
    cancel_region: true
    description: "Cancellable region"
```

### Pattern Instance Parameters

| Parameter | Patterns | Description |
|-----------|----------|-------------|
| `split_task` | P2, P4, P6, P16 | Task that splits flow |
| `merge_task` | P3, P5, P7, P8 | Task that merges flow |
| `branches` | P2, P6 | List of branch task IDs |
| `choices` | P4, P16 | Choice options |
| `n`, `m` | P37 | N of M threshold |
| `instances` | P12-P15 | Instance count/expression |
| `scope` | P25, P39 | Region/mutex identifier |
| `cancel_event` | P19, P20, P25 | Cancel trigger event |

---

## 4. Type Discipline

### Core Type Definitions

```erlang
%% Basic Types
-type place() :: atom().
-type trsn() :: atom().
-type token() :: term().
-type var() :: atom().

%% State Types
-type marking() :: #{place() => [token()]}.
-type consume_map() :: #{place() => [token()]}.
-type produce_map() :: #{place() => [token()]}.
-type mode() :: #{place() => [token()]}.

%% Colored Types
-type binding() :: #{var() => term()}.
-type cmode() :: {binding(), mode()}.

%% Execution Types
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

### Type Validation Functions

```erlang
%% All validators are total (return boolean, never crash)

pnet_types:is_place(term()) -> boolean().
pnet_types:is_trsn(term()) -> boolean().
pnet_types:is_token(term()) -> boolean().      % Always true
pnet_types:is_var(term()) -> boolean().

pnet_types:is_marking(term()) -> boolean().
pnet_types:is_consume_map(term()) -> boolean().
pnet_types:is_produce_map(term()) -> boolean().
pnet_types:is_mode(term()) -> boolean().

pnet_types:is_binding(term()) -> boolean().
pnet_types:is_cmode(term()) -> boolean().

pnet_types:is_move(term()) -> boolean().
pnet_types:is_receipt(term()) -> boolean().
```

### Common Types by Module

| Module | Key Types | Description |
|--------|-----------|-------------|
| `pnet_types` | place, trsn, token, marking, mode, move, receipt | Core Petri net types |
| `wf_yaml_spec` | spec_id, task_id, pattern_instance, variable_def | YAML specification types |
| `wf_engine` | case_id, wi_id, wi_status, case_status, work_item | Engine types |
| `yawl_recovery` | checkpoint_id, checkpoint_options, timestamp | Recovery types |
| `wf_exception` | exception, exception_type, compensation, handler | Exception types |

### Type Assertions in Guards

```erlang
%% In pattern matching on function heads
fire(Trsn, Mode, UsrInfo) when is_atom(Trsn), is_map(Mode) ->
    %% Safe to proceed
    ok.

%% Type checking in guards
is_enabled(Trsn, Mode, _UsrInfo)
    when is_atom(Trsn), is_map(Mode) ->
    %% Additional checks
    true.
```

### Common Type Patterns

```erlang
%% User info map (pass-through data)
-record(usr_info, {
    user_id :: binary(),
    case_id :: binary(),
    timestamp :: integer(),
    data :: map()
}).

%% Workflow data
-type workflow_data() :: #{
    required_key := term(),
    optional_key => term()
}.

%% Transition results
-type fire_result() :: {produce, produce_map()}.
-type enabled_result() :: boolean().
```

---

## 5. Receipt Format

### Receipt Structure

```erlang
-type receipt() :: #{
    before_hash := binary(),    % Hash of marking before execution
    after_hash := binary(),     % Hash of marking after execution
    move := move(),             % The transition that was fired
    ts := integer()             % Timestamp (milliseconds since epoch)
}.

-type move() :: #{
    trsn := trsn(),             % Transition atom
    mode := mode() | cmode(),   % Binding + token mode
    produce := produce_map()    % Tokens produced
}.
```

### Receipt Verification

```erlang
%% Verify receipt structure
pnet_types:is_receipt(Receipt) -> boolean().

%% Verify hash integrity
verify_receipt_hash(Receipt) ->
    #{before_hash := Before,
      after_hash := After,
      move := Move} = Receipt,
    %% Compare with computed hashes
    Before == compute_hash(Move).
```

### Audit Log Operations

```erlang
%% Open audit log
{ok, Log} = wf_audit_log:open(#{
    name => receipts,
    file => "/var/log/receipts.log"
}).

%% Append receipt
ok = wf_audit_log:append(Log, Receipt).

%% Read with cursor pagination
{ok, [Receipt1, Receipt2], NextCursor} =
    wf_audit_log:read(Log, 0, 10).

%% Close log
ok = wf_audit_log:close(Log).
```

### Receipt Creation

```erlang
%% In gen_pnet transition execution
create_receipt(BeforeMarking, AfterMarking, Trsn, Mode, Produce) ->
    #{
        before_hash => hash_marking(BeforeMarking),
        after_hash => hash_marking(AfterMarking),
        move => #{
            trsn => Trsn,
            mode => Mode,
            produce => Produce
        },
        ts => erlang:system_time(millisecond)
    }.
```

### Hash Computation

```erlang
%% Simple hash (for illustration)
hash_marking(Marking) ->
    crypto:hash(sha256, term_to_binary(Marking)).

%% Production hash
hash_marking(Marking) ->
    Sorted = lists:sort(maps:to_list(Marking)),
    crypto:hash(sha256, term_to_binary(Sorted)).
```

---

## 6. Guard Conditions

### Common Guard Patterns

```erlang
%% Token presence guard
is_enabled(t1, #{p1 := [_]}, _UsrInfo) -> true;
is_enabled(t1, #{p1 := []}, _UsrInfo) -> false.

%% Multiple token guard
is_enabled(t2, #{p1 := [Token1, Token2]}, _UsrInfo)
    when length(Token1) > 0, length(Token2) > 0 -> true.

%% Data value guard
is_enabled(t_approve, #{p_data := [#{amount := Amount}]}, _UsrInfo)
    when Amount > 1000 -> true;
is_enabled(t_approve, _Mode, _UsrInfo) -> false.

%% User role guard
is_enabled(t_admin, Mode, #{role := Role}) ->
    lists:member(admin, Role);
is_enabled(t_admin, _Mode, _UsrInfo) -> false.

%% Time-based guard
is_enabled(t_business_hours, _Mode, _UsrInfo) ->
    Hour = calendar:local_time() |> element(3),
    Hour >= 9 andalso Hour =< 17.
```

### Predicate Evaluation

```erlang
%% XPath-style predicate evaluation
eval_predicate(<<"amount > 1000">>, Mode, UsrInfo) ->
    #{p_data := [Data]} = Mode,
    maps:get(amount, Data, 0) > 1000;

eval_predicate(<<"role = 'admin'">>, _Mode, #{role := Role}) ->
    Role =:= admin.

%% Complex predicate
eval_predicate(<<"amount > X and status = 'pending'">>,
              #{p_data := [Data]}, _UsrInfo) ->
    maps:get(amount, Data, 0) > maps:get(X, Data, 0)
    andalso maps:get(status, Data, "") =:= <<"pending">>.
```

### Multi-Condition Guards

```erlang
%% AND condition
is_enabled(t1, Mode, UsrInfo) ->
    check_condition_a(Mode) andalso check_condition_b(Mode, UsrInfo).

%% OR condition
is_enabled(t2, Mode, UsrInfo) ->
    check_condition_a(Mode) orelse check_condition_b(Mode, UsrInfo).

%% NOT condition
is_enabled(t3, Mode, UsrInfo) ->
    not check_condition_a(Mode).
```

### Rule-Based Guards

```erlang
%% Using wf_rules engine
is_enabled(t_rule_based, Mode, UsrInfo) ->
    Context = build_context(Mode, UsrInfo),
    {ok, Result} = wf_rules:evaluate_rule(approval_rule, Context),
    Result =:= true.

build_context(#{p_data := [Data]}, UsrInfo) ->
    #{
        data => Data,
        user => maps:get(user_id, UsrInfo),
        timestamp => maps:get(timestamp, UsrInfo)
    }.
```

---

## 7. Error Handling

### Exception Types

```erlang
-type exception_type() ::
    application_error |   % Business logic errors
    system_error |        % Technical failures
    timeout_error |       % Time-based exceptions
    resource_error |      % Resource unavailable
    validation_error.     % Input validation failures
```

### Creating Exceptions

```erlang
%% Create exception with data
Exception = wf_exception:new(
    application_error,
    payment_failed,
    #{amount => 100, reason => insufficient_funds}
).

%% Create from Erlang error
Exception2 = wf_exception:from_error({error, ebadf}).
%% => #{type => system_error, reason => error, data => #{reason => ebadf}}
```

### Exception Handlers

```erlang
%% Define handler
Handler = wf_exception:handler(
    fun(Exception) -> wf_exception:type(Exception) =:= application_error end,
    fun(Exception, _Context) ->
        #{data := Data} = Exception,
        {ok, handle_application_error(Data)}
    end
).

%% Use handler
case wf_exception:handle(Exception, Handler) of
    {handled, Result} -> Result;
    {unhandled, Exc} -> error(unhandled_exception)
end.
```

### Compensation Actions

```erlang
%% Define compensation
Compensation = wf_exception:compensation(
    refund_payment,
    #{txn_id => <<"tx123">>, amount => 100}
).

%% Execute compensation
execute_compensation(Compensation) ->
    Action = wf_exception:comp_action(Compensation),
    Data = wf_exception:comp_data(Compensation),
    %% Execute compensation action
    apply(Action, [Data]).
```

### Try-Catch Regions

```erlang
%% Try region with compensation
try_region(ActivityFun, Compensations) ->
    try
        Result = ActivityFun(),
        {ok, Result}
    catch
        Type:Reason:Stack ->
            Exception = wf_exception:new(Type, Reason, #{}),
            execute_compensations(Compensations),
            {error, Exception}
    end.

execute_compensations([]) -> ok;
execute_compensations([Comp | Rest]) ->
    execute_compensation(Comp),
    execute_compensations(Rest).
```

### Exception Bubbling

```erlang
%% Mark exception for bubbling
BubblingException = wf_exception:bubble(Exception).

%% Check if bubbleable
wf_exception:is_bubbleable(BubblingException).
%% => true

%% Set source
Exception2 = wf_exception:set_source(Exception, task_123).
```

### Recovery Patterns

```erlang
%% Checkpoint before risky operation
{ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data),

try
    risky_operation()
catch
    _:Error ->
        %% Rollback to checkpoint
        {ok, {RestoredMarking, RestoredData}} =
            yawl_recovery:resume(SpecId, CaseId, Cpid),
        retry_operation(RestoredMarking, RestoredData)
end.
```

---

## 8. Swarm Roles

### Role Types

| Role | Module | Description |
|------|--------|-------------|
| **System Architect** | `system_architect` | Design scalable, secure architectures |
| **Code Analyzer** | `code-analyzer` | Deep code quality analysis |
| **Performance Benchmarker** | `performance-benchmarker` | Performance measurement |
| **Backend Developer** | `backend-dev` | Docker, containers, APIs |
| **Task Orchestrator** | `task-orchestrator` | Complex workflow coordination |
| **Coder** | `coder` | Basic implementation |
| **Reviewer** | `reviewer` | Code review |
| **Tester** | `tester` | Test creation |
| **Planner** | `planner` | Task planning |
| **Researcher** | `researcher` | Investigation and analysis |

### Role Interfaces

```erlang
%% System Architect
-callback design_architecture(Spec :: map()) -> Arch :: map().
-callback verify_soundness(Arch :: map()) -> ok | {error, term()}.

%% Code Analyzer
-callback analyze_quality(Code :: [file:filename()]) -> Report :: map().
-callback detect_smells(Code :: [file:filename()]) -> Smells :: [term()].

%% Performance Benchmarker
-callback benchmark(Module :: atom()) -> Metrics :: map().
-callback find_bottlenecks(Metrics :: map()) -> Bottlenecks :: [term()].

%% Backend Developer
-callback implement_api(Spec :: map()) -> {ok, Module :: atom()}.
-callback deploy_service(Service :: atom()) -> ok | {error, term()}.

%% Task Orchestrator
-callback orchestrate(Workflow :: map()) -> {ok, Results :: map()}.
-callback coordinate(Tasks :: [task()]) -> ok.
```

### Swarm Coordination

```bash
# Initialize swarm topology
npx claude-flow@alpha swarm init --topology mesh --max-agents 6

# Spawn agents
npx claude-flow@alpha agent spawn --type researcher
npx claude-flow@alpha agent spawn --type coder
npx claude-flow@alpha agent spawn --type tester

# Orchestrate workflow
npx claude-flow@alpha task orchestrate --workflow "complex_build"
```

### Pre/Post Hooks

```bash
# Before work
npx claude-flow@alpha hooks pre-task --description "Implement feature X"

# After edit
npx claude-flow@alpha hooks post-edit --file "src/module.erl"

# After completion
npx claude-flow@alpha hooks post-task --task-id "task-123"

# Session end
npx claude-flow@alpha hooks session-end --export-metrics true
```

### Memory Operations

```bash
# Store decision
npx claude-flow@alpha memory store --key "pattern_choice" --value "P1_Sequence"

# Retrieve memory
npx claude-flow@alpha memory retrieve --key "pattern_choice"

# Search memory
npx claude-flow@alpha memory search --query "YAWL patterns"
```

---

## 9. Verification Commands

### Build Commands

```bash
# Compile
rebar3 compile

# Clean build
rebar3 clean
rebar3 compile

# Format code
rebar3 efmt -c

# Generate documentation
rebar3 edoc
```

### Testing Commands

```bash
# Unit tests (EUnit)
rebar3 eunit
rebar3 eunit -m pattern_module_test
rebar3 eunit -v

# Integration tests (Common Test)
rebar3 ct
rebar3 ct -c test/yawl_engine_SUITE
rebar3 ct -v

# Coverage
rebar3 cover
rebar3 cover --verbose
rebar3 cover --export
```

### Static Analysis

```bash
# Dialyzer (type analysis)
rebar3 dialyzer
rebar3 dialyzer -Wunmatched_returns

# Xref (cross-reference checks)
rebar3 xref

# Format check
rebar3 efmt -c --check
```

### Pattern Verification

```bash
# Verify pattern registry
erl -pa _build/default/lib/cre/ebin -eval "
    yawl_pattern_registry:all_patterns(),
    init:stop().
"

# Validate YAML specification
erl -pa _build/default/lib/cre/ebin -eval "
    {ok, Spec} = wf_yaml_spec:from_yaml_file(\"workflow.yaml\"),
    wf_yaml_spec:validate(Spec),
    init:stop().
"

# Test pattern module
rebar3 eunit -m sequence_test
```

### Runtime Verification

```bash
# Start with observer
rebar3 shell
# Then in shell:
observer:start().

# Enable tracing
gen_pnet:trace(Pid, true).

# Get statistics
gen_pnet:stats(Pid).

# Check receipts
gen_pnet:receipts(Pid).
```

### Quick Reference Table

| Command | Purpose |
|---------|---------|
| `rebar3 compile` | Build all sources |
| `rebar3 eunit` | Run unit tests |
| `rebar3 ct` | Run integration tests |
| `rebar3 cover` | Generate coverage report |
| `rebar3 dialyzer` | Static type analysis |
| `rebar3 xref` | Cross-reference check |
| `rebar3 edoc` | Generate API docs |
| `rebar3 efmt -c` | Format code |
| `rebar3 shell` | Interactive shell |
| `rebar3 clean` | Clean build artifacts |

---

## Quick Index

| Topic | Reference Card |
|-------|----------------|
| All 43 patterns | Card 1 |
| Pattern implementation | Card 2 |
| YAML syntax | Card 3 |
| Type validation | Card 4 |
| Receipt format | Card 5 |
| Guard conditions | Card 6 |
| Error handling | Card 7 |
| Swarm roles | Card 8 |
| Verification | Card 9 |

---

**Document Version**: 1.0.0
**Generated**: 2026-02-07
**For**: CRE - Common Runtime Environment
