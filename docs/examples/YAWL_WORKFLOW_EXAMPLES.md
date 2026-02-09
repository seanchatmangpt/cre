# YAWL Workflow Examples

This document provides practical, working examples of YAWL (Yet Another Workflow Language) workflows in CRE. Each example includes the YAML specification, Petri net diagram description, execution trace, and expected outcome.

## Table of Contents

1. [Basic Patterns](#basic-patterns)
   - [Sequence (P1)](#sequence-p1)
   - [Parallel Split (P2)](#parallel-split-p2)
   - [Synchronization (P3)](#synchronization-p3)
   - [Exclusive Choice (P4)](#exclusive-choice-p4)
2. [Advanced Patterns](#advanced-patterns)
   - [Discriminator (P9)](#discriminator-p9)
   - [N-out-of-M Join (P30)](#n-out-of-m-join-p30)
   - [Milestone (P18)](#milestone-p18)
3. [Cancellation Patterns](#cancellation-patterns)
   - [Cancel Activity (P19)](#cancel-activity-p19)
   - [Cancel Case (P20)](#cancel-case-p20)
4. [Multi-Instance Patterns](#multi-instance-patterns)
   - [Static MI (P13)](#static-mi-p13)
   - [Dynamic MI (P14)](#dynamic-mi-p14)
5. [Data Flow Patterns](#data-flow-patterns)
   - [Data Distribution](#data-distribution)
   - [Data Accumulation](#data-accumulation)
6. [Complete Workflow](#complete-workflow)

---

## Basic Patterns

### Sequence (P1)

The simplest pattern where tasks execute one after another in order.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "basic_sequence"
  metaData:
    title: "Basic Sequence Pattern"
    version: "1.0"
    creator: "CRE Examples"
    identifier: "UID_SEQ_001"

  rootNet: SequenceNet

  roles:
    - Worker

  nets:
    - id: SequenceNet
      type: NetFacts
      variables:
        - {name: step1_result, type: string, initial: ""}
        - {name: step2_result, type: string, initial: ""}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: Task1, kind: task, taskType: automated, name: "Step 1"}
        - {id: Task2, kind: task, taskType: automated, name: "Step 2"}
        - {id: Task3, kind: task, taskType: automated, name: "Step 3"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: Task1}
        - {from: Task1, to: Task2}
        - {from: Task2, to: Task3}
        - {from: Task3, to: End}
```

#### Petri Net Diagram Description

```
    [Start] --> (Task1) --> (Task2) --> (Task3) --> [End]
       |          |          |          |
       p_start   p_task1   p_task2   p_end
```

**Places:**
- `p_start` - Initial place with start token
- `p_task1` - Task 1 active
- `p_task2` - Task 2 active
- `p_end` - Final place

**Transitions:**
- `t_start` - Begins Task 1
- `t_complete1` - Completes Task 1, enables Task 2
- `t_complete2` - Completes Task 2, enables Task 3
- `t_finish` - Finalizes workflow

#### Execution Trace

```
1. Initial marking: p_start = [start]
2. Fire t_start: p_task1 = [token]
3. Task1 completes, fire t_complete1: p_task2 = [token]
4. Task2 completes, fire t_complete2: p_task3 = [token]
5. Task3 completes, fire t_finish: p_end = [done]
```

#### Expected Outcome

- All tasks execute in strict order
- Each task starts only after the previous completes
- Final state: workflow completed with all steps done

---

### Parallel Split (P2)

Splits a single thread into multiple parallel branches that execute concurrently.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "parallel_split_example"
  metaData:
    title: "Parallel Split Pattern"
    version: "1.0"
    creator: "CRE Examples"
    identifier: "UID_PARALLEL_001"

  rootNet: ParallelNet

  roles:
    - Worker

  nets:
    - id: ParallelNet
      type: NetFacts
      variables:
        - {name: branch1_complete, type: boolean, initial: false}
        - {name: branch2_complete, type: boolean, initial: false}
        - {name: branch3_complete, type: boolean, initial: false}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: Split, kind: task, taskType: automated, name: "Split Tasks"}
        - {id: Branch1, kind: task, taskType: automated, name: "Process A"}
        - {id: Branch2, kind: task, taskType: automated, name: "Process B"}
        - {id: Branch3, kind: task, taskType: automated, name: "Process C"}
        - {id: Join, kind: task, taskType: automated, name: "Join Results"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: Split}
        - {from: Split, to: Branch1}
        - {from: Split, to: Branch2}
        - {from: Split, to: Branch3}
        - {from: Branch1, to: Join}
        - {from: Branch2, to: Join}
        - {from: Branch3, to: Join}
        - {from: Join, to: End}
```

#### Petri Net Diagram Description

```
                    [Branch1]
                   /          \
    [Start] --> [Split] -----> [Join] --> [End]
                   \          /
                    [Branch2] --+
                    [Branch3] --+
```

**Places:**
- `p_start` - Initial place
- `p_branch1`, `p_branch2`, `p_branch3` - Parallel execution places
- `p_join_ready` - Synchronization point
- `p_end` - Final place

**Transitions:**
- `t_split` - Creates parallel tokens
- `t_join_branch1/2/3` - Completes individual branches
- `t_finish` - Finalizes after all branches complete

#### Execution Trace

```
1. Initial: p_start = [start]
2. Fire t_split: p_branch1 = [1], p_branch2 = [2], p_branch3 = [3]
3. Parallel execution:
   - Branch1 processes independently
   - Branch2 processes independently
   - Branch3 processes independently
4. All branches complete: p_join_ready = [1, 2, 3]
5. Fire t_finish: p_end = [complete]
```

#### Expected Outcome

- All three branches execute concurrently
- Join waits for ALL branches to complete
- Execution time equals the slowest branch

---

### Synchronization (P3)

AND-join pattern that waits for all incoming branches before proceeding.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "synchronization_example"
  metaData:
    title: "Synchronization Pattern"
    version: "1.0"
    identifier: "UID_SYNC_001"

  rootNet: SyncNet

  nets:
    - id: SyncNet
      type: NetFacts
      variables:
        - {name: all_ready, type: boolean, initial: false}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: Split, kind: task, taskType: automated, name: "Distribute"}
        - {id: Worker1, kind: task, taskType: service, name: "Worker A"}
        - {id: Worker2, kind: task, taskType: service, name: "Worker B"}
        - {id: Worker3, kind: task, taskType: service, name: "Worker C"}
        - {id: SyncPoint, kind: task, taskType: automated, name: "AND-Join"}
        - {id: Continue, kind: task, taskType: automated, name: "Continue"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: Split}
        - {from: Split, to: Worker1}
        - {from: Split, to: Worker2}
        - {from: Split, to: Worker3}
        - {from: Worker1, to: SyncPoint}
        - {from: Worker2, to: SyncPoint}
        - {from: Worker3, to: SyncPoint}
        - {from: SyncPoint, to: Continue}
        - {from: Continue, to: End}
```

#### Petri Net Diagram Description

```
    [Start] --> [Split] --> [Worker1] --\
                 |            [Worker2] ---> [SyncPoint] --> [Continue] --> [End]
                 |            [Worker3] --/
```

**Key Behavior:**
- `SyncPoint` has preset: `[p_branch1, p_branch2, p_branch3]`
- `is_enabled` only returns `true` when ALL three branches have tokens
- This creates a barrier synchronization

#### Execution Trace

```
1. Fire t_split: tokens distributed to all branches
2. Workers execute concurrently:
   - Worker1 completes: p_branch1 = [done]
   - SyncPoint NOT enabled (waiting for Worker2, Worker3)
   - Worker2 completes: p_branch2 = [done]
   - SyncPoint NOT enabled (waiting for Worker3)
   - Worker3 completes: p_branch3 = [done]
3. Now SyncPoint is enabled (all branches present)
4. Fire t_sync: p_continue = [ready]
```

#### Expected Outcome

- Workflow continues ONLY after all branches complete
- Acts as a barrier for parallel execution
- Guarantees all parallel work is done before continuing

---

### Exclusive Choice (P4)

Exactly one of multiple alternative branches is selected based on conditions.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "exclusive_choice_example"
  metaData:
    title: "Exclusive Choice Pattern"
    version: "1.0"
    identifier: "UID_CHOICE_001"

  rootNet: ChoiceNet

  nets:
    - id: ChoiceNet
      type: NetFacts
      variables:
        - {name: order_value, type: decimal, initial: "0.00"}
        - {name: customer_type, type: string, initial: "standard"}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: Evaluate, kind: task, taskType: automated, name: "Evaluate Order"}
        - {id: PremiumApproval, kind: task, taskType: human, name: "Premium Approval"}
        - {id: StandardApproval, kind: task, taskType: automated, name: "Auto Approve"}
        - {id: RejectOrder, kind: task, taskType: automated, name: "Reject"}
        - {id: Merge, kind: condition}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: Evaluate}
        # XOR split: only one path taken based on condition
        - {from: Evaluate, to: PremiumApproval}
        - {from: Evaluate, to: StandardApproval}
        - {from: Evaluate, to: RejectOrder}
        # Simple merge: all paths converge
        - {from: PremiumApproval, to: Merge}
        - {from: StandardApproval, to: Merge}
        - {from: RejectOrder, to: Merge}
        - {from: Merge, to: End}
```

#### Petri Net Diagram Description

```
                    --> [PremiumApproval] --
                   /                         \
    [Start] --> [Evaluate] --> [StandardApproval] --> [Merge] --> [End]
                   \                         /
                    --> [RejectOrder] -------
```

**Places:**
- `p_start` - Initial
- `p_choice` - Decision point (one token consumed)
- `p_selected` - Indicates selected branch
- `p_end` - Final

**Transitions:**
- `t_select_a` - Select Premium branch
- `t_select_b` - Select Standard branch
- `t_select_c` - Select Reject branch
- `t_finish` - Complete

#### Execution Trace

```
Example 1: High value order
1. p_choice = [order_data]
2. Condition: order_value > 1000 AND customer_type = "premium"
3. Fire t_select_a: p_premium_approval = [selected]
4. PremiumApproval executes
5. Merge and complete

Example 2: Standard order
1. p_choice = [order_data]
2. Condition: order_value < 1000
3. Fire t_select_b: p_standard_approval = [auto_approve]
4. Automatic approval executes
5. Merge and complete

Example 3: Invalid order
1. p_choice = [order_data]
2. Condition: invalid_data = true
3. Fire t_select_c: p_reject = [rejected]
4. Rejection path executes
5. Merge and complete
```

#### Expected Outcome

- Exactly ONE branch is selected
- Other branches are never executed
- All branches merge back to single flow
- Selection based on data conditions at runtime

---

## Advanced Patterns

### Discriminator (P9)

Waits for the FIRST completion among multiple branches and proceeds, ignoring remaining completions.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "discriminator_example"
  metaData:
    title: "Discriminator Pattern"
    version: "1.0"
    identifier: "UID_DISC_001"

  rootNet: DiscNet

  nets:
    - id: DiscNet
      type: NetFacts
      variables:
        - {name: winner_index, type: integer, initial: "0"}
        - {name: first_complete_time, type: long, initial: "0"}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: Split, kind: task, taskType: automated, name: "Race Start"}
        - {id: BidderA, kind: task, taskType: service, name: "Bidder A"}
        - {id: BidderB, kind: task, taskType: service, name: "Bidder B"}
        - {id: BidderC, kind: task, taskType: service, name: "Bidder C"}
        - {id: FirstWin, kind: task, taskType: automated, name: "First Bid Wins"}
        - {id: Consume, kind: task, taskType: automated, name: "Consume Others"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: Split}
        - {from: Split, to: BidderA}
        - {from: Split, to: BidderB}
        - {from: Split, to: BidderC}
        - {from: BidderA, to: FirstWin}
        - {from: BidderB, to: FirstWin}
        - {from: BidderC, to: FirstWin}
        - {from: FirstWin, to: Consume}
        - {from: Consume, to: End}
```

#### Petri Net Diagram Description

```
    [Start] --> [Split] --> [BidderA] --\
                 |           [BidderB] ---> [FirstWin] --> [Consume] --> [End]
                 |           [BidderC] --/
```

**Key Behavior:**
- First branch to complete triggers the output
- Remaining branches complete but don't re-trigger
- Discriminator resets after all branches complete

#### Execution Trace

```
Scenario: BidderB responds first
1. Fire t_split: all three bidders get tokens
2. Parallel execution begins
3. BidderB completes first:
   - Fire t_first_complete: p_triggered = [first_complete, 2]
   - Store winner_index = 2
4. Output fires immediately (doesn't wait for A or C)
5. Remaining bidders complete:
   - BidderA completes: consumed without trigger
   - BidderC completes: consumed without trigger
6. All consumed, reset discriminator ready for next cycle
```

#### Expected Outcome

- First responder wins
- Output generated on first completion
- Other branches are ignored (results discarded)
- Useful for: auctions, competitive bidding, first-response scenarios

---

### N-out-of-M Join (P30)

Proceeds when N out of M branches complete (partial join).

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "n_of_m_join_example"
  metaData:
    title: "N-out-of-M Join Pattern"
    version: "1.0"
    identifier: "UID_N_OF_M_001"

  rootNet: PartialJoinNet

  nets:
    - id: PartialJoinNet
      type: NetFacts
      variables:
        - {name: m_count, type: integer, initial: "5"}
        - {name: n_threshold, type: integer, initial: "3"}
        - {name: completed_count, type: integer, initial: "0"}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: Split, kind: task, taskType: automated, name: "Split to Reviewers"}
        - {id: Reviewer1, kind: task, taskType: human, name: "Reviewer 1"}
        - {id: Reviewer2, kind: task, taskType: human, name: "Reviewer 2"}
        - {id: Reviewer3, kind: task, taskType: human, name: "Reviewer 3"}
        - {id: Reviewer4, kind: task, taskType: human, name: "Reviewer 4"}
        - {id: Reviewer5, kind: task, taskType: human, name: "Reviewer 5"}
        - {id: PartialJoin, kind: task, taskType: automated, name: "3 of 5 Ready"}
        - {id: Proceed, kind: task, taskType: automated, name: "Proceed with Approval"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: Split}
        - {from: Split, to: Reviewer1}
        - {from: Split, to: Reviewer2}
        - {from: Split, to: Reviewer3}
        - {from: Split, to: Reviewer4}
        - {from: Split, to: Reviewer5}
        - {from: Reviewer1, to: PartialJoin}
        - {from: Reviewer2, to: PartialJoin}
        - {from: Reviewer3, to: PartialJoin}
        - {from: Reviewer4, to: PartialJoin}
        - {from: Reviewer5, to: PartialJoin}
        - {from: PartialJoin, to: Proceed}
        - {from: Proceed, to: End}
```

#### Petri Net Diagram Description

```
                      [Reviewer1] --\
                      [Reviewer2] ---> [PartialJoin] --> [Proceed] --> [End]
    [Start] --> [Split] [Reviewer3] ---> (N=3, M=5)
                      [Reviewer4] --/
                      [Reviewer5] --/
```

**Key Behavior:**
- Threshold N=3, Total M=5
- Join enables when 3 reviewers complete
- Does NOT wait for all 5

#### Execution Trace

```
1. Fire t_split: 5 reviewers get tokens
2. Reviewers complete at different times:
   - Reviewer2 completes: count = 1
   - Reviewer5 completes: count = 2
   - Reviewer1 completes: count = 3 --> THRESHOLD MET
3. Fire t_partial_join: p_proceed = [ready]
   - Doesn't wait for Reviewer3 or Reviewer4
4. Proceed with approval
5. Remaining reviewers complete later (results consumed)
```

#### Expected Outcome

- Quorum-based approval (3 of 5 sufficient)
- Faster than waiting for all M
- Useful for: peer review, voting, consensus systems
- Remaining branches can be cancelled or ignored

---

### Milestone (P18)

Activity is enabled only when a specific milestone state has been reached.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "milestone_example"
  metaData:
    title: "Milestone Pattern"
    version: "1.0"
    identifier: "UID_MILESTONE_001"

  rootNet: MilestoneNet

  nets:
    - id: MilestoneNet
      type: NetFacts
      variables:
        - {name: milestone_reached, type: boolean, initial: "false"}
        - {name: early_bird_deadline, type: datetime, initial: "2025-01-01T00:00:00Z"}
        - {name: current_time, type: datetime, initial: "2024-01-01T00:00:00Z"}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: CheckMilestone, kind: task, taskType: automated, name: "Check Milestone"}
        - {id: BeforeMilestone, kind: task, taskType: automated, name: "Before Deadline"}
        - {id: MilestoneGate, kind: condition, name: "Milestone Gate"}
        - {id: AfterMilestone, kind: task, taskType: automated, name: "After Deadline"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: CheckMilestone}
        - {from: CheckMilestone, to: MilestoneGate}
        - {from: MilestoneGate, to: BeforeMilestone}
        - {from: MilestoneGate, to: AfterMilestone}
        - {from: BeforeMilestone, to: End}
        - {from: AfterMilestone, to: End}
```

#### Petri Net Diagram Description

```
    [Start] --> [CheckMilestone] --> [MilestoneGate]
                                           |
                      +--------------------+--------------------+
                      |                                         |
                   [BeforeMilestone]                        [AfterMilestone]
                      |                                         |
                      +--------------------+--------------------+
                                           |
                                        [End]
```

**Key Behavior:**
- `MilestoneGate` acts as guard condition
- `BeforeMilestone` only enabled if `current_time < deadline`
- `AfterMilestone` only enabled if `current_time >= deadline`

#### Execution Trace

```
Scenario 1: Before milestone (early registration)
1. p_milestone_ready = [check]
2. Fire t_check: current_time < deadline
3. p_milestone_reached = [] (not reached)
4. Only BeforeMilestone branch enabled
5. Early bird pricing applied

Scenario 2: After milestone (regular registration)
1. p_milestone_ready = [check]
2. Fire t_check: current_time >= deadline
3. p_milestone_reached = [reached]
4. Only AfterMilestone branch enabled
5. Standard pricing applied
```

#### Expected Outcome

- Activities enabled/disabled based on milestone state
- Once milestone reached, it stays valid
- Useful for: deadlines, phased releases, state-based guards

---

## Cancellation Patterns

### Cancel Activity (P19)

Cancels a single specific activity while the workflow continues.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "cancel_activity_example"
  metaData:
    title: "Cancel Activity Pattern"
    version: "1.0"
    identifier: "UID_CANCEL_ACT_001"

  rootNet: CancelActNet

  nets:
    - id: CancelActNet
      type: NetFacts
      variables:
        - {name: cancel_event, type: boolean, initial: "false"}
        - {name: activity_cancelled, type: boolean, initial: "false"}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: StartActivity, kind: task, taskType: automated, name: "Start Long Task"}
        - {id: LongTask, kind: task, taskType: service, name: "Long Running Task"}
        - {id: CancelTrigger, kind: task, taskType: human, name: "Cancel Request"}
        - {id: CancelHandler, kind: task, taskType: automated, name: "Handle Cancel"}
        - {id: Continue, kind: task, taskType: automated, name: "Continue Workflow"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: StartActivity}
        - {from: StartActivity, to: LongTask}
        - {from: LongTask, to: Continue}
        - {from: CancelTrigger, to: CancelHandler}
        - {from: CancelHandler, to: Continue}
        - {from: Continue, to: End}
```

#### Petri Net Diagram Description

```
    [Start] --> [StartActivity] --> [LongTask] ---> [Continue] --> [End]
                            ^                        ^
                            |                        |
                 [CancelTrigger] --> [CancelHandler]
```

**Key Behavior:**
- `LongTask` can be cancelled mid-execution
- Cancel event removes token from `LongTask` place
- Workflow continues via alternative path

#### Execution Trace

```
Scenario 1: Normal completion
1. LongTask starts execution
2. No cancel event received
3. LongTask completes normally
4. Continue workflow

Scenario 2: Cancellation
1. LongTask starts execution
2. CancelRequest triggers:
   - p_cancel_event = [cancel]
   - Fire t_cancel: p_cancelled = [cancelled]
3. LongTask token removed, activity terminated
4. CancelHandler executes cleanup
5. Continue workflow via alternative path
```

#### Expected Outcome

- Single activity cancelled, workflow continues
- Other activities unaffected
- Useful for: user-initiated cancellation, timeout handling

---

### Cancel Case (P20)

Cancels the entire workflow case (all activities terminated).

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "cancel_case_example"
  metaData:
    title: "Cancel Case Pattern"
    version: "1.0"
    identifier: "UID_CANCEL_CASE_001"

  rootNet: CancelCaseNet

  nets:
    - id: CancelCaseNet
      type: NetFacts
      variables:
        - {name: emergency_stop, type: boolean, initial: "false"}
        - {name: case_cancelled, type: boolean, initial: "false"}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: EmergencyStop, kind: task, taskType: automated, name: "Emergency Stop"}
        - {id: Task1, kind: task, taskType: service, name: "Task 1"}
        - {id: Task2, kind: task, taskType: service, name: "Task 2"}
        - {id: Task3, kind: task, taskType: service, name: "Task 3"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: Task1}
        - {from: Task1, to: Task2}
        - {from: Task2, to: Task3}
        - {from: Task3, to: End}
        - {from: EmergencyStop, to: End}
```

#### Petri Net Diagram Description

```
    [Start] --> [Task1] --> [Task2] --> [Task3] --> [End]
       |
       +------------> [EmergencyStop] --------+
                                                |
                                              [CANCEL ALL]
```

**Key Behavior:**
- Emergency stop has immediate effect
- All active places cleared
- Workflow terminated in cancelled state

#### Execution Trace

```
Scenario: Emergency during execution
1. Task1 completes, Task2 starts
2. EmergencyStop triggered:
   - p_active = []
   - p_cancelled = [cancelled]
3. All tasks immediately terminated:
   - Task2 token removed
   - Task3 never starts
4. Workflow ends with cancelled status
```

#### Expected Outcome

- Entire workflow case cancelled
- All activities terminated immediately
- Useful for: force majeure, emergency shutdown, critical failures

---

## Multi-Instance Patterns

### Static MI (P13)

Fixed number of instances known at design time, with synchronization.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "static_mi_example"
  metaData:
    title: "Static Multi-Instance Pattern"
    version: "1.0"
    identifier: "UID_STATIC_MI_001"

  rootNet: StaticMINet

  nets:
    - id: StaticMINet
      type: NetFacts
      variables:
        - {name: instance_count, type: integer, initial: "3"}
        - {name: instances_completed, type: integer, initial: "0"}
        - {name: results, type: string, initial: ""}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: Spawn, kind: task, taskType: automated, name: "Spawn Reviews"}
        - {id: Review1, kind: task, taskType: human, name: "Reviewer 1"}
        - {id: Review2, kind: task, taskType: human, name: "Reviewer 2"}
        - {id: Review3, kind: task, taskType: human, name: "Reviewer 3"}
        - {id: Collect, kind: task, taskType: automated, name: "Collect Results"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: Spawn}
        - {from: Spawn, to: Review1}
        - {from: Spawn, to: Review2}
        - {from: Spawn, to: Review3}
        - {from: Review1, to: Collect}
        - {from: Review2, to: Collect}
        - {from: Review3, to: Collect}
        - {from: Collect, to: End}
```

#### Petri Net Diagram Description

```
                        [Review1] --\
    [Start] --> [Spawn] [Review2] ---> [Collect] --> [End]
                        [Review3] --/
```

**Key Behavior:**
- Exactly 3 instances (design time knowledge)
- Synchronization waits for ALL 3
- Results collected into array

#### Execution Trace

```
1. Fire t_spawn: create 3 instance tokens
2. Parallel execution:
   - Instance 1: Reviewer1 processes
   - Instance 2: Reviewer2 processes
   - Instance 3: Reviewer3 processes
3. Collect waits for all 3:
   - Reviewer1 completes: results = [result1]
   - Reviewer2 completes: results = [result1, result2]
   - Reviewer3 completes: results = [result1, result2, result3]
4. All complete, fire t_collect: output results
```

#### Expected Outcome

- Fixed number of parallel instances
- All instances must complete
- Synchronization on completion
- Useful for: peer review (fixed reviewers), batch processing

---

### Dynamic MI (P14)

Number of instances determined at runtime, with synchronization.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "dynamic_mi_example"
  metaData:
    title: "Dynamic Multi-Instance Pattern"
    version: "1.0"
    identifier: "UID_DYNAMIC_MI_001"

  rootNet: DynamicMINet

  nets:
    - id: DynamicMINet
      type: NetFacts
      variables:
        - {name: input_list, type: string, initial: "item1,item2,item3,item4,item5"}
        - {name: runtime_count, type: integer, initial: "0"}
        - {name: completed_count, type: integer, initial: "0"}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: CountItems, kind: task, taskType: automated, name: "Calculate Instances"}
        - {id: Spawn, kind: task, taskType: automated, name: "Spawn Instances"}
        - {id: ProcessItem, kind: task, taskType: service, name: "Process Item"}
        - {id: Collect, kind: task, taskType: automated, name: "Collect Results"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: CountItems}
        - {from: CountItems, to: Spawn}
        - {from: Spawn, to: ProcessItem}
        - {from: ProcessItem, to: Collect}
        - {from: Collect, to: End}
```

#### Petri Net Diagram Description

```
    [Start] --> [CountItems] --> [Spawn] --> [ProcessItem]* --> [Collect] --> [End]
                                          (dynamic count instances)
```

**Key Behavior:**
- CountItems calculates N from input data at runtime
- Spawn creates N instances of ProcessItem
- Synchronization waits for all N

#### Execution Trace

```
1. Input: "item1,item2,item3,item4,item5"
2. CountItems: runtime_count = 5
3. Spawn: create 5 instance tokens
4. ProcessItem executes 5 times in parallel:
   - Instance 1: processes item1
   - Instance 2: processes item2
   - Instance 3: processes item3
   - Instance 4: processes item4
   - Instance 5: processes item5
5. Each completes, increments completed_count
6. When completed_count == runtime_count:
   - Collect fires: all results gathered
7. End with aggregated results
```

#### Expected Outcome

- Instance count determined at runtime
- All instances must complete
- Synchronization on completion
- Useful for: batch processing, variable-sized collections

---

## Data Flow Patterns

### Data Distribution

Distribute data to multiple recipients/targets.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "data_distribute_example"
  metaData:
    title: "Data Distribution Pattern"
    version: "1.0"
    identifier: "UID_DATA_DIST_001"

  rootNet: DistributeNet

  nets:
    - id: DistributeNet
      type: NetFacts
      variables:
        - {name: message, type: string, initial: "Broadcast Message"}
        - {name: targets, type: string, initial: "A,B,C"}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: Distribute, kind: task, taskType: automated, name: "Distribute Data"}
        - {id: TargetA, kind: task, taskType: service, name: "Target A"}
        - {id: TargetB, kind: task, taskType: service, name: "Target B"}
        - {id: TargetC, kind: task, taskType: service, name: "Target C"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: Distribute}
        - {from: Distribute, to: TargetA}
        - {from: Distribute, to: TargetB}
        - {from: Distribute, to: TargetC}
        - {from: TargetA, to: End}
        - {from: TargetB, to: End}
        - {from: TargetC, to: End}
```

#### Petri Net Diagram Description

```
                        [TargetA] --\
    [Start] --> [Distribute] [TargetB] ---> [End]
                        [TargetC] --/
```

#### Execution Trace

```
1. Input data: message = "Broadcast Message"
2. Fire t_distribute:
   - p_targets = [(message, A), (message, B), (message, C)]
3. Each target receives copy:
   - TargetA processes message
   - TargetB processes message
   - TargetC processes message
4. All targets complete independently
```

#### Expected Outcome

- Same data sent to all targets
- One-to-many distribution
- Targets process independently
- Useful for: broadcasts, notifications, parallel processing

---

### Data Accumulation

Collect and aggregate data from multiple sources.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "data_accumulate_example"
  metaData:
    title: "Data Accumulation Pattern"
    version: "1.0"
    identifier: "UID_DATA_ACCUM_001"

  rootNet: AccumulateNet

  nets:
    - id: AccumulateNet
      type: NetFacts
      variables:
        - {name: sources, type: string, initial: "source1,source2,source3"}
        - {name: accumulated_result, type: string, initial: ""}

      nodes:
        - {id: Start, kind: inputCondition}
        - {id: SourceA, kind: task, taskType: service, name: "Source A"}
        - {id: SourceB, kind: task, taskType: service, name: "Source B"}
        - {id: SourceC, kind: task, taskType: service, name: "Source C"}
        - {id: Accumulate, kind: task, taskType: automated, name: "Accumulate Results"}
        - {id: End, kind: outputCondition}

      flows:
        - {from: Start, to: SourceA}
        - {from: Start, to: SourceB}
        - {from: Start, to: SourceC}
        - {from: SourceA, to: Accumulate}
        - {from: SourceB, to: Accumulate}
        - {from: SourceC, to: Accumulate}
        - {from: Accumulate, to: End}
```

#### Petri Net Diagram Description

```
    [SourceA] --\
    [SourceB] ---> [Accumulate] --> [End]
    [SourceC] --/
```

#### Execution Trace

```
1. Parallel data collection:
   - SourceA produces: dataA = 10
   - SourceB produces: dataB = 20
   - SourceC produces: dataC = 30
2. Fire t_accumulate with accumulator_fun:
   - Result = foldl(fun(Acc, X) -> Acc + X end, 0, [10, 20, 30])
   - Result = 60
3. Output: accumulated_result = 60
```

#### Expected Outcome

- Many-to-one data collection
- Aggregation using accumulator function
- Useful for: summation, reduction, gathering results

---

## Complete Workflow

### Order Fulfillment

A comprehensive workflow combining multiple patterns.

#### YAML Specification

```yaml
yawl_yaml_version: "0.2"
specificationSet:
  yawl_schema_version: "2.1"
  uri: "order_fulfillment"
  metaData:
    title: "Order Fulfillment Workflow"
    version: "1.0"
    creator: "CRE Examples"
    description: "Complete order processing with approval, payment, and shipping"
    identifier: "UID_ORDER_FULL_001"

  rootNet: OrderFulfillment

  roles:
    - Customer
    - SalesRep
    - Finance
    - Warehouse
    - Shipping

  nets:
    - id: OrderFulfillment
      type: NetFacts
      variables:
        - {name: order_id, type: string, initial: ""}
        - {name: order_total, type: decimal, initial: "0.00"}
        - {name: payment_received, type: boolean, initial: "false"}
        - {name: approval_granted, type: boolean, initial: "false"}
        - {name: items_picked, type: boolean, initial: "false"}
        - {name: shipment_sent, type: boolean, initial: "false"}

      nodes:
        # Start/End
        - {id: Start, kind: inputCondition}
        - {id: End, kind: outputCondition}

        # Order entry (Sequence)
        - {id: ReceiveOrder, kind: task, taskType: human, name: "Receive Order"}

        # Approval branch (Exclusive Choice)
        - {id: CheckApproval, kind: task, taskType: automated, name: "Check Approval Needed"}
        - {id: AutoApprove, kind: task, taskType: automated, name: "Auto Approve"}
        - {id: ManualApproval, kind: task, taskType: human, name: "Manual Approval"}

        # Parallel processing (Parallel Split + Sync)
        - {id: SplitProcessing, kind: task, taskType: automated, name: "Split Processing"}
        - {id: ProcessPayment, kind: task, taskType: service, name: "Process Payment"}
        - {id: PickItems, kind: task, taskType: human, name: "Pick Items"}
        - {id: ArrangeShipping, kind: task, taskType: human, name: "Arrange Shipping"}
        - {id: JoinProcessing, kind: task, taskType: automated, name: "Join Processing"}

        # Shipping (Multi-instance for multiple packages)
        - {id: CreatePackages, kind: task, taskType: automated, name: "Create Packages"}
        - {id: ShipPackage, kind: task, taskType: service, name: "Ship Package"}
        - {id: AllShipped, kind: task, taskType: automated, name: "All Shipped"}

        # Completion
        - {id: SendInvoice, kind: task, taskType: service, name: "Send Invoice"}
        - {id: ArchiveOrder, kind: task, taskType: automated, name: "Archive Order"}

      flows:
        # Main sequence
        - {from: Start, to: ReceiveOrder}
        - {from: ReceiveOrder, to: CheckApproval}

        # Exclusive choice for approval
        - {from: CheckApproval, to: AutoApprove}
        - {from: CheckApproval, to: ManualApproval}
        - {from: AutoApprove, to: SplitProcessing}
        - {from: ManualApproval, to: SplitProcessing}

        # Parallel split for processing
        - {from: SplitProcessing, to: ProcessPayment}
        - {from: SplitProcessing, to: PickItems}
        - {from: SplitProcessing, to: ArrangeShipping}

        # Synchronization
        - {from: ProcessPayment, to: JoinProcessing}
        - {from: PickItems, to: JoinProcessing}
        - {from: ArrangeShipping, to: JoinProcessing}
        - {from: JoinProcessing, to: CreatePackages}

        # Multi-instance shipping
        - {from: CreatePackages, to: ShipPackage}
        - {from: ShipPackage, to: AllShipped}

        # Completion sequence
        - {from: AllShipped, to: SendInvoice}
        - {from: SendInvoice, to: ArchiveOrder}
        - {from: ArchiveOrder, to: End}

      regions:
        - {id: CancellationRegion, cancel_region: true, description: "Can cancel entire order"}

  pattern_instances:
    # P4: Exclusive Choice - approval path
    - {id: P4_approval_choice, pattern: P4_ExclusiveChoice, net: OrderFulfillment,
       at: CheckApproval, choices: [auto_approve, manual_approve],
       label: "P4: Auto vs Manual approval"}

    # P2: Parallel Split - processing
    - {id: P2_parallel_processing, pattern: P2_ParallelSplit, net: OrderFulfillment,
       split_task: SplitProcessing, branches: [ProcessPayment, PickItems, ArrangeShipping],
       label: "P2: Parallel order processing"}

    # P3: Synchronization - join
    - {id: P3_processing_sync, pattern: P3_Synchronization, net: OrderFulfillment,
       join_task: JoinProcessing, waits_for: [ProcessPayment, PickItems, ArrangeShipping],
       label: "P3: Wait for all processing"}

    # P13: Static MI - shipping packages
    - {id: P13_package_shipping, pattern: P13_MI_DesignTime, net: OrderFulfillment,
       task: ShipPackage, instances: 3,
       label: "P13: Ship 3 packages"}

    # P19: Cancel Activity
    - {id: P19_cancel_order, pattern: P19_CancelActivity, net: OrderFulfillment,
       target: ReceiveOrder, cancel_event: customer_cancel,
       label: "P19: Cancel single activity"}

    # P20: Cancel Case
    - {id: P20_cancel_fulfillment, pattern: P20_CancelCase, net: OrderFulfillment,
       cancel_event: emergency_cancel,
       label: "P20: Cancel entire order"}

  pattern_registry:
    P2_ParallelSplit: {macro: "parallel_split"}
    P3_Synchronization: {macro: "and_join"}
    P4_ExclusiveChoice: {macro: "xor_choice"}
    P13_MI_DesignTime: {macro: "mi_design_time"}
    P19_CancelActivity: {macro: "cancel_activity"}
    P20_CancelCase: {macro: "cancel_case"}
```

#### Petri Net Diagram Description

```
                                  [ManualApproval] --\
[Start] --> [ReceiveOrder] --> [CheckApproval] -----> [SplitProcessing] --+--> [ProcessPayment] --\
                                  |                                       +--> [PickItems] --------> [JoinProcessing] --> [CreatePackages] --> [ShipPackage]* --> [AllShipped] --> [SendInvoice] --> [ArchiveOrder] --> [End]
                                  [AutoApprove] --/                       +--> [ArrangeShipping] --/

* = Multi-instance (static N)
```

#### Execution Trace

```
1. [Sequence] ReceiveOrder:
   - Order #1234 received, order_total = $1500

2. [Exclusive Choice] CheckApproval:
   - order_total > $1000, requires approval
   - Fire t_select_manual: ManualApproval selected

3. ManualApproval:
   - Sales rep reviews and approves
   - approval_granted = true

4. [Parallel Split] SplitProcessing:
   - Fire t_split: 3 parallel tokens created

5. Parallel execution:
   a) ProcessPayment: Finance processes payment
      - payment_received = true
   b) PickItems: Warehouse picks items
      - items_picked = true
   c) ArrangeShipping: Shipping arranges carrier
      - carrier = "FedEx", tracking assigned

6. [Synchronization] JoinProcessing:
   - Waits for all 3 branches
   - All complete, fire t_join

7. [Static MI] CreatePackages:
   - Order has 3 packages
   - Spawn 3 ShipPackage instances

8. Multi-instance shipping:
   - Package 1: shipped, tracking1 = "FDX123456789"
   - Package 2: shipped, tracking2 = "FDX123456790"
   - Package 3: shipped, tracking3 = "FDX123456791"
   - AllShipped waits for all 3

9. [Sequence] Completion:
   - SendInvoice: invoice sent to customer
   - ArchiveOrder: order archived

10. [End] Workflow complete
```

#### Expected Outcome

- Order processed through multiple stages
- Approval based on order value
- Payment, picking, and shipping happen in parallel
- Multiple packages shipped in parallel
- Invoice sent and order archived
- All patterns coordinate seamlessly

---

## Running These Examples

### Using the Erlang API

```erlang
%% Start a sequence workflow
{ok, Pid} = sequence:start(#{from => task_a, to => task_b}),
{ok, Result} = sequence:run(Pid).

%% Start a parallel split
BranchFuns = [
    fun() -> task_a() end,
    fun() -> task_b() end,
    fun() -> task_c() end
],
{ok, Results} = parallel_split:execute(BranchFuns, input_data).

%% Start exclusive choice
Branches = #{
    premium => fun(Data) -> handle_premium(Data) end,
    standard => fun(Data) -> handle_standard(Data) end
},
{ok, {Selected, Result}} = exclusive_choice:execute(Branches, OrderData).

%% Start discriminator (first wins)
RaceFuns = [
    fun() -> bidder_a() end,
    fun() -> bidder_b() end,
    fun() -> bidder_c() end
],
{ok, {WinnerIndex, Result}} = discriminator:execute(RaceFuns, BidData).

%% Start multi-instance (static)
InstanceFuns = [
    fun(Item1) -> process(Item1) end,
    fun(Item2) -> process(Item2) end,
    fun(Item3) -> process(Item3) end
],
{ok, Results} = multi_instance:multiple_instances_design_time(
    fun process/1,
    [item1, item2, item3],
    3
).

%% Data distribution
Targets = [target_a, target_b, target_c],
{ok, DistResults} = data_distribute:distribute(
    message_data,
    Targets,
    fun(Data, Target) -> send_to(Data, Target) end
).

%% Data accumulation
Sources = [1, 2, 3, 4, 5],
Accumulator = fun(Acc, X) -> Acc + X end,
{ok, Sum} = data_accumulate:accumulate(Sources, Accumulator, 0).
```

### Loading from YAML

```erlang
%% Load and compile a YAWL specification
{ok, Spec} = yawl_schema:load_file("/path/to/workflow.yaml"),
{ok, Compiled} = yawl_compile:compile(Spec),

%% Start the workflow
{ok, Pid} = gen_yawl:start_link(Compiled, #{}, []),

%% Execute workflow
{ok, Result} = gen_yawl:sync(Pid, 30000).
```

---

## Pattern Reference

| Pattern ID | Pattern Name | Module | Description |
|------------|--------------|--------|-------------|
| P1 | Sequence | `sequence` | Sequential execution of tasks |
| P2 | Parallel Split | `parallel_split` | Split into concurrent branches |
| P3 | Synchronization | `synchronization` | AND-join all branches |
| P4 | Exclusive Choice | `exclusive_choice` | Select one of many branches |
| P9 | Discriminator | `discriminator` | First completion wins |
| P13 | Static MI | `multi_instance` | Fixed N instances, design time |
| P14 | Dynamic MI | `multi_instance` | N instances, runtime |
| P18 | Milestone | `milestone` | Enable on milestone reached |
| P19 | Cancel Activity | `cancel_activity` | Cancel single activity |
| P20 | Cancel Case | `cancel_case` | Cancel entire workflow |
| P30 | N-of-M Join | `blocking_partial_join` | Partial join (N of M) |
| WDP-03 | Data Distribute | `data_distribute` | One-to-many data |
| WDP-04 | Data Accumulate | `data_accumulate` | Many-to-one data |

---

## Further Reading

- Pattern Documentation: `/Users/sac/cre/docs/patterns/`
- Source Code: `/Users/sac/cre/src/patterns/`
- Test Fixtures: `/Users/sac/cre/test/fixtures/`
- AGI Symposium Example: `/Users/sac/cre/test/fixtures/agi_symposium_omega.yaml`
