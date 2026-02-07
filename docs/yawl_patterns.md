# YAWL Pattern Reference Documentation

**Version:** CRE 0.2.0
**Last Updated:** 2026-02-06
**Total Patterns:** 36 implemented of 43 YAWL workflow patterns

---

## Overview

This document provides a comprehensive reference for all YAWL (Yet Another Workflow Language) workflow control patterns implemented in the Common Runtime Environment (CRE). Patterns are organized by category with their pattern IDs, descriptions, module names, and API functions.

## Pattern Categories

| Category | ID Range | Implemented | Description |
|----------|----------|-------------|-------------|
| **Basic Control Flow** | WCP-01 to WCP-10 | 10/10 | Fundamental workflow structures |
| **Advanced Synchronization** | WCP-11 to WCP-17 | 7/7 | Complex synchronization and multiple instances |
| **State-Based Patterns** | WCP-18 to WCP-20 | 3/3 | State-dependent workflow behavior |
| **Extended Control Flow** | WCP-21 to WCP-28 | 6/8 | Advanced flow control structures |
| **Data Flow Patterns** | WDP-01 to WDP-05 | 5/5 | Data transformation and movement |
| **Resource Patterns** | WRP-01 to WRP-05 | 5/5 | Resource management and allocation |
| **Exception Handling** | WHP-01 to WHP-05 | 5/5 | Error handling and recovery |

---

## 1. Basic Control Flow Patterns (WCP-01 to WCP-10)

### WCP-01: Sequence

**Description:** Execute tasks in a strict sequential order where each task completes before the next begins.

**Module:** `cre_yawl` (core)

**API Functions:**
- `cre_yawl:add_task/3` - Add tasks to workflow
- `cre_yawl:connect/3` - Connect tasks in sequence

**Petri Net Structure:**
```
p_start -> t_task1 -> p_task1 -> t_task2 -> p_task2 -> p_end
```

---

### WCP-02: Parallel Split

**Description:** Split workflow execution into multiple parallel branches that execute concurrently.

**Module:** `parallel_split`

**API Functions:**
- `parallel_split:place_lst/0` - List of Petri net places
- `parallel_split:trsn_lst/0` - List of Petri net transitions
- `parallel_split:preset/1` - Input places for transition
- `parallel_split:is_enabled/3` - Check if transition enabled
- `parallel_split:init_marking/2` - Initial token distribution
- `parallel_split:fire/3` - Transition firing

**Petri Net Structure:**
```
p_start -> t_split -> p_branch1
                     -> p_branch2
                     -> p_branch3
                     -> p_branch4
```

---

### WCP-03: Synchronization

**Description:** Wait for multiple parallel branches to complete before proceeding.

**Module:** `or_join`

**API Functions:**
- `or_join:place_lst/0` - List of Petri net places
- `or_join:trsn_lst/0` - List of Petri net transitions
- `or_join:preset/1` - Input places for transition
- `or_join:is_enabled/3` - Check if transition enabled
- `or_join:init_marking/2` - Initial token distribution
- `or_join:fire/3` - Transition firing

**Petri Net Structure:**
```
p_branch1 -> t_sync
p_branch2 ->       -> p_merged
p_branch3 ->
```

---

### WCP-04: Exclusive Choice

**Description:** Select one branch from multiple alternatives based on conditions (XOR semantics).

**Module:** `exclusive_choice`

**API Functions:**
- `exclusive_choice:place_lst/0` - List of Petri net places
- `exclusive_choice:trsn_lst/0` - List of Petri net transitions
- `exclusive_choice:preset/1` - Input places for transition
- `exclusive_choice:is_enabled/3` - Check if transition enabled
- `exclusive_choice:init_marking/2` - Initial token distribution
- `exclusive_choice:fire/3` - Transition firing

**Petri Net Structure:**
```
p_start -> t_choice -> p_branch1 (condition A)
                  -> p_branch2 (condition B)
                  -> p_branch3 (condition C)
```

---

### WCP-05: Simple Merge

**Description:** Merge multiple incoming paths into single output (XOR semantics).

**Module:** `simple_merge`

**API Functions:**
- `simple_merge:place_lst/0` - List of Petri net places
- `simple_merge:trsn_lst/0` - List of Petri net transitions
- `simple_merge:preset/1` - Input places for transition
- `simple_merge:is_enabled/3` - Check if transition enabled
- `simple_merge:init_marking/2` - Initial token distribution
- `simple_merge:fire/3` - Transition firing

**Petri Net Structure:**
```
p_branch1 -> t_merge -> p_output
p_branch2 ->
p_branch3 ->
```

---

### WCP-06: Multi-Choice

**Description:** Enable multiple branches simultaneously (OR semantics).

**Module:** `multiple_choice`

**API Functions:**
- `multiple_choice:place_lst/0` - List of Petri net places
- `multiple_choice:trsn_lst/0` - List of Petri net transitions
- `multiple_choice:preset/1` - Input places for transition
- `multiple_choice:is_enabled/3` - Check if transition enabled
- `multiple_choice:init_marking/2` - Initial token distribution
- `multiple_choice:fire/3` - Transition firing

**Petri Net Structure:**
```
p_start -> t_choice -> p_branch1
                  -> p_branch2
                  -> p_branch3
```

---

### WCP-07: Synchronizing Merge

**Description:** Wait for all branches to complete (AND semantics) with intermediate result processing.

**Module:** `multiple_merge`

**API Functions:**
- `multiple_merge:place_lst/0` - List of Petri net places
- `multiple_merge:trsn_lst/0` - List of Petri net transitions
- `multiple_merge:preset/1` - Input places for transition
- `multiple_merge:is_enabled/3` - Check if transition enabled
- `multiple_merge:init_marking/2` - Initial token distribution
- `multiple_merge:fire/3` - Transition firing

**Petri Net Structure:**
```
p_branch1 -> t_sync -> p_partial1
p_branch2 ->        -> p_partial2
p_branch3 ->        -> p_final
```

---

### WCP-08: Multi-Merge

**Description:** Collect results from multiple paths and process them.

**Module:** `implicit_merge`

**API Functions:**
- `implicit_merge:place_lst/0` - List of Petri net places
- `implicit_merge:trsn_lst/0` - List of Petri net transitions
- `implicit_merge:preset/1` - Input places for transition
- `implicit_merge:is_enabled/3` - Check if transition enabled
- `implicit_merge:init_marking/2` - Initial token distribution
- `implicit_merge:fire/3` - Transition firing
- `implicit_merge:new/2` - Create new implicit merge state

**Petri Net Structure:**
```
p_branch1 -> t_merge -> p_results
p_branch2 ->
p_branch3 ->
```

---

### WCP-09: Discriminator

**Description:** Accept first completion and discard subsequent branches.

**Module:** `discriminator`

**API Functions:**
- `discriminator:place_lst/0` - List of Petri net places
- `discriminator:trsn_lst/0` - List of Petri net transitions
- `discriminator:preset/1` - Input places for transition
- `discriminator:is_enabled/3` - Check if transition enabled
- `discriminator:init_marking/2` - Initial token distribution
- `discriminator:fire/3` - Transition firing

**Petri Net Structure:**
```
p_branch1 -> t_discriminate -> p_selected
p_branch2 ->                -> p_discarded
p_branch3 ->
```

---

### WCP-10: N-out-of-M

**Description:** Proceed when N out of M parallel branches complete.

**Module:** `n_out_of_m`

**API Functions:**
- `n_out_of_m:place_lst/0` - List of Petri net places
- `n_out_of_m:trsn_lst/0` - List of Petri net transitions
- `n_out_of_m:preset/1` - Input places for transition
- `n_out_of_m:is_enabled/3` - Check if transition enabled
- `n_out_of_m:init_marking/2` - Initial token distribution
- `n_out_of_m:fire/3` - Transition firing

**Petri Net Structure:**
```
p_start -> t_split -> p_branch_pool
         -> t_execute -> p_running
         -> t_complete -> p_completed
         -> t_check_quorum -> p_quorum_met
```

---

## 2. Advanced Synchronization & Multiple Instances (WCP-11 to WCP-17)

### WCP-11: Implicit Termination

**Description:** Terminate subprocess when no work remains and all inputs are satisfied.

**Module:** `implicit_termination`

**API Functions:**
- `cre_yawl_patterns:implicit_termination/1` - Create pattern state
- `cre_yawl_patterns:execute_implicit_termination/2` - Execute pattern

**Petri Net Structure:**
```
p_start -> t_activate -> p_active
         -> t_queue_work -> p_work_pending
         -> t_dequeue_work -> p_work
         -> t_implicit_term -> p_terminate
```

---

### WCP-12: Multiple Instances without Synchronization

**Description:** Create concurrent instances without synchronizing after completion.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:multiple_instances_no_sync/3` - Create pattern state
- `cre_yawl_patterns:execute_multiple_instances_no_sync/3` - Execute pattern

**Parameters:**
- `Subprocess` - Module or function for each instance
- `InstanceCount` - Number of instances to create
- `InputData` - Data to distribute among instances

---

### WCP-13: Multiple Instances with Design Time Knowledge

**Description:** Create fixed number of instances known at design time, synchronized on completion.

**Module:** `multiple_instances_sync`

**API Functions:**
- `cre_yawl_patterns:multiple_instances_static/3` - Create pattern state
- `cre_yawl_patterns:execute_multiple_instances_static/3` - Execute pattern

**Parameters:**
- `Subprocess` - Module or function for each instance
- `InstanceCount` - Fixed number of instances
- `InputData` - List of input data, one per instance

---

### WCP-14: Multiple Instances with Runtime Knowledge

**Description:** Create instances where count is determined at runtime but before creation.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:multiple_instances_runtime/3` - Create pattern state
- `cre_yawl_patterns:execute_multiple_instances_runtime/3` - Execute pattern

**Parameters:**
- `Subprocess` - Module or function for each instance
- `CountFun` - Function that determines instance count from input data
- `InputData` - Data for evaluating count and distributing to instances

---

### WCP-15: Multiple Instances without Prior Knowledge

**Description:** Dynamically create instances during execution based on data availability.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:multiple_instances_dynamic/3` - Create pattern state
- `cre_yawl_patterns:execute_multiple_instances_dynamic/3` - Execute pattern

**Parameters:**
- `Subprocess` - Module or function for each instance
- `DataFun` - Function that returns stream of data for instance creation
- `InitialData` - Initial data for the first instances

---

### WCP-16: Deferred Choice

**Description:** Defer choice between alternatives until runtime based on data availability.

**Module:** `deferred_choice`

**API Functions:**
- `cre_yawl_patterns:deferred_choice/3` - Create pattern state
- `cre_yawl_patterns:execute_deferred_choice/3` - Execute pattern

**Parameters:**
- `Options` - Map of option identifiers to their subprocess modules/functions
- `ConditionFun` - Function that evaluates which option to select
- `InitialData` - Initial data for condition evaluation

---

### WCP-17: Interleaved Parallel Routing

**Description:** Execute multiple branches in interleaved, non-deterministic order.

**Module:** `interleaved_routing`

**API Functions:**
- `cre_yawl_patterns:interleaved_routing/2` - Create pattern state
- `cre_yawl_patterns:execute_interleaved_routing/2` - Execute pattern

**Parameters:**
- `Branches` - Map of branch identifiers to their subprocess modules
- `InitialData` - Initial data for all branches

---

## 3. State-Based Patterns (WCP-18 to WCP-20)

### WCP-18: Milestone

**Description:** Enable activity only if a specific milestone has been reached.

**Module:** `milestone`

**API Functions:**
- `cre_yawl_patterns:milestone/2` - Create pattern state

**Parameters:**
- `Activity` - The activity module/function to guard with milestone
- `MilestoneFun` - Function checking if milestone reached (returns boolean)

**Petri Net Structure:**
```
p_milestone_guard -> t_set_milestone -> p_milestone_reached
                  -> t_enable_activity -> p_activity_enabled -> t_complete -> p_activity_complete
```

---

### WCP-19: Cancel Activity

**Description:** Allow cancellation of a specific running activity based on external events.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:cancel_activity/2` - Create pattern state

**Parameters:**
- `Activity` - The activity that can be cancelled
- `CancelFun` - Function determining cancellation condition

---

### WCP-20: Cancel Case

**Description:** Allow cancellation of the entire workflow case, terminating all activities.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:cancel_case/2` - Create pattern state

**Parameters:**
- `Activities` - List of activities in the case
- `CancelFun` - Function determining case cancellation condition

---

## 4. Extended Control Flow Patterns (WCP-21 to WCP-28)

### WCP-21: Structured Synchronization

**Description:** Define a synchronized block where multiple concurrent activities must complete before proceeding.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:structured_sync/2` - Create pattern state

**Parameters:**
- `Activities` - List of activities to synchronize
- `InitialData` - Initial data for all activities

---

### WCP-22: Partial Join (Structured Partial Join)

**Description:** Wait for a subset of parallel branches to complete before proceeding.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:partial_join/2` - Create pattern state

**Parameters:**
- `Activities` - List of activities in partial join
- `Quorum` - Number of activities required for continuation

---

### WCP-23: Structured Loop

**Description:** Implement while/until loop constructs with proper Petri net semantics.

**Module:** `structured_loop`

**API Functions:**
- `cre_yawl_patterns:structured_loop/3` - Create pattern state

**Parameters:**
- `BodyTask` - The task to execute in each iteration
- `LoopType` - 'while' or 'until'
- `ConditionFun` - Function returning true/false for loop control

---

### WCP-24: Recursion

**Description:** Enable recursive workflow invocation with base case detection.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:recursion/2` - Create pattern state
- `cre_yawl_patterns:execute_recursion/2` - Execute pattern

**Parameters:**
- `RecursiveFun` - The recursive function implementing the workflow
- `BaseCaseFun` - Function detecting base case for termination

---

### WCP-25: Interleaved Loop

**Description:** Combine looping with parallel execution in interleaved fashion.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:interleaved_loop/2` - Create pattern state
- `cre_yawl_patterns:execute_interleaved_loop/2` - Execute pattern

**Parameters:**
- `Activities` - List of activities for interleaved parallel execution
- `ConditionFun` - Function determining loop continuation

---

### WCP-26: Critical Section

**Description:** Ensure mutually exclusive access to a shared resource.

**Module:** `critical_section`

**API Functions:**
- `cre_yawl_patterns:critical_section/2` - Create pattern state
- `cre_yawl_patterns:execute_critical_section/2` - Execute pattern

**Parameters:**
- `CriticalActivity` - The activity requiring mutual exclusion
- `LockId` - Identifier for the lock/semaphore

**Petri Net Structure:**
```
p_cs_request -> t_cs_acquire -> p_cs_lock -> t_cs_execute -> p_cs_active -> t_cs_release -> p_cs_release
```

---

### WCP-27: Protocol Pattern

**Description:** Implement request-response communication between workflow participants.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:protocol_pattern/3` - Create pattern state

**Parameters:**
- `RequestFun` - Function generating the request
- `ResponseHandlerFun` - Function handling the response
- `Timeout` - Timeout in milliseconds (or 'infinity')

---

### WCP-28: Try-Catch

**Description:** Implement exception handling with protected region (try) and exception handling region (catch).

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:try_catch/3` - Create pattern state

**Parameters:**
- `TryFun` - The protected function to attempt
- `CatchFun` - Function handling exceptions
- `ExceptionTypes` - List of exception types to catch ('_' for all)

---

## 5. Data Flow Patterns (WDP-01 to WDP-05)

### WDP-01: Parameter Passing

**Description:** Pass parameters between tasks in a workflow.

**Module:** `param_pass`

**API Functions:**
- `cre_yawl_patterns:param_pass/1` - Create pattern state
- `cre_yawl_patterns:execute_param_pass/2` - Execute pattern

**Parameters:**
- `Params` - Map of parameter names to values

---

### WDP-02: Data Transformation

**Description:** Transform data between tasks with custom logic.

**Module:** `data_transform`

**API Functions:**
- `cre_yawl_patterns:data_transform/2` - Create pattern state
- `cre_yawl_patterns:execute_data_transform/2` - Execute pattern

**Parameters:**
- `TransformFun` - Function for data transformation
- `InputData` - Data to transform

---

### WDP-03: Data Distribution

**Description:** Distribute data to multiple tasks.

**Module:** `data_distribute`

**API Functions:**
- `cre_yawl_patterns:data_distribute/2` - Create pattern state
- `cre_yawl_patterns:execute_data_distribute/2` - Execute pattern

**Parameters:**
- `Data` - Data to distribute
- `Recipients` - List of recipient identifiers

---

### WDP-04: Data Accumulation

**Description:** Accumulate data from multiple tasks.

**Module:** `data_accumulate`

**API Functions:**
- `cre_yawl_patterns:data_accumulate/2` - Create pattern state
- `cre_yawl_patterns:execute_data_accumulate/2` - Execute pattern

**Parameters:**
- `DataList` - List of data items to accumulate
- `AggregateFun` - Function for data aggregation

---

### WDP-05: Data Visibility

**Description:** Control data visibility scope within the workflow.

**Module:** `data_visibility`

**API Functions:**
- `cre_yawl_patterns:data_visibility/2` - Create pattern state
- `cre_yawl_patterns:execute_data_visibility/2` - Execute pattern

**Parameters:**
- `Data` - Data to control visibility for
- `VisibilityFun` - Function determining visibility rules

---

## 6. Resource Patterns (WRP-01 to WRP-05)

### WRP-01: Direct Resource Creation

**Description:** Create resources on-demand within workflow.

**Module:** `direct_resource_creation`

**API Functions:**
- `cre_yawl_patterns:direct_resource_creation/1` - Create pattern state
- `cre_yawl_patterns:execute_direct_resource_creation/1` - Execute pattern

**Parameters:**
- `CreateFun` - Function that creates the resource

---

### WRP-02: Role-Based Allocation

**Description:** Allocate resources based on role definitions.

**Module:** `role_based_allocation`

**API Functions:**
- `cre_yawl_patterns:role_based_allocation/2` - Create pattern state
- `cre_yawl_patterns:execute_role_based_allocation/2` - Execute pattern

**Parameters:**
- `Role` - Role identifier to allocate
- `RoleRegistry` - Map of roles to available resources

---

### WRP-03: Resource Initialization

**Description:** Initialize and configure resources before use.

**Module:** `resource_initialization`

**API Functions:**
- `cre_yawl_patterns:resource_initialization/2` - Create pattern state
- `cre_yawl_patterns:execute_resource_initialization/2` - Execute pattern

**Parameters:**
- `InitFun` - Function that initializes the resource
- `Resource` - Resource to initialize

---

### WRP-04: Resource Allocation

**Description:** Assign resources to tasks and track availability.

**Module:** `resource_allocation`

**API Functions:**
- `cre_yawl_patterns:resource_allocation/2` - Create pattern state
- `cre_yawl_patterns:execute_resource_allocation/2` - Execute pattern

**Parameters:**
- `ResourcePool` - List of available resources
- `TaskId` - Task requiring resource allocation

---

### WRP-05: Resource Deallocation

**Description:** Release resources after task completion.

**Module:** `resource_deallocation`

**API Functions:**
- `cre_yawl_patterns:resource_deallocation/2` - Create pattern state
- `cre_yawl_patterns:execute_resource_deallocation/2` - Execute pattern

**Parameters:**
- `CleanupFun` - Function that cleanup/releases the resource
- `Resource` - Resource to deallocate

---

## 7. Exception Handling Patterns (WHP-01 to WHP-05)

### WHP-01: Error Handler

**Description:** Provide catch-all mechanism for handling unexpected errors.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:error_handler/2` - Create pattern state

**Parameters:**
- `ProtectedActivity` - The activity to protect with error handling
- `ErrorHandlerFun` - Function to call when error occurs

---

### WHP-02: Retry

**Description:** Automatically retry failed activities with configurable retry limits and backoff.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:retry/3` - Create pattern state

**Parameters:**
- `Activity` - The activity to retry on failure
- `MaxRetries` - Maximum number of retry attempts
- `BackoffFun` - Function calculating delay before next retry

---

### WHP-03: Compensation

**Description:** Allow undoing of completed activities when downstream error occurs.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:compensate/2` - Create pattern state

**Parameters:**
- `CompensableActivity` - The activity that can be compensated
- `CompensatorFun` - Function that undoes the activity

---

### WHP-04: Triggered Compensation

**Description:** Allow explicit triggering of compensation based on specific events.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:triggered_compensation/3` - Create pattern state

**Parameters:**
- `Activity` - The compensable activity
- `CompensatorFun` - Function that undoes the activity
- `TriggerFun` - Function determining when to trigger compensation

---

### WHP-05: Consecutive Compensation

**Description:** Execute multiple compensation handlers in reverse order.

**Module:** `cre_yawl_patterns`

**API Functions:**
- `cre_yawl_patterns:consecutive_compensate/1` - Create pattern state

**Parameters:**
- `ActivityCompensatorPairs` - List of {Activity, Compensator} tuples

---

## 8. Unimplemented Patterns (7 Remaining)

The following patterns from the YAWL specification are not yet implemented:

| Pattern ID | Pattern Name | Category |
|------------|--------------|----------|
| WCP-29 | Structured Discriminator | Extended Control Flow |
| WCP-30 | Structured N-out-of-M Join | Extended Control Flow |
| WCP-31 | Arbitrary Cycle | Advanced Control Flow |
| WCP-32 | Recursion with Previous State | Advanced Control Flow |
| WCP-33 | Cancel Region | State-Based |
| WCP-34 | Cancel Multiple Instance | State-Based |
| WCP-35 | Advanced Cancel Activity | State-Based |

---

## Quick Reference Table

| ID | Name | Module | Category |
|----|------|--------|----------|
| WCP-01 | Sequence | cre_yawl | Basic Control |
| WCP-02 | Parallel Split | parallel_split | Basic Control |
| WCP-03 | Synchronization | or_join | Basic Control |
| WCP-04 | Exclusive Choice | exclusive_choice | Basic Control |
| WCP-05 | Simple Merge | simple_merge | Basic Control |
| WCP-06 | Multi-Choice | multiple_choice | Basic Control |
| WCP-07 | Synchronizing Merge | multiple_merge | Advanced Sync |
| WCP-08 | Multi-Merge | implicit_merge | Advanced Sync |
| WCP-09 | Discriminator | discriminator | Advanced Sync |
| WCP-10 | N-out-of-M | n_out_of_m | Advanced Sync |
| WCP-11 | Implicit Termination | implicit_termination | Multiple Instance |
| WCP-12 | MI without Sync | cre_yawl_patterns | Multiple Instance |
| WCP-13 | MI Static | multiple_instances_sync | Multiple Instance |
| WCP-14 | MI Runtime | cre_yawl_patterns | Multiple Instance |
| WCP-15 | MI Dynamic | cre_yawl_patterns | Multiple Instance |
| WCP-16 | Deferred Choice | deferred_choice | Multiple Instance |
| WCP-17 | Interleaved Routing | interleaved_routing | Multiple Instance |
| WCP-18 | Milestone | milestone | State-Based |
| WCP-19 | Cancel Activity | cre_yawl_patterns | State-Based |
| WCP-20 | Cancel Case | cre_yawl_patterns | State-Based |
| WCP-21 | Structured Sync | cre_yawl_patterns | Extended Control |
| WCP-22 | Partial Join | cre_yawl_patterns | Extended Control |
| WCP-23 | Structured Loop | structured_loop | Extended Control |
| WCP-24 | Recursion | cre_yawl_patterns | Extended Control |
| WCP-25 | Interleaved Loop | cre_yawl_patterns | Extended Control |
| WCP-26 | Critical Section | critical_section | Extended Control |
| WCP-27 | Protocol | cre_yawl_patterns | Extended Control |
| WCP-28 | Try-Catch | cre_yawl_patterns | Extended Control |
| WDP-01 | Parameter Passing | param_pass | Data Flow |
| WDP-02 | Data Transform | data_transform | Data Flow |
| WDP-03 | Data Distribute | data_distribute | Data Flow |
| WDP-04 | Data Accumulate | data_accumulate | Data Flow |
| WDP-05 | Data Visibility | data_visibility | Data Flow |
| WRP-01 | Resource Creation | direct_resource_creation | Resource |
| WRP-02 | Role Allocation | role_based_allocation | Resource |
| WRP-03 | Resource Init | resource_initialization | Resource |
| WRP-04 | Resource Alloc | resource_allocation | Resource |
| WRP-05 | Resource Dealloc | resource_deallocation | Resource |
| WHP-01 | Error Handler | cre_yawl_patterns | Exception |
| WHP-02 | Retry | cre_yawl_patterns | Exception |
| WHP-03 | Compensation | cre_yawl_patterns | Exception |
| WHP-04 | Triggered Comp | cre_yawl_patterns | Exception |
| WHP-05 | Consecutive Comp | cre_yawl_patterns | Exception |

---

## Module Index

- `cre_yawl` - Core YAWL engine and basic patterns
- `cre_yawl_patterns` - Advanced pattern implementations
- `parallel_split` - WCP-02 Parallel Split
- `or_join` - WCP-03 Synchronization
- `exclusive_choice` - WCP-04 Exclusive Choice
- `simple_merge` - WCP-05 Simple Merge
- `multiple_choice` - WCP-06 Multi-Choice
- `multiple_merge` - WCP-07 Synchronizing Merge
- `implicit_merge` - WCP-08 Multi-Merge
- `discriminator` - WCP-09 Discriminator
- `n_out_of_m` - WCP-10 N-out-of-M
- `implicit_termination` - WCP-11 Implicit Termination
- `multiple_instances_sync` - WCP-13 MI Static
- `deferred_choice` - WCP-16 Deferred Choice
- `interleaved_routing` - WCP-17 Interleaved Routing
- `milestone` - WCP-18 Milestone
- `structured_loop` - WCP-23 Structured Loop
- `critical_section` - WCP-26 Critical Section
- `param_pass` - WDP-01 Parameter Passing
- `data_transform` - WDP-02 Data Transform
- `data_distribute` - WDP-03 Data Distribute
- `data_accumulate` - WDP-04 Data Accumulate
- `data_visibility` - WDP-05 Data Visibility
- `direct_resource_creation` - WRP-01 Resource Creation
- `role_based_allocation` - WRP-02 Role Allocation
- `resource_initialization` - WRP-03 Resource Init
- `resource_allocation` - WRP-04 Resource Allocation
- `resource_deallocation` - WRP-05 Resource Deallocation

---

## References

1. van der Aalst, W. M. P., ter Hofstede, A. H. M., Kiepuszewski, B., & Barros, A. P. (2003). Workflow patterns. *Distributed and Parallel Databases*, 14(1), 5-51.

2. Russell, N., ter Hofstede, A. H. M., van der Aalst, W. M. P., & Mulyar, N. (2006). *Workflow control-flow patterns: A revised view*. BPM Center Report.

3. Workflow Patterns Initiative: https://www.workflowpatterns.com

---

**Document Version:** 1.0
**Generated:** 2026-02-06
**CRE Version:** 0.2.0
