# YAWL Workflow Patterns - Complete Reference

## Overview

YAWL (Yet Another Workflow Language) is a comprehensive workflow language based on Petri nets and Workflow Patterns. This document provides a complete reference to all 43 workflow patterns from the Workflow Patterns Initiative, organized into logical categories.

The Workflow Patterns Initiative was established to formally define and catalog the range of control-flow, data-flow, and resource-oriented constructs required to support contemporary workflow processes. These patterns serve as a design resource for workflow system developers and as an evaluation framework for workflow languages.

**Pattern Categories:**
- Basic Control Flow Patterns (WCP 1-6)
- Advanced Synchronization Patterns (WCP 7-10)
- Structural Patterns (WCP 11-13)
- Multiple Instance Patterns (WCP 14-17)
- State-Based Patterns (WCP 18-20)
- Extended Control Flow Patterns (WCP 21-28)
- Data Flow Patterns (WDP 1-5)
- Resource Patterns (WRP 1-5)
- Exception Handling Patterns (WHP 1-5)

---

## Pattern Categories

---

### 1. Basic Control Flow Patterns (1-6)

#### WCP-1: Sequence

**Description**: The most fundamental workflow pattern where activities are executed in a sequential order. An activity can only start when its predecessor has completed.

**Petri Net Representation**:
```
     t1       t2       t3
  p0 ---> p1 ---> p2 ---> p3
```
Where:
- `p0`: Initial place (contains token)
- `p1, p2`: Intermediate places
- `p3`: Final place
- `t1, t2, t3`: Transitions (activities)

**YAWL Representation**:
```yaml
condition: p0
task: t1 (condition: p0, output: p1)
task: t2 (condition: p1, output: p2)
task: t3 (condition: p2, output: p3)
```

**Use Case Example**:
```erlang
%% Order Processing Sequence
process_order(Order) ->
    Order1 = validate_order(Order),
    Order2 = check_inventory(Order1),
    Order3 = calculate_payment(Order2),
    confirm_order(Order3).
```

**Related Patterns**: WCP-2 (Parallel Split), WCP-4 (Exclusive Choice)

---

#### WCP-2: Parallel Split

**Description**: A point in the workflow where a single thread of control splits into multiple parallel threads that can execute concurrently. All outgoing branches are activated simultaneously.

**Petri Net Representation**:
```
        t1
      / | \
     /  |  \
   p1  p2  p3
   |   |   |
  t2  t3  t4
```

**YAWL Representation**:
```yaml
task: split (type: AND-split)
  outputs: [p1, p2, p3]
```

**Use Case Example**:
```erlang
%% Parallel order processing
process_order_parallel(Order) ->
    Parent = self(),
    spawn(fun() -> Parent ! {inventory, check_inventory(Order)} end),
    spawn(fun() -> Parent ! {payment, verify_payment(Order)} end),
    spawn(fun() -> Parent ! {shipping, arrange_shipping(Order)} end),
    collect_results().
```

**Related Patterns**: WCP-3 (Synchronization), WCP-22 (Structured Partial Join)

---

#### WCP-3: Synchronization

**Description**: A point in the workflow where multiple parallel threads converge into a single thread. The workflow continues only after all incoming branches have completed.

**Petri Net Representation**:
```
   p1   p2   p3
    |   |   |
    \  |  /
     \ | /
       t1
       |
       p4
```

**YAWL Representation**:
```yaml
task: join (type: AND-join)
  inputs: [p1, p2, p3]
  output: p4
  join_code: all  % All incoming branches must complete
```

**Use Case Example**:
```erlang
%% Synchronize parallel order checks
synchronize_checks() ->
    receive
        {inventory, ok} -> inventory_done();
        {inventory, error} -> inventory_failed()
    end,
    receive
        {payment, ok} -> payment_done();
        {payment, error} -> payment_failed()
    end,
    receive
        {shipping, ok} -> shipping_done();
        {shipping, error} -> shipping_failed()
    end,
    proceed_to_fulfillment().
```

**Related Patterns**: WCP-2 (Parallel Split), WCP-22 (Structured Partial Join)

---

#### WCP-4: Exclusive Choice

**Description**: A point in the workflow where exactly one branch is chosen from multiple alternatives based on data-based or event-based selection criteria.

**Petri Net Representation**:
```
       t1
      / | \
     /  |  \
    c1 c2 c3  (conditions)
    |  |  |
   t2 t3 t4
```

**YAWL Representation**:
```yaml
task: choice (type: OR-split)
  outputs: [p1, p2, p3]
  predicate:
    p1: amount > 1000
    p2: amount > 500
    p3: true
```

**Use Case Example**:
```erlang
%% Choose approval level based on amount
select_approval_level(Order) ->
    case Order#order.amount of
        Amount when Amount > 10000 -> executive_approval();
        Amount when Amount > 5000 -> manager_approval();
        Amount when Amount > 1000 -> supervisor_approval();
        _ -> auto_approve()
    end.
```

**Related Patterns**: WCP-5 (Simple Merge), WCP-16 (Deferred Choice)

---

#### WCP-5: Simple Merge

**Description**: A point in the workflow where multiple alternative branches converge without synchronization. The workflow continues as soon as ANY one incoming branch completes.

**Petri Net Representation**:
```
   p1   p2   p3
    |   |   |
    \  |  /
     \ | /
       t1
       |
       p4
```

**YAWL Representation**:
```yaml
task: merge (type: XOR-join)
  inputs: [p1, p2, p3]
  output: p4
  join_code: first  % First incoming branch to complete
```

**Use Case Example**:
```erlang
%% Merge after alternative payment methods
merge_payment_result() ->
    receive
        {credit_card, Result} -> process_payment(Result);
        {paypal, Result} -> process_payment(Result);
        {bank_transfer, Result} -> process_payment(Result)
    end.
```

**Related Patterns**: WCP-4 (Exclusive Choice), WCP-7 (Synchronizing Merge)

---

#### WCP-6: Multi-Choice

**Description**: A point in the workflow where multiple branches can be activated simultaneously, but not necessarily all branches. The selection is based on data or events.

**Petri Net Representation**:
```
       t1
      / | \
     /  |  \
    c1 c2 c3  (independent conditions)
    |  |  |
   t2 t3 t4
```

**YAWL Representation**:
```yaml
task: multi_choice (type: OR-split)
  outputs: [p1, p2, p3, p4]
  predicate:
    p1: is_vip_customer()
    p2: amount > 1000
    p3: requires_shipping()
    p4: true
```

**Use Case Example**:
```erlang
%% Multi-choice for order processing options
select_order_processes(Order) ->
    Processes = [],
    Processes = case is_vip(Order) of
        true -> [vip_handling | Processes];
        false -> Processes
    end,
    Processes = case Order#order.amount > 1000 of
        true -> [approval_required | Processes];
        false -> Processes
    end,
    Processes = case needs_shipping(Order) of
        true -> [shipping | Processes];
        false -> Processes
    end,
    spawn_processes(Processes).
```

**Related Patterns**: WCP-4 (Exclusive Choice), WCP-8 (Multi-Merge)

---

### 2. Advanced Synchronization Patterns (7-10)

#### WCP-7: Synchronizing Merge

**Description**: Multiple threads converge into a single thread, but the merge only occurs when ALL active branches have completed. Inactive branches are ignored.

**Petri Net Representation**:
```
   p1   p2   p3
    |   |   |
    \  |  /
     \ | /
       t1
     / | \
    /  |  \
   c1  c2  c3  (track active branches)
```

**YAWL Representation**:
```yaml
task: sync_merge (type: OR-join)
  inputs: [p1, p2, p3]
  output: p4
  join_code: all_active
```

**Use Case Example**:
```erlang
%% Synchronizing merge for optional order steps
-record(state, {expected = 0, completed = 0}).

synchronizing_merge(ExpectedBranches) ->
    State = #state{expected = ExpectedBranches},
    sync_loop(State).

sync_loop(#state{expected = E, completed = C} = State) when E =:= C ->
    all_complete();
sync_loop(State) ->
    receive
        {branch_complete, _} ->
            sync_loop(State#state{completed = State#state.completed + 1})
    end.
```

**Related Patterns**: WCP-3 (Synchronization), WCP-5 (Simple Merge)

---

#### WCP-8: Multi-Merge

**Description**: Multiple threads merge into a single thread without synchronization, but unlike simple merge, all incoming branches are processed independently and each triggers the subsequent activity.

**Petri Net Representation**:
```
   p1   p2   p3
    |   |   |
    |   |   +--> t1a --> p4
    |   +-------> t1b --> p5
    +-----------> t1c --> p6
```

**YAWL Representation**:
```yaml
task: multi_merge (type: multi-instance)
  inputs: [p1, p2, p3]
  outputs: [p4, p5, p6]
  merging: parallel
```

**Use Case Example**:
```erlang
%% Multi-merge for handling multiple notifications
handle_notifications(NotificationStreams) ->
    lists:foreach(
        fun(Stream) ->
            spawn(fun() -> process_notification(Stream) end)
        end,
        NotificationStreams
    ).
```

**Related Patterns**: WCP-5 (Simple Merge), WCP-7 (Synchronizing Merge)

---

#### WCP-9: Discriminator

**Description**: After parallel execution, the workflow continues as soon as ONE of the incoming branches completes. Once triggered, the discriminator waits for ALL remaining branches to complete without allowing further execution.

**Petri Net Representation**:
```
   p1   p2   p3
    |   |   |
    +---+---+---> t1 (trigger on first complete)
    |   |   |
    d   d   d   (drain remaining tokens)
```

**YAWL Representation**:
```yaml
task: discriminator (type: discriminator)
  inputs: [p1, p2, p3]
  output: p4
  behavior: first_triggers_then_wait_all
```

**Use Case Example**:
```erlang
%% Discriminator for quote collection
-record(discriminator, {triggered = false, remaining}).

collect_quotes(Vendors) ->
    Discriminator = #discriminator{remaining = length(Vendors)},
    spawn_vendors(Vendors),
    discriminator_loop(Discriminator).

discriminator_loop(#discriminator{triggered = false} = D) ->
    receive
        {quote, Quote} ->
            proceed_with_quote(Quote),
            drain_remaining(D#discriminator{triggered = true, remaining = D#discriminator.remaining - 1})
    end;
discriminator_loop(#discriminator{triggered = true, remaining = 0}) ->
    all_drained();
discriminator_loop(#discriminator{triggered = true} = D) ->
    receive
        {quote, _} ->
            discriminator_loop(D#discriminator{remaining = D#discriminator.remaining - 1})
    end.
```

**Related Patterns**: WCP-3 (Synchronization), WCP-5 (Simple Merge)

---

#### WCP-10: Arbitrary Cycles

**Description**: A point in the workflow where a thread can return to a previous activity in the workflow, creating loops. Unlike structured loops, these cycles can be entered and exited at multiple points.

**Petri Net Representation**:
```
    t1       t4
    ^       ^
    |       |
    t2 ---> t3
    |       |
    +-------+
```

**YAWL Representation**:
```yaml
task: loop (type: unstructured)
  from: t2
  to: t1
  condition: retry_needed()
```

**Use Case Example**:
```erlang
%% Arbitrary cycle for document approval
approve_document(Document) ->
    case submit_for_approval(Document) of
        approved ->
            finalize_document(Document);
        {rejected, Reason} ->
            Revised = revise_document(Document, Reason),
            approve_document(Revised);  % Loop back
        {changes_requested, Changes} ->
            Revised = apply_changes(Document, Changes),
            approve_document(Revised)   % Loop back
    end.
```

**Related Patterns**: WCP-23 (Structured Loop), WCP-25 (Arbitrary Interleaved Loop)

---

### 3. Structural Patterns (11-13)

#### WCP-11: Implicit Termination

**Description**: A workflow terminates when there are no remaining activities to be executed. No explicit termination activity is required; the workflow ends "implicitly" when all active threads complete.

**Petri Net Representation**:
```
   p1   p2   p3
    |   |   |
   t1  t2  t3
    |   |   |
    \   |   /
     \  |  /
      (implicit termination when no tokens remain)
```

**YAWL Representation**:
```yaml
workflow: auto_terminate
  termination: implicit
  condition: no_active_tasks
```

**Use Case Example**:
```erlang
%% Implicit termination with supervisor
implicit_termination_supervisor() ->
    process_flag(trap_exit, true),
    spawn_children(),
    terminate_when_all_done().

terminate_when_all_done() ->
    Children = get(children),
    case Children of
        [] ->
            cleanup(),
            exit(normal);
        _ ->
            receive
                {'EXIT', _Pid, _Reason} ->
                    remove_child(_Pid),
                    terminate_when_all_done()
            end
    end.
```

**Related Patterns**: WCP-19 (Cancel Activity), WCP-20 (Cancel Case)

---

#### WCP-12: Multiple Instances (without synchronization)

**Description**: An activity is instantiated multiple times without any synchronization between the instances. Each instance runs independently and the workflow continues immediately without waiting for all instances to complete.

**Petri Net Representation**:
```
       t1
       |
       v
    +--t2--+  (multiple concurrent instances)
    |  |  |
    t2 t2 t2
```

**YAWL Representation**:
```yaml
task: multi_instance (type: deferred)
  creation: dynamic
  synchronization: none
```

**Use Case Example**:
```erlang
%% Fire-and-forget notifications
send_notifications(Recipients, Message) ->
    lists:foreach(
        fun(Recipient) ->
            spawn(fun() -> send_notification(Recipient, Message) end)
        end,
        Recipients
    ),
    continue_immediately().  % Don't wait for notifications
```

**Related Patterns**: WCP-13 (Multiple Instances with a priori knowledge), WCP-14 (Multiple Instances with runtime knowledge)

---

#### WCP-13: Multiple Instances (with a priori design time knowledge)

**Description**: An activity is instantiated multiple times where the number of instances is known at design time. All instances must complete before the workflow continues.

**Petri Net Representation**:
```
       t1
       |
       v
    +--t2--+  (n instances, n known at design time)
    |  |  |
    v  v  v
  (synchronization barrier)
       |
       v
       t3
```

**YAWL Representation**:
```yaml
task: multi_instance (type: static)
  cardinality: 3  % Known at design time
  synchronization: all
```

**Use Case Example**:
```erlang
%% Fixed number of parallel approvals
require_three_signatures(Document) ->
    Approvers = [finance, legal, management],
    Pids = [spawn_link(Approver, approve, [Document]) || Approver <- Approvers],
    await_all_signatures(Pids).

await_all_signatures(Pids) ->
    [receive {Pid, signature} -> ok end || Pid <- Pids],
    all_signatures_complete().
```

**Related Patterns**: WCP-12 (Multiple Instances without synchronization), WCP-14 (Multiple Instances with runtime knowledge)

---

### 4. Multiple Instance Patterns (14-17)

#### WCP-14: Multiple Instances (with a priori runtime knowledge)

**Description**: An activity is instantiated multiple times where the number of instances is determined at runtime (before instances are created). All instances must complete before continuing.

**Petri Net Representation**:
```
       t1
       |
       v
    [determine n at runtime]
       |
       v
    +--t2--+  (n instances)
    |  |  |
    v  v  v
  (synchronization barrier)
       |
       v
       t3
```

**YAWL Representation**:
```yaml
task: multi_instance (type: dynamic)
  cardinality: evaluate_at_runtime
  source: recipient_list
  synchronization: all
```

**Use Case Example**:
```erlang
%% Runtime-determined parallel processing
process_for_all_recipients(Recipients, Document) ->
    NumInstances = length(Recipients),
    Pids = [spawn_link(fun() -> process_recipient(R, Document) end)
            || R <- Recipients],
    await_completions(NumInstances, Pids).

await_completions(N, Pids) when length(Pids) =:= N ->
    [receive {Pid, done} -> ok end || Pid <- Pids],
    all_complete().
```

**Related Patterns**: WCP-13 (Multiple Instances with design time knowledge), WCP-15 (Multiple Instances without runtime knowledge)

---

#### WCP-15: Multiple Instances (without a priori runtime knowledge)

**Description**: An activity is instantiated multiple times where the number of instances is NOT known at runtime. Instances may be created dynamically during execution. All instances must complete before continuing.

**Petri Net Representation**:
```
       t1
       |
       v
    +--t2--+  (dynamic creation during execution)
    |  |  |
    +--+--+
    |  |  |
    v  v  v
  (synchronization barrier)
       |
       v
       t3
```

**YAWL Representation**:
```yaml
task: multi_instance (type: dynamic)
  cardinality: unknown
  creation: during_execution
  synchronization: all
```

**Use Case Example**:
```erlang
%% Dynamic instance creation for quote requests
request_quotes(Product) ->
    % Start with known vendors
    KnownVendors = get_known_vendors(Product),
    [spawn_vendor_request(V, Product) || V <- KnownVendors],

    % Additional vendors may be discovered during process
    UnknownVendors = discover_vendors_during(Product),
    [spawn_vendor_request(V, Product) || V <- UnknownVendors],

    % We don't know total count upfront
    await_all_quotes().

await_all_quotes() ->
    receive
        {no_more_vendors} ->
            all_quotes_collected();
        {new_vendor, Vendor} ->
            spawn_vendor_request(Vendor),
            await_all_quotes()
    end.
```

**Related Patterns**: WCP-14 (Multiple Instances with runtime knowledge), WCP-12 (Multiple Instances without synchronization)

---

#### WCP-16: Deferred Choice

**Description**: A point in the workflow where one branch is chosen from multiple alternatives based on events that occur at runtime, not on data. The first event to occur determines the chosen branch.

**Petri Net Representation**:
```
       t1
       |
       v
      / \
     /   \
   e1     e2  (competing events)
   |      |
  t2      t3
```

**YAWL Representation**:
```yaml
task: deferred_choice (type: event-based)
  alternatives:
    - event: timeout
      action: escalate
    - event: response_received
      action: proceed
    - event: cancellation
      action: terminate
```

**Use Case Example**:
```erlang
%% Deferred choice for auction bidding
deferred_auction_choice(Auction) ->
    Ref = make_ref(),
    Self = self(),

    % Set up timeout
    erlang:send_after(Auction#auction.timeout, Self, {Ref, timeout}),

    % Wait for first event
    receive
        {Ref, timeout} ->
            handle_timeout(Auction);
        {Ref, {bid, Bid}} ->
            handle_winning_bid(Auction, Bid);
        {Ref, cancel} ->
            handle_cancellation(Auction)
    end.
```

**Related Patterns**: WCP-4 (Exclusive Choice), WCP-17 (Interleaved Parallel Routing)

---

#### WCP-17: Interleaved Parallel Routing

**Description**: Multiple activities are enabled concurrently, but execution is restricted such that only one activity can be active at a time. Activities can interleave but not run in parallel.

**Petri Net Representation**:
```
       t1
       |
       v
     [mutex]
     / | \
    v  v  v
   t2 t3 t4  (only one active at a time)
    ^  ^  ^
     \ | /
     [mutex]
       |
       v
       t5
```

**YAWL Representation**:
```yaml
task: interleaved (type: mutex)
  activities: [t2, t3, t4]
  constraint: single_instance
  ordering: any
```

**Use Case Example**:
```erlang
%% Interleaved document editing with mutex
interleaved_editing(Editors, Document) ->
    Mutex = spawn(fun() -> mutex_loop(locked) end),
    [spawn(fun() -> editor_loop(Editor, Document, Mutex) end)
     || Editor <- Editors].

editor_loop(Editor, Document, Mutex) ->
    request_access(Mutex),
    edit_document(Editor, Document),
    release_access(Mutex),
    editor_loop(Editor, Document, Mutex).

mutex_loop(State) ->
    receive
        {request, From} when State =:= free ->
            From ! {granted, self()},
            mutex_loop(locked);
        {release, From} when State =:= locked ->
            From ! {released, self()},
            mutex_loop(free);
        {request, From} ->
            From ! {wait, self()},
            mutex_loop(State)
    end.
```

**Related Patterns**: WCP-2 (Parallel Split), WCP-26 (Critical Section)

---

### 5. State-Based Patterns (18-20)

#### WCP-18: Milestone

**Description**: An activity can only execute if a specific milestone has been reached. The milestone represents a particular state in the workflow process.

**Petri Net Representation**:
```
   [milestone_place]
       |
       v
     guard
       |
       v
       t1
```

**YAWL Representation**:
```yaml
task: milestone_reached
  id: approval_obtained
  guards:
    - activity: t1
      condition: milestone_reached(approval_obtained)
```

**Use Case Example**:
```erlang
%% Milestone-based activity execution
-record(state, {milestones = []}).

execute_after_milestone(Activity, Milestone) ->
    State = get_state(),
    case lists:member(Milestone, State#state.milestones) of
        true ->
            execute_activity(Activity);
        false ->
            {error, milestone_not_reached, Milestone}
    end.

set_milestone(Milestone) ->
    State = get_state(),
    NewMilestones = [Milestone | State#state.milestones],
    put_state(State#state{milestones = NewMilestones}).
```

**Related Patterns**: WCP-16 (Deferred Choice), WCP-26 (Critical Section)

---

#### WCP-19: Cancel Activity

**Description**: A running activity can be cancelled (interrupted) before completion. The workflow continues with alternative processing.

**Petri Net Representation**:
```
       t1
       |
       v
   +---t2---+  (cancellable)
   |        |
 cancel   complete
   |        |
   v        v
  t3a      t3b
```

**YAWL Representation**:
```yaml
task: cancellable
  id: t2
  cancellation:
    - event: user_cancel
      target: t2
    - event: timeout
      target: t2
```

**Use Case Example**:
```erlang
%% Cancellable long-running task
cancellable_task(Task) ->
    Parent = self(),
    TaskPid = spawn_link(fun() -> execute_task(Task, Parent) end),
    cancellable_loop(TaskPid, Task).

cancellable_loop(TaskPid, Task) ->
    receive
        {cancel, Reason} ->
            exit(TaskPid, kill),
            handle_cancellation(Task, Reason);
        {TaskPid, complete, Result} ->
            handle_completion(Task, Result);
        {TaskPid, progress, P} ->
            update_progress(P),
            cancellable_loop(TaskPid, Task)
    after
        Task#task.timeout ->
            exit(TaskPid, kill),
            handle_timeout(Task)
    end.
```

**Related Patterns**: WCP-20 (Cancel Case), WCP-28 (Try-Catch)

---

#### WCP-20: Cancel Case

**Description**: The entire workflow case (process instance) can be cancelled. All active activities are terminated and the workflow instance is removed.

**Petri Net Representation**:
```
   t1    t2    t3
    |    |    |
    +----+----+
         |
    [cancel_case]
         |
         v
    (termination)
```

**YAWL Representation**:
```yaml
workflow: cancellable
  cancellation:
    - event: emergency_shutdown
      scope: case
      cleanup: terminate_all_activities
```

**Use Case Example**:
```erlang
%% Cancel entire workflow case
cancel_case(CaseId, Reason) ->
    case_registry:lookup(CaseId),
    Activities = case_registry:get_activities(CaseId),
    lists:foreach(
        fun(ActivityPid) ->
            exit(ActivityPid, {case_cancelled, Reason})
        end,
        Activities
    ),
    case_registry:cleanup(CaseId),
    log_cancellation(CaseId, Reason).

%% Supervisor that handles case cancellation
case_supervisor(CaseId) ->
    process_flag(trap_exit, true),
    case_registry:register(CaseId, self()),
    supervise_loop(CaseId).

supervise_loop(CaseId) ->
    receive
        {cancel, Reason} ->
            cancel_case(CaseId, Reason),
            exit({case_cancelled, Reason});
        {'EXIT', _Pid, _Reason} ->
            handle_activity_exit(),
            supervise_loop(CaseId)
    end.
```

**Related Patterns**: WCP-19 (Cancel Activity), WCP-11 (Implicit Termination)

---

### 6. Extended Control Flow Patterns (21-28)

#### WCP-21: Structured Synchronization

**Description**: Multiple parallel threads synchronize in a structured manner, maintaining proper nesting and avoiding non-structured jumps. This pattern enforces single-entry, single-exit blocks.

**Petri Net Representation**:
```
       split
       / | \
      v  v  v
     t1 t2 t3
      \ | /
       v
      join
```

**YAWL Representation**:
```yaml
block: structured_sync
  type: and_join
  activities: [t1, t2, t3]
  nesting: proper
```

**Use Case Example**:
```erlang
%% Structured synchronization with gen_server
-behaviour(gen_server).

structured_sync(Activities) ->
    gen_server:call(?MODULE, {sync_activities, Activities}).

handle_call({sync_activities, Activities}, _From, State) ->
    % Spawn all activities
    Pids = [spawn_monitor(fun() -> A() end) || A <- Activities],
    % Wait for all to complete
    Results = await_all(Pids),
    {reply, Results, State}.

await_all([]) -> [];
await_all([{Pid, MRef} | Rest]) ->
    receive
        {'DOWN', MRef, process, Pid, Result} ->
            [Result | await_all(Rest)]
    end.
```

**Related Patterns**: WCP-3 (Synchronization), WCP-22 (Structured Partial Join)

---

#### WCP-22: Structured Partial Join

**Description**: A subset of parallel threads synchronize while others continue independently. The partial join maintains structure while allowing for selective synchronization.

**Petri Net Representation**:
```
       split
       / | \
      v  v  v
     t1 t2 t3
      \ |
       v
      partial_join
       |
       v
      t4
      /
     t3 (continues)
```

**YAWL Representation**:
```yaml
block: partial_join
  join_subset: [t1, t2]
  continue: [t3]
  next: t4
```

**Use Case Example**:
```erlang
%% Partial join for order processing
partial_join_order() ->
    % Start parallel activities
    InventoryPid = spawn(fun() -> check_inventory() end),
    CreditPid = spawn(fun() -> check_credit() end),
    ShippingPid = spawn(fun() -> arrange_shipping() end),

    % Join only inventory and credit before proceeding
    await_both(InventoryPid, CreditPid),
    confirm_order(),

    % Shipping continues independently
    receive
        {ShippingPid, done} -> shipping_complete()
    end.

await_both(Pid1, Pid2) ->
    Ref1 = erlang:monitor(process, Pid1),
    Ref2 = erlang:monitor(process, Pid2),
    await_both_loop(Ref1, Ref2, {false, false}).

await_both_loop(Ref1, Ref2, {true, true}) ->
    erlang:demonitor(Ref1),
    erlang:demonitor(Ref2),
    both_complete();
await_both_loop(Ref1, Ref2, {Done1, Done2}) ->
    receive
        {'DOWN', Ref1, process, _, _} ->
            await_both_loop(Ref1, Ref2, {true, Done2});
        {'DOWN', Ref2, process, _, _} ->
            await_both_loop(Ref1, Ref2, {Done1, true})
    end.
```

**Related Patterns**: WCP-3 (Synchronization), WCP-21 (Structured Synchronization)

---

#### WCP-23: Structured Loop

**Description**: A well-structured loop construct with explicit entry and exit conditions. The loop can repeat a set of activities until a condition is met.

**Petri Net Representation**:
```
       t1
       |
       v
    +--t2--+
    |      |
    v      |
   test    |
    |      |
   true    |
    |      |
    t2     |
    |      |
    v      |
   false   |
    |      |
    v      |
    t3     |
    +------+
```

**YAWL Representation**:
```yaml
block: structured_loop
  activities: [t2]
  condition: test
  while: true
```

**Use Case Example**:
```erlang
%% Structured loop with clear entry/exit
structured_loop(State) ->
    structured_loop_init(State).

structured_loop_init(State) ->
    case should_continue(State) of
        true ->
            NewState = execute_loop_body(State),
            structured_loop_init(NewState);
        false ->
            structured_loop_exit(State)
    end.

% Example: Retry pattern with max attempts
retry_loop(Task, MaxAttempts) ->
    retry_loop(Task, MaxAttempts, 0).

retry_loop(_Task, MaxAttempts, Attempt) when Attempt >= MaxAttempts ->
    {error, max_attempts_exceeded};
retry_loop(Task, MaxAttempts, Attempt) ->
    case execute_task(Task) of
        {ok, Result} ->
            {ok, Result};
        {error, _Reason} when Attempt < MaxAttempts - 1 ->
            retry_loop(Task, MaxAttempts, Attempt + 1)
    end.
```

**Related Patterns**: WCP-10 (Arbitrary Cycles), WCP-25 (Arbitrary Interleaved Loop)

---

#### WCP-24: Recursion

**Description**: An activity can invoke itself or a parent activity, creating recursive execution patterns. This allows for nested execution of the same workflow structure.

**Petri Net Representation**:
```
       t1
       |
       v
      [t2]
       |
       v
   (invoke t2)
       |
       v
      [t2]
       |
       v
    (return)
```

**YAWL Representation**:
```yaml
task: recursive
  id: t2
  recursion:
    - invoke: self
      condition: has_subitems()
```

**Use Case Example**:
```erlang
%% Recursive workflow for tree processing
process_tree(Tree) when is_list(Tree) ->
    [process_node(Node) || Node <- Tree];
process_tree({node, Value, Children}) ->
    % Process current node
    Result = process_value(Value),
    % Recursively process children
    ChildResults = process_tree(Children),
    {node, Result, ChildResults};
process_tree(Leaf) ->
    process_leaf(Leaf).

%% Recursive subprocess spawning
recursive_subprocess(Depth) when Depth =< 0 ->
    base_case();
recursive_subprocess(Depth) ->
    spawn_subprocess(fun() -> recursive_subprocess(Depth - 1) end).
```

**Related Patterns**: WCP-23 (Structured Loop), WCP-12 (Multiple Instances)

---

#### WCP-25: Arbitrary Interleaved Loop

**Description**: Multiple activities can be executed in an interleaved manner within a loop. The loop allows for complex execution ordering where activities may be revisited in various sequences.

**Petri Net Representation**:
```
    +-----t1-----+
    |     |      |
    v     v      |
   t2<--->t3<-->t4  (interleaved execution)
    ^     ^      ^
    |     |      |
    +-----|------+
          |
      (exit condition)
```

**YAWL Representation**:
```yaml
block: interleaved_loop
  activities: [t1, t2, t3, t4]
  ordering: arbitrary
  condition: continue_processing
```

**Use Case Example**:
```erlang
%% Interleaved loop for collaborative editing
interleaved_collaboration(Users, Document) ->
    ActiveUsers = Users,
    interleaved_loop(ActiveUsers, Document).

interleaved_loop([], Document) ->
    {complete, Document};
interleaved_loop(ActiveUsers, Document) ->
    receive
        {edit, User, Edit} ->
            case lists:member(User, ActiveUsers) of
                true ->
                    NewDoc = apply_edit(Document, Edit),
                    broadcast_update(ActiveUsers, NewDoc),
                    interleaved_loop(ActiveUsers, NewDoc);
                false ->
                    interleaved_loop(ActiveUsers, Document)
            end;
        {join, User} ->
            interleaved_loop([User | ActiveUsers], Document);
        {leave, User} ->
            interleaved_loop(ActiveUsers -- [User], Document);
        {complete} ->
            {complete, Document}
    end.
```

**Related Patterns**: WCP-17 (Interleaved Parallel Routing), WCP-23 (Structured Loop)

---

#### WCP-26: Critical Section

**Description**: A critical section ensures that only one thread can execute a specific set of activities at a time. Other threads must wait until the critical section is released.

**Petri Net Representation**:
```
    t1      t3
     |       |
     v       v
  [mutex] [mutex]
     |       |
     v       v
    t2      t2  (critical section)
     |       |
     v       v
  [mutex] [mutex]
     |       |
     v       v
    t3      t4
```

**YAWL Representation**:
```yaml
resource: mutex
  scope: critical_section
  activities: [t2]
  constraint: exclusive_access
```

**Use Case Example**:
```erlang
%% Critical section using gen_server for mutual exclusion
-module(critical_section).
-behaviour(gen_server).

%% API
enter_critical_section(Request) ->
    gen_server:call(?MODULE, {enter, Request}, infinity).

leave_critical_section() ->
    gen_server:cast(?MODULE, leave).

%% Callbacks
init([]) ->
    {ok, #{available => true, queue => []}}.

handle_call({enter, Request}, From, State = #{available := true}) ->
    {reply, ok, State#{available => false, current => From}};
handle_call({enter, Request}, From, State = #{available := false, queue := Q}) ->
    {noreply, State#{queue => Q ++ [{From, Request}]}}.

handle_cast(leave, State = #{queue := []}) ->
    {noreply, State#{available => true, current => undefined}};
handle_cast(leave, State = #{queue := [{Next, Request} | Rest]}) ->
    gen_server:reply(Next, ok),
    {noreply, State#{queue => Rest, current => Next}}.

%% Usage
critical_task(Task) ->
    ok = enter_critical_section(Task),
    try
        execute_critical_work(Task)
    after
        leave_critical_section()
    end.
```

**Related Patterns**: WCP-17 (Interleaved Parallel Routing), WCP-18 (Milestone)

---

#### WCP-27: Protocol Pattern

**Description**: A protocol pattern enforces a specific sequence of interactions between parties. Messages must follow a defined protocol with specific ordering constraints.

**Petri Net Representation**:
```
  Client          Server
    |               |
    |---request---->|
    |               | t1: process
    |               |
    |<--response----|
    |               |
  (optional)
    |---ack-------->|
    |               | t2: confirm
```

**YAWL Representation**:
```yaml
protocol: request_response
  steps:
    - from: client
      action: send_request
      to: server
    - from: server
      action: process_request
      to: client
    - from: client
      action: send_acknowledgment
      to: server
      optional: true
```

**Use Case Example**:
```erlang
%% Protocol pattern for client-server communication
-module(protocol).
-export([client/2, server/0]).

% Server protocol
server() ->
    server_loop(awaiting_request).

server_loop(awaiting_request) ->
    receive
        {request, From, Payload} ->
            process_request(Payload),
            From ! {response, self(), process_result(Payload)},
            server_loop(awaiting_ack)
    end;
server_loop(awaiting_ack) ->
    receive
        {ack, _From} ->
            server_loop(awaiting_request);
        {request, From, Payload} ->  % Early next request
            process_request(Payload),
            From ! {response, self(), process_result(Payload)},
            server_loop(awaiting_ack)
    after 5000 ->
        server_loop(awaiting_request)
    end.

% Client protocol
client(ServerPid, Payload) ->
    ServerPid ! {request, self(), Payload},
    receive
        {response, ServerPid, Result} ->
            ServerPid ! {ack, self()},
            {ok, Result}
    end.
```

**Related Patterns**: WCP-21 (Structured Synchronization), WCP-26 (Critical Section)

---

#### WCP-28: Try-Catch

**Description**: An exception handling pattern where a block of activities is attempted, and if an exception occurs, an alternative exception handling block is executed.

**Petri Net Representation**:
```
       try
        |
        v
       t1
       |
       v
   [error_check]
       |
   +---+---+
   |       |
error    normal
   |       |
   v       v
  catch    v
   |       t2
   t3
```

**YAWL Representation**:
```yaml
block: try_catch
  try: [t1, t2]
  catch:
    - exception: error_type1
      handler: t3
    - exception: error_type2
      handler: t4
  finally: t5
```

**Use Case Example**:
```erlang
%% Try-catch pattern in Erlang
try_catch_pattern(RiskyOperation) ->
    try
        % Try block
        Result = execute_risky_operation(RiskyOperation),
        process_result(Result),
        {success, Result}
    catch
        throw:Term ->
            % Catch thrown exceptions
            handle_thrown_exception(Term);
        exit:Reason ->
            % Catch exit signals
            handle_exit_exception(Reason);
        error:Reason:Stacktrace ->
            % Catch runtime errors
            handle_error_exception(Reason, Stacktrace)
    after
        % Finally block (always executes)
        cleanup_resources()
    end.

%% Usage example
safe_file_operation(File) ->
    try
        {ok, Content} = file:read_file(File),
        Processed = process_content(Content),
        file:write_file(File ++ ".processed", Processed),
        {ok, processed}
    catch
        error:enoent ->
            {error, file_not_found};
        error:eacces ->
            {error, permission_denied}
    after
        io:format("Operation completed for ~p~n", [File])
    end.
```

**Related Patterns**: WCP-19 (Cancel Activity), WHP-1 (Error Handler)

---

### 7. Data Flow Patterns (29-33)

#### WDP-1: Parameter Passing

**Description**: Data is passed between activities in the workflow. Parameters can be passed as input to an activity and received as output from an activity.

**Petri Net Representation**:
```
   p1(data=d1)   p2(data=d2)
       |            |
       v            v
       t1(x=d1)    t2(y=d2)
       |            |
       v            v
   p3(data=d1')  p4(data=d2')
```

**YAWL Representation**:
```yaml
task: t1
  input:
    - name: x
      source: p1.data
  output:
    - name: result
      target: p3.data
```

**Use Case Example**:
```erlang
%% Parameter passing between functions
-module(parameter_passing).

% Activity with input/output parameters
process_order(InputData) ->
    % Extract parameters
    OrderId = InputData#input.order_id,
    Customer = InputData#input.customer,
    Items = InputData#input.items,

    % Process
    Result = calculate_total(Items),

    % Output parameters
    #output{
        order_id = OrderId,
        total = Result,
        status = processed
    }.

% Workflow with parameter passing
workflow(InitialData) ->
    Step1Result = step1(InitialData),
    Step2Result = step2(Step1Result),
    Step3Result = step3(Step2Result),
    finalize_workflow(Step3Result).
```

**Related Patterns**: WDP-2 (Data Transformation), WDP-3 (Data Distribution)

---

#### WDP-2: Data Transformation

**Description**: Data is transformed from one format or structure to another as it passes through the workflow. Transformations may include format conversion, aggregation, filtering, or enrichment.

**Petri Net Representation**:
```
   p1(data=A)          p2(data=A')
       |                   |
       v                   v
       t1(transform: A->B)
       |                   |
       v                   v
   p3(data=B)
```

**YAWL Representation**:
```yaml
task: transform
  input: A
  transformation: convert_A_to_B
  output: B
```

**Use Case Example**:
```erlang
%% Data transformation pipeline
transform_data(RawData) ->
    Parsed = parse_raw(RawData),
    Validated = validate(Parsed),
    Enriched = enrich(Validated),
    Formatted = format_output(Enriched),
    Formatted.

% Specific transformations
parse_raw(Raw) ->
    json:decode(Raw).

validate(Parsed) ->
    lists:filter(fun is_valid_record/1, Parsed).

validate_record(Record) ->
    maps:is_key(<<"id">>, Record) andalso maps:is_key(<<"value">>, Record).

enrich(Records) ->
    [enrich_record(R) || R <- Records].

enrich_record(R) ->
    R#{<<"timestamp">> => erlang:system_time(millisecond),
      <<"processed">> => true}.

format_output(Records) ->
    json:encode(Records).
```

**Related Patterns**: WDP-1 (Parameter Passing), WDP-4 (Data Accumulation)

---

#### WDP-3: Data Distribution

**Description**: Data from a single source is distributed to multiple destination activities or branches. Each recipient may receive all or part of the data.

**Petri Net Representation**:
```
       p1(data=D)
           |
           v
       +---+---+
       |       |
       v       v
       t1      t2      t3
       |       |       |
   (D)  (D1)  (D2)   (D3)
```

**YAWL Representation**:
```yaml
task: distribute
  input: D
  outputs:
    - target: t1
      data: D
    - target: t2
      data: filter(D, type1)
    - target: t3
      data: filter(D, type2)
```

**Use Case Example**:
```erlang
%% Data distribution to multiple consumers
distribute_data(Data, Consumers) ->
    lists:foreach(
        fun(Consumer) ->
            ConsumerPid = get_consumer(Consumer),
            ConsumerPid ! {data, self(), Data}
        end,
        Consumers
    ).

%% Selective distribution based on data type
distribute_by_type(Data, Routes) ->
    lists:foreach(
        fun({Route, FilterFn}) ->
            case FilterFn(Data) of
                true ->
                    Route ! {data, Data};
                false ->
                    ok
            end
        end,
        Routes
    ).

%% Usage example
route_order(Order) ->
    Routes = [
        {finance_route(), fun is_payment_required/1},
        {shipping_route(), fun requires_shipping/1},
        {inventory_route(), fun needs_inventory/1}
    ],
    distribute_by_type(Order, Routes).
```

**Related Patterns**: WDP-1 (Parameter Passing), WDP-5 (Data Visibility)

---

#### WDP-4: Data Accumulation

**Description**: Data from multiple sources is accumulated into a single data structure. Results from parallel or sequential activities are collected and combined.

**Petri Net Representation**:
```
   p1    p2    p3
    |     |     |
    v     v     v
    t1    t2    t3
    |     |     |
    +--+--+--+--+
       |
       v
   accumulator(D1+D2+D3)
```

**YAWL Representation**:
```yaml
task: accumulate
  sources: [p1, p2, p3]
  accumulation: merge
  output: combined_data
```

**Use Case Example**:
```erlang
%% Data accumulation from multiple sources
accumulate_results(Sources) ->
    Refs = [spawn_monitor(fun() -> fetch_from_source(S) end) || S <- Sources],
    accumulate_loop(Refs, []).

accumulate_loop([], Accumulated) ->
    {ok, lists:flatten(Accumulated)};
accumulate_loop([{Pid, MRef} | Rest], Accumulated) ->
    receive
        {'DOWN', MRef, process, Pid, {ok, Data}} ->
            accumulate_loop(Rest, [Data | Accumulated]);
        {'DOWN', MRef, process, Pid, {error, Reason}} ->
            accumulate_loop(Rest, Accumulated)
    end.

%% Aggregator for parallel results
aggregate_parallel_results(TaskFun, Inputs) ->
    Parent = self(),
    Pids = [spawn(fun() -> Parent ! {self(), TaskFun(I)} end) || I <- Inputs],
    collect_results(length(Pids), []).

collect_results(0, Results) ->
    lists:reverse(Results);
collect_results(Remaining, Results) ->
    receive
        {Pid, Result} ->
            collect_results(Remaining - 1, [Result | Results])
    end.
```

**Related Patterns**: WDP-2 (Data Transformation), WDP-3 (Data Distribution)

---

#### WDP-5: Data Visibility

**Description**: Defines the scope and visibility of data within the workflow. Data may be local to an activity, shared between specific activities, or globally accessible across the entire workflow.

**Petri Net Representation**:
```
   [global_scope: D1]
          |
    +-----+-----+
    |     |     |
   [L1]  [L2]  [L3]
   D2    D3    D4
    |     |     |
    v     v     v
   t1    t2    t3
```

**YAWL Representation**:
```yaml
data_scopes:
  global: [D1]
  local:
    t1: [D2]
    t2: [D3]
    t3: [D4]
  shared:
    - activities: [t1, t2]
      data: [D5]
```

**Use Case Example**:
```erlang
%% Data visibility with process dictionary and ETS
-module(data_visibility).

%% Global data - stored in ETS table
init_global_data() ->
    ets:new(global_data, [named_table, public, set]),
    ets:insert(global_data, {config, get_config()}),
    ets:insert(global_data, {schema, get_schema()}).

%% Local data - passed as function arguments
local_task(LocalData) ->
    Result = process_with_local_data(LocalData),
    Result.

%% Shared data - shared between related tasks
create_shared_scope(OwnerId) ->
    ets:new(shared_scope(OwnerId), [named_table, protected, set]),
    ets:insert(shared_scope(OwnerId), [{counter, 0}, {status, init}]).

update_shared_data(OwnerId, Key, Value) ->
    ets:insert(shared_scope(OwnerId), {Key, Value}).

%% Workflow with scoped data
workflow_scoped(GlobalConfig) ->
    % Global scope
    init_global_data(GlobalConfig),

    % Local scope for task1
    Task1Local = #{input => input1, temp => temp1},
    Task1Result = task1(Task1Local),

    % Shared scope between task2 and task3
    SharedId = make_ref(),
    create_shared_scope(SharedId),
    task2(SharedId, Task1Result),
    task3(SharedId),
    cleanup_shared_scope(SharedId).
```

**Related Patterns**: WDP-3 (Data Distribution), WRP-4 (Role-Based Distribution)

---

### 8. Resource Patterns (34-38)

#### WRP-1: Creation

**Description**: Defines how and when workflow resources (agents, users, systems) are created or initialized for workflow execution.

**Petri Net Representation**:
```
       t1
       |
       v
   [create_resource]
       |
       v
   resource(R)
       |
       v
       t2
```

**YAWL Representation**:
```yaml
resource: creation
  strategy: dynamic
  initialization:
    - type: user
      creation: on_demand
    - type: system
      creation: at_workflow_start
```

**Use Case Example**:
```erlang
%% Resource creation using supervisors
-module(resource_manager).

%% Create a new resource (worker process)
create_resource(ResourceType, Config) ->
    {ok, Pid} = resource_sup:start_child(ResourceType, Config),
    {ok, Pid}.

%% Supervisor for resource management
init_resource_supervisor() ->
    ChildSpecs = [
        #{
            id => worker_resource,
            start => {worker_resource, start_link, []},
            restart => transient,
            shutdown => 5000,
            type => worker,
            modules => [worker_resource]
        },
        #{
            id => system_resource,
            start => {system_resource, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [system_resource]
        }
    ],
    {ok, {{one_for_one, 10, 60}, ChildSpecs}}.

%% Dynamic resource creation
allocate_task_resource(TaskType) ->
    case resource_pool:checkout(TaskType) of
        {ok, Resource} ->
            {ok, Resource};
        {error, no_resource} ->
            {ok, NewResource} = create_resource(TaskType, #{}),
            {ok, NewResource}
    end.
```

**Related Patterns**: WRP-2 (Role-Based Allocation), WRP-3 (Start)

---

#### WRP-2: Role-Based Allocation

**Description**: Workflow tasks are allocated to resources based on their roles or capabilities. A task specifies required role(s), and resources with matching roles are eligible.

**Petri Net Representation**:
```
       task(T)
         |
         v
   [check_role(R)]
         |
    +----+----+
    |        |
   has      no
   role     role
    |        |
    v        v
 allocate  error
    |
    v
resource(R in role)
```

**YAWL Representation**:
```yaml
task: allocate_by_role
  allocation_strategy: role_based
  required_roles:
    - manager
    - supervisor
  resources:
    - id: user1
      roles: [manager, admin]
    - id: user2
      roles: [supervisor]
```

**Use Case Example**:
```erlang
%% Role-based allocation system
-module(role_allocation).

-record(resource, {
    id,
    roles = [],
    capabilities = [],
    current_task = undefined
}).

-record(task, {
    id,
    required_roles = [],
    required_capabilities = [],
    priority = normal
}).

%% Allocate task to resource based on role
allocate_by_role(Task, AvailableResources) ->
    Eligible = find_eligible_resources(Task, AvailableResources),
    select_best_resource(Task, Eligible).

find_eligible_resources(Task, Resources) ->
    RequiredRoles = Task#task.required_roles,
    lists:filter(
        fun(Resource) ->
            has_required_roles(Resource, RequiredRoles)
        end,
        Resources
    ).

has_required_roles(Resource, RequiredRoles) ->
    ResourceRoles = Resource#resource.roles,
    lists:all(fun(Role) -> lists:member(Role, ResourceRoles) end, RequiredRoles).

%% Select resource (could be based on load, capability, etc.)
select_best_resource(_Task, []) ->
    {error, no_eligible_resource};
select_best_resource(Task, [Resource | _]) ->
    {ok, Resource}.

%% Usage
approve_document(Document) ->
    Task = #task{
        id = Document#document.id,
        required_roles = [manager],
        priority = urgent
    },
    Managers = get_resources_by_role(manager),
    case allocate_by_role(Task, Managers) of
        {ok, Manager} ->
            Manager ! {approve, Document};
        {error, Reason} ->
            escalate_to_admin(Document)
    end.
```

**Related Patterns**: WRP-1 (Creation), WRP-4 (Role-Based Distribution)

---

#### WRP-3: Start

**Description**: Defines how and when resources are started or activated to begin working on workflow tasks. A resource may be started explicitly or triggered by events.

**Petri Net Representation**:
```
       task
        |
        v
   [start_resource]
        |
    +---+---+
    |       |
  ready  not_ready
    |       |
    v       v
  begin   wait
    |
    v
execution
```

**YAWL Representation**:
```yaml
resource: start
  trigger: automatic
  modes:
    - mode: manual
      action: user_initiates
    - mode: automatic
      action: on_assignment
    - mode: scheduled
      action: at_specific_time
```

**Use Case Example**:
```erlang
%% Resource start patterns
-module(resource_starter).

%% Manual start - resource waits for explicit start signal
manual_start(Resource) ->
    Resource ! {init, self()},
    receive
        {Resource, ready} ->
            Resource ! {start, self()},
            {ok, started};
        {Resource, error, Reason} ->
            {error, Reason}
    end.

%% Automatic start - resource starts immediately upon assignment
automatic_start(Resource, Task) ->
    Resource ! {assign_and_start, Task},
    receive
        {Resource, started} ->
            {ok, started};
        {Resource, error, Reason} ->
            {error, Reason}
    end.

%% Scheduled start - resource starts at specified time
scheduled_start(Resource, Task, StartTime) ->
    TimerRef = erlang:send_after(StartTime, Resource, {start, Task}),
    {ok, TimerRef}.

%% Resource implementation
resource_loop(State) ->
    receive
        {init, From} ->
            From ! {self(), ready},
            resource_loop(State#{status => ready});
        {start, From} ->
            From ! {self(), started},
            resource_loop(State#{status => running});
        {assign_and_start, Task} ->
            execute_task(Task),
            resource_loop(State#{status => running});
        {complete, Result} ->
            handle_completion(Result),
            resource_loop(State#{status => idle})
    end.
```

**Related Patterns**: WRP-1 (Creation), WRP-2 (Role-Based Allocation)

---

#### WRP-4: Role-Based Distribution

**Description**: Work items or tasks are distributed to resources based on their roles. This pattern manages how tasks are routed to appropriate resources within a role.

**Petri Net Representation**:
```
        task_queue
             |
             v
        [distributor]
        /    |    \
       v     v     v
     R1     R2     R3  (resources in role)
      |     |     |
     [work distribution]
```

**YAWL Representation**:
```yaml
distribution: role_based
  strategy: round_robin
  role: processor
  resources:
    - id: processor1
      load: 2
    - id: processor2
      load: 3
    - id: processor3
      load: 1
```

**Use Case Example**:
```erlang
%% Role-based task distribution
-module(role_distribution).

%% Distribute tasks using round-robin within a role
distribute_round_robin(Role, Task) ->
    Resources = get_resources_by_role(Role),
    NextIndex = get_next_index(Role),
    Resource = lists:nth(NextIndex + 1, Resources),
    assign_task(Resource, Task),
    increment_next_index(Role).

%% Distribute based on current load
distribute_least_loaded(Role, Task) ->
    Resources = get_resources_by_role(Role),
    SortedResources = lists:keysort(#resource.current_load, Resources),
    [#resource{id = LeastLoaded} | _] = SortedResources,
    assign_task(LeastLoaded, Task).

%% Distribute based on capability matching
distribute_by_capability(Role, Task) ->
    Resources = get_resources_by_role(Role),
    Capable = lists:filter(
        fun(R) -> has_capability(R, Task#task.required_capability) end,
        Resources
    ),
    case Capable of
        [] -> {error, no_capable_resource};
        _ -> assign_task(hd(Capable), Task)
    end.

%% Distributor gen_server
-behaviour(gen_server).

init([]) ->
    {ok, #{
        role_indices => #{},
        resource_loads => #{}
    }}.

handle_call({distribute, Role, Task}, _From, State) ->
    Result = distribute_least_loaded(Role, Task),
    {reply, Result, State};

handle_cast({task_complete, ResourceId}, State) ->
    #{resource_loads := Loads} = State,
    CurrentLoad = maps:get(ResourceId, Loads, 0),
    NewLoads = Loads#{ResourceId => CurrentLoad - 1},
    {noreply, State#{resource_loads => NewLoads}}.
```

**Related Patterns**: WRP-2 (Role-Based Allocation), WDP-3 (Data Distribution)

---

#### WRP-5: Capability Allocation

**Description**: Resources are allocated based on their specific capabilities or skills rather than just roles. This allows for more fine-grained matching of tasks to resources.

**Petri Net Representation**:
```
        task(required_capability=C)
                  |
                  v
           [match_capability]
                  |
         +--------+--------+
         |                 |
      match            no_match
         |                 |
         v                 v
    allocate           escalate
         |
         v
resource(with_capability=C)
```

**YAWL Representation**:
```yaml
allocation: capability_based
  task_requirements:
    - capability: java_development
      level: senior
    - capability: code_review
      level: intermediate
  resource_capabilities:
    - id: dev1
      capabilities:
        - name: java_development
          level: senior
        - name: code_review
          level: expert
```

**Use Case Example**:
```erlang
%% Capability-based allocation
-module(capability_allocation).

-record(capability, {
    name,
    level  % beginner, intermediate, senior, expert
}).

-record(resource, {
    id,
    capabilities = []  % list of capability records
}).

%% Find resources with required capability
find_by_capability(Resources, CapabilityName, MinLevel) ->
    lists:filter(
        fun(R) -> has_capability_at_level(R, CapabilityName, MinLevel) end,
        Resources
    ).

has_capability_at_level(Resource, CapabilityName, MinLevel) ->
    Capabilities = Resource#resource.capabilities,
    case lists:keyfind(CapabilityName, #capability.name, Capabilities) of
        false -> false;
        #capability{level = Level} ->
            level_at_least(Level, MinLevel)
    end.

level_at_least(CapLevel, MinLevel) ->
    Levels = [beginner, intermediate, senior, expert],
    CapIndex = index_of(CapLevel, Levels),
    MinIndex = index_of(MinLevel, Levels),
    CapIndex >= MinIndex.

%% Score-based allocation
allocate_best_fit(Resources, RequiredCapabilities) ->
    Scored = score_resources(Resources, RequiredCapabilities),
    case lists:reverse(lists:keysort(1, Scored)) of
        [{_Score, Resource} | _] -> {ok, Resource};
        [] -> {error, no_suitable_resource}
    end.

score_resources(Resources, RequiredCapabilities) ->
    lists:map(
        fun(R) -> {calculate_score(R, RequiredCapabilities), R} end,
        Resources
    ).

calculate_score(Resource, RequiredCapabilities) ->
    lists:foldl(
        fun(Cap, Acc) ->
            case find_capability(Resource, Cap) of
                {ok, Level} -> Acc + level_score(Level);
                no_match -> Acc - 100
            end
        end,
        0,
        RequiredCapabilities
    ).

level_score(beginner) -> 1;
level_score(intermediate) -> 2;
level_score(senior) -> 3;
level_score(expert) -> 4.

%% Usage
assign_code_review(ReviewTask) ->
    RequiredCaps = [
        #capability{name = code_review, level = intermediate},
        #capability{name = language_expertise, level = senior}
    ],
    Developers = get_all_developers(),
    case allocate_best_fit(Developers, RequiredCaps) of
        {ok, Reviewer} ->
            Reviewer ! {review, ReviewTask};
        {error, _} ->
            escalate_to_lead(ReviewTask)
    end.
```

**Related Patterns**: WRP-2 (Role-Based Allocation), WRP-4 (Role-Based Distribution)

---

### 9. Exception Handling Patterns (39-43)

#### WHP-1: Error Handler

**Description**: A designated activity or handler is invoked when an error occurs in the workflow. The error handler can correct the error, log it, or initiate alternative processing.

**Petri Net Representation**:
```
       t1
       |
       v
   [error_check]
       |
   +---+---+
   |       |
error    normal
   |       |
   v       v
  handler  t2
   |
   v
recover/retry
```

**YAWL Representation**:
```yaml
task: with_error_handler
  activity: t1
  handlers:
    - exception: connection_error
      handler: h1
      action: retry
    - exception: validation_error
      handler: h2
      action: compensate
```

**Use Case Example**:
```erlang
%% Error handler pattern
-module(error_handler).

%% Execute with error handler
with_error_handler(Task, ErrorHandler) ->
    try
        Task()
    catch
        Type:Reason:Stacktrace ->
            ErrorHandler(Type, Reason, Stacktrace)
    end.

%% Specific error handlers
handle_connection_error(error, Reason, _Stacktrace) ->
    log_error(connection, Reason),
    attempt_reconnect();

handle_validation_error(error, Reason, _Stacktrace) ->
    log_error(validation, Reason),
    return_validation_error(Reason);

handle_timeout_error(exit, timeout, _Stacktrace) ->
    log_error(timeout, "Operation timed out"),
    escalate_to_human();

handle_unexpected(Type, Reason, Stacktrace) ->
    log_error(unexpected, {Type, Reason, Stacktrace}),
    notify_admins(Type, Reason, Stacktrace).

%% Usage example with try/catch/after
safe_api_call(ApiFunction) ->
    try
        case ApiFunction() of
            {ok, Result} -> Result;
            {error, Reason} -> error(api_error, [Reason])
        end
    catch
        error:{api_error, Reason} ->
            handle_api_error(Reason);
        exit:timeout ->
            handle_timeout();
        Type:Reason:Stacktrace ->
            handle_unexpected(Type, Reason, Stacktrace)
    after
        cleanup_resources()
    end.
```

**Related Patterns**: WHP-2 (Retry), WHP-3 (Compensate)

---

#### WHP-2: Retry

**Description**: When an activity fails, it is retried a specified number of times before giving up. Retries may include delays or exponential backoff.

**Petri Net Representation**:
```
       t1
       |
       v
   [attempt]
       |
   +---+---+
   |       |
success  failure
   |       |
   v       v
   next  [retry_count]
          |
       +--+--+
       |     |
     under over
     limit limit
       |     |
       v     v
     retry  fail
       |
       +--> t1
```

**YAWL Representation**:
```yaml
task: retry_on_failure
  activity: t1
  retry:
    max_attempts: 3
    backoff: exponential
    base_delay: 1000ms
    on_exhausted: escalate
```

**Use Case Example**:
```erlang
%% Retry pattern with exponential backoff
-module(retry).

retry(Task, Options) ->
    MaxAttempts = maps:get(max_attempts, Options, 3),
    BaseDelay = maps:get(base_delay, Options, 1000),
    MaxDelay = maps:get(max_delay, Options, 30000),
    retry_loop(Task, 1, MaxAttempts, BaseDelay, MaxDelay).

retry_loop(Task, Attempt, MaxAttempts, BaseDelay, MaxDelay) when Attempt > MaxAttempts ->
    {error, max_attempts_exceeded};
retry_loop(Task, Attempt, MaxAttempts, BaseDelay, MaxDelay) ->
    case Task() of
        {ok, Result} ->
            {ok, Result};
        {error, Reason} ->
            Delay = calculate_delay(Attempt, BaseDelay, MaxDelay),
            log_retry_attempt(Attempt, Reason, Delay),
            timer:sleep(Delay),
            retry_loop(Task, Attempt + 1, MaxAttempts, BaseDelay, MaxDelay)
    end.

%% Calculate exponential backoff with jitter
calculate_delay(Attempt, BaseDelay, MaxDelay) ->
    Exponential = min(BaseDelay * trunc(math:pow(2, Attempt - 1)), MaxDelay),
    Jitter = rand:uniform(Exponential div 4),
    Exponential + Jitter.

%% Usage examples
%% Simple retry
retry_database_query(Query) ->
    retry(
        fun() -> database:execute(Query) end,
        #{max_attempts => 3, base_delay => 500}
    ).

%% Retry with circuit breaker
retry_with_circuit_breaker(Task, Options) ->
    case circuit_breaker:check_state(service_name) of
        closed ->
            case retry(Task, Options) of
                {ok, Result} -> {ok, Result};
                {error, Reason} ->
                    circuit_breaker:record_failure(service_name),
                    {error, Reason}
            end;
        open ->
            {error, circuit_breaker_open}
    end.
```

**Related Patterns**: WHP-1 (Error Handler), WHP-4 (Triggered Compensation)

---

#### WHP-3: Compensate

**Description**: When an activity cannot be completed or needs to be undone, a compensating activity is invoked to reverse the effects of the original activity.

**Petri Net Representation**:
```
        t1
        |
        v
    (execute)
        |
        v
   [check_result]
        |
    +---+---+
    |       |
success  failure
    |       |
    v       v
    t2    compensate(t1)
    |
    v
complete
```

**YAWL Representation**:
```yaml
task: compensatable
  activity: book_flight
  compensation: cancel_booking
  compensation_scope: full
```

**Use Case Example**:
```erlang
%% Compensation pattern for transactions
-module(compensation).

-record(compensation, {
    activity_id,
    compensate_fun,
    context
}).

%% Execute with compensation
with_compensation(ActivityFun, CompensateFun) ->
    ActivityId = make_ref(),
    try
        Result = ActivityFun(),
        register_compensation(ActivityId, CompensateFun),
        {ok, Result, ActivityId}
    catch
        Type:Reason:Stacktrace ->
            execute_compensation(ActivityId),
            {error, {Type, Reason, Stacktrace}}
    end.

%% Execute compensation chain
execute_compensation(ActivityId) ->
    Compensations = get_compensations_since(ActivityId),
    execute_compensations(lists:reverse(Compensations)).

execute_compensations([]) ->
    ok;
execute_compensations([#compensation{compensate_fun = Fun, context = Ctx} | Rest]) ->
    case Fun(Ctx) of
        ok -> execute_compensations(Rest);
        {error, Reason} ->
            log_compensation_failure(Reason),
            execute_compensations(Rest)
    end.

%% Business transaction with compensation
book_trip_itinerary(Itinerary) ->
    % Step 1: Book flight
    {ok, FlightBooking, FwdId} = with_compensation(
        fun() -> flight_service:book(Itinerary#itinerary.flight) end,
        fun(Ctx) -> flight_service:cancel(Ctx#booking.confirmation) end
    ),

    % Step 2: Book hotel
    {ok, HotelBooking, HotId} = with_compensation(
        fun() -> hotel_service:book(Itinerary#itinerary.hotel) end,
        fun(Ctx) -> hotel_service:cancel(Ctx#booking.confirmation) end
    ),

    % Step 3: Book rental car (may fail, triggering compensation)
    case with_compensation(
        fun() -> rental_service:book(Itinerary#itinerary.rental) end,
        fun(Ctx) -> rental_service:cancel(Ctx#booking.confirmation) end
    ) of
        {ok, RentalBooking, _} ->
            {ok, #{flight => FlightBooking, hotel => HotelBooking, rental => RentalBooking}};
        {error, _Reason} ->
            % This will trigger compensation for flight and hotel
            {error, booking_failed}
    end.

%% Simplified transaction with compensation
book_trip(Itinerary) ->
    Compensations = [],
    try
        {FlightRef, Booking1} = book_with_comp(Itinerary#itinerary.flight,
                                               fun flight:cancel/1),
        Compensations1 = [{flight, FlightRef, fun flight:cancel/1} | Compensations],

        {HotelRef, Booking2} = book_with_comp(Itinerary#itinerary.hotel,
                                              fun hotel:cancel/1),
        Compensations2 = [{hotel, HotelRef, fun hotel:cancel/1} | Compensations1],

        {CarRef, Booking3} = book_with_comp(Itinerary#itinerary.rental,
                                             fun rental:cancel/1),

        {ok, #{flight => Booking1, hotel => Booking2, rental => Booking3}}
    catch
        throw:Error ->
            execute_compensations(Compensations),
            {error, Error}
    end.
```

**Related Patterns**: WHP-1 (Error Handler), WHP-5 (Consecutive Compensation)

---

#### WHP-4: Triggered Compensation

**Description**: Compensation is triggered by an external event or condition, not just by failure. This allows for proactive compensation based on business rules or events.

**Petri Net Representation**:
```
        t1        t2
         |         |
         v         v
    (execute)  (execute)
         |         |
         v         v
    [registered] [registered]
         |         |
         +----+----+
              |
      [trigger_event]
              |
              v
         compensate_all
```

**YAWL Representation**:
```yaml
task: triggered_compensation
  triggers:
    - event: customer_cancellation
      compensate: [book_flight, book_hotel]
    - event: deadline_exceeded
      compensate: [reserve_item]
```

**Use Case Example**:
```erlang
%% Triggered compensation for cancellation
-module(triggered_compensation).

-record(transaction, {
    id,
    activities = [],
    status = active
}).

-record(activity, {
    id,
    compensate_fun,
    completed = false
}).

%% Start a transaction with compensatable activities
start_transaction() ->
    TxId = make_ref(),
    Tx = #transaction{id = TxId},
    register_transaction(TxId, Tx),
    TxId.

%% Register compensatable activity
register_activity(TxId, ActivityId, CompensateFun) ->
    Activity = #activity{id = ActivityId, compensate_fun = CompensateFun},
    add_activity_to_transaction(TxId, Activity).

%% Trigger compensation externally
trigger_compensation(TxId, Reason) ->
    case get_transaction(TxId) of
        #transaction{activities = Activities} ->
            execute_compensations(Activities, Reason),
            mark_transaction_compensated(TxId);
        undefined ->
            {error, transaction_not_found}
    end.

%% Event-based compensation handlers
handle_cancellation_event(CustomerId, OrderId) ->
    TxId = get_transaction_for_order(OrderId),
    trigger_compensation(TxId, {customer_cancellation, CustomerId}).

handle_deadline_event(OrderId) ->
    TxId = get_transaction_for_order(OrderId),
    trigger_compensation(TxId, deadline_exceeded).

%% Business example: Order with triggered compensation
process_order(Order) ->
    TxId = start_transaction(),

    % Book items
    {ok, ItemBooking} = inventory:book(Order#order.items),
    register_activity(TxId, item_booking,
                      fun(_) -> inventory:release(ItemBooking) end),

    % Process payment
    {ok, PaymentId} = payment:charge(Order#order.payment_info),
    register_activity(TxId, payment,
                      fun(_) -> payment:refund(PaymentId) end),

    % Arrange shipping
    {ok, ShipmentId} = shipping:arrange(Order#order.shipping),
    register_activity(TxId, shipping,
                      fun(_) -> shipping:cancel(ShipmentId) end),

    % If customer cancels within 24 hours, compensate everything
    schedule_cancellation_check(OrderId, TxId, 24 * 60 * 60 * 1000),

    {ok, Order#order{transaction_id = TxId}}.

check_and_compensate_if_needed(OrderId, TxId, Deadline) ->
    case get_order_status(OrderId) of
        {shipped, _} ->
            ok;  % Already shipped, cannot compensate
        {pending, _} ->
            Now = erlang:system_time(millisecond),
            if
                Now > Deadline ->
                    trigger_compensation(TxId, auto_cancellation);
                true ->
                    ok
            end
    end.
```

**Related Patterns**: WHP-3 (Compensate), WHP-5 (Consecutive Compensation)

---

#### WHP-5: Consecutive Compensation

**Description**: Multiple compensating activities are executed in a specific order. The compensation activities themselves may have dependencies on each other.

**Petri Net Representation**:
```
        t1    t2    t3
         |     |     |
         v     v     v
    (executed in order)
         |     |     |
         v     v     v
    [compensation_trigger]
         |
         v
    c3(t3) -> c2(t2) -> c1(t1)  (reverse order)
```

**YAWL Representation**:
```yaml
task: consecutive_compensation
  activities: [t1, t2, t3]
  compensations:
    - for: t1
      activity: c1
      order: 3
    - for: t2
      activity: c2
      order: 2
    - for: t3
      activity: c3
      order: 1
```

**Use Case Example**:
```erlang
%% Consecutive compensation with ordering
-module(consecutive_compensation).

-record(compensation_step, {
    id,
    activity_id,
    compensate_fun,
    order  % Determines execution order
}).

%% Execute compensations in consecutive order
execute_consecutive_compensations(Compensations) ->
    % Sort by order (ascending: lower numbers execute first)
    Sorted = lists:keysort(#compensation_step.order, Compensations),
    execute_compensations_sequential(Sorted).

execute_compensations_sequential([]) ->
    {ok, all_compensated};
execute_compensations_sequential([Step | Rest]) ->
    case Step#compensation_step.compensate_fun() of
        ok ->
            log_compensation_success(Step#compensation_step.id),
            execute_compensations_sequential(Rest);
        {error, Reason} ->
            log_compensation_failure(Step#compensation_step.id, Reason),
            case should_continue_on_failure() of
                true ->
                    execute_compensations_sequential(Rest);
                false ->
                    {error, {compensation_failed, Step#compensation_step.id, Reason}}
            end
    end.

%% Business example: Order cancellation with consecutive compensation
cancel_order(Order) ->
    % Compensations in LIFO order (last in, first compensated)
    Compensations = [
        #compensation_step{
            id = cancel_shipment,
            activity_id = arrange_shipping,
            compensate_fun = fun() -> shipping:cancel(Order#order.shipment_id) end,
            order = 1  % Execute first
        },
        #compensation_step{
            id = refund_payment,
            activity_id = process_payment,
            compensate_fun = fun() -> payment:refund(Order#order.payment_id) end,
            order = 2  % Execute second
        },
        #compensation_step{
            id = restock_inventory,
            activity_id = reserve_inventory,
            compensate_fun = fun() -> inventory:restock(Order#order.items) end,
            order = 3  % Execute last
        }
    ],

    case execute_consecutive_compensations(Compensations) of
        {ok, all_compensated} ->
            notify_order_cancelled(Order),
            {ok, compensated};
        {error, Reason} ->
            notify_compensation_failure(Order, Reason),
            {error, partial_compensation}
    end.

%% Saga pattern for distributed transactions
saga_execute(Steps) ->
    saga_execute(Steps, []).

saga_execute([], Compensations) ->
    {ok, lists:reverse(Compensations)};
saga_execute([{Action, Compensate} | Rest], Compensations) ->
    case Action() of
        {ok, Result} ->
            saga_execute(Rest, [{Compensate, Result} | Compensations]);
        {error, Reason} ->
            % Execute compensations in reverse order
            execute_saga_compensations(Compensations),
            {error, Reason}
    end.

execute_saga_compensations([]) ->
    ok;
execute_saga_compensations([{Compensate, Result} | Rest]) ->
    case Compensate(Result) of
        ok ->
            execute_saga_compensations(Rest);
        {error, CompReason} ->
            log_error("Compensation failed: ~p", [CompReason]),
            execute_saga_compensations(Rest)
    end.
```

**Related Patterns**: WHP-3 (Compensate), WHP-4 (Triggered Compensation)

---

## Pattern Summary Table

| ID | Name | Category | YAWL Support |
|----|------|----------|--------------|
| WCP-1 | Sequence | Basic Control | Native |
| WCP-2 | Parallel Split | Basic Control | Native (AND-split) |
| WCP-3 | Synchronization | Basic Control | Native (AND-join) |
| WCP-4 | Exclusive Choice | Basic Control | Native (OR-split) |
| WCP-5 | Simple Merge | Basic Control | Native (XOR-join) |
| WCP-6 | Multi-Choice | Basic Control | Native (OR-split) |
| WCP-7 | Synchronizing Merge | Advanced Sync | Native (OR-join) |
| WCP-8 | Multi-Merge | Advanced Sync | Extended |
| WCP-9 | Discriminator | Advanced Sync | Extended |
| WCP-10 | Arbitrary Cycles | Advanced Sync | Native |
| WCP-11 | Implicit Termination | Structural | Native |
| WCP-12 | MI without Sync | Structural | Native (MI) |
| WCP-13 | MI with Design Time Knowledge | Structural | Native (MI) |
| WCP-14 | MI with Runtime Knowledge | Multiple Instance | Native (MI) |
| WCP-15 | MI without Runtime Knowledge | Multiple Instance | Native (MI) |
| WCP-16 | Deferred Choice | Multiple Instance | Native |
| WCP-17 | Interleaved Parallel Routing | Multiple Instance | Extended |
| WCP-18 | Milestone | State-Based | Extended |
| WCP-19 | Cancel Activity | State-Based | Native |
| WCP-20 | Cancel Case | State-Based | Native |
| WCP-21 | Structured Synchronization | Extended Control | Native |
| WCP-22 | Structured Partial Join | Extended Control | Extended |
| WCP-23 | Structured Loop | Extended Control | Native |
| WCP-24 | Recursion | Extended Control | Extended |
| WCP-25 | Arbitrary Interleaved Loop | Extended Control | Extended |
| WCP-26 | Critical Section | Extended Control | Extended |
| WCP-27 | Protocol Pattern | Extended Control | Extended |
| WCP-28 | Try-Catch | Extended Control | Native |
| WDP-1 | Parameter Passing | Data Flow | Native |
| WDP-2 | Data Transformation | Data Flow | Native |
| WDP-3 | Data Distribution | Data Flow | Extended |
| WDP-4 | Data Accumulation | Data Flow | Extended |
| WDP-5 | Data Visibility | Data Flow | Extended |
| WRP-1 | Creation | Resource | Extended |
| WRP-2 | Role-Based Allocation | Resource | Native |
| WRP-3 | Start | Resource | Native |
| WRP-4 | Role-Based Distribution | Resource | Extended |
| WRP-5 | Capability Allocation | Resource | Extended |
| WHP-1 | Error Handler | Exception Handling | Native |
| WHP-2 | Retry | Exception Handling | Extended |
| WHP-3 | Compensate | Exception Handling | Extended |
| WHP-4 | Triggered Compensation | Exception Handling | Extended |
| WHP-5 | Consecutive Compensation | Exception Handling | Extended |

---

## References

1. van der Aalst, W.M.P., ter Hofstede, A.H.M., Kiepuszewski, B., & Barros, A.P. (2003). "Workflow Patterns". Distributed and Parallel Databases, 14(3), 5-51.

2. Russell, N., ter Hofstede, A.H.M., van der Aalst, W.M.P., & Mulyar, N. (2006). "Workflow Control-Flow Patterns: A Revised View". BPM Center Report BPM-06-22.

3. Russell, N., van der Aalst, W.M.P., ter Hofstede, A.H.M., & Mulyar, N. (2006). "Workflow Resource Patterns". BPM Center Report BPM-06-23.

4. Russell, N., ter Hofstede, A.H.M., & van der Aalst, W.M.P. (2006). "Workflow Data Patterns". BPM Center Report BPM-06-24.

5. Adams, M., ter Hofstede, A.H.M., Edmond, D., & van der Aalst, W.M.P. (2006). "Workflow Exception Patterns". BPM Center Report BPM-06-10.

---

*This document is part of the YAWL Workflow Patterns reference documentation for the CRE project.*
