# CRE Workflow Examples

This document provides a comprehensive guide to the workflow examples included in CRE (Common Runtime Environment). These examples demonstrate YAWL (Yet Another Workflow Language) patterns implemented as Petri nets using the `gen_pnet` behavior.

## Overview

CRE includes several categories of workflow examples:

| Category | Description | Location |
|----------|-------------|----------|
| **YAWL Patterns** | Complete YAWL workflow patterns from the CAISE 2013 paper | `examples/workflows/` |
| **Order Fulfillment** | End-to-end order processing workflow | `examples/workflows/order_fulfillment.erl` |
| **Demo Scripts** | Standalone demonstration scripts | `examples/*.erl` |

## Table of Contents

1. [Order Fulfillment Workflows](#order-fulfillment-workflows)
2. [Individual Subprocess Examples](#individual-subprocess-examples)
3. [Van Der Aalst Comprehensive Workflow](#van-der-aalst-comprehensive-workflow)
4. [Basic Workflow Tutorial](#basic-workflow-tutorial)
5. [Demo Scripts](#demo-scripts)
6. [Expected Outputs](#expected-outputs)
7. [Modifying Examples](#modifying-examples)

---

## Order Fulfillment Workflows

### File: `examples/workflows/order_fulfillment.erl`

**Purpose**: Top-level orchestrator for the complete order fulfillment process from the CAISE 2013 paper.

**Use Case**: Coordinate the entire order-to-delivery lifecycle with 5 sequential subprocesses.

**Patterns Used**:
- **WCP-01 (Sequence)**: All subprocesses execute sequentially
- subprocess coordination with result tracking

#### Petri Net Structure

```
PLACES:                          TRANSITIONS:
p_input                          t_start_ordering
p_ordering_complete              t_start_carrier
p_carrier_complete               t_start_payment
p_payment_complete               t_start_transit
p_transit_complete               t_start_delivery
p_delivery_complete              t_complete
p_output
```

#### Subprocesses

1. **Ordering** (`ordering.erl`) - Customer order entry and validation
2. **Carrier Appointment** (`carrier_appointment.erl`) - Shipping arrangement
3. **Payment** (`payment.erl`) - Payment processing
4. **Freight In Transit** (`freight_in_transit.erl`) - Shipment tracking
5. **Freight Delivered** (`freight_delivered.erl`) - Delivery confirmation

#### How to Run

```erlang
%% In the Erlang shell
rebar3 shell.

%% Run the demo
order_fulfillment_demo:run_demo().

%% Or run with custom order data
OrderInput = #{
    <<"order_id">> => <<"ORDER-001">>,
    <<"customer_id">> => <<"CUST-001">>,
    <<"customer_name">> => <<"John Doe">>,
    <<"customer_email">> => <<"john@example.com">>,
    <<"items">> => [
        #{<<"sku">> => <<"SKU-001">>, <<"quantity">> => 2, <<"price">> => 29.99}
    ],
    <<"shipping_address">> => #{<<"city">> => <<"New York">>},
    <<"total">> => 59.98,
    <<"payment_details">> => #{<<"method">> => credit_card}
},
order_fulfillment:run(OrderInput).
```

#### API Functions

| Function | Description |
|----------|-------------|
| `new/1` | Create new fulfillment state from order |
| `start/1` | Start the fulfillment workflow |
| `run/1` | Run synchronously (blocks until complete) |
| `get_status/1` | Get current workflow status |
| `get_state/1` | Get full workflow state |

---

## Individual Subprocess Examples

### Ordering Subprocess

**File**: `examples/workflows/ordering.erl`

**Purpose**: Handle customer order entry, inventory checking, and order confirmation.

**Patterns Used**:
- **WCP-26 (Critical Section)**: Item reservation with atomic inventory check
- **WCP-01 (Sequence)**: Sequential validation steps

#### How to Run

```erlang
%% Create order
Order = #order{
    order_id = <<"ORDER-001">>,
    customer_id = <<"CUST-001">>,
    customer_name = <<"Jane Smith">>,
    customer_email = <<"jane@example.com">>,
    items = [
        #item{sku = <<"SKU-001">>, name = <<"Widget">>, quantity = 2, price = 10.0}
    ],
    subtotal = 20.0,
    tax = 1.6,
    shipping_cost = 5.99,
    total = 27.59,
    status = pending
},

%% Start ordering subprocess
{ok, Pid} = ordering:start(Order).

%% Or run synchronously
{ok, FinalOrder} = ordering:run(Order).
```

### Payment Subprocess

**File**: `examples/workflows/payment.erl`

**Purpose**: Process payments with multiple payment methods and retry logic.

**Patterns Used**:
- **WCP-04 (Exclusive Choice)**: Route to payment method (credit card, PayPal, bank transfer)
- **WHP-01 (Error Handler)**: Handle payment failures
- **WCP-23 (Structured Loop)**: Retry failed payments (up to 3 attempts)

#### How to Run

```erlang
PaymentDetails = #{<<"method">> => credit_card},

%% For credit card payment
PaymentDetails = #{
    <<"method">> => credit_card,
    <<"credit_card">> => #{
        <<"card_number">> => <<"4111111111111111">>,
        <<"cardholder_name">> => <<"John Doe">>,
        <<"expiry_month">> => 12,
        <<"expiry_year">> => 2025,
        <<"cvv">> => <<"123">>
    }
},

{ok, Pid} = payment:start(Order, PaymentDetails).
```

### Carrier Appointment Subprocess

**File**: `examples/workflows/carrier_appointment.erl`

**Purpose**: Arrange shipping appointments with carriers based on order characteristics.

**Patterns Used**:
- **WCP-06 (Multi Choice)**: Select shipping method (FTL, LTL, single package)
- **WCP-08 (Multi Merge)**: Collect multiple carrier responses

#### How to Run

```erlang
PurchaseOrder = #{
    <<"id">> => <<"PO-001">>,
    <<"destination">> => <<"New York">>,
    <<"total_volume">> => 5000,
    <<"items">> => [#{<<"sku">> => <<"ITEM-001">>, <<"quantity">> => 10}]
},

{ok, Pid} = carrier_appointment:start(PurchaseOrder, <<"log_001">>).
```

### Freight In Transit Subprocess

**File**: `examples/workflows/freight_in_transit.erl`

**Purpose**: Track shipment during transit with location updates and delay notifications.

**Patterns Used**:
- **WCP-25 (Interleaved Loop)**: Continuous monitoring loop
- **WCP-16 (Deferred Choice)**: Delay notification decision

#### How to Run

```erlang
Shipment = #shipment{
    shipment_id = <<"SHIP-001">>,
    order_id = <<"ORDER-001">>,
    carrier = <<"FEDEX">>,
    tracking_number = <<"1Z123456">>,
    origin = <<"Warehouse">>,
    destination = <<"New York">>
},

{ok, Pid} = freight_in_transit:start(Shipment, Order).

%% Add tracking event
freight_in_transit:add_tracking_event(Pid, #{
    <<"location">> => <<"Distribution Center">>,
    <<"status">> => <<"in_transit">>
}).
```

### Freight Delivered Subprocess

**File**: `examples/workflows/freight_delivered.erl`

**Purpose**: Handle final delivery confirmation, item verification, and signature capture.

**Patterns Used**:
- **WCP-18 (Milestone)**: Items must be verified before signature

#### How to Run

```erlang
{ok, Pid} = freight_delivered:start(Shipment, Order).

%% Or run synchronously
{ok, Delivery} = freight_delivered:run(Shipment, Order).
```

---

## Van Der Aalst Comprehensive Workflow

### File: `examples/workflows/van_der_aalst_workflow.erl`

**Purpose**: Demonstrates all 43 YAWL workflow control patterns in a single comprehensive order processing workflow.

**Use Case**: Learning and testing all YAWL patterns with real-world scenarios.

**Patterns Demonstrated**:

#### Basic Control Flow (WCP-01 to WCP-10)

| Pattern | Description | Use Case |
|---------|-------------|----------|
| WCP-01: Sequence | Ordered task execution | Order verification steps |
| WCP-02: Parallel Split | Concurrent execution | Warehouse and credit check |
| WCP-03: Synchronization | Wait for all branches | Sync check results |
| WCP-04: Exclusive Choice | XOR split | Credit path selection |
| WCP-05: Simple Merge | OR join | Merge approval paths |
| WCP-06: Multi Choice | OR split | Select multiple shippers |
| WCP-07: Synchronizing Merge | AND-progress | Sync with shippers |
| WCP-08: Multi Merge | OR-collect | Collect supplier quotes |
| WCP-09: Discriminator | First response wins | First shipper quote |
| WCP-10: Arbitration | N of M selection | 2 of 3 manager approvals |

#### Multiple Instances (WCP-11 to WCP-17)

| Pattern | Description |
|---------|-------------|
| WCP-11: Implicit Termination | Auto-end tasks |
| WCP-12: Multiple Instances (No Sync) | Independent notifications |
| WCP-13: Static Instances | Fixed 3 suppliers |
| WCP-14: Runtime Instances | Dynamic supplier count |
| WCP-15: Dynamic Instances | Unbounded item processing |
| WCP-16: Deferred Choice | Runtime shipping method |
| WCP-17: Interleaved Routing | Round-robin processing |

#### State-Based (WCP-18 to WCP-20)

| Pattern | Description |
|---------|-------------|
| WCP-18: Milestone | Inventory guard |
| WCP-19: Cancel Activity | Cancel shipment |
| WCP-20: Cancel Case | Cancel entire order |

#### Extended Control Flow (WCP-21 to WCP-28)

| Pattern | Description |
|---------|-------------|
| WCP-21: Structured Sync | Supplier sync barrier |
| WCP-22: Partial Join | Quorum for multi-approval |
| WCP-23: Structured Loop | Retry failed payments |
| WCP-24: Recursion | Sub-workflow calls |
| WCP-25: Interleaved Loop | Parallel batch processing |
| WCP-26: Critical Section | Inventory mutex |
| WCP-27: Protocol Pattern | Supplier communication |
| WCP-28: Try-Catch | Payment exception handling |

#### Data Flow, Resource, and Exception Patterns

- **WDP-01 to WDP-05**: Parameter passing, transformation, distribution, accumulation, visibility
- **WRP-01 to WRP-05**: Resource creation, role allocation, start, distribution, capability
- **WHP-01 to WHP-05**: Error handler, retry, compensation, triggered compensation, consecutive compensation

#### How to Run

```erlang
%% Run with default sample order
van_der_aalst_workflow:run_full_workflow().

%% Run with custom order data
OrderData = #{
    <<"id">> => <<"ORDER-001">>,
    <<"customer">> => <<"CUST-001">>,
    <<"items">> => [
        #{<<"sku">> => <<"ITEM-001">>, <<"quantity">> => 2, <<"price">> => 99.99}
    ],
    <<"total">> => 199.98
},
van_der_aalst_workflow:run_full_workflow(OrderData).
```

---

## Basic Workflow Tutorial

### File: `examples/example_basic_workflow.erl`

**Purpose**: A simple introductory example demonstrating sequential workflow patterns with parameter passing.

**Use Case**: Learning the basics of CRE workflow implementation.

**Patterns Demonstrated**:
- **WCP-01 (Sequence)**: Sequential task execution
- **WDP-01 (Parameter Passing)**: Data between tasks
- Error handling patterns

#### Workflow Structure

```
Task1 (Initialize) -> Task2 (Process) -> Task3 (Finalize)
```

#### Step-by-Step Tutorial

1. **Start the CRE runtime**:

```bash
# Terminal 1: Start CRE
cd /Users/sac/cre
rebar3 shell
```

2. **Run the example**:

```erlang
%% In the Erlang shell
(example_basic_workflow:run()).
```

#### Expected Output

```
=== Basic Workflow Example ===

Initializing client with workflow: [...]
Starting workflow with 3 tasks
Executing task <<"task1">> (ID: <<"init">>) with params: []
Executing task <<"task2">> (ID: <<"process">>) with params: [{<<"value">>, 42}]
Executing task <<"task3">> (ID: <<"finalize">>) with params: [...]
All tasks completed

Workflow completed successfully
```

#### Customization

Modify the workflow definition in `run/1`:

```erlang
Workflow = [
    {<<"task1">>, <<"init">>, []},
    {<<"task2">>, <<"process">>, [{<<"value">>, 42}]},
    {<<"task3">>, <<"finalize">>, [{<<"result">>, undefined}]}
],
```

---

## Demo Scripts

### YAWL Petri Net Demo

**File**: `examples/yawl_pnet_demo.erl`

**Purpose**: Comprehensive demo of order processing with approval workflow.

**How to Run**:

```bash
# Using the shell script
./examples/yawl_pnet_demo.sh

# Or manually
rebar3 compile
erlc -I include -o ebin examples/yawl_pnet_example.erl
erl -pa ebin -pa _build/default/lib/*/ebin -eval "yawl_pnet_demo:run()" -s init stop
```

#### Scenarios

1. **Low-value order**: Direct processing without approval
2. **High-value order**: Requires approval before processing
3. **Multiple concurrent orders**: Parallel processing demonstration

### YAWL Demo Service

**File**: `examples/yawl_demo.erl`

**Purpose**: Demo service module with sample workflows and task handlers.

**Available Workflows**:

```erlang
%% Order fulfillment
yawl_demo:run_order_fulfillment_workflow().

%% Loan approval
yawl_demo:run_loan_approval_workflow().

%% Customer onboarding
yawl_demo:run_customer_onboarding_workflow().

%% Claim processing
yawl_demo:run_claim_processing_workflow().
```

#### Demo Data Generation

```erlang
%% Generate test data
Order = yawl_demo:generate_order_data(),
Customer = yawl_demo:generate_customer_data(),
Claim = yawl_demo:generate_claim_data(),
```

### Order Fulfillment Demo

**File**: `examples/order_fulfillment_demo.erl`

**Purpose**: Run the complete YAWL Order Fulfillment workflow.

**How to Run**:

```erlang
%% Full demo
order_fulfillment_demo:run_demo().

%% With custom order
order_fulfillment_demo:run_demo(OrderInput).

%% Test individual subprocesses
order_fulfillment_demo:run_simple_test().
```

---

## Expected Outputs

### Order Fulfillment Output

```
=== YAWL Order Fulfillment Demo ===
Order ID: DEMO-ORDER-001
Customer: John Doe (john.doe@example.com)
Items: [...]
Total: $131.76

[Processing logs...]

=== Order Fulfillment Complete ===
Order ID: DEMO-ORDER-001
Status: All subprocesses completed

Exporting XES log: fulfillment_log_...
XES log exported to xes_logs/fulfillment_log_....xes
```

### Van Der Aalst Workflow Output

```
Starting workflow execution...
[WCP-01] Sequence: Order verification complete
[WCP-02] Parallel Split: Concurrent checks started
[WCP-03] Synchronization: All branches complete
[WCP-04] Exclusive Choice: Credit path selected
...
[WCP-28] Try-Catch: Exception handling complete
[XES] Log exported to xes_logs/order_processing.xes
```

---

## Modifying Examples

### Adding a New Subprocess

1. **Create the module** in `examples/workflows/`:

```erlang
-module(my_subprocess).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3,
    init/1,
    terminate/2
]).

%% API
-export([new/1, start/1, run/1]).
```

2. **Define places and transitions**:

```erlang
place_lst() -> ['p_input', 'p_working', 'p_output'].
trsn_lst() -> ['t_start', 't_complete'].

init_marking('p_input', _) -> [start];
init_marking(_, _) -> [].

preset('t_start') -> ['p_input'];
preset('t_complete') -> ['p_working'];
preset(_) -> [].
```

3. **Implement the fire/3 callback**:

```erlang
fire('t_start', #{'p_input' := [start]}, State) ->
    {produce, #{'p_working' => [start]}};

fire('t_complete', #{'p_working' := [start]}, State) ->
    {produce, #{'p_output' => [start]}};
```

### Modifying Workflow Logic

**Change the order of subprocesses** in `order_fulfillment.erl`:

```erlang
%% Original: Ordering -> Carrier -> Payment -> Transit -> Delivery
%% Modified: Ordering -> Payment -> Carrier -> Transit -> Delivery

%% Modify the preset/1 function:
preset('t_start_payment') -> ['p_ordering_complete'];  %% Changed
preset('t_start_carrier') -> ['p_payment_complete'];   %% Changed
```

### Adding New YAWL Patterns

**Implement a new pattern** using the helper modules in `src/pnet/`:

```erlang
%% Example: Adding a Deferred Choice pattern
fire('t_select_option', #{'p_ready' := [start]}, State) ->
    Options = ['option_a', 'option_b'],
    Selected = pnet_choice:select(Options),
    {produce, #{Selected => [start]}}.
```

### Customizing Data Types

**Modify the shared types** in `include/order_fulfillment_types.hrl`:

```erlang
%% Add new fields to the order record
-record(order, {
    order_id,
    customer_id,
    %% ... existing fields ...
    priority = normal,       %% NEW: Order priority
    tags = []                %% NEW: Order tags
}).
```

---

## Testing and Debugging

### Running Tests

```bash
# Run all tests
rebar3 ct

# Run specific test suite
rebar3 ct --suite workflows_test

# Run with coverage
rebar3 cover
```

### Debugging with XES Logs

```erlang
%% Start XES logger
yawl_xes:start_link().

%% Export logs
{ok, Logs} = yawl_xes:list_logs(),
{LogId, _} = hd(Logs),
yawl_xes:export_xes(LogId, "xes_logs").

%% View log contents
file:read_file("xes_logs/" ++ binary_to_list(LogId) ++ ".xes").
```

### Monitoring Workflow Progress

```erlang
%% Check workflow status
{ok, Status} = order_fulfillment:get_status(Pid),
io:format("Status: ~p~n", [Status]).

%% Get full state
{ok, State} = order_fulfillment:get_state(Pid).
```

---

## References

- **YAWL Paper**: "Supporting Risk-Informed Decisions during Business Process Execution" (CAISE 2013)
- **Van Der Aalst Patterns**: "Workflow Patterns" (van der Aalst, ter Hofstede, Kiepuszewski, Barros)
- **gen_pnet Documentation**: See `src/gen_pnet/README.md`
- **CRE Architecture**: See `docs/ARCHITECTURE.md`

---

## Summary

| Example | Patterns | Complexity | Use Case |
|---------|----------|------------|----------|
| `example_basic_workflow` | WCP-01, WDP-01 | Beginner | Learn CRE basics |
| `ordering` | WCP-01, WCP-26 | Beginner | Order processing |
| `payment` | WCP-04, WHP-01, WCP-23 | Intermediate | Payment processing |
| `carrier_appointment` | WCP-06, WCP-08 | Intermediate | Shipping logistics |
| `freight_in_transit` | WCP-25, WCP-16 | Intermediate | Shipment tracking |
| `freight_delivered` | WCP-18 | Beginner | Delivery handling |
| `order_fulfillment` | WCP-01 | Advanced | Full order lifecycle |
| `van_der_aalst_workflow` | All 43 patterns | Expert | Pattern reference |
