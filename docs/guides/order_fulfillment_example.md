# Order Fulfillment YAWL 2.1 - Usage Guide

## Overview

This guide shows how to use the Order Fulfillment workflow with the CRE YAWL engine. The Order Fulfillment workflow is a comprehensive example from the CAISE 2013 paper demonstrating complex workflow patterns including:

- Sequential subprocess execution
- Payment method branching (FTL/LTL/SP)
- Cancellation regions
- Timer-based timeout handling
- Multi-instance task coordination

**Specification**: `test/fixtures/orderfulfillment_2_1.yawl`

**Test Helpers**: `test/yawl_of_helpers.erl`

**Demo**: `test/yawl_of_demo.erl`

**Reference**: Supporting Risk-Informed Decisions during Business Process Execution (CAISE 2013)

---

## Table of Contents

1. [Quick Start Example](#quick-start-example)
2. [Loading the YAWL Specification](#loading-the-yawl-specification)
3. [Compiling to pnet_net](#compiling-to-pnet_net)
4. [Executing with gen_pnet](#executing-with-gen_pnet)
5. [Handling Payment Branching](#handling-payment-branching)
6. [Working with Cancellation](#working-with-cancellation)
7. [API Reference](#api-reference)
8. [Troubleshooting](#troubleshooting)

---

## Quick Start Example

The fastest way to run the Order Fulfillment workflow is using the demo module:

```erlang
% Start the CRE application
application:ensure_all_started(cre).

% Run the demo with default order
yawl_of_demo:run().

% Run with custom options
yawl_of_demo:run(#{verbose => true, seed => 42}).
```

**Expected Output**:
```
=== YAWL Order Fulfillment Demo ===
Order ID: ORD-001
Customer: Acme Corp
Items: [...]
Total: $299.90

=== Order Fulfillment Complete ===
Order ID: ORD-001
Status: All subprocesses completed
```

---

## Loading the YAWL Specification

### Method 1: Using wf_spec (Recommended)

```erlang
% Parse the YAWL specification from file
{ok, Spec} = wf_spec:from_xml_file("test/fixtures/orderfulfillment_2_1.yawl").

% Verify specification loaded correctly
SpecId = wf_spec:id(Spec),
% Returns: <<"orderfulfillment">>

Title = wf_spec:title(Spec),
% Returns: <<"Order Fulfillment">>

% Validate the specification
ok = wf_spec:validate(Spec).
```

### Method 2: Using from_xml/1 with Binary

```erlang
% Read file content directly
{ok, XmlContent} = file:read_file("test/fixtures/orderfulfillment_2_1.yawl").

% Parse from binary
{ok, Spec} = wf_spec:from_xml(XmlContent).
```

### Specification Structure

The Order Fulfillment spec contains:

```erlang
% Get root decomposition
RootNet = wf_spec:root_net(Spec),
% Returns: <<"Overall">>

% Get all tasks
Tasks = wf_spec:tasks(Spec),
% Returns: ['Ordering_3', 'Carrier_Appointment_4', 'Payment_5', ...]

% Get task information
TaskType = wf_spec:task_type(Spec, 'Payment_5'),
% Returns: human | atomic

SplitType = wf_spec:split_type(Spec, 'Carrier_Appointment_4'),
% Returns: 'or' (for branching)
```

---

## Compiling to pnet_net

The YAWL specification must be compiled into a gen_pnet-compatible module.

### Basic Compilation

```erlang
% Compile the specification
{ok, Compiled} = wf_spec:compile(Spec).

% Access compiled structure
#{spec_id := SpecId,
  modules := Modules,
  places := Places,
  transitions := Transitions} = Compiled.
```

### Writing Compiled Modules to Files

```erlang
% Compile and write to output directory
{ok, Files} = yawl_compile:compile_to_file(
    Spec,
    #{},  % Empty options map
    "src/compiled"  % Output directory
).

% Files contains paths to generated .erl files
```

### Compilation Options

```erlang
Options = #{
    seed => 42,                    % For deterministic RNG
    module_prefix => <<"yawl_">>,  % Module name prefix
    output_dir => "src/compiled",
    include_source => true,         % Include YAWL source in docs
    gen_observer => false           % Generate observer callbacks
}.

{ok, Compiled} = yawl_compile:compile(Spec, Options).
```

### Compiled Module Structure

Each decomposition generates a module like:

```erlang
-module(yawl_Overall).
-behaviour(gen_pnet).

% Net structure
place_lst() -> [input, ordering, carrier_appointment, ...].
trsn_lst() -> [t_ordering, t_carrier, ...].

% Callbacks
init_marking(Place, UsrInfo) -> ...
preset(Transition) -> ...
is_enabled(Transition, Mode, UsrInfo) -> ...
fire(Transition, Mode, UsrInfo) -> ...
```

---

## Executing with gen_pnet

### Starting the Workflow

```erlang
% Method 1: Using the high-level orchestrator
OrderInput = #{
    <<"order_id">> => <<"ORD-001">>,
    <<"customer_id">> => <<"CUST-123">>,
    <<"items">> => [#{<<"sku">> => <<"WIDGET-001">>, <<"quantity">> => 5}],
    <<"total">> => 149.95
}.

{ok, Pid} = order_fulfillment:start(OrderInput).

% Method 2: Direct gen_pnet start
{ok, Pid} = gen_pnet:start_link(yawl_Overall, #{}, []).

% Method 3: Registered name
{ok, Pid} = gen_pnet:start_link({local, order_fulfill}, yawl_Overall, #{}, []).
```

### Synchronous Execution

```erlang
% Run workflow to completion
Result = order_fulfillment:run(OrderInput).

case Result of
    {ok, State} ->
        io:format("Order completed successfully~n");
    {error, Reason} ->
        io:format("Order failed: ~p~n", [Reason])
end.
```

### Monitoring Execution

```erlang
% Check current status
Status = order_fulfillment:get_status(Pid),
% Returns: #{
%   <<"order_id">> => <<"ORD-001">>,
%   <<"ordering">> => <<"ok">>,
%   <<"carrier">> => <<"ok">>,
%   <<"payment">> => <<"pending">>,
%   ...
% }

% Get full state
{ok, State} = order_fulfillment:get_state(Pid).

% Query marking (Petri net state)
Marking = gen_pnet:marking(Pid),
% Returns: #{place1 => [token1, token2], place2 => [], ...}
```

### Step-by-Step Execution

```erlang
% Fire one transition
case gen_pnet:step(Pid) of
    {ok, Receipt} ->
        io:format("Transition fired: ~p~n", [Receipt]);
    abort ->
        io:format("No enabled transitions~n")
end.

% Fire transitions until quiescence or limit
{ok, Receipts} = gen_pnet:drain(Pid, 100).

% Check for limit reached
case gen_pnet:drain(Pid, 10) of
    {ok, Receipts} -> io:format("Completed: ~p receipts~n", [Receipts]);
    {error, limit} -> io:format("Hit step limit (not completed)~n")
end.
```

### Stopping the Workflow

```erlang
% Graceful stop
ok = gen_pnet:stop(Pid).

% The terminate/2 callback will be invoked
% XES log will be finalized if configured
```

---

## Handling Payment Branching

The Order Fulfillment workflow uses XOR branching based on shipment characteristics:

### Payment Method Determination

```erlang
% Using the handler module
OrderData = #{
    total_amount => 15000,
    weight => 1000,
    distance => 500,
    customer_tier => standard
}.

Rules = #{
    ftl_threshold => 10000,
    ltl_threshold => 1000,
    sp_threshold => 100
}.

% Determine payment method
{ok, PaymentMethod} = yawl_of_helpers:determine_payment_method(
    OrderData,
    Rules
).

% PaymentMethod will be:
% - ftl (Full Truck Load): volume >= 10000 OR very heavy
% - ltl (Less Than Truckload): volume >= 1000 OR medium weight
% - sp (Small Parcel): small orders
```

### Branch Selection Logic

```erlang
% FTL: Full Truck Load (>= 10000 volume)
{ok, ftl} = yawl_of_helpers:determine_payment_method(
    #{total_volume => 15000, weight => 1000, distance => 100},
    #{ftl_threshold => 10000, ltl_threshold => 1000}
).

% LTL: Less Than Truckload (>= 1000 volume, < 10000)
{ok, ltl} = yawl_of_helpers:determine_payment_method(
    #{total_volume => 5000, weight => 500, distance => 100},
    #{ftl_threshold => 10000, ltl_threshold => 1000}
).

% SP: Small Parcel (< 1000 volume)
{ok, sp} = yawl_of_helpers:determine_payment_method(
    #{total_volume => 500, weight => 50, distance => 50},
    #{ftl_threshold => 10000, ltl_threshold => 1000}
).
```

### Carrier Selection by Payment Method

```erlang
% FTL Carrier Selection
{ok, Carrier} = yawl_of_helpers:select_carrier(
    ftl,
    #{distance => 2000}
),
% Returns: {ok, xpo} for long haul

% LTL Carrier Selection
{ok, Carrier} = yawl_of_helpers:select_carrier(
    ltl,
    #{distance => 500, urgency => normal}
),
% Returns: {ok, estes} for regional

% SP (Small Parcel) Carrier Selection
{ok, Carrier} = yawl_of_helpers:select_carrier(
    sp,
    #{distance => 200, international => false}
),
% Returns: {ok, ontrac} for regional
```

### Shipping Cost Calculation

```erlang
OrderData = #{
    weight => 1000,
    distance => 500
}.

{ok, Cost} = yawl_of_helpers:calculate_shipping(
    OrderData,
    fedex_freight
),
% Returns: {ok, 370.00} (base + weight_factor + distance_factor)
```

---

## Working with Cancellation

The Carrier Timeout task demonstrates cancellation region handling.

### Cancellation Regions in YAWL

```erlang
% Get cancellation regions from spec
CancelRegions = wf_spec:cancellation_regions(Spec).

% Each region: {TaskId, CancellationSet}
% Example: {carrier_timeout, [estimate_trailer_usage,
%                             prepare_route_guide,
%                             prepare_transportation_quote]}
```

### Cancellation Set Verification

```erlang
% Check what a task cancels
CancelSet = wf_spec:cancellation_set(Spec, 'Carrier_Timeout'),

% Returns list of task IDs that get canceled
% when Carrier_Timeout fires (timeout expires)
```

### Testing Cancellation

```erlang
% In unit tests, verify cancellation works
ActiveMarking = #{
    calculate_carrier_timeout => [fired],
    estimate_trailer_usage => [active],
    prepare_route_guide => [active],
    prepare_transportation_quote => [pending]
}.

% After carrier timeout fires:
CanceledMarking = #{
    calculate_carrier_timeout => [fired],
    estimate_trailer_usage => [],  % CANCELED
    prepare_route_guide => [],     % CANCELED
    prepare_transportation_quote => []  % CANCELED
}.
```

### Timeout State Variables

```erlang
% The workflow tracks timeout state in variables
Variables = [
    {<<"PO_timedout">>, false},   % Purchase Order timeout
    {<<"SP_timedout">>, false}     % Shipment Quote timeout
].

% When timeout occurs:
SP_timedout = true,
% Payment flow redirects based on this flag
```

---

## API Reference

### order_fulfillment Module

#### new/1

```erlang
% Create new fulfillment state from order record
State = order_fulfillment:new(#order{
    order_id => <<"ORD-001">>,
    customer_id => <<"CUST-123">>,
    items => [#item{sku = <<"WIDGET-001">>, quantity = 5}],
    total => 149.95
}).

% Create from map
State = order_fulfillment:new(#{
    <<"order_id">> => <<"ORD-001">>,
    <<"customer_id">> => <<"CUST-123">>,
    <<"items">> => [#{<<"sku">> => <<"WIDGET-001">>, <<"quantity">> => 5}],
    <<"total">> => 149.95
}).
```

#### start/1

```erlang
% Start workflow process
{ok, Pid} = order_fulfillment:start(OrderInput).

{error, Reason} = order_fulfillment:start(InvalidInput).
```

#### run/1

```erlang
% Synchronous execution with 5-minute timeout
{ok, FinalState} = order_fulfillment:run(OrderInput).

{error, timeout} = order_fulfillment:run(LargeOrder).

{error, {payment_failed, _}} = order_fulfillment:run(OrderInput).
```

#### get_status/1

```erlang
% Query workflow status
Status = order_fulfillment:get_status(Pid).
% Returns: #{
%   <<"order_id">> => <<"ORD-001">>,
%   <<"ordering">> => <<"ok">>,
%   <<"carrier">> => <<"ok">>,
%   <<"payment">> => <<"ok">>,
%   <<"transit">> => <<"pending">>,
%   <<"delivery">> => <<"pending">>,
%   <<"started_at">> => 1704067200000,
%   <<"completed_at">> => undefined
% }
```

#### get_state/1

```erlang
% Get full internal state
{ok, State} = order_fulfillment:get_state(Pid).
```

### yawl_of_helpers Module (Test Helpers)

#### determine_payment_method/2

```erlang
-spec determine_payment_method(OrderData :: order_data(),
                               Rules :: rules()) ->
    {ok, ftl | ltl | sp} | {error, Reason :: term()}.

OrderData = #{
    total_amount => 15000,
    weight => 1000,
    distance => 500,
    customer_tier => standard
}.

Rules = #{
    ftl_threshold => 10000,
    ltl_threshold => 1000
}.

{ok, PaymentMethod} = yawl_of_helpers:determine_payment_method(
    OrderData,
    Rules
).
```

#### select_carrier/2

```erlang
-spec select_carrier(ShippingMethod :: ftl | ltl | sp,
                     OrderData :: order_data()) ->
    {ok, Carrier :: carrier()} | {error, Reason :: term()}.

% Returns carriers:
% FTL: fedex_freight, ups_freight, estes, xpo
% LTL: fedex_freight, ups_freight, estes
% SP: fedex, ups, dhl, usps, ontrac
```

#### calculate_shipping/2

```erlang
-spec calculate_shipping(OrderData :: order_data(),
                        Carrier :: carrier()) ->
    {ok, Cost :: number()} | {error, Reason :: term()}.

{ok, Cost} = yawl_of_helpers:calculate_shipping(
    #{weight => 100, distance => 500},
    fedex
).
```

### gen_pnet Workflow APIs

#### Starting and Stopping

```erlang
% Start workflow
{ok, Pid} = gen_pnet:start_link(NetModule, NetArg, Options).

% Stop workflow
ok = gen_pnet:stop(Pid).
```

#### Querying State

```erlang
% Get marking (all places and tokens)
Marking = gen_pnet:marking(Pid).

% Get tokens at specific place
{ok, Tokens} = gen_pnet:ls(Pid, place_name).

% Get user info
UsrInfo = gen_pnet:usr_info(Pid).

% Get statistics
#stats{current = #stat{fps = Fps}} = gen_pnet:stats(Pid).
```

#### Execution Control

```erlang
% Fire one transition
{ok, Receipt} = gen_pnet:step(Pid).

% Fire until quiescence
{ok, Receipts} = gen_pnet:drain(Pid, MaxSteps).

% Inject tokens
{ok, Receipt} = gen_pnet:inject(Pid, #{place => [token]}).

% State property check
ok = gen_pnet:state_property(Pid, fun([T]) -> length(T) > 0 end, [place]).
```

### wf_spec Module

#### Loading Specifications

```erlang
% From file
{ok, Spec} = wf_spec:from_xml_file("path/to/workflow.yawl").

% From binary
{ok, Spec} = wf_spec:from_xml(XmlBinary).

% Validate
ok = wf_spec:validate(Spec).
```

#### Querying Specification

```erlang
% Basic info
Id = wf_spec:id(Spec),
Title = wf_spec:title(Spec),
RootNet = wf_spec:root_net(Spec).

% Tasks
Tasks = wf_spec:tasks(Spec),
TaskType = wf_spec:task_type(Spec, TaskId),
SplitType = wf_spec:split_type(Spec, TaskId),
JoinType = wf_spec:join_type(Spec, TaskId).

% Decompositions
Decomps = wf_spec:decomposition_nets(Spec),
{ok, DecompTasks} = wf_spec:decomposition_tasks(Spec, NetId).

% Cancellation
CancelRegions = wf_spec:cancellation_regions(Spec),
CancelSet = wf_spec:cancellation_set(Spec, TaskId).
```

#### Compilation

```erlang
% Compile to structure
{ok, Compiled} = wf_spec:compile(Spec).

% Compile to files
{ok, Files} = yawl_compile:compile_to_file(Spec, Options, OutputDir).
```

---

## Troubleshooting

### Common Issues

#### 1. Specification File Not Found

**Problem**: `{error, enoent}` when loading YAWL file.

**Solution**:
```erlang
% Check file exists
filelib:is_file("test/fixtures/orderfulfillment_2_1.yawl").

% Use absolute path
{ok, Cwd} = file:get_cwd(),
YawlPath = filename:join([Cwd, "test", "fixtures", "orderfulfillment_2_1.yawl"]),
{ok, Spec} = wf_spec:from_xml_file(YawlPath).
```

#### 2. Module Not Compiled

**Problem**: `{error, unloaded_module}` when starting workflow.

**Solution**:
```erlang
% Compile first
{ok, Files} = yawl_compile:compile_to_file(Spec, #{}, "src/compiled").

% Load modules
[Module:module_info() || File <- Files].

% Or add to code path
code:add_patha("src/compiled").
```

#### 3. Transition Not Enabled

**Problem**: `abort` returned from `gen_pnet:step/1`.

**Solution**:
```erlang
% Check marking
Marking = gen_pnet:marking(Pid),
% Verify places have tokens

% Check preset for transition
Preset = yawl_Overall:preset(TransitionName),
% Verify preset places have tokens

% Check is_enabled condition
Mode = #{Place1 => [Token], Place2 => [Token]},
Enabled = yawl_Overall:is_enabled(TransitionName, Mode, UsrInfo).
```

#### 4. Timeout During Execution

**Problem**: `{error, timeout}` from `order_fulfillment:run/1`.

**Solution**:
```erlang
% Check subprocess status
Status = order_fulfillment:get_status(Pid).

% Verify all subprocesses are available
application:which_applications().
% cre should be in list

% Check for deadlock
% Get marking and verify no tokens stuck
Marking = gen_pnet:marking(Pid).
```

#### 5. Deterministic Execution Issues

**Problem**: Different results on repeated runs.

**Solution**:
```erlang
% Use seed for deterministic RNG
{ok, Pid} = gen_pnet:start_link(
    NetModule,
    #{seed => 42},  % Fixed seed
    []
).

% Verify same seed produces same results
{ok, Pid1} = gen_pnet:start_link(NetModule, #{seed => 42}, []),
{ok, Pid2} = gen_pnet:start_link(NetModule, #{seed => 42}, []),
% Both should behave identically
```

### Debugging Tips

#### Enable Logging

```erlang
% Start XES logger for event tracing
application:ensure_all_started(cre),
{ok, XesPid} = yawl_xes:start_link().

% Export XES log
case yawl_xes:list_logs() of
    [{LogId, _} | _] ->
        {ok, Content} = yawl_xes:export_xes(LogId, "xes_logs"),
        io:format("Log exported to xes_logs/~s.xes~n", [LogId])
end.
```

#### Trace Token Flow

```erlang
% Subscribe to place changes
gen_pnet:call(Pid, {subscribe_place, place_name}).

% Query marking after each step
{ok, _} = gen_pnet:step(Pid),
Marking = gen_pnet:marking(Pid),
io:format("Marking: ~p~n", [Marking]).
```

#### Verify Net Structure

```erlang
% Check places
Places = yawl_Overall:place_lst(),
io:format("Places: ~p~n", [Places]).

% Check transitions
Transitions = yawl_Overall:trsn_lst(),
io:format("Transitions: ~p~n", [Transitions]).

% Check presets
lists:map(fun(T) ->
    Preset = yawl_Overall:preset(T),
    io:format("~w preset: ~p~n", [T, Preset])
end, Transitions).
```

### Testing Patterns

#### Unit Test Structure

```erlang
% Basic test structure
setup() ->
    {ok, Pid} = order_fulfillment:start(test_order()),
    Pid.

cleanup(Pid) ->
    gen_pnet:stop(Pid).

payment_split_test() ->
    Pid = setup(),

    % Inject to payment branch point
    {ok, _} = gen_pnet:inject(Pid, #{
        'p_payment_ready' => [start]
    }),

    % Drain to completion
    {ok, Receipts} = gen_pnet:drain(Pid, 50),

    % Verify branch taken
    Marking = gen_pnet:marking(Pid),
    ?assertEqual([done], maps:get('p_output', Marking)),

    cleanup(Pid).
```

---

## Additional Resources

### Related Documentation

- **[COMPLETE_API_REFERENCE.md](COMPLETE_API_REFERENCE.md)** - Full API documentation
- **[CORE_YAWL_PATTERNS_GUIDE.md](CORE_YAWL_PATTERNS_GUIDE.md)** - YAWL patterns reference
- **[YAWL_PATTERNS_WORKBOOK.md](YAWL_PATTERNS_WORKBOOK.md)** - Practice exercises
- **[HUMAN_IN_THE_LOOP.md](HUMAN_IN_THE_LOOP.md)** - Approval workflows

### Source Files

- **Specification**: `test/fixtures/orderfulfillment_2_1.yawl`
- **Test Helpers**: `test/yawl_of_helpers.erl`
- **Demo**: `test/yawl_of_demo.erl`
- **Integration Tests**: `test/orderfulfillment_integration_SUITE.erl`

### Related Documentation

- **Handler Development**: See `docs/tutorials/HANDLER_DEVELOPMENT.md`
- **YAWL Patterns**: See `docs/CORE_YAWL_PATTERNS_GUIDE.md`
- **Complete API Reference**: See `docs/COMPLETE_API_REFERENCE.md`

---

*Last Updated: February 2025*
*CRE Version: 0.3.0*
*YAWL Specification: 2.1*
