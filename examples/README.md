# CRE YAWL Petri Net Integration Examples

This directory contains integration examples demonstrating the use of the new helper modules with `gen_pnet` for implementing YAWL-style workflows.

## Files

### `yawl_pnet_example.erl`

A complete working example module that implements an order processing workflow with approval using the `gen_pnet` behavior. It demonstrates:

- **`pnet_types`**: Type definitions for places, tokens, markings, modes, and receipts
- **`pnet_marking`**: Marking algebra for state management (add, take, apply operations)
- **`pnet_receipt`**: Receipt creation for audit trail of state transitions
- **`wf_task`**: Token constructors for external work integration

#### Workflow Structure

The example implements a Petri net for order processing:

```
p_start
  |
t_receive
  |
p_validating --> t_validate --> p_validated
  |                                    |
  |                             t_route
  |                                    |
  |                             +------+------+
  |                             |             |
  |                        p_pending       p_processing
  |                             |             |
  |                        t_approve        |
  |                             |             |
  |                        p_approved       |
  |                             \            /
  |                              \          /
  |                               t_payment
  |                                    |
  |                             p_paying --> t_ship --> p_shipping
  |                                                           |
  |                                                        t_complete
  |                                                           |
  +------------------------------------------------------- p_complete
```

#### Running the Example

```erlang
%% Start the workflow
{ok, Pid} = yawl_pnet_example:start_link().

%% Submit a low-value order (direct processing)
yawl_pnet_example:submit_order(Pid, <<"customer1">>, [
    #{<<"sku">> => <<"PROD_BASIC">>, <<"quantity">> => 2},
    #{<<"sku">> => <<"PROD_STANDARD">>, <<"quantity">> => 1}
]).

%% Submit a high-value order (requires approval)
yawl_pnet_example:submit_order(Pid, <<"customer2">>, [
    #{<<"sku">> => <<"PROD_ENTERPRISE">>, <<"quantity">> => 1}
]).

%% Check current marking
yawl_pnet_example:get_marking(Pid).

%% Inject approval decision
yawl_pnet_example:inject_approval(Pid, OrderId, approved).

%% Get receipt history
yawl_pnet_example:get_receipts(Pid).
```

### `yawl_pnet_demo.erl`

A standalone demo script that runs through three scenarios:

1. **Low-value order**: Direct processing without approval
2. **High-value order**: Routing to approval, then processing after approval
3. **Multiple concurrent orders**: Demonstrating parallel processing

### `yawl_pnet_demo.sh`

A shell script to compile and run the demo:

```bash
./examples/yawl_pnet_demo.sh
```

## Helper Modules Referenced

| Module | Purpose |
|--------|---------|
| `pnet_types` | Type definitions and validation helpers |
| `pnet_marking` | Marking algebra operations (new, get, set, add, take, apply) |
| `pnet_receipt` | Receipt creation for audit trail |
| `wf_task` | Token injection constructors for external work |
| `pnet_mode` | Mode enumeration for transition firing |
| `pnet_choice` | Deterministic choice operations |

## Compiling

The examples are compiled automatically by the demo script, or manually:

```bash
cd /Users/sac/cre
rebar3 compile
erlc -I include -o ebin examples/yawl_pnet_example.erl
```

## Interactive Testing

Start an Erlang shell with the CRE application:

```bash
cd /Users/sac/cre
rebar3 shell
```

Then run the example interactively as shown above.
