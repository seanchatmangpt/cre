# YAWL Timeout and Cancellation Infrastructure

## Overview

This document describes the timeout and cancellation infrastructure implemented for all YAWL patterns in the CRE workflow system. The implementation provides comprehensive support for timeout handling, cancellation, deadlock detection, resource leak detection, and state persistence.

## Module: `yawl_timeout.erl`

The `yawl_timeout` module is a gen_server that provides centralized timeout and cancellation management for all YAWL workflow patterns.

### Key Features

1. **Timeout Support** - Per-pattern timeout configuration with detection and handling
2. **Cancellation Token Propagation** - Cooperative cancellation throughout pattern execution
3. **Deadlock Detection** - Circular wait condition detection and resolution
4. **Resource Leak Detection** - Automatic resource tracking and cleanup
5. **State Persistence** - Checkpoint-based state recovery for crash resilience

### API Functions

#### Timeout Management

| Function | Description |
|----------|-------------|
| `with_timeout/3,4` | Execute a function with timeout |
| `cancel_timeout/1` | Cancel an active timeout |
| `set_pattern_timeout/2` | Set timeout for a pattern |
| `get_pattern_timeout/1` | Get configured timeout |
| `start_timeout_monitor/2` | Start timeout monitor process |
| `stop_timeout_monitor/1` | Stop timeout monitor |
| `extend_timeout/2` | Extend timeout duration |
| `reset_timeout/1` | Reset to original timeout |

#### Cancellation Support

| Function | Description |
|----------|-------------|
| `new_cancellation_token/0,1` | Create new cancellation token |
| `check_cancelled/1` | Check if token is cancelled |
| `is_cancelled/1` | Alias for check_cancelled |
| `request_cancel/1,2` | Request cancellation |
| `throw_if_cancelled/1` | Throw if token cancelled |
| `link_token/2` | Link two tokens together |
| `unlink_token/2` | Unlink two tokens |
| `get_cancel_reason/1` | Get cancellation reason |

#### Deadlock Detection

| Function | Description |
|----------|-------------|
| `detect_deadlock/1` | Detect deadlocks in context |
| `resolve_deadlock/1` | Resolve detected deadlock |
| `check_cycle/1` | Check for cycles in wait graph |
| `add_wait_edge/3` | Add wait edge for tracking |
| `remove_wait_edge/2` | Remove wait edge |
| `get_wait_graph/1` | Get current wait graph |
| `detect_resource_deadlock/0` | Detect resource deadlocks |
| `break_deadlock/2` | Break deadlock with strategy |

#### Resource Tracking

| Function | Description |
|----------|-------------|
| `track_resource/2` | Track resource allocation |
| `release_resource/1` | Release tracked resource |
| `check_leaks/1` | Check for resource leaks |
| `get_resource_usage/1` | Get resource usage stats |
| `cleanup_all_resources/1` | Clean up all resources |
| `list_resources/1` | List tracked resources |
| `get_resource_info/2` | Get resource details |
| `set_resource_cleanup/2` | Set cleanup function |
| `mark_resource_critical/2` | Mark as critical |

#### State Persistence

| Function | Description |
|----------|-------------|
| `save_checkpoint/2,3` | Save state checkpoint |
| `load_checkpoint/1` | Load checkpoint |
| `list_checkpoints/0` | List all checkpoints |
| `cleanup_checkpoint/1` | Remove checkpoint |
| `restore_from_checkpoint/1` | Restore from latest |
| `checkpoint_before_block/2` | Save before blocking |
| `restore_after_block/1` | Restore after blocking |
| `get_checkpoint_info/1` | Get checkpoint details |
| `validate_checkpoint/1` | Validate checkpoint |

### Integration Points

The timeout module integrates with:

1. **`cre_yawl_patterns`** - Pattern execution functions
2. **`yawl_engine`** - Workflow case management
3. **`yawl_stateless`** - Stateless execution
4. **`gen_pnet`** - Petri net integration
5. **`yawl_executor`** - Workflow dispatch

### Configuration

Configuration is managed via `cre_config` persistent terms:

| Key | Default | Description |
|-----|---------|-------------|
| `yawl_timeout_checkpoint_dir` | `priv/yawl_checkpoints` | Checkpoint storage |
| `yawl_timeout_default_timeout` | `30000` | Default timeout (ms) |
| `yawl_timeout_deadlock_interval` | `5000` | Deadlock check interval (ms) |
| `yawl_timeout_resource_check_interval` | `60000` | Resource check interval (ms) |

### Usage Examples

#### Basic Timeout

```erlang
% Execute with 5 second timeout
Fun = fun() -> long_running_operation() end,
{ok, Result} = yawl_timeout:with_timeout(Fun, <<"my_pattern">>, 5000).
```

#### Cancellation

```erlang
% Create cancellable operation
Token = yawl_timeout:new_cancellation_token(<<"my_pattern">>),
Parent = self(),
spawn_link(fun() ->
    Result = yawl_timeout:with_timeout(Fun, PatternId, 10000, Token),
    Parent ! {result, Result}
end),

% Cancel if needed
yawl_timeout:request_cancel(Token).
```

#### Resource Tracking

```erlang
% Track a resource
{ok, ResourceId} = yawl_timeout:track_resource(
    <<"my_pattern">>,
    #{
        type => database_connection,
        cleanup_fn => fun(Id) -> close_connection(Id) end,
        critical => false,
        metadata => #{pool => main}
    }
),

% Release when done
yawl_timeout:release_resource(ResourceId).
```

#### Deadlock Detection

```erlang
% Check for deadlocks
case yawl_timeout:detect_deadlock(<<"my_pattern">>) of
    {ok, no_deadlock} -> ok;
    {ok, DeadlockInfo} ->
        logger:warning("Deadlock detected: ~p", [DeadlockInfo]),
        yawl_timeout:resolve_deadlock(<<"my_pattern">>)
end.
```

### State Persistence

The module supports state checkpointing for crash recovery:

```erlang
% Save state before blocking operation
{ok, CheckpointId} = yawl_timeout:checkpoint_before_block(
    <<"my_pattern">>,
    CurrentState
),

% After crash, restore state
{ok, RestoredState} = yawl_timeout:restore_after_block(<<"my_pattern">>).
```

### Supervisor Integration

The timeout gen_server is registered in the CRE supervisor:

```erlang
TimeoutSpec = #{
    id => yawl_timeout,
    start => {yawl_timeout, start_link, []},
    restart => permanent,
    shutdown => 5000,
    type => worker,
    modules => [yawl_timeout]
}
```

## Testing

Comprehensive test suite in `test/yawl_timeout_test.erl`:

- Timeout management tests
- Cancellation token tests
- Deadlock detection tests
- Resource tracking tests
- State persistence tests
- Integration tests

Run tests with:
```bash
rebar3 eunit -m yawl_timeout_test
```

## Files Modified

- `/Users/sac/cre/src/yawl_timeout.erl` - New module
- `/Users/sac/cre/src/cre_config.erl` - Added timeout configuration keys
- `/Users/sac/cre/src/cre_sup.erl` - Added timeout gen_server child
- `/Users/sac/cre/src/cre.app.src` - Added yawl_timeout to modules
- `/Users/sac/cre/test/yawl_timeout_test.erl` - Test suite

## Implementation Notes

### Timeout Handling

Timeouts use Erlang's `erlang:send_after/3` for efficient timer management. Each timeout has a monitor process that sends a timeout message to the parent.

### Cancellation Tokens

Cancellation tokens are identified by unique binary IDs and support:
- Linked cancellation (cascading)
- Observer notification
- Cancellation reasons
- Token metadata

### Deadlock Detection

Deadlock detection uses a wait-for graph and depth-first search to detect cycles:
- Processes waiting on resources
- Resources held by processes
- Circular wait conditions

### Resource Tracking

Resources are tracked in ETS tables with:
- Owner PID tracking
- Allocation timestamps
- Cleanup functions
- Critical resource marking
- Leak detection

### State Persistence

Checkpoints are stored:
1. In ETS for fast access
2. On disk for crash recovery
3. With version tracking for rollback support

## Future Enhancements

- Distributed deadlock detection across nodes
- Configurable deadlock resolution strategies
- Resource pooling integration
- Advanced checkpoint compression
- Timeout backoff strategies
- Cancellation observer callbacks
