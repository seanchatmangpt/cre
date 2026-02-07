# WRP (Resource) Patterns

## Overview

The WRP (Resource) patterns in YAWL handle the lifecycle management of resources within workflow executions. These patterns implement the van der Aalst workflow resource patterns, covering creation, allocation, initialization, role-based distribution, and deallocation of resources.

All WRP patterns are implemented as `gen_yawl` behaviours using Petri Net semantics via `gen_pnet`. Each pattern defines:

- A set of **places** representing states in the resource lifecycle
- A set of **transitions** representing state changes
- Token flow rules controlling resource movement through states
- State records tracking resource metadata

## Pattern Summary

| Pattern ID | Name | Module | Purpose |
|------------|------|--------|---------|
| WRP-01 | Direct Resource Creation | `direct_resource_creation` | Create resources on-demand within workflows |
| WRP-02 | Role-Based Allocation | `role_based_allocation` | Allocate resources based on role definitions |
| WRP-03 | Resource Initialization | `resource_initialization` | Initialize and validate resources before use |
| WRP-04 | Resource Allocation | `resource_allocation` | Manage allocation of resources to tasks |
| WRP-05 | Resource Deallocation | `resource_deallocation` | Release resources with proper cleanup |

---

## WRP-01: Direct Resource Creation

### Module
`direct_resource_creation`

### Purpose
Enables direct creation and initialization of resources within a workflow. Resources are created on-demand and made available for subsequent tasks. This pattern is useful when resources need to be dynamically instantiated during workflow execution.

### Use Cases
- Creating temporary data structures during workflow execution
- Instantiating service connections on-demand
- Generating unique identifiers or tokens
- Allocating memory resources for computation

### API Functions

```erlang
% Create a new Direct Resource Creation pattern state
-spec new(CreateFun :: function()) -> direct_resource_creation_state().

% Start the workflow as a gen_pnet process
-spec start(CreateFun :: function()) -> {ok, pid()} | {error, term()}.

% Get the current state
-spec get_state(Pid :: pid()) ->
    {ok, direct_resource_creation_state()} | {error, term()}.
```

### Petri Net Structure

**Places:**
- `p_start` - Start of resource creation (initial: `[start]`)
- `p_creating` - Resource in creation
- `p_ready` - Resource ready for use
- `p_end` - Pattern completed

**Transitions:**
| Transition | Preset | Postset | Description |
|------------|--------|---------|-------------|
| `t_create` | `p_start` | `p_ready` | Execute creation function |
| `t_finish` | `p_ready` | `p_end` | Complete the pattern |

**State Record:**
```erlang
-record(direct_resource_creation_state, {
    create_fun :: function(),      % Function to create resource
    resource :: undefined | term(), % The created resource
    status = pending :: pending | creating | ready | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

### Example Usage

```erlang
% Create a resource with a simple creation function
CreateFun = fun() -> #{id => 1, type => disk} end,
{ok, Pid} = direct_resource_creation:start(CreateFun),

% Check the state
{ok, State} = direct_resource_creation:get_state(Pid),
% State#direct_resource_creation_state.status -> ready
% State#direct_resource_creation_state.resource -> #{id => 1, type => disk}
```

### Resource Lifecycle Considerations
- The creation function is executed synchronously in the `t_create` transition
- Resources are created in the creator process's context
- No automatic cleanup is provided; consider pairing with WRP-05 for deallocation
- Creation failures will halt the pattern with `abort` status

---

## WRP-02: Role-Based Allocation

### Module
`role_based_allocation`

### Purpose
Allocates resources to tasks based on role definitions. Resources with matching roles are automatically assigned to tasks requiring those roles. This pattern supports organizational hierarchy and separation of concerns.

### Use Cases
- Assigning tasks to users with specific roles (admin, reviewer, approver)
- Routing work to specialized processing units
- Implementing permission-based task distribution
- Organizational workflow automation

### API Functions

```erlang
% Create a new Role-Based Allocation pattern state
-spec new(RequiredRole :: atom(), RoleMap :: map()) -> role_based_allocation_state().

% Start the workflow as a gen_pnet process
-spec start(RequiredRole :: atom(), RoleMap :: map()) -> {ok, pid()} | {error, term()}.

% Get the current state
-spec get_state(Pid :: pid()) ->
    {ok, role_based_allocation_state()} | {error, term()}.
```

### Petri Net Structure

**Places:**
- `p_start` - Start of allocation (initial: `[start]`)
- `p_checking` - Checking role match
- `p_allocated` - Resource allocated
- `p_failed` - Allocation failed
- `p_end` - Pattern completed

**Transitions:**
| Transition | Preset | Postset | Description |
|------------|--------|---------|-------------|
| `t_check_role` | `p_start` | `p_checking` | Check if role exists in role map |
| `t_allocate` | `p_checking` | `p_allocated` | Allocate first matching resource |
| `t_fail` | `p_checking` | `p_failed` | Handle unavailable role |
| `t_finish` | `p_allocated` | `p_end` | Complete the pattern |

**State Record:**
```erlang
-record(role_based_allocation_state, {
    required_role :: atom(),              % Role being requested
    role_map :: #{atom() => [term()]},   % Map of roles to resources
    allocated :: undefined | term(),      % Allocated resource
    status = pending :: pending | checking | allocated | failed | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

### Example Usage

```erlang
% Define role mappings
RoleMap = #{
    admin => [user1, user2],
    reviewer => [user3, user4, user5],
    approver => [user6]
},

% Request allocation for admin role
{ok, Pid} = role_based_allocation:start(admin, RoleMap),

% Check allocation result
{ok, State} = role_based_allocation:get_state(Pid),
% State#role_based_allocation_state.allocated -> user1
% State#role_based_allocation_state.status -> allocated
```

### Resource Lifecycle Considerations
- The first resource in the role list is allocated (head of list)
- Unavailable roles result in `failed` status with `p_failed` marked
- Resources are not removed from the role map; use WRP-04 for pool management
- Consider implementing round-robin or load balancing at the caller level

---

## WRP-03: Resource Initialization

### Module
`resource_initialization`

### Purpose
Handles the initialization phase of resources, ensuring they are properly configured before use. This may include loading configurations, establishing connections, or performing startup validation.

### Use Cases
- Database connection initialization
- Configuration loading and validation
- Service client setup with authentication
- Resource state preparation

### API Functions

```erlang
% Create a new Resource Initialization pattern state
-spec new(InitFun :: function(), Resource :: term()) -> resource_initialization_state().

% Start the workflow as a gen_pnet process
-spec start(InitFun :: function(), Resource :: term()) -> {ok, pid()} | {error, term()}.

% Get the current state
-spec get_state(Pid :: pid()) ->
    {ok, resource_initialization_state()} | {error, term()}.
```

### Petri Net Structure

**Places:**
- `p_start` - Start of initialization (initial: `[start]`)
- `p_initializing` - Resource being initialized
- `p_validated` - Resource validated
- `p_ready` - Resource ready for use
- `p_end` - Pattern completed

**Transitions:**
| Transition | Preset | Postset | Description |
|------------|--------|---------|-------------|
| `t_init` | `p_start` | `p_initializing` | Initialize the resource |
| `t_validate` | `p_initializing` | `p_validated` | Validate initialized resource |
| `t_finish` | `p_validated` | `p_ready`, `p_end` | Complete and mark ready |

**State Record:**
```erlang
-record(resource_initialization_state, {
    init_fun :: function(),              % Function to initialize resource
    resource :: term(),                  % Resource being initialized
    status = pending :: pending | initializing | validated | ready | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

### Example Usage

```erlang
% Initialize a database connection configuration
InitFun = fun(Conf) ->
    maps:merge(Conf, #{
        initialized => true,
        pool_size => 10,
        timeout => 5000
    })
end,

InitialConfig = #{host => "localhost", port => 5432},
{ok, Pid} = resource_initialization:start(InitFun, InitialConfig),

% Get initialized resource
{ok, State} = resource_initialization:get_state(Pid),
% State#resource_initialization_state.resource ->
%   #{host => "localhost", port => 5432, initialized => true, pool_size => 10, timeout => 5000}
```

### Resource Lifecycle Considerations
- The init function receives the resource and returns an updated version
- Validation is a separate transition, allowing for validation logic to be added
- Resources in `ready` state are safe for downstream tasks to use
- Initialization failures will abort the pattern; handle errors in the init function

---

## WRP-04: Resource Allocation

### Module
`resource_allocation`

### Purpose
Manages the allocation of resources to tasks. Ensures resources are properly assigned and tracks which resources are available or in use. Implements a pool-based allocation strategy.

### Use Cases
- Connection pool management
- Worker thread allocation
- Limited resource distribution (e.g., licenses, permits)
- Load balancing across resources

### API Functions

```erlang
% Create a new Resource Allocation pattern state
-spec new(Resources :: [term()], TaskId :: term()) -> resource_allocation_state().

% Start the workflow as a gen_pnet process
-spec start(Resources :: [term()], TaskId :: term()) -> {ok, pid()} | {error, term()}.

% Get the current state
-spec get_state(Pid :: pid()) ->
    {ok, resource_allocation_state()} | {error, term()}.
```

### Petri Net Structure

**Places:**
- `p_start` - Start of allocation (initial: `[start]`)
- `p_available` - Available resources (initial: resource list)
- `p_allocating` - Allocation in progress
- `p_allocated` - Resource allocated to task
- `p_busy` - Resource busy/in use
- `p_end` - Pattern completed

**Transitions:**
| Transition | Preset | Postset | Description |
|------------|--------|---------|-------------|
| `t_request` | `p_start` | `p_allocating` | Request allocation for task |
| `t_allocate` | `p_allocating`, `p_available` | `p_allocated` | Allocate available resource |
| `t_release` | `p_busy` | `p_allocated`, `p_available` | Release allocated resource |
| `t_finish` | `p_allocated` | `p_end` | Complete the pattern |

**State Record:**
```erlang
-record(resource_allocation_state, {
    resources :: [term()],           % All resources
    available :: [term()],           % Currently available
    allocated :: undefined | term(), % Currently allocated
    task_id :: term(),               % Task requesting allocation
    status = pending :: pending | requesting | allocated | busy | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

### Example Usage

```erlang
% Define a resource pool
Resources = [conn1, conn2, conn3],

% Allocate to a task
{ok, Pid} = resource_allocation:start(Resources, task_123),

% Check allocation
{ok, State} = resource_allocation:get_state(Pid),
% State#resource_allocation_state.allocated -> conn1
% State#resource_allocation_state.available -> [conn2, conn3]
```

### Resource Lifecycle Considerations
- Resources are allocated in FIFO order (head of list first)
- The `p_available` place is initially marked with all resources
- Released resources return to `p_available` for re-allocation
- No blocking for exhausted pools; the pattern will not fire `t_allocate` without available resources
- Consider implementing retry logic at the workflow level for exhausted pools

---

## WRP-05: Resource Deallocation

### Module
`resource_deallocation`

### Purpose
Manages the release of resources after task completion. Ensures proper cleanup and makes resources available for future allocations. Critical for preventing resource leaks in long-running workflows.

### Use Cases
- Connection cleanup and return to pool
- File handle closure
- Memory deallocation
- Temporary resource disposal

### API Functions

```erlang
% Create a new Resource Deallocation pattern state
-spec new(CleanupFun :: function(), Resource :: term()) -> resource_deallocation_state().

% Start the workflow as a gen_pnet process
-spec start(CleanupFun :: function(), Resource :: term()) -> {ok, pid()} | {error, term()}.

% Get the current state
-spec get_state(Pid :: pid()) ->
    {ok, resource_deallocation_state()} | {error, term()}.
```

### Petri Net Structure

**Places:**
- `p_start` - Start of deallocation (initial: `[start]`)
- `p_releasing` - Resource being released
- `p_cleanup` - Cleanup in progress
- `p_available` - Resource available again
- `p_deallocated` - Resource deallocated
- `p_end` - Pattern completed

**Transitions:**
| Transition | Preset | Postset | Description |
|------------|--------|---------|-------------|
| `t_start_release` | `p_start` | `p_releasing` | Initiate resource release |
| `t_cleanup` | `p_releasing` | `p_cleanup` | Perform cleanup operations |
| `t_make_available` | `p_cleanup` | `p_available`, `p_deallocated` | Mark resource as available |
| `t_finish` | `p_deallocated` | `p_end` | Complete the pattern |

**State Record:**
```erlang
-record(resource_deallocation_state, {
    cleanup_fun :: function(),  % Function to cleanup resource
    resource :: term(),          % Resource being deallocated
    status = pending :: pending | releasing | cleanup | available | deallocated | completed,
    start_time :: integer(),
    log_id :: binary() | undefined
}).
```

### Example Usage

```erlang
% Define cleanup function
CleanupFun = fun(Conn) ->
    % Close connection, clear state, etc.
    maps:put(closed, true, Conn)
end,

% Deallocate a resource
{ok, Pid} = resource_deallocation:start(CleanupFun, connection_1),

% Check deallocation
{ok, State} = resource_deallocation:get_state(Pid),
% State#resource_deallocation_state.resource -> #{closed => true, ...}
% State#resource_deallocation_state.status -> deallocated
```

### Resource Lifecycle Considerations
- The cleanup function can transform the resource state before return to pool
- Resources transition through explicit cleanup state, ensuring cleanup always runs
- The `p_available` place indicates resources ready for re-allocation
- Combine with WRP-04 for complete allocation-deallocation cycles
- Cleanup failures will abort the pattern; ensure cleanup functions handle errors gracefully

---

## Resource Lifecycle Integration

The WRP patterns are designed to work together in sequence:

```
WRP-01 (Creation)
       |
       v
WRP-03 (Initialization)
       |
       v
WRP-02 (Role-Based Allocation) or WRP-04 (Allocation)
       |
       v
[Resource in Use]
       |
       v
WRP-05 (Deallocation)
       |
       v
[Available for Reallocation]
```

### Typical Workflow

```erlang
% 1. Create a resource
CreateFun = fun() -> #{id => new_resource()} end,
{ok, CreatePid} = direct_resource_creation:start(CreateFun),

% 2. Initialize it
InitFun = fun(R) -> maps:put(configured, true, R) end,
{ok, InitPid} = resource_initialization:start(InitFun, Resource),

% 3. Allocate based on role
{ok, AllocPid} = role_based_allocation:start(worker, RoleMap),

% 4. Use resource for task...

% 5. Deallocate with cleanup
CleanupFun = fun(R) -> cleanup(R) end,
{ok, DeallocPid} = resource_deallocation:start(CleanupFun, UsedResource),
```

---

## Common Patterns and Best Practices

### Error Handling
All WRP patterns return `abort` from the `fire/3` callback when transitions fire in unexpected modes. Wrap pattern execution in error-handling code:

```erlang
case direct_resource_creation:start(CreateFun) of
    {ok, Pid} ->
        {ok, State} = direct_resource_creation:get_state(Pid),
        handle_success(State);
    {error, Reason} ->
        handle_failure(Reason)
end
```

### Status Monitoring
Each pattern state includes a `status` field and `start_time` for monitoring:

```erlang
{ok, State} = pattern_module:get_state(Pid),
CurrentStatus = State#pattern_state.status,
ElapsedTime = erlang:system_time(millisecond) - State#pattern_state.start_time,
```

### Composition with YAWL Workflows
WRP patterns can be embedded in larger YAWL workflow nets:

```erlang
% In a parent workflow's fire/3 callback
fire('t_allocate_resource', _Mode, _UsrInfo) ->
    {ok, AllocPid} = resource_allocation:start(Resources, TaskId),
    {produce, #{'p_allocated' => [{allocator, AllocPid}]}}.
```

---

## References

- van der Aalst, W. M. P., et al. "Workflow Resource Patterns: Draft." Eindhoven University of Technology, 2005.
- YAWL (Yet Another Workflow Language) Specification
- CRE Pattern Implementation Guide

---

## Module Index

- `direct_resource_creation` - WRP-01
- `role_based_allocation` - WRP-02
- `resource_initialization` - WRP-03
- `resource_allocation` - WRP-04
- `resource_deallocation` - WRP-05
