# YAWL Java vs CRE Erlang: Architecture Comparison

## Executive Summary

This document compares the architectural design of two YAWL (Yet Another Workflow Language) implementations:

1. **YAWL Java** - The reference implementation in Java (2004-2020)
2. **CRE Erlang** - A reimagined implementation in Erlang/OTP (2015-2025)

Both implement the YAWL workflow language but take fundamentally different approaches to concurrency, state management, error handling, and persistence.

---

## 1. OOP Design vs Actor Design

### YAWL Java: Object-Oriented Design

YAWL Java follows traditional object-oriented patterns with deep inheritance hierarchies and mutable state.

#### Key Classes and Hierarchy

```java
// Core engine class
public class YEngine implements InterfaceADesign,
                                InterfaceAManagement,
                                InterfaceBClient,
                                InterfaceBInterop {
    // Static singleton instance
    protected static YEngine _thisInstance;

    // Mutable instance fields
    private YWorkItemRepository _workItemRepository;
    protected YNetRunnerRepository _netRunnerRepository;
    private Map<YIdentifier, YSpecification> _runningCaseIDToSpecMap;
    private Map<String, YAWLServiceReference> _yawlServices;

    // Enum-based state management
    public enum Status { Dormant, Initialising, Running, Terminating }
}

// Net runner for workflow execution
public class YNetRunner {
    private YNet _net;
    private YIdentifier _caseIDForNet;
    private Set<YTask> _enabledTasks = new HashSet<YTask>();
    private Set<YTask> _busyTasks = new HashSet<YTask>();

    public enum ExecutionStatus { Normal, Suspending, Suspended, Resuming }
}

// Exception hierarchy
public class YStateException extends YAWLException {
    public YStateException(String msg) { super(msg); }
}

public class YPersistenceException extends YAWLException {
    // Fatal exception - should terminate engine
}
```

#### Design Characteristics

| Aspect | YAWL Java Approach |
|--------|-------------------|
| **Paradigm** | Object-oriented with inheritance |
| **State Management** | Mutable fields in objects |
| **Concurrency** | Thread-based with locks |
| **Lifecycle** | Constructor-based initialization |
| **Error Handling** | Checked/unchecked exceptions |
| **Singleton** | Static instance pattern |
| **Collections** | HashMap, HashSet, ConcurrentHashMap |

### CRE Erlang: Actor Design with Petri Nets

CRE Erlang uses the Actor Model with OTP behaviors and Petri net semantics for workflow execution.

#### Key Modules and Behaviors

```erlang
%% Main workflow executor (gen_server behavior)
-module(gen_yawl).
-behaviour(gen_server).

%% Wrapper state record (immutable)
-record(wrapper_state, {
          net_mod :: atom(),
          net_state :: term(),
          fire_timeout = 5000 :: pos_integer(),
          progress_timeout = 30000 :: pos_integer(),
          shutting_down = false :: boolean(),
          active_fires = 0 :: non_neg_integer(),
          marking_history = [] :: [non_neg_integer()],
          max_marking_history = 10 :: pos_integer(),
          continue_count = 0 :: non_neg_integer(),
          max_continue = 1000 :: pos_integer(),
          regions = #{} :: #{binary() => [atom()]}
         }).

%% Petri net behavior
-module(gen_pnet).
-behaviour(gen_server).

%% Petri net state (immutable during transitions)
-record(net_state, {
          marking :: #{atom() => [term()]},
          usr_info :: term(),
          stats :: #stats{} | undefined,
          tstart :: integer(),
          cnt :: non_neg_integer()
         }).
```

#### Design Characteristics

| Aspect | CRE Erlang Approach |
|--------|-------------------|
| **Paradigm** | Functional + Actor Model |
| **State Management** | Immutable data structures |
| **Concurrency** | Process-based (no locks) |
| **Lifecycle** | Callback-based (init, handle_call, etc.) |
| **Error Handling** | Let-it-crash + supervision trees |
| **Singleton** | Registered process names |
| **Collections** | Maps, lists (immutable) |

### Comparison Table: OOP vs Actor

| Dimension | YAWL Java (OOP) | CRE Erlang (Actor) |
|-----------|------------------|-------------------|
| **Communication** | Method calls | Message passing |
| **State Access** | Direct field access | Only via API functions |
| **Concurrency Model** | Shared memory | Message passing |
| **Isolation** | Thread synchronization required | Process isolation automatic |
| **Failure Domain** | Exception propagation | Process crash (supervised) |
| **Hot Code Upgrade** | Requires restart | Supported natively |
| **Distribution** | RMI/Custom | Transparent |
| **State Representation** | Petri nets as mutable objects | Petri nets as immutable data |

---

## 2. Thread-Based vs Process-Based Concurrency

### YAWL Java: Thread-Based Concurrency

YAWL Java uses Java threads for concurrent execution, with explicit synchronization.

```java
// Thread pool for workitem execution
public class YWorkItemRepository {
    private Map<YWorkItem, YWorkItemStatus> _items;
    private Set<YWorkItem> _enabledItems;

    // Synchronized access required
    public synchronized void addEnabledWorkItem(YWorkItem item) {
        _enabledItems.add(item);
    }

    // Concurrent collections for thread safety
    private Map<YIdentifier, YSpecification> _runningCaseIDToSpecMap =
        new ConcurrentHashMap<YIdentifier, YSpecification>();
}
```

#### Thread Management

| Feature | YAWL Java |
|---------|-----------|
| **Model** | Thread-per-request or thread pool |
| **Synchronization** | synchronized blocks, locks |
| **Context Switch** | OS-level (heavyweight) |
| **Memory** | Shared heap |
| **Isolation** | None - threads share process |
| **Scalability** | Limited by thread count |

### CRE Erlang: Process-Based Concurrency

CRE uses lightweight Erlang processes (not OS processes) for isolation.

```erlang
%% Supervisor manages worker processes
-module(cre_sup).
-behaviour(supervisor).

%% One-for-one restart strategy
init(_Args) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 0,
        period => 5
    },

    ChildSpec = #{
        id => cre_master,
        start => {cre_master, start_link, [cre_master]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [cre_master]
    },

    {ok, {SupFlags, [ChildSpec]}}.
```

#### Process Management

| Feature | CRE Erlang |
|---------|------------|
| **Model** | Lightweight processes (BEAM) |
| **Synchronization** | Message passing (no locks) |
| **Context Switch** | VM-level (microseconds) |
| **Memory** | Per-process heaps |
| **Isolation** | Complete - crash isolation |
| **Scalability** | Millions of processes possible |

### Comparison Diagram

```
YAWL Java Thread Model:
+------------------+       +------------------+       +------------------+
|   Main Thread    |------>|  Worker Thread 1  |------>|  Shared Heap     |
+------------------+       +------------------+       +------------------+
        |                           ^                          ^
        |                           |                          |
        v                           |                          |
+------------------+               |                          |
|  Worker Thread 2 |---------------+                          |
+------------------+                                          |
        |                                                     |
        +-----------------------------------------------------+
        (Explicit synchronization required)

CRE Erlang Process Model:
+--------------+       +--------------+       +--------------+
| Main Process | <---> | Worker Proc 1| <---> | Worker Proc 2|
+--------------+       +--------------+       +--------------+
        |                       |                       |
        v                       v                       v
   Isolated Heap          Isolated Heap          Isolated Heap
        |                       |                       |
        +-----------------------+-----------------------+
                  (Message passing, no shared state)
```

---

## 3. Mutable vs Immutable State

### YAWL Java: Mutable State

State is modified in place through setter methods and direct field access.

```java
public class YNetRunner {
    private Set<YTask> _enabledTasks = new HashSet<YTask>();
    private ExecutionStatus _executionStatus;

    // Direct mutation
    public void setExecutionStatus(ExecutionStatus status) {
        _executionStatus = status;
    }

    public void addEnabledTask(YTask task) {
        _enabledTasks.add(task);  // Mutates the set
    }
}

public class YNet {
    private Map<String, YExternalNetElement> _netElements = new HashMap<>();

    public void addNetElement(YExternalNetElement netElement) {
        _netElements.put(netElement.getID(), netElement);  // Mutates map
    }
}
```

#### State Mutation Patterns

| Pattern | Description |
|---------|-------------|
| **Setter methods** | Direct field modification |
| **Collection mutators** | add(), remove(), put() |
| **In-place updates** | Objects modified directly |
| **Transactional persistence** | Hibernate saves mutated state |

### CRE Erlang: Immutable State

State transitions return new state records; original state is unchanged.

```erlang
%% Marking operations return updated state
-record(net_state, {
          marking :: #{atom() => [term()]},
          usr_info :: term(),
          stats :: #stats{} | undefined
         }).

%% Consume tokens - returns NEW state
-spec cns(Mode :: #{atom() => [term()]}, NetState :: #net_state{}) -> #net_state{}.
cns(Mode, NetState = #net_state{marking = Marking}) ->
    F = fun(T, TkLst, Acc) ->
                Acc#{T => TkLst -- maps:get(T, Mode, [])}
        end,
    NetState#net_state{marking = maps:fold(F, #{}, Marking)}.

%% Produce tokens - returns NEW state
-spec prd(ProdMap :: #{atom() => [term()]}, NetState :: #net_state{}) -> #net_state{}.
prd(ProdMap, NetState = #net_state{marking = Marking}) ->
    F = fun(T, TkLst, Acc) ->
                Acc#{T => TkLst ++ maps:get(T, ProdMap, [])}
        end,
    NetState#net_state{marking = maps:fold(F, #{}, Marking)}.
```

#### State Immutability Patterns

| Pattern | Description |
|---------|-------------|
| **Record updates** | `#record{field = NewValue}` creates new copy |
| **Map operations** | `maps:put/3` returns new map |
| **List operations** | `[H | T]` creates new list |
| **Persistent data structures** | HAMT-based maps (O(log n)) |

### Comparison: State Transitions

```
YAWL Java State Transition:
+------------------+           +------------------+
| Current Object   |           | Current Object   |
| _enabledTasks = {}|  ----->  | _enabledTasks = {t1}|  (Same object mutated)
+------------------+           +------------------+

CRE Erlang State Transition:
+------------------+           +------------------+
| Current State    |           | New State        |
| marking = #{}    |  ----->  | marking = #{p1 => [t]}|  (New state created)
+------------------+           +------------------+
| (Old state still accessible, GC when unreferenced)
```

---

## 4. Exception Handling vs Let-It-Crash

### YAWL Java: Structured Exception Handling

YAWL Java uses Java's exception hierarchy with checked and unchecked exceptions.

```java
// Exception hierarchy
package org.yawlfoundation.yawl.exceptions;

public class YAWLException extends Exception {
    public YAWLException(String msg) { super(msg); }
}

public class YStateException extends YAWLException {
    public YStateException(String msg) { super(msg); }
}

public class YPersistenceException extends YAWLException {
    // Fatal - should gracefully terminate engine
    public YPersistenceException(String message, Throwable cause) {
        super(message, cause);
    }
}

// Usage in code
public void startWorkflow(YSpecification spec) throws YStateException, YPersistenceException {
    try {
        _pmgr.startTransaction();
        YNetRunner runner = new YNetRunner(_pmgr, net, params, caseID);
    } catch (YDataStateException e) {
        _pmgr.abort();
        throw new YStateException("Failed to start workflow", e);
    } finally {
        if (_pmgr.isActiveTransaction()) {
            _pmgr.commit();
        }
    }
}
```

#### Exception Handling Characteristics

| Aspect | YAWL Java |
|--------|-----------|
| **Propagation** | Up call stack via throws |
| **Recovery** | Try-catch blocks |
| **Fatal errors** | YPersistenceException terminates engine |
| **Logging** | Before re-throwing |
| **State restoration** | Transaction rollback |

### CRE Erlang: Let-It-Crash with Supervision

CRE uses Erlang's "let it crash" philosophy with supervisor trees.

```erlang
%% Exception as data (not control flow)
-module(wf_exception).

-type exception() :: #{
    type := exception_type(),
    reason := reason(),
    data := exception_data(),
    source := source(),
    timestamp := integer()
}.

-spec new(Type :: exception_type(),
          Reason :: reason(),
          Data :: exception_data()) -> exception().
new(Type, Reason, Data) ->
    #{
        type => Type,
        reason => Reason,
        data => Data,
        source => undefined,
        timestamp => erlang:monotonic_time(millisecond)
    }.

%% Supervision tree
-module(cre_sup).
-behaviour(supervisor).

init(_Args) ->
    SupFlags = #{
        strategy => one_for_one,     % Only crashed child restarts
        intensity => 0,               % No auto-restart
        period => 5
    },
    % Children restart independently
    {ok, {SupFlags, [ChildSpec1, ChildSpec2]}}.

%% Process crash handling
%% When a worker crashes:
%% 1. Supervisor receives EXIT signal
%% 2. Logs the crash reason
%% 3. Restarts the child (if permanent)
%% 4. Other children unaffected
```

#### Let-It-Crash Characteristics

| Aspect | CRE Erlang |
|--------|------------|
| **Propagation** | EXIT signal to supervisor |
| **Recovery** | Supervisor restarts process |
| **Fatal errors** | Process crash, supervised restart |
| **Logging** | Automatic error reports |
| **State restoration** | Clean state on restart |

### Comparison: Error Handling Flow

```
YAWL Java Exception Handling:
+----------+     throws     +----------+     catch      +----------+
|  Method  | -------------> |  Method  | -------------> |  Handler |
+----------+                +----------+                +----------+
       |                          |                          |
       v                          v                          v
  Throw exception           Stack unwind             Execute catch
                                                      (or finally)
                                                           |
                                                           v
                                                 Recovery or rethrow

CRE Erlang Let-It-Crash:
+----------+     message    +----------+     crash     +-----------+
| Process  | -------------> | Process  | ---------> | Supervisor|
+----------+                +----------+  EXIT signal +-----------+
       |                          |                       |
   Continue                    Crash                  Restart
   (other processes)         (isolated)               (new state)
```

---

## 5. Persistence Mechanisms

### YAWL Java: Hibernate-Based Persistence

YAWL Java uses Hibernate ORM for database persistence with transaction management.

```java
public class YPersistenceManager {
    private SessionFactory factory;

    public static final Class[] persistedClasses = {
        YSpecification.class,
        YNetRunner.class,
        YWorkItem.class,
        YIdentifier.class,
        YNetData.class,
        YAWLServiceReference.class,
        YExternalClient.class,
        YWorkItemTimer.class,
        YLaunchDelayer.class,
        YCaseNbrStore.class,
        Problem.class,
        GroupedMIOutputData.class
    };

    public void startTransaction() throws YPersistenceException {
        getSession().beginTransaction();
    }

    public void commit() throws YPersistenceException {
        Session session = getSession();
        Transaction tx = session.getTransaction();
        if (tx != null && tx.isActive()) {
            tx.commit();
        }
    }

    public void execInsert(Object obj) throws YPersistenceException {
        Session session = getSession();
        session.saveOrUpdate(obj);
    }
}
```

#### Persistence Characteristics

| Aspect | YAWL Java |
|--------|-----------|
| **Framework** | Hibernate ORM |
| **Databases** | H2 (default), MySQL, PostgreSQL, etc. |
| **Transactions** | ACID via Hibernate |
| **Schema** | Auto-generated from @Entity classes |
| **Caching** | First-level (session) and second-level cache |
| **Restoration** | Deserialization from database |

### CRE Erlang: Mnesia-Based Persistence

CRE uses Mnesia (Erlang's distributed database) for in-memory and disk-based persistence.

```erlang
-module(yawl_persistence).

%% Mnesia table definitions
-record(case_table, {
          case_id :: binary(),
          workflow_id :: binary(),
          spec :: term(),
          status :: running | completed | cancelled,
          data :: map(),
          created_at :: integer(),
          completed_at :: integer() | undefined
         }).

-record(workitem_table, {
          workitem_id :: binary(),
          case_id :: binary(),
          task_id :: binary(),
          status :: enabled | started | completed | failed,
          data :: map()
         }).

%% Initialize schema
-spec init_schema() -> ok | {error, term()}.
init_schema() ->
    case mnesia:system_info(is_running) of
        true -> ok;
        false ->
            mnesia:create_schema([node()]),
            mnesia:start(),
            create_tables()
    end.

create_tables() ->
    mnesia:create_table(case_table, [
        {attributes, record_info(fields, case_table)},
        {disc_copies, [node()]},
        {type, set}
    ]),
    mnesia:create_table(workitem_table, [
        {attributes, record_info(fields, workitem_table)},
        {disc_copies, [node()]},
        {type, bag}
    ]).

%% Save case state
-spec save_case(map()) -> {ok, binary()} | {error, term()}.
save_case(CaseMap) ->
    case_id = maps:get(case_id, CaseMap),
    Record = #case_table{
        case_id = case_id,
        workflow_id = maps:get(workflow_id, CaseMap),
        spec = maps:get(spec, CaseMap),
        status = maps:get(status, CaseMap),
        data = maps:get(data, CaseMap, #{}),
        created_at = maps:get(created_at, CaseMap),
        completed_at = maps:get(completed_at, CaseMap, undefined)
    },
    mnesia:transaction(fun() -> mnesia:write(Record) end),
    {ok, case_id}.
```

#### Persistence Characteristics

| Aspect | CRE Erlang |
|--------|------------|
| **Framework** | Mnesia (built-in) |
| **Storage** | In-memory with disk replication |
| **Transactions** | ACID via mnesia:transaction/1 |
| **Schema** | Erlang record definitions |
| **Distribution** | Built-in table fragmentation |
| **Restoration** | Direct term loading (no serialization) |

### Comparison: Persistence Architecture

```
YAWL Java Persistence (Hibernate):
+----------+    Hibernate     +------------+    JDBC     +----------+
| YEngine  | ---------------> | ORM Layer  | ---------> | Database |
+----------+                  +------------+            +----------+
       |                            |                        ^
       v                            v                        |
+----------+                  +------------+                |
 Persisted                   Serialized                 Stored
 Objects                     Classes                    Rows

CRE Erlang Persistence (Mnesia):
+----------+    Mnesia        +------------+    Dets     +----------+
| gen_yawl | ---------------> | In-Memory  | ---------> | Disk     |
+----------+    Transaction    +  Tables    |   Replication+----------+
       |                            |                        ^
       v                            v                        |
+----------+                  +------------+                |
 Erlang                      Native                    Stored
 Terms                       Terms                    Terms
   (No serialization overhead)
```

---

## 6. Architecture Diagrams

### YAWL Java Architecture

```
+-----------------------------------------------------------------------+
|                             YAWL Java Engine                          |
+-----------------------------------------------------------------------+
|                                                                       |
|  +-------------------+       +-------------------+                     |
|  |  YEngine (Singleton)      |                  |                     |
|  |  - _workItemRepository     |                  |                     |
|  |  - _netRunnerRepository    |                  |                     |
|  |  - _specifications         |                  |                     |
|  +-------------------+       +-------------------+                     |
|           |                          |                                 |
|           v                          v                                 |
|  +-------------------+       +-------------------+                     |
|  |  YNetRunner        |      | YPersistence      |                     |
|  |  - _net (mutable)  |<---->| Manager           |                     |
|  |  - _enabledTasks   |      | (Hibernate)       |                     |
|  |  - _busyTasks      |      |                   |                     |
|  +-------------------+       +-------------------+                     |
|           |                                                                 |
|           v                                                                 |
|  +-------------------+       +-------------------+                     |
|  |  YNet (Petri)      |      | YTask             |                     |
|  |  - _netElements    |<---->| - _mi_data        |                     |
|  |  - _localVariables |      | - _joinType        |                     |
|  +-------------------+       +-------------------+                     |
|                                                                       |
+-----------------------------------------------------------------------+
           |                    ^                    ^
           v                    |                    |
+-------------------+    +----------------+   +----------------+
| Workitem Threads  |    | HTTP Requests  |   | Timer Threads  |
+-------------------+    +----------------+   +----------------+
```

### CRE Erlang Architecture

```
+-----------------------------------------------------------------------+
|                        CRE Erlang Application                          |
+-----------------------------------------------------------------------+
|                                                                       |
|  +-------------------+       +-------------------+                     |
|  |  cre_sup (Supervisor)    |                  |                     |
|  |  strategy: one_for_one    |                  |                     |
|  |  intensity: 0             |                  |                     |
|  +-------------------+       +-------------------+                     |
|           |                          |                                 |
|           +--------------------------+-----------------+                |
|           |                          |                 |                |
|           v                          v                 v                |
|  +----------------+    +----------------+    +----------------+        |
|  |  cre_master    |    | yawl_timeout   |    | yawl_approval  |        |
|  |  (gen_server)  |    | (gen_server)   |    | (gen_server)   |        |
|  +----------------+    +----------------+    +----------------+        |
|           |                  |                      |                |
|           v                  v                      v                |
|  +----------------+    +----------------+    +----------------+        |
|  | gen_yawl Pool  |    | Timer Queue    |    | Approval State |        |
|  | (gen_pnet)      |    | (Priority Q)   |    | (Maps)         |        |
|  +----------------+    +----------------+    +----------------+        |
|                                                                       |
+-----------------------------------------------------------------------+
           |                    ^                    ^
           v                    |                    |
+-------------------+    +----------------+   +----------------+
| gen_yawl Processes |    | API Calls      |   | Yawl Executor  |
| (Actor Model)      |    | (HTTP/gRPC)    |   | (YAML Spec)    |
+-------------------+    +----------------+   +----------------+
```

---

## 7. Key Differences Summary

### Paradigm Comparison

| Dimension | YAWL Java | CRE Erlang |
|-----------|-----------|------------|
| **Programming Paradigm** | Object-Oriented | Functional + Actor |
| **Concurrency Model** | Threads (OS) | Processes (VM) |
| **State Management** | Mutable | Immutable |
| **Error Handling** | Exceptions | Let-it-crash |
| **Persistence** | Hibernate ORM | Mnesia |
| **Distribution** | Manual (RMI) | Transparent |
| **Hot Upgrade** | Not supported | Native |
| **Scalability** | 1000s of threads | Millions of processes |

### Code Pattern Comparison

| Pattern | YAWL Java | CRE Erlang |
|---------|-----------|------------|
| **Workflow Definition** | YSpecification object | YAML/Maps |
| **Task Execution** | Thread pool | Worker processes |
| **State Query** | Getter methods | gen_server:call |
| **State Update** | Setter methods | New state returned |
| **Event Notification** | Observer pattern | Message broadcast |
| **Resource Management** | try-finally blocks | Process links/monitors |

---

## 8. When to Use Each Implementation

### Choose YAWL Java When:

- **Existing Java ecosystem** - Integrating with Java services
- **Traditional RDBMS** - Required to use specific database
- **Debugging tools** - Need mature Java debugging/profiling
- **Developer familiarity** - Team knows OOP but not functional/actor
- **Limited scale** - 100s of concurrent workflows sufficient

### Choose CRE Erlang When:

- **High availability** - 99.999%+ uptime requirement
- **Massive concurrency** - 10,000+ concurrent workflows
- **Fault tolerance** - Automatic recovery from failures
- **Hot upgrades** - Zero-downtime deployments
- **Distribution** - Multi-node deployment required
- **Long-running processes** - Days/weeks workflow execution

---

## References

- YAWL Java Source: `/Users/sac/cre/vendors/yawl/src/`
- CRE Erlang Source: `/Users/sac/cre/src/`
- gen_pnet Documentation: `/Users/sac/cre/src/core/gen_pnet.erl`
- gen_yawl Documentation: `/Users/sac/cre/src/core/gen_yawl.erl`
- YPersistenceManager: `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/engine/YPersistenceManager.java`

---

*Document Version: 1.0*
*Generated: 2025-02-07*
