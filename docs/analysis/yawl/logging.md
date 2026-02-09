# YAWL Java Logging Implementation Analysis

**Interface E - Process Event Logging**

Analysis date: 2026-02-07
Source: `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/`

---

## Executive Summary

YAWL's Interface E provides a comprehensive process event logging system that captures all workflow execution events in a relational database schema (via Hibernate). The logging architecture supports both real-time event capture and retrospective analysis, with multiple output formats including native XML and the industry-standard XES (eXtensible Event Stream) format for process mining.

### Key Components

| Component | Location | Purpose |
|-----------|----------|---------|
| `YEventLogger` | `/logging/YEventLogger.java` | Core event capture and persistence |
| `YLogServer` | `/logging/YLogServer.java` | Query API for log retrieval |
| `YLogGateway` | `/engine/interfce/interfaceE/YLogGateway.java` | HTTP servlet gateway |
| `YLogGatewayClient` | `/engine/interfce/interfaceE/YLogGatewayClient.java` | Client-side API |
| `YXESBuilder` | `/logging/YXESBuilder.java` | XES format export |
| `YLogPredicate*` | `/logging/` | Predicate evaluation system |

---

## 1. Event Logging Architecture

### 1.1 High-Level Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                        YAWL Event Logging Architecture                      │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────┐         ┌─────────────┐                                   │
│  │   Engine    │         │  External   │                                   │
│  │             │         │  Services   │                                   │
│  └──────┬──────┘         └──────┬──────┘                                   │
│         │                       │                                           │
│         │ logCaseCreated()      │                                           │
│         │ logWorkItemEvent()    │                                           │
│         │ logDataEvent()        │                                           │
│         ▼                       │                                           │
│  ┌─────────────────────────────────────────────────────┐                   │
│  │              YEventLogger (Singleton)                │                   │
│  │  • Async logging via ExecutorService thread pool    │                   │
│  │  • Hibernate-based persistence                      │                   │
│  │  • YEventKeyCache for foreign key resolution        │                   │
│  └────────────────────────┬────────────────────────────┘                   │
│                           │                                                 │
│                           ▼                                                 │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                     Database (Hibernate)                            │   │
│  │  ┌─────────────────┐ ┌─────────────────┐ ┌─────────────────┐       │   │
│  │  │ Specifications  │ │   Instances     │ │     Events      │       │   │
│  │  │    & Tasks      │ │   (Net/Task)    │ │  & Data Items   │       │   │
│  │  └─────────────────┘ └─────────────────┘ └─────────────────┘       │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                           │                                                 │
│                           ▼                                                 │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                       YLogServer (Query API)                         │   │
│  │  • getCaseEvents()    • getSpecificationStatistics()                │   │
│  │  • getCompleteCaseLog() • getXESLog()                               │   │
│  └────────────────────────┬────────────────────────────────────────────┘   │
│                           │                                                 │
│                           ▼                                                 │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                     YLogGateway (HTTP)                               │   │
│  │  Servlet endpoint: /yawl/logGateway                                 │   │
│  │  Actions: connect, getAllSpecifications, getCaseEvents, ...         │   │
│  └─────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 1.2 Module Relationships

```
┌──────────────────────────────────────────────────────────────────────┐
│                         Module Dependencies                          │
├──────────────────────────────────────────────────────────────────────┤
│                                                                      │
│   YEngine                                                             │
│      │                                                                │
│      ├──> YEventLogger ──> YEventKeyCache                            │
│      │                    │                                          │
│      │                    ├──> YDataSchemaCache                      │
│      │                    │                                          │
│      │                    └──> [Map-based FK caches]                 │
│      │                                                                │
│      └──> YLogServer ──> HibernateEngine ──> [Log Tables]            │
│                           │                                          │
│                           └──> YXESBuilder ──> SpecHistory           │
│                                                                      │
│   Interface_E                                                        │
│      │                                                                │
│      ├──> YLogGateway (Servlet) ──> YLogServer                       │
│      │                                                                │
│      └──> YLogGatewayClient (API)                                    │
│                                                                      │
│   Predicate System                                                   │
│      │                                                                │
│      ├──> YLogPredicate (container)                                  │
│      │                                                                │
│      ├──> YLogPredicateWorkItemParser                                │
│      ├──> YLogPredicateDecompositionParser                            │
│      └──> YLogPredicateParameterParser                               │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘
```

---

## 2. Database Schema

### 2.1 Entity-Relationship Diagram

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                          YAWL Log Database Schema                           │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌──────────────────┐        ┌──────────────────┐                          │
│  │ YLogSpecification│───────1│    YLogNet       │                          │
│  │ ────────────────│        │ ────────────────│                          │
│  │ • rowKey (PK)    │        │ • netID (PK)     │                          │
│  │ • identifier     │        │ • name           │                          │
│  │ • version        │        │ • specKey (FK)   │                          │
│  │ • uri            │        │                  │                          │
│  │ • rootNetID (FK) │        │                  │                          │
│  └──────────────────┘        └────┬─────────────┘                          │
│                                     │                                        │
│                                     │ 1                                     │
│                                     │                                        │
│                                     │ N                                      │
│                                     │                                        │
│                      ┌──────────────┴──────────────┐                        │
│                      │                             │                        │
│                      ▼                             ▼                        │
│         ┌──────────────────────┐     ┌──────────────────────┐              │
│         │  YLogNetInstance     │     │     YLogTask         │              │
│         │  ───────────────────  │     │  ───────────────────  │              │
│         │  • netInstanceID(PK) │     │  • taskID (PK)       │              │
│         │  • engineInstanceID  │     │  • name              │              │
│         │  • netID (FK)        │     │  • parentNetID (FK)  │              │
│         │  • parentTaskInstID  │     │  • childNetID (FK)   │              │
│         └──────────┬───────────┘     └──────────┬───────────┘              │
│                    │                            │                          │
│                    │ 1                          │ 1                        │
│                    │                            │                          │
│                    │ N                          │ N                        │
│                    │                            │                          │
│         ┌──────────┴───────────┐     ┌──────────┴───────────┐              │
│         │   YLogEvent          │     │  YLogTaskInstance    │              │
│         │  ───────────────────  │     │  ───────────────────  │              │
│         │  • eventID (PK)       │     │  • taskInstanceID(PK) │              │
│         │  • instanceID (FK)    │◄────│  • engineInstanceID   │              │
│         │  • descriptor         │     │  • taskID (FK)        │              │
│         │  • timestamp          │     │  • parentNetInstID(FK)│              │
│         │  • serviceID (FK)     │     │  • parentTaskInstID   │              │
│         │  • rootNetInstID (FK) │     └───────────────────────┘              │
│         └──────────┬───────────┘                                          │
│                    │                                                       │
│                    │ 1                                                     │
│                    │                                                       │
│                    │ N                                                     │
│                    │                                                       │
│                    ▼                                                       │
│         ┌──────────────────────────┐      ┌──────────────────┐             │
│         │ YLogDataItemInstance     │      │   YLogService    │             │
│         │ ──────────────────────── │      │  ───────────────  │             │
│         │ • dataItemID (PK)        │      │  • serviceID (PK) │             │
│         │ • eventID (FK)           │      │  • name           │             │
│         │ • dataItem (embedded)    │      │  • url            │             │
│         │ • dataTypeID (FK)        │      └──────────────────┘             │
│         └──────────┬───────────────┘                                      │
│                    │                                                       │
│                    │                                                       │
│                    ▼                                                       │
│         ┌───────────────────────┐                                        │
│         │    YLogDataType       │                                        │
│         │  ──────────────────── │                                        │
│         │  • dataTypeID (PK)    │                                        │
│         │  • definition         │                                        │
│         └───────────────────────┘                                        │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 2.2 Table Definitions

#### YLogSpecification
Represents a unique workflow specification (process definition).

| Field | Type | Description |
|-------|------|-------------|
| rowKey | long (PK) | Auto-generated primary key |
| identifier | String | UUID of the specification |
| version | String | Version string |
| uri | String | Specification URI |
| rootNetID | long (FK) | Reference to root net |

#### YLogNet
Represents a net (root or sub-net) within a specification.

| Field | Type | Description |
|-------|------|-------------|
| netID | long (PK) | Auto-generated primary key |
| name | String | Net identifier |
| specKey | long (FK) | Reference to specification |

#### YLogNetInstance
Represents a runtime instance of a net (case or sub-process).

| Field | Type | Description |
|-------|------|-------------|
| netInstanceID | long (PK) | Auto-generated primary key |
| engineInstanceID | String | Case ID (engine-assigned) |
| netID | long (FK) | Reference to net definition |
| parentTaskInstanceID | long (FK) | Parent task for sub-nets (-1 for root) |

#### YLogTask
Represents a task definition within a net.

| Field | Type | Description |
|-------|------|-------------|
| taskID | long (PK) | Auto-generated primary key |
| name | String | Task name |
| parentNetID | long (FK) | Parent net |
| childNetID | long (FK) | Child net for composite tasks (-1 for atomic) |

#### YLogTaskInstance
Represents a runtime instance of a task (work item).

| Field | Type | Description |
|-------|------|-------------|
| taskInstanceID | long (PK) | Auto-generated primary key |
| engineInstanceID | String | Work item ID |
| taskID | long (FK) | Reference to task definition |
| parentNetInstanceID | long (FK) | Containing net instance |
| parentTaskInstanceID | long (FK) | Parent for child workitems |

#### YLogEvent
Represents a single runtime event (state change).

| Field | Type | Description |
|-------|------|-------------|
| eventID | long (PK) | Auto-generated primary key |
| instanceID | long (FK) | Reference to task or net instance |
| descriptor | String | Event type (see Event Types) |
| timestamp | long | Milliseconds since epoch |
| serviceID | long (FK) | External service that triggered event |
| rootNetInstanceID | long (FK) | Convenience field for queries |

#### YLogDataItemInstance
Represents a data value associated with an event.

| Field | Type | Description |
|-------|------|-------------|
| dataItemID | long (PK) | Auto-generated primary key |
| eventID | long (FK) | Reference to event |
| dataItem | embedded | Name, value, descriptor, dataType |
| dataTypeID | long (FK) | Reference to data type definition |

#### YLogDataType
Represents a data type definition (XSD schema).

| Field | Type | Description |
|-------|------|-------------|
| dataTypeID | long (PK) | Auto-generated primary key |
| name | String | Type name |
| definition | String | XSD type definition |

#### YLogService
Represents an external service (client).

| Field | Type | Description |
|-------|------|-------------|
| serviceID | long (PK) | Auto-generated primary key |
| name | String | Service/user name |
| url | String | Service endpoint URL |

---

## 3. Event Types

### 3.1 Case-Level Events

| Event Constant | Descriptor | Trigger |
|----------------|------------|---------|
| `CASE_START` | "CaseStart" | New case launched |
| `CASE_COMPLETE` | "CaseComplete" | Root net completed normally |
| `CASE_CANCEL` | "CaseCancel" | Case cancelled |

### 3.2 Net-Level Events

| Event Constant | Descriptor | Trigger |
|----------------|------------|---------|
| `NET_UNFOLD` | "NetUnfold" | Composite task enabled (sub-net created) |
| `NET_START` | "NetStart" | Sub-net execution begins |
| `NET_COMPLETE` | "NetComplete" | Sub-net completed normally |
| `NET_CANCEL` | "NetCancel" | Sub-net cancelled |

### 3.3 Task-Level Events (YWorkItemStatus)

| Status | Descriptor | Trigger |
|--------|------------|---------|
| `statusEnabled` | "Enabled" | Task enabled |
| `statusExecuting` | "Executing" | Task started execution |
| `statusComplete` | "Complete" | Task completed |
| `statusDeleted` | "Deleted" | Task deleted |
| `statusCancelledByCase` | "CancelledByCase" | Cancelled due to case cancellation |
| `statusFailed` | "Failed" | Task execution failed |
| `statusForcedComplete` | "ForcedComplete" | Admin forced completion |
| `statusSuspended` | "Suspended" | Task suspended |
| `statusIsParent` | "Decompose" | Composite task decomposed |

### 3.4 Data Events

| Event Descriptor | Description |
|------------------|-------------|
| "DataValueChange" | Input or output data mapping |
| "Predicate" | Log predicate evaluation result |

### 3.5 Event to XES Mapping

| YAWL Event | XES Transition |
|------------|----------------|
| Enabled | schedule |
| Executing | start |
| Complete | complete |
| CaseCancel | pi_abort |
| NetStart | schedule |
| NetComplete | complete |
| NetCancel | ate_abort |
| Deleted | ate_abort |
| CancelledByCase | pi_abort |
| Failed | ate_abort |
| ForcedComplete | complete |
| Suspended | suspend |

---

## 4. Log Predicate System

### 4.1 Predicate Overview

Log predicates allow runtime extraction of workflow state and metadata for logging. They are defined at the task level and evaluated when events occur.

**Key Classes:**
- `YLogPredicate` - Container for start and completion predicates
- `YLogPredicateWorkItemParser` - Evaluates predicates against work items
- `YLogPredicateDecompositionParser` - Evaluates predicates against decompositions
- `YLogPredicateParameterParser` - Evaluates predicates against parameters

### 4.2 Predicate Syntax

Predicates use `${expression}` syntax that is parsed and resolved at runtime.

#### Work Item Predicates

| Predicate | Returns |
|-----------|---------|
| `${item:id}` | Work item identifier string |
| `${task:id}` | Task ID from specification |
| `${spec:name}` | Specification name |
| `${task:name}` | Task name |
| `${spec:version}` | Specification version |
| `${spec:key}` | Specification identifier |
| `${item:handlingservice:name}` | User/service name |
| `${item:handlingservice:uri}` | Service URI |
| `${item:handlingservice:doco}` | Service documentation |
| `${item:codelet}` | Codelet assignment |
| `${item:customform}` | Custom form URL |
| `${item:enabledtime}` | Timestamp when enabled |
| `${item:firedtime}` | Timestamp when fired |
| `${item:startedtime}` | Timestamp when started |
| `${item:status}` | Current status |
| `${task:doco}` | Task documentation |
| `${task:decomposition:name}` | Decomposition ID |
| `${item:timer:status}` | Timer status |
| `${item:timer:expiry}` | Timer expiry timestamp |
| `${item:attribute:xxx}` | Custom attribute value |
| `${expression:xxx}` | XPath/XQuery evaluation result |

### 4.3 Predicate Processing Flow

```
┌──────────────────────────────────────────────────────────────────────┐
│                    Log Predicate Processing                          │
├──────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  Task Definition                                                     │
│  ┌────────────────────────────────────────────────────┐             │
│  │ <logPredicate>                                     │             │
│  │   <start>${spec:name} - ${task:name} started       │             │
│  │   </start>                                         │             │
│  │   <completion>Status: ${item:status}               │             │
│  │   </completion>                                    │             │
│  │ </logPredicate>                                    │             │
│  └────────────────────────────────────────────────────┘             │
│                          │                                           │
│                          ▼                                           │
│  ┌────────────────────────────────────────────────────┐             │
│  │ YLogPredicate.getParsedStartPredicate(workItem)    │             │
│  │   └─> YLogPredicateWorkItemParser.parse()          │             │
│  │        └─> valueOf("${spec:name}")                 │             │
│  │             └─> workItem.getSpecName()             │             │
│  └────────────────────────────────────────────────────┘             │
│                          │                                           │
│                          ▼                                           │
│  ┌────────────────────────────────────────────────────┐             │
│  │ Result: "OrderProcessing - ValidateOrder started"  │             │
│  └────────────────────────────────────────────────────┘             │
│                                                                      │
└──────────────────────────────────────────────────────────────────────┘
```

---

## 5. YAWL Event Logger (YEventLogger)

### 5.1 Class Overview

```java
public class YEventLogger {
    // Singleton instance
    private static YEventLogger INSTANCE;

    // Async executor
    private static final ExecutorService _executor =
        Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

    // Foreign key cache
    private final YEventKeyCache _keyCache = new YEventKeyCache();

    // Hibernate persistence
    private HibernateEngine _db;

    // Enable/disable flag
    private boolean _enabled = true;
}
```

### 5.2 Key Methods

| Method | Purpose |
|--------|---------|
| `logCaseCreated()` | Record case launch |
| `logCaseCancelled()` | Record case cancellation |
| `logSubNetCreated()` | Record sub-net launch |
| `logNetCompleted()` | Record net completion |
| `logNetCancelled()` | Record net cancellation |
| `logWorkItemEvent()` | Record task status change |
| `logDataEvent()` | Record data value changes |

### 5.3 Async Logging Pattern

All logging operations use asynchronous execution to avoid blocking the workflow engine:

```java
public void logCaseCreated(final YSpecificationID ySpecID,
                           final YIdentifier caseID,
                           final YLogDataItemList datalist,
                           final String serviceRef) {
    if (loggingEnabled()) {
        _executor.execute(new Runnable() {
            @Override
            public void run() {
                long netInstanceID = insertNetInstance(caseID, getRootNetID(ySpecID), -1);
                long serviceID = getServiceID(serviceRef);
                logEvent(netInstanceID, CASE_START, datalist, serviceID, netInstanceID);
                _keyCache.netInstances.put(caseID, netInstanceID);
            }
        });
    }
}
```

### 5.4 Foreign Key Caching (YEventKeyCache)

The `YEventKeyCache` maintains in-memory maps to avoid repeated database lookups:

| Cache Map | Key | Value | Scope |
|-----------|-----|-------|-------|
| `services` | URL or name | serviceID | Permanent |
| `dataDefn` | (name, schema) | dataTypeID | Permanent |
| `specEntries` | YSpecificationID | YLogSpecification | Until spec unloaded |
| `rootNets` | YSpecificationID | rootNetID | Until spec unloaded |
| `nets` | (specID, netName) | netID | Until spec unloaded |
| `tasks` | (netID, taskName) | taskID | Until spec unloaded |
| `netInstances` | YIdentifier | netInstanceID | Until case completes |
| `taskInstances` | (caseID, taskID) | taskInstanceID | Until case completes |

---

## 6. Interface E Gateway (YLogGateway)

### 6.1 HTTP API Endpoints

The `YLogGateway` servlet provides REST-like access to log data via HTTP GET/POST.

#### Authentication Actions

| Action | Parameters | Description |
|--------|------------|-------------|
| `connect` | userid, password, encrypt? | Create session |
| `checkConnection` | sessionHandle | Validate session |

#### Specification Queries

| Action | Parameters | Returns |
|--------|------------|---------|
| `getAllSpecifications` | sessionHandle | All logged specifications |
| `getSpecificationStatistics` | identifier/version/uri or key, [from, to] | Statistics |
| `getSpecificationCaseIDs` | identifier/version/uri or key | List of case IDs |
| `getNetInstancesOfSpecification` | identifier/version/uri or key | All net instances |

#### Case Queries

| Action | Parameters | Returns |
|--------|------------|---------|
| `getCompleteCaseLog` | caseid | Full case history |
| `getCaseEvents` | caseid or key | All case events |
| `getCaseEvent` | caseid, event | Specific event |
| `getTaskInstancesForCase` | caseid | All task instances |
| `getTaskInstancesForTask` | caseid, taskname or key | Task instances |

#### Event & Data Queries

| Action | Parameters | Returns |
|--------|------------|---------|
| `getEventsForTaskInstance` | itemid | Task instance events |
| `getInstanceEvents` | key | Instance events |
| `getDataForEvent` | key | Data items for event |
| `getDataTypeForDataItem` | key | Data type definition |

#### Service Queries

| Action | Parameters | Returns |
|--------|------------|---------|
| `getAllCasesStartedByService` | name | Cases started by service |
| `getAllCasesCancelledByService` | name | Cases cancelled by service |
| `getServiceName` | key | Service name |

#### Export

| Action | Parameters | Returns |
|--------|------------|---------|
| `getSpecificationXESLog` | identifier/version/uri, withData?, ignoreUnknowns? | XES format log |

### 6.2 Request/Response Format

**Request Example:**
```http
POST /yawl/logGateway HTTP/1.1
Content-Type: application/x-www-form-urlencoded

action=getCaseEvents&caseid=123&sessionHandle=abc...
```

**Response Format:**
```xml
<events rootNetInstanceKey="456">
    <event key="789">
        <instanceKey>101</instanceKey>
        <descriptor>CaseStart</descriptor>
        <timestamp>1677649200000</timestamp>
        <serviceKey>1</serviceKey>
        <rootNetInstanceKey>456</rootNetInstanceKey>
    </event>
    ...
</events>
```

---

## 7. XES Export (YXESBuilder)

### 7.1 XES Format

YAWL exports logs in the OpenXES format for compatibility with process mining tools (ProM, Disco, etc.).

**XES Structure:**
```
<log xes.version="1.0" ...>
    <extension .../>
    <global scope="trace">...</global>
    <global scope="event">...</global>
    <classifier .../>
    <string key="concept:name" value="specID"/>
    <trace>
        <string key="concept:name" value="caseID"/>
        <event>
            <date key="time:timestamp" value="..."/>
            <string key="concept:name" value="taskName"/>
            <string key="lifecycle:transition" value="start"/>
            <string key="concept:instance" value="instanceID"/>
        </event>
    </trace>
</log>
```

### 7.2 XES Translation Mapping

| YAWL Event | XES Transition | Notes |
|------------|----------------|-------|
| Enabled | schedule | Task enabled |
| Executing | start | Task started |
| Complete | complete | Task completed |
| Suspended | suspend | Task suspended |
| CaseCancel | pi_abort | Process instance abort |
| NetCancel | ate_abort | Activity (sub-net) abort |
| Failed | ate_abort | Activity abort |
| Deleted | ate_abort | Activity abort |
| ForcedComplete | complete | Treated as complete |

### 7.3 Data Events in XES

Data changes are attached to `Executing` (input) and `Complete` (output) events:

```xml
<event>
    <date key="time:timestamp" value="2023-01-01T10:00:00"/>
    <string key="lifecycle:transition" value="start"/>
    <string key="concept:name" value="Approve"/>
    <!-- Input data -->
    <string key="input/amount" value="1000"/>
    <string key="input/currency" value="USD"/>
</event>
<event>
    <date key="time:timestamp" value="2023-01-01T10:05:00"/>
    <string key="lifecycle:transition" value="complete"/>
    <string key="concept:name" value="Approve"/>
    <!-- Output data -->
    <string key="output/approved" value="true"/>
    <string key="output/approver" value="alice"/>
</event>
```

---

## 8. Log Data Items

### 8.1 YLogDataItem Structure

```java
public class YLogDataItem {
    private String name;              // Variable name
    private String value;             // Serialized value
    private String dataTypeName;      // Type name (e.g., "string", "int")
    private String dataTypeDefinition; // Full XSD type definition
    private String descriptor;        // Context label (Input/Output/Predicate)
}
```

### 8.2 Descriptor Values

| Descriptor | Description | When Logged |
|------------|-------------|-------------|
| "Input" | Input parameter mapping | Task start |
| "Output" | Output parameter mapping | Task completion |
| "Predicate" | Log predicate result | Task start/completion |
| "unknown" | Unclassified data | Default |

### 8.3 Data Type Handling

Built-in types:
- `string` - Text values
- `boolean` - true/false
- `int`, `long`, `integer` - Integral types
- `float`, `double`, `decimal` - Floating point
- `date`, `datetime`, `time` - Temporal types

Complex types are represented with full XSD schema definitions.

---

## 9. Statistics and Analysis

### 9.1 Specification Statistics

`YLogServer.getSpecificationStatistics()` returns:

```xml
<specification id="http://sample.com - 1.0" key="1">
    <started>150</started>
    <completed>142</completed>
    <cancelled>8</cancelled>
    <completionMaxtime>5m 23s</completionMaxtime>
    <completionMintime>1m 12s</completionMintime>
    <completionAvgtime>2m 45s</completionAvgtime>
    <cancelledMaxtime>3m 10s</cancelledMaxtime>
    <cancelledMintime>0m 30s</cancelledMintime>
    <cancelledAvgtime>1m 20s</cancelledAvgtime>
</specification>
```

### 9.2 Query Patterns

The system supports several query patterns:

1. **By Specification**: Get all cases for a process version
2. **By Case ID**: Get full history of a single case
3. **By Time Range**: Filter events by timestamp window
4. **By Task**: Get all instances of a specific task
5. **By Service**: Find all cases triggered by a service

---

## 10. Integration Points

### 10.1 Engine Integration

`YEventLogger` is called from:
- `YEngine` - Case lifecycle
- `YNetRunner` - Net lifecycle
- `YWorkItem` - Task lifecycle
- `YTask` - Data mapping

### 10.2 External Client Integration

`YLogGatewayClient` provides a Java client API:

```java
YLogGatewayClient client = new YLogGatewayClient("http://localhost:8080/yawl/logGateway");
String handle = client.connect("admin", "YAWL");
String cases = client.getSpecificationCaseIDs(specID, handle);
String xes = client.getSpecificationXESLog(specID, true, handle);
```

---

## 11. Design Patterns

### 11.1 Singleton Pattern
- `YEventLogger` - Single logger instance per engine
- `YLogServer` - Single query server

### 11.2 Data Access Object (DAO)
- `YLogServer` encapsulates all database queries
- Uses Hibernate for ORM

### 11.3 Gateway Pattern
- `YLogGateway` servlet as HTTP interface
- `YLogGatewayClient` as remote proxy

### 11.4 Builder Pattern
- `YXESBuilder` constructs XES documents
- `SpecHistory` builds case histories

### 11.5 Strategy Pattern
- Predicate parser variants (WorkItem, Decomposition, Parameter)
- XES translation strategy

---

## 12. Performance Considerations

### 12.1 Async Logging
- Uses `ExecutorService` with thread pool sized to CPU count
- Prevents logging from blocking workflow execution

### 12.2 Caching
- `YEventKeyCache` maintains foreign key mappings in memory
- Reduces database queries during event logging

### 12.3 Batch Operations
- `startTransaction()` / `commitTransaction()` for multi-query operations
- Reduces transaction overhead

---

## 13. Extensibility

### 13.1 Custom Event Types
New event descriptors can be added by calling:
```java
logEvent(instanceID, "CustomEvent", data, serviceID, rootNetID);
```

### 13.2 Custom Predicates
Extend `YPredicateParser` for new predicate types:
```java
public class YLogCustomPredicateParser extends YPredicateParser {
    protected String valueOf(String predicate) {
        if (predicate.startsWith("${custom:")) {
            return resolveCustom(predicate);
        }
        return super.valueOf(predicate);
    }
}
```

### 13.3 Custom Export Formats
Implement a new builder following `YXESBuilder` pattern:
```java
public class YCustomFormatBuilder {
    public String buildLog(YSpecificationID specID, XNode events) {
        // Build custom format
    }
}
```

---

## 14. Comparison with CRE Logging

| Aspect | YAWL Java | CRE (Erlang) |
|--------|-----------|--------------|
| Storage | Relational (Hibernate) | TBD |
| Async | ExecutorService thread pool | TBD |
| Query | HQL/SQL-based | TBD |
| Export | Native XML + XES | TBD |
| Predicates | Template-based resolution | TBD |
| OpenXES | Full support | TBD |
| Schema | Predefined tables | TBD |

---

## 15. References

### Source Files

| File | Lines | Purpose |
|------|-------|---------|
| `YEventLogger.java` | 908 | Core event capture |
| `YLogServer.java` | 1010 | Query API |
| `YLogGateway.java` | 244 | HTTP gateway |
| `YLogGatewayClient.java` | 613 | Client API |
| `YXESBuilder.java` | 466 | XES export |
| `YLogPredicate.java` | 133 | Predicate container |
| `YLogPredicateWorkItemParser.java` | 128 | WorkItem predicates |
| `SpecHistory.java` | 273 | History builder |
| `YEventKeyCache.java` | 159 | FK caching |
| `YLogDataItem.java` | 146 | Data item model |

### Table Classes

| File | Purpose |
|------|---------|
| `YLogSpecification.java` | Specification record |
| `YLogNet.java` | Net definition |
| `YLogNetInstance.java` | Net instance |
| `YLogTask.java` | Task definition |
| `YLogTaskInstance.java` | Task instance |
| `YLogEvent.java` | Event record |
| `YLogDataItemInstance.java` | Data item |
| `YLogDataType.java` | Type definition |
| `YLogService.java` | Service record |

---

## Appendix A: Sample XML Output

### Case Events Response

```xml
<events rootNetInstanceKey="123">
    <event key="456">
        <instanceKey>789</instanceKey>
        <descriptor>CaseStart</descriptor>
        <timestamp>1677649200000</timestamp>
        <serviceKey>1</serviceKey>
        <rootNetInstanceKey>123</rootNetInstanceKey>
    </event>
    <event key="457">
        <instanceKey>790</instanceKey>
        <descriptor>Enabled</descriptor>
        <timestamp>1677649200100</timestamp>
        <serviceKey>-1</serviceKey>
        <rootNetInstanceKey>123</rootNetInstanceKey>
    </event>
</events>
```

### Complete Case Log Response

```xml
<case id="45">
    <specification key="1">
        <id>
            <identifier>OrderProcess</identifier>
            <version>1.0</version>
            <uri>http://example.com/order</uri>
        </id>
        <rootnetkey>10</rootnetkey>
    </specification>
    <netinstance key="123" root="true">
        <net name="OrderProcess" key="10"/>
        <event key="456">...</event>
        <taskinstance key="789">...</taskinstance>
    </netinstance>
</case>
```

---

## Appendix B: XES Sample

```xml
<?xml version="1.0" encoding="UTF-8"?>
<log xes.version="1.0" xes.features="arbitrary-depth" openxes.version="1.5">
    <extension name="Lifecycle" prefix="lifecycle"
               uri="http://www.xes-standard.org/lifecycle.xesext"/>
    <extension name="Time" prefix="time"
               uri="http://www.xes-standard.org/time.xesext"/>
    <extension name="Concept" prefix="concept"
               uri="http://www.xes-standard.org/concept.xesext"/>
    <global scope="trace">
        <string key="concept:name" value="UNKNOWN"/>
    </global>
    <global scope="event">
        <date key="time:timestamp" value="1970-01-01T00:00:00.000+01:00"/>
        <string key="concept:name" value="UNKNOWN"/>
        <string key="lifecycle:transition" value="UNKNOWN"/>
    </global>
    <string key="concept:name" value="OrderProcess-1.0"/>
    <trace>
        <string key="concept:name" value="123"/>
        <event>
            <date key="time:timestamp" value="2023-03-01T10:15:00.000+10:00"/>
            <string key="concept:name" value="ValidateOrder"/>
            <string key="lifecycle:transition" value="schedule"/>
            <string key="concept:instance" value="123.1"/>
        </event>
        <event>
            <date key="time:timestamp" value="2023-03-01T10:15:05.000+10:00"/>
            <string key="concept:name" value="ValidateOrder"/>
            <string key="lifecycle:transition" value="start"/>
            <string key="concept:instance" value="123.1"/>
            <string key="input/amount" value="1000"/>
            <string key="input/currency" value="USD"/>
        </event>
        <event>
            <date key="time:timestamp" value="2023-03-01T10:15:10.000+10:00"/>
            <string key="concept:name" value="ValidateOrder"/>
            <string key="lifecycle:transition" value="complete"/>
            <string key="concept:instance" value="123.1"/>
        </event>
    </trace>
</log>
```
