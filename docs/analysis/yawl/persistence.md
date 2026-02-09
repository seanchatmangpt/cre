# YAWL Java Persistence Implementation Analysis

## Executive Summary

This document analyzes the YAWL (Yet Another Workflow Language) Java persistence implementation, comparing it with CRE's Erlang-based approach. The YAWL engine uses Hibernate ORM with relational database storage, while CRE uses Mnesia (Erlang's distributed database). Both approaches solve the same fundamental problem: durable workflow state for recovery and long-running business processes.

---

## Table of Contents

1. [Persistence Mechanism](#1-persistence-mechanism)
2. [Restoration Sequence](#2-restoration-sequence)
3. [Hibernate Integration](#3-hibernate-integration)
4. [CRE Comparison](#4-cre-comparison)
5. [Sequence Diagrams](#5-sequence-diagrams)
6. [Key Differences](#6-key-differences)

---

## 1. Persistence Mechanism

### 1.1 Architecture Overview

The YAWL persistence layer follows a classic 3-tier architecture:

```
+-------------------+     +----------------------+     +-------------------+
|   YAWL Engine     | <=> | YPersistenceManager  | <=> |    Hibernate      |
|                   |     |                      |     |    ORM Layer      |
| - YEngine         |     | - Transaction Mgmt   |     | - Session Factory |
| - YNetRunner      |     | - Session Management |     | - HQL Queries     |
| - YWorkItem       |     | - Object CRUD        |     | - Schema Update   |
+-------------------+     +----------------------+     +-------------------+
                                                              ^
                                                              |
                                                              v
                                                     +-------------------+
                                                     |  Relational DB    |
                                                     |  (H2/PostgreSQL)  |
                                                     +-------------------+
```

### 1.2 Persisted Classes

YPersistenceManager manages 12 persisted entity types:

```java
private static final Class[] persistedClasses = {
    YSpecification.class,      // Workflow specifications
    YNetRunner.class,          // Active workflow instances
    YWorkItem.class,           // Work items (tasks)
    YIdentifier.class,         // Case identifiers
    YNetData.class,            // Net data storage
    YAWLServiceReference.class,// Service references
    YExternalClient.class,     // External client credentials
    YWorkItemTimer.class,      // Timer management
    YLaunchDelayer.class,      // Delayed launch
    YCaseNbrStore.class,       // Case number generation
    Problem.class,             // Error/problems
    GroupedMIOutputData.class  // Multi-instance output
};
```

### 1.3 Persistence Operations

The persistence manager supports three core operations:

| Operation | Method | Description |
|-----------|--------|-------------|
| **DB_INSERT** | `storeObject()` | Insert new objects |
| **DB_UPDATE** | `updateObject()` | Update existing objects |
| **DB_DELETE** | `deleteObject()` | Remove objects |

Transaction boundaries are explicit:

```java
public boolean startTransaction() throws YPersistenceException
public void commit() throws YPersistenceException
protected void rollbackTransaction() throws YPersistenceException
```

### 1.4 The `restoring` Flag

A critical flag that prevents persistence loops during restoration:

```java
private boolean restoring = false;

protected void storeObject(Object obj) throws YPersistenceException {
    if ((!restoring) && isEnabled()) {
        doPersistAction(obj, INSERT);
    }
}
```

When `restoring = true`, objects loaded from the database are NOT re-persisted, preventing circular writes during recovery.

---

## 2. Restoration Sequence

### 2.1 High-Level Restoration Flow

```
+----------------+     +---------------------+     +------------------+
| Engine Startup | --> | YEngineRestorer    | --> | Phase 1: Static |
+----------------+     +---------------------+     | Data Recovery   |
                            |                        +------------------+
                            |
                            v
                        +------------------+
                        | Phase 2: Dynamic |
                        | Data Recovery    |
                        +------------------+
                            |
                            v
                        +------------------+
                        | Phase 3: Runner  |
                        | Restart          |
                        +------------------+
```

### 2.2 Detailed Restoration Steps

**Phase 1: Static Data Recovery**

```java
protected void restoreServicesAndClients() throws YPersistenceException {
    restoreYAWLServices();      // Registered YAWL services
    restoreExternalClients();   // External client credentials
}

protected void restoreSpecifications() throws YPersistenceException {
    for (YSpecification s : restoreObjects(YSpecification.class)) {
        loadSpecification(s);   // Reconstruct from XML
    }
}

protected YCaseNbrStore restoreNextAvailableCaseNumber() {
    // Restore case number counter
}
```

**Phase 2: Dynamic Data Recovery**

```java
protected void restoreInstances() throws YPersistenceException {
    restoreGroupedMIOutputData(); // Multi-instance data
    restoreProcessInstances();    // Active cases
    restoreWorkItems();           // Work items
    resortMultiInstanceStartingData();
}
```

**Phase 3: Runner Restart**

```java
protected void restartRestoredProcessInstances() throws YPersistenceException {
    for (YNetRunner runner : _runners) {
        if (!runner.isCompleted()) {
            runner.start(_pmgr);  // Resume execution
        }
    }
}
```

### 2.3 Process Instance Restoration

The most complex restoration involves reconstructing the net runner hierarchy:

```java
// 1. Remove runners without specifications
runners = removeDeadRunners(runners);

// 2. Restore root nets first (no parent)
for (YNetRunner runner : runners) {
    if (runner.getContainingTaskID() == null) {
        YNet net = (YNet) getSpecification(runner).getRootNet().clone();
        runner.setNet(net);
        runnerMap.put(runner.getCaseID().toString(), runner);
    }
}

// 3. Then restore subnet runners
for (YNetRunner runner : runners) {
    if (runner.getContainingTaskID() != null) {
        // Find parent and link
        YNetRunner parentRunner = runnerMap.get(parentID);
        YNet net = (YNet) task.getDecompositionPrototype().clone();
        runner.setNet(net);
    }
}

// 4. Restore YIdentifiers recursively
restoreYIdentifiers(runnerMap, caseID, null, net);

// 5. Restore locations (tokens in places)
restoreLocations(runnerMap, id, parent, net);
```

### 2.4 Work Item Restoration

Work items require special handling for data reconstruction:

```java
protected void restoreWorkItems(List<YWorkItem> workItems) {
    for (YWorkItem witem : workItems) {
        // Reconstruct data from stored string
        String data = witem.get_dataString();
        if (data != null) {
            witem.setInitData(JDOMUtil.stringToElement(data));
        }

        // Reconstruct caseID-YIdentifier
        YIdentifier yCaseID = _idLookupTable.get(caseID);
        witem.setWorkItemID(new YWorkItemID(yCaseID, taskID, uniqueID));

        // Link to task and engine
        witem.setTask(getTaskReference(specID, taskID));
        witem.setEngine(_engine);

        // Restore data to net
        witem.restoreDataToNet(_engine.getYAWLServices());
    }
}
```

---

## 3. Hibernate Integration

### 3.1 Configuration

Hibernate uses XML-based configuration (`hibernate.cfg.xml`):

```xml
<hibernate-configuration>
   <session-factory>
       <property name="current_session_context_class">thread</property>
       <property name="show_sql">false</property>
   </session-factory>
</hibernate-configuration>
```

Key settings:
- **Thread-bound sessions**: Each thread gets its own session
- **SQL logging disabled** for production
- **Schema auto-update** enabled via `SchemaUpdate`

### 3.2 Session Factory Initialization

```java
protected SessionFactory initialise(boolean journalising) {
    StandardServiceRegistry standardRegistry =
        new StandardServiceRegistryBuilder().configure().build();

    MetadataSources metadataSources = new MetadataSources(standardRegistry);
    for (Class clazz : persistedClasses) {
        metadataSources.addClass(clazz);
    }

    Metadata metadata = metadataSources.buildMetadata();
    factory = metadata.buildSessionFactory();

    // Auto-update schema
    new SchemaUpdate().execute(targetTypes, metadata);
    return factory;
}
```

### 3.3 Query Capabilities

The persistence manager provides query methods:

```java
// HQL query execution
public Query createQuery(String queryString)
public List execQuery(String queryString)

// Class-based queries
public List getObjectsForClass(String className)
public List getObjectsForClassWhere(String className, String whereClause)

// Scalar queries
public Object selectScalar(String className, String field, String value)
```

### 3.4 Merge vs SaveOrUpdate

YAWL uses a fallback strategy for updates:

```java
private void updateOrMerge(Object obj) {
    try {
        getSession().saveOrUpdate(obj);
    } catch (Exception e) {
        logger.error("Persistence update failed, trying merge.");
        getSession().merge(obj);
    }
}
```

- **saveOrUpdate**: Uses identifier to determine insert vs update
- **merge**: Re-attaches detached entities (more expensive but safer)

---

## 4. CRE Comparison

### 4.1 Storage Technology Comparison

| Aspect | YAWL (Java) | CRE (Erlang) |
|--------|-------------|--------------|
| **Storage** | Hibernate + RDBMS | Mnesia (distributed) |
| **Schema** | Hibernate annotations/Mappings | Erlang records |
| **Transactions** | ACID via Hibernate | ACID via Mnesia |
| **Query Language** | HQL (Hibernate Query Language) | Mnesia match/read |
| **Distribution** | Application-level | Built-in (native) |
| **Memory Config** | RAM + Disk (disc_copies) | RAM + Disk (disc_copies) |

### 4.2 Entity Mapping

| YAWL Entity | CRE Equivalent |
|-------------|----------------|
| `YNetRunner` | `#wf_case{}` / `#persistent_case{}` |
| `YWorkItem` | `#work_item{}` / `#persistent_workitem{}` |
| `YSpecification` | `spec` field in case map |
| `YIdentifier` | Case ID / marking structure |
| `YWorkItemTimer` | `#wf_persistent_event{}` (timestamped) |

### 4.3 Persistence Code Comparison

**YAWL - Saving a case:**
```java
// Implicit via Hibernate transaction
pmgr.startTransaction();
pmgr.storeObject(runner);
pmgr.commit();
```

**CRE - Saving a case:**
```erlang
%% Explicit record creation
save_case(CaseMap) ->
    PersistentCase = #persistent_case{
        case_id = maps:get(case_id, CaseMap),
        status = maps:get(status, CaseMap, running),
        ...
    },
    mnesia:transaction(fun() -> mnesia:write(PersistentCase) end).
```

### 4.4 Restoration Comparison

**YAWL restoration flow:**
1. Load specifications from database
2. Unmarshal XML to spec objects
3. Load net runners
4. Reconstruct net hierarchy (root then children)
5. Restore YIdentifiers recursively
6. Restore locations (token positions)
7. Restore work items with data
8. Restart runners

**CRE restoration flow:**
```erlang
%% Load case and reconstruct
load_case(CaseId) ->
    case mnesia:read(persistent_case, CaseId) of
        [PersistentCase] ->
            Case = persistent_case_to_wf_case(PersistentCase),
            {ok, Case}
    end.
```

### 4.5 Data Serialization

| Aspect | YAWL | CRE |
|--------|------|-----|
| **Spec Format** | XML (YAWL schema) | Maps / Erlang terms |
| **Data Storage** | JDOM XML strings | Native Erlang terms |
| **Serialization** | JDOM + XML | term_to_binary |
| **External Format** | YAWL XML | YAML (via wf_yaml_spec) |

---

## 5. Sequence Diagrams

### 5.1 Startup and Restoration Sequence

```
Engine          YEngineRestorer      YPersistenceManager      Hibernate         Database
  |                   |                      |                     |                |
  |---restore()------>|                      |                     |                |
  |                   |                      |                     |                |
  |                   |--restoreServices()-->|                     |                |
  |                   |                      |--createQuery------>|                |
  |                   |                      |<-----ResultSet------|                |
  |                   |                      |--execQuery()------->|                |
  |                   |<-----services--------|<-----objects--------|                |
  |                   |                      |                     |                |
  |                   |--restoreInstances()-->|                     |                |
  |                   |                      |                     |                |
  |                   |-->restoreNetRunners()|                     |                |
  |                   |                      |--from YNetRunner--->|                |
  |                   |                      |<---runner objects----|                |
  |                   |                      |                     |                |
  |                   |-->restoreIdentifiers()|                    |                |
  |                   |                      |--from YIdentifier--->|                |
  |                   |                      |<---ID objects-------|                |
  |                   |                      |                     |                |
  |                   |-->restoreWorkItems() |                     |                |
  |                   |                      |--from YWorkItem---->|                |
  |                   |                      |<---work items-------|                |
  |                   |                      |                     |                |
  |                   |--setRestoring(false) |                     |                |
  |                   |                      |                     |                |
  |<--restored--------|                      |                     |                |
```

### 5.2 Runtime Persistence Sequence

```
YNetRunner      YPersistenceManager      Hibernate Session        Database
    |                   |                      |                      |
    |---startTransaction()---------------->|                      |
    |                   |                      |--beginTransaction()->|
    |                   |                      |<----connection-------|
    |<---true-----------|                      |                      |
    |                   |                      |                      |
    |---storeObject()-->|                      |                      |
    |                   |--save(obj)---------->|                      |
    |                   |                      |--INSERT/UPDATE------>|
    |                   |                      |<----success---------|
    |                   |                      |                      |
    |---updateObject()->|                      |                      |
    |                   |--saveOrUpdate(obj)-->|                      |
    |                   |                      |--UPDATE------------->|
    |                   |                      |<----success---------|
    |                   |                      |                      |
    |---commit()--------|                      |                      |
    |                   |--commit()----------->|                      |
    |                   |                      |--COMMIT------------->|
    |                   |                      |<----ack-------------|
    |<---ok-------------|                      |                      |
```

### 5.3 CRE Persistence Sequence (for comparison)

```
wf_engine        yawl_persistence        Mnesia            Mnesia Storage
    |                   |                   |                      |
    |---save_case()---->|                   |                      |
    |                   |--transaction()---->|                      |
    |                   |                   |--write to log-------->|
    |                   |                   |--write to disc------->|
    |                   |<--{atomic, ok}----|                      |
    |<--{ok, CaseId}----|                   |                      |
    |                   |                   |                      |
    |---load_case()---->|                   |                      |
    |                   |--read()----------->|                      |
    |                   |<--record----------|                      |
    |<--{ok, Case}------|                   |                      |
```

---

## 6. Key Differences

### 6.1 Philosophical Differences

| Aspect | YAWL (Java) | CRE (Erlang) |
|--------|-------------|--------------|
| **Paradigm** | Object-Relational Mapping | Native term storage |
| **Complexity** | High (framework overhead) | Low (direct storage) |
| **Flexibility** | Strong typing, schemas | Dynamic typing |
| **Startup Cost** | High (Hibernate init) | Low (Mnesia built-in) |
| **Recovery Time** | Longer (reconstruction) | Faster (direct load) |

### 6.2 Advantages of YAWL Approach

1. **Schema Validation**: Hibernate enforces schema at compile time
2. **Query Power**: HQL provides powerful querying capabilities
3. **Database Independence**: Switch between H2, PostgreSQL, etc.
4. **Tooling**: Rich ecosystem (Hibernate, database tools)
5. **Audit Trail**: Relational model supports external auditing

### 6.3 Advantages of CRE Approach

1. **Let it Crash**: Erlang's supervision trees handle failures naturally
2. **Hot Code Upgrade**: Can upgrade code without stopping
3. **Native Distribution**: Mnesia scales across nodes transparently
4. **Zero Latency**: In-memory table replication
5. **Simpler Model**: No ORM impedance mismatch

### 6.4 Failure Handling Comparison

**YAWL Failure Recovery:**
```java
try {
    getSession().save(obj);
} catch (Exception e) {
    getTransaction().rollback();
    throw new YPersistenceException(...);
}
```

**CRE Failure Recovery:**
```erlang
case mnesia:transaction(Fun) of
    {atomic, Result} -> Result;
    {aborted, Reason} -> {error, Reason}
end
```

The Mnesia approach is more functional and composable.

### 6.5 Multi-Instance Handling

YAWL has sophisticated multi-instance task persistence:

```java
// Restore grouped MI output data
protected void restoreGroupedMIOutputData() {
    for (GroupedMIOutputData miOutputData : restoreObjects(...)) {
        _miOutputDataLookupTable.put(miOutputData.getUniqueIdentifier(), miOutputData);
    }
}

// Reattach to tasks
for (YTask task : _miTasks) {
    GroupedMIOutputData miOutputData = _miOutputDataLookupTable.get(uniqueID);
    if (miOutputData != null) {
        task.setGroupedMultiInstanceOutputData(miOutputData);
    }
}
```

CRE handles this through map-based marking structures.

---

## Conclusion

The YAWL Java persistence implementation represents a traditional enterprise approach: Hibernate ORM with relational database storage, providing strong schema validation and powerful querying. The restoration process is complex, requiring careful reconstruction of object graphs, identifier hierarchies, and parent-child relationships.

CRE's Erlang-based approach leverages Mnesia for a more lightweight, distributed solution that aligns with Erlang's "let it crash" philosophy. The restoration is simpler due to native term storage and the functional paradigm.

For future CRE development, consider:

1. **Hybrid Approach**: Could integrate PostgreSQL for long-term archival
2. **Event Sourcing**: Complement Mnesia with event log for audit trails
3. **Snapshots**: Implement periodic snapshots for faster recovery
4. **Migration Tools**: Build tools to import YAWL XML specifications

---

*Document generated: 2025*
*Source files analyzed:*
- `/vendors/yawl/src/org/yawlfoundation/yawl/engine/YPersistenceManager.java`
- `/vendors/yawl/src/org/yawlfoundation/yawl/engine/YEngineRestorer.java`
- `/src/yawl_persistence.erl`
- `/src/cre_yawl_persistence.erl`
- `/src/wf/wf_persistence.erl`
- `/src/yawl/yawl_marshal.erl`
