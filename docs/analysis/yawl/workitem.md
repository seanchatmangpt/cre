# YAWL Work Item Lifecycle Analysis

## Overview

This document analyzes the YAWL (Yet Another Workflow Language) Java implementation of work items and their repository, based on the source code in `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/engine/`.

## Work Item Status States

The `YWorkItemStatus` enum defines 13 distinct states:

| Status | Description |
|--------|-------------|
| `statusEnabled` | Work item is enabled, ready to be fired |
| `statusFired` | Work item has been fired, ready to start execution |
| `statusExecuting` | Work item is currently being executed |
| `statusComplete` | Work item completed normally |
| `statusForcedComplete` | Work item was forced to complete |
| `statusFailed` | Work item execution failed |
| `statusIsParent` | Work item has children (multi-instance) |
| `statusSuspended` | Work item is suspended (from a live status) |
| `statusDeadlocked` | Work item is deadlocked |
| `statusDeleted` ("Cancelled") | Work item was cancelled (by cancel set or exception) |
| `statusWithdrawn` | Work item was withdrawn (by deferred choice, etc.) |
| `statusCancelledByCase` | Work item cancelled due to case cancellation |
| `statusDiscarded` | Tokens left in net after completion |

## State Transition Diagram

```
                    +------------------+
                    |   statusEnabled  |
                    +------------------+
                            |
                            | fire()
                            v
                    +------------------+
                    |    statusFired   |
                    +------------------+
                            |
                            | start()
                            v
                    +------------------+
        +-----------| statusExecuting  |-----------+
        |           +------------------+           |
        |                    |                      |
        |                    | complete()          | rollback()
        |                    v                      |
        |           +------------------+           |
        |           |  statusComplete  |           |
        |           +------------------+           |
        |                    |                      |
        |                    v                      |
        |              (removed from               |
        |               repository)                |
        |                                          |
        |   suspend()               suspend()      |
        +------------------> statusSuspended <-----+
                                   |
                                   | resume()
                                   v
                            (previous status)
```

### Valid State Transitions

The code enforces the following transitions through specific methods:

| Method | From Status | To Status | Preconditions |
|--------|-------------|-----------|---------------|
| Constructor | - | `statusEnabled` | Initial creation |
| `createChild()` | `statusEnabled` | `statusIsParent` | First child creation |
| Constructor | - | `statusFired` | Child work item creation |
| `setStatusToStarted()` | `statusFired` | `statusExecuting` | Must be fired |
| `setStatusToComplete()` | `statusExecuting` | `statusComplete` | Must be executing/suspended |
| `setStatusToComplete()` | `statusExecuting` | `statusForcedComplete` | With Force flag |
| `setStatusToComplete()` | `statusExecuting` | `statusFailed` | With Fail flag |
| `rollBackStatus()` | `statusExecuting` | `statusFired` | Must be executing |
| `setStatusToSuspended()` | any live status | `statusSuspended` | Must be live |
| `setStatusToUnsuspended()` | `statusSuspended` | previous status | Must be suspended |
| `setStatusToDeleted()` | `statusExecuting` | `statusDeleted` | Must be executing/suspended |
| `setStatusToDiscarded()` | any | `statusDiscarded` | Net completion cleanup |

### Status Classification Methods

```java
// Live statuses: Enabled, Fired, Executing
public boolean hasLiveStatus() {
    return _status.equals(statusFired) ||
           _status.equals(statusEnabled) ||
           _status.equals(statusExecuting);
}

// Finished statuses: Complete, ForcedComplete, Deleted, Failed
public boolean hasFinishedStatus() {
    return hasCompletedStatus() ||
           _status.equals(statusDeleted)  ||
           _status.equals(statusFailed);
}

// Completed statuses: Complete, ForcedComplete
public boolean hasCompletedStatus() {
    return _status.equals(statusComplete) ||
           _status.equals(statusForcedComplete);
}

// Unfinished: Live, Suspended, Deadlocked
public boolean hasUnfinishedStatus() {
    return hasLiveStatus() || _status.equals(statusSuspended) ||
           _status.equals(statusDeadlocked);
}
```

## Work Item Repository Pattern

### Architecture

The `YWorkItemRepository` class implements a **Repository Pattern** with the following characteristics:

1. **In-Memory Cache**: Uses `ConcurrentHashMap` for thread-safe access
2. **Composite Key**: Work items keyed by `"{caseID}:{taskID}"` string
3. **Family Management**: Parent-child relationships for multi-instance tasks
4. **Synchronization**: Periodic cleansing to sync with engine state

### Repository Structure

```java
public class YWorkItemRepository {
    // Key: "caseID:taskID" -> Value: YWorkItem
    private final Map<String, YWorkItem> _itemMap;

    public YWorkItemRepository() {
        _itemMap = new ConcurrentHashMap<>(500);
    }
}
```

### Core Repository Operations

#### Add
```java
protected YWorkItem add(YWorkItem workItem) {
    return _itemMap.put(workItem.getIDString(), workItem);
}
```

#### Get (by various keys)
```java
// By case and task
public YWorkItem get(String caseIDStr, String taskID) {
    return get(caseIDStr + ":" + taskID);
}

// By full item ID
public YWorkItem get(String itemID) {
    return _itemMap.get(itemID);
}
```

#### Remove
```java
public YWorkItem remove(YWorkItem workItem) {
    return _itemMap.remove(workItem.getIDString());
}
```

#### Family Operations (Multi-Instance Support)
```java
// Remove entire family: parent + all children
public Set<YWorkItem> removeWorkItemFamily(YWorkItem workItem) {
    Set<YWorkItem> removedSet = new HashSet<YWorkItem>();
    YWorkItem parent = workItem.getParent() != null ? workItem.getParent() : workItem;
    Set<YWorkItem> children = parent.getChildren();
    if (children != null) {
       for (YWorkItem siblingItem : children) {
           remove(siblingItem);
           removedSet.add(siblingItem);
       }
    }
    remove(parent);
    removedSet.add(parent);
    return removedSet;
}
```

#### Query by Status
```java
public Set<YWorkItem> getWorkItems(YWorkItemStatus status) {
    Set<YWorkItem> itemSet = new HashSet<YWorkItem>();
    for (YWorkItem workitem : _itemMap.values()) {
        if (workitem.getStatus() == status) {
            itemSet.add(workitem);
        }
    }
    return itemSet;
}

// Convenience methods
public Set<YWorkItem> getEnabledWorkItems()    { return getWorkItems(statusEnabled); }
public Set<YWorkItem> getFiredWorkItems()      { return getWorkItems(statusFired); }
public Set<YWorkItem> getExecutingWorkItems()  { return getWorkItems(statusExecuting); }
public Set<YWorkItem> getCompletedWorkItems()  { return getWorkItems(statusComplete); }
public Set<YWorkItem> getParentWorkItems()     { return getWorkItems(statusIsParent); }
```

### Case-Level Operations

```java
// Cancel all work items for a net
public Set<YWorkItem> cancelNet(YIdentifier caseIDForNet) {
    Set<String> itemsToRemove = new HashSet<String>();
    for (YWorkItem item : _itemMap.values()) {
        YIdentifier identifier = item.getWorkItemID().getCaseID();
        if (identifier.isImmediateChildOf(caseIDForNet) ||
                identifier.toString().equals(caseIDForNet.toString())) {
            itemsToRemove.add(item.getIDString());
        }
    }
    return removeItems(itemsToRemove);
}

// Remove all work items for a case
public Set<YWorkItem> removeWorkItemsForCase(YIdentifier caseID) {
    Set<YWorkItem> removedItems = new HashSet<YWorkItem>();
    for (YWorkItem item : getWorkItemsForCase(caseID)) {
        removedItems.addAll(removeWorkItemFamily(item));
    }
    return removedItems;
}
```

### Repository Synchronization

The repository includes a `cleanseRepository()` method that synchronizes the in-memory cache with the actual engine state:

```java
public void cleanseRepository() {
    Set<String> itemsToRemove = new HashSet<String>();
    for (YWorkItem workitem : _itemMap.values()) {

        // Keep completed MI tasks in repository until parent completes
        if (workitem.getTask().isMultiInstance() && workitem.hasCompletedStatus()) {
            continue;
        }

        YNetRunner runner = YEngine.getInstance().getNetRunnerRepository().get(workitem);

        if (runner != null) {
            boolean foundOne = false;
            for (YTask task : runner.getActiveTasks()) {
                if (task.getID().equals(workitem.getTaskID())) {
                    foundOne = true;
                    break;
                }
            }

            // Clean up all work items out of sync with the engine
            if (! foundOne) itemsToRemove.add(workitem.getIDString());
        }
    }
    if (! itemsToRemove.isEmpty()) removeItems(itemsToRemove);
}
```

## Work Item Structure

### Core Attributes

```java
public class YWorkItem {
    private YWorkItemID _workItemID;              // Composite ID (caseID + taskID + uniqueID)
    private YSpecificationID _specID;             // Specification identifier
    private YTask _task;                          // Task this item is derived from
    private YWorkItemStatus _status;              // Current status
    private YWorkItemStatus _prevStatus;          // Previous status (for suspend/resume)
    private YClient _externalClient;              // Service/app that started execution

    // Timestamps
    private Date _enablementTime;                 // When work item was enabled
    private Date _firingTime;                     // When work item was fired
    private Date _startTime;                      // When execution started

    // Family relationships (multi-instance)
    private YWorkItem _parent;                    // Parent work item
    private Set<YWorkItem> _children;             // Child work items

    // Data
    private Element _dataList;                    // Input/output data
    private String _dataString;                   // Persisted data

    // Configuration
    private YAttributeMap _attributes;            // Decomposition attributes
    private boolean _allowsDynamicCreation;       // Can create dynamic instances
    private boolean _requiresManualResourcing;    // Requires manual resource assignment

    // Timer support
    private YTimerParameters _timerParameters;    // Timer configuration
    private boolean _timerStarted;                // Timer active flag
    private long _timerExpiry;                    // Timer expiry timestamp

    // Extensions
    private URL _customFormURL;                   // Custom form URL
    private String _codelet;                      // Codelet identifier
    private String _documentation;                // Task documentation
    private String _externalStartingLogPredicate;  // Set on checkout
    private String _externalCompletionLogPredicate;// Set on checkin
}
```

### Work Item ID Structure

```java
// Composite ID format: "caseID.taskID!uniqueID"
// Example: "123_45.task_A!1"
```

## Persistence Integration

### Persistence Lifecycle

1. **Creation**: Work item stored immediately on creation (unless deadlocked)
   ```java
   if ((pmgr != null) && (! isDeadlocked)) pmgr.storeObject(this);
   ```

2. **Updates**: Work item updated on status changes
   ```java
   if (pmgr != null) pmgr.updateObject(this);
   ```

3. **Completion**: Work item removed from persistence on completion
   ```java
   if (pmgr != null) pmgr.deleteObject(item);
   ```

### Completion Flow

```java
private void completePersistence(YPersistenceManager pmgr,
                                 YWorkItemStatus completionStatus)
        throws YPersistenceException {

    // Validate can complete
    if (!(_status.equals(statusExecuting) || _status.equals(statusSuspended))) {
        throw new RuntimeException(this + " [when current status is \""
               + _status + "\" it cannot be moved to \"" + completionStatus + "\"]");
    }

    // Set final status, log event and remove from persistence
    set_status(null, completionStatus);              // don't persist status update
    logAndUnpersist(pmgr, this);
    completeParentPersistence(pmgr);                  // Handle parent if multi-instance
}
```

## Multi-Instance (Parent-Child) Pattern

### Parent Creation

When a work item creates its first child:

```java
public YWorkItem createChild(YPersistenceManager pmgr, YIdentifier childCaseID)
       throws YPersistenceException {
    if (this._parent == null) {
        // Validate child caseID
        YIdentifier parentCaseID = getWorkItemID().getCaseID();
        if ((childCaseID == null) || (childCaseID.getParent() == null) ||
                (! childCaseID.getParent().equals(parentCaseID))) return null;

        // Parent transitions to IsParent status
        set_status(pmgr, statusIsParent);

        // Create children set if needed
        if (_children == null) {
            _children = new HashSet<YWorkItem>();
            _eventLog.logWorkItemEvent(this, _status, createLogDataList("createChild"));
        }

        // Create child work item (starts as Fired)
        YWorkItem childItem = new YWorkItem(pmgr,
                new YWorkItemID(childCaseID, getWorkItemID().getTaskID()),
                _specID, getEnablementTime(), this, _allowsDynamicCreation);

        // Inherit attributes from parent
        childItem.setTask(getTask());
        childItem.setRequiresManualResourcing(requiresManualResourcing());
        childItem.setAttributes(getAttributes());
        childItem.setTimerParameters(getTimerParameters());
        childItem.setCustomFormURL(getCustomFormURL());
        childItem.setCodelet(getCodelet());

        _children.add(childItem);
        if (pmgr != null) pmgr.updateObject(this);
        return childItem;
    }
    return null;
}
```

### Parent Completion Logic

```java
private void completeParentPersistence(YPersistenceManager pmgr)
        throws YPersistenceException {

    synchronized(_parent) {                      // sequentially handle children
        // If all siblings are completed, then the parent is completed too
        boolean parentComplete = true;

        if (_parent.getChildren().size() >  1) {  // short-circuit if not multi-task
            for (YWorkItem mysibling : _parent.getChildren()) {
                if (mysibling.hasUnfinishedStatus()) {
                    parentComplete = false;
                    break;
                }
            }
        }

        if (parentComplete) logAndUnpersist(pmgr, _parent);
    }
}
```

## Timer Support

### Timer Parameters

```java
public void checkStartTimer(YPersistenceManager pmgr, YNetData data)
        throws YPersistenceException {

    if (_timerParameters != null) {
        // Get values from net-level var if necessary
        String netParam = _timerParameters.getVariableName();
        if (netParam != null) {
            if (!unpackTimerParams(netParam, data)) return;
        }

        // If current workitem status equals trigger status, start the timer
        if (_timerParameters.triggerMatchesStatus(_status)) {
            YWorkItemTimer timer = null;
            switch (_timerParameters.getTimerType()) {
                case Expiry: {
                    timer = new YWorkItemTimer(_workItemID.toString(),
                            _timerParameters.getDate(), (pmgr != null));
                    break;
                }
                case Duration: {
                    timer = new YWorkItemTimer(_workItemID.toString(),
                            _timerParameters.getWorkDayDuration(), (pmgr != null));
                    break;
                }
                case Interval: {
                    timer = new YWorkItemTimer(_workItemID.toString(),
                            _timerParameters.getTicks(),
                            _timerParameters.getTimeUnit(), (pmgr != null));
                }
            }
            if (timer != null) {
                _timerExpiry = timer.getEndTime();
                setTimerActive();
                _timerStarted = true;
                if (pmgr != null) {
                    pmgr.storeObject(timer);
                }
            }
        }
    }
}
```

## Key Design Patterns

### 1. Repository Pattern
- Centralized collection of active work items
- Query by status, case, task, service
- Family-based operations for multi-instance

### 2. State Machine Pattern
- Explicit status enum with 13 states
- Guarded transitions (precondition checks)
- Previous status tracking for suspend/resume

### 3. Composite Pattern
- Parent-child relationships for multi-instance tasks
- Recursive family operations
- Synchronized child completion handling

### 4. Observer Pattern (via Events)
- `YEventLogger` logs all state transitions
- `YEngine` announcements for status changes
- Configurable log predicates

## Erlang/OTP Implications for CRE

When porting to Erlang/OTP:

1. **State Machine**: Use a finite state machine behavior (gen_fsm or gen_statem)
2. **Repository**: Use ETS or DETS for the work item cache
3. **Parent-Child**: Use supervisor hierarchy for multi-instance
4. **Persistence**: Consider Mnesia for distributed persistence
5. **Timers**: Use Erlang's native timer:apply_after or gen_server timeout
6. **Event Logging**: Use telemetry or custom event handlers

## References

- `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/engine/YWorkItem.java`
- `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/engine/YWorkItemStatus.java`
- `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/engine/YWorkItemRepository.java`
