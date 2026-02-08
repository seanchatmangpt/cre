# YAWL Java Timer Implementation Analysis

**Date:** 2026-02-07
**Source:** `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/engine/time/`

---

## Overview

The YAWL timer subsystem provides time-based workflow execution capabilities including:
- Scheduled task expiration
- Process launch delays
- Deadline management with persistence
- Work-day calendar support (holidays, weekends)

The implementation follows the classic Java `java.util.Timer` pattern with singleton management and persistence integration.

---

## Architecture

### Class Diagram

```
YTimer (Singleton)
    extends java.util.Timer
    manages: Map<String, TimeKeeper>
    |
    +-- TimeKeeper (inner TimerTask)
         |
         +--> YTimedObject (interface)
               |
               +-- YWorkItemTimer (atomic task timers)
               +-- YLaunchDelayer (case launch delays)
```

### Supporting Classes

```
YTimerVariable
    - State machine for timer status (dormant/active/closed/expired)
    - Predicate evaluation for workflow conditions

WorkDayAdjuster
    - Duration adjustment for business days only
    - Uses HolidayLoader for holiday lookups

HolidayLoader
    - Loads holidays from external SOAP service
    - Caches by year in Map<Integer, List<Holiday>>

Holiday
    - Value object: date + name
    - Comparable by date

HolidayRegion
    - Stores country/region for holiday resolution
```

---

## Timer Scheduling Mechanism

### YTimer: Singleton Scheduler

**File:** `YTimer.java`

YTimer is a singleton wrapper around `java.util.Timer` (daemon thread). It manages a registry of active timers by owner ID.

#### Key Methods

```java
// Singleton access
public static YTimer getInstance()

// Schedule by duration (milliseconds)
public long schedule(YTimedObject timee, long durationAsMilliseconds)

// Schedule by absolute Date
public long schedule(YTimedObject timee, Date expiryTime)

// Schedule by XML Duration (javax.xml.datatype.Duration)
public long schedule(YTimedObject timee, Duration duration)

// Schedule by TimeUnit enum (YEAR, MONTH, WEEK, DAY, HOUR, MIN, SEC, MSEC)
public long schedule(YTimedObject timee, long count, TimeUnit unit)
```

#### Internal Structure

```java
private final Map<String, TimeKeeper> _runners;  // itemID -> TimeKeeper
```

Each `TimeKeeper` is a `TimerTask` that:
1. Stores a reference to its `YTimedObject` owner
2. Calls `owner.handleTimerExpiry()` when triggered
3. Auto-removes itself from `_runners` map

#### Cancellation

```java
public YTimedObject cancelTimerTask(String itemID)
public void cancelTimersForCase(String caseID)  // Bulk cancel by case prefix
public void cancelAll()
public void shutdown()
```

**Important:** Cancellation is two-step:
1. Cancel the `TimerTask` (stops pending execution)
2. Call `owner.cancel()` on the `YTimedObject` (cleanup/unpersist)

---

## Deadline Handling

### YWorkItemTimer: Atomic Task Timers

**File:** `YWorkItemTimer.java`

Represents a timer associated with a single atomic task (work item).

#### States (YTimerVariable)

```
dormant --> active --> { closed | expired }
```

- **dormant**: Timer defined but not started
- **active**: Timer is running
- **closed**: Timer cancelled before expiry
- **expired**: Timer fired and handled

State transitions are validated (no expired -> closed allowed).

#### Trigger Modes (YWorkItemTimer.Trigger)

```java
public enum Trigger { OnEnabled, OnExecuting, Never }
```

- **OnEnabled**: Timer starts when task enters enabled state
- **OnExecuting**: Timer starts when task begins execution
- **Never**: Timer never activates

#### Expiry Handling

When a timer fires, `handleTimerExpiry()` is called:

```java
public void handleTimerExpiry() {
    unpersistThis();                    // Remove from persistence
    YEngine engine = YEngine.getInstance();
    YWorkItem item = engine.getWorkItem(_ownerID);

    if (item != null) {
        // Update timer state in the net
        engine.getNetRunner(item).updateTimerState(item.getTask(), State.expired);

        // Handle based on work item status
        if (item.getStatus().equals(YWorkItemStatus.statusEnabled)) {
            if (item.requiresManualResourcing())
                engine.skipWorkItem(item, null);          // Skip manual tasks
            engine.getAnnouncer().announceTimerExpiryEvent(item);
        }
        else if (item.hasUnfinishedStatus()) {
            if (item.requiresManualResourcing())
                engine.completeWorkItem(item, item.getDataString(), null,
                        WorkItemCompletion.Force);       // Force complete
            engine.getAnnouncer().announceTimerExpiryEvent(item);
        }
    }
}
```

**Key behaviors:**
1. Timer is first unpersisted from database
2. Work item is looked up by ID
3. Special case: if item became a parent, child is used
4. Timer state updated to "expired"
5. If enabled: task is **skipped** (for manual tasks)
6. If executing: task is **force completed**

#### Persistence Integration

Each timer can optionally persist to database:

```java
private boolean _persisting;

public void unpersistThis() {
    if (_persisting) {
        YPersistenceManager pmgr = YEngine.getPersistenceManager();
        if (pmgr != null) {
            try {
                boolean localTransaction = pmgr.startTransaction();
                pmgr.deleteObjectFromExternal(this);
                if (localTransaction) pmgr.commit();
            }
            catch (YPersistenceException ype) {
                // handle exc.
            }
        }
    }
}
```

Hibernate is used for ORM. Timers are persisted when created and unpersisted on:
- Timer expiry
- Timer cancellation
- Work item cancellation

---

## Work-Day Calendar Support

### WorkDayAdjuster

**File:** `workdays/WorkDayAdjuster.java`

Adjusts durations to count only business days (excluding weekends and holidays).

#### Core Algorithm

```java
public Duration adjust(Calendar startDate, Duration duration) {
    Calendar endDate = addDuration(startDate, duration);
    endDate = adjust(startDate, endDate);      // Push forward non-working days
    return spanAsDuration(startDate, endDate);
}

private void calcAdjustedEndDate(Calendar step, Calendar end) {
    while (step.before(end)) {
        step.add(Calendar.DATE, 1);
        if (isWeekend(step) || isHoliday(step)) {
            end.add(Calendar.DATE, 1);         // Extend deadline
        }
    }
}
```

**Logic:** For each day in the duration, if it's a weekend or holiday, extend the end date by one day.

#### Weekend Detection

```java
private boolean isWeekend(Calendar date) {
    int day = date.get(Calendar.DAY_OF_WEEK);
    return day == Calendar.SATURDAY || day == Calendar.SUNDAY;
}
```

### Holiday Management

**Files:** `HolidayLoader.java`, `Holiday.java`, `HolidayRegion.java`

#### HolidayLoader

Loads public holidays from external web service and caches by year.

**Web Service:**
- **Endpoint:** `http://kayaposoft.com/enrico/ws/v1.0/index.php`
- **Operation:** `getPublicHolidaysForYear`
- **Parameters:** year, country, region (optional)

**Caching Strategy:**
```java
private Map<Integer, List<Holiday>> _yearHolidayMap;

public List<Holiday> getHolidays(int year) {
    List<Holiday> holidays = _yearHolidayMap.get(year);
    if (holidays == null) {
        holidays = load(String.valueOf(year));  // SOAP call
        _yearHolidayMap.put(year, holidays);
        persist(persister, holidays);            // Cache to DB
    }
    return holidays;
}
```

**Persistence:** Holidays are stored in database via Hibernate for offline operation.

#### Holiday

Value object representing a single holiday:

```java
public class Holiday implements Comparable<Holiday> {
    private Calendar _date;    // Year, month, day
    private String _name;      // Display name
}
```

#### HolidayRegion

Stores the geographic configuration:

```java
public class HolidayRegion {
    private String _country;   // ISO country code
    private String _region;    // Optional region/subdivision
}
```

**Format:** `country;region` (e.g., "us;ca" for California)

---

## Launch Delayer

### YLaunchDelayer

**File:** `YLaunchDelayer.java`

Delays the launching of a new workflow case until a specified time.

#### Use Cases

- Scheduled workflow execution
- Delayed case start based on external trigger
- Time-based process instantiation

#### Constructor Pattern

```java
public YLaunchDelayer(YSpecificationID specID, String caseParams,
                      URI completionObserver, String caseID,
                      YLogDataItemList logData, String serviceHandle,
                      long msec, boolean persisting)
```

Stores all case launch parameters and schedules execution via `YTimer`.

#### Expiry Handling

```java
public void handleTimerExpiry() {
    try {
        YEngine.getInstance().launchCase(_specID, _caseParams, _completionObserver,
            _caseID, _logData, _serviceURI, true);
    }
    catch (YAWLException ye) {
        LogManager.getLogger(YLaunchDelayer.class).error(
                "Unable to launch delayed instance of " + _specID.toString(), ye);
    }
    persistThis(false);    // Unpersist after launch
}
```

---

## Comparison with CRE's wf_timerq

### Architectural Differences

| Aspect | YAWL (Java) | CRE (Erlang) |
|--------|-------------|--------------|
| **Pattern** | Singleton Timer with TimerTask registry | Pure functional queue |
| **Threading** | Background daemon thread | User process polling |
| **State** | Mutable object with side effects | Immutable data structure |
| **Concurrency** | Synchronized methods | Actor model (message passing) |
| **Time Base** | `System.currentTimeMillis()` (wall clock) | `erlang:monotonic_time(millisecond)` |
| **Persistence** | Integrated with Hibernate | External (user-managed) |

### API Comparison

| Feature | YAWL | CRE wf_timerq |
|---------|------|--------------|
| **Create queue** | Singleton getInstance() | wf_timerq:new() |
| **Add timer** | schedule(obj, duration/Date) | arm(Q, Key, Deadline, Event) |
| **Cancel timer** | cancelTimerTask(itemID) | disarm(Q, Key) |
| **Poll expired** | Implicit (TimerTask.run()) | poll(Q, Now) |
| **Peek next** | None | peek(Q) |
| **Clear all** | cancelAll() | clear_all(Q) |
| **Size check** | None (iterate map) | size(Q), is_empty(Q) |
| **Duration format** | XML Duration, TimeUnit enum | ISO 8601 string |

### Key Differences

#### 1. Implicit vs Explicit Expiry

**YAWL:**
```java
// TimerTask.run() calls handleTimerExpiry() automatically
timer.schedule(new TimeKeeper(obj), duration);
```

**CRE:**
```erlang
% User must explicitly poll for expired timers
{Events, NewQ} = wf_timerq:poll(Q, Now),
```

#### 2. Key vs Object Association

**YAWL:** Associates timer with `YTimedObject` instance. One timer per owner ID.

**CRE:** Generic key-value association. Multiple timers per key allowed (via arming with same key).

#### 3. Time Representation

**YAWL:**
- Wall clock time (`System.currentTimeMillis()`)
- Absolute `Date` objects
- `javax.xml.datatype.Duration` for XML compliance

**CRE:**
- Monotonic time (unaffected by clock changes)
- Integer millisecond deadlines
- ISO 8601 duration strings for YAML compliance

#### 4. Persistence

**YAWL:** Built-in persistence via Hibernate. Timers persist across restarts.

**CRE:** No built-in persistence. User must manage timer state serialization.

### Work-Day Calendar

| Feature | YAWL | CRE wf_timerq |
|---------|------|--------------|
| **Business days** | Yes (WorkDayAdjuster) | No |
| **Holiday loading** | SOAP service + cache | N/A |
| **Weekend skip** | Yes | No |
| **Regional support** | Country + region | N/A |

CRE does not currently implement work-day calendar functionality. This would need to be added as a pre-processing step before arming timers.

### State Machine

**YAWL:** Full state machine (dormant -> active -> closed/expired) with validation.

**CRE:** No explicit state machine. Timer exists or doesn't exist.

---

## Usage Patterns

### YAWL Timer Lifecycle

```
1. Create YWorkItemTimer (auto-schedules via YTimer)
2. Timer persists to database (if enabled)
3. Wait for expiry...
4. TimeKeeper.run() calls YWorkItemTimer.handleTimerExpiry()
5. Unpersist from database
6. Update work item state
7. Skip or force-complete work item
8. Announce expiry event
```

### CRE Timer Lifecycle

```
1. wf_timerq:new() - Create empty queue
2. wf_timerq:arm() - Add timers
3. User process periodically calls wf_timerq:poll(Now)
4. Inject expired events into workflow
5. Update queue with remaining timers
```

---

## Summary

### YAWL Strengths

1. **Integrated persistence** - Timers survive restarts
2. **Implicit expiry** - No polling required
3. **Work-day calendar** - Business duration support
4. **Web service integration** - Dynamic holiday loading
5. **State machine** - Clear timer lifecycle management
6. **XML Duration support** - W3C standard compliance

### YAWL Weaknesses

1. **Singleton bottleneck** - All timers through one instance
2. **Wall clock dependency** - Vulnerable to system time changes
3. **Thread-based** - More complex error handling
4. **Tight coupling** - Direct dependency on YEngine and persistence

### CRE wf_timerq Strengths

1. **Pure functional** - No side effects, easy to test
2. **Monotonic time** - Immune to clock adjustments
3. **Actor model** - Natural concurrency via BEAM
4. **Generic events** - Not tied to specific workflow actions
5. **ISO 8601** - YAML workflow standard compliance

### CRE wf_timerq Weaknesses

1. **No persistence** - User must manage
2. **Explicit polling** - Requires user process to poll
3. **No work-day calendar** - Would need separate implementation
4. **No state machine** - Timer state is implicit

---

## Recommendations for CRE Enhancement

Based on YAWL's implementation, CRE could benefit from:

1. **Work-day calendar module** - Similar to WorkDayAdjuster
2. **Persistence integration** - Timer queue serialization for recovery
3. **State machine** - Explicit timer states (dormant/active/expired)
4. **Regional holiday support** - Configurable holiday definitions
5. **Duration expansion** - ISO 8601 with business day semantics

---

## File Reference

| YAWL File | Purpose | CRE Equivalent |
|-----------|---------|----------------|
| `YTimer.java` | Singleton scheduler | gen_yawl (via user process) |
| `YWorkItemTimer.java` | Atomic task timer | wf_task constructs |
| `YTimedObject.java` | Timer callback interface | timer_event() term |
| `YTimerVariable.java` | State machine | (not implemented) |
| `YLaunchDelayer.java` | Delayed case launch | (not implemented) |
| `WorkDayAdjuster.java` | Business day calc | (not implemented) |
| `HolidayLoader.java` | Holiday cache | (not implemented) |
| `Holiday.java` | Holiday value | (not implemented) |
| `HolidayRegion.java` | Location config | (not implemented) |

---

*Analysis generated from YAWL 2.5+ source code*
