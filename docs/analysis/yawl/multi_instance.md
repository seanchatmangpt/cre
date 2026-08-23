# YAWL Multi-Instance Implementation Analysis

## Executive Summary

This document analyzes the multi-instance (MI) task implementation in YAWL (Yet Another Workflow Language) Java reference implementation and compares it with the CRE (Common Runtime Environment) Erlang implementation.

**Key Files Analyzed:**
- YAWL: `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YMultiInstanceAttributes.java`
- YAWL: `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YAtomicTask.java`
- YAWL: `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/GroupedMIOutputData.java`
- CRE: `/Users/sac/cre/src/patterns/multiple_instances_sync.erl`
- CRE: `/Users/sac/cre/src/wf/yawl_mi_runtime.erl`
- CRE: `/Users/sac/cre/src/wf/wf_mi.erl`

---

## 1. YAWL Multi-Instance Modes

### 1.1 Creation Modes

YAWL supports **two creation modes** for multi-instance tasks:

```java
// YMultiInstanceAttributes.java lines 38-39
public final static String CREATION_MODE_DYNAMIC = "dynamic";
public final static String CREATION_MODE_STATIC = "static";
```

| Mode | Description | Behavior |
|------|-------------|----------|
| **Static** | All instances created at task enablement | Instances are created once when the task fires |
| **Dynamic** | Instances created incrementally | New instances can be added while existing instances execute |

### 1.2 Mode Detection

```java
// YMultiInstanceAttributes.java lines 137-139
public boolean isDynamicCreationMode() {
    return _creationMode.equalsIgnoreCase(CREATION_MODE_DYNAMIC);
}
```

### 1.3 What YAWL Does NOT Have

Contrary to some documentation, YAWL does NOT have a distinct "runtime" mode. The terminology confusion:
- **Static**: Pre-determined number of instances
- **Dynamic**: Number determined at runtime via queries

Both are "runtime" in the sense that queries are evaluated, but the distinction is about *when* instances are created, not *how* the count is determined.

---

## 2. Query-Based Instance Creation

### 2.1 Instance Count Parameters

YAWL uses **three parameters** with both literal and query variants:

```java
// YMultiInstanceAttributes.java lines 44-49
private Integer _minInstances;        // Literal value OR
private String _minInstancesQuery;    // XPath query to evaluate

private Integer _maxInstances;        // Literal value OR
private String _maxInstancesQuery;    // XPath query to evaluate

private Integer _threshold;           // Literal value OR
private String _thresholdQuery;       // XPath query to evaluate
```

### 2.2 Lazy Query Evaluation

Instance counts can be specified as:
1. **Integer literals** (e.g., `"5"`) - parsed immediately
2. **XPath queries** (e.g., `"/data/itemCount"`) - evaluated at runtime

```java
// YMultiInstanceAttributes.java lines 143-163
public void setProperties(String minInstancesQuery, String maxInstancesQuery,
                          String thresholdQuery, String creationMode) {
    try {
        _minInstances = new Integer(minInstancesQuery);
    } catch (NumberFormatException e) {
        _minInstancesQuery = minInstancesQuery;  // Store as query
        _minInstances = null;
    }
    // ... similar for max and threshold
    _creationMode = creationMode;
}
```

### 2.3 Runtime Query Evaluation

```java
// YMultiInstanceAttributes.java lines 76-86
public int getMinInstances() {
    if (_minInstances != null) return _minInstances;

    try {
        return getQueryValue(_minInstancesQuery);
    }
    catch (Exception e) {
        _log.warn("The minInstances query at {} didn't produce numerical output" +
                " as expected. Returning default 1.", _task);
        return 1;
    }
}
```

The `getQueryValue` method uses XPath to extract values from the net's internal data document:

```java
// YMultiInstanceAttributes.java lines 261-265
private int getQueryValue(String query) throws IllegalArgumentException {
    Element element = JDOMUtil.selectElement(_task._net.getInternalDataDocument(), query);
    if (element == null) throw new IllegalArgumentException();
    return new Integer(element.getText());
}
```

---

## 3. Input Data Distribution

### 3.1 Splitting Query

YAWL uses a **splitting query** to distribute data across instances:

```java
// YMultiInstanceAttributes.java lines 185-191
public void setUniqueInputMISplittingQuery(String inputQuery) {
    _inputSplittingQuery = inputQuery;
}

public String getMISplittingQuery() {
    return _inputSplittingQuery;
}
```

### 3.2 XML Structure for Input

```xml
<miDataInput>
    <expression query="/data/inputData"/>
    <splittingExpression query="/data/items/item"/>
    <formalInputParam>item</formalInputParam>
</miDataInput>
```

The splitting query typically returns a **node set** that is iterated over, creating one instance per node.

---

## 4. Output Aggregation

### 4.1 Joining Query

Output from multiple instances is aggregated using a joining query:

```java
// YMultiInstanceAttributes.java lines 214-220
public String getMIJoiningQuery() {
    return _outputProcessingQuery;
}

public void setUniqueOutputMIJoiningQuery(String outputProcessingQuery) {
    _outputProcessingQuery = outputProcessingQuery;
}
```

### 4.2 XML Structure for Output

```xml
<miDataOutput>
    <formalOutputExpression query="/result"/>
    <outputJoiningExpression query="/data/aggregateResults"/>
    <resultAppliedToLocalVariable>aggregatedResults</resultAppliedToLocalVariable>
</miDataOutput>
```

### 4.3 GroupedMIOutputData Class

YAWL persists in-progress MI output data using `GroupedMIOutputData`:

```java
// GroupedMIOutputData.java lines 16-23
/**
 * Provides for the persistence of in-progress multiple instance task output data -
 * i.e. stores the output data of completed child work items of an MI task, until
 * the entire task completes
 */
public class GroupedMIOutputData {
    private String _uniqueID;
    private Document _dataDoc;          // Static instance outputs
    private Document _dynamicDataDoc;   // Dynamic instance outputs
    private Document _completedWorkitems;
}
```

This ensures outputs are available for aggregation even if the workflow is interrupted.

### 4.4 Static vs Dynamic Output

```java
// GroupedMIOutputData.java lines 41-48
protected void addStaticContent(Element content) {
    addContent(_dataDoc, content);
}

protected void addDynamicContent(Element content) {
    addContent(_dynamicDataDoc, content);
}
```

Static instances (created at task start) and dynamic instances (added during execution) have separate output documents.

---

## 5. CRE Implementation Comparison

### 5.1 Architecture Differences

| Aspect | YAWL (Java) | CRE (Erlang) |
|--------|-------------|--------------|
| **State Model** | Object-oriented with persistence | Functional Petri nets (gen_pnet) |
| **Instance Tokens** | YWorkItem objects | `{mi_instance, TaskId, Index}` tuples |
| **Completion** | Repository-based callbacks | Token transition in marking |
| **Aggregation** | XML-based joining queries | Pure functional reduction |

### 5.2 CRE Multi-Instance Parameters

```erlang
% wf_yaml_spec.erl lines 79-82
mi_params :: #{
    min_instances := non_neg_integer(),
    max_instances := non_neg_integer() | unlimited,
    continuation_threshold := non_neg_integer()
}
```

### 5.3 CRE Instance Token Creation

```erlang
% wf_mi.erl lines 228-234
-spec create_instance_tokens(TaskId :: task_id(), Count :: non_neg_integer()) ->
    [mi_token()].
create_instance_tokens(_TaskId, 0) -> [];
create_instance_tokens(TaskId, Count) when is_atom(TaskId), is_integer(Count), Count > 0 ->
    [{mi_instance, TaskId, Index} || Index <- lists:seq(0, Count - 1)].
```

### 5.4 CRE Completion Check

```erlang
% yawl_mi_runtime.erl lines 243-251
-spec is_complete(InstanceTokens :: [mi_token() | mi_complete_token()],
                  MIParams :: mi_params() | undefined) ->
          boolean().
is_complete(InstanceTokens, MIParams) when is_list(InstanceTokens) ->
    CompletedCount = count_completed_instances(InstanceTokens, undefined),
    wf_mi:instance_threshold(MIParams, CompletedCount).
```

### 5.5 CRE Multiple Instance Sync Pattern

The `multiple_instances_sync` module implements WCP-12 as a gen_pnet:

```erlang
% multiple_instances_sync.erl lines 282-298
place_lst() ->
    [
        'p_start',
        'p_spawn',
        'p_instance_pool',
        'p_active_1', 'p_active_2', 'p_active_3', 'p_active_4',
        'p_complete_1', 'p_complete_2', 'p_complete_3', 'p_complete_4',
        'p_sync_barrier',
        'p_all_done',
        'p_complete'
    ].
```

**Key difference**: CRE uses **explicit places** for each instance (up to 4), then distributes additional instances across these places using modulo:

```erlang
% multiple_instances_sync.erl lines 594-599
spawn_instances(Count, Index, Acc) when Index > Count ->
    Acc;
spawn_instances(Count, Index, Acc) when Index =< 4 ->
    Place = list_to_atom("p_active_" ++ integer_to_list(Index)),
    spawn_instances(Count, Index + 1, maps:put(Place, [{instance, Index}], Acc));
spawn_instances(Count, Index, Acc) ->
    %% For instances beyond 4, distribute across existing places
    PlaceNum = ((Index - 1) rem 4) + 1,
    Place = list_to_atom("p_active_" ++ integer_to_list(PlaceNum)),
    ExistingTokens = maps:get(Place, Acc, []),
    spawn_instances(Count, Index + 1, maps:put(Place, ExistingTokens ++ [{instance, Index}], Acc)).
```

---

## 6. Feature Comparison Matrix

| Feature | YAWL Java | CRE Erlang | Notes |
|---------|-----------|------------|-------|
| **Static Instances** | Yes | Yes | Both support pre-determined instance count |
| **Dynamic Instances** | Yes | Partial | CRE has patterns but not true dynamic addition |
| **Min/Max Bounds** | Yes | Yes | Both enforce min/max validation |
| **Threshold Continuation** | Yes | Yes | Both allow continuing before all complete |
| **Query-Based Count** | Yes (XPath) | Yes (map-based) | Different query languages |
| **Input Splitting** | Yes (XPath) | Partial | CRE uses data lists |
| **Output Aggregation** | Yes (XPath) | Pure functional | CRE uses maps/lists |
| **Persistence** | Hibernate | ETS/Disk | Different persistence models |
| **Token Model** | WorkItem objects | Petri net tokens | Fundamental architectural difference |

---

## 7. Code Comparison: Instance Creation

### YAWL (Conceptual)

```java
// YAWL evaluates queries and creates YWorkItem instances
int instanceCount = miAttributes.getMaxInstances(); // Evaluates query if needed
for (int i = 0; i < instanceCount; i++) {
    YWorkItem item = new YWorkItem(caseID, taskID, String.valueOf(i));
    workItemRepository.add(item);
}
```

### CRE (Actual)

```erlang
% CRE creates tokens in a marking
create_instance_tokens(TaskId, _Spec, _NetId, Data, MIParams) ->
    case wf_mi:evaluate_mi(MIParams, Data) of
        {ok, Count} when is_integer(Count), Count >= 0 ->
            [{mi_instance, TaskId, Index} || Index <- lists:seq(0, Count - 1)];
        {error, _Reason} -> []
    end.
```

---

## 8. Code Comparison: Threshold Checking

### YAWL

```java
// YMultiInstanceAttributes.java lines 114-124
public int getThreshold() {
    if (_threshold != null) return _threshold;

    try {
        return getQueryValue(_thresholdQuery);
    }
    catch (Exception e) {
        _log.warn("The threshold query at {} didn't produce numerical output" +
                " as expected. Returning default 1.", _task);
        return 1;
    }
}
```

### CRE

```erlang
% wf_mi.erl lines 273-282
-spec instance_threshold(MIParams :: mi_params() | undefined, Completed :: non_neg_integer()) ->
    boolean().
instance_threshold(undefined, Completed) when Completed > 0 ->
    true;
instance_threshold(undefined, _Completed) ->
    false;
instance_threshold(#{continuation_threshold := Threshold}, Completed) ->
    Completed >= Threshold.
```

---

## 9. Output Aggregation Comparison

### YAWL (XML-based)

```xml
<!-- Aggregate results from multiple instances -->
<outputJoiningExpression query>
    for $result in /results/result
    return &lt;aggregated&gt;{$result}&lt;/aggregated&gt;
</outputJoiningExpression>
```

### CRE (Pure Functional)

```erlang
% Results accumulated in a map, then converted to ordered list
fire('t_complete', #{'p_all_done' := [all_done]}, #multi_instance_state{results = Results}) ->
    ResultList = [maps:get(I, Results) || I <- lists:seq(1, map_size(Results))],
    {produce, #{
        'p_all_done' => [],
        'p_complete' => [{complete, ResultList}]
    }, State};
```

---

## 10. Key Takeaways

### 10.1 YAWL Strengths
1. **Mature persistence** with Hibernate-backed GroupedMIOutputData
2. **XPath-based queries** for flexible runtime evaluation
3. **Separate static/dynamic output** tracking
4. **Comprehensive XML marshalling** of MI configuration

### 10.2 CRE Strengths
1. **Pure functional design** with no side effects
2. **Petri net formalism** provides soundness guarantees
3. **Explicit token model** makes state visible and debuggable
4. **Pattern-based modularity** with gen_yawl behavior

### 10.3 Architectural Differences

| Concept | YAWL | CRE |
|---------|------|-----|
| **State** | Object fields | Petri net marking |
| **Process** | Thread/Callback | gen_server/gen_yawl |
| **Query** | XPath/XQuery | Erlang map patterns |
| **Persistence** | Hibernate | Custom (ETS/Disk) |
| **Validation** | YVerificationHandler | Dialyzer typespec |

---

## 11. Recommendations for CRE Enhancement

Based on YAWL's implementation:

1. **Add XPath query support** for runtime count evaluation
2. **Implement grouped output storage** for better interruption recovery
3. **Support true dynamic instance creation** during execution
4. **Add XML-based output aggregation** for compatibility

---

## 12. Glossary

| Term | Definition |
|------|------------|
| **MI Task** | Multi-Instance task that creates multiple concurrent execution paths |
| **Static Creation** | All instances created simultaneously at task start |
| **Dynamic Creation** | Instances created incrementally during execution |
| **Threshold** | Minimum number of completed instances required for continuation |
| **Splitting Query** | XPath query that divides input data across instances |
| **Joining Query** | XPath query that aggregates output from all instances |
| **Marking** | Petri net state mapping places to tokens |
| **gen_pnet** | CRE's Petri net behavior module |
| **gen_yawl** | CRE's YAWL-specific wrapper around gen_pnet |

---

## Appendix A: File Reference

### YAWL Files
- `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YMultiInstanceAttributes.java` (267 lines)
- `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YTask.java` (300+ lines shown)
- `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YAtomicTask.java` (276 lines)
- `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/GroupedMIOutputData.java` (177 lines)
- `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/stateless/elements/YMultiInstanceAttributes.java` (274 lines)

### CRE Files
- `/Users/sac/cre/src/patterns/multiple_instances_sync.erl` (763 lines)
- `/Users/sac/cre/src/wf/yawl_mi_runtime.erl` (725 lines)
- `/Users/sac/cre/src/wf/wf_mi.erl` (497 lines)
- `/Users/sac/cre/src/wf/wf_yaml_spec.erl` (911 lines)
- `/Users/sac/cre/src/core/yawl_compile.erl` (400+ lines shown)

---

*Document generated: 2026-02-07*
*Analysis of YAWL 2.1/2.2 vs CRE multi-instance implementations*
