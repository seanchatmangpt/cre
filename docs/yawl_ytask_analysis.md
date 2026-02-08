# YAWL YTask Class Hierarchy Analysis

**Source**: `/Users/sac/cre/vendors/yawl/src/org/yawlfoundation/yawl/elements/YTask.java`

## Overview

`YTask` is an abstract class that serves as the superclass for all task types in the YAWL (Yet Another Workflow Language) workflow engine. It extends `YExternalNetElement` and implements the core workflow execution semantics including split/join behaviors, multi-instance task support, cancellation regions, and data mapping.

## Class Hierarchy

```
YNetElement
    └── YExternalNetElement
            └── YTask (abstract)
                    ├── YAtomicTask
                    └── YCompositeTask
```

## 1. Split/Join Type Handling

### Constants

```java
public static final int _AND = 95;   // AND split/join
public static final int _OR = 103;   // OR split/join
public static final int _XOR = 126;  // XOR split/join
```

### Instance Variables

```java
private int _splitType;  // How task outputs are distributed
private int _joinType;   // How task inputs are synchronized
```

### Join Type Semantics (t_enabled method)

The `t_enabled(YIdentifier id)` method (lines 1000-1024) determines when a task can fire based on join type:

| Join Type | Enable Condition | Description |
|-----------|-----------------|-------------|
| **AND** | All preset conditions have tokens | Waits for all incoming branches |
| **OR** | Delegated to `_net.orJoinEnabled()` | Complex OR-join semantics requiring net-wide analysis |
| **XOR** | At least one preset condition has a token | Enabled on any incoming branch |

**Code Implementation**:

```java
switch (_joinType) {
    case YTask._AND:
        for (YExternalNetElement condition : getPresetElements()) {
            if (!((YCondition) condition).containsIdentifier()) {
                return false;
            }
        }
        return true;
    case YTask._OR:
        return _net.orJoinEnabled(this, id);
    case YTask._XOR:
        for (YExternalNetElement condition : getPresetElements()) {
            if (((YCondition) condition).containsIdentifier()) {
                return true;
            }
        }
        return false;
}
```

### Split Type Semantics (t_exit method)

The `t_exit(YPersistenceManager pmgr)` method (lines 768-778) handles token distribution after task completion:

| Split Type | Behavior | Predicate Support |
|------------|----------|-------------------|
| **AND** | All outgoing branches receive a token | No predicates allowed |
| **OR** | All branches with true predicates receive tokens | XPath predicates required, default optional |
| **XOR** | Exactly one branch receives a token (first true predicate) | XPath predicates OR default flow required |

**Code Implementation**:

```java
switch (_splitType) {
    case YTask._AND:
        doAndSplit(pmgr, i);
        break;
    case YTask._OR:
        doOrSplit(pmgr, i);
        break;
    case YTask._XOR:
        doXORSplit(pmgr, i);
        break;
}
```

### Split Implementation Details

#### AND Split (lines 928-935)

```java
private void doAndSplit(YPersistenceManager pmgr, YIdentifier tokenToSend)
        throws YPersistenceException {
    if (tokenToSend != null) {
        for (YExternalNetElement element : getPostsetElements()) {
            ((YCondition) element).add(pmgr, tokenToSend);
        }
    } else throw new RuntimeException("token is equal to null = " + tokenToSend);
}
```

#### OR Split (lines 901-925)

```java
private void doOrSplit(YPersistenceManager pmgr, YIdentifier tokenToSend)
        throws YQueryException, YPersistenceException {
    boolean noTokensOutput = true;
    List<YFlow> flows = new ArrayList<YFlow>(getPostsetFlows());
    Collections.sort(flows);  // Sort by evaluation ordering

    for (YFlow flow : flows) {
        if (evaluateSplitQuery(flow.getXpathPredicate(), tokenToSend)) {
            ((YCondition) flow.getNextElement()).add(pmgr, tokenToSend);
            noTokensOutput = false;
        }
        if (flow.isDefaultFlow() && noTokensOutput) {
            ((YCondition) flow.getNextElement()).add(pmgr, tokenToSend);
        }
    }
}
```

#### XOR Split (lines 873-898)

```java
private void doXORSplit(YPersistenceManager pmgr, YIdentifier tokenToSend)
        throws YQueryException, YPersistenceException {
    List<YFlow> flows = new ArrayList<YFlow>(getPostsetFlows());
    Collections.sort(flows);  // Sort by evaluation ordering, default last

    for (YFlow flow : flows) {
        if (flow.isDefaultFlow()) {                 // Last flow - default
            ((YCondition) flow.getNextElement()).add(pmgr, tokenToSend);
            return;
        }
        if (evaluateSplitQuery(flow.getXpathPredicate(), tokenToSend)) {
            ((YCondition) flow.getNextElement()).add(pmgr, tokenToSend);
            return;
        }
    }
}
```

### Predicate Evaluation

The `evaluateSplitQuery` method (lines 938-975) evaluates XPath predicates:

```java
private boolean evaluateSplitQuery(String query, YIdentifier tokenToSend)
        throws YQueryException {
    // Timer predicate support
    if (isTimerPredicate(query)) {
        return evaluateTimerPredicate(query, tokenToSend);
    }

    // Plugin evaluator expressions
    query = PredicateEvaluatorCache.process(getDecompositionPrototype(), query, tokenToSend);

    // Standard XQuery boolean evaluation
    String xquery = "boolean(" + query + ")";
    String result = SaxonUtil.evaluateQuery(xquery, _net.getInternalDataDocument());
    return result.equalsIgnoreCase("true");
}
```

### Flow Predicate Rules (YFlow.java)

| Flow Property | AND-Split | OR-Split | XOR-Split |
|---------------|-----------|----------|-----------|
| Predicate | Not allowed | Required | Optional (with default) |
| Default Flow | Not allowed | Optional | Optional (without predicate) |
| Evaluation Ordering | Not allowed | Not allowed | Required for predicates |

## 2. Multi-Instance Attributes

### Core Attributes

The `YMultiInstanceAttributes` class (lines 36-267 in YMultiInstanceAttributes.java) manages multi-instance task configuration:

| Attribute | Type | Description |
|-----------|------|-------------|
| `_minInstances` | Integer | Minimum number of instances (or query) |
| `_maxInstances` | Integer | Maximum number of instances (or query) |
| `_threshold` | Integer | Minimum completed instances before task can exit |
| `_creationMode` | String | "static" or "dynamic" |
| `_inputVarName` | String | Formal input parameter for MI data |
| `_inputSplittingQuery` | String | XQuery to split input data into instances |
| `_remoteOutputQuery` | String | XQuery to extract output from each instance |
| `_outputProcessingQuery` | String | XQuery to aggregate instance outputs |

### Creation Modes

| Mode | Description | Instance Creation |
|------|-------------|-------------------|
| **static** | All instances created at task firing | Number determined at start |
| **dynamic** | Instances can be added during execution | Via `t_add()` method |

### Instance Count Determination (lines 410-436)

```java
public long determineHowManyInstancesToCreate() throws YDataStateException, YQueryException {
    if (!isMultiInstance()) {
        return 1;
    }

    List<Content> multiInstanceList = splitStartingDataForMultiInstances();
    int listSize = multiInstanceList.size();
    int max = _multiInstAttr.getMaxInstances();
    int min = _multiInstAttr.getMinInstances();

    // Validate instance count bounds
    if (listSize > max || listSize < min) {
        throw new YDataQueryException(
            String.format(
                "The number of instances produced by MI split (%d) is %s than " +
                "the %s instance bound specified (%d).",
                listSize,
                (listSize > max ? "more" : "less"),
                (listSize > max ? "maximum" : "minimum"),
                (listSize > max ? max : min))
        );
    }
    _multiInstanceSpecificParamsIterator = multiInstanceList.iterator();
    return listSize;
}
```

### Threshold-Based Exit (lines 498-506)

Multi-instance tasks can exit before all instances complete:

```java
public synchronized boolean t_isExitEnabled() {
    return t_isBusy() &&
            ((
                    _mi_active.getIdentifiers().containsAll(_mi_complete.getIdentifiers()) &&
                            _mi_complete.getIdentifiers().containsAll(_mi_active.getIdentifiers())
            ) || (
                    _mi_complete.getIdentifiers().size() >= _multiInstAttr.getThreshold()
            ));
}
```

Exit conditions:
1. **All complete**: All active instances are complete, OR
2. **Threshold reached**: Completed instances >= threshold value

### Dynamic Instance Addition (lines 382-396)

```java
public synchronized YIdentifier t_add(YPersistenceManager pmgr,
                                      YIdentifier siblingWithPermission, Element newInstanceData)
        throws YDataStateException, YStateException, YQueryException, YPersistenceException {
    if (!YMultiInstanceAttributes.CREATION_MODE_DYNAMIC.equals(_multiInstAttr.getCreationMode())) {
        throw new RuntimeException(this + " does not allow dynamic instance creation.");
    }
    if (t_addEnabled(siblingWithPermission)) {
        List<Content> newData = new ArrayList<>();
        newData.add(newInstanceData);
        _multiInstanceSpecificParamsIterator = newData.iterator();
        YIdentifier newInstance = createFiredIdentifier(pmgr);
        prepareDataForInstanceStarting(newInstance);
        return newInstance;
    }
    return null;
}
```

Dynamic addition constraints:
- Task must be in dynamic creation mode
- Task must be busy
- Active instances < maxInstances
- Requesting identifier must be in executing set

### Data Splitting for Multi-Instance (lines 472-482)

```java
private List<Content> splitStartingDataForMultiInstances()
        throws YQueryException, YDataQueryException {
    String queryString = getPreSplittingMIQuery();
    Element dataToSplit = evaluateTreeQuery(queryString, _net.getInternalDataDocument());
    if (dataToSplit == null) {
        throw new YDataQueryException(queryString, dataToSplit, this.getID(),
                "No data available for MI splitting at task start");
    }
    return evaluateListQuery(_multiInstAttr.getMISplittingQuery(), dataToSplit);
}
```

Data flow:
1. **Pre-splitting query**: Extracts data from net variables (`getPreSplittingMIQuery()`)
2. **Splitting query**: Divides data into list for each instance (`getMISplittingQuery()`)

## 3. Remove Set (Cancellation Mechanism)

### Cancellation Region (lines 74, 277-300)

The remove set implements the cancellation region pattern:

```java
private Set<YExternalNetElement> _removeSet = new HashSet<YExternalNetElement>();

public Set<YExternalNetElement> getRemoveSet() {
    if (_removeSet != null) {
        return new HashSet<YExternalNetElement>(_removeSet);
    }
    return null;
}

public void addRemovesTokensFrom(List<YExternalNetElement> removeSet) {
    _removeSet.addAll(removeSet);

    // Need to add the task to the CancelledBySet as well
    for (YExternalNetElement element : removeSet) {
        element.addToCancelledBySet(this);
    }
}
```

### Bidirectional Cancellation Tracking

| Direction | Set | Purpose |
|-----------|-----|---------|
| Forward | `_removeSet` | Elements this task cancels when it completes |
| Backward | `_cancelledBySet` (in YExternalNetElement) | Tasks that can cancel this element |

### Cancellation Execution (lines 759-766)

During task exit (`t_exit`):

```java
// Remove tokens from cancellation set
for (YExternalNetElement netElement : _removeSet) {
    if (netElement instanceof YTask) {
        ((YTask) netElement).cancel(pmgr);
    } else if (netElement instanceof YCondition) {
        ((YCondition) netElement).removeAll(pmgr);
    }
}
```

Cancellation behavior:
- **Tasks**: Calls `cancel()` which recursively cancels and removes the identifier
- **Conditions**: Removes all tokens from the condition

### Task Cancel Implementation (lines 1270-1277)

```java
public synchronized void cancel(YPersistenceManager pmgr) throws YPersistenceException {
    purgeLocations(pmgr);
    if (_i != null) {
        _i.removeLocation(pmgr, this);
        _caseToDataMap.remove(_i);
        _i = null;
    }
}
```

### Remove Set Verification (lines 1714-1723)

```java
for (YExternalNetElement element : _removeSet) {
    if (element == null) {
        handler.error(this,
                this + " refers to a non existent element in its remove set.");
    } else if (!element._net.equals(_net)) {
        handler.error(this,
                this + " and " + element + " must be contained in the same net."
                        + " (container " + _net + " & " + element._net + ")");
    }
}
```

Constraints:
1. Remove set elements must exist
2. Remove set elements must be in the same net

## 4. Data Mapping for Task Starting/Completion

### Data Mapping Structures

```java
// Task starting: net variables -> task input parameters
protected final Map<String, String> _dataMappingsForTaskStarting =
        new HashMap<String, String>();  // [key=ParamName, value=query]

// Task completion: task output -> net variables
private final Map<String, String> _dataMappingsForTaskCompletion =
        new HashMap<String, String>();  // [key=query, value=NetVarName]

// Task enablement: special enablement parameters
protected final Map<String, String> _dataMappingsForTaskEnablement =
        new HashMap<String, String>();  // [key=ParamName, value=query]
```

### Task Starting Data Flow (lines 1073-1130)

```
getStartingDataSnapshot():
    1. Create root element for decomposition data
    2. For each input parameter:
       a. Get mapping query from _dataMappingsForTaskStarting
       b. If MI parameter and instance: use iterator
       c. Otherwise: evaluate query against net data
       d. Validate result against schema
       e. Add to child case data
    3. Return data document for task instance
```

**Code Snippet**:

```java
public Element getStartingDataSnapshot()
        throws YDataStateException, YStateException, YQueryException {
    Element dataForChildCase = produceDataRootElement();
    List<YParameter> inputParams =
            new ArrayList<YParameter>(_decompositionPrototype.getInputParameters().values());
    Collections.sort(inputParams);

    for (YParameter parameter : inputParams) {
        String inputParamName = parameter.getPreferredName();
        String expression = _dataMappingsForTaskStarting.get(inputParamName);

        // Multi-instance parameter handling
        if (this.isMultiInstance() && inputParamName.equals(
                _multiInstAttr.getMIFormalInputParam())) {
            if (_multiInstanceSpecificParamsIterator == null) continue;

            Element specificMIData = _multiInstanceSpecificParamsIterator.hasNext() ?
                    (Element) _multiInstanceSpecificParamsIterator.next() : null;

            if (specificMIData != null) {
                dataForChildCase.addContent(specificMIData.detach());
            }
        } else {
            // Regular parameter handling
            Element result = ExternalDataGatewayFactory.isExternalDataMappingExpression(expression) ?
                    performExternalDataExtraction(expression, parameter) :
                    performDataExtraction(expression, parameter);

            if (result != null) {
                dataForChildCase.addContent(result.clone());
            }
        }
    }
    return dataForChildCase;
}
```

### Task Completion Data Flow (lines 509-596)

```
t_complete(childID, decompositionOutputData):
    1. Validate output data against schema
    2. For each completion mapping:
       a. Evaluate query against decomposition output
       b. If MI output query: add to grouped output
       c. Otherwise: store in local variable map
       d. Validate result type
    3. Move child from executing to complete
    4. If exit enabled: call t_exit()
```

**Code Snippet**:

```java
public synchronized boolean t_complete(YPersistenceManager pmgr, YIdentifier childID,
                                       Document decompositionOutputData)
        throws YDataStateException, YStateException, YQueryException,
        YPersistenceException {
    if (t_isBusy()) {
        YSpecification spec = _net.getSpecification();
        YDataValidator validator = spec.getDataValidator();
        validateOutputs(validator, decompositionOutputData);

        for (String query : getQueriesForTaskCompletion()) {
            if (ExternalDataGatewayFactory.isExternalDataMappingExpression(query)) {
                // External data gateway handling
                ExternalDataGateway gateway =
                        ExternalDataGatewayFactory.getInstance(query);
                updateExternalFromTaskCompletion(gateway, query, decompositionOutputData);
                continue;
            }

            String localVarThatQueryResultGetsAppliedTo = getMIOutputAssignmentVar(query);
            Element queryResultElement = evaluateTreeQuery(query, decompositionOutputData);

            if (queryResultElement == null) {
                throw new YDataQueryException(query, queryResultElement, null,
                        "The result of the output query (" + query + ") is null");
            }

            // Multi-instance output handling
            if (query.equals(getPreJoiningMIQuery())) {
                _groupedMultiInstanceOutputData.addStaticContent(queryResultElement);
                pmgr.updateObjectExternal(_groupedMultiInstanceOutputData);
            }
            else {
                _localVariableNameToReplaceableOutputData.put(
                        localVarThatQueryResultGetsAppliedTo, queryResultElement);
            }
        }
        _mi_executing.removeOne(pmgr, childID);
        _mi_complete.add(pmgr, childID);
        if (t_isExitEnabled()) {
            t_exit(pmgr);
            return true;
        }
        return false;
    }
    // ... exception handling
}
```

### Output Assignment (lines 793-840)

During task exit, collected output data is assigned back to net variables:

```java
private void performDataAssignmentsAccordingToOutputExpressions(YPersistenceManager pmgr)
        throws YDataStateException, YQueryException, YPersistenceException {
    if (null == getDecompositionPrototype()) {
        return;
    }

    // Regular output mappings
    for (String localVariableName : _localVariableNameToReplaceableOutputData.keySet()) {
        Element queryResult = _localVariableNameToReplaceableOutputData.get(localVariableName);
        _net.addData(pmgr, queryResult);
    }

    // Multi-instance aggregation
    if (this.isMultiInstance() && _multiInstAttr.getMIJoiningQuery() != null) {
        Element result = evaluateTreeQuery(
                _multiInstAttr.getMIJoiningQuery(),
                _groupedMultiInstanceOutputData.getDataDoc());

        // Schema validation for aggregated result
        if (_net.getSpecification().getSchemaVersion().isSchemaValidating()) {
            String uniqueInstanceOutputQuery = _multiInstAttr.getMIFormalOutputQuery();
            String localVarThatQueryResultGetsAppliedTo =
                    _dataMappingsForTaskCompletion.get(uniqueInstanceOutputQuery);
            YVariable var = _net.getLocalOrInputVariable(localVarThatQueryResultGetsAppliedTo);
            Element tempRoot = new Element(_decompositionPrototype.getID());
            tempRoot.addContent(result.clone());
            _net.getSpecification().getDataValidator().validate(var, tempRoot, getID());
        }
        _net.addData(pmgr, result);
    }
}
```

## 5. Internal State Management

### Internal Conditions

YTask maintains four internal conditions for multi-instance lifecycle tracking:

```java
protected YInternalCondition _mi_active = new YInternalCondition(YInternalCondition._mi_active, this);
protected YInternalCondition _mi_entered = new YInternalCondition(YInternalCondition._mi_entered, this);
protected YInternalCondition _mi_complete = new YInternalCondition(YInternalCondition._mi_complete, this);
protected YInternalCondition _mi_executing = new YInternalCondition(YInternalCondition._mi_executing, this);
```

| Condition | Purpose | Lifecycle |
|-----------|---------|-----------|
| `_mi_active` | Tracks all active instances | Added at fire, removed at exit |
| `_mi_entered` | Tracks instances that have entered | Added at fire, cleared at exit |
| `_mi_executing` | Tracks instances currently executing | Added at start, removed at complete |
| `_mi_complete` | Tracks completed instances | Added at complete, cleared at exit |

### Instance Lifecycle

```
t_fire():
    1. Create child identifier
    2. Add to _mi_active
    3. Add to _mi_entered
    4. Prepare starting data

t_start():
    1. Transition to executing state

t_complete():
    1. Remove from _mi_executing
    2. Add to _mi_complete
    3. Check exit conditions

t_exit():
    1. Perform output assignments
    2. Process remove set (cancel)
    3. Perform split
    4. Purge all internal conditions
    5. Remove identifier location
```

### Identifier Management

```java
protected YIdentifier createFiredIdentifier(YPersistenceManager pmgr) throws YPersistenceException {
    YIdentifier childCaseID = _i.createChild(pmgr);
    _mi_active.add(pmgr, childCaseID);
    _mi_entered.add(pmgr, childCaseID);
    return childCaseID;
}
```

## 6. External Data Gateway Integration

YAWL supports external data sources through a plugin mechanism:

```java
if (ExternalDataGatewayFactory.isExternalDataMappingExpression(expression)) {
    ExternalDataGateway gateway = ExternalDataGatewayFactory.getInstance(expression);
    if (gateway != null) {
        Element netData = _net.getInternalDataDocument().getRootElement();
        result = gateway.populateTaskParameter(this, inputParam, netData);
    }
}
```

External data mapping expressions are identified by a specific prefix format and allow:
- Database queries
- Web service calls
- Custom data sources
- Legacy system integration

## 7. Timer Support

Tasks can have timer parameters for deadline-based execution:

```java
private YTimerParameters _timerParams;
private YTimerVariable _timerVariable;
```

Timer predicates in XOR/OR splits:

```java
private boolean isTimerPredicate(String predicate) {
    return predicate.trim().startsWith("timer(");
}

private boolean evaluateTimerPredicate(String predicate, YIdentifier token) throws YQueryException {
    YNetRunner runner = getNetRunnerRepository().get(token);
    if (runner != null) {
        return runner.evaluateTimerPredicate(predicate);
    } else throw new YQueryException("Unable to determine current timer status for " +
            "predicate: " + predicate);
}
```

## Summary Comparison Tables

### Split Types Comparison

| Feature | AND | OR | XOR |
|---------|-----|-------|-----|
| Predicate Required | No | Yes | Optional |
| Default Flow | No | Optional | Optional |
| Evaluation Order | No | No | Yes |
| Branches Executed | All | All true | First true only |
| Semantics | Parallel execution | Conditional parallel | Mutual exclusion |

### Join Types Comparison

| Feature | AND | OR | XOR |
|---------|-----|-------|-----|
| Enable Condition | All inputs have tokens | Complex net-wide analysis | Any input has token |
| Token Consumption | All tokens | All tokens with tokens | One token only |
| Semantics | Synchronization | Merging | Choice |

### Data Mapping Direction

| Phase | Direction | Map Structure | Purpose |
|-------|-----------|---------------|---------|
| Starting | Net -> Task | ParamName -> Query | Extract net data for task input |
| Completion | Task -> Net | Query -> NetVarName | Extract task output to net variables |
| Enablement | Net -> Task | ParamName -> Query | Special enablement-time data |
| MI Input | Net -> Instances | Splitting Query | Divide data among instances |
| MI Output | Instances -> Net | Aggregation Query | Combine instance outputs |
