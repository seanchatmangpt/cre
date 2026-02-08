# YAWL Java Marking Implementation Analysis

## Executive Summary

This document analyzes the YAWL (Yet Another Workflow Language) Java marking implementation and compares it with CRE's `pnet_marking` Erlang module. Both implement multiset marking algebra for Petri Net-based workflow execution, but with different design approaches reflecting their respective languages and architectural philosophies.

## 1. YAWL Marking Data Structure

### 1.1 YMarking Class

**Location**: `/vendors/yawl/src/org/yawlfoundation/yawl/elements/state/YMarking.java`

**Primary Data Structure**:
```java
private List<YNetElement> _locations;
```

The YAWL marking uses a `List<YNetElement>` (implemented as `Vector<YNetElement>`) to track locations where tokens (represented by `YIdentifier` objects) reside. This is a **flat multiset representation** - each element in the list represents a token at a location.

**Key Characteristics**:
- **Flat multiset**: Tokens are represented directly as list entries
- **Location-based**: Each entry is a `YNetElement` (either `YCondition` or `YTask`)
- **Multiplicity through repetition**: Multiple tokens at same location = multiple list entries
- **Order-independent**: Equality checks ignore order (sorts before comparison)

### 1.2 YInternalCondition Class

**Location**: `/vendors/yawl/src/org/yawlfoundation/yawl/elements/state/YInternalCondition.java`

**Primary Data Structure**:
```java
private YIdentifierBag _bag;
```

`YInternalCondition` represents a condition (place) in the YAWL net and uses `YIdentifierBag` for token management.

### 1.3 YIdentifierBag Class

**Location**: `/vendors/yawl/src/org/yawlfoundation/yawl/util/YIdentifierBag.java`

**Primary Data Structure**:
```java
private Map<YIdentifier, Integer> _idToQtyMap = new HashMap<YIdentifier, Integer>();
```

This is a **multiset (bag) implementation** using:
- **Key**: `YIdentifier` - the token type
- **Value**: `Integer` - the count (multiplicity) of that identifier

**Key Insight**: YAWL uses two complementary representations:
1. `YMarking`: Flat list of locations for reachability analysis
2. `YIdentifierBag`: Frequency map for efficient token management at conditions

### 1.4 YIdentifier Class

**Location**: `/vendors/yawl/src/org/yawlfoundation/yawl/elements/state/YIdentifier.java`

`YIdentifier` represents a **case ID** (workflow instance) with:
```java
private List<YNetElement> _locations = new Vector<YNetElement>();  // Where this token is
private List<YIdentifier> _children = new Vector<YIdentifier>();    // Child identifiers
private YIdentifier _parent;                                        // Parent identifier
private String _idString;                                            // Unique ID string
```

This implements **hierarchical token structures** for nested workflow instances (composite tasks).

## 2. YAWL Add/Remove Operations

### 2.1 YIdentifierBag Operations

| Operation | Method | Signature | Behavior |
|-----------|--------|-----------|----------|
| **Add** | `addIdentifier` | `addIdentifier(YPersistenceManager pmgr, YIdentifier identifier)` | Increments count for identifier; adds location back-reference |
| **Remove One** | `remove` | `remove(pmgr, identifier, 1)` | Decrements count; removes entry if count reaches 0 |
| **Remove Amount** | `remove` | `remove(pmgr, identifier, amount)` | Decrements by amount; validates sufficient quantity |
| **Remove All** | `removeAll` | `removeAll(pmgr)` | Clears all identifiers from bag |
| **Contains** | `contains` | `contains(YIdentifier identifier)` | Checks if identifier exists in bag |
| **Get Amount** | `getAmount` | `getAmount(YIdentifier identifier)` | Returns count for specific identifier |

**Add Implementation**:
```java
public void addIdentifier(YPersistenceManager pmgr, YIdentifier identifier) {
    int amount = getAmount(identifier);
    _idToQtyMap.put(identifier, ++amount);
    identifier.addLocation(pmgr, _condition);
}
```

**Remove Implementation** (with validation):
```java
public void remove(YPersistenceManager pmgr, YIdentifier identifier, int amountToRemove) {
    if (_idToQtyMap.containsKey(identifier)) {
        int amountExisting = _idToQtyMap.get(identifier);
        if (amountToRemove > amountExisting) {
            throw new RuntimeException("Cannot remove " + amountToRemove +
                " tokens - only " + amountExisting + " available");
        }
        int amountLeft = amountExisting - amountToRemove;
        if (amountLeft > 0) {
            _idToQtyMap.put(identifier, amountLeft);
        } else {
            _idToQtyMap.remove(identifier);
        }
        identifier.removeLocation(pmgr, _condition);
    }
}
```

### 2.2 YInternalCondition Operations

`YInternalCondition` delegates to its `YIdentifierBag`:

```java
public void add(YPersistenceManager pmgr, YIdentifier identifier) {
    _bag.addIdentifier(pmgr, identifier);
}

public void removeOne(YPersistenceManager pmgr, YIdentifier identifier) {
    _bag.remove(pmgr, identifier, 1);
}

public void remove(YPersistenceManager pmgr, YIdentifier identifier, int amount) {
    _bag.remove(pmgr, identifier, amount);
}
```

### 2.3 YMarking Operations (Reachability)

`YMarking` operates on its flat list representation for **reachability analysis**:

```java
public YSetOfMarkings reachableInOneStep(YTask task, YTask orJoin) {
    // 1. Check if task enabled
    // 2. Remove task from locations (consume)
    // 3. Apply cancellation set (removeAll)
    // 4. Add postset based on split type (AND/OR/XOR)
    switch (task.getSplitType()) {
        case YTask._AND:
        case YTask._OR:
            // Add ALL postset elements to each marking
            marking._locations.addAll(postset);
            break;
        case YTask._XOR:
            // Create separate marking for EACH postset element
            for (YExternalNetElement element : postset) {
                YMarking aFinalMarking = new YMarking(halfbakedMarking.getLocations());
                aFinalMarking._locations.add((YCondition) element);
                finishedSet.addMarking(aFinalMarking);
            }
            break;
    }
}
```

## 3. CRE pnet_marking Module

**Location**: `/src/pnet/pnet_marking.erl`

### 3.1 Data Structure

```erlang
-type marking() :: #{place() => [token()]}.
```

CRE uses a **map-based multiset representation**:
- **Key**: `place()` (atom) - the place/condition identifier
- **Value**: `[token()]` (list) - list of tokens at that place

### 3.2 CRE Operations

| Operation | Function | Signature | Behavior |
|-----------|----------|-----------|----------|
| **Create** | `new/1` | `new([place()]) -> marking()` | Creates empty marking with places |
| **Get** | `get/2` | `get(marking(), place()) -> {ok, [token()]}` | Total function - returns {ok, []} for missing places |
| **Set** | `set/3` | `set(marking(), place(), [token()]) -> marking()` | Replaces tokens at place |
| **Add** | `add/2` | `add(marking(), produce_map()) -> marking()` | Multiset union (appends tokens) |
| **Take** | `take/2` | `take(marking(), consume_map()) -> {ok, marking()} \| {error, insufficient}` | Multiset subtraction with validation |
| **Apply** | `apply/2` | `apply(marking(), move()) -> {ok, marking()} \| error` | Atomic consume+produce |
| **Hash** | `hash/1` | `hash(marking()) -> binary()` | SHA-256 of canonical representation |

### 3.3 Multiset Algebra Implementation

**Add (Union)**:
```erlang
add(Marking, ProduceMap) ->
    maps:fold(fun
        (Place, NewTokens, Acc) ->
            ExistingTokens = maps:get(Place, Acc, []),
            Acc#{Place => ExistingTokens ++ NewTokens}
    end, Marking, ProduceMap).
```

**Take (Subtraction)**:
```erlang
take(Marking, ConsumeMap) ->
    take_fold(maps:to_list(ConsumeMap), Marking).

multiset_subtract(Available, Remove) ->
    AvailableCounts = count_tokens(Available),
    RemoveCounts = count_tokens(Remove),
    case can_consume(AvailableCounts, RemoveCounts) of
        true ->
            RemainingCounts = subtract_counts(AvailableCounts, RemoveCounts),
            {ok, expand_counts(RemainingCounts)};
        false ->
            {error, insufficient}
    end.
```

## 4. Comparison: YAWL vs CRE

### 4.1 Data Structure Comparison

| Aspect | YAWL (Java) | CRE (Erlang) |
|--------|-------------|--------------|
| **Primary Structure** | `List<YNetElement>` (flat) + `Map<YIdentifier,Integer>` (bag) | `#{place() => [token()]}` (map) |
| **Multiplicity Representation** | List repetition (flat) or Map count (bag) | List length (implicit multiset) |
| **Token Identity** | `YIdentifier` objects with hierarchy | Any Erlang term (flexible) |
| **Place Identity** | `YCondition`/`YTask` objects | Atoms (efficient) |
| **Memory Layout** | Object-based (heap allocated) | Immutable map (term storage) |
| **Concurrency Safety** | Requires external synchronization | Immutable by default |

### 4.2 Operation Comparison

| Operation | YAWL | CRE |
|-----------|------|-----|
| **Add Token** | `bag.addIdentifier(pmgr, id)` - increments count | `add(Marking, #{Place => [Token]})` - appends to list |
| **Remove Token** | `bag.remove(pmgr, id, amount)` - validates count | `take(Marking, #{Place => [Token]})` - validates existence |
| **Get Tokens** | `bag.getIdentifiers()` - expands counts to list | `get(Marking, Place)` - returns list directly |
| **Check Enabled** | `nonOrJoinEnabled(task)` - checks preset conditions | Implicit via `take` success/failure |
| **Error Handling** | Throws `RuntimeException` | Returns `{error, insufficient}` tuple |

### 4.3 Equality and Comparison

| Aspect | YAWL | CRE |
|--------|------|-----|
| **Equality** | Sorts both lists, compares element-wise | Structural equality (maps are comparable) |
| **Hashing** | Sum of element hashes modulo `Integer.MAX_VALUE` | SHA-256 of sorted canonical form |
| **Ordering** | `isBiggerThan`, `equivalentTo` for reachability | No ordering (not needed for execution) |

**YAWL Equality**:
```java
public boolean equivalentTo(YMarking marking) {
    Vector otherMarkingsLocations = new Vector(marking.getLocations());
    if (otherMarkingsLocations.size() != _locations.size()) return false;
    Vector thisMarkingsLocations = new Vector(_locations);
    Collections.sort(otherMarkingsLocations);
    Collections.sort(thisMarkingsLocations);
    return thisMarkingsLocations.equals(otherMarkingsLocations);
}
```

**CRE Hashing**:
```erlang
hash(Marking) ->
    Canonical = lists:sort([{Place, lists:sort(Tokens)}
                            || {Place, Tokens} <- maps:to_list(Marking)]),
    crypto:hash(sha256, term_to_binary(Canonical)).
```

### 4.4 Architectural Differences

| Aspect | YAWL | CRE |
|--------|------|-----|
| **Immutability** | Mutable (Vector) | Immutable (maps) |
| **Persistence** | Integrated via `YPersistenceManager` | External (application layer) |
| **Token Hierarchy** | Parent/child identifiers | Flat (application manages hierarchy) |
| **Reachability** | Built into `YMarking` | Separate concern |
| **Concurrency** | Requires external synchronization | Actor model (process isolation) |

### 4.5 Performance Characteristics

| Operation | YAWL Complexity | CRE Complexity |
|-----------|-----------------|----------------|
| **Add token** | O(1) hash map update | O(log n) map update + append |
| **Remove token** | O(1) hash map update | O(k * m) where k=places, m=avg tokens |
| **Get tokens at place** | O(m) expand counts to list | O(1) map lookup |
| **Equality check** | O(n log n) sort + compare | O(n log n) sort for hash only |
| **Enabled check** | O(k) preset size | O(k * m) consume validation |

## 5. Key Insights

### 5.1 Dual Representation Pattern

YAWL uses **two complementary marking representations**:

1. **YMarking (Flat List)**: Used for reachability analysis and state exploration
   - Advantages: Simple iteration, easy set operations
   - Disadvantages: No multiplicity tracking, requires linear search

2. **YIdentifierBag (Frequency Map)**: Used for token storage at conditions
   - Advantages: Efficient multiplicity tracking, O(1) add/remove
   - Disadvantages: Requires expansion for iteration

CRE uses a **single unified representation** (map of lists) that serves both purposes:
- Token lists naturally represent multisets
- Maps provide efficient place lookup
- Immutable semantics simplify reasoning

### 5.2 Persistence Integration

YAWL's marking operations **integrate persistence**:
```java
public void addIdentifier(YPersistenceManager pmgr, YIdentifier identifier) {
    _idToQtyMap.put(identifier, ++amount);
    identifier.addLocation(pmgr, _condition);  // Persistence side-effect
}
```

CRE's marking is **pure functional** - persistence is a separate concern.

### 5.3 Hierarchy Support

YAWL has **first-class support for hierarchical identifiers**:
- `YIdentifier` maintains parent/child relationships
- Enables composite task decomposition
- `createChild()` generates descendant identifiers

CRE treats hierarchy as **application-level** - tokens can contain any data including hierarchical references.

## 6. Recommendations for CRE

### 6.1 Potential Enhancements

1. **Frequency Map Optimization**: For places with many duplicate tokens, consider a `{place, #{token => count()}}` representation
2. **Bulk Operations**: Add `add_multi/2` and `take_multi/2` for batch operations
3. **Persistence Hooks**: Consider callback interface for state change notifications

### 6.2 Things to Keep

1. **Immutability**: Critical for Erlang/OTP correctness
2. **Total Functions**: `get/2` returning `{ok, []}` for missing places is excellent API design
3. **Explicit Error Handling**: Return tuples vs exceptions matches Erlang conventions
4. **Flexibility**: Arbitrary token terms vs fixed `YIdentifier` class

### 6.3 Mapping for YAWL Integration

When integrating YAWL patterns into CRE:

| YAWL Concept | CRE Equivalent |
|--------------|----------------|
| `YIdentifier` | Token term (e.g., `{case_id, SubId}`) |
| `YCondition` | Place atom (e.g., `p_active`) |
| `YInternalCondition` | Implicit (place with empty token list) |
| `YIdentifierBag` | Implicit (token list at place) |
| `YMarking._locations` | `pnet_marking:marking()` (places with tokens) |
| `YSetOfMarkings` | List of markings (nondeterminism) |

## 7. Conclusion

YAWL's marking implementation reflects **object-oriented design** with:
- Mutable state
- Dual representations for different use cases
- Integrated persistence
- Hierarchical token structures

CRE's marking reflects **functional design** with:
- Immutable data structures
- Single unified representation
- Separation of concerns (persistence external)
- Flexible token types

Both approaches are valid for their respective environments. The YAWL implementation provides useful patterns for:
- Frequency-based multiset representation
- Hierarchical token management
- Persistence integration

These can inform future CRE enhancements while maintaining its functional core.
