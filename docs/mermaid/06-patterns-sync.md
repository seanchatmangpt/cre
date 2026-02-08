# Synchronization Patterns (P30–P33, P37–P38)

## P30 Structured Partial Join

N-of-M join; fixed threshold.

```mermaid
flowchart TD
    p_b1((p_branch1))
    p_b2((p_branch2))
    p_b3((p_branch3))
    p_b1 --> t_join[t_join]
    p_b2 --> t_join
    p_b3 --> t_join
    t_join --> p_partial((p_partial))
```

**Module:** `structured_partial_join`

---

## P31 Blocking Partial Join

N-of-M; internal output early, final when all complete.

```mermaid
flowchart TD
    p_branches((branches)) --> t_partial[t_partial_join]
    t_partial --> p_internal((p_internal))
    p_branches --> t_final[t_final_join]
    t_final --> p_final((p_final))
```

**Module:** `blocking_partial_join`

---

## P32 Cancelling Partial Join

N-of-M; remaining branches cancelled.

```mermaid
flowchart TD
    p_branches((branches)) --> t_join[t_partial_join]
    t_join --> p_output((p_output))
    t_join -.->|cancel| p_remaining
```

**Module:** `cancelling_partial_join`

---

## P33 Generalized AND Join

Rejoin across active branches (arbitrary cycles).

```mermaid
flowchart TD
    p_b1((p_branch1))
    p_b2((p_branch2))
    p_b3((p_branch3))
    p_b1 --> t_join[t_join]
    p_b2 --> t_join
    p_b3 --> t_join
    t_join --> p_joined((p_joined))
```

**Module:** `generalized_and_join`

---

## P37 Local Sync Merge

Per-scope merge (e.g. per-track).

```mermaid
flowchart TD
    p_local1((p_local1))
    p_local2((p_local2))
    p_local3((p_local3))
    p_local1 --> t_join[t_join]
    p_local2 --> t_join
    p_local3 --> t_join
    t_join --> p_joined((p_joined))
```

**Module:** `local_sync_merge`

---

## P38 General Sync Merge

Merge across unstructured active branches.

```mermaid
flowchart TD
    p_b1((p_branch1))
    p_b2((p_branch2))
    p_b3((p_branch3))
    p_b1 --> t_join[t_join]
    p_b2 --> t_join
    p_b3 --> t_join
    t_join --> p_joined((p_joined))
```

**Module:** `general_sync_merge`
