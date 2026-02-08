# Multi-Instance Patterns (P12–P15, P26–P27, P34–P36)

## P12–P15 Multi-Instance (No Sync / Design Time / Runtime Known / Unknown)

```mermaid
flowchart TD
    p_start([p_start]) --> t_spawn[t_spawn]
    t_spawn --> p_pool((p_instance_pool))
    p_pool --> t_exec[t_execute]
    t_exec --> p_running((p_running))
    p_running --> p_done((p_done))
    p_done --> t_complete[t_complete]
```

**Module:** `multiple_instances_sync`

---

## P26 Cancel MI Activity

Cancel one or more multi-instance activities.

```mermaid
flowchart TD
    p_active((p_active)) --> t_cancel[t_cancel]
    t_cancel --> p_cancelled((p_cancelled))
```

**Module:** `cancel_mi_activity`

---

## P27 Complete MI Activity

Early completion of multi-instance (N-of-M done).

```mermaid
flowchart TD
    p_running((p_running)) --> t_complete[t_complete]
    t_complete --> p_done((p_done))
```

**Module:** `complete_mi_activity`

---

## P34 Static Partial Join MI

Static N-of-M join for multi-instance.

```mermaid
flowchart TD
    p_b1((p_branch1))
    p_b2((p_branch2))
    p_b3((p_branch3))
    p_b1 --> t_join[t_partial_join]
    p_b2 --> t_join
    p_b3 --> t_join
    t_join --> p_output((p_output))
```

**Module:** `static_partial_join_mi`

---

## P35 Cancelling Partial Join MI

N-of-M join; remaining cancelled.

```mermaid
flowchart TD
    p_branches((branches)) --> t_join[t_partial_join]
    t_join --> p_output((p_output))
    t_join -.->|cancel| p_remaining
```

**Module:** `cancelling_partial_join_mi`

---

## P36 Dynamic Partial Join MI

Dynamic N-of-M threshold (e.g. from variable).

```mermaid
flowchart TD
    p_branches((branches)) --> t_join[t_dynamic_join]
    t_join --> p_output((p_output))
```

**Module:** `dynamic_partial_join_mi`
