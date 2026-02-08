# Cancellation Patterns (P19–P20, P25–P26, P29, P32, P35)

## P19 Cancel Activity

Cancel a single activity.

```mermaid
flowchart TD
    p_start([p_start]) --> t_start[t_start]
    t_start --> p_running((p_running))
    p_running --> p_cancel((p_cancel_signal))
    p_cancel --> t_cancel[t_cancel]
    t_cancel --> p_cancelled((p_cancelled))
    p_cancelled --> t_finish[t_finish]
    t_finish --> p_end([p_end])
```

**Module:** `cancel_activity`

---

## P20 Cancel Case

Cancel entire workflow case.

```mermaid
flowchart TD
    p_start([p_start]) --> p_active((p_active))
    p_cancel((p_cancel_event)) --> t_cancel[t_cancel_case]
    t_cancel --> p_cancelled((p_cancelled))
```

**Module:** `cancel_case`

---

## P25 Cancel Region

Cancel region (see [04-patterns-structural](04-patterns-structural.md)).

**Module:** `cancel_region`

---

## P26 Cancel MI Activity

Cancel multi-instance activity (see [05-patterns-multi-instance](05-patterns-multi-instance.md)).

**Module:** `cancel_mi_activity`

---

## P29 Cancelling Discriminator

First branch wins; cancel remaining.

```mermaid
flowchart TD
    p_b1((p_branch1))
    p_b2((p_branch2))
    p_b3((p_branch3))
    p_b1 --> t_disc[t_discriminate]
    p_b2 --> t_disc
    p_b3 --> t_disc
    t_disc --> p_selected((p_selected))
    t_disc -.->|cancel| p_b2
    t_disc -.->|cancel| p_b3
```

**Module:** `cancelling_discriminator`

---

## P32 Cancelling Partial Join

N-of-M join; cancel remaining (see [06-patterns-sync](06-patterns-sync.md)).

**Module:** `cancelling_partial_join`

---

## P35 Cancelling Partial Join MI

N-of-M for multi-instance; cancel remaining (see [05-patterns-multi-instance](05-patterns-multi-instance.md)).

**Module:** `cancelling_partial_join_mi`
