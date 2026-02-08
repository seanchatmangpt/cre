# Structural Patterns (P22–P25)

## P22 Recursion

Subnet invocation; same net called recursively.

```mermaid
flowchart TD
    p_start([p_start]) --> t_call[t_call]
    t_call --> subnet[Subnet / Recursive Call]
    subnet --> t_return[t_return]
    t_return --> p_end([p_end])
```

**Module:** `recursion`

---

## P23 Transient Trigger

Event must occur while task enabled; transient.

```mermaid
flowchart TD
    p_start([p_start]) --> t_enable[t_enable]
    t_enable --> p_enabled((p_enabled))
    p_event((p_event)) --> t_event[t_event]
    p_enabled --> t_trigger[t_trigger]
    t_event --> t_trigger
    t_trigger --> p_triggered((p_triggered))
    p_triggered --> t_complete[t_complete]
    t_complete --> p_end([p_end])
```

**Module:** `transient_trigger`

---

## P24 Persistent Trigger

Event persists in pool until consumed.

```mermaid
flowchart TD
    p_start([p_start]) --> t_arrive[t_event_arrives]
    t_arrive --> p_pool((p_event_pool))
    p_pool --> t_consume[t_consume]
    t_consume --> p_consumed((p_consumed))
    p_consumed --> t_complete[t_complete]
    t_complete --> p_end([p_end])
```

**Module:** `persistent_trigger`

---

## P25 Cancel Region

Cancel activities within a region.

```mermaid
flowchart TD
    p_start([p_start]) --> t_start[t_start]
    t_start --> p_active((p_region_active))
    p_active --> p_cancel((p_cancel_event))
    p_cancel --> t_cancel[t_cancel_region]
    t_cancel --> p_cancelled((p_region_cancelled))
    p_cancelled --> t_finish[t_finish]
    t_finish --> p_end([p_end])
```

**Module:** `cancel_region`
