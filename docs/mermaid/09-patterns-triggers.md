# Trigger Patterns (P23–P24)

## P23 Transient Trigger

Event must occur *while* task is enabled. Event is transient—lost if not consumed immediately.

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

Event persists in pool until consumed by waiting task.

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
