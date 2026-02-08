# Termination Patterns (P11, P43)

## P11 Implicit Termination

Workflow ends when no more enabled transitions (natural termination).

```mermaid
flowchart TD
    p_active((p_active)) --> |no more enabled| End([Terminated])
```

**Module:** `implicit_termination`

---

## P43 Explicit Termination

Hard-stop; terminator transition cancels all.

```mermaid
flowchart TD
    p_active((p_active)) --> t_term[t_terminator]
    t_term --> p_stopped((p_stopped))
```

**Module:** `explicit_termination`
