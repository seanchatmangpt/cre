# Miscellaneous Patterns (P28, P39, P40)

## P28 Blocking Discriminator

First branch wins; others block until join.

```mermaid
flowchart TD
    p_b1((p_branch1))
    p_b2((p_branch2))
    p_b3((p_branch3))
    p_b1 --> t_disc[t_discriminate]
    p_b2 --> t_disc
    p_b3 --> t_disc
    t_disc --> p_selected((p_selected))
```

**Module:** `blocking_discriminator`

---

## P39 Critical Section

Mutex-protected section.

```mermaid
flowchart TD
    p_start([p_start]) --> t_enter[t_enter]
    t_enter --> p_mutex((p_mutex))
    p_mutex --> t_protected[t_protected]
    t_protected --> t_exit[t_exit]
    t_exit --> p_end([p_end])
```

**Module:** `critical_section`

---

## P40 Interleaved Routing

Same as P17; interleaved execution.

```mermaid
flowchart TD
    p_start([p_start]) --> t_dist[t_distribute]
    t_dist --> p_pool((p_branch_pool))
    p_pool --> t_pick[t_pick_next]
    t_pick --> p_next((p_next_branch))
    p_next --> t_exec[t_execute]
    t_exec --> p_done((p_branch_done))
    p_done --> t_return[t_return]
    t_return --> p_pool
```

**Module:** `interleaved_routing`
