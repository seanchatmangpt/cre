# Advanced Branching Patterns (P10–P21)

## P10 Arbitrary Cycles

Unrestricted backward flow; cycles allowed.

```mermaid
flowchart TD
    p_start([p_start]) --> p_nodes[DeskTriage, AssignTracks, ReviewCycle, MetaReview]
    p_nodes -->|cycle back| p_nodes
```

**Module:** `arbitrary_cycles`

---

## P16 Deferred Choice

Runtime choice; external event selects branch.

```mermaid
flowchart TD
    p_start([p_start]) --> t_offer[t_offer]
    t_offer --> p_pool((p_option_pool))
    p_pool --> t_consume[t_consume]
    t_consume --> p_selected((p_selected))
```

**Module:** `deferred_choice`

---

## P17 Interleaved Parallel Routing

Interleaved execution of branches.

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

---

## P18 Milestone

Activity enabled only when milestone reached.

```mermaid
flowchart TD
    p_start([p_start]) --> t_start[t_start]
    t_start --> p_guard((p_milestone_guard))
    p_guard --> p_activity((p_activity))
    p_activity --> t_complete[t_complete]
    t_complete --> p_end([p_end])
```

**Module:** `milestone`

---

## P21 Structured Loop

Structured loop with entry/body/exit.

```mermaid
flowchart TD
    p_start([p_start]) --> t_start[t_start]
    t_start --> p_check((p_check))
    p_check -->|loop| p_body((p_body))
    p_body --> p_check
    p_check -->|exit| p_end([p_end])
```

**Module:** `structured_loop`
