# Basic Control Patterns (P1–P9)

## P1 Sequence

Sequential execution of tasks.

```mermaid
flowchart LR
    p_start([p_start]) --> t_start[t_start]
    t_start --> p_task1((p_task1))
    p_task1 --> t_c1[t_complete1]
    t_c1 --> p_task2((p_task2))
    p_task2 --> t_c2[t_complete2]
    t_c2 --> p_task2
    p_task2 --> t_finish[t_finish]
    t_finish --> p_end([p_end])
```

**Module:** `sequence`

---

## P2 Parallel Split

AND-split into N concurrent branches.

```mermaid
flowchart TD
    p_start([p_start]) --> t_split[t_split]
    t_split --> p_b1((p_branch1))
    t_split --> p_b2((p_branch2))
    t_split --> p_b3((p_branch3))
    t_split --> p_b4((p_branch4))
    p_b1 --> t_j1[t_join_branch1]
    p_b2 --> t_j2[t_join_branch2]
    p_b3 --> t_j3[t_join_branch3]
    p_b4 --> t_j4[t_join_branch4]
    t_j1 --> p_ready((p_join_ready))
    t_j2 --> p_ready
    t_j3 --> p_ready
    t_j4 --> p_ready
    p_ready --> t_finish[t_finish]
    t_finish --> p_end([p_end])
```

**Module:** `parallel_split`

---

## P3 Synchronization

AND-join waiting for all branches.

```mermaid
flowchart TD
    p_b1((p_branch1))
    p_b2((p_branch2))
    p_b3((p_branch3))
    p_start([p_start]) --> t_split[t_split]
    t_split --> p_b1
    t_split --> p_b2
    t_split --> p_b3
    p_b1 --> t_join[t_join]
    p_b2 --> t_join
    p_b3 --> t_join
    t_join --> p_joined((p_joined))
    p_joined --> t_finish[t_finish]
    t_finish --> p_end([p_end])
```

**Module:** `synchronization`

---

## P4 Exclusive Choice

XOR-split: exactly one branch selected.

```mermaid
flowchart TD
    p_start([p_start]) --> t_start[t_start]
    t_start --> p_choice((p_choice))
    p_choice --> t_a{t_select_a}
    p_choice --> t_b{t_select_b}
    t_a --> p_selected((p_selected))
    t_b --> p_selected
    p_selected --> t_finish[t_finish]
    t_finish --> p_end([p_end])
```

**Module:** `exclusive_choice`

---

## P5 Simple Merge

Simple merge of two branches.

```mermaid
flowchart TD
    p_a((p_branch_a))
    p_b((p_branch_b))
    p_a --> t_ma[t_merge_a]
    p_b --> t_mb[t_merge_b]
    p_ready((p_merge_ready))
    p_ready --> t_ma
    p_ready --> t_mb
    t_ma --> p_merged((p_merged))
    t_mb --> p_merged
    p_merged --> t_finish[t_finish]
    t_finish --> p_end([p_end])
```

**Module:** `simple_merge`

---

## P6 Multiple Choice

OR-split: one or more branches selected.

```mermaid
flowchart TD
    p_start([p_start]) --> t_eval[t_eval]
    t_eval --> p_eval((p_eval))
    p_eval --> t_1[t_select_1]
    p_eval --> t_2[t_select_2]
    p_eval --> t_3[t_select_3]
    p_eval --> t_4[t_select_4]
    t_1 --> p_b1((p_branch1))
    t_2 --> p_b2((p_branch2))
    t_3 --> p_b3((p_branch3))
    t_4 --> p_b4((p_branch4))
```

**Module:** `multiple_choice`

---

## P7 Structured Sync Merge

Structured merge with synchronization.

```mermaid
flowchart TD
    p_b1((p_branch1))
    p_b2((p_branch2))
    p_b3((p_branch3))
    p_b4((p_branch4))
    p_b1 --> t_join[t_join]
    p_b2 --> t_join
    p_b3 --> t_join
    p_b4 --> t_join
    t_join --> p_joined((p_joined))
    p_joined --> t_finish[t_finish]
    t_finish --> p_end([p_end])
```

**Module:** `structured_sync_merge`

---

## P8 Multiple Merge

Multiple merge (fan-in from multiple sources).

```mermaid
flowchart TD
    p1((from1))
    p2((from2))
    p3((from3))
    p1 --> t_merge[t_merge]
    p2 --> t_merge
    p3 --> t_merge
    t_merge --> p_to((to))
```

**Module:** `multiple_merge`

---

## P9 Discriminator

First branch wins; others discarded.

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

**Module:** `discriminator`
