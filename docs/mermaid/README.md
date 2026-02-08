# CRE Mermaid Diagrams

Mermaid diagrams for all 43 YAWL workflow control patterns and related concepts.

## Index

| File | Content |
|------|---------|
| [01-patterns-overview](01-patterns-overview.md) | All 43 patterns index and categories |
| [02-patterns-basic](02-patterns-basic.md) | P1–P9 Basic Control Flow |
| [03-patterns-advanced](03-patterns-advanced.md) | P10–P21 Advanced Branching |
| [04-patterns-structural](04-patterns-structural.md) | P22–P25 Structural |
| [05-patterns-multi-instance](05-patterns-multi-instance.md) | P12–P15, P26–P27, P34–P36 |
| [06-patterns-sync](06-patterns-sync.md) | P30–P33, P37–P38 Synchronization |
| [07-patterns-cancellation](07-patterns-cancellation.md) | P19–P20, P25–P26, P29, P32, P35 |
| [08-patterns-thread](08-patterns-thread.md) | P41–P42 Thread Split/Merge |
| [09-patterns-triggers](09-patterns-triggers.md) | P23–P24 Triggers |
| [10-patterns-termination](10-patterns-termination.md) | P11, P43 Termination |
| [11-patterns-misc](11-patterns-misc.md) | P39 Critical Section, P40 Interleaved Routing |
| [12-execution-flow](12-execution-flow.md) | Omega demo loop, gen_yawl step, subnet execution |
| [13-case-lifecycle](13-case-lifecycle.md) | Case lifecycle state diagram |
| [14-omega-symposium](14-omega-symposium.md) | AGI Symposium Ω workflow |
| [GAP_ANALYSIS](GAP_ANALYSIS.md) | Gap analysis: diagrams vs implementation |

## Pattern Registry

All implementations live in `src/patterns/` and are registered in `src/core/yawl_pattern_registry.erl`.
