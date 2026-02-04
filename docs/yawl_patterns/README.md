# YAWL Workflow Patterns - Petri Net Diagrams

This directory contains Petri net diagrams for all 43 YAWL workflow patterns.

## Diagram Format

All diagrams are provided in Graphviz DOT format for easy rendering and modification.

### Rendering Diagrams

To render a diagram as PNG:
```bash
dot -Tpng wcp01_sequence.dot -o wcp01_sequence.png
```

To render all diagrams:
```bash
for f in *.dot; do dot -Tpng "$f" -o "${f%.dot}.png"; done
```

## Pattern Categories

### Basic Control Flow Patterns (WCP 1-6)
- `wcp01_sequence.dot` - Sequence
- `wcp02_parallel_split.dot` - Parallel Split
- `wcp03_synchronization.dot` - Synchronization
- `wcp04_exclusive_choice.dot` - Exclusive Choice
- `wcp05_simple_merge.dot` - Simple Merge
- `wcp06_multi_choice.dot` - Multi-Choice

### Advanced Synchronization Patterns (WCP 7-10)
- `wcp07_synchronizing_merge.dot` - Synchronizing Merge
- `wcp08_multi_merge.dot` - Multi-Merge
- `wcp09_discriminator.dot` - Discriminator
- `wcp10_arbitrary_cycles.dot` - Arbitrary Cycles

### Multiple Instance Patterns (WCP 11-17)
- `wcp11_implicit_termination.dot` - Implicit Termination
- `wcp12_multi_instances_no_sync.dot` - Multiple Instances without Synchronization
- `wcp13_multi_instances_static.dot` - Multiple Instances (Design Time Knowledge)
- `wcp14_multi_instances_runtime.dot` - Multiple Instances (Runtime Knowledge)
- `wcp15_multi_instances_dynamic.dot` - Multiple Instances (No Prior Knowledge)
- `wcp16_deferred_choice.dot` - Deferred Choice
- `wcp17_interleaved_routing.dot` - Interleaved Parallel Routing

### State-Based Patterns (WCP 18-20)
- `wcp18_milestone.dot` - Milestone
- `wcp19_cancel_activity.dot` - Cancel Activity
- `wcp20_cancel_case.dot` - Cancel Case

## Legend

- **Circle (p_*)**: Place - holds tokens (state)
- **Box (t_*)**: Transition - consumes/produces tokens (action)
- **Arrow**: Arc - connects places to transitions
- **Bold arrow**: Indicates main flow
- **Dashed arrow**: Indicates exception/cancellation flow

## Petri Net Semantics

- **Token**: Represents workflow state or data
- **Marking**: Distribution of tokens across places
- **Firing**: Transition executes when enabled (has required tokens)
- **Enabled**: Transition has all required input tokens

## CRE Integration

These patterns map to the `gen_pnet` behavior implementation in:
- `src/cre_yawl.erl` - Workflow definitions and validation
- `src/cre_yawl_patterns.erl` - Pattern implementations
- `src/cre_yawl_client.erl` - Workflow execution client
- `src/cre_yawl_worker.erl` - Task execution worker

## References

1. van der Aalst, W. M. P., ter Hofstede, A. H. M., Kiepuszewski, B., & Barros, A. P. (2003). Workflow patterns. *Distributed and Parallel Databases*, 14(1), 5-51.

2. Russell, N., ter Hofstede, A. H. M., van der Aalst, W. M. P., & Mulyar, N. (2006). *Workflow control-flow patterns: A revised view*. BPM Center Report.

3. Workflow Patterns Initiative: https://www.workflowpatterns.com
