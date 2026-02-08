# Traceability Matrix: AGI Symposium Omega Workflow

This document provides end-to-end traceability from Business Concept Definitions (BCD) through pattern instances, Erlang modules, and implementation code for the AGI Symposium Omega workflow.

## Overview

The AGI Symposium Omega workflow demonstrates a complete implementation of all 43 YAWL workflow control patterns. This matrix maps:

1. **BCD -> Pattern Instance**: Business requirements to YAML pattern definitions
2. **Pattern Instance -> Erlang Module**: Pattern definitions to implementation modules
3. **Module -> Function**: Key implementing functions
4. **Function -> Receipt Field**: Execution proof data

## Traceability Matrix

| Requirement (BCD) | Pattern Instance | YAML Location | Erlang Module | Key Functions | Receipt Fields |
|------------------|------------------|--------------|---------------|---------------|----------------|
| **Parallel Workstreams** | | | | | |
| Symposium must execute program, operations, and communications workstreams in parallel | `P42_split_megathreads` | `agi_symposium_omega.yaml:160-162` | `thread_split.erl` | `fire/3`, `place_lst/0`, `trsn_lst/0` | `trsn: t_split`, `produce: #{p_thread1 => [token], ...}` |
| Symposium must wait for all workstreams before Go/No-Go decision | `P41_merge_megathreads` | `agi_symposium_omega.yaml:165-166` | `thread_merge.erl` | `fire/3`, `is_enabled/3` | `trsn: t_merge`, `produce: #{p_merged => [merged]}` |
| **Paper Lifecycle** | | | | | |
| Papers must progress through CFP -> submission -> review -> decision | `P1_seq_paper_lifecycle` | `agi_symposium_omega.yaml:179-181` | `sequence.erl` | `fire/3`, `preset/1` | `trsn: t_start/t_complete1/...`, `produce: #{p_task1 => [token]}` |
| Desk triage must split into ethics, artifact, and topical lanes | `P2_parallel_triage_lanes` | `agi_symposium_omega.yaml:169-171` | `parallel_split.erl` | `fire/3`, `place_lst/0` | `trsn: t_split`, `produce: #{p_branch1 => [token], ...}` |
| Go/No-Go must wait for program, ops, and comms completion | `P3_sync_gonogo` | `agi_symposium_omega.yaml:174-176` | `synchronization.erl` | `is_enabled/3`, `fire/3` | `trsn: t_join`, `mode: #{p_branch1 => ..., p_branch2 => ...}` |
| **Decision Points** | | | | | |
| Paper decision must choose: accept, reject, or revise | `P4_choice_decision` | `agi_symposium_omega.yaml:184-186` | `exclusive_choice.erl` | `fire/3`, `is_enabled/3` | `trsn: t_select_a/t_select_b`, `mode: #{p_choice => [token]}` |
| Invited and accepted talks converge without waiting | `P5_simple_merge_talk_types` | `agi_symposium_omega.yaml:210-212` | `simple_merge.erl` | `fire/3`, `preset/1` | `trsn: t_finish`, `produce: #{p_output => [token]}` |
| One submission can be assigned to multiple tracks | `P6_multi_track_assign` | `agi_symposium_omega.yaml:200-202` | `multiple_choice.erl` | `fire/3`, `place_lst/0` | `trsn: t_start`, `produce: #{p_branch1 => [token], ...}` |
| Only activated track branches must join for review | `P7_struct_sync_merge_tracks` | `agi_symposium_omega.yaml:205-207` | `structured_sync_merge.erl` | `is_enabled/3`, `fire/3` | `trsn: t_join`, `mode: #{p_active => [token]}` |
| Multiple independent updates drive dashboard | `P8_multiple_merge_updates` | `agi_symposium_omega.yaml:300-302` | `multiple_merge.erl` | `fire/3`, `preset/1` | `trsn: t_forward`, `produce: #{p_output => [token]}` |
| First confirmed keynote locks slot (others ignored) | `P9_discriminator_keynote` | `agi_symposium_omega.yaml:290-292` | `discriminator.erl` | `fire/3`, `is_enabled/3` | `trsn: t_output`, `produce: #{p_winner => [token]}` |
| **Exception Handling** | | | | | |
| Send paper back anywhere on ethics/plagiarism exceptions | `P10_arbitrary_rework` | `agi_symposium_omega.yaml:195-197` | `arbitrary_cycles.erl` | `fire/3`, `preset/1` | `trsn: t_rework`, `produce: #{p_node => [token]}` |
| Micro-workshops end naturally when empty | `P11_implicit_termination_microevents` | `agi_symposium_omega.yaml:345-347` | `implicit_termination.erl` | `is_enabled/3` | `trsn: (none)`, `quiescent: true` |
| **Multiple Instances** | | | | | |
| CFP broadcast to 20 channels, no blocking on stragglers | `P12_mi_cfp_blast` | `agi_symposium_omega.yaml:215-217` | `multiple_instances_sync.erl` | `fire/3` | `trsn: t_spawn`, `produce: #{p_instances => [{id, token}]}` |
| Default 3 reviewers per paper (design-time) | `P13_mi_reviews3` | `agi_symposium_omega.yaml:220-222` | `multiple_instances_sync.erl` | `fire/3` | `trsn: t_create_instances`, `instances: 3` |
| High-risk papers get N reviewers (runtime-known) | `P14_mi_reviewsN_risk` | `agi_symposium_omega.yaml:225-227` | `multiple_instances_sync.erl` | `fire/3` | `trsn: t_create_instances`, `instances_expr: "risk_to_N"` |
| Fact checks spawn until confidence threshold met | `P15_mi_factcheck_until` | `agi_symposium_omega.yaml:230-232` | `multiple_instances_sync.erl` | `fire/3`, `is_enabled/3` | `trsn: t_spawn_until`, `stop_condition: "confidence>=threshold"` |
| Partial joins for multiple instances | | | | | |
| Proceed after 3 of 5 reviews (static partial join) | `P34_static_partial_join_mi` | `agi_symposium_omega.yaml:235-237` | `static_partial_join_mi.erl` | `is_enabled/3`, `fire/3` | `threshold: 3`, `total_instances: 5` |
| Cancel remaining reviewers after threshold met | `P35_cancel_partial_join_mi` | `agi_symposium_omega.yaml:240-242` | `cancelling_partial_join_mi.erl` | `fire/3` | `cancel_remaining: true`, `threshold: 3` |
| Feedback threshold from attendance (dynamic) | `P36_dynamic_partial_join_feedback` | `agi_symposium_omega.yaml:340-342` | `dynamic_partial_join_mi.erl` | `is_enabled/3` | `threshold_expr: "ceil(attendance_estimate*0.08)"` |
| **Routing Choices** | | | | | |
| Workshop slot assigned by first confirmation | `P16_deferred_workshop_slot` | `agi_symposium_omega.yaml:255-257` | `deferred_choice.erl` | `fire/3`, `is_enabled/3` | `trsn: t_first_wins`, `events: [sponsor_slot_confirmed, ...]` |
| Speaker onboarding fields one-at-a-time | `P17_interleaved_onboarding` | `agi_symposium_omega.yaml:260-262` | `interleaved_routing.erl` | `fire/3`, `is_enabled/3` | `trsn: t_next`, `tasks: [bio, headshot, ...]` |
| Early-bird registration ends at deadline | `P18_milestone_earlybird` | `agi_symposium_omega.yaml:270-272` | `milestone.erl` | `is_enabled/3` | `gate_task: Registration`, `milestone: earlybird_closed` |
| Policy checklist must complete all once | `P40_interleaved_policy_checklist` | `agi_symposium_omega.yaml:265-267` | `interleaved_routing.erl` | `fire/3`, `is_enabled/3` | `tasks: [policy_coc, policy_accessibility, ...]` |
| **Triggers** | | | | | |
| Fire alarm only matters while symposium is live | `P23_transient_fire_alarm` | `agi_symposium_omega.yaml:275-277` | `transient_trigger.erl` | `is_enabled/3` | `event: fire_alarm`, `enabled_only_in: OpenDoors` |
| Visa approvals persist until consumed | `P24_persistent_visa` | `agi_symposium_omega.yaml:280-282` | `persistent_trigger.erl` | `fire/3` | `event: visa_approved`, `consumed_in: VisaLetters` |
| **Cancellation** | | | | | |
| First venue contract wins, cancel others | `P29_cancel_discriminator_venue` | `agi_symposium_omega.yaml:285-287` | `cancelling_discriminator.erl` | `fire/3` | `race: [venue_a, venue_b, venue_c]`, `cancel_rest: true` |
| Demo zone cancellation only (not entire symposium) | `P25_cancel_region_highrisk` | `agi_symposium_omega.yaml:355-357` | `cancel_region.erl` | `fire/3` | `region: Region_HighRisk_Demos`, `cancel_event: safety_red_flag` |
| Withdraw paper cancels all pending reviews | `P26_cancel_mi_on_withdraw` | `agi_symposium_omega.yaml:250-252` | `cancel_mi_activity.erl` | `fire/3` | `mi_task: ReviewCycle`, `cancel_event: paper_withdrawn` |
| Early complete if confidence achieved | `P27_complete_mi_early` | `agi_symposium_omega.yaml:245-247` | `complete_mi_activity.erl` | `is_enabled/3` | `mi_task: ReviewCycle`, `complete_condition: "reviews>=3 && confidence>=threshold"` |
| Cancel single talk prep if speaker drops | `P19_cancel_activity_speaker_drop` | `agi_symposium_omega.yaml:360-362` | `cancel_activity.erl` | `fire/3` | `target: talk_slot_prep`, `cancel_event: speaker_withdraws` |
| Force majeure cancels entire symposium | `P20_cancel_case_force_majeure` | `agi_symposium_omega.yaml:365-367` | `cancel_case.erl` | `fire/3` | `cancel_event: EmergencyShutdown` |
| Emergency hard-stop overrides everything | `P43_explicit_termination_emergency` | `agi_symposium_omega.yaml:370-372` | `explicit_termination.erl` | `fire/3` | `terminator: EmergencyShutdown`, `cancels_all: true` |
| **Joins and Merges** | | | | | |
| Incident blocks final comms until handled | `P28_blocking_discriminator_incident` | `agi_symposium_omega.yaml:295-297` | `blocking_discriminator.erl` | `is_enabled/3`, `fire/3` | `trigger: IncidentIntake`, `blocks_until: [IncidentResponse, ...]` |
| Per-track wrap-up joins only local sessions | `P37_local_sync_merge_trackwrap` | `agi_symposium_omega.yaml:305-307` | `local_sync_merge.erl` | `is_enabled/3`, `fire/3` | `scope: track_local`, `join: track_wrapup` |
| Global merge across unstructured live branches | `P38_general_sync_merge_close` | `agi_symposium_omega.yaml:310-312` | `general_sync_merge.erl` | `is_enabled/3`, `fire/3` | `join: CloseSymposium` |
| Main hall scheduling mutex | `P39_critical_section_mainhall` | `agi_symposium_omega.yaml:315-317` | `critical_section.erl` | `is_enabled/3`, `fire/3` | `mutex: MainHallMutex`, `protected: main_hall_schedule` |
| 2 of 3 chairs approve internal schedule | `P30_struct_partial_join_schedule` | `agi_symposium_omega.yaml:320-322` | `structured_partial_join.erl` | `is_enabled/3`, `fire/3` | `m: 3`, `n: 2`, `join: internal_schedule_ready` |
| Internal vs public schedule gate | `P31_blocking_partial_join_public` | `agi_symposium_omega.yaml:325-327` | `blocking_partial_join.erl` | `is_enabled/3`, `fire/3` | `partial_out: internal_schedule_ready`, `final_out: public_schedule_published` |
| Demo safety 2-of-3 cancels remaining | `P32_cancel_partial_join_demo_safety` | `agi_symposium_omega.yaml:330-332` | `cancelling_partial_join.erl` | `fire/3` | `m: 3`, `n: 2`, `cancel_remaining: true` |
| Rework paths rejoin across active branches | `P33_generalized_and_join_rework` | `agi_symposium_omega.yaml:335-337` | `generalized_and_join.erl` | `is_enabled/3`, `fire/3` | `join: rework_join` |
| **Loops and Recursion** | | | | | |
| Rebuttal/review until accept or reject | `P21_loop_revise` | `agi_symposium_omega.yaml:189-192` | `structured_loop.erl` | `is_enabled/3`, `fire/3` | `entry: AuthorRebuttal`, `body: [ReviewCycle, ...]`, `exit_condition: "accept||reject"` |
| Satellite symposium runs same pipeline | `P22_recursion_satellite` | `agi_symposium_omega.yaml:350-352` | `recursion.erl` | `fire/3` | `call: SatelliteSymposium` |

## File Location Index

| Component | File Path |
|-----------|-----------|
| YAML Specification | `/Users/sac/cre/test/fixtures/agi_symposium_omega.yaml` |
| YAML Parser | `/Users/sac/cre/src/wf/wf_yaml_spec.erl` |
| Pattern Registry | `/Users/sac/cre/src/core/yawl_pattern_registry.erl` |
| Pattern Expander | `/Users/sac/cre/src/core/yawl_pattern_expander.erl` |
| Workflow Compiler | `/Users/sac/cre/src/core/yawl_compile.erl` |
| Workflow Executor | `/Users/sac/cre/src/wf/wf_yawl_executor.erl` |
| Runtime Engine | `/Users/sac/cre/src/core/gen_yawl.erl` |
| Pattern Modules | `/Users/sac/cre/src/patterns/*.erl` |

## Receipt Field Reference

Receipts are the proof of execution generated by the workflow engine. Each transition firing produces a receipt with the following structure:

```erlang
#{
    trsn => atom(),              %% Transition that fired (e.g., t_split, t_merge)
    mode => #{atom() => [term()]}, %% Tokens consumed (from preset places)
    produce => #{atom() => [term()]} %% Tokens produced (to postset places)
}
```

### Common Receipt Fields by Pattern

| Pattern | Trsn Field | Produce Field Pattern |
|---------|------------|---------------------|
| P1 Sequence | `t_start`, `t_complete1`, `t_complete2`, `t_finish` | `#{p_task1 => [token]}`, `#{p_task2 => [done]}`, `#{p_end => [done]}` |
| P2 Parallel Split | `t_split` | `#{p_branch1 => [token]}, #{p_branch2 => [token]}, ...` |
| P3 Synchronization | `t_join` | `#{p_joined => [tokens]}` |
| P4 Exclusive Choice | `t_select_a`, `t_select_b` | `#{p_choice_a => [token]}` or `#{p_choice_b => [token]}` |
| P42 Thread Split | `t_split` | `#{p_thread1 => [token]}, #{p_thread2 => [token]}, ...` |
| P41 Thread Merge | `t_merge` | `#{p_merged => [merged]}` |
| P20 Cancel Case | `t_cancel` | `#{p_cancelled => [case_id]}` |
| P43 Explicit Termination | `t_terminate` | `#{p_terminated => [emergency]}` |

## Tracing a Receipt to Originating Requirement

To trace a receipt back to its originating requirement:

1. **Extract the `trsn` field** from the receipt
2. **Find the transition name** in the receipt (e.g., `t_split`)
3. **Locate the pattern instance** that uses this transition (via `yawl_pattern_expander:build_transition_mapping/2`)
4. **Map to the pattern ID** (e.g., `P42_ThreadSplit` via `yawl_pattern_registry:pattern_module/1`)
5. **Find the pattern instance** in the YAML (e.g., `P42_split_megathreads`)
6. **Read the `label` field** to understand the business requirement

### Example Trace

Given this receipt:
```erlang
#{trsn => t_SplitMegaThreads, mode => #{p_Start => [token]}, produce => #{ProgramThread => [token], OpsThread => [token], CommsThread => [token]}}
```

1. **trsn**: `t_SplitMegaThreads` (renamed from `t_split` via `build_transition_mapping/2`)
2. **Pattern instance**: `P42_split_megathreads` (from `split_task: SplitMegaThreads`)
3. **Pattern module**: `thread_split` (via `yawl_pattern_registry:pattern_module(<<"P42_ThreadSplit">>)`)
4. **Label**: "P42 Thread Split: Program/Ops/Comms/Incident"
5. **Business requirement**: "Symposium must execute program, operations, and communications workstreams in parallel"

### Reverse Trace (Requirement to Code)

To trace from requirement to implementation:

1. **Start with business requirement**: "Symposium must execute workstreams in parallel"
2. **Find in YAML**: `P42_split_megathreads` at line 160-162
3. **Get pattern**: `P42_ThreadSplit`
4. **Get module**: `thread_split.erl` (via registry)
5. **Key functions**: `fire/3` (line 46-62), `place_lst/0` (line 29-30), `trsn_lst/0` (line 32-33)
6. **Receipt proof**: `trsn: t_split`, `produce: #{p_thread1 => [token], p_thread2 => [token], ...}`

## Pattern Module Quick Reference

| Pattern ID | Module | File | Key Places | Key Transitions |
|------------|--------|------|------------|-----------------|
| P1 | `sequence` | `sequence.erl` | `p_start`, `p_task1`, `p_task2`, `p_end` | `t_start`, `t_complete1`, `t_complete2`, `t_finish` |
| P2 | `parallel_split` | `parallel_split.erl` | `p_start`, `p_branch1..4`, `p_end` | `t_split`, `t_finish1..4` |
| P3 | `synchronization` | `synchronization.erl` | `p_start`, `p_branch1..3`, `p_joined`, `p_end` | `t_split`, `t_complete1..3`, `t_join`, `t_finish` |
| P4 | `exclusive_choice` | `exclusive_choice.erl` | `p_start`, `p_choice_a`, `p_choice_b`, `p_end` | `t_finish`, `t_select_a`, `t_select_b` |
| P5 | `simple_merge` | `simple_merge.erl` | `p_branch_a`, `p_branch_b`, `p_output` | `t_finish` |
| P6 | `multiple_choice` | `multiple_choice.erl` | `p_start`, `p_branch1..4`, `p_end` | `t_start`, `t_finish1..4` |
| P7 | `structured_sync_merge` | `structured_sync_merge.erl` | `p_branch1..4`, `p_joined`, `p_end` | `t_join`, `t_finish` |
| P8 | `multiple_merge` | `multiple_merge.erl` | `p_input1..4`, `p_output` | `t_forward` |
| P9 | `discriminator` | `discriminator.erl` | `p_branch_pool`, `p_winner`, `p_end` | `t_trigger`, `t_output` |
| P10 | `arbitrary_cycles` | `arbitrary_cycles.erl` | `p_start`, `p_node1..4`, `p_end` | `t_forward`, `t_rework` |
| P11 | `implicit_termination` | `implicit_termination.erl` | `p_start`, `p_active`, `p_end` | (quiescent detection) |
| P12-P15 | `multiple_instances_sync` | `multiple_instances_sync.erl` | `p_start`, `p_instances`, `p_end` | `t_spawn`, `t_complete` |
| P16 | `deferred_choice` | `deferred_choice.erl` | `p_events`, `p_selected`, `p_end` | `t_first_wins` |
| P17 | `interleaved_routing` | `interleaved_routing.erl` | `p_start`, `p_task1..5`, `p_end` | `t_next` |
| P18 | `milestone` | `milestone.erl` | `p_before`, `p_after`, `p_end` | `t_check_milestone`, `t_pass`, `t_fail` |
| P19 | `cancel_activity` | `cancel_activity.erl` | `p_active`, `p_cancelled`, `p_end` | `t_start`, `t_cancel` |
| P20 | `cancel_case` | `cancel_case.erl` | `p_active`, `p_cancelled` | `t_cancel` |
| P21 | `structured_loop` | `structured_loop.erl` | `p_start`, `p_body_ready`, `p_body`, `p_end` | `t_start`, `t_enter_body`, `t_exit_loop` |
| P22 | `recursion` | `recursion.erl` | `p_call_ready`, `p_return`, `p_end` | `t_call`, `t_return` |
| P23 | `transient_trigger` | `transient_trigger.erl` | `p_enabled`, `p_event`, `p_end` | `t_event`, `t_consume` |
| P24 | `persistent_trigger` | `persistent_trigger.erl` | `p_event_pending`, `p_consume_ready`, `p_end` | `t_store`, `t_consume` |
| P25 | `cancel_region` | `cancel_region.erl` | `p_region_active`, `p_region_cancelled` | `t_cancel_region` |
| P26 | `cancel_mi_activity` | `cancel_mi_activity.erl` | `p_instances`, `p_cancelled` | `t_create_instances`, `t_cancel_all` |
| P27 | `complete_mi_activity` | `complete_mi_activity.erl` | `p_instances`, `p_complete` | `t_create_instances`, `t_complete_early` |
| P28 | `blocking_discriminator` | `blocking_discriminator.erl` | `p_trigger`, `p_blocked`, `p_cleared`, `p_end` | `t_trigger`, `t_clear` |
| P29 | `cancelling_discriminator` | `cancelling_discriminator.erl` | `p_race1..3`, `p_winner`, `p_cancelled` | `t_trigger`, `t_cancel_others` |
| P30 | `structured_partial_join` | `structured_partial_join.erl` | `p_branch1..3`, `p_threshold`, `p_end` | `t_partial_join` |
| P31 | `blocking_partial_join` | `blocking_partial_join.erl` | `p_branch1..3`, `p_partial`, `p_final`, `p_end` | `t_partial`, `t_final` |
| P32 | `cancelling_partial_join` | `cancelling_partial_join.erl` | `p_branch1..3`, `p_threshold`, `p_cancelled` | `t_partial_join`, `t_cancel_remaining` |
| P33 | `generalized_and_join` | `generalized_and_join.erl` | `p_dynamic`, `p_joined`, `p_end` | `t_join` |
| P34 | `static_partial_join_mi` | `static_partial_join_mi.erl` | `p_instances`, `p_partial`, `p_end` | `t_check_threshold` |
| P35 | `cancelling_partial_join_mi` | `cancelling_partial_join_mi.erl` | `p_instances`, `p_partial`, `p_cancelled` | `t_check_threshold`, `t_cancel_remaining` |
| P36 | `dynamic_partial_join_mi` | `dynamic_partial_join_mi.erl` | `p_instances`, `p_threshold`, `p_end` | `t_evaluate_threshold` |
| P37 | `local_sync_merge` | `local_sync_merge.erl` | `p_local1..3`, `p_joined`, `p_end` | `t_join` |
| P38 | `general_sync_merge` | `general_sync_merge.erl` | `p_branch1..3`, `p_joined`, `p_end` | `t_join`, `t_finish` |
| P39 | `critical_section` | `critical_section.erl` | `p_lock_request`, `p_critical`, `p_lock_free`, `p_end` | `t_acquire`, `t_release` |
| P40 | `interleaved_routing` | `interleaved_routing.erl` | `p_start`, `p_task1..4`, `p_end` | `t_next` |
| P41 | `thread_merge` | `thread_merge.erl` | `p_start`, `p_thread1..4`, `p_merged`, `p_end` | `t_split`, `t_complete1..4`, `t_merge`, `t_finish` |
| P42 | `thread_split` | `thread_split.erl` | `p_start`, `p_thread1..4`, `p_end` | `t_split`, `t_finish1..4` |
| P43 | `explicit_termination` | `explicit_termination.erl` | `p_active`, `p_terminated` | `t_terminate` |

## Usage in Testing

The traceability matrix is used in `/Users/sac/cre/test/agi_symposium_omega_SUITE.erl` to verify:

1. **Pattern Coverage**: All 43 patterns are present (`test_pattern_registry_coverage/1`)
2. **Pattern Instances**: Each pattern has at least one instance (`test_parse_pattern_instances/1`)
3. **Compilation**: YAML compiles to valid Erlang modules (`test_compile_spec/1`)
4. **Execution**: All patterns execute and produce receipts (`test_symposium_execution/1`)

## References

- YAWL Specification: van der Aalst & ter Hofstede (2005)
- YAWL Pattern Catalog: Russell et al. (2006)
- gen_pnet: CRE extended Petri net behavior
- gen_yawl: YAWL wrapper with 3-tuple fire/3 support

---

*Generated: 2026-02-07*
*Workflow: AGI Symposium Omega*
*Specification: agi_symposium_omega.yaml*
