%% -*- erlang -*-
%% @doc Pattern Registry - maps pattern macros to gen_yawl module implementations.
%%
%% **Joe Armstrong Design: Pure Helper Module (Stateless)**
%%
%% This module provides pure functional lookups for pattern mappings.
%% No state is maintained - all functions are stateless transformations.
%%
%% Provides mapping from YAML pattern names (P1_Sequence, P2_ParallelSplit, etc.)
%% to their corresponding gen_yawl module names and expansion functions.
-module(yawl_pattern_registry).
-export([
    pattern_module/1,
    all_patterns/0,
    pattern_macro/1,
    validate_pattern/1
]).

-type pattern_id() :: binary() | atom().
-type module_name() :: atom().

%% Pattern registry: maps pattern macro name to module name
-spec pattern_module(pattern_id()) -> module_name() | undefined.

pattern_module(<<"P1_Sequence">>) -> sequence;
pattern_module(<<"P2_ParallelSplit">>) -> parallel_split;
pattern_module(<<"P3_Synchronization">>) -> synchronization;
pattern_module(<<"P4_ExclusiveChoice">>) -> exclusive_choice;
pattern_module(<<"P5_SimpleMerge">>) -> simple_merge;
pattern_module(<<"P6_MultipleChoice">>) -> multiple_choice;
pattern_module(<<"P7_StructuredSyncMerge">>) -> structured_sync_merge;
pattern_module(<<"P8_MultipleMerge">>) -> multiple_merge;
pattern_module(<<"P9_Discriminator">>) -> discriminator;
pattern_module(<<"P10_ArbitraryCycles">>) -> arbitrary_cycles;
pattern_module(<<"P11_ImplicitTermination">>) -> implicit_termination;
pattern_module(<<"P12_MI_NoSync">>) -> multiple_instances_sync;
pattern_module(<<"P13_MI_DesignTime">>) -> multiple_instances_sync;
pattern_module(<<"P14_MI_RuntimeKnown">>) -> multiple_instances_sync;
pattern_module(<<"P15_MI_RuntimeUnknown">>) -> multiple_instances_sync;
pattern_module(<<"P16_DeferredChoice">>) -> deferred_choice;
pattern_module(<<"P17_InterleavedParallelRouting">>) -> interleaved_routing;
pattern_module(<<"P18_Milestone">>) -> milestone;
pattern_module(<<"P19_CancelActivity">>) -> cancel_activity;
pattern_module(<<"P20_CancelCase">>) -> cancel_case;
pattern_module(<<"P21_StructuredLoop">>) -> structured_loop;
pattern_module(<<"P22_Recursion">>) -> recursion;
pattern_module(<<"P23_TransientTrigger">>) -> transient_trigger;
pattern_module(<<"P24_PersistentTrigger">>) -> persistent_trigger;
pattern_module(<<"P25_CancelRegion">>) -> cancel_region;
pattern_module(<<"P26_CancelMIActivity">>) -> cancel_mi_activity;
pattern_module(<<"P27_CompleteMIActivity">>) -> complete_mi_activity;
pattern_module(<<"P28_BlockingDiscriminator">>) -> blocking_discriminator;
pattern_module(<<"P29_CancellingDiscriminator">>) -> cancelling_discriminator;
pattern_module(<<"P30_StructuredPartialJoin">>) -> structured_partial_join;
pattern_module(<<"P31_BlockingPartialJoin">>) -> blocking_partial_join;
pattern_module(<<"P32_CancellingPartialJoin">>) -> cancelling_partial_join;
pattern_module(<<"P33_GeneralizedANDJoin">>) -> generalized_and_join;
pattern_module(<<"P34_StaticPartialJoinMI">>) -> static_partial_join_mi;
pattern_module(<<"P35_CancellingPartialJoinMI">>) -> cancelling_partial_join_mi;
pattern_module(<<"P36_DynamicPartialJoinMI">>) -> dynamic_partial_join_mi;
pattern_module(<<"P37_LocalSyncMerge">>) -> local_sync_merge;
pattern_module(<<"P38_GeneralSyncMerge">>) -> general_sync_merge;
pattern_module(<<"P39_CriticalSection">>) -> critical_section;
pattern_module(<<"P40_InterleavedRouting">>) -> interleaved_routing;
pattern_module(<<"P41_ThreadMerge">>) -> thread_merge;
pattern_module(<<"P42_ThreadSplit">>) -> thread_split;
pattern_module(<<"P43_ExplicitTermination">>) -> explicit_termination;

%% Handle atom versions
pattern_module(P1_Sequence) -> sequence;
pattern_module(P2_ParallelSplit) -> parallel_split;
pattern_module(P3_Synchronization) -> synchronization;
pattern_module(P4_ExclusiveChoice) -> exclusive_choice;
pattern_module(P5_SimpleMerge) -> simple_merge;
pattern_module(P6_MultipleChoice) -> multiple_choice;
pattern_module(P7_StructuredSyncMerge) -> structured_sync_merge;
pattern_module(P8_MultipleMerge) -> multiple_merge;
pattern_module(P9_Discriminator) -> discriminator;
pattern_module(P10_ArbitraryCycles) -> arbitrary_cycles;
pattern_module(P11_ImplicitTermination) -> implicit_termination;
pattern_module(P12_MI_NoSync) -> multiple_instances_sync;
pattern_module(P13_MI_DesignTime) -> multiple_instances_sync;
pattern_module(P14_MI_RuntimeKnown) -> multiple_instances_sync;
pattern_module(P15_MI_RuntimeUnknown) -> multiple_instances_sync;
pattern_module(P16_DeferredChoice) -> deferred_choice;
pattern_module(P17_InterleavedParallelRouting) -> interleaved_routing;
pattern_module(P18_Milestone) -> milestone;
pattern_module(P19_CancelActivity) -> cancel_activity;
pattern_module(P20_CancelCase) -> cancel_case;
pattern_module(P21_StructuredLoop) -> structured_loop;
pattern_module(P22_Recursion) -> recursion;
pattern_module(P23_TransientTrigger) -> transient_trigger;
pattern_module(P24_PersistentTrigger) -> persistent_trigger;
pattern_module(P25_CancelRegion) -> cancel_region;
pattern_module(P26_CancelMIActivity) -> cancel_mi_activity;
pattern_module(P27_CompleteMIActivity) -> complete_mi_activity;
pattern_module(P28_BlockingDiscriminator) -> blocking_discriminator;
pattern_module(P29_CancellingDiscriminator) -> cancelling_discriminator;
pattern_module(P30_StructuredPartialJoin) -> structured_partial_join;
pattern_module(P31_BlockingPartialJoin) -> blocking_partial_join;
pattern_module(P32_CancellingPartialJoin) -> cancelling_partial_join;
pattern_module(P33_GeneralizedANDJoin) -> generalized_and_join;
pattern_module(P34_StaticPartialJoinMI) -> static_partial_join_mi;
pattern_module(P35_CancellingPartialJoinMI) -> cancelling_partial_join_mi;
pattern_module(P36_DynamicPartialJoinMI) -> dynamic_partial_join_mi;
pattern_module(P37_LocalSyncMerge) -> local_sync_merge;
pattern_module(P38_GeneralSyncMerge) -> general_sync_merge;
pattern_module(P39_CriticalSection) -> critical_section;
pattern_module(P40_InterleavedRouting) -> interleaved_routing;
pattern_module(P41_ThreadMerge) -> thread_merge;
pattern_module(P42_ThreadSplit) -> thread_split;
pattern_module(P43_ExplicitTermination) -> explicit_termination;

pattern_module(PatternId) when is_binary(PatternId) ->
    %% Try atom version
    case catch binary_to_existing_atom(PatternId, utf8) of
        Atom when is_atom(Atom) -> pattern_module(Atom);
        _ -> undefined
    end;
pattern_module(_PatternId) -> undefined.

%% Get macro name for pattern
-spec pattern_macro(pattern_id()) -> binary() | undefined.

pattern_macro(<<"P1_Sequence">>) -> <<"sequence">>;
pattern_macro(<<"P2_ParallelSplit">>) -> <<"parallel_split">>;
pattern_macro(<<"P3_Synchronization">>) -> <<"and_join">>;
pattern_macro(<<"P4_ExclusiveChoice">>) -> <<"xor_choice">>;
pattern_macro(<<"P5_SimpleMerge">>) -> <<"simple_merge">>;
pattern_macro(<<"P6_MultipleChoice">>) -> <<"or_split_structured">>;
pattern_macro(<<"P7_StructuredSyncMerge">>) -> <<"or_join_structured">>;
pattern_macro(<<"P8_MultipleMerge">>) -> <<"multiple_merge">>;
pattern_macro(<<"P9_Discriminator">>) -> <<"discriminator">>;
pattern_macro(<<"P10_ArbitraryCycles">>) -> <<"arbitrary_cycles">>;
pattern_macro(<<"P11_ImplicitTermination">>) -> <<"implicit_termination">>;
pattern_macro(<<"P12_MI_NoSync">>) -> <<"mi_no_sync">>;
pattern_macro(<<"P13_MI_DesignTime">>) -> <<"mi_design_time">>;
pattern_macro(<<"P14_MI_RuntimeKnown">>) -> <<"mi_runtime_known">>;
pattern_macro(<<"P15_MI_RuntimeUnknown">>) -> <<"mi_runtime_unknown">>;
pattern_macro(<<"P16_DeferredChoice">>) -> <<"deferred_choice">>;
pattern_macro(<<"P17_InterleavedParallelRouting">>) -> <<"interleaved_parallel_routing">>;
pattern_macro(<<"P18_Milestone">>) -> <<"milestone">>;
pattern_macro(<<"P19_CancelActivity">>) -> <<"cancel_activity">>;
pattern_macro(<<"P20_CancelCase">>) -> <<"cancel_case">>;
pattern_macro(<<"P21_StructuredLoop">>) -> <<"structured_loop">>;
pattern_macro(<<"P22_Recursion">>) -> <<"recursion">>;
pattern_macro(<<"P23_TransientTrigger">>) -> <<"transient_trigger">>;
pattern_macro(<<"P24_PersistentTrigger">>) -> <<"persistent_trigger">>;
pattern_macro(<<"P25_CancelRegion">>) -> <<"cancel_region">>;
pattern_macro(<<"P26_CancelMIActivity">>) -> <<"cancel_mi_activity">>;
pattern_macro(<<"P27_CompleteMIActivity">>) -> <<"complete_mi_activity">>;
pattern_macro(<<"P28_BlockingDiscriminator">>) -> <<"blocking_discriminator">>;
pattern_macro(<<"P29_CancellingDiscriminator">>) -> <<"cancelling_discriminator">>;
pattern_macro(<<"P30_StructuredPartialJoin">>) -> <<"structured_partial_join">>;
pattern_macro(<<"P31_BlockingPartialJoin">>) -> <<"blocking_partial_join">>;
pattern_macro(<<"P32_CancellingPartialJoin">>) -> <<"cancelling_partial_join">>;
pattern_macro(<<"P33_GeneralizedANDJoin">>) -> <<"generalized_and_join">>;
pattern_macro(<<"P34_StaticPartialJoinMI">>) -> <<"static_partial_join_mi">>;
pattern_macro(<<"P35_CancellingPartialJoinMI">>) -> <<"cancelling_partial_join_mi">>;
pattern_macro(<<"P36_DynamicPartialJoinMI">>) -> <<"dynamic_partial_join_mi">>;
pattern_macro(<<"P37_LocalSyncMerge">>) -> <<"local_sync_merge">>;
pattern_macro(<<"P38_GeneralSyncMerge">>) -> <<"general_sync_merge">>;
pattern_macro(<<"P39_CriticalSection">>) -> <<"critical_section">>;
pattern_macro(<<"P40_InterleavedRouting">>) -> <<"interleaved_routing">>;
pattern_macro(<<"P41_ThreadMerge">>) -> <<"thread_merge">>;
pattern_macro(<<"P42_ThreadSplit">>) -> <<"thread_split">>;
pattern_macro(<<"P43_ExplicitTermination">>) -> <<"explicit_termination">>;
pattern_macro(_) -> undefined.

%% Get all registered patterns
-spec all_patterns() -> [pattern_id()].

all_patterns() ->
    [
        <<"P1_Sequence">>, <<"P2_ParallelSplit">>, <<"P3_Synchronization">>,
        <<"P4_ExclusiveChoice">>, <<"P5_SimpleMerge">>, <<"P6_MultipleChoice">>,
        <<"P7_StructuredSyncMerge">>, <<"P8_MultipleMerge">>, <<"P9_Discriminator">>,
        <<"P10_ArbitraryCycles">>, <<"P11_ImplicitTermination">>, <<"P12_MI_NoSync">>,
        <<"P13_MI_DesignTime">>, <<"P14_MI_RuntimeKnown">>, <<"P15_MI_RuntimeUnknown">>,
        <<"P16_DeferredChoice">>, <<"P17_InterleavedParallelRouting">>, <<"P18_Milestone">>,
        <<"P19_CancelActivity">>, <<"P20_CancelCase">>, <<"P21_StructuredLoop">>,
        <<"P22_Recursion">>, <<"P23_TransientTrigger">>, <<"P24_PersistentTrigger">>,
        <<"P25_CancelRegion">>, <<"P26_CancelMIActivity">>, <<"P27_CompleteMIActivity">>,
        <<"P28_BlockingDiscriminator">>, <<"P29_CancellingDiscriminator">>,
        <<"P30_StructuredPartialJoin">>, <<"P31_BlockingPartialJoin">>,
        <<"P32_CancellingPartialJoin">>, <<"P33_GeneralizedANDJoin">>,
        <<"P34_StaticPartialJoinMI">>, <<"P35_CancellingPartialJoinMI">>,
        <<"P36_DynamicPartialJoinMI">>, <<"P37_LocalSyncMerge">>,
        <<"P38_GeneralSyncMerge">>, <<"P39_CriticalSection">>,
        <<"P40_InterleavedRouting">>, <<"P41_ThreadMerge">>, <<"P42_ThreadSplit">>,
        <<"P43_ExplicitTermination">>
    ].

%% Validate that a pattern exists
-spec validate_pattern(pattern_id()) -> boolean().

validate_pattern(PatternId) ->
    pattern_module(PatternId) =/= undefined.
