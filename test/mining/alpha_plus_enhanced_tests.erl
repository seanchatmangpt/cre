%% -*- erlang -*-
%% @doc Test suite for alpha_plus_enhanced module

-module(alpha_plus_enhanced_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% Test Data
%%====================================================================

simple_log() ->
    [[a, b, c], [a, b, c], [a, b, c]].

noisy_log() ->
    [[a, b, c], [a, b, c], [a, b, c], [a, x, c], [a, y, c]].

parallel_log() ->
    [[a, b, c, d], [a, c, b, d], [a, b, c, d], [a, c, b, d]].

complex_log() ->
    [[start, register, approve, complete],
     [start, register, approve, complete],
     [start, register, review, approve, complete]].

empty_log() ->
    [].

%%====================================================================
%% Trace Frequency Tests
%%====================================================================

calculate_trace_frequency_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = [[a, b], [a, b], [b, c]],
         Freq = alpha_plus_enhanced:calculate_trace_frequency(Log),
         ?assertEqual(2, maps:get([a, b], Freq)),
         ?assertEqual(1, maps:get([b, c], Freq))
     end),

      ?_test(begin
         Log = [],
         Freq = alpha_plus_enhanced:calculate_trace_frequency(Log),
         ?assertEqual(0, map_size(Freq))
     end)
     ]}.

filter_by_frequency_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = [[a, b], [a, b], [b, c]],
         Filtered = alpha_plus_enhanced:filter_by_frequency(Log, 2),
         ?assertEqual(2, length(Filtered)),
         ?assertNot(lists:member([b, c], Filtered))
     end),

      ?_test(begin
         Log = [[a, b], [c, d]],
         Filtered = alpha_plus_enhanced:filter_by_frequency(Log, 2),
         ?assertEqual([], Filtered)
     end),

      ?_test(begin
         Log = [[a, b], [a, b], [a, b], [a, b]],
         Filtered = alpha_plus_enhanced:filter_by_frequency(Log, 3),
         ?assertEqual(4, length(Filtered))
     end)
     ]}.

%%====================================================================
%% Discovery Tests
%%====================================================================

discover_with_noise_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = simple_log(),
         Net = alpha_plus_enhanced:discover_with_noise(Log, #{noise_threshold => 0.1}),
         ?assert(is_list(maps:get(places, Net))),
         ?assert(is_list(maps:get(transitions, Net))),
         ?assertEqual(i_source, maps:get(initial_place, Net)),
         ?assertEqual(o_sink, maps:get(final_place, Net))
     end),

      ?_test(begin
         Log = simple_log(),
         Net = alpha_plus_enhanced:discover_with_noise(Log, #{}),
         Transitions = maps:get(transitions, Net),
         ?assert(lists:member(a, Transitions)),
         ?assert(lists:member(b, Transitions)),
         ?assert(lists:member(c, Transitions))
     end),

      ?_test(begin
         Log = noisy_log(),
         Net = alpha_plus_enhanced:discover_with_noise(Log, #{noise_threshold => 0.5}),
         Transitions = maps:get(transitions, Net),
         ?assert(lists:member(a, Transitions)),
         ?assert(lists:member(b, Transitions)),
         ?assert(lists:member(c, Transitions))
     end),

      ?_test(begin
         Log = parallel_log(),
         Net = alpha_plus_enhanced:discover_with_noise(Log, #{noise_threshold => 0.1}),
         Transitions = maps:get(transitions, Net),
         ?assert(lists:member(a, Transitions)),
         ?assert(lists:member(b, Transitions)),
         ?assert(lists:member(c, Transitions)),
         ?assert(lists:member(d, Transitions))
     end)
     ]}.

%%====================================================================
%% Handle Infrequent Traces Tests
%%====================================================================

handle_infrequent_traces_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = [[a, b], [a, b], [a, b], [b, c]],
         Filtered = alpha_plus_enhanced:handle_infrequent_traces(Log, 2),
         ?assertEqual(3, length(Filtered)),
         ?assertNot(lists:member([b, c], Filtered))
     end),

      ?_test(begin
         Log = [[a, b], [c, d]],
         Filtered = alpha_plus_enhanced:handle_infrequent_traces(Log, 2),
         ?assertEqual([], Filtered)
     end),

      ?_test(begin
         Log = [[a, b], [a, b], [c, d], [c, d]],
         Filtered = alpha_plus_enhanced:handle_infrequent_traces(Log, 2),
         ?assertEqual(4, length(Filtered)),
         ?assert(lists:member([a, b], Filtered)),
         ?assert(lists:member([c, d], Filtered))
     end)
     ]}.

%%====================================================================
%% Main Entry Point Tests
%%====================================================================

mine_workflow_net_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = simple_log(),
         Net = alpha_plus_enhanced:mine_workflow_net(Log),
         ?assert(is_list(maps:get(places, Net))),
         ?assert(is_list(maps:get(transitions, Net))),
         ?assert(is_list(maps:get(arcs, Net)))
     end),

      ?_test(begin
         Log = complex_log(),
         Net = alpha_plus_enhanced:mine_workflow_net(Log),
         Transitions = maps:get(transitions, Net),
         ?assert(lists:member(start, Transitions)),
         ?assert(lists:member(register, Transitions)),
         ?assert(lists:member(approve, Transitions)),
         ?assert(lists:member(complete, Transitions))
     end)
     ]}.

%%====================================================================
%% Significance Tests
%%====================================================================

calculate_significance_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Sig = alpha_plus_enhanced:calculate_significance(10, 8, 100),
         ?assert(is_float(Sig)),
         ?assert(Sig > 0.0)
     end),

      ?_test(begin
         Sig = alpha_plus_enhanced:calculate_significance(0, 0, 0),
         ?assert(is_float(Sig))
     end)
     ]}.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

edge_cases_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = empty_log(),
         Net = alpha_plus_enhanced:discover_with_noise(Log, #{}),
         ?assert(is_map(Net))
     end),

      ?_test(begin
         Log = [[a]],
         Net = alpha_plus_enhanced:discover_with_noise(Log, #{}),
         Transitions = maps:get(transitions, Net),
         ?assert(lists:member(a, Transitions))
     end),

      ?_test(begin
         Log = [[a, b], [a, b]],
         Net = alpha_plus_enhanced:discover_with_noise(Log, #{noise_threshold => 0.99}),
         ?assert(is_map(Net))
     end)
     ]}.

%%====================================================================
%% WF-net Structure Tests
%%====================================================================

wf_net_structure_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      ?_test(begin
         Log = [[a, b, c]],
         Net = alpha_plus_enhanced:mine_workflow_net(Log),
         Places = maps:get(places, Net),
         ?assert(lists:member(i_source, Places)),
         ?assert(lists:member(o_sink, Places))
     end),

      ?_test(begin
         Log = [[a, b, c]],
         Net = alpha_plus_enhanced:mine_workflow_net(Log),
         Arcs = maps:get(arcs, Net),
         ?assert(is_list(Arcs)),
         ?assert(length(Arcs) > 0)
     end)
     ]}.

%%====================================================================
%% Test Suite
%%====================================================================

alpha_plus_enhanced_test_() ->
    [
     {"Trace frequency tests", calculate_trace_frequency_test_()},
     {"Filter by frequency tests", filter_by_frequency_test_()},
     {"Discovery tests", discover_with_noise_test_()},
     {"Handle infrequent traces tests", handle_infrequent_traces_test_()},
     {"Main entry point tests", mine_workflow_net_test_()},
     {"Significance tests", calculate_significance_test_()},
     {"Edge cases tests", edge_cases_test_()},
     {"WF-net structure tests", wf_net_structure_test_()}
    ].
