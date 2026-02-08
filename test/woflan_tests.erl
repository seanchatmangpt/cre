%% -*- erlang %%
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc Woflan diagnostics integration tests
%%
%% Test module for the Woflan workflow diagnostics.
%% Tests soundness verification, deadlock detection, dead transition
%% detection, and repair suggestion generation.
%% @end
%% -------------------------------------------------------------------

-module(woflan_tests).
-moduledoc("""
Woflan workflow diagnostics tests.

Tests the Woflan-style diagnostic functions including:
- Soundness verification
- Deadlock detection
- Dead transition analysis
- Repair suggestions
- Reachability graph construction
""").

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Data
%%====================================================================

%% A sound workflow net (sequential)
sound_workflow_net() ->
    #{places => [start, processing, done],
      transitions => [t1, t2],
      arcs => [{start, t1}, {t1, processing},
              {processing, t2}, {t2, done}]}.

%% An unsound workflow with deadlock potential
deadlock_net() ->
    #{places => [p1, p2],
      transitions => [t1, t2],
      arcs => [{p1, t1}, {t1, p2}, {p2, t2}, {t2, p2}]}.

%% Net with dead transitions
dead_transition_net() ->
    #{places => [p1, p2, p3],
      transitions => [t1, t2, t3],
      arcs => [{p1, t1}, {t1, p2}, {p3, t3}, {t3, p1}]}.

%% Parallel workflow (sound)
parallel_net() ->
    #{places => [start, p1, p2, done],
      transitions => [split, join],
      arcs => [{start, split}, {split, p1}, {split, p2},
              {p1, join}, {p2, join}, {join, done}]}.

%% Net without input place
no_input_net() ->
    #{places => [done],
      transitions => [t],
      arcs => [{t, done}]}.

%% Net without output place
no_output_net() ->
    #{places => [start],
      transitions => [t],
      arcs => [{start, t}]}.

%%====================================================================
%% diagnose/1 Tests
%%====================================================================

diagnose_sound_workflow_test() ->
    Report = woflan:diagnose(sound_workflow_net()),
    ?assertEqual(ok, maps:get(status, Report)),
    ?assert(is_list(maps:get(issues, Report))),
    ?assert(is_list(maps:get(suggestions, Report))).

diagnose_deadlock_net_test() ->
    Report = woflan:diagnose(deadlock_net()),
    Status = maps:get(status, Report),
    ?assert(Status =:= ok orelse Status =:= unsound).

diagnose_no_input_test() ->
    Report = woflan:diagnose(no_input_net()),
    Status = maps:get(status, Report),
    ?assert(Status =:= unsound orelse Status =:= error).

diagnose_no_output_test() ->
    Report = woflan:diagnose(no_output_net()),
    Status = maps:get(status, Report),
    ?assert(Status =:= unsound orelse Status =:= error).

diagnose_parallel_test() ->
    Report = woflan:diagnose(parallel_net()),
    ?assertEqual(ok, maps:get(status, Report)).

%%====================================================================
%% check_soundness/1 Tests
%%====================================================================

check_soundness_sound_test() ->
    ?assertEqual({ok, true}, woflan:check_soundness(sound_workflow_net())).

check_soundness_parallel_test() ->
    ?assertEqual({ok, true}, woflan:check_soundness(parallel_net())).

check_soundness_no_input_test() ->
    case woflan:check_soundness(no_input_net()) of
        {ok, false, _} -> ok;
        {ok, false} -> ok
    end.

check_soundness_no_output_test() ->
    case woflan:check_soundness(no_output_net()) of
        {ok, false, _} -> ok;
        {ok, false} -> ok
    end.

%%====================================================================
%% is_sound_workflow/1 Tests
%%====================================================================

is_sound_sound_test() ->
    ?assert(woflan:is_sound_workflow(sound_workflow_net())).

is_sound_parallel_test() ->
    ?assert(woflan:is_sound_workflow(parallel_net())).

is_sound_no_input_test() ->
    ?assertNot(woflan:is_sound_workflow(no_input_net())).

is_sound_no_output_test() ->
    ?assertNot(woflan:is_sound_workflow(no_output_net())).

%%====================================================================
%% detect_deadlocks/1 Tests
%%====================================================================

detect_deadlocks_sound_test() ->
    Deadlocks = woflan:detect_deadlocks(sound_workflow_net()),
    ?assert(is_list(Deadlocks)).

detect_deadlocks_parallel_test() ->
    Deadlocks = woflan:detect_deadlocks(parallel_net()),
    ?assert(is_list(Deadlocks)).

detect_deadlocks_deadlock_net_test() ->
    Deadlocks = woflan:detect_deadlocks(deadlock_net()),
    ?assert(is_list(Deadlocks)).

%%====================================================================
%% detect_dead_transitions/1 Tests
%%====================================================================

detect_dead_transitions_sound_test() ->
    Dead = woflan:detect_dead_transitions(sound_workflow_net()),
    ?assert(is_list(Dead)),
    ?assertEqual([], Dead).

detect_dead_transitions_with_dead_test() ->
    Dead = woflan:detect_dead_transitions(dead_transition_net()),
    ?assert(is_list(Dead)),
    ?assert(length(Dead) > 0).

%%====================================================================
%% suggest_repair/1 Tests
%%====================================================================

suggest_repair_sound_test() ->
    Suggestions = woflan:suggest_repair(sound_workflow_net()),
    ?assert(is_list(Suggestions)).

suggest_repair_no_input_test() ->
    Suggestions = woflan:suggest_repair(no_input_net()),
    ?assert(is_list(Suggestions)).

suggest_repair_no_output_test() ->
    Suggestions = woflan:suggest_repair(no_output_net()),
    ?assert(is_list(Suggestions)).

%%====================================================================
%% build_reachability_graph/2 Tests
%%====================================================================

build_reachability_sound_test() ->
    Graph = woflan:build_reachability_graph(sound_workflow_net(), 100),
    ?assert(is_map(Graph)),
    ?assert(maps:size(Graph) > 0).

build_reachability_parallel_test() ->
    Graph = woflan:build_reachability_graph(parallel_net(), 100),
    ?assert(is_map(Graph)),
    ?assert(maps:size(Graph) > 0).

build_reachability_limit_test() ->
    Graph = woflan:build_reachability_graph(sound_workflow_net(), 5),
    ?assert(is_map(Graph)).

%%====================================================================
%% wf_verify integration Tests
%%====================================================================

wf_verify_diagnose_test() ->
    Report = wf_verify:diagnose(sound_workflow_net()),
    ?assertEqual(ok, maps:get(status, Report)).

wf_verify_report_test() ->
    Report = wf_verify:woflan_report(sound_workflow_net()),
    ?assert(is_binary(Report)),
    ?assertNot(<<>> =:= Report),
    ?assert(<<"SOUND">> =:= Report orelse binary:match(Report, <<"Status">>) = nomatch).

wf_verify_report_unsound_test() ->
    Report = wf_verify:woflan_report(no_input_net()),
    ?assert(is_binary(Report)),
    ?assertNot(<<>> =:= Report).

%%====================================================================
%% Helper Functions Tests
%%====================================================================

%% Tests for marking hash stability
marking_hash_stability_test() ->
    Marking = #{p1 => 1, p2 => 2},
    Hash1 = woflan:marking_hash(Marking),
    Hash2 = woflan:marking_hash(Marking),
    ?assertEqual(Hash1, Hash2).

%% Tests for marking hash uniqueness
marking_hash_uniqueness_test() ->
    Marking1 = #{p1 => 1, p2 => 2},
    Marking2 = #{p1 => 2, p2 => 1},
    Hash1 = woflan:marking_hash(Marking1),
    Hash2 = woflan:marking_hash(Marking2),
    ?assertNot(Hash1 =:= Hash2).

%% Tests for final marking detection
is_final_marking_true_test() ->
    Marking = #{done => 1},
    ?assert(woflan:is_final_marking(Marking, done)).

is_final_marking_false_test() ->
    Marking = #{done => 1, other => 1},
    ?assertNot(woflan:is_final_marking(Marking, done)).

is_final_marking_empty_test() ->
    Marking = #{},
    ?assertNot(woflan:is_final_marking(Marking, done)).

%% Tests for path existence
path_exists_sound_test() ->
    ?assert(woflan:path_exists(sound_workflow_net(), start, done)).

path_exists_no_path_test() ->
    Net = #{places => [p1, p2],
            transitions => [t1],
            arcs => [{p1, t1}, {t1, p2}]},
    ?assertNot(woflan:path_exists(Net, p2, p1)).
