%% -*- erlang -*-
%% @doc Tests for Declarative Discovery (Declare Constraints)

-module(declare_discovery_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
;; Test Fixtures
;;====================================================================

simple_log() ->
    [[a, b, c, d], [a, c, b, d], [a, b, c, e, d]].

log_with_response() ->
    [[a, b], [a, c, b], [a, d, b]].

log_with_precedence() ->
    [[a, b], [c, a, b], [a, d, b]].

log_with_choice() ->
    [[a, c], [b, c], [a, d], [b, d]].

log_with_coexistence() ->
    [[a, b], [b, c], [a, b, c]].

empty_log() ->
    [].

%%====================================================================
;; Discovery Tests
;;====================================================================

discover_constraints_test() ->
    Log = simple_log(),
    {ok, Constraints} = declare_discovery:discover_constraints(Log),
    ?assert(is_list(Constraints)),
    ?assert(length(Constraints) > 0).

discover_constraints_with_options_test() ->
    Log = simple_log(),
    {ok, Constraints} = declare_discovery:discover_constraints(Log, #{min_support => 0.5}),
    ?assert(is_list(Constraints)).

discover_constraints_empty_log_test() ->
    Log = empty_log(),
    {ok, Constraints} = declare_discovery:discover_constraints(Log),
    ?assert(is_list(Constraints)).

discover_constraints_with_max_test() ->
    Log = simple_log(),
    {ok, Constraints} = declare_discovery:discover_constraints(Log, #{max_constraints => 3}),
    ?assert(length(Constraints) =< 3).

%%====================================================================
;; Existence Constraint Tests
;;====================================================================

discover_existence_test() ->
    Log = simple_log(),
    Activities = [a, b, c, d, e],
    Constraints = declare_discovery:discover_existence(Log, Activities),
    ?assert(length(Constraints) > 0),
    lists:foreach(fun(C) ->
        ?assertEqual(existence, maps:get(template, C))
    end, Constraints).

discover_existence_all_present_test() ->
    Log = [[a, b], [a, b], [a, b]],
    Activities = [a, b],
    Constraints = declare_discovery:discover_existence(Log, Activities),
    ?assertEqual(2, length(Constraints)).

%%====================================================================
;; Response Constraint Tests
;;====================================================================

discover_response_test() ->
    Log = log_with_response(),
    Activities = [a, b, c, d],
    Constraints = declare_discovery:discover_response(Log, Activities),
    ?assert(is_list(Constraints)),
    %% Should find a ->> b
    HasABResponse = lists:any(fun(C) ->
        maps:get(template, C) =:= response andalso
        maps:get(activities, C) =:= [a, b]
    end, Constraints),
    ?assert(HasABResponse).

analyze_response_test() ->
    Log = [[a, b], [a], [a, c, b]],
    {Violations, Total} = declare_discovery:analyze_response(a, b, Log),
    ?assertEqual(1, Violations),
    ?assertEqual(3, Total).

analyze_response_perfect_test() ->
    Log = [[a, b], [a, b], [c, a, b]],
    {Violations, Total} = declare_discovery:analyze_response(a, b, Log),
    ?assertEqual(0, Violations),
    ?assertEqual(3, Total).

analyze_response_no_a_test() ->
    Log = [[b], [c, b]],
    {Violations, Total} = declare_discovery:analyze_response(a, b, Log),
    ?assertEqual(0, Violations),
    ?assertEqual(0, Total).

%%====================================================================
;; Precedence Constraint Tests
;;====================================================================

discover_precedence_test() ->
    Log = log_with_precedence(),
    Activities = [a, b, c, d],
    Constraints = declare_discovery:discover_precedence(Log, Activities),
    ?assert(is_list(Constraints)).

analyze_precedence_test() ->
    Log = [[a, b], [b], [c, a, b]],
    {Violations, Total} = declare_discovery:analyze_precedence(a, b, Log),
    ?assertEqual(1, Violations),
    ?assertEqual(3, Total).

analyze_precedence_perfect_test() ->
    Log = [[a, b], [c, a, b], [d, a, b]],
    {Violations, Total} = declare_discovery:analyze_precedence(a, b, Log),
    ?assertEqual(0, Violations),
    ?assertEqual(3, Total).

%%====================================================================
;; Succession Constraint Tests
;;====================================================================

discover_succession_test() ->
    Log = [[a, b, c], [a, c, b], [b, a, c]],
    Activities = [a, b, c],
    Constraints = declare_discovery:discover_succession(Log, Activities),
    ?assert(is_list(Constraints)).

discover_succession_empty_test() ->
    Log = [],
    Activities = [a, b],
    Constraints = declare_discovery:discover_succession(Log, Activities),
    ?assert(is_list(Constraints)).

%%====================================================================
;; Co-existence Constraint Tests
;;====================================================================

discover_coexistence_test() ->
    Log = log_with_coexistence(),
    Activities = [a, b, c],
    Constraints = declare_discovery:discover_coexistence(Log, Activities),
    ?assert(is_list(Constraints)).

analyze_coexistence_test() ->
    Log = [[a, b], [a], [b], []],
    {ABOnly, BAOnly, Both, Neither} = declare_discovery:analyze_coexistence(a, b, Log),
    ?assertEqual(1, ABOnly),
    ?assertEqual(1, BAOnly),
    ?assertEqual(1, Both),
    ?assertEqual(1, Neither).

analyze_coexistence_perfect_test() ->
    Log = [[a, b], [a, b], [a, b]],
    {ABOnly, BAOnly, Both, Neither} = declare_discovery:analyze_coexistence(a, b, Log),
    ?assertEqual(0, ABOnly),
    ?assertEqual(0, BAOnly),
    ?assertEqual(3, Both),
    ?assertEqual(0, Neither).

%%====================================================================
;; Choice Constraint Tests
;;====================================================================

discover_choice_test() ->
    Log = log_with_choice(),
    Activities = [a, b, c, d],
    Constraints = declare_discovery:discover_choice(Log, Activities),
    ?assert(is_list(Constraints)),
    %% Should find choice between a and b
    HasABChoice = lists:any(fun(C) ->
        maps:get(template, C) =:= choice andalso
        maps:get(activities, C) =:= [a, b]
    end, Constraints),
    ?assert(HasABChoice).

discover_choice_mutual_exclusive_test() ->
    Log = [[a, c], [b, c], [a, d], [b, d], [a, b, c]],
    Activities = [a, b, c],
    Constraints = declare_discovery:discover_choice(Log, Activities),
    ?assert(is_list(Constraints)).

%%====================================================================
;; Constraint Evaluation Tests
;;====================================================================

test_constraint_test() ->
    Log = simple_log(),
    Constraint = #{
        template => existence,
        activities => [a],
        parameters => #{min_count => 1},
        support => 1.0,
        confidence => 1.0
    },
    {ok, Result} = declare_discovery:test_constraint(Constraint, Log),
    ?assert(maps:is_key(support, Result)),
    ?assert(maps:is_key(confidence, Result)),
    ?assert(maps:is_key(violations, Result)),
    ?assert(maps:is_key(holds, Result)).

get_support_test() ->
    Log = simple_log(),
    Constraint = #{
        template => existence,
        activities => [a],
        parameters => #{},
        support => 0.0,
        confidence => 0.0
    },
    Support = declare_discovery:get_support(Constraint, Log),
    ?assert(Support > 0.0).

get_confidence_test() ->
    Log = simple_log(),
    Constraint = #{
        template => existence,
        activities => [a],
        parameters => #{},
        support => 1.0,
        confidence => 0.8
    },
    Confidence = declare_discovery:get_confidence(Constraint, Log),
    ?assert(is_float(Confidence)).

evaluate_constraint_test() ->
    Log = simple_log(),
    Constraint = #{
        template => existence,
        activities => [a],
        parameters => #{min_count => 1},
        support => 1.0,
        confidence => 1.0
    },
    {ok, Confidence} = declare_discovery:evaluate_constraint(Constraint, Log),
    ?assert(is_float(Confidence)).

%%====================================================================
;; Template Tests
;;====================================================================

get_constraint_templates_test() ->
    Templates = declare_discovery:get_constraint_templates(),
    ?assert(is_list(Templates)),
    ?assert(lists:member(existence, Templates)),
    ?assert(lists:member(response, Templates)),
    ?assert(lists:member(precedence, Templates)),
    ?assert(lists:member(succession, Templates)).

%%====================================================================
;; Filtering Tests
;;====================================================================

filter_constraints_test() ->
    Constraints = [
        #{template => existence, activities => [a], support => 0.9, confidence => 0.8},
        #{template => response, activities => [a, b], support => 0.4, confidence => 0.6},
        #{template => precedence, activities => [a, b], support => 0.7, confidence => 0.9}
    ],
    Filtered = declare_discovery:filter_constraints(Constraints, 0.5, 0.7, infinity),
    ?assertEqual(2, length(Filtered)).

filter_constraints_max_test() ->
    Constraints = [
        #{template => existence, activities => [a], support => 0.9, confidence => 0.8},
        #{template => response, activities => [a, b], support => 0.4, confidence => 0.6},
        #{template => precedence, activities => [a, b], support => 0.7, confidence => 0.9}
    ],
    Filtered = declare_discovery:filter_constraints(Constraints, 0.0, 0.0, 2),
    ?assertEqual(2, length(Filtered)).

filter_constraints_empty_test() ->
    Filtered = declare_discovery:filter_constraints([], 0.5, 0.5, 10),
    ?assertEqual([], Filtered).

%%====================================================================
;; Activity Counting Tests
;;====================================================================

count_activity_occurrences_test() ->
    Log = [[a, b, c], [a, b], [b, c]],
    Count = declare_discovery:count_activity_occurrences(a, Log),
    ?assertEqual(2.0, Count).

count_traces_with_test() ->
    Log = [[a, b], [a, c], [b, c]],
    Count = declare_discovery:count_traces_with(a, Log),
    ?assertEqual(2.0, Count).

count_traces_with_both_test() ->
    Log = [[a, b], [a, c], [a, b, c]],
    Count = declare_discovery:count_traces_with_both(a, b, Log),
    ?assertEqual(2.0, Count).

count_traces_with_both_none_test() ->
    Log = [[a, c], [b, c]],
    Count = declare_discovery:count_traces_with_both(a, b, Log),
    ?assertEqual(0.0, Count).

%%====================================================================
;; Integration Tests
;;====================================================================

discover_full_pipeline_test() ->
    Log = [
        [a, b, c, d],
        [a, c, b, d],
        [a, b, c, e, d],
        [b, a, c, d]
    ],
    {ok, Constraints} = declare_discovery:discover_constraints(Log, #{
        min_support => 0.3,
        min_confidence => 0.5
    }),
    ?assert(length(Constraints) > 0),
    %% Verify all constraints meet thresholds
    lists:foreach(fun(C) ->
        ?assert(maps:get(support, C) >= 0.3),
        ?assert(maps:get(confidence, C) >= 0.5)
    end, Constraints).

discover_all_constraint_types_test() ->
    Log = simple_log(),
    {ok, Constraints} = declare_discovery:discover_constraints(Log),
    Templates = lists:usort([maps:get(template, C) || C <- Constraints]),
    %% Should have at least some constraint types
    ?assert(length(Templates) >= 1).

discover_and_evaluate_test() ->
    Log = simple_log(),
    {ok, Constraints} = declare_discovery:discover_constraints(Log),
    lists:foreach(fun(C) ->
        {ok, Result} = declare_discovery:test_constraint(C, Log),
        ?assert(maps:is_key(holds, Result))
    end, Constraints).

%%====================================================================
;; Violation Detection Tests
;;====================================================================

find_violations_existence_test() ->
    Log = [[a, b], [b, c], [a, c]],
    Constraint = #{
        template => existence,
        activities => [d],
        parameters => #{},
        support => 0.0,
        confidence => 0.0
    },
    Violations = declare_discovery:find_violations(Constraint, Log),
    ?assert(is_list(Violations)),
    ?assertEqual(3, length(Violations)).

find_violations_response_test() ->
    Log = [[a, b], [a], [a, c]],
    Constraint = #{
        template => response,
        activities => [a, b],
        parameters => #{},
        support => 0.5,
        confidence => 0.5
    },
    Violations = declare_discovery:find_violations(Constraint, Log),
    ?assert(length(Violations) >= 1).

find_violations_precedence_test() ->
    Log = [[a, b], [b], [a, b]],
    Constraint = #{
        template => precedence,
        activities => [a, b],
        parameters => #{},
        support => 0.5,
        confidence => 0.5
    },
    Violations = declare_discovery:find_violations(Constraint, Log),
    ?assertEqual(1, length(Violations)).
