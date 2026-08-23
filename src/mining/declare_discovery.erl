%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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
%% @doc Declarative Process Discovery (Declare Constraints)
%%
%% This module discovers Declare constraints from event logs.
%% Declare is a declarative process modeling language that specifies
%% constraints on activity executions rather than explicit control flow.
%%
%% <h3>Declare Constraint Templates</h3>
%%
%% <ul>
%%   <li><b>Existence:</b> Activity must occur at least N times</li>
%%   <li><b>Absence:</b> Activity cannot occur</li>
%%   <li><b>Selection:</b> Activity occurs between min and max times</li>
%%   <li><b>Response:</b> If A occurs, B must eventually follow</li>
%%   <li><b>Precedence:</b> If B occurs, A must have occurred before</li>
%%   <li><b>Succession:</b> Combined response + precedence</li>
%%   <li><b>Co-existence:</b> A and B always occur together</li>
%%   <li><b>Choice:</b> Either A or B occurs, but not both</li>
%% </ul>
%%
%% <h3>Discovery Process</h3>
%%
%% <ol>
%%   <li>Count activity frequencies</li>
%%   <li>Analyze pairwise relationships</li>
%%   <li>Test constraint templates</li>
%%   <li>Filter by support/confidence thresholds</li>
%% </ol>
%%
%% @end
%% -------------------------------------------------------------------

-module(declare_discovery).

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([discover_constraints/1]).
-export([discover_constraints/2]).
-export([test_constraint/2]).
-export([get_constraint_templates/0]).

%% Constraint discovery
-export([discover_existence/2]).
-export([discover_response/2]).
-export([discover_precedence/2]).
-export([discover_succession/2]).
-export([discover_coexistence/2]).
-export([discover_choice/2]).

%% Constraint evaluation
-export([evaluate_constraint/2]).
-export([get_support/2]).
-export([get_confidence/2]).

%%====================================================================
%% Types
%%====================================================================

-type activity() :: atom().
-type trace() :: [activity()].
-type event_log() :: [trace()].

-type constraint_template() :: existence | absence | selection | response |
                               precedence | succession | coexistence | choice |
                               responded_existence | alternate_response |
                               chain_response | alternate_precedence |
                               chain_precedence.

-type constraint() :: #{
    template := constraint_template(),
    activities := [activity()],
    parameters := map(),
    support := float(),
    confidence := float()
}.

-type discovery_options() :: #{
    min_support => float(),
    min_confidence => float(),
    max_constraints => pos_integer() | infinity
}.

-type constraint_set() :: [constraint()].

-export_type([
    activity/0, trace/0, event_log/0,
    constraint_template/0, constraint/0,
    discovery_options/0, constraint_set/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Discover Declare constraints from log with default options.
-spec discover_constraints(event_log()) -> {ok, constraint_set()}.
discover_constraints(Log) ->
    discover_constraints(Log, #{}).

%% @doc Discover Declare constraints with custom options.
-spec discover_constraints(event_log(), discovery_options()) -> {ok, constraint_set()}.
discover_constraints(Log, Options) when is_list(Log), is_map(Options) ->
    %% Extract activities
    Activities = extract_activities(Log),

    %% Discover all constraint types
    AllConstraints = [
        discover_existence(Log, Activities),
        discover_response(Log, Activities),
        discover_precedence(Log, Activities),
        discover_succession(Log, Activities),
        discover_coexistence(Log, Activities),
        discover_choice(Log, Activities)
    ],

    %% Flatten and filter by thresholds
    MinSupport = maps:get(min_support, Options, 0.1),
    MinConfidence = maps:get(min_confidence, Options, 0.5),
    MaxConstraints = maps:get(max_constraints, Options, infinity),

    Filtered = filter_constraints(
        lists:flatten(AllConstraints),
        MinSupport,
        MinConfidence,
        MaxConstraints
    ),

    {ok, Filtered}.

%% @doc Test a constraint against the log.
-spec test_constraint(constraint(), event_log()) -> {ok, map()}.
test_constraint(Constraint, Log) ->
    Support = get_support(Constraint, Log),
    Confidence = get_confidence(Constraint, Log),
    Violations = find_violations(Constraint, Log),

    {ok, #{
        support => Support,
        confidence => Confidence,
        violations => Violations,
        holds => Support >= 0.5
    }}.

%% @doc Get available constraint templates.
-spec get_constraint_templates() -> [constraint_template()].
get_constraint_templates() ->
    [existence, absence, selection, response, precedence, succession,
     coexistence, choice, responded_existence, alternate_response,
     chain_response, alternate_precedence, chain_precedence].

%%====================================================================
%% Constraint Discovery Functions
%%====================================================================

%% @doc Discover existence constraints.
-spec discover_existence(event_log(), [activity()]) -> [constraint()].
discover_existence(Log, Activities) ->
    lists:filtermap(fun(A) ->
        Count = count_activity_occurrences(A, Log),
        TotalTraces = length(Log),
        Support = Count / TotalTraces,

        case Support >= 0.1 of
            true ->
                {true, #{
                    template => existence,
                    activities => [A],
                    parameters => #{min_count => 1},
                    support => Support,
                    confidence => 1.0
                }};
            false ->
                false
        end
    end, Activities).

%% @doc Discover response constraints (A ->> B).
-spec discover_response(event_log(), [activity()]) -> [constraint()].
discover_response(Log, Activities) ->
    Pairs = [{A, B} || A <- Activities, B <- Activities, A =/= B],

    lists:filtermap(fun({A, B}) ->
        %% Response: if A occurs, B must eventually follow
        {Violations, TotalWithA} = analyze_response(A, B, Log),

        Support = case TotalWithA of
            0 -> 0.0;
            N -> (N - Violations) / N
        end,

        case Support >= 0.5 of
            true ->
                {true, #{
                    template => response,
                    activities => [A, B],
                    parameters => #{},
                    support => Support,
                    confidence => compute_response_confidence(A, B, Log)
                }};
            false ->
                false
        end
    end, Pairs).

%% @doc Discover precedence constraints (A -> B).
-spec discover_precedence(event_log(), [activity()]) -> [constraint()].
discover_precedence(Log, Activities) ->
    Pairs = [{A, B} || A <- Activities, B <- Activities, A =/= B],

    lists:filtermap(fun({A, B}) ->
        %% Precedence: if B occurs, A must have occurred before
        {Violations, TotalWithB} = analyze_precedence(A, B, Log),

        Support = case TotalWithB of
            0 -> 0.0;
            N -> (N - Violations) / N
        end,

        case Support >= 0.5 of
            true ->
                {true, #{
                    template => precedence,
                    activities => [A, B],
                    parameters => #{},
                    support => Support,
                    confidence => compute_precedence_confidence(A, B, Log)
                }};
            false ->
                false
        end
    end, Pairs).

%% @doc Discover succession constraints (response + precedence).
-spec discover_succession(event_log(), [activity()]) -> [constraint()].
discover_succession(Log, Activities) ->
    Response = discover_response(Log, Activities),
    Precedence = discover_precedence(Log, Activities),

    %% Find pairs that satisfy both
    lists:filtermap(fun(R) ->
        RActs = maps:get(activities, R),
        case lists:search(fun(P) ->
            maps:get(activities, P) =:= RActs
        end, Precedence) of
            {value, _} ->
                {true, R#{
                    template => succession,
                    confidence => min(
                        maps:get(confidence, R),
                        get_precedence_confidence(RActs, Precedence)
                    )
                }};
            false ->
                false
        end
    end, Response).

%% @doc Discover co-existence constraints (A <-> B).
-spec discover_coexistence(event_log(), [activity()]) -> [constraint()].
discover_coexistence(Log, Activities) ->
    Pairs = [{A, B} || A <- Activities, B <- Activities, A < B],

    lists:filtermap(fun({A, B}) ->
        %% Co-existence: A and B always occur together
        {ABOnly, BAOnly, Both, Neither} = analyze_coexistence(A, B, Log),

        Total = length(Log),
        Support = Both / Total,

        %% High support for both occurring together
        Confidence = case Both + ABOnly + BAOnly of
            0 -> 0.0;
            N -> Both / N
        end,

        case Support >= 0.3 andalso Confidence >= 0.7 of
            true ->
                {true, #{
                    template => coexistence,
                    activities => [A, B],
                    parameters => #{},
                    support => Support,
                    confidence => Confidence
                }};
            false ->
                false
        end
    end, Pairs).

%% @doc Discover choice constraints (A xor B).
-spec discover_choice(event_log(), [activity()]) -> [constraint()].
discover_choice(Log, Activities) ->
    Pairs = [{A, B} || A <- Activities, B <- Activities, A < B],

    lists:filtermap(fun({A, B}) ->
        %% Choice: either A or B occurs, but rarely both
        TracesA = count_traces_with(A, Log),
        TracesB = count_traces_with(B, Log),
        TracesBoth = count_traces_with_both(A, B, Log),

        Total = length(Log),
        Support = (TracesA + TracesB - TracesBoth) / Total,

        %% Low co-occurrence indicates mutual exclusivity
        Confidence = case TracesA + TracesB of
            0 -> 0.0;
            N -> 1.0 - (2 * TracesBoth / N)
        end,

        case Support >= 0.3 andalso Confidence >= 0.7 of
            true ->
                {true, #{
                    template => choice,
                    activities => [A, B],
                    parameters => #{},
                    support => Support,
                    confidence => Confidence
                }};
            false ->
                false
        end
    end, Pairs).

%%====================================================================
%% Constraint Evaluation Functions
%%====================================================================

%% @doc Evaluate a constraint on a log.
-spec evaluate_constraint(constraint(), event_log()) -> {ok, float()}.
evaluate_constraint(Constraint, Log) ->
    {ok, Result} = test_constraint(Constraint, Log),
    {ok, maps:get(confidence, Result)}.

%% @doc Get support for a constraint.
-spec get_support(constraint(), event_log()) -> float().
get_support(Constraint, Log) ->
    Template = maps:get(template, Constraint),
    Activities = maps:get(activities, Constraint),

    Support = case Template of
        existence ->
            [A] = Activities,
            count_traces_with(A, Log) / length(Log);
        response ->
            [A, B] = Activities,
            {Violations, Total} = analyze_response(A, B, Log),
            case Total of
                0 -> 0.0;
                N -> (N - Violations) / N
            end;
        precedence ->
            [A, B] = Activities,
            {Violations, Total} = analyze_precedence(A, B, Log),
            case Total of
                0 -> 0.0;
                N -> (N - Violations) / N
            end;
        _ ->
            0.5  %% Default
    end,
    Support.

%% @doc Get confidence for a constraint.
-spec get_confidence(constraint(), event_log()) -> float().
get_confidence(Constraint, Log) ->
    maps:get(confidence, Constraint, compute_confidence(Constraint, Log)).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec extract_activities(event_log()) -> [activity()].
extract_activities(Log) ->
    lists:usort(lists:flatten(Log)).

%% @private
-spec count_activity_occurrences(activity(), event_log()) -> float().
count_activity_occurrences(Activity, Log) ->
    lists:foldl(fun(Trace, Acc) ->
        case lists:member(Activity, Trace) of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Log).

%% @private
-spec count_traces_with(activity(), event_log()) -> float().
count_traces_with(Activity, Log) ->
    lists:foldl(fun(Trace, Acc) ->
        case lists:member(Activity, Trace) of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Log).

%% @private
-spec count_traces_with_both(activity(), activity(), event_log()) -> float().
count_traces_with_both(A, B, Log) ->
    lists:foldl(fun(Trace, Acc) ->
        HasA = lists:member(A, Trace),
        HasB = lists:member(B, Trace),
        case HasA andalso HasB of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, Log).

%% @private
-spec analyze_response(activity(), activity(), event_log()) ->
    {Violations :: non_neg_integer(), TotalWithA :: non_neg_integer()}.
analyze_response(A, B, Log) ->
    lists:foldl(fun(Trace, {Violations, Total}) ->
        case lists:member(A, Trace) of
            true ->
                %% Check if B occurs after A
                {_, RestAfterA} = lists:splitwith(fun(X) -> X =/= A end, Trace),
                HasB = lists:member(B, RestAfterA),
                Total1 = Total + 1,
                case HasB of
                    true -> {Violations, Total1};
                    false -> {Violations + 1, Total1}
                end;
            false ->
                {Violations, Total}
        end
    end, {0, 0}, Log).

%% @private
-spec analyze_precedence(activity(), activity(), event_log()) ->
    {Violations :: non_neg_integer(), TotalWithB :: non_neg_integer()}.
analyze_precedence(A, B, Log) ->
    lists:foldl(fun(Trace, {Violations, Total}) ->
        case lists:member(B, Trace) of
            true ->
                %% Check if A occurs before B
                {BeforeB, _} = lists:splitwith(fun(X) -> X =/= B end, Trace),
                HasA = lists:member(A, BeforeB),
                Total1 = Total + 1,
                case HasA of
                    true -> {Violations, Total1};
                    false -> {Violations + 1, Total1}
                end;
            false ->
                {Violations, Total}
        end
    end, {0, 0}, Log).

%% @private
-spec analyze_coexistence(activity(), activity(), event_log()) ->
    {ABOnly :: non_neg_integer(), BAOnly :: non_neg_integer(),
     Both :: non_neg_integer(), Neither :: non_neg_integer()}.
analyze_coexistence(A, B, Log) ->
    lists:foldl(fun(Trace, {ABOnly, BAOnly, Both, Neither}) ->
        HasA = lists:member(A, Trace),
        HasB = lists:member(B, Trace),
        case {HasA, HasB} of
            {true, true} -> {ABOnly, BAOnly, Both + 1, Neither};
            {true, false} -> {ABOnly + 1, BAOnly, Both, Neither};
            {false, true} -> {ABOnly, BAOnly + 1, Both, Neither};
            {false, false} -> {ABOnly, BAOnly, Both, Neither + 1}
        end
    end, {0, 0, 0, 0}, Log).

%% @private
-spec compute_response_confidence(activity(), activity(), event_log()) -> float().
compute_response_confidence(A, B, Log) ->
    TracesWithA = count_traces_with(A, Log),
    TracesWithBoth = count_traces_with_both(A, B, Log),

    case TracesWithA of
        0 -> 0.0;
        N -> TracesWithBoth / N
    end.

%% @private
-spec compute_precedence_confidence(activity(), activity(), event_log()) -> float().
compute_precedence_confidence(A, B, Log) ->
    TracesWithB = count_traces_with(B, Log),
    TracesWithBoth = count_traces_with_both(A, B, Log),

    case TracesWithB of
        0 -> 0.0;
        N -> TracesWithBoth / N
    end.

%% @private
-spec get_precedence_confidence([activity()], [constraint()]) -> float().
get_precedence_confidence(Activities, Precedences) ->
    case lists:search(fun(P) ->
        maps:get(activities, P) =:= Activities
    end, Precedences) of
        {value, P} -> maps:get(confidence, P);
        false -> 0.0
    end.

%% @private
-spec compute_confidence(constraint(), event_log()) -> float().
compute_confidence(Constraint, Log) ->
    Template = maps:get(template, Constraint),
    Activities = maps:get(activities, Constraint),

    case Activities of
        [A, B] when Template =:= response ->
            compute_response_confidence(A, B, Log);
        [A, B] when Template =:= precedence ->
            compute_precedence_confidence(A, B, Log);
        _ ->
            0.5
    end.

%% @private
-spec filter_constraints([constraint()], float(), float(),
                         pos_integer() | infinity) -> [constraint()].
filter_constraints(Constraints, MinSupport, MinConfidence, MaxConstraints) ->
    Filtered = lists:filter(fun(C) ->
        maps:get(support, C) >= MinSupport andalso
        maps:get(confidence, C) >= MinConfidence
    end, Constraints),

    %% Sort by support * confidence
    Sorted = lists:sort(fun(C1, C2) ->
        Score1 = maps:get(support, C1) * maps:get(confidence, C1),
        Score2 = maps:get(support, C2) * maps:get(confidence, C2),
        Score1 >= Score2
    end, Filtered),

    case MaxConstraints of
        infinity -> Sorted;
        N -> lists:sublist(Sorted, N)
    end.

%% @private
-spec find_violations(constraint(), event_log()) -> [trace()].
find_violations(Constraint, Log) ->
    Template = maps:get(template, Constraint),
    Activities = maps:get(activities, Constraint),

    lists:filter(fun(Trace) ->
        case Template of
            existence ->
                [A] = Activities,
                not lists:member(A, Trace);
            response ->
                [A, B] = Activities,
                lists:member(A, Trace) andalso not lists:member(B, Trace);
            precedence ->
                [A, B] = Activities,
                lists:member(B, Trace) andalso not lists:member(A, Trace);
            _ ->
                false
        end
    end, Log).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test data
%%--------------------------------------------------------------------

simple_log() ->
    [[a, b, c, d], [a, c, b, d], [a, b, c, e, d]].

log_with_response() ->
    [[a, b], [a, c, b], [a, d, b]].

log_with_precedence() ->
    [[a, b], [c, a, b], [a, d, b]].

log_with_choice() ->
    [[a, c], [b, c], [a, d], [b, d]].

%%--------------------------------------------------------------------
%% Discovery tests
%%--------------------------------------------------------------------

discover_constraints_test() ->
    Log = simple_log(),
    {ok, Constraints} = discover_constraints(Log),
    ?assert(is_list(Constraints)),
    ?assert(length(Constraints) > 0).

discover_constraints_with_options_test() ->
    Log = simple_log(),
    {ok, Constraints} = discover_constraints(Log, #{min_support => 0.5}),
    ?assert(is_list(Constraints)).

%%--------------------------------------------------------------------
%% Existence constraint tests
%%--------------------------------------------------------------------

discover_existence_test() ->
    Log = simple_log(),
    Activities = [a, b, c, d, e],
    Constraints = discover_existence(Log, Activities),
    ?assert(length(Constraints) > 0),
    lists:foreach(fun(C) ->
        ?assertEqual(existence, maps:get(template, C))
    end, Constraints).

%%--------------------------------------------------------------------
%% Response constraint tests
%%--------------------------------------------------------------------

discover_response_test() ->
    Log = log_with_response(),
    Activities = [a, b, c, d],
    Constraints = discover_response(Log, Activities),
    ?assert(is_list(Constraints)),
    %% Should find a ->> b
    HasABResponse = lists:any(fun(C) ->
        maps:get(template, C) =:= response andalso
        maps:get(activities, C) =:= [a, b]
    end, Constraints),
    ?assert(HasABResponse).

analyze_response_test() ->
    Log = [[a, b], [a], [a, c, b]],
    {Violations, Total} = analyze_response(a, b, Log),
    ?assertEqual(1, Violations),  %% Second trace has a but no b
    ?assertEqual(3, Total).

%%--------------------------------------------------------------------
%% Precedence constraint tests
%%--------------------------------------------------------------------

discover_precedence_test() ->
    Log = log_with_precedence(),
    Activities = [a, b, c, d],
    Constraints = discover_precedence(Log, Activities),
    ?assert(is_list(Constraints)).

analyze_precedence_test() ->
    Log = [[a, b], [b], [c, a, b]],
    {Violations, Total} = analyze_precedence(a, b, Log),
    ?assertEqual(1, Violations),  %% Second trace has b without a
    ?assertEqual(3, Total).

%%--------------------------------------------------------------------
%% Succession constraint tests
%%--------------------------------------------------------------------

discover_succession_test() ->
    Log = [[a, b, c], [a, c, b], [b, a, c]],
    Activities = [a, b, c],
    Constraints = discover_succession(Log, Activities),
    ?assert(is_list(Constraints)).

%%--------------------------------------------------------------------
%% Co-existence constraint tests
%%--------------------------------------------------------------------

discover_coexistence_test() ->
    Log = [[a, b], [b, c], [a, b, c]],
    Activities = [a, b, c],
    Constraints = discover_coexistence(Log, Activities),
    ?assert(is_list(Constraints)).

analyze_coexistence_test() ->
    Log = [[a, b], [a], [b], []],
    {ABOnly, BAOnly, Both, Neither} = analyze_coexistence(a, b, Log),
    ?assertEqual(1, ABOnly),
    ?assertEqual(1, BAOnly),
    ?assertEqual(1, Both),
    ?assertEqual(1, Neither).

%%--------------------------------------------------------------------
%% Choice constraint tests
%%--------------------------------------------------------------------

discover_choice_test() ->
    Log = log_with_choice(),
    Activities = [a, b, c, d],
    Constraints = discover_choice(Log, Activities),
    ?assert(is_list(Constraints)),
    %% Should find choice between a and b
    HasABChoice = lists:any(fun(C) ->
        maps:get(template, C) =:= choice andalso
        maps:get(activities, C) =:= [a, b]
    end, Constraints),
    ?assert(HasABChoice).

%%--------------------------------------------------------------------
%% Constraint evaluation tests
%%--------------------------------------------------------------------

test_constraint_test() ->
    Log = simple_log(),
    Constraint = #{
        template => existence,
        activities => [a],
        parameters => #{min_count => 1},
        support => 1.0,
        confidence => 1.0
    },
    {ok, Result} = test_constraint(Constraint, Log),
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
    Support = get_support(Constraint, Log),
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
    Confidence = get_confidence(Constraint, Log),
    ?assert(is_float(Confidence)).

%%--------------------------------------------------------------------
%% Template tests
%%--------------------------------------------------------------------

get_constraint_templates_test() ->
    Templates = get_constraint_templates(),
    ?assert(is_list(Templates)),
    ?assert(lists:member(existence, Templates)),
    ?assert(lists:member(response, Templates)),
    ?assert(lists:member(precedence, Templates)).

%%--------------------------------------------------------------------
%% Filtering tests
%%--------------------------------------------------------------------

filter_constraints_test() ->
    Constraints = [
        #{template => existence, activities => [a], support => 0.9, confidence => 0.8},
        #{template => response, activities => [a, b], support => 0.4, confidence => 0.6},
        #{template => precedence, activities => [a, b], support => 0.7, confidence => 0.9}
    ],
    Filtered = filter_constraints(Constraints, 0.5, 0.7, infinity),
    ?assertEqual(2, length(Filtered)).

filter_constraints_max_test() ->
    Constraints = [
        #{template => existence, activities => [a], support => 0.9, confidence => 0.8},
        #{template => response, activities => [a, b], support => 0.4, confidence => 0.6},
        #{template => precedence, activities => [a, b], support => 0.7, confidence => 0.9}
    ],
    Filtered = filter_constraints(Constraints, 0.0, 0.0, 2),
    ?assertEqual(2, length(Filtered)).

%%--------------------------------------------------------------------
%% Integration tests
%%--------------------------------------------------------------------

discover_full_pipeline_test() ->
    Log = [
        [a, b, c, d],
        [a, c, b, d],
        [a, b, c, e, d],
        [b, a, c, d]
    ],
    {ok, Constraints} = discover_constraints(Log, #{
        min_support => 0.3,
        min_confidence => 0.5
    }),
    ?assert(length(Constraints) > 0),
    %% Verify all constraints meet thresholds
    lists:foreach(fun(C) ->
        ?assert(maps:get(support, C) >= 0.3),
        ?assert(maps:get(confidence, C) >= 0.5)
    end, Constraints).

-endif.
