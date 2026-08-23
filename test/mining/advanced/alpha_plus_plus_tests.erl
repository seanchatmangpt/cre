%% -*- erlang -*-
%% @doc Tests for Alpha+++ Miner (invisible tasks)

-module(alpha_plus_plus_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

simple_log() ->
    [[a, b, c, d]].

log_with_invisible() ->
    [[a, b, d], [a, b, c, d]].

log_with_loops() ->
    [[a, b, b, c], [a, b, c]].

log_with_short_loops() ->
    [[a, b, a, c], [a, b, c]].

comprehensive_log() ->
    [
        [a, b, c, d],
        [a, b, b, c, d],
        [a, c, b, d]
    ].

%%====================================================================
%% Basic Mining Tests
%%====================================================================

mine_simple_log_test() ->
    Log = simple_log(),
    {ok, Net} = alpha_plus_plus:mine(Log),
    ?assert(is_map(maps:get(places, Net))),
    ?assert(is_list(maps:get(transitions, Net))),
    ?assertEqual(i_source, maps:get(initial_place, Net)),
    ?assertEqual(o_sink, maps:get(final_place, Net)).

mine_with_options_test() ->
    Log = simple_log(),
    {ok, Net} = alpha_plus_plus:mine(Log, #{min_frequency => 0.2}),
    ?assert(is_map(Net)).

%%====================================================================
%% Invisible Task Detection Tests
%%====================================================================

detect_invisible_tasks_empty_test() ->
    Log = [[a]],
    Invisible = alpha_plus_plus:detect_invisible_tasks(Log),
    ?assert(is_list(Invisible)).

detect_invisible_tasks_gap_test() ->
    Log = log_with_invisible(),
    Invisible = alpha_plus_plus:detect_invisible_tasks(Log),
    ?assert(is_list(Invisible)).

detect_invisible_tasks_loop_test() ->
    Log = [[a, b, a, c]],
    Invisible = alpha_plus_plus:detect_invisible_tasks(Log),
    ?assert(is_list(Invisible)).

%%====================================================================
%% Loop Detection Tests
%%====================================================================

detect_loops_empty_test() ->
    Log = [[a, b, c]],
    Loops = alpha_plus_plus:detect_loops(Log),
    ?assertEqual([], Loops).

detect_loops_one_length_test() ->
    Log = log_with_loops(),
    Loops = alpha_plus_plus:detect_loops(Log),
    ?assert(is_list(Loops)),
    HasSelfLoop = lists:any(fun(L) ->
        maps:get(type, L, undefined) =:= one_length
    end, Loops),
    ?assert(HasSelfLoop).

detect_loops_two_length_test() ->
    Log = log_with_short_loops(),
    Loops = alpha_plus_plus:detect_loops(Log),
    ?assert(is_list(Loops)).

%%====================================================================
%% WF-net Structure Tests
%%====================================================================

wf_net_extended_has_invisible_test() ->
    Log = log_with_invisible(),
    {ok, Net} = alpha_plus_plus:mine(Log),
    ?assert(maps:is_key(invisible_tasks, Net)),
    ?assert(is_list(maps:get(invisible_tasks, Net))).

wf_net_extended_has_loops_test() ->
    Log = log_with_loops(),
    {ok, Net} = alpha_plus_plus:mine(Log),
    ?assert(maps:is_key(loops, Net)),
    ?assert(is_list(maps:get(loops, Net))).

%%====================================================================
%% Frequency Threshold Tests
%%====================================================================

set_frequency_threshold_test() ->
    ?assertEqual(ok, alpha_plus_plus:set_frequency_threshold(0.5)).

%%====================================================================
%% Integration Tests
%%====================================================================

mine_comprehensive_test() ->
    Log = comprehensive_log(),
    {ok, Net} = alpha_plus_plus:mine(Log),
    ?assert(is_map(Net)),
    ?assert(maps:is_key(places, Net)),
    ?assert(maps:is_key(transitions, Net)),
    ?assert(maps:is_key(arcs, Net)).

mine_with_invisible_and_loops_test() ->
    Log = [[a, b, b, c, d], [a, b, d], [a, b, a, c]],
    {ok, Net} = alpha_plus_plus:mine(Log),
    ?assert(maps:is_key(invisible_tasks, Net)),
    ?assert(maps:is_key(loops, Net)).
