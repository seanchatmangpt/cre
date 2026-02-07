%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @doc Common Test Suite for pnet_choice Coverage
%%
%% Comprehensive test suite for pnet_choice module targeting 80% coverage.
%%
%% @end
%% -------------------------------------------------------------------

-module(pnet_choice_coverage_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Suite Callbacks
%%====================================================================

suite() ->
    [{timetrap, {seconds, 10}}].

all() ->
    [
        seed_returns_valid_state,
        pick_returns_list_element,
        pick_deterministic_same_seed,
        pick_empty_list_returns_error,
        pick_weighted_respects_weights,
        pick_weighted_zero_total_error,
        pick_weighted_empty_error,
        pick_weighted_invalid_weight_error,
        pick_weighted_deterministic_same_seed,
        pick_advances_state,
        pick_sequence,
        pick_weighted_single_element,
        pick_weighted_zero_allowed
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

seed_returns_valid_state(_Config) ->
    State = pnet_choice:seed(42),
    %% Verify state is valid by using it
    {Element, _NewState} = pnet_choice:pick([a, b], State),
    true = lists:member(Element, [a, b]),
    ok.

pick_returns_list_element(_Config) ->
    State0 = pnet_choice:seed(123),
    {Element, _State1} = pnet_choice:pick([a, b, c], State0),
    true = lists:member(Element, [a, b, c]),
    ok.

pick_deterministic_same_seed(_Config) ->
    State0a = pnet_choice:seed(42),
    State0b = pnet_choice:seed(42),
    {X1, _} = pnet_choice:pick([a, b, c], State0a),
    {X2, _} = pnet_choice:pick([a, b, c], State0b),
    ?assertEqual(X1, X2),
    ok.

pick_empty_list_returns_error(_Config) ->
    State0 = pnet_choice:seed(1),
    ?assertEqual({error, empty}, pnet_choice:pick([], State0)),
    ok.

pick_weighted_respects_weights(_Config) ->
    State0 = pnet_choice:seed(9),
    {Element, _State1} = pnet_choice:pick_weighted([{a, 1}, {b, 3}, {c, 1}], State0),
    true = lists:member(Element, [a, b, c]),
    ok.

pick_weighted_zero_total_error(_Config) ->
    State0 = pnet_choice:seed(1),
    ?assertEqual({error, bad_weights}, pnet_choice:pick_weighted([{a, 0}], State0)),
    ?assertEqual({error, bad_weights}, pnet_choice:pick_weighted([{a, 0}, {b, 0}], State0)),
    ok.

pick_weighted_empty_error(_Config) ->
    State0 = pnet_choice:seed(1),
    ?assertEqual({error, empty}, pnet_choice:pick_weighted([], State0)),
    ok.

pick_weighted_invalid_weight_error(_Config) ->
    State0 = pnet_choice:seed(1),
    ?assertEqual({error, bad_weights}, pnet_choice:pick_weighted([{a, -1}], State0)),
    ?assertEqual({error, bad_weights}, pnet_choice:pick_weighted([{a, 1.5}], State0)),
    ok.

pick_weighted_deterministic_same_seed(_Config) ->
    State0a = pnet_choice:seed(7),
    State0b = pnet_choice:seed(7),
    {X1, _} = pnet_choice:pick_weighted([{a, 1}, {b, 2}, {c, 3}], State0a),
    {X2, _} = pnet_choice:pick_weighted([{a, 1}, {b, 2}, {c, 3}], State0b),
    ?assertEqual(X1, X2),
    ok.

pick_advances_state(_Config) ->
    State0 = pnet_choice:seed(42),
    {_Element, State1} = pnet_choice:pick([a, b, c], State0),
    ?assertNotEqual(State0, State1),
    ok.

pick_sequence(_Config) ->
    State0 = pnet_choice:seed(123),
    {X1, S1} = pnet_choice:pick([a, b, c], State0),
    {X2, S2} = pnet_choice:pick([a, b, c], S1),
    {X3, _S3} = pnet_choice:pick([a, b, c], S2),
    true = lists:member(X1, [a, b, c]),
    true = lists:member(X2, [a, b, c]),
    true = lists:member(X3, [a, b, c]),
    %% Verify determinism
    State0a = pnet_choice:seed(123),
    {Y1, S1a} = pnet_choice:pick([a, b, c], State0a),
    {Y2, S2a} = pnet_choice:pick([a, b, c], S1a),
    {Y3, _S3a} = pnet_choice:pick([a, b, c], S2a),
    ?assertEqual(X1, Y1),
    ?assertEqual(X2, Y2),
    ?assertEqual(X3, Y3),
    ok.

pick_weighted_single_element(_Config) ->
    State0 = pnet_choice:seed(1),
    {Element, _State1} = pnet_choice:pick_weighted([{only, 100}], State0),
    ?assertEqual(only, Element),
    ok.

pick_weighted_zero_allowed(_Config) ->
    State0 = pnet_choice:seed(5),
    {Element, _State1} = pnet_choice:pick_weighted([{a, 0}, {b, 1}, {c, 0}], State0),
    ?assertEqual(b, Element),
    ok.
