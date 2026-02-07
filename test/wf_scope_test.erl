%% -*- erlang -*-
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

-module(wf_scope_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% EUnit Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test bindings/2 returns mapping for known scope.
%%--------------------------------------------------------------------
bindings_known_scope_test() ->
    BT = #{s1 => #{pin => cin, pout => cout}},
    ?assertEqual(#{pin => cin, pout => cout}, wf_scope:bindings(BT, s1)).

%%--------------------------------------------------------------------
%% @doc Test bindings/2 returns error for unknown scope.
%%--------------------------------------------------------------------
bindings_unknown_scope_test() ->
    BT = #{s1 => #{pin => cin, pout => cout}},
    ?assertEqual({error, unknown_scope}, wf_scope:bindings(BT, missing)).

%%--------------------------------------------------------------------
%% @doc Test enter/3 translates parent keys to child keys.
%%--------------------------------------------------------------------
enter_translation_test() ->
    BT = #{s1 => #{pin => cin, pout => cout}},
    Parent = #{pin => [a, b]},
    ?assertEqual(#{cin => [a, b]}, wf_scope:enter(BT, s1, Parent)).

%%--------------------------------------------------------------------
%% @doc Test leave/3 translates child keys to parent keys.
%%--------------------------------------------------------------------
leave_translation_test() ->
    BT = #{s1 => #{pin => cin, pout => cout}},
    Child = #{cout => [ok]},
    ?assertEqual(#{pout => [ok]}, wf_scope:leave(BT, s1, Child)).

%%--------------------------------------------------------------------
%% @doc Test unknown scope returns identity mapping (enter).
%%--------------------------------------------------------------------
enter_unknown_scope_identity_test() ->
    BT = #{s1 => #{pin => cin, pout => cout}},
    Input = #{p1 => [x]},
    ?assertEqual(Input, wf_scope:enter(BT, missing, Input)).

%%--------------------------------------------------------------------
%% @doc Test unknown scope returns identity mapping (leave).
%%--------------------------------------------------------------------
leave_unknown_scope_identity_test() ->
    BT = #{s1 => #{pin => cin, pout => cout}},
    Input = #{p1 => [x]},
    ?assertEqual(Input, wf_scope:leave(BT, missing, Input)).

%%--------------------------------------------------------------------
%% @doc Test enter/3 with empty marking.
%%--------------------------------------------------------------------
enter_empty_marking_test() ->
    BT = #{s1 => #{pin => cin}},
    ?assertEqual(#{}, wf_scope:enter(BT, s1, #{})).

%%--------------------------------------------------------------------
%% @doc Test leave/3 with empty marking.
%%--------------------------------------------------------------------
leave_empty_marking_test() ->
    BT = #{s1 => #{pin => cin}},
    ?assertEqual(#{}, wf_scope:leave(BT, s1, #{})).

%%--------------------------------------------------------------------
%% @doc Test enter/3 with list input (edge case).
%%--------------------------------------------------------------------
enter_list_input_test() ->
    BT = #{s1 => #{pin => cin}},
    ?assertEqual(#{p1 => []}, wf_scope:enter(BT, missing, [p1])).

%%--------------------------------------------------------------------
%% @doc Test leave/3 with list input (edge case).
%%--------------------------------------------------------------------
leave_list_input_test() ->
    BT = #{s1 => #{pin => cin}},
    ?assertEqual(#{p2 => []}, wf_scope:leave(BT, missing, [p2])).

%%--------------------------------------------------------------------
%% @doc Test that enter/3 preserves unmapped places.
%%--------------------------------------------------------------------
enter_preserves_unmapped_test() ->
    BT = #{scope => #{a => x, b => y, c => z}},
    Input = #{a => [1], b => [2], d => [4]},
    ?assertEqual(#{x => [1], y => [2], d => [4]}, wf_scope:enter(BT, scope, Input)).

%%--------------------------------------------------------------------
%% @doc Test that leave/3 preserves unmapped places.
%%--------------------------------------------------------------------
leave_preserves_unmapped_test() ->
    BT = #{scope => #{a => x, b => y}},
    Input = #{x => [1], y => [2], z => [3]},
    ?assertEqual(#{a => [1], b => [2], z => [3]}, wf_scope:leave(BT, scope, Input)).

%%--------------------------------------------------------------------
%% @doc Test empty binding table (enter).
%%--------------------------------------------------------------------
enter_empty_binding_table_test() ->
    BT = #{},
    Input = #{a => [1]},
    ?assertEqual(Input, wf_scope:enter(BT, any, Input)).

%%--------------------------------------------------------------------
%% @doc Test empty binding table (leave).
%%--------------------------------------------------------------------
leave_empty_binding_table_test() ->
    BT = #{},
    Input = #{a => [1]},
    ?assertEqual(Input, wf_scope:leave(BT, any, Input)).

%%--------------------------------------------------------------------
%% @doc Doctest example from moduledoc - all together.
%%--------------------------------------------------------------------
doctest_combined_test() ->
    BT = #{s1 => #{pin => cin, pout => cout}},
    ?assertEqual(#{pin => cin, pout => cout}, wf_scope:bindings(BT, s1)),
    ?assertEqual({error, unknown_scope}, wf_scope:bindings(BT, missing)),
    ?assertEqual(#{cin => [a, b]}, wf_scope:enter(BT, s1, #{pin => [a, b]})),
    ?assertEqual(#{pout => [ok]}, wf_scope:leave(BT, s1, #{cout => [ok]})),
    ?assertEqual(#{p1 => [x]}, wf_scope:enter(BT, missing, #{p1 => [x]})).
