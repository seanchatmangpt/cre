%% -*- erlang -*-
%% @doc Unit tests for wf_case_sup

-module(wf_case_sup_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Cases
%%====================================================================

wf_case_sup_init_test() ->
    % Test that init/1 returns proper supervisor flags
    {ok, {SupFlags, [ChildSpec]}} = wf_case_sup:init([]),
    ?assertEqual(simple_one_for_one, maps:get(strategy, SupFlags)),
    ?assertEqual(10, maps:get(intensity, SupFlags)),
    ?assertEqual(60, maps:get(period, SupFlags)),
    ?assertEqual(case_instance, maps:get(id, ChildSpec)),
    ?assertEqual(temporary, maps:get(restart, ChildSpec)),
    ?assertEqual(worker, maps:get(type, ChildSpec)).

wf_case_sup_child_spec_test() ->
    % Test child spec structure
    {ok, {_, [ChildSpec]}} = wf_case_sup:init([]),
    ?assert(maps:is_key(id, ChildSpec)),
    ?assert(maps:is_key(start, ChildSpec)),
    ?assert(maps:is_key(restart, ChildSpec)),
    ?assert(maps:is_key(shutdown, ChildSpec)),
    ?assert(maps:is_key(type, ChildSpec)),
    ?assert(maps:is_key(modules, ChildSpec)).

wf_case_sup_api_exports_test() ->
    % Test that all API functions are exported
    Exports = proplists:get_value(exports, wf_case_sup:module_info()),
    ?assert(lists:keymember(start_link, 1, Exports)),
    ?assert(lists:keymember(start_case, 1, Exports)),
    ?assert(lists:keymember(stop_case, 1, Exports)),
    ?assert(lists:keymember(list_cases, 1, Exports)),
    ?assert(lists:keymember(find_case, 1, Exports)),
    ?assert(lists:keymember(get_case_status, 1, Exports)),
    ?assert(lists:keymember(case_count, 1, Exports)).
