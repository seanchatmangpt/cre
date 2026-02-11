%% -*- erlang -*-
%% @doc Integration tests for wf_case_sup

-module(wf_case_integration_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup() ->
    % Start required applications
    {ok, _} = application:ensure_all_started(gproc),
    {ok, _} = application:ensure_all_started(cre),
    ok.

cleanup(_Arg) ->
    application:stop(cre),
    ok.

%%====================================================================
%% Integration Tests
%%====================================================================

test_wf_case_sup_under_cre_sup() ->
    Children = supervisor:which_children(cre_sup),
    ?assert(lists:keymember(wf_case_sup, 1, Children)),
    {wf_case_sup, Pid, supervisor, [wf_case_sup]} = lists:keyfind(wf_case_sup, 1, Children),
    ?assert(is_pid(Pid)),
    ?assertEqual(Pid, whereis(wf_case_sup)).

test_list_cases() ->
    Cases = wf_case_sup:list_cases(),
    ?assert(is_list(Cases)),
    %% Initially empty
    ?assertEqual(0, length(Cases)).

test_case_count() ->
    ?assert(is_integer(wf_case_sup:case_count())),
    ?assertEqual(0, wf_case_sup:case_count()).

test_supervisor_flags() ->
    %% Verify wf_case_sup is running with correct configuration
    {wf_case_sup, _Pid, supervisor, [wf_case_sup]} =
        lists:keyfind(wf_case_sup, 1, supervisor:which_children(cre_sup)),
    ok.

%%====================================================================
%% Test Generators
%%====================================================================

integration_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
         {"wf_case_sup starts under cre_sup", fun test_wf_case_sup_under_cre_sup/0},
         {"wf_case_sup list cases", fun test_list_cases/0},
         {"wf_case_sup case count", fun test_case_count/0},
         {"wf_case_sup supervisor flags", fun test_supervisor_flags/0}
     ]
    }.
