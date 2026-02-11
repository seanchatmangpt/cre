%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Project
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
%% @doc Tests for CRE HTTP Handler
%%
%% Unit tests for the REST API workflow management endpoints.
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_http_handler_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% Simple test workflow module for testing
-module(test_workflow).
-behaviour(gen_yawl).

-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3, fire/3]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

place_lst() -> [p1, p2].
trsn_lst() -> [t1].
init_marking(p1, _) -> [token];
init_marking(_, _) -> [].
preset(t1) -> [p1].
is_enabled(t1, _Mode, _UsrInfo) -> true.
fire(t1, _Mode, UsrInfo) -> {produce, #{p2 => [token]}, UsrInfo}.

init(Args) -> Args.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Req, _From, State) -> {reply, ok}.
handle_cast(_Req, State) -> noreply.
handle_info(_Info, State) -> noreply.

%%====================================================================
%% Unit Tests
%%====================================================================

validate_create_request_test() ->
    %% Valid request
    ValidBody = #{
        <<"workflow_module">> => <<"gen_yawl">>,
        <<"case_id">> => <<"test-001">>,
        <<"init_args">> => #{},
        <<"options">> => []
    },
    ?assertMatch({ok, gen_yawl, <<"test-001">>, #{}, []},
                 cre_http_handler:validate_create_request(ValidBody)).

validate_create_request_missing_field_test() ->
    %% Missing workflow_module
    InvalidBody = #{
        <<"case_id">> => <<"test-001">>
    },
    ?assertMatch({error, _}, cre_http_handler:validate_create_request(InvalidBody)).

validate_create_request_invalid_module_test() ->
    %% Non-existent module
    InvalidBody = #{
        <<"workflow_module">> => <<"nonexistent_module_xyz">>,
        <<"case_id">> => <<"test-001">>
    },
    ?assertMatch({error, _}, cre_http_handler:validate_create_request(InvalidBody)).

encode_term_test() ->
    %% Atoms
    ?assertEqual(<<"test">>, cre_http_handler:encode_term(test)),

    %% Maps
    ?assertEqual(#{<<"key">> => <<"value">>},
                 cre_http_handler:encode_term(#{key => value})),

    %% Lists
    ?assertEqual([1, 2, 3], cre_http_handler:encode_term([1, 2, 3])),

    %% Strings
    ?assertEqual(<<"hello">>, cre_http_handler:encode_term("hello")).

%%====================================================================
%% Integration Tests (require running system)
%%====================================================================

%% These tests are commented out as they require:
%% - CRE application running
%% - yawl_registry started
%% - yawl_workflow_supervisor started

%% integration_create_workflow_test_() ->
%%     {setup,
%%      fun() ->
%%          %% Setup: start CRE application
%%          cre:start(),
%%          ok
%%      end,
%%      fun(_) ->
%%          %% Cleanup: stop workflows
%%          lists:foreach(
%%              fun({CaseId, Pid}) ->
%%                  yawl_workflow_supervisor:stop_workflow(Pid),
%%                  yawl_registry:unregister(CaseId)
%%              end,
%%              yawl_registry:list())
%%      end,
%%      [
%%          {"Create workflow", fun test_create_workflow/0},
%%          {"Get workflow status", fun test_get_workflow_status/0},
%%          {"List workflows", fun test_list_workflows/0},
%%          {"Stop workflow", fun test_stop_workflow/0}
%%      ]}.

%% test_create_workflow() ->
%%     Request = #{
%%         <<"workflow_module">> => <<"test_workflow">>,
%%         <<"case_id">> => <<"test-create-001">>,
%%         <<"init_args">> => #{},
%%         <<"options">> => []
%%     },
%%     {ok, Response} = cre_http_handler:handle_request(<<"POST">>, [], Request),
%%     ?assertMatch(#{status := created, case_id := <<"test-create-001">>}, Response).

%% test_get_workflow_status() ->
%%     %% First create a workflow
%%     CaseId = <<"test-status-001">>,
%%     Request = #{
%%         <<"workflow_module">> => <<"test_workflow">>,
%%         <<"case_id">> => CaseId,
%%         <<"init_args">> => #{},
%%         <<"options">> => []
%%     },
%%     {ok, _} = cre_http_handler:handle_request(<<"POST">>, [], Request),
%%
%%     %% Now get its status
%%     {ok, StatusResponse} = cre_http_handler:handle_request(<<"GET">>, [CaseId], #{}),
%%     ?assertMatch(#{status := running, case_id := CaseId}, StatusResponse).

%% test_list_workflows() ->
%%     %% Create multiple workflows
%%     lists:foreach(
%%         fun(N) ->
%%             CaseId = list_to_binary(io_lib:format("test-list-~3..0B", [N])),
%%             Request = #{
%%                 <<"workflow_module">> => <<"test_workflow">>,
%%                 <<"case_id">> => CaseId,
%%                 <<"init_args">> => #{},
%%                 <<"options">> => []
%%             },
%%             {ok, _} = cre_http_handler:handle_request(<<"POST">>, [], Request)
%%         end,
%%         lists:seq(1, 3)),
%%
%%     %% List all workflows
%%     {ok, ListResponse} = cre_http_handler:handle_request(<<"GET">>, [], #{}),
%%     ?assertMatch(#{status := ok, count := Count} when Count >= 3, ListResponse).

%% test_stop_workflow() ->
%%     %% Create a workflow
%%     CaseId = <<"test-stop-001">>,
%%     Request = #{
%%         <<"workflow_module">> => <<"test_workflow">>,
%%         <<"case_id">> => CaseId,
%%         <<"init_args">> => #{},
%%         <<"options">> => []
%%     },
%%     {ok, _} = cre_http_handler:handle_request(<<"POST">>, [], Request),
%%
%%     %% Stop it
%%     {ok, StopResponse} = cre_http_handler:handle_request(<<"POST">>, [CaseId, <<"stop">>], #{}),
%%     ?assertMatch(#{status := stopped, case_id := CaseId}, StopResponse),
%%
%%     %% Verify it's gone
%%     ?assertEqual({error, not_found}, yawl_registry:lookup(CaseId)).
