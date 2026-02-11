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
%% Unit tests for REST API workflow management endpoints.
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_http_handler_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Unit Tests
%%====================================================================

%% @doc Test handle_request with valid create workflow request
create_workflow_request_test() ->
    Body = #{
        <<"workflow_module">> => <<"test_workflow">>,
        <<"case_id">> => <<"test-001">>,
        <<"init_args">> => #{},
        <<"options">> => []
    },
    %% This will fail because test_workflow module doesn't exist
    %% but we can verify the request parsing works
    Result = cre_http_handler:handle_request(<<"POST">>, [], Body),
    %% Should get an error since test_workflow module doesn't exist
    ?assertMatch({error, _}, Result).

%% @doc Test handle_request with missing fields
missing_field_request_test() ->
    InvalidBody = #{
        <<"case_id">> => <<"test-001">>
    },
    Result = cre_http_handler:handle_request(<<"POST">>, [], InvalidBody),
    %% Should get an error due to missing workflow_module
    ?assertMatch({error, _}, Result).

%% @doc Test handle_request with invalid module name
invalid_module_request_test() ->
    InvalidBody = #{
        <<"workflow_module">> => <<"nonexistent_module_xyz">>,
        <<"case_id">> => <<"test-001">>,
        <<"init_args">> => #{},
        <<"options">> => []
    },
    Result = cre_http_handler:handle_request(<<"POST">>, [], InvalidBody),
    %% Should get an error since module doesn't exist
    ?assertMatch({error, _}, Result).

%% @doc Test GET request to list workflows (empty list)
list_workflows_request_test() ->
    Result = cre_http_handler:handle_request(<<"GET">>, [], #{}),
    ?assertMatch({ok, #{status := ok, workflows := [], count := 0}}, Result).

%% @doc Test GET request for non-existent workflow
get_nonexistent_workflow_test() ->
    Result = cre_http_handler:handle_request(<<"GET">>, [<<"nonexistent">>], #{}),
    ?assertMatch({error, not_found}, Result).

%% @doc Test POST start for non-existent workflow
start_nonexistent_workflow_test() ->
    Result = cre_http_handler:handle_request(<<"POST">>, [<<"nonexistent">>, <<"start">>], #{}),
    ?assertMatch({error, not_found}, Result).

%% @doc Test POST stop for non-existent workflow
stop_nonexistent_workflow_test() ->
    Result = cre_http_handler:handle_request(<<"POST">>, [<<"nonexistent">>, <<"stop">>], #{}),
    ?assertMatch({error, not_found}, Result).

%% @doc Test unsupported endpoint
unsupported_endpoint_test() ->
    Result = cre_http_handler:handle_request(<<"DELETE">>, [<<"some">>, <<"path">>], #{}),
    ?assertMatch({error, unsupported_endpoint}, Result).

%% @doc Test listener management functions (smoke test)
listener_management_test() ->
    %% Test that the functions are exported and don't crash on type errors
    ?assertMatch({error, _}, cre_http_handler:start_listener(invalid_port)),
    ?assertEqual(ok, cre_http_handler:stop_listener()).

%%====================================================================
%% Integration Tests (require running system)
%%====================================================================

%% These tests are commented out as they require:
%% - CRE application running
%% - yawl_registry started
%% - yawl_workflow_supervisor started
%% - An actual workflow module (e.g., test_workflow)

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
