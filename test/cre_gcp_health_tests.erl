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
%% @author CRE Project
%% @copyright 2025
%%
%% @doc EUnit Tests for GCP Health Check Modules
%%
%% Tests the health check endpoints and aggregation logic for
%% Google Cloud Platform integration.
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_gcp_health_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% cre_http_gcp_health Module Tests
%%====================================================================

%% @doc Test mnesia cluster check returns result
mnesia_check_returns_result_test() ->
    Result = cre_http_gcp_health:check_mnesia_cluster(),
    #{name := mnesia, status := Status} = Result,
    ?assert(lists:member(Status, [healthy, unhealthy, disabled])).

%% @doc Test EPMD status check returns result
epmd_check_returns_result_test() ->
    Result = cre_http_gcp_health:check_epmd_status(),
    #{name := epmd, status := Status} = Result,
    ?assert(lists:member(Status, [healthy, unhealthy, disabled])).

%% @doc Test workflow check returns result
workflow_check_returns_result_test() ->
    Result = cre_http_gcp_health:check_active_workflows(),
    #{name := workflows, status := Status} = Result,
    ?assert(lists:member(Status, [healthy, unhealthy])).

%% @doc Test CRE master check returns result
cre_master_check_returns_result_test() ->
    Result = cre_http_gcp_health:check_cre_master_status(),
    #{name := cre_master, status := Status} = Result,
    ?assert(lists:member(Status, [healthy, unhealthy])).

%% @doc Test worker pool check returns result
worker_pool_check_returns_result_test() ->
    Result = cre_http_gcp_health:check_worker_pool_status(),
    #{name := worker_pool, status := Status} = Result,
    ?assert(lists:member(Status, [healthy, unhealthy])).

%% @doc Test spanner check returns disabled (not configured)
spanner_check_returns_disabled_test() ->
    Result = cre_http_gcp_health:check_spanner_connectivity(),
    #{name := spanner, status := Status} = Result,
    ?assert(lists:member(Status, [disabled, unhealthy])).

%% @doc Test redis check returns disabled (not configured)
redis_check_returns_disabled_test() ->
    Result = cre_http_gcp_health:check_redis_connectivity(),
    #{name := redis, status := Status} = Result,
    ?assert(lists:member(Status, [disabled, unhealthy])).

%%====================================================================
%% Status Computation Tests
%%====================================================================

%% @doc Test compute overall status with all healthy
compute_status_all_healthy_test() ->
    Results = [
        #{name => beam, status => healthy, message => <<"OK">>, details => #{}},
        #{name => cre_master, status => healthy, message => <<"OK">>, details => #{}}
    ],
    Status = cre_http_gcp_health:compute_overall_status(Results),
    ?assertEqual(healthy, Status).

%% @doc Test compute overall status with critical unhealthy
compute_status_critical_unhealthy_test() ->
    Results = [
        #{name => beam, status => unhealthy, message => <<"Error">>, details => #{}},
        #{name => cre_master, status => healthy, message => <<"OK">>, details => #{}}
    ],
    Status = cre_http_gcp_health:compute_overall_status(Results),
    ?assertEqual(unhealthy, Status).

%% @doc Test compute overall status with non-critical unhealthy
compute_status_non_critical_unhealthy_test() ->
    Results = [
        #{name => beam, status => healthy, message => <<"OK">>, details => #{}},
        #{name => worker_pool, status => unhealthy, message => <<"No workers">>, details => #{}}
    ],
    Status = cre_http_gcp_health:compute_overall_status(Results),
    ?assertEqual(degraded, Status).

%% @doc Test is_healthy with healthy status
is_healthy_true_test() ->
    Health = #{overall_status => healthy, timestamp => 0, timeout_ms => 5000, subsystems => []},
    ?assert(cre_http_gcp_health:is_healthy(Health)).

%% @doc Test is_healthy with degraded status
is_healthy_degraded_test() ->
    Health = #{overall_status => degraded, timestamp => 0, timeout_ms => 5000, subsystems => []},
    ?assert(cre_http_gcp_health:is_healthy(Health)).

%% @doc Test is_healthy with unhealthy status
is_healthy_false_test() ->
    Health = #{overall_status => unhealthy, timestamp => 0, timeout_ms => 5000, subsystems => []},
    ?assertNot(cre_http_gcp_health:is_healthy(Health)).

%% @doc Test is_ready with healthy status
is_ready_healthy_test() ->
    Health = #{overall_status => healthy, timestamp => 0, timeout_ms => 5000, subsystems => []},
    ?assert(cre_http_gcp_health:is_ready(Health)).

%% @doc Test is_ready with degraded status
is_ready_degraded_test() ->
    Health = #{overall_status => degraded, timestamp => 0, timeout_ms => 5000, subsystems => []},
    ?assert(cre_http_gcp_health:is_ready(Health)).

%% @doc Test is_ready with unhealthy status
is_ready_unhealthy_test() ->
    Health = #{overall_status => unhealthy, timestamp => 0, timeout_ms => 5000, subsystems => []},
    ?assertNot(cre_http_gcp_health:is_ready(Health)).

%%====================================================================
%% Timeout Handling Tests
%%====================================================================

%% @doc Test with_timeout returns result before timeout
with_timeout_quick_function_test() ->
    Fun = fun() ->
        #{name => test, status => healthy, message => <<"Quick check">>, details => #{}}
    end,
    Result = cre_http_gcp_health:with_timeout(test, Fun, 5000),
    #{name := test, status := healthy} = Result,
    ?assert(maps:is_key(duration_ms, Result)).

%% @doc Test with_timeout handles timeout
with_timeout_timeout_test() ->
    Fun = fun() ->
        timer:sleep(6000),
        #{name => test, status => healthy, message => <<"Slow check">>, details => #{}}
    end,
    Result = cre_http_gcp_health:with_timeout(test, Fun, 1000),
    #{status := timeout} = Result.

%% @doc Test with_timeout handles crash
with_timeout_crash_test() ->
    Fun = fun() ->
        exit(crash)
    end,
    Result = cre_http_gcp_health:with_timeout(test, Fun, 1000),
    #{status := unhealthy} = Result.

%%====================================================================
%% Aggregation Tests
%%====================================================================

%% @doc Test check_all returns aggregated health
check_all_returns_aggregated_test() ->
    Health = cre_http_gcp_health:check_all(),
    #{overall_status := _OverallStatus,
      timestamp := Timestamp,
      timeout_ms := Timeout,
      subsystems := Subsystems} = Health,
    ?assert(is_integer(Timestamp)),
    ?assert(is_integer(Timeout)),
    ?assert(is_list(Subsystems)),
    ?assert(Timeout > 0).

%% @doc Test check_all with custom timeout
check_all_custom_timeout_test() ->
    Health = cre_http_gcp_health:check_all_with_timeout(1000),
    #{timeout_ms := Timeout} = Health,
    ?assertEqual(1000, Timeout).

%% @doc Test get_aggregated_status uses quick timeout
get_aggregated_status_quick_test() ->
    Health = cre_http_gcp_health:get_aggregated_status(),
    #{timeout_ms := Timeout} = Health,
    ?assert(Timeout =< 2000).  %% QUICK_TIMEOUT_MS is 1000

%%====================================================================
%% Integration Tests
%%====================================================================

%% @doc Test health check subsystems have required fields
subsystem_fields_test() ->
    Health = cre_http_gcp_health:check_all(),
    #{subsystems := Subsystems} = Health,
    lists:foreach(fun(Subsystem) ->
        ?assert(maps:is_key(name, Subsystem)),
        ?assert(maps:is_key(status, Subsystem)),
        ?assert(maps:is_key(message, Subsystem))
    end, Subsystems).

%% @doc Test timeout is recorded in health result
timeout_recorded_test() ->
    Fun = fun() ->
        timer:sleep(10),
        #{name => test, status => healthy, message => <<"OK">>, details => #{}}
    end,
    Result = cre_http_gcp_health:with_timeout(test, Fun, 5000),
    ?assert(maps:is_key(duration_ms, Result)),
    Duration = maps:get(duration_ms, Result),
    ?assert(Duration >= 0).

%%====================================================================
%% Doctests
%%====================================================================

%% @doc Run doctests for the module
doctest_test() ->
    %% Test status computation
    AllHealthy = [
        #{name => beam, status => healthy, message => <<"OK">>, details => #{}},
        #{name => cre_master, status => healthy, message => <<"OK">>, details => #{}}
    ],
    ?assertEqual(healthy, cre_http_gcp_health:compute_overall_status(AllHealthy)),

    ok.
