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
%% @doc GCP Health Check Aggregation Module
%%
%% Provides health check aggregation, status computation, and timeout
%% handling for Google Cloud Platform integration.
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_http_gcp_health).

%%====================================================================
%% Exports
%%====================================================================

%% Health check aggregation
-export([check_all/0]).
-export([check_all_with_timeout/1]).
-export([get_aggregated_status/0]).

%% Individual health checks
-export([check_mnesia_cluster/0]).
-export([check_spanner_connectivity/0]).
-export([check_redis_connectivity/0]).
-export([check_epmd_status/0]).
-export([check_active_workflows/0]).
-export([check_cre_master_status/0]).
-export([check_worker_pool_status/0]).

%% Status computation
-export([compute_overall_status/1]).
-export([is_healthy/1]).
-export([is_ready/1]).

%% Timeout handling
-export([with_timeout/2]).
-export([with_timeout/3]).

%%====================================================================
%% Types
%%====================================================================

-type subsystem_name() :: mnesia | spanner | redis | epmd |
                         workflows | cre_master | worker_pool | beam.
-type health_result() :: #{
    name := subsystem_name(),
    status := healthy | unhealthy | disabled | timeout,
    message := binary(),
    details => map(),
    duration_ms => number()
}.
-type aggregated_health() :: #{
    overall_status := healthy | unhealthy | degraded,
    timestamp := integer(),
    timeout_ms := number(),
    subsystems := [health_result()]
}.

-export_type([subsystem_name/0, health_result/0, aggregated_health/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(DEFAULT_TIMEOUT_MS, 5000).
-define(QUICK_TIMEOUT_MS, 1000).

%%====================================================================
%% Helper Functions (must be defined before use)
%%====================================================================

%% @private Check if a subsystem is critical
-spec is_critical_subsystem(subsystem_name()) -> boolean().
is_critical_subsystem(beam) -> true;
is_critical_subsystem(cre_master) -> true;
is_critical_subsystem(mnesia) -> false;
is_critical_subsystem(spanner) -> false;
is_critical_subsystem(redis) -> false;
is_critical_subsystem(epmd) -> false;
is_critical_subsystem(workflows) -> false;
is_critical_subsystem(worker_pool) -> false.

%% @private Get any registered CRE master
-spec get_registered_cre_master() -> atom() | undefined.
get_registered_cre_master() ->
    try
        case whereis(cre_master) of
            undefined ->
                Registered = registered(),
                CreMasters = [N || N <- Registered, is_atom(N),
                                  case atom_to_list(N) of
                                      "cre_master" ++ _ -> true;
                                      _ -> false
                                  end],
                case CreMasters of
                    [Name | _] -> Name;
                    [] -> undefined
                end;
            Pid when is_pid(Pid) ->
                cre_master
        end
    catch
        _:_ -> undefined
    end.

%% @private Get active workflow count
-spec get_active_workflow_count() -> non_neg_integer().
get_active_workflow_count() ->
    case get_registered_cre_master() of
        undefined -> 0;
        MasterName ->
            try
                Status = cre_master:get_status(MasterName),
                #{app_info := #{active := ActiveList}} = Status,
                length(ActiveList)
            catch
                _:_ -> 0
            end
    end.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Check all subsystems and return aggregated health
-spec check_all() -> aggregated_health().
check_all() ->
    check_all_with_timeout(?DEFAULT_TIMEOUT_MS).

%% @doc Check all subsystems with custom timeout
-spec check_all_with_timeout(number()) -> aggregated_health().
check_all_with_timeout(TimeoutMs) ->
    Checks = [
        {mnesia, fun check_mnesia_cluster/0},
        {spanner, fun check_spanner_connectivity/0},
        {redis, fun check_redis_connectivity/0},
        {epmd, fun check_epmd_status/0},
        {workflows, fun check_active_workflows/0},
        {cre_master, fun check_cre_master_status/0},
        {worker_pool, fun check_worker_pool_status/0}
    ],

    Results = lists:map(fun({Name, CheckFun}) ->
        with_timeout(Name, CheckFun, TimeoutMs)
    end, Checks),

    #{
        overall_status => compute_overall_status(Results),
        timestamp => erlang:system_time(millisecond),
        timeout_ms => TimeoutMs,
        subsystems => Results
    }.

%% @doc Get aggregated health status (quick check)
-spec get_aggregated_status() -> aggregated_health().
get_aggregated_status() ->
    check_all_with_timeout(?QUICK_TIMEOUT_MS).

%%====================================================================
%% Individual Health Checks
%%====================================================================

%% @doc Check Mnesia cluster status
-spec check_mnesia_cluster() -> health_result().
check_mnesia_cluster() ->
    StartTime = erlang:monotonic_time(millisecond),
    Result = try
        case application:which_applications() of
            Apps when is_list(Apps) ->
                case lists:keyfind(mnesia, 1, Apps) of
                    false ->
                        #{
                            name => mnesia,
                            status => disabled,
                            message => <<"Mnesia not running">>,
                            details => #{}
                        };
                    {mnesia, _Desc, _Vsn} ->
                        RunningNodes = mnesia:system_info(running_db_nodes),
                        Tables = mnesia:system_info(tables),
                        LocalTables = mnesia:system_info(local_tables),
                        IsHealthy = length(RunningNodes) > 0 andalso length(Tables) > 0,
                        Status = case IsHealthy of
                            true -> healthy;
                            false -> unhealthy
                        end,
                        Message = case IsHealthy of
                            true -> <<"Mnesia cluster is operational">>;
                            false -> <<"Mnesia cluster has issues">>
                        end,
                        #{
                            name => mnesia,
                            status => Status,
                            message => Message,
                            details => #{
                                running_nodes => length(RunningNodes),
                                tables => length(Tables),
                                local_tables => length(LocalTables)
                            }
                        }
                end;
            _ ->
                #{
                    name => mnesia,
                    status => unhealthy,
                    message => <<"Cannot query applications">>,
                    details => #{}
                }
        end
    catch
        _:Error ->
            #{
                name => mnesia,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Mnesia error: ~p", [Error])),
                details => #{error => term_to_binary(Error)}
            }
    end,
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    Result#{duration_ms => Duration}.

%% @doc Check Spanner/Cloud SQL connectivity
-spec check_spanner_connectivity() -> health_result().
check_spanner_connectivity() ->
    StartTime = erlang:monotonic_time(millisecond),
    Result = try
        case application:get_env(cre, spanner_config) of
            undefined ->
                #{
                    name => spanner,
                    status => disabled,
                    message => <<"Spanner not configured">>,
                    details => #{}
                };
            {ok, Config} when is_map(Config); is_list(Config) ->
                #{
                    name => spanner,
                    status => disabled,
                    message => <<"Spanner configured but connectivity check not implemented">>,
                    details => #{configured => true}
                };
            _ ->
                #{
                    name => spanner,
                    status => disabled,
                    message => <<"Spanner not configured">>,
                    details => #{}
                }
        end
    catch
        _:Error ->
            #{
                name => spanner,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Spanner error: ~p", [Error])),
                details => #{error => term_to_binary(Error)}
            }
    end,
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    Result#{duration_ms => Duration}.

%% @doc Check Redis/Memorystore connectivity
-spec check_redis_connectivity() -> health_result().
check_redis_connectivity() ->
    StartTime = erlang:monotonic_time(millisecond),
    Result = try
        case application:get_env(cre, redis_config) of
            undefined ->
                #{
                    name => redis,
                    status => disabled,
                    message => <<"Redis not configured">>,
                    details => #{}
                };
            {ok, Config} when is_map(Config); is_list(Config) ->
                #{
                    name => redis,
                    status => disabled,
                    message => <<"Redis configured but connectivity check not implemented">>,
                    details => #{configured => true}
                };
            _ ->
                #{
                    name => redis,
                    status => disabled,
                    message => <<"Redis not configured">>,
                    details => #{}
                }
        end
    catch
        _:Error ->
            #{
                name => redis,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Redis error: ~p", [Error])),
                details => #{error => term_to_binary(Error)}
            }
    end,
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    Result#{duration_ms => Duration}.

%% @doc Check EPMD status
-spec check_epmd_status() -> health_result().
check_epmd_status() ->
    StartTime = erlang:monotonic_time(millisecond),
    Result = try
        case net_adm:ping(list_to_atom("nonode@nohost")) of
            pang ->
                #{
                    name => epmd,
                    status => healthy,
                    message => <<"EPMD check skipped (single node mode)">>,
                    details => #{mode => single}
                };
            pong ->
                #{
                    name => epmd,
                    status => healthy,
                    message => <<"EPMD is reachable">>,
                    details => #{mode => distributed}
                }
        end
    catch
        _:Error ->
            #{
                name => epmd,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("EPMD error: ~p", [Error])),
                details => #{error => term_to_binary(Error)}
            }
    end,
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    Result#{duration_ms => Duration}.

%% @doc Check active workflow count
-spec check_active_workflows() -> health_result().
check_active_workflows() ->
    StartTime = erlang:monotonic_time(millisecond),
    Result = try
        ActiveCount = get_active_workflow_count(),
        #{
            name => workflows,
            status => healthy,
            message => iolist_to_binary(io_lib:format("~p active workflows", [ActiveCount])),
            details => #{active_count => ActiveCount}
        }
    catch
        _:Error ->
            #{
                name => workflows,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Workflow check error: ~p", [Error])),
                details => #{error => term_to_binary(Error)}
            }
    end,
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    Result#{duration_ms => Duration}.

%% @doc Check CRE master status
-spec check_cre_master_status() -> health_result().
check_cre_master_status() ->
    StartTime = erlang:monotonic_time(millisecond),
    Result = try
        case get_registered_cre_master() of
            undefined ->
                #{
                    name => cre_master,
                    status => unhealthy,
                    message => <<"No CRE master process found">>,
                    details => #{}
                };
            MasterName ->
                Pid = whereis(MasterName),
                #{
                    name => cre_master,
                    status => healthy,
                    message => <<"CRE master is running">>,
                    details => #{
                        name => atom_to_binary(MasterName, utf8),
                        pid => list_to_binary(pid_to_list(Pid))
                    }
                }
        end
    catch
        _:Error ->
            #{
                name => cre_master,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("CRE master error: ~p", [Error])),
                details => #{error => term_to_binary(Error)}
            }
    end,
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    Result#{duration_ms => Duration}.

%% @doc Check worker pool status
-spec check_worker_pool_status() -> health_result().
check_worker_pool_status() ->
    StartTime = erlang:monotonic_time(millisecond),
    Result = try
        case get_registered_cre_master() of
            undefined ->
                #{
                    name => worker_pool,
                    status => unhealthy,
                    message => <<"No CRE master available">>,
                    details => #{}
                };
            MasterName ->
                Status = cre_master:get_status(MasterName),
                #{cre_info := #{n_wrk := WorkerCount, load := Load}} = Status,
                IsHealthy = WorkerCount > 0,
                StatusAtom = case IsHealthy of
                    true -> healthy;
                    false -> unhealthy
                end,
                Message = case IsHealthy of
                    true -> <<"Worker pool is operational">>;
                    false -> <<"No workers available">>
                end,
                #{
                    name => worker_pool,
                    status => StatusAtom,
                    message => Message,
                    details => #{
                        worker_count => WorkerCount,
                        load => Load
                    }
                }
        end
    catch
        _:{error, _} ->
            #{
                name => worker_pool,
                status => unhealthy,
                message => <<"Cannot get CRE status">>,
                details => #{}
            };
        _:Error ->
            #{
                name => worker_pool,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Worker pool error: ~p", [Error])),
                details => #{error => term_to_binary(Error)}
            }
    end,
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    Result#{duration_ms => Duration}.

%%====================================================================
%% Status Computation
%%====================================================================

%% @doc Compute overall status from subsystem results
-spec compute_overall_status([health_result()]) -> healthy | unhealthy | degraded.
compute_overall_status(Results) ->
    CriticalUnhealthy = [R || R <- Results,
                             maps:get(status, R) =:= unhealthy,
                             is_critical_subsystem(maps:get(name, R))],
    NonCriticalUnhealthy = [R || R <- Results,
                                 maps:get(status, R) =:= unhealthy,
                                 not is_critical_subsystem(maps:get(name, R))],
    Status = if
        length(CriticalUnhealthy) > 0 -> unhealthy;
        length(NonCriticalUnhealthy) > 0 -> degraded;
        true -> healthy
    end,
    Status.

%% @doc Check if health result indicates healthy status
-spec is_healthy(aggregated_health()) -> boolean().
is_healthy(#{overall_status := Status}) when Status =:= healthy; Status =:= degraded ->
    true;
is_healthy(#{status := Status}) when Status =:= healthy; Status =:= disabled ->
    true;
is_healthy(_) ->
    false.

%% @doc Check if health result indicates ready status
-spec is_ready(aggregated_health()) -> boolean().
is_ready(#{overall_status := Status}) ->
    Status =:= healthy orelse Status =:= degraded.

%%====================================================================
%% Timeout Handling
%%====================================================================

%% @doc Execute a function with a timeout
-spec with_timeout(subsystem_name(), fun(), number()) -> health_result().
with_timeout(Name, Fun, TimeoutMs) ->
    StartTime = erlang:monotonic_time(millisecond),
    Ref = make_ref(),
    Self = self(),
    Pid = spawn(fun() ->
        try
            Result = Fun(),
            Self ! {Ref, {ok, Result}}
        catch
            _:Error ->
                Self ! {Ref, {error, Error}}
        end
    end),
    MRef = erlang:monitor(process, Pid),
    receive
        {Ref, {ok, Result0}} ->
            erlang:demonitor(MRef, [flush]),
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            Result0#{duration_ms => Duration};
        {Ref, {error, Error}} ->
            erlang:demonitor(MRef, [flush]),
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            #{
                name => Name,
                status => unhealthy,
                message => iolist_to_binary(io_lib:format("Error: ~p", [Error])),
                details => #{error => term_to_binary(Error)},
                duration_ms => Duration
            };
        {'DOWN', MRef, process, Pid, _Reason} ->
            Duration = erlang:monotonic_time(millisecond) - StartTime,
            #{
                name => Name,
                status => timeout,
                message => iolist_to_binary(io_lib:format("Check timed out after ~p ms", [Duration])),
                details => #{timeout_ms => TimeoutMs},
                duration_ms => Duration
            }
    after TimeoutMs ->
        erlang:demonitor(MRef, [flush]),
        erlang:exit(Pid, kill),
        Duration = erlang:monotonic_time(millisecond) - StartTime,
        receive
            {'DOWN', MRef, process, Pid, _} -> ok
        after 0 -> ok
        end,
        #{
            name => Name,
            status => timeout,
            message => iolist_to_binary(io_lib:format("Check timed out after ~p ms", [TimeoutMs])),
            details => #{timeout_ms => TimeoutMs},
            duration_ms => Duration
        }
    end.

%% @doc Execute a function with default timeout
-spec with_timeout(subsystem_name(), fun()) -> health_result().
with_timeout(Name, Fun) ->
    with_timeout(Name, Fun, ?DEFAULT_TIMEOUT_MS).
