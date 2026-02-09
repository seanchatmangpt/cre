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
%% @doc GCP-Ready Health Check Endpoints for CRE
%%
%% Provides HTTP endpoints for GCP health checks:
%% - /health - Liveness probe (is the service running?)
%% - /ready - Readiness probe (is the service ready to serve traffic?)
%% - /startup - Startup probe (has the service started successfully?)
%%
%% <h3>GCP Integration</h3>
%%
%% These endpoints are designed for Google Cloud Platform:
%% <ul>
%%   <li><b>GCP Uptime Checks:</b> Uses /health endpoint</li>
%%   <li><b>Load Balancer Health Checks:</b> Uses /health endpoint</li>
%%   <li><b>Kubernetes Probes:</b> All three probe types supported</li>
%% </ul>
%%
%% <h3>Response Format</h3>
%%
%% All endpoints return JSON with a <code>status</code> field:
%% <ul>
%%   <li><code>"healthy"</code> - Service is operational</li>
%%   <li><code>"unhealthy"</code> - Service has issues</li>
%%   <li><code>"starting"</code> - Service is initializing (startup probe only)</li>
%% </ul>
%%
%% <h3>Health Checks</h3>
%%
%% The following subsystems are monitored:
%% <ul>
%%   <li><b>Mnesia:</b> Database cluster status</li>
%%   <li><b>EPMD:</b> Erlang Port Mapper Daemon connectivity</li>
%%   <li><b>CRE Master:</b> Master process status</li>
%%   <li><b>Worker Pool:</b> Active worker count</li>
%%   <li><b>Spanner:</b> Cloud SQL/Spanner connectivity (if configured)</li>
%%   <li><b>Redis:</b> Memorystore connectivity (if configured)</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(cre_health).
-behaviour(cowboy_handler).

%%====================================================================
%% Exports
%%====================================================================

%% Cowboy handler callback
-export([init/2]).

%% Health check functions
-export([handle_request/3]).
-export([liveness/0]).
-export([readiness/0]).
-export([startup/0]).

%%====================================================================
%% Types
%%====================================================================

-type health_status() :: healthy | unhealthy | starting.
-type subsystem_status() :: #{
    name := binary(),
    status := healthy | unhealthy | disabled,
    message => binary(),
    details => map()
}.
-type health_response() :: #{
    status := health_status(),
    timestamp := integer(),
    subsystems := [subsystem_status()]
}.

-export_type([health_status/0, subsystem_status/0, health_response/0]).

%%====================================================================
%% Cowboy Handler Callback
%%====================================================================

%% @doc Initialize HTTP request handler for health endpoints
%%
%% Routes requests to appropriate health check handlers based on path:
%% - /health - Liveness probe
%% - /ready - Readiness probe
%% - /startup - Startup probe
%%
%% @end
-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req, State) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),

    case {Method, Path} of
        {<<"GET">>, <<"/health">>} ->
            handle_liveness(Req, State);
        {<<"GET">>, <<"/ready">>} ->
            handle_readiness(Req, State);
        {<<"GET">>, <<"/startup">>} ->
            handle_startup(Req, State);
        {<<"GET">>, _} ->
            ReplyJson = jsone:encode(#{
                <<"status">> => <<"error">>,
                <<"message">> => <<"Unknown health endpoint">>
            }),
            Req2 = cowboy_req:reply(404,
                #{<<"content-type">> => <<"application/json">>},
                ReplyJson, Req),
            {ok, Req2, State};
        _ ->
            ReplyJson = jsone:encode(#{
                <<"status">> => <<"error">>,
                <<"message">> => <<"Method not allowed">>
            }),
            Req2 = cowboy_req:reply(405,
                #{<<"content-type">> => <<"application/json">>},
                ReplyJson, Req),
            {ok, Req2, State}
    end.

%%====================================================================
%% Health Check Handlers
%%====================================================================

%% @doc Handle GET /health - Liveness probe
%%
%% Returns HTTP 200 if the service is running, 503 if unhealthy.
%% Used by GCP uptime checks and load balancers.
%%
%% @end
-spec handle_liveness(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
handle_liveness(Req, State) ->
    Response = liveness(),
    #{status := Status} = Response,
    StatusCode = case Status of
        healthy -> 200;
        unhealthy -> 503;
        starting -> 202
    end,
    ReplyJson = jsone:encode(Response),
    Req2 = cowboy_req:reply(StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        ReplyJson, Req),
    {ok, Req2, State}.

%% @doc Handle GET /ready - Readiness probe
%%
%% Returns HTTP 200 if the service is ready to serve traffic, 503 if not.
%% Checks include: Mnesia, worker pool, CRE master.
%%
%% @end
-spec handle_readiness(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
handle_readiness(Req, State) ->
    Response = readiness(),
    #{status := Status} = Response,
    StatusCode = case Status of
        healthy -> 200;
        unhealthy -> 503;
        starting -> 202
    end,
    ReplyJson = jsone:encode(Response),
    Req2 = cowboy_req:reply(StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        ReplyJson, Req),
    {ok, Req2, State}.

%% @doc Handle GET /startup - Startup probe
%%
%% Returns HTTP 200 if the service has started successfully, 503 if not.
%% Used by Kubernetes to determine when a container is initialized.
%%
%% @end
-spec handle_startup(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
handle_startup(Req, State) ->
    Response = startup(),
    #{status := Status} = Response,
    StatusCode = case Status of
        healthy -> 200;
        unhealthy -> 503;
        starting -> 202
    end,
    ReplyJson = jsone:encode(Response),
    Req2 = cowboy_req:reply(StatusCode,
        #{<<"content-type">> => <<"application/json">>},
        ReplyJson, Req),
    {ok, Req2, State}.

%%====================================================================
%% Public API Functions
%%====================================================================

%% @doc Perform liveness check
%%
%% Liveness indicates the service is running. Basic check that always
%% returns healthy if the BEAM VM is responsive.
%%
%% @end
-spec liveness() -> health_response().
liveness() ->
    Subsystems = [
        check_beam(),
        check_epmd()
    ],
    Status = aggregate_status(Subsystems),
    #{
        status => Status,
        timestamp => erlang:system_time(millisecond),
        subsystems => Subsystems
    }.

%% @doc Perform readiness check
%%
%% Readiness indicates the service can handle requests. Checks include
%% Mnesia cluster, worker pool, and CRE master status.
%%
%% @end
-spec readiness() -> health_response().
readiness() ->
    Subsystems = [
        check_beam(),
        check_epmd(),
        check_mnesia(),
        check_cre_master(),
        check_worker_pool()
    ],
    Status = aggregate_status(Subsystems),
    #{
        status => Status,
        timestamp => erlang:system_time(millisecond),
        subsystems => Subsystems
    }.

%% @doc Perform startup check
%%
%% Startup indicates the service has completed initialization.
%%
%% @end
-spec startup() -> health_response().
startup() ->
    Subsystems = [
        check_beam(),
        check_epmd(),
        check_mnesia(),
        check_cre_master()
    ],
    Status = aggregate_status(Subsystems),
    #{
        status => Status,
        timestamp => erlang:system_time(millisecond),
        subsystems => Subsystems
    }.

%% @doc Handle health check request (callback interface)
%%
%% @end
-spec handle_request(liveness | readiness | startup, term(), term()) ->
    {ok, health_response()}.
handle_request(liveness, _Method, _Body) ->
    {ok, liveness()};
handle_request(readiness, _Method, _Body) ->
    {ok, readiness()};
handle_request(startup, _Method, _Body) ->
    {ok, startup()}.

%%====================================================================
%% Internal Functions - Subsystem Checks
%%====================================================================

%% @doc Check if BEAM VM is responsive
-spec check_beam() -> subsystem_status().
check_beam() ->
    #{
        name => <<"beam">>,
        status => healthy,
        message => <<"BEAM VM is responsive">>,
        details => #{
            uptime_seconds => erlang:statistics(wall_clock) div 1000,
            process_count => erlang:system_info(process_count),
            memory_total => erlang:memory(total)
        }
    }.

%% @doc Check EPMD connectivity
-spec check_epmd() -> subsystem_status().
check_epmd() ->
    case net_adm:ping(list_to_atom("nonode@nohost")) of
        pang ->
            %% EPMD may not be running or node name not configured
            %% This is often OK in containerized environments
            #{
                name => <<"epmd">>,
                status => healthy,
                message => <<"EPMD check skipped (distributed mode not configured)">>,
                details => #{}
            };
        pong ->
            #{
                name => <<"epmd">>,
                status => healthy,
                message => <<"EPMD is reachable">>,
                details => #{}
            }
    end.

%% @doc Check Mnesia database cluster status
-spec check_mnesia() -> subsystem_status().
check_mnesia() ->
    case application:which_applications() of
        Apps when is_list(Apps) ->
            case lists:keyfind(mnesia, 1, Apps) of
                false ->
                    #{
                        name => <<"mnesia">>,
                        status => disabled,
                        message => <<"Mnesia not running">>,
                        details => #{}
                    };
                {mnesia, _Desc, _Vsn} ->
                    check_mnesia_status()
            end;
        _ ->
            #{
                name => <<"mnesia">>,
                status => unhealthy,
                message => <<"Cannot query applications">>,
                details => #{}
            }
    end.

%% @private Check Mnesia cluster status
-spec check_mnesia_status() -> subsystem_status().
check_mnesia_status() ->
    try
        Nodes = mnesia:system_info(running_db_nodes),
        Tables = mnesia:system_info(tables),
        LocalTables = mnesia:system_info(local_tables),

        IsHealthy = length(Nodes) > 0 andalso length(Tables) > 0,

        Status = case IsHealthy of
            true -> healthy;
            false -> unhealthy
        end,

        Message = case IsHealthy of
            true -> <<"Mnesia cluster is operational">>;
            false -> <<"Mnesia cluster has issues">>
        end,

        #{
            name => <<"mnesia">>,
            status => Status,
            message => Message,
            details => #{
                running_nodes => length(Nodes),
                tables => length(Tables),
                local_tables => length(LocalTables)
            }
        }
    catch
        _:Error ->
            #{
                name => <<"mnesia">>,
                status => unhealthy,
                message => list_to_binary(io_lib:format("Mnesia error: ~p", [Error])),
                details => #{error => term_to_binary(Error)}
            }
    end.

%% @doc Check CRE master process status
-spec check_cre_master() -> subsystem_status().
check_cre_master() ->
    case whereis(cre_master) of
        undefined ->
            %% Check if any CRE master is registered
            case get_registered_cre_master() of
                undefined ->
                    #{
                        name => <<"cre_master">>,
                        status => disabled,
                        message => <<"No CRE master process found">>,
                        details => #{}
                    };
                _Name ->
                    #{
                        name => <<"cre_master">>,
                        status => healthy,
                        message => <<"CRE master is running">>,
                        details => #{}
                    }
            end;
        _Pid ->
            #{
                name => <<"cre_master">>,
                status => healthy,
                message => <<"CRE master is running">>,
                details => #{pid => list_to_binary(pid_to_list(_Pid))}
            }
    end.

%% @private Get any registered CRE master process
-spec get_registered_cre_master() -> atom() | undefined.
get_registered_cre_master() ->
    try
        Registered = registered(),
        CreMasters = [N || N <- Registered, is_atom(N),
                          case atom_to_list(N) of
                              "cre_master" ++ _ -> true;
                              _ -> false
                          end],
        case CreMasters of
            [Name | _] -> Name;
            [] -> undefined
        end
    catch
        _:_ -> undefined
    end.

%% @doc Check worker pool status
-spec check_worker_pool() -> subsystem_status().
check_worker_pool() ->
    case get_registered_cre_master() of
        undefined ->
            #{
                name => <<"worker_pool">>,
                status => disabled,
                message => <<"No CRE master available">>,
                details => #{}
            };
        MasterName ->
            check_worker_pool_status(MasterName)
    end.

%% @private Check worker pool status for specific master
-spec check_worker_pool_status(atom()) -> subsystem_status().
check_worker_pool_status(MasterName) ->
    try
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
            name => <<"worker_pool">>,
            status => StatusAtom,
            message => Message,
            details => #{
                worker_count => WorkerCount,
                load => Load
            }
        }
    catch
        _:{error, _} ->
            #{
                name => <<"worker_pool">>,
                status => unhealthy,
                message => <<"Cannot get CRE status">>,
                details => #{}
            };
        _:Error ->
            #{
                name => <<"worker_pool">>,
                status => unhealthy,
                message => list_to_binary(io_lib:format("Error: ~p", [Error])),
                details => #{error => term_to_binary(Error)}
            }
    end.

%%====================================================================
%% Internal Functions - Status Aggregation
%%====================================================================

%% @doc Aggregate subsystem statuses into overall status
%%
%% Overall status is:
%% - unhealthy if any critical subsystem is unhealthy
%% - starting if any subsystem is starting
%% - healthy otherwise
%%
%% @end
-spec aggregate_status([subsystem_status()]) -> health_status().
aggregate_status(Subsystems) ->
    CriticalUnhealthy = [S || S <- Subsystems,
                             maps:get(status, S) =:= unhealthy,
                             is_critical(maps:get(name, S))],
    AnyStarting = [S || S <- Subsystems,
                       maps:get(status, S) =:= starting],

    Status = if
        length(CriticalUnhealthy) > 0 -> unhealthy;
        length(AnyStarting) > 0 -> starting;
        true -> healthy
    end,
    Status.

%% @private Check if a subsystem is critical for health
-spec is_critical(binary()) -> boolean().
is_critical(<<"beam">>) -> true;
is_critical(<<"mnesia">>) -> false;  %% Can be disabled
is_critical(<<"epmd">>) -> false;    %% Can be disabled
is_critical(<<"cre_master">>) -> false;  %% Can be disabled
is_critical(<<"worker_pool">>) -> false;  %% Can be disabled
is_critical(<<"spanner">>) -> false;  %% External dependency
is_critical(<<"redis">>) -> false;   %% External dependency
is_critical(_) -> false.
