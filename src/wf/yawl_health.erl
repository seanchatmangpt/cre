%% -*- erlang -*-
%%%% @doc Enhanced Health Checks for CRE YAWL
%%
%% This module provides comprehensive health checks for Kubernetes
%% readiness and liveness probes.
%%
%% Health status levels:
%% - passing: All checks passed, system is healthy
%% - warning: Some checks failed but system is functional
%% - critical: Critical checks failed, system may be unavailable
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_health).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([check_health/0, check_health/1]).
-export([readiness_probe/0, liveness_probe/0]).
-export([health_handler/1]).
-export([register_check/2, unregister_check/1]).

%%====================================================================
%% Types
%%====================================================================

-type health_status() :: passing | warning | critical.
-type check_name() :: atom().
-type check_result() :: {ok, term()} | {error, term()} | {warning, term()}.
-type check_fun() :: fun(() -> check_result()).
-type health_report() :: #{
    status := health_status(),
    timestamp := integer(),
    checks := #{check_name() => check_result()}
}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs all health checks and returns a comprehensive report.
%%
%% @end
%%--------------------------------------------------------------------
-spec check_health() -> health_report().
check_health() ->
    check_health(all).

%%--------------------------------------------------------------------
%% @doc Runs specified health checks.
%%
%% Spec can be:
%% - all: Run all registered checks
%% - [CheckName]: Run specific checks
%% - {essential, [CheckName]}: Run only essential checks
%%
%% @end
%%--------------------------------------------------------------------
-spec check_health(all | [check_name()] | {essential, [check_name()]}) ->
          health_report().
check_health(Spec) ->
    Timestamp = erlang:system_time(millisecond),

    %% Built-in checks
    BuiltInChecks = #{
        mnesia => check_mnesia(),
        node_health => check_node(),
        epmd => check_epmd(),
        memory => check_memory()
    },

    %% Get registered checks
    RegisteredChecks = get_registered_checks(),

    %% Determine which checks to run
    CheckFuns = case Spec of
        all ->
            maps:merge(BuiltInChecks, RegisteredChecks);
        CheckList when is_list(CheckList) ->
            lists:foldl(fun(Key, Acc) ->
                case maps:get(Key, RegisteredChecks, undefined) of
                    undefined -> Acc;
                    Fun -> Acc#{Key => Fun()}
                end
            end, BuiltInChecks, CheckList);
        {essential, CheckList} when is_list(CheckList) ->
            lists:foldl(fun(Key, Acc) ->
                case maps:get(Key, RegisteredChecks, undefined) of
                    undefined -> Acc;
                    Fun -> Acc#{Key => Fun()}
                end
            end, #{}, CheckList)
    end,

    %% Determine overall status
    Status = determine_status(CheckFuns),

    #{
        status => Status,
        timestamp => Timestamp,
        checks => CheckFuns
    }.

%%--------------------------------------------------------------------
%% @doc Returns readiness probe result.
%%
%% Readiness means the service can accept traffic. Returns true
%% if essential checks pass.
%%
%% @end
%%--------------------------------------------------------------------
-spec readiness_probe() -> {ok, true} | {error, term()}.
readiness_probe() ->
    Report = check_health({essential, [mnesia, node_health]}),
    case Report of
        #{status := passing} ->
            {ok, true};
        #{status := warning} ->
            {ok, true};
        #{status := critical} ->
            {error, readiness_failed}
    end.

%%--------------------------------------------------------------------
%% @doc Returns liveness probe result.
%%
%% Liveness means the service is still running. Always returns true
%% if the Erlang node is alive.
%%
%% @end
%%--------------------------------------------------------------------
-spec liveness_probe() -> {ok, true}.
liveness_probe() ->
    {ok, true}.

%%--------------------------------------------------------------------
%% @doc HTTP handler for /health endpoint.
%%
%% Returns JSON health report.
%%
%% @end
%%--------------------------------------------------------------------
-spec health_handler(req()) -> req().
health_handler(Req) ->
    Report = check_health(),
    Body = jsone:encode(Report),
    StatusCode = case maps:get(status, Report) of
        passing -> 200;
        warning -> 200;
        critical -> 503
    end,
    cowboy_req:reply(StatusCode, #{
        <<"content-type">> => <<"application/json">>
    }, Body, Req).

%%--------------------------------------------------------------------
%% @doc Registers a custom health check.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_check(check_name(), check_fun()) -> ok.
register_check(Name, Fun) when is_atom(Name), is_function(Fun, 0) ->
    ets:insert(yawl_health_checks, {Name, Fun}),
    ok.

%%--------------------------------------------------------------------
%% @doc Unregisters a custom health check.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_check(check_name()) -> ok.
unregister_check(Name) when is_atom(Name) ->
    ets:delete(yawl_health_checks, Name),
    ok.

%%====================================================================
%% Built-in Health Checks
%%====================================================================

%% @private
-spec check_mnesia() -> check_result().
check_mnesia() ->
    case application:which_applications() of
        {ok, _} ->
            case lists:keyfind(mnesia, 1, application:which_applications()) of
                false ->
                    %% Mnesia not running, that's ok
                    {ok, not_running};
                {mnesia, _, _} ->
                    try mnesia:system_info(is_running) of
                        true ->
                            {ok, running};
                        false ->
                            {error, mnesia_not_running}
                    catch
                        _:_ ->
                            {error, mnesia_error}
                    end
            end;
        _ ->
            {ok, no_app_info}
    end.

%% @private
-spec check_node() -> check_result().
check_node() ->
    case net_kernel:connect_node(node()) of
        true ->
            {ok, connected};
        ignored ->
            {ok, local_node};
        false ->
            {error, node_not_connected}
    end.

%% @private
-spec check_epmd() -> check_result().
check_epmd() ->
    case net_adm:ping_list([node()]) of
        pang ->
            {warning, epmd_unreachable};
        pong ->
            {ok, reachable}
    end.

%% @private
-spec check_memory() -> check_result().
check_memory() ->
    MemoryTotal = erlang:memory(total),
    MemorySystem = erlang:memory(system),
    UsagePercent = (MemorySystem * 100) div MemoryTotal,
    case UsagePercent of
        P when P > 90 ->
            {warning, {high_memory_usage, P}};
        _ ->
            {ok, {memory_usage, UsagePercent}}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec determine_status(#{check_name() => check_result()}) -> health_status().
determine_status(Checks) ->
    Results = maps:values(Checks),
    CriticalFails = lists:filter(fun
        ({error, _}) -> true;
        (_) -> false
    end, Results),
    WarningFails = lists:filter(fun
        ({warning, _}) -> true;
        (_) -> false
    end, Results),
    case CriticalFails of
        [_ | _] -> critical;
        [] ->
            case WarningFails of
                [_ | _] -> warning;
                [] -> passing
            end
    end.

%% @private
-spec get_registered_checks() -> #{check_name() => check_fun()}.
get_registered_checks() ->
    case ets:whereis(yawl_health_checks) of
        undefined ->
            ets:new(yawl_health_checks, [named_table, public, set]),
            #{};
        _ ->
            lists:foldl(fun({Name, Fun}, Acc) ->
                Acc#{Name => Fun}
            end, #{}, ets:tab2list(yawl_health_checks))
    end.

%%====================================================================
%% Dummy Types for Cowboy
%%====================================================================

%% Cowboy req type - placeholder for compilation
-opaque req() :: term().
