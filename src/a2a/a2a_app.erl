%%%-----------------------------------------------------------------------------
%%% @doc A2A-CONSTRUCT Application
%%%
%%% Main application module for A2A-CONSTRUCT framework.
%%% Provides convenience functions for launching and managing A2A stations.
%%% @end
%%%-----------------------------------------------------------------------------
-module(a2a_app).

%% API
-export([
    start/0,
    start/1,
    stop/0,
    launch_20_stations/0,
    demo/0,
    status/0
]).

-include_lib("kernel/include/logger.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

%% @doc Start the A2A orchestrator with default configuration
-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    start([]).

%% @doc Start with initial station configurations
-spec start([map()]) -> {ok, pid()} | {error, term()}.
start(InitialStations) ->
    a2a_orchestrator:start(InitialStations).

%% @doc Stop the A2A orchestrator
-spec stop() -> ok.
stop() ->
    a2a_orchestrator:stop().

%% @doc Launch 20 A2A stations as requested
-spec launch_20_stations() -> {ok, [pid()]} | {error, term()}.
launch_20_stations() ->
    ?LOG_INFO(#{what => launching_20_stations}),

    % Define 5 protocol types
    Protocols = [
        <<"echo">>,       % Simple echo protocol
        <<"transform">>,  % Data transformation
        <<"aggregate">>,  % Data aggregation
        <<"validate">>,   % Validation protocol
        <<"route">>       % Routing protocol
    ],

    case a2a_orchestrator:launch_stations(20, Protocols) of
        {ok, Pids} ->
            ?LOG_INFO(#{
                what => stations_launched,
                count => length(Pids),
                pids => Pids
            }),
            {ok, Pids};
        {error, _} = Error ->
            ?LOG_ERROR(#{what => launch_failed, error => Error}),
            Error
    end.

%% @doc Run a demonstration of A2A-CONSTRUCT
-spec demo() -> ok.
demo() ->
    ?LOG_INFO(#{what => starting_demo}),

    % Start orchestrator
    {ok, _SupPid} = start(),
    timer:sleep(100),

    % Launch 20 stations
    {ok, Pids} = launch_20_stations(),
    ?LOG_INFO(#{what => demo_stations_launched, count => length(Pids)}),

    timer:sleep(500),

    % Create and submit test tasks
    Tasks = [
        a2a_types:new_task(<<"echo">>, [string], [string]),
        a2a_types:new_task(<<"transform">>, [json], [xml]),
        a2a_types:new_task(<<"validate">>, [data], [boolean]),
        a2a_types:new_task(<<"aggregate">>, [list], [summary]),
        a2a_types:new_task(<<"route">>, [message], [destination])
    ],

    ?LOG_INFO(#{what => submitting_test_tasks, count => length(Tasks)}),

    Results = lists:map(fun(Task) ->
        Protocol = maps:get(protocol, Task),
        case a2a_orchestrator:submit_task(Protocol, Task) of
            {ok, Pid, {ok, Receipt}} ->
                ?LOG_INFO(#{
                    what => task_completed,
                    protocol => Protocol,
                    station => Pid,
                    receipt_id => maps:get(id, Receipt)
                }),
                {success, Receipt};
            {ok, Pid, {refused, Refusal}} ->
                ?LOG_WARNING(#{
                    what => task_refused,
                    protocol => Protocol,
                    station => Pid,
                    refusal_type => maps:get(type, Refusal)
                }),
                {refused, Refusal};
            {error, Reason} ->
                ?LOG_ERROR(#{
                    what => task_failed,
                    protocol => Protocol,
                    reason => Reason
                }),
                {error, Reason}
        end
    end, Tasks),

    % Print statistics
    timer:sleep(200),
    Stats = a2a_orchestrator:get_station_stats(),
    ?LOG_INFO(#{
        what => demo_complete,
        total_stations => length(Stats),
        total_tasks => length(Tasks),
        successful => length([X || {success, X} <- Results]),
        refused => length([X || {refused, X} <- Results]),
        errors => length([X || {error, X} <- Results])
    }),

    % Print station details
    lists:foreach(fun(#{station_id := Id, receipt_count := Count, protocols := Protos}) ->
        ?LOG_INFO(#{
            station => Id,
            receipts => Count,
            protocols => Protos
        })
    end, Stats),

    ok.

%% @doc Get current status of all stations
-spec status() -> map().
status() ->
    Stats = a2a_orchestrator:get_station_stats(),
    Receipts = a2a_orchestrator:get_all_receipts(),

    TotalReceipts = lists:sum([length(R) || {_, R} <- Receipts]),

    #{
        total_stations => length(Stats),
        active_stations => length([S || #{state := State} = S <- Stats, State =/= idle]),
        total_receipts => TotalReceipts,
        stations => Stats
    }.
