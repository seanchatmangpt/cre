%%%-----------------------------------------------------------------------------
%%% @doc A2A Protocol Orchestrator
%%%
%%% Manages fleet of A2A stations and coordinates protocol execution.
%%% Provides high-level API for:
%%% - Launching N stations with protocol configurations
%%% - Distributing tasks across stations
%%% - Collecting receipts and building proof chains
%%% - Monitoring station health
%%% @end
%%%-----------------------------------------------------------------------------
-module(a2a_orchestrator).

%% API
-export([
    start/0,
    start/1,
    stop/0,
    launch_stations/1,
    launch_stations/2,
    submit_task/2,
    submit_task_to_station/3,
    get_all_receipts/0,
    get_station_stats/0,
    broadcast_task/2
]).

-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_PROTOCOLS, [
    <<"echo">>,
    <<"transform">>,
    <<"aggregate">>,
    <<"validate">>,
    <<"route">>
]).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start() -> {ok, pid()} | {error, term()}.
start() ->
    start([]).

-spec start([map()]) -> {ok, pid()} | {error, term()}.
start(InitialStations) ->
    case a2a_supervisor:start_link(InitialStations) of
        {ok, Pid} ->
            ?LOG_INFO(#{what => orchestrator_started, supervisor => Pid}),
            {ok, Pid};
        Error ->
            Error
    end.

-spec stop() -> ok.
stop() ->
    Stations = a2a_supervisor:get_stations(),
    lists:foreach(fun({Id, _, _}) ->
        a2a_supervisor:stop_station(Id)
    end, Stations),
    ?LOG_INFO(#{what => orchestrator_stopped}),
    ok.

%% @doc Launch N stations with default protocol distribution
-spec launch_stations(pos_integer()) -> {ok, [pid()]} | {error, term()}.
launch_stations(N) when N > 0 ->
    launch_stations(N, ?DEFAULT_PROTOCOLS).

%% @doc Launch N stations with specific protocols
-spec launch_stations(pos_integer(), [binary()]) -> {ok, [pid()]} | {error, term()}.
launch_stations(N, Protocols) when N > 0, length(Protocols) > 0 ->
    ?LOG_INFO(#{
        what => launching_stations,
        count => N,
        protocols => Protocols
    }),

    Results = lists:map(fun(I) ->
        StationId = generate_station_id(I),
        % Distribute protocols across stations (round-robin)
        StationProtocols = select_protocols_for_station(I, Protocols),
        Options = #{
            max_concurrent => 5,
            task_timeout => 30000
        },
        case a2a_supervisor:start_station(StationId, StationProtocols, Options) of
            {ok, Pid} ->
                ?LOG_INFO(#{
                    what => station_launched,
                    station_id => StationId,
                    pid => Pid,
                    protocols => StationProtocols
                }),
                {ok, Pid};
            Error ->
                ?LOG_ERROR(#{
                    what => station_launch_failed,
                    station_id => StationId,
                    error => Error
                }),
                Error
        end
    end, lists:seq(1, N)),

    case lists:partition(fun({ok, _}) -> true; (_) -> false end, Results) of
        {Success, []} ->
            Pids = [Pid || {ok, Pid} <- Success],
            {ok, Pids};
        {Success, Failures} ->
            ?LOG_WARNING(#{
                what => partial_launch_success,
                success_count => length(Success),
                failure_count => length(Failures)
            }),
            Pids = [Pid || {ok, Pid} <- Success],
            {ok, Pids}
    end.

%% @doc Submit task to any available station supporting the protocol
-spec submit_task(binary(), a2a_types:task()) -> {ok, pid(), a2a_types:receipt()} | {error, term()}.
submit_task(Protocol, Task) ->
    case find_station_for_protocol(Protocol) of
        {ok, Pid} ->
            Result = a2a_station:execute_task(Pid, Task),
            {ok, Pid, Result};
        {error, _} = Error ->
            Error
    end.

%% @doc Submit task to specific station
-spec submit_task_to_station(pid(), binary(), a2a_types:task()) -> {ok, a2a_types:receipt()} | {refused, a2a_types:refusal()}.
submit_task_to_station(StationPid, _Protocol, Task) ->
    a2a_station:execute_task(StationPid, Task).

%% @doc Broadcast task to all stations supporting the protocol
-spec broadcast_task(binary(), a2a_types:task()) -> [{pid(), {ok, a2a_types:receipt()} | {refused, a2a_types:refusal()}}].
broadcast_task(Protocol, Task) ->
    Stations = find_all_stations_for_protocol(Protocol),
    lists:map(fun(Pid) ->
        Result = a2a_station:execute_task(Pid, Task),
        {Pid, Result}
    end, Stations).

%% @doc Collect all receipts from all stations
-spec get_all_receipts() -> [{binary(), [a2a_types:receipt()]}].
get_all_receipts() ->
    Stations = a2a_supervisor:get_stations(),
    lists:map(fun({StationId, Pid, _}) ->
        Receipts = a2a_station:get_receipts(Pid),
        {StationId, Receipts}
    end, Stations).

%% @doc Get statistics for all stations
-spec get_station_stats() -> [map()].
get_station_stats() ->
    Stations = a2a_supervisor:get_stations(),
    lists:map(fun({StationId, Pid, _}) ->
        StationState = a2a_station:get_state(Pid),
        Receipts = a2a_station:get_receipts(Pid),
        #{
            station_id => StationId,
            pid => Pid,
            state => maps:get(state, StationState),
            protocols => maps:get(protocols, StationState),
            receipt_count => length(Receipts)
        }
    end, Stations).

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

-spec generate_station_id(pos_integer()) -> binary().
generate_station_id(N) ->
    list_to_binary(io_lib:format("station_~3..0b", [N])).

-spec select_protocols_for_station(pos_integer(), [binary()]) -> [binary()].
select_protocols_for_station(StationNum, AllProtocols) ->
    % Each station gets 2-3 protocols in round-robin fashion
    NumProtocols = length(AllProtocols),
    StartIdx = ((StationNum - 1) rem NumProtocols) + 1,
    Count = min(3, NumProtocols),

    Selected = lists:sublist(
        AllProtocols ++ AllProtocols,  % Duplicate to handle wrap-around
        StartIdx,
        Count
    ),

    % Ensure unique protocols
    lists:usort(Selected).

-spec find_station_for_protocol(binary()) -> {ok, pid()} | {error, no_station_found}.
find_station_for_protocol(Protocol) ->
    Stations = a2a_supervisor:get_station_pids(),
    case find_station_with_protocol(Protocol, Stations) of
        {ok, _} = Result -> Result;
        not_found -> {error, no_station_found}
    end.

-spec find_station_with_protocol(binary(), [pid()]) -> {ok, pid()} | not_found.
find_station_with_protocol(_Protocol, []) ->
    not_found;
find_station_with_protocol(Protocol, [Pid | Rest]) ->
    StationState = a2a_station:get_state(Pid),
    Protocols = maps:get(protocols, StationState),
    case lists:member(Protocol, Protocols) of
        true -> {ok, Pid};
        false -> find_station_with_protocol(Protocol, Rest)
    end.

-spec find_all_stations_for_protocol(binary()) -> [pid()].
find_all_stations_for_protocol(Protocol) ->
    Stations = a2a_supervisor:get_station_pids(),
    lists:filter(fun(Pid) ->
        StationState = a2a_station:get_state(Pid),
        Protocols = maps:get(protocols, StationState),
        lists:member(Protocol, Protocols)
    end, Stations).
