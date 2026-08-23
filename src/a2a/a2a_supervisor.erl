%%%-----------------------------------------------------------------------------
%%% @doc A2A Station Supervisor
%%%
%%% Supervises multiple A2A stations with one_for_one strategy.
%%% Can launch N stations with different protocol configurations.
%%% @end
%%%-----------------------------------------------------------------------------
-module(a2a_supervisor).
-behaviour(supervisor).

%% API
-export([
    start_link/0,
    start_link/1,
    start_station/2,
    start_station/3,
    stop_station/1,
    get_stations/0,
    get_station_pids/0
]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link([map()]) -> {ok, pid()} | {error, term()}.
start_link(StationConfigs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, StationConfigs).

-spec start_station(binary(), [binary()]) -> {ok, pid()} | {error, term()}.
start_station(StationId, Protocols) ->
    start_station(StationId, Protocols, #{}).

-spec start_station(binary(), [binary()], map()) -> {ok, pid()} | {error, term()}.
start_station(StationId, Protocols, Options) ->
    ChildSpec = #{
        id => StationId,
        start => {a2a_station, start_link, [StationId, Protocols, Options]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [a2a_station]
    },
    supervisor:start_child(?SERVER, ChildSpec).

-spec stop_station(binary()) -> ok | {error, term()}.
stop_station(StationId) ->
    case supervisor:terminate_child(?SERVER, StationId) of
        ok -> supervisor:delete_child(?SERVER, StationId);
        Error -> Error
    end.

-spec get_stations() -> [{binary(), pid(), atom()}].
get_stations() ->
    Children = supervisor:which_children(?SERVER),
    [{Id, Pid, worker} || {Id, Pid, worker, _} <- Children, is_pid(Pid)].

-spec get_station_pids() -> [pid()].
get_station_pids() ->
    [Pid || {_, Pid, _} <- get_stations()].

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

init(StationConfigs) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = lists:map(fun station_config_to_childspec/1, StationConfigs),

    {ok, {SupFlags, ChildSpecs}}.

%%%=============================================================================
%%% Internal Functions
%%%=============================================================================

-spec station_config_to_childspec(map()) -> supervisor:child_spec().
station_config_to_childspec(#{station_id := Id, protocols := Protocols} = Config) ->
    Options = maps:without([station_id, protocols], Config),
    #{
        id => Id,
        start => {a2a_station, start_link, [Id, Protocols, Options]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [a2a_station]
    }.
