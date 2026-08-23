%% Generated supervisor for f5_app_97
-module(f5_app_97_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
