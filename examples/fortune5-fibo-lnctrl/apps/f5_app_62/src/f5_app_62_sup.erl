%% Generated supervisor for f5_app_62
-module(f5_app_62_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 10, period => 60},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
