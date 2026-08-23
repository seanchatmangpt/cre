%% Generated application module for f5_app_62
-module(f5_app_62_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_62_sup:start_link().

stop(_State) ->
    ok.
