%% Generated application module for f5_app_17
-module(f5_app_17_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_17_sup:start_link().

stop(_State) ->
    ok.
