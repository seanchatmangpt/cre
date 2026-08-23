%% Generated application module for f5_app_113
-module(f5_app_113_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_113_sup:start_link().

stop(_State) ->
    ok.
