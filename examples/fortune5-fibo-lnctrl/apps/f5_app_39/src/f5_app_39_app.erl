%% Generated application module for f5_app_39
-module(f5_app_39_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_39_sup:start_link().

stop(_State) ->
    ok.
