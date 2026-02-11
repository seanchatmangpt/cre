%% Generated application module for f5_app_194
-module(f5_app_194_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_194_sup:start_link().

stop(_State) ->
    ok.
