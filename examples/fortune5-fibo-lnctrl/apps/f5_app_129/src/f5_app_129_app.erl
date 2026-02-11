%% Generated application module for f5_app_129
-module(f5_app_129_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_129_sup:start_link().

stop(_State) ->
    ok.
