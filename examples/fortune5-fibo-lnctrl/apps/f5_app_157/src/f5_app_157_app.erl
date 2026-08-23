%% Generated application module for f5_app_157
-module(f5_app_157_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_157_sup:start_link().

stop(_State) ->
    ok.
