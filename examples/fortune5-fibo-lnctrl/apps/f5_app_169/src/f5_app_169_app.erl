%% Generated application module for f5_app_169
-module(f5_app_169_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_169_sup:start_link().

stop(_State) ->
    ok.
