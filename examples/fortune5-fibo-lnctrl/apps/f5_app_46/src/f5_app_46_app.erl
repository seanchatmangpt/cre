%% Generated application module for f5_app_46
-module(f5_app_46_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_46_sup:start_link().

stop(_State) ->
    ok.
