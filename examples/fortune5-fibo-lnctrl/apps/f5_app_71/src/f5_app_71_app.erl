%% Generated application module for f5_app_71
-module(f5_app_71_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_71_sup:start_link().

stop(_State) ->
    ok.
