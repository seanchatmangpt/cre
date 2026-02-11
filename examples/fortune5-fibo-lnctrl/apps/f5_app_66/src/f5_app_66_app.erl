%% Generated application module for f5_app_66
-module(f5_app_66_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_66_sup:start_link().

stop(_State) ->
    ok.
