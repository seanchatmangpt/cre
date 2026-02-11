%% Generated application module for f5_app_166
-module(f5_app_166_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_app_166_sup:start_link().

stop(_State) ->
    ok.
