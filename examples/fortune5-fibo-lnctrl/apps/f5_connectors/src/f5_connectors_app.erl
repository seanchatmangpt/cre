%% Generated application module for f5_connectors
-module(f5_connectors_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_connectors_sup:start_link().

stop(_State) ->
    ok.
