%% Generated regulation suite app for FinTech Startup (Delaware)
-module(f5_reg_fintech_startup_de_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_reg_fintech_startup_de_sup:start_link().

stop(_State) ->
    ok.
