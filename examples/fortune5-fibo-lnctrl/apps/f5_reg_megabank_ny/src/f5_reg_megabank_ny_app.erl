%% Generated regulation suite app for MegaBank (New York)
-module(f5_reg_megabank_ny_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_reg_megabank_ny_sup:start_link().

stop(_State) ->
    ok.
