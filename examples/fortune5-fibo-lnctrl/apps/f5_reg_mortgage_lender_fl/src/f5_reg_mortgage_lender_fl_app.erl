%% Generated regulation suite app for Mortgage Lender (Florida)
-module(f5_reg_mortgage_lender_fl_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_reg_mortgage_lender_fl_sup:start_link().

stop(_State) ->
    ok.
