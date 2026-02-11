%% Generated regulation suite app for Credit Union (Texas)
-module(f5_reg_credit_union_tx_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_reg_credit_union_tx_sup:start_link().

stop(_State) ->
    ok.
