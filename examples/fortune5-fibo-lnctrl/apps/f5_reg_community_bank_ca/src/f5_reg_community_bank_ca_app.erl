%% Generated regulation suite app for Community Bank (California)
-module(f5_reg_community_bank_ca_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_reg_community_bank_ca_sup:start_link().

stop(_State) ->
    ok.
