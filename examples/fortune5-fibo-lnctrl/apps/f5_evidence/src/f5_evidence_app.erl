%% Evidence collection app
-module(f5_evidence_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_evidence_sup:start_link().

stop(_State) ->
    ok.
