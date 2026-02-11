%% Certification Runner Application
-module(f5_cert_runner_app).
-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    f5_cert_runner_sup:start_link().

-spec stop(term()) -> ok.
stop(_State) ->
    ok.
