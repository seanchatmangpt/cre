%% Application module for f5_ontology_tools
-module(f5_ontology_tools_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    f5_ontology_tools_sup:start_link().

stop(_State) ->
    ok.
