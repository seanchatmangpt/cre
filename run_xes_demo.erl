#!/usr/bin/env escript
main(_) ->
    code:add_pathsa(["_build/default/lib/*/ebin"]),
    io:format("Starting XES logger...~n"),
    {ok, _} = yawl_xes:start_link(),
    io:format("Running workflow...~n"),
    Result = van_der_aalst_workflow:run_full_workflow(),
    io:format("Result: ~p~n", [Result]),
    {ok, Logs} = yawl_xes:list_logs(),
    io:format("Logs: ~p~n", [length(Logs)]),
    init:stop().
