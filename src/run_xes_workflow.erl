-module(run_xes_workflow).
-export([run/0]).

run() ->
    {ok, _} = yawl_xes:start_link(),
    Result = van_der_aalst_workflow:run_full_workflow(),
    io:format("Result: ~p~n", [Result]),
    {ok, Logs} = yawl_xes:list_logs(),
    io:format("Total logs: ~p~n", [length(Logs)]),
    case Logs of
        [{LogId, _Log} | _] ->
            io:format("Log ID: ~s~n", [LogId]),
            case yawl_xes:export_xes(LogId, "xes_logs") of
                {ok, _} -> io:format("XES exported to xes_logs/~s.xes~n", [LogId]);
                {error, E} -> io:format("Export error: ~p~n", [E])
            end;
        _ ->
            io:format("No logs found~n")
    end,
    ok.
