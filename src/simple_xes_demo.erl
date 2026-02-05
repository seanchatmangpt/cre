-module(simple_xes_demo).
-export([run/0]).

run() ->
    io:format("Starting XES logger...~n"),
    {ok, _} = yawl_xes:start_link(),

    io:format("Creating new XES log...~n"),
    {ok, LogId} = yawl_xes:new_log(#{
        <<"workflow">> => <<"simple_test">>,
        <<"order_id">> => <<"TEST-ORDER-001">>
    }),

    io:format("Log ID: ~s~n", [LogId]),

    %% Log some workflow events
    io:format("Logging workflow events...~n"),
    yawl_xes:log_case_start(LogId, <<"CASE-001">>),

    %% WCP-01: Sequence
    yawl_xes:log_pattern_start(LogId, <<"WCP-01">>, <<"Sequence">>),
    timer:sleep(100),
    yawl_xes:log_pattern_complete(LogId, <<"WCP-01">>, <<"Sequence">>, #{<<"status">> => <<"ok">>}),

    %% WCP-02: Parallel Split
    yawl_xes:log_pattern_start(LogId, <<"WCP-02">>, <<"ParallelSplit">>),
    timer:sleep(100),
    yawl_xes:log_pattern_complete(LogId, <<"WCP-02">>, <<"ParallelSplit">>, #{<<"branches">> => 2}),

    %% Workitem events
    yawl_xes:log_workitem_start(LogId, <<"WI-001">>, <<"OrderVerification">>),
    timer:sleep(50),
    yawl_xes:log_workitem_complete(LogId, <<"WI-001">>, <<"OrderVerification">>, #{<<"valid">> => true}),

    %% Token moves
    yawl_xes:log_token_move(LogId, <<"p_start">>, <<"p_input">>, <<"p_processing">>),
    yawl_xes:log_token_move(LogId, <<"p_processing">>, <<"p_processing">>, <<"p_complete">>),

    %% Case complete
    yawl_xes:log_case_complete(LogId, <<"CASE-001">>, #{
        <<"duration">> => 350,
        <<"status">> => <<"completed">>
    }),

    %% List logs
    Logs = yawl_xes:list_logs(),
    io:format("Total logs: ~p~n", [length(Logs)]),

    %% Export XES
    case yawl_xes:export_xes(LogId, "xes_logs") of
        {ok, _} ->
            io:format("XES exported to xes_logs/~s.xes~n", [LogId]);
        {error, E} ->
            io:format("Export error: ~p~n", [E])
    end,

    io:format("~nDemo complete!~n"),
    ok.
