#!/usr/bin/env escript
%%%-----------------------------------------------------------------------------
%%% @doc Launch 20 A2A-CONSTRUCT Agents
%%%
%%% Executable script to start 20 A2A stations and run demonstration.
%%% @end
%%%-----------------------------------------------------------------------------

main([]) ->
    io:format("~n=== A2A-CONSTRUCT: Launching 20 Deterministic Agents ===~n~n"),

    %% Add code path
    code:add_pathz("_build/default/lib/cre/ebin"),

    %% Start the orchestrator
    io:format("[1/3] Starting A2A Orchestrator...~n"),
    {ok, SupPid} = a2a_orchestrator:start(),
    io:format("      Supervisor started: ~p~n~n", [SupPid]),

    timer:sleep(200),

    %% Launch 20 stations
    io:format("[2/3] Launching 20 A2A Stations...~n"),
    {ok, Pids} = a2a_app:launch_20_stations(),
    io:format("      Launched ~p stations~n", [length(Pids)]),

    timer:sleep(500),

    %% Display station configuration
    io:format("~n[3/3] Station Configuration:~n"),
    Stats = a2a_orchestrator:get_station_stats(),
    lists:foreach(fun(#{station_id := Id, protocols := Protos, pid := Pid}) ->
        io:format("      ~s [~p] -> protocols: ~p~n", [Id, Pid, Protos])
    end, Stats),

    %% Create and execute test tasks
    io:format("~n=== Executing Test Tasks Across 20 Agents ===~n~n"),

    Tasks = [
        a2a_types:new_task(<<"echo">>, [string], [string]),
        a2a_types:new_task(<<"transform">>, [json], [xml]),
        a2a_types:new_task(<<"validate">>, [data], [boolean]),
        a2a_types:new_task(<<"aggregate">>, [list], [summary]),
        a2a_types:new_task(<<"route">>, [message], [destination]),
        a2a_types:new_task(<<"echo">>, [binary], [binary]),
        a2a_types:new_task(<<"transform">>, [csv], [json]),
        a2a_types:new_task(<<"validate">>, [schema], [result]),
        a2a_types:new_task(<<"aggregate">>, [events], [metrics]),
        a2a_types:new_task(<<"route">>, [packet], [endpoint])
    ],

    Results = lists:map(fun(Task) ->
        #{protocol := Protocol, id := TaskId} = Task,
        case a2a_orchestrator:submit_task(Protocol, Task) of
            {ok, Pid, {ok, Receipt}} ->
                #{id := ReceiptId} = Receipt,
                io:format("✓ Task ~s executed by ~p -> Receipt: ~s~n",
                         [binary:part(TaskId, 0, 8), Pid, binary:part(ReceiptId, 0, 8)]),
                {success, Receipt};
            {ok, Pid, {refused, Refusal}} ->
                #{type := Type} = Refusal,
                io:format("✗ Task ~s refused by ~p -> Reason: ~p~n",
                         [binary:part(TaskId, 0, 8), Pid, Type]),
                {refused, Refusal};
            {error, Reason} ->
                io:format("✗ Task ~s failed -> Error: ~p~n",
                         [binary:part(TaskId, 0, 8), Reason]),
                {error, Reason}
        end
    end, Tasks),

    timer:sleep(200),

    %% Final statistics
    io:format("~n=== Execution Summary ===~n"),
    Successful = length([X || {success, X} <- Results]),
    Refused = length([X || {refused, X} <- Results]),
    Errors = length([X || {error, X} <- Results]),

    io:format("Total Tasks:    ~p~n", [length(Tasks)]),
    io:format("Successful:     ~p~n", [Successful]),
    io:format("Refused:        ~p~n", [Refused]),
    io:format("Errors:         ~p~n", [Errors]),

    %% Receipt chain statistics
    AllReceipts = a2a_orchestrator:get_all_receipts(),
    TotalReceipts = lists:sum([length(R) || {_, R} <- AllReceipts]),

    io:format("~nTotal Receipts: ~p~n", [TotalReceipts]),
    io:format("Active Stations: ~p~n", [length(Stats)]),

    io:format("~n=== A2A-CONSTRUCT Demonstration Complete ===~n~n"),

    %% Keep running for inspection
    io:format("Stations are running. Press Ctrl+C to exit.~n"),
    receive
        _ -> ok
    after 300000 ->
        ok
    end,

    halt(0).
