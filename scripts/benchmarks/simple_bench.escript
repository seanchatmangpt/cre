#!/usr/bin/env escript
%%
%% Simple Erlang Performance Benchmark
%%

main(_Args) ->
    io:format("~n"),
    io:format("Testing basic Erlang performance...~n~n"),

    %% Process creation benchmark
    io:format("Process Creation:~n"),
    NumProcs = 10000,
    StartProc = erlang:monotonic_time(microsecond),
    _Pids = [spawn(fun() -> ok end) || _ <- lists:seq(1, NumProcs)],
    EndProc = erlang:monotonic_time(microsecond),
    ProcTime = (EndProc - StartProc) / NumProcs,
    io:format("  Created ~p processes in ~.2f ms~n", [NumProcs, (EndProc - StartProc) / 1000]),
    io:format("  Time per process: ~.2f us~n", [ProcTime]),
    io:format("  Processes/sec: ~.2f~n~n", [1000000 / ProcTime]),

    %% Message passing benchmark
    io:format("Message Passing:~n"),
    Self = self(),
    TestProc = spawn(fun() ->
        receive_loop(Self, 0)
    end),

    NumMsgs = 10000,
    StartMsg = erlang:monotonic_time(microsecond),
    [TestProc ! {msg, N} || N <- lists:seq(1, NumMsgs)],
    TestProc ! {done, Self},
    receive
        {done, Count} ->
            EndMsg = erlang:monotonic_time(microsecond),
            MsgTime = (EndMsg - StartMsg) / NumMsgs,
            io:format("  Sent/received ~p messages in ~.2f ms~n", [Count, (EndMsg - StartMsg) / 1000]),
            io:format("  Time per message: ~.2f us~n", [MsgTime]),
            io:format("  Messages/sec: ~.2f~n~n", [1000000 / MsgTime])
    after 5000 ->
        io:format("  ERROR: Timeout~n~n")
    end,

    %% ETS benchmark
    io:format("ETS Operations:~n"),
    Tab = ets:new(bench_tab, [set, public]),
    NumOps = 100000,

    %% Insert benchmark
    StartInsert = erlang:monotonic_time(microsecond),
    [ets:insert(Tab, {N, data}) || N <- lists:seq(1, NumOps)],
    EndInsert = erlang:monotonic_time(microsecond),
    InsertTime = (EndInsert - StartInsert) / NumOps,
    io:format("  Insert: ~.2f us/op (~.2f ops/sec)~n", [InsertTime, 1000000 / InsertTime]),

    %% Lookup benchmark
    StartLookup = erlang:monotonic_time(microsecond),
    [ets:lookup(Tab, N) || N <- lists:seq(1, NumOps)],
    EndLookup = erlang:monotonic_time(microsecond),
    LookupTime = (EndLookup - StartLookup) / NumOps,
    io:format("  Lookup: ~.2f us/op (~.2f ops/sec)~n", [LookupTime, 1000000 / LookupTime]),

    ets:delete(Tab),
    io:format("~n"),

    %% Map operations benchmark
    io:format("Map Operations:~n"),
    TestMap = maps:from_list([{N, N * 2} || N <- lists:seq(1, 1000)]),

    NumMapOps = 100000,
    StartMapGet = erlang:monotonic_time(microsecond),
    [maps:get(rand:uniform(1000), TestMap) || _ <- lists:seq(1, NumMapOps)],
    EndMapGet = erlang:monotonic_time(microsecond),
    MapGetTime = (EndMapGet - StartMapGet) / NumMapOps,
    io:format("  Get: ~.2f us/op (~.2f ops/sec)~n", [MapGetTime, 1000000 / MapGetTime]),

    StartMapPut = erlang:monotonic_time(microsecond),
    [maps:put(N, N * 2, TestMap) || N <- lists:seq(1, NumMapOps)],
    EndMapPut = erlang:monotonic_time(microsecond),
    MapPutTime = (EndMapPut - StartMapPut) / NumMapOps,
    io:format("  Put: ~.2f us/op (~.2f ops/sec)~n", [MapPutTime, 1000000 / MapPutTime]),

    io:format("~n"),
    io:format("Benchmark complete!~n~n"),

    halt(0).

receive_loop(Parent, Count) ->
    receive
        {msg, _} ->
            receive_loop(Parent, Count + 1);
        {done, Parent} ->
            Parent ! {done, Count}
    end.
