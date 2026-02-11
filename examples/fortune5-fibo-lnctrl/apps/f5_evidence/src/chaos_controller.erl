%% Chaos Engineering Controller
%% Injects faults to test system resilience
-module(chaos_controller).
-export([kill_random_processes/1, partition_nodes/1, exhaust_memory/1]).

kill_random_processes(Config) ->
    KillRate = maps:get(kill_rate, Config, 10),  %% per second
    Duration = maps:get(duration, Config, 60),
    Excluded = maps:get(excluded, Config, []),

    io:format("~n=== CHAOS: Kill Random Processes ===~n"),
    io:format("Kill rate: ~p processes/second~n", [KillRate]),
    io:format("Duration: ~p seconds~n", [Duration]),

    StartTime = erlang:system_time(second),
    EndTime = StartTime + Duration,

    chaos_kill_loop(EndTime, KillRate, Excluded).

chaos_kill_loop(EndTime, KillRate, Excluded) ->
    Now = erlang:system_time(second),
    if Now >= EndTime ->
        io:format("Chaos experiment complete~n"),
        ok;
       true ->
        %% Kill processes
        lists:foreach(fun(_) ->
            Processes = erlang:processes(),
            RandomProc = lists:nth(rand:uniform(length(Processes)), Processes),

            case should_kill(RandomProc, Excluded) of
                true ->
                    exit(RandomProc, chaos_experiment),
                    io:format("  Killed process: ~p~n", [RandomProc]);
                false ->
                    ok
            end
        end, lists:seq(1, KillRate)),

        timer:sleep(1000),
        chaos_kill_loop(EndTime, KillRate, Excluded)
    end.

should_kill(Proc, Excluded) ->
    case process_info(Proc, registered_name) of
        {registered_name, Name} ->
            not lists:member(Name, Excluded);
        _ ->
            true
    end.

partition_nodes(Config) ->
    io:format("~n=== CHAOS: Network Partition ===~n"),
    %% Would implement network partition simulation
    %% For single-node, simulate by blocking gen_server calls
    {ok, simulated}.

exhaust_memory(Config) ->
    TargetUsage = maps:get(target_usage, Config, 90),  %% percent
    Duration = maps:get(duration, Config, 60),

    io:format("~n=== CHAOS: Memory Exhaustion ===~n"),
    io:format("Target usage: ~p%~n", [TargetUsage]),

    %% Allocate memory until target reached
    {total, TotalMem} = lists:keyfind(total, 1, erlang:memory()),
    TargetMem = (TotalMem * TargetUsage) div 100,

    allocate_memory_until(TargetMem, Duration).

allocate_memory_until(TargetMem, Duration) ->
    {total, CurrentMem} = lists:keyfind(total, 1, erlang:memory()),

    if CurrentMem >= TargetMem ->
        io:format("Target memory reached: ~p bytes~n", [CurrentMem]),
        timer:sleep(Duration * 1000),
        ok;
       true ->
        %% Allocate 10MB chunk
        _Chunk = binary:copy(<<0>>, 10 * 1024 * 1024),
        allocate_memory_until(TargetMem, Duration)
    end.
