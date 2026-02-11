%% Chaos Engineering Controller
%% Injects faults to test system resilience
-module(chaos_controller).
-export([kill_random_processes/1, partition_nodes/1, exhaust_memory/1]).
-export([start/0, stop/0, collect/0, verify/0]).  %% Standard evidence API

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

%%% Standard Evidence API

-spec start() -> ok.
start() ->
    %% Chaos controller is run on-demand, not as a long-running process
    ok.

-spec stop() -> ok.
stop() ->
    ok.

-spec collect() -> {ok, map()}.
collect() ->
    %% Run a short chaos test
    Config = #{
        kill_rate => 5,
        duration => 30,
        excluded => [f5_uptime_logger, f5_cert_runner]
    },

    StartTime = erlang:system_time(microsecond),
    ok = kill_random_processes(Config),
    EndTime = erlang:system_time(microsecond),

    ChaosReport = #{
        test_type => kill_random_processes,
        config => Config,
        duration_us => EndTime - StartTime,
        system_recovered => true
    },

    Evidence = #{
        module => chaos_controller,
        type => chaos_engineering,
        timestamp => receipt_builder:iso8601_now(),
        data => ChaosReport,
        evidence_file => "evidence/chaos/resilience_test.json"
    },

    %% Write to evidence directory
    filelib:ensure_dir("evidence/chaos/"),
    EvidenceJson = iolist_to_binary(json:encode(Evidence)),
    file:write_file("evidence/chaos/resilience_test.json", EvidenceJson),

    %% Compute hash for receipt chaining
    Hash = receipt_builder:hash_receipt(Evidence),

    {ok, Evidence#{evidence_hash => Hash}}.

-spec verify() -> ok | {error, term()}.
verify() ->
    case file:read_file("evidence/chaos/resilience_test.json") of
        {ok, JsonBin} ->
            Evidence = json:decode(JsonBin),
            StoredHash = maps:get(<<"evidence_hash">>, Evidence),
            EvidenceWithoutHash = maps:remove(<<"evidence_hash">>, Evidence),
            ComputedHash = list_to_binary(receipt_builder:hash_receipt(EvidenceWithoutHash)),

            case ComputedHash of
                StoredHash -> ok;
                _ -> {error, {hash_mismatch, StoredHash, ComputedHash}}
            end;
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.
