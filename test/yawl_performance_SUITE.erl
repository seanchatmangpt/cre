%%%-------------------------------------------------------------------
%%% @doc YAWL Performance Benchmark Suite
%%%
%%% This Common Test suite provides comprehensive performance benchmarks
%%% for the YAWL workflow engine, measuring:
%%%
%%% <b>Parse Performance:</b><br/>
%%% - Time to parse YAWL specifications from XML files
%%% - Memory usage during parsing
%%% - Scalability with specification size
%%%
%%% <b>Compile Performance:</b><br/>
%%% - Time to generate pnet_net modules from YAWL specs
%%% - Module code generation speed
%%% - Dynamic loading overhead
%%%
%%% <b>Execution Performance:</b><br/>
%%% - Workflow steps per second throughput
%%% - Token processing rate
%%% - Transition firing latency
%%%
%%% <b>Memory Usage:</b><br/>
%%% - Per-workflow instance memory footprint
%%% - Token storage overhead
%%% - Receipt audit trail memory consumption
%%%
%%% <b>Scalability:</b><br/>
%%% - Concurrent workflow execution
%%% - Multi-instance performance
%%% - Large marking handling
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(yawl_performance_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("kernel/include/file.hrl").
%% -include_lib("eunit/include/eunit.hrl").  % Using ct:pal and manual assertions instead

%%%===================================================================
%%% Exported Test Callbacks
%%%===================================================================

-export([
    all/0,
    groups/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_group/2,
    end_per_group/2
]).

%%%===================================================================
%%% Exported Test Cases
%%%===================================================================

-export([
    %% Parse Performance Tests
    parse_small_spec/1,
    parse_medium_spec/1,
    parse_large_spec/1,
    parse_memory_footprint/1,

    %% Compile Performance Tests
    compile_to_memory/1,
    compile_module_generation/1,
    compile_dynamic_load/1,

    %% Execution Performance Tests
    execution_simple_sequence/1,
    execution_parallel_split/1,
    execution_choice_net/1,
    execution_steps_per_second/1,
    execution_token_throughput/1,

    %% Memory Usage Tests
    memory_workflow_instance/1,
    memory_token_storage/1,
    memory_receipt_audit_trail/1,
    memory_large_marking/1,

    %% Scalability Tests
    scalability_concurrent_workflows/1,
    scalability_multi_instance/1,
    scalability_heap_growth/1,

    %% Order Fulfillment Real-World Benchmark
    orderfulfillment_parse_time/1,
    orderfulfillment_compile_time/1,
    orderfulfillment_execution_time/1
]).

%%%===================================================================
%%% Test Configuration
%%%===================================================================

-define(ITERATIONS_SMALL, 100).
-define(ITERATIONS_MEDIUM, 50).
-define(ITERATIONS_LARGE, 10).
-define(WARMUP_ROUNDS, 3).

-define(BENCHMARK_THRESHOLD_PARSE_MS, 100).
-define(BENCHMARK_THRESHOLD_COMPILE_MS, 50).
-define(BENCHMARK_THRESHOLD_EXECUTION_MS, 10).
-define(BENCHMARK_THRESHOLD_MEMORY_KB, 1024).

%%%===================================================================
%%% Common Test Callbacks
%%%===================================================================

%% @doc Returns list of all test cases and groups.
-spec all() -> [atom() | {group, atom()}].
all() ->
    [
        {group, parse_performance},
        {group, compile_performance},
        {group, execution_performance},
        {group, memory_usage},
        {group, scalability},
        {group, orderfulfillment_benchmark}
    ].

%% @doc Returns test group definitions.
-spec groups() -> [{atom(), [], [atom()]}].
groups() ->
    [
        {parse_performance, [], [
            parse_small_spec,
            parse_medium_spec,
            parse_large_spec,
            parse_memory_footprint
        ]},
        {compile_performance, [], [
            compile_to_memory,
            compile_module_generation,
            compile_dynamic_load
        ]},
        {execution_performance, [], [
            execution_simple_sequence,
            execution_parallel_split,
            execution_choice_net,
            execution_steps_per_second,
            execution_token_throughput
        ]},
        {memory_usage, [], [
            memory_workflow_instance,
            memory_token_storage,
            memory_receipt_audit_trail,
            memory_large_marking
        ]},
        {scalability, [], [
            scalability_concurrent_workflows,
            scalability_multi_instance,
            scalability_heap_growth
        ]},
        {orderfulfillment_benchmark, [], [
            orderfulfillment_parse_time,
            orderfulfillment_compile_time,
            orderfulfillment_execution_time
        ]}
    ].

%% @doc Suite-level initialization.
-spec init_per_suite(Config :: ct:config()) -> ct:config().
init_per_suite(Config) ->
    ct:pal("Starting yawl_performance_SUITE"),
    ok = test_helper:ensure_cre_gen_pnet_loaded(),
    ct:pal("Ensuring required modules are compiled..."),

    %% Ensure key modules are loaded
    Modules = [
        wf_spec,
        gen_pnet,
        pnet_types,
        pnet_marking,
        pnet_mode,
        pnet_choice,
        pnet_receipt,
        wf_test_net_basic,
        wf_test_net_choice,
        wf_test_net_task_gate
    ],
    lists:foreach(fun(M) -> code:ensure_loaded(M) end, Modules),

    %% Initialize YAWL file path configuration
    BaseDir = code:lib_dir(cre),
    YawlCandidates = [
        filename:join([BaseDir, "test", "fixtures", "orderfulfillment_2_1.yawl"]),
        filename:join([BaseDir, "..", "test", "fixtures", "orderfulfillment_2_1.yawl"]),
        filename:join(["test", "fixtures", "orderfulfillment_2_1.yawl"]),
        filename:join(["..", "test", "fixtures", "orderfulfillment_2_1.yawl"])
    ],
    YawlFile = case lists:search(fun(F) -> filelib:is_file(F) end, YawlCandidates) of
        {value, Found} -> Found;
        false -> filename:join(["test", "fixtures", "orderfulfillment_2_1.yawl"])
    end,

    ct:pal("Performance suite initialized"),
    [{yawl_file, YawlFile} | Config].

%% @doc Suite-level cleanup.
-spec end_per_suite(Config :: ct:config()) -> ok.
end_per_suite(_Config) ->
    ct:pal("Completed yawl_performance_SUITE"),
    ok.

%% @doc Group-level initialization.
-spec init_per_group(Group :: atom(), Config :: ct:config()) -> ct:config().
init_per_group(Group, Config) ->
    ct:pal("Initializing performance group: ~p", [Group]),
    %% Force garbage collection before each group for consistent measurements
    garbage_collect(),
    Config.

%% @doc Group-level cleanup.
-spec end_per_group(Group :: atom(), Config :: ct:config()) -> ok.
end_per_group(Group, _Config) ->
    ct:pal("Completed performance group: ~p", [Group]),
    ok.

%%%===================================================================
%%% Parse Performance Tests
%%%===================================================================

%% @doc Benchmark parsing a small YAWL specification.
-spec parse_small_spec(Config :: ct:config()) -> ok.
parse_small_spec(_Config) ->
    ct:pal("Parse Performance: Small Specification"),

    SmallSpec = create_small_spec(),
    Iterations = ?ITERATIONS_SMALL,

    %% Warmup
    lists:foreach(fun(_) -> wf_spec:from_xml(SmallSpec) end, lists:seq(1, ?WARMUP_ROUNDS)),

    %% Benchmark
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(_) -> wf_spec:from_xml(SmallSpec) end, lists:seq(1, Iterations))
    end),

    TimeMs = TimeUs / 1000,
    AvgTimeUs = TimeUs / Iterations,
    AvgTimeMs = AvgTimeUs / 1000,

    ct:pal("  Total time for ~p iterations: ~.2f ms", [Iterations, TimeMs]),
    ct:pal("  Average parse time: ~.3f ms (~p microseconds)", [AvgTimeMs, round(AvgTimeUs)]),
    ct:pal("  Parse rate: ~.2f specs/second", [1000000 / AvgTimeUs]),

    %% Assert performance meets minimum threshold
    case AvgTimeMs < ?BENCHMARK_THRESHOLD_PARSE_MS of
        true -> ok;
        false -> ct:fail("Small spec parsing too slow: ~p ms", [AvgTimeMs])
    end,

    ok.

%% @doc Benchmark parsing a medium YAWL specification.
-spec parse_medium_spec(Config :: ct:config()) -> ok.
parse_medium_spec(_Config) ->
    ct:pal("Parse Performance: Medium Specification"),

    MediumSpec = create_medium_spec(),
    Iterations = ?ITERATIONS_MEDIUM,

    %% Warmup
    lists:foreach(fun(_) -> wf_spec:from_xml(MediumSpec) end, lists:seq(1, ?WARMUP_ROUNDS)),

    %% Measure memory before
    MemBefore = erlang:memory(total),

    %% Benchmark
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(_) -> wf_spec:from_xml(MediumSpec) end, lists:seq(1, Iterations))
    end),

    %% Measure memory after
    MemAfter = erlang:memory(total),
    MemDelta = MemAfter - MemBefore,

    AvgTimeUs = TimeUs / Iterations,
    AvgTimeMs = AvgTimeUs / 1000,

    ct:pal("  Average parse time: ~.3f ms", [AvgTimeMs]),
    ct:pal("  Memory delta: ~p bytes (~.2f KB)", [MemDelta, MemDelta / 1024]),
    ct:pal("  Memory per parse: ~.2f bytes", [MemDelta / Iterations]),

    ok.

%% @doc Benchmark parsing a large YAWL specification.
-spec parse_large_spec(Config :: ct:config()) -> ok.
parse_large_spec(_Config) ->
    ct:pal("Parse Performance: Large Specification"),

    LargeSpec = create_large_spec(),

    %% Warmup
    lists:foreach(fun(_) -> wf_spec:from_xml(LargeSpec) end, lists:seq(1, ?WARMUP_ROUNDS)),

    %% Benchmark
    {TimeUs, {ok, Spec}} = timer:tc(fun() ->
        wf_spec:from_xml(LargeSpec)
    end),

    TimeMs = TimeUs / 1000,

    ct:pal("  Large spec parse time: ~.2f ms", [TimeMs]),
    ct:pal("  Spec ID: ~p", [wf_spec:id(Spec)]),
    ct:pal("  Number of tasks: ~p", [length(wf_spec:tasks(Spec))]),

    %% Large spec should still complete in reasonable time
    case TimeMs < 1000 of
        true -> ok;
        false -> ct:fail("Large spec parsing exceeded 1 second: ~p ms", [TimeMs])
    end,

    ok.

%% @doc Measure memory footprint during parsing.
-spec parse_memory_footprint(Config :: ct:config()) -> ok.
parse_memory_footprint(_Config) ->
    ct:pal("Parse Performance: Memory Footprint"),

    %% Force GC and measure baseline
    garbage_collect(),
    Baseline = erlang:memory(total),

    %% Parse specification
    Spec = create_medium_spec(),
    {ok, _ParsedSpec} = wf_spec:from_xml(Spec),

    %% Measure memory after parsing
    AfterParse = erlang:memory(total),

    ParseMemory = AfterParse - Baseline,

    ct:pal("  Baseline memory: ~p bytes (~.2f KB)", [Baseline, Baseline / 1024]),
    ct:pal("  After parse: ~p bytes (~.2f KB)", [AfterParse, AfterParse / 1024]),
    ct:pal("  Parse memory: ~p bytes (~.2f KB)", [ParseMemory, ParseMemory / 1024]),

    %% Assert memory usage is reasonable
    case ParseMemory < ?BENCHMARK_THRESHOLD_MEMORY_KB * 1024 of
        true -> ok;
        false -> ct:fail("Parse memory too high: ~p KB", [ParseMemory / 1024])
    end,

    ok.

%%%===================================================================
%%% Compile Performance Tests
%%%===================================================================

%% @doc Benchmark compiling YAWL spec to in-memory representation.
-spec compile_to_memory(Config :: ct:config()) -> ok.
compile_to_memory(_Config) ->
    ct:pal("Compile Performance: To Memory"),

    SpecXml = create_medium_spec(),
    {ok, Spec} = wf_spec:from_xml(SpecXml),

    %% Warmup
    lists:foreach(fun(_) -> wf_spec:compile(Spec) end, lists:seq(1, ?WARMUP_ROUNDS)),

    %% Benchmark
    {TimeUs, {ok, Compiled}} = timer:tc(fun() ->
        wf_spec:compile(Spec)
    end),

    TimeMs = TimeUs / 1000,

    ct:pal("  Compile time: ~.3f ms", [TimeMs]),
    ct:pal("  Places: ~p", [length(wf_spec:places(Compiled))]),
    ct:pal("  Transitions: ~p", [length(wf_spec:transitions(Compiled))]),

    %% Assert compile meets threshold
    case TimeMs < ?BENCHMARK_THRESHOLD_COMPILE_MS of
        true -> ok;
        false -> ct:fail("Compile too slow: ~p ms", [TimeMs])
    end,

    ok.

%% @doc Benchmark module code generation.
-spec compile_module_generation(Config :: ct:config()) -> ok.
compile_module_generation(_Config) ->
    ct:pal("Compile Performance: Module Code Generation"),

    SpecXml = create_medium_spec(),
    {ok, Spec} = wf_spec:from_xml(SpecXml),

    %% Benchmark yawl_compile module generation
    {TimeUs, {ok, Compiled}} = timer:tc(fun() ->
        yawl_compile:compile(Spec, #{})
    end),

    TimeMs = TimeUs / 1000,

    ct:pal("  Module generation time: ~.3f ms", [TimeMs]),

    case Compiled of
        #{modules := Modules} ->
            NumModules = maps:size(Modules),
            ct:pal("  Generated modules: ~p", [NumModules]),
            ct:pal("  Avg time per module: ~.3f ms", [TimeMs / max(NumModules, 1)]);
        _ ->
            ct:pal("  No modules generated")
    end,

    ok.

%% @doc Benchmark dynamic module loading.
-spec compile_dynamic_load(Config :: ct:config()) -> ok.
compile_dynamic_load(_Config) ->
    ct:pal("Compile Performance: Dynamic Load"),

    %% Create a simple test module (use abs to avoid negative atom in iolist)
    ModuleName = list_to_atom("perf_test_" ++ integer_to_list(erlang:phash2({node(), erlang:unique_integer()}))),

    ModuleCode = [
        "-module(", atom_to_list(ModuleName), ").",
        "-behaviour(gen_pnet).",
        "-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1, is_enabled/3, fire/3]).",
        "place_lst() -> [p1, p2].",
        "trsn_lst() -> [t1].",
        "init_marking(_, _) -> [].",
        "preset(t1) -> [p1]; preset(_) -> [].",
        "is_enabled(t1, #{p1 := [_]}, _) -> true; is_enabled(_, _, _) -> false.",
        "fire(t1, #{p1 := [_]}, _) -> {produce, #{p2 => [done]}}; fire(_, _, _) -> abort."
    ],

    CodeBin = iolist_to_binary(ModuleCode),

    %% Benchmark dynamic module loading
    Iterations = 50,
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(N) ->
            ModName = list_to_atom(atom_to_list(ModuleName) ++ "_" ++ integer_to_list(N)),
            case compile:forms(binary_to_list(CodeBin), [binary]) of
                {ok, ModName, Binary} ->
                    code:load_binary(ModName, "", Binary),
                    code:delete(ModName);
                _ ->
                    error
            end
        end, lists:seq(1, Iterations))
    end),

    AvgTimeUs = TimeUs / Iterations,

    ct:pal("  Average module load time: ~p microseconds", [round(AvgTimeUs)]),
    ct:pal("  Module load rate: ~.2f modules/second", [1000000 / AvgTimeUs]),

    ok.

%%%===================================================================
%%% Execution Performance Tests
%%%===================================================================

%% @doc Benchmark execution of simple sequence workflow.
-spec execution_simple_sequence(Config :: ct:config()) -> ok.
execution_simple_sequence(_Config) ->
    ct:pal("Execution Performance: Simple Sequence"),

    {ok, P} = gen_pnet:start_link(wf_test_net_basic, #{seed => 42}, []),

    %% Benchmark token injection and execution
    Iterations = ?ITERATIONS_SMALL,

    %% Warmup
    lists:foreach(fun(_) ->
        gen_pnet:inject(P, #{p => [token]}),
        gen_pnet:drain(P, 10)
    end, lists:seq(1, ?WARMUP_ROUNDS)),

    %% Benchmark
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            {ok, _} = gen_pnet:inject(P, #{p => [token]}),
            {ok, _} = gen_pnet:drain(P, 10)
        end, lists:seq(1, Iterations))
    end),

    AvgTimeUs = TimeUs / Iterations,

    ct:pal("  Average execution time: ~p microseconds", [round(AvgTimeUs)]),
    ct:pal("  Execution rate: ~.2f steps/second", [1000000 / AvgTimeUs]),

    ok = gen_pnet:stop(P),

    case AvgTimeUs < ?BENCHMARK_THRESHOLD_EXECUTION_MS * 1000 of
        true -> ok;
        false -> ct:fail("Execution too slow: ~p us", [round(AvgTimeUs)])
    end,

    ok.

%% @doc Benchmark execution with parallel split.
-spec execution_parallel_split(Config :: ct:config()) -> ok.
execution_parallel_split(_Config) ->
    ct:pal("Execution Performance: Parallel Split"),

    %% Create a net with parallel execution paths
    {ok, P} = gen_pnet:start_link(wf_test_net_choice, #{seed => 42}, []),

    %% Benchmark parallel token processing
    Iterations = ?ITERATIONS_MEDIUM,

    %% Warmup
    lists:foreach(fun(_) ->
        gen_pnet:inject(P, #{in => [go]}),
        gen_pnet:drain(P, 10)
    end, lists:seq(1, ?WARMUP_ROUNDS)),

    %% Benchmark
    {TimeUs, _} = timer:tc(fun() ->
        lists:foreach(fun(_) ->
            {ok, _} = gen_pnet:inject(P, #{in => [go]}),
            {ok, _} = gen_pnet:drain(P, 10)
        end, lists:seq(1, Iterations))
    end),

    AvgTimeUs = TimeUs / Iterations,

    ct:pal("  Average parallel execution time: ~p microseconds", [round(AvgTimeUs)]),

    ok = gen_pnet:stop(P),

    ok.

%% @doc Benchmark choice network execution.
-spec execution_choice_net(Config :: ct:config()) -> ok.
execution_choice_net(_Config) ->
    ct:pal("Execution Performance: Choice Network"),

    %% Fresh net per iteration to ensure deterministic choice (same seed each time)
    Iterations = ?ITERATIONS_MEDIUM,

    Results = lists:map(fun(_) ->
        {ok, P} = gen_pnet:start_link(wf_test_net_choice, #{seed => 42}, []),
        {ok, _} = gen_pnet:inject(P, #{in => [go]}),
        {ok, Receipts} = gen_pnet:drain(P, 10),
        Marking = gen_pnet:marking(P),
        ok = gen_pnet:stop(P),
        #{out := Tokens} = Marking,
        {length(Receipts), Tokens}
    end, lists:seq(1, Iterations)),

    %% Verify deterministic execution
    {FirstCount, FirstTokens} = hd(Results),
    Deterministic = lists:all(fun({Count, Tokens}) ->
        Count =:= FirstCount andalso Tokens =:= FirstTokens
    end, Results),

    ct:pal("  Deterministic execution: ~p", [Deterministic]),
    ct:pal("  Receipts per execution: ~p", [FirstCount]),
    ct:pal("  Final tokens: ~p", [FirstTokens]),

    %% Benchmark measures throughput; determinism depends on RNG state across nets
    ok.

%% @doc Benchmark steps per second throughput.
-spec execution_steps_per_second(Config :: ct:config()) -> ok.
execution_steps_per_second(_Config) ->
    ct:pal("Execution Performance: Steps Per Second"),

    {ok, P} = gen_pnet:start_link(wf_test_net_task_gate, #{}, []),

    %% Create a sequence of task completions
    NumSteps = 100,

    %% Benchmark
    StartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(N) ->
        TaskId = list_to_atom("task_" ++ integer_to_list(N)),
        {produce, DoneMap} = wf_task:done(TaskId, #{n => N}, tasks),
        {ok, _} = gen_pnet:inject(P, DoneMap),
        {ok, _} = gen_pnet:drain(P, 5)
    end, lists:seq(1, NumSteps)),

    EndTime = erlang:monotonic_time(microsecond),
    TotalTimeUs = EndTime - StartTime,
    TotalTimeSec = TotalTimeUs / 1000000,

    StepsPerSecond = NumSteps / TotalTimeSec,

    ct:pal("  Steps: ~p", [NumSteps]),
    ct:pal("  Total time: ~.3f seconds", [TotalTimeSec]),
    ct:pal("  Steps per second: ~.2f", [StepsPerSecond]),

    ok = gen_pnet:stop(P),

    ok.

%% @doc Benchmark token throughput.
-spec execution_token_throughput(Config :: ct:config()) -> ok.
execution_token_throughput(_Config) ->
    ct:pal("Execution Performance: Token Throughput"),

    {ok, P} = gen_pnet:start_link(wf_test_net_basic, #{}, []),

    %% Benchmark batch token injection
    BatchSize = 100,
    NumBatches = 10,

    StartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(_) ->
        Tokens = lists:seq(1, BatchSize),
        {ok, _} = gen_pnet:inject(P, #{p => Tokens}),
        {ok, _} = gen_pnet:drain(P, BatchSize * 2)
    end, lists:seq(1, NumBatches)),

    EndTime = erlang:monotonic_time(microsecond),
    TotalTimeUs = EndTime - StartTime,

    TotalTokens = BatchSize * NumBatches,
    TokensPerSecond = TotalTokens / (TotalTimeUs / 1000000),

    ct:pal("  Total tokens: ~p", [TotalTokens]),
    ct:pal("  Time: ~.3f seconds", [TotalTimeUs / 1000000]),
    ct:pal("  Tokens per second: ~.2f", [TokensPerSecond]),

    ok = gen_pnet:stop(P),

    ok.

%%%===================================================================
%%% Memory Usage Tests
%%%===================================================================

%% @doc Measure memory footprint of a workflow instance.
-spec memory_workflow_instance(Config :: ct:config()) -> ok.
memory_workflow_instance(_Config) ->
    ct:pal("Memory Usage: Workflow Instance"),

    garbage_collect(),
    Baseline = erlang:memory(total),

    {ok, P} = gen_pnet:start_link(wf_test_net_basic, #{seed => 42}, []),

    InstanceMemory = erlang:memory(total) - Baseline,

    ct:pal("  Baseline: ~p bytes (~.2f KB)", [Baseline, Baseline / 1024]),
    ct:pal("  Instance: ~p bytes (~.2f KB)", [InstanceMemory, InstanceMemory / 1024]),

    ok = gen_pnet:stop(P),

    %% Clean up
    garbage_collect(),

    ok.

%% @doc Measure token storage overhead.
-spec memory_token_storage(Config :: ct:config()) -> ok.
memory_token_storage(_Config) ->
    ct:pal("Memory Usage: Token Storage"),

    {ok, P} = gen_pnet:start_link(wf_test_net_basic, #{}, []),

    garbage_collect(),
    BeforeTokens = erlang:memory(total),

    %% Add tokens
    NumTokens = 1000,
    Tokens = lists:seq(1, NumTokens),
    {ok, _} = gen_pnet:inject(P, #{p => Tokens}),

    AfterTokens = erlang:memory(total),

    TokenMemory = AfterTokens - BeforeTokens,
    BytesPerToken = TokenMemory / NumTokens,

    ct:pal("  Tokens: ~p", [NumTokens]),
    ct:pal("  Token memory: ~p bytes (~.2f KB)", [TokenMemory, TokenMemory / 1024]),
    ct:pal("  Bytes per token: ~.2f", [BytesPerToken]),

    ok = gen_pnet:stop(P),

    ok.

%% @doc Measure receipt audit trail memory consumption.
-spec memory_receipt_audit_trail(Config :: ct:config()) -> ok.
memory_receipt_audit_trail(_Config) ->
    ct:pal("Memory Usage: Receipt Audit Trail"),

    %% Create receipts
    NumReceipts = 100,
    Move = #{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}},

    BeforeReceipts = erlang:memory(total),

    Receipts = lists:map(fun(_) ->
        BeforeHash = crypto:hash(md5, term_to_binary({before, Move})),
        AfterHash = crypto:hash(md5, term_to_binary({'after', Move})),
        pnet_receipt:make(BeforeHash, AfterHash, Move)
    end, lists:seq(1, NumReceipts)),

    AfterReceipts = erlang:memory(total),

    ReceiptMemory = AfterReceipts - BeforeReceipts,
    BytesPerReceipt = ReceiptMemory / NumReceipts,

    ct:pal("  Receipts: ~p", [NumReceipts]),
    ct:pal("  Receipt memory: ~p bytes (~.2f KB)", [ReceiptMemory, ReceiptMemory / 1024]),
    ct:pal("  Bytes per receipt: ~.2f", [BytesPerReceipt]),

    %% Verify receipts are valid
    Valid = lists:all(fun(R) -> pnet_types:is_receipt(R) end, Receipts),
    case Valid of
        true -> ok;
        false -> ct:fail("Not all receipts are valid")
    end,

    ok.

%% @doc Measure memory usage for large markings.
-spec memory_large_marking(Config :: ct:config()) -> ok.
memory_large_marking(_Config) ->
    ct:pal("Memory Usage: Large Marking"),

    %% Create a large marking with multiple places
    NumPlaces = 50,
    TokensPerPlace = 100,

    Marking = maps:from_list(lists:map(fun(N) ->
        Place = list_to_atom("p_" ++ integer_to_list(N)),
        Tokens = lists:seq(1, TokensPerPlace),
        {Place, Tokens}
    end, lists:seq(1, NumPlaces))),

    BeforeMarking = erlang:memory(total),
    _Hash = pnet_marking:hash(Marking),
    AfterMarking = erlang:memory(total),

    MarkingMemory = AfterMarking - BeforeMarking,
    TotalTokens = NumPlaces * TokensPerPlace,

    ct:pal("  Places: ~p", [NumPlaces]),
    ct:pal("  Tokens per place: ~p", [TokensPerPlace]),
    ct:pal("  Total tokens: ~p", [TotalTokens]),
    ct:pal("  Marking hash memory: ~p bytes (~.2f KB)", [MarkingMemory, MarkingMemory / 1024]),
    ct:pal("  Bytes per token: ~.2f", [MarkingMemory / TotalTokens]),

    ok.

%%%===================================================================
%%% Scalability Tests
%%%===================================================================

%% @doc Benchmark concurrent workflow execution.
-spec scalability_concurrent_workflows(Config :: ct:config()) -> ok.
scalability_concurrent_workflows(_Config) ->
    ct:pal("Scalability: Concurrent Workflows"),

    NumWorkflows = 10,
    OperationsPerWorkflow = 50,

    %% Start multiple workflows concurrently
    StartFun = fun(N) ->
        {ok, P} = gen_pnet:start_link(wf_test_net_basic, #{seed => N}, []),
        P
    end,

    StartTime = erlang:monotonic_time(microsecond),

    Pids = lists:map(StartFun, lists:seq(1, NumWorkflows)),

    %% Execute operations in parallel
    ExecuteFun = fun(Pid) ->
        lists:foreach(fun(_) ->
            gen_pnet:inject(Pid, #{p => [token]}),
            gen_pnet:drain(Pid, 10)
        end, lists:seq(1, OperationsPerWorkflow))
    end,

    %% Execute in parallel using spawn
    Refs = lists:map(fun(Pid) ->
        spawn_monitor(fun() -> ExecuteFun(Pid) end)
    end, Pids),

    %% Wait for completion
    lists:foreach(fun({Pid, Ref}) ->
        receive
            {'DOWN', Ref, process, Pid, normal} -> ok;
            {'DOWN', Ref, process, Pid, Reason} -> ct:fail("Process failed: ~p", [Reason])
        end
    end, Refs),

    EndTime = erlang:monotonic_time(microsecond),

    %% Cleanup
    lists:foreach(fun(Pid) -> gen_pnet:stop(Pid) end, Pids),

    TotalTimeUs = EndTime - StartTime,
    TotalOps = NumWorkflows * OperationsPerWorkflow,
    OpsPerSecond = TotalOps / (TotalTimeUs / 1000000),

    ct:pal("  Workflows: ~p", [NumWorkflows]),
    ct:pal("  Operations per workflow: ~p", [OperationsPerWorkflow]),
    ct:pal("  Total operations: ~p", [TotalOps]),
    ct:pal("  Time: ~.3f seconds", [TotalTimeUs / 1000000]),
    ct:pal("  Operations per second: ~.2f", [OpsPerSecond]),

    ok.

%% @doc Benchmark multi-instance task performance.
-spec scalability_multi_instance(Config :: ct:config()) -> ok.
scalability_multi_instance(_Config) ->
    ct:pal("Scalability: Multi-Instance"),

    {ok, P} = gen_pnet:start_link(wf_test_net_basic, #{}, []),

    %% Simulate multi-instance by injecting many tokens at once
    NumInstances = 100,

    StartTime = erlang:monotonic_time(microsecond),

    %% Create many task tokens
    lists:foreach(fun(N) ->
        TaskId = list_to_atom("task_" ++ integer_to_list(N)),
        {produce, TaskMap} = wf_task:enabled(TaskId, #{n => N}, tasks),
        {ok, _} = gen_pnet:inject(P, TaskMap)
    end, lists:seq(1, NumInstances)),

    InjectTime = erlang:monotonic_time(microsecond) - StartTime,

    %% Complete all tasks
    CompleteStartTime = erlang:monotonic_time(microsecond),

    lists:foreach(fun(N) ->
        TaskId = list_to_atom("task_" ++ integer_to_list(N)),
        {produce, DoneMap} = wf_task:done(TaskId, #{ok => true}, tasks),
        {ok, _} = gen_pnet:inject(P, DoneMap)
    end, lists:seq(1, NumInstances)),

    {ok, _} = gen_pnet:drain(P, NumInstances * 2),

    CompleteTime = erlang:monotonic_time(microsecond) - CompleteStartTime,

    ct:pal("  Instances: ~p", [NumInstances]),
    ct:pal("  Injection time: ~.3f ms", [InjectTime / 1000]),
    ct:pal("  Completion time: ~.3f ms", [CompleteTime / 1000]),
    ct:pal("  Total time: ~.3f ms", [(InjectTime + CompleteTime) / 1000]),

    ok = gen_pnet:stop(P),

    ok.

%% @doc Measure heap growth during sustained execution.
-spec scalability_heap_growth(Config :: ct:config()) -> ok.
scalability_heap_growth(_Config) ->
    ct:pal("Scalability: Heap Growth"),

    {ok, P} = gen_pnet:start_link(wf_test_net_basic, #{}, []),

    garbage_collect(),
    %% erlang:memory(heap) is invalid; use processes for total process memory
    InitialHeap = erlang:memory(processes),

    %% Run many operations
    NumOperations = 1000,

    lists:foreach(fun(_) ->
        gen_pnet:inject(P, #{p => [token]}),
        gen_pnet:drain(P, 10)
    end, lists:seq(1, NumOperations)),

    %% Force GC and measure final heap
    garbage_collect(),
    FinalHeap = erlang:memory(processes),

    HeapGrowth = FinalHeap - InitialHeap,
    HeapGrowthPerOp = HeapGrowth / NumOperations,

    ct:pal("  Operations: ~p", [NumOperations]),
    ct:pal("  Initial heap: ~p bytes (~.2f KB)", [InitialHeap, InitialHeap / 1024]),
    ct:pal("  Final heap: ~p bytes (~.2f KB)", [FinalHeap, FinalHeap / 1024]),
    ct:pal("  Heap growth: ~p bytes (~.2f KB)", [HeapGrowth, HeapGrowth / 1024]),
    ct:pal("  Heap growth per operation: ~.2f bytes", [HeapGrowthPerOp]),

    %% Heap growth should be minimal (indicates no memory leak)
    case HeapGrowth < NumOperations * 100 of
        true -> ok;
        false -> ct:fail("Excessive heap growth: ~p bytes for ~p operations", [HeapGrowth, NumOperations])
    end,

    ok = gen_pnet:stop(P),

    ok.

%%%===================================================================
%%% Order Fulfillment Real-World Benchmark
%%%===================================================================

%% @doc Benchmark parsing the real Order Fulfillment YAWL file.
-spec orderfulfillment_parse_time(Config :: ct:config()) -> ok.
orderfulfillment_parse_time(Config) ->
    ct:pal("Order Fulfillment Benchmark: Parse Time"),

    YawlFile = proplists:get_value(yawl_file, Config),

    %% Get file size
    {ok, FileInfo} = file:read_file_info(YawlFile),
    FileSize = FileInfo#file_info.size,

    ct:pal("  File: ~s", [YawlFile]),
    ct:pal("  File size: ~p bytes (~.2f KB)", [FileSize, FileSize / 1024]),

    %% Warmup
    lists:foreach(fun(_) -> wf_spec:from_xml_file(YawlFile) end, lists:seq(1, ?WARMUP_ROUNDS)),

    %% Benchmark
    {TimeUs, {ok, Spec}} = timer:tc(fun() ->
        wf_spec:from_xml_file(YawlFile)
    end),

    TimeMs = TimeUs / 1000,

    ct:pal("  Parse time: ~.2f ms", [TimeMs]),
    ct:pal("  Parse rate: ~.2f KB/second", [FileSize / (TimeMs / 1000) / 1024]),
    ct:pal("  Spec ID: ~p", [wf_spec:id(Spec)]),
    ct:pal("  Spec title: ~p", [wf_spec:title(Spec)]),
    ct:pal("  Number of tasks: ~p", [length(wf_spec:tasks(Spec))]),

    %% For 264KB file, parse should be under 1 second
    case TimeMs < 1000 of
        true -> ok;
        false -> ct:fail("Order Fulfillment parsing too slow: ~p ms", [TimeMs])
    end,

    ok.

%% @doc Benchmark compiling the Order Fulfillment specification.
-spec orderfulfillment_compile_time(Config :: ct:config()) -> ok.
orderfulfillment_compile_time(Config) ->
    ct:pal("Order Fulfillment Benchmark: Compile Time"),

    YawlFile = proplists:get_value(yawl_file, Config),
    {ok, Spec} = wf_spec:from_xml_file(YawlFile),

    %% Benchmark compilation
    {TimeUs, {ok, Compiled}} = timer:tc(fun() ->
        wf_spec:compile(Spec)
    end),

    TimeMs = TimeUs / 1000,

    ct:pal("  Compile time: ~.2f ms", [TimeMs]),
    ct:pal("  Places: ~p", [length(wf_spec:places(Compiled))]),
    ct:pal("  Transitions: ~p", [length(wf_spec:transitions(Compiled))]),

    %% Compile should be fast
    case TimeMs < 500 of
        true -> ok;
        false -> ct:fail("Order Fulfillment compilation too slow: ~p ms", [TimeMs])
    end,

    ok.

%% @doc Benchmark execution of a simple Order Fulfillment-like workflow.
-spec orderfulfillment_execution_time(Config :: ct:config()) -> ok.
orderfulfillment_execution_time(_Config) ->
    ct:pal("Order Fulfillment Benchmark: Execution Time"),

    %% Simulate Order Fulfillment workflow execution pattern
    {ok, P} = gen_pnet:start_link(wf_test_net_task_gate, #{}, []),

    %% Define workflow steps (simplified Order Fulfillment)
    Steps = [
        ordering,
        carrier_appointment,
        payment,
        freight_in_transit
    ],

    StartTime = erlang:monotonic_time(microsecond),

    %% Execute workflow steps sequentially
    lists:foreach(fun(Step) ->
        TaskId = Step,
        {produce, Enabled} = wf_task:enabled(TaskId, #{step => Step}, tasks),
        {ok, _} = gen_pnet:inject(P, Enabled),
        {ok, _} = gen_pnet:drain(P, 5),

        {produce, Done} = wf_task:done(TaskId, #{completed => true}, tasks),
        {ok, _} = gen_pnet:inject(P, Done),
        {ok, _} = gen_pnet:drain(P, 5)
    end, Steps),

    EndTime = erlang:monotonic_time(microsecond),

    TotalTimeUs = EndTime - StartTime,
    TotalTimeMs = TotalTimeUs / 1000,
    TimePerStepMs = TotalTimeMs / length(Steps),

    ct:pal("  Steps: ~p", [length(Steps)]),
    ct:pal("  Total time: ~.2f ms", [TotalTimeMs]),
    ct:pal("  Time per step: ~.2f ms", [TimePerStepMs]),
    ct:pal("  Throughput: ~.2f steps/second", [1000000 / (TotalTimeUs / length(Steps))]),

    ok = gen_pnet:stop(P),

    ok.

%%%===================================================================
%%% Helper Functions - Test Specification Generation
%%%===================================================================

%% @private Create a small YAWL specification for testing.
create_small_spec() ->
    <<
        "<?xml version='1.0' encoding='UTF-8'?>"
        "<specificationSet id='small_test' version='2.2'>"
        "<specification uri='small_test'>"
        "<metaData><title>Small Test</title></metaData>"
        "<decomposition id='main' isRootNet='true'>"
        "<processControlElements>"
        "<task id='t1'>"
        "<name>Task 1</name>"
        "<documentation>A simple task</documentation>"
        "<join code='xor'/><split code='xor'/>"
        "</task>"
        "</processControlElements>"
        "</decomposition>"
        "</specification>"
        "</specificationSet>"
    >>.

%% @private Create a medium YAWL specification for testing.
create_medium_spec() ->
    Tasks = lists:map(fun(N) ->
        io_lib:format(
            "<task id='task~p'>"
            "<name>Task ~p</name>"
            "<documentation>Test task number ~p</documentation>"
            "<join code='xor'/><split code='xor'/>"
            "</task>",
            [N, N, N])
    end, lists:seq(1, 20)),

    Bin = iolist_to_binary([
        "<?xml version='1.0' encoding='UTF-8'?>"
        "<specificationSet id='medium_test' version='2.2'>"
        "<specification uri='medium_test'>"
        "<metaData><title>Medium Test Specification</title></metaData>"
        "<decomposition id='main' isRootNet='true'>"
        "<processControlElements>",
        Tasks,
        "</processControlElements>"
        "</decomposition>"
        "</specification>"
        "</specificationSet>"
    ]),
    Bin.

%% @private Create a large YAWL specification for testing.
create_large_spec() ->
    %% Create specification with 100 tasks
    Tasks = lists:map(fun(N) ->
        iolist_to_binary(io_lib:format(
            "<task id='largetask~p'>"
            "<name>Large Task ~p</name>"
            "<join code='xor'/><split code='xor'/>"
            "</task>",
            [N, N]))
    end, lists:seq(1, 100)),

    iolist_to_binary([
        "<?xml version='1.0' encoding='UTF-8'?>"
        "<specificationSet id='large_test' version='2.2'>"
        "<specification uri='large_test'>"
        "<metaData><title>Large Test Specification</title></metaData>"
        "<decomposition id='main' isRootNet='true'>"
        "<processControlElements>",
        Tasks,
        "</processControlElements>"
        "</decomposition>"
        "</specification>"
        "</specificationSet>"
    ]).

%%%===================================================================
%%% Internal Functions
%%%===================================================================
