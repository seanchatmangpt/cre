%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc YAWL Compilation Benchmarks
%%
%% Benchmark suite for YAWL to Petri net compilation performance.
%% Measures parsing, compilation, and code generation times.
%%
%% <h3>Metrics Collected</h3>
%%
%% <ul>
%%   <li><b>Parse Time:</b> Time to parse YAWL XML specifications</li>
%%   <li><b>Compile Time:</b> Time to generate Petri net structure</li>
%%   <li><b>Code Gen Time:</b> Time to generate Erlang module code</li>
%%   <li><b>Load Time:</b> Time to dynamically load compiled module</li>
%%   <li><b>Memory Usage:</b> Memory consumed during compilation</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(compilation_benchmarks).
-behaviour(gen_server).
-include_lib("kernel/include/file.hrl").

%% API
-export([
    start_link/0,
    run_all_benchmarks/0,
    run_spec_benchmark/1,
    benchmark_yawl_file/1,
    export_results/1,
    get_results/0,
    create_test_spec/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Constants
%%%===================================================================

-define(WARMUP_ITERATIONS, 3).
-define(BENCHMARK_ITERATIONS, 20).
-define(BASELINE_FILE, "test/bench/compilation_baseline.json").

%%%===================================================================
%%% Test Specification Sizes
%%%===================================================================

-define(SPEC_SIZES, [
    {tiny, {2, 5}},      %% 2 tasks, 5 transitions
    {small, {5, 15}},    %% 5 tasks, 15 transitions
    {medium, {20, 50}},  %% 20 tasks, 50 transitions
    {large, {100, 250}}  %% 100 tasks, 250 transitions
]).

%%%===================================================================
%%% State Record
%%%===================================================================

-record(state, {
    results = #{} :: map(),
    baseline = undefined :: undefined | map()
}).

-record(compilation_result, {
    spec_size :: atom(),
    num_tasks :: pos_integer(),
    num_transitions :: pos_integer(),
    iterations :: pos_integer(),
    parse_time_us :: float(),
    compile_time_us :: float(),
    code_gen_time_us :: float(),
    total_time_us :: float(),
    memory_bytes :: integer()
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start the benchmark server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Run all compilation benchmarks.
-spec run_all_benchmarks() -> {ok, map()}.
run_all_benchmarks() ->
    gen_server:call(?MODULE, run_all_benchmarks, 600000).

%% @doc Run benchmark for a specific specification size.
-spec run_spec_benchmark(atom()) -> {ok, #compilation_result{}}.
run_spec_benchmark(Size) ->
    gen_server:call(?MODULE, {run_spec, Size}, 120000).

%% @doc Benchmark a YAWL file.
-spec benchmark_yawl_file(file:filename()) -> {ok, map()}.
benchmark_yawl_file(Filename) ->
    gen_server:call(?MODULE, {benchmark_file, Filename}, 120000).

%% @doc Export results to file.
-spec export_results(file:filename()) -> ok.
export_results(File) ->
    gen_server:call(?MODULE, {export, File}).

%% @doc Get current benchmark results.
-spec get_results() -> map().
get_results() ->
    gen_server:call(?MODULE, get_results).

%% @doc Create a test YAWL specification.
-spec create_test_spec(atom()) -> binary().
create_test_spec(Size) ->
    {NumTasks, _NumTransitions} = proplists:get_value(Size, ?SPEC_SIZES),
    create_yawl_spec_xml(NumTasks).

%%%===================================================================
%%% gen_server Callbacks
%%%===================================================================

init([]) ->
    {ok, #state{baseline = load_baseline()}}.

handle_call(run_all_benchmarks, _From, State) ->
    Results = run_all_compilation_benchmarks(),
    {reply, {ok, Results}, State#state{results = Results}};

handle_call({run_spec, Size}, _From, State) ->
    Result = benchmark_spec_size(Size),
    {reply, {ok, Result}, State};

handle_call({benchmark_file, Filename}, _From, State) ->
    Result = benchmark_actual_file(Filename),
    {reply, {ok, Result}, State};

handle_call({export, File}, _From, State = #state{results = Results}) ->
    ok = export_to_file(File, Results),
    {reply, ok, State};

handle_call(get_results, _From, State = #state{results = Results}) ->
    {reply, Results, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Benchmark Functions
%%%===================================================================

%% @doc Run all compilation benchmarks.
-spec run_all_compilation_benchmarks() -> map().
run_all_compilation_benchmarks() ->
    lists:foldl(fun({Size, _}, Acc) ->
        Result = benchmark_spec_size(Size),
        maps:put(Size, Result, Acc)
    end, #{}, ?SPEC_SIZES).

%% @doc Benchmark a specific specification size.
-spec benchmark_spec_size(atom()) -> #compilation_result{}.
benchmark_spec_size(Size) ->
    {NumTasks, NumTransitions} = proplists:get_value(Size, ?SPEC_SIZES),
    SpecXML = create_yawl_spec_xml(NumTasks),

    %% Warmup
    lists:foreach(fun(_) ->
        catch wf_spec:from_xml(SpecXML)
    end, lists:seq(1, ?WARMUP_ITERATIONS)),

    %% Benchmark parsing
    {ParseTime, ParseOk} = benchmark_parse(SpecXML),

    case ParseOk of
        {ok, Spec} ->
            %% Benchmark compilation
            {CompileTime, _} = benchmark_compile(Spec),

            %% Benchmark code generation
            {CodeGenTime, _} = benchmark_code_gen(Spec),

            %% Measure memory
            Memory = measure_compilation_memory(SpecXML),

            TotalTime = ParseTime + CompileTime + CodeGenTime,

            #compilation_result{
                spec_size = Size,
                num_tasks = NumTasks,
                num_transitions = NumTransitions,
                iterations = ?BENCHMARK_ITERATIONS,
                parse_time_us = ParseTime,
                compile_time_us = CompileTime,
                code_gen_time_us = CodeGenTime,
                total_time_us = TotalTime,
                memory_bytes = Memory
            };
        {error, _Reason} ->
            #compilation_result{
                spec_size = Size,
                num_tasks = NumTasks,
                num_transitions = NumTransitions,
                iterations = 0,
                parse_time_us = ParseTime,
                compile_time_us = 0.0,
                code_gen_time_us = 0.0,
                total_time_us = ParseTime,
                memory_bytes = 0
            }
    end.

%% @doc Benchmark parsing phase.
-spec benchmark_parse(binary()) -> {float(), {ok, term()} | {error, term()}}.
benchmark_parse(SpecXML) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() -> wf_spec:from_xml(SpecXML) end),
        Time / 1000.0  %% Convert to ms
    end, lists:seq(1, Iterations)),

    AvgTime = lists:sum(Times) / Iterations,
    {_, Result} = timer:tc(fun() -> wf_spec:from_xml(SpecXML) end),
    {AvgTime, Result}.

%% @doc Benchmark compilation phase.
-spec benchmark_compile(term()) -> {float(), term()}.
benchmark_compile(Spec) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() -> wf_spec:compile(Spec) end),
        Time / 1000.0
    end, lists:seq(1, Iterations)),
    {lists:sum(Times) / Iterations, ok}.

%% @doc Benchmark code generation phase.
-spec benchmark_code_gen(term()) -> {float(), term()}.
benchmark_code_gen(Spec) ->
    Iterations = ?BENCHMARK_ITERATIONS,
    Times = lists:map(fun(_) ->
        {Time, _} = timer:tc(fun() ->
            catch yawl_compile:compile(Spec, #{})
        end),
        Time / 1000.0
    end, lists:seq(1, Iterations)),
    {lists:sum(Times) / Iterations, ok}.

%% @doc Measure memory usage during compilation.
-spec measure_compilation_memory(binary()) -> integer().
measure_compilation_memory(SpecXML) ->
    garbage_collect(),
    MemBefore = erlang:memory(total),

    {ok, Spec} = wf_spec:from_xml(SpecXML),
    wf_spec:compile(Spec),

    MemAfter = erlang:memory(total),
    MemAfter - MemBefore.

%% @doc Benchmark an actual YAWL file.
-spec benchmark_actual_file(file:filename()) -> map().
benchmark_actual_file(Filename) ->
    {ok, #file_info{size = FileSize}} = file:read_file_info(Filename),

    %% Measure parse time
    {ParseTimeUs, {ok, Spec}} = timer:tc(fun() ->
        wf_spec:from_xml_file(Filename)
    end),

    %% Measure compile time
    {CompileTimeUs, {ok, Compiled}} = timer:tc(fun() ->
        wf_spec:compile(Spec)
    end),

    %% Measure memory
    garbage_collect(),
    MemBefore = erlang:memory(total),
    wf_spec:from_xml_file(Filename),
    MemAfter = erlang:memory(total),
    Memory = MemAfter - MemBefore,

    #{
        filename => Filename,
        file_size => FileSize,
        parse_time_us => ParseTimeUs,
        compile_time_us => CompileTimeUs,
        total_time_us => ParseTimeUs + CompileTimeUs,
        memory_bytes => Memory,
        places => length(wf_spec:places(Compiled)),
        transitions => length(wf_spec:transitions(Compiled))
    }.

%%%===================================================================
%%% YAWL Specification Generation
%%%===================================================================

%% @doc Create a test YAWL specification XML.
-spec create_yawl_spec_xml(pos_integer()) -> binary().
create_yawl_spec_xml(NumTasks) ->
    TasksXML = lists:map(fun(N) ->
        io_lib:format(
            "<task id='task~p'>"
            "<name>Task ~p</name>"
            "<documentation>Test task ~p</documentation>"
            "<join code='xor'/><split code='xor'/>"
            "</task>",
            [N, N, N])
    end, lists:seq(1, NumTasks)),

    iolist_to_binary([
        "<?xml version='1.0' encoding='UTF-8'?>"
        "<specificationSet id='bench_spec' version='2.2'>"
        "<specification uri='bench_spec'>"
        "<metaData><title>Benchmark Specification</title></metaData>"
        "<decomposition id='main' isRootNet='true'>"
        "<processControlElements>",
        TasksXML,
        "</processControlElements>"
        "</decomposition>"
        "</specification>"
        "</specificationSet>"
    ]).

%%%===================================================================
%%% Baseline Functions
%%%===================================================================

%% @doc Load baseline from file.
-spec load_baseline() -> undefined | map().
load_baseline() ->
    case file:read_file(?BASELINE_FILE) of
        {ok, Bin} ->
            try jsone:decode(Bin) of
                Map -> Map
            catch
                _:_ -> undefined
            end;
        {error, _} ->
            undefined
    end.

%%%===================================================================
%%% Export Functions
%%%===================================================================

%% @doc Export results to JSON file.
-spec export_to_file(file:filename(), map()) -> ok.
export_to_file(File, Results) ->
    JSON = maps:map(fun(Size, Result) ->
        #compilation_result{
            spec_size = Size,
            num_tasks = Tasks,
            num_transitions = Transitions,
            iterations = Iterations,
            parse_time_us = ParseTime,
            compile_time_us = CompileTime,
            code_gen_time_us = CodeGenTime,
            total_time_us = TotalTime,
            memory_bytes = Memory
        } = Result,

        #{
            <<"spec_size">> => atom_to_binary(Size),
            <<"num_tasks">> => Tasks,
            <<"num_transitions">> => Transitions,
            <<"iterations">> => Iterations,
            <<"parse_time_us">> => ParseTime,
            <<"compile_time_us">> => CompileTime,
            <<"code_gen_time_us">> => CodeGenTime,
            <<"total_time_us">> => TotalTime,
            <<"memory_bytes">> => Memory,
            <<"timestamp">> => integer_to_binary(erlang:system_time(second))
        }
    end, Results),

    Encoded = jsone:encode(JSON, [pretty]),
    ok = file:write_file(File, Encoded).

%%%===================================================================
%%% EUnit Tests
%%%===================================================================

-ifdef(TEST).

create_test_spec_test() ->
    Spec = create_test_spec(small),
    ?assert(is_binary(Spec)),
    ?assert(<<"<specificationSet">> =< Spec).

benchmark_spec_size_test() ->
    Result = benchmark_spec_size(tiny),
    ?assertEqual(tiny, Result#compilation_result.spec_size),
    ?assert(Result#compilation_result.parse_time_us >= 0).

-endif.
