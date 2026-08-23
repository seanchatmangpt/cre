%% -*- erlang -*-
%% @doc Memory Usage Benchmarking
%%
%% Memory tracking and analysis for Erlang processes:
%% - Before/after memory comparison
%% - Per-process and system-wide memory tracking
%% - Memory leak detection helpers
%%
%% @end

-module(mem_bench).
-author("CRE Team").

%% API
-export([mem_usage/0, mem_usage/1]).
-export([mem_diff/2]).
-export([measure_fun/1, measure_fun/2]).
-export([format_mem/1]).

%% Types
-type mem_info() :: #{
    total => non_neg_integer(),
    processes => non_neg_integer(),
    processes_used => non_neg_integer(),
    system => non_neg_integer(),
    atom => non_neg_integer(),
    binary => non_neg_integer(),
    code => non_neg_integer(),
    ets => non_neg_integer()
}.
-type mem_diff_result() :: #{
    before => mem_info(),
    after_mem => mem_info(),
    diff => #{atom() => integer()}
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Get current process memory usage.
-spec mem_usage() -> mem_info().
mem_usage() ->
    mem_usage(self()).

%% @doc Get memory usage for a specific process.
-spec mem_usage(pid() | atom()) -> mem_info().
mem_usage(Pid) when is_pid(Pid); is_atom(Pid) ->
    Memory = erlang:memory(),
    %% Convert proplist to map if needed
    MemMap = case is_list(Memory) of
        true -> maps:from_list(Memory);
        false -> Memory
    end,
    #{
        total => maps:get(total, MemMap, 0),
        processes => maps:get(processes, MemMap, 0),
        processes_used => maps:get(processes_used, MemMap, 0),
        system => maps:get(system, MemMap, 0),
        atom => maps:get(atom, MemMap, 0),
        binary => maps:get(binary, MemMap, 0),
        code => maps:get(code, MemMap, 0),
        ets => maps:get(ets, MemMap, 0)
    }.

%% @doc Compare memory usage before and after.
%% Returns map with before/after_mem/diff entries.
-spec mem_diff(mem_info(), mem_info()) -> mem_diff_result().
mem_diff(Before, AfterMem) ->
    Diff = maps:map(fun(_K, VAfter) ->
        VBefore = maps:get(_K, Before, 0),
        VAfter - VBefore
    end, AfterMem),
    #{
        before => Before,
        after_mem => AfterMem,
        diff => Diff
    }.

%% @doc Measure memory usage of running a function.
%% Returns {Result, MemDiff}.
-spec measure_fun(fun(() -> term())) -> {term(), mem_diff_result()}.
measure_fun(Fun) when is_function(Fun, 0) ->
    measure_fun(Fun, fun(_) -> ok end).

%% @doc Measure memory usage with GC control.
%% GCFun receives the result and can trigger additional GC.
-spec measure_fun(fun(() -> term()), fun((term()) -> term())) ->
    {term(), mem_diff_result()}.
measure_fun(Fun, GCFun) when is_function(Fun, 0), is_function(GCFun, 1) ->
    %% Force GC before measurement
    garbage_collect(),
    Before = mem_usage(),

    %% Run the function
    Result = Fun(),

    %% Run optional GC function
    GCFun(Result),

    %% Force GC after for accurate measurement
    garbage_collect(),
    AfterMem = mem_usage(),

    %% Calculate diff
    Diff = mem_diff(Before, AfterMem),
    {Result, Diff}.

%% @doc Format memory information for display.
-spec format_mem(mem_info() | mem_diff_result()) -> iolist().
format_mem(#{total := Total} = Mem) when is_map(Mem) ->
    case maps:get(diff, Mem, undefined) of
        undefined ->
            %% Format single memory snapshot
            [
                "Memory Usage:~n",
                format_mem_line("Total", Total),
                format_mem_line("Processes", maps:get(processes, Mem, 0)),
                format_mem_line("Processes Used", maps:get(processes_used, Mem, 0)),
                format_mem_line("System", maps:get(system, Mem, 0)),
                format_mem_line("Atom", maps:get(atom, Mem, 0)),
                format_mem_line("Binary", maps:get(binary, Mem, 0)),
                format_mem_line("Code", maps:get(code, Mem, 0)),
                format_mem_line("ETS", maps:get(ets, Mem, 0))
            ];
        Diff ->
            %% Format memory diff
            FormatDiff = fun(Key, Label) ->
                Value = maps:get(Key, Diff, 0),
                Sign = if
                    Value > 0 -> "+";
                    true -> ""
                end,
                io_lib:format("  ~-18s: ~s ~s~n", [Label, Sign, format_bytes(Value)])
            end,
            [
                "Memory Difference:~n",
                FormatDiff(total, "Total"),
                FormatDiff(processes, "Processes"),
                FormatDiff(atom, "Atom"),
                FormatDiff(binary, "Binary"),
                FormatDiff(code, "Code"),
                FormatDiff(ets, "ETS")
            ]
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Format a single memory line.
format_mem_line(Label, Bytes) ->
    io_lib:format("  ~-18s: ~s~n", [Label, format_bytes(Bytes)]).

%% @private Format bytes in human-readable format.
format_bytes(Bytes) when Bytes >= 1024 * 1024 * 1024 ->
    io_lib:format("~.2f GB", [Bytes / (1024 * 1024 * 1024)]);
format_bytes(Bytes) when Bytes >= 1024 * 1024 ->
    io_lib:format("~.2f MB", [Bytes / (1024 * 1024)]);
format_bytes(Bytes) when Bytes >= 1024 ->
    io_lib:format("~.2f KB", [Bytes / 1024]);
format_bytes(Bytes) ->
    io_lib:format("~p B", [Bytes]).
