%% -*- erlang -*-
%%%% @doc TTB Trace Collection for OTP Evidence
%%
%% This module provides TTB (Trace Tool Box) trace collection functionality
%% for gathering OTP evidence during workflow execution. TTB is Erlang's
%% built-in trace browser that can capture function calls, process events,
%% and system messages with minimal overhead.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Configurable trace patterns for gen_yawl callbacks</li>
%%   <li>Automatic trace filtering by process, module, or function</li>
%%   <li>Trace summary generation with event counts and duration</li>
%%   <li>Temporary trace file handling with automatic cleanup</li>
%%   <li>Support for supervisor child event tracing</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Starting a trace with default pattern:
%% ```erlang
%% > {ok, TraceFile} = evidence_ttb:start_trace(my_case).
%% {ok, "/tmp/ttb_my_case_123456789.trace"}
%% ```
%%
%% Stopping the trace and getting results:
%% ```erlang
%% > {ok, File} = evidence_ttb:stop_trace().
%% {ok, "/tmp/ttb_my_case_123456789.trace"}
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(evidence_ttb).

%%====================================================================
%% Exports
%%====================================================================

%% Trace management
-export([start_trace/1, start_trace/2, stop_trace/0, stop_trace/1]).
-export([trace_pattern/0, trace_pattern/1]).
-export([filter_trace/2]).
-export([filter_trace_by_pid/2]).
-export([summarize_trace/1, summarize_trace/2]).
-export([format_summary/1]).
-export([cleanup_trace/1]).

%%====================================================================
%% Types
%%====================================================================

-type trace_name() :: atom().
-type trace_file() :: file:filename_all().

-type trace_opts() :: #{
    dir => file:filename_all(),
    size => integer(),
    queue_size => integer()
}.

-type filter() :: #{
    pid => pid(),
    module => module(),
    function => atom() | {atom(), non_neg_integer()}
}.

-type trace_summary() :: #{
    trace_file => trace_file(),
    event_count => non_neg_integer(),
    duration_ms => non_neg_integer(),
    modules_traced => [module()],
    top_functions => [{{module(), atom(), non_neg_integer()}, non_neg_integer()}]
}.

-export_type([trace_name/0, trace_file/0, trace_opts/0, filter/0, trace_summary/0]).

%%====================================================================
%% Trace State
%%====================================================================

-record(trace_state, {
    name :: trace_name(),
    file :: trace_file(),
    port :: port() | undefined,
    start_time :: integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts a TTB trace with default options.
%%
%% @param Name Unique identifier for this trace session
%% @returns {ok, TraceFile} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec start_trace(Name :: trace_name()) -> {ok, trace_file()} | {error, term()}.

start_trace(Name) ->
    start_trace(Name, #{}).

%%--------------------------------------------------------------------
%% @doc Starts a TTB trace with custom options.
%%
%% @param Name Unique identifier for this trace session
%% @param Opts Options map (dir, size, queue_size)
%% @returns {ok, TraceFile} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec start_trace(Name :: trace_name(), Opts :: trace_opts()) ->
    {ok, trace_file()} | {error, term()}.

start_trace(Name, Opts) ->
    Dir = maps:get(dir, Opts, "/tmp"),
    Size = maps:get(size, Opts, 10),
    QueueSize = maps:get(queue_size, Opts, 1000),

    %% Generate unique trace file name
    Unique = erlang:unique_integer([positive, monotonic]),
    Filename = lists:flatten([atom_to_list(Name), "_", integer_to_list(Unique)]),
    TraceFile = filename:join(Dir, Filename),

    %% Configure TTB options
    TtbOpts = [
        {name, Name},
        {file, TraceFile},
        {handler, {fun format_handler/4, Name}},
        {overwrite, true},
        {resume, true},
        {size, Size},
        {queue_size, QueueSize}
    ],

    case ttb:tracer(Name, TtbOpts) of
        {ok, _Port} ->
            %% Apply trace patterns
            Patterns = trace_pattern(),
            apply_patterns(Patterns),

            %% Store trace state in process dictionary
            put(evidence_ttb_state, #trace_state{
                name = Name,
                file = TraceFile,
                port = undefined,
                start_time = erlang:monotonic_time(millisecond)
            }),

            {ok, TraceFile};
        {error, Reason} ->
            {error, {trace_start_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Returns the default trace pattern specification.
%%
%% @returns List of {Module, Function, Arity} patterns
%%
%% @end
%%--------------------------------------------------------------------
-spec trace_pattern() -> [{module(), atom(), non_neg_integer()}].

trace_pattern() ->
    trace_pattern(all).

%%--------------------------------------------------------------------
%% @doc Returns trace patterns for a specific category.
%%
%% Categories: default, gen_yawl, wf_audit_log, supervisor, ln_ctrl, all
%%
%% @param Category Pattern category
%% @returns List of {Module, Function, Arity} patterns
%%
%% @end
%%--------------------------------------------------------------------
-spec trace_pattern(Category :: atom()) -> [{module(), atom(), non_neg_integer()}].

trace_pattern(default) ->
    trace_pattern(all);
trace_pattern(gen_yawl) ->
    [
        {gen_yawl, fire, 3},
        {gen_yawl, is_enabled, 3},
        {gen_yawl, trigger, 3},
        {gen_yawl, handle_call, 3},
        {gen_yawl, handle_cast, 2},
        {gen_yawl, handle_info, 2},
        {gen_yawl, init, 1},
        {gen_yawl, terminate, 2}
    ];
trace_pattern(wf_audit_log) ->
    [
        {wf_audit_log, append, 2},
        {wf_audit_log, read, 3},
        {wf_audit_log, close, 1}
    ];
trace_pattern(supervisor) ->
    [
        {supervisor, start_child, 2},
        {supervisor, terminate_child, 2},
        {supervisor, restart_child, 2},
        {supervisor, delete_child, 2},
        {supervisor, which_children, 1}
    ];
trace_pattern(ln_ctrl) ->
    [
        {ln_ctrl, init, 1},
        {ln_ctrl, handle_call, 3},
        {ln_ctrl, handle_cast, 2},
        {ln_ctrl, terminate, 2},
        {ln_ctrl, signal, 2},
        {ln_ctrl, call, 2}
    ];
trace_pattern(all) ->
    trace_pattern(gen_yawl) ++
    trace_pattern(wf_audit_log) ++
    trace_pattern(supervisor) ++
    trace_pattern(ln_ctrl);
trace_pattern(_) ->
    [].

%%--------------------------------------------------------------------
%% @doc Stops the current trace and returns the trace file.
%%
%% @returns {ok, TraceFile} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_trace() -> {ok, trace_file()} | {error, term()}.

stop_trace() ->
    case get(evidence_ttb_state) of
        #trace_state{name = Name} ->
            stop_trace(Name);
        undefined ->
            {error, no_active_trace}
    end.

%%--------------------------------------------------------------------
%% @doc Stops a specific named trace.
%%
%% @param Name The trace session name
%% @returns {ok, TraceFile} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_trace(Name :: trace_name()) -> {ok, trace_file()} | {error, term()}.

stop_trace(Name) ->
    case ttb:stop(Name) of
        {ok, _Stopped} ->
            case ttb:format(Name) of
                {ok, _Formatted} ->
                    %% Get the trace file location
                    case ttb:get_trace_dir(Name) of
                        Dir when is_list(Dir) ->
                            TraceFile = filename:join(Dir, "ttb"),
                            {ok, TraceFile};
                        _ ->
                            %% Try to get file from process dictionary
                            case get(evidence_ttb_state) of
                                #trace_state{file = File} ->
                                    {ok, File};
                                _ ->
                                    {error, unknown_trace_file}
                            end
                    end;
                {error, Reason} ->
                    {error, {format_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {stop_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Filters a trace file by process ID.
%%
%% @param TraceFile The trace file path
%% @param Pid Process ID to filter by
%% @returns {ok, FilteredEvents} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_trace_by_pid(TraceFile :: trace_file(), Pid :: pid()) ->
    {ok, [term()]} | {error, term()}.

filter_trace_by_pid(TraceFile, Pid) when is_pid(Pid) ->
    filter_trace(TraceFile, #{pid => Pid}).

%%--------------------------------------------------------------------
%% @doc Filters a trace file by multiple criteria.
%%
%% @param TraceFile The trace file path
%% @param Filter Filter specification map (pid, module, function)
%% @returns {ok, FilteredEvents} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec filter_trace(TraceFile :: trace_file(), Filter :: filter()) ->
    {ok, [term()]} | {error, term()}.

filter_trace(TraceFile, Filter) ->
    case file:read_file(TraceFile) of
        {ok, Binary} ->
            case parse_trace_file(Binary) of
                {ok, Events} ->
                    Filtered = apply_filter(Events, Filter),
                    {ok, Filtered};
                {error, _} = Error ->
                    Error
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Generates a summary from a trace file.
%%
%% @param TraceFile The trace file path
%% @returns Summary map with trace statistics
%%
%% @end
%%--------------------------------------------------------------------
-spec summarize_trace(TraceFile :: trace_file()) -> trace_summary() | {error, term()}.

summarize_trace(TraceFile) ->
    summarize_trace(TraceFile, #{}).

%%--------------------------------------------------------------------
%% @doc Generates a summary with custom analysis options.
%%
%% @param TraceFile The trace file path
%% @param Opts Analysis options (top_n)
%% @returns Summary map with trace statistics
%%
%% @end
%%--------------------------------------------------------------------
-spec summarize_trace(TraceFile :: trace_file(), Opts :: map()) ->
    trace_summary() | {error, term()}.

summarize_trace(TraceFile, Opts) ->
    case file:read_file(TraceFile) of
        {ok, Binary} ->
            case parse_trace_file(Binary) of
                {ok, Events} ->
                    generate_summary(TraceFile, Events, Opts);
                {error, Reason} ->
                    {error, {parse_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Formats a trace summary for display.
%%
%% @param Summary The trace summary map
%% @returns Formatted iolist
%%
%% @end
%%--------------------------------------------------------------------
-spec format_summary(Summary :: trace_summary()) -> iolist().

format_summary(#{trace_file := File, event_count := Count,
                 duration_ms := Duration, modules_traced := Modules,
                 top_functions := TopFuns}) ->
    [
        "=== TTB Trace Summary ===\n",
        "Trace File: ", File, "\n",
        "Event Count: ", integer_to_list(Count), "\n",
        "Duration: ", integer_to_list(Duration), " ms\n",
        "Modules Traced: ", format_modules(Modules), "\n",
        "Top Functions:\n", format_top_functions(TopFuns),
        "========================\n"
    ].

%%--------------------------------------------------------------------
%% @doc Cleans up a trace file.
%%
%% @param TraceFile The trace file path
%% @returns ok on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_trace(TraceFile :: trace_file()) -> ok | {error, term()}.

cleanup_trace(TraceFile) ->
    case file:delete(TraceFile) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Applies trace patterns to the TTB tracer.
apply_patterns(Patterns) ->
    lists:foreach(fun({Mod, Fun, Arity}) ->
        case ttb:tp({Mod, Fun, Arity}, []) of
            ok -> ok;
            {error, _} -> ok
        end
    end, Patterns).

%% @private
%% @doc Formats trace events with timestamp and process info.
format_handler(Trace, TraceInfo, _State, Name) ->
    {Trace, TraceInfo, Name}.

%% @private
%% @doc Parses trace events from binary or text format.
-spec parse_trace_file(binary()) -> {ok, [term()]} | {error, term()}.

parse_trace_file(Binary) ->
    try
        Events = binary_to_term(Binary),
        {ok, Events}
    catch
        error:badarg ->
            %% Try reading as text format
            try
                TextEvents = binary_to_list(Binary),
                {ok, TextEvents}
            catch
                _:_ ->
                    {error, invalid_format}
            end
    end.

%% @private
%% @doc Generates a summary from parsed trace events.
-spec generate_summary(trace_file(), [term()], map()) -> trace_summary().

generate_summary(TraceFile, Events, Opts) when is_list(Events) ->
    TopN = maps:get(top_n, Opts, 10),
    EventCount = length(Events),

    %% Extract unique modules
    Modules = extract_modules(Events),

    %% Count function calls
    FunCounts = count_functions(Events),

    %% Get top N functions
    TopFuns = lists:sublist(
        lists:sort(fun({_, A}, {_, B}) -> A > B end, FunCounts),
        TopN
    ),

    #{
        trace_file => TraceFile,
        event_count => EventCount,
        duration_ms => 0,
        modules_traced => Modules,
        top_functions => TopFuns
    };

generate_summary(TraceFile, _Events, _Opts) ->
    #{
        trace_file => TraceFile,
        event_count => 0,
        duration_ms => 0,
        modules_traced => [],
        top_functions => []
    }.

%% @private
%% @doc Extracts unique module names from events.
-spec extract_modules([term()]) -> [module()].

extract_modules(Events) ->
    Modules = lists:foldl(fun(Event, Acc) ->
        case Event of
            {trace, _Pid, call, {Mod, _Fun, _Args}} when is_atom(Mod) ->
                sets:add_element(Mod, Acc);
            {trace, _Pid, call, {Mod, _Fun, _Args, _Options}} when is_atom(Mod) ->
                sets:add_element(Mod, Acc);
            _ ->
                Acc
        end
    end, sets:new(), Events),
    lists:sort(sets:to_list(Modules)).

%% @private
%% @doc Counts function calls from events.
-spec count_functions([term()]) -> [{{module(), atom(), non_neg_integer()}, non_neg_integer()}].

count_functions(Events) ->
    Dict = lists:foldl(fun(Event, Acc) ->
        case Event of
            {trace, _Pid, call, {Mod, Fun, Arity}} ->
                Key = {Mod, Fun, Arity},
                maps:update_counter(Key, 1, Acc);
            {trace, _Pid, call, {Mod, Fun, Arity, _Options}} ->
                Key = {Mod, Fun, Arity},
                maps:update_counter(Key, 1, Acc);
            _ ->
                Acc
        end
    end, maps:new(), Events),

    %% Format as {FunWithArity, Count} tuples
    maps:fold(fun(Key, Count, Acc) ->
        [{Key, Count} | Acc]
    end, [], Dict).

%% @private
%% @doc Applies filter to trace events.
-spec apply_filter([term()], filter()) -> [term()].

apply_filter(Events, Filter) when is_list(Events) ->
    FilterPid = maps:get(pid, Filter, undefined),
    FilterMod = maps:get(module, Filter, undefined),
    FilterFun = maps:get(function, Filter, undefined),

    lists:filter(fun(Event) ->
        matches_filter(Event, FilterPid, FilterMod, FilterFun)
    end, Events);

apply_filter(_Events, _Filter) ->
    [].

%% @private
%% @doc Checks if an event matches the filter criteria.
-spec matches_filter(term(), pid() | undefined, module() | undefined, atom() | {atom(), non_neg_integer()} | undefined) -> boolean().

matches_filter({trace, Pid, _Type, Data}, FilterPid, FilterMod, FilterFun) ->
    PidMatch = case FilterPid of
        undefined -> true;
        _ -> Pid =:= FilterPid
    end,

    {Mod, Fun} = extract_module_function(Data),

    ModMatch = case FilterMod of
        undefined -> true;
        _ -> Mod =:= FilterMod
    end,

    FunMatch = case FilterFun of
        undefined -> true;
        F when is_atom(F) -> Fun =:= F;
        {F, Arity} when is_atom(F), is_integer(Arity) ->
            case Data of
                {_M, Fn, A} -> Fn =:= F andalso A =:= Arity;
                {_M, Fn, A, _Opts} -> Fn =:= F andalso A =:= Arity;
                _ -> false
            end
    end,

    PidMatch andalso ModMatch andalso FunMatch;

matches_filter(_, _, _, _) ->
    false.

%% @private
%% @doc Extracts module and function from trace data.
-spec extract_module_function(term()) -> {module() | undefined, atom() | undefined}.

extract_module_function({Mod, Fun, _Args}) when is_atom(Mod), is_atom(Fun) ->
    {Mod, Fun};
extract_module_function({Mod, Fun, _Args, _Options}) when is_atom(Mod), is_atom(Fun) ->
    {Mod, Fun};
extract_module_function(_) ->
    {undefined, undefined}.

%% @private
%% @doc Formats module list for display.
-spec format_modules([module()]) -> string().

format_modules([]) ->
    "[]";
format_modules(Modules) ->
    StringMods = [atom_to_list(M) || M <- Modules],
    "[" ++ string:join(StringMods, ", ") ++ "]".

%% @private
%% @doc Formats top functions list for display.
-spec format_top_functions([{{module(), atom(), non_neg_integer()}, non_neg_integer()}]) -> iolist().

format_top_functions([]) ->
    "  (none)\n";
format_top_functions(TopFuns) ->
    lists:map(fun
        ({{Mod, Fun, Arity}, Count}) ->
            ["  ", atom_to_list(Mod), ":", atom_to_list(Fun), "/",
             integer_to_list(Arity), " (", integer_to_list(Count), ")\n"]
    end, TopFuns).

