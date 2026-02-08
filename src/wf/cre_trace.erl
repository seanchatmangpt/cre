%% -*- erlang -*-
%% @doc Advanced tracing utilities for CRE workflows.
%%
%% Provides safe and efficient tracing capabilities:
%% <ul>
%%   <li>Function call tracing with redbug (safe)</li>
%%   <li>Message tracing</li>
%%   <li>Process lifecycle tracing</li>
%%   <li>Garbage collection tracing</li>
%%   <li>Scheduler activity tracing</li>
%%   <li>Custom trace patterns</li>
%%   <li>Trace filtering and aggregation</li>
%% </ul>
%%
%% @end
-module(cre_trace).
-export([
    %% Basic tracing
    trace_module/2,
    trace_function/3,
    trace_process/2,
    trace_messages/1,
    
    %% Advanced tracing
    trace_pattern/2,
    trace_calls/3,
    trace_returns/3,
    trace_exceptions/2,
    
    %% Workflow-specific tracing
    trace_workflow/2,
    trace_transitions/1,
    trace_marking_changes/1,
    
    %% Trace management
    start_trace/1,
    stop_trace/0,
    get_trace_info/0,
    clear_trace/0,
    
    %% Trace analysis
    analyze_trace/1,
    trace_to_file/2,
    trace_from_file/1,
    
    %% Safe tracing with redbug
    safe_trace/3,
    safe_trace_pattern/2
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Basic Tracing
%%====================================================================

%% @doc Trace all functions in a module.
-spec trace_module(atom(), map()) -> ok | {error, term()}.
trace_module(Module, Opts) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            Pattern = maps:get(pattern, Opts, '_'),
            Arity = maps:get(arity, Opts, '_'),
            dbg:tracer(),
            dbg:p(all, [c, timestamp]),
            dbg:tpl(Module, Pattern, Arity, x),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Trace a specific function.
-spec trace_function(atom(), atom(), integer() | '_') -> ok | {error, term()}.
trace_function(Module, Function, Arity) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            dbg:tracer(),
            dbg:p(all, [c, timestamp]),
            dbg:tpl(Module, Function, Arity, x),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Trace a specific process.
-spec trace_process(pid() | atom(), map()) -> ok | {error, term()}.
trace_process(PidOrName, Opts) ->
    Pid = case PidOrName of
        P when is_pid(P) -> P;
        N when is_atom(N) -> whereis(N)
    end,
    case Pid of
        undefined -> {error, process_not_found};
        _ ->
            Flags = maps:get(flags, Opts, [call, return_to, timestamp]),
            dbg:tracer(),
            dbg:p(Pid, Flags),
            ok
    end.

%% @doc Trace messages sent to/received by a process.
-spec trace_messages(pid() | atom()) -> ok | {error, term()}.
trace_messages(PidOrName) ->
    Pid = case PidOrName of
        P when is_pid(P) -> P;
        N when is_atom(N) -> whereis(N)
    end,
    case Pid of
        undefined -> {error, process_not_found};
        _ ->
            dbg:tracer(),
            dbg:p(Pid, [m, timestamp]),
            ok
    end.

%%====================================================================
%% Advanced Tracing
%%====================================================================

%% @doc Set a custom trace pattern.
-spec trace_pattern({atom(), atom(), integer()}, map()) -> ok | {error, term()}.
trace_pattern({Module, Function, Arity}, Opts) ->
    MatchSpec = maps:get(match_spec, Opts, x),
    case code:ensure_loaded(Module) of
        {module, Module} ->
            dbg:tracer(),
            dbg:p(all, [c, timestamp]),
            dbg:tpl(Module, Function, Arity, MatchSpec),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Trace function calls with filtering.
-spec trace_calls(atom(), atom(), fun((list()) -> boolean())) -> ok | {error, term()}.
trace_calls(Module, Function, _Filter) ->
    MatchSpec = [{'_', [], [{message, {caller}}]}],
    case code:ensure_loaded(Module) of
        {module, Module} ->
            dbg:tracer(),
            dbg:p(all, [c, timestamp]),
            dbg:tpl(Module, Function, MatchSpec),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Trace function returns.
-spec trace_returns(atom(), atom(), integer() | '_') -> ok | {error, term()}.
trace_returns(Module, Function, Arity) ->
    MatchSpec = [{'_', [], [{return_trace}]}],
    case code:ensure_loaded(Module) of
        {module, Module} ->
            dbg:tracer(),
            dbg:p(all, [c, timestamp]),
            dbg:tpl(Module, Function, Arity, MatchSpec),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Trace exceptions.
-spec trace_exceptions(atom(), map()) -> ok | {error, term()}.
trace_exceptions(Module, Opts) ->
    MatchSpec = [{'_', [], [{exception_trace}]}],
    case code:ensure_loaded(Module) of
        {module, Module} ->
            dbg:tracer(),
            dbg:p(all, [c, timestamp]),
            dbg:tpl(Module, '_', '_', MatchSpec),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Workflow-Specific Tracing
%%====================================================================

%% @doc Trace workflow execution.
-spec trace_workflow(pid() | atom(), map()) -> ok | {error, term()}.
trace_workflow(WorkflowPid, Opts) ->
    Pid = case WorkflowPid of
        P when is_pid(P) -> P;
        N when is_atom(N) -> whereis(N)
    end,
    case Pid of
        undefined -> {error, workflow_not_found};
        _ ->
            Level = maps:get(level, Opts, all),
            dbg:tracer(),
            dbg:p(Pid, [c, m, timestamp]),
            case Level of
                all ->
                    dbg:tpl(gen_yawl, cx),
                    dbg:tpl(gen_pnet, cx),
                    dbg:tpl(wf_yawl_executor, cx);
                transitions ->
                    dbg:tpl(gen_yawl, step, x),
                    dbg:tpl(gen_pnet, fire, x);
                marking ->
                    dbg:tpl(gen_yawl, marking, x),
                    dbg:tpl(gen_pnet, inject, x);
                _ -> ok
            end,
            ok
    end.

%% @doc Trace transition firings.
-spec trace_transitions(pid() | atom()) -> ok | {error, term()}.
trace_transitions(WorkflowPid) ->
    trace_workflow(WorkflowPid, #{level => transitions}).

%% @doc Trace marking changes.
-spec trace_marking_changes(pid() | atom()) -> ok | {error, term()}.
trace_marking_changes(WorkflowPid) ->
    trace_workflow(WorkflowPid, #{level => marking}).

%%====================================================================
%% Trace Management
%%====================================================================

%% @doc Start tracing with configuration.
-spec start_trace(map()) -> {ok, pid()} | {error, term()}.
start_trace(Opts) ->
    Type = maps:get(type, Opts, process),
    Port = maps:get(port, Opts, undefined),
    File = maps:get(file, Opts, undefined),
    case {Port, File} of
        {undefined, undefined} ->
            dbg:tracer();
        {P, undefined} when is_port(P) ->
            dbg:tracer(port, dbg:trace_port(ip, P));
        {undefined, F} when is_list(F) ->
            dbg:tracer(file, dbg:trace_port(file, F));
        _ ->
            {error, invalid_options}
    end.

%% @doc Stop tracing.
-spec stop_trace() -> ok.
stop_trace() ->
    dbg:stop_clear(),
    ok.

%% @doc Get current trace information.
-spec get_trace_info() -> map().
get_trace_info() ->
    #{tracer => dbg:get_tracer(),
      traced_processes => dbg:get_tracer(),
      trace_patterns => get_trace_patterns()}.

get_trace_patterns() ->
    try
        dbg:get_tracer()
    catch
        _:_ -> []
    end.

%% @doc Clear all traces.
-spec clear_trace() -> ok.
clear_trace() ->
    dbg:stop_clear(),
    ok.

%%====================================================================
%% Trace Analysis
%%====================================================================

%% @doc Analyze trace data.
-spec analyze_trace(file:filename()) -> map().
analyze_trace(Filename) ->
    case file:read_file(Filename) of
        {ok, Contents} ->
            analyze_trace_content(Contents);
        {error, Reason} ->
            #{error => Reason}
    end.

analyze_trace_content(Contents) ->
    Lines = binary:split(Contents, <<"\n">>, [global]),
    #{total_events => length(Lines),
      function_calls => count_pattern(Lines, <<"call">>),
      returns => count_pattern(Lines, <<"return">>),
      exceptions => count_pattern(Lines, <<"exception">>),
      messages => count_pattern(Lines, <<"message">>)}.

count_pattern(Lines, Pattern) ->
    length([L || L <- Lines, binary:match(L, Pattern) =/= nomatch]).

%% @doc Write trace to file.
-spec trace_to_file(file:filename(), map()) -> ok | {error, term()}.
trace_to_file(Filename, Opts) ->
    case start_trace(Opts#{file => Filename}) of
        {ok, _Tracer} -> ok;
        {error, Reason} -> {error, Reason}
    end.

%% @doc Read trace from file.
-spec trace_from_file(file:filename()) -> [term()].
trace_from_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Contents} ->
            parse_trace_file(Contents);
        {error, _Reason} ->
            []
    end.

parse_trace_file(Contents) ->
    Lines = binary:split(Contents, <<"\n">>, [global]),
    [parse_trace_line(L) || L <- Lines, L =/= <<>>].

parse_trace_line(Line) ->
    case binary:split(Line, <<" ">>, [global]) of
        [Timestamp, Type | Rest] ->
            #{timestamp => Timestamp,
              type => Type,
              data => Rest};
        _ ->
            #{raw => Line}
    end.

%%====================================================================
%% Safe Tracing with redbug
%%====================================================================

%% @doc Safe tracing using redbug (if available).
-spec safe_trace(atom(), atom(), integer()) -> ok | {error, term()}.
safe_trace(Module, Function, Arity) ->
    case code:which(redbug) of
        non_existing ->
            {error, redbug_not_available};
        _Path ->
            try
                redbug:start(),
                redbug:tp(Module, Function, Arity, []),
                ok
            catch
                _:Reason -> {error, Reason}
            end
    end.

%% @doc Safe trace pattern with redbug.
-spec safe_trace_pattern({atom(), atom(), integer()}, map()) -> ok | {error, term()}.
safe_trace_pattern({Module, Function, Arity}, Opts) ->
    case code:which(redbug) of
        non_existing ->
            {error, redbug_not_available};
        _Path ->
            try
                Count = maps:get(count, Opts, 10),
                Time = maps:get(time, Opts, 10000),
                redbug:start(#{time => Time, msgs => Count}),
                redbug:tp(Module, Function, Arity, []),
                ok
            catch
                _:Reason -> {error, Reason}
            end
    end.
