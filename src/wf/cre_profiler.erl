%% -*- erlang -*-
%% @doc Profiling utilities for CRE workflows.
%%
%% Provides comprehensive profiling capabilities:
%% <ul>
%%   <li>fprof - Function call profiling</li>
%%   <li>eprof - Execution time profiling</li>
%%   <li>eflame - Flame graph generation</li>
%%   <li>cprof - Count profiling</li>
%%   <li>Custom profiling hooks</li>
%% </ul>
%%
%% @end
-module(cre_profiler).
-export([
    %% fprof profiling
    fprof_start/0,
    fprof_stop/0,
    fprof_profile/1,
    fprof_analyze/1,
    
    %% eprof profiling
    eprof_start/0,
    eprof_profile/1,
    eprof_stop/0,
    eprof_analyze/0,
    
    %% eflame profiling
    eflame_start/0,
    eflame_stop/1,
    eflame_to_file/2,
    
    %% cprof profiling
    cprof_start/1,
    cprof_stop/0,
    cprof_analyze/0,
    
    %% Workflow profiling
    profile_workflow/2,
    profile_transition/3,
    profile_marking_operation/2,
    
    %% Custom profiling
    profile_function/2,
    profile_with_hooks/2
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% fprof Profiling
%%====================================================================

%% @doc Start fprof profiling.
-spec fprof_start() -> ok | {error, term()}.
fprof_start() ->
    try
        fprof:trace(start),
        ok
    catch
        _:Reason -> {error, Reason}
    end.

%% @doc Stop fprof profiling and write to file.
-spec fprof_stop() -> {ok, file:filename()} | {error, term()}.
fprof_stop() ->
    try
        fprof:trace(stop),
        Filename = "/tmp/fprof.trace",
        fprof:profile(file, Filename),
        {ok, Filename}
    catch
        _:Reason -> {error, Reason}
    end.

%% @doc Profile a function call.
-spec fprof_profile(fun(() -> term())) -> {term(), file:filename()}.
fprof_profile(Fun) ->
    fprof:trace(start),
    Result = Fun(),
    fprof:trace(stop),
    Filename = "/tmp/fprof.trace",
    fprof:profile(file, Filename),
    {Result, Filename}.

%% @doc Analyze fprof trace file.
-spec fprof_analyze(file:filename()) -> map().
fprof_analyze(Filename) ->
    case fprof:analyse(dest, Filename, [{cols, 120}]) of
        {ok, _} ->
            #{status => ok, file => Filename};
        {error, Reason} ->
            #{status => error, reason => Reason}
    end.

%%====================================================================
%% eprof Profiling
%%====================================================================

%% @doc Start eprof profiling.
-spec eprof_start() -> ok | {error, term()}.
eprof_start() ->
    try
        eprof:start(),
        eprof:start_profiling([self()]),
        ok
    catch
        _:Reason -> {error, Reason}
    end.

%% @doc Profile a function with eprof.
-spec eprof_profile(fun(() -> term())) -> {term(), map()}.
eprof_profile(Fun) ->
    eprof:start(),
    eprof:start_profiling([self()]),
    Result = Fun(),
    eprof:stop_profiling(),
    Analysis = eprof:analyze(),
    eprof:stop(),
    {Result, Analysis}.

%% @doc Stop eprof profiling.
-spec eprof_stop() -> map().
eprof_stop() ->
    eprof:stop_profiling(),
    Analysis = eprof:analyze(),
    eprof:stop(),
    Analysis.

%% @doc Analyze eprof results.
-spec eprof_analyze() -> [map()].
eprof_analyze() ->
    case eprof:analyze() of
        {ok, Data} ->
            [format_eprof_entry(E) || E <- Data];
        {error, Reason} ->
            [#{error => Reason}]
    end.

format_eprof_entry({MFA, Count, Percent}) ->
    #{module => element(1, MFA),
      function => element(2, MFA),
      arity => element(3, MFA),
      call_count => Count,
      percentage => Percent}.

%%====================================================================
%% eflame Profiling
%%====================================================================

%% @doc Start eflame profiling.
-spec eflame_start() -> {ok, reference()} | {error, term()}.
eflame_start() ->
    case code:which(eflame) of
        non_existing ->
            {error, eflame_not_available};
        _Path ->
            try
                eflame:start(),
                ok
            catch
                _:Reason -> {error, Reason}
            end
    end.

%% @doc Stop eflame profiling.
-spec eflame_stop(reference()) -> ok | {error, term()}.
eflame_stop(Ref) ->
    case code:which(eflame) of
        non_existing ->
            {error, eflame_not_available};
        _Path ->
            try
                eflame:stop(Ref),
                ok
            catch
                _:Reason -> {error, Reason}
            end
    end.

%% @doc Write eflame data to file.
-spec eflame_to_file(reference(), file:filename()) -> ok | {error, term()}.
eflame_to_file(Ref, Filename) ->
    case code:which(eflame) of
        non_existing ->
            {error, eflame_not_available};
        _Path ->
            try
                eflame:to_file(Ref, Filename),
                ok
            catch
                _:Reason -> {error, Reason}
            end
    end.

%%====================================================================
%% cprof Profiling
%%====================================================================

%% @doc Start cprof profiling for modules.
-spec cprof_start([atom()]) -> ok | {error, term()}.
cprof_start(Modules) ->
    try
        cprof:start(),
        [cprof:analyse(Module) || Module <- Modules],
        ok
    catch
        _:Reason -> {error, Reason}
    end.

%% @doc Stop cprof profiling.
-spec cprof_stop() -> map().
cprof_stop() ->
    Analysis = cprof:analyse(),
    cprof:stop(),
    Analysis.

%% @doc Analyze cprof results.
-spec cprof_analyze() -> [map()].
cprof_analyze() ->
    case cprof:analyse() of
        {ok, Data} ->
            [format_cprof_entry(E) || E <- Data];
        {error, Reason} ->
            [#{error => Reason}]
    end.

format_cprof_entry({MFA, Count}) ->
    #{module => element(1, MFA),
      function => element(2, MFA),
      arity => element(3, MFA),
      call_count => Count}.

%%====================================================================
%% Workflow Profiling
%%====================================================================

%% @doc Profile a workflow execution.
-spec profile_workflow(pid() | atom(), map()) -> map().
profile_workflow(WorkflowPid, Opts) ->
    Pid = case WorkflowPid of
        P when is_pid(P) -> P;
        N when is_atom(N) -> whereis(N)
    end,
    case Pid of
        undefined -> #{error => workflow_not_found};
        _ ->
            Type = maps:get(type, Opts, eprof),
            case Type of
                eprof ->
                    {_Result, Analysis} = eprof_profile(fun() ->
                        gen_yawl:drain(Pid, maps:get(max_steps, Opts, 1000))
                    end),
                    Analysis;
                fprof ->
                    {_Result, Filename} = fprof_profile(fun() ->
                        gen_yawl:drain(Pid, maps:get(max_steps, Opts, 1000))
                    end),
                    #{trace_file => Filename};
                _ ->
                    #{error => unsupported_profiler}
            end
    end.

%% @doc Profile a specific transition firing.
-spec profile_transition(pid(), atom(), map()) -> map().
profile_transition(WorkflowPid, Transition, Opts) ->
    Pid = case WorkflowPid of
        P when is_pid(P) -> P;
        N when is_atom(N) -> whereis(N)
    end,
    case Pid of
        undefined -> #{error => workflow_not_found};
        _ ->
            {_Result, Analysis} = eprof_profile(fun() ->
                gen_yawl:step(Pid, #{trsn => Transition})
            end),
            Analysis
    end.

%% @doc Profile marking operations.
-spec profile_marking_operation(pid(), fun((pid()) -> term())) -> map().
profile_marking_operation(WorkflowPid, Operation) ->
    {_Result, Analysis} = eprof_profile(fun() ->
        Operation(WorkflowPid)
    end),
    Analysis.

%%====================================================================
%% Custom Profiling
%%====================================================================

%% @doc Profile a function with timing.
-spec profile_function(fun(() -> term()), map()) -> {term(), map()}.
profile_function(Fun, Opts) ->
    StartTime = erlang:monotonic_time(microsecond),
    StartReductions = erlang:statistics(reductions),
    Result = Fun(),
    EndTime = erlang:monotonic_time(microsecond),
    EndReductions = erlang:statistics(reductions),
    #{result => Result,
      execution_time_us => EndTime - StartTime,
      reductions => EndReductions - StartReductions,
      memory_before => erlang:memory(processes),
      memory_after => erlang:memory(processes)}.

%% @doc Profile with custom hooks.
-spec profile_with_hooks(fun(() -> term()), map()) -> {term(), map()}.
profile_with_hooks(Fun, Opts) ->
    BeforeHook = maps:get(before, Opts, fun() -> ok end),
    AfterHook = maps:get('after', Opts, fun(_) -> ok end),
    BeforeHook(),
    ProfileResult = profile_function(Fun, Opts),
    AfterHook(ProfileResult),
    ProfileResult.
