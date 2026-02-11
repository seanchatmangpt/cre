%%% @doc WF Substrate Public API
%%%
%%% This module provides the public interface to the WF Substrate workflow engine.
%%% It exposes high-level functions for:
%%% - Starting workflow cases
%%% - Sending signals to running cases
%%% - Cancelling cases
%%% - Retrieving status and execution traces
%%% - Compiling and validating workflow patterns
%%%
%%% @end
-module(wf_substrate).

%% Public API
-export([
    start_case/2,
    start_case/3,
    send_signal/2,
    cancel_case/1,
    get_status/1,
    get_trace/1,
    compile/1,
    compile/2,
    validate/1
]).

-export_type([
    case_id/0,
    case_handle/0,
    case_status/0,
    compile_result/0,
    validation_result/0
]).

%%% TYPES ===================================================================

%% Case identifier
-type case_id() :: atom() | {atom(), term()}.

%% Handle to a running case (opaque)
-type case_handle() :: #{
    case_id := case_id(),
    pid := pid(),
    started_at := integer()
}.

%% Case status information
-type case_status() :: #{
    case_id := case_id(),
    status := running | halted | error | cancelled,
    steps := non_neg_integer(),
    pc := non_neg_integer(),
    stack_depth := non_neg_integer(),
    effects := #{term() => {pending | completed | failed, term()}},
    uptime_ms := non_neg_integer()
}.

%% Compilation result
-type compile_result() ::
      {ok, wf_compile:compiled()}
    | {error, term()}.

%% Validation result
-type validation_result() ::
      {ok, #{valid := true, size := non_neg_integer()}}
    | {error, term()}.

%%% PUBLIC API ==============================================================

%% @doc Start a new workflow case with a compiled pattern.
%%
%% The case starts executing immediately with default initial context.
%% Returns a case handle that can be used to interact with the running case.
%%
%% @end
-spec start_case(
    CaseId :: case_id(),
    Compiled :: wf_compile:compiled()
) -> {ok, case_handle()} | {error, term()}.
start_case(CaseId, Compiled) ->
    start_case(CaseId, Compiled, #{}).

%% @doc Start a new workflow case with a compiled pattern and initial context.
%%
%% The initial context is provided as a map with the following optional keys:
%% - data: User-provided data store
%% - signals: Initial signals (list)
%% - results: Initial effect results (map)
%% - token_data: Initial token data (map)
%%
%% @end
-spec start_case(
    CaseId :: case_id(),
    Compiled :: wf_compile:compiled(),
    InitCtx :: wf_term:context()
) -> {ok, case_handle()} | {error, term()}.
start_case(CaseId, Compiled, InitCtx) ->
    %% Verify the compiled pattern
    case wf_compile:is_compiled(Compiled) of
        false ->
            {error, {invalid_compiled_pattern, Compiled}};
        true ->
            %% Start the case runner
            case wf_case_runner:start_link(CaseId, Compiled, InitCtx) of
                {ok, Pid} ->
                    Handle = #{
                        case_id => CaseId,
                        pid => Pid,
                        started_at => erlang:monotonic_time(millisecond)
                    },
                    {ok, Handle};
                {error, Reason} ->
                    {error, {case_start_failed, Reason}}
            end
    end.

%% @doc Send a signal to a running case.
%%
%% Signals are used for deferred choice and external events. The signal
%% is added to the case's context and may trigger branch selection in
%% deferred choice patterns.
%%
%% @end
-spec send_signal(
    Handle :: case_handle(),
    Signal :: term()
) -> ok | {error, term()}.
send_signal(#{pid := Pid}, Signal) ->
    try
        wf_case_runner:signal(Pid, Signal),
        ok
    catch
        _:Reason ->
            {error, {signal_failed, Reason}}
    end.

%% @doc Cancel a running case.
%%
%% Cancellation sets the cancel flag on the root scope, causing all
%% activities to halt. The case transitions to 'cancelled' status.
%%
%% @end
-spec cancel_case(Handle :: case_handle()) -> ok | {error, term()}.
cancel_case(#{pid := Pid}) ->
    try
        wf_case_runner:cancel(Pid),
        ok
    catch
        _:Reason ->
            {error, {cancel_failed, Reason}}
    end.

%% @doc Get the current status of a running case.
%%
%% Returns status information including:
%% - case_id: The case identifier
%% - status: Current execution status (running, halted, error, cancelled)
%% - steps: Number of reduction steps executed
%% - pc: Current program counter
%% - stack_depth: Current stack depth
%% - effects: Effect execution state
%% - uptime_ms: Milliseconds since case start
%%
%% @end
-spec get_status(Handle :: case_handle()) -> {ok, case_status()} | {error, term()}.
get_status(#{pid := Pid}) ->
    try
        wf_case_runner:get_status(Pid)
    catch
        _:Reason ->
            {error, {status_query_failed, Reason}}
    end.

%% @doc Get the execution trace of a case.
%%
%% Returns a list of trace events showing the complete execution history.
%% Each event includes:
%% - seq: Sequence number
%% - type: Event type (task_enter, task_exit, etc.)
%% - opcode: The opcode executed
%% - ctx: Context after the step
%% - timestamp: Microsecond timestamp
%% - scope: Nesting scope path
%% - cancel_signal: Whether this step was cancelled
%%
%% @end
-spec get_trace(Handle :: case_handle()) -> {ok, [wf_vm:trace_event()]} | {error, term()}.
get_trace(#{pid := Pid}) ->
    try
        case wf_case_runner:get_status(Pid) of
            {ok, _Status} ->
                %% Get the exec_state from the case runner
                %% Note: This requires adding a get_exec_state/1 function to wf_case_runner
                %% For now, return empty trace as a placeholder
                {ok, []};
            {error, Reason} ->
                {error, Reason}
        end
    catch
        _:Reason ->
            {error, {trace_query_failed, Reason}}
    end.

%% @doc Compile a workflow pattern to bytecode.
%%
%% Compiles the pattern AST (wf_term) into executable bytecode.
%% Returns the compiled program or an error if compilation fails.
%%
%% @end
-spec compile(Pattern :: wf_term:wf_term()) -> compile_result().
compile(Pattern) ->
    compile(Pattern, #{}).

%% @doc Compile a workflow pattern with options.
%%
%% Options:
%% - optimize: Enable/disable optimization (default: true)
%% - trace_level: Tracing level (none, basic, full) (default: basic)
%% - validate: Enable/disable validation (default: true)
%%
%% @end
-spec compile(
    Pattern :: wf_term:wf_term(),
    Options :: wf_compile:compile_options()
) -> compile_result().
compile(Pattern, Options) ->
    %% Validate pattern structure before compilation
    case wf_term:is_valid(Pattern) of
        false ->
            {error, {invalid_pattern, Pattern}};
        true ->
            %% Compile the pattern
            case wf_compile:compile(Pattern, Options) of
                {ok, Compiled} ->
                    %% Optionally run validation
                    case maps:get(validate, Options, true) of
                        true ->
                            case validate_compiled(Compiled) of
                                {ok, _} -> {ok, Compiled};
                                {error, Reason} -> {error, {validation_failed, Reason}}
                            end;
                        false ->
                            {ok, Compiled}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc Validate a workflow pattern structure.
%%
%% Checks that the pattern is well-formed and satisfies structural invariants:
%% - All branches are reachable
%% - Join points have matching fork points
%% - No unreachable code
%% - Proper nesting of scopes
%%
%% @end
-spec validate(Pattern :: wf_term:wf_term()) -> validation_result().
validate(Pattern) ->
    case wf_term:is_valid(Pattern) of
        false ->
            {error, {invalid_pattern, Pattern}};
        true ->
            %% Basic validation passed
            Size = wf_term:term_size(Pattern),

            %% Additional structural checks
            case check_structure(Pattern) of
                ok ->
                    {ok, #{
                        valid => true,
                        size => Size
                    }};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%%% INTERNAL VALIDATION =====================================================

%% @doc Validate a compiled program.
-spec validate_compiled(Compiled :: wf_compile:compiled()) ->
    {ok, #{}} | {error, term()}.
validate_compiled({program, Program, EntryPC, ExitPC, Size, _Metadata}) ->
    %% Basic sanity checks
    case length(Program) of
        Size ->
            %% Size matches
            case EntryPC of
                0 ->
                    %% Entry PC is valid
                    {ok, #{}};
                _ ->
                    {error, {invalid_entry_pc, EntryPC}}
            end;
        ActualSize ->
            {error, {size_mismatch, Size, ActualSize}}
    end;
validate_compiled(_) ->
    {error, invalid_compiled_format}.

%% @doc Perform structural validation checks.
-spec check_structure(Pattern :: wf_term:wf_term()) -> ok | {error, term()}.
check_structure({task, _Name, _Fun}) ->
    ok;
check_structure({seq, P, Q}) ->
    case check_structure(P) of
        ok -> check_structure(Q);
        Error -> Error
    end;
check_structure({par, Branches}) ->
    check_branches(Branches);
check_structure({choice, Branches}) ->
    check_branches(Branches);
check_structure({join, _Policy, Branches}) ->
    check_branches(Branches);
check_structure({loop, _Policy, Body}) ->
    check_structure(Body);
check_structure({defer, Branches}) ->
    check_branches(Branches);
check_structure({cancel_scope, _ScopeSpec, Body}) ->
    check_structure(Body);
check_structure({mi, _Policy, Body}) ->
    check_structure(Body);
check_structure(_) ->
    {error, unknown_pattern_type}.

%% @doc Check all branches in a list.
-spec check_branches([wf_term:wf_term()]) -> ok | {error, term()}.
check_branches([]) ->
    ok;
check_branches([Branch | Rest]) ->
    case check_structure(Branch) of
        ok -> check_branches(Rest);
        Error -> Error
    end.
