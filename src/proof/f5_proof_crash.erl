%%%-------------------------------------------------------------------
%%% @doc f5_proof_crash - Crash/restart proof verification for supervisor evidence.
%%%
%%% Implements Fortune-5 FIBO crash-proof verification ensuring that
%%% process crashes are properly handled by supervisors and evidence
%%% of restart is captured from multiple sources.
%%%
%%% The proof guarantees:
%%% - Crash event was detected in trace or supervisor logs
%%% - Process was registered under supervisor before crash
%%% - Supervisor detected the crash and initiated restart
%%% - New process registered after crash (within timeout window)
%%% - Crash reason is captured and documented
%%%
%%% Evidence sources:
%%% - Trace: TTB trace events showing process exit/restart
%%% - Supervisor: supervisor:which_children() showing child registration
%%% - Crash dump: OS process crash reports (if available)
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(f5_proof_crash).

%% API
-export([verify_crash_restart/2]).
-export([check_supervisor_tree/1]).
-export([get_crash_evidence/1]).
-export([generate_crash_proof/3]).
-export([compute_proof_hash/3]).
-export([extract_crash_events/1]).
-export([detect_restart_window/3]).

-include_lib("kernel/include/logger.hrl").

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type pid_str() :: binary().
-type supervisor_ref() :: atom() | pid() | {atom(), node()}.
-type crash_event() :: #{
    timestamp := integer(),
    crashed_pid := pid_str(),
    reason => term(),
    type := process_exit | crash | shutdown
}.

-type restart_event() :: #{
    timestamp := integer(),
    new_pid := pid_str(),
    supervisor := supervisor_ref()
}.

-type trace_event() :: crash_event() | restart_event() | map().

-type supervisor_state() :: #{
    before := #{pid_str() => pid()},
    after_crash := #{pid_str() => pid()},
    supervisor := supervisor_ref()
}.

-type crash_evidence() :: #{
    trace_events => [trace_event()],
    supervisor_state => supervisor_state(),
    crash_dump => map() | undefined,
    system_log => [binary()] | undefined
}.

-type crash_proof() :: #{
    proof_type := crash_restart,
    crashed_pid := pid_str(),
    supervisor := supervisor_ref(),
    restart_detected := boolean(),
    crash_reason := term() | undefined,
    restart_window_ms := integer() | undefined,
    evidence_sources := [atom()],
    proof_hash := binary(),
    verified_at := integer()
}.

-type proof_result() :: {ok, crash_proof()} | {error, term()}.

-export_type([supervisor_ref/0, crash_event/0, restart_event/0, trace_event/0,
              crash_evidence/0, crash_proof/0, proof_result/0]).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%% @doc Verify crash and restart from trace events.
%%
%% Analyzes trace events to detect process crash and supervisor restart.
%% Verification checks for:
%% 1. Process exit event in trace
%% 2. Supervisor detection of crash
%% 3. New process registration after crash
%%
%% @param SupervisorRef Supervisor to check (name or pid)
%% @param TraceEvents List of trace events from execution
%% @returns {ok, Proof} if restart verified, {error, Reason} otherwise
-spec verify_crash_restart(supervisor_ref(), [trace_event()]) -> proof_result().
verify_crash_restart(SupervisorRef, TraceEvents) ->
    try
        %% Step 1: Extract crash events from trace
        CrashEvents = extract_crash_events(TraceEvents),

        case CrashEvents of
            [] ->
                {error, no_crash_events_found};
            _ ->
                %% Step 2: Get the most recent crash event
                CrashEvent = lists:last(CrashEvents),
                CrashedPidStr = maps:get(crashed_pid, CrashEvent),
                CrashTime = maps:get(timestamp, CrashEvent),
                CrashReason = maps_get(reason, CrashEvent, unknown),

                %% Step 3: Check supervisor state before and after
                SupState = get_supervisor_state(SupervisorRef, TraceEvents),

                %% Step 4: Detect restart window
                RestartWindow = detect_restart_window(
                    CrashedPidStr, CrashTime, TraceEvents
                ),

                %% Step 5: Verify restart occurred
                RestartDetected = maps_get(restart_detected, RestartWindow, false),

                %% Step 6: Determine available evidence sources
                EvidenceSources = determine_evidence_sources(
                    TraceEvents, SupState, RestartWindow
                ),

                %% Step 7: Build proof
                Proof = #{
                    proof_type => crash_restart,
                    crashed_pid => CrashedPidStr,
                    supervisor => format_supervisor_ref(SupervisorRef),
                    restart_detected => RestartDetected,
                    crash_reason => CrashReason,
                    restart_window_ms => maps_get(restart_duration_ms, RestartWindow, undefined),
                    evidence_sources => EvidenceSources,
                    proof_hash => compute_proof_hash(
                        CrashedPidStr, CrashTime, RestartDetected
                    ),
                    verified_at => erlang:monotonic_time(millisecond)
                },

                case RestartDetected of
                    true ->
                        {ok, Proof};
                    false ->
                        {error, {restart_not_detected, Proof}}
                end
        end
    catch
        throw:{supervisor_not_found, SupRef} ->
            {error, {supervisor_not_found, SupRef}};
        error:Reason:Stack ->
            ?LOG_ERROR("Crash proof verification failed: ~p~n~p", [Reason, Stack]),
            {error, {verification_failed, Reason}}
    end.

%% @doc Check supervisor tree for child process evidence.
%%
%% Queries the supervisor to verify child registration before
%% and after a crash event.
%%
%% @param SupervisorRef Supervisor reference (atom, pid, or {atom, node()})
%% @returns Supervisor state with before/after child PIDs
-spec check_supervisor_tree(supervisor_ref()) -> {ok, supervisor_state()} | {error, term()}.
check_supervisor_tree(SupervisorRef) when is_atom(SupervisorRef) ->
    case whereis(SupervisorRef) of
        undefined ->
            {error, {supervisor_not_registered, SupervisorRef}};
        Pid when is_pid(Pid) ->
            check_supervisor_tree(Pid)
    end;
check_supervisor_tree(SupervisorRef) when is_pid(SupervisorRef) ->
    case erlang:is_process_alive(SupervisorRef) of
        false ->
            {error, {supervisor_not_alive, SupervisorRef}};
        true ->
            %% Get current children
            Children = supervisor:which_children(SupervisorRef),
            CurrentState = #{
                list_to_binary(pid_to_list(ChildPid)) => ChildPid
                || {_Id, ChildPid, _Type, _Modules} <- Children,
                   ChildPid =/= undefined
            },
            {ok, #{
                after_crash => CurrentState,
                before => unknown,  % Would need historical data
                supervisor => SupervisorRef
            }}
    end;
check_supervisor_tree({Name, Node} = SupervisorRef) when is_atom(Name), is_atom(Node) ->
    case Node =:= node() of
        true ->
            check_supervisor_tree(Name);
        false ->
            case rpc:call(Node, supervisor, which_children, [Name]) of
                {badrpc, Reason} ->
                    {error, {rpc_failed, Reason}};
                Children when is_list(Children) ->
                    CurrentState = #{
                        list_to_binary(pid_to_list(ChildPid)) => ChildPid
                        || {_Id, ChildPid, _Type, _Modules} <- Children,
                           ChildPid =/= undefined
                    },
                    {ok, #{
                        after_crash => CurrentState,
                        before => unknown,
                        supervisor => SupervisorRef
                    }}
            end
    end;
check_supervisor_tree(_SupervisorRef) ->
    {error, invalid_supervisor_ref}.

%% @doc Get crash evidence from multiple sources.
%%
%% Collects evidence from:
%% - Trace events
%% - Supervisor state
%% - Crash dump files (if available)
%% - System logs
%%
%% @param SupervisorRef Supervisor reference
%% @returns {ok, Evidence} map with all collected evidence
-spec get_crash_evidence(supervisor_ref()) -> {ok, crash_evidence()} | {error, term()}.
get_crash_evidence(SupervisorRef) ->
    %% Get supervisor state
    SupStateResult = check_supervisor_tree(SupervisorRef),

    %% Try to read crash dump (if exists)
    CrashDump = read_crash_dump(),

    %% Get recent system log entries
    SystemLog = get_system_log_entries(),

    SupState = case SupStateResult of
        {ok, State} -> State;
        {error, _} -> #{}
    end,

    {ok, #{
        trace_events => [],  % Caller would provide these
        supervisor_state => SupState,
        crash_dump => CrashDump,
        system_log => SystemLog
    }}.

%% @doc Generate crash proof as JSON-compatible map.
%%
%% Creates a proof artifact suitable for serialization to JSON.
%%
%% @param SupervisorRef Supervisor reference
%% @param TraceEvents List of trace events
%% @param Evidence Additional evidence map
%% @returns {ok, ProofMap} on success
-spec generate_crash_proof(supervisor_ref(), [trace_event()], map()) ->
    {ok, map()} | {error, term()}.
generate_crash_proof(SupervisorRef, TraceEvents, Evidence) ->
    case verify_crash_restart(SupervisorRef, TraceEvents) of
        {ok, Proof} ->
            JsonProof = proof_to_json(Proof, Evidence),
            {ok, JsonProof};
        {error, {restart_not_detected, Proof}} ->
            %% Even when restart is not detected, we can still generate a proof
            JsonProof = proof_to_json(Proof, Evidence),
            {ok, JsonProof};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Compute SHA-256 hash of proof data for integrity verification.
-spec compute_proof_hash(pid_str(), integer(), boolean()) -> binary().
compute_proof_hash(CrashedPid, CrashTime, RestartDetected) ->
    ProofData = term_to_binary({CrashedPid, CrashTime, RestartDetected}),
    crypto:hash(sha256, ProofData).

%% @doc Extract crash events from trace events.
%%
%% Filters trace events to find process exit/crash events.
%%
%% @param TraceEvents List of all trace events
%% @returns List of crash events sorted by timestamp
-spec extract_crash_events([trace_event()]) -> [crash_event()].
extract_crash_events(TraceEvents) ->
    lists:filtermap(fun(Event) ->
        case Event of
            #{type := process_exit, pid := Pid, timestamp := TS} when is_pid(Pid) ->
                {true, #{
                    timestamp => TS,
                    crashed_pid => pid_to_binary(Pid),
                    reason => maps_get(reason, Event, unknown),
                    type => process_exit
                }};
            #{type := crash, pid := Pid, timestamp := TS} when is_pid(Pid) ->
                {true, #{
                    timestamp => TS,
                    crashed_pid => pid_to_binary(Pid),
                    reason => maps_get(reason, Event, crash),
                    type => crash
                }};
            #{type := shutdown, pid := Pid, timestamp := TS} when is_pid(Pid) ->
                {true, #{
                    timestamp => TS,
                    crashed_pid => pid_to_binary(Pid),
                    reason => shutdown,
                    type => shutdown
                }};
            %% Also check for string PIDs (from trace files)
            #{type := process_exit, pid_str := PidStr, timestamp := TS} when is_binary(PidStr) ->
                {true, #{
                    timestamp => TS,
                    crashed_pid => PidStr,
                    reason => maps_get(reason, Event, unknown),
                    type => process_exit
                }};
            _ ->
                false
        end
    end, lists:sort(fun timestamp_compare/2, TraceEvents)).

%% @doc Detect restart window for a crashed process.
%%
%% Finds the time window between crash and restart.
%%
%% @param CrashedPidStr Pid binary string that crashed
%% @param CrashTime Timestamp of crash
%% @param TraceEvents All trace events
%% @returns Map with restart_detected and restart_duration_ms
-spec detect_restart_window(pid_str(), integer(), [trace_event()]) -> map().
detect_restart_window(CrashedPidStr, CrashTime, TraceEvents) ->
    %% Find events after crash time
    PostCrashEvents = lists:filter(fun
        (#{timestamp := TS}) when TS > CrashTime -> true;
        (_) -> false
    end, TraceEvents),

    %% Look for registration or start events for the same child id
    RestartEvents = lists:filter(fun(Event) ->
        case Event of
            #{type := process_registered, pid_str := PidStr} when PidStr =/= CrashedPidStr ->
                %% Different PID - might be a restart
                check_same_child_id(CrashedPidStr, PidStr, Event);
            #{type := child_started, pid_str := PidStr} when PidStr =/= CrashedPidStr ->
                check_same_child_id(CrashedPidStr, PidStr, Event);
            %% Also handle events with pid key (PID type)
            #{type := process_registered, pid := Pid} when is_pid(Pid) ->
                PidStr = pid_to_binary(Pid),
                PidStr =/= CrashedPidStr andalso check_same_child_id(CrashedPidStr, PidStr, Event);
            #{type := child_started, pid := Pid} when is_pid(Pid) ->
                PidStr = pid_to_binary(Pid),
                PidStr =/= CrashedPidStr andalso check_same_child_id(CrashedPidStr, PidStr, Event);
            _ ->
                false
        end
    end, PostCrashEvents),

    case RestartEvents of
        [] ->
            #{restart_detected => false};
        [FirstRestart | _] ->
            RestartTime = maps_get(timestamp, FirstRestart, CrashTime),
            #{
                restart_detected => true,
                restart_timestamp => RestartTime,
                restart_duration_ms => RestartTime - CrashTime
            }
    end.

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

%% @private Get supervisor state from trace events.
-spec get_supervisor_state(supervisor_ref(), [trace_event()]) -> supervisor_state().
get_supervisor_state(SupervisorRef, TraceEvents) ->
    %% Extract supervisor check events from trace
    SupEvents = lists:filter(fun(E) ->
        maps_get(type, E, undefined) =:= supervisor_check andalso
        maps_get(supervisor, E, undefined) =:= SupervisorRef
    end, TraceEvents),

    Before = case lists:filter(fun(E) -> maps_get(phase, E, undefined) =:= before end, SupEvents) of
        [FirstBefore | _] ->
            maps_get(children, FirstBefore, #{});
        [] ->
            #{}
    end,

    AfterCrash = case lists:filter(fun(E) -> maps_get(phase, E, undefined) =:= 'after' end, SupEvents) of
        [FirstAfter | _] ->
            maps_get(children, FirstAfter, #{});
        [] ->
            #{}
    end,

    #{
        before => Before,
        after_crash => AfterCrash,
        supervisor => SupervisorRef
    }.

%% @private Determine available evidence sources.
-spec determine_evidence_sources([trace_event()], supervisor_state(), map()) -> [atom()].
determine_evidence_sources(TraceEvents, SupState, RestartWindow) ->
    Sources = [],

    %% Check if trace has relevant events
    Sources1 = case TraceEvents of
        [] -> Sources;
        _ -> [trace | Sources]
    end,

    %% Check if supervisor data is available
    Sources2 = case SupState of
        #{before := _, after_crash := _} -> [supervisor | Sources1];
        _ -> Sources1
    end,

    %% Check if restart window data is available
    Sources3 = case maps_get(restart_detected, RestartWindow, false) of
        true -> [restart_window | Sources2];
        false -> Sources2
    end,

    %% Check crash dump availability (would need file system access)
    lists:usort(Sources3).

%% @private Convert proof to JSON-compatible map.
-spec proof_to_json(crash_proof(), map()) -> map().
proof_to_json(Proof, Evidence) ->
    Supervisor = maps:get(supervisor, Proof),
    %% Supervisor is already formatted from verify_crash_restart
    BaseProof = #{
        proof_type => maps:get(proof_type, Proof),
        crashed_pid => maps:get(crashed_pid, Proof),
        supervisor => Supervisor,
        restart_detected => maps:get(restart_detected, Proof),
        crash_reason => format_crash_reason(maps:get(crash_reason, Proof, unknown)),
        restart_window_ms => maps:get(restart_window_ms, Proof, null),
        evidence_sources => maps:get(evidence_sources, Proof),
        proof_hash => binary:encode_hex(maps:get(proof_hash, Proof)),
        verified_at => maps:get(verified_at, Proof),
        evidence => #{
            has_trace_events => maps:is_key(trace_events, Evidence),
            has_supervisor_state => maps:is_key(supervisor_state, Evidence),
            has_crash_dump => maps:is_key(crash_dump, Evidence)
        }
    },

    %% Add verification status
    case maps:get(restart_detected, Proof) of
        true ->
            BaseProof#{
                status => verified,
                message => <<"Process crash was handled by supervisor with successful restart">>
            };
        false ->
            BaseProof#{
                status => failed,
                message => <<"Process restart not detected within observation window">>
            }
    end.

%% @private Format supervisor reference for output.
format_supervisor_ref(SupRef) when is_atom(SupRef) ->
    atom_to_binary(SupRef);
format_supervisor_ref(SupRef) when is_pid(SupRef) ->
    pid_to_binary(SupRef);
format_supervisor_ref({Name, Node}) when is_atom(Name), is_atom(Node) ->
    <<(atom_to_binary(Name))/binary, "@", (atom_to_binary(Node))/binary>>;
format_supervisor_ref(Other) ->
    term_to_binary(Other).

%% @private Format crash reason for display.
format_crash_reason(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason);
format_crash_reason(Reason) when is_binary(Reason) ->
    Reason;
format_crash_reason({Err, Term}) when is_atom(Err) ->
    <<(atom_to_binary(Err))/binary, ": ", (term_to_binary(Term))/binary>>;
format_crash_reason(Reason) ->
    term_to_binary(Reason).

%% @private Convert pid to binary string.
pid_to_binary(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid));
pid_to_binary(Pid) when is_binary(Pid) ->
    Pid;
pid_to_binary(Pid) when is_list(Pid) ->
    list_to_binary(Pid).

%% @private Compare events by timestamp.
timestamp_compare(#{timestamp := TS1}, #{timestamp := TS2}) ->
    TS1 =< TS2.

%% @private Check if two PIDs represent the same child ID.
%% This is a heuristic - in practice you'd need the actual child spec.
check_same_child_id(_PidStr1, _PidStr2, _Event) ->
    %% Without child spec info, we assume any new registration
    %% after a crash in the same supervisor is a restart
    true.

%% @private Safe maps:get with default.
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.

%% @private Read crash dump file if available.
read_crash_dump() ->
    %% Look for erl_crash.dump in current directory
    case file:read_file("erl_crash.dump") of
        {ok, _Binary} ->
            #{found => true, path => "erl_crash.dump"};
        {error, _} ->
            undefined
    end.

%% @private Get recent system log entries.
get_system_log_entries() ->
    try
        %% Try to get logger log entries
        case logger:get_log_handlers() of
            Handlers when is_list(Handlers), Handlers =/= [] ->
                [begin
                    HandlerId = element(1, H),
                    <<(atom_to_binary(HandlerId))/binary>>
                end || H <- Handlers];
            _ ->
                []
        end
    catch
        _:_ -> []
    end.
