%%%-------------------------------------------------------------------
%%% @doc f5_proof_crash test suite
%%%
%%% Tests crash/restart proof verification ensuring supervisor
%%% properly handles process crashes and restarts.
%%% @end
%%%-------------------------------------------------------------------
-module(f5_proof_crash_test).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Data
%%%-------------------------------------------------------------------

%% Helper to create a crash event
crash_event(Pid, Timestamp, Reason) ->
    #{
        timestamp => Timestamp,
        type => process_exit,
        pid => Pid,
        reason => Reason
    }.

%% Helper to create a restart event
restart_event(Pid, Timestamp, Supervisor) ->
    #{
        timestamp => Timestamp,
        type => process_registered,
        pid => Pid,
        supervisor => Supervisor
    }.

%% Helper to create a restart event with pid_str
restart_event_str(PidStr, Timestamp, Supervisor) ->
    #{
        timestamp => Timestamp,
        type => process_registered,
        pid_str => PidStr,
        supervisor => Supervisor
    }.

%% Helper to create supervisor check event
supervisor_check_event(Supervisor, Phase, Children, Timestamp) ->
    #{
        timestamp => Timestamp,
        type => supervisor_check,
        supervisor => Supervisor,
        phase => Phase,
        children => Children
    }.

%% Helper to create a child map
child_map(ChildId, Pid) ->
    #{ChildId => pid_to_binary(Pid)}.

%% Helper to convert pid to binary
pid_to_binary(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).

%%%-------------------------------------------------------------------
%%% Extract Crash Events Tests
%%%-------------------------------------------------------------------

%% @doc Test extract_crash_events finds process_exit events
extract_crash_events_process_exit_test() ->
    Pid = list_to_pid("<0.100.0>"),
    TraceEvents = [
        #{
            timestamp => 1000,
            type => process_started,
            pid => Pid
        },
        crash_event(Pid, 2000, normal),
        #{
            timestamp => 3000,
            type => other_event,
            pid => Pid
        }
    ],

    CrashEvents = f5_proof_crash:extract_crash_events(TraceEvents),

    ?assertEqual(1, length(CrashEvents)),
    CrashEvent = lists:nth(1, CrashEvents),
    ?assertEqual(2000, maps:get(timestamp, CrashEvent)),
    ?assertEqual(pid_to_binary(Pid), maps:get(crashed_pid, CrashEvent)),
    ?assertEqual(process_exit, maps:get(type, CrashEvent)).

%% @doc Test extract_crash_events finds crash events
extract_crash_events_crash_type_test() ->
    Pid = list_to_pid("<0.101.0>"),
    Reason = {badarith, [{module, function, 1}]},

    TraceEvents = [
        #{
            timestamp => 1000,
            type => crash,
            pid => Pid,
            reason => Reason
        }
    ],

    CrashEvents = f5_proof_crash:extract_crash_events(TraceEvents),

    ?assertEqual(1, length(CrashEvents)),
    CrashEvent = lists:nth(1, CrashEvents),
    ?assertEqual(crash, maps:get(type, CrashEvent)),
    ?assertEqual(Reason, maps:get(reason, CrashEvent)).

%% @doc Test extract_crash_events handles shutdown events
extract_crash_events_shutdown_test() ->
    Pid = list_to_pid("<0.102.0>"),

    TraceEvents = [
        #{
            timestamp => 1000,
            type => shutdown,
            pid => Pid
        }
    ],

    CrashEvents = f5_proof_crash:extract_crash_events(TraceEvents),

    ?assertEqual(1, length(CrashEvents)),
    CrashEvent = lists:nth(1, CrashEvents),
    ?assertEqual(shutdown, maps:get(type, CrashEvent)).

%% @doc Test extract_crash_events handles string PIDs from traces
extract_crash_events_string_pid_test() ->
    PidStr = <<"<0.103.0>">>,

    TraceEvents = [
        #{
            timestamp => 1000,
            type => process_exit,
            pid_str => PidStr,
            reason => timeout
        }
    ],

    CrashEvents = f5_proof_crash:extract_crash_events(TraceEvents),

    ?assertEqual(1, length(CrashEvents)),
    ?assertEqual(PidStr, maps:get(crashed_pid, lists:nth(1, CrashEvents))).

%% @doc Test extract_crash_events returns empty for no crashes
extract_crash_events_no_crashes_test() ->
    TraceEvents = [
        #{timestamp => 1000, type => process_started},
        #{timestamp => 2000, type => message_sent},
        #{timestamp => 3000, type => timer_expired}
    ],

    CrashEvents = f5_proof_crash:extract_crash_events(TraceEvents),

    ?assertEqual(0, length(CrashEvents)).

%% @doc Test extract_crash_events sorts by timestamp
extract_crash_events_sorted_test() ->
    Pid = list_to_pid("<0.104.0>"),

    TraceEvents = [
        crash_event(Pid, 3000, reason3),
        crash_event(Pid, 1000, reason1),
        crash_event(Pid, 2000, reason2)
    ],

    CrashEvents = f5_proof_crash:extract_crash_events(TraceEvents),

    ?assertEqual(3, length(CrashEvents)),
    ?assertEqual(1000, maps:get(timestamp, lists:nth(1, CrashEvents))),
    ?assertEqual(2000, maps:get(timestamp, lists:nth(2, CrashEvents))),
    ?assertEqual(3000, maps:get(timestamp, lists:nth(3, CrashEvents))).

%%%-------------------------------------------------------------------
%%% Detect Restart Window Tests
%%%-------------------------------------------------------------------

%% @doc Test detect_restart_window finds restart after crash
detect_restart_window_found_test() ->
    CrashedPid = <<"<0.200.0>">>,
    CrashTime = 1000,

    TraceEvents = [
        #{
            timestamp => 500,
            type => process_started,
            pid_str => CrashedPid
        },
        #{
            timestamp => CrashTime,
            type => process_exit,
            pid_str => CrashedPid,
            reason => normal
        },
        #{
            timestamp => 1500,
            type => process_registered,
            pid_str => <<"<0.201.0>">>,
            supervisor => test_sup
        }
    ],

    Result = f5_proof_crash:detect_restart_window(CrashedPid, CrashTime, TraceEvents),

    ?assertEqual(true, maps_get(restart_detected, Result)),
    ?assertEqual(500, maps_get(restart_duration_ms, Result)),
    ?assertEqual(1500, maps_get(restart_timestamp, Result)).

%% @doc Test detect_restart_window returns false when no restart
detect_restart_window_not_found_test() ->
    CrashedPid = <<"<0.202.0>">>,
    CrashTime = 1000,

    TraceEvents = [
        #{
            timestamp => CrashTime,
            type => process_exit,
            pid_str => CrashedPid
        }
        %% No restart event
    ],

    Result = f5_proof_crash:detect_restart_window(CrashedPid, CrashTime, TraceEvents),

    ?assertEqual(false, maps_get(restart_detected, Result)),
    ?assertEqual(undefined, maps_get(restart_duration_ms, Result, undefined)).

%% @doc Test detect_restart_window ignores events before crash
detect_restart_window_timing_test() ->
    CrashedPid = <<"<0.203.0>">>,
    CrashTime = 2000,

    TraceEvents = [
        %% Event before crash - should be ignored
        #{
            timestamp => 1000,
            type => process_registered,
            pid_str => <<"<0.204.0>">>
        },
        #{
            timestamp => CrashTime,
            type => process_exit,
            pid_str => CrashedPid
        },
        %% Event after crash - should be detected
        #{
            timestamp => 2500,
            type => process_registered,
            pid_str => <<"<0.205.0>">>
        }
    ],

    Result = f5_proof_crash:detect_restart_window(CrashedPid, CrashTime, TraceEvents),

    ?assertEqual(true, maps_get(restart_detected, Result)),
    ?assertEqual(500, maps_get(restart_duration_ms, Result)).

%% @doc Test detect_restart_window with child_started events
detect_restart_window_child_started_test() ->
    CrashedPid = <<"<0.206.0>">>,
    CrashTime = 1000,

    TraceEvents = [
        #{
            timestamp => CrashTime,
            type => process_exit,
            pid_str => CrashedPid
        },
        #{
            timestamp => 1200,
            type => child_started,
            pid_str => <<"<0.207.0>">>,
            supervisor => my_sup
        }
    ],

    Result = f5_proof_crash:detect_restart_window(CrashedPid, CrashTime, TraceEvents),

    ?assertEqual(true, maps_get(restart_detected, Result)),
    ?assertEqual(200, maps_get(restart_duration_ms, Result)).

%%%-------------------------------------------------------------------
%%% Check Supervisor Tree Tests
%%%-------------------------------------------------------------------

%% @doc Test check_supervisor_tree with non-existent supervisor
check_supervisor_tree_not_found_test() ->
    Result = f5_proof_crash:check_supervisor_tree(nonexistent_sup),

    ?assertMatch({error, {supervisor_not_registered, nonexistent_sup}}, Result).

%% @doc Test check_supervisor_tree with invalid ref type
check_supervisor_tree_invalid_ref_test() ->
    Result = f5_proof_crash:check_supervisor_tree({invalid, "type"}),

    ?assertEqual({error, invalid_supervisor_ref}, Result).

%% @doc Test check_supervisor_tree returns correct structure
check_supervisor_tree_structure_test() ->
    %% We can't easily test with a real supervisor in EUnit
    %% without starting one, so we test the error path
    Result = f5_proof_crash:check_supervisor_tree(undefined_sup),

    ?assertMatch({error, {supervisor_not_registered, undefined_sup}}, Result).

%%%-------------------------------------------------------------------
%%% Verify Crash Restart Tests
%%%-------------------------------------------------------------------

%% @doc Test verify_crash_restart with successful restart
verify_crash_restart_success_test() ->
    SupName = test_sup,
    OldPid = list_to_pid("<0.300.0>"),
    NewPid = list_to_pid("<0.301.0>"),

    TraceEvents = [
        supervisor_check_event(
            SupName, before,
            child_map(worker1, OldPid),
            500
        ),
        crash_event(OldPid, 1000, {badarg, []}),
        supervisor_check_event(
            SupName, 'after',
            child_map(worker1, NewPid),
            1500
        ),
        restart_event(NewPid, 1500, SupName)
    ],

    Result = f5_proof_crash:verify_crash_restart(SupName, TraceEvents),

    ?assertMatch({ok, #{
        proof_type := crash_restart,
        crashed_pid := _,
        supervisor := <<"test_sup">>,
        restart_detected := true
    }}, Result),

    {ok, Proof} = Result,
    ?assertEqual(pid_to_binary(OldPid), maps:get(crashed_pid, Proof)).

%% @doc Test verify_crash_restart fails when no crash events
verify_crash_restart_no_crash_test() ->
    SupName = test_sup,

    TraceEvents = [
        supervisor_check_event(SupName, before, #{}, 500),
        supervisor_check_event(SupName, 'after', #{}, 1500)
    ],

    Result = f5_proof_crash:verify_crash_restart(SupName, TraceEvents),

    ?assertEqual({error, no_crash_events_found}, Result).

%% @doc Test verify_crash_restart fails when restart not detected
verify_crash_restart_no_restart_detected_test() ->
    SupName = test_sup,
    CrashedPid = list_to_pid("<0.302.0>"),

    TraceEvents = [
        crash_event(CrashedPid, 1000, normal)
        %% No restart event
    ],

    Result = f5_proof_crash:verify_crash_restart(SupName, TraceEvents),

    ?assertMatch({error, {restart_not_detected, _}}, Result),

    {error, {restart_not_detected, FailedProof}} = Result,
    ?assertEqual(false, maps_get(restart_detected, FailedProof)).

%% @doc Test verify_crash_restart captures crash reason
verify_crash_restart_captures_reason_test() ->
    SupName = test_sup,
    CrashedPid = list_to_pid("<0.303.0>"),
    Reason = {badarith, [{erlang, '+', [1, foo]}]},

    TraceEvents = [
        crash_event(CrashedPid, 1000, Reason),
        restart_event_str(<<"<0.304.0>">>, 1500, SupName)
    ],

    {ok, Proof} = f5_proof_crash:verify_crash_restart(SupName, TraceEvents),

    ?assertEqual(Reason, maps_get(crash_reason, Proof)).

%% @doc Test verify_crash_restart with multiple crashes picks most recent
verify_crash_restart_multiple_crashes_test() ->
    SupName = test_sup,
    Pid1 = list_to_pid("<0.305.0>"),
    Pid2 = list_to_pid("<0.306.0>"),

    TraceEvents = [
        crash_event(Pid1, 1000, reason1),
        crash_event(Pid2, 2000, reason2),  %% Most recent
        restart_event_str(<<"<0.307.0>">>, 2500, SupName)
    ],

    {ok, Proof} = f5_proof_crash:verify_crash_restart(SupName, TraceEvents),

    %% Should capture the most recent crash
    ?assertEqual(pid_to_binary(Pid2), maps_get(crashed_pid, Proof)),
    ?assertEqual(reason2, maps_get(crash_reason, Proof)).

%%%-------------------------------------------------------------------
%%% Generate Crash Proof Tests
%%%-------------------------------------------------------------------

%% @doc Test generate_crash_proof creates valid JSON
generate_crash_proof_valid_json_test() ->
    SupName = test_sup,
    CrashedPid = list_to_pid("<0.400.0>"),

    TraceEvents = [
        crash_event(CrashedPid, 1000, normal),
        restart_event_str(<<"<0.401.0>">>, 1500, SupName)
    ],

    Evidence = #{
        extra_info => <<"test evidence">>
    },

    Result = f5_proof_crash:generate_crash_proof(SupName, TraceEvents, Evidence),

    ?assertMatch({ok, #{
        proof_type := crash_restart,
        crashed_pid := _,
        supervisor := <<"test_sup">>,
        status := verified,
        restart_detected := true
    }}, Result),

    {ok, JsonProof} = Result,
    %% Check hex-encoded hash
    ?assertEqual(64, byte_size(maps_get(proof_hash, JsonProof))),
    %% Check evidence section
    ?assert(is_map(maps_get(evidence, JsonProof))).

%% @doc Test generate_crash_proof includes failure status
generate_crash_proof_failure_status_test() ->
    SupName = test_sup,
    CrashedPid = list_to_pid("<0.402.0>"),

    TraceEvents = [
        crash_event(CrashedPid, 1000, normal)
        %% No restart
    ],

    Evidence = #{},

    Result = f5_proof_crash:generate_crash_proof(SupName, TraceEvents, Evidence),

    ?assertMatch({ok, #{
        status := failed,
        restart_detected := false
    }}, Result).

%% @doc Test generate_crash_proof formats crash reason
generate_crash_proof_reason_format_test() ->
    SupName = test_sup,
    CrashedPid = list_to_pid("<0.403.0>"),
    Reason = {badmatch, 42},

    TraceEvents = [
        #{
            timestamp => 1000,
            type => process_exit,
            pid => CrashedPid,
            reason => Reason
        },
        restart_event_str(<<"<0.404.0>">>, 1500, SupName)
    ],

    {ok, Proof} = f5_proof_crash:generate_crash_proof(SupName, TraceEvents, #{}),

    CrashReason = maps_get(crash_reason, Proof),
    ?assert(is_binary(CrashReason)),
    ?assertNotEqual(<<>>, CrashReason).

%% @doc Test generate_crash_proof includes restart window duration
generate_crash_proof_restart_window_test() ->
    SupName = test_sup,
    CrashedPid = list_to_pid("<0.405.0>"),

    TraceEvents = [
        crash_event(CrashedPid, 1000, normal),
        restart_event_str(<<"<0.406.0>">>, 1750, SupName)  %% 750ms later
    ],

    {ok, Proof} = f5_proof_crash:generate_crash_proof(SupName, TraceEvents, #{}),

    ?assertEqual(750, maps_get(restart_window_ms, Proof)).

%%%-------------------------------------------------------------------
%%% Compute Proof Hash Tests
%%%-------------------------------------------------------------------

%% @doc Test compute_proof_hash is deterministic
compute_proof_hash_deterministic_test() ->
    Pid = <<"<0.500.0>">>,
    Time = 12345,
    RestartDetected = true,

    Hash1 = f5_proof_crash:compute_proof_hash(Pid, Time, RestartDetected),
    Hash2 = f5_proof_crash:compute_proof_hash(Pid, Time, RestartDetected),

    ?assertEqual(Hash1, Hash2),
    ?assertEqual(32, byte_size(Hash1)).

%% @doc Test compute_proof_hash differs with different inputs
compute_proof_hash_different_inputs_test() ->
    Pid = <<"<0.501.0>">>,
    Time = 12345,

    Hash1 = f5_proof_crash:compute_proof_hash(Pid, Time, true),
    Hash2 = f5_proof_crash:compute_proof_hash(Pid, Time, false),
    Hash3 = f5_proof_crash:compute_proof_hash(Pid, 99999, true),

    ?assertNotEqual(Hash1, Hash2),
    ?assertNotEqual(Hash1, Hash3),
    ?assertNotEqual(Hash2, Hash3).

%%%-------------------------------------------------------------------
%%% Get Crash Evidence Tests
%%%-------------------------------------------------------------------

%% @doc Test get_crash_evidence returns valid structure
get_crash_evidence_structure_test() ->
    SupName = test_sup,

    Result = f5_proof_crash:get_crash_evidence(SupName),

    ?assertMatch({ok, #{
        trace_events := [],
        supervisor_state := _,
        crash_dump := _,
        system_log := _
    }}, Result).

%%%-------------------------------------------------------------------
%%% Complex Scenario Tests
%%%-------------------------------------------------------------------

%% @doc Test full crash and restart workflow
full_crash_restart_workflow_test() ->
    SupName = workflow_sup,
    WorkerPid = list_to_pid("<0.600.0>"),
    NewWorkerPid = list_to_pid("<0.601.0>"),

    TraceEvents = [
        %% Before crash: worker is running
        supervisor_check_event(
            SupName, before,
            child_map(worker, WorkerPid),
            500
        ),
        %% Worker crashes
        crash_event(WorkerPid, 1000, {badarity, [{mod, 'fun', 1}]}),
        %% Supervisor detects and restarts
        restart_event(NewWorkerPid, 1200, SupName),
        %% After crash: new worker registered
        supervisor_check_event(
            SupName, 'after',
            child_map(worker, NewWorkerPid),
            1300
        )
    ],

    %% Verify crash restart
    {ok, Proof} = f5_proof_crash:verify_crash_restart(SupName, TraceEvents),

    %% Check all expected fields
    ?assertEqual(crash_restart, maps_get(proof_type, Proof)),
    ?assertEqual(pid_to_binary(WorkerPid), maps_get(crashed_pid, Proof)),
    ?assertEqual(<<"workflow_sup">>, maps_get(supervisor, Proof)),
    ?assertEqual(true, maps_get(restart_detected, Proof)),
    ?assert(is_integer(maps_get(restart_window_ms, Proof))),
    ?assert(is_list(maps_get(evidence_sources, Proof))),
    ?assertEqual(32, byte_size(maps_get(proof_hash, Proof))),
    ?assert(is_integer(maps_get(verified_at, Proof))).

%% @doc Test crash with shutdown reason
crash_with_shutdown_reason_test() ->
    SupName = shutdown_sup,
    Pid = list_to_pid("<0.700.0>"),

    TraceEvents = [
        #{
            timestamp => 1000,
            type => shutdown,
            pid => Pid
        },
        restart_event_str(<<"<0.701.0>">>, 1500, SupName)
    ],

    {ok, Proof} = f5_proof_crash:verify_crash_restart(SupName, TraceEvents),

    ?assertEqual(shutdown, maps_get(crash_reason, Proof)),
    ?assertEqual(true, maps_get(restart_detected, Proof)).

%% @doc Test multiple crashes in sequence
multiple_crashes_sequence_test() ->
    SupName = multi_sup,
    Pid1 = list_to_pid("<0.800.0>"),
    Pid2 = list_to_pid("<0.801.0>"),
    _Pid3 = list_to_pid("<0.802.0>"),

    TraceEvents = [
        crash_event(Pid1, 1000, crash1),
        restart_event_str(<<"<0.801.0>">>, 1100, SupName),
        crash_event(Pid2, 2000, crash2),
        restart_event_str(<<"<0.802.0>">>, 2100, SupName),
        restart_event_str(<<"<0.803.0>">>, 2200, SupName)
    ],

    {ok, Proof} = f5_proof_crash:verify_crash_restart(SupName, TraceEvents),

    %% Should verify the most recent crash (Pid2)
    ?assertEqual(pid_to_binary(Pid2), maps_get(crashed_pid, Proof)),
    ?assertEqual(crash2, maps_get(crash_reason, Proof)),
    ?assertEqual(true, maps_get(restart_detected, Proof)).

%% @doc Test evidence sources are correctly detected
evidence_sources_detection_test() ->
    SupName = evidence_sup,
    Pid = list_to_pid("<0.900.0>"),

    TraceEvents = [
        supervisor_check_event(SupName, before, child_map(w1, Pid), 500),
        crash_event(Pid, 1000, normal),
        restart_event_str(<<"<0.901.0>">>, 1500, SupName),
        supervisor_check_event(SupName, 'after', child_map(w1, list_to_pid("<0.901.0>")), 1600)
    ],

    {ok, Proof} = f5_proof_crash:verify_crash_restart(SupName, TraceEvents),

    Sources = maps_get(evidence_sources, Proof),

    %% Should have at least trace, supervisor, restart_window
    ?assert(lists:member(trace, Sources)).

%%%-------------------------------------------------------------------
%%% Helper Functions
%%%-------------------------------------------------------------------

%% @private Safe maps get with default
maps_get(Key, Map, Default) ->
    case maps:find(Key, Map) of
        {ok, Value} -> Value;
        error -> Default
    end.

maps_get(Key, Map) ->
    maps_get(Key, Map, undefined).
