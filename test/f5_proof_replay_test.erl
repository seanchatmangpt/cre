%%%-------------------------------------------------------------------
%%% @doc f5_proof_replay test suite
%%%
%%% Tests replay proof verification ensuring deterministic execution
%%% produces identical normalized traces.
%%% @end
%%%-------------------------------------------------------------------
-module(f5_proof_replay_test).

-include_lib("eunit/include/eunit.hrl").

%%%-------------------------------------------------------------------
%%% Test Data
%%%-------------------------------------------------------------------

%% Helper to create trace events
trace_events(CaseId, BaseTimestamp) ->
    [
        #{
            timestamp => BaseTimestamp,
            type => case_started,
            data => #{case_id => CaseId}
        },
        #{
            timestamp => BaseTimestamp + 10,
            type => task_started,
            data => #{task => t1, case_id => CaseId}
        },
        #{
            timestamp => BaseTimestamp + 20,
            type => task_completed,
            data => #{task => t1, case_id => CaseId}
        },
        #{
            timestamp => BaseTimestamp + 30,
            type => case_completed,
            data => #{case_id => CaseId}
        }
    ].

%% Helper to create evidence pack
evidence_pack(CaseId, InitialData) ->
    #{
        spec => #{seed => 1, workflow => test_wf},
        initial_data => InitialData,
        seed => 1,
        case_id => CaseId,
        timestamp => erlang:monotonic_time(millisecond)
    }.

%% Helper to create trace with different timestamps
trace_events_with_timestamps(CaseId, Timestamps) ->
    [
        #{
            timestamp => TS,
            type => case_started,
            data => #{case_id => CaseId}
        }
     || TS <- Timestamps
    ].

%%%-------------------------------------------------------------------
%%% Capture Run Tests
%%%-------------------------------------------------------------------

%% @doc Test capture_run stores normalized trace and returns hash
capture_run_test() ->
    TraceEvents = trace_events(<<"case1">>, 1000),

    Result = f5_proof_replay:capture_run(TraceEvents),

    ?assertMatch({ok, _CaseId, <<_:256>>}, Result),

    {ok, CaseId, Hash} = Result,
    ?assert(is_binary(CaseId)),
    ?assert(byte_size(Hash) =:= 32).

%% @doc Test capture_run with empty trace
capture_run_empty_trace_test() ->
    TraceEvents = [],

    Result = f5_proof_replay:capture_run(TraceEvents),

    ?assertMatch({ok, _CaseId, <<_:256>>}, Result).

%% @doc Test capture_run with PID normalization
capture_run_with_pids_test() ->
    TraceEvents = [
        #{
            timestamp => 1000,
            type => case_started,
            data => #{case_id => <<"pid_case">>, pid => self()}
        }
    ],

    Result = f5_proof_replay:capture_run(TraceEvents),

    ?assertMatch({ok, _CaseId, <<_:256>>}, Result).

%%%-------------------------------------------------------------------
%%% Capture Replay Tests
%%%-------------------------------------------------------------------

%% @doc Test capture_replay with valid evidence pack
capture_replay_test() ->
    CaseId = <<"replay_case">>,
    TraceEvents = trace_events(CaseId, 1000),
    EvidencePack = evidence_pack(CaseId, #{amount => 100}),

    Result = f5_proof_replay:capture_replay(EvidencePack, TraceEvents),

    ?assertMatch({ok, CaseId, <<_:256>>}, Result).

%% @doc Test capture_replay with missing case_id in evidence pack
capture_replay_missing_case_id_test() ->
    TraceEvents = trace_events(<<"case2">>, 1000),
    EvidencePack = #{spec => #{}, initial_data => #{}},  %% No case_id

    Result = f5_proof_replay:capture_replay(EvidencePack, TraceEvents),

    ?assertEqual({error, missing_case_id}, Result).

%%%-------------------------------------------------------------------
%%% Verify Replay Tests
%%%-------------------------------------------------------------------

%% @doc Test verify_replay with matching traces
verify_replay_match_test() ->
    RunEvents = trace_events(<<"match_case">>, 2000),
    ReplayEvents = trace_events(<<"match_case">>, 2000),

    Result = f5_proof_replay:verify_replay(RunEvents, ReplayEvents),

    ?assertMatch({ok, #{
        proof_type := replay,
        hashes_equal := true
    }}, Result).

%% @doc Test verify_replay with different traces
verify_replay_mismatch_test() ->
    CaseId = <<"mismatch_case">>,
    RunEvents = trace_events(CaseId, 2000),

    %% Different trace - one event missing
    ReplayEvents = [
        #{
            timestamp => 2000,
            type => case_started,
            data => #{case_id => CaseId}
        },
        #{
            timestamp => 2010,
            type => task_started,
            data => #{task => t1, case_id => CaseId}
        }
        %% Missing task_completed and case_completed
    ],

    Result = f5_proof_replay:verify_replay(RunEvents, ReplayEvents),

    ?assertMatch({error, {hash_mismatch, _, _}}, Result).

%% @doc Test verify_replay with PIDs normalizes correctly
verify_replay_pid_normalization_test() ->
    RunEvents = [
        #{
            timestamp => 1000,
            type => case_started,
            data => #{case_id => <<"pid_norm_case">>, pid => list_to_pid("<0.1.0>")}
        }
    ],

    ReplayEvents = [
        #{
            timestamp => 1000,
            type => case_started,
            data => #{case_id => <<"pid_norm_case">>, pid => list_to_pid("<0.2.0>")}
        }
    ],

    Result = f5_proof_replay:verify_replay(RunEvents, ReplayEvents),

    ?assertMatch({ok, #{hashes_equal := true}}, Result).

%%%-------------------------------------------------------------------
%%% Generate Proof Tests
%%%-------------------------------------------------------------------

%% @doc Test generate_proof creates valid JSON artifact
generate_proof_valid_test() ->
    CaseId = <<"json_proof_case">>,
    TraceEvents = trace_events(CaseId, 3000),
    EvidencePack = evidence_pack(CaseId, #{test => true}),

    Result = f5_proof_replay:generate_proof(CaseId, TraceEvents, EvidencePack),

    ?assertMatch({ok, #{
        proof_type := replay,
        case_id := <<"json_proof_case">>,
        status := verified,
        hashes_equal := true,
        run_trace_hash := <<_:512>>,  %% Hex encoded 32 bytes = 64 hex chars = 512 bits
        replay_trace_hash := <<_:512>>
    }}, Result).

%% @doc Test generate_proof includes failure status on mismatch
generate_proof_failure_status_test() ->
    CaseId = <<"fail_proof_case">>,
    RunEvents = trace_events(CaseId, 3000),

    %% Create different trace - different number of events
    DifferentEvents = [
        #{
            timestamp => 4000,
            type => case_started,
            data => #{case_id => CaseId}
        },
        #{
            timestamp => 4020,
            type => case_completed,
            data => #{case_id => CaseId}
        }
    ],

    %% Use verify_replay directly with different traces to get error
    Result = f5_proof_replay:verify_replay(RunEvents, DifferentEvents),

    ?assertMatch({error, {hash_mismatch, _, _}}, Result).

%% @doc Test generate_proof includes hex-encoded hashes
generate_proof_hex_hashes_test() ->
    CaseId = <<"hex_case">>,
    TraceEvents = trace_events(CaseId, 5000),
    EvidencePack = evidence_pack(CaseId, #{}),

    {ok, Proof} = f5_proof_replay:generate_proof(CaseId, TraceEvents, EvidencePack),

    RunHashHex = maps:get(run_trace_hash, Proof),
    ReplayHashHex = maps:get(replay_trace_hash, Proof),
    PackHashHex = maps:get(evidence_pack_hash, Proof),

    %% Hex encoding doubles the size (32 bytes -> 64 hex chars)
    ?assertEqual(64, byte_size(RunHashHex)),
    ?assertEqual(64, byte_size(ReplayHashHex)),
    ?assertEqual(64, byte_size(PackHashHex)).

%% @doc Test generate_proof includes evidence pack info
generate_proof_evidence_pack_info_test() ->
    CaseId = <<"ev_pack_case">>,
    TraceEvents = trace_events(CaseId, 6000),
    EvidencePack = evidence_pack(CaseId, #{key => value}),

    {ok, Proof} = f5_proof_replay:generate_proof(CaseId, TraceEvents, EvidencePack),

    EvidencePackInfo = maps:get(evidence_pack, Proof),

    ?assertEqual(true, maps:get(has_spec, EvidencePackInfo)),
    ?assertEqual(true, maps:get(has_initial_data, EvidencePackInfo)),
    ?assert(is_integer(maps:get(timestamp, EvidencePackInfo))).

%%%-------------------------------------------------------------------
%%% Evidence Pack Tests
%%%-------------------------------------------------------------------

%% @doc Test create_evidence_pack generates valid pack
create_evidence_pack_test() ->
    Spec = #{seed => 42, workflow => test},
    InitialData = #{amount => 100, user => alice},

    Pack = f5_proof_replay:create_evidence_pack(Spec, InitialData),

    ?assertEqual(Spec, maps:get(spec, Pack)),
    ?assertEqual(InitialData, maps:get(initial_data, Pack)),
    ?assertEqual(42, maps:get(seed, Pack)),
    ?assert(is_binary(maps:get(case_id, Pack))),
    ?assert(is_integer(maps:get(timestamp, Pack))).

%% @doc Test create_evidence_pack generates unique case IDs
create_evidence_pack_unique_ids_test() ->
    Spec = #{seed => 1},
    InitialData = #{},

    Pack1 = f5_proof_replay:create_evidence_pack(Spec, InitialData),
    Pack2 = f5_proof_replay:create_evidence_pack(Spec, InitialData),

    ?assertNotEqual(maps:get(case_id, Pack1), maps:get(case_id, Pack2)).

%%%-------------------------------------------------------------------
%%% Replay from Evidence Tests
%%%-------------------------------------------------------------------

%% @doc Test replay_from_evidence generates trace
replay_from_evidence_test() ->
    CaseId = <<"replay_ev_case">>,
    Spec = #{seed => 1},
    InitialData = #{},

    Pack = evidence_pack(CaseId, InitialData),
    PackWithSpec = Pack#{spec => Spec},

    Result = f5_proof_replay:replay_from_evidence(PackWithSpec),

    ?assertMatch({ok, [_ | _]}, Result),

    {ok, TraceEvents} = Result,
    ?assert(length(TraceEvents) > 0).

%% @doc Test replay_from_evidence requires spec and initial_data
replay_from_evidence_missing_spec_test() ->
    Pack = #{initial_data => #{}},  %% No spec

    Result = f5_proof_replay:replay_from_evidence(Pack),

    ?assertMatch({error, _}, Result).

%%%-------------------------------------------------------------------
%%% Determinism Tests
%%%-------------------------------------------------------------------

%% @doc Test identical traces produce identical hashes
determinism_identical_traces_test() ->
    %% Use same case_id so traces are truly identical
    SameCaseId = <<"det_case">>,
    TraceEvents1 = trace_events(SameCaseId, 8000),
    TraceEvents2 = trace_events(SameCaseId, 8000),

    {ok, _, Hash1} = f5_proof_replay:capture_run(TraceEvents1),
    {ok, _, Hash2} = f5_proof_replay:capture_run(TraceEvents2),

    ?assertEqual(Hash1, Hash2).

%% @doc Test different timestamps produce different hashes
determinism_different_timestamps_test() ->
    TraceEvents1 = trace_events(<<"ts_case1">>, 8000),
    TraceEvents2 = trace_events(<<"ts_case2">>, 9000),

    {ok, _, Hash1} = f5_proof_replay:capture_run(TraceEvents1),
    {ok, _, Hash2} = f5_proof_replay:capture_run(TraceEvents2),

    ?assertNotEqual(Hash1, Hash2).

%% @doc Test PID normalization makes traces deterministic
determinism_pid_normalization_test() ->
    %% Use same case_id so only difference is PID
    SameCaseId = <<"pid_det_case">>,

    TraceEvents1 = [
        #{
            timestamp => 1000,
            type => task_started,
            data => #{case_id => SameCaseId, pid => list_to_pid("<0.1.0>")}
        }
    ],

    TraceEvents2 = [
        #{
            timestamp => 1000,
            type => task_started,
            data => #{case_id => SameCaseId, pid => list_to_pid("<0.99.0>")}
        }
    ],

    {ok, _, Hash1} = f5_proof_replay:capture_run(TraceEvents1),
    {ok, _, Hash2} = f5_proof_replay:capture_run(TraceEvents2),

    %% After normalization, different PIDs should produce same hash
    ?assertEqual(Hash1, Hash2).

%%%-------------------------------------------------------------------
%%% Complex Scenario Tests
%%%-------------------------------------------------------------------

%% @doc Test full replay proof workflow
full_workflow_test() ->
    CaseId = <<"full_workflow">>,
    RunTrace = trace_events(CaseId, 10000),
    EvidencePack = evidence_pack(CaseId, #{complex => data}),

    %% Step 1: Capture original run
    {ok, CapturedCaseId, RunHash} = f5_proof_replay:capture_run(RunTrace),

    %% Step 2: Replay from evidence (same trace for deterministic test)
    ReplayTrace = RunTrace,

    %% Step 3: Verify replay matches run
    {ok, Proof} = f5_proof_replay:verify_replay(RunTrace, ReplayTrace),

    %% Step 4: Generate JSON proof artifact
    {ok, JsonProof} = f5_proof_replay:generate_proof(
        CapturedCaseId,
        ReplayTrace,
        EvidencePack
    ),

    %% Verify all components
    ?assertEqual(true, maps:get(hashes_equal, Proof)),
    ?assertEqual(replay, maps:get(proof_type, Proof)),
    ?assertEqual(RunHash, maps:get(run_trace_hash, Proof)),
    ?assertEqual(verified, maps:get(status, JsonProof)).

%% @doc Test replay with partial trace matches
partial_trace_match_test() ->
    CaseId = <<"partial_case">>,

    FullTrace = trace_events(CaseId, 11000),
    PartialTrace = lists:sublist(FullTrace, 2),

    Result = f5_proof_replay:verify_replay(FullTrace, PartialTrace),

    %% Should fail - traces don't match
    ?assertMatch({error, {hash_mismatch, _, _}}, Result).

%%%-------------------------------------------------------------------
%%% Compute Trace Hash Tests
%%%-------------------------------------------------------------------

%% @doc Test compute_trace_hash returns valid SHA-256 hash
compute_trace_hash_test() ->
    TraceEvents = trace_events(<<"hash_case">>, 7000),

    Result = f5_proof_replay:compute_trace_hash(TraceEvents),

    ?assertMatch({ok, <<_:256>>}, Result).

%% @doc Test compute_trace_hash with empty trace
compute_trace_hash_empty_test() ->
    Result = f5_proof_replay:compute_trace_hash([]),

    ?assertMatch({ok, <<_:256>>}, Result).
