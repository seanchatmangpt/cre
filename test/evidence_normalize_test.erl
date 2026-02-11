%%%-------------------------------------------------------------------
%%% @doc Unit tests for evidence_normalize module
%%%
%%% Tests trace normalization for deterministic hashing including:
%%% - PID remapping to sequential indices
%%% - Port remapping to sequential indices
%%% - Reference normalization to sequential indices
%%% - Timestamp conversion to deltas
%%% - Function canonicalization
%%% - SHA-256 hashing of normalized traces
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(evidence_normalize_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Data Generation
%%====================================================================

%% Create test PIDs with specific process identifiers
test_pid(N) ->
    list_to_pid("<0." ++ integer_to_list(N) ++ ".0>").

%% Create test ports
test_port(N) ->
    list_to_port("#Port<0." ++ integer_to_list(N) ++ ">").

%% Create test references
test_ref(N) ->
    %% Create a unique ref - actual binary varies per runtime
    make_ref().

%%====================================================================
%% normalize_trace/1 Tests
%%====================================================================

normalize_trace_empty_test() ->
    ?assertEqual({ok, []}, evidence_normalize:normalize_trace([])).

normalize_trace_single_event_test() ->
    Trace = [
        #{timestamp => 1000, type => case_started, data => #{}}
    ],
    {ok, [Normalized]} = evidence_normalize:normalize_trace(Trace),
    ?assertEqual(0, maps:get(timestamp, Normalized)),
    ?assertEqual(case_started, maps:get(type, Normalized)).

normalize_trace_timestamp_deltas_test() ->
    Trace = [
        #{timestamp => 1000, type => case_started, data => #{}},
        #{timestamp => 1050, type => step_started, data => #{}},
        #{timestamp => 1200, type => step_completed, data => #{}}
    ],
    {ok, Normalized} = evidence_normalize:normalize_trace(Trace),
    ?assertEqual([0, 50, 200], [maps:get(timestamp, E) || E <- Normalized]).

normalize_trace_with_pids_test() ->
    Pid1 = test_pid(1),
    Pid2 = test_pid(2),
    Trace = [
        #{timestamp => 1000, type => case_started, data => #{pid => Pid1}},
        #{timestamp => 1050, type => step_started, data => #{pid => Pid1, caller => Pid2}}
    ],
    {ok, Normalized} = evidence_normalize:normalize_trace(Trace),
    [First, Second] = Normalized,
    ?assertEqual({pid, 0}, maps:get(pid, maps:get(data, First))),
    ?assertEqual({pid, 0}, maps:get(pid, maps:get(data, Second))),
    ?assertEqual({pid, 1}, maps:get(caller, maps:get(data, Second))).

normalize_trace_preserves_structure_test() ->
    Trace = [
        #{
            timestamp => 1000,
            type => case_started,
            data => #{key1 => val1, nested => #{inner => val2}}
        },
        #{
            timestamp => 1100,
            type => step_completed,
            data => #{result => ok}
        }
    ],
    {ok, Normalized} = evidence_normalize:normalize_trace(Trace),
    ?assertEqual(2, length(Normalized)),
    ?assertEqual(case_started, maps:get(type, lists:nth(1, Normalized))),
    ?assertEqual(step_completed, maps:get(type, lists:nth(2, Normalized))).

%%====================================================================
%% remap_pids/1 Tests
%%====================================================================

remap_pids_empty_test() ->
    ?assertEqual({ok, []}, evidence_normalize:remap_pids([])).

remap_pids_single_pid_test() ->
    Pid = test_pid(5),
    Trace = [
        #{timestamp => 1000, type => event, data => #{pid => Pid}}
    ],
    {ok, [Remapped]} = evidence_normalize:remap_pids(Trace),
    ?assertEqual({pid, 0}, maps:get(pid, maps:get(data, Remapped))).

remap_pids_multiple_pids_test() ->
    Pid1 = test_pid(1),
    Pid2 = test_pid(2),
    Pid3 = test_pid(3),
    Trace = [
        #{timestamp => 1000, type => e1, data => #{pid => Pid2}},
        #{timestamp => 1100, type => e2, data => #{pid => Pid1}},
        #{timestamp => 1200, type => e3, data => #{pid => Pid3, from => Pid2}}
    ],
    {ok, Remapped} = evidence_normalize:remap_pids(Trace),
    ?assertEqual({pid, 0}, maps:get(pid, maps:get(data, lists:nth(1, Remapped)))),
    ?assertEqual({pid, 1}, maps:get(pid, maps:get(data, lists:nth(2, Remapped)))),
    ?assertEqual({pid, 2}, maps:get(pid, maps:get(data, lists:nth(3, Remapped)))),
    ?assertEqual({pid, 0}, maps:get(from, maps:get(data, lists:nth(3, Remapped)))).

remap_pids_nested_data_test() ->
    Pid = test_pid(7),
    Trace = [
        #{
            timestamp => 1000,
            type => event,
            data => #{
                level1 => #{
                    level2 => #{
                        pid => Pid
                    }
                }
            }
        }
    ],
    {ok, [Remapped]} = evidence_normalize:remap_pids(Trace),
    Data = maps:get(data, Remapped),
    L1 = maps:get(level1, Data),
    L2 = maps:get(level2, L1),
    ?assertEqual({pid, 0}, maps:get(pid, L2)).

%%====================================================================
%% strip_timestamps/1 Tests
%%====================================================================

strip_timestamps_empty_test() ->
    ?assertEqual({ok, []}, evidence_normalize:strip_timestamps([])).

strip_timestamps_single_event_test() ->
    Trace = [
        #{timestamp => 5000, type => event, data => #{}}
    ],
    {ok, [Stripped]} = evidence_normalize:strip_timestamps(Trace),
    ?assertEqual(0, maps:get(timestamp, Stripped)).

strip_timestamps_deltas_test() ->
    Trace = [
        #{timestamp => 1000, type => e1, data => #{}},
        #{timestamp => 1500, type => e2, data => #{}},
        #{timestamp => 1600, type => e3, data => #{}},
        #{timestamp => 2000, type => e4, data => #{}}
    ],
    {ok, Stripped} = evidence_normalize:strip_timestamps(Trace),
    ?assertEqual([0, 500, 600, 1000], [maps:get(timestamp, E) || E <- Stripped]).

strip_timestamps_preserves_other_fields_test() ->
    Trace = [
        #{
            timestamp => 1000,
            type => case_started,
            data => #{key => value}
        }
    ],
    {ok, [Stripped]} = evidence_normalize:strip_timestamps(Trace),
    ?assertEqual(case_started, maps:get(type, Stripped)),
    ?assertEqual(#{key => value}, maps:get(data, Stripped)).

%%====================================================================
%% canonicalize_terms/1 Tests
%%====================================================================

canonicalize_terms_simple_test() ->
    ?assertEqual(42, evidence_normalize:canonicalize_terms(42)),
    ?assertEqual(<<"test">>, evidence_normalize:canonicalize_terms(<<"test">>)),
    ?assertEqual(atom, evidence_normalize:canonicalize_terms(atom)),
    ?assertEqual([1, 2, 3], evidence_normalize:canonicalize_terms([1, 2, 3])).

canonicalize_terms_map_test() ->
    Input = #{a => 1, b => #{c => 2}},
    Result = evidence_normalize:canonicalize_terms(Input),
    ?assertEqual(1, maps:get(a, Result)),
    ?assertEqual(2, maps:get(c, maps:get(b, Result))).

canonicalize_terms_tuple_test() ->
    Input = {a, {b, c}, d},
    Result = evidence_normalize:canonicalize_terms(Input),
    ?assertEqual({a, {b, c}, d}, Result).

canonicalize_terms_list_test() ->
    Input = [1, #{nested => 2}, [3, 4]],
    Result = evidence_normalize:canonicalize_terms(Input),
    ?assertEqual([1, #{nested => 2}, [3, 4]], Result).

canonicalize_terms_pid_test() ->
    Pid = test_pid(99),
    Result = evidence_normalize:canonicalize_terms(Pid),
    ?assert(is_list(Result)),
    ?assertEqual("<0.99.0>", Result).

canonicalize_terms_port_test() ->
    Result = evidence_normalize:canonicalize_terms(test_port(5)),
    ?assert(is_list(Result)).

canonicalize_terms_ref_test() ->
    Ref = make_ref(),
    Result = evidence_normalize:canonicalize_terms(Ref),
    ?assert(is_list(Result)).

canonicalize_terms_function_test() ->
    Fun = fun lists:map/2,
    Result = evidence_normalize:canonicalize_terms(Fun),
    ?assertEqual({function, lists, map, 2}, Result).

canonicalize_terms_anon_function_test() ->
    Fun = fun(X) -> X + 1 end,
    Result = evidence_normalize:canonicalize_terms(Fun),
    %% Anonymous functions can be {function, Module, Name, Arity} or {'fun', Arity}
    %% depending on Erlang version and how the fun is created
    ?assert(is_tuple(Result)),
    ?assertEqual(2, tuple_size(Result)),
    ?assertEqual(1, element(2, Result)).

%%====================================================================
%% hash_normalized/1 Tests
%%====================================================================

hash_normalized_empty_test() ->
    ?assertMatch({ok, <<_:256>>}, evidence_normalize:hash_normalized([])).

hash_normalized_deterministic_test() ->
    %% Same content should produce same hash regardless of timestamps
    Pid = test_pid(1),
    Trace1 = [
        #{timestamp => 1000, type => event, data => #{pid => Pid, value => 1}}
    ],
    Trace2 = [
        #{timestamp => 5000, type => event, data => #{pid => Pid, value => 1}}
    ],
    {ok, Hash1} = evidence_normalize:hash_normalized(Trace1),
    {ok, Hash2} = evidence_normalize:hash_normalized(Trace2),
    ?assertEqual(Hash1, Hash2),
    %% Also verify the hash is stable (calling twice on same trace)
    {ok, Hash3} = evidence_normalize:hash_normalized(Trace1),
    ?assertEqual(Hash1, Hash3).

hash_normalized_different_content_test() ->
    Trace1 = [
        #{timestamp => 1000, type => event, data => #{value => 1}}
    ],
    Trace2 = [
        #{timestamp => 1000, type => event, data => #{value => 2}}
    ],
    {ok, Hash1} = evidence_normalize:hash_normalized(Trace1),
    {ok, Hash2} = evidence_normalize:hash_normalized(Trace2),
    ?assertNotEqual(Hash1, Hash2).

hash_normalized_pid_remap_test() ->
    %% Different PIDs at same position should hash identically
    Pid1 = test_pid(10),
    Pid2 = test_pid(20),
    Trace1 = [
        #{timestamp => 1000, type => event, data => #{pid => Pid1}}
    ],
    Trace2 = [
        #{timestamp => 1000, type => event, data => #{pid => Pid2}}
    ],
    {ok, Hash1} = evidence_normalize:hash_normalized(Trace1),
    {ok, Hash2} = evidence_normalize:hash_normalized(Trace2),
    ?assertEqual(Hash1, Hash2).

hash_normalized_complex_trace_test() ->
    %% Test that complex traces normalize consistently
    Pid1 = test_pid(1),
    Pid2 = test_pid(2),
    Trace = [
        #{
            timestamp => 1000,
            type => case_started,
            data => #{
                case_id => <<"case123">>,
                pid => Pid1,
                nested => #{key => value}
            }
        },
        #{
            timestamp => 1100,
            type => step_started,
            data => #{
                pid => Pid2,
                parent => Pid1
            }
        }
    ],
    ?assertMatch({ok, <<_:256>>}, evidence_normalize:hash_normalized(Trace)).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_full_normalization_test() ->
    %% Test complete normalization pipeline
    Pid1 = test_pid(100),
    Pid2 = test_pid(200),
    Trace = [
        #{
            timestamp => 5000,
            type => case_started,
            data => #{
                case_id => abc123,
                pid => Pid1,
                config => #{key => val}
            }
        },
        #{
            timestamp => 5100,
            type => step_started,
            data => #{
                pid => Pid2,
                parent => Pid1,
                step => step1
            }
        },
        #{
            timestamp => 5200,
            type => step_completed,
            data => #{
                pid => Pid2,
                result => ok
            }
        }
    ],
    {ok, Hash} = evidence_normalize:hash_normalized(Trace),
    %% Verify hash is stable binary
    ?assert(is_binary(Hash)),
    ?assertEqual(32, byte_size(Hash)).

integration_replay_determinism_test() ->
    %% Simulate two executions with different timestamps/PIDs but same logical flow
    Trace1 = [
        #{timestamp => 1000, type => e1, data => #{pid => test_pid(1), x => 1}},
        #{timestamp => 1050, type => e2, data => #{pid => test_pid(2), x => 2}}
    ],
    Trace2 = [
        #{timestamp => 9999, type => e1, data => #{pid => test_pid(99), x => 1}},
        #{timestamp => 10049, type => e2, data => #{pid => test_pid(88), x => 2}}
    ],
    {ok, Hash1} = evidence_normalize:hash_normalized(Trace1),
    {ok, Hash2} = evidence_normalize:hash_normalized(Trace2),
    ?assertEqual(Hash1, Hash2).

integration_different_flows_different_hashes_test() ->
    Trace1 = [
        #{timestamp => 1000, type => e1, data => #{value => 1}},
        #{timestamp => 1100, type => e2, data => #{value => 2}}
    ],
    Trace2 = [
        #{timestamp => 1000, type => e1, data => #{value => 1}},
        #{timestamp => 1100, type => e3, data => #{value => 2}}
    ],
    {ok, Hash1} = evidence_normalize:hash_normalized(Trace1),
    {ok, Hash2} = evidence_normalize:hash_normalized(Trace2),
    ?assertNotEqual(Hash1, Hash2).
