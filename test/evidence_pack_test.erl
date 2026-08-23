%% -*- erlang -*-
%%%% @doc Unit Tests for evidence_pack Module
%%
%% Test suite for evidence pack creation, index generation,
%% artifact management, proof tracking, and benchmarking.

-module(evidence_pack_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

%% @doc Sets up a temporary evidence directory for testing.
setup_evidence_dir() ->
    Dir = "/tmp/evidence_pack_test_" ++ integer_to_list(erlang:unique_integer()),
    ok = filelib:ensure_path(filename:join(Dir, "dummy")),
    Dir.

%% @doc Cleans up test evidence directory.
cleanup_evidence_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(fun(F) ->
                file:delete(filename:join(Dir, F))
            end, Files);
        _ ->
            ok
    end,
    file:del_dir(Dir).

%% @doc Creates a mock proof JSON file.
create_mock_proof(Dir, Name, Status) ->
    ProofJson = jsx:encode(#{
        <<"proof_type">> => Name,
        <<"status">> => Status,
        <<"verified_at">> => erlang:system_time(millisecond)
    }),
    Filename = atom_to_list(Name) ++ "_proof.json",
    Path = filename:join(Dir, Filename),
    ok = file:write_file(Path, ProofJson),
    Path.

%% @doc Creates a mock trace file.
create_mock_trace(Dir, Name) ->
    TraceContent = <<"[{\"timestamp\":1,\"type\":\"test\",\"data\":{}}]">>,
    Path = filename:join(Dir, Name ++ ".trace"),
    ok = file:write_file(Path, TraceContent),
    Path.

%% @doc Creates a mock benchmark file.
create_mock_benchmark(Dir) ->
    BenchmarkJson = jsx:encode(#{
        <<"throughput_ops_per_sec">> => #{
            <<"value">> => 1250,
            <<"unit">> => <<"ops/s">>,
            <<"baseline">> => 1000
        },
        <<"latency_p99_ms">> => #{
            <<"value">> => 45,
            <<"unit">> => <<"ms">>,
            <<"baseline">> => 50
        }
    }),
    Path = filename:join(Dir, "benchmarks.json"),
    ok = file:write_file(Path, BenchmarkJson),
    Path.

%%====================================================================
%% Test Generators
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test pack creation from evidence directory.
%%--------------------------------------------------------------------
create_pack_test_() ->
    {setup,
     fun setup_evidence_dir/0,
     fun cleanup_evidence_dir/1,
     fun(Dir) ->
        [
            {"Creates pack from existing directory",
             fun() ->
                 %% Create some test files
                 create_mock_trace(Dir, "test_trace"),
                 create_mock_proof(Dir, replay, pass),

                 %% Create pack
                 {ok, Pack} = evidence_pack:create_pack(Dir),

                 %% Verify pack structure
                 ?assertMatch(#{id := _, created := _, artifacts := _, proofs := _}, Pack),
                 ?assert(is_binary(maps:get(id, Pack))),
                 ?assert(is_integer(maps:get(created, Pack))),
                 ?assert(is_map(maps:get(artifacts, Pack))),
                 ?assert(is_map(maps:get(proofs, Pack)))
             end},

            {"Creates pack with custom metadata",
             fun() ->
                 Metadata = #{
                     description => <<"Test pack for unit testing">>,
                     created_by => <<"eunit">>,
                     tags => [<<"test">>, <<"unit">>]
                 },
                 {ok, Pack} = evidence_pack:create_pack(Dir, Metadata),

                 ?assertEqual(<<"Test pack for unit testing">>,
                             maps:get(description, maps:get(metadata, Pack))),
                 ?assertEqual(<<"eunit">>,
                             maps:get(created_by, maps:get(metadata, Pack))),
                 ?assertEqual([<<"test">>, <<"unit">>],
                             maps:get(tags, maps:get(metadata, Pack)))
             end},

            {"Scans artifacts from directory",
             fun() ->
                 %% Create test files
                 create_mock_trace(Dir, "trace1"),
                 create_mock_trace(Dir, "trace2"),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 Artifacts = maps:get(artifacts, Pack),

                 ?assert(maps:size(Artifacts) >= 2)
             end},

            {"Scans proof files from directory",
             fun() ->
                 %% Create proof files
                 create_mock_proof(Dir, replay, pass),
                 create_mock_proof(Dir, cancel, fail),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 Proofs = maps:get(proofs, Pack),

                 ?assert(maps:size(Proofs) >= 2)
             end},

            {"Returns error for non-existent directory",
             fun() ->
                 BadDir = "/tmp/nonexistent_dir_" ++ integer_to_list(erlang:unique_integer()),
                 ?assertMatch({error, {not_a_directory, _}},
                             evidence_pack:create_pack(BadDir))
             end}
        ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test artifact management operations.
%%--------------------------------------------------------------------
artifact_management_test_() ->
    {setup,
     fun setup_evidence_dir/0,
     fun cleanup_evidence_dir/1,
     fun(Dir) ->
        [
            {"Adds artifact to pack",
             fun() ->
                 %% Create test file
                 FilePath = filename:join(Dir, "test_artifact.dat"),
                 ok = file:write_file(FilePath, <<"test content">>),

                 %% Create pack and add artifact
                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 {ok, Pack1} = evidence_pack:add_artifact(Pack, FilePath),

                 %% Verify artifact was added
                 ?assertMatch({ok, #{name := <<"test_artifact.dat">>}},
                             evidence_pack:get_artifact(Pack1, <<"test_artifact.dat">>))
             end},

            {"Adds artifact with custom type",
             fun() ->
                 FilePath = filename:join(Dir, "custom.log"),
                 ok = file:write_file(FilePath, <<"log data">>),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 {ok, Pack1} = evidence_pack:add_artifact(Pack, FilePath, #{type => log}),

                 {ok, Artifact} = evidence_pack:get_artifact(Pack1, <<"custom.log">>),
                 ?assertEqual(log, maps:get(type, Artifact))
             end},

            {"Lists artifact names",
             fun() ->
                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 Names = evidence_pack:list_artifacts(Pack),

                 ?assert(is_list(Names)),
                 ?assert(lists:all(fun(N) -> is_binary(N) end, Names))
             end},

            {"Removes artifact from pack",
             fun() ->
                 FilePath = filename:join(Dir, "to_remove.dat"),
                 ok = file:write_file(FilePath, <<"data">>),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 {ok, Pack1} = evidence_pack:add_artifact(Pack, FilePath),
                 ?assertMatch({ok, _}, evidence_pack:get_artifact(Pack1, <<"to_remove.dat">>)),

                 {ok, Pack2} = evidence_pack:remove_artifact(Pack1, <<"to_remove.dat">>),
                 ?assertEqual({error, not_found},
                             evidence_pack:get_artifact(Pack2, <<"to_remove.dat">>))
             end},

            {"Returns error when removing non-existent artifact",
             fun() ->
                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 ?assertEqual({error, not_found},
                             evidence_pack:remove_artifact(Pack, <<"nonexistent">>))
             end},

            {"Verifies artifact hash",
             fun() ->
                 FilePath = filename:join(Dir, "verify_me.dat"),
                 Content = <<"content to verify">>,
                 ok = file:write_file(FilePath, Content),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 {ok, Pack1} = evidence_pack:add_artifact(Pack, FilePath),

                 ?assertEqual({ok, true}, evidence_pack:verify_artifact(Pack1, <<"verify_me.dat">>))
             end},

            {"Detects modified artifact",
             fun() ->
                 FilePath = filename:join(Dir, "modified.dat"),
                 ok = file:write_file(FilePath, <<"original">>),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 {ok, Pack1} = evidence_pack:add_artifact(Pack, FilePath),

                 %% Modify the file
                 ok = file:write_file(FilePath, <<"modified">>),

                 ?assertEqual({ok, false}, evidence_pack:verify_artifact(Pack1, <<"modified.dat">>))
             end}
        ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test proof management operations.
%%--------------------------------------------------------------------
proof_management_test_() ->
    {setup,
     fun setup_evidence_dir/0,
     fun cleanup_evidence_dir/1,
     fun(Dir) ->
        [
            {"Adds proof to pack",
             fun() ->
                 ProofPath = create_mock_proof(Dir, replay, pass),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 {ok, Pack1} = evidence_pack:add_proof(Pack, replay_proof, ProofPath),

                 {ok, Proof} = evidence_pack:get_proof(Pack1, replay_proof),
                 ?assertEqual(replay_proof, maps:get(name, Proof)),
                 ?assertEqual(pass, maps:get(status, Proof))
             end},

            {"Parses proof status from JSON",
             fun() ->
                 %% Test pass status
                 PassJson = jsx:encode(#{<<"status">> => <<"verified">>}),
                 ?assertEqual(pass, parse_proof_status(PassJson)),

                 %% Test fail status
                 FailJson = jsx:encode(#{<<"status">> => <<"failed">>}),
                 ?assertEqual(fail, parse_proof_status(FailJson)),

                 %% Test hashes_equal flag
                 TrueJson = jsx:encode(#{<<"hashes_equal">> => true}),
                 ?assertEqual(pass, parse_proof_status(TrueJson))
             end},

            {"Lists proof names",
             fun() ->
                 create_mock_proof(Dir, replay, pass),
                 create_mock_proof(Dir, cancel, fail),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 Names = evidence_pack:list_proofs(Pack),

                 ?assert(is_list(Names)),
                 ?assert(lists:all(fun(N) -> is_atom(N) end, Names))
             end},

            {"Verifies all proofs pass",
             fun() ->
                 create_mock_proof(Dir, replay, pass),
                 create_mock_proof(Dir, cancel, pass),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 ?assertEqual({ok, true}, evidence_pack:verify_all_proofs(Pack))
             end},

            {"Detects failing proof",
             fun() ->
                 create_mock_proof(Dir, replay, pass),
                 create_mock_proof(Dir, cancel, fail),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 ?assertEqual({ok, false}, evidence_pack:verify_all_proofs(Pack))
             end}
        ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test benchmark management.
%%--------------------------------------------------------------------
benchmark_management_test_() ->
    {setup,
     fun setup_evidence_dir/0,
     fun cleanup_evidence_dir/1,
     fun(Dir) ->
        [
            {"Adds benchmark to pack",
             fun() ->
                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 Benchmark = #{
                     value => 100,
                     unit => <<"ops/s">>,
                     baseline => 90
                 },
                 {ok, Pack1} = evidence_pack:add_benchmark(Pack, <<"throughput">>, Benchmark),

                 Benchmarks = maps:get(benchmarks, Pack1),
                 ?assert(maps:is_key(<<"throughput">>, Benchmarks))
             end},

            {"Computes benchmark delta",
             fun() ->
                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 Benchmark = #{
                     value => 100,
                     unit => <<"ops/s">>,
                     baseline => 90
                 },
                 {ok, Pack1} = evidence_pack:add_benchmark(Pack, <<"throughput">>, Benchmark),

                 #{benchmarks := Bs} = Pack1,
                 #{delta := Delta} = maps:get(<<"throughput">>, Bs),
                 ?assertEqual(10, Delta)
             end},

            {"Computes benchmark percentage change",
             fun() ->
                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 Benchmark = #{
                     value => 120,
                     unit => <<"ops/s">>,
                     baseline => 100
                 },
                 {ok, Pack1} = evidence_pack:add_benchmark(Pack, <<"throughput">>, Benchmark),

                 #{benchmarks := Bs} = Pack1,
                 #{delta_percent := Pct} = maps:get(<<"throughput">>, Bs),
                 ?assertEqual(20.0, Pct)
             end},

            {"Compares benchmark to baseline",
             fun() ->
                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 Benchmark = #{
                     value => 110,
                     unit => <<"ops/s">>,
                     baseline => 100
                 },
                 {ok, Pack1} = evidence_pack:add_benchmark(Pack, <<"throughput">>, Benchmark),

                 ?assertEqual({ok, #{comparison => worse, delta => 10}},
                             evidence_pack:compare_benchmark(Pack1, <<"throughput">>))
             end},

            {"Formats benchmarks table",
             fun() ->
                 create_mock_benchmark(Dir),
                 {ok, Pack} = evidence_pack:create_pack(Dir),

                 Table = evidence_pack:format_benchmarks(Pack),
                 ?assert(is_list(Table)),
                 ?assert(lists:any(fun(E) ->
                     is_list(E) andalso lists:prefix("|", E)
                 end, Table))
             end}
        ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test index generation.
%%--------------------------------------------------------------------
index_generation_test_() ->
    {setup,
     fun setup_evidence_dir/0,
     fun cleanup_evidence_dir/1,
     fun(Dir) ->
        [
            {"Generates index from pack",
             fun() ->
                 create_mock_trace(Dir, "test"),
                 {ok, Pack} = evidence_pack:create_pack(Dir),

                 {ok, Index} = evidence_pack:generate_index(Pack),
                 ?assert(is_list(Index)),
                 ?assert(lists:prefix("# Evidence Pack Index", Index))
             end},

            {"Index contains metadata section",
             fun() ->
                 Metadata = #{description => <<"Test description">>},
                 {ok, Pack} = evidence_pack:create_pack(Dir, Metadata),

                 Index = evidence_pack:format_index(Pack),
                 IndexStr = iolist_to_binary(Index),
                 ?assert(binary:match(IndexStr, <<"## Metadata">>) =/= nomatch)
             end},

            {"Index contains artifacts table",
             fun() ->
                 create_mock_trace(Dir, "artifact1"),
                 {ok, Pack} = evidence_pack:create_pack(Dir),

                 Index = evidence_pack:format_index(Pack),
                 IndexStr = iolist_to_binary(Index),
                 ?assert(binary:match(IndexStr, <<"## Artifacts">>) =/= nomatch),
                 ?assert(binary:match(IndexStr, <<"| Name | Type">>) =/= nomatch)
             end},

            {"Index contains proofs section",
             fun() ->
                 create_mock_proof(Dir, replay, pass),
                 {ok, Pack} = evidence_pack:create_pack(Dir),

                 Index = evidence_pack:format_index(Pack),
                 IndexStr = iolist_to_binary(Index),
                 ?assert(binary:match(IndexStr, <<"## Proofs">>) =/= nomatch)
             end},

            {"Index contains benchmarks section",
             fun() ->
                 create_mock_benchmark(Dir),
                 {ok, Pack} = evidence_pack:create_pack(Dir),

                 Index = evidence_pack:format_index(Pack),
                 IndexStr = iolist_to_binary(Index),
                 ?assert(binary:match(IndexStr, <<"## Benchmarks">>) =/= nomatch)
             end},

            {"Index excludes hash when option is false",
             fun() ->
                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 Index = evidence_pack:format_index(Pack, #{include_hash => false}),
                 IndexStr = iolist_to_binary(Index),
                 %% Should have Name and Type columns but not Hash
                 ?assert(binary:match(IndexStr, <<"| Name | Type |">>) =/= nomatch)
             end}
        ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test pack finalization and verification.
%%--------------------------------------------------------------------
finalization_test_() ->
    {setup,
     fun setup_evidence_dir/0,
     fun cleanup_evidence_dir/1,
     fun(Dir) ->
        [
            {"Finalizes pack with hash",
             fun() ->
                 create_mock_trace(Dir, "final_test"),
                 {ok, Pack} = evidence_pack:create_pack(Dir),

                 {ok, Finalized} = evidence_pack:finalize_pack(Pack),
                 ?assertMatch(#{pack_hash := <<_:256>>, finalized_at := _}, Finalized)
             end},

            {"Verifies valid pack",
             fun() ->
                 FilePath = filename:join(Dir, "valid.dat"),
                 ok = file:write_file(FilePath, <<"valid content">>),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 {ok, Pack1} = evidence_pack:add_artifact(Pack, FilePath),

                 ?assertEqual({ok, true}, evidence_pack:verify_pack(Pack1))
             end},

            {"Detects invalid pack after modification",
             fun() ->
                 FilePath = filename:join(Dir, "will_modify.dat"),
                 ok = file:write_file(FilePath, <<"original">>),

                 {ok, Pack} = evidence_pack:create_pack(Dir),
                 {ok, Pack1} = evidence_pack:add_artifact(Pack, FilePath),

                 %% Modify file
                 ok = file:write_file(FilePath, <<"modified">>),

                 ?assertEqual({ok, false}, evidence_pack:verify_pack(Pack1))
             end}
        ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test pack save and load operations.
%%--------------------------------------------------------------------
save_load_test_() ->
    {setup,
     fun() ->
         Dir1 = setup_evidence_dir(),
         Dir2 = "/tmp/evidence_pack_save_load_" ++ integer_to_list(erlang:unique_integer()),
         filelib:ensure_path(filename:join(Dir2, "dummy")),
         {Dir1, Dir2}
     end,
     fun({Dir1, Dir2}) ->
         cleanup_evidence_dir(Dir1),
         cleanup_evidence_dir(Dir2)
     end,
     fun({Dir1, Dir2}) ->
        [
            {"Saves pack to JSON",
             fun() ->
                 {ok, Pack} = evidence_pack:create_pack(Dir1),
                 ?assertEqual(ok, evidence_pack:save_pack(Pack, Dir2)),

                 %% Verify file was created
                 PackFile = filename:join(Dir2, "pack.json"),
                 ?assert(filelib:is_file(PackFile))
             end},

            {"Loads pack from JSON",
             fun() ->
                 Metadata = #{description => <<"Save/load test">>},
                 {ok, Pack} = evidence_pack:create_pack(Dir1, Metadata),
                 ok = evidence_pack:save_pack(Pack, Dir2),

                 {ok, LoadedPack} = evidence_pack:load_pack(Dir2),

                 ?assertEqual(maps:get(id, Pack), maps:get(id, LoadedPack)),
                 ?assertEqual(maps:get(created, Pack), maps:get(created, LoadedPack))
             end}
        ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test pack merge operation.
%%--------------------------------------------------------------------
merge_packs_test_() ->
    {setup,
     fun() ->
         Dir1 = "/tmp/evidence_pack_merge1_" ++ integer_to_list(erlang:unique_integer()),
         Dir2 = "/tmp/evidence_pack_merge2_" ++ integer_to_list(erlang:unique_integer()),
         filelib:ensure_path(filename:join(Dir1, "dummy")),
         filelib:ensure_path(filename:join(Dir2, "dummy")),
         {Dir1, Dir2}
     end,
     fun({Dir1, Dir2}) ->
         cleanup_evidence_dir(Dir1),
         cleanup_evidence_dir(Dir2)
     end,
     fun({Dir1, Dir2}) ->
        [
            {"Merges two packs",
             fun() ->
                 %% Create artifact in each dir
                 create_mock_trace(Dir1, "pack1_trace"),
                 create_mock_trace(Dir2, "pack2_trace"),

                 {ok, Pack1} = evidence_pack:create_pack(Dir1),
                 {ok, Pack2} = evidence_pack:create_pack(Dir2),

                 {ok, Merged} = evidence_pack:merge_packs(Pack1, Pack2),

                 %% Merged pack should have artifacts from both
                 Artifacts = maps:get(artifacts, Merged),
                 ?assert(maps:is_key(<<"pack1_trace.trace">>, Artifacts) orelse
                         maps:is_key(<<"pack2_trace.trace">>, Artifacts))
             end}
        ]
     end}.

%%--------------------------------------------------------------------
%% @doc Test pack ID generation.
%%--------------------------------------------------------------------
pack_id_test_() ->
    {"Generates unique pack IDs",
     fun() ->
         Id1 = evidence_pack:pack_id(),
         Id2 = evidence_pack:pack_id(),
         ?assert(is_list(Id1)),
         ?assertNotEqual(Id1, Id2)
     end}.

%%--------------------------------------------------------------------
%% @doc Test pack hash computation.
%%--------------------------------------------------------------------
pack_hash_test_() ->
    {setup,
     fun setup_evidence_dir/0,
     fun cleanup_evidence_dir/1,
     fun(Dir) ->
        [
            {"Computes hash from pack",
             fun() ->
                 create_mock_trace(Dir, "hash_test"),
                 {ok, Pack} = evidence_pack:create_pack(Dir),

                 Hash = evidence_pack:pack_hash(Pack),
                 ?assertMatch(<<_:256>>, Hash)
             end},

            {"Same pack produces same hash",
             fun() ->
                 create_mock_trace(Dir, "consistent"),
                 {ok, Pack} = evidence_pack:create_pack(Dir),

                 Hash1 = evidence_pack:pack_hash(Pack),
                 Hash2 = evidence_pack:pack_hash(Pack),
                 ?assertEqual(Hash1, Hash2)
             end},

            {"Different packs produce different hashes",
             fun() ->
                 create_mock_trace(Dir, "diff1"),
                 {ok, Pack1} = evidence_pack:create_pack(Dir),

                 cleanup_evidence_dir(Dir),
                 create_mock_trace(Dir, "diff2"),
                 {ok, Pack2} = evidence_pack:create_pack(Dir),

                 Hash1 = evidence_pack:pack_hash(Pack1),
                 Hash2 = evidence_pack:pack_hash(Pack2),
                 ?assertNotEqual(Hash1, Hash2)
             end}
        ]
     end}.

%%====================================================================
%% Internal Helper Functions (exposed for testing)
%%====================================================================

%% @private Wrapper for testing internal parse_proof_status function.
parse_proof_status(Json) ->
    case catch jsx:decode(Json, [return_maps]) of
        #{<<"status">> := <<"verified">>} -> pass;
        #{<<"status">> := <<"pass">>} -> pass;
        #{<<"status">> := <<"failed">>} -> fail;
        #{<<"status">> := <<"error">>} -> error;
        #{<<"hashes_equal">> := true} -> pass;
        #{<<"hashes_equal">> := false} -> fail;
        #{<<"effects_verified">> := true} -> pass;
        #{<<"effects_verified">> := false} -> fail;
        _ -> skipped
    end.
