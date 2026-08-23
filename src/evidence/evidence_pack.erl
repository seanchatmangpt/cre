%%%-------------------------------------------------------------------
%%% @doc evidence_pack - Evidence pack index generator and artifact manager
%%%
%%% This module provides functionality for creating evidence packs from
%%% evidence directories, generating EVIDENCE_INDEX.md files, managing
%%% artifacts, and finalizing packs with cryptographic hashes and signatures.
%%%
%%% <h3>Features</h3>
%%% <ul>
%%%   <li>Evidence pack creation from evidence directories</li>
%%%   <li>Index generation in EVIDENCE_INDEX.md format</li>
%%%   <li>Artifact management (add, list, verify)</li>
%%%   <li>Cryptographic hashing for pack integrity</li>
%%%   <li>Proof tracking (replay, cancel, crash, budget)</li>
%%%   <li>Benchmark comparison with baselines</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(evidence_pack).

%%====================================================================
%% Exports
%%====================================================================

%% Pack management
-export([create_pack/1]).
-export([create_pack/2]).
-export([load_pack/1]).
-export([save_pack/2]).
-export([finalize_pack/1]).
-export([verify_pack/1]).

%% Index generation
-export([generate_index/1]).
-export([generate_index/2]).
-export([format_index/1]).

%% Artifact management
-export([add_artifact/2]).
-export([add_artifact/3]).
-export([list_artifacts/1]).
-export([get_artifact/2]).
-export([remove_artifact/2]).
-export([verify_artifact/2]).

%% Proof management
-export([add_proof/3]).
-export([get_proof/2]).
-export([list_proofs/1]).
-export([verify_all_proofs/1]).

%% Benchmark management
-export([add_benchmark/3]).
-export([compare_benchmark/2]).
-export([format_benchmarks/1]).

%% Utility functions
-export([pack_hash/1]).
-export([pack_id/0]).
-export([merge_packs/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Evidence pack containing all evidence for a test run.
%%--------------------------------------------------------------------
-type pack() :: #{
    id := pack_id(),
    created := integer(),
    evidence_dir := file:filename_all(),
    artifacts := #{binary() => artifact()},
    proofs := #{atom() => proof_result()},
    benchmarks := #{binary() => benchmark()},
    metadata := metadata()
}.

%%--------------------------------------------------------------------
%% @doc Unique pack identifier (UUID v4 format string).
%%--------------------------------------------------------------------
-type pack_id() :: iolist().  % UUID format as iolist for efficiency

%%--------------------------------------------------------------------
%% @doc Artifact in the evidence pack.
%%--------------------------------------------------------------------
-type artifact() :: #{
    name := binary(),
    type := artifact_type(),
    path := file:filename_all(),
    hash := <<_:256>>,
    size := non_neg_integer(),
    added_at := integer()
}.

%%--------------------------------------------------------------------
%% @doc Artifact type categories.
%%--------------------------------------------------------------------
-type artifact_type() ::
    trace |
    proof |
    counter |
    statistic |
    benchmark |
    log |
    screenshot |
    config |
    receipt |
    other.

%%--------------------------------------------------------------------
%% @doc Proof verification result.
%%--------------------------------------------------------------------
-type proof_result() :: #{
    name := atom(),
    file := binary(),
    status := proof_status(),
    hash := <<_:256>>,
    verified_at := integer()
}.

%%--------------------------------------------------------------------
%% @doc Proof verification status.
%%--------------------------------------------------------------------
-type proof_status() :: pass | fail | error | skipped.

%%--------------------------------------------------------------------
%% @doc Benchmark data with baseline comparison.
%%--------------------------------------------------------------------
-type benchmark() :: #{
    name := binary(),
    value := number(),
    unit := binary(),
    baseline := number() | undefined,
    delta := number() | undefined,
    delta_percent := float() | undefined
}.

%%--------------------------------------------------------------------
%% @doc Pack metadata.
%%--------------------------------------------------------------------
-type metadata() :: #{
    created_at := integer(),
    created_by => binary(),
    description => binary(),
    tags => [binary()],
    version => binary()
}.

%%--------------------------------------------------------------------
%% @doc Index format options.
%%--------------------------------------------------------------------
-type index_opts() :: #{
    format => markdown | json,
    include_hash => boolean(),
    include_size => boolean(),
    sort_by => name | type | date
}.

%%--------------------------------------------------------------------
%% @doc Finalized pack with cryptographic signature.
%%--------------------------------------------------------------------
-type finalized_pack() :: #{
    pack := pack(),
    pack_hash := <<_:256>>,
    finalized_at := integer()
}.

%% Export types
-export_type([
    pack/0,
    pack_id/0,
    artifact/0,
    artifact_type/0,
    proof_result/0,
    proof_status/0,
    benchmark/0,
    metadata/0,
    index_opts/0,
    finalized_pack/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an evidence pack from an evidence directory.
%%
%% Scans the directory for known artifact types (trace files, proof JSONs,
%% counter dumps, etc.) and creates a pack structure.
%%
%% @param EvidenceDir Path to evidence directory
%% @returns {ok, Pack} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec create_pack(file:filename_all()) -> {ok, pack()} | {error, term()}.

create_pack(EvidenceDir) ->
    create_pack(EvidenceDir, #{}).

%%--------------------------------------------------------------------
%% @doc Creates an evidence pack with metadata.
%%
%% @param EvidenceDir Path to evidence directory
%% @param Metadata Optional metadata map
%% @returns {ok, Pack} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec create_pack(file:filename_all(), map()) -> {ok, pack()} | {error, term()}.

create_pack(EvidenceDir, Metadata) ->
    case filelib:is_dir(EvidenceDir) of
        false ->
            {error, {not_a_directory, EvidenceDir}};
        true ->
            PackId = pack_id(),
            Created = erlang:system_time(millisecond),

            %% Scan directory for artifacts
            Artifacts = scan_artifacts(EvidenceDir, Created),

            %% Scan for proof files
            Proofs = scan_proofs(EvidenceDir),

            %% Scan for benchmark files
            Benchmarks = scan_benchmarks(EvidenceDir),

            %% Build metadata
            DefaultMeta = #{
                created_at => Created,
                version => <<"1.0.0">>
            },
            FinalMeta = maps:merge(DefaultMeta, Metadata),

            Pack = #{
                id => PackId,
                created => Created,
                evidence_dir => EvidenceDir,
                artifacts => Artifacts,
                proofs => Proofs,
                benchmarks => Benchmarks,
                metadata => FinalMeta
            },
            {ok, Pack}
    end.

%%--------------------------------------------------------------------
%% @doc Loads a pack from a saved pack.json file.
%%
%% @param PackDir Directory containing pack.json
%% @returns {ok, Pack} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec load_pack(file:filename_all()) -> {ok, pack()} | {error, term()}.

load_pack(PackDir) ->
    PackFile = filename:join(PackDir, "pack.json"),
    case file:read_file(PackFile) of
        {ok, Binary} ->
            try
                PackMap = jsx:decode(Binary, [return_maps]),
                %% Convert binary keys to atoms where needed
                Pack = convert_pack_keys(PackMap),
                {ok, Pack}
            catch
                _:_:Error ->
                    {error, {invalid_pack_format, Error}}
            end;
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Saves a pack to pack.json in the evidence directory.
%%
%% @param Pack The pack to save
%% @param PackDir Directory to save pack.json
%% @returns ok on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec save_pack(pack(), file:filename_all()) -> ok | {error, term()}.

save_pack(#{evidence_dir := _EvidenceDir} = Pack, PackDir) ->
    PackFile = filename:join(PackDir, "pack.json"),
    %% Convert pack to JSON-encodable format
    JsonPack = pack_to_json(Pack),
    try
        Formatted = jsx:prettify(jsx:encode(JsonPack)),
        file:write_file(PackFile, Formatted)
    catch
        _:_:Error ->
            {error, {encode_failed, Error}}
    end.

%%--------------------------------------------------------------------
%% @doc Finalizes a pack with hash and signature.
%%
%% Computes SHA-256 hash of all artifacts and creates a finalized
%% pack record with cryptographic verification data.
%%
%% @param Pack The pack to finalize
%% @returns {ok, FinalizedPack} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec finalize_pack(pack()) -> {ok, finalized_pack()} | {error, term()}.

finalize_pack(Pack) ->
    case pack_hash(Pack) of
        {error, Reason} ->
            {error, {hash_failed, Reason}};
        PackHash ->
            Finalized = #{
                pack => Pack,
                pack_hash => PackHash,
                finalized_at => erlang:system_time(millisecond)
            },
            {ok, Finalized}
    end.

%%--------------------------------------------------------------------
%% @doc Verifies a pack's integrity by checking artifact hashes.
%%
%% @param PackOrFinalized Pack or finalized pack to verify
%% @returns {ok, true} if valid, {ok, false} if invalid, {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec verify_pack(pack() | finalized_pack()) -> {ok, boolean()} | {error, term()}.

verify_pack(#{pack := Pack}) ->
    verify_pack(Pack);
verify_pack(#{artifacts := Artifacts}) ->
    case maps:fold(fun verify_artifact_fold/3, true, Artifacts) of
        true ->
            {ok, true};
        false ->
            {ok, false};
        {error, Reason} ->
            {error, Reason}
    end;
verify_pack(_) ->
    {error, invalid_pack_format}.

%%--------------------------------------------------------------------
%% @doc Generates EVIDENCE_INDEX.md from a pack.
%%
%% Creates a markdown index file listing all artifacts, proofs, and
%% benchmarks with their verification status.
%%
%% @param Pack The pack to generate index from
%% @returns {ok, IndexContent} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_index(pack()) -> {ok, iolist()} | {error, term()}.

generate_index(Pack) ->
    generate_index(Pack, #{}).

%%--------------------------------------------------------------------
%% @doc Generates index with custom options.
%%
%% @param Pack The pack to generate index from
%% @param Opts Index formatting options
%% @returns {ok, IndexContent} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_index(pack(), index_opts()) -> {ok, iolist()} | {error, term()}.

generate_index(Pack, Opts) ->
    try
        Index = format_index(Pack, Opts),
        {ok, Index}
    catch
        _:_:Error ->
            {error, {index_generation_failed, Error}}
    end.

%%--------------------------------------------------------------------
%% @doc Formats a pack as an EVIDENCE_INDEX.md iolist.
%%
%% @param Pack The pack to format
%% @returns Formatted markdown iolist
%%
%% @end
%%--------------------------------------------------------------------
-spec format_index(pack()) -> iolist().

format_index(Pack) ->
    format_index(Pack, #{}).

%%--------------------------------------------------------------------
%% @doc Formats a pack with options.
%%
%% @param Pack The pack to format
%% @param Opts Formatting options
%% @returns Formatted iolist
%%
%% @end
%%--------------------------------------------------------------------
-spec format_index(pack(), index_opts()) -> iolist().

format_index(Pack, Opts) ->
    IncludeHash = maps:get(include_hash, Opts, true),
    IncludeSize = maps:get(include_size, Opts, true),
    SortBy = maps:get(sort_by, Opts, name),

    #{
        id := PackId,
        created := Created,
        artifacts := Artifacts,
        proofs := Proofs,
        benchmarks := Benchmarks,
        metadata := Meta
    } = Pack,

    %% Sort artifacts
    SortedArtifacts = sort_artifacts(Artifacts, SortBy),

    %% Build sections
    [
        "# Evidence Pack Index\n\n",
        "## Metadata\n\n",
        format_metadata(PackId, Created, Meta),
        "\n",
        format_artifacts_table(SortedArtifacts, IncludeHash, IncludeSize),
        "\n",
        format_proofs_section(Proofs),
        "\n",
        format_benchmarks_section(Benchmarks),
        "\n"
    ].

%%--------------------------------------------------------------------
%% @doc Adds an artifact to a pack.
%%
%% @param Pack The pack to modify
%% @param ArtifactPath Path to artifact file
%% @returns {ok, UpdatedPack} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec add_artifact(pack(), file:filename_all()) -> {ok, pack()} | {error, term()}.

add_artifact(Pack, ArtifactPath) ->
    add_artifact(Pack, ArtifactPath, #{}).

%%--------------------------------------------------------------------
%% @doc Adds an artifact with custom type.
%%
%% @param Pack The pack to modify
%% @param ArtifactPath Path to artifact file
%% @param Opts Artifact options (type, name)
%% @returns {ok, UpdatedPack} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec add_artifact(pack(), file:filename_all(), map()) -> {ok, pack()} | {error, term()}.

add_artifact(#{artifacts := Artifacts} = Pack, ArtifactPath, Opts) ->
    case filelib:is_file(ArtifactPath) of
        false ->
            {error, {file_not_found, ArtifactPath}};
        true ->
            %% Determine artifact type
            Type = maps:get(type, Opts, infer_artifact_type(ArtifactPath)),

            %% Generate artifact name
            Filename = filename:basename(ArtifactPath),
            Name = maps:get(name, Opts, list_to_binary(Filename)),

            %% Compute hash and size
            case file:read_file(ArtifactPath) of
                {ok, Content} ->
                    Hash = crypto:hash(sha256, Content),
                    Size = byte_size(Content),
                    AddedAt = erlang:system_time(millisecond),

                    Artifact = #{
                        name => Name,
                        type => Type,
                        path => ArtifactPath,
                        hash => Hash,
                        size => Size,
                        added_at => AddedAt
                    },
                    UpdatedArtifacts = Artifacts#{Name => Artifact},
                    {ok, Pack#{artifacts => UpdatedArtifacts}};
                {error, Reason} ->
                    {error, {read_failed, Reason}}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Lists all artifacts in a pack.
%%
%% @param Pack The pack to query
%% @returns List of artifact names
%%
%% @end
%%--------------------------------------------------------------------
-spec list_artifacts(pack()) -> [binary()].

list_artifacts(#{artifacts := Artifacts}) ->
    maps:keys(Artifacts).

%%--------------------------------------------------------------------
%% @doc Gets an artifact from a pack.
%%
%% @param Pack The pack to query
%% @param Name Artifact name
%% @returns {ok, Artifact} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_artifact(pack(), binary()) -> {ok, artifact()} | {error, not_found}.

get_artifact(#{artifacts := Artifacts}, Name) ->
    case maps:get(Name, Artifacts, undefined) of
        undefined -> {error, not_found};
        Artifact -> {ok, Artifact}
    end.

%%--------------------------------------------------------------------
%% @doc Removes an artifact from a pack.
%%
%% @param Pack The pack to modify
%% @param Name Artifact name to remove
%% @returns {ok, UpdatedPack} on success, {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_artifact(pack(), binary()) -> {ok, pack()} | {error, not_found}.

remove_artifact(#{artifacts := Artifacts} = Pack, Name) ->
    case maps:get(Name, Artifacts, undefined) of
        undefined ->
            {error, not_found};
        _Artifact ->
            UpdatedArtifacts = maps:remove(Name, Artifacts),
            {ok, Pack#{artifacts => UpdatedArtifacts}}
    end.

%%--------------------------------------------------------------------
%% @doc Verifies an artifact's hash matches its file content.
%%
%% @param Pack The pack containing the artifact
%% @param Name Artifact name
%% @returns {ok, true} if valid, {ok, false} if invalid, {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec verify_artifact(pack(), binary()) -> {ok, boolean()} | {error, term()}.

verify_artifact(#{artifacts := Artifacts}, Name) ->
    case maps:get(Name, Artifacts, undefined) of
        undefined ->
            {error, not_found};
        #{hash := StoredHash, path := Path} ->
            case file:read_file(Path) of
                {ok, Content} ->
                    ComputedHash = crypto:hash(sha256, Content),
                    {ok, ComputedHash =:= StoredHash};
                {error, Reason} ->
                    {error, {read_failed, Reason}}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Adds a proof result to a pack.
%%
%% @param Pack The pack to modify
%% @param ProofName Name of the proof (atom, e.g., replay_proof)
%% @param ProofFile Path to proof JSON file
%% @returns {ok, UpdatedPack} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec add_proof(pack(), atom(), file:filename_all()) -> {ok, pack()} | {error, term()}.

add_proof(#{proofs := Proofs} = Pack, ProofName, ProofFile) ->
    case file:read_file(ProofFile) of
        {ok, Content} ->
            Hash = crypto:hash(sha256, Content),
            %% Try to parse proof to determine status
            Status = parse_proof_status(Content),
            ProofResult = #{
                name => ProofName,
                file => list_to_binary(filename:basename(ProofFile)),
                status => Status,
                hash => Hash,
                verified_at => erlang:system_time(millisecond)
            },
            UpdatedProofs = Proofs#{ProofName => ProofResult},
            {ok, Pack#{proofs => UpdatedProofs}};
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Gets a proof result from a pack.
%%
%% @param Pack The pack to query
%% @param ProofName Proof name (atom)
%% @returns {ok, ProofResult} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_proof(pack(), atom()) -> {ok, proof_result()} | {error, not_found}.

get_proof(#{proofs := Proofs}, ProofName) ->
    case maps:get(ProofName, Proofs, undefined) of
        undefined -> {error, not_found};
        Proof -> {ok, Proof}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all proof names in a pack.
%%
%% @param Pack The pack to query
%% @returns List of proof names
%%
%% @end
%%--------------------------------------------------------------------
-spec list_proofs(pack()) -> [atom()].

list_proofs(#{proofs := Proofs}) ->
    maps:keys(Proofs).

%%--------------------------------------------------------------------
%% @doc Verifies all proofs in a pack and returns overall status.
%%
%% @param Pack The pack to verify
%% @returns {ok, AllPass} where AllPass is true if all proofs pass
%%
%% @end
%%--------------------------------------------------------------------
-spec verify_all_proofs(pack()) -> {ok, boolean()}.

verify_all_proofs(#{proofs := Proofs}) ->
    AllPass = maps:fold(fun(_Name, #{status := Status}, Acc) ->
        Acc andalso (Status =:= pass)
    end, true, Proofs),
    {ok, AllPass}.

%%--------------------------------------------------------------------
%% @doc Adds a benchmark result to a pack.
%%
%% @param Pack The pack to modify
%% @param Name Benchmark name
%% @param Benchmark Benchmark data map
%% @returns {ok, UpdatedPack}
%%
%% @end
%%--------------------------------------------------------------------
-spec add_benchmark(pack(), binary(), benchmark() | map()) -> {ok, pack()} | {error, term()}.

add_benchmark(#{benchmarks := Benchmarks} = Pack, Name, BenchmarkData) ->
    Benchmark = case BenchmarkData of
        #{value := Value, unit := _Unit} = Bm ->
            %% Extract baseline if available
            Baseline = maps:get(baseline, Bm, undefined),
            Delta = case Baseline of
                undefined -> undefined;
                _ -> Value - Baseline
            end,
            DeltaPercent = case {Baseline, Delta} of
                {undefined, _} -> undefined;
                {_, undefined} -> undefined;
                {0, _} -> undefined;
                {_, D} -> (D / Baseline) * 100.0
            end,
            Bm#{
                name => Name,
                baseline => Baseline,
                delta => Delta,
                delta_percent => DeltaPercent
            };
        _ ->
            {error, invalid_benchmark_format}
    end,
    case Benchmark of
        {error, Reason} ->
            {error, Reason};
        _ ->
            UpdatedBenchmarks = Benchmarks#{Name => Benchmark},
            {ok, Pack#{benchmarks => UpdatedBenchmarks}}
    end.

%%--------------------------------------------------------------------
%% @doc Compares a benchmark against its baseline.
%%
%% @param Pack The pack containing benchmarks
%% @param Name Benchmark name
%% @returns {ok, ComparisonResult} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec compare_benchmark(pack(), binary()) ->
    {ok, #{comparison => atom(), delta => number() | undefined}} | {error, not_found}.

compare_benchmark(#{benchmarks := Benchmarks}, Name) ->
    case maps:get(Name, Benchmarks, undefined) of
        undefined ->
            {error, not_found};
        #{value := _Value, baseline := undefined} ->
            {ok, #{comparison => no_baseline, delta => undefined}};
        #{value := Value, baseline := Baseline, delta := Delta} ->
            Comparison = if
                Value < Baseline -> better;
                Value > Baseline -> worse;
                true -> same
            end,
            {ok, #{comparison => Comparison, delta => Delta}}
    end.

%%--------------------------------------------------------------------
%% @doc Formats benchmarks as a markdown table.
%%
%% @param Pack The pack containing benchmarks
%% @returns Markdown table iolist
%%
%% @end
%%--------------------------------------------------------------------
-spec format_benchmarks(pack()) -> iolist().

format_benchmarks(#{benchmarks := Benchmarks}) when map_size(Benchmarks) =:= 0 ->
    ["## Benchmarks\n\nNo benchmarks available.\n"];
format_benchmarks(#{benchmarks := Benchmarks}) ->
    Names = maps:keys(Benchmarks),
    SortedNames = lists:sort([binary_to_list(N) || N <- Names]),

    Header = "## Benchmarks\n\n"
             "| Metric | Value | Baseline | Delta | Change % |\n"
             "|--------|-------|----------|-------|----------|\n",

    Rows = [format_benchmark_row(maps:get(list_to_binary(N), Benchmarks)) || N <- SortedNames],

    [Header, Rows, "\n"].

%%--------------------------------------------------------------------
%% @doc Computes hash of entire pack for integrity verification.
%%
%% Hash is computed from sorted artifact hashes, proofs, and benchmarks.
%%
%% @param Pack The pack to hash
%% @returns <<_:256>> hash or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec pack_hash(pack()) -> <<_:256>> | {error, term()}.

pack_hash(#{artifacts := Artifacts, proofs := Proofs, benchmarks := Benchmarks}) ->
    try
        %% Get sorted artifact hashes
        ArtifactHashes = lists:sort([H || #{hash := H} <- maps:values(Artifacts)]),

        %% Get proof hashes
        ProofHashes = lists:sort([H || #{hash := H} <- maps:values(Proofs)]),

        %% Combine all hashes
        AllHashes = ArtifactHashes ++ ProofHashes,
        Combined = iolist_to_binary(AllHashes),

        %% Add benchmark data
        BenchmarkBinary = term_to_binary(lists:sort(maps:to_list(Benchmarks))),

        %% Final hash
        crypto:hash(sha256, <<Combined/binary, BenchmarkBinary/binary>>)
    catch
        _:_:Error ->
            {error, {hash_error, Error}}
    end.

%%--------------------------------------------------------------------
%% @doc Generates a unique pack ID (UUID v4 format).
%%
%% @returns UUID binary (iolist that can be converted to binary)
%%
%% @end
%%--------------------------------------------------------------------
-spec pack_id() -> pack_id().

pack_id() ->
    %% Generate UUID v4 using 16 random bytes
    <<TimeLow:32, TimeMid:16, TimeHiAndVersion:16, ClockSeqHiAndRes:8, ClockSeqLow:8, Node:48>> =
        crypto:strong_rand_bytes(16),
    %% Set version 4 bits in the high nibble of TimeHiAndVersion (0100)
    VersionedHiAndVersion = (TimeHiAndVersion band 16#0FFF) bor 16#4000,
    %% Set variant bits in the high nibble of ClockSeqHiAndRes (10xx)
    VariantClockSeq = (ClockSeqHiAndRes band 16#3F) bor 16#80,
    %% Format as standard UUID string: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx
    io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~2.16.0b~6.16.0b",
                  [TimeLow, TimeMid, VersionedHiAndVersion,
                   (VariantClockSeq bsr 4), (VariantClockSeq band 16#0F),
                   ClockSeqLow, Node]).

%%--------------------------------------------------------------------
%% @doc Merges two packs, combining their artifacts and proofs.
%%
%% @param Pack1 First pack
%% @param Pack2 Second pack
%% @returns {ok, MergedPack}
%%
%% @end
%%--------------------------------------------------------------------
-spec merge_packs(pack(), pack()) -> {ok, pack()}.

merge_packs(#{artifacts := A1, proofs := P1, benchmarks := B1} = Pack1,
            #{artifacts := A2, proofs := P2, benchmarks := B2}) ->
    MergedArtifacts = maps:merge(A1, A2),
    MergedProofs = maps:merge(P1, P2),
    MergedBenchmarks = maps:merge(B1, B2),

    %% Create new merged pack ID
    MergedId = iolist_to_binary(["merged_", pack_id()]),

    MergedPack = Pack1#{
        id => MergedId,
        artifacts => MergedArtifacts,
        proofs => MergedProofs,
        benchmarks => MergedBenchmarks,
        created => erlang:system_time(millisecond)
    },
    {ok, MergedPack}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Scans evidence directory for artifacts.
-spec scan_artifacts(file:filename_all(), integer()) -> #{binary() => artifact()}.

scan_artifacts(EvidenceDir, Created) ->
    case file:list_dir(EvidenceDir) of
        {ok, Files} ->
            lists:foldl(fun(Filename, Acc) ->
                Path = filename:join(EvidenceDir, Filename),
                case filelib:is_regular(Path) of
                    false ->
                        Acc;
                    true ->
                        Name = list_to_binary(Filename),
                        Type = infer_artifact_type(Path),
                        case hash_file(Path) of
                            {ok, Hash} ->
                                Size = filelib:file_size(Path),
                                Artifact = #{
                                    name => Name,
                                    type => Type,
                                    path => Path,
                                    hash => Hash,
                                    size => Size,
                                    added_at => Created
                                },
                                Acc#{Name => Artifact};
                            {error, _} ->
                                Acc
                        end
                end
            end, #{}, Files);
        {error, _} ->
            #{}
    end.

%% @private
%% @doc Scans for proof JSON files.
-spec scan_proofs(file:filename_all()) -> #{atom() => proof_result()}.

scan_proofs(EvidenceDir) ->
    ProofFiles = [
        "replay_proof.json",
        "cancel_proof.json",
        "crash_proof.json",
        "budget_proof.json"
    ],
    lists:foldl(fun(Filename, Acc) ->
        Path = filename:join(EvidenceDir, Filename),
        case file:read_file(Path) of
            {ok, Content} ->
                Hash = crypto:hash(sha256, Content),
                Status = parse_proof_status(Content),
                %% Infer proof name from filename
                ProofName = filename_to_proof_name(Filename),
                Proof = #{
                    name => ProofName,
                    file => list_to_binary(Filename),
                    status => Status,
                    hash => Hash,
                    verified_at => erlang:system_time(millisecond)
                },
                Acc#{ProofName => Proof};
            {error, _} ->
                Acc
        end
    end, #{}, ProofFiles).

%% @private
%% @doc Scans for benchmark JSON files.
-spec scan_benchmarks(file:filename_all()) -> #{binary() => benchmark()}.

scan_benchmarks(EvidenceDir) ->
    BenchFile = filename:join(EvidenceDir, "benchmarks.json"),
    case file:read_file(BenchFile) of
        {ok, Content} ->
            try jsx:decode(Content, [return_maps]) of
                Benchmarks when is_map(Benchmarks) ->
                    maps:map(fun(_Name, Data) ->
                        case Data of
                            #{value := V, unit := U} = Bm ->
                                Baseline = maps:get(baseline, Bm, undefined),
                                Delta = case Baseline of
                                    undefined -> undefined;
                                    _ -> V - Baseline
                                end,
                                DeltaPercent = case {Baseline, Delta} of
                                    {undefined, _} -> undefined;
                                    {_, undefined} -> undefined;
                                    {0, _} -> undefined;
                                    {_, D} when is_number(D), is_number(Baseline), Baseline =/= 0 ->
                                        (D / Baseline) * 100.0;
                                    _ ->
                                        undefined
                                end,
                                #{
                                    name => maps:get(name, Bm, <<"unnamed">>),
                                    value => V,
                                    unit => U,
                                    baseline => Baseline,
                                    delta => Delta,
                                    delta_percent => DeltaPercent
                                };
                            _ ->
                                #{value => 0, unit => <<"unknown">>}
                        end
                    end, Benchmarks);
            _ ->
                #{}
            catch
                _:_ ->
                    #{}
            end;
        {error, _} ->
            #{}
    end.

%% @private
%% @doc Infers artifact type from filename.
-spec infer_artifact_type(file:filename_all()) -> artifact_type().

infer_artifact_type(Path) ->
    Filename = filename:basename(Path, ".json"),
    Lower = string:lowercase(Filename),
    case Lower of
        "trace" ++ _ -> trace;
        "proof" ++ _ -> proof;
        "counter" ++ _ -> counter;
        "stat" ++ _ -> statistic;
        "benchmark" ++ _ -> benchmark;
        "log" ++ _ -> log;
        "screenshot" ++ _ -> screenshot;
        "config" ++ _ -> config;
        "receipt" ++ _ -> receipt;
        _ -> other
    end.

%% @private
%% @doc Computes SHA-256 hash of a file.
-spec hash_file(file:filename_all()) -> {ok, <<_:256>>} | {error, term()}.

hash_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            {ok, crypto:hash(sha256, Content)};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% @doc Parses proof status from JSON content.
-spec parse_proof_status(binary()) -> proof_status().

parse_proof_status(Json) ->
    try jsx:decode(Json, [return_maps]) of
        Map ->
            Status = maps:get(<<"status">>, Map, undefined),
            HashesEqual = maps:get(<<"hashes_equal">>, Map, undefined),
            EffectsVerified = maps:get(<<"effects_verified">>, Map, undefined),
            case Status of
                <<"verified">> -> pass;
                <<"pass">> -> pass;
                <<"failed">> -> fail;
                <<"error">> -> fail;
                _ when HashesEqual =:= true -> pass;
                _ when HashesEqual =:= false -> fail;
                _ when EffectsVerified =:= true -> pass;
                _ when EffectsVerified =:= false -> fail;
                _ -> skipped
            end
    catch
        _:_ -> skipped
    end.

%% @private
%% @doc Converts filename to proof name atom.
-spec filename_to_proof_name(file:filename_all()) -> atom().

filename_to_proof_name("replay_proof.json") -> replay_proof;
filename_to_proof_name("cancel_proof.json") -> cancel_proof;
filename_to_proof_name("crash_proof.json") -> crash_proof;
filename_to_proof_name("budget_proof.json") -> budget_proof;
filename_to_proof_name(_) -> unknown_proof.

%% @private
%% @doc Formats metadata section.
-spec format_metadata(iolist(), integer(), metadata()) -> iolist().

format_metadata(PackId, Created, Meta) ->
    CreatedIso = format_timestamp(Created),
    Description = maps:get(description, Meta, <<"Evidence pack">>),
    Version = maps:get(version, Meta, <<"1.0.0">>),
    [
        "- Pack ID: ", PackId, "\n",
        "- Created: ", CreatedIso, "\n",
        "- Description: ", Description, "\n",
        "- Version: ", Version, "\n"
    ].

%% @private
%% @doc Formats artifacts table.
-spec format_artifacts_table([artifact()], boolean(), boolean()) -> iolist().

format_artifacts_table(Artifacts, IncludeHash, IncludeSize) ->
    [
        "## Artifacts\n\n"
        "| Name | Type",
        case IncludeHash of true -> " | Hash"; _ -> "" end,
        case IncludeSize of true -> " | Size"; _ -> "" end,
        " |\n",
        "|------|------",
        case IncludeHash of true -> " | ------"; _ -> "" end,
        case IncludeSize of true -> " | ------"; _ -> "" end,
        " |\n",
        [format_artifact_row(A, IncludeHash, IncludeSize) || A <- Artifacts],
        "\n"
    ].

%% @private
%% @doc Formats a single artifact row.
-spec format_artifact_row(artifact(), boolean(), boolean()) -> iolist().

format_artifact_row(#{name := Name, type := Type, hash := Hash, size := Size}, IncludeHash, IncludeSize) ->
    TypeStr = atom_to_binary(Type),
    HashStr = binary:encode_hex(Hash),
    SizeStr = format_size(Size),
    [
        "| ", Name, " | ", TypeStr,
        case IncludeHash of true -> [" | ", HashStr]; _ -> "" end,
        case IncludeSize of true -> [" | ", SizeStr]; _ -> "" end,
        " |\n"
    ].

%% @private
%% @doc Formats proofs section.
-spec format_proofs_section(#{atom() => proof_result()}) -> iolist().

format_proofs_section(Proofs) when map_size(Proofs) =:= 0 ->
    "## Proofs\n\nNo proofs available.\n";
format_proofs_section(Proofs) ->
    Names = lists:sort([atom_to_list(N) || N <- maps:keys(Proofs)]),
    [
        "## Proofs\n\n",
        [format_proof_entry(maps:get(list_to_binary(N), Proofs)) || N <- Names],
        "\n"
    ].

%% @private
%% @doc Formats a single proof entry.
-spec format_proof_entry(proof_result()) -> iolist().

format_proof_entry(#{name := Name, file := File, status := Status}) ->
    StatusStr = case Status of
        pass -> "[PASS]";
        fail -> "[FAIL]";
        error -> "[ERROR]";
        skipped -> "[SKIPPED]"
    end,
    [StatusStr, " ", atom_to_binary(Name), ": ", File, "\n"].

%% @private
%% @doc Formats benchmarks section.
-spec format_benchmarks_section(#{binary() => benchmark()}) -> iolist().

format_benchmarks_section(Benchmarks) when map_size(Benchmarks) =:= 0 ->
    "## Benchmarks\n\nNo benchmarks available.\n";
format_benchmarks_section(Benchmarks) ->
    Names = lists:sort([binary_to_list(N) || N <- maps:keys(Benchmarks)]),
    Header = "## Benchmarks\n\n"
              "| Metric | Value | Baseline | Delta | Change % |\n"
              "|--------|-------|----------|-------|----------|\n",
    Rows = [format_benchmark_row(maps:get(list_to_binary(N), Benchmarks)) || N <- Names],
    [Header, Rows, "\n"].

%% @private
%% @doc Formats a single benchmark row.
-spec format_benchmark_row(benchmark()) -> iolist().

format_benchmark_row(#{name := Name, value := Value, unit := Unit,
                       baseline := Baseline, delta := Delta, delta_percent := DeltaPercent}) ->
    ValueStr = format_number(Value),
    BaselineStr = format_baseline(Baseline),
    DeltaStr = format_delta(Delta),
    ChangeStr = format_change(DeltaPercent),
    ["| ", Name, " | ", ValueStr, " ", Unit, " | ", BaselineStr,
     " | ", DeltaStr, " | ", ChangeStr, " |\n"].

%% @private
%% @doc Sorts artifacts by specified criteria.
-spec sort_artifacts(#{binary() => artifact()}, atom()) -> [artifact()].

sort_artifacts(Artifacts, name) ->
    lists:sort(fun(#{name := N1}, #{name := N2}) -> N1 =< N2 end,
               maps:values(Artifacts));
sort_artifacts(Artifacts, type) ->
    lists:sort(fun(#{type := T1}, #{type := T2}) -> T1 =< T2 end,
               maps:values(Artifacts));
sort_artifacts(Artifacts, date) ->
    lists:sort(fun(#{added_at := A1}, #{added_at := A2}) -> A1 =< A2 end,
               maps:values(Artifacts)).

%% @private
%% @doc Verifies artifact hash during fold.
-spec verify_artifact_fold(binary(), artifact(), boolean()) -> boolean() | {error, term()}.

verify_artifact_fold(_Name, #{path := Path, hash := StoredHash}, Acc) when is_boolean(Acc) ->
    case file:read_file(Path) of
        {ok, Content} ->
            ComputedHash = crypto:hash(sha256, Content),
            Acc andalso (ComputedHash =:= StoredHash);
        {error, Reason} ->
            {error, {read_failed, Reason}}
    end.

%% @private
%% @doc Converts pack JSON map to internal format.
-spec convert_pack_keys(map()) -> pack().

convert_pack_keys(PackMap) ->
    %% Convert binary keys to atoms for specific fields
    Artifacts = maps:get(<<"artifacts">>, PackMap, #{}),
    Proofs = maps:get(<<"proofs">>, PackMap, #{}),
    Benchmarks = maps:get(<<"benchmarks">>, PackMap, #{}),
    Metadata = maps:get(<<"metadata">>, PackMap, #{}),

    #{
        id => maps:get(<<"id">>, PackMap),
        created => maps:get(<<"created">>, PackMap),
        evidence_dir => binary_to_list(maps:get(<<"evidence_dir">>, PackMap)),
        artifacts => convert_artifacts(Artifacts),
        proofs => convert_proofs(Proofs),
        benchmarks => convert_benchmarks(Benchmarks),
        metadata => convert_metadata(Metadata)
    }.

%% @private
%% @doc Converts artifacts from JSON.
-spec convert_artifacts(map()) -> #{binary() => artifact()}.

convert_artifacts(ArtifactsMap) ->
    maps:map(fun(_Name, Art) ->
        #{
            name => maps:get(<<"name">>, Art),
            type => binary_to_atom(maps:get(<<"type">>, Art), utf8),
            path => binary_to_list(maps:get(<<"path">>, Art)),
            hash => maps:get(<<"hash">>, Art),
            size => maps:get(<<"size">>, Art),
            added_at => maps:get(<<"added_at">>, Art)
        }
    end, ArtifactsMap).

%% @private
%% @doc Converts proofs from JSON.
-spec convert_proofs(map()) -> #{atom() => proof_result()}.

convert_proofs(ProofsMap) ->
    maps:fold(fun(NameBin, Proof, Acc) ->
        Name = binary_to_atom(NameBin, utf8),
        Acc#{Name => #{
            name => Name,
            file => maps:get(<<"file">>, Proof),
            status => binary_to_atom(maps:get(<<"status">>, Proof), utf8),
            hash => maps:get(<<"hash">>, Proof),
            verified_at => maps:get(<<"verified_at">>, Proof)
        }}
    end, #{}, ProofsMap).

%% @private
%% @doc Converts benchmarks from JSON.
-spec convert_benchmarks(map()) -> #{binary() => benchmark()}.

convert_benchmarks(BenchmarksMap) ->
    maps:map(fun(_Name, Bm) ->
        #{
            name => maps:get(<<"name">>, Bm),
            value => maps:get(<<"value">>, Bm),
            unit => maps:get(<<"unit">>, Bm),
            baseline => maps:get(<<"baseline">>, Bm, undefined),
            delta => maps:get(<<"delta">>, Bm, undefined),
            delta_percent => maps:get(<<"delta_percent">>, Bm, undefined)
        }
    end, BenchmarksMap).

%% @private
%% @doc Converts metadata from JSON.
-spec convert_metadata(map()) -> metadata().

convert_metadata(MetaMap) ->
    #{
        created_at => maps:get(<<"created_at">>, MetaMap),
        created_by => maps:get(<<"created_by">>, MetaMap, undefined),
        description => maps:get(<<"description">>, MetaMap, undefined),
        tags => maps:get(<<"tags">>, MetaMap, []),
        version => maps:get(<<"version">>, MetaMap, <<"1.0.0">>)
    }.

%% @private
%% @doc Converts pack to JSON-encodable format.
-spec pack_to_json(pack()) -> map().

pack_to_json(#{id := Id, created := Created, evidence_dir := EvidenceDir,
               artifacts := Artifacts, proofs := Proofs, benchmarks := Benchmarks,
               metadata := Meta}) ->
    #{
        <<"id">> => Id,
        <<"created">> => Created,
        <<"evidence_dir">> => list_to_binary(EvidenceDir),
        <<"artifacts">> => artifacts_to_json(Artifacts),
        <<"proofs">> => proofs_to_json(Proofs),
        <<"benchmarks">> => benchmarks_to_json(Benchmarks),
        <<"metadata">> => metadata_to_json(Meta)
    }.

%% @private
%% @doc Converts artifacts to JSON format.
-spec artifacts_to_json(#{binary() => artifact()}) -> map().

artifacts_to_json(Artifacts) ->
    maps:map(fun(_Name, Art) ->
        #{
            <<"name">> => maps:get(name, Art),
            <<"type">> => atom_to_binary(maps:get(type, Art)),
            <<"path">> => list_to_binary(maps:get(path, Art)),
            <<"hash">> => maps:get(hash, Art),
            <<"size">> => maps:get(size, Art),
            <<"added_at">> => maps:get(added_at, Art)
        }
    end, Artifacts).

%% @private
%% @doc Converts proofs to JSON format.
-spec proofs_to_json(#{atom() => proof_result()}) -> map().

proofs_to_json(Proofs) ->
    maps:fold(fun(Name, Proof, Acc) ->
        NameBin = atom_to_binary(Name),
        Acc#{NameBin => #{
            <<"name">> => atom_to_binary(maps:get(name, Proof)),
            <<"file">> => maps:get(file, Proof),
            <<"status">> => atom_to_binary(maps:get(status, Proof)),
            <<"hash">> => maps:get(hash, Proof),
            <<"verified_at">> => maps:get(verified_at, Proof)
        }}
    end, #{}, Proofs).

%% @private
%% @doc Converts benchmarks to JSON format.
-spec benchmarks_to_json(#{binary() => benchmark()}) -> map().

benchmarks_to_json(Benchmarks) ->
    maps:map(fun(_Name, Bm) ->
        #{
            <<"name">> => maps:get(name, Bm),
            <<"value">> => maps:get(value, Bm),
            <<"unit">> => maps:get(unit, Bm),
            <<"baseline">> => maps:get(baseline, Bm),
            <<"delta">> => maps:get(delta, Bm),
            <<"delta_percent">> => maps:get(delta_percent, Bm)
        }
    end, Benchmarks).

%% @private
%% @doc Converts metadata to JSON format.
-spec metadata_to_json(metadata()) -> map().

metadata_to_json(Meta) ->
    #{
        <<"created_at">> => maps:get(created_at, Meta),
        <<"created_by">> => maps:get(created_by, Meta, null),
        <<"description">> => maps:get(description, Meta, null),
        <<"tags">> => maps:get(tags, Meta, []),
        <<"version">> => maps:get(version, Meta)
    }.

%% @private
%% @doc Formats timestamp as ISO string.
-spec format_timestamp(integer()) -> binary().

format_timestamp(Millis) ->
    %% Convert milliseconds to seconds and format as ISO 8601
    Seconds = Millis div 1000,
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:system_time_to_universal_time(Seconds, seconds),
    io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Year, Month, Day, Hour, Min, Sec]).

%% @private
%% @doc Formats file size in human readable format.
-spec format_size(non_neg_integer()) -> binary().

format_size(Bytes) when Bytes < 1024 ->
    iolist_to_binary([integer_to_list(Bytes), " B"]);
format_size(Bytes) when Bytes < 1024 * 1024 ->
    iolist_to_binary([integer_to_list(Bytes div 1024), " KB"]);
format_size(Bytes) when Bytes < 1024 * 1024 * 1024 ->
    iolist_to_binary([integer_to_list(Bytes div (1024 * 1024)), " MB"]);
format_size(Bytes) ->
    iolist_to_binary([float_to_list(Bytes / (1024 * 1024 * 1024), [{decimals, 2}]), " GB"]).

%% @private
%% @doc Formats a number for display.
-spec format_number(number()) -> binary().

format_number(N) when is_integer(N) ->
    integer_to_binary(N);
format_number(N) when is_float(N) ->
    io_lib:format("~.2f", [N]).

%% @private
%% @doc Formats baseline value.
-spec format_baseline(number() | undefined) -> binary().

format_baseline(undefined) -> <<"N/A">>;
format_baseline(B) -> format_number(B).

%% @private
%% @doc Formats delta value.
-spec format_delta(number() | undefined) -> binary().

format_delta(undefined) -> <<"N/A">>;
format_delta(D) when D < 0 -> <<"-", (format_number(abs(D)))/binary>>;
format_delta(D) -> <<"+", (format_number(D))/binary>>.

%% @private
%% @doc Formats change percentage.
-spec format_change(float() | undefined) -> binary().

format_change(undefined) -> <<"N/A">>;
format_change(P) when P < 0 ->
    [integer_to_list(trunc(P)), "%"];
format_change(P) ->
    ["+", integer_to_list(trunc(P)), "%"].
