%% Evidence Manifest Builder
-module(evidence_collector).

-export([
    collect_evidence/0,
    build_manifest/1,
    verify_manifest/0
]).

-type evidence_file() :: #{
    path := binary(),
    sha256 := binary(),
    size_bytes := integer()
}.

-type evidence_manifest() :: #{
    chain := #{prev_hash := binary() | null, this_hash := binary()},
    environment_fingerprint := map(),
    evidence_files := [evidence_file()],
    generator_version := binary(),
    manifest_hash := binary(),
    ontology_hash := binary(),
    timestamp := binary()
}.

%%% API

-spec collect_evidence() -> evidence_manifest().
collect_evidence() ->
    %% Find all evidence files
    EvidenceFiles = find_evidence_files("evidence/"),

    %% Build manifest
    build_manifest(#{evidence_files => EvidenceFiles}).

-spec build_manifest(map()) -> evidence_manifest().
build_manifest(Params) ->
    EvidenceFiles = maps:get(evidence_files, Params),

    %% Generate evidence.sha256 file
    ManifestLines = [io_lib:format("~s  ~s~n", [Sha256, Path])
                     || #{path := Path, sha256 := Sha256} <- EvidenceFiles],
    ManifestContent = iolist_to_binary(ManifestLines),
    filelib:ensure_dir("evidence/"),
    file:write_file("evidence/evidence.sha256", ManifestContent),

    %% Hash the manifest
    ManifestHash = binary:encode_hex(crypto:hash(sha256, ManifestContent), lowercase),

    %% Get previous evidence receipt
    PrevHash = case file:read_file("receipts/evidence.last.json") of
        {ok, PrevJson} ->
            PrevReceipt = json:decode(PrevJson),
            case maps:get(<<"chain">>, PrevReceipt, undefined) of
                undefined -> null;  %% Old format without chain
                Chain -> maps:get(<<"this_hash">>, Chain, null)
            end;
        _ ->
            null
    end,

    %% Build receipt
    Receipt0 = #{
        chain => #{prev_hash => PrevHash},
        environment_fingerprint => receipt_builder:get_environment_fingerprint(),
        evidence_files => EvidenceFiles,
        generator_version => receipt_builder:get_generator_version(),
        manifest_hash => ManifestHash,
        ontology_hash => get_ontology_hash(),
        timestamp => receipt_builder:iso8601_now()
    },

    ThisHash = receipt_builder:hash_receipt(Receipt0),
    Receipt = Receipt0#{chain := (maps:get(chain, Receipt0))#{this_hash => ThisHash}},

    %% Write receipt
    CanonicalJson = receipt_builder:canonical_json(Receipt),
    file:write_file("receipts/evidence.last.json", CanonicalJson),

    Receipt.

-spec verify_manifest() -> ok | {error, term()}.
verify_manifest() ->
    %% Read manifest
    {ok, ManifestContent} = file:read_file("evidence/evidence.sha256"),

    %% Parse and verify each file
    Lines = binary:split(ManifestContent, <<"\n">>, [global]),
    Results = [verify_evidence_file(Line) || Line <- Lines, Line =/= <<>>],

    case lists:all(fun(R) -> R =:= ok end, Results) of
        true -> ok;
        false -> {error, {manifest_verification_failed, Results}}
    end.

%%% Internal Functions

find_evidence_files(Dir) ->
    case filelib:wildcard(Dir ++ "**/*") of
        [] -> [];
        Files ->
            [evidence_file_info(F) || F <- Files, filelib:is_regular(F)]
    end.

evidence_file_info(Path) ->
    {ok, Content} = file:read_file(Path),
    Hash = binary:encode_hex(crypto:hash(sha256, Content), lowercase),
    Size = byte_size(Content),

    #{
        path => list_to_binary(Path),
        sha256 => Hash,
        size_bytes => Size
    }.

verify_evidence_file(Line) ->
    case binary:split(Line, <<"  ">>) of
        [ExpectedHash, Path] ->
            case file:read_file(binary_to_list(Path)) of
                {ok, Content} ->
                    ActualHash = binary:encode_hex(crypto:hash(sha256, Content), lowercase),
                    case ActualHash of
                        ExpectedHash -> ok;
                        _ -> {error, {hash_mismatch, Path}}
                    end;
                {error, Reason} ->
                    {error, {file_not_found, Path, Reason}}
            end;
        _ ->
            {error, {invalid_manifest_line, Line}}
    end.

get_ontology_hash() ->
    %% Hash all ontology files
    OntologyFiles = filelib:wildcard("ontology/*.ttl"),
    case OntologyFiles of
        [] -> <<"no-ontology">>;
        Files ->
            Content = iolist_to_binary([file:read_file(F) || {ok, F} <- [file:read_file(F) || F <- Files]]),
            binary:encode_hex(crypto:hash(sha256, Content), lowercase)
    end.
