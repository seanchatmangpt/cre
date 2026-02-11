%%%-------------------------------------------------------------------
%%% @doc Period Evidence Index for SOC 2 Type II
%%% Maintains deterministic index of evidence snapshots over time.
%%% Creates snapshots by copying evidence/ to evidence/period/snapshot_YYYYMMDD/
%%% with manifest and verdict files. Implements 365-day rotation.
%%% @end
%%%-------------------------------------------------------------------
-module(period_evidence_index).

-export([
    create_snapshot/1,
    update_index/1,
    get_index/0,
    verify_index/0,
    rotate_snapshots/0,
    get_snapshot_path/1,
    list_snapshots/0
]).

-define(INDEX_FILE, "evidence/period/index.json").
-define(SNAPSHOT_BASE_DIR, "evidence/period").
-define(EVIDENCE_DIR, "evidence").
-define(RETENTION_DAYS, 365).
-define(MANIFEST_FILE, "evidence.last.json").
-define(VERDICT_FILE, "verdict.last.json").

-type snapshot() :: #{
    snapshot_id => binary(),
    timestamp => binary(),
    manifest_hash => binary(),
    verdict_hash => binary(),
    suites => [binary()],
    evidence_count => integer()
}.

%%====================================================================
%% API
%%====================================================================

-spec create_snapshot(map()) -> {ok, binary()} | {error, term()}.
create_snapshot(Opts) ->
    %% Generate snapshot ID (deterministic based on date, not timestamp)
    Date = maps:get(date, Opts, current_date()),
    SnapshotId = generate_snapshot_id(Date),
    SnapshotPath = get_snapshot_path(SnapshotId),

    try
        %% Create snapshot directory
        ok = filelib:ensure_dir(filename:join([SnapshotPath, "dummy"])),

        %% Copy evidence files to snapshot directory
        EvidenceFiles = collect_evidence_files(),
        CopyResults = lists:map(fun(EvidenceFile) ->
            copy_evidence_file(EvidenceFile, SnapshotPath)
        end, EvidenceFiles),

        %% Check for copy errors
        case lists:filter(fun({ok, _}) -> false; (_) -> true end, CopyResults) of
            [] ->
                ok;  %% All copies successful
            Errors ->
                logger:warning("Some evidence files failed to copy: ~p", [Errors])
        end,

        %% Copy manifest and verdict files to snapshot
        copy_snapshot_metadata(SnapshotPath),

        %% Collect evidence files that were actually copied
        CopiedFiles = lists:filtermap(fun
            ({ok, File}) -> {true, File};
            (_) -> false
        end, CopyResults),

        %% Get manifest and verdict hashes
        ManifestFile = filename:join([SnapshotPath, ?MANIFEST_FILE]),
        VerdictFile = filename:join([SnapshotPath, ?VERDICT_FILE]),
        ManifestHash = get_file_hash(ManifestFile),
        VerdictHash = get_file_hash(VerdictFile),

        %% Get suites from verdict or opts
        Suites = maps:get(suites, Opts, get_suites_from_verdict()),

        %% Create snapshot metadata
        Snapshot = #{
            snapshot_id => SnapshotId,
            timestamp => iso8601_now(),
            path => list_to_binary(SnapshotPath),
            manifest_hash => ManifestHash,
            verdict_hash => VerdictHash,
            suites => Suites,
            evidence_count => length(CopiedFiles),
            created_at => iso8601_now()
        },

        %% Write snapshot metadata file
        SnapshotMetadataFile = filename:join([SnapshotPath, "snapshot_metadata.json"]),
        SnapshotJson = canonical_json(Snapshot),
        ok = file:write_file(SnapshotMetadataFile, SnapshotJson),

        logger:info("Created snapshot ~s at ~s with ~p evidence files",
                   [SnapshotId, SnapshotPath, length(CopiedFiles)]),

        %% Update index
        case update_index(Snapshot) of
            ok ->
                %% Attempt rotation after successful snapshot
                ok = rotate_snapshots(),
                {ok, SnapshotId};
            {error, Reason} ->
                {error, {index_update_failed, Reason}}
        end
    catch
        Class:Reason:Stacktrace ->
            logger:error("Failed to create snapshot ~s: ~p:~p~n~p",
                        [SnapshotId, Class, Reason, Stacktrace]),
            {error, {snapshot_creation_failed, Class, Reason}}
    end.

-spec update_index(snapshot()) -> ok | {error, term()}.
update_index(Snapshot) ->
    %% Read current index
    Index = case get_index() of
        {ok, I} -> I;
        {error, enoent} -> #{snapshots => [], period_start => null, period_end => null}
    end,

    %% Add new snapshot (deterministically)
    Snapshots = maps:get(snapshots, Index, []),
    NewSnapshots = add_snapshot_deterministically(Snapshot, Snapshots),

    %% Update period bounds
    {PeriodStart, PeriodEnd} = compute_period_bounds(NewSnapshots),

    %% Create updated index
    NewIndex = #{
        version => <<"1.0.0">>,
        period_start => PeriodStart,
        period_end => PeriodEnd,
        snapshots => NewSnapshots,
        snapshot_count => length(NewSnapshots),
        type_ii_days => compute_days_covered(NewSnapshots)
    },

    %% Write index (deterministic JSON)
    IndexJson = canonical_json(NewIndex),
    filelib:ensure_dir(?INDEX_FILE),
    file:write_file(?INDEX_FILE, IndexJson).

-spec get_index() -> {ok, map()} | {error, term()}.
get_index() ->
    case file:read_file(?INDEX_FILE) of
        {ok, JsonBin} ->
            {ok, json:decode(JsonBin)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec verify_index() -> ok | {error, term()}.
verify_index() ->
    case get_index() of
        {ok, Index} ->
            Snapshots = maps:get(snapshots, Index, []),

            %% Verify each snapshot directory exists and contains required files
            Results = lists:map(fun(Snapshot) ->
                SnapshotId = maps:get(snapshot_id, Snapshot),
                SnapshotPath = get_snapshot_path(SnapshotId),

                case filelib:is_dir(SnapshotPath) of
                    true ->
                        %% Check for snapshot_metadata.json
                        MetadataFile = filename:join([SnapshotPath, "snapshot_metadata.json"]),
                        case filelib:is_regular(MetadataFile) of
                            true -> ok;
                            false -> {error, {missing_metadata, SnapshotId}}
                        end;
                    false ->
                        {error, {missing_snapshot_dir, SnapshotId}}
                end
            end, Snapshots),

            case lists:all(fun(R) -> R =:= ok end, Results) of
                true -> ok;
                false -> {error, {verification_failed, Results}}
            end;
        {error, Reason} ->
            {error, {index_not_found, Reason}}
    end.

-spec rotate_snapshots() -> ok | {error, term()}.
rotate_snapshots() ->
    try
        SnapshotDirs = list_snapshot_directories(),
        Now = erlang:system_time(second),
        RotationThreshold = Now - (?RETENTION_DAYS * 86400),

        DeletedDirs = lists:filtermap(fun(SnapshotDir) ->
            SnapshotId = filename:basename(SnapshotDir),
            case extract_date_from_snapshot_id(SnapshotId) of
                invalid_format ->
                    false;
                DateBin ->
                    case date_to_seconds(DateBin) of
                        invalid_date -> false;
                        SnapshotTime ->
                            case SnapshotTime < RotationThreshold of
                                true ->
                                    case delete_snapshot_directory(SnapshotDir) of
                                        ok ->
                                            logger:info("Deleted expired snapshot: ~s", [SnapshotId]),
                                            {true, SnapshotId};
                                        {error, Reason} ->
                                            logger:error("Failed to delete snapshot ~s: ~p", [SnapshotId, Reason]),
                                            false
                                    end;
                                false ->
                                    false
                            end
                    end
            end
        end, SnapshotDirs),

        case DeletedDirs of
            [] ->
                logger:debug("No snapshots expired for rotation");
            _ ->
                logger:info("Rotated ~p expired snapshots", [length(DeletedDirs)])
        end,

        ok
    catch
        Class:Reason:Stacktrace ->
            logger:error("Snapshot rotation failed: ~p:~p~n~p", [Class, Reason, Stacktrace]),
            {error, {rotation_failed, Class, Reason}}
    end.

-spec list_snapshots() -> [binary()] | {error, term()}.
list_snapshots() ->
    case file:list_dir(?SNAPSHOT_BASE_DIR) of
        {ok, Files} ->
            lists:filter(fun(F) ->
                filelib:is_dir(filename:join([?SNAPSHOT_BASE_DIR, F]))
            end, Files);
        {error, _Reason} ->
            []
    end.

-spec get_snapshot_path(binary()) -> string().
get_snapshot_path(SnapshotId) ->
    filename:join([?SNAPSHOT_BASE_DIR, binary_to_list(SnapshotId)]).

%%====================================================================
%% Internal Functions
%%====================================================================

generate_snapshot_id(Date) ->
    %% Format: snapshot_YYYYMMDD
    list_to_binary(io_lib:format("snapshot_~s", [Date])).

current_date() ->
    {{Y, M, D}, _} = calendar:universal_time(),
    io_lib:format("~4..0B~2..0B~2..0B", [Y, M, D]).

iso8601_now() ->
    list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second), [{unit, second}])).

collect_evidence_files() ->
    %% Find all evidence files in evidence/ directory (excluding period/ subdirectory)
    EvidenceJsonFiles = filelib:wildcard(filename:join([?EVIDENCE_DIR, "**/*.json"])),
    EvidenceJsonlFiles = filelib:wildcard(filename:join([?EVIDENCE_DIR, "**/*.jsonl"])),

    %% Filter out files from evidence/period/ subdirectory
    FilteredJson = lists:filter(fun(F) ->
        not string:prefix(F, filename:join([?EVIDENCE_DIR, "period"]))
    end, EvidenceJsonFiles),
    FilteredJsonl = lists:filter(fun(F) ->
        not string:prefix(F, filename:join([?EVIDENCE_DIR, "period"]))
    end, EvidenceJsonlFiles),

    FilteredJson ++ FilteredJsonl.

get_file_hash(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            Hash = crypto:hash(sha256, Content),
            binary:encode_hex(Hash, lowercase);
        {error, _} ->
            <<"not_found">>
    end.

copy_evidence_file(SourceFile, SnapshotPath) ->
    try
        %% Build relative path preserving directory structure
        RelativePath = filename:relative_path(SourceFile, ?EVIDENCE_DIR),
        DestFile = filename:join([SnapshotPath, RelativePath]),

        %% Ensure destination directory exists
        ok = filelib:ensure_dir(DestFile),

        %% Copy file
        case file:copy(SourceFile, DestFile) of
            {ok, _BytesCopied} ->
                {ok, DestFile};
            {error, Reason} ->
                logger:warning("Failed to copy evidence file ~s: ~p", [SourceFile, Reason]),
                {error, {copy_failed, SourceFile, Reason}}
        end
    catch
        Class:Reason:Stacktrace ->
            logger:error("Exception copying file ~s: ~p:~p~n~p",
                        [SourceFile, Class, Reason, Stacktrace]),
            {error, {exception, SourceFile, Class, Reason}}
    end.

copy_snapshot_metadata(SnapshotPath) ->
    %% Copy manifest file if it exists
    case file:copy(filename:join(["receipts", ?MANIFEST_FILE]),
                   filename:join([SnapshotPath, ?MANIFEST_FILE])) of
        {ok, _} ->
            logger:debug("Copied manifest file to snapshot");
        {error, enoent} ->
            logger:debug("Manifest file not found, skipping copy");
        {error, Reason} ->
            logger:warning("Failed to copy manifest file: ~p", [Reason])
    end,

    %% Copy verdict file if it exists
    case file:copy(filename:join(["receipts", ?VERDICT_FILE]),
                   filename:join([SnapshotPath, ?VERDICT_FILE])) of
        {ok, _} ->
            logger:debug("Copied verdict file to snapshot");
        {error, enoent} ->
            logger:debug("Verdict file not found, skipping copy");
        {error, Reason} ->
            logger:warning("Failed to copy verdict file: ~p", [Reason])
    end,

    ok.

get_suites_from_verdict() ->
    case file:read_file("receipts/verdict.last.json") of
        {ok, JsonBin} ->
            Verdict = json:decode(JsonBin),
            case maps:get(<<"soc2_suites">>, Verdict, undefined) of
                undefined -> [<<"soc2_security">>];
                Suites -> Suites
            end;
        {error, _} ->
            [<<"soc2_security">>]
    end.

add_snapshot_deterministically(NewSnapshot, Snapshots) ->
    %% Add snapshot maintaining chronological order by snapshot_id
    NewId = maps:get(snapshot_id, NewSnapshot),

    %% Filter out duplicate if exists
    Filtered = lists:filter(fun(S) ->
        maps:get(snapshot_id, S) =/= NewId
    end, Snapshots),

    %% Add and sort by snapshot_id (which includes date)
    Sorted = lists:sort(fun(A, B) ->
        maps:get(snapshot_id, A) =< maps:get(snapshot_id, B)
    end, [NewSnapshot | Filtered]),

    Sorted.

compute_period_bounds(Snapshots) ->
    case Snapshots of
        [] ->
            {null, null};
        _ ->
            Sorted = lists:sort(fun(A, B) ->
                maps:get(snapshot_id, A) =< maps:get(snapshot_id, B)
            end, Snapshots),

            First = hd(Sorted),
            Last = lists:last(Sorted),

            {maps:get(snapshot_id, First), maps:get(snapshot_id, Last)}
    end.

compute_days_covered(Snapshots) ->
    case Snapshots of
        [] -> 0;
        [_] -> 1;
        _ ->
            Sorted = lists:sort(fun(A, B) ->
                maps:get(snapshot_id, A) =< maps:get(snapshot_id, B)
            end, Snapshots),

            First = hd(Sorted),
            Last = lists:last(Sorted),

            %% Extract dates from snapshot IDs (format: snapshot_YYYYMMDD)
            FirstDate = extract_date_from_snapshot_id(maps:get(snapshot_id, First)),
            LastDate = extract_date_from_snapshot_id(maps:get(snapshot_id, Last)),

            %% Compute days difference
            days_between(FirstDate, LastDate)
    end.

extract_date_from_snapshot_id(SnapshotId) when is_binary(SnapshotId) ->
    %% snapshot_YYYYMMDD -> YYYYMMDD
    case byte_size(SnapshotId) >= 17 of
        true ->
            binary:part(SnapshotId, {9, 8});
        false ->
            invalid_format
    end;
extract_date_from_snapshot_id(_) ->
    invalid_format.

days_between(Date1, Date2) ->
    %% Parse dates and compute difference
    %% Date format: YYYYMMDD
    D1 = parse_date(Date1),
    D2 = parse_date(Date2),

    calendar:date_to_gregorian_days(D2) - calendar:date_to_gregorian_days(D1).

parse_date(<<Y1, Y2, Y3, Y4, M1, M2, D1, D2>>) ->
    Year = list_to_integer([Y1, Y2, Y3, Y4]),
    Month = list_to_integer([M1, M2]),
    Day = list_to_integer([D1, D2]),
    {Year, Month, Day}.

canonical_json(Map) ->
    %% Use OTP 28 json:encode with sorted keys
    iolist_to_binary(json:encode(sort_map_keys(Map))).

sort_map_keys(Map) when is_map(Map) ->
    Keys = lists:sort(maps:keys(Map)),
    maps:from_list([{K, sort_map_keys(maps:get(K, Map))} || K <- Keys]);
sort_map_keys(List) when is_list(List) ->
    [sort_map_keys(Item) || Item <- List];
sort_map_keys(Other) ->
    Other.

list_snapshot_directories() ->
    BasePath = ?SNAPSHOT_BASE_DIR,
    case file:list_dir(BasePath) of
        {ok, Files} ->
            lists:filtermap(fun(FileName) ->
                FullPath = filename:join([BasePath, FileName]),
                case filelib:is_dir(FullPath) of
                    true -> {true, FullPath};
                    false -> false
                end
            end, Files);
        {error, _Reason} ->
            []
    end.

delete_snapshot_directory(DirPath) ->
    try
        ok = delete_directory_recursive(DirPath),
        logger:info("Deleted snapshot directory: ~s", [DirPath]),
        ok
    catch
        Class:Reason:Stacktrace ->
            logger:error("Failed to delete directory ~s: ~p:~p~n~p",
                        [DirPath, Class, Reason, Stacktrace]),
            {error, {deletion_failed, Class, Reason}}
    end.

delete_directory_recursive(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            lists:foreach(fun(File) ->
                Path = filename:join([Dir, File]),
                case filelib:is_dir(Path) of
                    true -> delete_directory_recursive(Path);
                    false -> ok = file:delete(Path)
                end
            end, Files),
            ok = file:del_dir(Dir);
        {error, enoent} ->
            ok
    end.

date_to_seconds(DateBin) when is_binary(DateBin), byte_size(DateBin) =:= 8 ->
    try
        {Date, _} = {parse_date(DateBin), undefined},
        Seconds = calendar:datetime_to_gregorian_seconds({Date, {0, 0, 0}}),
        Seconds
    catch
        _:_ -> invalid_date
    end;
date_to_seconds(_) ->
    invalid_date.
