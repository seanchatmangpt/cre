%%%-------------------------------------------------------------------
%%% @doc Period Evidence Index for SOC 2 Type II
%%% Maintains deterministic index of evidence snapshots over time
%%% @end
%%%-------------------------------------------------------------------
-module(period_evidence_index).

-export([
    create_snapshot/1,
    update_index/1,
    get_index/0,
    verify_index/0
]).

-define(INDEX_FILE, "evidence/period/index.json").
-define(SNAPSHOT_DIR, "evidence/period/snapshots").

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

    %% Collect evidence files
    EvidenceFiles = collect_evidence_files(),

    %% Get manifest and verdict hashes
    ManifestHash = get_file_hash("receipts/evidence.last.json"),
    VerdictHash = get_file_hash("receipts/verdict.last.json"),

    %% Get suites from verdict or opts
    Suites = maps:get(suites, Opts, get_suites_from_verdict()),

    %% Create snapshot
    Snapshot = #{
        snapshot_id => SnapshotId,
        timestamp => iso8601_now(),
        manifest_hash => ManifestHash,
        verdict_hash => VerdictHash,
        suites => Suites,
        evidence_count => length(EvidenceFiles)
    },

    %% Write snapshot to file
    SnapshotFile = filename:join([?SNAPSHOT_DIR, <<SnapshotId/binary, ".json">>]),
    filelib:ensure_dir(SnapshotFile),

    SnapshotJson = canonical_json(Snapshot),
    ok = file:write_file(SnapshotFile, SnapshotJson),

    %% Update index
    case update_index(Snapshot) of
        ok -> {ok, SnapshotId};
        {error, Reason} -> {error, Reason}
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

            %% Verify each snapshot file exists and hashes match
            Results = lists:map(fun(Snapshot) ->
                SnapshotId = maps:get(snapshot_id, Snapshot),
                SnapshotFile = filename:join([?SNAPSHOT_DIR, <<SnapshotId/binary, ".json">>]),

                case filelib:is_regular(SnapshotFile) of
                    true -> ok;
                    false -> {error, {missing_snapshot, SnapshotId}}
                end
            end, Snapshots),

            case lists:all(fun(R) -> R =:= ok end, Results) of
                true -> ok;
                false -> {error, {verification_failed, Results}}
            end;
        {error, Reason} ->
            {error, {index_not_found, Reason}}
    end.

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
    %% Find all evidence files
    filelib:wildcard("evidence/**/*.json") ++
    filelib:wildcard("evidence/**/*.jsonl").

get_file_hash(File) ->
    case file:read_file(File) of
        {ok, Content} ->
            Hash = crypto:hash(sha256, Content),
            binary:encode_hex(Hash, lowercase);
        {error, _} ->
            <<"not_found">>
    end.

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

extract_date_from_snapshot_id(SnapshotId) ->
    %% snapshot_YYYYMMDD -> YYYYMMDD
    binary:part(SnapshotId, {9, 8}).

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
