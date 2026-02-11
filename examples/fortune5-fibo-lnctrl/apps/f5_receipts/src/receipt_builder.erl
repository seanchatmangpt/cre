%% Receipt Builder - Hash-Chained Proofs
%% Uses OTP 28 json:encode/1 with stable alphabetical key ordering
-module(receipt_builder).

-export([
    build_receipt/1,
    verify_receipt/1,
    verify_chain/2,
    canonical_json/1,
    hash_receipt/1,
    get_environment_fingerprint/0,
    get_generator_version/0,
    iso8601_now/0
]).

-type receipt() :: #{
    chain := #{prev_hash := binary() | null, this_hash := binary()},
    counts := #{apps := integer(), modules := integer(), loc := integer(), tests := integer()},
    environment_fingerprint := #{arch := binary(), emulator := binary(), os := binary(), otp_version := binary()},
    generator_version := binary(),
    ontology_hash := binary(),
    timestamp := binary(),
    timings := #{generation_us := integer(), validation_us := integer()}
}.

%%% API

-spec build_receipt(map()) -> receipt().
build_receipt(Params) ->
    %% Get previous receipt hash
    PrevHash = case file:read_file("receipts/build.last.json") of
        {ok, PrevJson} ->
            PrevReceipt = json:decode(PrevJson),
            case maps:get(<<"chain">>, PrevReceipt, undefined) of
                undefined -> null;  %% Old format without chain
                Chain -> maps:get(<<"this_hash">>, Chain, null)
            end;
        _ ->
            null
    end,

    %% Build receipt without this_hash
    Receipt0 = #{
        chain => #{prev_hash => PrevHash},
        counts => maps:get(counts, Params),
        environment_fingerprint => get_environment_fingerprint(),
        generator_version => get_generator_version(),
        ontology_hash => maps:get(ontology_hash, Params),
        timestamp => iso8601_now(),
        timings => maps:get(timings, Params)
    },

    %% Compute this_hash over canonical JSON
    ThisHash = hash_receipt(Receipt0),

    %% Add this_hash to chain
    Receipt = Receipt0#{
        chain := (maps:get(chain, Receipt0))#{this_hash => ThisHash}
    },

    %% Write receipt
    filelib:ensure_dir("receipts/"),
    CanonicalJson = canonical_json(Receipt),
    file:write_file("receipts/build.last.json", CanonicalJson),
    file:write_file("receipts/build.last.sha", ThisHash),

    Receipt.

-spec verify_receipt(receipt()) -> ok | {error, term()}.
verify_receipt(Receipt) ->
    %% Extract this_hash
    Chain = maps:get(chain, Receipt),
    ThisHash = maps:get(this_hash, Chain),

    %% Remove this_hash and recompute
    Receipt0 = Receipt#{chain := maps:remove(this_hash, Chain)},
    ComputedHash = hash_receipt(Receipt0),

    case ComputedHash of
        ThisHash -> ok;
        _ -> {error, {hash_mismatch, ThisHash, ComputedHash}}
    end.

-spec verify_chain(receipt(), receipt()) -> ok | {error, term()}.
verify_chain(PrevReceipt, CurrReceipt) ->
    PrevThisHash = maps:get(this_hash, maps:get(chain, PrevReceipt)),
    CurrPrevHash = maps:get(prev_hash, maps:get(chain, CurrReceipt)),

    case PrevThisHash of
        CurrPrevHash -> ok;
        _ -> {error, {chain_broken, PrevThisHash, CurrPrevHash}}
    end.

-spec canonical_json(map()) -> binary().
canonical_json(Map) ->
    %% Use OTP 28 json:encode with stable ordering
    %% Maps are encoded with alphabetically sorted keys
    iolist_to_binary(json:encode(sort_map_keys(Map))).

-spec hash_receipt(map()) -> binary().
hash_receipt(Receipt) ->
    CanonicalJson = canonical_json(Receipt),
    Hash = crypto:hash(sha256, CanonicalJson),
    binary:encode_hex(Hash, lowercase).

%%% Internal Functions

get_environment_fingerprint() ->
    #{
        arch => list_to_binary(erlang:system_info(system_architecture)),
        emulator => list_to_binary(erlang:system_info(version)),
        os => list_to_binary(os_type()),
        otp_version => list_to_binary(erlang:system_info(otp_release))
    }.

os_type() ->
    case os:type() of
        {unix, darwin} -> "darwin";
        {unix, linux} -> "linux";
        {win32, nt} -> "windows";
        {Type, Subtype} -> io_lib:format("~p/~p", [Type, Subtype])
    end.

get_generator_version() ->
    %% Try to get git commit hash
    case os:cmd("git rev-parse HEAD 2>/dev/null") of
        [] -> <<"unknown">>;
        Hash -> list_to_binary(string:trim(Hash))
    end.

iso8601_now() ->
    list_to_binary(calendar:system_time_to_rfc3339(erlang:system_time(second), [{unit, second}])).

sort_map_keys(Map) when is_map(Map) ->
    %% Recursively sort all nested maps
    Keys = lists:sort(maps:keys(Map)),
    maps:from_list([{K, sort_map_keys(maps:get(K, Map))} || K <- Keys]);
sort_map_keys(List) when is_list(List) ->
    [sort_map_keys(Item) || Item <- List];
sort_map_keys(Other) ->
    Other.

%%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

canonical_json_test() ->
    %% Keys should be alphabetically sorted
    Map = #{z => 1, a => 2, m => 3},
    Json = canonical_json(Map),
    io:format("Generated JSON: ~s~n", [Json]),
    %% Should be {"a":2,"m":3,"z":1}
    {match, [{APos, _}]} = re:run(Json, <<"a">>),
    {match, [{MPos, _}]} = re:run(Json, <<"m">>),
    {match, [{ZPos, _}]} = re:run(Json, <<"z">>),
    io:format("Positions - a:~p m:~p z:~p~n", [APos, MPos, ZPos]),
    ?assert(APos < MPos),
    ?assert(MPos < ZPos).

hash_determinism_test() ->
    Map = #{test => true, value => 42},
    Hash1 = hash_receipt(Map),
    Hash2 = hash_receipt(Map),
    ?assertEqual(Hash1, Hash2).

receipt_verification_test() ->
    Receipt = build_receipt(#{
        counts => #{apps => 1, modules => 10, loc => 1000, tests => 5},
        ontology_hash => <<"test123">>,
        timings => #{generation_us => 1000, validation_us => 500}
    }),
    ?assertEqual(ok, verify_receipt(Receipt)).

-endif.
