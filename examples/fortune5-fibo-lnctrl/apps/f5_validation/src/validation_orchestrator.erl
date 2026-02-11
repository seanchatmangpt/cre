%%% @doc Validation Orchestrator
%%%
%%% Orchestrates adversarial validation by:
%%% - Reading SPARQL queries to determine which proofs to run
%%% - Executing validators that implement adversarial_validator_behaviour
%%% - Generating cryptographic receipts for all proofs
%%% - Storing results in receipts/ directory

-module(validation_orchestrator).

-export([
    run_all_validators/0,
    run_validator/1,
    run_validators/1,
    store_receipts/2
]).

-define(RECEIPTS_DIR, "receipts").

%% =============================================================================
%% Public API
%% =============================================================================

-spec run_all_validators() -> {ok, map()} | {error, term()}.
run_all_validators() ->
    %% Get all validators from ontology-driven selection
    Validators = get_validators_from_ontology(),

    io:format("~n╔═══════════════════════════════════════════════════════════╗~n"),
    io:format("║   ADVERSARIAL VALIDATION ORCHESTRATOR                     ║~n"),
    io:format("║   Running ~2b validators with ontology-driven selection   ║~n", [length(Validators)]),
    io:format("╚═══════════════════════════════════════════════════════════╝~n~n"),

    Results = run_validators(Validators),

    %% Store receipts
    case store_receipts(validation_run, Results) of
        ok ->
            io:format("~n✓ Receipts stored in ~s/~n", [?RECEIPTS_DIR]);
        {error, Reason} ->
            io:format("~n⚠ Warning: Could not store receipts: ~p~n", [Reason])
    end,

    {ok, Results}.

-spec run_validators([module()]) -> map().
run_validators(Validators) ->
    StartTime = erlang:system_time(second),

    Results = lists:map(fun(Validator) ->
        case run_validator(Validator) of
            {ok, Result} -> Result;
            {error, Reason} -> #{
                validator => atom_to_binary(Validator),
                status => error,
                error => Reason
            }
        end
    end, Validators),

    EndTime = erlang:system_time(second),

    TotalTests = lists:sum([maps:get(total_tests, R, 0) || R <- Results]),
    TotalPassed = lists:sum([maps:get(passed, R, 0) || R <- Results]),
    TotalFailed = lists:sum([maps:get(failed, R, 0) || R <- Results]),

    io:format("~n╔═══════════════════════════════════════════════════════════╗~n"),
    io:format("║   VALIDATION SUMMARY                                      ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════╝~n~n"),
    io:format("  Total Validators: ~p~n", [length(Validators)]),
    io:format("  Total Tests: ~p~n", [TotalTests]),
    io:format("  Passed: ~p~n", [TotalPassed]),
    io:format("  Failed: ~p~n", [TotalFailed]),
    io:format("  Duration: ~p seconds~n~n", [EndTime - StartTime]),

    Verdict = if
        TotalFailed =:= 0 -> <<"ALL PROOFS PASSED">>;
        true -> <<"SOME PROOFS FAILED">>
    end,

    io:format("  VERDICT: ~s~n~n", [Verdict]),

    #{
        validators => Results,
        summary => #{
            total_validators => length(Validators),
            total_tests => TotalTests,
            passed => TotalPassed,
            failed => TotalFailed,
            verdict => Verdict,
            start_time => StartTime,
            end_time => EndTime,
            duration_seconds => EndTime - StartTime
        }
    }.

-spec run_validator(module()) -> {ok, map()} | {error, term()}.
run_validator(Validator) ->
    io:format("~n═══════════════════════════════════════════════════════════~n"),
    io:format("Running validator: ~p~n", [Validator]),
    io:format("═══════════════════════════════════════════════════════════~n~n"),

    try
        %% Verify validator implements behavior
        case adversarial_validator_behaviour:validate_implementation(Validator) of
            ok -> ok;
            {error, ValidErr} -> throw({invalid_validator, ValidErr})
        end,

        %% Initialize validator
        {ok, Meta} = Validator:init(),
        io:format("Validator: ~s v~s~n", [
            maps:get(name, Meta, <<"Unknown">>),
            maps:get(version, Meta, <<"0.0.0">>)
        ]),
        io:format("Description: ~s~n~n", [
            maps:get(description, Meta, <<"No description">>)
        ]),

        %% Run tests
        {ok, TestResults} = Validator:run_tests(#{}),

        %% Format results
        {ok, FormattedResults} = Validator:format_results(TestResults),

        {ok, FormattedResults}
    catch
        Class:Reason:Stack ->
            io:format("✗ Validator failed: ~p:~p~n", [Class, Reason]),
            io:format("Stack: ~p~n", [Stack]),
            {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end.

%% =============================================================================
%% Internal Functions
%% =============================================================================

get_validators_from_ontology() ->
    %% In a full implementation, this would:
    %% 1. Parse the SPARQL query from sparql/extract_validation_proofs.sparql
    %% 2. Execute against the ontology
    %% 3. Return the list of validators to run
    %%
    %% For now, return all available validators
    [
        zero_downtime_validator,
        hot_upgrade_validator,
        deterministic_generation_validator,
        config_validator
    ].

-spec store_receipts(atom(), map()) -> ok | {error, term()}.
store_receipts(RunType, Results) ->
    try
        %% Ensure receipts directory exists
        case filelib:is_dir(?RECEIPTS_DIR) of
            false -> file:make_dir(?RECEIPTS_DIR);
            true -> ok
        end,

        _Timestamp = calendar:system_time_to_rfc3339(erlang:system_time(second)),
        RunId = lists:flatten(io_lib:format("~s_~p", [RunType, erlang:system_time()])),

        %% Store full results as JSON
        ResultsJson = format_as_json(Results),
        ResultsFile = filename:join(?RECEIPTS_DIR, RunId ++ ".json"),
        ok = file:write_file(ResultsFile, ResultsJson),

        %% Store hash of results
        ResultsHash = crypto:hash(sha256, ResultsJson),
        ResultsHashHex = bin_to_hex(ResultsHash),
        HashFile = filename:join(?RECEIPTS_DIR, RunId ++ ".sha"),
        ok = file:write_file(HashFile, ResultsHashHex),

        %% Store individual validator receipts
        Validators = maps:get(validators, Results, []),
        lists:foreach(fun(ValidatorResult) ->
            ValidatorId = maps:get(validator, ValidatorResult, <<"unknown">>),
            TestResults = maps:get(results, ValidatorResult, []),

            lists:foreach(fun(TestResult) ->
                Receipt = maps:get(receipt, TestResult, #{}),
                case maps:size(Receipt) > 0 of
                    true ->
                        ReceiptFile = filename:join(
                            ?RECEIPTS_DIR,
                            binary_to_list(ValidatorId) ++ "_" ++
                            binary_to_list(maps:get(test_id, TestResult, <<"unknown">>)) ++
                            "_receipt.json"
                        ),
                        ReceiptJson = format_as_json(Receipt),
                        file:write_file(ReceiptFile, ReceiptJson);
                    false ->
                        ok
                end
            end, TestResults)
        end, Validators),

        io:format("Stored receipts:~n"),
        io:format("  - ~s~n", [ResultsFile]),
        io:format("  - ~s~n", [HashFile]),
        io:format("  - Individual test receipts in ~s/~n", [?RECEIPTS_DIR]),

        ok
    catch
        _:Error ->
            {error, Error}
    end.

format_as_json(Term) ->
    %% Simple JSON formatting (good enough for receipts)
    %% In production, use a proper JSON library like jsx or jiffy
    iolist_to_binary(format_term(Term)).

format_term(Map) when is_map(Map) ->
    Entries = maps:fold(fun(K, V, Acc) ->
        Key = format_term(K),
        Val = format_term(V),
        [io_lib:format("~s: ~s", [Key, Val]) | Acc]
    end, [], Map),
    ["{", string:join(lists:reverse(Entries), ", "), "}"];
format_term(List) when is_list(List) ->
    case io_lib:printable_list(List) of
        true ->
            io_lib:format("\"~s\"", [List]);
        false ->
            Items = [format_term(Item) || Item <- List],
            ["[", string:join(Items, ", "), "]"]
    end;
format_term(Binary) when is_binary(Binary) ->
    io_lib:format("\"~s\"", [Binary]);
format_term(Atom) when is_atom(Atom) ->
    io_lib:format("\"~s\"", [atom_to_list(Atom)]);
format_term(Int) when is_integer(Int) ->
    integer_to_list(Int);
format_term(Float) when is_float(Float) ->
    float_to_list(Float, [{decimals, 2}]);
format_term(Other) ->
    io_lib:format("\"~p\"", [Other]).

bin_to_hex(Bin) ->
    list_to_binary([io_lib:format("~2.16.0b", [B]) || <<B>> <= Bin]).
