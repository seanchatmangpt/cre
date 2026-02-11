#!/usr/bin/env escript
%%% Adversarial Validator v2
%%% Uses behavior-based validation framework with ontology-driven proof selection

-mode(compile).

main([]) ->
    io:format("~n╔═══════════════════════════════════════════════════════════╗~n"),
    io:format("║   ADVERSARIAL VALIDATOR v2.0                              ║~n"),
    io:format("║   Behavior-Based • Ontology-Driven • Receipt-Generating   ║~n"),
    io:format("╚═══════════════════════════════════════════════════════════╝~n~n"),

    %% Add all app paths
    code:add_pathsz(filelib:wildcard("apps/*/ebin")),

    %% Run orchestrator
    case validation_orchestrator:run_all_validators() of
        {ok, Results} ->
            Summary = maps:get(summary, Results, #{}),
            Failed = maps:get(failed, Summary, 0),

            case Failed of
                0 ->
                    io:format("~n✓ ALL VALIDATIONS PASSED~n~n"),
                    halt(0);
                _ ->
                    io:format("~n✗ SOME VALIDATIONS FAILED (~p)~n~n", [Failed]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("~n✗ VALIDATION ERROR: ~p~n~n", [Reason]),
            halt(1)
    end.
