%% CLI tool for FIBO linter
-module(fibo_linter_cli).
-export([main/1]).

-include("fibo_linter.hrl").

%% @doc Command-line entry point for FIBO linter
-spec main([string()]) -> ok.
main([]) ->
    print_usage(),
    halt(1);
main(["lint", OntologyFile]) ->
    lint_file(OntologyFile, "docs/FIBO_ALIGNMENT_REPORT.md");
main(["lint", OntologyFile, OutputFile]) ->
    lint_file(OntologyFile, OutputFile);
main(["check-term", Term]) ->
    check_term(Term);
main(_) ->
    print_usage(),
    halt(1).

lint_file(OntologyFile, OutputFile) ->
    io:format("FIBO Linter - Analyzing ~s~n", [OntologyFile]),
    io:format("----------------------------------------~n"),

    case fibo_linter:lint_file(OntologyFile) of
        {ok, Result} ->
            io:format("Total terms: ~B~n", [Result#lint_result.total_terms]),
            io:format("FIBO-aligned: ~B~n", [Result#lint_result.fibo_aligned]),
            io:format("Undefined terms: ~B~n",
                     [length(Result#lint_result.undefined_terms)]),
            io:format("Warnings: ~B~n", [length(Result#lint_result.warnings)]),
            io:format("~n"),

            case fibo_linter:generate_report(Result, OutputFile) of
                ok ->
                    io:format("✓ Report generated: ~s~n", [OutputFile]),

                    %% Print summary of undefined terms
                    case Result#lint_result.undefined_terms of
                        [] ->
                            io:format("~n✓ All terms are FIBO-aligned!~n");
                        Terms ->
                            io:format("~n⚠️  Undefined terms requiring FIBO alignment:~n"),
                            lists:foreach(
                                fun(#term_info{term = T, suggestion = S}) ->
                                    case S of
                                        undefined ->
                                            io:format("  - ~s (no suggestion)~n", [T]);
                                        Sugg ->
                                            io:format("  - ~s → ~s~n", [T, Sugg])
                                    end
                                end,
                                lists:sublist(Terms, 10)
                            ),
                            case length(Terms) > 10 of
                                true ->
                                    io:format("  ... and ~B more (see report)~n",
                                             [length(Terms) - 10]);
                                false ->
                                    ok
                            end
                    end,
                    halt(0);
                {error, Reason} ->
                    io:format("Error generating report: ~p~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format("Error linting file: ~p~n", [Reason]),
            halt(1)
    end.

check_term(Term) ->
    io:format("Checking term: ~s~n", [Term]),
    case fibo_linter:check_term(Term) of
        {ok, {Namespace, FiboTerm}} ->
            io:format("✓ FIBO-aligned: ~s:~s~n", [Namespace, FiboTerm]),
            halt(0);
        {error, not_fibo} ->
            case fibo_linter:suggest_fibo_term(Term) of
                {ok, Suggestion} ->
                    io:format("⚠️  Not FIBO-aligned~n"),
                    io:format("Suggestion: ~s~n", [Suggestion]),
                    halt(1);
                {error, no_suggestion} ->
                    io:format("⚠️  Not FIBO-aligned (no suggestion available)~n"),
                    halt(1)
            end
    end.

print_usage() ->
    io:format("~nFIBO Ontology Linter~n"),
    io:format("====================~n~n"),
    io:format("Usage:~n"),
    io:format("  escript fibo_linter_cli.erl lint <ontology-file> [output-file]~n"),
    io:format("  escript fibo_linter_cli.erl check-term <term>~n~n"),
    io:format("Examples:~n"),
    io:format("  escript fibo_linter_cli.erl lint ontology/f5_line_control.ttl~n"),
    io:format("  escript fibo_linter_cli.erl check-term LoanApplication~n~n").
