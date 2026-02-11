#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa _build/default/lib/cre/ebin -pa _build/default/lib/*/ebin
%%
%% CRE dot command - Main entry point for CLI subcommands
%%
%% Usage: ./dot <command> [options]
%%
%% Commands:
%%   validate   - Run validation checks on workflow specifications
%%   sync       - Synchronize evidence with remote storage
%%   evidence   - Collect evidence pack for audit
%%   bench      - Run benchmarks with regression detection
%%   prove      - Run proof verification (soundness, liveness, boundedness)
%%   andon      - Display andon gate status (PASS/FAIL)
%%
%% @end

-mode(compile).

-define(EXIT_SUCCESS, 0).
-define(EXIT_ERROR, 1).
-define(EXIT_ANDON_FAIL, 2).

%% Main entry point
main(Args) ->
    try
        case Args of
            [] ->
                print_usage(),
                halt(?EXIT_ERROR);
            [Command | Rest] ->
                case dispatch(Command, Rest) of
                    ok -> halt(?EXIT_SUCCESS);
                    {ok, _} -> halt(?EXIT_SUCCESS);
                    {error, Reason} ->
                        io:format(standard_error, "Error: ~p~n", [Reason]),
                        halt(?EXIT_ERROR);
                    {fail, Reason} ->
                        io:format("FAIL: ~p~n", [Reason]),
                        halt(?EXIT_ANDON_FAIL);
                    pass ->
                        io:format("PASS~n"),
                        halt(?EXIT_SUCCESS);
                    fail ->
                        io:format("FAIL~n"),
                        halt(?EXIT_ANDON_FAIL);
                    {pass, _} ->
                        io:format("PASS~n"),
                        halt(?EXIT_SUCCESS);
                    {fail, _} ->
                        io:format("FAIL~n"),
                        halt(?EXIT_ANDON_FAIL)
                end
        end
    catch
        Type:Error:Stack ->
            io:format(standard_error, "Crash: ~p:~p~n~p~n", [Type, Error, Stack]),
            halt(?EXIT_ERROR)
    end.

%% Dispatch to subcommands
dispatch("validate", Args) ->
    dot_validate:run(Args);
dispatch("sync", Args) ->
    dot_sync:run(Args);
dispatch("evidence", Args) ->
    dot_evidence:run(Args);
dispatch("bench", Args) ->
    dot_bench:run(Args);
dispatch("prove", Args) ->
    dot_prove:run(Args);
dispatch("andon", Args) ->
    dot_andon:run(Args);
dispatch(Command, _Args) ->
    io:format(standard_error, "Unknown command: ~s~n", [Command]),
    print_usage(),
    error.

%% Print usage information
print_usage() ->
    io:format(standard_error,
        "CRE dot command - Line Controller Factory CLI~n"
        "~n"
        "Usage: ./dot <command> [options]~n"
        "~n"
        "Commands:~n"
        "  validate   Run validation checks on workflow specifications~n"
        "  sync       Synchronize evidence with remote storage~n"
        "  evidence   Collect evidence pack for audit~n"
        "  bench      Run benchmarks with regression detection~n"
        "  prove      Run proof verification (soundness, liveness)~n"
        "  andon      Display andon gate status (PASS/FAIL)~n"
        "~n"
        "Options:~n"
        "  --help, -h     Show this help message~n"
        "  --verbose, -v  Enable verbose output~n"
        "  --output, -o   Specify output file~n"
        "~n"
        "Examples:~n"
        "  ./dot validate my_workflow.erl~n"
        "  ./dot bench --iterations 1000~n"
        "  ./dot prove --soundness --liveness~n"
        "  ./dot andon~n"
        "~n",
        []).
