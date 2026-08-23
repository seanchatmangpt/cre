%% -*- erlang -*-
%% @doc dot prove - Run proof verification (soundness, liveness, boundedness)
%%
%% Executes formal proof verification on workflow specifications:
%% - Soundness proofs (option to complete, proper completion)
%% - Liveness proofs (no dead transitions, no deadlocks)
%% - Boundedness proofs (k-bounded places)
%% - Structural properties (connectedness, no isolated nodes)
%%
%% Usage: ./dot prove <module_or_spec> [options]
%%
%% Options:
%%   --soundness      Verify soundness properties
%%   --liveness       Verify liveness properties
%%   --boundedness    Verify boundedness properties
%%   --structural     Verify structural properties
%%   --all            Verify all properties (default)
%%   --format=<fmt>   Output format (text, json)
%%
%% @end

-module(dot_prove).
-export([run/1]).
-export([prove/1, prove/2]).

%%====================================================================
%% Types
%%====================================================================

-type proof_type() :: soundness | liveness | boundedness | structural.
-type proof_result() :: #{
    type => proof_type(),
    status => pass | fail,
    details => map()
}.
-type prove_options() :: #{
    types => [proof_type()],
    format => text | json,
    verbose => boolean()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Run proof command from CLI
-spec run([string()]) -> ok | {error, term()} | pass | fail.
run(Args) ->
    OptSpec = opt_spec(),
    case parse_opts(Args, OptSpec) of
        {ok, Opts, Positional} ->
            case proplists:get_value(help, Opts) of
                true ->
                    print_help(),
                    ok;
                _ ->
                    case Positional of
                        [Target] ->
                            Types = parse_types(Opts),
                            Format = proplists:get_value(format, Opts, text),
                            Verbose = proplists:get_value(verbose, Opts, false),
                            do_prove(Target, Types, Format, Verbose);
                        [] ->
                            io:format(standard_error, "Error: No target specified~n"),
                            print_help(),
                            {error, no_target};
                        _ ->
                            io:format(standard_error, "Error: Too many targets~n"),
                            {error, too_many_targets}
                    end
            end;
        {error, Reason} ->
            io:format(standard_error, "Error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Prove all properties for a module
-spec prove(module()) -> {ok, [proof_result()]} | {error, term()}.
prove(Module) when is_atom(Module) ->
    prove(Module, [soundness, liveness, boundedness, structural]).

%% @doc Prove specific properties for a module
-spec prove(module(), [proof_type()]) -> {ok, [proof_result()]} | {error, term()}.
prove(Module, Types) when is_atom(Module), is_list(Types) ->
    case wfnet_validate:validate_workflow(Module) of
        {ok, []} ->
            {ok, [#{type => T, status => pass, details => #{}} || T <- Types]};
        {ok, Warnings} ->
            {ok, convert_to_proof_results(Warnings, Types)};
        {error, Errors} ->
            {ok, convert_to_proof_results(Errors, Types)}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

do_prove(Target, Types, Format, Verbose) ->
    io:format("Running proof verification...~n"),
    io:format("  Target: ~s~n", [Target]),
    io:format("  Proofs: ~p~n", [Types]),

    Module = target_to_module(Target),

    case prove(Module, Types) of
        {ok, Results} ->
            output_results(Results, Format, Verbose),
            case all_passed(Results) of
                true -> pass;
                false -> fail
            end;
        {error, Reason} ->
            io:format(standard_error, "Prove failed: ~p~n", [Reason]),
            fail
    end.

target_to_module(Target) ->
    case filename:extension(Target) of
        ".erl" ->
            Base = filename:basename(Target, ".erl"),
            list_to_atom(Base);
        _ ->
            list_to_atom(Target)
    end.

convert_to_proof_results(Issues, Types) ->
    %% Group issues by type
    IssueMap = lists:foldl(fun(Issue, Acc) ->
        Category = maps:get(category, Issue, structural),
        maps:append(Category, Issue, Acc)
    end, #{}, Issues),

    lists:map(fun(Type) ->
        TypeIssues = maps:get(type_to_category(Type), IssueMap, []),
        Status = case [E || E <- TypeIssues, maps:get(severity, E, warning) =:= error] of
            [] -> pass;
            _ -> fail
        end,
        #{type => Type, status => Status, details => #{issues => TypeIssues}}
    end, Types).

type_to_category(soundness) -> soundness_error;
type_to_category(liveness) -> deadlock_error;
type_to_category(boundedness) -> unbounded_error;
type_to_category(structural) -> structural_error.

output_results(Results, Format, Verbose) ->
    case Format of
        json ->
            output_json_results(Results);
        text ->
            output_text_results(Results, Verbose)
    end.

output_text_results(Results, Verbose) ->
    io:format("~nProof Results:~n"),
    lists:foreach(fun(#{type := Type, status := Status, details := Details}) ->
        StatusStr = case Status of pass -> "PASS"; fail -> "FAIL" end,
        io:format("  ~-12s: ~s~n", [Type, StatusStr]),

        case Verbose of
            true ->
                Issues = maps:get(issues, Details, []),
                lists:foreach(fun(Issue) ->
                    Msg = maps:get(message, Issue, <<>>),
                    io:format("    - ~s~n", [Msg])
                end, Issues);
            false ->
                ok
        end
    end, Results).

output_json_results(Results) ->
    JsonResults = lists:map(fun(#{type := Type, status := Status}) ->
        #{
            type => Type,
            status => Status
        }
    end, Results),
    Json = jsone:encode(#{
        timestamp => erlang:system_time(millisecond),
        results => JsonResults
    }),
    io:format("~s~n", [Json]).

all_passed(Results) ->
    lists:all(fun(#{status := Status}) -> Status =:= pass end, Results).

parse_types(Opts) ->
    Types = [
        {soundness, proplists:get_value(soundness, Opts, false)},
        {liveness, proplists:get_value(liveness, Opts, false)},
        {boundedness, proplists:get_value(boundedness, Opts, false)},
        {structural, proplists:get_value(structural, Opts, false)}
    ],
    ActiveTypes = [T || {T, true} <- Types],
    case ActiveTypes of
        [] -> [soundness, liveness, boundedness, structural];
        _ -> ActiveTypes
    end.

opt_spec() ->
    [
        {help, $h, "help", undefined, "Show this help message"},
        {verbose, $v, "verbose", undefined, "Enable verbose output"},
        {format, $f, "format", {string, "text"}, "Output format (text|json)"},
        {soundness, $s, "soundness", undefined, "Verify soundness"},
        {liveness, $l, "liveness", undefined, "Verify liveness"},
        {boundedness, $b, "boundedness", undefined, "Verify boundedness"},
        {structural, $t, "structural", undefined, "Verify structural"}
    ].

%% Simple option parser
parse_opts(Args, OptSpec) ->
    parse_opts(Args, OptSpec, [], []).

parse_opts([], _OptSpec, Acc, Positional) ->
    {ok, lists:reverse(Acc), lists:reverse(Positional)};
parse_opts(["--help" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{help, true} | Acc], Positional);
parse_opts(["-h" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{help, true} | Acc], Positional);
parse_opts(["--verbose" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{verbose, true} | Acc], Positional);
parse_opts(["-v" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{verbose, true} | Acc], Positional);
parse_opts(["--format", Format | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{format, Format} | Acc], Positional);
parse_opts(["-f", Format | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{format, Format} | Acc], Positional);
parse_opts(["--soundness" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{soundness, true} | Acc], Positional);
parse_opts(["-s" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{soundness, true} | Acc], Positional);
parse_opts(["--liveness" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{liveness, true} | Acc], Positional);
parse_opts(["-l" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{liveness, true} | Acc], Positional);
parse_opts(["--boundedness" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{boundedness, true} | Acc], Positional);
parse_opts(["-b" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{boundedness, true} | Acc], Positional);
parse_opts(["--structural" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{structural, true} | Acc], Positional);
parse_opts(["-t" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{structural, true} | Acc], Positional);
parse_opts([Arg | Rest], OptSpec, Acc, Positional) ->
    case Arg of
        "-" ++ _ -> {error, {unknown_option, Arg}};
        _ -> parse_opts(Rest, OptSpec, Acc, [Arg | Positional])
    end.

print_help() ->
    io:format(
        "dot prove - Run proof verification on workflow specifications~n"
        "~n"
        "Usage: ./dot prove <module_or_spec> [options]~n"
        "~n"
        "Options:~n"
        "  --help, -h          Show this help message~n"
        "  --verbose, -v       Enable verbose output~n"
        "  --format, -f <fmt>  Output format (text|json)~n"
        "  --soundness, -s     Verify soundness properties~n"
        "  --liveness, -l      Verify liveness properties~n"
        "  --boundedness, -b   Verify boundedness properties~n"
        "  --structural, -t    Verify structural properties~n"
        "~n"
        "If no proof types specified, all are verified.~n"
        "~n"
        "Examples:~n"
        "  ./dot prove my_workflow~n"
        "  ./dot prove my_workflow --soundness --liveness~n"
        "  ./dot prove my_workflow --verbose --format=json~n"
        "~n").
