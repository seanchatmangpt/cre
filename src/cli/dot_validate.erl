%% -*- erlang -*-
%% @doc dot validate - Run validation checks on workflow specifications
-module(dot_validate).
-export([run/1]).
-export([validate_module/1, validate_spec/1]).

%% API
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
                            do_validate(Target, Opts);
                        [] ->
                            io:format(standard_error, "Error: No target specified~n~n", []),
                            print_help(),
                            {error, no_target};
                        _ ->
                            io:format(standard_error, "Error: Too many targets~n~n", []),
                            {error, too_many_targets}
                    end
            end;
        {error, Reason} ->
            io:format(standard_error, "Error: ~p~n", [Reason]),
            {error, Reason}
    end.

-spec validate_module(module()) -> {ok, [map()]} | {error, [map()]}.
validate_module(Module) when is_atom(Module) ->
    case code:ensure_loaded(Module) of
        {module, Module} ->
            case wfnet_validate:validate_workflow(Module) of
                {ok, Warnings} ->
                    {ok, Warnings};
                {error, Errors} ->
                    {error, Errors}
            end;
        {error, Reason} ->
            {error, [#{category => structural,
                       severity => error,
                       message => list_to_binary(io_lib:format("Failed to load module: ~p", [Reason])),
                       code => module_load_failed}]}
    end.

-spec validate_spec(map()) -> {ok, [map()]} | {error, [map()]}.
validate_spec(Spec) ->
    wfnet_validate:validate_spec(Spec).

%% Internal
do_validate(Target, Opts) ->
    Module = target_to_module(Target),
    case validate_module(Module) of
        {ok, Warnings} ->
            Format = proplists:get_value(format, Opts, text),
            output_results(ok, Warnings, Format, Opts),
            case proplists:get_value(warnings_as_errors, Opts, false) of
                true when Warnings =/= [] -> fail;
                _ -> pass
            end;
        {error, Errors} ->
            Format = proplists:get_value(format, Opts, text),
            output_results(error, Errors, Format, Opts),
            fail
    end.

target_to_module(Target) ->
    case filename:extension(Target) of
        ".erl" ->
            Base = filename:basename(Target, ".erl"),
            erlang:list_to_existing_atom(Base);
        _ ->
            erlang:list_to_existing_atom(Target)
    end.

output_results(Result, Issues, Format, Opts) ->
    case Format of
        json ->
            output_json(Result, Issues);
        text ->
            output_text(Result, Issues, Opts)
    end.

output_text(Result, Issues, Opts) ->
    Verbose = proplists:get_value(verbose, Opts, false),
    case Result of
        ok ->
            case Issues of
                [] ->
                    io:format("Validation PASSED~n");
                _ ->
                    io:format("Validation PASSED with ~p warnings~n", [length(Issues)]),
                    maybe_verbose(Verbose, fun() -> print_issues(Issues) end)
            end;
        error ->
            io:format("Validation FAILED with ~p errors~n", [length(Issues)]),
            print_issues(Issues)
    end.

output_json(Result, Issues) ->
    Status = result_to_status(Result),
    JsonObj = #{
        status => Status,
        issues => lists:map(fun issue_to_json/1, Issues),
        count => length(Issues)
    },
    io:format("~s~n", [jsone:encode(JsonObj)]).

result_to_status(ok) -> <<"pass">>;
result_to_status(error) -> <<"fail">>.

issue_to_json(#{severity := Sev, message := Msg} = Issue) ->
    Base = #{severity => Sev, message => Msg},
    Base2 = case maps:get(category, Issue, undefined) of
        undefined -> Base;
        Cat -> Base#{category => Cat}
    end,
    Base3 = case maps:get(code, Issue, undefined) of
        undefined -> Base2;
        Code -> Base2#{code => Code}
    end,
    Base4 = case maps:get(location, Issue, undefined) of
        undefined -> Base3;
        Loc -> Base3#{location => Loc}
    end,
    Base4.

print_issues(Issues) ->
    lists:foreach(fun(Issue) ->
        Sev = maps:get(severity, Issue, error),
        Msg = maps:get(message, Issue, <<>>),
        Code = maps:get(code, Issue, unknown),
        Loc = maps:get(location, Issue, <<>>),
        SevStr = case Sev of error -> "[ERROR]"; warning -> "[WARN]" end,
        LocStr = case Loc of
            <<>> -> "";
            _ -> io_lib:format(" at ~s", [Loc])
        end,
        io:format("  ~s ~s (~s)~s~n", [SevStr, Msg, Code, LocStr])
    end, Issues).

maybe_verbose(true, Fun) -> Fun();
maybe_verbose(false, _Fun) -> ok.

opt_spec() ->
    [
        {help, $h, "help", undefined, "Show this help message"},
        {verbose, $v, "verbose", undefined, "Enable verbose output"},
        {format, $f, "format", {string, "text"}, "Output format (text|json)"},
        {warnings_as_errors, $w, "warnings-as-errors", undefined, "Treat warnings as errors"}
    ].

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
parse_opts(["--warnings-as-errors" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{warnings_as_errors, true} | Acc], Positional);
parse_opts(["-w" | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{warnings_as_errors, true} | Acc], Positional);
parse_opts(["--format", Format | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{format, Format} | Acc], Positional);
parse_opts(["-f", Format | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{format, Format} | Acc], Positional);
parse_opts([Arg | Rest], OptSpec, Acc, Positional) ->
    case Arg of
        "-" ++ _ -> {error, {unknown_option, Arg}};
        _ -> parse_opts(Rest, OptSpec, Acc, [Arg | Positional])
    end.

print_help() ->
    io:format(
        "dot validate - Run validation checks on workflow specifications~n"
        "~n"
        "Usage: ./dot validate <module_or_file> [options]~n"
        "~n"
        "Options:~n"
        "  --help, -h             Show this help message~n"
        "  --verbose, -v          Enable verbose output~n"
        "  --format, -f <fmt>     Output format (text|json)~n"
        "  --warnings-as-errors   Treat warnings as errors~n"
        "~n"
        "Examples:~n"
        "  ./dot validate my_workflow~n"
        "  ./dot validate src/workflows/payment.erl --verbose~n"
        "  ./dot validate my_workflow --format=json~n"
        "~n").
