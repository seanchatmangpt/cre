%% -*- erlang -*-
%% @doc dot evidence - Collect evidence pack for audit
%%
%% Collects all evidence artifacts into a pack for audit purposes:
%% - Receipt logs with hash chains
%% - Andon status snapshots
%% - Benchmark results
%% - Proof verification results
%% - System configuration
%%
%% Usage: ./dot evidence [options]
%%
%% Options:
%%   --output=<path>  Output directory for evidence pack (default: ./evidence-pack)
%%   --format=<fmt>   Output format: tar, zip, directory
%%   --include=<type> Include specific evidence types
%%   --since=<date>   Only include evidence since date
%%
%% @end

-module(dot_evidence).
-export([run/1]).
-export([collect/1, collect/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Run evidence collection command from CLI
-spec run([string()]) -> ok | {error, term()}.
run(Args) ->
    OptSpec = opt_spec(),
    case parse_opts(Args, OptSpec) of
        {ok, Opts, _Positional} ->
            case proplists:get_value(help, Opts) of
                true ->
                    print_help(),
                    ok;
                _ ->
                    OutputDir = proplists:get_value(output, Opts, "evidence-pack"),
                    Format = proplists:get_value(format, Opts, directory),
                    Include = proplists:get_value(include, Opts, all),
                    Since = proplists:get_value(since, Opts, undefined),
                    do_collect(OutputDir, Format, Include, Since);
                _ ->
                    ok
            end;
        {error, Reason} ->
            io:format(standard_error, "Error: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Collect evidence pack
-spec collect(file:filename()) -> {ok, map()} | {error, term()}.
collect(OutputDir) ->
    collect(OutputDir, #{}).

%% @doc Collect evidence with options
-spec collect(file:filename(), map()) -> {ok, map()} | {error, term()}.
collect(OutputDir, Options) ->
    OutputDir2 = case OutputDir of
        undefined -> "evidence-pack";
        _ -> OutputDir
    end,
    do_collect(OutputDir2,
        maps:get(format, Options, directory),
        maps:get(include, Options, all),
        maps:get(since, Options, undefined)).

%%====================================================================
%% Internal Functions
%%====================================================================

do_collect(OutputDir, Format, Include, Since) ->
    io:format("Collecting evidence pack...~n"),
    io:format("  Output: ~s~n", [OutputDir]),
    io:format("  Format: ~p~n", [Format]),
    io:format("  Include: ~p~n", [Include]),
    io:format("  Since: ~p~n", [Since]),

    %% Create output directory
    ok = filelib:ensure_dir(OutputDir ++ "/"),

    %% Collect evidence items
    Items = collect_items(Include, Since, OutputDir),

    %% Write manifest
    Manifest = create_manifest(Items),
    ok = file:write_file(OutputDir ++ "/MANIFEST.json", manifest_to_json(Manifest)),

    %% Finalize based on format
    case Format of
        directory ->
            io:format("Evidence pack created: ~s~n", [OutputDir]),
            io:format("  Items: ~p~n", [length(Items)]),
            ok;
        tar ->
            create_tarball(OutputDir, OutputDir ++ ".tar.gz");
        zip ->
            create_zip(OutputDir, OutputDir ++ ".zip")
    end.

collect_items(Include, Since, OutputDir) ->
    Items0 = maybe_collect_receipts(Include, Since, OutputDir),
    Items1 = Items0 ++ maybe_collect_andon(Include, Since, OutputDir),
    Items2 = Items1 ++ maybe_collect_benchmarks(Include, Since, OutputDir),
    Items3 = Items2 ++ maybe_collect_proofs(Include, Since, OutputDir),
    Items4 = Items3 ++ maybe_collect_config(Include, Since, OutputDir),
    Items4.

maybe_collect_receipts(all, Since, OutputDir) ->
    collect_receipts(Since, OutputDir);
maybe_collect_receipts(IncludeList, Since, OutputDir) when is_list(IncludeList) ->
    case lists:member(receipts, IncludeList) of
        true -> collect_receipts(Since, OutputDir);
        false -> []
    end;
maybe_collect_receipts(_Include, _Since, _OutputDir) ->
    [].

collect_receipts(Since, OutputDir) ->
    LogPath = "evidence/receipt.log",
    case file:read_file_info(LogPath) of
        {ok, _} ->
            TargetPath = OutputDir ++ "/receipts.log",
            case Since of
                undefined ->
                    %% Copy entire log
                    {ok, _} = file:copy(LogPath, TargetPath),
                    [{receipts, TargetPath, filelib:file_size(TargetPath)}];
                Date ->
                    %% Filter by date
                    filter_log_by_date(LogPath, TargetPath, Date)
            end;
        {error, _} ->
            io:format("  No receipt log found~n"),
            []
    end.

filter_log_by_date(SourcePath, TargetPath, SinceDate) ->
    %% TODO: Implement date filtering
    {ok, _} = file:copy(SourcePath, TargetPath),
    [{receipts, TargetPath, filelib:file_size(TargetPath)}].

maybe_collect_andon(all, Since, OutputDir) ->
    collect_andon(Since, OutputDir);
maybe_collect_andon(IncludeList, Since, OutputDir) when is_list(IncludeList) ->
    case lists:member(andon, IncludeList) of
        true -> collect_andon(Since, OutputDir);
        false -> []
    end;
maybe_collect_andon(_Include, _Since, _OutputDir) ->
    [].

collect_andon(_Since, OutputDir) ->
    case ln_receipt_andon:new_andon() of
        {ok, AndonHandle} ->
            {Color, Details} = ln_receipt_andon:status(AndonHandle),
            StatusJson = jsone:encode(#{
                color => Color,
                details => Details,
                timestamp => erlang:system_time(millisecond)
            }),
            TargetPath = OutputDir ++ "/andon-status.json",
            ok = file:write_file(TargetPath, StatusJson),
            [{andon, TargetPath, byte_size(StatusJson)}];
        {error, Reason} ->
            io:format("  Failed to collect andon status: ~p~n", [Reason]),
            []
    end.

maybe_collect_benchmarks(all, Since, OutputDir) ->
    collect_benchmarks(Since, OutputDir);
maybe_collect_benchmarks(IncludeList, Since, OutputDir) when is_list(IncludeList) ->
    case lists:member(benchmarks, IncludeList) of
        true -> collect_benchmarks(Since, OutputDir);
        false -> []
    end;
maybe_collect_benchmarks(_Include, _Since, _OutputDir) ->
    [].

collect_benchmarks(_Since, OutputDir) ->
    BenchPath = "evidence/benchmarks.json",
    case file:read_file_info(BenchPath) of
        {ok, _} ->
            TargetPath = OutputDir ++ "/benchmarks.json",
            {ok, _} = file:copy(BenchPath, TargetPath),
            [{benchmarks, TargetPath, filelib:file_size(TargetPath)}];
        {error, _} ->
            io:format("  No benchmark results found~n"),
            []
    end.

maybe_collect_proofs(all, Since, OutputDir) ->
    collect_proofs(Since, OutputDir);
maybe_collect_proofs(IncludeList, Since, OutputDir) when is_list(IncludeList) ->
    case lists:member(proofs, IncludeList) of
        true -> collect_proofs(Since, OutputDir);
        false -> []
    end;
maybe_collect_proofs(_Include, _Since, _OutputDir) ->
    [].

collect_proofs(_Since, OutputDir) ->
    ProofPath = "evidence/proofs.json",
    case file:read_file_info(ProofPath) of
        {ok, _} ->
            TargetPath = OutputDir ++ "/proofs.json",
            {ok, _} = file:copy(ProofPath, TargetPath),
            [{proofs, TargetPath, filelib:file_size(ProofPath)}];
        {error, _} ->
            io:format("  No proof results found~n"),
            []
    end.

maybe_collect_config(all, Since, OutputDir) ->
    collect_config(Since, OutputDir);
maybe_collect_config(IncludeList, Since, OutputDir) when is_list(IncludeList) ->
    case lists:member(config, IncludeList) of
        true -> collect_config(Since, OutputDir);
        false -> []
    end;
maybe_collect_config(_Include, _Since, _OutputDir) ->
    [].

collect_config(_Since, OutputDir) ->
    %% Collect system configuration
    Config = #{
        node => node(),
        erlang_version => erlang:system_info(otp_release),
        cre_version => "0.3.0",
        timestamp => erlang:system_time(millisecond)
    },
    ConfigJson = jsone:encode(Config),
    TargetPath = OutputDir ++ "/config.json",
    ok = file:write_file(TargetPath, ConfigJson),
    [{config, TargetPath, byte_size(ConfigJson)}].

create_manifest(Items) ->
    #{
        created => erlang:system_time(millisecond),
        version => "1.0",
        items => lists:map(fun({Type, Path, Size}) ->
            #{
                type => Type,
                path => filename:basename(Path),
                size => Size,
                hash => hash_file(Path)
            }
        end, Items)
    }.

manifest_to_json(Manifest) ->
    jsone:encode(Manifest).

hash_file(Path) ->
    case file:read_file(Path) of
        {ok, Data} ->
            <<Hash:256>> = crypto:hash(sha256, Data),
            lists:flatten(io_lib:format("~64.16.0b", [Hash]));
        {error, _} ->
            "unknown"
    end.

create_tarball(SourceDir, OutputFile) ->
    %% Simple tarball creation using tar command
    BaseName = filename:basename(SourceDir),
    DirName = filename:dirname(SourceDir),
    Cmd = io_lib:format("tar -czf ~s -C ~s ~s", [OutputFile, DirName, BaseName]),
    case os:cmd(Cmd) of
        [] ->
            io:format("Evidence pack created: ~s~n", [OutputFile]),
            ok;
        Error ->
            {error, {tar_failed, Error}}
    end.

create_zip(SourceDir, OutputFile) ->
    %% Simple zip creation using zip command
    Cmd = io_lib:format("zip -r ~s ~s", [OutputFile, SourceDir]),
    case os:cmd(Cmd) of
        [] ->
            io:format("Evidence pack created: ~s~n", [OutputFile]),
            ok;
        Error ->
            {error, {zip_failed, Error}}
    end.

opt_spec() ->
    [
        {help, $h, "help", undefined, "Show this help message"},
        {output, $o, "output", {string, "evidence-pack"}, "Output directory"},
        {format, $f, "format", {string, "directory"}, "Output format (tar|zip|directory)"},
        {include, $i, "include", {string, "all"}, "Include specific types"},
        {since, $s, "since", undefined, "Only include evidence since date"}
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
parse_opts(["--output", Output | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{output, Output} | Acc], Positional);
parse_opts(["-o", Output | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{output, Output} | Acc], Positional);
parse_opts(["--format", Format | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{format, Format} | Acc], Positional);
parse_opts(["-f", Format | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{format, Format} | Acc], Positional);
parse_opts(["--include", Include | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{include, Include} | Acc], Positional);
parse_opts(["-i", Include | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{include, Include} | Acc], Positional);
parse_opts(["--since", Since | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{since, Since} | Acc], Positional);
parse_opts(["-s", Since | Rest], OptSpec, Acc, Positional) ->
    parse_opts(Rest, OptSpec, [{since, Since} | Acc], Positional);
parse_opts([Arg | Rest], OptSpec, Acc, Positional) ->
    case Arg of
        "-" ++ _ -> {error, {unknown_option, Arg}};
        _ -> parse_opts(Rest, OptSpec, Acc, [Arg | Positional])
    end.

print_help() ->
    io:format(
        "dot evidence - Collect evidence pack for audit~n"
        "~n"
        "Usage: ./dot evidence [options]~n"
        "~n"
        "Options:~n"
        "  --help, -h          Show this help message~n"
        "  --output, -o <dir>  Output directory (default: ./evidence-pack)~n"
        "  --format, -f <fmt>  Output format: tar, zip, directory~n"
        "  --include, -i <typ> Include specific types (receipts,andon,benchmarks,proofs,config)~n"
        "  --since, -s <date>  Only include evidence since date~n"
        "~n"
        "Examples:~n"
        "  ./dot evidence~n"
        "  ./dot evidence --output=/tmp/audit-pack~n"
        "  ./dot evidence --format=tar --include=receipts,proofs~n"
        "~n").
