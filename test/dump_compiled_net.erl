%% -*- erlang -*-
%% @doc Dump Omega compiled nets to files for inspection.
%%
%% Run via: ./scripts/dump_compiled_net.sh [output_dir]
%% Or: rebar3 run -eval "dump_compiled_net:main(), halt(0)."
-module(dump_compiled_net).
-export([main/0, main/1]).

main() ->
    main(["/tmp/omega_compiled_dump"]).

main(Args) ->
    OutputDir = case Args of
        [D | _] when is_list(D) -> D;
        [] -> "/tmp/omega_compiled_dump";
        _ -> "/tmp/omega_compiled_dump"
    end,
    YamlFile = "test/fixtures/agi_symposium_omega.yaml",
    io:format("Loading YAML: ~s~n", [YamlFile]),
    case wf_yaml_spec:from_yaml_file(YamlFile) of
        {ok, Spec} ->
            io:format("Compiling to: ~s~n", [OutputDir]),
            filelib:ensure_dir(filename:join(OutputDir, "dummy")),
            case yawl_compile:compile_to_file(Spec, #{}, OutputDir) of
                {ok, Files} ->
                    io:format("Generated ~p .erl files:~n", [length(Files)]),
                    [io:format("  ~s~n", [F]) || F <- lists:sort(Files)],
                    io:format("~nTo inspect place_lst/trsn_lst, load a module and call:~n"),
                    io:format("  Mod:place_lst().~n  Mod:trsn_lst().~n"),
                    ok;
                {error, Reason} ->
                    io:format(standard_error, "ERROR: compile_to_file failed: ~p~n", [Reason]),
                    halt(1)
            end;
        {error, Reason} ->
            io:format(standard_error, "ERROR: YAML parse failed: ~p~n", [Reason]),
            halt(1)
    end.
