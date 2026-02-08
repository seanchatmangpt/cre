#!/usr/bin/env escript
%% -*- erlang -*-
%% @doc Dump Omega compiled nets to files for inspection.
%%
%% Compiles the Omega YAML spec to .erl files and lists generated modules.
%%
%% Usage: ./scripts/dump_compiled_net.sh [output_dir]
%%        or: ./scripts/dump_compiled_net.escript [output_dir]
%%
%% Default output: /tmp/omega_compiled_dump
%% @end
main(Args) ->
    add_cre_paths(),
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

add_cre_paths() ->
    ScriptDir = filename:dirname(escript:script_name()),
    Root = filename:absname(filename:join(ScriptDir, "..")),
    Paths = [
        filename:join(Root, "_build/test/lib/cre/ebin"),
        filename:join(Root, "_build/default/lib/cre/ebin"),
        filename:join(Root, "_build/test/lib/yamerl/ebin")
    ],
    [code:add_pathz(P) || P <- Paths, filelib:is_dir(P)],
    ok.
