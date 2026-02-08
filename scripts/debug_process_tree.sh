#!/usr/bin/env bash
# Process tree visualization.
#
# Usage:
#   ./scripts/debug_process_tree.sh [process_name_or_pid]
#   ./scripts/debug_process_tree.sh gen_yawl
#
set -e
cd "$(dirname "$0")/.."

PROCESS="${1:-init}"

rebar3 as test compile >/dev/null 2>&1
PA=$(rebar3 as test path 2>/dev/null)
PA_ARGS=""
for p in $PA; do
    PA_ARGS="$PA_ARGS -pa $p"
done

cat <<EOF | erl -noshell $PA_ARGS
-module(process_tree_script).
-export([run/0]).

run() ->
    Pid = case '$PROCESS' of
        "init" -> list_to_pid("<0.0.0>");
        Name when is_list(Name) ->
            case whereis(list_to_atom(Name)) of
                undefined -> list_to_pid("<0.0.0>");
                P -> P
            end
    end,
    Tree = cre_debug_advanced:inspect_process_tree(Pid),
    io:format("Process Tree~n"),
    io:format("============~n"),
    print_tree(Tree, 0).

print_tree(#{pid := Pid, children := Children} = Node, Indent) ->
    IndentStr = lists:duplicate(Indent * 2, $ ),
    io:format("~s~p~n", [IndentStr, Pid]),
    [print_tree(Child, Indent + 1) || Child <- Children];
print_tree(#{pid := Pid}, Indent) ->
    IndentStr = lists:duplicate(Indent * 2, $ ),
    io:format("~s~p~n", [IndentStr, Pid]);
print_tree(Other, Indent) ->
    IndentStr = lists:duplicate(Indent * 2, $ ),
    io:format("~s~p~n", [IndentStr, Other]).

run().
EOF
