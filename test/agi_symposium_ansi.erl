%% -*- erlang -*-
%% @doc ANSI escape codes for terminal formatting.
-module(agi_symposium_ansi).
-export([green/1, red/1, cyan/1, bold/1, reset/0]).

-define(ESC, "\e[").
-define(RESET, "\e[0m").
-define(GREEN, "\e[32m").
-define(RED, "\e[31m").
-define(CYAN, "\e[36m").
-define(BOLD, "\e[1m").

-spec green(string()) -> string().
green(S) -> ?GREEN ++ S ++ ?RESET.

-spec red(string()) -> string().
red(S) -> ?RED ++ S ++ ?RESET.

-spec cyan(string()) -> string().
cyan(S) -> ?CYAN ++ S ++ ?RESET.

-spec bold(string()) -> string().
bold(S) -> ?BOLD ++ S ++ ?RESET.

-spec reset() -> string().
reset() -> ?RESET.
