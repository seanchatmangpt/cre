%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 CRE Project
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author CRE Project
%% @copyright 2025
%%
%% @doc Compile Warnings Detection Module
%%
%% Provides quality gate for Erlang compilation warnings:
%% <ul>
%%   <li><b>Warning Detection:</b> Parses rebar3 compile output for warnings</li>
%%   <li><b>Report Generation:</b> Creates markdown reports with warning details</li>
%%   <li><b>Quality Gate:</b> Returns pass/fail based on warning count</li>
%%   <li><b>CI Integration:</b> Exit code 1 on warnings for CI pipelines</li>
%% </ul>
%%
%% <h3>Warning Format Support</h3>
%%
%% Parses standard rebar3 compile warning formats:
%% <ul>
%%   <li>Line-based: <code>src/file.erl:123: Warning: message</code></li>
%%   <li>Block-based: <code>┌─ src/file.erl: │ 123 │ ╰── Warning: message</code></li>
%%   <li>Optimization: <code>src/file.erl:456: Warning: OPTIMIZED: message</code></li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% ```
%% %% Get all warnings from current compile
%% Warnings = evidence_compile:get_compile_warnings(),
%%
%% %% Generate markdown report
%% Report = evidence_compile:warnings_to_report(Warnings),
%%
%% %% Save report to file
%% evidence_compile:save_warning_report(Warnings, "logs/COMPILE_WARNINGS.md"),
%%
%% %% Check quality gate (exits with code 1 if warnings > 0)
%% evidence_compile:check_warnings_gate(),
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(evidence_compile).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/0]).
-export([get_compile_warnings/0]).
-export([warnings_to_report/1]).
-export([save_warning_report/2]).
-export([check_warnings_gate/0]).
-export([parse_compile_output/1]).

%% gen_server callbacks
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type warning_type() :: unused_var | unused_type | match_context | ignored_result
                      | clause_fail | other.

-type warning() :: #{
    type := warning_type(),
    file := binary(),
    line := non_neg_integer(),
    message := binary(),
    severity := warning | error
}.

-type compile_status() :: #{
    total_warnings := non_neg_integer(),
    warnings_by_file => #{binary() := [warning()]},
    warnings_by_type => #{warning_type() := non_neg_integer()},
    status := pass | fail
}.

-export_type([warning/0, warning_type/0, compile_status/0]).

%%====================================================================
%% gen_server State
%%====================================================================

-record(state, {
    warnings :: [warning()],
    last_compile_time :: integer() | undefined
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the compile warnings monitor
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Get compile warnings from the latest compilation
%%
%% Parses compile output from rebar3 to extract all warnings.
%% Returns a map with total count, warnings grouped by file,
%% and warnings grouped by type.
%%
%% @end
-spec get_compile_warnings() -> compile_status().
get_compile_warnings() ->
    case get_compile_output() of
        {ok, Output} ->
            Warnings = parse_compile_output(Output),
            #{
                total_warnings => length(Warnings),
                warnings_by_file => group_warnings_by_file(Warnings),
                warnings_by_type => group_warnings_by_type(Warnings),
                status => status_from_warnings(Warnings)
            };
        {error, Reason} ->
            logger:error("Failed to get compile output: ~p", [Reason]),
            #{
                total_warnings => 0,
                warnings_by_file => #{},
                warnings_by_type => #{},
                status => error
            }
    end.

%% @doc Parse rebar3 compile output and extract warnings
%%
%% Handles multiple warning formats:
%% <ul>
%%   <li>Line-based: <code>src/file.erl:123: Warning: message</code></li>
%%   <li>Block-based: Multi-line with ┌─ and ╰── markers</li>
%%   <li>Optimization: OPTIMIZED: prefix warnings</li>
%% </ul>
%%
%% @end
-spec parse_compile_output(binary() | string()) -> [warning()].

parse_compile_output(Output) when is_binary(Output) ->
    parse_compile_output(unicode:characters_to_list(Output, utf8));
parse_compile_output(Output) when is_list(Output) ->
    Lines = string:split(Output, "\n", all),
    parse_lines(Lines, [], undefined).

%% @doc Convert warnings list to markdown report format
%%
%% Generates a structured markdown report with:
%% <ul>
%%   <li>Summary section (total count, status)</li>
%%   <li>Warnings grouped by file</li>
%%   <li>Warnings grouped by type</li>
%%   <li>Recommendations section</li>
%% </ul>
%%
%% @end
-spec warnings_to_report(compile_status() | [warning()]) -> binary().
warnings_to_report(Status) when is_map(Status), map_size(Status) > 0 ->
    case maps:get(total_warnings, Status, undefined) of
        undefined ->
            warnings_to_report_list(Status);
        Total ->
            WarningsByFile = maps:get(warnings_by_file, Status, #{}),
            StatusAtom = maps_get(status, Status, fail),
            Report = [
                "# Compile Warnings Report\n\n",
                "Generated: ", iso8601_timestamp(), "\n\n",
                "## Summary\n\n",
                "- **Total Warnings**: ", integer_to_binary(Total), "\n",
                "- **Status**: ", status_to_binary(StatusAtom), "\n",
                "- **Files Affected**: ", integer_to_binary(maps:size(WarningsByFile)), "\n\n",
                generate_warnings_by_file(WarningsByFile),
                generate_recommendations(Status)
            ],
            iolist_to_binary(Report)
    end;
warnings_to_report(Warnings) when is_list(Warnings) ->
    Status = #{
        total_warnings => length(Warnings),
        warnings_by_file => group_warnings_by_file(Warnings),
        warnings_by_type => group_warnings_by_type(Warnings),
        status => status_from_warnings(Warnings)
    },
    warnings_to_report(Status).

%% @doc Save warning report to file
%%
%% Writes the markdown report to the specified path.
%% Creates parent directories if needed.
%%
%% @end
-spec save_warning_report(compile_status() | [warning()], file:filename_all()) ->
    ok | {error, term()}.
save_warning_report(Status, Path) ->
    Report = warnings_to_report(Status),
    case filelib:ensure_dir(Path) of
        ok ->
            file:write_file(Path, Report);
        Error ->
            Error
    end.

%% @doc Check warnings gate for CI/CD
%%
%% Returns <code>pass</code> if no warnings found.
%% Exits with code 1 if warnings > 0 (for CI integration).
%%
%% @end
-spec check_warnings_gate() -> pass | no_return().
check_warnings_gate() ->
    Status = get_compile_warnings(),
    Total = maps_get(total_warnings, Status, 0),
    case Total of
        0 ->
            logger:info("Compile warnings check: PASS (0 warnings)"),
            pass;
        N when N > 0 ->
            logger:error("Compile warnings check: FAIL (~p warnings)", [N]),
            %% Save report for debugging
            save_warning_report(Status, "logs/COMPILE_WARNINGS.md"),
            erlang:halt(1)
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    {ok, #state{
        warnings = [],
        last_compile_time = undefined
    }}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call(get_warnings, _From, State) ->
    {reply, State#state.warnings, State};
handle_call(Request, _From, State) ->
    {reply, {error, {unknown_request, Request}}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({update_warnings, Warnings}, State) ->
    {noreply, State#state{warnings = Warnings, last_compile_time = erlang:system_time(millisecond)}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Get compile output from rebar3
-spec get_compile_output() -> {ok, binary()} | {error, term()}.
get_compile_output() ->
    %% Check if compile log exists from recent build
    CompileLog = "_build/default/lib/cre/compile.log",
    case file:read_file(CompileLog) of
        {ok, Content} ->
            {ok, Content};
        {error, enoent} ->
            %% Try running compile and capturing output
            capture_compile_output();
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Run rebar3 compile and capture output
-spec capture_compile_output() -> {ok, binary()} | {error, term()}.
capture_compile_output() ->
    Port = open_port({spawn, "rebar3 compile 2>&1"}, [exit_status, binary, {line, 1024}]),
    capture_port_data(Port, <<>>).

%% @private Collect data from port
-spec capture_port_data(port(), binary()) -> {ok, binary()} | {error, term()}.
capture_port_data(Port, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            capture_port_data(Port, <<Acc/binary, Line/binary, "\n">>);
        {Port, {data, {noeol, Line}}} ->
            capture_port_data(Port, <<Acc/binary, Line/binary>>);
        {Port, {exit_status, 0}} ->
            {ok, Acc};
        {Port, {exit_status, Status}} when Status > 0 ->
            %% Non-zero exit might still have warnings we can parse
            {ok, Acc}
    after
        30000 ->
            port_close(Port),
            {error, timeout}
    end.

%% @private Parse lines, accumulating warnings
-spec parse_lines([string()], [warning()], undefined | binary()) -> [warning()].
parse_lines([], Acc, _CurrentFile) ->
    lists:reverse(Acc);
parse_lines([Line | Rest], Acc, CurrentFile) ->
    case parse_warning_line(Line, CurrentFile) of
        {warning, Warning} ->
            parse_lines(Rest, [Warning | Acc], CurrentFile);
        {file, File} ->
            parse_lines(Rest, Acc, File);
        ignore ->
            parse_lines(Rest, Acc, CurrentFile)
    end.

%% @private Parse a single line for warning patterns
-spec parse_warning_line(string(), undefined | binary()) ->
    {warning, warning()} | {file, binary()} | ignore.
parse_warning_line(Line, CurrentFile) ->
    Trimmed = string:trim(Line),
    case Trimmed of
        "" ->
            ignore;
        %% Line-based warning: src/file.erl:123: Warning: message
        _ ->
            case re:run(Trimmed,
                       "^(.+\\.erl):(\\d+):\\s*Warning:\\s*(.+)$",
                       [unicode, {capture, all_but_first, list}]) of
                {match, [File, LineNum, Message]} ->
                    {warning, make_warning(File, LineNum, Message)};
                nomatch ->
                    %% Block-based: ┌─ src/file.erl:
                    case re:run(Trimmed,
                               "^┌─\\s*(.+\\.erl):?$",
                               [unicode, {capture, all_but_first, list}]) of
                        {match, [File]} ->
                            {file, list_to_binary(File)};
                        nomatch ->
                            %% Block-based: │ 123 │ ╰── Warning: message
                            case re:run(Trimmed,
                                       "^\\s*│\\s*(\\d+)\\s*│\\s*╰──\\s*Warning:\\s*(.+)$",
                                       [unicode, {capture, all_but_first, list}]) of
                                {match, [LineNum, Message]} ->
                                    case CurrentFile of
                                        undefined ->
                                            ignore;
                                        _ ->
                                            {warning, make_warning(
                                                       binary_to_list(CurrentFile),
                                                       LineNum,
                                                       Message)}
                                    end;
                                nomatch ->
                                    %% Simple warning line with message
                                    case re:run(Trimmed,
                                               "^Warning:\\s*(.+)$",
                                               [unicode, {capture, all_but_first, list}]) of
                                        {match, [Message]} ->
                                            case CurrentFile of
                                                undefined ->
                                                    ignore;
                                                _ ->
                                                    {warning, #{
                                                        type => classify_warning(Message),
                                                        file => CurrentFile,
                                                        line => 0,
                                                        message => list_to_binary(Message),
                                                        severity => warning
                                                    }}
                                            end;
                                        nomatch ->
                                            ignore
                                    end
                            end
                    end
            end
    end.

%% @private Create a warning record
-spec make_warning(string(), string(), string()) -> warning().
make_warning(FileStr, LineNumStr, Message) ->
    LineNum = case string:to_integer(LineNumStr) of
        {N, _} -> N;
        _ -> 0
    end,
    #{
        type => classify_warning(Message),
        file => list_to_binary(FileStr),
        line => LineNum,
        message => list_to_binary(Message),
        severity => warning
    }.

%% @private Classify warning by type
-spec classify_warning(string()) -> warning_type().
classify_warning(Message) ->
    Lower = string:lowercase(Message),
    Cond1 = string:str(Lower, "variable") > 0 andalso string:str(Lower, "unused") > 0,
    Cond2 = string:str(Lower, "type") > 0 andalso string:str(Lower, "unused") > 0,
    Cond3 = string:str(Lower, "match context") > 0,
    Cond4 = string:str(Lower, "ignored") > 0 orelse string:str(Lower, "ignore") > 0,
    Cond5 = string:str(Lower, "clause") > 0 andalso string:str(Lower, "fail") > 0,
    Cond6 = string:str(Lower, "optimized") > 0,
    if
        Cond1 -> unused_var;
        Cond2 -> unused_type;
        Cond3 -> match_context;
        Cond4 -> ignored_result;
        Cond5 -> clause_fail;
        Cond6 -> match_context;  %% Optimization warnings are match context
        true -> other
    end.

%% @private Group warnings by file
-spec group_warnings_by_file([warning()]) -> #{binary() := [warning()]}.
group_warnings_by_file(Warnings) ->
    lists:foldl(fun(W, Acc) ->
        File = maps:get(file, W),
        maps:update_with(File, fun(Old) -> [W | Old] end, [W], Acc)
    end, #{}, Warnings).

%% @private Group warnings by type
-spec group_warnings_by_type([warning()]) -> #{warning_type() := non_neg_integer()}.
group_warnings_by_type(Warnings) ->
    lists:foldl(fun(W, Acc) ->
        Type = maps:get(type, W),
        maps:update_with(Type, fun(Old) -> Old + 1 end, 1, Acc)
    end, #{}, Warnings).

%% @private Determine status from warnings
-spec status_from_warnings([warning()]) -> pass | fail.
status_from_warnings([]) -> pass;
status_from_warnings(Warnings) when length(Warnings) > 0 -> fail.

%% @private Helper for warnings_to_report
-spec warnings_to_report_list(compile_status()) -> binary().
warnings_to_report_list(Status) ->
    WarningsByFile = maps_get(warnings_by_file, Status, #{}),
    Total = maps:get(total_warnings, Status, 0),
    StatusAtom = maps_get(status, Status, fail),
    Report = [
        "# Compile Warnings Report\n\n",
        "Generated: ", iso8601_timestamp(), "\n\n",
        "## Summary\n\n",
        "- **Total Warnings**: ", integer_to_binary(Total), "\n",
        "- **Status**: ", status_to_binary(StatusAtom), "\n\n",
        generate_warnings_by_file(WarningsByFile)
    ],
    iolist_to_binary(Report).

%% @private Generate warnings by file section
-spec generate_warnings_by_file(#{binary() := [warning()]}) -> iolist().
generate_warnings_by_file(WarningsByFile) ->
    SortedFiles = lists:sort(maps:keys(WarningsByFile)),
    FilesSection = ["## Warnings by File\n\n"],
    lists:foldl(fun(File, Acc) ->
        Warnings = maps:get(File, WarningsByFile),
        FileWarnings = [
            "### ", File, "\n\n",
            format_file_warnings(Warnings),
            "\n"
        ],
        [Acc | FileWarnings]
    end, FilesSection, SortedFiles).

%% @private Format warnings for a single file
-spec format_file_warnings([warning()]) -> iolist().
format_file_warnings(Warnings) ->
    lists:map(fun(W) ->
        Line = maps:get(line, W),
        Message = maps:get(message, W),
        Type = maps:get(type, W),
        ["- **Line ", integer_to_binary(Line), "** [",
         type_to_binary(Type), "]: ", Message, "\n"]
    end, lists:sort(fun(A, B) ->
        maps:get(line, A) =< maps:get(line, B)
    end, Warnings)).

%% @private Generate recommendations section
-spec generate_recommendations(compile_status()) -> iolist().
generate_recommendations(Status) ->
    WarningsByType = maps_get(warnings_by_type, Status, #{}),
    HasUnusedVar = maps_get(unused_var, WarningsByType, 0) > 0,
    HasUnusedType = maps_get(unused_type, WarningsByType, 0) > 0,
    HasIgnored = maps_get(ignored_result, WarningsByType, 0) > 0,

    Recs = ["## Recommendations\n\n"],
    Recs2 = case HasUnusedVar of
        true ->
            [Recs | ["- **Unused Variables**: Prefix unused variables with `_` (e.g., `_Var`)\n"]];
        false -> Recs
    end,
    Recs3 = case HasUnusedType of
        true ->
            [Recs2 | ["- **Unused Types**: Remove unused type definitions or mark with `-compile({nowarn_unused_type, ...})`\n"]];
        false -> Recs2
    end,
    Recs4 = case HasIgnored of
        true ->
            [Recs3 | ["- **Ignored Results**: Assign ignored results to `_` variable\n"]];
        false -> Recs3
    end,
    [Recs4 | ["\n## Quality Gate\n\n",
              "This report serves as a quality gate for CI/CD pipelines.\n",
              "To fix failing builds, address the warnings listed above.\n\n"]].

%% @private Convert status atom to binary
-spec status_to_binary(pass | fail | error) -> binary().
status_to_binary(pass) -> <<"**PASS**">>;
status_to_binary(fail) -> <<"**FAIL**">>;
status_to_binary(error) -> <<"**ERROR**">>.

%% @private Convert warning type to binary
-spec type_to_binary(warning_type()) -> binary().
type_to_binary(unused_var) -> <<"Unused Var">>;
type_to_binary(unused_type) -> <<"Unused Type">>;
type_to_binary(match_context) -> <<"Optimization">>;
type_to_binary(ignored_result) -> <<"Ignored Result">>;
type_to_binary(clause_fail) -> <<"Clause Fail">>;
type_to_binary(other) -> <<"Other">>.

%% @private Get current ISO8601 timestamp
-spec iso8601_timestamp() -> binary().
iso8601_timestamp() ->
    UTCDateTime = calendar:universal_time(),
    {{Y, M, D}, {H, Min, S}} = UTCDateTime,
    iolist_to_binary(io_lib:format("~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
                  [Y, M, D, H, Min, S])).

%% @private Helper for maps:get with default
-spec maps_get(term(), map(), term()) -> term().
maps_get(Key, Map, Default) ->
    try maps:get(Key, Map) of
        Value -> Value
    catch
        error:{badkey, _} -> Default
    end.
