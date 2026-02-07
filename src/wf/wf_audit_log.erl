%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @doc Append-Only Receipt Audit Log
%%
%% This module provides an append-only audit log for storing workflow receipts
%% using Erlang's disk_log for durable, sequential storage.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Append-only log with disk_log for durability</li>
%%   <li>Cursor-based reading for efficient pagination</li>
%%   <li>Temporary file support for testing</li>
%%   <li>Automatic file cleanup on close for temp files</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Opening a log with temporary file (for testing):
%% ```erlang
%% > {ok, Log} = wf_audit_log:open(#{name => receipts, file => tmp}).
%% {ok, {log, receipts, "/tmp/receipts_..."}}
%% ```
%%
%% Appending receipts:
%% ```erlang
%% > R1 = #{before_hash => <<"b1">>, after_hash => <<"a1">>,
%% >       move => #{trsn => t1, mode => #{}, produce => #{}}, ts => 1}.
%% > ok = wf_audit_log:append(Log, R1).
%% ok
%% ```
%%
%% Reading with cursor (integer position):
%% ```erlang
%% > {ok, [R1], C1} = wf_audit_log:read(Log, 0, 1).
%% {ok, [#{before_hash => <<"b1">>, ...}], 1}
%% > {ok, [R2], _} = wf_audit_log:read(Log, C1, 10).
%% {ok, [...], 2}
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_audit_log).

%%====================================================================
%% Exports
%%====================================================================

%% Log management
-export([open/1, append/2, read/3, close/1, doctest_test/0]).

%%====================================================================
%% Records
%%====================================================================

-record(log, {
    name :: atom(),
    file :: file:filename_all(),
    is_temp :: boolean(),
    disk_log :: disk_log:log(),
    cache = [] :: [term()]  % Cached terms from disk_log
}).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Log configuration map.
%%
%% Required keys:
%% - <b>name:</b> Atom name for the log (used for disk_log naming)
%%
%% Optional keys:
%% - <b>file:</b> Path to log file. Use atom `tmp` for temporary file
%%--------------------------------------------------------------------
-type config() :: #{
    name := atom(),
    file => file:filename_all() | tmp
}.

%%--------------------------------------------------------------------
%% @doc Log handle.
%%
%% Opaque handle returned by open/1 and used by other functions.
%%--------------------------------------------------------------------
-opaque log() :: #log{}.

%%--------------------------------------------------------------------
%% @doc Receipt entry.
%%
%% Maps representing workflow transition receipts with at minimum:
%% - <b>before_hash:</b> Binary hash of marking before transition
%% - <b>after_hash:</b> Binary hash of marking after transition
%% - <b>move:</b> Move record with trsn, mode, and consume/produce keys
%% - <b>ts:</b> Timestamp integer
%%--------------------------------------------------------------------
-type receipt() :: #{
    before_hash := binary(),
    after_hash := binary(),
    move := map(),
    ts := integer()
}.

%%--------------------------------------------------------------------
%% @doc Read cursor position.
%%
%% Integer representing the receipt index in the log for pagination.
%% Use 0 to start reading from the beginning.
%%--------------------------------------------------------------------
-type cursor() :: non_neg_integer().

%% Export types
-export_type([log/0, config/0, receipt/0, cursor/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Opens an append-only audit log.
%%
%% Creates a new disk_log with the given configuration. If `file => tmp`
%% is specified, creates a temporary file in /tmp with a unique name.
%% The temporary file will be automatically deleted when close/1 is called.
%%
%% <h4>Configuration Options</h4>
%% <ul>
%%   <li><b>name:</b> Atom name for the log (required)</li>
%%   <li><b>file:</b> Path or `tmp` for temporary file (required)</li>
%% </ul>
%%
%% @param Config Configuration map for the log
%% @returns {ok, Log} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec open(Config :: config()) -> {ok, log()} | {error, term()}.

open(#{name := Name, file := FileSpec}) ->
    %% Determine file path and temp flag
    {FilePath, IsTemp} = resolve_file_path(Name, FileSpec),

    %% disk_log:open/1 takes a list of {Key, Value} options
    %% Use default wrap log options
    DiskLogOpts = [
        {name, Name},
        {file, FilePath}
    ],

    %% Open the disk_log
    case disk_log:open(DiskLogOpts) of
        {ok, LogName} ->
            Log = #log{
                name = Name,
                file = FilePath,
                is_temp = IsTemp,
                disk_log = LogName
            },
            {ok, Log};
        {repaired, LogName, _, _} ->
            %% Log was repaired and opened
            Log = #log{
                name = Name,
                file = FilePath,
                is_temp = IsTemp,
                disk_log = LogName
            },
            {ok, Log};
        {error, _Reason} = Error ->
            %% Clean up temp file on error
            IsTemp andalso file:delete(FilePath),
            Error
    end;

open(_Config) ->
    {error, badarg}.

%%--------------------------------------------------------------------
%% @doc Appends a receipt to the audit log.
%%
%% Writes the receipt to the end of the log. The log is append-only,
%% so existing entries cannot be modified.
%%
%% @param Log Log handle from open/1
%% @param Receipt Receipt map to append
%% @returns ok on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec append(Log :: log(), Receipt :: receipt()) -> ok | {error, term()}.

append(#log{disk_log = LogName}, Receipt) when is_map(Receipt) ->
    case disk_log:alog_terms(LogName, [Receipt]) of
        ok -> ok;
        {error, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Reads receipts from the log starting at a cursor position.
%%
%% Cursor-based pagination allows efficient reading through large logs.
%% The returned NextCursor can be used in subsequent calls to continue
%% reading. When the end of the log is reached, returns an empty list.
%%
%% The cursor is an integer index into the receipt sequence.
%% Use 0 to begin reading from the beginning.
%%
%% @param Log Log handle from open/1
%% @param Cursor Starting position (use 0 for beginning)
%% @param Count Maximum number of receipts to return
%% @returns {ok, Receipts, NextCursor} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec read(Log :: log(), Cursor :: cursor(), Count :: non_neg_integer()) ->
    {ok, [receipt()], cursor()}.

read(#log{} = Log, Cursor, Count)
        when is_integer(Cursor), Cursor >= 0, is_integer(Count), Count > 0 ->

    %% Ensure cache is populated with enough items for the read
    %% We need at least Cursor + Count items in the cache
    Log1 = ensure_cache(Log, Cursor + Count),

    %% Extract the requested number of receipts from the cache
    CachedTerms = Log1#log.cache,
    Available = length(CachedTerms),

    if
        Cursor >= Available ->
            %% Past the end of the log
            {ok, [], Cursor};
        true ->
            %% Get the slice of terms starting at Cursor
            Slice = lists:sublist(CachedTerms, Cursor + 1, Count),
            Receipts = filter_receipts(Slice),
            NextCursor = Cursor + length(Slice),
            {ok, Receipts, NextCursor}
    end.

%%--------------------------------------------------------------------
%% @doc Closes the audit log.
%%
%% Closes the disk_log and cleans up temporary files if this was a temp log.
%%
%% @param Log Log handle from open/1
%% @returns ok on success
%%
%% @end
%%--------------------------------------------------------------------
-spec close(Log :: log()) -> ok.

close(#log{disk_log = LogName, file = FilePath, is_temp = IsTemp}) ->
    %% Close the disk_log
    ok = disk_log:close(LogName),

    %% Delete temp file if applicable
    IsTemp andalso file:delete(FilePath),

    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Resolves the file path and determines if this is a temporary file.
-spec resolve_file_path(atom(), file:filename_all() | tmp) -> {file:filename_all(), boolean()}.

resolve_file_path(Name, tmp) ->
    %% Create unique temp file path
    Unique = erlang:unique_integer([positive, monotonic]),
    Filename = atom_to_list(Name) ++ "_" ++ integer_to_list(Unique),
    Path = filename:join("/tmp", Filename),
    {Path, true};

resolve_file_path(_Name, FilePath) when is_list(FilePath); is_binary(FilePath) ->
    %% Use provided path, not a temp file
    {FilePath, false};

resolve_file_path(Name, _) ->
    %% Default to temp if invalid spec
    resolve_file_path(Name, tmp).

%% @private
%% @doc Ensures the cache is populated at least up to the given position.
-spec ensure_cache(log(), non_neg_integer()) -> log().

ensure_cache(#log{cache = Cache} = Log, Position) ->
    CurrentSize = length(Cache),
    if
        Position < CurrentSize ->
            %% Cache already has enough data
            Log;
        true ->
            %% Need to fetch more data from disk_log
            fetch_more(Log, Position)
    end.

%% @private
%% @doc Fetches more terms from disk_log until we reach the target position or end.
-spec fetch_more(log(), non_neg_integer()) -> log().

fetch_more(#log{disk_log = LogName, cache = Cache} = Log, TargetPosition) ->
    StartCont = case Cache of
        [] -> start;
        _ -> continue
    end,

    Result = fetch_until(LogName, StartCont, TargetPosition - length(Cache), []),
    NewCache = Cache ++ Result,
    Log#log{cache = NewCache}.

%% @private
%% @doc Fetches terms until we have at least Count terms or reach end of log.
-spec fetch_until(disk_log:log(), start | continue, non_neg_integer(), [term()]) -> [term()].

fetch_until(_LogName, _Cont, 0, Acc) ->
    lists:reverse(Acc);

fetch_until(LogName, start, Count, Acc) ->
    case disk_log:chunk(LogName, start) of
        {Cont, Terms} when is_list(Terms) ->
            NewCount = Count - length(Terms),
            if
                NewCount =< 0 ->
                    %% We have enough terms (possibly too many)
                    %% Return only what we need
                    lists:reverse(Acc) ++ lists:sublist(Terms, Count);
                true ->
                    %% Need more, continue fetching
                    fetch_until(LogName, Cont, NewCount, lists:reverse(Terms, Acc))
            end;
        eof ->
            lists:reverse(Acc)
    end;

fetch_until(LogName, Cont, Count, Acc) ->
    case disk_log:chunk(LogName, Cont) of
        {NewCont, Terms} when is_list(Terms) ->
            NewCount = Count - length(Terms),
            if
                NewCount =< 0 ->
                    lists:reverse(Acc) ++ lists:sublist(Terms, Count);
                true ->
                    fetch_until(LogName, NewCont, NewCount, lists:reverse(Terms, Acc))
            end;
        eof ->
            lists:reverse(Acc)
    end.

%% @private
%% @doc Filters a list of terms to only include receipt maps.
-spec filter_receipts([term()]) -> [receipt()].

filter_receipts(Terms) ->
    lists:filtermap(fun filter_receipt/1, Terms).

%% @private
%% @doc Checks if a term is a valid receipt and returns it.
-spec filter_receipt(term()) -> {true, receipt()} | false.

filter_receipt(Term) when is_map(Term) ->
    %% Check for required receipt keys
    case maps:is_key(before_hash, Term) andalso maps:is_key(after_hash, Term) of
        true -> {true, Term};
        false -> false
    end;

filter_receipt(_) ->
    false.

%%====================================================================
%% Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the audit log module.
%%
%% Tests open/append/read/close lifecycle with temporary files.
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Open log with temp file
    {ok, Log} = open(#{name => receipts, file => tmp}),
    #log{name = receipts, is_temp = true} = Log,

    %% Test 2: Create and append receipts
    R1 = #{
        before_hash => <<"b1">>,
        after_hash => <<"a1">>,
        move => #{trsn => t1, mode => #{}, produce => #{}},
        ts => 1
    },
    R2 = R1#{
        before_hash => <<"b2">>,
        after_hash => <<"a2">>,
        ts => 2
    },
    ok = append(Log, R1),
    ok = append(Log, R2),

    %% Test 3: Read with cursor pagination (read 1 item)
    {ok, [ReadR1], C1} = read(Log, 0, 1),
    R1 = ReadR1,
    1 = C1,

    %% Test 4: Continue reading from position 1
    {ok, [ReadR2], C2} = read(Log, C1, 10),
    R2 = ReadR2,
    2 = C2,

    %% Test 5: Read past end returns empty
    {ok, [], _} = read(Log, C2, 10),

    %% Test 6: Close log (should clean up temp file)
    LogFile = Log#log.file,
    ok = close(Log),

    %% Verify temp file was deleted
    {error, enoent} = file:read_file_info(LogFile),

    %% Test 7: Multiple receipts in single read
    {ok, Log2} = open(#{name => multi_receipt, file => tmp}),
    R3 = #{before_hash => <<"b3">>, after_hash => <<"a3">>, move => #{}, ts => 3},
    R4 = #{before_hash => <<"b4">>, after_hash => <<"a4">>, move => #{}, ts => 4},
    R5 = #{before_hash => <<"b5">>, after_hash => <<"a5">>, move => #{}, ts => 5},
    ok = append(Log2, R3),
    ok = append(Log2, R4),
    ok = append(Log2, R5),

    %% Read all at once
    {ok, [R3, R4, R5], _} = read(Log2, 0, 10),

    %% Close second log
    ok = close(Log2),

    %% Test 8: Read with count limit
    {ok, Log3} = open(#{name => count_test, file => tmp}),
    lists:foreach(fun(I) ->
        ok = append(Log3, #{before_hash => <<"b", I:8>>, after_hash => <<"a", I:8>>, move => #{}, ts => I})
    end, lists:seq(1, 5)),

    %% Read only 2 at a time
    {ok, Receipts1, 2} = read(Log3, 0, 2),
    2 = length(Receipts1),

    {ok, Receipts2, 4} = read(Log3, 2, 2),
    2 = length(Receipts2),

    {ok, Receipts3, 5} = read(Log3, 4, 2),
    1 = length(Receipts3),

    ok = close(Log3),

    %% Test 9: Empty log
    {ok, EmptyLog} = open(#{name => empty_test, file => tmp}),
    {ok, [], _} = read(EmptyLog, 0, 10),
    ok = close(EmptyLog),

    %% Test 10: Re-reading from same position works
    {ok, Log4} = open(#{name => reread_test, file => tmp}),
    ok = append(Log4, #{before_hash => <<"bx">>, after_hash => <<"ax">>, move => #{}, ts => 99}),
    {ok, [Rx], _} = read(Log4, 0, 1),
    #{ts := 99} = Rx,
    {ok, [Rx], _} = read(Log4, 0, 10),  %% Re-read from start
    ok = close(Log4),

    ok.
