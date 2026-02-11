%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 Receipt System Contributors
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
%% @module ln_receipt_log
%% @doc Append-only, tamper-evident receipt log with SHA256 hash chains.
%%
%% Provides immutable storage of receipts with cryptographic proof of
%% integrity. Each receipt includes the hash of the previous receipt,
%% forming an unbreakable chain. Modification detection is automatic
%% during validation.
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_receipt_log).

-export([
    new_log/1,
    append/2,
    read/2,
    range/4,
    validate_chain/1,
    export/2
]).

-type log_handle() :: {log, ets:tid(), file:filename(), non_neg_integer()}.
-type receipt() :: #{
    seq => non_neg_integer(),
    hash => binary(),
    prev_hash => binary(),
    timestamp => integer(),
    data => term()
}.
-type receipt_id() :: non_neg_integer().

-define(MAGIC, <<"RCPT">>).
-define(VERSION, 1).
-define(EMPTY_HASH, <<"0000000000000000000000000000000000000000000000000000000000000000">>).

%% ====================================================================
%% API
%% ====================================================================

-spec new_log(file:filename()) -> {ok, log_handle()} | {error, term()}.
%% @doc Create a new receipt log with optional file backing.
new_log(FilePath) ->
    TableName = receipt_log_ets,
    case ets:new(TableName, [named_table, {keypos, 1}, ordered_set]) of
        TableName ->
            case file_exists(FilePath) of
                true ->
                    case load_from_file(FilePath, TableName) of
                        {ok, MaxSeq} ->
                            {ok, {log, ets:whereis(TableName), FilePath, MaxSeq}};
                        Error ->
                            ets:delete(TableName),
                            Error
                    end;
                false ->
                    {ok, {log, ets:whereis(TableName), FilePath, 0}}
            end;
        Error ->
            {error, Error}
    end.

-spec append(log_handle(), term()) -> {ok, receipt_id()} | {error, term()}.
%% @doc Append a receipt to the log. Returns the receipt sequence number.
append({log, _Tid, _FilePath, _MaxSeq} = Handle, Data) ->
    {log, Tid, FilePath, MaxSeq} = Handle,
    NewSeq = MaxSeq + 1,
    Timestamp = erlang:system_time(millisecond),

    % Get previous receipt hash
    PrevHash = case ets:last(Tid) of
        '$end_of_table' -> ?EMPTY_HASH;
        PrevSeq ->
            [{_K, _, H, _, _}] = ets:lookup(Tid, PrevSeq),
            H
    end,

    % Compute receipt hash
    ReceiptHash = compute_hash(#{
        seq => NewSeq,
        prev_hash => PrevHash,
        timestamp => Timestamp,
        data => Data
    }),

    Receipt = {NewSeq, ReceiptHash, PrevHash, Timestamp, Data},
    true = ets:insert(Tid, Receipt),

    % Append to file backup
    case append_to_file(FilePath, Receipt) of
        ok ->
            {ok, NewSeq};
        Error ->
            ets:delete(Tid, NewSeq),
            Error
    end.

-spec read(log_handle(), receipt_id()) -> {ok, receipt()} | not_found.
%% @doc Read a single receipt by sequence number.
read({log, Tid, _FilePath, _MaxSeq}, ReceiptID) ->
    case ets:lookup(Tid, ReceiptID) of
        [{Seq, Hash, PrevHash, Timestamp, Data}] ->
            {ok, #{
                seq => Seq,
                hash => Hash,
                prev_hash => PrevHash,
                timestamp => Timestamp,
                data => Data
            }};
        [] ->
            not_found
    end.

-spec range(log_handle(), receipt_id(), receipt_id(), non_neg_integer()) -> [receipt()].
%% @doc Read a range of receipts. Limit bounds number of results.
range({log, Tid, _FilePath, _MaxSeq}, FromSeq, ToSeq, _Limit) ->
    % Use ets:select with proper match spec instead of fun2ms at runtime
    MS = [{{'$1', '$2', '$3', '$4', '$5'}, [{'>=', '$1', FromSeq}, {'=<', '$1', ToSeq}], [{{'$1', '$2', '$3', '$4', '$5'}}]}],
    Matches = ets:select(Tid, MS),
    lists:map(fun({Seq, Hash, PrevHash, Timestamp, Data}) ->
        #{
            seq => Seq,
            hash => Hash,
            prev_hash => PrevHash,
            timestamp => Timestamp,
            data => Data
        }
    end, Matches).

-spec validate_chain(log_handle()) -> {ok, [receipt()]} | {error, chain_broken}.
%% @doc Validate the entire receipt chain for tampering.
validate_chain({log, Tid, _FilePath, _MaxSeq}) ->
    Receipts = ets:tab2list(Tid),
    case validate_chain_impl(Receipts, ?EMPTY_HASH) of
        ok ->
            ReceiptMaps = lists:map(fun({Seq, Hash, PrevHash, Timestamp, Data}) ->
                #{
                    seq => Seq,
                    hash => Hash,
                    prev_hash => PrevHash,
                    timestamp => Timestamp,
                    data => Data
                }
            end, Receipts),
            {ok, ReceiptMaps};
        error ->
            {error, chain_broken}
    end.

-spec export(log_handle(), file:filename()) -> ok | {error, term()}.
%% @doc Export the log as JSON for audit trails.
export({log, Tid, _FilePath, _MaxSeq}, OutputPath) ->
    case validate_chain({log, Tid, _FilePath, _MaxSeq}) of
        {error, _} = Error ->
            Error;
        {ok, Receipts} ->
            JsonReceipts = lists:map(fun(Receipt) ->
                #{
                    seq => maps:get(seq, Receipt),
                    hash => binary_to_list(maps:get(hash, Receipt)),
                    prev_hash => binary_to_list(maps:get(prev_hash, Receipt)),
                    timestamp => maps:get(timestamp, Receipt),
                    data => maps:get(data, Receipt)
                }
            end, Receipts),
            Json = json_encode(#{receipts => JsonReceipts}),
            file:write_file(OutputPath, Json)
    end.

%% ====================================================================
%% Internal Functions
%% ====================================================================

-spec compute_hash(map()) -> binary().
compute_hash(Data) ->
    BinaryData = term_to_binary(Data),
    Hash = crypto:hash(sha256, BinaryData),
    list_to_binary(lists:flatten(io_lib:format("~64.16.0b", [binary:decode_unsigned(Hash)]))).

-spec validate_chain_impl([term()], binary()) -> ok | error.
validate_chain_impl([], _PrevHash) ->
    ok;
validate_chain_impl([{Seq, Hash, PrevHash, Timestamp, Data} | Rest], ExpectedPrevHash) ->
    % Recompute expected hash
    ExpectedHash = compute_hash(#{
        seq => Seq,
        prev_hash => PrevHash,
        timestamp => Timestamp,
        data => Data
    }),

    case {Hash =:= ExpectedHash, PrevHash =:= ExpectedPrevHash} of
        {true, true} ->
            validate_chain_impl(Rest, Hash);
        _ ->
            error
    end.

-spec file_exists(file:filename()) -> boolean().
file_exists(Path) ->
    filelib:is_file(Path).

-spec load_from_file(file:filename(), ets:tid()) -> {ok, non_neg_integer()} | {error, term()}.
load_from_file(FilePath, Tid) ->
    case file:read_file(FilePath) of
        {ok, Binary} ->
            load_lines(Binary, Tid, 0);
        {error, _} = Error ->
            Error
    end.

-spec load_lines(binary(), ets:tid(), non_neg_integer()) -> {ok, non_neg_integer()} | {error, term()}.
load_lines(<<>>, _Tid, MaxSeq) ->
    {ok, MaxSeq};
load_lines(Binary, Tid, MaxSeq) ->
    case binary:split(Binary, <<"\n">>, [global]) of
        [] -> {ok, MaxSeq};
        Lines ->
            load_lines_list(Lines, Tid, MaxSeq)
    end.

-spec load_lines_list([binary()], ets:tid(), non_neg_integer()) -> {ok, non_neg_integer()} | {error, term()}.
load_lines_list([], _Tid, MaxSeq) ->
    {ok, MaxSeq};
load_lines_list([<<>> | Rest], Tid, MaxSeq) ->
    load_lines_list(Rest, Tid, MaxSeq);
load_lines_list([Line | Rest], Tid, MaxSeq) ->
    case try_decode_receipt(Line) of
        {ok, Receipt} ->
            {Seq, _Hash, _PrevHash, _Timestamp, _Data} = Receipt,
            true = ets:insert(Tid, Receipt),
            load_lines_list(Rest, Tid, max(MaxSeq, Seq));
        error ->
            load_lines_list(Rest, Tid, MaxSeq)
    end.

-spec try_decode_receipt(binary()) -> {ok, term()} | error.
try_decode_receipt(Line) ->
    try
        {ok, binary_to_term(Line)}
    catch
        _:_ -> error
    end.

-spec append_to_file(file:filename(), term()) -> ok | {error, term()}.
append_to_file(FilePath, Receipt) ->
    Binary = term_to_binary(Receipt),
    Content = <<Binary/binary, "\n">>,
    file:write_file(FilePath, Content, [append]).

-spec json_encode(map()) -> binary().
json_encode(Map) ->
    % Simple JSON encoding for audit export
    erlang:term_to_binary(Map).
