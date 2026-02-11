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
%% @module ln_receipt_effect
%% @doc Runtime effect receipt with idempotency caching and latency tracking.
%%
%% Each effect (function call, API invocation) generates a receipt
%% with idempotency guarantees. If the same effect_id and input_hash
%% are called again, the cached result is returned.
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_receipt_effect).

-export([
    start_effect/3,
    complete/3,
    failed/3,
    idempotent_receipt/2
]).

-type effect_handle() :: {effect, maps:map()}.
-type receipt() :: maps:map().

-define(IDEMPOTENCY_CACHE, effect_idempotency_cache).
-define(MAX_CACHE_SIZE, 1000).

%% ====================================================================
%% API
%% ====================================================================

-spec start_effect(term(), atom(), maps:map()) -> {ok, effect_handle()} | {error, term()}.
%% @doc Start a new effect with ID, connector name, and parameters.
start_effect(EffectID, ConnectorName, Params) ->
    StartTime = erlang:system_time(millisecond),
    InputHash = hash_params(Params),

    Handle = {effect, #{
        effect_id => EffectID,
        connector => ConnectorName,
        params => Params,
        input_hash => InputHash,
        start_time => StartTime,
        status => pending
    }},

    {ok, Handle}.

-spec complete(effect_handle(), term(), non_neg_integer()) -> {ok, receipt()} | {error, term()}.
%% @doc Mark an effect as complete with result and latency.
complete({effect, State}, Result, Latency) ->
    EffectID = maps:get(effect_id, State),
    InputHash = maps:get(input_hash, State),
    ConnectorName = maps:get(connector, State),
    StartTime = maps:get(start_time, State),
    EndTime = erlang:system_time(millisecond),
    ResultHash = hash_result(Result),

    Receipt = #{
        effect_id => EffectID,
        connector => ConnectorName,
        input_hash => InputHash,
        result_hash => ResultHash,
        result => Result,
        status => success,
        latency_ms => Latency,
        start_time => StartTime,
        end_time => EndTime
    },

    % Cache for idempotency
    CacheKey = {EffectID, InputHash},
    cache_effect_result(CacheKey, Receipt),

    {ok, Receipt}.

-spec failed(effect_handle(), term(), non_neg_integer()) -> {ok, receipt()} | {error, term()}.
%% @doc Mark an effect as failed with error details and latency.
failed({effect, State}, Error, Latency) ->
    EffectID = maps:get(effect_id, State),
    InputHash = maps:get(input_hash, State),
    ConnectorName = maps:get(connector, State),
    StartTime = maps:get(start_time, State),
    EndTime = erlang:system_time(millisecond),

    ErrorDetails = case Error of
        {error, Reason, Stack} ->
            #{reason => Reason, stack => Stack};
        {error, Reason} ->
            #{reason => Reason};
        _ ->
            #{reason => Error}
    end,

    Receipt = #{
        effect_id => EffectID,
        connector => ConnectorName,
        input_hash => InputHash,
        error => ErrorDetails,
        status => failed,
        latency_ms => Latency,
        start_time => StartTime,
        end_time => EndTime
    },

    {ok, Receipt}.

-spec idempotent_receipt(term(), binary()) -> {ok, receipt()} | not_cached.
%% @doc Check if an effect with same ID and input hash has been cached.
idempotent_receipt(EffectID, InputHash) ->
    CacheKey = {EffectID, InputHash},
    case get_cached_effect(CacheKey) of
        {found, Receipt} ->
            {ok, Receipt};
        not_found ->
            not_cached
    end.

%% ====================================================================
%% Internal Functions
%% ====================================================================

-spec hash_params(maps:map()) -> binary().
hash_params(Params) ->
    Data = term_to_binary(Params),
    Hash = crypto:hash(sha256, Data),
    list_to_binary(lists:flatten(io_lib:format("~64.16.0b", [binary:decode_unsigned(Hash)]))).

-spec hash_result(term()) -> binary().
hash_result(Result) ->
    Data = term_to_binary(Result),
    Hash = crypto:hash(sha256, Data),
    list_to_binary(lists:flatten(io_lib:format("~64.16.0b", [binary:decode_unsigned(Hash)]))).

-spec cache_effect_result({term(), binary()}, receipt()) -> ok.
cache_effect_result(CacheKey, Receipt) ->
    TableName = ?IDEMPOTENCY_CACHE,
    ensure_cache_table(TableName),
    CurrentSize = ets:info(TableName, size),
    case CurrentSize >= ?MAX_CACHE_SIZE of
        true ->
            % Evict oldest entry
            OldestKey = ets:first(TableName),
            ets:delete(TableName, OldestKey),
            ok;
        false ->
            ok
    end,
    ets:insert(TableName, {CacheKey, Receipt, erlang:system_time(millisecond)}),
    ok.

-spec get_cached_effect({term(), binary()}) -> {found, receipt()} | not_found.
get_cached_effect(CacheKey) ->
    TableName = ?IDEMPOTENCY_CACHE,
    case ets:whereis(TableName) of
        undefined ->
            not_found;
        _Tid ->
            case ets:lookup(TableName, CacheKey) of
                [] ->
                    not_found;
                [{_K, Receipt, _Timestamp}] ->
                    {found, Receipt}
            end
    end.

-spec ensure_cache_table(atom()) -> ok.
ensure_cache_table(TableName) ->
    case ets:whereis(TableName) of
        undefined ->
            ets:new(TableName, [named_table, {keypos, 1}, ordered_set]),
            ok;
        _Tid ->
            ok
    end.
