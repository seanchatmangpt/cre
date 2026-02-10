%% -*- erlang -*-
%%
%% @doc ETS-based caching for pattern instances.
%%
%% Provides in-memory caching with TTL (time-to-live) support for
%% workflow pattern instances. Uses ETS (Erlang Term Storage) for
%% efficient key-value storage with automatic expiration.
%%
%% == Features ==
%%
%% <ul>
%%   <li>Fast in-memory caching using ETS</li>
%%   <li>TTL-based automatic expiration</li>
%%   <li>Thread-safe concurrent access</li>
%%   <li>Manual cleanup of expired entries</li>
%%   <li>Cache statistics and monitoring</li>
%% </ul>
%%
%% == Usage ==
%%
%% Starting the cache:
%% ```erlang
%% > ok = wf_cache:start().
%% ok
%% ```
%%
%% Caching a pattern instance with 5 second TTL:
%% ```erlang
%% > Instance = #{pattern => seq, tasks => [t1, t2]}.
%% _
%% > ok = wf_cache:put(<<"pattern_1">>, Instance, 5000).
%% ok
%% > wf_cache:get(<<"pattern_1">>).
%% {ok, #{pattern => seq, tasks => [t1, t2]}}
%% ```
%%
%% Checking cache existence:
%% ```erlang
%% > wf_cache:exists(<<"pattern_1">>).
%% true
%% > wf_cache:exists(<<"unknown">>).
%% false
%% ```
%%
%% Deleting entries:
%% ```erlang
%% > ok = wf_cache:delete(<<"pattern_1">>).
%% ok
%% > wf_cache:get(<<"pattern_1">>).
%% {error, not_found}
%% ```
%%
%% Clearing the cache:
%% ```erlang
%% > ok = wf_cache:clear().
%% ok
%% ```
%%
%% Getting cache statistics:
%% ```erlang
%% > Stats = wf_cache:stats().
%% #{size => 0, memory => 0}
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_cache).

%%====================================================================
%% Exports
%%====================================================================

%% Cache lifecycle
-export([start/0, stop/0]).

%% Cache operations
-export([put/3, get/1, exists/1, delete/1]).

%% Cache management
-export([clear/0, cleanup/0, stats/0]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Cache key identifier.
%%--------------------------------------------------------------------
-type cache_key() :: binary() | atom().

%%--------------------------------------------------------------------
%% @doc Cache value stored in the table.
%%--------------------------------------------------------------------
-type cache_value() :: term().

%%--------------------------------------------------------------------
%% @doc Time-to-live in milliseconds.
%%--------------------------------------------------------------------
-type ttl() :: pos_integer().

%%--------------------------------------------------------------------
%% @doc Cache record with expiration time.
%%--------------------------------------------------------------------
-record(cache_entry, {
    key :: cache_key(),
    value :: cache_value(),
    expires_at :: integer()
}).

-type cache_entry() :: #cache_entry{}.

%%--------------------------------------------------------------------
%% @doc Cache statistics.
%%--------------------------------------------------------------------
-type cache_stats() :: #{
    size => non_neg_integer(),
    memory => non_neg_integer()
}.

-export_type([cache_key/0, cache_value/0, ttl/0, cache_stats/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(CACHE_TABLE, wf_pattern_cache).
-define(CACHE_OPTIONS, [
    set,
    public,
    {keypos, #cache_entry.key},
    {write_concurrency, true},
    {read_concurrency, true}
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the cache by creating an ETS table.
%%
%% Creates a public set-based ETS table for caching pattern instances.
%% Safe to call multiple times - returns ok if table already exists.
%%
%% == Example
%%
%% ```erlang
%% > ok = wf_cache:start().
%% ok
%% > ok = wf_cache:start().
%% ok
%% ```
%%
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec start() -> ok.

start() ->
    case ets:whereis(?CACHE_TABLE) of
        undefined ->
            ets:new(?CACHE_TABLE, ?CACHE_OPTIONS),
            ok;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Stops the cache by destroying the ETS table.
%%
%% Deletes the entire cache table. Safe to call even if table does
%% not exist.
%%
%% == Example
%%
%% ```erlang
%% > ok = wf_cache:start().
%% ok
%% > ok = wf_cache:stop().
%% ok
%% > ok = wf_cache:stop().
%% ok
%% ```
%%
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.

stop() ->
    case ets:whereis(?CACHE_TABLE) of
        undefined ->
            ok;
        _TableId ->
            ets:delete(?CACHE_TABLE),
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Stores a value in the cache with TTL.
%%
%% Caches a pattern instance with a specified time-to-live. The entry
%% will be considered expired after TTL milliseconds.
%%
%% == Example
%%
%% ```erlang
%% > ok = wf_cache:start().
%% ok
%% > Instance = #{pattern => seq, tasks => [t1, t2]}.
%% _
%% > ok = wf_cache:put(<<"pattern_1">>, Instance, 5000).
%% ok
%% ```
%%
%% @param Key Cache key identifier
%% @param Value Value to cache (any term)
%% @param TTL Time-to-live in milliseconds
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec put(Key :: cache_key(), Value :: cache_value(), TTL :: ttl()) -> ok.

put(Key, Value, TTL) when is_integer(TTL), TTL > 0 ->
    start(),
    ExpiresAt = erlang:system_time(millisecond) + TTL,
    Entry = #cache_entry{
        key = Key,
        value = Value,
        expires_at = ExpiresAt
    },
    ets:insert(?CACHE_TABLE, Entry),
    ok.

%%--------------------------------------------------------------------
%% @doc Retrieves a value from the cache.
%%
%% Returns the cached value if found and not expired.
%% Returns {error, not_found} if key does not exist or has expired.
%%
%% == Example
%%
%% ```erlang
%% > ok = wf_cache:start().
%% ok
%% > ok = wf_cache:put(<<"pattern_1">>, #{data => 42}, 5000).
%% ok
%% > {ok, #{data => 42}} = wf_cache:get(<<"pattern_1">>).
%% {ok, #{data => 42}}
%% > wf_cache:get(<<"missing">>).
%% {error, not_found}
%% ```
%%
%% @param Key Cache key to retrieve
%% @return {ok, Value} | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: cache_key()) -> {ok, cache_value()} | {error, not_found}.

get(Key) ->
    start(),
    Now = erlang:system_time(millisecond),
    case ets:lookup(?CACHE_TABLE, Key) of
        [] ->
            {error, not_found};
        [#cache_entry{value = Value, expires_at = ExpiresAt}] ->
            case ExpiresAt > Now of
                true ->
                    {ok, Value};
                false ->
                    ets:delete(?CACHE_TABLE, Key),
                    {error, not_found}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a key exists in the cache and is not expired.
%%
%% Returns true if key exists and has not expired, false otherwise.
%%
%% == Example
%%
%% ```erlang
%% > ok = wf_cache:start().
%% ok
%% > ok = wf_cache:put(<<"key1">>, value, 5000).
%% ok
%% > wf_cache:exists(<<"key1">>).
%% true
%% > wf_cache:exists(<<"missing">>).
%% false
%% ```
%%
%% @param Key Cache key to check
%% @return boolean()
%%
%% @end
%%--------------------------------------------------------------------
-spec exists(Key :: cache_key()) -> boolean().

exists(Key) ->
    start(),
    Now = erlang:system_time(millisecond),
    case ets:lookup(?CACHE_TABLE, Key) of
        [] ->
            false;
        [#cache_entry{expires_at = ExpiresAt}] ->
            case ExpiresAt > Now of
                true ->
                    true;
                false ->
                    ets:delete(?CACHE_TABLE, Key),
                    false
            end
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a cache entry.
%%
%% Removes the entry with the specified key from the cache.
%% Does nothing if key does not exist.
%%
%% == Example
%%
%% ```erlang
%% > ok = wf_cache:start().
%% ok
%% > ok = wf_cache:put(<<"key1">>, value, 5000).
%% ok
%% > ok = wf_cache:delete(<<"key1">>).
%% ok
%% > wf_cache:exists(<<"key1">>).
%% false
%% ```
%%
%% @param Key Cache key to delete
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(Key :: cache_key()) -> ok.

delete(Key) ->
    start(),
    ets:delete(?CACHE_TABLE, Key),
    ok.

%%--------------------------------------------------------------------
%% @doc Clears all entries from the cache.
%%
%% Removes all cached entries regardless of expiration status.
%% Does nothing if cache is empty or does not exist.
%%
%% == Example
%%
%% ```erlang
%% > ok = wf_cache:start().
%% ok
%% > ok = wf_cache:put(<<"key1">>, value1, 5000).
%% ok
%% > ok = wf_cache:put(<<"key2">>, value2, 5000).
%% ok
%% > ok = wf_cache:clear().
%% ok
%% > wf_cache:stats().
%% #{size => 0, memory => 0}
%% ```
%%
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec clear() -> ok.

clear() ->
    start(),
    ets:delete_all_objects(?CACHE_TABLE),
    ok.

%%--------------------------------------------------------------------
%% @doc Removes all expired entries from the cache.
%%
%% Scans the cache table and deletes entries whose TTL has expired.
%% Returns the number of entries removed.
%%
%% == Example
%%
%% ```erlang
%% > ok = wf_cache:start().
%% ok
%% > ok = wf_cache:put(<<"key1">>, value1, 1).
%% ok
%% > ok = wf_cache:put(<<"key2">>, value2, 60000).
%% ok
%% > timer:sleep(10).
%% ok
%% > Count = wf_cache:cleanup().
%% 1
%% ```
%%
%% @return non_neg_integer() Number of entries removed
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup() -> non_neg_integer().

cleanup() ->
    start(),
    Now = erlang:system_time(millisecond),
    ets:foldl(
        fun(#cache_entry{key = Key, expires_at = ExpiresAt}, Count) ->
            case ExpiresAt =< Now of
                true ->
                    ets:delete(?CACHE_TABLE, Key),
                    Count + 1;
                false ->
                    Count
            end
        end,
        0,
        ?CACHE_TABLE
    ).

%%--------------------------------------------------------------------
%% @doc Returns cache statistics.
%%
%% Returns a map with cache size and memory usage information.
%% Size is the number of entries, memory is in bytes.
%%
%% == Example
%%
%% ```erlang
%% > ok = wf_cache:start().
%% ok
%% > ok = wf_cache:put(<<"key1">>, large_value, 5000).
%% ok
%% > #{size := Size} = wf_cache:stats(), Size > 0.
%% true
%% ```
%%
%% @return cache_stats() Map with size and memory information
%%
%% @end
%%--------------------------------------------------------------------
-spec stats() -> cache_stats().

stats() ->
    start(),
    Info = ets:info(?CACHE_TABLE),
    Size = proplists:get_value(size, Info, 0),
    Memory = proplists:get_value(memory, Info, 0),
    #{
        size => Size,
        memory => Memory
    }.
