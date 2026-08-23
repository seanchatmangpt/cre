%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joorgen@cuneiform-lang.org>
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
%% @doc Dual-Write Adapter for Mnesia to Spanner Migration
%%
%% This module provides a gen_server that manages dual-write operations
%% during migration from Mnesia to Cloud Spanner. It ensures data consistency
%% by writing to both databases and reading with intelligent fallback.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Dual Writes:</b> Synchronous writes to both Mnesia and Spanner</li>
%%   <li><b>Async Spanner:</b> Non-blocking Spanner writes with retry logic</li>
%%   <li><b>Smart Reads:</b> Read from Spanner, fallback to Mnesia on failure</li>
%%   <li><b>Circuit Breaker:</b> Automatic failure detection and recovery</li>
%%   <li><b>Migration Control:</b> Enable/disable dual-write mode</li>
%% </ul>
%%
%% <h3>Migration Modes</h3>
%%
%% <ol>
%%   <li><b>mnesia_only:</b> All operations go to Mnesia only</li>
%%   <li><b>dual_write:</b> Writes go to both, reads prefer Spanner</li>
%%   <li><b>spanner_only:</b> All operations go to Spanner only</li>
%% </ol>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Start the adapter
%% {ok, Pid} = dual_write_adapter:start_link().
%%
%% %% Enable dual-write mode
%% ok = dual_write_adapter:enable_dual_write().
%%
%% %% Save a case (writes to both Mnesia and Spanner)
%% ok = dual_write_adapter:save_case([{id, 1}, {data, "test"}]).
%%
%% %% Load a case (reads from Spanner, falls back to Mnesia)
%% {ok, Case} = dual_write_adapter:load_case(1).
%%
%% %% Get migration status
%% Status = dual_write_adapter:get_migration_status().
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(dual_write_adapter).
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API functions
-export([start_link/0, start_link/1,
         save_case/1, load_case/1,
         delete_case/1,
         enable_dual_write/0, disable_dual_write/0,
         set_migration_mode/1,
         sync_state/0,
         get_migration_status/0,
         get_stats/0,
         reset_stats/0,
         health_check/0]).

%% gen_server callback functions
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

%%====================================================================
%% Type definitions
%%====================================================================

-type migration_mode() :: mnesia_only | dual_write | spanner_only.
-type case_id() :: term().
-type case_data() :: [{atom(), term()}].
-type write_result() :: ok | {error, term()}.
-type read_result() :: {ok, case_data()} | {error, not_found | term()}.
-type circuit_state() :: closed | open | half_open.
-type adapter_stats() :: #{mnesia_writes => non_neg_integer(),
                           spanner_writes => non_neg_integer(),
                           spanner_failures => non_neg_integer(),
                           mnesia_reads => non_neg_integer(),
                           spanner_reads => non_neg_integer(),
                           fallback_reads => non_neg_integer(),
                           sync_discrepancies => non_neg_integer()}.
-type adapter_state() :: #{migration_mode => migration_mode(),
                           circuit_state => circuit_state(),
                           circuit_failures => non_neg_integer(),
                           circuit_threshold => pos_integer(),
                           circuit_timeout => non_neg_integer(),
                           retry_queue => queue:queue({case_id(), case_data(), non_neg_integer()}),
                           max_retries => non_neg_integer(),
                           stats => adapter_stats(),
                           spanner_available => boolean()}.

-export_type([migration_mode/0, case_id/0, case_data/0, adapter_stats/0]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts the dual-write adapter with default configuration.
%%
%%      Registered locally as `dual_write_adapter`. Uses dual_write mode
%%      with circuit breaker threshold of 5 failures.
%%
%% @returns `{ok, Pid}' | `{error, Reason}'
%%
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Starts the dual-write adapter with custom options.
%%
%%      Options:
%%      - `{migration_mode, Mode}' - mnesia_only, dual_write, or spanner_only
%%      - `{circuit_threshold, N}' - Failures before opening circuit (default 5)
%%      - `{circuit_timeout, Ms}' - Milliseconds before half-open state (default 30000)
%%      - `{max_retries, N}' - Maximum retry attempts for Spanner writes (default 3)
%%
-spec start_link([proplists:property()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Saves a case record to both databases (in dual_write mode).
%%
%%      In dual_write mode, writes synchronously to Mnesia and
%%      asynchronously to Spanner. Failed Spanner writes are queued
%%      for retry based on circuit breaker state.
%%
%% @param CaseData List of {Field, Value} tuples representing the case
%% @returns `ok' | `{error, Reason}'
%%
-spec save_case(case_data()) -> write_result().
save_case(CaseData) when is_list(CaseData) ->
    gen_server:call(?MODULE, {save_case, CaseData}, infinity).

%% @doc Loads a case by ID from Spanner with fallback to Mnesia.
%%
%%      Read strategy depends on migration mode:
%%      - `mnesia_only': Read only from Mnesia
%%      - `dual_write': Read from Spanner, fallback to Mnesia
%%      - `spanner_only': Read only from Spanner
%%
%% @param CaseId The case identifier
%% @returns `{ok, CaseData}' | `{error, not_found}' | `{error, Reason}'
%%
-spec load_case(case_id()) -> read_result().
load_case(CaseId) ->
    gen_server:call(?MODULE, {load_case, CaseId}, infinity).

%% @doc Deletes a case from both databases (in dual_write mode).
%%
%%      Performs delete operation on both Mnesia and Spanner when
%%      in dual_write mode. Spanner deletion is fire-and-forget.
%%
%% @param CaseId The case identifier
%% @returns `ok' | `{error, Reason}'
%%
-spec delete_case(case_id()) -> write_result().
delete_case(CaseId) ->
    gen_server:call(?MODULE, {delete_case, CaseId}, infinity).

%% @doc Enables dual-write migration mode.
%%
%%      Sets the adapter to write to both Mnesia and Spanner.
%%      Reads will prefer Spanner with Mnesia fallback.
%%
%% @returns `ok'
%%
-spec enable_dual_write() -> ok.
enable_dual_write() ->
    gen_server:call(?MODULE, enable_dual_write).

%% @doc Disables dual-write mode, reverts to Mnesia only.
%%
%%      Sets the adapter to mnesia_only mode for safety.
%%      Use this when Spanner is experiencing issues.
%%
%% @returns `ok'
%%
-spec disable_dual_write() -> ok.
disable_dual_write() ->
    gen_server:call(?MODULE, disable_dual_write).

%% @doc Sets the migration mode explicitly.
%%
%%      Modes:
%%      - `mnesia_only' - All operations to Mnesia
%%      - `dual_write' - Writes to both, reads from Spanner
%%      - `spanner_only' - All operations to Spanner
%%
%% @param Mode The migration mode to set
%% @returns `ok'
%%
-spec set_migration_mode(migration_mode()) -> ok.
set_migration_mode(Mode) when Mode =:= mnesia_only;
                             Mode =:= dual_write;
                             Mode =:= spanner_only ->
    gen_server:call(?MODULE, {set_migration_mode, Mode}).

%% @doc Synchronizes state between Mnesia and Spanner.
%%
%%      Triggers a background sync process to compare and resolve
%%      differences between the two databases.
%%
%% @returns `{ok, SyncResult}' | `{error, Reason}'
%%
-spec sync_state() -> {ok, map()} | {error, term()}.
sync_state() ->
    gen_server:call(?MODULE, sync_state, 60000).

%% @doc Gets the current migration status and statistics.
%%
%%      Returns a map with current mode, circuit breaker state,
%%      write/read counts, and health information.
%%
%% @returns Map with migration status information
%%
-spec get_migration_status() -> #{atom() => term()}.
get_migration_status() ->
    gen_server:call(?MODULE, get_migration_status).

%% @doc Gets detailed statistics about adapter operations.
%%
%%      Returns counters for writes, reads, failures, and fallbacks.
%%
%% @returns Statistics map
%%
-spec get_stats() -> adapter_stats().
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% @doc Resets all statistics counters.
%%
%%      Clears all operation counts and discrepancy trackers.
%%
%% @returns `ok'
%%
-spec reset_stats() -> ok.
reset_stats() ->
    gen_server:call(?MODULE, reset_stats).

%% @doc Performs a health check on both databases.
%%
%%      Tests connectivity and responsiveness of Mnesia and Spanner.
%%
%% @returns Health map with status of each database
%%
-spec health_check() -> #{mnesia => ok | {error, term()},
                          spanner => ok | {error, term()}}.
health_check() ->
    gen_server:call(?MODULE, health_check).

%%====================================================================
%% gen_server callback functions
%%====================================================================

%% @private
init(Options) ->
    process_flag(trap_exit, true),

    MigrationMode = proplists:get_value(migration_mode, Options, dual_write),
    CircuitThreshold = proplists:get_value(circuit_threshold, Options, 5),
    CircuitTimeout = proplists:get_value(circuit_timeout, Options, 30000),
    MaxRetries = proplists:get_value(max_retries, Options, 3),

    State = #{
        migration_mode => MigrationMode,
        circuit_state => closed,
        circuit_failures => 0,
        circuit_threshold => CircuitThreshold,
        circuit_timeout => CircuitTimeout,
        retry_queue => queue:new(),
        max_retries => MaxRetries,
        stats => #{
            mnesia_writes => 0,
            spanner_writes => 0,
            spanner_failures => 0,
            mnesia_reads => 0,
            spanner_reads => 0,
            fallback_reads => 0,
            sync_discrepancies => 0
        },
        spanner_available => true
    },

    logger:info("Dual-write adapter started: mode=~p, circuit_threshold=~p",
                [MigrationMode, CircuitThreshold],
                [{info, "dual_write_init"}, {application, cre}]),

    {ok, State}.

%% @private
handle_call({save_case, CaseData}, _From, State = #{migration_mode := Mode,
                                                     retry_queue := Queue,
                                                     max_retries := MaxRetries}) ->
    CaseId = proplists:get_value(id, CaseData),
    {Reply, NewState} = handle_save_case(CaseId, CaseData, Mode, MaxRetries, Queue, State),
    {reply, Reply, NewState};

handle_call({load_case, CaseId}, _From, State = #{migration_mode := Mode,
                                                      stats := Stats}) ->
    {Reply, NewStats} = handle_load_case(CaseId, Mode, Stats),
    {reply, Reply, State#{stats => NewStats}};

handle_call({delete_case, CaseId}, _From, State = #{migration_mode := Mode}) ->
    {Reply, NewState} = handle_delete_case(CaseId, Mode, State),
    {reply, Reply, NewState};

handle_call(enable_dual_write, _From, State) ->
    logger:info("Dual-write mode enabled",
                [{info, "dual_write_enabled"}, {application, cre}]),
    {reply, ok, State#{migration_mode => dual_write}};

handle_call(disable_dual_write, _From, State) ->
    logger:info("Dual-write mode disabled, switching to mnesia_only",
                [{info, "dual_write_disabled"}, {application, cre}]),
    {reply, ok, State#{migration_mode => mnesia_only}};

handle_call({set_migration_mode, Mode}, _From, State) ->
    logger:info("Migration mode changed: ~p", [Mode],
                [{info, "mode_change"}, {application, cre}]),
    {reply, ok, State#{migration_mode => Mode}};

handle_call(sync_state, _From, State) ->
    {Reply, NewState} = handle_sync_state(State),
    {reply, Reply, NewState};

handle_call(get_migration_status, _From, State = #{migration_mode := Mode,
                                                   circuit_state := CircuitState,
                                                   circuit_failures := Failures,
                                                   stats := Stats}) ->
    Status = #{
        migration_mode => Mode,
        circuit_state => CircuitState,
        circuit_failures => Failures,
        spanner_available => maps:get(spanner_available, State, true),
        stats => Stats,
        retry_queue_size => queue:len(maps:get(retry_queue, State, queue:new()))
    },
    {reply, Status, State};

handle_call(get_stats, _From, State = #{stats := Stats}) ->
    {reply, Stats, State};

handle_call(reset_stats, _From, State) ->
    ResetStats = #{
        mnesia_writes => 0,
        spanner_writes => 0,
        spanner_failures => 0,
        mnesia_reads => 0,
        spanner_reads => 0,
        fallback_reads => 0,
        sync_discrepancies => 0
    },
    logger:info("Statistics reset",
                [{info, "stats_reset"}, {application, cre}]),
    {reply, ok, State#{stats => ResetStats}};

handle_call(health_check, _From, State) ->
    Health = #{
        mnesia => check_mnesia_health(),
        spanner => check_spanner_health(State)
    },
    {reply, Health, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({circuit_timeout, Ref}, State = #{circuit_state := open,
                                              circuit_timeout_ref := Ref}) ->
    logger:info("Circuit breaker transitioning to half-open",
                [{info, "circuit_half_open"}, {application, cre}]),
    {noreply, State#{circuit_state => half_open, circuit_failures => 0}};

handle_info(retry_spanner_writes, State = #{retry_queue := Queue,
                                            circuit_state := CircuitState,
                                            max_retries := MaxRetries}) ->
    case CircuitState of
        open ->
            %% Circuit is open, skip retries
            {noreply, State};
        _ ->
            {NewQueue, Failures, NewCircuitState} = process_retry_queue(
                Queue, MaxRetries, maps:get(circuit_failures, State, 0),
                maps:get(circuit_threshold, State, 5), []
            ),
            NewState = State#{
                retry_queue => NewQueue,
                circuit_failures => Failures,
                circuit_state => NewCircuitState
            },
            %% Schedule next retry if queue not empty
            case queue:is_empty(NewQueue) of
                false ->
                    erlang:send_after(5000, self(), retry_spanner_writes);
                true ->
                    ok
            end,
            {noreply, NewState}
    end;

handle_info({'EXIT', _Pid, Reason}, State) ->
    logger:error("Process exit: ~p", [Reason],
                 [{info, "process_exit"}, {application, cre}]),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    logger:info("Dual-write adapter stopping",
                [{info, "adapter_terminate"}, {application, cre}]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Handles save_case operations based on migration mode.
-spec handle_save_case(case_id(), case_data(), migration_mode(), non_neg_integer(),
                       queue:queue({case_id(), case_data(), non_neg_integer()}),
                       adapter_state()) ->
    {write_result(), adapter_state()}.
handle_save_case(CaseId, CaseData, mnesia_only, _MaxRetries, _Queue, State) ->
    %% Write to Mnesia only
    Result = write_to_mnesia(CaseId, CaseData),
    NewStats = update_write_stats(mnesia, Result, maps:get(stats, State, #{})),
    {Result, State#{stats => NewStats}};

handle_save_case(CaseId, CaseData, spanner_only, _MaxRetries, _Queue,
                  State = #{circuit_state := CircuitState}) ->
    %% Write to Spanner only, respecting circuit breaker
    case CircuitState of
        open ->
            {{error, circuit_open}, State};
        _ ->
            Result = write_to_spanner(CaseId, CaseData),
            NewState = update_circuit_state(Result, State),
            NewStats = update_write_stats(spanner, Result, maps:get(stats, NewState, #{})),
            {Result, NewState#{stats => NewStats}}
    end;

handle_save_case(CaseId, CaseData, dual_write, MaxRetries, Queue,
                  State = #{circuit_state := CircuitState}) ->
    %% Write to Mnesia synchronously
    MnesiaResult = write_to_mnesia(CaseId, CaseData),
    NewStats1 = update_write_stats(mnesia, MnesiaResult, maps:get(stats, State, #{})),

    %% Write to Spanner asynchronously
    case CircuitState of
        open ->
            %% Circuit open, queue for retry
            NewQueue = queue:in({CaseId, CaseData, 0}, Queue),
            logger:warning("Circuit open, queuing Spanner write for case ~p", [CaseId],
                           [{info, "write_queued"}, {application, cre}]),
            %% Schedule retry processor
            erlang:send_after(1000, self(), retry_spanner_writes),
            {MnesiaResult, State#{stats => NewStats1, retry_queue => NewQueue}};
        _ ->
            %% Circuit closed or half-open, attempt write
            SpannerResult = write_to_spanner_async(CaseId, CaseData),
            NewState = case SpannerResult of
                {error, _} when CircuitState =:= half_open ->
                    %% Failed in half-open, reopen circuit
                    State#{circuit_state => open,
                           circuit_failures => maps:get(circuit_failures, State, 0) + 1};
                _ ->
                    update_circuit_state(SpannerResult, State)
            end,
            NewStats2 = update_write_stats(spanner, SpannerResult, maps:get(stats, NewState, #{})),
            {MnesiaResult, NewState#{stats => NewStats2}}
    end.

%% @private Handles load_case operations based on migration mode.
-spec handle_load_case(case_id(), migration_mode(), adapter_stats()) ->
    {{ok, case_data()} | {error, term()}, adapter_stats()}.
handle_load_case(CaseId, mnesia_only, Stats) ->
    Result = read_from_mnesia(CaseId),
    NewStats = update_read_stats(mnesia, Result, Stats),
    {Result, NewStats};

handle_load_case(CaseId, spanner_only, Stats = #{circuit_state := CircuitState}) ->
    Result = case CircuitState of
        open ->
            read_from_spanner_fallback(CaseId);
        _ ->
            read_from_spanner(CaseId)
    end,
    NewStats = update_read_stats(spanner, Result, Stats),
    {Result, NewStats};

handle_load_case(CaseId, dual_write, Stats) ->
    %% Try Spanner first, fallback to Mnesia
    CircuitState = closed,
    Result = case CircuitState of
        open ->
            %% Circuit open, go directly to fallback
            logger:info("Circuit open, using Mnesia fallback for case ~p", [CaseId],
                        [{info, "read_fallback"}, {application, cre}]),
            read_from_mnesia(CaseId);
        _ ->
            case read_from_spanner(CaseId) of
                {error, _} ->
                    %% Spanner failed, fallback to Mnesia
                    logger:warning("Spanner read failed for case ~p, using Mnesia fallback",
                                   [CaseId],
                                   [{info, "read_fallback"}, {application, cre}]),
                    read_from_mnesia(CaseId);
                Success ->
                    Success
            end
    end,

    %% Update stats based on actual source
    NewStats = case Result of
        {ok, _} ->
            update_read_stats(spanner, Result, Stats);
        {error, _} ->
            update_read_stats(mnesia, Result, Stats)
    end,
    {Result, NewStats}.

%% @private Handles delete_case operations.
-spec handle_delete_case(case_id(), migration_mode(), adapter_state()) ->
    {write_result(), adapter_state()}.
handle_delete_case(CaseId, mnesia_only, State) ->
    Result = delete_from_mnesia(CaseId),
    {Result, State};

handle_delete_case(CaseId, spanner_only, State = #{circuit_state := CircuitState}) ->
    Result = case CircuitState of
        open -> {error, circuit_open};
        _ -> delete_from_spanner(CaseId)
    end,
    NewState = update_circuit_state(Result, State),
    {Result, NewState};

handle_delete_case(CaseId, dual_write, State) ->
    %% Delete from both Mnesia and Spanner
    MnesiaResult = delete_from_mnesia(CaseId),
    SpannerResult = delete_from_spanner_async(CaseId),
    NewState = update_circuit_state(SpannerResult, State),
    {{MnesiaResult, SpannerResult}, NewState}.

%% @private Handles state synchronization.
-spec handle_sync_state(adapter_state()) ->
    {{ok, map()}, adapter_state()} | {{error, term()}, adapter_state()}.
handle_sync_state(State) ->
    logger:info("Starting Mnesia to Spanner sync",
                [{info, "sync_start"}, {application, cre}]),

    %% Get all case IDs from Mnesia
    MnesiaCases = get_all_mnesia_cases(),

    %% Compare with Spanner
    {SyncResult, Discrepancies} = compare_and_sync(MnesiaCases),

    %% Update stats
    CurrentStats = maps:get(stats, State, #{}),
    NewStats = CurrentStats#{sync_discrepancies =>
        maps:get(sync_discrepancies, CurrentStats, 0) + Discrepancies},

    logger:info("Sync completed: discrepancies=~p", [Discrepancies],
                [{info, "sync_complete"}, {application, cre}]),

    {{ok, SyncResult}, State#{stats => NewStats}}.

%% @private Updates circuit breaker state based on operation result.
-spec update_circuit_state({ok, any()} | {error, term()}, adapter_state()) -> adapter_state().
update_circuit_state({ok, _}, State = #{circuit_state := half_open}) ->
    %% Success in half-open, close the circuit
    Ref = maps:get(circuit_timeout_ref, State, undefined),
    case Ref of
        undefined -> ok;
        _ -> erlang:cancel_timer(Ref)
    end,
    logger:info("Circuit breaker closed after successful recovery",
                [{info, "circuit_closed"}, {application, cre}]),
    State#{circuit_state => closed, circuit_failures => 0, spanner_available => true};
update_circuit_state({ok, _}, State) ->
    %% Success, reset failure count
    State#{circuit_failures => 0, spanner_available => true};
update_circuit_state({error, _}, State = #{circuit_state := closed,
                                           circuit_failures := _Failures,
                                           circuit_threshold := Threshold,
                                           circuit_timeout := Timeout}) ->
    Failures = maps:get(circuit_failures, State, 0) + 1,
    case Failures >= Threshold of
        true ->
            %% Open the circuit
            Ref = erlang:send_after(Timeout, self(), {circuit_timeout, make_ref()}),
            logger:error("Circuit breaker opened after ~p failures",
                        [Failures],
                        [{info, "circuit_open"}, {application, cre}]),
            State#{circuit_state => open,
                   circuit_failures => Failures,
                   circuit_timeout_ref => Ref,
                   spanner_available => false};
        false ->
            State#{circuit_failures => Failures, spanner_available => true}
    end;
update_circuit_state({error, _}, State) ->
    State.

%% @private Processes retry queue for failed Spanner writes.
-spec process_retry_queue(queue:queue({case_id(), case_data(), non_neg_integer()}),
                          non_neg_integer(), non_neg_integer(), pos_integer(),
                          [{case_id(), non_neg_integer()}]) ->
    {queue:queue({case_id(), case_data(), non_neg_integer()}), non_neg_integer(),
     circuit_state()}.
process_retry_queue(Queue, MaxRetries, Failures, Threshold, AccFailed) ->
    case queue:out(Queue) of
        {empty, EmptyQueue} ->
            {EmptyQueue, Failures, closed};
        {{value, {_CaseId, _CaseData, Attempt}}, RemainingQueue} when Attempt >= MaxRetries ->
            %% Max retries exceeded, log and drop
            logger:error("Max retries exceeded, dropping queued write",
                         [{info, "retry_dropped"}, {application, cre}]),
            process_retry_queue(RemainingQueue, MaxRetries, Failures + 1, Threshold, AccFailed);
        {{value, {CaseId, CaseData, Attempt}}, RemainingQueue} ->
            case write_to_spanner(CaseId, CaseData) of
                {ok, _} ->
                    logger:info("Retry successful for case ~p", [CaseId],
                                [{info, "retry_success"}, {application, cre}]),
                    %% Reset failures on success
                    NewFailures = case AccFailed of
                        [] -> 0;
                        _ -> Failures
                    end,
                    process_retry_queue(RemainingQueue, MaxRetries, NewFailures, Threshold, []);
                {error, Reason} ->
                    logger:warning("Retry failed for case ~p: ~p", [CaseId, Reason],
                                   [{info, "retry_failed"}, {application, cre}]),
                    %% Re-queue with incremented attempt
                    NewQueue = queue:in({CaseId, CaseData, Attempt + 1}, RemainingQueue),
                    {NewQueue, Failures + 1, open}
            end
    end.

%% @private Writes case data to Mnesia.
-spec write_to_mnesia(case_id(), case_data()) -> ok | {error, term()}.
write_to_mnesia(CaseId, CaseData) ->
    try
        {atomic, ok} = mnesia:transaction(fun() ->
            Record = list_to_tuple([case_record | [{id, CaseId} | CaseData]]),
            mnesia:write(case_table, Record, write)
        end),
        ok
    catch
        _:{aborted, Reason} -> {error, {mnesia_aborted, Reason}};
        _:Reason -> {error, {mnesia_error, Reason}}
    end.

%% @private Writes case data to Spanner (synchronous).
-spec write_to_spanner(case_id(), case_data()) -> {ok, term()} | {error, term()}.
write_to_spanner(_CaseId, _CaseData) ->
    %% TODO: Implement actual Spanner client call
    %% For now, simulate with some randomness for testing
    case rand:uniform(10) of
        N when N =< 8 ->
            {ok, {spanner_result, written}};
        _ ->
            {error, spanner_unavailable}
    end.

%% @private Writes case data to Spanner (asynchronous).
-spec write_to_spanner_async(case_id(), case_data()) -> {ok, reference()} | {error, term()}.
write_to_spanner_async(CaseId, CaseData) ->
    %% Spawn async process for Spanner write
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        Result = write_to_spanner(CaseId, CaseData),
        Parent ! {spanner_write_result, Ref, Result}
    end),
    {ok, Ref}.

%% @private Reads case data from Mnesia.
-spec read_from_mnesia(case_id()) -> {ok, case_data()} | {error, not_found | term()}.
read_from_mnesia(CaseId) ->
    try
        case mnesia:transaction(fun() ->
            mnesia:read(case_table, CaseId)
        end) of
            {atomic, []} ->
                {error, not_found};
            {atomic, [Record]} when is_tuple(Record), tuple_size(Record) > 1 ->
                [_Name | Fields] = tuple_to_list(Record),
                {ok, Fields};
            {atomic, Records} when is_list(Records) ->
                {ok, Records};
            {aborted, Reason} ->
                {error, {mnesia_aborted, Reason}}
        end
    catch
        _:Error -> {error, {mnesia_error, Error}}
    end.

%% @private Reads case data from Spanner.
-spec read_from_spanner(case_id()) -> {ok, case_data()} | {error, term()}.
read_from_spanner(_CaseId) ->
    %% TODO: Implement actual Spanner client call
    %% For now, simulate with some randomness for testing
    case rand:uniform(10) of
        N when N =< 7 ->
            {ok, [{id, _CaseId}, {data, "spanner_data"}]};
        _ ->
            {error, spanner_unavailable}
    end.

%% @private Reads from Spanner with forced fallback (when circuit is open).
-spec read_from_spanner_fallback(case_id()) -> {ok, case_data()} | {error, term()}.
read_from_spanner_fallback(_CaseId) ->
    {error, circuit_open}.

%% @private Deletes case from Mnesia.
-spec delete_from_mnesia(case_id()) -> ok | {error, term()}.
delete_from_mnesia(CaseId) ->
    try
        {atomic, ok} = mnesia:transaction(fun() ->
            mnesia:delete(case_table, CaseId, write)
        end),
        ok
    catch
        _:{aborted, Reason} -> {error, {mnesia_aborted, Reason}};
        _:Error -> {error, {mnesia_error, Error}}
    end.

%% @private Deletes case from Spanner (synchronous).
-spec delete_from_spanner(case_id()) -> ok | {error, term()}.
delete_from_spanner(_CaseId) ->
    %% TODO: Implement actual Spanner client call
    {ok, deleted}.

%% @private Deletes case from Spanner (asynchronous).
-spec delete_from_spanner_async(case_id()) -> {ok, reference()} | {error, term()}.
delete_from_spanner_async(CaseId) ->
    Parent = self(),
    Ref = make_ref(),
    spawn(fun() ->
        Result = delete_from_spanner(CaseId),
        Parent ! {spanner_delete_result, Ref, Result}
    end),
    {ok, Ref}.

%% @private Gets all case IDs from Mnesia.
-spec get_all_mnesia_cases() -> [case_id()].
get_all_mnesia_cases() ->
    try
        case mnesia:transaction(fun() ->
            mnesia:all_keys(case_table)
        end) of
            {atomic, Keys} -> Keys;
            _ -> []
        end
    catch
        _:_ -> []
    end.

%% @private Compares and syncs cases between Mnesia and Spanner.
-spec compare_and_sync([case_id()]) -> {map(), non_neg_integer()}.
compare_and_sync(CaseIds) ->
    SyncResults = lists:map(fun(CaseId) ->
        MnesiaResult = read_from_mnesia(CaseId),
        SpannerResult = read_from_spanner(CaseId),
        case {MnesiaResult, SpannerResult} of
            {{ok, MnesiaData}, {error, _}} ->
                %% Mnesia has data, Spanner doesn't - sync to Spanner
                write_to_spanner(CaseId, MnesiaData),
                {synced_to_spanner, CaseId};
            {{error, _}, {ok, _}} ->
                %% Spanner has data, Mnesia doesn't - sync to Mnesia
                {synced_to_mnesia, CaseId};
            {{ok, MnesiaData}, {ok, SpannerData}} when MnesiaData =/= SpannerData ->
                %% Data mismatch - record discrepancy
                {discrepancy, CaseId, MnesiaData, SpannerData};
            _ ->
                {in_sync, CaseId}
        end
    end, CaseIds),

    DiscrepancyCount = length([R || R <- SyncResults,
                                    element(1, R) =:= discrepancy]),

    #{synced_cases => length(CaseIds),
      discrepancies => DiscrepancyCount,
      discrepancy_count => DiscrepancyCount,
      details => SyncResults}.

%% @private Checks Mnesia health.
-spec check_mnesia_health() -> ok | {error, term()}.
check_mnesia_health() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        no -> {error, mnesia_not_running}
    end.

%% @private Checks Spanner health.
-spec check_spanner_health(adapter_state()) -> ok | {error, term()}.
check_spanner_health(#{circuit_state := open}) ->
    {error, circuit_open};
check_spanner_health(_) ->
    %% TODO: Implement actual Spanner health check
    ok.

%% @private Updates write statistics.
-spec update_write_stats(mnesia | spanner, ok | {error, term()}, adapter_stats()) ->
    adapter_stats().
update_write_stats(mnesia, ok, Stats) ->
    Stats#{mnesia_writes => maps:get(mnesia_writes, Stats, 0) + 1};
update_write_stats(mnesia, {error, _}, Stats) ->
    Stats;
update_write_stats(spanner, {ok, _}, Stats) ->
    Stats#{spanner_writes => maps:get(spanner_writes, Stats, 0) + 1};
update_write_stats(spanner, {error, _}, Stats) ->
    Stats#{spanner_failures => maps:get(spanner_failures, Stats, 0) + 1}.

%% @private Updates read statistics.
-spec update_read_stats(mnesia | spanner, {ok, _} | {error, _}, adapter_stats()) ->
    adapter_stats().
update_read_stats(mnesia, {ok, _}, Stats) ->
    Stats#{mnesia_reads => maps:get(mnesia_reads, Stats, 0) + 1};
update_read_stats(spanner, {ok, _}, Stats) ->
    Stats#{spanner_reads => maps:get(spanner_reads, Stats, 0) + 1};
update_read_stats(_, {error, _}, Stats) ->
    Stats#{fallback_reads => maps:get(fallback_reads, Stats, 0) + 1}.
