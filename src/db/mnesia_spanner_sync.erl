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
%% @doc Background Sync Process for Mnesia to Spanner Migration
%%
%% This module provides a background process that continuously compares
%% Mnesia and Spanner data, detects inconsistencies, and resolves conflicts.
%% It runs as a gen_server with configurable sync intervals and batch sizes.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Continuous Sync:</b> Periodic comparison of both databases</li>
%%   <li><b>Batch Processing:</b> Efficient processing of large datasets</li>
%%   <li><b>Conflict Resolution:</b> Configurable strategies for discrepancies</li>
%%   <li><b>Detailed Logging:</b> Comprehensive logging of sync operations</li>
%%   <li><b>Progress Tracking:</b> Real-time sync status and metrics</li>
%% </ul>
%%
%% <h3>Conflict Resolution Strategies</h3>
%%
%% <ol>
%%   <li><b>mnesia_wins:</b> Always prefer Mnesia data</li>
%%   <li><b>spanner_wins:</b> Always prefer Spanner data</li>
%%   <li><b>newest_wins:</b> Prefer data with newer timestamp</li>
%%   <li><b>report_only:</b> Log discrepancies without resolving</li>
%% </ol>
%%
%% <h3>Examples</h3>
%%
%% ```erlang
%% %% Start the sync process
%% {ok, Pid} = mnesia_spanner_sync:start_link().
%%
%% %% Trigger immediate sync
%% {ok, Result} = mnesia_spanner_sync:sync_now().
%%
%% %% Get sync status
%% Status = mnesia_spanner_sync:get_status().
%%
%% %% Set conflict resolution strategy
%% ok = mnesia_spanner_sync:set_resolution_strategy(newest_wins).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(mnesia_spanner_sync).
-behavior(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API functions
-export([start_link/0, start_link/1,
         sync_now/0,
         get_status/0,
         set_resolution_strategy/1,
         set_sync_interval/1,
         set_batch_size/1,
         pause_sync/0,
         resume_sync/0,
         get_sync_report/0,
         reset_metrics/0]).

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

-type resolution_strategy() :: mnesia_wins | spanner_wins | newest_wins | report_only.
-type sync_status() :: idle | syncing | paused | error.
-type sync_metrics() :: #{total_records => non_neg_integer(),
                          synced_records => non_neg_integer(),
                          discrepancies_found => non_neg_integer(),
                          discrepancies_resolved => non_neg_integer(),
                          last_sync_time => calendar:datetime() | undefined,
                          last_sync_duration => non_neg_integer() | undefined,
                          total_sync_count => non_neg_integer()}.
-type sync_state() :: #{resolution_strategy => resolution_strategy(),
                        sync_status => sync_status(),
                        sync_interval => non_neg_integer(),
                        batch_size => pos_integer(),
                        sync_timer => reference() | undefined,
                        metrics => sync_metrics(),
                        current_batch => non_neg_integer(),
                        pause_reason => term() | undefined}.

-export_type([resolution_strategy/0, sync_status/0, sync_metrics/0]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc Starts the sync process with default configuration.
%%
%%      Registered locally as `mnesia_spanner_sync`. Uses 60 second
%%      sync interval with batch size of 100 records.
%%
%% @returns `{ok, Pid}' | `{error, Reason}'
%%
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Starts the sync process with custom options.
%%
%%      Options:
%%      - `{sync_interval, Millis}' - Time between syncs (default 60000)
%%      - `{batch_size, N}' - Records per batch (default 100)
%%      - `{resolution_strategy, Strategy}' - Conflict resolution (default mnesia_wins)
%%      - `{auto_start, boolean()' - Start syncing immediately (default true)
%%
-spec start_link([proplists:property()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Triggers an immediate sync operation.
%%
%%      Runs a full sync regardless of scheduled interval.
%%      Returns detailed results of the sync operation.
%%
%% @returns `{ok, SyncResult}' | `{error, Reason}'
%%
-spec sync_now() -> {ok, map()} | {error, term()}.
sync_now() ->
    gen_server:call(?MODULE, sync_now, 60000).

%% @doc Gets the current sync status and metrics.
%%
%%      Returns information about sync state, progress, and
%%      historical metrics.
%%
%% @returns Map with sync status information
%%
-spec get_status() -> #{atom() => term()}.
get_status() ->
    gen_server:call(?MODULE, get_status).

%% @doc Sets the conflict resolution strategy.
%%
%%      Strategies:
%%      - `mnesia_wins' - Mnesia data is authoritative
%%      - `spanner_wins' - Spanner data is authoritative
%%      - `newest_wins' - Data with newer timestamp wins
%%      - `report_only' - Log only, don't resolve
%%
%% @param Strategy The resolution strategy to use
%% @returns `ok'
%%
-spec set_resolution_strategy(resolution_strategy()) -> ok.
set_resolution_strategy(Strategy) when Strategy =:= mnesia_wins;
                                      Strategy =:= spanner_wins;
                                      Strategy =:= newest_wins;
                                      Strategy =:= report_only ->
    gen_server:call(?MODULE, {set_resolution_strategy, Strategy}).

%% @doc Sets the interval between automatic syncs.
%%
%%      @param IntervalMs Milliseconds between sync operations
%%      @returns `ok'
%%
-spec set_sync_interval(non_neg_integer()) -> ok.
set_sync_interval(IntervalMs) when is_integer(IntervalMs), IntervalMs >= 1000 ->
    gen_server:call(?MODULE, {set_sync_interval, IntervalMs}).

%% @doc Sets the batch size for sync operations.
%%
%%      Larger batches are more efficient but use more memory.
%%
%% @param BatchSize Number of records to process per batch
%% @returns `ok'
%%
-spec set_batch_size(pos_integer()) -> ok.
set_batch_size(BatchSize) when is_integer(BatchSize), BatchSize >= 1 ->
    gen_server:call(?MODULE, {set_batch_size, BatchSize}).

%% @doc Pauses automatic syncing.
%%
%%      Stops the scheduled sync timer. Manual syncs (sync_now/0)
%%      can still be triggered.
%%
%% @returns `ok'
%%
-spec pause_sync() -> ok.
pause_sync() ->
    gen_server:call(?MODULE, pause_sync).

%% @doc Resumes automatic syncing.
%%
%%      Restarts the scheduled sync timer with the configured interval.
%%
%% @returns `ok'
%%
-spec resume_sync() -> ok.
resume_sync() ->
    gen_server:call(?MODULE, resume_sync).

%% @doc Gets a detailed report of the last sync operation.
%%
%%      Returns comprehensive information about discrepancies,
%%      resolutions, and timing.
%%
%% @returns Sync report map
%%
-spec get_sync_report() -> map().
get_sync_report() ->
    gen_server:call(?MODULE, get_sync_report).

%% @doc Resets all sync metrics.
%%
%%      Clears counters and timing statistics.
%%
%% @returns `ok'
%%
-spec reset_metrics() -> ok.
reset_metrics() ->
    gen_server:call(?MODULE, reset_metrics).

%%====================================================================
%% gen_server callback functions
%%====================================================================

%% @private
init(Options) ->
    process_flag(trap_exit, true),

    SyncInterval = proplists:get_value(sync_interval, Options, 60000),
    BatchSize = proplists:get_value(batch_size, Options, 100),
    Strategy = proplists:get_value(resolution_strategy, Options, mnesia_wins),
    AutoStart = proplists:get_value(auto_start, Options, true),

    Metrics = #{
        total_records => 0,
        synced_records => 0,
        discrepancies_found => 0,
        discrepancies_resolved => 0,
        last_sync_time => undefined,
        last_sync_duration => undefined,
        total_sync_count => 0
    },

    State = #{
        resolution_strategy => Strategy,
        sync_status => idle,
        sync_interval => SyncInterval,
        batch_size => BatchSize,
        sync_timer => undefined,
        metrics => Metrics,
        current_batch => 0,
        pause_reason => undefined,
        last_sync_report => #{}
    },

    logger:info("Mnesia-Spanner sync started: interval=~p, batch_size=~p, strategy=~p",
                [SyncInterval, BatchSize, Strategy],
                [{info, "sync_init"}, {application, cre}]),

    %% Start sync timer if auto_start is enabled
    NewState = case AutoStart of
        true ->
            TimerRef = schedule_sync(SyncInterval),
            State#{sync_timer => TimerRef};
        false ->
            State
    end,

    {ok, NewState}.

%% @private
handle_call(sync_now, _From, State = #{sync_status := Status}) ->
    case Status of
        syncing ->
            {reply, {error, already_syncing}, State};
        paused ->
            {reply, {error, sync_paused}, State};
        _ ->
            {Reply, NewState} = perform_sync(State),
            {reply, Reply, NewState}
    end;

handle_call(get_status, _From, State = #{sync_status := Status,
                                         metrics := Metrics,
                                         resolution_strategy := Strategy,
                                         current_batch := Batch}) ->
    StatusMap = #{
        sync_status => Status,
        resolution_strategy => Strategy,
        current_batch => Batch,
        metrics => Metrics,
        sync_interval => maps:get(sync_interval, State, 60000),
        batch_size => maps:get(batch_size, State, 100)
    },
    {reply, StatusMap, State};

handle_call({set_resolution_strategy, Strategy}, _From, State) ->
    logger:info("Resolution strategy changed: ~p", [Strategy],
                [{info, "strategy_change"}, {application, cre}]),
    {reply, ok, State#{resolution_strategy => Strategy}};

handle_call({set_sync_interval, IntervalMs}, _From, State = #{sync_timer := OldTimer}) ->
    %% Cancel old timer and schedule new one
    case OldTimer of
        undefined -> ok;
        _ -> erlang:cancel_timer(OldTimer)
    end,
    NewTimer = schedule_sync(IntervalMs),
    logger:info("Sync interval changed: ~p", [IntervalMs],
                [{info, "interval_change"}, {application, cre}]),
    {reply, ok, State#{sync_interval => IntervalMs, sync_timer => NewTimer}};

handle_call({set_batch_size, BatchSize}, _From, State) ->
    logger:info("Batch size changed: ~p", [BatchSize],
                [{info, "batch_size_change"}, {application, cre}]),
    {reply, ok, State#{batch_size => BatchSize}};

handle_call(pause_sync, _From, State = #{sync_timer := Timer}) ->
    case Timer of
        undefined -> ok;
        _ -> erlang:cancel_timer(Timer)
    end,
    logger:info("Sync paused",
                [{info, "sync_paused"}, {application, cre}]),
    {reply, ok, State#{sync_status => paused, sync_timer => undefined, pause_reason => manual}};

handle_call(resume_sync, _From, State = #{sync_interval := Interval}) ->
    TimerRef = schedule_sync(Interval),
    logger:info("Sync resumed",
                [{info, "sync_resumed"}, {application, cre}]),
    {reply, ok, State#{sync_status => idle, sync_timer => TimerRef, pause_reason => undefined}};

handle_call(get_sync_report, _From, State = #{last_sync_report := Report}) ->
    {reply, Report, State};

handle_call(reset_metrics, _From, State) ->
    ResetMetrics = #{
        total_records => 0,
        synced_records => 0,
        discrepancies_found => 0,
        discrepancies_resolved => 0,
        last_sync_time => undefined,
        last_sync_duration => undefined,
        total_sync_count => 0
    },
    logger:info("Sync metrics reset",
                [{info, "metrics_reset"}, {application, cre}]),
    {reply, ok, State#{metrics => ResetMetrics}};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(sync_trigger, State = #{sync_status := Status}) ->
    case Status of
        syncing ->
            %% Already syncing, reschedule
            Interval = maps:get(sync_interval, State, 60000),
            TimerRef = schedule_sync(Interval),
            {noreply, State#{sync_timer => TimerRef}};
        paused ->
            {noreply, State};
        _ ->
            {_, NewState} = perform_sync(State),
            %% Schedule next sync
            Interval = maps:get(sync_interval, NewState, 60000),
            TimerRef = schedule_sync(Interval),
            {noreply, NewState#{sync_timer => TimerRef}}
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
    logger:info("Mnesia-Spanner sync stopping",
                [{info, "sync_terminate"}, {application, cre}]),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Performs a full sync operation.
-spec perform_sync(sync_state()) -> {{ok, map()}, sync_state()} | {{error, term()}, sync_state()}.
perform_sync(State) ->
    StartTime = erlang:monotonic_time(millisecond),

    %% Update status to syncing
    State1 = State#{sync_status => syncing},

    logger:info("Starting Mnesia to Spanner sync",
                [{info, "sync_start"}, {application, cre}]),

    %% Get all case IDs from Mnesia
    MnesiaCases = get_all_case_ids(),
    TotalCount = length(MnesiaCases),

    %% Process in batches
    BatchSize = maps:get(batch_size, State1, 100),
    Strategy = maps:get(resolution_strategy, State1, mnesia_wins),

    {SyncResult, State2} = process_sync_batches(
        MnesiaCases, BatchSize, Strategy, State1, 0, 0, 0, []
    ),

    EndTime = erlang:monotonic_time(millisecond),
    Duration = EndTime - StartTime,

    %% Update metrics
    OldMetrics = maps:get(metrics, State2, #{}),
    NewMetrics = OldMetrics#{
        total_records => TotalCount,
        synced_records => maps:get(synced_records, SyncResult, 0),
        discrepancies_found => maps:get(discrepancies_found, SyncResult, 0),
        discrepancies_resolved => maps:get(discrepancies_resolved, SyncResult, 0),
        last_sync_time => calendar:universal_time(),
        last_sync_duration => Duration,
        total_sync_count => maps:get(total_sync_count, OldMetrics, 0) + 1
    },

    %% Build sync report
    SyncReport = #{
        started_at => StartTime,
        completed_at => EndTime,
        duration_ms => Duration,
        total_records => TotalCount,
        discrepancies => SyncResult,
        resolution_strategy => Strategy
    },

    logger:info("Sync completed: records=~p, duration=~pms, discrepancies=~p",
                [TotalCount, Duration, maps:get(discrepancies_found, SyncResult, 0)],
                [{info, "sync_complete"}, {application, cre}]),

    State3 = State2#{
        sync_status => idle,
        metrics => NewMetrics,
        current_batch => 0,
        last_sync_report => SyncReport
    },

    {{ok, SyncReport}, State3}.

%% @private Processes case IDs in batches.
-spec process_sync_batches([term()], pos_integer(), resolution_strategy(),
                           sync_state(), non_neg_integer(), non_neg_integer(),
                           non_neg_integer(), [map()]) ->
    {map(), sync_state()}.
process_sync_batches([], _BatchSize, _Strategy, State, Synced, Discrepancies, Resolved, Acc) ->
    %% All batches processed
    Result = #{
        synced_records => Synced,
        discrepancies_found => Discrepancies,
        discrepancies_resolved => Resolved,
        discrepancy_details => Acc
    },
    {Result, State};
process_sync_batches(Cases, BatchSize, Strategy, State, Synced, Discrepancies, Resolved, Acc) ->
    %% Get next batch
    {Batch, Remaining} = case length(Cases) > BatchSize of
        true -> lists:split(BatchSize, Cases);
        false -> {Cases, []}
    end,

    %% Process batch
    {BatchSynced, BatchDiscrepancies, BatchResolved, BatchDetails} =
        process_batch(Batch, Strategy),

    %% Update state
    NewState = State#{current_batch => maps:get(current_batch, State, 0) + 1},

    %% Continue with remaining
    process_sync_batches(
        Remaining,
        BatchSize,
        Strategy,
        NewState,
        Synced + BatchSynced,
        Discrepancies + BatchDiscrepancies,
        Resolved + BatchResolved,
        Acc ++ BatchDetails
    ).

%% @private Processes a single batch of case IDs.
-spec process_batch([term()], resolution_strategy()) ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer(), [map()]}.
process_batch(CaseIds, Strategy) ->
    lists:foldl(
        fun(CaseId, {Synced, Discrepancies, Resolved, Acc}) ->
            case compare_and_resolve(CaseId, Strategy) of
                {in_sync, _} ->
                    {Synced + 1, Discrepancies, Resolved, Acc};
                {discrepancy_resolved, Detail} ->
                    {Synced + 1, Discrepancies + 1, Resolved + 1, [Detail | Acc]};
                {discrepancy_logged, Detail} ->
                    {Synced + 1, Discrepancies + 1, Resolved, [Detail | Acc]};
                {synced_to_spanner, Detail} ->
                    {Synced + 1, Discrepancies, Resolved, [Detail | Acc]};
                {synced_to_mnesia, Detail} ->
                    {Synced + 1, Discrepancies, Resolved, [Detail | Acc]};
                {error, Reason} ->
                    logger:error("Failed to sync case ~p: ~p", [CaseId, Reason],
                                [{info, "sync_error"}, {application, cre}]),
                    {Synced, Discrepancies, Resolved, Acc}
            end
        end,
        {0, 0, 0, []},
        CaseIds
    ).

%% @private Compares and resolves a single case.
-spec compare_and_resolve(term(), resolution_strategy()) ->
    {in_sync, term()} |
    {discrepancy_resolved, map()} |
    {discrepancy_logged, map()} |
    {synced_to_spanner, map()} |
    {synced_to_mnesia, map()} |
    {error, term()}.
compare_and_resolve(CaseId, Strategy) ->
    MnesiaResult = read_from_mnesia(CaseId),
    SpannerResult = read_from_spanner(CaseId),

    case {MnesiaResult, SpannerResult} of
        {{ok, MnesiaData}, {ok, SpannerData}} when MnesiaData =:= SpannerData ->
            %% Data is in sync
            {in_sync, CaseId};

        {{ok, MnesiaData}, {ok, SpannerData}} when MnesiaData =/= SpannerData ->
            %% Data mismatch - resolve based on strategy
            Detail = #{
                case_id => CaseId,
                mnesia_data => MnesiaData,
                spanner_data => SpannerData,
                strategy => Strategy,
                timestamp => calendar:universal_time()
            },
            resolve_discrepancy(CaseId, MnesiaData, SpannerData, Strategy, Detail);

        {{ok, MnesiaData}, {error, not_found}} ->
            %% Only in Mnesia - sync to Spanner
            case write_to_spanner(CaseId, MnesiaData) of
                {ok, _} ->
                    {synced_to_spanner, #{case_id => CaseId, direction => mnesia_to_spanner}};
                {error, Reason} ->
                    {error, {spanner_write_failed, Reason}}
            end;

        {{error, not_found}, {ok, SpannerData}} ->
            %% Only in Spanner - sync to Mnesia
            case write_to_mnesia(CaseId, SpannerData) of
                ok ->
                    {synced_to_mnesia, #{case_id => CaseId, direction => spanner_to_mnesia}};
                {error, Reason} ->
                    {error, {mnesia_write_failed, Reason}}
            end;

        {{error, MnesiaReason}, {error, SpannerReason}} ->
            %% Error in both
            {error, {both_failed, MnesiaReason, SpannerReason}}
    end.

%% @private Resolves a data discrepancy based on strategy.
-spec resolve_discrepancy(term(), map(), map(), resolution_strategy(), map()) ->
    {discrepancy_resolved, map()} | {discrepancy_logged, map()}.
resolve_discrepancy(CaseId, MnesiaData, SpannerData, mnesia_wins, Detail) ->
    logger:info("Resolving discrepancy for ~p: mnesia_wins", [CaseId],
                [{info, "discrepancy_resolved"}, {application, cre}]),
    write_to_spanner(CaseId, MnesiaData),
    {discrepancy_resolved, Detail#{resolved_with => mnesia_wins}};
resolve_discrepancy(CaseId, MnesiaData, SpannerData, spanner_wins, Detail) ->
    logger:info("Resolving discrepancy for ~p: spanner_wins", [CaseId],
                [{info, "discrepancy_resolved"}, {application, cre}]),
    write_to_mnesia(CaseId, SpannerData),
    {discrepancy_resolved, Detail#{resolved_with => spanner_wins}};
resolve_discrepancy(CaseId, MnesiaData, SpannerData, newest_wins, Detail) ->
    %% Compare timestamps if available
    MnesiaTime = proplists:get_value(updated_at, MnesiaData, 0),
    SpannerTime = proplists:get_value(updated_at, SpannerData, 0),
    case MnesiaTime >= SpannerTime of
        true ->
            write_to_spanner(CaseId, MnesiaData),
            {discrepancy_resolved, Detail#{resolved_with => newest_wins, winner => mnesia}};
        false ->
            write_to_mnesia(CaseId, SpannerData),
            {discrepancy_resolved, Detail#{resolved_with => newest_wins, winner => spanner}}
    end;
resolve_discrepancy(CaseId, _MnesiaData, _SpannerData, report_only, Detail) ->
    logger:warning("Discrepancy detected for ~p (report_only mode)", [CaseId],
                   [{info, "discrepancy_logged"}, {application, cre}]),
    {discrepancy_logged, Detail#{resolved_with => none}}.

%% @private Schedules the next sync operation.
-spec schedule_sync(non_neg_integer()) -> reference().
schedule_sync(IntervalMs) ->
    erlang:send_after(IntervalMs, self(), sync_trigger).

%% @private Gets all case IDs from Mnesia.
-spec get_all_case_ids() -> [term()].
get_all_case_ids() ->
    try
        case mnesia:transaction(fun() ->
            case lists:member(case_table, mnesia:system_info(tables)) of
                true -> mnesia:all_keys(case_table);
                false -> []
            end
        end) of
            {atomic, Keys} -> Keys;
            _ -> []
        end
    catch
        _:_ -> []
    end.

%% @private Reads case data from Mnesia.
-spec read_from_mnesia(term()) -> {ok, map()} | {error, term()}.
read_from_mnesia(CaseId) ->
    try
        case lists:member(case_table, mnesia:system_info(tables)) of
            false ->
                {error, not_found};
            true ->
                case mnesia:transaction(fun() ->
                    mnesia:read(case_table, CaseId)
                end) of
                    {atomic, []} -> {error, not_found};
                    {atomic, [Record]} when is_tuple(Record) ->
                        [_Name | Fields] = tuple_to_list(Record),
                        {ok, maps:from_list(Fields)};
                    {aborted, AbortReason} -> {error, {mnesia_aborted, AbortReason}}
                end
        end
    catch
        _:CatchReason -> {error, {mnesia_error, CatchReason}}
    end.

%% @private Reads case data from Spanner.
-spec read_from_spanner(term()) -> {ok, map()} | {error, term()}.
read_from_spanner(_CaseId) ->
    %% TODO: Implement actual Spanner client call
    %% For testing, simulate some data
    {ok, #{id => _CaseId, data => "spanner_data", updated_at => 0}}.

%% @private Writes case data to Spanner.
-spec write_to_spanner(term(), map() | [tuple()]) -> {ok, term()} | {error, term()}.
write_to_spanner(_CaseId, _Data) ->
    %% TODO: Implement actual Spanner client call
    {ok, spanner_written}.

%% @private Writes case data to Mnesia.
-spec write_to_mnesia(term(), map() | [tuple()]) -> ok | {error, term()}.
write_to_mnesia(CaseId, Data) when is_map(Data) ->
    write_to_mnesia(CaseId, maps:to_list(Data));
write_to_mnesia(CaseId, Data) when is_list(Data) ->
    try
        case lists:member(case_table, mnesia:system_info(tables)) of
            false ->
                %% Table doesn't exist, create it
                {atomic, ok} = mnesia:create_table(case_table,
                    [{attributes, [id, data]}, {ram_copies, [node()]}]),
                ok;
            true ->
                ok
        end,
        {atomic, ok} = mnesia:transaction(fun() ->
            Record = list_to_tuple([case_record | [{id, CaseId} | Data]]),
            mnesia:write(case_table, Record, write)
        end),
        ok
    catch
        _:{aborted, Reason} -> {error, {mnesia_aborted, Reason}};
        _:Reason -> {error, {mnesia_error, Reason}}
    end.
