%%%-------------------------------------------------------------------
%%% @doc evidence_counter - ETS-based effect counter for runtime verification.
%%%
%%% This module provides a gen_server that maintains effect counters in an ETS table.
%%% It tracks various workflow effect types for runtime truth verification and
%%% evidence pack generation.
%%%
%%% <h3>Effect Types</h3>
%%% <ul>
%%%   <li><b>task_start:</b> A task execution has started</li>
%%%   <li><b>task_complete:</b> A task execution has completed</li>
%%%   <li><b>cancel:</b> A cancellation was requested/completed</li>
%%%   <li><b>fork:</b> A workflow fork operation</li>
%%%   <li><b>join:</b> A workflow join operation</li>
%%%   <li><b>scope_enter:</b> Entered a new scope context</li>
%%%   <li><b>scope_exit:</b> Exited a scope context</li>
%%%   <li><b>wait_signal:</b> Waiting for a signal</li>
%%%   <li><b>effect_receipt:</b> An effect receipt was generated</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(evidence_counter).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([count_effect/2]).
-export([count_effect_tuple/1]).
-export([get_counts/0]).
-export([get_count/1]).
-export([reset_counters/0]).
-export([dump_counts/0]).
-export([stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Effect type atoms tracked by the counter.
%%--------------------------------------------------------------------
-type effect_type() ::
    task_start |
    task_complete |
    cancel |
    fork |
    join |
    scope_enter |
    scope_exit |
    wait_signal |
    effect_receipt.

%%--------------------------------------------------------------------
%% @doc Details map for an effect event.
%%--------------------------------------------------------------------
-type effect_details() :: #{
    timestamp => integer(),
    workflow_id => binary() | undefined,
    case_id => binary() | undefined,
    details => map()
}.

%%--------------------------------------------------------------------
%% @doc Counter record stored in ETS.
%%--------------------------------------------------------------------
-record(counter, {
    type :: effect_type(),
    count :: non_neg_integer(),
    last_seen :: integer(),
    details :: [effect_details()]
}).

%%--------------------------------------------------------------------
%% @doc Server state.
%%--------------------------------------------------------------------
-record(state, {
    table :: ets:tid(),
    start_time :: integer()
}).

-export_type([effect_type/0, effect_details/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the evidence counter gen_server.
%%
%% Creates an ETS table named `effect_counters` with type `set`.
%%
%% @returns {ok, Pid} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Records an effect occurrence.
%%
%% Increments the counter for the given effect type and stores details.
%%
%% @param Type The effect type atom
%% @param Details Map containing event details (timestamp, workflow_id, etc)
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec count_effect(Type :: effect_type(), Details :: effect_details()) -> ok.
count_effect(Type, Details) when is_atom(Type), is_map(Details) ->
    gen_server:cast(?MODULE, {count, Type, Details}).

%%--------------------------------------------------------------------
%% @doc Records an effect occurrence from trace hooks (tuple format).
%%
%% Simplified interface for trace patterns that can't construct maps.
%%
%% @param Type The effect type atom
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec count_effect_tuple(Type :: effect_type()) -> ok.
count_effect_tuple(Type) when is_atom(Type) ->
    gen_server:cast(?MODULE, {count, Type, #{}}).

%%--------------------------------------------------------------------
%% @doc Returns all effect counts.
%%
%% @returns Map of effect_type => count
%%
%% @end
%%--------------------------------------------------------------------
-spec get_counts() -> #{effect_type() => non_neg_integer()}.
get_counts() ->
    gen_server:call(?MODULE, get_counts).

%%--------------------------------------------------------------------
%% @doc Returns count for a specific effect type.
%%
%% @param Type The effect type atom
%% @returns {ok, Count} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_count(Type :: effect_type()) -> {ok, non_neg_integer()} | {error, not_found}.
get_count(Type) when is_atom(Type) ->
    gen_server:call(?MODULE, {get_count, Type}).

%%--------------------------------------------------------------------
%% @doc Clears all effect counters.
%%
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_counters() -> ok.
reset_counters() ->
    gen_server:call(?MODULE, reset_counters).

%%--------------------------------------------------------------------
%% @doc Returns a snapshot of all counts for evidence pack generation.
%%
%% Includes metadata like server start time and snapshot timestamp.
%%
%% @returns Map with counts, metadata, and recent details
%%
%% @end
%%--------------------------------------------------------------------
-spec dump_counts() -> #{
    counts := #{effect_type() => non_neg_integer()},
    timestamp := integer(),
    uptime_ms := non_neg_integer(),
    total_effects := non_neg_integer()
}.
dump_counts() ->
    gen_server:call(?MODULE, dump_counts).

%%--------------------------------------------------------------------
%% @doc Stops the evidence counter server.
%%
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.
stop() ->
    gen_server:stop(?MODULE).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Create ETS table for effect counters
    Table = ets:new(effect_counters, [
        set,
        protected,
        {read_concurrency, true},
        {write_concurrency, true}
    ]),
    %% Initialize all counters to zero
    Types = [
        task_start, task_complete, cancel, fork, join,
        scope_enter, scope_exit, wait_signal, effect_receipt
    ],
    Now = erlang:system_time(millisecond),
    lists:foreach(fun(Type) ->
        ets:insert(Table, #counter{
            type = Type,
            count = 0,
            last_seen = Now,
            details = []
        })
    end, Types),
    {ok, #state{
        table = Table,
        start_time = Now
    }}.

%% @private
-spec handle_call(term(), {pid(), term()}, #state{}) ->
    {reply, term(), #state{}}.
handle_call(get_counts, _From, #state{table = Table} = State) ->
    %% Get all counts from table, default to 0 for missing types
    All = ets:tab2list(Table),
    Counts = lists:foldl(fun(#counter{type = Type, count = Count}, Acc) ->
        Acc#{Type => Count}
    end, #{}, All),
    %% Ensure all effect types are present with default 0
    Types = [
        task_start, task_complete, cancel, fork, join,
        scope_enter, scope_exit, wait_signal, effect_receipt
    ],
    CompleteCounts = lists:foldl(fun(Type, Acc) ->
        maps:put(Type, maps:get(Type, Counts, 0), Acc)
    end, #{}, Types),
    {reply, CompleteCounts, State};

handle_call({get_count, Type}, _From, #state{table = Table} = State) ->
    %% Valid effect types that should always be present
    ValidTypes = [
        task_start, task_complete, cancel, fork, join,
        scope_enter, scope_exit, wait_signal, effect_receipt
    ],
    case lists:member(Type, ValidTypes) of
        true ->
            %% Return count from table or 0 if not yet counted
            case ets:lookup(Table, Type) of
                [#counter{count = Count}] ->
                    {reply, {ok, Count}, State};
                [] ->
                    {reply, {ok, 0}, State}
            end;
        false ->
            {reply, {error, not_found}, State}
    end;

handle_call(reset_counters, _From, #state{table = Table} = State) ->
    Now = erlang:system_time(millisecond),
    %% Always re-initialize all effect types, even if missing
    Types = [
        task_start, task_complete, cancel, fork, join,
        scope_enter, scope_exit, wait_signal, effect_receipt
    ],
    lists:foreach(fun(Type) ->
        ets:insert(Table, #counter{
            type = Type,
            count = 0,
            last_seen = Now,
            details = []
        })
    end, Types),
    {reply, ok, State};

handle_call(dump_counts, _From, #state{table = Table, start_time = StartTime} = State) ->
    Now = erlang:system_time(millisecond),
    %% All effect types that should be present
    AllTypes = [
        task_start, task_complete, cancel, fork, join,
        scope_enter, scope_exit, wait_signal, effect_receipt
    ],
    %% Build counts from table, with defaults for missing types
    Counts = lists:foldl(fun(Type, Acc) ->
        case ets:lookup(Table, Type) of
            [#counter{count = Count, last_seen = LastSeen, details = Details}] ->
                maps:put(Type, #{
                    count => Count,
                    last_seen => LastSeen,
                    recent_details => lists:sublist(lists:reverse(Details), 10)
                }, Acc);
            [] ->
                maps:put(Type, #{
                    count => 0,
                    last_seen => Now,
                    recent_details => []
                }, Acc)
        end
    end, #{}, AllTypes),
    Total = maps:fold(fun(_, #{count := C}, Acc) -> Acc + C end, 0, Counts),
    Snapshot = #{
        counts => Counts,
        timestamp => Now,
        uptime_ms => Now - StartTime,
        total_effects => Total
    },
    {reply, Snapshot, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast({count, Type, Details}, #state{table = Table} = State) ->
    Now = erlang:system_time(millisecond),
    DetailsWithTs = Details#{timestamp => Now},
    case ets:lookup(Table, Type) of
        [#counter{count = Count, details = ExistingDetails} = Counter] ->
            %% Keep only last 100 details per type to prevent unbounded growth
            NewDetails = case length(ExistingDetails) >= 100 of
                true ->
                    lists:sublist([DetailsWithTs | ExistingDetails], 100);
                false ->
                    [DetailsWithTs | ExistingDetails]
            end,
            ets:insert(Table, Counter#counter{
                count = Count + 1,
                last_seen = Now,
                details = NewDetails
            });
        [] ->
            %% Initialize new counter type if not exists
            ets:insert(Table, #counter{
                type = Type,
                count = 1,
                last_seen = Now,
                details = [DetailsWithTs]
            })
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{table = Table}) ->
    ets:delete(Table),
    ok.
