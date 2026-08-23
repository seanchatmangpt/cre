%%%-------------------------------------------------------------------
%%% @doc evidence_normalize - Trace normalization for deterministic hashing
%%%
%%% This module provides normalization functions for converting workflow
%%% execution traces into canonical, deterministic forms suitable for
%%% hashing and verification.
%%%
%%% <h3>Features</h3>
%%% <ul>
%%%   <li>PID remapping to sequential indices (pid_0, pid_1, ...)</li>
%%%   <li>Port remapping to sequential indices (port_0, port_1, ...)</li>
%%%   <li>Reference normalization to sequential indices (ref_0, ref_1, ...)</li>
%%%   <li>Timestamp conversion to deltas from first event</li>
%%%   <li>Function canonicalization to {Module, Function, Arity}</li>
%%%   <li>SHA-256 hashing of normalized traces</li>
%%% </ul>
%%%
%%% @end
%%%-------------------------------------------------------------------

-module(evidence_normalize).

%%====================================================================
%% Exports
%%====================================================================

%% Normalization operations
-export([normalize_trace/1]).
-export([remap_pids/1]).
-export([strip_timestamps/1]).
-export([canonicalize_terms/1]).
-export([hash_normalized/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Normalized trace with deterministic values.
%%--------------------------------------------------------------------
-type normalized_trace() :: [map()].

%%--------------------------------------------------------------------
%% @doc Normalization context tracking remappings.
%%--------------------------------------------------------------------
-record(norm_ctx, {
    pid_map :: #{pid() => non_neg_integer()},
    port_map :: #{port() => non_neg_integer()},
    ref_map :: #{reference() => non_neg_integer()},
    next_pid :: non_neg_integer(),
    next_port :: non_neg_integer(),
    next_ref :: non_neg_integer(),
    base_timestamp :: integer() | undefined
}).

%%--------------------------------------------------------------------
%% @doc Opaque normalization context.
%%--------------------------------------------------------------------
-opaque context() :: #norm_ctx{}.

%% Export types
-export_type([normalized_trace/0, context/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Normalizes a trace to canonical form for deterministic hashing.
%%
%% Applies all normalization rules:
%% <ul>
%%   <li>PIDs -> pid_N where N is index of first occurrence</li>
%%   <li>Ports -> port_N</li>
%%   <li>Refs -> ref_N</li>
%%   <li>Timestamps -> delta from first event</li>
%%   <li>Functions -> {Module, Function, Arity}</li>
%% </ul>
%%
%% @param Trace List of trace events (maps with timestamp, type, data)
%% @returns {ok, NormalizedTrace} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_trace([map()]) -> {ok, normalized_trace()}.

normalize_trace([]) ->
    {ok, []};

normalize_trace(Trace) when is_list(Trace) ->
    Ctx = init_context(),
    {NormalizedTrace, _FinalCtx} = normalize_events(Trace, Ctx),
    {ok, NormalizedTrace}.

%%--------------------------------------------------------------------
%% @doc Remaps PIDs to sequential indices (pid_0, pid_1, ...).
%%
%% Each unique PID gets assigned a sequential index based on its first
%% occurrence in the trace. This makes traces deterministic across runs.
%%
%% @param Trace List of trace events
%% @returns {ok, TraceWithRemappedPIDs} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec remap_pids([map()]) -> {ok, [map()]}.

remap_pids([]) ->
    {ok, []};

remap_pids(Trace) when is_list(Trace) ->
    Ctx = init_context(),
    {Remapped, _FinalCtx} = remap_pids_in_events(Trace, Ctx),
    {ok, Remapped}.

%%--------------------------------------------------------------------
%% @doc Converts absolute timestamps to deltas from first event.
%%
%% The first event's timestamp becomes 0, and all subsequent events
%% are expressed as milliseconds from that base.
%%
%% @param Trace List of trace events
%% @returns {ok, TraceWithDeltas} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec strip_timestamps([map()]) -> {ok, [map()]}.

strip_timestamps([]) ->
    {ok, []};

strip_timestamps(Trace) when is_list(Trace) ->
    {ok, strip_timestamps_in_events(Trace)}.

%%--------------------------------------------------------------------
%% @doc Canonicalizes terms to deterministic form.
%%
%% Converts functions, funs, and other non-deterministic terms to
%% canonical representations suitable for hashing.
%%
%% @param Term Any Erlang term to canonicalize
%% @returns Canonicalized term
%%
%% @end
%%--------------------------------------------------------------------
-spec canonicalize_terms(term()) -> term().

canonicalize_terms(Term) when is_map(Term) ->
    maps:map(fun(_K, V) -> canonicalize_terms(V) end, Term);
canonicalize_terms(List) when is_list(List) ->
    [canonicalize_terms(E) || E <- List];
canonicalize_terms(Tuple) when is_tuple(Tuple) ->
    list_to_tuple([canonicalize_terms(E) || E <- tuple_to_list(Tuple)]);
canonicalize_terms(Fun) when is_function(Fun) ->
    %% Extract function info: {Module, Name, Arity} or {'fun', Arity}
    case erlang:fun_info(Fun, module) of
        {module, Module} when Module =/= undefined ->
            {name, Name} = erlang:fun_info(Fun, name),
            {arity, Arity} = erlang:fun_info(Fun, arity),
            {function, Module, Name, Arity};
        _ ->
            {arity, Arity} = erlang:fun_info(Fun, arity),
            {'fun', Arity}
    end;
canonicalize_terms(Pid) when is_pid(Pid) ->
    %% PIDs should be remapped by remap_pids/1 before hashing
    %% This is a fallback for direct canonicalization
    pid_to_list(Pid);
canonicalize_terms(Port) when is_port(Port) ->
    %% Ports should be remapped before hashing
    erlang:port_to_list(Port);
canonicalize_terms(Ref) when is_reference(Ref) ->
    %% Refs should be remapped before hashing
    erlang:ref_to_list(Ref);
canonicalize_terms(Term) ->
    Term.

%%--------------------------------------------------------------------
%% @doc Returns SHA-256 hash of normalized trace.
%%
%% Normalizes the trace first, then computes the SHA-256 hash of
%% the canonicalized term.
%%
%% @param Trace List of trace events
%% @returns {ok, Hash} on success
%%
%% @end
%%--------------------------------------------------------------------
-spec hash_normalized([map()]) -> {ok, <<_:256>>}.

hash_normalized(Trace) when is_list(Trace) ->
    {ok, Normalized} = normalize_trace(Trace),
    %% Canonicalize any remaining non-deterministic elements
    Canonical = canonicalize_terms(Normalized),
    Binary = term_to_binary(Canonical),
    Hash = crypto:hash(sha256, Binary),
    {ok, Hash}.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Initializes a fresh normalization context.
-spec init_context() -> context().

init_context() ->
    #norm_ctx{
        pid_map = #{},
        port_map = #{},
        ref_map = #{},
        next_pid = 0,
        next_port = 0,
        next_ref = 0,
        base_timestamp = undefined
    }.

%% @private
%% @doc Normalizes a list of events, updating context.
-spec normalize_events([map()], context()) -> {[map()], context()}.

normalize_events(Events, Ctx) ->
    lists:mapfoldl(fun normalize_event/2, Ctx, Events).

%% @private
%% @doc Normalizes a single event.
-spec normalize_event(map(), context()) -> {map(), context()}.

normalize_event(#{timestamp := TS} = Event, Ctx) ->
    %% Update base timestamp if not set
    Base = case Ctx#norm_ctx.base_timestamp of
        undefined -> TS;
        B -> B
    end,
    UpdatedCtx = Ctx#norm_ctx{base_timestamp = Base},
    %% Normalize timestamp to delta
    DeltaTS = TS - Base,
    %% Normalize data map
    {NormalizedData, FinalCtx} = normalize_map(maps:get(data, Event, #{}), UpdatedCtx),
    %% Return normalized event with delta timestamp
    NormalizedEvent = Event#{
        timestamp => DeltaTS,
        data => NormalizedData
    },
    {NormalizedEvent, FinalCtx};

normalize_event(Event, Ctx) ->
    %% Event without timestamp - just normalize data
    {NormalizedData, FinalCtx} = normalize_map(maps:get(data, Event, #{}), Ctx),
    NormalizedEvent = Event#{data => NormalizedData},
    {NormalizedEvent, FinalCtx}.

%% @private
%% @doc Normalizes a map, handling PIDs, ports, refs.
-spec normalize_map(map(), context()) -> {map(), context()}.

normalize_map(Map, Ctx) ->
    maps:fold(fun normalize_map_entry/3, {#{}, Ctx}, Map).

%% @private
%% @doc Normalizes a single map entry.
-spec normalize_map_entry(term(), term(), {map(), context()}) -> {map(), context()}.

normalize_map_entry(Key, Pid, {Acc, Ctx}) when is_pid(Pid) ->
    {NormalizedPid, NewCtx} = remap_pid(Pid, Ctx),
    {Acc#{Key => NormalizedPid}, NewCtx};

normalize_map_entry(Key, Port, {Acc, Ctx}) when is_port(Port) ->
    {NormalizedPort, NewCtx} = remap_port(Port, Ctx),
    {Acc#{Key => NormalizedPort}, NewCtx};

normalize_map_entry(Key, Ref, {Acc, Ctx}) when is_reference(Ref) ->
    {NormalizedRef, NewCtx} = remap_ref(Ref, Ctx),
    {Acc#{Key => NormalizedRef}, NewCtx};

normalize_map_entry(Key, Fun, {Acc, Ctx}) when is_function(Fun) ->
    CanonicalFun = canonicalize_function(Fun),
    {Acc#{Key => CanonicalFun}, Ctx};

normalize_map_entry(Key, Map, {Acc, Ctx}) when is_map(Map) ->
    {NestedMap, NewCtx} = normalize_map(Map, Ctx),
    {Acc#{Key => NestedMap}, NewCtx};

normalize_map_entry(Key, List, {Acc, Ctx}) when is_list(List) ->
    {NormalizedList, NewCtx} = normalize_list(List, Ctx),
    {Acc#{Key => NormalizedList}, NewCtx};

normalize_map_entry(Key, Term, {Acc, Ctx}) when is_tuple(Term) ->
    NormalizedTuple = normalize_tuple(Term, Ctx),
    {Acc#{Key => NormalizedTuple}, Ctx};

normalize_map_entry(Key, Term, {Acc, Ctx}) ->
    {Acc#{Key => Term}, Ctx}.

%% @private
%% @doc Normalizes a list.
-spec normalize_list(term(), context()) -> {term(), context()}.

normalize_list(List, Ctx) when is_list(List) ->
    lists:mapfoldl(fun normalize_element/2, Ctx, List);

normalize_list(Term, Ctx) ->
    {Term, Ctx}.

%% @private
%% @doc Normalizes a single element.
-spec normalize_element(term(), context()) -> {term(), context()}.

normalize_element(Pid, Ctx) when is_pid(Pid) ->
    remap_pid(Pid, Ctx);

normalize_element(Port, Ctx) when is_port(Port) ->
    remap_port(Port, Ctx);

normalize_element(Ref, Ctx) when is_reference(Ref) ->
    remap_ref(Ref, Ctx);

normalize_element(Fun, Ctx) when is_function(Fun) ->
    {canonicalize_function(Fun), Ctx};

normalize_element(Map, Ctx) when is_map(Map) ->
    normalize_map(Map, Ctx);

normalize_element(List, Ctx) when is_list(List) ->
    normalize_list(List, Ctx);

normalize_element(Tuple, Ctx) when is_tuple(Tuple) ->
    {normalize_tuple(Tuple, Ctx), Ctx};

normalize_element(Term, Ctx) ->
    {Term, Ctx}.

%% @private
%% @doc Normalizes a tuple.
-spec normalize_tuple(tuple(), context()) -> tuple().

normalize_tuple(Tuple, Ctx) ->
    {Result, _} = normalize_list(tuple_to_list(Tuple), Ctx),
    list_to_tuple(Result).

%% @private
%% @doc Remaps a PID to pid_N format.
-spec remap_pid(pid(), context()) -> {atom(), context()}.

remap_pid(Pid, #norm_ctx{pid_map = Map, next_pid = Next} = Ctx) ->
    case maps:find(Pid, Map) of
        {ok, Index} ->
            {{pid, Index}, Ctx};
        error ->
            NewMap = Map#{Pid => Next},
            NewCtx = Ctx#norm_ctx{pid_map = NewMap, next_pid = Next + 1},
            {{pid, Next}, NewCtx}
    end.

%% @private
%% @doc Remaps a port to port_N format.
-spec remap_port(port(), context()) -> {atom(), context()}.

remap_port(Port, #norm_ctx{port_map = Map, next_port = Next} = Ctx) ->
    case maps:find(Port, Map) of
        {ok, Index} ->
            {{port, Index}, Ctx};
        error ->
            NewMap = Map#{Port => Next},
            NewCtx = Ctx#norm_ctx{port_map = NewMap, next_port = Next + 1},
            {{port, Next}, NewCtx}
    end.

%% @private
%% @doc Remaps a reference to ref_N format.
-spec remap_ref(reference(), context()) -> {atom(), context()}.

remap_ref(Ref, #norm_ctx{ref_map = Map, next_ref = Next} = Ctx) ->
    case maps:find(Ref, Map) of
        {ok, Index} ->
            {{ref, Index}, Ctx};
        error ->
            NewMap = Map#{Ref => Next},
            NewCtx = Ctx#norm_ctx{ref_map = NewMap, next_ref = Next + 1},
            {{ref, Next}, NewCtx}
    end.

%% @private
%% @doc Canonicalizes a function to {Module, Function, Arity} or {'fun', Arity}.
-spec canonicalize_function(function()) -> {function, module(), atom(), arity()} | {'fun', arity()}.

canonicalize_function(Fun) ->
    case erlang:fun_info(Fun, module) of
        {module, Module} when Module =/= undefined ->
            {name, Name} = erlang:fun_info(Fun, name),
            {arity, Arity} = erlang:fun_info(Fun, arity),
            {function, Module, Name, Arity};
        _ ->
            {arity, Arity} = erlang:fun_info(Fun, arity),
            {'fun', Arity}
    end.

%% @private
%% @doc Remaps PIDs in events, updating context.
-spec remap_pids_in_events([map()], context()) -> {[map()], context()}.

remap_pids_in_events(Events, Ctx) ->
    lists:mapfoldl(fun remap_pids_in_event/2, Ctx, Events).

%% @private
%% @doc Remaps PIDs in a single event.
-spec remap_pids_in_event(map(), context()) -> {map(), context()}.

remap_pids_in_event(Event, Ctx) ->
    maps:fold(fun
        (timestamp, _V, {Acc, C}) -> {Acc, C};
        (type, _V, {Acc, C}) -> {Acc, C};
        (data, Data, {Acc, C}) when is_map(Data) ->
            {RemappedData, NewCtx} = remap_pids_in_data(Data, C),
            {Acc#{data => RemappedData}, NewCtx};
        (K, V, {Acc, C}) when is_pid(V) ->
            {Remapped, NewCtx} = remap_pid(V, C),
            {Acc#{K => Remapped}, NewCtx};
        (K, V, {Acc, C}) ->
            {Acc#{K => V}, C}
    end, {#{}, Ctx}, Event).

%% @private
%% @doc Remaps PIDs in a data map.
-spec remap_pids_in_data(map(), context()) -> {map(), context()}.

remap_pids_in_data(Data, Ctx) ->
    maps:fold(fun
        (K, V, {Acc, C}) when is_pid(V) ->
            {Remapped, NewCtx} = remap_pid(V, C),
            {Acc#{K => Remapped}, NewCtx};
        (K, V, {Acc, C}) when is_map(V) ->
            {Nested, NewCtx} = remap_pids_in_data(V, C),
            {Acc#{K => Nested}, NewCtx};
        (K, V, {Acc, C}) when is_list(V) ->
            {List, NewCtx} = remap_pids_in_list(V, C),
            {Acc#{K => List}, NewCtx};
        (K, V, {Acc, C}) ->
            {Acc#{K => V}, C}
    end, {#{}, Ctx}, Data).

%% @private
%% @doc Remaps PIDs in a list.
-spec remap_pids_in_list([term()], context()) -> {[term()], context()}.

remap_pids_in_list(List, Ctx) ->
    lists:mapfoldl(fun
        (Pid, C) when is_pid(Pid) -> remap_pid(Pid, C);
        (Map, C) when is_map(Map) -> remap_pids_in_data(Map, C);
        (L, C) when is_list(L) -> remap_pids_in_list(L, C);
        (Term, C) -> {Term, C}
    end, Ctx, List).

%% @private
%% @doc Strips timestamps to deltas in events.
-spec strip_timestamps_in_events([map()]) -> [map()].

strip_timestamps_in_events([]) ->
    [];

strip_timestamps_in_events([First | Rest]) ->
    Base = maps:get(timestamp, First, 0),
    [First#{timestamp => 0} | [strip_timestamp(Event, Base) || Event <- Rest]].

%% @private
%% @doc Strips timestamp to delta from base.
-spec strip_timestamp(map(), integer()) -> map().

strip_timestamp(Event, Base) ->
    case maps:find(timestamp, Event) of
        {ok, TS} ->
            Event#{timestamp => TS - Base};
        error ->
            Event
    end.
