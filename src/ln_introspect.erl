%% -*- erlang -*-
%%%% @doc Linear Nesting Introspection
%%
%% This module provides introspection capabilities for workflow execution state,
%% including status maps, path-based inspection, tracing, and stack/scopes dumping.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Status map: state, steps, effects, active_scopes, waiting_conditions, budget_usage</li>
%%   <li>Path-based lookup: inspect/2 for navigating nested state</li>
%%   <li>Trace extraction: trace/1 and trace/3 for execution history</li>
%%   <li>Stack dumping: dump_stack/1 for call stack visualization</li>
%%   <li>Scopes dumping: dump_scopes/1 for active scope hierarchy</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Getting status from a workflow case:
%% ```erlang
%% > Status = ln_introspect:status(CaseState).
%% #{state => running, steps => 5, effects => 2, ...}
%% ```
%%
%% Inspecting nested state:
%% ```erlang
%% > ln_introspect:inspect(CaseState, [marking, p_active]).
%% [token1, token2]
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_introspect).

%%====================================================================
%% Exports
%%====================================================================

%% Status operations
-export([status/1]).

%% Path-based inspection
-export([inspect/2, inspect/3]).

%% Trace operations
-export([trace/1, trace/3]).

%% Dump operations
-export([dump_stack/1, dump_scopes/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Workflow case state or compatible state map.
%%
%% A map containing workflow execution state with keys such as:
%% - status: case status atom
%% - marking: Petri net marking
%% - receipts: list of receipts
%% - context: nested state map
%% - budget: ln_budget budget state
%%--------------------------------------------------------------------
-type case_state() :: map().

%%--------------------------------------------------------------------
%% @doc Status map returned by status/1.
%%
%% Contains a comprehensive snapshot of workflow execution state.
%%--------------------------------------------------------------------
-type status_map() :: #{
    state := atom(),
    steps := non_neg_integer(),
    effects := non_neg_integer(),
    active_scopes := [term()],
    waiting_conditions := [term()],
    budget_usage := map()
}.

%%--------------------------------------------------------------------
%% @doc Path element for nested state lookup.
%%
%% Can be an atom key, binary key, or integer index for lists.
%%--------------------------------------------------------------------
-type path_elem() :: atom() | binary() | integer().

%%--------------------------------------------------------------------
%% @doc Path for navigating nested state structures.
%%
%% A list of path elements for deep lookup.
%%--------------------------------------------------------------------
-type path() :: [path_elem()].

%%--------------------------------------------------------------------
%% @doc Trace entry representing a single execution step.
%%
%% Contains transition fired, mode, and timestamp.
%%--------------------------------------------------------------------
-type trace_entry() :: #{
    step := non_neg_integer(),
    transition := atom(),
    mode => map(),
    timestamp := integer(),
    before_hash => binary(),
    after_hash => binary()
}.

%%--------------------------------------------------------------------
%% @doc Stack frame entry for call stack visualization.
%%
%% Represents a single frame in the execution stack.
%%--------------------------------------------------------------------
-type stack_frame() :: #{
    name := atom() | binary(),
    type := atom(),
    line => non_neg_integer(),
    file => binary()
}.

%%--------------------------------------------------------------------
%% @doc Scope entry for scope hierarchy.
%%
%% Represents a single active scope with its bindings.
%%--------------------------------------------------------------------
-type scope_entry() :: #{
    id := term(),
    parent => term() | undefined,
    bindings := map(),
    depth := non_neg_integer()
}.

%% Export types
-export_type([
    status_map/0,
    path/0,
    path_elem/0,
    trace_entry/0,
    stack_frame/0,
    scope_entry/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns a comprehensive status map for a workflow case state.
%%
%% Extracts and aggregates key information from the case state including:
%% - state: Current execution status (running, completed, etc.)
%% - steps: Number of execution steps taken (from receipts)
%% - effects: Number of effects performed (from receipts)
%% - active_scopes: List of currently active scope IDs
%% - waiting_conditions: List of conditions being waited on
%% - budget_usage: Budget resource usage if present
%%
%% @param CaseState Workflow case state map
%% @return Status map with aggregated state information
%%
%% @end
%%--------------------------------------------------------------------
-spec status(CaseState :: case_state()) -> status_map().

status(CaseState) when is_map(CaseState) ->
    %% Extract base state
    State = maps:get(status, CaseState, unknown),

    %% Count steps and effects from receipts
    Receipts = maps:get(receipts, CaseState, []),
    {Steps, Effects} = count_from_receipts(Receipts),

    %% Extract active scopes from context
    Context = maps:get(context, CaseState, #{}),
    ActiveScopes = extract_active_scopes(Context),

    %% Extract waiting conditions
    WaitingConditions = maps:get(waiting_conditions, CaseState, []),

    %% Extract budget usage
    BudgetUsage = extract_budget_usage(CaseState),

    #{
        state => State,
        steps => Steps,
        effects => Effects,
        active_scopes => ActiveScopes,
        waiting_conditions => WaitingConditions,
        budget_usage => BudgetUsage
    }.

%%--------------------------------------------------------------------
%% @doc Inspects a nested state structure using a path.
%%
%% Navigates through nested maps and lists using a path of keys/indices.
%% Returns {ok, Value} if found, {error, not_found} if path is invalid.
%%
%% <h4>Example</h4>
%% ```erlang
%% > State = #{context => #{nested => #{value => 42}}}.
%% > ln_introspect:inspect(State, [context, nested, value]).
%% {ok, 42}
%% > ln_introspect:inspect(State, [context, missing]).
%% {error, not_found}
%% ```
%%
%% @param CaseState Workflow case state map
%% @param Path Path of keys/indices to navigate
%% @return {ok, Value} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec inspect(CaseState :: case_state(), Path :: path()) ->
    {ok, term()} | {error, not_found}.

inspect(CaseState, Path) when is_map(CaseState), is_list(Path) ->
    inspect(CaseState, Path, #{}).

%%--------------------------------------------------------------------
%% @doc Inspects a nested state structure using a path with options.
%%
%% Options:
%% - <b>default:</b> Default value to return if path not found (default: undefined)
%% - <b>strict:</b> If true, return error on invalid path (default: false)
%%
%% @param CaseState Workflow case state map
%% @param Path Path of keys/indices to navigate
%% @param Options Options map
%% @return {ok, Value} or {error, not_found}, or default value
%%
%% @end
%%--------------------------------------------------------------------
-spec inspect(CaseState :: case_state(), Path :: path(), Options :: map()) ->
    {ok, term()} | {error, not_found} | term().

inspect(_CaseState, [], _Options) ->
    {error, empty_path};
inspect(CaseState, Path, Options) when is_map(CaseState), is_list(Path) ->
    Default = maps:get(default, Options, undefined),
    Strict = maps:get(strict, Options, false),
    case traverse_path(CaseState, Path) of
        {ok, Value} -> {ok, Value};
        {error, not_found} when Strict =:= false -> Default;
        Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc Extracts execution trace from case state.
%%
%% Returns a list of trace entries representing the execution history
%% derived from receipts in the case state.
%%
%% <h4>Example</h4>
%% ```erlang
%% > Trace = ln_introspect:trace(CaseState).
%% [#{step => 1, transition => t1, timestamp => 12345}, ...]
%% ```
%%
%% @param CaseState Workflow case state map
%% @return List of trace entries
%%
%% @end
%%--------------------------------------------------------------------
-spec trace(CaseState :: case_state()) -> [trace_entry()].

trace(CaseState) when is_map(CaseState) ->
    trace(CaseState, 0, all).

%%--------------------------------------------------------------------
%% @doc Extracts execution trace with offset and limit.
%%
%% Options:
%% - <b>Offset:</b> Number of trace entries to skip
%% - <b>Limit:</b> Maximum number of entries to return, or 'all'
%%
%% @param CaseState Workflow case state map
%% @param Offset Number of entries to skip
%% @param Limit Maximum entries to return or 'all'
%% @return List of trace entries
%%
%% @end
%%--------------------------------------------------------------------
-spec trace(CaseState :: case_state(), Offset :: non_neg_integer(),
            Limit :: non_neg_integer() | all) -> [trace_entry()].

trace(CaseState, Offset, Limit) when is_map(CaseState) ->
    Receipts = maps:get(receipts, CaseState, []),
    TraceEntries = receipts_to_trace(Receipts, 1),
    apply_offset_limit(TraceEntries, Offset, Limit).

%%--------------------------------------------------------------------
%% @doc Dumps the execution stack from case state.
%%
%% Returns a list of stack frames representing the call hierarchy.
%% Useful for debugging and understanding execution flow.
%%
%% @param CaseState Workflow case state map
%% @return List of stack frames
%%
%% @end
%%--------------------------------------------------------------------
-spec dump_stack(CaseState :: case_state()) -> [stack_frame()].

dump_stack(CaseState) when is_map(CaseState) ->
    Context = maps:get(context, CaseState, #{}),
    Stack = maps:get(stack, Context, []),
    normalize_stack_frames(Stack).

%%--------------------------------------------------------------------
%% @doc Dumps the active scopes from case state.
%%
%% Returns a list of scope entries representing the active scope
%% hierarchy with parent-child relationships.
%%
%% @param CaseState Workflow case state map
%% @return List of scope entries
%%
%% @end
%%--------------------------------------------------------------------
-spec dump_scopes(CaseState :: case_state()) -> [scope_entry()].

dump_scopes(CaseState) when is_map(CaseState) ->
    Context = maps:get(context, CaseState, #{}),
    ScopesMap = maps:get(scopes, Context, #{}),
    normalize_scope_entries(ScopesMap).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Counts steps and effects from receipts.
%%
%% Returns {Steps, Effects} tuple.
-spec count_from_receipts([term()]) -> {non_neg_integer(), non_neg_integer()}.

count_from_receipts(Receipts) when is_list(Receipts) ->
    lists:foldl(fun
        (#{move := #{produce := ProduceMap}}, {Steps, Effects}) ->
            %% Count each receipt as a step
            %% Count non-empty produce maps as effects
            EffectCount = case maps:size(ProduceMap) of
                0 -> 0;
                _ -> 1
            end,
            {Steps + 1, Effects + EffectCount};
        (_, Acc) ->
            %% Non-standard receipt format, count as step only
            {element(1, Acc) + 1, element(2, Acc)}
    end, {0, 0}, Receipts);
count_from_receipts(_) ->
    {0, 0}.

%% @private
%% @doc Extracts active scopes from context map.
-spec extract_active_scopes(map()) -> [term()].

extract_active_scopes(Context) when is_map(Context) ->
    case maps:get(active_scopes, Context, undefined) of
        undefined ->
            %% Try to derive from scopes map
            ScopesMap = maps:get(scopes, Context, #{}),
            lists:sort(maps:keys(ScopesMap));
        Scopes when is_list(Scopes) ->
            Scopes;
        _ ->
            []
    end;
extract_active_scopes(_) ->
    [].

%% @private
%% @doc Extracts budget usage from case state.
-spec extract_budget_usage(case_state()) -> map().

extract_budget_usage(CaseState) ->
    case maps:get(budget, CaseState, undefined) of
        undefined ->
            #{
                steps => 0,
                effects => 0,
                elapsed_ms => 0,
                exceeded => false
            };
        Budget when is_map(Budget) ->
            %% Extract known budget keys, default to 0/false
            #{
                steps => maps:get(steps, Budget, 0),
                effects => maps:get(effects, Budget, 0),
                elapsed_ms => maps:get(elapsed_ms, Budget, 0),
                exceeded => maps:get(exceeded, Budget, false)
            };
        _ ->
            #{
                steps => 0,
                effects => 0,
                elapsed_ms => 0,
                exceeded => false
            }
    end.

%% @private
%% @doc Traverses a path through nested data structures.
-spec traverse_path(term(), [path_elem()]) -> {ok, term()} | {error, not_found}.

traverse_path(Current, []) ->
    {ok, Current};
traverse_path(Current, [Key | Rest]) when is_map(Current) ->
    case maps:get(Key, Current, undefined) of
        undefined -> {error, not_found};
        Next -> traverse_path(Next, Rest)
    end;
traverse_path(Current, [Index | Rest]) when is_list(Current), is_integer(Index) ->
    case Index >= 0 andalso Index < length(Current) of
        true -> traverse_path(lists:nth(Index + 1, Current), Rest);
        false -> {error, not_found}
    end;
traverse_path(_, _) ->
    {error, not_found}.

%% @private
%% @doc Converts receipts to trace entries.
-spec receipts_to_trace([term()], non_neg_integer()) -> [trace_entry()].

receipts_to_trace(Receipts, StartStep) when is_list(Receipts) ->
    lists:foldl(fun
        (Receipt = #{move := #{trsn := Trsn}, ts := Ts}, Acc) ->
            Entry = #{
                step => StartStep + length(Acc),
                transition => Trsn,
                timestamp => Ts,
                before_hash => maps:get(before_hash, Receipt, <<>>),
                after_hash => maps:get(after_hash, Receipt, <<>>),
                mode => maps:get(mode, maps:get(move, Receipt, #{}), #{})
            },
            [Entry | Acc];
        (Receipt, Acc) when is_map(Receipt) ->
            %% Fallback for non-standard receipt format
            Entry = #{
                step => StartStep + length(Acc),
                transition => maps:get(trsn, Receipt, unknown),
                timestamp => maps:get(ts, Receipt, 0)
            },
            [Entry | Acc];
        (_, Acc) ->
            Acc
    end, [], Receipts);

receipts_to_trace(_, _) ->
    [].

%% @private
%% @doc Applies offset and limit to a list.
-spec apply_offset_limit(list(), non_neg_integer(), non_neg_integer() | all) -> list().

apply_offset_limit(List, 0, all) ->
    lists:reverse(List);
apply_offset_limit(List, Offset, all) ->
    case length(List) > Offset of
        true -> lists:nthtail(Offset, lists:reverse(List));
        false -> []
    end;
apply_offset_limit(List, 0, Limit) when is_integer(Limit) ->
    lists:sublist(lists:reverse(List), Limit);
apply_offset_limit(List, Offset, Limit) when is_integer(Limit) ->
    Sliced = case length(List) > Offset of
        true -> lists:nthtail(Offset, lists:reverse(List));
        false -> []
    end,
    lists:sublist(Sliced, Limit).

%% @private
%% @doc Normalizes stack frame entries to standard format.
-spec normalize_stack_frames([term()]) -> [stack_frame()].

normalize_stack_frames(Stack) when is_list(Stack) ->
    lists:map(fun normalize_stack_frame/1, Stack);
normalize_stack_frames(_) ->
    [].

%% @private
%% @doc Normalizes a single stack frame.
-spec normalize_stack_frame(term()) -> stack_frame().

normalize_stack_frame(Frame) when is_map(Frame) ->
    #{
        name => maps:get(name, Frame, unknown),
        type => maps:get(type, Frame, function),
        line => maps:get(line, Frame, 0),
        file => maps:get(file, Frame, <<>>)
    };
normalize_stack_frame({Name, Arity}) when is_atom(Name), is_integer(Arity) ->
    #{
        name => Name,
        type => function,
        arity => Arity,
        line => 0,
        file => <<>>
    };
normalize_stack_frame(Name) when is_atom(Name) ->
    #{
        name => Name,
        type => function,
        line => 0,
        file => <<>>
    };
normalize_stack_frame(_) ->
    #{
        name => unknown,
        type => unknown,
        line => 0,
        file => <<>>
    }.

%% @private
%% @doc Normalizes scope entries to standard format.
-spec normalize_scope_entries(map()) -> [scope_entry()].

normalize_scope_entries(ScopesMap) when is_map(ScopesMap) ->
    lists:map(fun normalize_scope_entry/1, maps:to_list(ScopesMap));
normalize_scope_entries(_) ->
    [].

%% @private
%% @doc Normalizes a single scope entry.
-spec normalize_scope_entry({term(), term()}) -> scope_entry().

normalize_scope_entry({ScopeId, ScopeData}) when is_map(ScopeData) ->
    #{
        id => ScopeId,
        parent => maps:get(parent, ScopeData, undefined),
        bindings => maps:get(bindings, ScopeData, #{}),
        depth => maps:get(depth, ScopeData, 0)
    };
normalize_scope_entry({ScopeId, _}) ->
    #{
        id => ScopeId,
        parent => undefined,
        bindings => #{},
        depth => 0
    }.
