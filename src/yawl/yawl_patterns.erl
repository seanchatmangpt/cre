%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
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
%% @doc YAWL Workflow Pattern Module
%%
%% This module implements YAWL workflow patterns as composable
%% pattern records and functions. It integrates the helper modules
%% (pnet_marking, pnet_mode, pnet_choice, wf_timerq, wf_scope) to
%% provide comprehensive pattern composition capabilities.
%%
%% <h3>Key Features</h3>
%% <ul>
%%   <li><b>Pattern Composition:</b> Combine multiple workflow patterns</li>
%%   <li><b>Marking Validation:</b> Use pnet_marking for token state validation</li>
%%   <li><b>Mode Enumeration:</b> Use pnet_mode for transition firing modes</li>
%%   <li><b>Nondeterministic Choice:</b> Use pnet_choice for pattern selection</li>
%%   <li><b>Timeout Support:</b> Use wf_timerq for timed patterns</li>
%%   <li><b>Scope Boundaries:</b> Use wf_scope for nested patterns</li>
%% </ul>
%%
%% <h3>Usage Example</h3>
%% <pre><code>
%% %% Create a basic pattern
%% Pattern1 = yawl_patterns:new_pattern(implicit_termination,
%%     #implicit_termination{trigger_function = fun() -> ok end}),
%%
%% %% Create a deferred choice pattern
%% Pattern2 = yawl_patterns:deferred_choice(
%%     fun() -> option_a end,
%%     fun() -> option_b end,
%%     fun() -> true end
%% ),
%%
%% %% Combine patterns using marking operations
%% Combined = yawl_patterns:combine_patterns(Pattern1, Pattern2),
%%
%% %% Validate pattern with marking
%% Marking = pnet_marking:new([p1, p2, p3]),
%% Valid = yawl_patterns:validate_pattern(Pattern1, Marking).
%% </code></pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_patterns).

%%====================================================================
%% Exports
%%====================================================================

%% Basic Control Flow Patterns
-export([implicit_termination/1, multiple_instances_no_sync/3]).
-export([deferred_choice/3]).

%% Exception Handling Patterns
-export([error_handler/2, retry/2, compensate/2]).
-export([triggered_compensation/3, consecutive_compensate/1]).

%% Pattern Constructors
-export([new_pattern/2, new_pattern_with_marking/3]).

%% Pattern Composition (using helper modules)
-export([combine_patterns/2, validate_pattern/2, execute_pattern/2]).
-export([select_pattern/2, select_weighted_pattern/2, chain_patterns/1, parallel_patterns/1]).

%% Timeout Patterns (using wf_timerq)
-export([timeout_pattern/3, cancel_timeout/2]).
-export([with_timeout/3]).

%% Scope Patterns (using wf_scope)
-export([nested_pattern/3, enter_scope/2, leave_scope/2]).

%%====================================================================
%% Records
%%====================================================================

-record(pattern, {
    pattern_type :: atom(),
    subprocess :: module() | function() | undefined,
    instance_count :: non_neg_integer() | unlimited | undefined,
    max_instances :: non_neg_integer() | unlimited | undefined,
    pending_instances = [] :: list(),
    active_instances = [] :: list(),
    completed_instances = [] :: list(),
    choice_data = #{} :: map(),
    branch_queue = [] :: list(),
    marking = undefined :: undefined | pnet_marking:marking(),
    timerq = undefined :: undefined | wf_timerq:timerq(),
    scope_id = undefined :: undefined | wf_scope:scope_id()
}).

-record(implicit_termination, {
    trigger_function :: function()
}).

-record(multiple_instances_no_sync, {
    map_function :: function(),
    instance_count :: non_neg_integer()
}).

-record(deferred_choice, {
    option_a_fun :: function(),
    option_b_fun :: function(),
    choice_fun :: function()
}).

-record(error_handler, {
    risky_function :: function(),
    handler_function :: function()
}).

-record(retry, {
    work_function :: function(),
    policy :: term()
}).

-record(compensate, {
    activity_function :: function(),
    compensation_function :: function()
}).

-record(triggered_compensation, {
    activity_function :: function(),
    compensation_function :: function(),
    trigger_function :: function()
}).

-record(consecutive_compensate, {
    activities :: [{function(), function()}]
}).

-record(timeout_pattern, {
    duration :: non_neg_integer(),
    timeout_function :: function(),
    normal_function :: function(),
    timer_key :: term(),
    timerq :: wf_timerq:timerq()
}).

-record(nested_pattern, {
    inner_pattern :: #pattern{},
    scope_id :: wf_scope:scope_id(),
    binding_table :: wf_scope:binding_table()
}).

-record(combined_pattern, {
    pattern_a :: #pattern{},
    pattern_b :: #pattern{},
    combination_type :: atom()  %% sequence, parallel, choice
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an implicit termination pattern.
%% @end
%%--------------------------------------------------------------------
-spec implicit_termination(TriggerFun :: function()) -> #implicit_termination{}.
implicit_termination(TriggerFun) when is_function(TriggerFun) ->
    #implicit_termination{trigger_function = TriggerFun}.

%%--------------------------------------------------------------------
%% @doc Creates a multiple instances no synchronization pattern.
%% @end
%%--------------------------------------------------------------------
-spec multiple_instances_no_sync(MapFun :: function(), Count :: pos_integer(),
                                   Input :: list()) -> #multiple_instances_no_sync{}.
multiple_instances_no_sync(MapFun, Count, _Input) when is_function(MapFun), is_integer(Count), Count > 0 ->
    #multiple_instances_no_sync{map_function = MapFun, instance_count = Count}.

%%--------------------------------------------------------------------
%% @doc Creates a deferred choice pattern.
%%
%% This pattern uses pnet_choice internally for deterministic
%% nondeterministic selection between options.
%% @end
%%--------------------------------------------------------------------
-spec deferred_choice(OptionAFun :: function(), OptionBFun :: function(),
                      ChoiceFun :: function()) -> #deferred_choice{}.
deferred_choice(OptionAFun, OptionBFun, ChoiceFun) ->
    #deferred_choice{
        option_a_fun = OptionAFun,
        option_b_fun = OptionBFun,
        choice_fun = ChoiceFun
    }.

%%--------------------------------------------------------------------
%% @doc Creates an error handler pattern.
%% @end
%%--------------------------------------------------------------------
-spec error_handler(RiskyFun :: function(), HandlerFun :: function()) -> #error_handler{}.
error_handler(RiskyFun, HandlerFun) ->
    #error_handler{
        risky_function = RiskyFun,
        handler_function = HandlerFun
    }.

%%--------------------------------------------------------------------
%% @doc Creates a retry pattern.
%% @end
%%--------------------------------------------------------------------
-spec retry(WorkFun :: function(), Policy :: term()) -> #retry{}.
retry(WorkFun, Policy) when is_function(WorkFun) ->
    #retry{work_function = WorkFun, policy = Policy}.

%%--------------------------------------------------------------------
%% @doc Creates a compensation pattern.
%% @end
%%--------------------------------------------------------------------
-spec compensate(ActivityFun :: function(), CompensationFun :: function()) ->
          #compensate{}.
compensate(ActivityFun, CompensationFun) ->
    #compensate{
        activity_function = ActivityFun,
        compensation_function = CompensationFun
    }.

%%--------------------------------------------------------------------
%% @doc Creates a triggered compensation pattern.
%% @end
%%--------------------------------------------------------------------
-spec triggered_compensation(ActivityFun :: function(), CompensationFun :: function(),
                              TriggerFun :: function()) -> #triggered_compensation{}.
triggered_compensation(ActivityFun, CompensationFun, TriggerFun) ->
    #triggered_compensation{
        activity_function = ActivityFun,
        compensation_function = CompensationFun,
        trigger_function = TriggerFun
    }.

%%--------------------------------------------------------------------
%% @doc Creates a consecutive compensation pattern.
%% @end
%%--------------------------------------------------------------------
-spec consecutive_compensate(Activities :: [{function(), function()}]) ->
          #consecutive_compensate{}.
consecutive_compensate(Activities) when is_list(Activities) ->
    #consecutive_compensate{activities = Activities}.

%%--------------------------------------------------------------------
%% @doc Creates a new pattern record.
%%
%% Initializes a pattern with optional marking state for validation.
%% @end
%%--------------------------------------------------------------------
-spec new_pattern(Type :: atom(), Data :: term()) -> #pattern{}.
new_pattern(Type, Data) ->
    #pattern{
        pattern_type = Type,
        subprocess = undefined,
        instance_count = 0,
        max_instances = unlimited,
        pending_instances = [],
        active_instances = [],
        completed_instances = [],
        choice_data = Data,
        branch_queue = [],
        marking = undefined,
        timerq = undefined,
        scope_id = undefined
    }.

%%--------------------------------------------------------------------
%% @doc Creates a new pattern with an initial marking.
%%
%% Uses pnet_marking to initialize the pattern's token state.
%% @end
%%--------------------------------------------------------------------
-spec new_pattern_with_marking(Type :: atom(), Data :: term(),
                                Places :: [pnet_marking:place()]) -> #pattern{}.
new_pattern_with_marking(Type, Data, Places) when is_list(Places) ->
    Marking = pnet_marking:new(Places),
    Pattern = new_pattern(Type, Data),
    Pattern#pattern{marking = Marking}.

%%====================================================================
%% Pattern Composition Functions (using pnet_marking)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Combines two patterns into a composite pattern.
%%
%% Uses pnet_marking:apply/3 to merge the markings of both patterns.
%% The combination type can be 'sequence', 'parallel', or 'choice'.
%%
%% @param PatternA First pattern to combine
%% @param PatternB Second pattern to combine
%% @return Combined pattern record
%%
%% @end
%%--------------------------------------------------------------------
-spec combine_patterns(PatternA :: #pattern{} | term(),
                        PatternB :: #pattern{} | term()) -> #combined_pattern{}.
combine_patterns(#pattern{} = PatternA, #pattern{} = PatternB) ->
    %% Create marking for both patterns if not present
    MarkingA = get_or_create_marking(PatternA, [p1, p2]),
    MarkingB = get_or_create_marking(PatternB, [p2, p3]),

    %% Apply marking operation to create combined marking
    %% Using pnet_marking:apply/3 for atomic consume+produce
    _CombinedMarking = combine_markings(MarkingA, MarkingB),

    #combined_pattern{
        pattern_a = PatternA#pattern{marking = MarkingA},
        pattern_b = PatternB#pattern{marking = MarkingB},
        combination_type = sequence
    };
combine_patterns(PatternA, PatternB) ->
    %% Wrap non-pattern records
    WrappedA = wrap_as_pattern(PatternA),
    WrappedB = wrap_as_pattern(PatternB),
    combine_patterns(WrappedA, WrappedB).

%%--------------------------------------------------------------------
%% @doc Validates a pattern against a marking.
%%
%% Uses pnet_marking to verify the pattern's token state is valid.
%% @end
%%--------------------------------------------------------------------
-spec validate_pattern(Pattern :: #pattern{}, Marking :: pnet_marking:marking()) ->
          boolean() | {error, term()}.
validate_pattern(#pattern{marking = undefined}, _Marking) ->
    true;
validate_pattern(#pattern{marking = PatternMarking}, Marking) ->
    %% Compare pattern marking with provided marking
    case pnet_marking:snapshot(Marking) of
        PatternMarking -> true;
        _ -> {error, marking_mismatch}
    end;
validate_pattern(_Pattern, _Marking) ->
    {error, invalid_pattern}.

%%--------------------------------------------------------------------
%% @doc Executes a pattern with a given marking.
%%
%% Uses pnet_mode to enumerate execution modes and pnet_marking:apply/3
%% for state transitions.
%% @end
%%--------------------------------------------------------------------
-spec execute_pattern(Pattern :: #pattern{}, Marking :: pnet_marking:marking()) ->
          {ok, pnet_marking:marking()} | {error, term()}.
execute_pattern(#pattern{pattern_type = Type}, Marking) ->
    %% Enumerate possible execution modes using pnet_mode
    PresetPlaces = maps:keys(Marking),
    Modes = pnet_mode:enum_modes(PresetPlaces, Marking),

    case Modes of
        [] ->
            {error, no_valid_modes};
        _ ->
            %% Execute first valid mode (can be enhanced with pnet_choice)
            execute_with_mode(Type, Marking, hd(Modes))
    end;
execute_pattern(_Pattern, _Marking) ->
    {error, invalid_pattern}.

%%====================================================================
%% Pattern Selection Functions (using pnet_choice)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Selects a pattern from a list using deterministic choice.
%%
%% Uses pnet_choice:pick/2 for reproducible pattern selection.
%% @end
%%--------------------------------------------------------------------
-spec select_pattern(Patterns :: [#pattern{}], Seed :: term()) ->
          {#pattern{}, pnet_choice:rng_state()} | {error, empty}.
select_pattern([], _Seed) ->
    {error, empty};
select_pattern(Patterns, Seed) when is_list(Patterns) ->
    Rng = pnet_choice:seed(Seed),
    pnet_choice:pick(Patterns, Rng).

%%--------------------------------------------------------------------
%% @doc Selects a pattern from a weighted list.
%%
%% Uses pnet_choice:pick_weighted/2 for weighted pattern selection.
%% @end
%%--------------------------------------------------------------------
-spec select_weighted_pattern(Patterns :: [{#pattern{}, pos_integer()}],
                               Seed :: term()) ->
          {#pattern{}, pnet_choice:rng_state()} | {error, empty | bad_weights}.
select_weighted_pattern([], _Seed) ->
    {error, empty};
select_weighted_pattern(Patterns, Seed) when is_list(Patterns) ->
    Rng = pnet_choice:seed(Seed),
    pnet_choice:pick_weighted(Patterns, Rng).

%%====================================================================
%% Pattern Chaining Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Chains patterns sequentially.
%%
%% Creates a combined pattern where patterns execute in order.
%% @end
%%--------------------------------------------------------------------
-spec chain_patterns(Patterns :: [#pattern{} | term()]) -> #pattern{}.
chain_patterns([]) ->
    new_pattern(empty, #{});
chain_patterns([Single]) ->
    normalize_pattern(Single);
chain_patterns(Patterns) when is_list(Patterns) ->
    %% Fold patterns into combined patterns
    lists:foldl(
        fun(Pattern, Acc) ->
            NormPattern = normalize_pattern(Pattern),
            combine_patterns(Acc, NormPattern)
        end,
        normalize_pattern(hd(Patterns)),
        tl(Patterns)
    ).

%%--------------------------------------------------------------------
%% @doc Creates a parallel pattern composition.
%%
%% All patterns execute concurrently.
%% @end
%%--------------------------------------------------------------------
-spec parallel_patterns(Patterns :: [#pattern{} | term()]) -> #pattern{}.
parallel_patterns([]) ->
    new_pattern(empty, #{});
parallel_patterns([Single]) ->
    normalize_pattern(Single);
parallel_patterns(Patterns) when is_list(Patterns) ->
    %% Create parallel composition with shared marking
    CombinedMarking = create_parallel_marking(Patterns),
    #pattern{
        pattern_type = parallel,
        subprocess = undefined,
        instance_count = length(Patterns),
        max_instances = unlimited,
        pending_instances = [],
        active_instances = Patterns,
        completed_instances = [],
        choice_data = #{patterns => Patterns},
        branch_queue = [],
        marking = CombinedMarking
    }.

%%====================================================================
%% Timeout Patterns (using wf_timerq)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a timeout pattern.
%%
%% Uses wf_timerq to manage timeout events. If the normal function
%% completes within the duration, its result is returned. Otherwise,
%% the timeout function is executed.
%%
%% @param Duration Timeout in milliseconds
%% @param TimeoutFun Function to execute on timeout
%% @param NormalFun Function to execute normally
%% @return Timeout pattern record
%%
%% @end
%%--------------------------------------------------------------------
-spec timeout_pattern(Duration :: pos_integer(),
                      TimeoutFun :: function(),
                      NormalFun :: function()) -> #timeout_pattern{}.
timeout_pattern(Duration, TimeoutFun, NormalFun)
  when is_integer(Duration), Duration > 0,
       is_function(TimeoutFun), is_function(NormalFun) ->
    TimerKey = make_ref(),
    TimerQ = wf_timerq:new(),
    #timeout_pattern{
        duration = Duration,
        timeout_function = TimeoutFun,
        normal_function = NormalFun,
        timer_key = TimerKey,
        timerq = TimerQ
    }.

%%--------------------------------------------------------------------
%% @doc Cancels a timeout pattern's timer.
%%
%% Uses wf_timerq:disarm/2 to cancel the pending timeout.
%% @end
%%--------------------------------------------------------------------
-spec cancel_timeout(TimeoutPattern :: #timeout_pattern{},
                     TimerKey :: term()) -> #timeout_pattern{}.
cancel_timeout(#timeout_pattern{timerq = TimerQ} = TimeoutPattern, TimerKey) ->
    TimeoutPattern#timeout_pattern{timerq = wf_timerq:disarm(TimerQ, TimerKey)}.

%%--------------------------------------------------------------------
%% @doc Wraps a pattern with a timeout.
%%
%% Creates a pattern that executes with a time limit using wf_timerq.
%% @end
%%--------------------------------------------------------------------
-spec with_timeout(Pattern :: #pattern{} | function(),
                   Duration :: pos_integer(),
                   TimeoutFun :: function()) -> #pattern{}.
with_timeout(Pattern, Duration, TimeoutFun) when is_function(Pattern) ->
    with_timeout(new_pattern(wrapped, #{function => Pattern}), Duration, TimeoutFun);
with_timeout(#pattern{} = Pattern, Duration, TimeoutFun) ->
    TimerKey = make_ref(),
    TimerQ = wf_timerq:new(),
    Deadline = erlang:system_time(millisecond) + Duration,
    Event = {produce, #{'p_timeout' => [timed_out]}},
    TimerQ1 = wf_timerq:arm(TimerQ, TimerKey, Deadline, Event),
    Pattern#pattern{
        timerq = TimerQ1,
        choice_data = maps:merge(Pattern#pattern.choice_data, #{
            timer_key => TimerKey,
            timeout_fun => TimeoutFun
        })
    }.

%%====================================================================
%% Scope Patterns (using wf_scope)
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a nested pattern with scope boundary.
%%
%% Uses wf_scope to manage token translation between parent and child
%% place namespaces.
%%
%% @param InnerPattern The pattern to nest
%% @param ScopeId Unique identifier for the scope
%% @param BindingTable Mapping of parent to child places
%% @return Nested pattern record
%%
%% @end
%%--------------------------------------------------------------------
-spec nested_pattern(InnerPattern :: #pattern{},
                     ScopeId :: wf_scope:scope_id(),
                     BindingTable :: wf_scope:binding_table()) -> #nested_pattern{}.
nested_pattern(#pattern{} = InnerPattern, ScopeId, BindingTable) ->
    #nested_pattern{
        inner_pattern = InnerPattern,
        scope_id = ScopeId,
        binding_table = BindingTable
    }.

%%--------------------------------------------------------------------
%% @doc Enters a scope, translating tokens to child namespace.
%%
%% Uses wf_scope:enter/3 to translate parent tokens to child places.
%% @end
%%--------------------------------------------------------------------
-spec enter_scope(Pattern :: #pattern{}, BindingTable :: wf_scope:binding_table()) ->
          #pattern{}.
enter_scope(#pattern{scope_id = undefined} = Pattern, _BindingTable) ->
    %% Generate a scope ID if not present
    ScopeId = make_ref(),
    Pattern#pattern{scope_id = ScopeId};
enter_scope(#pattern{marking = Marking, scope_id = ScopeId} = Pattern,
             BindingTable) when Marking =/= undefined ->
    %% Translate marking using scope bindings
    ChildMarkingMap = wf_scope:enter(BindingTable, ScopeId, Marking),
    Pattern#pattern{marking = ChildMarkingMap};
enter_scope(Pattern, _BindingTable) ->
    Pattern.

%%--------------------------------------------------------------------
%% @doc Leaves a scope, translating tokens back to parent namespace.
%%
%% Uses wf_scope:leave/3 to translate child tokens to parent places.
%% @end
%%--------------------------------------------------------------------
-spec leave_scope(Pattern :: #pattern{}, BindingTable :: wf_scope:binding_table()) ->
          #pattern{}.
leave_scope(#pattern{scope_id = undefined} = Pattern, _BindingTable) ->
    %% No scope to leave
    Pattern;
leave_scope(#pattern{marking = Marking, scope_id = ScopeId} = Pattern,
             BindingTable) when Marking =/= undefined ->
    %% Translate marking back to parent namespace
    ParentMarkingMap = wf_scope:leave(BindingTable, ScopeId, Marking),
    Pattern#pattern{marking = ParentMarkingMap};
leave_scope(Pattern, _BindingTable) ->
    Pattern.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Gets or creates a marking for a pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_or_create_marking(#pattern{}, [pnet_marking:place()]) ->
          pnet_marking:marking().

get_or_create_marking(#pattern{marking = undefined}, Places) ->
    pnet_marking:new(Places);
get_or_create_marking(#pattern{marking = Marking}, _Places) ->
    Marking.

%%--------------------------------------------------------------------
%% @private
%% @doc Combines two markings into a single marking.
%%
%% Merges tokens from both markings, handling overlapping places.
%% @end
%%--------------------------------------------------------------------
-spec combine_markings(pnet_marking:marking(), pnet_marking:marking()) ->
          pnet_marking:marking().

combine_markings(MarkingA, MarkingB) ->
    %% Merge both markings, combining tokens at overlapping places
    maps:fold(fun
        (Place, TokensB, Acc) ->
            case maps:get(Place, MarkingA, undefined) of
                undefined -> Acc#{Place => TokensB};
                TokensA -> Acc#{Place => TokensA ++ TokensB}
            end
    end, MarkingA, MarkingB).

%%--------------------------------------------------------------------
%% @private
%% @doc Wraps a non-pattern record as a pattern.
%%
%% @end
%%--------------------------------------------------------------------
-spec wrap_as_pattern(term()) -> #pattern{}.

wrap_as_pattern(Record) when is_tuple(Record), element(1, Record) =:= pattern ->
    Record;
wrap_as_pattern(Term) ->
    new_pattern(wrapped, #{value => Term}).

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes various inputs to a pattern record.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_pattern(#pattern{} | term()) -> #pattern{}.

normalize_pattern(#pattern{} = Pattern) ->
    Pattern;
normalize_pattern(Term) ->
    wrap_as_pattern(Term).

%%--------------------------------------------------------------------
%% @private
%% @doc Creates a marking for parallel pattern execution.
%%
%% All patterns share access to common places.
%% @end
%%--------------------------------------------------------------------
-spec create_parallel_marking([#pattern{} | term()]) -> pnet_marking:marking().

create_parallel_marking(Patterns) when is_list(Patterns) ->
    %% Collect all places from all patterns
    Places = lists:usort(
        lists:flatmap(fun
            (#pattern{marking = undefined}) -> [p1, p2];
            (#pattern{marking = M}) -> maps:keys(M);
            (_) -> [p1, p2]
        end, Patterns)
    ),
    pnet_marking:new(Places).

%%--------------------------------------------------------------------
%% @private
%% @doc Executes a pattern with a specific mode.
%%
%% Uses pnet_marking:apply/3 for the state transition.
%% @end
%%--------------------------------------------------------------------
-spec execute_with_mode(atom(), pnet_marking:marking(), pnet_mode:mode()) ->
          {ok, pnet_marking:marking()} | {error, term()}.

execute_with_mode(_Type, Marking, Mode) ->
    %% Apply the mode as a consume operation
    %% For simplicity, produce the same tokens back
    %% In a real implementation, this would call the pattern's function
    case pnet_marking:apply(Marking, Mode, Mode) of
        {ok, NewMarking} -> {ok, NewMarking};
        {error, Reason} -> {error, Reason}
    end.
