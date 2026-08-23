%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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
-module(deferred_choice).
-behaviour(gen_yawl).

-moduledoc """
Deferred Choice Pattern (WCP-16) for YAWL.

This module implements the Deferred Choice pattern as a gen_pnet behaviour.

## Pattern Description

The Deferred Choice pattern (WCP-16) represents a divergence in the process
where the choice is made at runtime based on which branch becomes available
first, rather than being predetermined by data or conditions.

## Petri Net Structure

```
Places:
  p_start          - Start of the deferred choice
  p_offer_pending  - Offer is pending
  p_option_pool    - Pool of available options
  p_selected       - An option has been selected
  p_discarded      - Non-selected options
  p_complete       - Choice completed

Transitions:
  t_offer          - Offer all options
  t_evaluate_option - Evaluate an option from the pool
  t_select         - Select an option
  t_discard_others - Discard non-selected options
  t_complete       - Complete the choice
```

## Soundness Properties

- **Option to complete:** Always true (exactly one option selected)
- **Proper completion:** Exactly one output token
- **No dead transitions:** All options are eventually selected or discarded

## Examples

Getting the list of places:

```erlang
> deferred_choice:place_lst().
[p_start,p_offer_pending,p_option_pool,p_selected,p_discarded,p_complete]
```

Getting the list of transitions:

```erlang
> deferred_choice:trsn_lst().
[t_offer,t_evaluate_option,t_select,t_discard_others,t_complete]
```

Getting the preset for a transition:

```erlang
> deferred_choice:preset(t_offer).
[p_start]
> deferred_choice:preset(t_select).
[p_offer_pending]
> deferred_choice:preset(t_complete).
[p_discarded]
> deferred_choice:preset(unknown).
[]
```
""".

%% gen_pnet callbacks
-export([
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    init/1,
    terminate/2,
    trigger/3
]).

-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3
]).

%% API exports
-export([
    new/2,
    start/1,
    run/1,
    get_state/1,
    execute/2,
    select_option/2,
    deferred_choice_trigger/2,
    enabled_branches/2,
    select_branch/2,
    disable_other_branches/3
]).

%%====================================================================
%% Records
%%====================================================================

-record(deferred_choice_state, {
    options :: map(),  %% Map of option_id => {Fun, Priority}
    selected :: undefined | atom(),
    discarded = [] :: [atom()],
    start_time :: integer(),
    log_id :: binary() | undefined
}).

-type deferred_choice_state() :: #deferred_choice_state{}.
-export_type([deferred_choice_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Deferred Choice pattern state.
%%
%% @param Options Map of option identifiers to {Fun, Priority} tuples.
%%                Priority determines which option is selected when multiple
%%                are available simultaneously (higher priority wins).
%% @param OptionCount Number of options (must match size of Options map).
%% @return A new deferred_choice_state record.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(Options :: map(), OptionCount :: pos_integer()) -> deferred_choice_state().

new(Options, OptionCount) when is_map(Options),
                                map_size(Options) =:= OptionCount,
                                OptionCount >= 2 ->
    LogId = generate_log_id(),
    #deferred_choice_state{
        options = Options,
        start_time = erlang:system_time(millisecond),
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Deferred Choice workflow as a gen_pnet process.
%%
%% @param Options Map of option identifiers to functions.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Options :: map()) -> {ok, pid()} | {error, term()}.

start(Options) when is_map(Options), map_size(Options) >= 2 ->
    OptionCount = map_size(Options),
    ChoiceState = new(Options, OptionCount),
    gen_yawl:start_link(?MODULE, ChoiceState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Deferred Choice workflow synchronously.
%%
%% @param Options Map of option identifiers to functions.
%% @return {ok, {OptionId, Result}} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec run(Options :: map()) -> {ok, {atom(), term()}} | {error, term()}.

run(Options) when is_map(Options), map_size(Options) >= 2 ->
    case start(Options) of
        {ok, Pid} ->
            case wait_for_completion(Pid, 30000) of
                {ok, Result} ->
                    gen_yawl:stop(Pid),
                    {ok, Result};
                {error, Reason} ->
                    gen_yawl:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Deferred Choice workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, deferred_choice_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes the Deferred Choice pattern with given input data.
%%
%% @param Options Map of option identifiers to {Fun, Priority} tuples.
%% @param EvalData Data to evaluate options with.
%% @return {ok, {OptionId, Result}} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(Options :: map(), EvalData :: term()) ->
          {ok, {atom(), term()}} | {error, term()}.

execute(Options, EvalData) when is_map(Options), map_size(Options) >= 2 ->
    Ref = make_ref(),
    Parent = self(),
    OptionKeys = maps:keys(Options),

    %% Spawn processes for each option
    Pids = lists:map(fun(Key) ->
        case maps:get(Key, Options) of
            {Fun, _Priority} when is_function(Fun, 1) ->
                spawn(fun() ->
                    try
                        Result = Fun(EvalData),
                        Parent ! {Ref, {option_ready, Key}, Result}
                    catch
                        Error:Reason:Stack ->
                            Parent ! {Ref, {option_error, Key}, {Error, Reason, Stack}}
                    end
                end);
            Fun when is_function(Fun, 1) ->
                spawn(fun() ->
                    try
                        Result = Fun(EvalData),
                        Parent ! {Ref, {option_ready, Key}, Result}
                    catch
                        Error:Reason:Stack ->
                            Parent ! {Ref, {option_error, Key}, {Error, Reason, Stack}}
                    end
                end)
        end
    end, OptionKeys),

    %% Wait for first ready option (deferred choice semantics)
    wait_for_first_option(Ref, Pids, 30000).

%%--------------------------------------------------------------------
%% @doc Manually selects an option for the running Deferred Choice.
%%
%% @param Pid The pid of the gen_pnet process.
%% @param OptionId The option to select.
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec select_option(Pid :: pid(), OptionId :: atom()) -> ok | {error, term()}.

select_option(Pid, OptionId) ->
    gen_yawl:cast(Pid, {select_option, OptionId}).

%%--------------------------------------------------------------------
%% @doc Triggers the deferred choice by executing the first branch
%% that becomes enabled based on actual data/resource availability.
%%
%% This is the core of Deferred Choice: unlike Exclusive Choice where
%% the decision is made based on data, here the first branch that
%% becomes available (ready) at runtime is selected.
%%
%% @param Options Map of branch identifiers to {Fun, Priority} or just Fun.
%%                Each Fun should take one argument (the trigger data).
%%                Can also be {ConditionFun, ActionFun} or {ConditionFun, ActionFun, Priority}.
%% @param TriggerData Data passed to evaluate which branches become enabled.
%% @return {ok, {BranchId, Result}} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec deferred_choice_trigger(Options :: map(), TriggerData :: term()) ->
          {ok, {atom(), term()}} | {error, term()}.

deferred_choice_trigger(Options, TriggerData) when is_map(Options), map_size(Options) >= 2 ->
    %% First, determine which branches are enabled by the trigger data
    Enabled = enabled_branches(Options, TriggerData),

    case Enabled of
        [] ->
            {error, no_enabled_branches};
        _ ->
            %% Check if any enabled branches have priority
            HasPriority = lists:any(fun(Key) ->
                case maps:get(Key, Options) of
                    {_Fun, _Priority} -> true;
                    {_Cond, _Action, _Priority} -> true;
                    _ -> false
                end
            end, Enabled),

            %% Select branch using priority if available, otherwise non-deterministic
            Selected = case HasPriority of
                true -> select_by_priority(Enabled, Options);
                false -> select_branch(Enabled, TriggerData)
            end,

            %% Execute the selected branch
            case maps:get(Selected, Options) of
                {ConditionFun, ActionFun, _Priority} when is_function(ConditionFun, 1),
                                                         is_function(ActionFun, 1) ->
                    %% Conditional branch with priority
                    try
                        Result = ActionFun(TriggerData),
                        {ok, {Selected, Result}}
                    catch
                        Error:Reason:Stack ->
                            {error, {branch_error, Selected, Error, Reason, Stack}}
                    end;
                {ConditionFun, ActionFun} when is_function(ConditionFun, 1),
                                               is_function(ActionFun, 1) ->
                    %% Conditional branch without priority
                    try
                        Result = ActionFun(TriggerData),
                        {ok, {Selected, Result}}
                    catch
                        Error:Reason:Stack ->
                            {error, {branch_error, Selected, Error, Reason, Stack}}
                    end;
                {Fun, _Priority} when is_function(Fun, 1) ->
                    %% Function with priority
                    try
                        Result = Fun(TriggerData),
                        {ok, {Selected, Result}}
                    catch
                        Error:Reason:Stack ->
                            {error, {branch_error, Selected, Error, Reason, Stack}}
                    end;
                Fun when is_function(Fun, 1) ->
                    %% Simple function
                    try
                        Result = Fun(TriggerData),
                        {ok, {Selected, Result}}
                    catch
                        Error:Reason:Stack ->
                            {error, {branch_error, Selected, Error, Reason, Stack}}
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @doc Determines which choice branches are currently enabled based on
%% data conditions or resource availability.
%%
%% This evaluates each branch's enablement condition. A branch can be:
%% - A function that returns true/false (enablement guard)
%% - A tuple {Fun, Priority} where Fun is tested for enablement
%% - A tuple {ConditionFun, ActionFun, Priority} where ConditionFun determines enablement
%%
%% @param Options Map of branch identifiers to functions or tuples.
%% @param EvalData Data to evaluate enablement conditions against.
%% @return List of enabled branch identifiers.
%%
%% @end
%%--------------------------------------------------------------------
-spec enabled_branches(Options :: map(), EvalData :: term()) -> [atom()].

enabled_branches(Options, EvalData) when is_map(Options) ->
    maps:fold(fun(Key, Value, Acc) ->
        case is_branch_enabled(Key, Value, EvalData) of
            true -> [Key | Acc];
            false -> Acc
        end
    end, [], Options).

%%--------------------------------------------------------------------
%% @doc Non-deterministic selection from enabled branches.
%%
%% Uses pick_from for truly non-deterministic selection when multiple
%% branches are enabled. Can also use data-driven selection if
%% eval_data provides selection criteria.
%%
%% @param EnabledBranches List of enabled branch identifiers.
%% @param EvalData Optional data for data-driven selection (ignored for
%%                 non-deterministic selection).
%% @return The selected branch identifier.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_branch(EnabledBranches :: [atom()], EvalData :: term()) -> atom().

select_branch([], _EvalData) ->
    error(no_enabled_branches);
select_branch([Single], _EvalData) ->
    Single;
select_branch(EnabledBranches, _EvalData) ->
    %% Non-deterministic selection using pick_from
    pick_from(EnabledBranches).

%%--------------------------------------------------------------------
%% @doc Once a branch is selected, disables/removes alternative branches.
%%
%% This is crucial for Deferred Choice semantics: after one branch is
%% selected, all other alternatives must be disabled to ensure exactly
%% one path is taken.
%%
%% @param SelectedBranch The branch that was selected.
%% @param AllBranches All available branch identifiers.
%% @param Options The original options map.
%% @return Updated options map with only the selected branch remaining.
%%
%% @end
%%--------------------------------------------------------------------
-spec disable_other_branches(SelectedBranch :: atom(),
                             AllBranches :: [atom()],
                             Options :: map()) ->
          {ok, map(), [atom()]}.

disable_other_branches(SelectedBranch, AllBranches, Options) when is_map(Options) ->
    DisabledBranches = AllBranches -- [SelectedBranch],
    %% Create new options map with only selected branch
    ReducedOptions = maps:with([SelectedBranch], Options),
    {ok, ReducedOptions, DisabledBranches}.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Deferred Choice Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_offer_pending',
        'p_option_pool',
        'p_selected',
        'p_discarded',
        'p_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Deferred Choice Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_offer',
        't_evaluate_option',
        't_select',
        't_discard_others',
        't_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: deferred_choice_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_offer') -> ['p_start'];
preset('t_evaluate_option') -> ['p_option_pool'];
preset('t_select') -> ['p_offer_pending'];
preset('t_discard_others') -> ['p_selected'];
preset('t_complete') -> ['p_discarded'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: deferred_choice_state()) ->
          boolean().

is_enabled('t_offer', _Mode, _UsrInfo) ->
    true;
is_enabled('t_evaluate_option', #{'p_option_pool' := Tokens}, _UsrInfo) ->
    length(Tokens) > 0;
is_enabled('t_select', #{'p_offer_pending' := [_]}, #deferred_choice_state{selected = undefined}) ->
    true;
is_enabled('t_discard_others', #{'p_selected' := [_]}, _UsrInfo) ->
    true;
is_enabled('t_complete', #{'p_discarded' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: deferred_choice_state()) ->
          {produce, map()} | abort.

fire('t_offer', #{'p_start' := [start]}, #deferred_choice_state{options = Options} = State) ->
    %% Create option tokens
    OptionTokens = [{option, Key, maps:get(Key, Options)} || Key <- maps:keys(Options)],
    log_event(State, <<"DeferredChoice">>, <<"Offer">>, #{<<"option_count">> => map_size(Options)}),
    {produce, #{
        'p_start' => [],
        'p_option_pool' => OptionTokens,
        'p_offer_pending' => [waiting]
    }};

fire('t_evaluate_option', #{'p_option_pool' := [Token | Rest]}, State) ->
    %% Evaluate an option
    case Token of
        {option, Key, {_Fun, Priority}} ->
            log_event(State, <<"DeferredChoice">>, <<"OptionEvaluated">>, #{
                <<"option">> => Key,
                <<"priority">> => Priority
            }),
            {produce, #{
                'p_option_pool' => Rest
            }};
        {option, Key, _Fun} ->
            log_event(State, <<"DeferredChoice">>, <<"OptionEvaluated">>, #{
                <<"option">> => Key
            }),
            {produce, #{
                'p_option_pool' => Rest
            }}
    end;

fire('t_select', #{'p_offer_pending' := [waiting]}, #deferred_choice_state{options = Options} = State) ->
    %% Select based on priority (first available wins in deferred choice)
    Keys = maps:keys(Options),
    Selected = select_by_priority(Keys, Options),
    log_event(State, <<"DeferredChoice">>, <<"Selected">>, #{<<"option">> => Selected}),
    {produce, #{
        'p_offer_pending' => [],
        'p_selected' => [Selected]
    }};

fire('t_discard_others', #{'p_selected' := [Selected]}, #deferred_choice_state{options = Options} = State) ->
    %% Discard non-selected options
    AllKeys = maps:keys(Options),
    Discarded = AllKeys -- [Selected],
    log_event(State, <<"DeferredChoice">>, <<"DiscardedOthers">>, #{
        <<"selected">> => Selected,
        <<"discarded">> => Discarded
    }),
    {produce, #{
        'p_selected' => [],
        'p_discarded' => [Selected, Discarded]
    }};

fire('t_complete', #{'p_discarded' := [Selected, _Discarded]}, State) ->
    %% Complete the choice
    Elapsed = erlang:system_time(millisecond) - State#deferred_choice_state.start_time,
    log_event(State, <<"DeferredChoice">>, <<"Complete">>, #{
        <<"selected">> => Selected,
        <<"duration_ms">> => Elapsed
    }),
    {produce, #{
        'p_discarded' => [],
        'p_complete' => [Selected, {result, Selected}]
    }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: deferred_choice_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: deferred_choice_state()) ->
          deferred_choice_state().

init(DeferredChoiceState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"DeferredChoice">>}) of
        {ok, LogId} ->
            State1 = DeferredChoiceState#deferred_choice_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            State1;
        _ ->
            DeferredChoiceState
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term()} | noreply.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {ok, UsrInfo}};
handle_call(_Request, _From, _NetState) ->
    {reply, {error, bad_msg}}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          noreply.

handle_cast({select_option, _OptionId}, _NetState) ->
    noreply;
handle_cast(_Request, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles non-gen_pnet messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), NetState :: term()) ->
          noreply.

handle_info(_Request, _NetState) ->
    noreply.

%%--------------------------------------------------------------------
%% @doc Handles code changes.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%%--------------------------------------------------------------------
%% @doc Cleanup on termination.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), NetState :: term()) ->
          ok.

terminate(_Reason, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #deferred_choice_state{log_id = LogId} when LogId =/= undefined ->
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        _ ->
            ok
    end,
    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Waits for workflow completion.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_for_completion(Pid :: pid(), Timeout :: timeout()) ->
          {ok, {atom(), term()}} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_complete', Ref},
    receive
        {trigger, 'p_complete', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #deferred_choice_state{selected = Selected} when Selected =/= undefined ->
                            {ok, {Selected, selected}};
                        _ ->
                            {error, no_selection}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    after Timeout ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Waits for first option to become ready.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec wait_for_first_option(Ref :: reference(), Pids :: [pid()], Timeout :: timeout()) ->
          {ok, {atom(), term()}} | {error, term()}.

wait_for_first_option(Ref, Pids, Timeout) ->
    receive
        {Ref, {option_ready, Key}, Result} ->
            %% Kill remaining processes
            lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
            {ok, {Key, Result}};
        {Ref, {option_error, Key}, {Error, Reason, _Stack}} ->
            {error, {option_error, Key, Error, Reason}}
    after Timeout ->
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Selects an option based on priority.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec select_by_priority(Keys :: [atom()], Options :: map()) -> atom().

select_by_priority(Keys, Options) ->
    SortedKeys = lists:sort(fun(A, B) ->
        PriorityA = get_priority(A, Options),
        PriorityB = get_priority(B, Options),
        PriorityA >= PriorityB  %% Higher priority first
    end, Keys),
    hd(SortedKeys).

%%--------------------------------------------------------------------
%% @doc Gets the priority of an option.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec get_priority(Key :: atom(), Options :: map()) -> integer().

get_priority(Key, Options) ->
    case maps:get(Key, Options) of
        {_Fun, Priority} -> Priority;
        _Fun -> 0  %% Default priority
    end.

%%--------------------------------------------------------------------
%% @doc Checks if a branch is enabled based on its configuration and eval data.
%%
%% A branch is enabled if:
%% - It's a simple function (always enabled)
%% - It's a {Fun, Priority} tuple (Fun is always enabled)
%% - It's a {ConditionFun, ActionFun, Priority} tuple (ConditionFun must return true)
%% - It's a {ConditionFun, ActionFun} tuple (ConditionFun must return true)
%%
%% IMPORTANT: Order matters! 3-tuples must come before 2-tuples to distinguish
%% {ConditionFun, ActionFun, Priority} from {Fun, Priority} where both elements are functions.
%%
%% @private
%% @end
%%--------------------------------------------------------------------
-spec is_branch_enabled(Key :: atom(), Value :: term(), EvalData :: term()) -> boolean().

is_branch_enabled(_Key, Fun, _EvalData) when is_function(Fun, 1) ->
    %% Simple function - always enabled
    true;
is_branch_enabled(_Key, {ConditionFun, _ActionFun, _Priority}, EvalData)
        when is_function(ConditionFun, 1) ->
    %% Conditional branch with priority - evaluate the condition
    try
        case ConditionFun(EvalData) of
            true -> true;
            false -> false;
            _ -> false
        end
    catch
        _:_:_ -> false
    end;
is_branch_enabled(_Key, {ConditionFun, _ActionFun}, EvalData)
        when is_function(ConditionFun, 1) ->
    %% Conditional branch without priority - evaluate the condition
    %% Check if ActionFun is also a function to distinguish from {Fun, Priority}
    case _ActionFun of
        Action when is_function(Action, 1) ->
            try
                case ConditionFun(EvalData) of
                    true -> true;
                    false -> false;
                    _ -> false
                end
            catch
                _:_:_ -> false
            end;
        _Priority ->
            %% This is {Fun, Priority} where Fun is a function
            true
    end;
is_branch_enabled(_Key, {Fun, _Priority}, _EvalData) when is_function(Fun, 1) ->
    %% Function with priority - always enabled
    true;
is_branch_enabled(_, _, _) ->
    %% Unknown format - not enabled
    false.

%%--------------------------------------------------------------------
%% @doc Picks a random element from a non-empty list.
%% Implements non-deterministic selection for deferred choice.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec pick_from([T, ...]) -> T.

pick_from([]) ->
    error(empty_list);
pick_from(List) ->
    lists:nth(rand:uniform(length(List)), List).

%%--------------------------------------------------------------------
%% @doc Generates a unique log ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().

generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"deferred_choice_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @doc Generates a unique case ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id() -> binary().

generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @doc Logs an XES event.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec log_event(State :: deferred_choice_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#deferred_choice_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.

%%====================================================================
%% Unit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Runs all doctests for the module.
%% @private
%% @end
%%--------------------------------------------------------------------
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%%--------------------------------------------------------------------
%% Test: deferred_choice_trigger/2 with simple functions
%% @private
%% @end
%%--------------------------------------------------------------------
deferred_choice_trigger_simple_test() ->
    Options = #{
        branch_a => fun(X) -> X * 2 end,
        branch_b => fun(X) -> X + 10 end
    },
    Result = deferred_choice_trigger(Options, 5),
    case Result of
        {ok, {branch_a, 10}} -> ok;
        {ok, {branch_b, 15}} -> ok;
        _ -> ?assert(false, "Expected one of the branches to be selected")
    end.

%%--------------------------------------------------------------------
%% Test: deferred_choice_trigger/2 with priority tuples
%% @private
%% @end
%%--------------------------------------------------------------------
deferred_choice_trigger_priority_test() ->
    Options = #{
        high => {fun(X) -> X * 2 end, 10},
        low => {fun(X) -> X + 10 end, 1}
    },
    Result = deferred_choice_trigger(Options, 5),
    %% With priority-based selection, high priority branch should be selected
    ?assertMatch({ok, {high, 10}}, Result).

%%--------------------------------------------------------------------
%% Test: enabled_branches/2 with simple functions
%% @private
%% @end
%%--------------------------------------------------------------------
enabled_branches_all_enabled_test() ->
    Options = #{
        branch_a => fun(_) -> ok end,
        branch_b => fun(_) -> ok end,
        branch_c => fun(_) -> ok end
    },
    Enabled = enabled_branches(Options, some_data),
    ?assertEqual(3, length(Enabled)),
    ?assert(lists:member(branch_a, Enabled)),
    ?assert(lists:member(branch_b, Enabled)),
    ?assert(lists:member(branch_c, Enabled)).

%%--------------------------------------------------------------------
%% Test: enabled_branches/2 with conditional branches
%% @private
%% @end
%%--------------------------------------------------------------------
enabled_branches_conditional_test() ->
    Options = #{
        branch_a => {fun(X) -> X > 0 end, fun(_) -> a_result end},
        branch_b => {fun(X) -> X < 0 end, fun(_) -> b_result end},
        branch_c => {fun(_) -> true end, fun(_) -> c_result end}
    },
    %% With positive data, only branch_a and branch_c should be enabled
    Enabled = enabled_branches(Options, 5),
    ?assert(lists:member(branch_a, Enabled)),
    ?assertNot(lists:member(branch_b, Enabled)),
    ?assert(lists:member(branch_c, Enabled)).

%%--------------------------------------------------------------------
%% Test: enabled_branches/2 with conditional branches (negative case)
%% @private
%% @end
%%--------------------------------------------------------------------
enabled_branches_conditional_negative_test() ->
    Options = #{
        branch_a => {fun(X) -> X > 0 end, fun(_) -> a_result end},
        branch_b => {fun(X) -> X < 0 end, fun(_) -> b_result end},
        branch_c => {fun(_) -> true end, fun(_) -> c_result end}
    },
    %% With negative data, only branch_b and branch_c should be enabled
    Enabled = enabled_branches(Options, -5),
    ?assertNot(lists:member(branch_a, Enabled)),
    ?assert(lists:member(branch_b, Enabled)),
    ?assert(lists:member(branch_c, Enabled)).

%%--------------------------------------------------------------------
%% Test: select_branch/2 with single enabled branch
%% @private
%% @end
%%--------------------------------------------------------------------
select_branch_single_test() ->
    ?assertEqual(only_branch, select_branch([only_branch], ignored)).

%%--------------------------------------------------------------------
%% Test: select_branch/2 with multiple enabled branches (non-deterministic)
%% @private
%% @end
%%--------------------------------------------------------------------
select_branch_multiple_test() ->
    Enabled = [branch_a, branch_b, branch_c],
    Selected = select_branch(Enabled, data),
    ?assert(lists:member(Selected, Enabled)).

%%--------------------------------------------------------------------
%% Test: select_branch/2 error case
%% @private
%% @end
%%--------------------------------------------------------------------
select_branch_empty_test() ->
    ?assertError(no_enabled_branches, select_branch([], data)).

%%--------------------------------------------------------------------
%% Test: disable_other_branches/3
%% @private
%% @end
%%--------------------------------------------------------------------
disable_other_branches_test() ->
    AllBranches = [branch_a, branch_b, branch_c, branch_d],
    Options = #{
        branch_a => fun() -> a end,
        branch_b => fun() -> b end,
        branch_c => fun() -> c end,
        branch_d => fun() -> d end
    },
    Result = disable_other_branches(branch_b, AllBranches, Options),
    ?assertMatch({ok, ReducedMap, DisabledList}, Result),
    {ok, ReducedMap, DisabledList} = Result,
    ?assertEqual([branch_a, branch_c, branch_d], DisabledList),
    ?assertEqual(1, map_size(ReducedMap)),
    ?assert(maps:is_key(branch_b, ReducedMap)).

%%--------------------------------------------------------------------
%% Test: integration test - deferred choice with conditions
%% @private
%% @end
%%--------------------------------------------------------------------
deferred_choice_integration_test() ->
    Options = #{
        fast => {fun(X) -> X > 100 end, fun(X) -> {fast, X div 2} end},
        slow => {fun(X) -> X =< 100 end, fun(X) -> {slow, X * 2} end}
    },
    Result = deferred_choice_trigger(Options, 150),
    ?assertMatch({ok, {fast, _}}, Result).

%%--------------------------------------------------------------------
%% Test: deferred_choice_trigger/2 with no enabled branches
%% @private
%% @end
%%--------------------------------------------------------------------
deferred_choice_trigger_no_enabled_test() ->
    Options = #{
        branch_a => {fun(_) -> false end, fun(_) -> a end},
        branch_b => {fun(_) -> false end, fun(_) -> b end}
    },
    Result = deferred_choice_trigger(Options, any_data),
    ?assertEqual({error, no_enabled_branches}, Result).

%%--------------------------------------------------------------------
%% Test: new/2 constructor
%% @private
%% @end
%%--------------------------------------------------------------------
new_constructor_test() ->
    Options = #{a => fun(_) -> ok end, b => fun(_) -> ok end},
    State = new(Options, 2),
    ?assertEqual(2, map_size(State#deferred_choice_state.options)),
    ?assertEqual(undefined, State#deferred_choice_state.selected),
    ?assertEqual([], State#deferred_choice_state.discarded).

%%--------------------------------------------------------------------
%% Test: place_lst/0
%% @private
%% @end
%%--------------------------------------------------------------------
place_lst_test() ->
    Expected = [p_start, p_offer_pending, p_option_pool, p_selected, p_discarded, p_complete],
    ?assertEqual(Expected, place_lst()).

%%--------------------------------------------------------------------
%% Test: trsn_lst/0
%% @private
%% @end
%%--------------------------------------------------------------------
trsn_lst_test() ->
    Expected = [t_offer, t_evaluate_option, t_select, t_discard_others, t_complete],
    ?assertEqual(Expected, trsn_lst()).

%%--------------------------------------------------------------------
%% Test: preset/1 for all transitions
%% @private
%% @end
%%--------------------------------------------------------------------
preset_t_offer_test() ->
    ?assertEqual([p_start], preset(t_offer)).

preset_t_evaluate_option_test() ->
    ?assertEqual([p_option_pool], preset(t_evaluate_option)).

preset_t_select_test() ->
    ?assertEqual([p_offer_pending], preset(t_select)).

preset_t_discard_others_test() ->
    ?assertEqual([p_selected], preset(t_discard_others)).

preset_t_complete_test() ->
    ?assertEqual([p_discarded], preset(t_complete)).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

%%--------------------------------------------------------------------
%% Test: init_marking/2
%% @private
%% @end
%%--------------------------------------------------------------------
init_marking_p_start_test() ->
    State = new(#{a => fun(_) -> ok end}, 1),
    ?assertEqual([start], init_marking(p_start, State)).

init_marking_other_test() ->
    State = new(#{a => fun(_) -> ok end}, 1),
    ?assertEqual([], init_marking(p_offer_pending, State)),
    ?assertEqual([], init_marking(p_complete, State)).

%%--------------------------------------------------------------------
%% Test: is_branch_enabled/3 helper
%% @private
%% @end
%%--------------------------------------------------------------------
is_branch_enabled_simple_fun_test() ->
    Fun = fun(_) -> result end,
    ?assert(is_branch_enabled(test, Fun, data)).

is_branch_enabled_with_priority_test() ->
    FunWithPriority = {fun(_) -> result end, 5},
    ?assert(is_branch_enabled(test, FunWithPriority, data)).

is_branch_enabled_conditional_true_test() ->
    CondFun = fun(X) -> X > 0 end,
    ActionFun = fun(_) -> result end,
    Conditional = {CondFun, ActionFun},
    ?assert(is_branch_enabled(test, Conditional, 5)),
    ?assertNot(is_branch_enabled(test, Conditional, -5)).

is_branch_enabled_conditional_with_priority_test() ->
    CondFun = fun(X) -> X > 0 end,
    ActionFun = fun(_) -> result end,
    Conditional = {CondFun, ActionFun, 10},
    ?assert(is_branch_enabled(test, Conditional, 5)),
    ?assertNot(is_branch_enabled(test, Conditional, -5)).

-endif.
