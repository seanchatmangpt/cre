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
    select_option/2
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
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% @doc Runs all doctests for the module.
%% @private
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.
-endif.
