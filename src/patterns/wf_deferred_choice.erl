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

-module(wf_deferred_choice).
-moduledoc """
Deferred Choice Pattern (WCP-16) for YAWL.

This module implements the Deferred Choice pattern as a gen_pnet behaviour.

## Pattern Description

The Deferred Choice pattern (WCP-16) represents a divergence in the process
where the choice is made at runtime based on which branch becomes available
first, rather than being predetermined by data or conditions. The first
thread to offer a token wins, and other options are canceled.

## Petri Net Structure

Places:
- `p_input` - Input point for the choice
- `p_options_offered` - Options are offered to external environment
- `p_option_A` - Option A execution
- `p_option_B` - Option B execution
- `p_option_C` - Option C execution (optional)
- `p_selected` - One option was selected
- `p_output` - Final output after deferred choice

Transitions:
- `t_offer` - Offer all options
- `t_select_A` - Select option A (enabled when available)
- `t_select_B` - Select option B (enabled when available)
- `t_select_C` - Select option C (enabled when available)
- `t_cancel_B` - Cancel option B when A selected
- `t_cancel_C` - Cancel option C when A selected
- `t_cancel_A_C` - Cancel option C when B selected
- `t_finalize` - Complete after cancellations

## Soundness Properties

- **Option to complete:** Always true (at least one option available)
- **Proper completion:** Exactly one output token
- **Fairness:** First available option selected; others canceled

## Examples

Get the list of places:

```erlang
> wf_deferred_choice:place_lst().
[p_input,p_options_offered,p_option_A,p_option_B,p_option_C,
 p_selected,p_output]
```

Get the list of transitions:

```erlang
> wf_deferred_choice:trsn_lst().
[t_offer,t_select_A,t_select_B,t_select_C,t_cancel_B,t_cancel_C,
 t_cancel_A_C,t_finalize]
```

Get the preset for a transition:

```erlang
> wf_deferred_choice:preset(t_offer).
[p_input]
> wf_deferred_choice:preset(t_select_A).
[p_option_A,p_options_offered]
```
""".

-behaviour(gen_pnet).

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
    execute/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(wf_deferred_choice_state, {
    option_count :: pos_integer(),
    options = #{} :: map(),
    selected :: undefined | atom(),
    pending = [] :: [atom()],
    cancelled = [] :: [atom()],
    start_time :: integer(),
    log_id :: binary() | undefined
}).

-type wf_deferred_choice_state() :: #wf_deferred_choice_state{}.
-export_type([wf_deferred_choice_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec new(Options :: map(), OptionCount :: pos_integer()) ->
          wf_deferred_choice_state().

new(Options, OptionCount) when is_map(Options),
                               map_size(Options) =:= OptionCount,
                               OptionCount >= 2 ->
    LogId = generate_log_id(),
    #wf_deferred_choice_state{
        option_count = OptionCount,
        options = Options,
        start_time = erlang:system_time(millisecond),
        log_id = LogId
    }.

-spec start(Options :: map()) -> {ok, pid()} | {error, term()}.

start(Options) when is_map(Options), map_size(Options) >= 2 ->
    OptionCount = map_size(Options),
    ChoiceState = new(Options, OptionCount),
    gen_pnet:start_link(?MODULE, ChoiceState, []).

-spec run(Options :: map()) -> {ok, {atom(), term()}} | {error, term()}.

run(Options) when is_map(Options), map_size(Options) >= 2 ->
    case start(Options) of
        {ok, Pid} ->
            case wait_for_completion(Pid, 30000) of
                {ok, Result} ->
                    gen_pnet:stop(Pid),
                    {ok, Result};
                {error, Reason} ->
                    gen_pnet:stop(Pid),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

-spec get_state(Pid :: pid()) ->
          {ok, wf_deferred_choice_state()} | {error, term()}.

get_state(Pid) ->
    gen_pnet:call(Pid, get_state).

-spec execute(Options :: map(), InputData :: term()) ->
          {ok, {atom(), term()}} | {error, term()}.

execute(Options, InputData) when is_map(Options), map_size(Options) >= 2 ->
    Ref = make_ref(),
    Parent = self(),
    OptionKeys = maps:keys(Options),

    Pids = lists:map(fun(Key) ->
        spawn(fun() ->
            case maps:get(Key, Options) of
                Fun when is_function(Fun, 1) ->
                    try
                        Result = Fun(InputData),
                        Parent ! {Ref, {option_ready, Key}, Result}
                    catch
                        Error:Reason:Stack ->
                            Parent ! {Ref, {option_error, Key}, {Error, Reason, Stack}}
                    end;
                {Fun, _Priority} when is_function(Fun, 1) ->
                    try
                        Result = Fun(InputData),
                        Parent ! {Ref, {option_ready, Key}, Result}
                    catch
                        Error:Reason:Stack ->
                            Parent ! {Ref, {option_error, Key}, {Error, Reason, Stack}}
                    end
            end
        end)
    end, OptionKeys),

    wait_for_first_option(Ref, Pids, 30000).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_input',
        'p_options_offered',
        'p_option_A',
        'p_option_B',
        'p_option_C',
        'p_selected',
        'p_output'
    ].

-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_offer',
        't_select_A',
        't_select_B',
        't_select_C',
        't_cancel_B',
        't_cancel_C',
        't_cancel_A_C',
        't_finalize'
    ].

-spec init_marking(Place :: atom(), UsrInfo :: wf_deferred_choice_state()) ->
          [term()].

init_marking('p_input', _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

-spec preset(Trsn :: atom()) -> [atom()].

preset('t_offer') -> ['p_input'];
preset('t_select_A') -> ['p_option_A', 'p_options_offered'];
preset('t_select_B') -> ['p_option_B', 'p_options_offered'];
preset('t_select_C') -> ['p_option_C', 'p_options_offered'];
preset('t_cancel_B') -> ['p_option_B'];
preset('t_cancel_C') -> ['p_option_C'];
preset('t_cancel_A_C') -> ['p_option_A', 'p_option_C'];
preset('t_finalize') -> ['p_selected'];
preset(_) -> [].

-spec is_enabled(Trsn :: atom(), Mode :: map(),
                 UsrInfo :: wf_deferred_choice_state()) ->
          boolean().

is_enabled('t_offer', #{'p_input' := [start]}, _UsrInfo) ->
    true;
is_enabled('t_select_A', #{'p_option_A' := [_], 'p_options_offered' := [_]},
           #wf_deferred_choice_state{selected = undefined}) ->
    true;
is_enabled('t_select_B', #{'p_option_B' := [_], 'p_options_offered' := [_]},
           #wf_deferred_choice_state{selected = undefined}) ->
    true;
is_enabled('t_select_C', #{'p_option_C' := [_], 'p_options_offered' := [_]},
           #wf_deferred_choice_state{selected = undefined, option_count = Count})
          when Count >= 3 ->
    true;
is_enabled('t_cancel_B', #{'p_option_B' := [_]},
           #wf_deferred_choice_state{selected = a}) ->
    true;
is_enabled('t_cancel_C', #{'p_option_C' := [_]},
           #wf_deferred_choice_state{selected = a}) ->
    true;
is_enabled('t_cancel_A_C', #{'p_option_A' := [_], 'p_option_C' := [_]},
           #wf_deferred_choice_state{selected = b}) ->
    true;
is_enabled('t_finalize', #{'p_selected' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

-spec fire(Trsn :: atom(), Mode :: map(),
           UsrInfo :: wf_deferred_choice_state()) ->
          {produce, map()} | abort.

fire('t_offer', #{'p_input' := [start]},
     #wf_deferred_choice_state{option_count = Count} = State) ->
    log_event(State, <<"DeferredChoice">>, <<"Offer">>,
              #{<<"option_count">> => Count}),
    OptionTokens = case Count of
        2 -> [{option, a}, {option, b}];
        3 -> [{option, a}, {option, b}, {option, c}];
        N when N > 3 -> [{option, a}, {option, b}, {option, c}]
    end,
    {produce, #{
        'p_input' => [],
        'p_options_offered' => [offered],
        'p_option_A' => [{option, a}],
        'p_option_B' => [{option, b}],
        'p_option_C' => [{option, c}]
    }};

fire('t_select_A', #{'p_option_A' := [{option, a}],
                     'p_options_offered' := [offered]},
     #wf_deferred_choice_state{} = State) ->
    NewState = State#wf_deferred_choice_state{selected = a},
    log_event(State, <<"DeferredChoice">>, <<"SelectA">>, #{}),
    {produce, #{
        'p_option_A' => [],
        'p_options_offered' => [],
        'p_option_B' => [],
        'p_option_C' => [],
        'p_selected' => [selected_a]
    }};

fire('t_select_B', #{'p_option_B' := [{option, b}],
                     'p_options_offered' := [offered]},
     #wf_deferred_choice_state{} = State) ->
    NewState = State#wf_deferred_choice_state{selected = b},
    log_event(State, <<"DeferredChoice">>, <<"SelectB">>, #{}),
    {produce, #{
        'p_option_A' => [],
        'p_options_offered' => [],
        'p_option_B' => [],
        'p_option_C' => [],
        'p_selected' => [selected_b]
    }};

fire('t_select_C', #{'p_option_C' := [{option, c}],
                     'p_options_offered' := [offered]},
     #wf_deferred_choice_state{option_count = Count} = State)
     when Count >= 3 ->
    NewState = State#wf_deferred_choice_state{selected = c},
    log_event(State, <<"DeferredChoice">>, <<"SelectC">>, #{}),
    {produce, #{
        'p_option_A' => [],
        'p_options_offered' => [],
        'p_option_B' => [],
        'p_option_C' => [],
        'p_selected' => [selected_c]
    }};

fire('t_cancel_B', #{'p_option_B' := [{option, b}]},
     #wf_deferred_choice_state{selected = a} = State) ->
    log_event(State, <<"DeferredChoice">>, <<"CancelB">>, #{}),
    {produce, #{
        'p_option_B' => []
    }};

fire('t_cancel_C', #{'p_option_C' := [{option, c}]},
     #wf_deferred_choice_state{selected = a} = State) ->
    log_event(State, <<"DeferredChoice">>, <<"CancelC">>, #{}),
    {produce, #{
        'p_option_C' => []
    }};

fire('t_cancel_A_C', #{'p_option_A' := [{option, a}],
                       'p_option_C' := [{option, c}]},
     #wf_deferred_choice_state{selected = b} = State) ->
    log_event(State, <<"DeferredChoice">>, <<"CancelAC">>, #{}),
    {produce, #{
        'p_option_A' => [],
        'p_option_C' => []
    }};

fire('t_finalize', #{'p_selected' := [Token]}, State) ->
    Elapsed = erlang:system_time(millisecond) - State#wf_deferred_choice_state.start_time,
    log_event(State, <<"DeferredChoice">>, <<"Complete">>,
              #{<<"duration_ms">> => Elapsed}),
    {produce, #{
        'p_selected' => [],
        'p_output' => [Token]
    }};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

-spec trigger(Place :: atom(), Token :: term(),
              UsrInfo :: wf_deferred_choice_state()) ->
          pass | drop.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

-spec init(UsrInfo :: wf_deferred_choice_state()) ->
          wf_deferred_choice_state().

init(DeferredChoiceState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"DeferredChoice">>}) of
        {ok, LogId} ->
            State1 = DeferredChoiceState#wf_deferred_choice_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            State1;
        _ ->
            DeferredChoiceState
    end.

-spec handle_call(Request :: term(), From :: {pid(), term()},
                  NetState :: term()) ->
          {reply, term()} | noreply.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_pnet:get_usr_info(NetState),
    {reply, {ok, UsrInfo}};
handle_call(_Request, _From, _NetState) ->
    {reply, {error, bad_msg}}.

-spec handle_cast(Request :: term(), NetState :: term()) ->
          noreply.

handle_cast(_Request, _NetState) ->
    noreply.

-spec handle_info(Request :: term(), NetState :: term()) ->
          noreply.

handle_info(_Request, _NetState) ->
    noreply.

-spec code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
          {ok, term()}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

-spec terminate(Reason :: term(), NetState :: term()) ->
          ok.

terminate(_Reason, NetState) ->
    UsrInfo = gen_pnet:get_usr_info(NetState),
    case UsrInfo of
        #wf_deferred_choice_state{log_id = LogId} when LogId =/= undefined ->
            yawl_xes:log_case_end(LogId),
            yawl_xes:close_log(LogId);
        _ ->
            ok
    end,
    ok.

%%====================================================================
%% Internal Helper Functions
%%====================================================================

-spec wait_for_completion(Pid :: pid(), Timeout :: timeout()) ->
          {ok, {atom(), term()}} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_output', Ref},
    receive
        {trigger, 'p_output', Ref, pass} ->
            case gen_pnet:sync(Pid, 1000) of
                {ok, _} ->
                    {ok, {selected, success}};
                {error, Reason} ->
                    {error, Reason}
            end
    after Timeout ->
        {error, timeout}
    end.

-spec wait_for_first_option(Ref :: reference(), Pids :: [pid()],
                            Timeout :: timeout()) ->
          {ok, {atom(), term()}} | {error, term()}.

wait_for_first_option(Ref, Pids, Timeout) ->
    receive
        {Ref, {option_ready, Key}, Result} ->
            lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
            {ok, {Key, Result}};
        {Ref, {option_error, Key}, {Error, Reason, _Stack}} ->
            {error, {option_error, Key, Error, Reason}}
    after Timeout ->
        lists:foreach(fun(Pid) -> exit(Pid, kill) end, Pids),
        {error, timeout}
    end.

-spec generate_log_id() -> binary().

generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"wf_deferred_choice_", Hex/binary>>.

-spec generate_case_id() -> binary().

generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

-spec log_event(State :: wf_deferred_choice_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#wf_deferred_choice_state{log_id = LogId}, Concept, Lifecycle, Data)
         when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

place_lst_test() ->
    Expected = [p_input, p_options_offered, p_option_A, p_option_B,
                p_option_C, p_selected, p_output],
    ?assertEqual(Expected, place_lst()).

trsn_lst_test() ->
    Expected = [t_offer, t_select_A, t_select_B, t_select_C, t_cancel_B,
                t_cancel_C, t_cancel_A_C, t_finalize],
    ?assertEqual(Expected, trsn_lst()).

preset_t_offer_test() ->
    ?assertEqual([p_input], preset(t_offer)).

preset_t_select_A_test() ->
    ?assertEqual([p_option_A, p_options_offered], preset(t_select_A)).

preset_t_finalize_test() ->
    ?assertEqual([p_selected], preset(t_finalize)).

preset_unknown_test() ->
    ?assertEqual([], preset(unknown)).

new_2_options_test() ->
    Options = #{a => fun(_) -> ok end, b => fun(_) -> ok end},
    State = new(Options, 2),
    ?assertEqual(2, State#wf_deferred_choice_state.option_count),
    ?assertEqual(undefined, State#wf_deferred_choice_state.selected).

init_marking_p_input_test() ->
    Options = #{a => fun(_) -> ok end, b => fun(_) -> ok end},
    State = new(Options, 2),
    ?assertEqual([start], init_marking(p_input, State)).

init_marking_other_places_test() ->
    Options = #{a => fun(_) -> ok end, b => fun(_) -> ok end},
    State = new(Options, 2),
    ?assertEqual([], init_marking(p_option_A, State)),
    ?assertEqual([], init_marking(p_selected, State)).

-endif.
