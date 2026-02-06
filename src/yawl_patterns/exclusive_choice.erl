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
%% @doc Exclusive Choice Pattern (WCP-04) for YAWL
%%
%% This module implements the Exclusive Choice pattern as a gen_pnet behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Exclusive Choice pattern (WCP-04) represents a divergence in the process
%% where exactly one of multiple alternative branches is selected based on
%% conditions or data available at runtime. Only ONE branch is taken.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start          - Start of the exclusive choice
%%     p_choice         - Choice point (one token consumed)
%%     p_branch_a       - Branch A execution place
%%     p_branch_b       - Branch B execution place
%%     p_selected       - Indicates which branch was selected
%%     p_end            - End of the pattern
%%
%%   Transitions:
%%     t_select_a       - Select branch A (consumes from p_choice)
%%     t_select_b       - Select branch B (consumes from p_choice)
%%     t_finish         - Complete the pattern
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always true (exactly one branch selected)</li>
%%   <li><b>Proper completion:</b> Exactly one output token per input</li>
%%   <li><b>No dead transitions:</b> One selection transition always fires</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(exclusive_choice).
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
    execute/2,
    select_branch/2
]).

%%====================================================================
%% Records
%%====================================================================

-record(exclusive_choice_state, {
    branches :: map(),  %% Map of branch_id => function()
    selected :: undefined | atom(),
    branch_count :: pos_integer(),
    start_time :: integer(),
    log_id :: binary() | undefined
}).

-type exclusive_choice_state() :: #exclusive_choice_state{}.
-export_type([exclusive_choice_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Exclusive Choice pattern state.
%%
%% @param Branches Map of branch identifiers to functions.
%% @param BranchCount Number of branches (must match size of Branches map).
%% @return A new exclusive_choice_state record.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(Branches :: map(), BranchCount :: pos_integer()) -> exclusive_choice_state().

new(Branches, BranchCount) when is_map(Branches),
                                map_size(Branches) =:= BranchCount,
                                BranchCount >= 2 ->
    LogId = generate_log_id(),
    #exclusive_choice_state{
        branches = Branches,
        branch_count = BranchCount,
        start_time = erlang:system_time(millisecond),
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Exclusive Choice workflow as a gen_pnet process.
%%
%% @param Branches Map of branch identifiers to functions.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(Branches :: map()) -> {ok, pid()} | {error, term()}.

start(Branches) when is_map(Branches), map_size(Branches) >= 2 ->
    BranchCount = map_size(Branches),
    ChoiceState = new(Branches, BranchCount),
    gen_pnet:start_link(?MODULE, ChoiceState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Exclusive Choice workflow synchronously.
%%
%% @param Branches Map of branch identifiers to functions.
%% @return {ok, {BranchId, Result}} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec run(Branches :: map()) -> {ok, {atom(), term()}} | {error, term()}.

run(Branches) when is_map(Branches), map_size(Branches) >= 2 ->
    case start(Branches) of
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

%%--------------------------------------------------------------------
%% @doc Gets the current state of the Exclusive Choice workflow.
%%
%% @param Pid The pid of the gen_pnet process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, exclusive_choice_state()} | {error, term()}.

get_state(Pid) ->
    gen_pnet:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes the Exclusive Choice pattern with given input data.
%%
%% @param Branches Map of branch identifiers to functions.
%% @param InputData Input data to evaluate branch conditions with.
%% @return {ok, {BranchId, Result}} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(Branches :: map(), InputData :: term()) ->
          {ok, {atom(), term()}} | {error, term()}.

execute(Branches, InputData) when is_map(Branches), map_size(Branches) >= 2 ->
    %% Use lib_combin:pick_from for nondeterministic selection
    BranchKeys = maps:keys(Branches),
    Selected = lib_combin:pick_from(BranchKeys),
    Fun = maps:get(Selected, Branches),
    try
        Result = Fun(InputData),
        {ok, {Selected, Result}}
    catch
        Error:Reason:Stack ->
            {error, {branch_error, Selected, Error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Manually selects a branch for the running Exclusive Choice.
%%
%% @param Pid The pid of the gen_pnet process.
%% @param BranchId The branch to select.
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec select_branch(Pid :: pid(), BranchId :: atom()) -> ok | {error, term()}.

select_branch(Pid, BranchId) ->
    gen_pnet:cast(Pid, {select_branch, BranchId}).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Exclusive Choice Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_choice',
        'p_selected',
        'p_end'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Exclusive Choice Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_select_a',
        't_select_b',
        't_finish'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: exclusive_choice_state()) ->
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

preset('t_select_a') -> ['p_start'];
preset('t_select_b') -> ['p_start'];
preset('t_finish') -> ['p_selected'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: exclusive_choice_state()) ->
          boolean().

is_enabled('t_select_a', #{'p_start' := [start]}, #exclusive_choice_state{selected = undefined}) ->
    true;
is_enabled('t_select_b', #{'p_start' := [start]}, #exclusive_choice_state{selected = undefined}) ->
    true;
is_enabled('t_finish', #{'p_selected' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: exclusive_choice_state()) ->
          {produce, map()} | {produce, map(), exclusive_choice_state()} | abort.

fire('t_select_a', #{'p_start' := [start]}, #exclusive_choice_state{branches = Branches} = State) ->
    %% Select branch A using nondeterministic choice
    BranchKeys = maps:keys(Branches),
    Selected = lib_combin:pick_from(BranchKeys),
    NewState = State#exclusive_choice_state{selected = Selected},
    log_event(State, <<"ExclusiveChoice">>, <<"SelectedA">>, #{<<"branch">> => Selected}),
    {produce, #{
        'p_start' => [],
        'p_choice' => [Selected],
        'p_selected' => [Selected]
    }, NewState};

fire('t_select_b', #{'p_start' := [start]}, #exclusive_choice_state{branches = Branches} = State) ->
    %% Select branch B using nondeterministic choice
    BranchKeys = maps:keys(Branches),
    Selected = lib_combin:pick_from(BranchKeys),
    NewState = State#exclusive_choice_state{selected = Selected},
    log_event(State, <<"ExclusiveChoice">>, <<"SelectedB">>, #{<<"branch">> => Selected}),
    {produce, #{
        'p_start' => [],
        'p_choice' => [Selected],
        'p_selected' => [Selected]
    }, NewState};

fire('t_finish', #{'p_selected' := [Selected]}, State) ->
    %% Complete the exclusive choice
    Elapsed = erlang:system_time(millisecond) - State#exclusive_choice_state.start_time,
    log_event(State, <<"ExclusiveChoice">>, <<"Complete">>, #{
        <<"selected">> => Selected,
        <<"duration_ms">> => Elapsed
    }),
    {produce, #{
        'p_selected' => [],
        'p_end' => [complete, Selected]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: exclusive_choice_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: exclusive_choice_state()) ->
          {ok, exclusive_choice_state()}.

init(ExclusiveChoiceState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"ExclusiveChoice">>}) of
        {ok, LogId} ->
            State1 = ExclusiveChoiceState#exclusive_choice_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, ExclusiveChoiceState}
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_pnet:get_usr_info(NetState),
    {reply, {ok, UsrInfo}, NetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_cast({select_branch, BranchId}, NetState) ->
    UsrInfo = gen_pnet:get_usr_info(NetState),
    case UsrInfo of
        #exclusive_choice_state{selected = undefined, branches = Branches} = State ->
            case maps:is_key(BranchId, Branches) of
                true ->
                    NewState = State#exclusive_choice_state{selected = BranchId},
                    NewUsrInfo = gen_pnet:set_usr_info(NetState, NewState),
                    {noreply, NewUsrInfo};
                false ->
                    {noreply, NetState}
            end;
        _ ->
            {noreply, NetState}
    end;
handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles non-gen_pnet messages.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_info(_Request, NetState) ->
    {noreply, NetState}.

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
    UsrInfo = gen_pnet:get_usr_info(NetState),
    case UsrInfo of
        #exclusive_choice_state{log_id = LogId} when LogId =/= undefined ->
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
    Pid ! {trigger, 'p_end', Ref},
    receive
        {trigger, 'p_end', Ref, pass} ->
            case gen_pnet:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_pnet:get_usr_info(Pid),
                    case UsrInfo of
                        #exclusive_choice_state{selected = Selected} when Selected =/= undefined ->
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
%% @doc Generates a unique log ID.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().

generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"exclusive_choice_", Hex/binary>>.

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
-spec log_event(State :: exclusive_choice_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#exclusive_choice_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.
