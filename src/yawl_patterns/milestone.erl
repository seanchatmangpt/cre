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
%% @doc Milestone Pattern (WCP-18) for YAWL
%%
%% This module implements the Milestone pattern as a gen_pnet behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Milestone pattern (WCP-18) enables an activity only when a specific
%% milestone state has been reached. The milestone acts as a guard condition
%% that must be satisfied before the activity can execute.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start           - Start of the workflow
%%     p_milestone_guard - Milestone guard check
%%     p_milestone_ready - Milestone is ready (not yet reached)
%%     p_milestone_reached - Milestone has been reached
%%     p_activity_pending - Activity waiting for milestone
%%     p_activity_active  - Activity is executing
%%     p_activity_done    - Activity completed
%%     p_complete         - Workflow complete
%%
%%   Transitions:
%%     t_start          - Start the workflow
%%     t_check_milestone - Check if milestone is reached
%%     t_reach_milestone - Mark milestone as reached
%%     t_enable_activity - Enable activity when milestone reached
%%     t_execute        - Execute the activity
%%     t_complete       - Complete the workflow
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always true (milestone eventually reached)</li>
%%   <li><b>Proper completion:</b> Activity executes only after milestone</li>
%%   <li><b>No dead transitions:</b> All transitions fire when conditions met</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(milestone).
-behaviour(gen_yawl).

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
    start/2,
    run/2,
    get_state/1,
    execute/3,
    set_milestone/1
]).

%%====================================================================
%% Records
%%====================================================================

-record(milestone_state, {
    activity_fun :: function(),
    milestone_fun :: function(),  %% Fun(State) -> boolean()
    milestone_reached = false :: boolean(),
    activity_result :: undefined | term(),
    activity_executed = false :: boolean(),
    log_id :: binary() | undefined
}).

-type milestone_state() :: #milestone_state{}.
-export_type([milestone_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new Milestone pattern state.
%%
%% @param ActivityFun The function to execute after milestone is reached.
%% @param MilestoneFun Function that checks if milestone is reached.
%%                      Takes current state and returns boolean().
%% @return A new milestone_state record.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(ActivityFun :: function(), MilestoneFun :: function()) -> milestone_state().

new(ActivityFun, MilestoneFun) when is_function(ActivityFun), is_function(MilestoneFun) ->
    LogId = generate_log_id(),
    #milestone_state{
        activity_fun = ActivityFun,
        milestone_fun = MilestoneFun,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
%% @doc Starts the Milestone workflow as a gen_yawl process.
%%
%% @param ActivityFun The function to execute after milestone is reached.
%% @param MilestoneFun Function that checks if milestone is reached.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start(ActivityFun :: function(), MilestoneFun :: function()) ->
          {ok, pid()} | {error, term()}.

start(ActivityFun, MilestoneFun) when is_function(ActivityFun), is_function(MilestoneFun) ->
    MilestoneState = new(ActivityFun, MilestoneFun),
    gen_yawl:start_link(?MODULE, MilestoneState, []).

%%--------------------------------------------------------------------
%% @doc Runs the Milestone workflow synchronously.
%%
%% @param ActivityFun The function to execute after milestone is reached.
%% @param InitialState Initial state to pass to milestone check.
%% @return {ok, Result} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec run(ActivityFun :: function(), InitialState :: term()) ->
          {ok, term()} | {error, term()}.

run(ActivityFun, InitialState) when is_function(ActivityFun) ->
    %% Create a milestone function that checks state
    MilestoneFun = fun(State) ->
        case State of
            reached -> true;
            _ -> false
        end
    end,
    case start(ActivityFun, MilestoneFun) of
        {ok, Pid} ->
            %% Inject initial state
            gen_yawl:cast(Pid, {initial_state, InitialState}),
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
%% @doc Gets the current state of the Milestone workflow.
%%
%% @param Pid The pid of the gen_yawl process.
%% @return {ok, State} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(Pid :: pid()) -> {ok, milestone_state()} | {error, term()}.

get_state(Pid) ->
    gen_yawl:call(Pid, get_state).

%%--------------------------------------------------------------------
%% @doc Executes the Milestone pattern with given initial state.
%%
%% @param ActivityFun The function to execute after milestone is reached.
%% @param MilestoneFun Function that checks if milestone is reached.
%% @param InitialState Initial state to pass to milestone check.
%% @return {ok, Result} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(ActivityFun :: function(), MilestoneFun :: function(), InitialState :: term()) ->
          {ok, term()} | {error, term()}.

execute(ActivityFun, MilestoneFun, InitialState) when is_function(ActivityFun), is_function(MilestoneFun) ->
    Ref = make_ref(),
    Parent = self(),

    %% Spawn milestone checker
    CheckerPid = spawn(fun() ->
        check_milestone_loop(Ref, Parent, MilestoneFun, InitialState)
    end),

    %% Wait for milestone to be reached
    receive
        {Ref, milestone_reached} ->
            %% Execute activity
            try
                Result = ActivityFun(InitialState),
                {ok, Result}
            catch
                Error:Reason:Stack ->
                    {error, {Error, Reason, Stack}}
            end;
        {Ref, {milestone_error, Reason}} ->
            {error, {milestone_error, Reason}}
    after 30000 ->
        exit(CheckerPid, kill),
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Manually sets the milestone as reached.
%%
%% @param Pid The pid of the gen_yawl process.
%% @return ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec set_milestone(Pid :: pid()) -> ok | {error, term()}.

set_milestone(Pid) ->
    gen_yawl:cast(Pid, set_milestone_reached).

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Returns the list of places for the Milestone Petri net.
%% @end
%%--------------------------------------------------------------------
-spec place_lst() -> [atom()].

place_lst() ->
    [
        'p_start',
        'p_milestone_guard',
        'p_milestone_ready',
        'p_milestone_reached',
        'p_activity_pending',
        'p_activity_active',
        'p_activity_done',
        'p_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the list of transitions for the Milestone Petri net.
%% @end
%%--------------------------------------------------------------------
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        't_start',
        't_check_milestone',
        't_reach_milestone',
        't_enable_activity',
        't_execute',
        't_complete'
    ].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: milestone_state()) ->
          [term()].

init_marking('p_start', _UsrInfo) ->
    [start];
init_marking('p_milestone_ready', _UsrInfo) ->
    [check];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
%% @doc Returns the preset (input places) for each transition.
%% @end
%%--------------------------------------------------------------------
-spec preset(Trsn :: atom()) -> [atom()].

preset('t_start') -> ['p_start'];
preset('t_check_milestone') -> ['p_milestone_guard'];
preset('t_reach_milestone') -> ['p_milestone_ready'];
preset('t_enable_activity') -> ['p_milestone_reached'];
preset('t_execute') -> ['p_activity_pending'];
preset('t_complete') -> ['p_activity_done'];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: milestone_state()) ->
          boolean().

is_enabled('t_start', _Mode, _UsrInfo) ->
    true;
is_enabled('t_check_milestone', #{'p_milestone_guard' := [check]}, _UsrInfo) ->
    true;
is_enabled('t_reach_milestone', #{'p_milestone_ready' := [check]}, _UsrInfo) ->
    true;
is_enabled('t_enable_activity', #{'p_milestone_reached' := [reached]}, _UsrInfo) ->
    true;
is_enabled('t_execute', #{'p_activity_pending' := [_]}, #milestone_state{milestone_reached = true}) ->
    true;
is_enabled('t_complete', #{'p_activity_done' := [_]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: milestone_state()) ->
          {produce, map()} | {produce, map(), milestone_state()} | abort.

fire('t_start', #{'p_start' := [start]}, State) ->
    log_event(State, <<"Milestone">>, <<"Start">>, #{}),
    {produce, #{
        'p_start' => [],
        'p_milestone_guard' => [check]
    }, State};

fire('t_check_milestone', #{'p_milestone_guard' := [check]}, #milestone_state{milestone_fun = MilestoneFun} = State) ->
    %% Check milestone function
    Reached = try
        MilestoneFun(check)
    catch
        _:_ -> false
    end,
    log_event(State, <<"Milestone">>, <<"Checked">>, #{<<"reached">> => Reached}),
    case Reached of
        true ->
            {produce, #{
                'p_milestone_guard' => [],
                'p_milestone_reached' => [reached]
            }, State#milestone_state{milestone_reached = true}};
        false ->
            {produce, #{
                'p_milestone_guard' => [],
                'p_milestone_ready' => [not_reached]
            }, State}
    end;

fire('t_reach_milestone', #{'p_milestone_ready' := [not_reached]}, State) ->
    %% Loop back to check again
    {produce, #{
        'p_milestone_ready' => [],
        'p_milestone_guard' => [check]
    }, State};

fire('t_enable_activity', #{'p_milestone_reached' := [reached]}, State) ->
    log_event(State, <<"Milestone">>, <<"ActivityEnabled">>, #{}),
    {produce, #{
        'p_milestone_reached' => [],
        'p_activity_pending' => [enabled]
    }, State};

fire('t_execute', #{'p_activity_pending' := [enabled]}, #milestone_state{activity_fun = ActivityFun} = State) ->
    %% Execute the activity
    Result = try ActivityFun() catch _:_ -> {error, activity_failed} end,
    NewState = State#milestone_state{
        activity_result = Result,
        activity_executed = true
    },
    log_event(State, <<"Milestone">>, <<"ActivityExecuted">>, #{<<"result">> => Result}),
    {produce, #{
        'p_activity_pending' => [],
        'p_activity_done' => [Result]
    }, NewState};

fire('t_complete', #{'p_activity_done' := [Result]}, State) ->
    log_event(State, <<"Milestone">>, <<"Complete">>, #{<<"result">> => Result}),
    {produce, #{
        'p_activity_done' => [],
        'p_complete' => [Result]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), UsrInfo :: milestone_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: milestone_state()) ->
          {ok, milestone_state()}.

init(MilestoneState) ->
    case yawl_xes:new_log(#{<<"process">> => <<"Milestone">>}) of
        {ok, LogId} ->
            State1 = MilestoneState#milestone_state{log_id = LogId},
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, State1};
        _ ->
            {ok, MilestoneState}
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(get_state, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    {reply, {ok, UsrInfo}, NetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_cast(set_milestone_reached, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #milestone_state{} = State ->
            NewState = State#milestone_state{milestone_reached = true},
            NewUsrInfo = gen_yawl:set_usr_info(NetState, NewState),
            {noreply, NewUsrInfo};
        _ ->
            {noreply, NetState}
    end;
handle_cast({initial_state, _State}, NetState) ->
    {noreply, NetState};
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
    UsrInfo = gen_yawl:get_usr_info(NetState),
    case UsrInfo of
        #milestone_state{log_id = LogId} when LogId =/= undefined ->
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
          {ok, term()} | {error, term()}.

wait_for_completion(Pid, Timeout) ->
    Ref = make_ref(),
    Pid ! {trigger, 'p_complete', Ref},
    receive
        {trigger, 'p_complete', Ref, pass} ->
            case gen_yawl:sync(Pid, 1000) of
                {ok, _} ->
                    UsrInfo = gen_yawl:get_usr_info(Pid),
                    case UsrInfo of
                        #milestone_state{activity_result = Result} ->
                            {ok, Result};
                        _ ->
                            {error, no_result}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end
    after Timeout ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Loops to check if milestone is reached.
%% @private
%% @end
%%--------------------------------------------------------------------
-spec check_milestone_loop(Ref :: reference(), Parent :: pid(),
                           MilestoneFun :: function(), State :: term()) ->
          no_return().

check_milestone_loop(Ref, Parent, MilestoneFun, State) ->
    case MilestoneFun(State) of
        true ->
            Parent ! {Ref, milestone_reached};
        {true, NewState} ->
            Parent ! {Ref, milestone_reached, NewState};
        false ->
            timer:sleep(100),
            check_milestone_loop(Ref, Parent, MilestoneFun, State);
        {false, NewState} ->
            timer:sleep(100),
            check_milestone_loop(Ref, Parent, MilestoneFun, NewState);
        {error, Reason} ->
            Parent ! {Ref, {milestone_error, Reason}}
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
    <<"milestone_", Hex/binary>>.

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
-spec log_event(State :: milestone_state(),
                Concept :: binary(),
                Lifecycle :: binary(),
                Data :: map()) ->
          ok.

log_event(#milestone_state{log_id = LogId}, Concept, Lifecycle, Data) when LogId =/= undefined ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data);
log_event(_State, _Concept, _Lifecycle, _Data) ->
    ok.
