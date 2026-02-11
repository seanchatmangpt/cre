%% -*- erlang -*-
%% @doc Explicit Termination Pattern (WCP-43) for YAWL.
%%
%% This module implements Explicit Termination pattern (WCP-43) as a gen_yawl behaviour.
%%
%% <h3>Pattern Description</h3>
%% The Explicit Termination pattern provides a hard-stop mechanism that cancels
%% all active activities when triggered. Unlike implicit termination which
%% waits for natural completion, this immediately terminates workflow.
%%
%% <h3>Petri Net Structure</h3>
%% <pre>
%%   Places:
%%     p_start         - Initial place, workflow starts
%%     p_active        - Workflow is actively executing
%%     p_terminate_event - Termination signal received
%%     p_terminated    - Termination in progress
%%     p_cancelled     - All activities cancelled
%%     p_end           - Final place, workflow complete
%%
%%   Transitions:
%%     t_start         - Start the workflow
%%     t_terminate      - Trigger explicit termination
%%     t_cancel_all    - Cancel all active activities
%%     t_finish        - Complete the workflow
%% </pre>
%%
%% <h3>Soundness Properties</h3>
%% <ul>
%%   <li><b>Option to complete:</b> Always reachable via explicit termination</li>
%%   <li><b>Proper completion:</b> All activities cancelled on termination</li>
%%   <li><b>No dead transitions:</b> All transitions fireable when conditions met</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(explicit_termination).
-moduledoc """
Explicit Termination Pattern (WCP-43) for YAWL.

This pattern provides a hard-stop mechanism that cancels all active
activities when triggered.

## Example

```erlang
%% Create an explicit termination pattern
State = explicit_termination:new(),
{ok, Pid} = gen_yawl:start_link(explicit_termination, State, []).

%% Trigger explicit termination
ok = gen_yawl:cast(Pid, terminate).
```

## API

- `new/0` - Create new explicit termination state
- `new/2` - Create with custom terminator and cancel mode
- `is_terminated/1` - Check if termination occurred
- `get_terminator/1` - Get the terminator source
- `cancels_all/1` - Check if all activities are cancelled
- `terminate/1` - Set termination state
- `reset/1` - Reset termination state

""".
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

%% gen_yawl callbacks
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
    new/0,
    new/2,
    is_terminated/1,
    get_terminator/1,
    cancels_all/1,
    terminate/1,
    reset/1
]).

%%====================================================================
%% Records
%%====================================================================

-record(termination_state, {
    terminator :: atom() | undefined,
    cancels_all = true :: boolean(),
    terminated = false :: boolean(),
    log_id :: binary() | undefined
}).

-type termination_state() :: #termination_state{}.
-export_type([termination_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Creates a new Explicit Termination pattern state.

## Example

```erlang
> State = explicit_termination:new().
#termination_state{cancels_all = true, terminated = false, ...}
```
""".
-spec new() -> termination_state().
new() ->
    LogId = generate_log_id(),
    #termination_state{
        terminator = undefined,
        cancels_all = true,
        terminated = false,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Creates a new Explicit Termination pattern state with custom settings.

## Example

```erlang
> State = explicit_termination:new(emergency_stop, true).
#termination_state{terminator = emergency_stop, cancels_all = true, ...}
```
""".
-spec new(Terminator :: atom() | undefined, CancelsAll :: boolean()) -> termination_state().
new(Terminator, CancelsAll) when is_boolean(CancelsAll) ->
    LogId = generate_log_id(),
    #termination_state{
        terminator = Terminator,
        cancels_all = CancelsAll,
        terminated = false,
        log_id = LogId
    }.

%%--------------------------------------------------------------------
-doc """
Checks if termination has occurred.

## Example

```erlang
> State0 = explicit_termination:new(),
> explicit_termination:is_terminated(State0).
false
> State1 = State0#termination_state{terminated = true},
> explicit_termination:is_terminated(State1).
true
```
""".
-spec is_terminated(State :: termination_state()) -> boolean().
is_terminated(#termination_state{terminated = Terminated}) ->
    Terminated.

%%--------------------------------------------------------------------
-doc """
Gets the terminator source.

## Example

```erlang
> State = explicit_termination:new(emergency_stop, true),
> explicit_termination:get_terminator(State).
emergency_stop
```
""".
-spec get_terminator(State :: termination_state()) -> atom() | undefined.
get_terminator(#termination_state{terminator = Terminator}) ->
    Terminator.

%%--------------------------------------------------------------------
-doc """
Checks if all activities are cancelled on termination.

## Example

```erlang
> State = explicit_termination:new(),
> explicit_termination:cancels_all(State).
true
```
""".
-spec cancels_all(State :: termination_state()) -> boolean().
cancels_all(#termination_state{cancels_all = CancelsAll}) ->
    CancelsAll.

%%--------------------------------------------------------------------
-doc """
Sets the termination state to true.

## Example

```erlang
> State0 = explicit_termination:new(),
> State1 = explicit_termination:terminate(State0).
#termination_state{terminated = true, ...}
```
""".
-spec terminate(State :: termination_state()) -> termination_state().
terminate(#termination_state{} = State) ->
    State#termination_state{terminated = true}.

%%--------------------------------------------------------------------
-doc """
Resets the termination state.

## Example

```erlang
> State0 = #termination_state{terminated = true},
> State1 = explicit_termination:reset(State0).
#termination_state{terminated = false, ...}
```
""".
-spec reset(State :: termination_state()) -> termination_state().
reset(State) ->
    State#termination_state{terminated = false}.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
-doc """
Returns the list of places for the Explicit Termination Petri net.

```erlang
> explicit_termination:place_lst().
[p_start, p_active, p_terminate_event, p_terminated, p_cancelled, p_end]
```
""".
-spec place_lst() -> [atom()].

place_lst() ->
    [p_start, p_active, p_terminate_event, p_terminated, p_cancelled, p_end].

%%--------------------------------------------------------------------
-doc """
Returns the list of transitions for the Explicit Termination Petri net.

```erlang
> explicit_termination:trsn_lst().
[t_start, t_terminate, t_cancel_all, t_finish]
```
""".
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [t_start, t_terminate, t_cancel_all, t_finish].

%%--------------------------------------------------------------------
%% @doc Returns the initial marking for a given place.
%% @end
%%--------------------------------------------------------------------
-spec init_marking(Place :: atom(), UsrInfo :: termination_state()) ->
          [term()].

init_marking(p_start, _UsrInfo) ->
    [start];
init_marking(_, _UsrInfo) ->
    [].

%%--------------------------------------------------------------------
-doc """
Returns the preset (input places) for each transition.

```erlang
> explicit_termination:preset(t_start).
[p_start]
> explicit_termination:preset(t_terminate).
[p_active, p_terminate_event]
```
""".
-spec preset(Trsn :: atom()) -> [atom()].

preset(t_start) -> [p_start];
preset(t_terminate) -> [p_active, p_terminate_event];
preset(t_cancel_all) -> [p_terminated];
preset(t_finish) -> [p_cancelled];
preset(_) -> [].

%%--------------------------------------------------------------------
%% @doc Checks if a transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(Trsn :: atom(), Mode :: map(), UsrInfo :: termination_state()) ->
          boolean().

is_enabled(t_start, _Mode, _UsrInfo) ->
    true;
is_enabled(t_terminate, #{p_active := [active], p_terminate_event := [_Event]}, _UsrInfo) ->
    true;
is_enabled(t_cancel_all, #{p_terminated := [terminated]}, _UsrInfo) ->
    true;
is_enabled(t_finish, #{p_cancelled := [cancelled]}, _UsrInfo) ->
    true;
is_enabled(_Trsn, _Mode, _UsrInfo) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fires a transition, consuming and producing tokens.
%% @end
%%--------------------------------------------------------------------
-spec fire(Trsn :: atom(), Mode :: map(), UsrInfo :: termination_state()) ->
          {produce, map()} | {produce, map(), termination_state()} | abort.

fire(t_start, #{p_start := [start]}, #termination_state{log_id = LogId} = State) ->
    log_event(LogId, <<"ExplicitTermination">>, <<"Start">>, #{}),
    {produce, #{
        p_start => [],
        p_active => [active]
    }, State};

fire(t_terminate, #{p_active := [active], p_terminate_event := [Event]}, #termination_state{terminator = Terminator, log_id = LogId} = State) ->
    log_event(LogId, <<"ExplicitTermination">>, <<"Terminate">>, #{
        <<"terminator">> => format_terminator(Terminator),
        <<"event">> => format_event(Event)
    }),
    NewState = State#termination_state{terminated = true},
    {produce, #{
        p_active => [],
        p_terminate_event => [],
        p_terminated => [terminated]
    }, NewState};

fire(t_cancel_all, #{p_terminated := [terminated]}, #termination_state{cancels_all = CancelsAll, log_id = LogId} = State) ->
    log_event(LogId, <<"ExplicitTermination">>, <<"CancelAll">>, #{
        <<"cancels_all">> => CancelsAll
    }),
    {produce, #{
        p_terminated => [],
        p_cancelled => [cancelled]
    }, State};

fire(t_finish, #{p_cancelled := [cancelled]}, #termination_state{log_id = LogId} = State) ->
    log_event(LogId, <<"ExplicitTermination">>, <<"Complete">>, #{
        <<"terminated">> => State#termination_state.terminated
    }),
    {produce, #{
        p_cancelled => [],
        p_end => [done]
    }, State};

fire(_Trsn, _Mode, _UsrInfo) ->
    abort.

%%--------------------------------------------------------------------
%% @doc Trigger callback for token-based processing.
%% @end
%%--------------------------------------------------------------------
-spec trigger(Place :: atom(), Token :: term(), NetState :: termination_state()) ->
          pass | {consume, [term()]}.

trigger(_Place, _Token, _UsrInfo) ->
    pass.

%%--------------------------------------------------------------------
%% @doc Initializes the gen_pnet.
%% @end
%%--------------------------------------------------------------------
-spec init(UsrInfo :: termination_state()) ->
          {ok, termination_state()}.

init(TermState) ->
    LogId = TermState#termination_state.log_id,
    case yawl_xes:new_log(#{<<"process">> => <<"ExplicitTermination">>}) of
        {ok, XesLogId} ->
            State1 = TermState#termination_state{log_id = XesLogId},
            yawl_xes:log_case_start(XesLogId, generate_case_id()),
            {ok, State1};
        _ when LogId =/= undefined ->
            yawl_xes:log_case_start(LogId, generate_case_id()),
            {ok, TermState};
        _ ->
            {ok, TermState}
    end.

%%--------------------------------------------------------------------
%% @doc Handles synchronous calls.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, NetState :: term()) ->
          {reply, term(), term()}.

handle_call(is_terminated, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Terminated = is_terminated(UsrInfo),
    {reply, {ok, Terminated}, NetState};
handle_call(get_terminator, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    Terminator = get_terminator(UsrInfo),
    {reply, {ok, Terminator}, NetState};
handle_call(cancels_all, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    CancelsAll = cancels_all(UsrInfo),
    {reply, {ok, CancelsAll}, NetState};
handle_call(reset, _From, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    NewUsrInfo = reset(UsrInfo),
    NewNetState = gen_yawl:set_usr_info(NetState, NewUsrInfo),
    {reply, ok, NewNetState};
handle_call(_Request, _From, NetState) ->
    {reply, {error, bad_msg}, NetState}.

%%--------------------------------------------------------------------
%% @doc Handles asynchronous casts.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: term()) ->
          {noreply, term()}.

handle_cast(terminate, NetState) ->
    UsrInfo = gen_yawl:get_usr_info(NetState),
    NewUsrInfo = terminate(UsrInfo),
    NewNetState = gen_yawl:set_usr_info(NetState, NewUsrInfo),
    {noreply, NewNetState};
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
        #termination_state{log_id = LogId} when LogId =/= undefined ->
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
%% @private
%% @doc Formats the terminator for XES logging.
%% @end
%%--------------------------------------------------------------------
-spec format_terminator(atom() | undefined) -> binary().
format_terminator(undefined) -> <<"undefined">>;
format_terminator(Atom) when is_atom(Atom) -> atom_to_binary(Atom).

%%--------------------------------------------------------------------
%% @private
%% @doc Formats an event for XES logging.
%% @end
%%--------------------------------------------------------------------
-spec format_event(term()) -> binary().
format_event(Event) when is_atom(Event) -> atom_to_binary(Event);
format_event(Event) when is_binary(Event) -> Event;
format_event(Event) -> term_to_binary(Event).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique log ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_log_id() -> binary().
generate_log_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"explicit_termination_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique case ID.
%% @end
%%--------------------------------------------------------------------
-spec generate_case_id() -> binary().
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Logs an XES event.
%% @end
%%--------------------------------------------------------------------
-spec log_event(LogId :: binary() | undefined, Concept :: binary(),
                Lifecycle :: binary(), Data :: map()) -> ok.
log_event(undefined, _Concept, _Lifecycle, _Data) ->
    ok;
log_event(LogId, Concept, Lifecycle, Data) ->
    yawl_xes:log_event(LogId, Concept, Lifecycle, Data).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% Test new/0
%%--------------------------------------------------------------------
new_test() ->
    State = new(),
    ?assert(is_record(State, termination_state)),
    ?assertEqual(undefined, State#termination_state.terminator),
    ?assertEqual(true, State#termination_state.cancels_all),
    ?assertEqual(false, State#termination_state.terminated).

%%--------------------------------------------------------------------
%% Test new/2
%%--------------------------------------------------------------------
new_custom_test() ->
    State = new(emergency_stop, true),
    ?assertEqual(emergency_stop, State#termination_state.terminator),
    ?assertEqual(true, State#termination_state.cancels_all).

%%--------------------------------------------------------------------
%% Test is_terminated/1
%%--------------------------------------------------------------------
is_terminated_test() ->
    State0 = #termination_state{terminated = false},
    ?assertNot(is_terminated(State0)),
    State1 = #termination_state{terminated = true},
    ?assert(is_terminated(State1)).

%%--------------------------------------------------------------------
%% Test get_terminator/1
%%--------------------------------------------------------------------
get_terminator_test() ->
    State = #termination_state{terminator = test_term},
    ?assertEqual(test_term, get_terminator(State)).

%%--------------------------------------------------------------------
%% Test cancels_all/1
%%--------------------------------------------------------------------
cancels_all_test() ->
    State = #termination_state{cancels_all = true},
    ?assert(cancels_all(State)).

%%--------------------------------------------------------------------
%% Test terminate/1
%%--------------------------------------------------------------------
terminate_test() ->
    State0 = #termination_state{terminated = false},
    State1 = terminate(State0),
    ?assertEqual(true, State1#termination_state.terminated).

%%--------------------------------------------------------------------
%% Test reset/1
%%--------------------------------------------------------------------
reset_test() ->
    State0 = #termination_state{terminated = true},
    State1 = reset(State0),
    ?assertEqual(false, State1#termination_state.terminated).

%%--------------------------------------------------------------------
%% Test place_lst/0
%%--------------------------------------------------------------------
place_lst_test() ->
    Places = place_lst(),
    ?assert(lists:member(p_start, Places)),
    ?assert(lists:member(p_active, Places)),
    ?assert(lists:member(p_terminated, Places)),
    ?assert(lists:member(p_cancelled, Places)),
    ?assert(lists:member(p_end, Places)).

%%--------------------------------------------------------------------
%% Test trsn_lst/0
%%--------------------------------------------------------------------
trsn_lst_test() ->
    Transitions = trsn_lst(),
    ?assert(lists:member(t_start, Transitions)),
    ?assert(lists:member(t_terminate, Transitions)),
    ?assert(lists:member(t_cancel_all, Transitions)),
    ?assert(lists:member(t_finish, Transitions)).

%%--------------------------------------------------------------------
%% Test preset/1
%%--------------------------------------------------------------------
preset_test() ->
    ?assertEqual([p_start], preset(t_start)),
    ?assertEqual([p_active, p_terminate_event], preset(t_terminate)),
    ?assertEqual([p_terminated], preset(t_cancel_all)),
    ?assertEqual([p_cancelled], preset(t_finish)).

%%--------------------------------------------------------------------
%% Test format_terminator/1
%%--------------------------------------------------------------------
format_terminator_test() ->
    ?assertEqual(<<"undefined">>, format_terminator(undefined)),
    ?assertEqual(<<"test">>, format_terminator(test)).

%%--------------------------------------------------------------------
%% Test format_event/1
%%--------------------------------------------------------------------
format_event_test() ->
    ?assertEqual(<<"test">>, format_event(test)),
    ?assertEqual(<<"binary">>, format_event(<<"binary">>)).

-endif.
