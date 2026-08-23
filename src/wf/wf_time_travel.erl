%% -*- erlang -*-
%%%% @doc wf_time_travel - Time-travel debugger for workflow debugging.
%%
%% This module provides time-travel debugging capabilities for workflows,
%% allowing developers to:
%% <ul>
%%   <li><b>Record state transitions:</b> Track every state change</li>
%%   <li><b>Replay execution:</b> Step through workflow history</li>
%%   <li><b>Inspect state at any point:</b> View tokens, marking, and mode</li>
%%   <li><b>Set breakpoints:</b> Pause execution at specific points</li>
%%   <li><b>Reverse execution:</b> Step backwards through transitions</li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% <pre>
%% %% Start debugging a workflow
%% wf_time_travel:start_session(CaseId, NetMod).
%%
%% %% Replay from a specific point
%% wf_time_travel:replay_to(CaseId, Timestamp).
%%
%% %% Get state at a point in time
%% wf_time_travel:get_state_at(CaseId, Timestamp).
%% </pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_time_travel).
-author("CRE Team").

-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Session management
-export([start_link/0]).
-export([start_link/1]).
-export([start_session/2]).
-export([start_session/3]).
-export([stop_session/1]).
-export([get_active_sessions/0]).

%% Recording
-export([record_transition/5]).
-export([record_marking/3]).
-export([record_mode_change/4]).
-export([record_token_event/5]).

%% Replay
-export([replay_from_start/1]).
-export([replay_to/2]).
-export([step_forward/1]).
-export([step_backward/1]).
-export([jump_to/2]).

%% Inspection
-export([get_state_at/2]).
-export([get_timeline/1]).
-export([get_token_history/2]).
-export([get_transition_history/1]).

%% Breakpoints
-export([set_breakpoint/2]).
-export([clear_breakpoint/2]).
-export([list_breakpoints/1]).
-export([check_breakpoints/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(session, {
    session_id :: binary(),
    case_id :: binary(),
    net_mod :: atom(),
    start_time :: integer(),
    current_index = 0 :: non_neg_integer(),
    status :: recording | replaying | paused
}).

-record(event, {
    timestamp :: integer(),
    event_type :: transition | marking | mode_change | token_event,
    data :: map()
}).

-record(timeline, {
    session_id :: binary(),
    events = [] :: [#event{}],
    breakpoints = [] :: [breakpoint_spec()]
}).

-record(state_snapshot, {
    timestamp :: integer(),
    marking :: map(),
    mode :: term(),
    metadata :: map()
}).

-type breakpoint_spec() :: {transition, atom()} | {place, atom()} | {condition, fun()}.
-type session_id() :: binary().
-type case_id() :: binary().

-export_type([session_id/0, case_id/0, breakpoint_spec/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the time-travel debugger service.
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    start_link(#{}).

%% @doc Starts the service with options.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.

start_link(Options) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Options, []).

%% @doc Starts a new debugging session for a case.
-spec start_session(case_id(), atom()) -> {ok, session_id()} | {error, term()}.

start_session(CaseId, NetMod) ->
    start_session(CaseId, NetMod, #{}).

%% @doc Starts a session with options.
-spec start_session(case_id(), atom(), map()) -> {ok, session_id()} | {error, term()}.

start_session(CaseId, NetMod, Options) ->
    gen_server:call(?MODULE, {start_session, CaseId, NetMod, Options}).

%% @doc Stops a debugging session.
-spec stop_session(session_id()) -> ok.

stop_session(SessionId) ->
    gen_server:cast(?MODULE, {stop_session, SessionId}).

%% @doc Gets all active debugging sessions.
-spec get_active_sessions() -> [session_id()].

get_active_sessions() ->
    gen_server:call(?MODULE, get_active_sessions).

%% @doc Records a transition firing event.
-spec record_transition(session_id(), atom(), map(), term(), term()) -> ok.

record_transition(SessionId, Transition, Marking, Mode, UsrInfo) ->
    gen_server:cast(?MODULE, {record_event, SessionId, #event{
        timestamp = erlang:system_time(microsecond),
        event_type = transition,
        data = #{
            transition => Transition,
            marking => Marking,
            mode => Mode,
            usr_info => UsrInfo
        }
    }}).

%% @doc Records a marking change event.
-spec record_marking(session_id(), map(), map()) -> ok.

record_marking(SessionId, OldMarking, NewMarking) ->
    gen_server:cast(?MODULE, {record_event, SessionId, #event{
        timestamp = erlang:system_time(microsecond),
        event_type = marking,
        data = #{
            old_marking => OldMarking,
            new_marking => NewMarking
        }
    }}).

%% @doc Records a mode change event.
-spec record_mode_change(session_id(), atom(), atom(), term()) -> ok.

record_mode_change(SessionId, NetMod, OldMode, NewMode) ->
    gen_server:cast(?MODULE, {record_event, SessionId, #event{
        timestamp = erlang:system_time(microsecond),
        event_type = mode_change,
        data = #{
            net_mod => NetMod,
            old_mode => OldMode,
            new_mode => NewMode
        }
    }}).

%% @doc Records a token event (creation, consumption, movement).
-spec record_token_event(session_id(), atom(), term(), atom(), atom()) -> ok.

record_token_event(SessionId, Place, Token, EventType, TargetPlace) ->
    gen_server:cast(?MODULE, {record_event, SessionId, #event{
        timestamp = erlang:system_time(microsecond),
        event_type = token_event,
        data = #{
            place => Place,
            token => Token,
            event_type => EventType,
            target => TargetPlace
        }
    }}).

%% @doc Replays a session from the beginning.
-spec replay_from_start(session_id()) -> {ok, [term()]} | {error, term()}.

replay_from_start(SessionId) ->
    gen_server:call(?MODULE, {replay_from_start, SessionId}).

%% @doc Replays a session up to a specific timestamp.
-spec replay_to(session_id(), integer()) -> {ok, [term()]} | {error, term()}.

replay_to(SessionId, Timestamp) ->
    gen_server:call(?MODULE, {replay_to, SessionId, Timestamp}).

%% @doc Steps forward one event in the replay.
-spec step_forward(session_id()) -> {ok, term()} | {error, term()}.

step_forward(SessionId) ->
    gen_server:call(?MODULE, {step_forward, SessionId}).

%% @doc Steps backward one event in the replay.
-spec step_backward(session_id()) -> {ok, term()} | {error, term()}.

step_backward(SessionId) ->
    gen_server:call(?MODULE, {step_backward, SessionId}).

%% @doc Jumps to a specific event index.
-spec jump_to(session_id(), non_neg_integer()) -> {ok, term()} | {error, term()}.

jump_to(SessionId, Index) ->
    gen_server:call(?MODULE, {jump_to, SessionId, Index}).

%% @doc Gets the workflow state at a specific timestamp.
-spec get_state_at(session_id(), integer()) -> {ok, #state_snapshot{}} | {error, term()}.

get_state_at(SessionId, Timestamp) ->
    gen_server:call(?MODULE, {get_state_at, SessionId, Timestamp}).

%% @doc Gets the timeline of events for a session.
-spec get_timeline(session_id()) -> {ok, [#event{}]} | {error, term()}.

get_timeline(SessionId) ->
    gen_server:call(?MODULE, {get_timeline, SessionId}).

%% @doc Gets the history of a specific token.
-spec get_token_history(session_id(), term()) -> {ok, [#event{}]} | {error, term()}.

get_token_history(SessionId, Token) ->
    gen_server:call(?MODULE, {get_token_history, SessionId, Token}).

%% @doc Gets the transition history for a session.
-spec get_transition_history(session_id()) -> {ok, [#event{}]} | {error, term()}.

get_transition_history(SessionId) ->
    gen_server:call(?MODULE, {get_transition_history, SessionId}).

%% @doc Sets a breakpoint.
-spec set_breakpoint(session_id(), breakpoint_spec()) -> ok.

set_breakpoint(SessionId, Breakpoint) ->
    gen_server:cast(?MODULE, {set_breakpoint, SessionId, Breakpoint}).

%% @doc Clears a breakpoint.
-spec clear_breakpoint(session_id(), breakpoint_spec()) -> ok.

clear_breakpoint(SessionId, Breakpoint) ->
    gen_server:cast(?MODULE, {clear_breakpoint, SessionId, Breakpoint}).

%% @doc Lists all breakpoints for a session.
-spec list_breakpoints(session_id()) -> {ok, [breakpoint_spec()]}.

list_breakpoints(SessionId) ->
    gen_server:call(?MODULE, {list_breakpoints, SessionId}).

%% @doc Checks if any breakpoints are triggered.
-spec check_breakpoints(session_id(), #event{}) -> boolean().

check_breakpoints(SessionId, Event) ->
    gen_server:call(?MODULE, {check_breakpoints, SessionId, Event}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init(Options) ->
    MaxSessions = maps:get(max_sessions, Options, 100),
    State = #{
        sessions => #{},
        timelines => #{},
        max_sessions => MaxSessions
    },
    {ok, State}.

%% @private
handle_call({start_session, CaseId, NetMod, Options}, _From, State) ->
    SessionId = generate_session_id(),
    Session = #session{
        session_id = SessionId,
        case_id = CaseId,
        net_mod = NetMod,
        start_time = erlang:system_time(microsecond),
        status = recording
    },
    Timeline = #timeline{
        session_id = SessionId,
        breakpoints = maps:get(breakpoints, Options, [])
    },
    CurrentSessions = maps:get(sessions, State, #{}),
    CurrentTimelines = maps:get(timelines, State, #{}),
    Sessions = maps:put(SessionId, Session, CurrentSessions),
    Timelines = maps:put(SessionId, Timeline, CurrentTimelines),
    {reply, {ok, SessionId}, State#{sessions => Sessions, timelines => Timelines}};

handle_call(get_active_sessions, _From, State) ->
    Sessions = maps:get(sessions, State, #{}),
    ActiveIds = [SessionId || #session{session_id = SessionId, status = recording} <- maps:values(Sessions)],
    {reply, ActiveIds, State};

handle_call({replay_from_start, SessionId}, _From, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{events = Events} ->
            {reply, {ok, Events}, State};
        undefined ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({replay_to, SessionId, Timestamp}, _From, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{events = Events} ->
            Filtered = [E || #event{timestamp = TS} = E <- Events, TS =< Timestamp],
            {reply, {ok, Filtered}, State};
        undefined ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({step_forward, SessionId}, _From, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        #session{current_index = Idx} = Session ->
            Timelines = maps:get(timelines, State, #{}),
            case maps:get(SessionId, Timelines, undefined) of
                #timeline{events = Events} when Idx < length(Events) ->
                    Event = lists:nth(Idx + 1, Events),
                    NewSession = Session#session{current_index = Idx + 1, status = replaying},
                    NewSessions = maps:put(SessionId, NewSession, Sessions),
                    {reply, {ok, Event}, State#{sessions => NewSessions}};
                _ ->
                    {reply, {error, end_of_timeline}, State}
            end;
        undefined ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({step_backward, SessionId}, _From, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        #session{current_index = Idx} = Session when Idx > 0 ->
            Timelines = maps:get(timelines, State, #{}),
            case maps:get(SessionId, Timelines, undefined) of
                #timeline{events = Events} when Idx > 0, Idx =< length(Events) ->
                    Event = lists:nth(Idx, Events),
                    NewSession = Session#session{current_index = Idx - 1, status = replaying},
                    NewSessions = maps:put(SessionId, NewSession, Sessions),
                    {reply, {ok, Event}, State#{sessions => NewSessions}};
                _ ->
                    {reply, {ok, at_start}, State}
            end;
        _ ->
            {reply, {error, session_not_found_or_at_start}, State}
    end;

handle_call({jump_to, SessionId, Index}, _From, State) ->
    Sessions = maps:get(sessions, State, #{}),
    case maps:get(SessionId, Sessions, undefined) of
        #session{} = Session ->
            Timelines = maps:get(timelines, State, #{}),
            case maps:get(SessionId, Timelines, undefined) of
                #timeline{events = Events} when Index >= 0, Index < length(Events) ->
                    NewSession = Session#session{current_index = Index, status = replaying},
                    NewSessions = maps:put(SessionId, NewSession, Sessions),
                    Event = if
                        Index == 0 -> at_start;
                        Index > 0 -> lists:nth(Index, Events)
                    end,
                    {reply, {ok, Event}, State#{sessions => NewSessions}};
                _ ->
                    {reply, {error, index_out_of_bounds}, State}
            end;
        undefined ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({get_state_at, SessionId, Timestamp}, _From, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{events = Events} ->
            %% Find the state snapshot at or before the timestamp
            case find_state_at(Events, Timestamp, undefined) of
                {ok, Snapshot} ->
                    {reply, {ok, Snapshot}, State};
                error ->
                    {reply, {error, no_state_at_timestamp}, State}
            end;
        undefined ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({get_timeline, SessionId}, _From, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{events = Events} ->
            {reply, {ok, Events}, State};
        undefined ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({get_token_history, SessionId, Token}, _From, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{events = Events} ->
            Filtered = [E || #event{event_type = token_event, data = Data} = E <- Events,
                           maps:get(token, Data, undefined) =:= Token],
            {reply, {ok, Filtered}, State};
        undefined ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({get_transition_history, SessionId}, _From, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{events = Events} ->
            Filtered = [E || #event{event_type = transition} = E <- Events],
            {reply, {ok, Filtered}, State};
        undefined ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({list_breakpoints, SessionId}, _From, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{breakpoints = BPs} ->
            {reply, {ok, BPs}, State};
        undefined ->
            {reply, {error, session_not_found}, State}
    end;

handle_call({check_breakpoints, SessionId, Event}, _From, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{breakpoints = BPs} ->
            Triggered = check_breakpoints_internal(BPs, Event),
            {reply, Triggered, State};
        undefined ->
            {reply, false, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast({record_event, SessionId, Event}, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{events = Events} = Timeline ->
            NewTimeline = Timeline#timeline{events = Events ++ [Event]},
            NewTimelines = maps:put(SessionId, NewTimeline, Timelines),
            {noreply, State#{timelines => NewTimelines}};
        undefined ->
            %% Create a new timeline for this session if it doesn't exist
            NewTimeline = #timeline{session_id = SessionId, events = [Event]},
            NewTimelines = maps:put(SessionId, NewTimeline, Timelines),
            {noreply, State#{timelines => NewTimelines}}
    end;

handle_cast({stop_session, SessionId}, State) ->
    Sessions = maps:remove(SessionId, maps:get(sessions, State)),
    Timelines = maps:remove(SessionId, maps:get(timelines, State)),
    {noreply, State#{sessions => Sessions, timelines => Timelines}};

handle_cast({set_breakpoint, SessionId, Breakpoint}, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{breakpoints = BPs} = Timeline ->
            NewTimeline = Timeline#timeline{breakpoints = [Breakpoint | BPs]},
            NewTimelines = maps:put(SessionId, NewTimeline, Timelines),
            {noreply, State#{timelines => NewTimelines}};
        undefined ->
            {noreply, State}
    end;

handle_cast({clear_breakpoint, SessionId, Breakpoint}, State) ->
    Timelines = maps:get(timelines, State, #{}),
    case maps:get(SessionId, Timelines, undefined) of
        #timeline{breakpoints = BPs} = Timeline ->
            NewTimeline = Timeline#timeline{breakpoints = lists:delete(Breakpoint, BPs)},
            NewTimelines = maps:put(SessionId, NewTimeline, Timelines),
            {noreply, State#{timelines => NewTimelines}};
        undefined ->
            {noreply, State}
    end;

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
generate_session_id() ->
    Time = erlang:system_time(microsecond),
    Random = rand:uniform(16#ffff),
    <<Time:48, Random:16>>.

%% @private
find_state_at([], _Timestamp, Acc) ->
    case Acc of
        undefined -> error;
        _ -> {ok, Acc}
    end;
find_state_at([#event{timestamp = TS} | Rest], Timestamp, _Acc) when TS > Timestamp ->
    case Rest of
        [] -> error;
        [#event{data = NextData} | _] -> {ok, NextData}
    end;
find_state_at([#event{event_type = transition, data = Data} | Rest], Timestamp, _Acc) ->
    %% Transition events contain the full state
    Snapshot = #state_snapshot{
        timestamp = Timestamp,
        marking = maps:get(marking, Data, #{}),
        mode = maps:get(mode, Data, undefined),
        metadata = Data
    },
    find_state_at(Rest, Timestamp, Snapshot);
find_state_at([_Event | Rest], Timestamp, Acc) ->
    find_state_at(Rest, Timestamp, Acc).

%% @private
check_breakpoints_internal(Breakpoints, #event{event_type = EventType, data = Data}) ->
    lists:any(fun(BP) -> matches_breakpoint(BP, EventType, Data) end, Breakpoints).

%% @private
matches_breakpoint({transition, Trsn}, transition, Data) ->
    maps:get(transition, Data, undefined) =:= Trsn;
matches_breakpoint({place, Place}, token_event, Data) ->
    maps:get(place, Data, undefined) =:= Place orelse
    maps:get(target, Data, undefined) =:= Place;
matches_breakpoint({condition, Fun}, _EventType, Data) ->
    try
        Fun(Data)
    catch
        _:_ -> false
    end;
matches_breakpoint(_BP, _EventType, _Data) ->
    false.
