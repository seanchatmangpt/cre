-module(wf_tracer).
-behaviour(gen_server).

-export([start_link/0, start_link/1]).
-export([start_trace/1, start_trace/2]).
-export([stop_trace/1]).
-export([get_snapshot/1]).
-export([get_snapshots/1]).
-export([detect_deadlocks/0, detect_deadlocks/1]).
-export([replay/1, replay/2]).
-export([visualize/1]).
-export([clear_trace/1]).
-export([list_traces/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(trace_session, {
    pid :: pid(),
    module :: atom(),
    started :: erlang:timestamp(),
    events = [] :: [trace_event()],
    snapshots = [] :: [state_snapshot()],
    last_activity :: erlang:timestamp(),
    options = #{} :: map()
}).

-record(trace_event, {
    timestamp :: erlang:timestamp(),
    type :: transition | state_change | binding | firing | enabled,
    pid :: pid(),
    data :: term()
}).

-record(state_snapshot, {
    timestamp :: erlang:timestamp(),
    pid :: pid(),
    marking :: term(),
    bindings :: term(),
    metadata :: map()
}).

-record(state, {
    traces = #{} :: #{pid() => #trace_session{}},
    deadlock_threshold = 5000 :: non_neg_integer()
}).

-type trace_event() :: #trace_event{}.
-type state_snapshot() :: #state_snapshot{}.
-type trace_option() :: {snapshot_interval, pos_integer()} |
                        {capture_bindings, boolean()} |
                        {max_events, pos_integer()}.
-type trace_options() :: [trace_option()].

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

-spec start_link(list()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

-spec start_trace(pid()) -> ok | {error, term()}.
start_trace(Pid) ->
    start_trace(Pid, []).

-spec start_trace(pid(), trace_options()) -> ok | {error, term()}.
start_trace(Pid, Options) ->
    gen_server:call(?MODULE, {start_trace, Pid, Options}).

-spec stop_trace(pid()) -> ok | {error, term()}.
stop_trace(Pid) ->
    gen_server:call(?MODULE, {stop_trace, Pid}).

-spec get_snapshot(pid()) -> {ok, state_snapshot()} | {error, term()}.
get_snapshot(Pid) ->
    gen_server:call(?MODULE, {get_snapshot, Pid}).

-spec get_snapshots(pid()) -> {ok, [state_snapshot()]} | {error, term()}.
get_snapshots(Pid) ->
    gen_server:call(?MODULE, {get_snapshots, Pid}).

-spec detect_deadlocks() -> [pid()].
detect_deadlocks() ->
    gen_server:call(?MODULE, detect_deadlocks).

-spec detect_deadlocks(non_neg_integer()) -> [pid()].
detect_deadlocks(ThresholdMs) ->
    gen_server:call(?MODULE, {detect_deadlocks, ThresholdMs}).

-spec replay(pid()) -> {ok, [trace_event()]} | {error, term()}.
replay(Pid) ->
    replay(Pid, #{}).

-spec replay(pid(), map()) -> {ok, [trace_event()]} | {error, term()}.
replay(Pid, Options) ->
    gen_server:call(?MODULE, {replay, Pid, Options}).

-spec visualize(pid()) -> {ok, iolist()} | {error, term()}.
visualize(Pid) ->
    gen_server:call(?MODULE, {visualize, Pid}).

-spec clear_trace(pid()) -> ok | {error, term()}.
clear_trace(Pid) ->
    gen_server:call(?MODULE, {clear_trace, Pid}).

-spec list_traces() -> [{pid(), atom(), erlang:timestamp()}].
list_traces() ->
    gen_server:call(?MODULE, list_traces).

init(Opts) ->
    process_flag(trap_exit, true),
    Threshold = proplists:get_value(deadlock_threshold, Opts, 5000),
    {ok, #state{deadlock_threshold = Threshold}}.

handle_call({start_trace, Pid, Options}, _From, State = #state{traces = Traces}) ->
    case maps:is_key(Pid, Traces) of
        true ->
            {reply, {error, already_tracing}, State};
        false ->
            case setup_trace(Pid) of
                {ok, Module} ->
                    Session = #trace_session{
                        pid = Pid,
                        module = Module,
                        started = erlang:timestamp(),
                        last_activity = erlang:timestamp(),
                        options = maps:from_list(Options)
                    },
                    erlang:monitor(process, Pid),
                    NewTraces = maps:put(Pid, Session, Traces),
                    {reply, ok, State#state{traces = NewTraces}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({stop_trace, Pid}, _From, State = #state{traces = Traces}) ->
    case maps:get(Pid, Traces, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        _Session ->
            teardown_trace(Pid),
            NewTraces = maps:remove(Pid, Traces),
            {reply, ok, State#state{traces = NewTraces}}
    end;

handle_call({get_snapshot, Pid}, _From, State = #state{traces = Traces}) ->
    case maps:get(Pid, Traces, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            case capture_snapshot(Pid, Session) of
                {ok, Snapshot, NewSession} ->
                    NewTraces = maps:put(Pid, NewSession, Traces),
                    {reply, {ok, Snapshot}, State#state{traces = NewTraces}};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({get_snapshots, Pid}, _From, State = #state{traces = Traces}) ->
    case maps:get(Pid, Traces, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #trace_session{snapshots = Snapshots} ->
            {reply, {ok, lists:reverse(Snapshots)}, State}
    end;

handle_call(detect_deadlocks, _From, State = #state{deadlock_threshold = Threshold}) ->
    Deadlocked = find_deadlocked(State, Threshold),
    {reply, Deadlocked, State};

handle_call({detect_deadlocks, ThresholdMs}, _From, State) ->
    Deadlocked = find_deadlocked(State, ThresholdMs),
    {reply, Deadlocked, State};

handle_call({replay, Pid, Options}, _From, State = #state{traces = Traces}) ->
    case maps:get(Pid, Traces, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #trace_session{events = Events} ->
            Replayed = replay_events(Events, Options),
            {reply, {ok, Replayed}, State}
    end;

handle_call({visualize, Pid}, _From, State = #state{traces = Traces}) ->
    case maps:get(Pid, Traces, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            Visualization = generate_visualization(Session),
            {reply, {ok, Visualization}, State}
    end;

handle_call({clear_trace, Pid}, _From, State = #state{traces = Traces}) ->
    case maps:get(Pid, Traces, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Session ->
            ClearedSession = Session#trace_session{events = [], snapshots = []},
            NewTraces = maps:put(Pid, ClearedSession, Traces),
            {reply, ok, State#state{traces = NewTraces}}
    end;

handle_call(list_traces, _From, State = #state{traces = Traces}) ->
    List = [{Pid, Mod, Started} ||
            {Pid, #trace_session{module = Mod, started = Started}} <- maps:to_list(Traces)],
    {reply, List, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({trace, Pid, call, {Mod, Fun, Args}}, State = #state{traces = Traces}) ->
    case maps:get(Pid, Traces, undefined) of
        undefined ->
            {noreply, State};
        Session ->
            Event = #trace_event{
                timestamp = erlang:timestamp(),
                type = determine_event_type(Fun),
                pid = Pid,
                data = {call, Mod, Fun, Args}
            },
            NewSession = add_event(Event, Session),
            NewTraces = maps:put(Pid, NewSession, Traces),
            {noreply, State#state{traces = NewTraces}}
    end;

handle_info({trace, Pid, return_from, {Mod, Fun, Arity}, Result}, State = #state{traces = Traces}) ->
    case maps:get(Pid, Traces, undefined) of
        undefined ->
            {noreply, State};
        Session ->
            Event = #trace_event{
                timestamp = erlang:timestamp(),
                type = determine_event_type(Fun),
                pid = Pid,
                data = {return, Mod, Fun, Arity, Result}
            },
            NewSession = add_event(Event, Session),
            NewTraces = maps:put(Pid, NewSession, Traces),
            {noreply, State#state{traces = NewTraces}}
    end;

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State = #state{traces = Traces}) ->
    case maps:get(Pid, Traces, undefined) of
        undefined ->
            {noreply, State};
        _Session ->
            teardown_trace(Pid),
            NewTraces = maps:remove(Pid, Traces),
            {noreply, State#state{traces = NewTraces}}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{traces = Traces}) ->
    maps:foreach(fun(Pid, _Session) -> teardown_trace(Pid) end, Traces),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

setup_trace(Pid) ->
    try
        case erlang:process_info(Pid, [dictionary, initial_call]) of
            undefined ->
                {error, process_not_found};
            Info ->
                Module = extract_module(Info),
                dbg:start(),
                dbg:p(Pid, [call, return_to, timestamp]),
                dbg:tpl(Module, [{'_', [], [{return_trace}]}]),
                {ok, Module}
        end
    catch
        _:Error:_Stack ->
            {error, Error}
    end.

teardown_trace(Pid) ->
    try
        dbg:p(Pid, clear),
        ok
    catch
        _:_ ->
            ok
    end.

extract_module(Info) ->
    case proplists:get_value(initial_call, Info) of
        {Module, _, _} -> Module;
        _ -> unknown
    end.

capture_snapshot(Pid, Session = #trace_session{snapshots = Snapshots, options = Opts}) ->
    try
        case sys:get_state(Pid, 1000) of
            {error, Reason} ->
                {error, Reason};
            ProcState ->
                Snapshot = #state_snapshot{
                    timestamp = erlang:timestamp(),
                    pid = Pid,
                    marking = extract_marking(ProcState),
                    bindings = extract_bindings(ProcState, Opts),
                    metadata = #{
                        memory => erlang:process_info(Pid, memory),
                        message_queue_len => erlang:process_info(Pid, message_queue_len)
                    }
                },
                NewSnapshots = [Snapshot | Snapshots],
                MaxSnapshots = maps:get(max_snapshots, Opts, 100),
                TrimmedSnapshots = lists:sublist(NewSnapshots, MaxSnapshots),
                {ok, Snapshot, Session#trace_session{snapshots = TrimmedSnapshots}}
        end
    catch
        _:Error:_Stack ->
            {error, Error}
    end.

extract_marking(State) when is_tuple(State) ->
    case element(1, State) of
        state ->
            case tuple_size(State) > 1 of
                true -> element(2, State);
                false -> undefined
            end;
        _ -> State
    end;
extract_marking(State) ->
    State.

extract_bindings(State, Opts) ->
    case maps:get(capture_bindings, Opts, true) of
        true when is_tuple(State), tuple_size(State) > 2 ->
            element(3, State);
        true when is_map(State) ->
            maps:get(bindings, State, #{});
        _ ->
            undefined
    end.

add_event(Event, Session = #trace_session{events = Events, options = Opts}) ->
    NewEvents = [Event | Events],
    MaxEvents = maps:get(max_events, Opts, 10000),
    TrimmedEvents = lists:sublist(NewEvents, MaxEvents),
    Session#trace_session{
        events = TrimmedEvents,
        last_activity = Event#trace_event.timestamp
    }.

determine_event_type(fire) -> firing;
determine_event_type(transition) -> transition;
determine_event_type(bind) -> binding;
determine_event_type(enabled) -> enabled;
determine_event_type(handle_call) -> state_change;
determine_event_type(handle_cast) -> state_change;
determine_event_type(_) -> transition.

find_deadlocked(#state{traces = Traces}, ThresholdMs) ->
    Now = erlang:timestamp(),
    ThresholdMicros = ThresholdMs * 1000,
    lists:filtermap(
        fun({Pid, #trace_session{last_activity = LastActivity, events = Events}}) ->
            case length(Events) > 0 of
                true ->
                    IdleTime = timer:now_diff(Now, LastActivity),
                    case IdleTime > ThresholdMicros of
                        true ->
                            case is_process_alive(Pid) of
                                true -> {true, Pid};
                                false -> false
                            end;
                        false ->
                            false
                    end;
                false ->
                    false
            end
        end,
        maps:to_list(Traces)
    ).

replay_events(Events, Options) ->
    ReversedEvents = lists:reverse(Events),
    StartTime = maps:get(start_time, Options, undefined),
    EndTime = maps:get(end_time, Options, undefined),
    EventType = maps:get(event_type, Options, undefined),

    Filtered1 = case StartTime of
        undefined -> ReversedEvents;
        _ -> lists:filter(fun(#trace_event{timestamp = T}) -> T >= StartTime end, ReversedEvents)
    end,

    Filtered2 = case EndTime of
        undefined -> Filtered1;
        _ -> lists:filter(fun(#trace_event{timestamp = T}) -> T =< EndTime end, Filtered1)
    end,

    Filtered3 = case EventType of
        undefined -> Filtered2;
        _ -> lists:filter(fun(#trace_event{type = T}) -> T =:= EventType end, Filtered2)
    end,

    Filtered3.

generate_visualization(#trace_session{events = Events, snapshots = Snapshots, module = Module}) ->
    [
        "=== Workflow Trace Visualization ===\n",
        io_lib:format("Module: ~p~n", [Module]),
        io_lib:format("Events: ~p~n", [length(Events)]),
        io_lib:format("Snapshots: ~p~n~n", [length(Snapshots)]),

        "Timeline:\n",
        format_timeline(lists:reverse(Events)),

        "\nState Progression:\n",
        format_snapshots(lists:reverse(Snapshots)),

        "\nTransition Graph:\n",
        format_transition_graph(lists:reverse(Events))
    ].

format_timeline(Events) ->
    lists:map(
        fun(#trace_event{timestamp = TS, type = Type, data = Data}) ->
            TimeStr = format_timestamp(TS),
            io_lib:format("  [~s] ~p: ~p~n", [TimeStr, Type, format_data(Data)])
        end,
        lists:sublist(Events, 50)
    ).

format_snapshots(Snapshots) ->
    lists:map(
        fun(#state_snapshot{timestamp = TS, marking = Marking, metadata = Meta}) ->
            TimeStr = format_timestamp(TS),
            io_lib:format("  [~s] Marking: ~p, Queue: ~p~n",
                [TimeStr, Marking, maps:get(message_queue_len, Meta, unknown)])
        end,
        lists:sublist(Snapshots, 10)
    ).

format_transition_graph(Events) ->
    Transitions = lists:filtermap(
        fun(#trace_event{type = Type, data = Data}) ->
            case Type of
                transition -> {true, format_data(Data)};
                firing -> {true, format_data(Data)};
                _ -> false
            end
        end,
        Events
    ),

    UniqueTransitions = lists:usort(Transitions),
    lists:map(
        fun(Trans) ->
            Count = length([T || T <- Transitions, T =:= Trans]),
            io_lib:format("  ~p [fired ~p times]~n", [Trans, Count])
        end,
        UniqueTransitions
    ).

format_timestamp({Mega, Sec, Micro}) ->
    DateTime = calendar:now_to_datetime({Mega, Sec, Micro}),
    {{Y, M, D}, {H, Min, S}} = DateTime,
    Ms = Micro div 1000,
    io_lib:format("~4..0B-~2..0B-~2..0B ~2..0B:~2..0B:~2..0B.~3..0B",
        [Y, M, D, H, Min, S, Ms]).

format_data({call, _Mod, Fun, Args}) ->
    io_lib:format("~p(~p)", [Fun, Args]);
format_data({return, _Mod, Fun, _Arity, Result}) ->
    io_lib:format("~p -> ~p", [Fun, Result]);
format_data(Data) ->
    Data.
