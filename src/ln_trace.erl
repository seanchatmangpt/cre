%%%-------------------------------------------------------------------
%%% @doc ln_trace - Structured event tracing and buffering.
%%%
%%% Provides event emission, buffering, and export for workflow
%%% execution observability.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_trace).
-include_lib("kernel/include/logger.hrl").

%% API
-export([new/0, new/1]).
-export([emit/2]).
-export([get_all/1]).
-export([get_range/3]).
-export([export/2]).
-export([clear/1]).
-export([save/2, load/1]).
-export([set_logger_level/1]).
-export([from_config/0]).

%% Types
-export_type([state/0, event/0, event_type/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type event_type() :: case_started
                     | step_started
                     | step_completed
                     | branch_chosen
                     | join_waiting
                     | effect_requested
                     | effect_completed
                     | scope_cancelled
                     | case_completed
                     | case_failed
                     | case_cancelled.

-type event() :: #{
    timestamp := integer(),
    type := event_type(),
    data => map()
}.

-type trace_level() :: none | min | full.

-record(trace_state, {
    events :: [event()],
    max_events :: non_neg_integer() | infinity,
    level :: trace_level(),
    seq :: non_neg_integer()
}).

-opaque state() :: #trace_state{}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Create a new trace buffer with defaults (full, unlimited).
-spec new() -> state().
new() ->
    new(#{level => full, max_events => infinity}).

%% @doc Create a new trace buffer with options.
-spec new(#{level => trace_level(), max_events => non_neg_integer() | infinity}) -> state().
new(Options) ->
    Level = maps:get(level, Options, full),
    MaxEvents = maps:get(max_events, Options, infinity),
    #trace_state{
        events = [],
        max_events = MaxEvents,
        level = Level,
        seq = 0
    }.

%% @doc Emit an event to the trace buffer.
-spec emit(event() | event_type(), state()) -> state().
emit(#{timestamp := _} = Event, #trace_state{events = Events, max_events = Max, seq = Seq} = State) ->
    NewEvents = [Event#{seq => Seq} | Events],
    Trimmed = trim_events(NewEvents, Max),
    %% Log to OTP logger for centralized observability
    ?LOG_INFO("Trace event ~p: ~p", [Seq, Event]),
    State#trace_state{events = Trimmed, seq = Seq + 1};
emit(EventType, #trace_state{level = Level} = State) when Level =:= none ->
    State;
emit(EventType, State) ->
    emit(#{
        timestamp => erlang:monotonic_time(millisecond),
        type => EventType,
        data => #{}
    }, State).

%% @doc Get all events from the trace buffer.
-spec get_all(state()) -> [event()].
get_all(#trace_state{events = Events}) ->
    lists:reverse(Events).

%% @doc Get events within a sequence range.
-spec get_range(state(), non_neg_integer(), non_neg_integer()) -> [event()].
get_range(#trace_state{events = Events}, From, To) ->
    lists:filter(fun
        (#{seq := Seq}) when Seq >= From, Seq =< To -> true;
        (_) -> false
    end, Events).

%% @doc Export trace in specified format.
-spec export(state(), map | list | json) -> term().
export(#trace_state{events = Events}, map) ->
    Events;
export(#trace_state{events = Events}, list) ->
    lists:map(fun event_to_list/1, Events);
export(#trace_state{events = Events}, json) ->
    %% Simple JSON export (jsx would be better for production)
    lists:map(fun event_to_json/1, Events).

%% @doc Clear all events from the trace buffer.
-spec clear(state()) -> state().
clear(State) ->
    State#trace_state{events = [], seq = 0}.

%% @doc Save trace to file in JSON format.
-spec save(state(), file:name()) -> ok | {error, term()}.
save(#trace_state{events = Events}, Filename) ->
    try
        %% Convert events to JSON-serializable format
        JSON = jsx:encode(lists:reverse(Events)),
        file:write_file(Filename, JSON)
    catch
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%% @doc Load trace from file.
-spec load(file:name()) -> {ok, state()} | {error, term()}.
load(Filename) ->
    case file:read_file(Filename) of
        {ok, JSON} ->
            try
                Events = jsx:decode(JSON, [return_maps]),
                %% Convert binary keys to atoms for event type, keep others as-is
                ConvertedEvents = convert_events(Events),
                {ok, #trace_state{
                    events = lists:reverse(ConvertedEvents),
                    max_events = infinity,
                    level = full,
                    seq = length(ConvertedEvents)
                }}
            catch
                Error:Reason:Stack ->
                    {error, {Error, Reason, Stack}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Set the logger level for trace events.
-spec set_logger_level(logger:level()) -> ok.
set_logger_level(Level) ->
    logger:set_application_level(cre, Level).

%% @doc Create a new trace buffer from application config.
-spec from_config() -> state().
from_config() ->
    Level = application:get_env(cre, trace_level, none),
    MaxEvents = application:get_env(cre, trace_max_events, infinity),
    new(#{level => Level, max_events => MaxEvents}).

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @doc Convert events from JSON (binary keys) to internal format.
convert_events(Events) ->
    lists:map(fun convert_event/1, Events).

%% @doc Convert a single event from JSON format.
convert_event(Event) ->
    %% Convert binary keys to atoms for all keys, recursively
    maps:fold(fun(K, V, Acc) ->
        AtomKey = binary_to_existing_atom(K, utf8),
        %% For type field, convert value from binary to atom
        %% For data field, recursively convert nested map
        Value = case {AtomKey, V} of
            {type, BinValue} when is_binary(BinValue) ->
                try binary_to_existing_atom(BinValue, utf8)
                catch
                    error:_ -> BinValue
                end;
            {data, MapValue} when is_map(MapValue) ->
                convert_data_map(MapValue);
            _ ->
                V
        end,
        Acc#{AtomKey => Value}
    end, #{}, Event).

%% @doc Convert nested data map from binary keys to atoms.
convert_data_map(DataMap) ->
    maps:fold(fun(K, V, Acc) ->
        AtomKey = try binary_to_existing_atom(K, utf8)
                     catch error:_ -> K
                   end,
        Acc#{AtomKey => V}
    end, #{}, DataMap).

%% @doc Trim events to max_events limit.
%% Events are stored in reverse order (newest first), so we keep the first Max.
trim_events(Events, infinity) ->
    Events;
trim_events(Events, Max) when length(Events) =< Max ->
    Events;
trim_events(Events, Max) ->
    %% Keep the most recent Max events (already at the front of the list)
    lists:sublist(Events, Max).

%% @doc Convert event map to list.
event_to_list(#{timestamp := TS, type := Type, seq := Seq, data := Data}) ->
    [{timestamp, TS}, {type, Type}, {seq, Seq} | maps:to_list(Data)].

%% @doc Convert event to simple JSON-like format.
event_to_json(Event) ->
    lists:flatten(io_lib:format("~p", [Event])).
