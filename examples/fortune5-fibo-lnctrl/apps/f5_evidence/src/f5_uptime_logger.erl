%% Continuous Uptime Logger
%% Logs all supervisor events for 90-day certification trial
-module(f5_uptime_logger).
-behaviour(gen_server).

-export([start_link/0, log_event/1, get_uptime_stats/0, stop/0]).
-export([start/0, collect/0, verify/0]).  %% Standard evidence API
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    log_file :: file:io_device(),
    start_time :: integer(),
    event_count = 0 :: integer(),
    unplanned_restart_count = 0 :: integer()
}).

%%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

log_event(Event) when is_map(Event) ->
    gen_server:cast(?MODULE, {log_event, Event}).

get_uptime_stats() ->
    gen_server:call(?MODULE, get_stats).

stop() ->
    gen_server:stop(?MODULE).

%%% gen_server callbacks

init([]) ->
    LogDir = "logs/continuous_operation",
    filelib:ensure_dir(LogDir ++ "/"),

    Date = calendar:system_time_to_rfc3339(erlang:system_time(second)),
    LogFile = LogDir ++ "/uptime_" ++ Date ++ ".log",

    {ok, Fd} = file:open(LogFile, [append]),

    %% Log startup
    StartupEvent = #{
        timestamp => erlang:system_time(microsecond),
        event_type => logger_started,
        otp_version => erlang:system_info(otp_release),
        system_architecture => erlang:system_info(system_architecture)
    },
    write_event(Fd, StartupEvent),

    {ok, #state{
        log_file = Fd,
        start_time = erlang:system_time(second)
    }}.

handle_call(get_stats, _From, State = #state{start_time = Start, event_count = Count, unplanned_restart_count = Restarts}) ->
    Now = erlang:system_time(second),
    Uptime = Now - Start,

    Stats = #{
        uptime_seconds => Uptime,
        uptime_days => Uptime / 86400,
        total_events => Count,
        unplanned_restarts => Restarts,
        uptime_percentage => calculate_uptime_percentage(Restarts, Uptime)
    },

    {reply, Stats, State}.

handle_cast({log_event, Event}, State = #state{log_file = Fd, event_count = Count, unplanned_restart_count = Restarts}) ->
    write_event(Fd, Event),

    NewRestarts = case maps:get(event_type, Event, undefined) of
        supervisor_restart ->
            case maps:get(planned, Event, false) of
                false -> Restarts + 1;
                true -> Restarts
            end;
        _ -> Restarts
    end,

    {noreply, State#state{event_count = Count + 1, unplanned_restart_count = NewRestarts}}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{log_file = Fd}) ->
    file:close(Fd),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

write_event(Fd, Event) ->
    Json = iolist_to_binary(json:encode(Event)),
    file:write(Fd, [Json, "\n"]).

calculate_uptime_percentage(0, _Uptime) ->
    100.0;
calculate_uptime_percentage(Restarts, Uptime) ->
    %% Assume 100ms recovery per restart
    DowntimeUs = Restarts * 100000,
    UptimeUs = Uptime * 1000000,
    ((UptimeUs - DowntimeUs) / UptimeUs) * 100.

%%% Standard Evidence API

-spec start() -> ok | {error, term()}.
start() ->
    case start_link() of
        {ok, _Pid} -> ok;
        {error, {already_started, _Pid}} -> ok;
        Error -> Error
    end.

-spec collect() -> {ok, map()}.
collect() ->
    Stats = get_uptime_stats(),
    Evidence = #{
        module => f5_uptime_logger,
        type => uptime_monitoring,
        timestamp => receipt_builder:iso8601_now(),
        data => Stats,
        evidence_file => "evidence/uptime/continuous_operation.json"
    },

    %% Write to evidence directory
    filelib:ensure_dir("evidence/uptime/"),
    EvidenceJson = iolist_to_binary(json:encode(Evidence)),
    file:write_file("evidence/uptime/continuous_operation.json", EvidenceJson),

    %% Compute hash for receipt chaining
    Hash = receipt_builder:hash_receipt(Evidence),

    {ok, Evidence#{evidence_hash => Hash}}.

-spec verify() -> ok | {error, term()}.
verify() ->
    case file:read_file("evidence/uptime/continuous_operation.json") of
        {ok, JsonBin} ->
            Evidence = json:decode(JsonBin),
            StoredHash = maps:get(<<"evidence_hash">>, Evidence),
            EvidenceWithoutHash = maps:remove(<<"evidence_hash">>, Evidence),
            ComputedHash = list_to_binary(receipt_builder:hash_receipt(EvidenceWithoutHash)),

            case ComputedHash of
                StoredHash -> ok;
                _ -> {error, {hash_mismatch, StoredHash, ComputedHash}}
            end;
        {error, Reason} ->
            {error, {file_error, Reason}}
    end.
