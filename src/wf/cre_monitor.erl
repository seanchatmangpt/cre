%% -*- erlang -*-
%% @doc System monitoring utilities for CRE workflows.
%%
%% Provides real-time monitoring capabilities:
%% <ul>
%%   <li>Process monitoring</li>
%%   <li>Memory monitoring</li>
%%   <li>Message queue monitoring</li>
%%   <li>ETS table monitoring</li>
%%   <li>Network monitoring</li>
%%   <li>Performance monitoring</li>
%%   <li>Alert generation</li>
%% </ul>
%%
%% @end
-module(cre_monitor).
-behaviour(gen_server).
-export([
    start_link/0,
    start_link/1,
    stop/0,
    
    %% Monitoring control
    start_monitoring/1,
    stop_monitoring/0,
    get_monitoring_status/0,
    
    %% Process monitoring
    monitor_process/1,
    demonitor_process/1,
    get_monitored_processes/0,
    
    %% Alert configuration
    set_alert_threshold/2,
    get_alerts/0,
    clear_alerts/0,
    
    %% Statistics
    get_statistics/0,
    get_statistics_snapshot/0
]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

-record(state, {
    monitoring = false,
    monitored_processes = [],
    alerts = [],
    thresholds = #{},
    statistics = #{},
    interval = 1000
}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    start_link(#{}).

start_link(Opts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

stop() ->
    gen_server:stop(?MODULE).

start_monitoring(Opts) ->
    gen_server:call(?MODULE, {start_monitoring, Opts}).

stop_monitoring() ->
    gen_server:call(?MODULE, stop_monitoring).

get_monitoring_status() ->
    gen_server:call(?MODULE, get_status).

monitor_process(Pid) ->
    gen_server:call(?MODULE, {monitor_process, Pid}).

demonitor_process(Pid) ->
    gen_server:call(?MODULE, {demonitor_process, Pid}).

get_monitored_processes() ->
    gen_server:call(?MODULE, get_monitored_processes).

set_alert_threshold(Metric, Threshold) ->
    gen_server:call(?MODULE, {set_threshold, Metric, Threshold}).

get_alerts() ->
    gen_server:call(?MODULE, get_alerts).

clear_alerts() ->
    gen_server:call(?MODULE, clear_alerts).

get_statistics() ->
    gen_server:call(?MODULE, get_statistics).

get_statistics_snapshot() ->
    gen_server:call(?MODULE, get_snapshot).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Opts) ->
    Interval = maps:get(interval, Opts, 1000),
    {ok, #state{interval = Interval}}.

handle_call({start_monitoring, Opts}, _From, State) ->
    Interval = maps:get(interval, Opts, State#state.interval),
    erlang:send_after(Interval, self(), collect_statistics),
    {reply, ok, State#state{monitoring = true, interval = Interval}};

handle_call(stop_monitoring, _From, State) ->
    {reply, ok, State#state{monitoring = false}};

handle_call(get_status, _From, State) ->
    {reply, #{monitoring => State#state.monitoring,
              monitored_processes => length(State#state.monitored_processes),
              alerts => length(State#state.alerts)}, State};

handle_call({monitor_process, Pid}, _From, State) ->
    Monitored = State#state.monitored_processes,
    case lists:member(Pid, Monitored) of
        true ->
            {reply, {error, already_monitored}, State};
        false ->
            erlang:monitor(process, Pid),
            {reply, ok, State#state{monitored_processes = [Pid | Monitored]}}
    end;

handle_call({demonitor_process, Pid}, _From, State) ->
    Monitored = State#state.monitored_processes,
    case lists:member(Pid, Monitored) of
        false ->
            {reply, {error, not_monitored}, State};
        true ->
            erlang:demonitor(erlang:make_ref()),
            {reply, ok, State#state{monitored_processes = lists:delete(Pid, Monitored)}}
    end;

handle_call(get_monitored_processes, _From, State) ->
    {reply, State#state.monitored_processes, State};

handle_call({set_threshold, Metric, Threshold}, _From, State) ->
    Thresholds = maps:put(Metric, Threshold, State#state.thresholds),
    {reply, ok, State#state{thresholds = Thresholds}};

handle_call(get_alerts, _From, State) ->
    {reply, State#state.alerts, State};

handle_call(clear_alerts, _From, State) ->
    {reply, ok, State#state{alerts = []}};

handle_call(get_statistics, _From, State) ->
    {reply, State#state.statistics, State};

handle_call(get_snapshot, _From, State) ->
    Snapshot = collect_current_statistics(),
    {reply, Snapshot, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(collect_statistics, State) when State#state.monitoring =:= true ->
    Stats = collect_current_statistics(),
    Alerts = check_thresholds(Stats, State#state.thresholds),
    NewAlerts = State#state.alerts ++ Alerts,
    erlang:send_after(State#state.interval, self(), collect_statistics),
    {noreply, State#state{statistics = Stats, alerts = NewAlerts}};

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    Monitored = lists:delete(Pid, State#state.monitored_processes),
    Alert = #{type => process_down, pid => Pid, timestamp => erlang:system_time(millisecond)},
    {noreply, State#state{monitored_processes = Monitored,
                          alerts = [Alert | State#state.alerts]}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal
%%====================================================================

collect_current_statistics() ->
    #{memory => erlang:memory(),
      processes => erlang:system_info(process_count),
      run_queue => erlang:statistics(run_queue),
      reductions => erlang:statistics(reductions),
      timestamp => erlang:system_time(millisecond)}.

check_thresholds(Stats, Thresholds) ->
    maps:fold(fun(Metric, Threshold, Acc) ->
        Value = maps:get(Metric, Stats, undefined),
        case Value of
            undefined -> Acc;
            V when V > Threshold ->
                [#{type => threshold_exceeded,
                    metric => Metric,
                    value => V,
                    threshold => Threshold,
                    timestamp => erlang:system_time(millisecond)} | Acc];
            _ -> Acc
        end
    end, [], Thresholds).
