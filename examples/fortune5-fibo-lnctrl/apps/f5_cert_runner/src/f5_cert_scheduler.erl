%% Certification Evidence Scheduler
%% Schedules periodic evidence collection for 90-day continuous operation
-module(f5_cert_scheduler).
-behaviour(gen_server).

-export([start_link/0, pause/0, resume/0, get_schedule_info/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    interval :: integer(),
    timer_ref :: timer:tref() | undefined,
    paused = false :: boolean(),
    collections = 0 :: integer(),
    start_time :: integer()
}).

%%% API

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec pause() -> ok.
pause() ->
    gen_server:call(?MODULE, pause).

-spec resume() -> ok.
resume() ->
    gen_server:call(?MODULE, resume).

-spec get_schedule_info() -> map().
get_schedule_info() ->
    gen_server:call(?MODULE, get_schedule_info).

%%% gen_server callbacks

-spec init([]) -> {ok, #state{}}.
init([]) ->
    %% Get collection interval from application environment
    Interval = application:get_env(f5_cert_runner, collection_interval, 3600000),  %% Default: 1 hour

    logger:info("Certification scheduler started with interval ~p ms (~.1f hours)",
               [Interval, Interval / 3600000]),

    %% Schedule first collection
    {ok, TRef} = timer:send_interval(Interval, self(), collect),

    {ok, #state{
        interval = Interval,
        timer_ref = TRef,
        start_time = erlang:system_time(second)
    }}.

-spec handle_call(term(), {pid(), term()}, #state{}) -> {reply, term(), #state{}}.
handle_call(pause, _From, State = #state{timer_ref = TRef, paused = false}) ->
    case TRef of
        undefined -> ok;
        _ -> timer:cancel(TRef)
    end,
    logger:info("Certification scheduler paused"),
    {reply, ok, State#state{timer_ref = undefined, paused = true}};

handle_call(pause, _From, State = #state{paused = true}) ->
    {reply, {error, already_paused}, State};

handle_call(resume, _From, State = #state{interval = Interval, paused = true}) ->
    {ok, TRef} = timer:send_interval(Interval, self(), collect),
    logger:info("Certification scheduler resumed"),
    {reply, ok, State#state{timer_ref = TRef, paused = false}};

handle_call(resume, _From, State = #state{paused = false}) ->
    {reply, {error, not_paused}, State};

handle_call(get_schedule_info, _From, State = #state{
    interval = Interval,
    paused = Paused,
    collections = Collections,
    start_time = StartTime
}) ->
    Now = erlang:system_time(second),
    RunTime = Now - StartTime,
    TargetDays = application:get_env(f5_cert_runner, continuous_operation_days, 90),
    TargetSeconds = TargetDays * 86400,
    Progress = (RunTime / TargetSeconds) * 100,

    Info = #{
        interval_ms => Interval,
        paused => Paused,
        collections => Collections,
        runtime_seconds => RunTime,
        runtime_days => RunTime / 86400,
        target_days => TargetDays,
        progress_percent => Progress
    },

    {reply, Info, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(collect, State = #state{paused = false, collections = Collections}) ->
    logger:info("Scheduled evidence collection triggered (collection #~p)", [Collections + 1]),

    %% Trigger collection in separate process to avoid blocking scheduler
    spawn(fun() ->
        case f5_cert_runner:start_collection() of
            {ok, Results} ->
                logger:info("Scheduled evidence collection completed: ~p", [Results]);
            {error, Reason} ->
                logger:error("Scheduled evidence collection failed: ~p", [Reason])
        end
    end),

    {noreply, State#state{collections = Collections + 1}};

handle_info(collect, State = #state{paused = true}) ->
    %% Skip collection if paused
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, #state{timer_ref = TRef}) ->
    case TRef of
        undefined -> ok;
        _ -> timer:cancel(TRef)
    end,
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
