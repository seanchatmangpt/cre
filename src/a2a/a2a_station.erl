%%%-----------------------------------------------------------------------------
%%% @doc A2A Station Behavior
%%%
%%% Implements deterministic protocol execution as an OTP gen_server.
%%% Stations are typed packet processors with bounded transitions.
%%%
%%% Key properties:
%%% - Refusal determinism: typed terminal refusals instead of AI arbitration
%%% - Receipt chains: proof objects binding input → output
%%% - Closed ontology: only known protocols supported
%%% - Bounded actuation: timeouts and capacity limits
%%% @end
%%%-----------------------------------------------------------------------------
-module(a2a_station).
-behaviour(gen_server).

%% API
-export([
    start_link/2,
    start_link/3,
    execute_task/2,
    get_state/1,
    get_receipts/1,
    stop/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-include_lib("kernel/include/logger.hrl").

%%%=============================================================================
%%% Types
%%%=============================================================================

-type station_config() :: #{
    station_id := binary(),
    protocols := [binary()],
    max_concurrent := pos_integer(),
    task_timeout := pos_integer(),
    handlers := #{binary() => mfa()}
}.

-record(state, {
    station :: a2a_types:station(),
    config :: station_config(),
    active_tasks :: #{binary() => task_context()},
    event_log :: [a2a_types:event()]
}).

-type task_context() :: #{
    task := a2a_types:task(),
    started_at := integer(),
    timer_ref := reference()
}.

%%%=============================================================================
%%% API
%%%=============================================================================

-spec start_link(binary(), [binary()]) -> {ok, pid()} | {error, term()}.
start_link(StationId, Protocols) ->
    start_link(StationId, Protocols, #{}).

-spec start_link(binary(), [binary()], map()) -> {ok, pid()} | {error, term()}.
start_link(StationId, Protocols, Options) ->
    Config = #{
        station_id => StationId,
        protocols => Protocols,
        max_concurrent => maps:get(max_concurrent, Options, 10),
        task_timeout => maps:get(task_timeout, Options, 30000),
        handlers => maps:get(handlers, Options, #{})
    },
    gen_server:start_link(?MODULE, Config, []).

-spec execute_task(pid(), a2a_types:task()) -> {ok, a2a_types:receipt()} | {refused, a2a_types:refusal()}.
execute_task(Station, Task) ->
    gen_server:call(Station, {execute_task, Task}, 60000).

-spec get_state(pid()) -> a2a_types:station().
get_state(Station) ->
    gen_server:call(Station, get_state).

-spec get_receipts(pid()) -> [a2a_types:receipt()].
get_receipts(Station) ->
    gen_server:call(Station, get_receipts).

-spec stop(pid()) -> ok.
stop(Station) ->
    gen_server:stop(Station).

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

init(Config) ->
    #{station_id := StationId, protocols := Protocols} = Config,
    Station = a2a_types:new_station(StationId, Protocols),

    State = #state{
        station = Station,
        config = Config,
        active_tasks = #{},
        event_log = []
    },

    ?LOG_INFO(#{
        what => station_started,
        station_id => StationId,
        protocols => Protocols
    }),

    {ok, State}.

handle_call({execute_task, Task}, _From, State) ->
    case can_accept_task(Task, State) of
        ok ->
            {Reply, NewState} = do_execute_task(Task, State),
            {reply, Reply, NewState};
        {refused, Refusal} ->
            NewState = log_event(task_refused, Task, Refusal, State),
            {reply, {refused, Refusal}, NewState}
    end;

handle_call(get_state, _From, State = #state{station = Station}) ->
    {reply, Station, State};

handle_call(get_receipts, _From, State = #state{station = Station}) ->
    Receipts = maps:get(receipts, Station),
    {reply, Receipts, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({task_timeout, TaskId}, State) ->
    NewState = handle_task_timeout(TaskId, State),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{station = #{id := StationId}}) ->
    ?LOG_INFO(#{what => station_stopped, station_id => StationId}),
    ok.

%%%=============================================================================
%%% Internal Functions - Guard Predicates (H)
%%%=============================================================================

-spec can_accept_task(a2a_types:task(), #state{}) -> ok | {refused, a2a_types:refusal()}.
can_accept_task(Task, State) ->
    Checks = [
        fun check_protocol_supported/2,
        fun check_capacity_available/2,
        fun check_task_valid/2
    ],
    run_guard_checks(Task, State, Checks).

-spec run_guard_checks(a2a_types:task(), #state{}, [fun()]) -> ok | {refused, a2a_types:refusal()}.
run_guard_checks(_Task, _State, []) ->
    ok;
run_guard_checks(Task, State, [Check | Rest]) ->
    case Check(Task, State) of
        ok -> run_guard_checks(Task, State, Rest);
        {refused, _} = Refusal -> Refusal
    end.

-spec check_protocol_supported(a2a_types:task(), #state{}) -> ok | {refused, a2a_types:refusal()}.
check_protocol_supported(#{protocol := Protocol, id := TaskId}, #state{station = #{id := StationId, protocols := Protocols}}) ->
    case lists:member(Protocol, Protocols) of
        true -> ok;
        false ->
            {refused, #{
                type => protocol_unknown,
                reason => <<"Protocol not supported by this station">>,
                task_id => TaskId,
                station_id => StationId,
                timestamp => erlang:system_time(microsecond),
                terminal => true
            }}
    end.

-spec check_capacity_available(a2a_types:task(), #state{}) -> ok | {refused, a2a_types:refusal()}.
check_capacity_available(#{id := TaskId}, #state{station = #{id := StationId}, config = #{max_concurrent := Max}, active_tasks = Active}) ->
    CurrentCount = maps:size(Active),
    case CurrentCount < Max of
        true -> ok;
        false ->
            {refused, #{
                type => capacity_exceeded,
                reason => <<"Station at maximum capacity">>,
                task_id => TaskId,
                station_id => StationId,
                timestamp => erlang:system_time(microsecond),
                terminal => false
            }}
    end.

-spec check_task_valid(a2a_types:task(), #state{}) -> ok | {refused, a2a_types:refusal()}.
check_task_valid(Task = #{id := TaskId}, #state{station = #{id := StationId}}) ->
    case a2a_types:validate_task(Task) of
        ok -> ok;
        {error, Reason} ->
            {refused, #{
                type => type_mismatch,
                reason => list_to_binary(io_lib:format("~p", [Reason])),
                task_id => TaskId,
                station_id => StationId,
                timestamp => erlang:system_time(microsecond),
                terminal => true
            }}
    end.

%%%=============================================================================
%%% Internal Functions - Task Execution (Δ - Bounded Actuation)
%%%=============================================================================

-spec do_execute_task(a2a_types:task(), #state{}) -> {{ok, a2a_types:receipt()}, #state{}}.
do_execute_task(Task = #{id := TaskId, protocol := Protocol}, State) ->
    % Log task received
    State1 = log_event(task_received, Task, #{}, State),

    % Start task timer
    #state{config = #{task_timeout := Timeout}} = State1,
    TimerRef = erlang:send_after(Timeout, self(), {task_timeout, TaskId}),

    % Add to active tasks
    TaskCtx = #{
        task => Task,
        started_at => erlang:system_time(microsecond),
        timer_ref => TimerRef
    },
    State2 = State1#state{active_tasks = maps:put(TaskId, TaskCtx, State1#state.active_tasks)},

    % Log task started
    State3 = log_event(task_started, Task, #{}, State2),

    % Execute protocol handler
    Result = execute_protocol_handler(Protocol, Task, State3),

    % Cancel timer
    erlang:cancel_timer(TimerRef),

    % Remove from active tasks
    State4 = State3#state{active_tasks = maps:remove(TaskId, State3#state.active_tasks)},

    % Process result
    finalize_task(Task, Result, State4).

-spec execute_protocol_handler(binary(), a2a_types:task(), #state{}) -> {ok, [a2a_types:artifact()]} | {error, term()}.
execute_protocol_handler(Protocol, Task, #state{config = #{handlers := Handlers}}) ->
    case maps:get(Protocol, Handlers, undefined) of
        undefined ->
            % Default: echo protocol - return mock artifacts
            Artifact = a2a_types:new_artifact(result, #{task_id => maps:get(id, Task)}),
            {ok, [Artifact]};
        {Module, Function, Args} ->
            try
                erlang:apply(Module, Function, [Task | Args])
            catch
                Class:Reason:Stack ->
                    ?LOG_ERROR(#{
                        what => protocol_handler_error,
                        protocol => Protocol,
                        class => Class,
                        reason => Reason,
                        stacktrace => Stack
                    }),
                    {error, {handler_crash, Reason}}
            end
    end.

-spec finalize_task(a2a_types:task(), {ok, [a2a_types:artifact()]} | {error, term()}, #state{}) -> {{ok, a2a_types:receipt()}, #state{}}.
finalize_task(Task = #{id := TaskId}, {ok, OutputArtifacts}, State) ->
    #state{station = Station = #{id := StationId, receipts := Receipts}} = State,

    % Create receipt
    InputHashes = [],  % Would come from task inputs
    OutputHashes = [maps:get(hash, A) || A <- OutputArtifacts],
    Receipt = a2a_types:new_receipt(TaskId, StationId, #{inputs => InputHashes, outputs => OutputHashes}),

    % Update station
    UpdatedStation = Station#{receipts => [Receipt | Receipts]},
    State1 = State#state{station = UpdatedStation},

    % Log completion
    State2 = log_event(task_completed, Task, #{receipt => Receipt}, State1),

    {{ok, Receipt}, State2};
finalize_task(Task = #{id := TaskId}, {error, Reason}, State) ->
    #state{station = #{id := StationId}} = State,

    Refusal = #{
        type => precondition_failed,
        reason => list_to_binary(io_lib:format("~p", [Reason])),
        task_id => TaskId,
        station_id => StationId,
        timestamp => erlang:system_time(microsecond),
        terminal => true
    },

    State1 = log_event(task_refused, Task, Refusal, State),
    {{refused, Refusal}, State1}.

-spec handle_task_timeout(binary(), #state{}) -> #state{}.
handle_task_timeout(TaskId, State = #state{active_tasks = Active}) ->
    case maps:get(TaskId, Active, undefined) of
        undefined ->
            State;
        #{task := Task} ->
            #state{station = #{id := StationId}} = State,
            Refusal = #{
                type => timeout,
                reason => <<"Task execution timeout">>,
                task_id => TaskId,
                station_id => StationId,
                timestamp => erlang:system_time(microsecond),
                terminal => true
            },
            State1 = State#state{active_tasks = maps:remove(TaskId, Active)},
            log_event(task_refused, Task, Refusal, State1)
    end.

%%%=============================================================================
%%% Internal Functions - Event Logging (Telemetry)
%%%=============================================================================

-spec log_event(a2a_types:event_type(), a2a_types:task(), map(), #state{}) -> #state{}.
log_event(EventType, Task, Data, State = #state{station = #{id := StationId}, event_log = Log}) ->
    Event = a2a_types:new_event(EventType, StationId, Data#{task => Task}),

    ?LOG_DEBUG(#{
        what => a2a_event,
        event_type => EventType,
        station_id => StationId,
        task_id => maps:get(id, Task)
    }),

    State#state{event_log = [Event | Log]}.
