%% -*- erlang -*-
-module(gen_pnet_upgrade).
-behaviour(gen_server).

-export([
    start_link/0,
    prepare_upgrade/3,
    execute_upgrade/1,
    rollback/1,
    checkpoint/2,
    verify_compatibility/2,
    list_checkpoints/1,
    get_upgrade_status/1
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include("gen_pnet.hrl").

-record(checkpoint, {
    id :: binary(),
    timestamp :: integer(),
    net_state :: #net_state{},
    version :: term(),
    metadata :: #{atom() => term()}
}).

-record(upgrade_plan, {
    id :: binary(),
    target_pid :: pid(),
    old_module :: atom(),
    new_module :: atom(),
    old_version :: term(),
    new_version :: term(),
    migration_fun :: fun((#net_state{}) -> #net_state{}),
    checkpoint :: #checkpoint{},
    status :: prepared | executing | completed | failed | rolled_back
}).

-record(state, {
    upgrade_plans = #{} :: #{binary() => #upgrade_plan{}},
    checkpoints = #{} :: #{pid() => [#checkpoint{}]},
    max_checkpoints = 10 :: pos_integer()
}).

-type upgrade_result() :: {ok, NewNetState :: #net_state{}} | {error, Reason :: term()}.
-type compatibility_result() :: compatible | {incompatible, Reason :: term()}.

-export_type([upgrade_result/0, compatibility_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec prepare_upgrade(Pid :: pid(), NewModule :: atom(), MigrationFun :: fun()) ->
    {ok, UpgradeId :: binary()} | {error, term()}.
prepare_upgrade(Pid, NewModule, MigrationFun)
  when is_pid(Pid), is_atom(NewModule), is_function(MigrationFun, 1) ->
    gen_server:call(?MODULE, {prepare_upgrade, Pid, NewModule, MigrationFun}, infinity).

-spec execute_upgrade(UpgradeId :: binary()) -> ok | {error, term()}.
execute_upgrade(UpgradeId) when is_binary(UpgradeId) ->
    gen_server:call(?MODULE, {execute_upgrade, UpgradeId}, infinity).

-spec rollback(UpgradeId :: binary()) -> ok | {error, term()}.
rollback(UpgradeId) when is_binary(UpgradeId) ->
    gen_server:call(?MODULE, {rollback, UpgradeId}, infinity).

-spec checkpoint(Pid :: pid(), Metadata :: #{atom() => term()}) ->
    {ok, CheckpointId :: binary()} | {error, term()}.
checkpoint(Pid, Metadata) when is_pid(Pid), is_map(Metadata) ->
    gen_server:call(?MODULE, {checkpoint, Pid, Metadata}, infinity).

-spec verify_compatibility(OldModule :: atom(), NewModule :: atom()) ->
    compatibility_result().
verify_compatibility(OldModule, NewModule)
  when is_atom(OldModule), is_atom(NewModule) ->
    gen_server:call(?MODULE, {verify_compatibility, OldModule, NewModule}).

-spec list_checkpoints(Pid :: pid()) -> {ok, [#checkpoint{}]} | {error, term()}.
list_checkpoints(Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {list_checkpoints, Pid}).

-spec get_upgrade_status(UpgradeId :: binary()) ->
    {ok, Status :: atom()} | {error, not_found}.
get_upgrade_status(UpgradeId) when is_binary(UpgradeId) ->
    gen_server:call(?MODULE, {get_upgrade_status, UpgradeId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

-spec init([]) -> {ok, #state{}}.
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: #state{}) ->
    {reply, term(), #state{}} | {noreply, #state{}}.

handle_call({prepare_upgrade, Pid, NewModule, MigrationFun}, _From,
            State = #state{upgrade_plans = Plans, checkpoints = Checkpoints}) ->
    try
        NetState = get_net_state(Pid),
        OldModule = NetState#net_state.net_mod,

        case verify_compatibility_internal(OldModule, NewModule) of
            compatible ->
                UpgradeId = generate_id(),

                CheckpointId = generate_id(),
                Checkpoint = #checkpoint{
                    id = CheckpointId,
                    timestamp = erlang:system_time(millisecond),
                    net_state = NetState,
                    version = get_module_version(OldModule),
                    metadata = #{reason => pre_upgrade}
                },

                Plan = #upgrade_plan{
                    id = UpgradeId,
                    target_pid = Pid,
                    old_module = OldModule,
                    new_module = NewModule,
                    old_version = get_module_version(OldModule),
                    new_version = get_module_version(NewModule),
                    migration_fun = MigrationFun,
                    checkpoint = Checkpoint,
                    status = prepared
                },

                PidCheckpoints = maps:get(Pid, Checkpoints, []),
                UpdatedCheckpoints = trim_checkpoints([Checkpoint | PidCheckpoints],
                                                      State#state.max_checkpoints),

                NewState = State#state{
                    upgrade_plans = Plans#{UpgradeId => Plan},
                    checkpoints = Checkpoints#{Pid => UpdatedCheckpoints}
                },

                {reply, {ok, UpgradeId}, NewState};

            {incompatible, Reason} ->
                {reply, {error, {incompatible, Reason}}, State}
        end
    catch
        _:Error:Stack ->
            {reply, {error, {preparation_failed, Error, Stack}}, State}
    end;

handle_call({execute_upgrade, UpgradeId}, _From,
            State = #state{upgrade_plans = Plans}) ->
    case maps:get(UpgradeId, Plans, undefined) of
        undefined ->
            {reply, {error, not_found}, State};

        Plan = #upgrade_plan{status = prepared} ->
            try
                Result = perform_upgrade(Plan),
                UpdatedPlan = Plan#upgrade_plan{status = completed},
                NewState = State#state{
                    upgrade_plans = Plans#{UpgradeId => UpdatedPlan}
                },
                {reply, Result, NewState}
            catch
                _:Error:Stack ->
                    FailedPlan = Plan#upgrade_plan{status = failed},
                    NewState = State#state{
                        upgrade_plans = Plans#{UpgradeId => FailedPlan}
                    },
                    {reply, {error, {upgrade_failed, Error, Stack}}, NewState}
            end;

        #upgrade_plan{status = Status} ->
            {reply, {error, {invalid_status, Status}}, State}
    end;

handle_call({rollback, UpgradeId}, _From,
            State = #state{upgrade_plans = Plans}) ->
    case maps:get(UpgradeId, Plans, undefined) of
        undefined ->
            {reply, {error, not_found}, State};

        Plan = #upgrade_plan{
            status = failed,
            target_pid = Pid,
            checkpoint = Checkpoint
        } ->
            try
                ok = restore_checkpoint(Pid, Checkpoint),
                UpdatedPlan = Plan#upgrade_plan{status = rolled_back},
                NewState = State#state{
                    upgrade_plans = Plans#{UpgradeId => UpdatedPlan}
                },
                {reply, ok, NewState}
            catch
                _:Error:Stack ->
                    {reply, {error, {rollback_failed, Error, Stack}}, State}
            end;

        #upgrade_plan{status = Status} ->
            {reply, {error, {cannot_rollback, Status}}, State}
    end;

handle_call({checkpoint, Pid, Metadata}, _From,
            State = #state{checkpoints = Checkpoints}) ->
    try
        NetState = get_net_state(Pid),
        CheckpointId = generate_id(),

        Checkpoint = #checkpoint{
            id = CheckpointId,
            timestamp = erlang:system_time(millisecond),
            net_state = NetState,
            version = get_module_version(NetState#net_state.net_mod),
            metadata = Metadata
        },

        PidCheckpoints = maps:get(Pid, Checkpoints, []),
        UpdatedCheckpoints = trim_checkpoints([Checkpoint | PidCheckpoints],
                                              State#state.max_checkpoints),

        NewState = State#state{
            checkpoints = Checkpoints#{Pid => UpdatedCheckpoints}
        },

        {reply, {ok, CheckpointId}, NewState}
    catch
        _:Error:Stack ->
            {reply, {error, {checkpoint_failed, Error, Stack}}, State}
    end;

handle_call({verify_compatibility, OldModule, NewModule}, _From, State) ->
    Result = verify_compatibility_internal(OldModule, NewModule),
    {reply, Result, State};

handle_call({list_checkpoints, Pid}, _From,
            State = #state{checkpoints = Checkpoints}) ->
    PidCheckpoints = maps:get(Pid, Checkpoints, []),
    {reply, {ok, PidCheckpoints}, State};

handle_call({get_upgrade_status, UpgradeId}, _From,
            State = #state{upgrade_plans = Plans}) ->
    case maps:get(UpgradeId, Plans, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #upgrade_plan{status = Status} ->
            {reply, {ok, Status}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

-spec handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, #state{}}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(Info :: term(), State :: #state{}) ->
    {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(Reason :: term(), State :: #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(OldVsn :: term(), State :: #state{}, Extra :: term()) ->
    {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

-spec get_net_state(Pid :: pid()) -> #net_state{}.
get_net_state(Pid) ->
    Marking = gen_pnet:marking(Pid),
    UsrInfo = gen_pnet:usr_info(Pid),
    Stats = gen_pnet:stats(Pid),

    {status, _Pid, {module, gen_server}, SInfo} = sys:get_status(Pid),
    [_PDict, _SysState, _Parent, _Dbg, [_Header, _Data, {data, [{"State", NetState}]}]] = SInfo,

    NetState.

-spec perform_upgrade(Plan :: #upgrade_plan{}) -> ok | {error, term()}.
perform_upgrade(#upgrade_plan{
    target_pid = Pid,
    new_module = NewModule,
    old_version = OldVersion,
    new_version = NewVersion,
    migration_fun = MigrationFun,
    checkpoint = Checkpoint
}) ->
    OldNetState = Checkpoint#checkpoint.net_state,

    try
        NewNetState = migrate_state(OldNetState, NewModule, MigrationFun),

        ok = suspend_process(Pid),

        sys:replace_state(Pid, fun(_) -> NewNetState end),

        ok = sys:change_code(Pid, NewModule, OldVersion, {upgrade, NewVersion}),

        ok = resume_process(Pid),

        ok
    catch
        _:Error ->
            _ = resume_process(Pid),
            {error, Error}
    end.

-spec migrate_state(OldNetState :: #net_state{},
                   NewModule :: atom(),
                   MigrationFun :: fun()) -> #net_state{}.
migrate_state(OldNetState, NewModule, MigrationFun) ->
    MigratedNetState = MigrationFun(OldNetState),

    MigratedNetState#net_state{net_mod = NewModule}.

-spec restore_checkpoint(Pid :: pid(), Checkpoint :: #checkpoint{}) -> ok.
restore_checkpoint(Pid, #checkpoint{net_state = NetState}) ->
    ok = suspend_process(Pid),

    try
        sys:replace_state(Pid, fun(_) -> NetState end),
        ok = resume_process(Pid),
        ok
    catch
        _:Error ->
            _ = resume_process(Pid),
            error(Error)
    end.

-spec verify_compatibility_internal(OldModule :: atom(), NewModule :: atom()) ->
    compatibility_result().
verify_compatibility_internal(OldModule, NewModule) ->
    try
        OldPlaces = OldModule:place_lst(),
        NewPlaces = NewModule:place_lst(),

        OldTransitions = OldModule:trsn_lst(),
        NewTransitions = NewModule:trsn_lst(),

        RemovedPlaces = OldPlaces -- NewPlaces,
        RemovedTransitions = OldTransitions -- NewTransitions,

        case {RemovedPlaces, RemovedTransitions} of
            {[], []} ->
                compatible;
            {[], _} ->
                {incompatible, {removed_transitions, RemovedTransitions}};
            {_, []} ->
                {incompatible, {removed_places, RemovedPlaces}};
            {_, _} ->
                {incompatible, {removed_elements, RemovedPlaces, RemovedTransitions}}
        end
    catch
        _:Error ->
            {incompatible, {verification_error, Error}}
    end.

-spec get_module_version(Module :: atom()) -> term().
get_module_version(Module) ->
    case code:is_loaded(Module) of
        {file, _} ->
            Attrs = Module:module_info(attributes),
            proplists:get_value(vsn, Attrs, undefined);
        false ->
            undefined
    end.

-spec generate_id() -> binary().
generate_id() ->
    Rand = rand:uniform(16#FFFFFFFFFFFFFFFF),
    Time = erlang:system_time(nanosecond),
    <<Rand:64, Time:64>>.

-spec trim_checkpoints(Checkpoints :: [#checkpoint{}], Max :: pos_integer()) ->
    [#checkpoint{}].
trim_checkpoints(Checkpoints, Max) when length(Checkpoints) =< Max ->
    Checkpoints;
trim_checkpoints(Checkpoints, Max) ->
    lists:sublist(Checkpoints, Max).

-spec suspend_process(Pid :: pid()) -> ok.
suspend_process(Pid) ->
    sys:suspend(Pid),
    ok.

-spec resume_process(Pid :: pid()) -> ok.
resume_process(Pid) ->
    sys:resume(Pid),
    ok.
