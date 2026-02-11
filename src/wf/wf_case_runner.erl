%% -*- erlang -*-
%% @doc Workflow Case Runner
%%
%% Gen_server that wraps workflow execution engines (gen_yawl, wf_engine, custom).
%% Provides uniform interface for case lifecycle management.
%% @end

-module(wf_case_runner).
-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([get_info/1]).
-export([cancel/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%% Types
-type case_type() :: gen_yawl | wf_engine | custom.
-type case_status() :: pending | running | completed | cancelled | failed.

-record(case_state, {
    case_id :: binary(),
    spec_id :: binary() | atom(),
    case_type :: case_type(),
    status :: case_status(),
    workflow_pid :: pid() | undefined,
    started_at :: integer(),
    completed_at :: integer() | undefined,
    data :: map(),
    options :: map()
}).

-type state() :: #case_state{}.

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts a case runner.
-spec start_link(SpecId, InitialData, Options) -> {ok, pid()} | {error, term()} when
      SpecId :: binary() | atom(),
      InitialData :: map(),
      Options :: map().
start_link(SpecId, InitialData, Options) ->
    gen_server:start_link(?MODULE, [SpecId, InitialData, Options], []).

%% @doc Gets case information.
-spec get_info(pid()) -> {ok, map()} | {error, term()}.
get_info(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, get_info, infinity).

%% @doc Cancels a running case.
-spec cancel(pid()) -> ok | {error, term()}.
cancel(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, cancel, infinity).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init([term()]) -> {ok, state()}.
init([SpecId, InitialData, Options]) ->
    CaseId = generate_case_id(),
    CaseType = maps:get(type, Options, gen_yawl),
    Now = erlang:system_time(millisecond),

    State = #case_state{
        case_id = CaseId,
        spec_id = SpecId,
        case_type = CaseType,
        status = pending,
        started_at = Now,
        data = InitialData,
        options = Options
    },

    % Register with gproc
    gproc:reg_local_name({wf_case, CaseId}, CaseId),
    gproc:reg_local_prop({wf_case, CaseId}, spec_id, SpecId),
    gproc:reg_local_prop({wf_case, CaseId}, started_at, Now),

    % Start workflow based on type
    case start_workflow(CaseType, SpecId, CaseId, InitialData, Options) of
        {ok, WfPid} ->
            {ok, State#case_state{workflow_pid = WfPid, status = running}};
        {error, Reason} ->
            % Cleanup on error
            gproc:unreg_local_name({wf_case, CaseId}),
            gproc:unreg_local_prop({wf_case, CaseId}, spec_id),
            gproc:unreg_local_prop({wf_case, CaseId}, started_at),
            {stop, Reason}
    end.

%% @private
-spec handle_call(term(), {pid(), term()}, state()) ->
    {reply, term(), state()} | {stop, normal, ok, state()}.
handle_call(get_info, _From, #case_state{} = State) ->
    Info = #{
        case_id => State#case_state.case_id,
        spec_id => State#case_state.spec_id,
        type => State#case_state.case_type,
        status => State#case_state.status,
        started_at => State#case_state.started_at,
        completed_at => State#case_state.completed_at,
        workflow_pid => State#case_state.workflow_pid
    },
    {reply, {ok, Info}, State};

handle_call(cancel, _From, #case_state{workflow_pid = undefined} = State) ->
    {reply, {error, not_running}, State};
handle_call(cancel, _From, #case_state{workflow_pid = WfPid, status = running} = State) ->
    case State#case_state.case_type of
        gen_yawl ->
            gen_yawl:stop(WfPid);
        wf_engine ->
            %% wf_engine doesn't have per-case processes, signal via engine
            ok;
        custom ->
            %% Try graceful shutdown
            exit(WfPid, shutdown)
    end,
    {reply, ok, State#case_state{status = cancelled}};
handle_call(cancel, _From, State) ->
    {reply, {error, invalid_status}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%% @private
-spec handle_cast(term(), state()) -> {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), state()) -> {noreply, state()}.
handle_info({'EXIT', WfPid, Reason}, #case_state{workflow_pid = WfPid} = State) ->
    case Reason of
        normal ->
            {noreply, State#case_state{status = completed,
                                       completed_at = erlang:system_time(millisecond)}};
        shutdown ->
            {noreply, State#case_state{status = cancelled,
                                       completed_at = erlang:system_time(millisecond)}};
        _ ->
            {noreply, State#case_state{status = failed}}
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
-spec terminate(term(), state()) -> ok.
terminate(_Reason, #case_state{case_id = CaseId}) ->
    % Unregister from gproc
    catch gproc:unreg_local_name({wf_case, CaseId}),
    catch gproc:unreg_local_prop({wf_case, CaseId}, spec_id),
    catch gproc:unreg_local_prop({wf_case, CaseId}, started_at),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec start_workflow(case_type(), term(), binary(), map(), map()) ->
    {ok, pid()} | {error, term()}.
start_workflow(gen_yawl, SpecId, _CaseId, _InitialData, Options) ->
    NetMod = SpecId,  % SpecId is the net module for gen_yawl
    NetArg = maps:get(net_arg, Options, #{}),
    GenYawlOptions = maps:get(gen_yawl_options, Options, []),
    try
        % Start gen_yawl process - it will be linked automatically
        {ok, WfPid} = gen_yawl:start_link(undefined, NetMod, NetArg, GenYawlOptions),
        {ok, WfPid}
    catch
        _:Error -> {error, Error}
    end;

start_workflow(wf_engine, _SpecId, _CaseId, _InitialData, _Options) ->
    %% For wf_engine, we'd need to create a case within the engine
    %% This is deferred - wf_engine cases remain unsupervised at process level
    {error, wf_engine_not_supported};

start_workflow(custom, SpecId, CaseId, InitialData, Options) ->
    %% Custom implementations provide their own start_module/start_function
    case {maps:get(start_module, Options, undefined), maps:get(start_function, Options, undefined)} of
        {Mod, Fun} when Mod =/= undefined, Fun =/= undefined ->
            try
                apply(Mod, Fun, [SpecId, CaseId, InitialData, Options])
            catch
                _:Error -> {error, {custom_workflow_failed, Error}}
            end;
        _ ->
            {error, {missing_custom_config, start_module, start_function}}
    end.

%% @private
-spec generate_case_id() -> binary().
generate_case_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"case_", Hex/binary>>.
