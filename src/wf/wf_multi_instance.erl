%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Multi-Instance Task Module
%%
%% Provides N-out-of-M pattern support for workflow tasks.
%% Creates M parallel/sequential instances where Q must complete.
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_multi_instance).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([start_multi_instance/7]).
-export([get_state/2]).
-export([complete_instance/6]).
-export([cancel/2]).
-export([get_quorum_status/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Multi-instance execution mode.
%%
%% - parallel: All instances run simultaneously
%% - sequential: Instances run one at a time
%%--------------------------------------------------------------------
-type mi_mode() :: parallel | sequential.

%%--------------------------------------------------------------------
%% @doc Multi-instance task status.
%%--------------------------------------------------------------------
-type mi_status() :: running | quorum_met | completed | cancelled.

%%--------------------------------------------------------------------
%% @doc Multi-instance task state.
%%--------------------------------------------------------------------
-record(mi_task, {
    mi_id :: binary(),
    task :: atom(),
    case_id :: binary(),
    m :: pos_integer(),          %% Total instances
    q :: pos_integer(),          %% Quorum needed
    mode :: mi_mode(),
    instance_ids = [] :: [binary()],
    completed = [] :: [binary()],
    results = [] :: [term()],
    created_at :: integer(),
    status :: mi_status()
}).

-type mi_task() :: #mi_task{}.

%%--------------------------------------------------------------------
%% @doc Multi-instance registry state.
%%--------------------------------------------------------------------
-record(state, {
    tasks = #{} :: #{binary() => mi_task()}
}).

-export_type([mi_mode/0, mi_status/0, mi_task/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the multi-instance registry.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Starts a multi-instance task with N-out-of-M semantics.
%%
%% == Doctests ==
%%
%% Creating a multi-instance task with 3 instances, quorum of 2:
%% ```erlang
%% > wf_multi_instance:start_link(),
%% > {ok, MiId, WiIds} = wf_multi_instance:start_multi_instance(
%%     self(), <<"case1">>, review_task, 3, 2, parallel, 0
%% ).
%% {ok, _, _}
%% ```
%%
%% Getting multi-instance state:
%% ```erlang
%% > {ok, State} = wf_multi_instance:get_state(<<"case1">>, MiId),
%% > maps:get(status, State).
%% running
%% > maps:get(m, State).
%% 3
%% > maps:get(q, State).
%% 2
%% ```
%%
%% Completing instances to reach quorum:
%% ```erlang
%% > [Wi1, Wi2, Wi3] = WiIds,
%% > ok = wf_multi_instance:complete_instance(<<"case1">>, MiId, Wi1, #{result => approve}, self(), 0),
%% ok
%% > ok = wf_multi_instance:complete_instance(<<"case1">>, MiId, Wi2, #{result => approve}, self(), 0),
%% ok
%% > {ok, State2} = wf_multi_instance:get_state(<<"case1">>, MiId),
%% > maps:get(status, State2).
%% quorum_met
%% > maps:get(results, State2).
%% [#{result => approve}, #{result => approve}]
%% ```
%%
%% Sequential execution:
%% ```erlang
%% > {ok, MiId2, _} = wf_multi_instance:start_multi_instance(
%%     self(), <<"case2">>, sequential_task, 5, 3, sequential, 0
%% ),
%% > {ok, S} = wf_multi_instance:get_state(<<"case2">>, MiId2),
%% > maps:get(mode, S).
%% sequential
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec start_multi_instance(Engine :: pid() | atom(), CaseId :: binary(),
                           Task :: atom(), M :: pos_integer(), Q :: pos_integer(),
                           Mode :: mi_mode(), Now :: integer()) ->
          {ok, binary(), [binary()]} | {error, term()}.

start_multi_instance(Engine, CaseId, Task, M, Q, Mode, Now) when M >= Q, Q >= 1 ->
    gen_server:call(Engine, {start_multi_instance, CaseId, Task, M, Q, Mode, Now}).

%%--------------------------------------------------------------------
%% @doc Gets the current state of a multi-instance task.
%%
%% Returns a map with keys: mi_id, task, case_id, m, q, mode,
%% instance_ids, completed, results, status, created_at.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_state(CaseId :: binary(), MiId :: binary()) ->
          {ok, map()} | {error, term()}.

get_state(CaseId, MiId) ->
    gen_server:call(?MODULE, {get_state, CaseId, MiId}).

%%--------------------------------------------------------------------
%% @doc Completes a single instance within a multi-instance task.
%%
%% When Q instances are complete, status changes to quorum_met.
%% When all M instances are complete, status changes to completed.
%%
%% @end
%%--------------------------------------------------------------------
-spec complete_instance(CaseId :: binary(), MiId :: binary(),
                        InstanceWiId :: binary(), Data :: map(),
                        Engine :: pid() | atom(), Now :: integer()) ->
          ok | {error, term()}.

complete_instance(CaseId, MiId, InstanceWiId, Data, Engine, Now) ->
    gen_server:call(?MODULE, {complete_instance, CaseId, MiId, InstanceWiId, Data, Engine, Now}).

%%--------------------------------------------------------------------
%% @doc Cancels a multi-instance task.
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel(CaseId :: binary(), MiId :: binary()) -> ok | {error, term()}.

cancel(CaseId, MiId) ->
    gen_server:call(?MODULE, {cancel, CaseId, MiId}).

%%--------------------------------------------------------------------
%% @doc Gets quorum status for a multi-instance task.
%%
%% Returns {completed, required} tuple.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_quorum_status(CaseId :: binary(), MiId :: binary()) ->
          {pos_integer(), pos_integer()} | {error, term()}.

get_quorum_status(CaseId, MiId) ->
    gen_server:call(?MODULE, {get_quorum_status, CaseId, MiId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the multi-instance registry.
%%
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> {ok, #state{}}.

init([]) ->
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles synchronous calls.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: #state{}) ->
          {reply, term(), #state{}}.

handle_call({start_multi_instance, CaseId, Task, M, Q, Mode, Now}, _From, State) ->
    MiId = generate_mi_id(CaseId, Task),

    %% Create M instance work item IDs
    WiIds = [generate_wi_id(MiId, I) || I <- lists:seq(1, M)],

    MiTask = #mi_task{
        mi_id = MiId,
        task = Task,
        case_id = CaseId,
        m = M,
        q = Q,
        mode = Mode,
        instance_ids = WiIds,
        created_at = Now,
        status = running
    },

    Tasks = maps:put(MiId, MiTask, State#state.tasks),
    {reply, {ok, MiId, WiIds}, State#state{tasks = Tasks}};

handle_call({get_state, CaseId, MiId}, _From, State) ->
    case maps:get(MiId, State#state.tasks, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #mi_task{} = MiTask ->
            case MiTask#mi_task.case_id of
                CaseId ->
                    StateMap = #{
                        mi_id => MiTask#mi_task.mi_id,
                        task => MiTask#mi_task.task,
                        case_id => MiTask#mi_task.case_id,
                        m => MiTask#mi_task.m,
                        q => MiTask#mi_task.q,
                        mode => MiTask#mi_task.mode,
                        instance_ids => MiTask#mi_task.instance_ids,
                        completed => MiTask#mi_task.completed,
                        results => MiTask#mi_task.results,
                        status => MiTask#mi_task.status,
                        created_at => MiTask#mi_task.created_at
                    },
                    {reply, {ok, StateMap}, State};
                _ ->
                    {reply, {error, case_mismatch}, State}
            end
    end;

handle_call({complete_instance, CaseId, MiId, InstanceWiId, Data, _Engine, _Now}, _From, State) ->
    case maps:get(MiId, State#state.tasks, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #mi_task{status = cancelled} ->
            {reply, {error, cancelled}, State};
        #mi_task{status = completed} ->
            {reply, {error, already_complete}, State};
        #mi_task{case_id = CaseId, instance_ids = WiIds} = MiTask ->
            case lists:member(InstanceWiId, WiIds) of
                false ->
                    {reply, {error, invalid_instance}, State};
                true ->
                    case lists:member(InstanceWiId, MiTask#mi_task.completed) of
                        true ->
                            {reply, {error, already_completed}, State};
                        false ->
                            %% Add to completed and results
                            Completed1 = [InstanceWiId | MiTask#mi_task.completed],
                            Results1 = [Data | MiTask#mi_task.results],

                            %% Update status based on quorum
                            Status1 = case length(Completed1) >= MiTask#mi_task.q of
                                true -> quorum_met;
                                false -> running
                            end,

                            %% Check if all complete
                            Status2 = case length(Completed1) >= MiTask#mi_task.m of
                                true -> completed;
                                false -> Status1
                            end,

                            MiTask1 = MiTask#mi_task{
                                completed = Completed1,
                                results = Results1,
                                status = Status2
                            },

                            Tasks = maps:put(MiId, MiTask1, State#state.tasks),
                            {reply, ok, State#state{tasks = Tasks}}
                    end
            end;
        #mi_task{} ->
            {reply, {error, case_mismatch}, State}
    end;

handle_call({cancel, CaseId, MiId}, _From, State) ->
    case maps:get(MiId, State#state.tasks, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #mi_task{case_id = CaseId, status = completed} ->
            {reply, {error, already_complete}, State};
        #mi_task{case_id = CaseId} = MiTask ->
            MiTask1 = MiTask#mi_task{status = cancelled},
            Tasks = maps:put(MiId, MiTask1, State#state.tasks),
            {reply, ok, State#state{tasks = Tasks}};
        #mi_task{} ->
            {reply, {error, case_mismatch}, State}
    end;

handle_call({get_quorum_status, CaseId, MiId}, _From, State) ->
    case maps:get(MiId, State#state.tasks, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #mi_task{case_id = CaseId, completed = Completed, q = Q} ->
            {reply, {length(Completed), Q}, State};
        #mi_task{} ->
            {reply, {error, case_mismatch}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles asynchronous casts.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #state{}) ->
          {noreply, #state{}}.

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles non-gen_server messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: #state{}) ->
          {noreply, #state{}}.

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles code change during hot code upgrade.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: #state{}, Extra :: term()) ->
          {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Cleanup on termination.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), #state{}) -> ok.

terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique multi-instance ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_mi_id(binary(), atom()) -> binary().

generate_mi_id(CaseId, Task) ->
    Unique = crypto:hash(md5, term_to_binary({CaseId, Task, erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"mi_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique work item ID for an instance.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_wi_id(binary(), pos_integer()) -> binary().

generate_wi_id(MiId, Index) ->
    <<MiId/binary, "_i", (integer_to_binary(Index))/binary>>.
