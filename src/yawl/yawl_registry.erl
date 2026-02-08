%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% @doc Instance registry - workflow instance lookup by case_id.
%%
%% Infrastructure concern per Joe Armstrong's principles. Simple gen_server
%% with ETS table for case_id -> Pid mapping. No shared mutable state in
%% core runtime - each lookup is a message.
%%
%% <h3>Usage</h3>
%%
%% Register a workflow instance:
%% ```erlang
%% yawl_registry:register(CaseId, Pid).
%% ```
%%
%% Lookup by case_id:
%% ```erlang
%% {ok, Pid} = yawl_registry:lookup(CaseId).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_registry).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

-export([start_link/0]).
-export([register/2, unregister/1]).
-export([lookup/1, lookup_by_workflow/1]).
-export([list/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type case_id() :: binary() | string().
-type workflow_id() :: binary() | string().

%%====================================================================
%% Records
%%====================================================================

-record(state, {
    table :: ets:tid(),
    monitors :: ets:tid()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the registry.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Registers a workflow instance by case_id.
-spec register(case_id(), pid()) -> ok | {error, term()}.
register(CaseId, Pid) when is_pid(Pid) ->
    gen_server:call(?MODULE, {register, normalize_id(CaseId), Pid}).

%% @doc Unregisters a workflow instance.
-spec unregister(case_id()) -> ok.
unregister(CaseId) ->
    gen_server:cast(?MODULE, {unregister, normalize_id(CaseId)}).

%% @doc Looks up workflow instance by case_id.
-spec lookup(case_id()) -> {ok, pid()} | {error, not_found}.
lookup(CaseId) ->
    gen_server:call(?MODULE, {lookup, normalize_id(CaseId)}).

%% @doc Looks up workflow instances by workflow_id (spec id).
%% Returns list of {CaseId, Pid} for matching workflow.
-spec lookup_by_workflow(workflow_id()) -> {ok, [{case_id(), pid()}]}.
lookup_by_workflow(WorkflowId) ->
    gen_server:call(?MODULE, {lookup_by_workflow, normalize_id(WorkflowId)}).

%% @doc Lists all registered instances.
-spec list() -> [{case_id(), pid()}].
list() ->
    gen_server:call(?MODULE, list).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    Table = ets:new(?MODULE, [set, public, {read_concurrency, true}]),
    Monitors = ets:new(yawl_registry_monitors, [set, private]),
    {ok, #state{table = Table, monitors = Monitors}}.

handle_call({register, CaseId, Pid}, _From, State = #state{table = Table, monitors = Monitors}) ->
    ets:delete(Table, CaseId),
    MRef = erlang:monitor(process, Pid),
    true = ets:insert(Table, {CaseId, Pid}),
    true = ets:insert(Monitors, {MRef, CaseId}),
    {reply, ok, State};
handle_call({lookup, CaseId}, _From, State = #state{table = Table}) ->
    Reply = case ets:lookup(Table, CaseId) of
        [{CaseId, Pid}] when is_pid(Pid) ->
            case is_process_alive(Pid) of
                true -> {ok, Pid};
                false ->
                    ets:delete(Table, CaseId),
                    {error, not_found}
            end;
        [] ->
            {error, not_found}
    end,
    {reply, Reply, State};
handle_call({lookup_by_workflow, _WorkflowId}, _From, State) ->
    %% Simplified: workflow_id not stored in current schema
    {reply, {ok, []}, State};
handle_call(list, _From, State = #state{table = Table}) ->
    List = [{K, V} || {K, V} <- ets:tab2list(Table), is_pid(V), is_process_alive(V)],
    {reply, List, State}.

handle_cast({unregister, CaseId}, State = #state{table = Table, monitors = Monitors}) ->
    case ets:match_object(Monitors, {'_', CaseId}) of
        [{MRef, CaseId}] ->
            erlang:demonitor(MRef, [flush]),
            ets:delete(Monitors, MRef);
        [] -> ok
    end,
    ets:delete(Table, CaseId),
    {noreply, State}.

handle_info({'DOWN', MRef, process, _Pid, _Reason}, State = #state{table = Table, monitors = Monitors}) ->
    case ets:lookup(Monitors, MRef) of
        [{MRef, CaseId}] ->
            ets:delete(Monitors, MRef),
            ets:delete(Table, CaseId);
        [] -> ok
    end,
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal
%%====================================================================

normalize_id(Id) when is_binary(Id) -> Id;
normalize_id(Id) when is_list(Id) -> list_to_binary(Id);
normalize_id(Id) when is_atom(Id) -> atom_to_binary(Id).
