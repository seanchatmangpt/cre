%% -*- erlang -*-
%%%% @doc swarm_agent - Agent gen_server behavior for swarm operations.
%%
%% This module implements the agent behavior for autonomous workflow execution.
%% Agents are workflow stations that execute autonomously through coordinated
%% decision-making.
%%
%% <h3>Agent States</h3>
%%
%% <ul>
%%   <li><b>idle:</b> Agent ready for work assignment</li>
%%   <li><b>busy:</b> Agent executing a task</li>
%%   <li><b>recovery:</b> Agent recovering from failure</li>
%%   <li><b>stopped:</b> Agent not accepting work</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(swarm_agent).
-author("CRE Team").

-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Agent API
-export([start_link/3]).
-export([start/3]).
-export([execute_task/2]).
-export([get_status/1]).
-export([stop/1]).
-export([send_message/2]).
-export([broadcast_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(agent_state, {
    agent_id :: binary(),
    role :: binary(),
    capabilities = [] :: [binary()],
    status = idle :: idle | busy | recovery | stopped,
    current_work :: undefined | work_item(),
    swarm_coordinator :: pid() | undefined,
    decision_module :: atom(),
    metrics :: #{
        tasks_completed => non_neg_integer(),
        tasks_failed => non_neg_integer(),
        total_duration_us => non_neg_integer()
    }
}).

-record(work_item, {
    id :: binary(),
    type :: binary(),
    payload :: term(),
    priority :: normal | high | urgent
}).

%%--------------------------------------------------------------------
%% @doc Agent Information Record (for swarm coordinator registration)
%%--------------------------------------------------------------------
-record(agent_info, {
    agent_id :: binary(),
    pid :: pid(),
    role :: binary(),
    capabilities = [] :: [binary()],
    status :: idle | busy | recovery | stopped,
    current_work :: undefined | work_item(),
    last_heartbeat :: integer()
}).

%%====================================================================
%% Types
%%====================================================================

-type agent_state() :: #agent_state{}.
-type work_item() :: #work_item{}.
-type agent_info() :: #agent_info{}.
-type agent_status() :: idle | busy | recovery | stopped.
-type agent_role() :: worker | coordinator | validator | observer.

-export_type([agent_state/0, work_item/0, agent_info/0, agent_status/0, agent_role/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts a swarm agent.
%%
%% @param Role Agent role
%% @param Capabilities List of capability identifiers
%% @param Options Agent options
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(agent_role(), [binary()], [proplists:property()]) ->
          {ok, pid()} | {error, term()}.

start_link(Role, Capabilities, Options) when is_list(Capabilities), is_list(Options) ->
    AgentId = generate_agent_id(),
    gen_server:start_link(?MODULE, {AgentId, Role, Capabilities, Options}, []).

%%--------------------------------------------------------------------
%% @doc Starts an agent without linking.
%%
%% @end
%%--------------------------------------------------------------------
-spec start(agent_role(), [binary()], [proplists:property()]) ->
          {ok, pid()} | {error, term()}.

start(Role, Capabilities, Options) ->
    AgentId = generate_agent_id(),
    gen_server:start(?MODULE, {AgentId, Role, Capabilities, Options}, []).

%%--------------------------------------------------------------------
%% @doc Assigns and executes a task on the agent.
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_task(pid(), work_item()) -> ok | {error, term()}.

execute_task(AgentPid, WorkItem) when is_pid(AgentPid), is_record(WorkItem, work_item) ->
    gen_server:call(AgentPid, {execute_task, WorkItem}, 30000).

%%--------------------------------------------------------------------
%% @doc Gets the current status of an agent.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_status(pid()) -> {ok, agent_status(), map()} | {error, term()}.

get_status(AgentPid) when is_pid(AgentPid) ->
    gen_server:call(AgentPid, get_status, 5000).

%%--------------------------------------------------------------------
%% @doc Stops an agent gracefully.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(pid()) -> ok.

stop(AgentPid) when is_pid(AgentPid) ->
    gen_server:stop(AgentPid, normal, 5000).

%%--------------------------------------------------------------------
%% @doc Sends a direct message to an agent.
%%
%% @end
%%--------------------------------------------------------------------
-spec send_message(pid(), term()) -> ok.

send_message(AgentPid, Message) when is_pid(AgentPid) ->
    gen_server:cast(AgentPid, {message, Message}).

%%--------------------------------------------------------------------
%% @doc Broadcasts a message to all agents.
%%
%% @end
%%--------------------------------------------------------------------
-spec broadcast_message(term(), atom()) -> ok.

broadcast_message(Message, Role) ->
    pg:get_members(swarm_agents, Role),
    lists:foreach(
        fun(Pid) ->
            send_message(Pid, Message)
        end,
        pg:get_members(swarm_agents, Role)
    ),
    ok.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init({binary(), agent_role(), [binary()], [proplists:property()]}) ->
          {ok, #agent_state{}}.

init({AgentId, Role, Capabilities, Options}) ->
    RoleBin = role_to_binary(Role),
    State = #agent_state{
        agent_id = AgentId,
        role = RoleBin,
        capabilities = Capabilities,
        status = idle,
        swarm_coordinator = proplists:get_value(coordinator, Options),
        decision_module = proplists:get_value(decision_module, Options, swarm_decision),
        metrics = #{
            tasks_completed => 0,
            tasks_failed => 0,
            total_duration_us => 0
        }
    },

    %% Join process group for role-based messaging
    try
        pg:join(swarm_agents, RoleBin),
        pg:join(swarm_agents, all_agents)
    catch
        error:badarg ->
            %% pg not started, ignore
            ok
    end,

    %% Register with swarm coordinator
    Coordinator = State#agent_state.swarm_coordinator,
    case Coordinator of
        undefined ->
            ok;
        _ ->
            AgentInfo = #agent_info{
                agent_id = AgentId,
                pid = self(),
                role = RoleBin,
                capabilities = Capabilities,
                status = idle,
                last_heartbeat = erlang:system_time(millisecond)
            },
            swarm_coordinator:register_agent(AgentId, AgentInfo)
    end,

    %% Start heartbeat timer
    erlang:send_after(5000, self(), heartbeat),

    logger:info("Agent ~s started with role ~s", [AgentId, RoleBin]),
    {ok, State}.

%% @private
-spec handle_call(term(), {pid(), term()}, #agent_state{}) ->
          {reply, term(), #agent_state{}}.

handle_call({execute_task, WorkItem}, _From, State = #agent_state{
        status = Status,
        current_work = CurrentWork
    }) ->
    case Status of
        busy ->
            {reply, {error, agent_busy}, State};
        _ when CurrentWork =/= undefined ->
            {reply, {error, has_work}, State};
        _ ->
            %% Start task execution
            StartTime = erlang:monotonic_time(microsecond),
            case execute_work_item(WorkItem, State) of
                {ok, Result} ->
                    EndTime = erlang:monotonic_time(microsecond),
                    Duration = EndTime - StartTime,

                    %% Update metrics
                    Metrics = State#agent_state.metrics,
                    Metrics1 = Metrics#{
                        tasks_completed => maps:get(tasks_completed, Metrics, 0) + 1,
                        total_duration_us => maps:get(total_duration_us, Metrics, 0) + Duration
                    },

                    State1 = State#agent_state{
                        status = idle,
                        current_work = undefined,
                        metrics = Metrics1
                    },

                    {reply, {ok, Result}, State1};
                {error, Reason} ->
                    %% Update failure metrics
                    Metrics = State#agent_state.metrics,
                    Metrics1 = Metrics#{
                        tasks_failed => maps:get(tasks_failed, Metrics, 0) + 1
                    },

                    State1 = State#agent_state{
                        status = idle,
                        current_work = undefined,
                        metrics = Metrics1
                    },

                    {reply, {error, Reason}, State1}
            end
    end;

handle_call(get_status, _From, State = #agent_state{
        status = Status,
        current_work = CurrentWork,
        metrics = Metrics
    }) ->
    Info = #{
        status => Status,
        current_work => CurrentWork,
        metrics => Metrics
    },
    {reply, {ok, Status, Info}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #agent_state{}) -> {noreply, #agent_state{}}.

handle_cast({message, Message}, State) ->
    logger:debug("Agent ~s received message: ~p", [State#agent_state.agent_id, Message]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #agent_state{}) -> {noreply, #agent_state{}}.

handle_info(heartbeat, State = #agent_state{swarm_coordinator = Coordinator, agent_id = AgentId}) ->
    %% Send heartbeat to coordinator
    case Coordinator of
        undefined ->
            ok;
        _ ->
            AgentInfo = #agent_info{
                agent_id = AgentId,
                pid = self(),
                role = State#agent_state.role,
                capabilities = State#agent_state.capabilities,
                status = State#agent_state.status,
                last_heartbeat = erlang:system_time(millisecond)
            },
            swarm_coordinator:register_agent(AgentId, AgentInfo)
    end,

    %% Schedule next heartbeat
    erlang:send_after(5000, self(), heartbeat),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec code_change(term(), #agent_state{}, term()) -> {ok, #agent_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
-spec terminate(term(), #agent_state{}) -> ok.

terminate(_Reason, #agent_state{agent_id = AgentId, role = Role}) ->
    logger:info("Agent ~s terminating", [AgentId]),
    try
        pg:leave(swarm_agents, Role),
        pg:leave(swarm_agents, all_agents)
    catch
        error:badarg -> ok
    end,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec execute_work_item(work_item(), #agent_state{}) ->
          {ok, term()} | {error, term()}.

execute_work_item(#work_item{id = Id, type = Type, payload = Payload}, State) ->
    logger:info("Agent ~s executing task ~s of type ~s",
               [State#agent_state.agent_id, Id, Type]),

    %% Check capability
    Capabilities = State#agent_state.capabilities,
    case lists:member(Type, Capabilities) of
        false ->
            {error, {no_capability, Type}};
        true ->
            %% Execute based on decision module
            DecisionModule = State#agent_state.decision_module,
            try
                Result = DecisionModule:execute(Type, Payload),
                {ok, #{
                    task_id => Id,
                    result => Result,
                    completed_at => erlang:system_time(millisecond)
                }}
            catch
                Type:Error:Stack ->
                    logger:error("Task execution error: ~p:~p~n~p", [Type, Error, Stack]),
                    {error, {execution_failed, Type, Error}}
            end
    end.

%% @private
-spec role_to_binary(agent_role()) -> binary().

role_to_binary(worker) -> <<"worker">>;
role_to_binary(coordinator) -> <<"coordinator">>;
role_to_binary(validator) -> <<"validator">>;
role_to_binary(observer) -> <<"observer">>;
role_to_binary(Role) when is_atom(Role) -> atom_to_binary(Role);
role_to_binary(Role) when is_binary(Role) -> Role.

%% @private
-spec generate_agent_id() -> binary().

generate_agent_id() ->
    Time = erlang:system_time(microsecond),
    Node = erlang:phash2(node()),
    <<Time:48, Node:16>>.
