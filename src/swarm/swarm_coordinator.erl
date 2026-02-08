%% -*- erlang -*-
%%%% @doc swarm_coordinator - Central coordinator for swarm operations.
%%
%% This module provides global swarm coordination for multi-agent autonomous
%% workflow execution. It manages agent discovery, consensus protocols,
%% leader election, and incident handling.
%%
%% <h3>Responsibilities</h3>
%%
%% <ul>
%%   <li><b>Agent Registration:</b> Track all swarm agents</li>
%%   <li><b>Consensus:</b> Coordinate decision-making protocols</li>
%%   <li><b>Leader Election:</b> Manage cluster leadership</li>
%%   <li><b>Incident Handling:</b> Detect and respond to failures</li>
%%   <li><b>Work Distribution:</b> Assign work to agents</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(swarm_coordinator).
-author("CRE Team").

-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% Coordinator API
-export([start_link/0]).
-export([register_agent/2]).
-export([unregister_agent/1]).
-export([list_agents/0]).
-export([get_agents_by_role/1]).
-export([request_consensus/3]).
-export([elect_leader/0]).
-export([distribute_work/1]).
-export([report_incident/2]).
-export([get_cluster_state/0]).

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

-record(agent_info, {
    agent_id :: binary(),
    pid :: pid(),
    role :: binary(),
    capabilities = [] :: [binary()],
    status :: idle | busy | recovery | stopped,
    current_work :: undefined | term(),
    last_heartbeat :: integer()
}).

-record(consensus_state, {
    proposal_id :: binary(),
    participants = [] :: [binary()],
    votes = #{} :: map(),
    required :: unanimous | majority | supermajority | {weighted, number()},
    timeout_ref :: undefined | reference()
}).

-record(swarm_state, {
    agents = #{} :: #{binary() => #agent_info{}},
    consensus_states = #{} :: #{binary() => #consensus_state{}},
    leader :: binary() | undefined,
    incident_history = [] :: [tuple()],
    stats = #{} :: map()
}).

%%====================================================================
%% Types
%%====================================================================

-type agent_info() :: #agent_info{}.
-type consensus_state() :: #consensus_state{}.
-type swarm_state() :: #swarm_state{}.
-type consensus_type() :: unanimous | majority | supermajority | {weighted, number()}.
-type incident_type() :: agent_failure | timeout | deadlock | resource_exhaustion.

-export_type([agent_info/0, consensus_type/0, incident_type/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the swarm coordinator.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Registers an agent with the swarm.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_agent(binary(), #agent_info{}) -> ok | {error, term()}.

register_agent(AgentId, AgentInfo) when is_binary(AgentId), is_record(AgentInfo, agent_info) ->
    gen_server:call(?MODULE, {register_agent, AgentId, AgentInfo}).

%%--------------------------------------------------------------------
%% @doc Unregisters an agent from the swarm.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_agent(binary()) -> ok.

unregister_agent(AgentId) when is_binary(AgentId) ->
    gen_server:call(?MODULE, {unregister_agent, AgentId}).

%%--------------------------------------------------------------------
%% @doc Lists all registered agents.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_agents() -> [binary()].

list_agents() ->
    gen_server:call(?MODULE, list_agents).

%%--------------------------------------------------------------------
%% @doc Gets agents filtered by role.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_agents_by_role(binary()) -> [binary()].

get_agents_by_role(Role) when is_binary(Role) ->
    gen_server:call(?MODULE, {get_agents_by_role, Role}).

%%--------------------------------------------------------------------
%% @doc Requests a consensus decision from the swarm.
%%
%% @param ProposalId Unique identifier for this proposal
%% @param Type Consensus type (unanimous, majority, supermajority, weighted)
%% @param Timeout Milliseconds to wait for consensus
%% @return {ok, agree} | {ok, disagree} | {error, timeout}
%%
%% @end
%%--------------------------------------------------------------------
-spec request_consensus(binary(), consensus_type(), timeout()) ->
          {ok, agree} | {ok, disagree} | {error, timeout}.

request_consensus(ProposalId, Type, Timeout) when is_binary(ProposalId) ->
    gen_server:call(?MODULE, {request_consensus, ProposalId, Type}, Timeout).

%%--------------------------------------------------------------------
%% @doc Triggers leader election.
%%
%% @end
%%--------------------------------------------------------------------
-spec elect_leader() -> {ok, binary()} | {error, term()}.

elect_leader() ->
    gen_server:call(?MODULE, elect_leader, 10000).

%%--------------------------------------------------------------------
%% @doc Distributes work items to available agents.
%%
%% @end
%%--------------------------------------------------------------------
-spec distribute_work([term()]) -> {ok, [{binary(), term()}]} | {error, term()}.

distribute_work(WorkItems) when is_list(WorkItems) ->
    gen_server:call(?MODULE, {distribute_work, WorkItems}, 5000).

%%--------------------------------------------------------------------
%% @doc Reports an incident to the swarm.
%%
%% @end
%%--------------------------------------------------------------------
-spec report_incident(incident_type(), map()) -> ok.

report_incident(Type, Details) ->
    gen_server:cast(?MODULE, {report_incident, Type, Details}).

%%--------------------------------------------------------------------
%% @doc Gets the current cluster state.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_cluster_state() -> {ok, map()}.

get_cluster_state() ->
    gen_server:call(?MODULE, get_cluster_state).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, #swarm_state{}}.

init([]) ->
    logger:info("Swarm coordinator starting"),
    %% Start heartbeat timer
    erlang:send_after(5000, self(), check_heartbeats),
    {ok, #swarm_state{
        stats = #{started_at => erlang:system_time(millisecond)}
    }}.

%% @private
-spec handle_call(term(), {pid(), term()}, #swarm_state{}) ->
          {reply, term(), #swarm_state{}}.

handle_call({register_agent, AgentId, AgentInfo}, _From, State) ->
    case maps:is_key(AgentId, State#swarm_state.agents) of
        true ->
            {reply, {error, already_registered}, State};
        false ->
            Agents1 = maps:put(AgentId, AgentInfo, State#swarm_state.agents),
            logger:info("Agent ~s registered with role ~s", [AgentId, AgentInfo#agent_info.role]),
            {reply, ok, State#swarm_state{agents = Agents1}}
    end;

handle_call({unregister_agent, AgentId}, _From, State) ->
    Agents1 = maps:remove(AgentId, State#swarm_state.agents),
    logger:info("Agent ~s unregistered", [AgentId]),
    {reply, ok, State#swarm_state{agents = Agents1}};

handle_call(list_agents, _From, State) ->
    AgentIds = maps:keys(State#swarm_state.agents),
    {reply, AgentIds, State};

handle_call({get_agents_by_role, Role}, _From, State) ->
    Filtered = maps:fold(
        fun(AgentId, #agent_info{role = R}, Acc) when R =:= Role ->
                [AgentId | Acc];
           (_, _, Acc) ->
                Acc
        end,
        [],
        State#swarm_state.agents
    ),
    {reply, lists:reverse(Filtered), State};

handle_call({request_consensus, ProposalId, Type}, _From, State) ->
    AgentIds = maps:keys(State#swarm_state.agents),
    case length(AgentIds) of
        0 ->
            {reply, {error, no_agents}, State};
        _ ->
            %% Create consensus state
            ConsensusState = #consensus_state{
                proposal_id = ProposalId,
                participants = AgentIds,
                votes = #{},
                required = Type
            },
            ConsensusStates1 = maps:put(ProposalId, ConsensusState, State#swarm_state.consensus_states),

            %% For now, immediate consensus with single agent
            Result = case {length(AgentIds), Type} of
                {1, _} -> {ok, agree};
                {N, unanimous} when N > 1 -> {error, not_implemented};
                {N, majority} when N > 1 -> {error, not_implemented};
                _ -> {error, not_implemented}
            end,

            {reply, Result, State#swarm_state{consensus_states = ConsensusStates1}}
    end;

handle_call(elect_leader, _From, State = #swarm_state{agents = Agents}) ->
    AgentIds = maps:keys(Agents),
    Leader = case AgentIds of
        [] -> undefined;
        _ ->
            %% Simple deterministic selection: lowest ID (lexicographic)
            lists:min(AgentIds)
    end,
    State1 = State#swarm_state{leader = Leader},
    logger:info("Leader elected: ~s", [Leader]),
    {reply, {ok, Leader}, State1};

handle_call({distribute_work, WorkItems}, _From, State = #swarm_state{agents = Agents}) ->
    %% Find idle agents
    IdleAgents = maps:fold(
        fun(AgentId, #agent_info{status = idle}, Acc) ->
                [AgentId | Acc];
           (_, _, Acc) ->
                Acc
        end,
        [],
        Agents
    ),

    case IdleAgents of
        [] ->
            {reply, {error, no_available_agents}, State};
        _ ->
            %% Distribute work round-robin
            NumAgents = length(IdleAgents),
            Assignments = lists:zipwith(
                fun(AgentId, Work) -> {AgentId, Work} end,
                cycle_list(IdleAgents, length(WorkItems)),
                WorkItems
            ),
            logger:info("Distributed ~p work items to ~p agents", [length(WorkItems), NumAgents]),
            {reply, {ok, Assignments}, State}
    end;

handle_call(get_cluster_state, _From, State) ->
    Summary = #{
        agent_count => maps:size(State#swarm_state.agents),
        leader => State#swarm_state.leader,
        active_consensus => maps:size(State#swarm_state.consensus_states),
        incidents => length(State#swarm_state.incident_history),
        stats => State#swarm_state.stats
    },
    {reply, {ok, Summary}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #swarm_state{}) -> {noreply, #swarm_state{}}.

handle_cast({report_incident, Type, Details}, State = #swarm_state{incident_history = History}) ->
    Incident = {erlang:system_time(millisecond), Type, Details},
    logger:warning("Incident reported: ~p ~p", [Type, Details]),
    {noreply, State#swarm_state{incident_history = [Incident | History]}};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #swarm_state{}) -> {noreply, #swarm_state{}}.

handle_info(check_heartbeats, State = #swarm_state{agents = Agents}) ->
    %% Check for stale agents
    Now = erlang:system_time(millisecond),
    Timeout = 30000, %% 30 second timeout

    StaleAgents = maps:fold(
        fun(AgentId, #agent_info{last_heartbeat = LastHeartbeat}, Acc) ->
                case Now - LastHeartbeat > Timeout of
                    true -> [AgentId | Acc];
                    false -> Acc
                end
        end,
        [],
        Agents
    ),

    State1 = lists:foldl(
        fun(AgentId, Acc) ->
                logger:warning("Agent ~s heartbeat timeout, marking as failed", [AgentId]),
                Agents = Acc#swarm_state.agents,
                case maps:get(AgentId, Agents, undefined) of
                    undefined -> Acc;
                    AgentInfo ->
                        AgentInfo1 = AgentInfo#agent_info{status = recovery},
                        Acc#swarm_state{agents = maps:put(AgentId, AgentInfo1, Agents)}
                end
        end,
        State,
        StaleAgents
    ),

    %% Schedule next check
    erlang:send_after(5000, self(), check_heartbeats),
    {noreply, State1};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec code_change(term(), #swarm_state{}, term()) -> {ok, #swarm_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
-spec terminate(term(), #swarm_state{}) -> ok.

terminate(_Reason, _State) ->
    logger:info("Swarm coordinator stopping"),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% Cycles a list to the required length
-spec cycle_list([T], non_neg_integer()) -> [T].

cycle_list([], _N) ->
    [];
cycle_list(List, N) when length(List) >= N ->
    lists:sublist(List, N);
cycle_list(List, N) ->
    %% Repeat the list to reach N elements
    cycle_list(List ++ List, N).
