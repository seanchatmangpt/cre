%%%-------------------------------------------------------------------
%%% @doc
%%% SOC 2 Control Executor Supervisor
%%%
%%% Manages individual control validator processes. Each SOC 2 control
%%% (CC6.1, CC7.1, etc.) runs as an independent gen_server.
%%%
%%% One-for-one strategy: If one control validator crashes, others
%%% continue running. This is critical for partial compliance proof.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_control_executor_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([get_all_statuses/0]).
-export([validate_now/1]).
-export([get_control_pid/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% SOC 2 controls to validate (from ontology)
-define(CONTROLS, [
    <<"CC6.1">>,  % Logical and physical access controls
    <<"CC7.1">>,  % Detection and monitoring
    <<"CC8.1">>,  % Change management
    <<"CC9.1">>,  % Risk mitigation
    <<"PI1.1">>, % Processing quality
    <<"C1.1">>,   % Confidentiality
    <<"P1.1">>    % Privacy
]).

%%%===================================================================
%%% API functions
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec get_all_statuses() -> #{binary() := #{status := pass | fail, last_validated := calendar:datetime()}}.
get_all_statuses() ->
    maps:from_list([
        {ControlId, soc2_control_executor:get_status(ControlId)}
        || ControlId <- ?CONTROLS
    ]).

-spec validate_now(binary()) -> {ok, map()} | {error, term()}.
validate_now(ControlId) ->
    case get_control_pid(ControlId) of
        {ok, Pid} ->
            soc2_control_executor:validate_now(Pid);
        {error, _} = Error ->
            Error
    end.

-spec get_control_pid(binary()) -> {ok, pid()} | {error, not_found}.
get_control_pid(ControlId) ->
    case whereis(control_executor_name(ControlId)) of
        undefined -> {error, not_found};
        Pid -> {ok, Pid}
    end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    logger:info(#{
        what => soc2_control_executor_supervisor_starting,
        controls => ?CONTROLS
    }),

    SupFlags = #{
        strategy => one_for_one,  % Independent control validators
        intensity => 10,
        period => 60
    },

    %% Create one worker per SOC 2 control
    ChildSpecs = [
        #{
            id => {soc2_control_executor, ControlId},
            start => {soc2_control_executor, start_link, [ControlId]},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [soc2_control_executor]
        }
        || ControlId <- ?CONTROLS
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

control_executor_name(ControlId) ->
    %% e.g., <<"CC6.1">> -> soc2_control_executor_cc6_1
    SafeId = binary:replace(ControlId, <<".">>, <<"_">>, [global]),
    binary_to_atom(<<"soc2_control_executor_", (string:lowercase(SafeId))/binary>>).
