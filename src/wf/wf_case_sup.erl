%% -*- erlang -*-
%% @doc Workflow Case Supervisor
%%
%% Unified supervisor for workflow case execution across multiple engines.
%% Provides fault tolerance, clean lifecycle, and case registry.
%% @end

-module(wf_case_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_case/3, stop_case/1, stop_case/2]).
-export([list_cases/0, find_case/1, get_case_status/1]).
-export([case_count/0]).

%% Supervisor callbacks
-export([init/1]).

%% Types
-type case_id() :: binary().
-type spec_id() :: binary() | atom().
-type case_options() :: #{
    type => gen_yawl | wf_engine | custom,
    timeout => pos_integer() | infinity,
    auto_continue => boolean()
}.
-type case_info() :: #{
    case_id => case_id(),
    spec_id => spec_id(),
    pid => pid() | undefined,
    status => running | completed | cancelled | failed,
    started_at => integer()
}.

-export_type([case_id/0, spec_id/0, case_options/0, case_info/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the workflow case supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts a new workflow case under supervision.
-spec start_case(SpecId, InitialData, Options) -> {ok, pid()} | {error, term()} when
      SpecId :: spec_id(),
      InitialData :: map(),
      Options :: case_options().
start_case(SpecId, InitialData, Options) ->
    supervisor:start_child(?MODULE, [SpecId, InitialData, Options]).

%% @doc Stops a workflow case gracefully.
-spec stop_case(case_id()) -> ok | {error, term()}.
stop_case(CaseId) ->
    stop_case(CaseId, 5000).

%% @doc Stops a workflow case with timeout.
-spec stop_case(case_id(), pos_integer()) -> ok | {error, term()}.
stop_case(CaseId, Timeout) ->
    case find_case(CaseId) of
        {ok, Pid} ->
            % Use exit with timeout for graceful shutdown
            try
                Mref = monitor(process, Pid),
                exit(Pid, shutdown),
                receive
                    {'DOWN', Mref, process, Pid, _Info} -> ok
                after Timeout ->
                    erlang:demonitor(Mref, [flush]),
                    exit(Pid, kill),
                    ok
                end
            catch
                _:_ -> {error, not_found}
            end;
        Error ->
            Error
    end.

%% @doc Lists all active cases.
-spec list_cases() -> [case_info()].
list_cases() ->
    Children = supervisor:which_children(?MODULE),
    lists:filtermap(
        fun({_Id, Pid, _Type, _Modules}) ->
            case Pid of
                undefined -> false;
                _ when is_pid(Pid) ->
                    try
                        {ok, Info} = wf_case_runner:get_info(Pid),
                        {true, Info}
                    catch
                        _:_ -> false
                    end
            end
        end,
        Children
    ).

%% @doc Finds a case by ID.
-spec find_case(case_id()) -> {ok, pid()} | {error, not_found}.
find_case(CaseId) ->
    case gproc:lookup_local_name({wf_case, CaseId}) of
        {Pid, _Value} -> {ok, Pid};
        undefined -> {error, not_found}
    end.

%% @doc Gets the current status of a case.
-spec get_case_status(case_id()) -> {ok, case_info()} | {error, not_found}.
get_case_status(CaseId) ->
    case find_case(CaseId) of
        {ok, Pid} ->
            try
                {ok, Info} = wf_case_runner:get_info(Pid),
                {ok, Info}
            catch
                _:_ -> {error, not_responding}
            end;
        Error ->
            Error
    end.

%% @doc Returns the count of active cases.
-spec case_count() -> non_neg_integer().
case_count() ->
    length(supervisor:which_children(?MODULE)).

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpec = #{
        id => case_instance,
        start => {wf_case_runner, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [wf_case_runner]
    },
    {ok, {SupFlags, [ChildSpec]}}.
