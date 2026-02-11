%%% @doc WF Case Supervisor - Manages case runner processes
%%%
%%% This module implements a simple_one_for_one supervisor that spawns
%%% wf_case_runner processes. Each case runner executes a single workflow
%%% case instance with its own bytecode VM and execution state.
%%%
%%% The supervisor provides dynamic child management:
%%% - Spawn new case runners on demand
%%% - Automatic cleanup on case termination
%%% - Case process registration
%%%
%%% @end
-module(wf_case_sup).
-behaviour(supervisor).

%%====================================================================
%% Exports
%%====================================================================

-export([start_link/0]).
-export([start_case/2, start_case/3]).
-export([stop_case/1]).
-export([which_cases/0]).
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Starts the case supervisor.
%%
%% The supervisor is registered locally as `wf_case_sup' and uses
%% simple_one_for_one to dynamically manage case runner processes.
%%
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts a new case runner process.
%%
%% Spawns a wf_case_runner under supervision for the given case ID
%% and compiled workflow program.
%%
%% @param CaseId Unique identifier for this case instance
%% @param Compiled Compiled workflow program (from wf_compile)
%% @returns {ok, Pid} | {error, Reason}
%%
-spec start_case(term(), wf_compile:compiled()) ->
    {ok, pid()} | {error, term()}.
start_case(CaseId, Compiled) ->
    start_case(CaseId, Compiled, #{}).

%% @doc Starts a new case runner with initial context.
%%
%% @param CaseId Unique identifier for this case instance
%% @param Compiled Compiled workflow program
%% @param InitCtx Initial user context (data, signals, etc.)
%% @returns {ok, Pid} | {error, Reason}
%%
-spec start_case(term(), wf_compile:compiled(), wf_term:context()) ->
    {ok, pid()} | {error, term()}.
start_case(CaseId, Compiled, InitCtx) ->
    supervisor:start_child(?MODULE, [CaseId, Compiled, InitCtx]).

%% @doc Stops a running case.
%%
%% Terminates the case runner process and removes it from supervision.
%% The case runner will perform cleanup before termination.
%%
%% @param Pid Case runner process ID
%% @returns ok | {error, Reason}
%%
-spec stop_case(pid()) -> ok | {error, term()}.
stop_case(Pid) when is_pid(Pid) ->
    case supervisor:terminate_child(?MODULE, Pid) of
        ok -> ok;
        {error, not_found} -> {error, not_found};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Returns list of all running case PIDs.
%%
%% @returns [pid()]
%%
-spec which_cases() -> [pid()].
which_cases() ->
    [Pid || {_Id, Pid, _Type, _Modules} <- supervisor:which_children(?MODULE),
            is_pid(Pid)].

%%====================================================================
%% Supervisor Callbacks
%%====================================================================

%% @doc Supervisor initialization callback.
%%
%% Configures simple_one_for_one strategy for dynamic case runner spawning.
%%
%% Supervisor Flags:
%% - strategy: simple_one_for_one - all children are instances of wf_case_runner
%% - intensity: 10 - allow up to 10 restarts
%% - period: 60 - within 60 seconds
%%
%% Child Specification:
%% - id: wf_case_runner
%% - start: {wf_case_runner, start_link, []} - args prepended during start_child
%% - restart: temporary - case runners are not restarted automatically
%% - shutdown: 5000 - allow 5s for graceful shutdown
%% - type: worker
%%
-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpec = #{
        id => wf_case_runner,
        start => {wf_case_runner, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [wf_case_runner]
    },

    {ok, {SupFlags, [ChildSpec]}}.
