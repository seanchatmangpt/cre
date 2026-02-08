%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% @doc Dynamic supervisor for workflow instances (gen_yawl processes).
%%
%% Implements Joe Armstrong's "let it crash" principle: each workflow instance
%% is supervised with one-for-one restart strategy. When a workflow process
%% crashes, the supervisor restarts it (or stops it if restart is temporary).
%%
%% <h3>Usage</h3>
%%
%% Start a workflow instance under supervision:
%% ```erlang
%% {ok, Pid} = yawl_workflow_supervisor:start_workflow(MyNetMod, NetArg, []).
%% ```
%%
%% Stop a workflow instance:
%% ```erlang
%% ok = yawl_workflow_supervisor:stop_workflow(Pid).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_workflow_supervisor).
-behaviour(supervisor).

%%====================================================================
%% Exports
%%====================================================================

-export([start_link/0]).
-export([init/1]).
-export([start_workflow/3]).
-export([stop_workflow/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the workflow supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Starts a workflow instance under supervision.
%%
%% Spawns a gen_yawl process as a supervised child. Uses temporary restart
%% so completed workflows are not restarted.
%%
%% @param NetMod The pattern/net module (callback module)
%% @param NetArg Initial argument for NetMod:init/1
%% @param Options gen_yawl options (e.g. fire_timeout, progress_timeout)
%% @return {ok, Pid} | {error, Reason}
-spec start_workflow(atom(), term(), [term()]) -> {ok, pid()} | {error, term()}.
start_workflow(NetMod, NetArg, Options) when is_atom(NetMod), is_list(Options) ->
    case supervisor:start_child(?MODULE, [NetMod, NetArg, Options]) of
        {ok, Pid} -> {ok, Pid};
        {ok, Pid, _Info} -> {ok, Pid};
        {error, Reason} -> {error, Reason}
    end.

%% @doc Stops a supervised workflow instance.
-spec stop_workflow(pid()) -> ok | {error, term()}.
stop_workflow(Pid) when is_pid(Pid) ->
    case supervisor:terminate_child(?MODULE, Pid) of
        ok -> ok;
        {error, not_found} ->
            %% Not under our supervision - try direct stop
            try
                gen_yawl:stop(Pid),
                ok
            catch
                _:_ -> {error, not_found}
            end;
        {error, _} = Err -> Err
    end.

%%====================================================================
%% Supervisor callback
%%====================================================================

init([]) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },
    ChildSpec = #{
        id => workflow_instance,
        start => {gen_yawl, start_link, []},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [gen_yawl]
    },
    {ok, {SupFlags, [ChildSpec]}}.
