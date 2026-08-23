%% -*- erlang -*-
%% @doc CRE License Supervisor
%%
%% Supervises license enforcement modules for Google Cloud Marketplace BYOL model.
%%
%% @end

-module(license_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Start the license supervisor.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, {supervisor:sup_ref(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    LicenseEnforcerSpec = #{
        id => license_enforcer,
        start => {license_enforcer, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [license_enforcer]
    },

    {ok, {SupFlags, [LicenseEnforcerSpec]}}.
