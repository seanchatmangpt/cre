%% Certification Runner Supervisor
-module(f5_cert_runner_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    Children = [
        #{
            id => f5_cert_runner,
            start => {f5_cert_runner, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [f5_cert_runner]
        },
        #{
            id => f5_cert_scheduler,
            start => {f5_cert_scheduler, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [f5_cert_scheduler]
        }
    ],

    {ok, {SupFlags, Children}}.
