%%%-------------------------------------------------------------------
%%% @doc
%%% SOC 2 Evidence Stream Supervisor
%%%
%%% Manages continuous evidence generators. These processes produce
%%% the evidence artifacts that control validators consume.
%%%
%%% Evidence streams run independently and feed evidence/ directory.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_evidence_stream_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    logger:info(#{what => soc2_evidence_stream_supervisor_starting}),

    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    %% Evidence generators (one per evidence type)
    UptimeGen = #{
        id => uptime_evidence_gen,
        start => {soc2_evidence_gen, start_link, [uptime]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [soc2_evidence_gen]
    },

    LoadTestGen = #{
        id => load_test_evidence_gen,
        start => {soc2_evidence_gen, start_link, [load_test]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [soc2_evidence_gen]
    },

    ChaosGen = #{
        id => chaos_evidence_gen,
        start => {soc2_evidence_gen, start_link, [chaos]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [soc2_evidence_gen]
    },

    BuildGen = #{
        id => build_evidence_gen,
        start => {soc2_evidence_gen, start_link, [build]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [soc2_evidence_gen]
    },

    ChildSpecs = [UptimeGen, LoadTestGen, ChaosGen, BuildGen],

    {ok, {SupFlags, ChildSpecs}}.
