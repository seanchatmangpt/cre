%% -*- erlang -*-
%%%% @doc active_token_sup - Supervisor for active token processes.
%%
%% This module provides a one_for_one supervisor for managing active token
%% processes. Tokens are transient workers that can be restarted.
%%
%% @end
%% -------------------------------------------------------------------

-module(active_token_sup).
-author("CRE Team").

-behaviour(supervisor).

%%====================================================================
%% Exports
%%====================================================================

%% Supervisor API
-export([start_link/0]).
-export([start_token/4]).
-export([terminate_token/1]).

%% supervisor callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the active token supervisor.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc Starts a new token under supervision.
%%
%% @param Place Initial place for the token
%% @param Payload Token data
%% @param Options Token options
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_token(atom(), term(), [gen_active_token:token_option()], []) ->
          {ok, pid()} | {error, term()}.

start_token(Place, Payload, Options, Opts) ->
    supervisor:start_child(?MODULE, [Place, Payload, Options, Opts]).

%%--------------------------------------------------------------------
%% @doc Terminates a token.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate_token(pid()) -> ok | {error, term()}.

terminate_token(Pid) when is_pid(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%%====================================================================
%% supervisor Callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()},
                         [supervisor:child_spec()]}}.

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [
        #{
            id => gen_active_token,
            start => {gen_active_token, start_link, []},
            restart => transient,
            shutdown => 5000,
            type => worker,
            modules => [gen_active_token]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.
