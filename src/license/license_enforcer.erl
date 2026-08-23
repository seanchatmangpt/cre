%% -*- erlang -*-
%% @doc CRE License Enforcer for Google Cloud Marketplace BYOL Model
%%
%% Enforces End User License Agreement (EULA) acceptance for CRE deployments.
%% Implements grace period logic and license validation for Marketplace compliance.
%%
%% @end

-module(license_enforcer).
-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1]).
-export([check_license/0]).
-export([get_license_status/0]).
-export([accept_eula/1]).
-export([validate_startup/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("kernel/include/logger.hrl").

%% Types
-type license_status() :: valid | invalid | grace_period.
-type eula_acceptance() :: #{accepted => boolean(),
                             timestamp => integer(),
                             version => binary(),
                             acceptor => binary()}.
-type state() :: #{eula => eula_acceptance(),
                   grace_period_start => integer() | undefined,
                   grace_period_days => pos_integer(),
                   status => license_status()}.

-export_type([license_status/0, eula_acceptance/0]).

-define(SERVER, ?MODULE).
-define(GRACE_PERIOD_DAYS, 30).
-define(EULA_VERSION, <<"1.0">>).
-define(LICENSE_FILE, "/opt/cre/data/license/eula_acceptance.json").

%%====================================================================
%% API
%%====================================================================

%% @doc Start the license enforcer with default options.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link([]).

%% @doc Start the license enforcer with custom options.
-spec start_link([map()]) -> {ok, pid()} | {error, term()}.
start_link(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

%% @doc Check if license is valid (for runtime validation).
-spec check_license() -> {ok, license_status()} | {error, term()}.
check_license() ->
    gen_server:call(?SERVER, check_license).

%% @doc Get current license status.
-spec get_license_status() -> {ok, map()}.
get_license_status() ->
    gen_server:call(?SERVER, get_status).

%% @doc Accept EULA (called during deployment).
-spec accept_eula(binary()) -> ok | {error, term()}.
accept_eula(Acceptor) ->
    gen_server:call(?SERVER, {accept_eula, Acceptor}).

%% @doc Validate license at startup (called by health probe).
-spec validate_startup() -> ok | {error, term()}.
validate_startup() ->
    gen_server:call(?SERVER, validate_startup).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([]) ->
    init([]);
init(Options) ->
    GracePeriodDays = maps_get(grace_period_days, Options, ?GRACE_PERIOD_DAYS),
    State = #{
        eula => load_eula_acceptance(),
        grace_period_start => undefined,
        grace_period_days => GracePeriodDays,
        status => invalid
    },
    {ok, validate_license(State)}.

%% @private
handle_call(check_license, _From, State) ->
    {reply, {ok, maps:get(status, State)}, State};

handle_call(get_status, _From, State) ->
    Status = #{
        status => maps:get(status, State),
        eula_accepted => maps_get(accepted, maps:get(eula, State), false),
        grace_period_remaining => calculate_grace_days(State),
        eula_version => ?EULA_VERSION
    },
    {reply, {ok, Status}, State};

handle_call({accept_eula, Acceptor}, _From, State) ->
    EulaAcceptance = #{
        accepted => true,
        timestamp => erlang:system_time(second),
        version => ?EULA_VERSION,
        acceptor => Acceptor
    },
    case save_eula_acceptance(EulaAcceptance) of
        ok ->
            NewState = State#{eula => EulaAcceptance, status => valid},
            ?LOG(info, "EULA accepted by ~s", [Acceptor]),
            {reply, ok, validate_license(NewState)};
        {error, Reason} ->
            ?LOG(error, "Failed to save EULA acceptance: ~p", [Reason]),
            {reply, {error, Reason}, State}
    end;

handle_call(validate_startup, _From, State) ->
    Status = maps:get(status, State),
    case Status of
        valid ->
            {reply, ok, State};
        grace_period ->
            DaysRemaining = calculate_grace_days(State),
            ?LOG(warning, "License in grace period, ~p days remaining",
                 [DaysRemaining]),
            {reply, ok, State};
        invalid ->
            ?LOG(error, "License validation failed - EULA not accepted"),
            {reply, {error, eula_not_accepted}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private Load EULA acceptance from disk.
-spec load_eula_acceptance() -> eula_acceptance().
load_eula_acceptance() ->
    case file:read_file(?LICENSE_FILE) of
        {ok, Data} ->
            try jsx:decode(Data, [return_maps]) of
                Map when is_map(Map) ->
                    ?LOG(info, "Loaded EULA acceptance from ~s", [?LICENSE_FILE]),
                    Map;
                _ ->
                    ?LOG(warning, "Invalid EULA file format, using default"),
                    default_eula()
            catch
                _:_ ->
                    ?LOG(warning, "Failed to decode EULA file, using default"),
                    default_eula()
            end;
        {error, enoent} ->
            ?LOG(info, "EULA file not found at ~s, using default", [?LICENSE_FILE]),
            default_eula();
        {error, Reason} ->
            ?LOG(error, "Failed to read EULA file: ~p, using default", [Reason]),
            default_eula()
    end.

%% @private Save EULA acceptance to disk.
-spec save_eula_acceptance(eula_acceptance()) -> ok | {error, term()}.
save_eula_acceptance(EulaAcceptance) ->
    Data = jsx:encode(EulaAcceptance),
    case ensure_license_dir() of
        ok ->
            case file:write_file(?LICENSE_FILE, Data) of
                ok ->
                    ?LOG(info, "Saved EULA acceptance to ~s", [?LICENSE_FILE]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "Failed to write EULA file: ~p", [Reason]),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Validate license and update status.
-spec validate_license(state()) -> state().
validate_license(State) ->
    Eula = maps:get(eula, State),
    case maps_get(accepted, Eula, false) of
        true ->
            State#{status => valid};
        false ->
            %% Check if grace period should start
            case maps_get(grace_period_start, State, undefined) of
                undefined ->
                    %% Start grace period on first validation
                    StartTime = erlang:system_time(second),
                    ?LOG(warning, "EULA not accepted, starting ~p-day grace period",
                         [maps_get(grace_period_days, State, ?GRACE_PERIOD_DAYS)]),
                    State#{
                        grace_period_start => StartTime,
                        status => grace_period
                    };
                StartTime ->
                    %% Check if grace period has expired
                    DaysElapsed = (erlang:system_time(second) - StartTime) div 86400,
                    MaxDays = maps_get(grace_period_days, State, ?GRACE_PERIOD_DAYS),
                    if
                        DaysElapsed >= MaxDays ->
                            ?LOG(error,
                                 "Grace period expired (~p days), EULA acceptance required",
                                 [DaysElapsed]),
                            State#{status => invalid};
                        true ->
                            ?LOG(info, "Grace period active: ~p/~p days remaining",
                                 [MaxDays - DaysElapsed, MaxDays]),
                            State#{status => grace_period}
                    end
            end
    end.

%% @private Calculate remaining grace period days.
-spec calculate_grace_days(state()) -> non_neg_integer().
calculate_grace_days(State) ->
    case maps_get(grace_period_start, State, undefined) of
        undefined -> 0;
        StartTime ->
            MaxDays = maps_get(grace_period_days, State, ?GRACE_PERIOD_DAYS),
            DaysElapsed = (erlang:system_time(second) - StartTime) div 86400,
            max(0, MaxDays - DaysElapsed)
    end.

%% @private Default EULA (not accepted).
-spec default_eula() -> eula_acceptance().
default_eula() ->
    #{
        accepted => false,
        timestamp => 0,
        version => ?EULA_VERSION,
        acceptor => <<>>
    }.

%% @private Ensure license directory exists.
-spec ensure_license_dir() -> ok | {error, term()}.
ensure_license_dir() ->
    Dir = filename:dirname(?LICENSE_FILE),
    case filelib:is_dir(Dir) of
        true ->
            ok;
        false ->
            case file:make_dir(Dir) of
                ok ->
                    ?LOG(info, "Created license directory: ~s", [Dir]),
                    ok;
                {error, Reason} ->
                    ?LOG(error, "Failed to create license directory ~s: ~p", [Dir, Reason]),
                    {error, Reason}
            end
    end.

%% @private Helper for maps:get with default.
maps_get(Key, Map, Default) ->
    try maps:get(Key, Map) of
        Value -> Value
    catch
        error:{badkey, _} -> Default
    end.
