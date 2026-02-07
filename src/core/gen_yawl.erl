%% -*- erlang -*-
%%%% @author CRE Team
%% @version 0.3.0
%% -------------------------------------------------------------------

-module(gen_yawl).
-author("CRE Team").
-behaviour(gen_server).

-moduledoc """
gen_yawl - A wrapper around gen_pnet that supports 3-tuple returns
from fire/3 for automatic user info updates.

`gen_yawl' extends the `gen_pnet' behavior by allowing the `fire/3' callback
to return a 3-tuple `{produce, ProduceMap, NewUsrInfo}' in addition to the
standard 2-tuple `{produce, ProduceMap}' or `abort'. When a 3-tuple is
returned, the user info is automatically updated.

This is particularly useful for YAWL workflows where the state needs to be
updated as part of transition firing, such as tracking workflow variables,
case state, or execution context.

<h3>Key Features</h3>

<ul>
  <li><b>Drop-in Replacement:</b> Same API surface as gen_pnet</li>
  <li><b>Enhanced fire/3:</b> Supports 3-tuple returns for automatic state updates</li>
  <li><b>Backward Compatible:</b> Works with existing gen_pnet callback modules</li>
  <li><b>Transparent Wrapping:</b> No changes to Petri net semantics</li>
</ul>

<h3>Enhanced fire/3 Callback</h3>

The fire/3 callback can return:

<ul>
  <li>`{produce, ProduceMap}' - Standard gen_pnet behavior, produces tokens</li>
  <li>`{produce, ProduceMap, NewUsrInfo}' - Enhanced behavior, produces tokens
      and updates user info</li>
  <li>`abort' - Abort the transition firing</li>
</ul>
""".

%%====================================================================
%% Includes
%%====================================================================

-include("gen_pnet.hrl").

%%====================================================================
%% Callback Specifications
%%====================================================================

%% Structure callbacks
-callback place_lst() -> [atom()].
-callback trsn_lst() -> [atom()].
-callback init_marking(Place :: atom(), UsrInfo :: term()) -> [term()].
-callback preset(Trsn :: atom()) -> [atom()].
-callback is_enabled(Trsn :: atom(), Mode :: #{atom() => [term()]}, UsrInfo :: term()) ->
              boolean().

%% Enhanced fire callback - supports 3-tuple return with usr_info update
-callback fire(Trsn :: atom(), Mode :: #{atom() => [term()]}, UsrInfo :: term()) ->
              abort |
              {produce, #{atom() => [term()]}} |
              {produce, #{atom() => [term()]}, NewUsrInfo :: term()}.

%% Interface callbacks
-callback init(NetArg :: term()) -> UsrInfo :: term().
-callback code_change(OldVsn :: term(), NetState :: term(), Extra :: term()) ->
              {ok, term()} | {error, term()}.
-callback handle_call(Request :: term(),
                      From :: {pid(), term()},
                      NetState :: term()) ->
              {reply, term()} |
              {reply, term(), #{atom() => [term()]}} |
              noreply |
              {noreply, #{atom() => [term()]}} |
              {stop, term(), term()}.
-callback handle_cast(Request :: term(), NetState :: term()) ->
              noreply |
              {noreply, #{atom() => [term()]}} |
              {stop, term()}.
-callback handle_info(Info :: term(), NetState :: term()) ->
              noreply |
              {noreply, #{atom() => [term()]}} |
              {stop, term()}.
-callback terminate(Reason :: term(), NetState :: term()) -> ok.
-callback trigger(Place :: atom(), Token :: term(), NetState :: term()) ->
              pass | drop.

%%====================================================================
%% Exports
%%====================================================================

%% API functions - same as gen_pnet
-export([start_link/3, start_link/4,
         ls/2,
         marking/1,
         call/2, call/3,
         cast/2,
         stats/1,
         reply/2,
         reset_stats/1,
         stop/1,
         usr_info/1,
         state_property/3]).

%% Net state accessor functions
-export([get_ls/2, get_usr_info/1, get_stats/1]).

%% gen_server callbacks
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

%%====================================================================
%% Type definitions
%%====================================================================

-type name() :: atom() |
                {atom(), atom()} |
                {global, _} |
                {via, atom(), _} |
                pid().

-type server_name() :: {local, atom()} |
                       {global, atom()} |
                       {via, atom(), _}.

-type start_link_result() :: {ok, pid()} |
                             ignore |
                             {error, _}.

-type prop() :: {debug, [log | statistics | trace | {_, _}]} |
                {hibernate_after, infinity | non_neg_integer()} |
                {spawn_opt, [link | monitor | {_, _}]} |
                {timeout, infinity | non_neg_integer()}.

%% Extended fire return type for gen_yawl
-type fire_result() ::
    abort |
    {produce, #{atom() => [term()]}} |
    {produce, #{atom() => [term()]}, term()}.

-export_type([fire_result/0]).

%% Internal state record for the gen_yawl wrapper
-record(wrapper_state, {
          net_mod :: atom(),
          net_state :: term(),
          fire_timeout = 5000 :: pos_integer(),
          progress_timeout = 30000 :: pos_integer(),
          shutting_down = false :: boolean(),
          active_fires = 0 :: non_neg_integer()
         }).

%%====================================================================
%% API functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts an unregistered gen_yawl net instance.
%%
%%      This is equivalent to `gen_pnet:start_link/3' but with the
%%      enhanced fire/3 behavior.
%%
%%      Options can include:
%%      - `{fire_timeout, Milliseconds}' - Timeout for fire/3 callbacks (default: 5000)
%%      - `{progress_timeout, Milliseconds}' - Timeout for progress loop (default: 30000)
%%
%% === Example ===
%% ```
%% {ok, Pid} = gen_yawl:start_link(my_workflow, InitArg, [{fire_timeout, 10000}]).
%% '''
%%
%% @see start_link/4
%% @end
%%--------------------------------------------------------------------
-spec start_link(NetMod :: atom(), NetArg :: term(), Options :: [prop()]) ->
          start_link_result().

start_link(NetMod, NetArg, Options)
  when is_atom(NetMod), is_list(Options) ->
    gen_server:start_link(?MODULE, {NetMod, NetArg, Options}, Options).


%%--------------------------------------------------------------------
%% @doc Starts a registered gen_yawl net instance.
%%
%%      The ServerName can be `{local, Name} | {global, Name} |
%%      {via, Module, ViaName}'. Internally, the server name and options
%%      are passed to gen_server:start_link/4.
%%
%%      Options can include:
%%      - `{fire_timeout, Milliseconds}' - Timeout for fire/3 callbacks (default: 5000)
%%      - `{progress_timeout, Milliseconds}' - Timeout for progress loop (default: 30000)
%%
%% === Example ===
%% ```
%% {ok, Pid} = gen_yawl:start_link({local, my_workflow}, my_workflow, InitArg, []).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(ServerName :: server_name(),
                 NetMod :: atom(),
                 InitArg :: term(),
                 Options :: [prop()]) ->
          start_link_result().

start_link(ServerName, NetMod, InitArg, Options)
  when is_tuple(ServerName), is_atom(NetMod), is_list(Options) ->
    gen_server:start_link(ServerName, ?MODULE, {NetMod, InitArg, Options}, Options).


%%--------------------------------------------------------------------
%% @doc Query the list of tokens on a place.
%%
%%      Equivalent to gen_pnet:ls/2.
%%
%% === Example ===
%% ```
%% {ok, Tokens} = gen_yawl:ls(Pid, place1).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec ls(Name :: name(), Place :: atom()) -> {ok, [term()]} | {error, #bad_place{}}.

ls(Name, Place) when is_atom(Place) ->
    gen_server:call(Name, {ls, Place}).


%%--------------------------------------------------------------------
%% @doc Query the marking map of the net instance.
%%
%%      Returns a map associating each place name with its token list.
%%
%% === Example ===
%% ```
%% Marking = gen_yawl:marking(Pid).
%% #{place1 := Tokens, place2 := []} = Marking.
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec marking(Name :: name()) -> #{atom() => [term()]}.

marking(Name) ->
    gen_server:call(Name, marking).


%%--------------------------------------------------------------------
%% @doc Query the user info term from the net instance.
%%
%%      Returns the user info that can be updated via the enhanced
%%      fire/3 3-tuple return.
%%
%% === Example ===
%% ```
%% UsrInfo = gen_yawl:usr_info(Pid).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec usr_info(Name :: name()) -> term().

usr_info(Name) ->
    gen_server:call(Name, usr_info).


%%--------------------------------------------------------------------
%% @doc Query the statistics gathered by the net instance.
%%
%%      Returns throughput statistics including current, max, and min
%%      firings per second.
%%
%% === Example ===
%% ```
%% #stats{current = #stat{fps = Fps}} = gen_yawl:stats(Pid).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec stats(Name :: name()) -> #stats{}.

stats(Name) ->
    gen_server:call(Name, stats).


%%--------------------------------------------------------------------
%% @doc Clear the statistics for the net instance.
%%
%% === Example ===
%% ```
%% ok = gen_yawl:reset_stats(Pid).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_stats(Name :: name()) -> ok.

reset_stats(Name) ->
    gen_server:call(Name, reset_stats).


%%--------------------------------------------------------------------
%% @doc Stop the gen_yawl process.
%%
%%      Equivalent to gen_pnet:stop/1.
%%
%% === Example ===
%% ```
%% ok = gen_yawl:stop(Pid).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Name :: name()) -> ok.

stop(Name) ->
    gen_server:stop(Name).


%%--------------------------------------------------------------------
%% @doc Synchronously send a request to the net instance.
%%
%%      Timeout is implicitly set to 5 seconds.
%%
%% === Example ===
%% ```
%% Reply = gen_yawl:call(Pid, get_status).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec call(Name :: name(), Request :: term()) -> term().

call(Name, Request) ->
    gen_server:call(Name, {call, Request}).


%%--------------------------------------------------------------------
%% @doc Synchronously send a request with explicit timeout.
%%
%%      Timeout must be a non-negative integer or infinity.
%%
%% === Example ===
%% ```
%% Reply = gen_yawl:call(Pid, get_status, 10000).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec call(Name :: name(), Request :: term(), Timeout :: non_neg_integer() | infinity) ->
          term().

call(Name, Request, Timeout) when is_integer(Timeout), Timeout >= 0 ->
    gen_server:call(Name, {call, Request}, Timeout);

call(Name, Request, infinity) ->
    gen_server:call(Name, {call, Request}, infinity).


%%--------------------------------------------------------------------
%% @doc Asynchronously send a request to the net instance.
%%
%%      The cast succeeds even if the process doesn't exist.
%%
%% === Example ===
%% ```
%% ok = gen_yawl:cast(Pid, {update, Data}).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec cast(Name :: name(), Request :: term()) -> ok.

cast(Name, Request) ->
    gen_server:cast(Name, {cast, Request}).


%%--------------------------------------------------------------------
%% @doc Reply to a deferred call.
%%
%%      Use when replying was deferred by returning `{noreply, _, _}'
%%      in handle_call/3.
%%
%% === Example ===
%% ```
%% gen_yawl:reply(Client, Response).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec reply(Client :: {pid(), gen_server:reply_tag()}, Reply :: term()) -> ok.

reply(Client, Reply) when is_tuple(Client) ->
    gen_server:reply(Client, Reply).


%%--------------------------------------------------------------------
%% @doc Check if a predicate about the state holds.
%%
%%      The predicate function receives token lists for the specified places.
%%
%% === Example ===
%% ```
%% Pred = fun(Tokens1, Tokens2) -> length(Tokens1) > length(Tokens2) end,
%% ok = gen_yawl:state_property(Pid, Pred, [place1, place2]).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec state_property(Name :: name(),
                     Pred :: fun((...) -> ok | {error, term()}),
                     PlaceLst :: [atom()]) ->
          ok | {error, term()}.

state_property(Name, Pred, PlaceLst)
  when is_list(PlaceLst),
       is_function(Pred, length(PlaceLst)) ->
    Marking = gen_yawl:marking(Name),
    ArgLst = [ maps:get(Place, Marking) || Place <- PlaceLst ],
    apply(Pred, ArgLst).


%%====================================================================
%% Net state accessor functions (forwarded from gen_pnet)
%%====================================================================

-doc """
Extract tokens on a place from a net state.

Throws an error if the place does not exist.
""".
-spec get_ls(Place :: atom(), NetState :: #net_state{}) -> [term()].

get_ls(Place, #net_state{marking = Marking}) ->
    maps:get(Place, Marking).

-doc """
Extract user info from a net state.

Returns the user info field from the net_state record.
""".
-spec get_usr_info(NetState :: #net_state{}) -> term().

get_usr_info(#net_state{usr_info = UsrInfo}) ->
    UsrInfo.

-doc """
Extract stats from a net state.

Returns the statistics record from the net_state.
""".
-spec get_stats(NetState :: #net_state{}) -> #stats{}.

get_stats(#net_state{stats = Stats}) ->
    Stats.


%%====================================================================
%% gen_server callback functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initialize the gen_yawl wrapper.
%%
%%      We initialize the underlying gen_pnet state and start
%%      Petri net execution.
%%
%% @end
%%--------------------------------------------------------------------
-spec init({NetMod :: atom(), NetArg :: term(), Options :: [prop()]}) ->
          {ok, #wrapper_state{}}.

init({NetMod, NetArg, Options}) ->
    %% Extract timeout options
    FireTimeout = proplists:get_value(fire_timeout, Options, 5000),
    ProgressTimeout = proplists:get_value(progress_timeout, Options, 30000),

    %% Validate timeouts
    true = is_integer(FireTimeout) andalso FireTimeout > 0,
    true = is_integer(ProgressTimeout) andalso ProgressTimeout > 0,

    %% Initialize user info from the callback module
    UsrInfo = NetMod:init(NetArg),

    %% Get place list and initialize marking
    PlaceLst = NetMod:place_lst(),

    F = fun(P, Acc) ->
                Acc#{P => NetMod:init_marking(P, UsrInfo)}
        end,

    InitMarking = lists:foldl(F, #{}, PlaceLst),

    %% Create initial net state
    NetState = #net_state{
                  net_mod = NetMod,
                  usr_info = UsrInfo,
                  marking = InitMarking,
                  stats = undefined,
                  tstart = os:system_time(),
                  cnt = 0
                 },

    %% Start the Petri net execution
    continue(self()),

    %% Create wrapper state with timeouts
    WrapperState = #wrapper_state{
                      net_mod = NetMod,
                      net_state = NetState,
                      fire_timeout = FireTimeout,
                      progress_timeout = ProgressTimeout,
                      shutting_down = false,
                      active_fires = 0
                     },

    {ok, WrapperState}.


%%--------------------------------------------------------------------
%% @private
%% @doc Handle synchronous calls.
%%
%%      Delegates to gen_pnet-style handling while maintaining our
%%      wrapper state.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(),
                  From :: {pid(), term()},
                  State :: #wrapper_state{}) ->
          {reply, term(), #wrapper_state{}} |
          {noreply, #wrapper_state{}} |
          {stop, term(), term(), #wrapper_state{}}.

handle_call({ls, Place}, _From, WrapperState = #wrapper_state{net_state = NetState}) ->
    #net_state{marking = Marking} = NetState,
    Reply = case maps:is_key(Place, Marking) of
                true -> {ok, maps:get(Place, Marking)};
                false -> {error, #bad_place{name = Place}}
            end,
    {reply, Reply, WrapperState};

handle_call(marking, _From, WrapperState = #wrapper_state{net_state = NetState}) ->
    #net_state{marking = Marking} = NetState,
    {reply, Marking, WrapperState};

handle_call(usr_info, _From, WrapperState = #wrapper_state{net_state = NetState}) ->
    #net_state{usr_info = UsrInfo} = NetState,
    {reply, UsrInfo, WrapperState};

handle_call({call, Request}, From,
            WrapperState = #wrapper_state{net_mod = NetMod, net_state = NetState}) ->
    case NetMod:handle_call(Request, From, NetState) of
        {reply, Reply} ->
            {reply, Reply, WrapperState};

        {reply, Reply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState, NetMod),
            continue(self()),
            {reply, Reply, WrapperState#wrapper_state{net_state = NetState1}};

        noreply ->
            {noreply, WrapperState};

        {noreply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState, NetMod),
            continue(self()),
            {noreply, WrapperState#wrapper_state{net_state = NetState1}};

        {stop, Reason, Reply} ->
            {stop, Reason, Reply, WrapperState}
    end;

handle_call(stats, _From, WrapperState = #wrapper_state{net_state = NetState}) ->
    #net_state{stats = Stats} = NetState,
    {reply, Stats, WrapperState};

handle_call(reset_stats, _From, WrapperState = #wrapper_state{net_state = NetState}) ->
    NetState1 = NetState#net_state{stats = undefined},
    {reply, ok, WrapperState#wrapper_state{net_state = NetState1}};

handle_call(_Request, _From, WrapperState) ->
    {reply, {error, bad_msg}, WrapperState}.


%%--------------------------------------------------------------------
%% @private
%% @doc Handle asynchronous messages.
%%
%%      This is where we intercept the continue message to implement
%%      our enhanced fire/3 behavior with 3-tuple support.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: #wrapper_state{}) ->
          {noreply, #wrapper_state{}} |
          {stop, term(), #wrapper_state{}}.

handle_cast(continue,
            WrapperState = #wrapper_state{
                              shutting_down = true
                             }) ->
    %% Stop accepting new transitions during shutdown
    logger:debug("gen_yawl ignoring continue during shutdown"),
    {noreply, WrapperState};

handle_cast(continue,
            WrapperState = #wrapper_state{
                              fire_timeout = FireTimeout,
                              progress_timeout = ProgressTimeout,
                              net_state = NetState0 = #net_state{
                                                        stats = Stats,
                                                        tstart = T1,
                                                        cnt = Cnt
                                                       }
                             }) ->
    %% Check progress timeout
    StartTime = erlang:monotonic_time(millisecond),

    case progress(NetState0, FireTimeout) of
        abort ->
            {noreply, WrapperState};

        {delta, Mode, Pm, NewUsrInfo} ->
            Elapsed = erlang:monotonic_time(millisecond) - StartTime,
            case Elapsed > ProgressTimeout of
                true ->
                    logger:warning("gen_yawl progress timeout: ~p ms", [Elapsed]),
                    %% Continue anyway but log warning
                    ok;
                false ->
                    ok
            end,

            %% Update net state with consumed tokens
            NetState1 = cns(Mode, NetState0),

            %% Update user info if provided by fire/3 3-tuple return
            NetState2 = case NewUsrInfo of
                undefined -> NetState1;
                _ -> NetState1#net_state{usr_info = NewUsrInfo}
            end,

            %% Handle trigger and produce tokens
            NetMod = WrapperState#wrapper_state.net_mod,
            NetState3 = handle_trigger(Pm, NetState2, NetMod),

            %% Check if we should continue (not shutting down)
            case WrapperState#wrapper_state.shutting_down of
                false ->
                    continue(self());
                true ->
                    logger:debug("gen_yawl not continuing due to shutdown")
            end,

            %% Update stats
            NetState4 = update_stats(NetState3, Stats, T1, Cnt),

            {noreply, WrapperState#wrapper_state{net_state = NetState4}};

        {error, Reason, NetState1} ->
            logger:error("gen_yawl progress error: ~p", [Reason]),
            %% Continue despite error - log and try next transition
            continue(self()),
            {noreply, WrapperState#wrapper_state{net_state = NetState1}}
    end;

handle_cast({cast, Request},
            WrapperState = #wrapper_state{net_mod = NetMod, net_state = NetState}) ->
    case NetMod:handle_cast(Request, NetState) of
        noreply ->
            {noreply, WrapperState};

        {noreply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState, NetMod),
            continue(self()),
            {noreply, WrapperState#wrapper_state{net_state = NetState1}};

        {stop, Reason} ->
            {stop, Reason, WrapperState}
    end;

handle_cast(_Request, WrapperState) ->
    {noreply, WrapperState}.


%%--------------------------------------------------------------------
%% @private
%% @doc Handle unexpected messages.
%%
%%      Delegates to the callback module's handle_info/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: #wrapper_state{}) ->
          {noreply, #wrapper_state{}} |
          {stop, term(), #wrapper_state{}}.

handle_info(Info, WrapperState = #wrapper_state{net_mod = NetMod, net_state = NetState}) ->
    case NetMod:handle_info(Info, NetState) of
        noreply ->
            {noreply, WrapperState};

        {noreply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState, NetMod),
            continue(self()),
            {noreply, WrapperState#wrapper_state{net_state = NetState1}};

        {stop, Reason} ->
            {stop, Reason, WrapperState}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Handle code change during hot code upgrade.
%%
%%      Delegates to the callback module's code_change/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: #wrapper_state{}, Extra :: term()) ->
          {ok, #wrapper_state{}}.

code_change(OldVsn, WrapperState = #wrapper_state{net_mod = NetMod, net_state = NetState}, Extra) ->
    case NetMod:code_change(OldVsn, NetState, Extra) of
        {ok, NetState1} -> {ok, WrapperState#wrapper_state{net_state = NetState1}};
        {error, Reason} -> {error, Reason}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Handle process termination.
%%
%%      Implements graceful shutdown with state persistence and
%%      delegates to the callback module's terminate/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: #wrapper_state{}) -> ok.

terminate(Reason, #wrapper_state{net_mod = NetMod,
                                net_state = NetState,
                                active_fires = ActiveFires,
                                shutting_down = IsShuttingDown}) ->
    %% Log shutdown event
    _ = logger:info("gen_yawl terminating: reason=~p, active_fires=~p, shutting_down=~p",
                [Reason, ActiveFires, IsShuttingDown]),

    %% If we have active transitions, wait a short time for them to complete
    _ = case ActiveFires > 0 of
        true ->
            _ = logger:info("gen_yawl waiting for ~p active transitions to complete",
                        [ActiveFires]),
            %% Give active fires a moment to complete (max 1 second)
            timer:sleep(100),
            ok;
        false ->
            ok
    end,

    %% Persist final state if available
    _ = case catch yawl_persistence:checkpoint_save(self(), NetState) of
        ok ->
            logger:info("gen_yawl successfully persisted final state"),
            ok;
        {error, _Why} ->
            logger:warning("gen_yawl failed to persist state"),
            ok;
        {'EXIT', _} ->
            %% Persistence module not available or failed
            ok
    end,

    %% Emit shutdown telemetry event
    _ = case catch yawl_telemetry:emit(shutdown, #{reason => Reason,
                                                  active_fires => ActiveFires}) of
        ok -> ok;
        _ -> ok
    end,

    %% Delegate to callback module's terminate
    NetMod:terminate(Reason, NetState).


%%====================================================================
%% Internal functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Continue making progress in the net instance.
%%
%%      Sends a continue message to self to trigger transition firing.
%%
%% @end
%%--------------------------------------------------------------------
-spec continue(Name :: pid()) -> ok.

continue(Name) ->
    gen_server:cast(Name, continue).


%%--------------------------------------------------------------------
%% @private
%% @doc Update statistics in the net state.
%%
%%      Computes throughput statistics (firings per second) and
%%      updates the stats field.
%%
%% @end
%%--------------------------------------------------------------------
-spec update_stats(NetState :: #net_state{},
                   Stats :: #stats{} | undefined,
                   T1 :: integer(),
                   Cnt :: non_neg_integer()) -> #net_state{}.

update_stats(NetState, Stats, T1, Cnt) ->
    if
        Cnt < 1000 ->
            NetState#net_state{cnt = Cnt + 1};
        true ->
            T2 = os:system_time(),
            Tmean = round((T1 + T2) / 2),
            Tdelta = T2 - T1,
            CurrentFps = 1000000000000 / Tdelta,

            Current = #stat{t = Tmean, fps = CurrentFps},

            {Hi1, Lo1} = case Stats of
                             undefined -> {Current, Current};
                             #stats{hi = H, lo = L} -> {H, L}
                         end,

            #stat{fps = HiFps} = Hi1,
            #stat{fps = LoFps} = Lo1,

            Hi2 = if
                      CurrentFps > HiFps -> Current;
                      true -> Hi1
                  end,

            Lo2 = if
                      CurrentFps < LoFps -> Current;
                      true -> Lo1
                  end,

            NetState#net_state{
              stats = #stats{
                        current = Current,
                        hi = Hi2,
                        lo = Lo2
                       },
              tstart = T2,
              cnt = 0
             }
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Make progress in the Petri net execution.
%%
%%      This is the core function where we call the module's fire/3
%%      and handle both the standard 2-tuple and enhanced 3-tuple returns.
%%      Wraps fire/3 in try-catch for error handling.
%%
%% @end
%%--------------------------------------------------------------------
-spec progress(NetState :: #net_state{}, FireTimeout :: pos_integer()) ->
          abort | {delta, #{atom() => [term()]}, #{atom() => [term()]}, term() | undefined} |
          {error, term(), #net_state{}}.

progress(#net_state{
           marking = Marking,
           net_mod = NetMod,
           usr_info = UsrInfo
          }, FireTimeout) ->
    %% Get all transitions in the net
    TrsnLst = NetMod:trsn_lst(),

    %% Find all enabled transitions and their modes
    F = fun(T, Acc) ->
                Preset = NetMod:preset(T),
                MLst = enum_mode(Preset, Marking),
                IsEnabled = fun(M) -> NetMod:is_enabled(T, M, UsrInfo) end,
                EnabledMLst = lists:filter(IsEnabled, MLst),
                case EnabledMLst of
                    [] -> Acc;
                    [_ | _] -> Acc#{T => EnabledMLst}
                end
        end,

    ModeMap = lists:foldl(F, #{}, TrsnLst),

    %% Attempt to fire a transition
    attempt_progress(ModeMap, NetMod, UsrInfo, FireTimeout).


%%--------------------------------------------------------------------
%% @private
%% @doc Attempt to make progress by firing an enabled transition.
%%
%%      Returns the delta (consumed mode, produced map, and optionally
%%      new user info) or abort. Handles both 2-tuple and 3-tuple
%%      fire/3 returns. Wraps fire/3 in try-catch for error handling.
%%
%% @end
%%--------------------------------------------------------------------
-spec attempt_progress(ModeMap :: #{atom() => [#{atom() => [term()]}]},
                       NetMod :: atom(),
                       UsrInfo :: term(),
                       FireTimeout :: pos_integer()) ->
          abort | {delta, #{atom() => [term()]}, #{atom() => [term()]}, term() | undefined} |
          {error, term(), #net_state{}}.

attempt_progress(ModeMap, NetMod, UsrInfo, FireTimeout) ->
    case maps:size(ModeMap) of
        0 ->
            abort;

        _ ->
            TrsnLst = maps:keys(ModeMap),
            Trsn = pick_from(TrsnLst),
            #{Trsn := ModeLst} = ModeMap,
            Mode = pick_from(ModeLst),

            %% Wrap fire/3 in try-catch with timeout for error handling
            try
                FireResult = case FireTimeout of
                    infinity ->
                        NetMod:fire(Trsn, Mode, UsrInfo);
                    _ ->
                        %% Use spawn_monitor for timeout protection
                        {Pid, Ref} = spawn_monitor(fun() ->
                            Result = NetMod:fire(Trsn, Mode, UsrInfo),
                            exit({fire_result, Result})
                        end),

                    receive
                        {fire_result, Result} ->
                            Result;
                        {'DOWN', Ref, process, Pid, Reason} ->
                            logger:error("gen_yawl fire/3 process crashed: ~p", [Reason]),
                            {error, {fire_crashed, Reason}}
                    after FireTimeout ->
                        demonitor(Ref, [flush]),
                        exit(Pid, kill),
                        logger:warning("gen_yawl fire/3 timeout for ~p after ~p ms", [Trsn, FireTimeout]),
                        {error, {fire_timeout, Trsn}}
                    end
                end,

                case FireResult of
                    {produce, ProdMap} ->
                        %% Standard 2-tuple return - no user info update
                        {delta, Mode, ProdMap, undefined};

                    {produce, ProdMap, NewUsrInfo} ->
                        %% Enhanced 3-tuple return - user info will be updated
                        {delta, Mode, ProdMap, NewUsrInfo};

                    abort ->
                        %% Try another mode or transition
                        ModeLst1 = ModeLst -- [Mode],
                        case ModeLst1 of
                            [] ->
                                attempt_progress(maps:remove(Trsn, ModeMap), NetMod, UsrInfo, FireTimeout);
                            [_ | _] ->
                                attempt_progress(ModeMap#{Trsn := ModeLst1}, NetMod, UsrInfo, FireTimeout)
                        end;

                    {error, _Reason} = ErrorTuple ->
                        ErrorTuple
                end

            catch
                Type:Exception:Stacktrace ->
                    logger:error("gen_yawl fire/3 exception: ~p:~p~n~p",
                                [Type, Exception, Stacktrace]),
                    %% Continue trying other transitions
                    attempt_progress_with_mode_removed(ModeMap, Trsn, ModeLst, Mode, NetMod, UsrInfo, FireTimeout)
            end
    end.

%% @private
-spec attempt_progress_with_mode_removed(
        #{atom() => [_]},
        atom(),
        [#{atom() => [_]}],
        #{atom() => [_]},
        atom(),
        term(),
        pos_integer()) ->
    abort | {delta, #{atom() => [term()]}, #{atom() => [term()]}, term() | undefined} |
    {error, term(), #net_state{}}.

attempt_progress_with_mode_removed(ModeMap, Trsn, ModeLst, Mode, NetMod, UsrInfo, FireTimeout) ->
    ModeLst1 = ModeLst -- [Mode],
    case ModeLst1 of
        [] ->
            attempt_progress(maps:remove(Trsn, ModeMap), NetMod, UsrInfo, FireTimeout);
        [_ | _] ->
            attempt_progress(ModeMap#{Trsn := ModeLst1}, NetMod, UsrInfo, FireTimeout)
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc Consume tokens from the marking based on the firing mode.
%%
%%      Removes tokens specified in the mode from the marking.
%%
%% @end
%%--------------------------------------------------------------------
-spec cns(Mode :: #{atom() => [term()]}, NetState :: #net_state{}) -> #net_state{}.

cns(Mode, NetState = #net_state{marking = Marking}) ->
    F = fun(T, TkLst, Acc) ->
                Acc#{T => TkLst -- maps:get(T, Mode, [])}
        end,

    NetState#net_state{marking = maps:fold(F, #{}, Marking)}.


%%--------------------------------------------------------------------
%% @private
%% @doc Produce tokens on the marking.
%%
%%      Adds tokens from ProdMap to the marking.
%%
%% @end
%%--------------------------------------------------------------------
-spec prd(ProdMap :: #{atom() => [term()]}, NetState :: #net_state{}) -> #net_state{}.

prd(ProdMap, NetState = #net_state{marking = Marking}) ->
    F = fun(T, TkLst, Acc) ->
                Acc#{T => TkLst ++ maps:get(T, ProdMap, [])}
        end,

    NetState#net_state{marking = maps:fold(F, #{}, Marking)}.


%%--------------------------------------------------------------------
%% @private
%% @doc Handle trigger callback for each token produced.
%%
%%      Calls the module's trigger/3 for each token, allowing the
%%      module to pass or drop individual tokens.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_trigger(ProdMap :: #{atom() => [term()]},
                     NetState :: #net_state{},
                     NetMod :: atom()) -> #net_state{}.

handle_trigger(ProdMap, NetState, NetMod) ->
    G = fun(P, TkLst, Acc) ->
                F = fun(Tk, A) ->
                            case NetMod:trigger(P, Tk, NetState) of
                                pass -> [Tk | A];
                                drop -> A
                            end
                    end,

                TkLst1 = lists:foldl(F, [], TkLst),
                Acc#{P => TkLst1}
        end,

    ProdMap1 = maps:fold(G, #{}, ProdMap),
    prd(ProdMap1, NetState).


%%--------------------------------------------------------------------
%% @private
%% @doc Enumerate all possible firing modes for a transition.
%%
%%      Generates all combinations of token consumptions from the
%%      preset places.
%%
%% @end
%%--------------------------------------------------------------------
-spec enum_mode(Preset :: [atom()], Marking :: #{atom() => [term()]}) ->
          [#{atom() => [term()]}].

enum_mode(Preset, Marking) ->
    F = fun(P, Acc) ->
                N = maps:get(P, Acc, 0),
                Acc#{P => N + 1}
        end,

    CountMap = lists:foldl(F, #{}, Preset),

    G = fun(P, N, Acc) ->
                #{P := TkLst} = Marking,
                Acc#{P => cnr(N, TkLst)}
        end,

    CmbMap = maps:fold(G, #{}, CountMap),

    permut_map(CmbMap).

%%--------------------------------------------------------------------
%% @private
%% @doc Pick a random element from a list.
%%
%% @end
%%--------------------------------------------------------------------
-spec pick_from([T]) -> T.

pick_from([]) ->
    error(empty_list);
pick_from(List) ->
    lists:nth(rand:uniform(length(List)), List).

%%--------------------------------------------------------------------
%% @private
%% @doc Generate combinations of N elements from a list.
%%
%% @end
%%--------------------------------------------------------------------
-spec cnr(N :: non_neg_integer(), Lst :: [T]) -> [[T]].

cnr(0, _Lst) ->
    [[]];
cnr(_N, []) ->
    [];
cnr(N, [H | T]) ->
    [[H | C] || C <- cnr(N - 1, T)] ++ cnr(N, T).

%%--------------------------------------------------------------------
%% @private
%% @doc Generate permutations of map values.
%%
%% @end
%%--------------------------------------------------------------------
-spec permut_map(#{K => [V]}) -> [#{K => V}].

permut_map(Map) when map_size(Map) =:= 0 ->
    [#{}];
permut_map(Map) ->
    Keys = maps:keys(Map),
    permut_map_keys(Keys, Map, [#{}]).

permut_map_keys([], _Map, Acc) ->
    Acc;
permut_map_keys([K | Rest], Map, Acc) ->
    Values = maps:get(K, Map),
    NewAcc = [{maps:put(K, V, M)} || M <- Acc, V <- Values],
    permut_map_keys(Rest, Map, NewAcc).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).
-endif.
