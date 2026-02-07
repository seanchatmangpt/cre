%% -*- erlang -*-
%%%% @doc gen_pnet - A generic Petri net OTP behavior.
%%
%% `gen_pnet' is a behavior module for implementing Petri net workflows
%% as Erlang/OTP gen_server processes. It provides automatic transition
%% firing, token processing, and statistics tracking.
%%
%% <h3>Net Structure Callback Functions</h3>
%%
%% Six callbacks define the Petri net structure and initial marking:
%%
%% <ul>
%%   <li>`place_lst/0' - Returns the names of all places in the net</li>
%%   <li>`trsn_lst/0' - Returns the names of all transitions in the net</li>
%%   <li>`init_marking/2' - Returns the initial marking for a given place</li>
%%   <li>`preset/1' - Returns the preset (input) places of a transition</li>
%%   <li>`is_enabled/3' - Determines if a transition is enabled in a mode</li>
%%   <li>`fire/3' - Returns tokens produced when a transition fires</li>
%% </ul>
%%
%% <h3>Interface Callback Functions</h3>
%%
%% Seven callbacks determine how the net instance appears as an Erlang
%% process to the outside world:
%%
%% <ul>
%%   <li>`init/1' - Initializes the net instance</li>
%%   <li>`handle_call/3' - Synchronous message exchange</li>
%%   <li>`handle_cast/2' - Asynchronous message reception</li>
%%   <li>`handle_info/2' - Unformatted message reception</li>
%%   <li>`code_change/3' - Hot code reload handling</li>
%%   <li>`terminate/2' - Cleanup on termination</li>
%%   <li>`trigger/3' - Side effects when tokens are produced</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_pnet).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API functions
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
         state_property/3,
         inject/2,
         step/1,
         drain/2]).

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
%% Includes
%%====================================================================

-include("gen_pnet.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A process name or identifier.
%%
%% Can be an atom (local name), a tuple (for via or global registration),
%% or a direct pid reference.
%%--------------------------------------------------------------------
-type name() :: atom() |
                {atom(), atom()} |
                {global, _} |
                {via, atom(), _} |
                pid().

%%--------------------------------------------------------------------
%% @doc Server name specification for gen_server registration.
%%
%% Follows gen_server conventions: `{local, Name}', `{global, Name}',
%% or `{via, Module, ViaName}'.
%%--------------------------------------------------------------------
-type server_name() :: {local, atom()} |
                       {global, atom()} |
                       {via, atom(), _}.

%%--------------------------------------------------------------------
%% @doc Result type for start_link functions.
%%
%% Returns `{ok, Pid}' on success, `ignore' if the process voluntarily
%% terminates, or `{error, Reason}' on failure.
%%--------------------------------------------------------------------
-type start_link_result() :: {ok, pid()} |
                             ignore |
                             {error, _}.

%%--------------------------------------------------------------------
%% @doc Process configuration options.
%%
%% Supports standard gen_server options including debug settings,
%% hibernation, spawn options, and timeout specification.
%%--------------------------------------------------------------------
-type prop() :: {debug, [log | statistics | trace | {_, _}]} |
                {hibernate_after, infinity | non_neg_integer()} |
                {spawn_opt, [link | monitor | {_, _}]} |
                {timeout, infinity | non_neg_integer()}.

%% Export types
-export_type([name/0, server_name/0, start_link_result/0, prop/0]).

%%====================================================================
%% Callback Definitions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Structure callbacks defining the Petri net topology.
%%--------------------------------------------------------------------

%% Returns the list of place names in the net.
-callback place_lst() -> [atom()].

%% Returns the list of transition names in the net.
-callback trsn_lst() -> [atom()].

%% Returns the initial marking for a given place.
%% The UsrInfo comes from init/1.
-callback init_marking(Place :: atom(), UsrInfo :: _) -> [_].

%% Returns the preset (input) places for a given transition.
-callback preset(Trsn :: atom()) -> [atom()].

%% Determines if a transition is enabled in a given mode.
%% A mode maps places to token lists to be consumed.
-callback is_enabled(Trsn :: atom(), Mode :: #{atom() => [_]}, UsrInfo :: _) ->
              boolean().

%% Returns tokens produced when a transition fires in a given mode.
%% Only called for modes where is_enabled returns true.
%% Returns `{produce, ProduceMap}' to produce tokens or `abort' to cancel.
-callback fire(Trsn :: atom(), Mode :: #{atom() => [_]}, UsrInfo :: _) ->
              abort | {produce, #{atom() => [_]}}.

%%--------------------------------------------------------------------
%% @doc Interface callbacks for process interaction.
%%--------------------------------------------------------------------

%% Determines if a produced token should pass or be dropped.
%% Called for each token in the produce map.
-callback trigger(Place :: atom(), Token :: _, NetState :: #net_state{}) ->
              pass | drop.

%% Handles hot code reload.
%% Identical to gen_server:code_change/3.
-callback code_change(OldVsn :: _, NetState :: #net_state{}, Extra :: _) ->
              {ok, #net_state{}} | {error, _}.

%% Handles synchronous calls from gen_pnet:call/2,3.
%% Can return:
%%   - `{reply, Reply}' - reply without changing marking
%%   - `{reply, Reply, ProduceMap}' - reply and produce tokens
%%   - `noreply' - defer reply, no marking change
%%   - `{noreply, ProduceMap}' - defer reply and produce tokens
%%   - `{stop, Reason, Reply}' - stop the net
-callback handle_call(Request :: _,
                      From :: {pid(), _},
                      NetState :: #net_state{}) ->
              {reply, _} |
              {reply, _, #{atom() => [_]}} |
              noreply |
              {noreply, #{atom() => [_]}} |
              {stop, _, _}.

%% Handles asynchronous casts from gen_pnet:cast/2.
%% Can return:
%%   - `noreply' - no marking change
%%   - `{noreply, ProduceMap}' - produce tokens
%%   - `{stop, Reason}' - stop the net
-callback handle_cast(Request :: _, NetState :: #net_state{}) ->
              noreply |
              {noreply, #{atom() => [_]}} |
              {stop, _}.

%% Handles unformatted messages.
%% Same return conventions as handle_cast/2.
-callback handle_info(Info :: _, NetState :: #net_state{}) ->
              noreply |
              {noreply, #{atom() => [_]}} |
              {stop, _}.

%% Initializes the net instance.
%% Receives the NetArg from start_link/3,4.
%% Returns the UsrInfo that will be passed to other callbacks.
-callback init(NetArg :: _) -> _.

%% Cleanup on termination.
%% Identical to gen_server:terminate/2.
-callback terminate(Reason :: _, NetState :: #net_state{}) -> ok.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts an unregistered gen_pnet net instance.
%%
%% Creates a new Petri net process without registering it.
%% The NetArg is passed to the module's init/1 callback.
%%
%% === Example ===
%% ```
%% {ok, Pid} = gen_pnet:start_link(my_net, InitArg, []).
%% '''
%%
%% @see start_link/4
%% @end
%%--------------------------------------------------------------------
-spec start_link(NetMod :: atom(), NetArg :: term(), Options :: [prop()]) ->
          start_link_result().

start_link(NetMod, NetArg, Options)
  when is_atom(NetMod), is_list(Options) ->
    gen_server:start_link(?MODULE, {NetMod, NetArg}, Options).

%%--------------------------------------------------------------------
%% @doc Starts a registered gen_pnet net instance.
%%
%% Creates a new Petri net process and registers it as ServerName.
%% ServerName can be `{local, Name}', `{global, Name}', or
%% `{via, Module, ViaName}'.
%%
%% === Example ===
%% ```
%% {ok, Pid} = gen_pnet:start_link({local, my_net}, my_net, InitArg, []).
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
    gen_server:start_link(ServerName, ?MODULE, {NetMod, InitArg}, Options).

%%--------------------------------------------------------------------
%% @doc Query the list of tokens on a place.
%%
%% Returns `{ok, TokenList}' if the place exists, or `{ok, []}' for
%% non-existent places (total function). Uses a gen_server call.
%%
%% === Example ===
%% ```
%% {ok, Tokens} = gen_pnet:ls(Pid, place1).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec ls(Name :: name(), Place :: atom()) -> {ok, [_]} | {error, #bad_place{}}.

ls(Name, Place) when is_atom(Place) ->
    gen_server:call(Name, {ls, Place}).

%%--------------------------------------------------------------------
%% @doc Query the marking map of the net instance.
%%
%% Returns a map associating each place name with its token list.
%% The marking represents the current state of all places in the net.
%%
%% === Example ===
%% ```
%% Marking = gen_pnet:marking(Pid).
%% #{place1 := Tokens, place2 := []} = Marking.
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec marking(Name :: name()) -> #{atom() => [_]}.

marking(Name) ->
    gen_server:call(Name, marking).

%%--------------------------------------------------------------------
%% @doc Query the user info term from the net instance.
%%
%% Returns the user info field created by init/1 and potentially
%% modified during net execution.
%%
%% === Example ===
%% ```
%% UsrInfo = gen_pnet:usr_info(Pid).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec usr_info(Name :: name()) -> _.

usr_info(Name) ->
    gen_server:call(Name, usr_info).

%%--------------------------------------------------------------------
%% @doc Query the statistics gathered by the net instance.
%%
%% Returns throughput statistics as a #stats{} record with current,
%% maximum, and minimum firings per second.
%%
%% === Example ===
%% ```
%% #stats{current = #stat{fps = Fps}} = gen_pnet:stats(Pid).
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
%% Resets all throughput statistics to undefined.
%%
%% === Example ===
%% ```
%% ok = gen_pnet:reset_stats(Pid).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_stats(Name :: name()) -> ok.

reset_stats(Name) ->
    gen_server:call(Name, reset_stats).

%%--------------------------------------------------------------------
%% @doc Stop the gen_pnet process.
%%
%% Terminates the net instance gracefully.
%%
%% === Example ===
%% ```
%% ok = gen_pnet:stop(Pid).
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
%% Sends Request to the module's handle_call/3 callback.
%% Timeout is implicitly set to 5 seconds.
%%
%% === Example ===
%% ```
%% Reply = gen_pnet:call(Pid, get_status).
%% '''
%%
%% @see call/3
%% @end
%%--------------------------------------------------------------------
-spec call(Name :: name(), Request :: term()) -> term().

call(Name, Request) ->
    gen_server:call(Name, {call, Request}).

%%--------------------------------------------------------------------
%% @doc Synchronously send a request with explicit timeout.
%%
%% Same as call/2 but allows specifying the timeout duration.
%% Timeout must be a non-negative integer or infinity.
%%
%% === Example ===
%% ```
%% Reply = gen_pnet:call(Pid, get_status, 10000).
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
%% The request is handled by the module's handle_cast/2 callback.
%% The cast succeeds even if the process doesn't exist.
%%
%% === Example ===
%% ```
%% ok = gen_pnet:cast(Pid, {update, Data}).
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
%% Use when replying was deferred by returning `{noreply, _, _}'
%% in handle_call/3.
%%
%% === Example ===
%% ```
%% gen_pnet:reply(Client, Response).
%% '''
%%
%% @see handle_call/3
%% @end
%%--------------------------------------------------------------------
-spec reply(Client :: {pid(), gen_server:reply_tag()}, Reply :: term()) -> ok.

reply(Client, Reply) when is_tuple(Client) ->
    gen_server:reply(Client, Reply).

%%--------------------------------------------------------------------
%% @doc Check if a predicate about the state holds.
%%
%% The predicate function receives token lists for each place in PlaceLst.
%% Returns `ok' if the predicate succeeds, or `{error, Reason}' if it fails.
%%
%% === Example ===
%% ```
%% Pred = fun([A]) -> if length(A) =:= 1 -> ok; true -> {error, bad_len} end end.
%% ok = gen_pnet:state_property(Pid, Pred, [place1]).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec state_property(Name :: name(),
                     Pred :: fun((...) -> ok | {error, _}),
                     PlaceLst :: [atom()]) ->
          ok | {error, _}.

state_property(Name, Pred, PlaceLst)
  when is_list(PlaceLst),
       is_function(Pred, length(PlaceLst)) ->
    Marking = gen_pnet:marking(Name),
    ArgLst = [ maps:get(Place, Marking) || Place <- PlaceLst ],
    apply(Pred, ArgLst).

%%--------------------------------------------------------------------
%% @doc Inject tokens into the net.
%%
%% Injects the tokens specified in ProduceMap into the marking.
%% Returns a receipt containing the injection result, or an error if
%% the injection fails. Triggers trigger/3 filtering for each token.
%%
%% === Example ===
%% ```
%% {ok, Receipt} = gen_pnet:inject(Pid, #{place1 => [token1]}).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec inject(Name :: name(), ProduceMap :: #{atom() => [_]}) ->
          {ok, Receipt :: #{atom() => [_]}} | {error, Reason :: term()}.

inject(Name, ProduceMap) when is_map(ProduceMap) ->
    gen_server:call(Name, {inject, ProduceMap}).

%%--------------------------------------------------------------------
%% @doc Fire at most one enabled transition.
%%
%% Attempts to fire a single enabled transition. Returns the receipt
%% from the fired transition, or `abort' if no transition is enabled.
%%
%% === Example ===
%% ```
%% abort = gen_pnet:step(Pid).  % No transitions enabled
%% {ok, Receipt} = gen_pnet:step(Pid).  % Transition fired
%% '''
%%
%% @see drain/2
%% @end
%%--------------------------------------------------------------------
-spec step(Name :: name()) -> abort | {ok, Receipt :: #{atom() => [_]}}.

step(Name) ->
    gen_server:call(Name, step).

%%--------------------------------------------------------------------
%% @doc Fire transitions until none enabled or MaxSteps reached.
%%
%% Repeatedly fires enabled transitions, collecting receipts from each
%% firing. Stops when no transitions are enabled or MaxSteps is reached.
%% Returns `{ok, Receipts}' with receipts in firing order, or
%% `{error, limit}' if MaxSteps was reached before quiescence.
%%
%% === Example ===
%% ```
%% {ok, Receipts} = gen_pnet:drain(Pid, 100).
%% {error, limit} = gen_pnet:drain(Pid, 5).  % Hit step limit
%% '''
%%
%% @see step/1
%% @end
%%--------------------------------------------------------------------
-spec drain(Name :: name(), MaxSteps :: non_neg_integer()) ->
          {ok, [Receipt :: #{atom() => [_]}]} | {error, limit}.

drain(Name, MaxSteps) when is_integer(MaxSteps), MaxSteps >= 0 ->
    gen_server:call(Name, {drain, MaxSteps, []}).

%%====================================================================
%% Net State Accessor Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Extract tokens on a place from a net state.
%%
%% Throws an error if the place does not exist in the marking.
%%
%% === Example ===
%% ```
%% Tokens = gen_pnet:get_ls(place1, NetState).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec get_ls(Place :: atom(), NetState :: #net_state{}) -> [_].

get_ls(Place, #net_state{marking = Marking}) ->
    maps:get(Place, Marking).

%%--------------------------------------------------------------------
%% @doc Extract user info from a net state.
%%
%% Returns the user info field from the net_state record.
%%
%% === Example ===
%% ```
%% UsrInfo = gen_pnet:get_usr_info(NetState).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec get_usr_info(NetState :: #net_state{}) -> _.

get_usr_info(#net_state{usr_info = UsrInfo}) ->
    UsrInfo.

%%--------------------------------------------------------------------
%% @doc Extract stats from a net state.
%%
%% Returns the statistics record from the net_state.
%%
%% === Example ===
%% ```
%% Stats = gen_pnet:get_stats(NetState).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec get_stats(NetState :: #net_state{}) -> #stats{}.

get_stats(#net_state{stats = Stats}) ->
    Stats.

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Handle code change during hot code upgrade.
%%
%% Delegates to the callback module's code_change/3.
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: _, NetState :: #net_state{}, Extra :: _) ->
          {ok, #net_state{}} | {error, _}.

code_change(OldVsn, NetState = #net_state{net_mod = NetMod}, Extra) ->
    NetMod:code_change(OldVsn, NetState, Extra).

%%--------------------------------------------------------------------
%% @private
%% @doc Handle synchronous calls.
%%
%% Handles built-in requests (ls, marking, usr_info, stats, reset_stats)
%% and delegates custom calls to the callback module's handle_call/3.
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(),
                  From :: {pid(), _},
                  NetState :: #net_state{}) ->
          {reply, _, #net_state{}} |
          {noreply, #net_state{}} |
          {stop, _, _, #net_state{}}.

handle_call({ls, Place}, _From, NetState = #net_state{marking = Marking}) ->
    Reply = case maps:is_key(Place, Marking) of
                true -> {ok, maps:get(Place, Marking)};
                false -> {error, #bad_place{name = Place}}
            end,
    {reply, Reply, NetState};

handle_call(marking, _From, NetState = #net_state{marking = Marking}) ->
    {reply, Marking, NetState};

handle_call(usr_info, _From, NetState = #net_state{usr_info = UsrInfo}) ->
    {reply, UsrInfo, NetState};

handle_call({call, Request}, From,
            NetState = #net_state{net_mod = NetMod}) ->
    case NetMod:handle_call(Request, From, NetState) of
        {reply, Reply} ->
            {reply, Reply, NetState};
        {reply, Reply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState),
            continue(self()),
            {reply, Reply, NetState1};
        noreply ->
            {noreply, NetState};
        {noreply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState),
            continue(self()),
            {noreply, NetState1};
        {stop, Reason, Reply} ->
            {stop, Reason, Reply, NetState}
    end;

handle_call(stats, _From, NetState = #net_state{stats = Stats}) ->
    {reply, Stats, NetState};

handle_call(reset_stats, _From, NetState) ->
    {reply, ok, NetState#net_state{stats = undefined}};

handle_call({inject, ProduceMap}, _From, NetState) ->
    try
        NetState1 = handle_trigger(ProduceMap, NetState),
        Receipt = ProduceMap,
        {reply, {ok, Receipt}, NetState1}
    catch
        _:_:Stack ->
            {reply, {error, Stack}, NetState}
    end;

handle_call(step, _From, NetState) ->
    case fire_transition(NetState) of
        abort ->
            {reply, abort, NetState};
        {ok, Receipt, NetState1} ->
            {reply, {ok, Receipt}, NetState1}
    end;

handle_call({drain, MaxSteps, _Acc}, _From, NetState) when MaxSteps =< 0 ->
    {reply, {error, limit}, NetState};

handle_call({drain, MaxSteps, Acc}, _From, NetState) ->
    case fire_transition(NetState) of
        abort ->
            {reply, {ok, lists:reverse(Acc)}, NetState};
        {ok, Receipt, NetState1} ->
            handle_call({drain, MaxSteps - 1, [Receipt | Acc]}, _From, NetState1)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle asynchronous messages.
%%
%% Handles the built-in 'continue' message for progress loop and
%% delegates custom casts to the callback module's handle_cast/2.
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), NetState :: #net_state{}) ->
          {noreply, #net_state{}} |
          {stop, _, #net_state{}}.

handle_cast(continue,
            NetState = #net_state{
                          stats = Stats,
                          tstart = T1,
                          cnt = Cnt
                         }) ->
    case progress(NetState) of
        abort ->
            {noreply, NetState};
        {delta, Mode, Pm} ->
            NetState1 = cns(Mode, NetState),
            NetState2 = handle_trigger(Pm, NetState1),
            continue(self()),
            NetState3 = update_stats(NetState2, Stats, T1, Cnt),
            {noreply, NetState3}
    end;

handle_cast({cast, Request}, NetState = #net_state{net_mod = NetMod}) ->
    case NetMod:handle_cast(Request, NetState) of
        noreply ->
            {noreply, NetState};
        {noreply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState),
            continue(self()),
            {noreply, NetState1};
        {stop, Reason} ->
            {stop, Reason, NetState}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle unexpected messages.
%%
%% Delegates to the callback module's handle_info/2.
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), NetState :: #net_state{}) ->
          {noreply, #net_state{}} |
          {stop, _, #net_state{}}.

handle_info(Info, NetState = #net_state{net_mod = NetMod}) ->
    case NetMod:handle_info(Info, NetState) of
        noreply ->
            {noreply, NetState};
        {noreply, ProdMap} ->
            NetState1 = handle_trigger(ProdMap, NetState),
            continue(self()),
            {noreply, NetState1};
        {stop, Reason} ->
            {stop, Reason, NetState}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Initialize the gen_pnet instance.
%%
%% Calls the module's init/1 to get UsrInfo, initializes the marking
%% from place_lst and init_marking/2, and starts the progress loop.
%% @end
%%--------------------------------------------------------------------
-spec init({NetMod :: atom(), NetArg :: term()}) -> {ok, #net_state{}}.

init({NetMod, NetArg}) ->
    UsrInfo = NetMod:init(NetArg),
    PlaceLst = NetMod:place_lst(),
    F = fun(P, Acc) ->
                Acc#{P => NetMod:init_marking(P, UsrInfo)}
        end,
    InitMarking = lists:foldl(F, #{}, PlaceLst),
    continue(self()),
    {ok, #net_state{
           net_mod = NetMod,
           usr_info = UsrInfo,
           marking = InitMarking,
           stats = undefined,
           tstart = os:system_time(),
           cnt = 0
          }}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle process termination.
%%
%% Delegates to the callback module's terminate/2.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: _, NetState :: #net_state{}) -> ok.

terminate(Reason, NetState = #net_state{net_mod = NetMod}) ->
    NetMod:terminate(Reason, NetState).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Continue the progress loop.
%%
%% Sends a continue message to self to trigger transition processing.
%% @end
%%--------------------------------------------------------------------
-spec continue(Name :: pid()) -> ok.

continue(Name) ->
    gen_server:cast(Name, continue).

%%--------------------------------------------------------------------
%% @private
%% @doc Update statistics in the net state.
%%
%% Computes throughput (firings per second) and updates stats every
%% 1000 firings.
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
%% Finds enabled transitions and attempts to fire one.
%% Returns {delta, Mode, ProduceMap} on success or abort if no
%% transition is enabled.
%% @end
%%--------------------------------------------------------------------
-spec progress(NetState :: #net_state{}) ->
          abort | {delta, #{atom() => [_]}, #{atom() => [_]}}.

progress(#net_state{
           marking = Marking,
           net_mod = NetMod,
           usr_info = UsrInfo
          }) ->
    TrsnLst = NetMod:trsn_lst(),
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
    attempt_progress(ModeMap, NetMod, UsrInfo).

%%--------------------------------------------------------------------
%% @private
%% @doc Attempt to fire an enabled transition.
%%
%% Picks a random transition and mode, then attempts to fire it.
%% Returns {delta, Mode, ProduceMap} on success or abort if all
%% attempts fail.
%% @end
%%--------------------------------------------------------------------
-spec attempt_progress(ModeMap :: #{atom() => [_]},
                       NetMod :: atom(),
                       UsrInfo :: _) ->
          abort | {delta, #{atom() => [_]}, #{atom() => [_]}}.

attempt_progress(ModeMap, NetMod, UsrInfo) ->
    case maps:size(ModeMap) of
        0 ->
            abort;
        _ ->
            TrsnLst = maps:keys(ModeMap),
            Trsn = pick_from(TrsnLst),
            #{Trsn := ModeLst} = ModeMap,
            Mode = pick_from(ModeLst),
            case NetMod:fire(Trsn, Mode, UsrInfo) of
                {produce, ProdMap} ->
                    {delta, Mode, ProdMap};
                abort ->
                    ModeLst1 = ModeLst -- [Mode],
                    case ModeLst1 of
                        [] ->
                            attempt_progress(maps:remove(Trsn, ModeMap), NetMod, UsrInfo);
                        [_ | _] ->
                            attempt_progress(ModeMap#{Trsn := ModeLst1}, NetMod, UsrInfo)
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Consume tokens from the marking based on the firing mode.
%%
%% Removes tokens specified in the mode from each place.
%% @end
%%--------------------------------------------------------------------
-spec cns(Mode :: #{atom() => [_]}, NetState :: #net_state{}) -> #net_state{}.

cns(Mode, NetState = #net_state{marking = Marking}) ->
    F = fun(T, TkLst, Acc) ->
                Acc#{T => TkLst -- maps:get(T, Mode, [])}
        end,
    NetState#net_state{marking = maps:fold(F, #{}, Marking)}.

%%--------------------------------------------------------------------
%% @private
%% @doc Produce tokens on the marking.
%%
%% Adds tokens from ProduceMap to each place.
%% @end
%%--------------------------------------------------------------------
-spec prd(ProdMap :: #{atom() => [_]}, NetState :: #net_state{}) -> #net_state{}.

prd(ProdMap, NetState = #net_state{marking = Marking}) ->
    F = fun(T, TkLst, Acc) ->
                Acc#{T => TkLst ++ maps:get(T, ProdMap, [])}
        end,
    NetState#net_state{marking = maps:fold(F, #{}, Marking)}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle trigger callback for each produced token.
%%
%% Calls the module's trigger/3 for each token, allowing the module
%% to pass or drop individual tokens.
%% @end
%%--------------------------------------------------------------------
-spec handle_trigger(ProdMap :: #{atom() => [_]},
                     NetState :: #net_state{}) -> #net_state{}.

handle_trigger(ProdMap, NetState = #net_state{net_mod = NetMod}) ->
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
%% Generates all combinations of token consumptions from the preset
%% places.
%% @end
%%--------------------------------------------------------------------
-spec enum_mode(Preset :: [atom()], Marking :: #{atom() => [_]}) ->
          [#{atom() => [_]}].

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
%% Returns a random element from the non-empty list.
%% @end
%%--------------------------------------------------------------------
-spec pick_from([T, ...]) -> T.

pick_from([]) ->
    error(empty_list);
pick_from(List) ->
    lists:nth(rand:uniform(length(List)), List).

%%--------------------------------------------------------------------
%% @private
%% @doc Generate combinations of N elements from a list.
%%
%% Returns all possible combinations of N elements from Lst.
%% @end
%%--------------------------------------------------------------------
-spec cnr(N :: non_neg_integer(), Lst :: [_]) -> [[_]].

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
%% Returns all possible maps formed by picking one value from each
%% key's value list.
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
    NewAcc = [maps:put(K, V, M) || M <- Acc, V <- Values],
    permut_map_keys(Rest, Map, NewAcc).

%%--------------------------------------------------------------------
%% @private
%% @doc Fire a single transition without recursion.
%%
%% Finds an enabled transition and fires it once. Used by step/1 and
%% drain/2 for non-recursive execution. Returns abort if no transition
%% is enabled, or {ok, Receipt, NetState} on successful firing.
%% @end
%%--------------------------------------------------------------------
-spec fire_transition(NetState :: #net_state{}) ->
          abort | {ok, Receipt :: #{atom() => [_]}, NetState :: #net_state{}}.

fire_transition(NetState = #net_state{
           marking = Marking,
           net_mod = NetMod,
           usr_info = UsrInfo
          }) ->
    TrsnLst = NetMod:trsn_lst(),
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
    attempt_fire_one(ModeMap, NetMod, UsrInfo, NetState).

%%--------------------------------------------------------------------
%% @private
%% @doc Attempt to fire a single enabled transition.
%%
%% Picks a random transition and mode from ModeMap and attempts to fire.
%% Returns abort if all attempts fail, or {ok, Receipt, NetState} on
%% success. Receipt is the map of consumed tokens.
%% @end
%%--------------------------------------------------------------------
-spec attempt_fire_one(ModeMap :: #{atom() => [_]},
                       NetMod :: atom(),
                       UsrInfo :: _,
                       NetState :: #net_state{}) ->
          abort | {ok, Receipt :: #{atom() => [_]}, NetState :: #net_state{}}.

attempt_fire_one(ModeMap, NetMod, UsrInfo, NetState) ->
    case maps:size(ModeMap) of
        0 ->
            abort;
        _ ->
            TrsnLst = maps:keys(ModeMap),
            Trsn = pick_from(TrsnLst),
            #{Trsn := ModeLst} = ModeMap,
            Mode = pick_from(ModeLst),
            case NetMod:fire(Trsn, Mode, UsrInfo) of
                {produce, ProdMap} ->
                    %% Consume tokens, then produce new ones
                    NetState1 = cns(Mode, NetState),
                    NetState2 = handle_trigger(ProdMap, NetState1),
                    %% Receipt is the consumed mode
                    {ok, Mode, NetState2};
                abort ->
                    ModeLst1 = ModeLst -- [Mode],
                    case ModeLst1 of
                        [] ->
                            ModeMap1 = maps:remove(Trsn, ModeMap),
                            attempt_fire_one(ModeMap1, NetMod, UsrInfo, NetState);
                        [_ | _] ->
                            ModeMap1 = ModeMap#{Trsn := ModeLst1},
                            attempt_fire_one(ModeMap1, NetMod, UsrInfo, NetState)
                    end
            end
    end.

%%====================================================================
%% Doctests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

-endif.
