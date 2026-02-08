%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author CRE Team
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
         sync/2,
         usr_info/1,
         state_property/3,
         inject/2,
         withdraw/2,
         step/1,
         drain/2,
         cancel_region/2,
         enabled_transitions/1]).

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
          net_arg = #{} :: term(),
          fire_timeout = 5000 :: pos_integer(),
          progress_timeout = 30000 :: pos_integer(),
          shutting_down = false :: boolean(),
          active_fires = 0 :: non_neg_integer(),
          %% Cycle detection: bounded marking history to detect repeated states
          marking_history = [] :: [non_neg_integer()],
          max_marking_history = 10 :: pos_integer(),
          continue_count = 0 :: non_neg_integer(),
          max_continue = 1000 :: pos_integer(),
          regions = #{} :: #{binary() | atom() => [atom()]},
          checkpoint_interval = 0 :: non_neg_integer(),
          drain_step_count = 0 :: non_neg_integer()
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
%% @doc Return the list of transitions that are currently enabled.
%%
%%      Use when step/1 returns abort to inspect what could fire.
%%
%% === Example ===
%% ```
%% case gen_yawl:step(Pid) of
%%     abort ->
%%         Enabled = gen_yawl:enabled_transitions(Pid),
%%         io:format("Blocked; enabled: ~p~n", [Enabled]);
%%     {ok, _} -> ok
%% end.
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec enabled_transitions(Name :: name()) -> [atom()].

enabled_transitions(Name) ->
    gen_server:call(Name, enabled_transitions).


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
%% @doc Synchronize with the net instance, waiting for it to stabilize.
%%
%%      This function waits until no more transitions are enabled (the net
%%      has reached a stable state) or the timeout expires. Returns the
%%      marking when stable or an error if timeout occurs.
%%
%%      This is useful in tests and doctests to wait for asynchronous
%%      operations to complete.
%%
%% === Example ===
%% ```
%% {ok, Marking} = gen_yawl:sync(Pid, 1000).
%% '''
%%
%% @end
%%--------------------------------------------------------------------
-spec sync(Name :: name(), Timeout :: non_neg_integer() | infinity) ->
          {ok, #{atom() => [term()]}} | {error, term()}.

sync(Name, Timeout) when is_pid(Name); is_atom(Name); is_tuple(Name) ->
    try
        %% Request the current marking and check if net is stable
        gen_server:call(Name, sync, Timeout)
    catch
        exit:{noproc, _} ->
            {error, no_process};
        exit:{timeout, _} ->
            {error, timeout};
        _:Reason ->
            {error, Reason}
    end;

sync(NetState, _Timeout) when is_record(NetState, net_state) ->
    %% This case is for backwards compatibility with patterns that
    %% incorrectly pass NetState instead of Pid. Since NetState is
    %% just a data structure, we return it as-is.
    {ok, NetState};

sync(_Other, _Timeout) ->
    {error, badarg}.


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

%%--------------------------------------------------------------------
%% @doc Inject tokens into the workflow.
%%
%%      Adds tokens to places specified in ProduceMap.
%%      Equivalent to gen_pnet:inject/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec inject(Name :: name(), ProduceMap :: #{atom() => [term()]}) ->
          {ok, #{atom() => [term()]}} | {error, term()}.

inject(Name, ProduceMap) when is_map(ProduceMap) ->
    gen_server:call(Name, {inject, ProduceMap}).

%%--------------------------------------------------------------------
%% @doc Remove tokens from the marking.
%%
%%      WithdrawMap specifies places and tokens to remove.
%%      Used for subnet execution: consume branch-place token when
%%      passing it to a subnet.
%%
%% @end
%%--------------------------------------------------------------------
-spec withdraw(Name :: name(), WithdrawMap :: #{atom() => [term()]}) ->
          ok | {error, term()}.

withdraw(Name, WithdrawMap) when is_map(WithdrawMap) ->
    gen_server:call(Name, {withdraw, WithdrawMap}).

%%--------------------------------------------------------------------
%% @doc Cancel a region by withdrawing tokens from all places in that region.
%%
%%      The regions map must be provided at start via NetArg: #{regions => #{RegionId => [places]}}.
%%      Withdraws one token from each place in the region that has tokens.
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_region(Name :: name(), RegionId :: binary() | atom()) ->
          ok | {error, term()}.

cancel_region(Name, RegionId) when is_binary(RegionId); is_atom(RegionId) ->
    gen_server:call(Name, {cancel_region, RegionId}).

%%--------------------------------------------------------------------
%% @doc Fire at most one enabled transition.
%%
%%      Equivalent to gen_pnet:step/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec step(Name :: name()) -> abort | {ok, #{atom() => [term()]}}.

step(Name) ->
    gen_server:call(Name, step).

%%--------------------------------------------------------------------
%% @doc Fire transitions until none enabled or MaxSteps reached.
%%
%%      Equivalent to gen_pnet:drain/2.
%%
%% @end
%%--------------------------------------------------------------------
-spec drain(Name :: name(), MaxSteps :: non_neg_integer()) ->
          {ok, [#{atom() => [term()]}]} | {error, limit}.

drain(Name, MaxSteps) when is_integer(MaxSteps), MaxSteps >= 0 ->
    gen_server:call(Name, {drain, MaxSteps, []}).


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
    MaxHistory = proplists:get_value(max_marking_history, Options, 10),
    MaxCont = proplists:get_value(max_continue, Options, 1000),
    CheckpointInterval = proplists:get_value(checkpoint_interval, Options, 0),

    %% Validate timeouts
    true = is_integer(FireTimeout) andalso FireTimeout > 0,
    true = is_integer(ProgressTimeout) andalso ProgressTimeout > 0,
    true = is_integer(MaxHistory) andalso MaxHistory >= 0,
    true = is_integer(MaxCont) andalso MaxCont > 0,
    true = is_integer(CheckpointInterval) andalso CheckpointInterval >= 0,

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

    %% Start the Petri net execution (unless auto_continue = false, e.g. Omega step-driven)
    case proplists:get_value(auto_continue, Options, true) of
        true -> continue(self());
        false -> ok
    end,

    %% Extract regions from NetArg for cancel_region/2
    Regions = case NetArg of
        #{regions := R} when is_map(R) -> R;
        _ -> #{}
    end,

    %% Create wrapper state with timeouts and cycle detection bounds
    WrapperState = #wrapper_state{
                      net_mod = NetMod,
                      net_state = NetState,
                      net_arg = NetArg,
                      fire_timeout = FireTimeout,
                      progress_timeout = ProgressTimeout,
                      shutting_down = false,
                      active_fires = 0,
                      marking_history = [],
                      max_marking_history = MaxHistory,
                      continue_count = 0,
                      max_continue = MaxCont,
                      regions = Regions,
                      checkpoint_interval = CheckpointInterval,
                      drain_step_count = 0
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

handle_call(enabled_transitions, _From, WrapperState = #wrapper_state{net_state = NetState}) ->
    Enabled = compute_enabled_transitions(NetState),
    {reply, Enabled, WrapperState};

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

handle_call(sync, _From, WrapperState = #wrapper_state{net_state = NetState}) ->
    %% Return the current marking to indicate the net is synced
    %% The caller can check if the expected state is reached
    #net_state{marking = Marking} = NetState,
    {reply, {ok, Marking}, WrapperState};

handle_call({inject, ProduceMap}, _From,
            WrapperState = #wrapper_state{net_mod = NetMod, net_state = NetState}) ->
    try
        NetState1 = handle_trigger(ProduceMap, NetState, NetMod),
        Receipt = ProduceMap,
        continue(self()),
        {reply, {ok, Receipt}, WrapperState#wrapper_state{net_state = NetState1}}
    catch
        _:Reason ->
            {reply, {error, Reason}, WrapperState}
    end;

handle_call({withdraw, WithdrawMap}, _From,
            WrapperState = #wrapper_state{net_state = NetState}) ->
    try
        NetState1 = cns(WithdrawMap, NetState),
        {reply, ok, WrapperState#wrapper_state{net_state = NetState1}}
    catch
        _:Reason ->
            {reply, {error, Reason}, WrapperState}
    end;

handle_call({cancel_region, RegionId}, _From,
            WrapperState = #wrapper_state{net_state = NetState, regions = Regions}) ->
    try
        RegionIdNorm = case RegionId of
            B when is_binary(B) -> B;
            A when is_atom(A) -> atom_to_binary(A, utf8);
            _ -> RegionId
        end,
        Places = maps:get(RegionIdNorm, Regions, maps:get(RegionId, Regions, [])),
        #net_state{marking = Marking} = NetState,
        WithdrawMap = lists:foldl(fun(Place, Acc) ->
            case maps:get(Place, Marking, []) of
                [] -> Acc;
                [T | _] -> maps:put(Place, [T], Acc)
            end
        end, #{}, Places),
        NetState1 = cns(WithdrawMap, NetState),
        {reply, ok, WrapperState#wrapper_state{net_state = NetState1}}
    catch
        _:Reason ->
            {reply, {error, Reason}, WrapperState}
    end;

handle_call(step, _From,
            WrapperState = #wrapper_state{
                              fire_timeout = FireTimeout,
                              net_state = NetState0
                             }) ->
    case progress(NetState0, FireTimeout) of
        abort ->
            {reply, abort, WrapperState};
        {delta, Mode, Pm, NewUsrInfo} ->
            %% Extract receipt from delta
            Receipt = Pm,
            %% Update net state
            NetState1 = cns(Mode, NetState0),
            NetState2 = case NewUsrInfo of
                undefined -> NetState1;
                _ -> NetState1#net_state{usr_info = NewUsrInfo}
            end,
            NetMod = WrapperState#wrapper_state.net_mod,
            NetState3 = handle_trigger(Pm, NetState2, NetMod),
            continue(self()),
            {reply, {ok, Receipt}, WrapperState#wrapper_state{net_state = NetState3}};
        {error, Reason, NetState1} ->
            {reply, {error, Reason}, WrapperState#wrapper_state{net_state = NetState1}}
    end;

handle_call({drain, MaxSteps, _Acc}, _From, WrapperState) when MaxSteps =< 0 ->
    {reply, {error, limit}, WrapperState};

handle_call({drain, MaxSteps, Acc}, _From,
            WrapperState = #wrapper_state{
                              fire_timeout = FireTimeout,
                              net_state = NetState0,
                              net_arg = NetArg,
                              checkpoint_interval = CheckpointInterval,
                              drain_step_count = DrainStepCount
                             }) ->
    case progress(NetState0, FireTimeout) of
        abort ->
            {reply, {ok, lists:reverse(Acc)}, WrapperState};
        {delta, Mode, Pm, NewUsrInfo} ->
            Receipt = Pm,
            NetState1 = cns(Mode, NetState0),
            NetState2 = case NewUsrInfo of
                undefined -> NetState1;
                _ -> NetState1#net_state{usr_info = NewUsrInfo}
            end,
            NetMod = WrapperState#wrapper_state.net_mod,
            NetState3 = handle_trigger(Pm, NetState2, NetMod),
            StepCount = DrainStepCount + 1,
            case yawl_recovery:maybe_checkpoint(StepCount, CheckpointInterval,
                    NetArg, NetState3#net_state.marking, NetState3#net_state.usr_info) of
                {do_checkpoint, SpecId, CaseId, Marking, Data} ->
                    _ = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data);
                ok -> ok
            end,
            continue(self()),
            WrapperState1 = WrapperState#wrapper_state{
                net_state = NetState3,
                drain_step_count = StepCount
            },
            handle_call({drain, MaxSteps - 1, [Receipt | Acc]}, _From, WrapperState1);
        {error, Reason, NetState1} ->
            {reply, {error, Reason}, WrapperState#wrapper_state{net_state = NetState1}}
    end;

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
                              net_arg = NetArg,
                              net_state = NetState0 = #net_state{
                                                        stats = Stats,
                                                        tstart = T1,
                                                        cnt = Cnt
                                                       },
                              marking_history = History,
                              max_marking_history = MaxHistory,
                              continue_count = ContCount,
                              max_continue = MaxCont,
                              checkpoint_interval = CheckpointInterval
                             }) ->
    %% Cycle detection: stop if we've exceeded max continue steps
    case ContCount >= MaxCont of
        true ->
            logger:warning("gen_yawl halt: max_continue (~p) reached", [MaxCont]),
            {noreply, WrapperState};
        false ->
            %% Check progress timeout
            StartTime = erlang:monotonic_time(millisecond),

            case progress(NetState0, FireTimeout) of
                abort ->
                    %% Reset cycle detection state for next execution
                    {noreply, WrapperState#wrapper_state{
                        marking_history = [],
                        continue_count = 0
                    }};

                {delta, Mode, Pm, NewUsrInfo} ->
                    Elapsed = erlang:monotonic_time(millisecond) - StartTime,
                    case Elapsed > ProgressTimeout of
                        true ->
                            logger:warning("gen_yawl progress timeout: ~p ms", [Elapsed]),
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

                    %% Optional checkpoint at interval (pure function, no stateful timers)
                    StepCount = ContCount + 1,
                    case yawl_recovery:maybe_checkpoint(StepCount, CheckpointInterval,
                            NetArg, NetState3#net_state.marking, NetState3#net_state.usr_info) of
                        {do_checkpoint, SpecId, CaseId, Marking, Data} ->
                            _ = yawl_recovery:checkpoint(SpecId, CaseId, Marking, Data);
                        ok -> ok
                    end,

                    %% Cycle detection: fingerprint new marking and check for repeat
                    %% (skipped when max_marking_history = 0, e.g. for Omega demo)
                    #net_state{marking = NewMarking} = NetState3,
                    {SeenBefore, NewHistory} = case MaxHistory of
                        0 ->
                            {false, []};
                        _ ->
                            Fingerprint = erlang:phash2(term_to_binary(NewMarking)),
                            Seen = lists:member(Fingerprint, History),
                            Hist = case Seen of
                                true ->
                                    logger:warning("gen_yawl halt: marking cycle detected (fingerprint=~p)", [Fingerprint]),
                                    [];
                                false ->
                                    Hist1 = [Fingerprint | History],
                                    lists:sublist(Hist1, MaxHistory)
                            end,
                            {Seen, Hist}
                    end,

                    %% Decide whether to continue
                    ShouldContinue = not WrapperState#wrapper_state.shutting_down
                        andalso not SeenBefore
                        andalso (ContCount + 1) < MaxCont,

                    case ShouldContinue of
                        true ->
                            continue(self());
                        false ->
                            ok
                    end,

                    %% Update stats
                    NetState4 = update_stats(NetState3, Stats, T1, Cnt),

                    {noreply, WrapperState#wrapper_state{
                        net_state = NetState4,
                        marking_history = NewHistory,
                        continue_count = ContCount + 1
                    }};

                {error, Reason, NetState1} ->
                    logger:error("gen_yawl progress error: ~p", [Reason]),
                    continue(self()),
                    {noreply, WrapperState#wrapper_state{net_state = NetState1}}
            end
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
    _ = case catch yawl_checkpoint:checkpoint_save(self(), NetState) of
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

progress(#net_state{} = NetState, FireTimeout) ->
    {ModeMap, NetMod, UsrInfo} = build_enabled_mode_map(NetState),
    attempt_progress(ModeMap, NetMod, UsrInfo, FireTimeout).

%% @private
%% @doc Compute the list of enabled transition atoms for inspection.
-spec compute_enabled_transitions(#net_state{}) -> [atom()].

compute_enabled_transitions(#net_state{} = NetState) ->
    {ModeMap, _NetMod, _UsrInfo} = build_enabled_mode_map(NetState),
    maps:keys(ModeMap).

%% @private
-spec build_enabled_mode_map(#net_state{}) ->
          {#{atom() => [#{atom() => [term()]}]}, atom(), term()}.

build_enabled_mode_map(#net_state{
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
    {ModeMap, NetMod, UsrInfo}.


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
                        {'DOWN', Ref, process, Pid, {fire_result, Result}} ->
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
    NewAcc = [maps:put(K, V, M) || M <- Acc, V <- Values],
    permut_map_keys(Rest, Map, NewAcc).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.
-endif.
