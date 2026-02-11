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
%% @doc gen_wfnet - Workflow Net Behavior
%%
%% A specialized behavior for building workflow nets based on Petri net
%% theory with BPMN-inspired features. gen_wfnet extends gen_pnet with:
%%
%% - Built-in soundness validation
%% - Automatic start/end place detection
%% - Event emission for workflow lifecycle
%% - Simplified API for workflow patterns
%%
%% <h3>Architecture</h3>
%%
%% gen_wfnet is implemented as a gen_server that wraps gen_pnet semantics.
%% The state includes both the gen_pnet net_state record and workflow-specific
%% extensions for event emission and validation.
%%
%% <h3>Required Callbacks</h3>
%%
%% ```erlang
%% workflow_spec() -> wfnet_types:workflow_spec().
%%   Returns the workflow structure definition.
%%
%% init_marking(Place, UsrInfo) -> [token()].
%%   Returns initial tokens for a place.
%%
%% fire(Trsn, Mode, UsrInfo) -> abort | {produce, ProduceMap}.
%%   Returns tokens produced when transition fires.
%%
%% is_enabled(Trsn, Mode, UsrInfo) -> boolean().
%%   Determines if transition can fire in given mode.
%%
%% init(Args) -> {ok, UsrInfo} | {ok, UsrInfo, Timeout}.
%%   Initializes user info state.
%% '''
%%
%% <h3>Optional Callbacks</h3>
%%
%% ```erlang
%% terminate(Reason, State) -> ok.
%%   Cleanup on termination.
%%
%% handle_call(Request, From, State) -> gen_wfnet:call_return().
%%   Synchronous message handling.
%%
%% handle_cast(Request, State) -> gen_wfnet:cast_return().
%%   Asynchronous message handling.
%%
%% handle_info(Info, State) -> gen_wfnet:cast_return().
%%   Unformatted message handling.
%%
%% code_change(OldVsn, State, Extra) -> {ok, State}.
%%   Hot code reload.
%%
%% on_workflow_event(Event, State) -> ok.
%%   Handle workflow lifecycle events.
%%
%% validate_soundness(Spec) -> wfnet_types:soundness_result().
%%   Custom soundness validation.
%% '''
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% -module(order_workflow).
%% -behaviour(gen_wfnet).
%%
%% workflow_spec() ->
%%     #{
%%         places => [start, validate, process, ship, end],
%%         transitions => [validate_order, process_payment, ship_goods],
%%         start_place => start,
%%         end_place => end,
%%         preset => #{
%%             validate_order => [start],
%%             process_payment => [validate],
%%             ship_goods => [process]
%%         },
%%         postset => #{
%%             validate_order => [validate],
%%             process_payment => [process],
%%             ship_goods => [end]
%%         }
%%     }.
%%
%% init_marking(start, _UsrInfo) -> [init];
%% init_marking(_Place, _UsrInfo) -> [].
%%
%% is_enabled(_Trsn, _Mode, _UsrInfo) -> true.
%%
%% fire(validate_order, _Mode, _UsrInfo) ->
%%     {produce, #{validate => [valid]}};
%% fire(process_payment, _Mode, _UsrInfo) ->
%%     {produce, #{process => [paid]}};
%% fire(ship_goods, _Mode, _UsrInfo) ->
%%     {produce, #{end => [shipped]}}.
%%
%% init(_Args) -> {ok, #{}}.
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_wfnet).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API functions
-export([start_link/3,
         start_link/4,
         ls/2,
         marking/1,
         call/2,
         call/3,
         cast/2,
         stats/1,
         reply/2,
         reset_stats/1,
         stop/1,
         usr_info/1,
         inject/2,
         step/1,
         drain/2,
         validate/1,
         spec_id/1,
         is_sound/1,
         emit_event/2,
         subscribe/2,
         unsubscribe/2]).

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

-include_lib("kernel/include/logger.hrl").
-include_lib("gen_pnet.hrl").
-include("gen_wfnet.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Process name or identifier.
%%--------------------------------------------------------------------
-type name() :: atom() |
                {global, _} |
                {via, atom(), _} |
                pid().

%%--------------------------------------------------------------------
%% @doc Server name specification for gen_server registration.
%%--------------------------------------------------------------------
-type server_name() :: {local, atom()} |
                       {global, atom()} |
                       {via, atom(), _}.

%%--------------------------------------------------------------------
%% @doc Result type for start_link functions.
%%--------------------------------------------------------------------
-type start_link_result() :: {ok, pid()} |
                             ignore |
                             {error, _}.

%%--------------------------------------------------------------------
%% @doc Process configuration options.
%%--------------------------------------------------------------------
-type option() :: {debug, [log | statistics | trace | {_, _}]} |
                {hibernate_after, infinity | non_neg_integer()} |
                {spawn_opt, [link | monitor | {_, _}]} |
                {timeout, infinity | non_neg_integer()}.

%%--------------------------------------------------------------------
%% @doc Extended state for workflow nets.
%%
%% Extends gen_pnet state with workflow-specific fields.
%%--------------------------------------------------------------------
-record(wfnet_state, {
    net_state :: #net_state{},          %% gen_pnet state
    spec :: wfnet_types:workflow_spec(), %% Workflow specification
    spec_id :: binary(),                 %% Unique spec identifier
    subscribers :: [pid()],              %% Event subscribers
    event_history :: [term()],           %% Event history
    soundness_checked :: boolean(),      %% Whether soundness was validated
    validation_result :: wfnet_types:soundness_result() | undefined
}).

%%--------------------------------------------------------------------
%% @doc Workflow event types.
%%--------------------------------------------------------------------
-type wfnet_event() ::
    {workflow_started, #{timestamp => integer()}}
    | {workflow_completed, #{timestamp => integer(), steps => non_neg_integer()}}
    | {transition_fired, #{
        transition => atom(),
        mode => wfnet_types:mode(),
        timestamp => integer()
    }}
    | {token_produced, #{
        place => atom(),
        token => term(),
        timestamp => integer()
    }}
    | {token_consumed, #{
        place => atom(),
        token => term(),
        timestamp => integer()
    }}
    | {validation_completed, #{
        result => wfnet_types:soundness_result(),
        timestamp => integer()
    }}
    | {error, #{
        reason => term(),
        timestamp => integer()
    }}.

%%--------------------------------------------------------------------
%% @doc Return types for handle callbacks.
%%--------------------------------------------------------------------
-type call_return() ::
    {reply, _} |
    {reply, _, #{atom() => [_]}} |
    noreply |
    {noreply, #{atom() => [_]}} |
    {stop, _, _}.

-type cast_return() ::
    noreply |
    {noreply, #{atom() => [_]}} |
    {stop, _}.

%% Export types
-export_type([name/0, server_name/0, start_link_result/0, option/0,
              wfnet_event/0, call_return/0, cast_return/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts an unregistered gen_wfnet instance.
%%
%% Creates a new workflow net process without registering it.
%% The NetArg is passed to the module's init/1 callback.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(NetMod :: atom(), NetArg :: term(), Options :: [option()]) ->
          start_link_result().

start_link(NetMod, NetArg, Options)
  when is_atom(NetMod), is_list(Options) ->
    gen_server:start_link(?MODULE, {NetMod, NetArg}, Options).

%%--------------------------------------------------------------------
%% @doc Starts a registered gen_wfnet instance.
%%
%% Creates a new workflow net process and registers it as ServerName.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(ServerName :: server_name(),
                 NetMod :: atom(),
                 NetArg :: term(),
                 Options :: [option()]) ->
          start_link_result().

start_link(ServerName, NetMod, NetArg, Options)
  when is_tuple(ServerName), is_atom(NetMod), is_list(Options) ->
    gen_server:start_link(ServerName, ?MODULE, {NetMod, NetArg}, Options).

%%--------------------------------------------------------------------
%% @doc Query the list of tokens on a place.
%%
%% Returns `{ok, TokenList}' if the place exists, or `{error, #bad_place{}}'
%% if the place does not exist.
%%
%% @end
%%--------------------------------------------------------------------
-spec ls(Name :: name(), Place :: atom()) -> {ok, [_]} | {error, #bad_place{}}.

ls(Name, Place) when is_atom(Place) ->
    gen_server:call(Name, {ls, Place}).

%%--------------------------------------------------------------------
%% @doc Query the marking map of the workflow net.
%%
%% Returns a map associating each place name with its token list.
%%
%% @end
%%--------------------------------------------------------------------
-spec marking(Name :: name()) -> #{atom() => [_]}.

marking(Name) ->
    gen_server:call(Name, marking).

%%--------------------------------------------------------------------
%% @doc Synchronously send a request to the workflow net.
%%
%% Sends Request to the module's handle_call/3 callback.
%% Timeout is implicitly set to 5 seconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec call(Name :: name(), Request :: term()) -> term().

call(Name, Request) ->
    gen_server:call(Name, {call, Request}).

%%--------------------------------------------------------------------
%% @doc Synchronously send a request with explicit timeout.
%%
%% Same as call/2 but allows specifying the timeout duration.
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
%% @doc Asynchronously send a request to the workflow net.
%%
%% The request is handled by the module's handle_cast/2 callback.
%%
%% @end
%%--------------------------------------------------------------------
-spec cast(Name :: name(), Request :: term()) -> ok.

cast(Name, Request) ->
    gen_server:cast(Name, {cast, Request}).

%%--------------------------------------------------------------------
%% @doc Query the statistics gathered by the workflow net.
%%
%% Returns throughput statistics as a #stats{} record.
%%
%% @end
%%--------------------------------------------------------------------
-spec stats(Name :: name()) -> #stats{}.

stats(Name) ->
    gen_server:call(Name, stats).

%%--------------------------------------------------------------------
%% @doc Clear the statistics for the workflow net.
%%
%% Resets all throughput statistics to undefined.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_stats(Name :: name()) -> ok.

reset_stats(Name) ->
    gen_server:call(Name, reset_stats).

%%--------------------------------------------------------------------
%% @doc Stop the gen_wfnet process.
%%
%% Terminates the workflow net instance gracefully.
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Name :: name()) -> ok.

stop(Name) ->
    gen_server:stop(Name).

%%--------------------------------------------------------------------
%% @doc Query the user info term from the workflow net.
%%
%% Returns the user info field created by init/1.
%%
%% @end
%%--------------------------------------------------------------------
-spec usr_info(Name :: name()) -> _.

usr_info(Name) ->
    gen_server:call(Name, usr_info).

%%--------------------------------------------------------------------
%% @doc Inject tokens into the workflow net.
%%
%% Injects the tokens specified in ProduceMap into the marking.
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
%% from the fired transition, or `{error, no_enabled_transition}' if
%% no transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec step(Name :: name()) ->
          {ok, Receipt :: #{atom() => [_]}} | {error, no_enabled_transition}.

step(Name) ->
    gen_server:call(Name, step).

%%--------------------------------------------------------------------
%% @doc Fire transitions until none enabled or MaxSteps reached.
%%
%% Repeatedly fires enabled transitions until quiescence or MaxSteps.
%% Returns `{ok, Receipts}' with receipts in firing order, or
%% `{error, limit}' if MaxSteps was reached before quiescence.
%%
%% @end
%%--------------------------------------------------------------------
-spec drain(Name :: name(), MaxSteps :: non_neg_integer()) ->
          {ok, [Receipt :: #{atom() => [_]}]} | {error, limit}.

drain(Name, MaxSteps) when is_integer(MaxSteps), MaxSteps >= 0 ->
    gen_server:call(Name, {drain, MaxSteps, []}, infinity).

%%--------------------------------------------------------------------
%% @doc Validate the workflow net specification.
%%
%% Runs comprehensive validation including soundness, liveness,
%% and structural checks.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(Name :: name()) -> wfnet_types:soundness_result().

validate(Name) ->
    gen_server:call(Name, validate).

%%--------------------------------------------------------------------
%% @doc Get the specification ID of the workflow net.
%%
%% Returns the unique identifier for this workflow specification.
%%
%% @end
%%--------------------------------------------------------------------
-spec spec_id(Name :: name()) -> binary().

spec_id(Name) ->
    gen_server:call(Name, spec_id).

%%--------------------------------------------------------------------
%% @doc Check if the workflow net is sound.
%%
%% Returns true if the workflow is sound, false otherwise.
%% Soundness means: (1) option to complete, (2) proper completion,
%% (3) no dead transitions, (4) bounded.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_sound(Name :: name()) -> boolean().

is_sound(Name) ->
    gen_server:call(Name, is_sound).

%%--------------------------------------------------------------------
%% @doc Reply to a deferred call.
%%
%% Use when replying was deferred by returning `{noreply, _, _}'
%% in handle_call/3.
%%
%% @end
%%--------------------------------------------------------------------
-spec reply(Client :: {pid(), gen_server:reply_tag()}, Reply :: term()) -> ok.

reply(Client, Reply) when is_tuple(Client) ->
    gen_server:reply(Client, Reply).

%%--------------------------------------------------------------------
%% @doc Emit a workflow event.
%%
%% Sends an event to all subscribers. Used internally for workflow
%% lifecycle events.
%%
%% @end
%%--------------------------------------------------------------------
-spec emit_event(Name :: name(), Event :: wfnet_event()) -> ok.

emit_event(Name, Event) ->
    gen_server:cast(Name, {emit_event, Event}).

%%--------------------------------------------------------------------
%% @doc Subscribe to workflow events.
%%
%% The calling process will receive workflow events as messages.
%% Filter can be 'all' to receive all events, or a specific filter.
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe(Name :: name(), Filter :: atom() | all) -> ok.

subscribe(Name, Filter) ->
    gen_server:cast(Name, {subscribe, Filter, self()}).

%%--------------------------------------------------------------------
%% @doc Unsubscribe from workflow events.
%%
%% The calling process will stop receiving workflow events.
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe(Name :: name(), Filter :: atom() | all) -> ok.

unsubscribe(Name, Filter) ->
    gen_server:cast(Name, {unsubscribe, Filter, self()}).

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initialize the gen_wfnet instance.
%%
%% Calls the module's init/1 to get UsrInfo, validates the workflow
%% specification, and initializes the marking from place_lst and
%% init_marking/2.
%%--------------------------------------------------------------------
-spec init({NetMod :: atom(), NetArg :: term()}) -> {ok, #wfnet_state{}}.

init({NetMod, NetArg}) ->
    %% Call the module's init callback
    UsrInfo = case erlang:function_exported(NetMod, init, 1) of
        true ->
            case NetMod:init(NetArg) of
                {ok, UI} -> UI;
                {ok, UI, _Timeout} -> UI;
                UI when is_map(UI); is_list(UI) -> UI
            end;
        false ->
            #{}
    end,

    %% Get workflow specification
    Spec = case erlang:function_exported(NetMod, workflow_spec, 0) of
        true ->
            try NetMod:workflow_spec() of
                S when is_map(S) -> S
            catch
                _:_ -> error({missing_callback, workflow_spec, 0})
            end;
        false ->
            error({missing_callback, workflow_spec, 0})
    end,

    %% Validate spec structure
    validate_spec_structure(Spec, NetMod),

    %% Extract spec fields
    Places = maps:get(places, Spec, []),

    %% Initialize marking using gen_pnet convention
    InitMarking = lists:foldl(fun(P, Acc) ->
        Tokens = case erlang:function_exported(NetMod, init_marking, 2) of
            true ->
                try NetMod:init_marking(P, UsrInfo) of
                    T when is_list(T) -> T;
                    _ -> []
                catch
                    _:_ -> []
                end;
            false ->
                []
        end,
        Acc#{P => Tokens}
    end, #{}, lists:usort(Places)),

    %% Initialize stats
    Stat = #stat{
        t = erlang:system_time(millisecond),
        fps = 0.0
    },
    Stats = #stats{
        current = Stat,
        hi = Stat,
        lo = Stat
    },

    %% Create gen_pnet state
    NetState = #net_state{
        marking = InitMarking,
        net_mod = NetMod,
        usr_info = UsrInfo,
        stats = Stats,
        tstart = erlang:monotonic_time(millisecond),
        cnt = 0
    },

    %% Generate spec ID
    SpecId = generate_spec_id(NetMod, Spec),

    %% Create wfnet state
    WFState = #wfnet_state{
        net_state = NetState,
        spec = Spec,
        spec_id = SpecId,
        subscribers = [],
        event_history = [],
        soundness_checked = false,
        validation_result = undefined
    },

    %% Emit workflow started event
    emit_workflow_started(WFState),

    {ok, WFState}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle code change during hot code upgrade.
%%
%% Delegates to the callback module's code_change/3.
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: _, WFState :: #wfnet_state{}, Extra :: _) ->
          {ok, #wfnet_state{}} | {error, _}.

code_change(OldVsn, #wfnet_state{net_state = NetState} = WFState, Extra) ->
    NetMod = NetState#net_state.net_mod,
    case erlang:function_exported(NetMod, code_change, 3) of
        true ->
            case NetMod:code_change(OldVsn, NetState, Extra) of
                {ok, NewNetState} ->
                    {ok, WFState#wfnet_state{net_state = NewNetState}};
                {error, _} = Error ->
                    Error
            end;
        false ->
            {ok, WFState}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle synchronous calls.
%%
%% Handles built-in requests and delegates custom calls to the
%% callback module's handle_call/3.
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(),
                  From :: {pid(), _},
                  WFState :: #wfnet_state{}) ->
          {reply, _, #wfnet_state{}} |
          {noreply, #wfnet_state{}} |
          {stop, _, _, #wfnet_state{}}.

handle_call({ls, Place}, _From,
            #wfnet_state{net_state = #net_state{marking = Marking}} = WFState) ->
    Reply = case maps:is_key(Place, Marking) of
        true -> {ok, maps:get(Place, Marking)};
        false -> {error, #bad_place{name = Place}}
    end,
    {reply, Reply, WFState};

handle_call(marking, _From,
            #wfnet_state{net_state = #net_state{marking = Marking}} = WFState) ->
    {reply, Marking, WFState};

handle_call(usr_info, _From,
            #wfnet_state{net_state = #net_state{usr_info = UsrInfo}} = WFState) ->
    {reply, UsrInfo, WFState};

handle_call({call, Request}, From,
            #wfnet_state{net_state = NetState} = WFState) ->
    NetMod = NetState#net_state.net_mod,
    case erlang:function_exported(NetMod, handle_call, 3) of
        true ->
            case NetMod:handle_call(Request, From, NetState) of
                {reply, Reply} ->
                    {reply, Reply, WFState};
                {reply, Reply, ProdMap} ->
                    WFState1 = handle_produce_map(ProdMap, WFState),
                    {reply, Reply, WFState1};
                noreply ->
                    {noreply, WFState};
                {noreply, ProdMap} ->
                    WFState1 = handle_produce_map(ProdMap, WFState),
                    {noreply, WFState1};
                {stop, Reason, Reply} ->
                    {stop, Reason, Reply, WFState}
            end;
        false ->
            {reply, {error, unknown_request}, WFState}
    end;

handle_call(stats, _From,
            #wfnet_state{net_state = #net_state{stats = Stats}} = WFState) ->
    {reply, Stats, WFState};

handle_call(reset_stats, _From,
            #wfnet_state{net_state = NetState} = WFState) ->
    {reply, ok, WFState#wfnet_state{net_state = NetState#net_state{stats = undefined}}};

handle_call({inject, ProduceMap}, _From, WFState) ->
    try
        WFState1 = handle_produce_map(ProduceMap, WFState),
        Receipt = ProduceMap,
        {reply, {ok, Receipt}, WFState1}
    catch
        _:_:Stack ->
            {reply, {error, Stack}, WFState}
    end;

handle_call(step, _From, #wfnet_state{net_state = NetState} = WFState) ->
    case fire_transition(NetState) of
        abort ->
            {reply, {error, no_enabled_transition}, WFState};
        {ok, Receipt, NewNetState} ->
            NewWFState = WFState#wfnet_state{net_state = NewNetState},
            {reply, {ok, Receipt}, NewWFState}
    end;

handle_call({drain, MaxSteps, _Acc}, _From, WFState) when MaxSteps =< 0 ->
    {reply, {error, limit}, WFState};

handle_call({drain, MaxSteps, Acc}, _From,
            #wfnet_state{net_state = NetState} = WFState) ->
    case fire_transition(NetState) of
        abort ->
            {reply, {ok, lists:reverse(Acc)}, WFState};
        {ok, Receipt, NewNetState} ->
            NewWFState = WFState#wfnet_state{net_state = NewNetState},
            handle_call({drain, MaxSteps - 1, [Receipt | Acc]}, _From, NewWFState)
    end;

handle_call(validate, _From, #wfnet_state{spec = Spec, net_state = #net_state{net_mod = NetMod}} = WFState) ->
    Result = validate_soundness(Spec, NetMod),
    NewWFState = WFState#wfnet_state{
        soundness_checked = true,
        validation_result = Result
    },
    emit_validation_completed(Result, NewWFState),
    {reply, Result, NewWFState};

handle_call(spec_id, _From, #wfnet_state{spec_id = SpecId} = WFState) ->
    {reply, SpecId, WFState};

handle_call(is_sound, _From,
            #wfnet_state{validation_result = undefined,
                         spec = Spec,
                         net_state = #net_state{net_mod = NetMod}} = WFState) ->
    Result = validate_soundness(Spec, NetMod),
    NewWFState = WFState#wfnet_state{
        soundness_checked = true,
        validation_result = Result
    },
    IsSound = case Result of
        {sound, _} -> true;
        {unsound, _} -> false
    end,
    {reply, IsSound, NewWFState};

handle_call(is_sound, _From,
            #wfnet_state{validation_result = Result} = WFState) ->
    IsSound = case Result of
        {sound, _} -> true;
        {unsound, _} -> false
    end,
    {reply, IsSound, WFState};

handle_call(_Request, _From, WFState) ->
    {reply, {error, unknown_request}, WFState}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle asynchronous messages.
%%
%% Handles event emission and delegates custom casts to the
%% callback module's handle_cast/2.
%%--------------------------------------------------------------------
-type cast_request() ::
    {cast, term()} |
    {emit_event, wfnet_event()} |
    {subscribe, atom() | all, pid()} |
    {unsubscribe, atom() | all, pid()}.

-spec handle_cast(cast_request(), WFState :: #wfnet_state{}) ->
          {noreply, #wfnet_state{}} |
          {stop, term(), #wfnet_state{}}.

handle_cast({cast, Request},
            #wfnet_state{net_state = NetState} = WFState) ->
    NetMod = NetState#net_state.net_mod,
    case erlang:function_exported(NetMod, handle_cast, 2) of
        true ->
            case NetMod:handle_cast(Request, NetState) of
                noreply ->
                    {noreply, WFState};
                {noreply, ProdMap} ->
                    WFState1 = handle_produce_map(ProdMap, WFState),
                    {noreply, WFState1};
                {stop, Reason} ->
                    {stop, Reason, WFState}
            end;
        false ->
            {noreply, WFState}
    end;

handle_cast({emit_event, Event}, #wfnet_state{subscribers = Subs,
                                              event_history = History} = WFState) ->
    %% Send event to all subscribers
    lists:foreach(fun(Pid) ->
        catch Pid ! {wfnet_event, Event}
    end, Subs),
    %% Add to history (keep last 100)
    NewHistory = lists:sublist([Event | History], 100),
    {noreply, WFState#wfnet_state{event_history = NewHistory}};

handle_cast({subscribe, _Filter, Pid},
            #wfnet_state{subscribers = Subs} = WFState) ->
    NewSubs = case lists:member(Pid, Subs) of
        true -> Subs;
        false -> [Pid | Subs]
    end,
    {noreply, WFState#wfnet_state{subscribers = NewSubs}};

handle_cast({unsubscribe, _Filter, Pid},
            #wfnet_state{subscribers = Subs} = WFState) ->
    NewSubs = lists:delete(Pid, Subs),
    {noreply, WFState#wfnet_state{subscribers = NewSubs}};

handle_cast(_Msg, WFState) ->
    {noreply, WFState}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle unexpected messages.
%%
%% Delegates to the callback module's handle_info/2.
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), WFState :: #wfnet_state{}) ->
          {noreply, #wfnet_state{}} |
          {stop, term(), #wfnet_state{}}.

handle_info(Info, #wfnet_state{net_state = NetState} = WFState) ->
    NetMod = NetState#net_state.net_mod,
    case erlang:function_exported(NetMod, handle_info, 2) of
        true ->
            case NetMod:handle_info(Info, NetState) of
                noreply ->
                    {noreply, WFState};
                {noreply, ProdMap} ->
                    WFState1 = handle_produce_map(ProdMap, WFState),
                    {noreply, WFState1};
                {stop, Reason} ->
                    {stop, Reason, WFState}
            end;
        false ->
            {noreply, WFState}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle process termination.
%%
%% Delegates to the callback module's terminate/2.
%%--------------------------------------------------------------------
-spec terminate(Reason :: _, WFState :: #wfnet_state{}) -> ok.

terminate(Reason, #wfnet_state{net_state = NetState}) ->
    NetMod = NetState#net_state.net_mod,
    %% Emit workflow termination event
    emit_workflow_terminated(Reason, NetMod),
    %% Call module's terminate if defined
    case erlang:function_exported(NetMod, terminate, 2) of
        true ->
            try
                NetMod:terminate(Reason, NetState)
            catch
                _:_ -> ok
            end;
        false ->
            ok
    end,
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Fire a single transition.
%%
%% Finds an enabled transition and fires it once.
%%--------------------------------------------------------------------
-spec fire_transition(NetState :: #net_state{}) ->
          abort | {ok, Receipt :: #{atom() => [_]}, NetState :: #net_state{}}.

fire_transition(NetState = #net_state{
           marking = Marking,
           net_mod = NetMod,
           usr_info = UsrInfo
          }) ->
    Preset = get_preset_from_spec(NetMod),
    Transitions = get_transitions_from_spec(NetMod),

    case find_enabled_transition(Transitions, Marking, Preset, NetMod, UsrInfo) of
        {ok, Transition, Mode} ->
            case NetMod:fire(Transition, Mode, UsrInfo) of
                {produce, ProdMap} ->
                    %% Consume tokens from mode
                    NewMarking = consume_tokens(Marking, Mode),
                    %% Produce new tokens
                    FinalMarking = produce_tokens(NewMarking, ProdMap),
                    NewNetState = NetState#net_state{marking = FinalMarking},
                    {ok, Mode, NewNetState};
                abort ->
                    {ok, Mode, NetState}
            end;
        {error, no_enabled_transition} ->
            abort
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Find an enabled transition.
%%--------------------------------------------------------------------
-spec find_enabled_transition([atom()], #{atom() => [_]},
                              #{atom() => [atom()]}, atom(), _) ->
          {ok, atom(), #{atom() => [_]}} | {error, no_enabled_transition}.

find_enabled_transition([], _Marking, _Preset, _NetMod, _UsrInfo) ->
    {error, no_enabled_transition};
find_enabled_transition([Transition | Rest], Marking, Preset, NetMod, UsrInfo) ->
    PresetPlaces = maps:get(Transition, Preset, []),
    case has_tokens(PresetPlaces, Marking) of
        false ->
            find_enabled_transition(Rest, Marking, Preset, NetMod, UsrInfo);
        true ->
            Mode = build_mode(PresetPlaces, Marking),
            IsEnabled = case erlang:function_exported(NetMod, is_enabled, 3) of
                true -> NetMod:is_enabled(Transition, Mode, UsrInfo);
                false -> true
            end,
            case IsEnabled of
                true -> {ok, Transition, Mode};
                false -> find_enabled_transition(Rest, Marking, Preset, NetMod, UsrInfo)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Check if all preset places have tokens.
%%--------------------------------------------------------------------
-spec has_tokens([atom()], #{atom() => [_]}) -> boolean().

has_tokens([], _Marking) -> true;
has_tokens([Place | Rest], Marking) ->
    case maps:get(Place, Marking, []) of
        [] -> false;
        _ -> has_tokens(Rest, Marking)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Build mode from preset places and marking.
%%--------------------------------------------------------------------
-spec build_mode([atom()], #{atom() => [_]}) -> #{atom() => [_]}.

build_mode(PresetPlaces, Marking) ->
    lists:foldl(fun(Place, Acc) ->
        Tokens = maps:get(Place, Marking, []),
        case Tokens of
            [] -> Acc;
            [Token | _] -> Acc#{Place => [Token]}
        end
    end, #{}, PresetPlaces).

%%--------------------------------------------------------------------
%% @private
%% @doc Consume tokens from marking based on mode.
%%--------------------------------------------------------------------
-spec consume_tokens(#{atom() => [_]}, #{atom() => [_]}) -> #{atom() => [_]}.

consume_tokens(Marking, Mode) ->
    maps:fold(fun(Place, TokensToConsume, Acc) ->
        Existing = maps:get(Place, Acc, []),
        Acc#{Place => Existing -- TokensToConsume}
    end, Marking, Mode).

%%--------------------------------------------------------------------
%% @private
%% @doc Produce tokens to marking.
%%--------------------------------------------------------------------
-spec produce_tokens(#{atom() => [_]}, #{atom() => [_]}) -> #{atom() => [_]}.

produce_tokens(Marking, ProduceMap) ->
    maps:fold(fun(Place, TokensToProduce, Acc) ->
        Existing = maps:get(Place, Acc, []),
        Acc#{Place => Existing ++ TokensToProduce}
    end, Marking, ProduceMap).

%%--------------------------------------------------------------------
%% @private
%% @doc Handle produce map with trigger callback.
%%--------------------------------------------------------------------
-spec handle_produce_map(#{atom() => [_]}, #wfnet_state{}) -> #wfnet_state{}.

handle_produce_map(ProduceMap, #wfnet_state{net_state = NetState} = WFState) ->
    NetMod = NetState#net_state.net_mod,
    %% Apply trigger filtering if defined
    FilteredMap = case erlang:function_exported(NetMod, trigger, 2) of
        true ->
            maps:fold(fun(Place, Tokens, Acc) ->
                Filtered = lists:filter(fun(Token) ->
                    case NetMod:trigger(Place, Token) of
                        pass -> true;
                        drop -> false
                    end
                end, Tokens),
                case Filtered of
                    [] -> Acc;
                    _ -> Acc#{Place => Filtered}
                end
            end, #{}, ProduceMap);
        false ->
            ProduceMap
    end,
    %% Update marking
    NewMarking = produce_tokens(NetState#net_state.marking, FilteredMap),
    WFState#wfnet_state{net_state = NetState#net_state{marking = NewMarking}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Get preset from workflow spec.
%%--------------------------------------------------------------------
-spec get_preset_from_spec(atom()) -> #{atom() => [atom()]}.

get_preset_from_spec(NetMod) ->
    case erlang:function_exported(NetMod, workflow_spec, 0) of
        true ->
            try
                Spec = NetMod:workflow_spec(),
                maps:get(preset, Spec, #{})
            catch
                _:_ -> #{}
            end;
        false ->
            #{}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Get transitions from workflow spec.
%%--------------------------------------------------------------------
-spec get_transitions_from_spec(atom()) -> [atom()].

get_transitions_from_spec(NetMod) ->
    case erlang:function_exported(NetMod, workflow_spec, 0) of
        true ->
            try
                Spec = NetMod:workflow_spec(),
                maps:get(transitions, Spec, [])
            catch
                _:_ -> []
            end;
        false ->
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate workflow spec structure.
%%--------------------------------------------------------------------
-spec validate_spec_structure(map(), atom()) -> ok.

validate_spec_structure(Spec, NetMod) ->
    Required = [places, transitions, start_place, end_place, preset, postset],
    lists:foreach(fun(Key) ->
        case maps:is_key(Key, Spec) of
            true -> ok;
            false ->
                ?LOG_ERROR(#{
                    message => "Missing required key in workflow_spec",
                    module => NetMod,
                    key => Key
                }),
                error({missing_spec_key, Key})
        end
    end, Required),

    %% Validate types
    Places = maps:get(places, Spec),
    Transitions = maps:get(transitions, Spec),
    StartPlace = maps:get(start_place, Spec),
    EndPlace = maps:get(end_place, Spec),
    Preset = maps:get(preset, Spec),
    Postset = maps:get(postset, Spec),

    true = is_list(Places) orelse error({invalid_type, places, list}),
    true = is_list(Transitions) orelse error({invalid_type, transitions, list}),
    true = is_atom(StartPlace) orelse error({invalid_type, start_place, atom}),
    true = is_atom(EndPlace) orelse error({invalid_type, end_place, atom}),
    true = is_map(Preset) orelse error({invalid_type, preset, map}),
    true = is_map(Postset) orelse error({invalid_type, postset, map}),

    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Validate workflow soundness.
%%
%% Performs structural soundness validation:
%% 1. Check start_place has no incoming transitions
%% 2. Check end_place has no outgoing transitions
%% 3. Check all places are reachable from start
%% 4. Check end place is reachable from all places
%%--------------------------------------------------------------------
-spec validate_soundness(wfnet_types:workflow_spec(), atom()) ->
          wfnet_types:soundness_result().

validate_soundness(Spec, NetMod) ->
    %% Check for custom validation
    case erlang:function_exported(NetMod, validate_soundness, 1) of
        true ->
            NetMod:validate_soundness(Spec);
        false ->
            default_soundness_validation(Spec)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Default soundness validation.
%%--------------------------------------------------------------------
-spec default_soundness_validation(wfnet_types:workflow_spec()) ->
          wfnet_types:soundness_result().

default_soundness_validation(Spec) ->
    Violations = [],

    %% Check start place has no incoming transitions
    StartPlace = maps:get(start_place, Spec),
    EndPlace = maps:get(end_place, Spec),
    Postset = maps:get(postset, Spec),
    Preset = maps:get(preset, Spec),

    %% Find transitions that lead to start_place (should be none)
    IncomingToStart = find_transitions_to_place(StartPlace, Postset),
    Violations1 = case IncomingToStart of
        [] -> Violations;
        _ -> [{deadlock, [StartPlace]} | Violations]
    end,

    %% Find transitions from end_place (should be none)
    OutgoingFromEnd = find_transitions_from_place(EndPlace, Preset),
    Violations2 = case OutgoingFromEnd of
        [] -> Violations1;
        _ -> [{unbounded_place, [EndPlace]} | Violations1]
    end,

    case Violations2 of
        [] -> {sound, bounded};
        _ -> {unsound, lists:reverse(Violations2)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Find transitions that lead to a place.
%%--------------------------------------------------------------------
-spec find_transitions_to_place(atom(), #{atom() => [atom()]}) -> [atom()].

find_transitions_to_place(Place, Postset) ->
    maps:fold(fun(Trsn, OutputPlaces, Acc) ->
        case lists:member(Place, OutputPlaces) of
            true -> [Trsn | Acc];
            false -> Acc
        end
    end, [], Postset).

%%--------------------------------------------------------------------
%% @private
%% @doc Find transitions from a place.
%%--------------------------------------------------------------------
-spec find_transitions_from_place(atom(), #{atom() => [atom()]}) -> [atom()].

find_transitions_from_place(Place, Preset) ->
    maps:fold(fun(Trsn, InputPlaces, Acc) ->
        case lists:member(Place, InputPlaces) of
            true -> [Trsn | Acc];
            false -> Acc
        end
    end, [], Preset).

%%--------------------------------------------------------------------
%% @private
%% @doc Generate unique spec ID.
%%--------------------------------------------------------------------
-spec generate_spec_id(atom(), wfnet_types:workflow_spec()) -> binary().

generate_spec_id(NetMod, Spec) ->
    %% Use module name and a hash of the spec for ID
    SpecHash = erlang:phash2(Spec),
    iolist_to_binary([
        atom_to_binary(NetMod),
        "_",
        integer_to_binary(SpecHash)
    ]).

%%--------------------------------------------------------------------
%% @private
%% @doc Emit workflow started event.
%%--------------------------------------------------------------------
-spec emit_workflow_started(#wfnet_state{}) -> ok.

emit_workflow_started(#wfnet_state{subscribers = Subs, spec_id = SpecId}) ->
    Event = {workflow_started, #{
        spec_id => SpecId,
        timestamp => erlang:system_time(millisecond)
    }},
    lists:foreach(fun(Pid) ->
        catch Pid ! {wfnet_event, Event}
    end, Subs),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Emit workflow terminated event.
%%--------------------------------------------------------------------
-spec emit_workflow_terminated(term(), atom()) -> ok.

emit_workflow_terminated(Reason, NetMod) ->
    %% Log termination
    ?LOG_INFO(#{
        message => "Workflow terminated",
        module => NetMod,
        reason => Reason
    }),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Emit validation completed event.
%%--------------------------------------------------------------------
-spec emit_validation_completed(wfnet_types:soundness_result(), #wfnet_state{}) -> ok.

emit_validation_completed(Result, #wfnet_state{subscribers = Subs}) ->
    Event = {validation_completed, #{
        result => Result,
        timestamp => erlang:system_time(millisecond)
    }},
    lists:foreach(fun(Pid) ->
        catch Pid ! {wfnet_event, Event}
    end, Subs),
    ok.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Basic spec validation test
validate_spec_structure_test() ->
    Spec = #{
        places => [p1, p2],
        transitions => [t1],
        start_place => p1,
        end_place => p2,
        preset => #{t1 => [p1]},
        postset => #{t1 => [p2]}
    },
    ?assertEqual(ok, validate_spec_structure(Spec, test_mod)),

    %% Missing key
    BadSpec = maps:remove(start_place, Spec),
    ?assertError({missing_spec_key, start_place},
                 validate_spec_structure(BadSpec, test_mod)).

%% Soundness validation test
default_soundness_validation_test() ->
    Spec = #{
        places => [start, 'end'],
        transitions => [t1],
        start_place => start,
        end_place => 'end',
        preset => #{t1 => [start]},
        postset => #{t1 => ['end']}
    },
    ?assertEqual({sound, bounded}, default_soundness_validation(Spec)),

    %% Invalid: transition to start_place
    BadSpec1 = Spec#{postset => #{t1 => [start]}},
    Result1 = default_soundness_validation(BadSpec1),
    ?assertMatch({unsound, [{deadlock, [start]}]}, Result1),

    %% Invalid: transition from end_place
    BadSpec2 = Spec#{preset => #{t1 => ['end']}},
    Result2 = default_soundness_validation(BadSpec2),
    ?assertMatch({unsound, [{unbounded_place, ['end']}]}, Result2).

%% Token manipulation tests
consume_tokens_test() ->
    Marking = #{p1 => [a, b, c], p2 => [x, y]},
    Mode = #{p1 => [a], p2 => [x]},
    Result = consume_tokens(Marking, Mode),
    ?assertEqual([b, c], maps:get(p1, Result)),
    ?assertEqual([y], maps:get(p2, Result)).

produce_tokens_test() ->
    Marking = #{p1 => [a], p2 => []},
    ProduceMap = #{p1 => [b], p3 => [c]},
    Result = produce_tokens(Marking, ProduceMap),
    ?assertEqual([a, b], maps:get(p1, Result)),
    ?assertEqual([], maps:get(p2, Result)),
    ?assertEqual([c], maps:get(p3, Result)).

%% Mode building tests
build_mode_test() ->
    Marking = #{p1 => [a], p2 => [x, y]},
    Mode = build_mode([p1, p2], Marking),
    ?assertEqual([a], maps:get(p1, Mode)),
    ?assertEqual([x], maps:get(p2, Mode)).

%% Has tokens tests
has_tokens_test() ->
    Marking = #{p1 => [a], p2 => [], p3 => [x]},
    ?assert(has_tokens([p1], Marking)),
    ?assertNot(has_tokens([p2], Marking)),
    ?assert(has_tokens([p1, p3], Marking)),
    ?assertNot(has_tokens([p2, p1], Marking)).

%% Spec ID generation test
generate_spec_id_test() ->
    Spec = #{places => [p1], transitions => [], start_place => p1,
              end_place => p1, preset => #{}, postset => #{}},
    SpecId = generate_spec_id(test_mod, Spec),
    ?assert(is_binary(SpecId)),
    ?assert(<<>> /= iolist_to_binary(SpecId)).

-endif.
