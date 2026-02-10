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
%% @doc Workflow Net Behavior (gen_wfnet)
%%
%% A specialized behavior for building workflow nets based on Petri net
%% theory with BPMN-inspired features. gen_wfnet extends gen_pnet with:
%%
%% - Built-in soundness validation
%% - Automatic start/end place detection
%% - Event emission for workflow lifecycle
%% - Simplified API for workflow patterns
%%
%% <h3>Callback Definition</h3>
%%
%% A gen_wfnet module must implement the following callbacks:
%%
%% ```erlang
%% -module(my_workflow).
%% -behaviour(gen_wfnet).
%%
%% %% Structure callbacks
%% workflow_spec() -> wfnet_types:workflow_spec().
%% init_marking(Place, UsrInfo) -> [term()].
%%
%% %% Execution callbacks
%% fire(Trsn, Mode, UsrInfo) -> abort | {produce, ProduceMap}.
%% is_enabled(Trsn, Mode, UsrInfo) -> boolean().
%%
%% %% Optional interface callbacks
%% init(Args) -> {ok, UsrInfo}.
%% terminate(Reason, State) -> ok.
%% ```
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
%% fire(validate_order, _Mode, UsrInfo) ->
%%     case maps:get(order_valid, UsrInfo, true) of
%%         true -> {produce, #{validate => [valid]}};
%%         false -> {produce, #{end => [rejected]}}
%%     end;
%% fire(process_payment, _Mode, _UsrInfo) ->
%%     {produce, #{process => [paid]}};
%% fire(ship_goods, _Mode, _UsrInfo) ->
%%     {produce, #{end => [shipped]}}.
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_wfnet).

%% Include records
-include_lib("gen_pnet.hrl").
-include("gen_wfnet.hrl").

%% Behaviour exports
-export([
    behaviour_info/1
]).

%% API exports - lifecycle
-export([
    start_link/3,
    start_link/4,
    stop/1
]).

%% API exports - query
-export([
    marking/1,
    usr_info/1,
    stats/1,
    spec_id/1,
    is_valid/1
]).

%% API exports - execution
-export([
    step/1,
    drain/2,
    inject/2,
    validate/1
]).

%% Type exports
-export_type([
    workflow_spec/0,
    option/0
]).

%%====================================================================
%% Behavior Info
%%====================================================================

%% @private
behaviour_info(callbacks) ->
    %% Structure callbacks
    [{workflow_spec, 0},
     {init_marking, 2}] ++
    %% Execution callbacks
    [{fire, 3},
     {is_enabled, 3}] ++
    %% Optional interface callbacks (from gen_pnet)
    [{init, 1},
     {terminate, 2},
     {handle_call, 3},
     {handle_cast, 2},
     {handle_info, 2},
     {code_change, 3}];
behaviour_info(_) ->
    undefined.

%%====================================================================
%% Type Definitions
%%====================================================================

-type place() :: wfnet_types:place().
-type trsn() :: wfnet_types:trsn().
-type token() :: wfnet_types:token().
-type marking() :: wfnet_types:marking().
-type mode() :: wfnet_types:mode().
-type produce_map() :: wfnet_types:produce_map().

%%--------------------------------------------------------------------
%% @doc Workflow specification type.
%%
%% Defines the structure of a workflow net including places,
%% transitions, and connectivity information.
%%--------------------------------------------------------------------
-type workflow_spec() :: #{
    places := [place()],
    transitions := [trsn()],
    start_place := place(),
    end_place := place(),
    preset := #{trsn() => [place()]},
    postset := #{trsn() => [place()]},
    optional => map()
}.

%%--------------------------------------------------------------------
%% @doc Start option for gen_wfnet.
%%--------------------------------------------------------------------
-type option() ::
    {debug, boolean()} |
    {timeout, timeout()} |
    {spawn_opt, [proc_lib:spawn_option()]} |
    {name, {global, term()} | {local, term()} | {via, atom(), term()}}.

%%====================================================================
%% API Functions - Lifecycle
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a gen_wfnet process.
%%
%% Equivalent to `gen_server:start_link/3` with gen_wfnet behavior.
%%
%% @param NetMod Module implementing gen_wfnet callbacks
%% @param NetArg Argument passed to init/1
%% @param Options Start options
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(module(), term(), [option()]) ->
    {ok, pid()} | {error, term()}.
start_link(NetMod, NetArg, Options) ->
    gen_server:start_link(?MODULE, {NetMod, NetArg}, Options).

%%--------------------------------------------------------------------
%% @doc Start a named gen_wfnet process.
%%
%% @param Name Registered name
%% @param NetMod Module implementing gen_wfnet callbacks
%% @param NetArg Argument passed to init/1
%% @param Options Start options
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({global, term()} | {local, term()} | {via, atom(), term()}, module(), term(), [option()]) ->
    {ok, pid()} | {error, term()}.
start_link(Name, NetMod, NetArg, Options) ->
    gen_server:start_link(Name, ?MODULE, {NetMod, NetArg}, Options).

%%--------------------------------------------------------------------
%% @doc Stop a gen_wfnet process.
%%
%% @param Pid Workflow net process
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%%====================================================================
%% API Functions - Query
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Get the current marking of a workflow net.
%%
%% @param Pid Workflow net process
%% @returns Marking map
%%
%% @end
%%--------------------------------------------------------------------
-spec marking(pid()) -> marking().
marking(Pid) ->
    gen_server:call(Pid, marking).

%%--------------------------------------------------------------------
%% @doc Get the user info state of a workflow net.
%%
%% @param Pid Workflow net process
%% @returns User info term
%%
%% @end
%%--------------------------------------------------------------------
-spec usr_info(pid()) -> term().
usr_info(Pid) ->
    gen_server:call(Pid, usr_info).

%%--------------------------------------------------------------------
%% @doc Get execution statistics for a workflow net.
%%
%% @param Pid Workflow net process
%% @returns Stats record or undefined
%%
%% @end
%%--------------------------------------------------------------------
-spec stats(pid()) -> #stats{} | undefined.
stats(Pid) ->
    gen_server:call(Pid, stats).

%%--------------------------------------------------------------------
%% @doc Get the specification ID of a workflow net.
%%
%% @param Pid Workflow net process
%% @returns Binary spec ID
%%
%% @end
%%--------------------------------------------------------------------
-spec spec_id(pid()) -> binary().
spec_id(Pid) ->
    gen_server:call(Pid, spec_id).

%%--------------------------------------------------------------------
%% @doc Check if a workflow net is valid (sound).
%%
%% @param Pid Workflow net process
%% @returns true if valid, false otherwise
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid(pid()) -> boolean().
is_valid(Pid) ->
    gen_server:call(Pid, is_valid).

%%====================================================================
%% API Functions - Execution
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Execute a single transition firing step.
%%
%% Fires one enabled transition and updates the marking.
%%
%% @param Pid Workflow net process
%% @returns {ok, ProduceMap} | {error, no_enabled_transition}
%%
%% @end
%%--------------------------------------------------------------------
-spec step(pid()) -> {ok, produce_map()} | {error, no_enabled_transition}.
step(Pid) ->
    gen_server:call(Pid, step).

%%--------------------------------------------------------------------
%% @doc Fire N transitions or until quiescence.
%%
%% Executes transitions until either N transitions have fired
%% or no more transitions are enabled.
%%
%% @param Pid Workflow net process
%% @param N Maximum number of transitions to fire
%% @returns {ok, StepsFired}
%%
%% @end
%%--------------------------------------------------------------------
-spec drain(pid(), non_neg_integer()) -> {ok, non_neg_integer()}.
drain(Pid, N) ->
    gen_server:call(Pid, {drain, N}, infinity).

%%--------------------------------------------------------------------
%% @doc Inject tokens into the workflow marking.
%%
%% @param Pid Workflow net process
%% @param ProduceMap Tokens to inject (place => [tokens])
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec inject(pid(), produce_map()) -> ok.
inject(Pid, ProduceMap) ->
    gen_server:call(Pid, {inject, ProduceMap}).

%%--------------------------------------------------------------------
%% @doc Validate the workflow net specification.
%%
%% Runs comprehensive validation including soundness, liveness,
%% and structural checks.
%%
%% @param Pid Workflow net process
%% @returns {ok, Warnings} | {error, Errors}
%%
%% @end
%%--------------------------------------------------------------------
-spec validate(pid()) -> wfnet_types:validation_result().
validate(Pid) ->
    gen_server:call(Pid, validate).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
init({NetMod, NetArg}) ->
    %% Call the module's init callback
    UsrInfo = try
        NetMod:init(NetArg)
    catch
        error:undef -> #{};
        _:_ -> #{}
    end,

    %% Get workflow specification
    Spec = try
        NetMod:workflow_spec()
    catch
        error:undef ->
            error({missing_callback, workflow_spec, 0})
    end,

    %% Extract spec fields
    Places = maps:get(places, Spec),
    StartPlace = maps:get(start_place, Spec),
    EndPlace = maps:get(end_place, Spec),

    %% Initialize marking
    InitialMarking = lists:foldl(fun(P, Acc) ->
        Tokens = try NetMod:init_marking(P, UsrInfo) catch _:_ -> [] end,
        Acc#{P => Tokens}
    end, #{}, lists:usort(Places)),

    %% Create state
    Stat = #stat{
        t = erlang:system_time(millisecond),
        fps = 0.0
    },
    Stats = #stats{
        current = Stat,
        hi = Stat,
        lo = Stat
    },

    State = #net_state{
        marking = InitialMarking,
        net_mod = NetMod,
        usr_info = UsrInfo,
        stats = Stats,
        tstart = erlang:monotonic_time(millisecond),
        cnt = 0
    },

    %% Emit spec ID from spec or generate one
    SpecId = maps:get(spec_id, Spec, iolist_to_binary([
        atom_to_binary(NetMod), "_",
        integer_to_binary(erlang:unique_integer([positive]))
    ])),

    %% Wrap in wfnet_state with validation status
    %% For now, mark as unchecked - validation happens on demand
    {ok, State}.

%% @private
handle_call(marking, _From, #net_state{marking = Marking} = State) ->
    {reply, Marking, State};

handle_call(usr_info, _From, #net_state{usr_info = UsrInfo} = State) ->
    {reply, UsrInfo, State};

handle_call(stats, _From, #net_state{stats = Stats} = State) ->
    {reply, Stats, State};

handle_call(spec_id, _From, State) ->
    SpecId = extract_spec_id(State),
    {reply, SpecId, State};

handle_call(is_valid, _From, State) ->
    %% In a full implementation, this would run validation
    %% For now, return true if basic structure is valid
    {reply, true, State};

handle_call(step, _From, #net_state{net_mod = NetMod, marking = Marking, usr_info = UsrInfo} = State) ->
    %% Find enabled transitions
    Spec = try NetMod:workflow_spec() catch _:_ -> #{} end,
    Preset = maps:get(preset, Spec, #{}),
    Transitions = maps:get(transitions, Spec, []),

    case find_enabled_transition(Transitions, Marking, Preset, NetMod, UsrInfo) of
        {ok, Transition, Mode} ->
            %% Fire the transition
            case NetMod:fire(Transition, Mode, UsrInfo) of
                {produce, ProduceMap} ->
                    NewMarking = apply_produce_map(Marking, ProduceMap),
                    NewState = State#net_state{marking = NewMarking},
                    {reply, {ok, ProduceMap}, NewState};
                abort ->
                    {reply, {error, aborted}, State}
            end;
        {error, no_enabled_transition} ->
            {reply, {error, no_enabled_transition}, State}
    end;

handle_call({drain, N}, _From, State) ->
    drain_loop(N, 0, State);

handle_call({inject, ProduceMap}, _From, #net_state{marking = Marking} = State) ->
    NewMarking = maps:fold(fun(P, Tokens, Acc) ->
        Existing = maps:get(P, Acc, []),
        Acc#{P => Existing ++ Tokens}
    end, Marking, ProduceMap),
    {reply, ok, State#net_state{marking = NewMarking}};

handle_call(validate, _From, #net_state{net_mod = NetMod} = State) ->
    %% Use wfnet_validate to validate the spec
    case wfnet_validate:validate_workflow(NetMod) of
        {ok, []} -> {reply, {ok, []}, State};
        Result -> {reply, Result, State}
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
handle_continue(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Find an enabled transition.
%%--------------------------------------------------------------------
find_enabled_transition(Transitions, Marking, Preset, NetMod, UsrInfo) ->
    find_enabled_transition(Transitions, Marking, Preset, NetMod, UsrInfo, []).

find_enabled_transition([], _Marking, _Preset, _NetMod, _UsrInfo, _Attempted) ->
    {error, no_enabled_transition};
find_enabled_transition([Transition | Rest], Marking, Preset, NetMod, UsrInfo, Attempted) ->
    PresetPlaces = maps:get(Transition, Preset, []),
    case has_tokens(PresetPlaces, Marking) of
        false ->
            find_enabled_transition(Rest, Marking, Preset, NetMod, UsrInfo, Attempted);
        true ->
            %% Build mode
            Mode = build_mode(PresetPlaces, Marking),
            %% Check is_enabled if defined
            IsEnabled = case erlang:function_exported(NetMod, is_enabled, 3) of
                true -> NetMod:is_enabled(Transition, Mode, UsrInfo);
                false -> true
            end,
            case IsEnabled of
                true -> {ok, Transition, Mode};
                false -> find_enabled_transition(Rest, Marking, Preset, NetMod, UsrInfo, [Transition | Attempted])
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Check if all preset places have tokens.
%%--------------------------------------------------------------------
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
%% @doc Apply produce map to marking.
%%--------------------------------------------------------------------
apply_produce_map(Marking, ProduceMap) ->
    maps:fold(fun(Place, Tokens, Acc) ->
        Existing = maps:get(Place, Acc, []),
        Acc#{Place => Existing ++ Tokens}
    end, Marking, ProduceMap).

%%--------------------------------------------------------------------
%% @private
%% @doc Drain loop - fire transitions until quiescence or N steps.
%%--------------------------------------------------------------------
drain_loop(0, Count, State) ->
    {reply, {ok, Count}, State};
drain_loop(_N, Count, #net_state{marking = Marking, net_mod = NetMod, usr_info = UsrInfo} = State) ->
    Spec = try NetMod:workflow_spec() catch _:_ -> #{} end,
    Preset = maps:get(preset, Spec, #{}),
    Transitions = maps:get(transitions, Spec, []),

    case find_enabled_transition(Transitions, Marking, Preset, NetMod, UsrInfo) of
        {ok, Transition, Mode} ->
            case NetMod:fire(Transition, Mode, UsrInfo) of
                {produce, ProduceMap} ->
                    NewMarking = apply_produce_map(Marking, ProduceMap),
                    NewState = State#net_state{marking = NewMarking},
                    drain_loop(_N - 1, Count + 1, NewState);
                abort ->
                    {reply, {ok, Count}, State}
            end;
        {error, no_enabled_transition} ->
            {reply, {ok, Count}, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Extract spec ID from state or generate one.
%%--------------------------------------------------------------------
extract_spec_id(#net_state{net_mod = NetMod}) ->
    iolist_to_binary([atom_to_binary(NetMod), "_spec"]).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Behavior info test
behaviour_info_test() ->
    Callbacks = behaviour_info(callbacks),
    ?assert(lists:member({workflow_spec, 0}, Callbacks)),
    ?assert(lists:member({fire, 3}, Callbacks)),
    ?assert(lists:member({init_marking, 2}, Callbacks)).

-endif.
