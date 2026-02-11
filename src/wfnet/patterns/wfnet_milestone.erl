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
%% @doc Milestone Workflow Pattern (WCP-18)
%%
%% Implements the Milestone pattern where execution can only proceed
%% when a specific milestone has been reached. Once a milestone is
%% reached, subsequent executions can bypass the milestone check.
%%
%% <h3>Pattern Specification</h3>
%%
%% Places:
%%   - start: Entry point to the milestone workflow
%%   - pending: Place waiting for milestone to be reached
%%   - milestone: The milestone state (reached/unreached)
%%   - reached: Place indicating milestone has been reached
%%   - 'end': Final place after passing milestone
%%
%% Transitions:
%%   - reach_milestone: Marks the milestone as reached
%%   - pass: Proceeds after milestone is reached
%%   - bypass: Fast path after milestone is reached
%%
%% Flow: start -> check milestone -> (reached?) -> pass -> end
%%                                    -> (not reached) -> pending
%%
%% <h3>Milestone Semantics</h3>
%%
%% - A milestone acts as a gate that must be passed
%% - Once the milestone is reached, it stays reached
%% - Work can proceed through the milestone without re-checking
%% - Milestones can be reset if needed
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create a milestone workflow
%% {ok, WF} = wfnet_milestone:start_link(#{
%%     name => approval_required,
%%     check_fun => fun(State) -> State >= 100 end
%% }).
%%
%% %% Create a workflow spec for composition
%% Spec = wfnet_milestone:new(#{name => data_validated}).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_milestone).
-behaviour(gen_wfnet).

%% API exports
-export([
    start_link/1,
    start_link/2,
    new/1,
    new/2,
    reach_milestone/1,
    is_milestone_reached/1,
    reset_milestone/1,
    get_milestone_name/1
]).

%% gen_wfnet callbacks
-export([
    workflow_spec/0,
    init_marking/2,
    fire/3,
    is_enabled/3,
    init/1
]).

%% Include records
-include_lib("gen_pnet.hrl").
-include_lib("gen_wfnet.hrl").

%% Types
-type milestone_name() :: atom().
-type milestone_config() :: #{
    name := milestone_name(),
    check_fun => function(),            %% Function to check if milestone is met
    reset_on_restart => boolean(),      %% Auto-reset on workflow restart
    persistent => boolean()             %% Persist milestone state
}.
-type milestone_state() :: reached | unreached | pending.
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(milestone_state, {
    name :: milestone_name(),
    check_fun :: undefined | function(),
    current_state :: milestone_state(),
    persistent :: boolean(),
    reached_count = 0 :: non_neg_integer(),
    last_reached_at :: undefined | integer()
}).

%% Export types
-export_type([milestone_name/0, milestone_config/0, milestone_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a milestone workflow process.
%%
%% @param Config Milestone configuration map
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(milestone_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) when is_map(Config) ->
    gen_wfnet:start_link(?MODULE, Config, []).

%%--------------------------------------------------------------------
%% @doc Start a named milestone workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, milestone_config()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Config) ->
    gen_wfnet:start_link(Name, ?MODULE, Config, []).

%%--------------------------------------------------------------------
%% @doc Create a milestone workflow specification.
%%
%% Returns a workflow spec map that can be used with other
%% composition operators.
%%
%% @param Config Milestone configuration
%% @returns workflow_spec()
%%
%% @end
%%--------------------------------------------------------------------
-spec new(milestone_config()) -> wfnet_types:workflow_spec().
new(Config) when is_map(Config) ->
    new(Config, #{}).

%%--------------------------------------------------------------------
%% @doc Create a milestone workflow specification with options.
%%
%% @param Config Milestone configuration
%% @param Options Additional options
%% @returns workflow_spec()
%%
%% Supported options:
%% - auto_pass: boolean() - If true, automatically marks milestone as reached on first check
%% - timeout: timeout() - Maximum time to wait for milestone
%%
%% @end
%%--------------------------------------------------------------------
-spec new(milestone_config(), map()) -> wfnet_types:workflow_spec().
new(Config, Options) when is_map(Config), is_map(Options) ->
    validate_config(Config),
    Name = maps:get(name, Config),
    build_milestone_spec(Name, Config, Options).

%%--------------------------------------------------------------------
%% @doc Manually mark the milestone as reached.
%%
%% This can be called to programmatically reach a milestone.
%%
%% @param Pid Process pid or registered name
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec reach_milestone(gen_wfnet:name()) -> ok | {error, term()}.
reach_milestone(Name) ->
    gen_wfnet:call(Name, reach_milestone).

%%--------------------------------------------------------------------
%% @doc Check if the milestone has been reached.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, boolean()} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec is_milestone_reached(gen_wfnet:name()) -> {ok, boolean()} | {error, term()}.
is_milestone_reached(Name) ->
    case gen_wfnet:usr_info(Name) of
        #milestone_state{current_state = reached} -> {ok, true};
        #milestone_state{} -> {ok, false};
        Other -> {error, {invalid_state, Other}}
    end.

%%--------------------------------------------------------------------
%% @doc Reset the milestone to unreached state.
%%
%% Allows the milestone to be reached again.
%%
%% @param Pid Process pid or registered name
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_milestone(gen_wfnet:name()) -> ok | {error, term()}.
reset_milestone(Name) ->
    gen_wfnet:call(Name, reset_milestone).

%%--------------------------------------------------------------------
%% @doc Get the name of the milestone.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, MilestoneName} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_milestone_name(gen_wfnet:name()) -> {ok, milestone_name()} | {error, term()}.
get_milestone_name(Name) ->
    case gen_wfnet:usr_info(Name) of
        #milestone_state{name = MilestoneName} -> {ok, MilestoneName};
        Other -> {error, {invalid_state, Other}}
    end.

%%====================================================================
%% gen_wfnet Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Return the workflow specification.
%%
%% @end
%%--------------------------------------------------------------------
-spec workflow_spec() -> wfnet_types:workflow_spec().
workflow_spec() ->
    %% Placeholder - actual spec built from state during init
    #{}.

%%--------------------------------------------------------------------
%% @doc Initialize the workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(milestone_config()) -> {ok, #milestone_state{}}.
init(Config) ->
    Name = maps:get(name, Config),
    CheckFun = maps:get(check_fun, Config, fun(_) -> true end),
    Persistent = maps:get(persistent, Config, false),
    State = #milestone_state{
        name = Name,
        check_fun = CheckFun,
        current_state = unreached,
        persistent = Persistent,
        reached_count = 0,
        last_reached_at = undefined
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #milestone_state{}) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #milestone_state{}) -> boolean().
is_enabled(check, #{start := [init]}, #milestone_state{}) ->
    true;
is_enabled(reach_milestone, #{pending := [check]}, #milestone_state{current_state = unreached}) ->
    true;
is_enabled(pass, _Mode, #milestone_state{current_state = reached}) ->
    true;
is_enabled(pass, #{pending := [check]}, #milestone_state{current_state = unreached, check_fun = CheckFun}) ->
    %% Check if milestone condition is met
    case CheckFun of
        undefined -> false;
        Fun -> try Fun(check) of
            true -> true;
            _ -> false
        catch
            _:_ -> false
        end
    end;
is_enabled(bypass, #{reached := [_]}, #milestone_state{}) ->
    true;
is_enabled(_Transition, _Mode, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #milestone_state{}) ->
    abort | {produce, wfnet_types:produce_map()} | {produce, wfnet_types:produce_map(), #milestone_state{}}.
fire(check, _Mode, #milestone_state{current_state = reached} = State) ->
    %% Already reached - use bypass path
    {produce, #{
        start => [],
        reached => [bypass]
    }, State};

fire(check, _Mode, #milestone_state{current_state = unreached} = State) ->
    %% Not reached - go to pending
    {produce, #{
        start => [],
        pending => [check]
    }, State};

fire(reach_milestone, _Mode, #milestone_state{} = State) ->
    %% Mark milestone as reached
    Timestamp = erlang:system_time(millisecond),
    NewState = State#milestone_state{
        current_state = reached,
        reached_count = State#milestone_state.reached_count + 1,
        last_reached_at = Timestamp
    },
    {produce, #{
        pending => [],
        reached => [reached]
    }, NewState};

fire(pass, _Mode, #milestone_state{current_state = reached} = State) ->
    %% Pass through after milestone reached
    {produce, #{
        reached => [],
        'end' => [{milestone_passed, State#milestone_state.name}]
    }, State};

fire(bypass, _Mode, #milestone_state{} = State) ->
    %% Fast bypass for already reached milestone
    {produce, #{
        reached => [],
        'end' => [{milestone_bypassed, State#milestone_state.name}]
    }, State};

fire(_Transition, _Mode, _State) ->
    abort.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Validate milestone configuration.
%%--------------------------------------------------------------------
validate_config(Config) ->
    case maps:is_key(name, Config) of
        false -> error({missing_config, name});
        true ->
            Name = maps:get(name, Config),
            true = is_atom(Name) orelse error({invalid_name, Name}),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Build milestone workflow specification.
%%--------------------------------------------------------------------
build_milestone_spec(Name, _Config, Options) ->
    %% Generate place names
    Start = start,
    End = 'end',
    PendingPlace = pending,
    ReachedPlace = reached,

    %% Generate transitions
    CheckTrans = check,
    ReachTrans = reach_milestone,
    PassTrans = pass,
    BypassTrans = bypass,

    %% Build places list
    Places = [Start, End, PendingPlace, ReachedPlace],

    %% Build preset (transition -> input places)
    Preset = #{
        CheckTrans => [Start],
        ReachTrans => [PendingPlace],
        PassTrans => [PendingPlace, ReachedPlace],
        BypassTrans => [ReachedPlace]
    },

    %% Build postset (transition -> output places)
    Postset = #{
        CheckTrans => [PendingPlace, ReachedPlace],
        ReachTrans => [ReachedPlace],
        PassTrans => [End],
        BypassTrans => [End]
    },

    %% Get options
    AutoPass = maps:get(auto_pass, Options, false),
    Timeout = maps:get(timeout, Options, infinity),

    #{
        places => Places,
        transitions => [CheckTrans, ReachTrans, PassTrans, BypassTrans],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{
            pattern => milestone,
            milestone_name => Name,
            auto_pass => AutoPass,
            timeout => Timeout
        }
    }.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% new test
new_test() ->
    Config = #{name => test_milestone},
    Spec = new(Config),
    ?assertMatch(#{places := _, transitions := _}, Spec),
    ?assertEqual(4, length(maps:get(transitions, Spec))),
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual('end', maps:get(end_place, Spec)).

%% validate_config test
validate_config_valid_test() ->
    Config = #{name => valid_milestone},
    ?assertEqual(ok, validate_config(Config)).

validate_config_missing_name_test() ->
    Config = #{},
    ?assertError({missing_config, name}, validate_config(Config)).

validate_config_invalid_name_test() ->
    Config = #{name => "not_an_atom"},
    ?assertError({invalid_name, _}, validate_config(Config)).

%% init test
init_test() ->
    Config = #{name => test_milestone, check_fun => fun(_) -> true end},
    {ok, State} = init(Config),
    ?assertEqual(test_milestone, State#milestone_state.name),
    ?assertEqual(unreached, State#milestone_state.current_state),
    ?assertEqual(0, State#milestone_state.reached_count),
    ?assertEqual(false, State#milestone_state.persistent).

%% is_enabled test
is_enabled_check_test() ->
    State = #milestone_state{current_state = unreached},
    Mode = #{start => [init]},
    ?assert(is_enabled(check, Mode, State)).

is_enabled_reach_milestone_test() ->
    State = #milestone_state{current_state = unreached},
    Mode = #{pending => [check]},
    ?assert(is_enabled(reach_milestone, Mode, State)).

is_enabled_pass_test() ->
    State = #milestone_state{current_state = reached},
    ?assert(is_enabled(pass, #{}, State)),

    State2 = State#milestone_state{current_state = unreached, check_fun = fun(_) -> true end},
    Mode2 = #{pending => [check]},
    ?assert(is_enabled(pass, Mode2, State2)).

is_enabled_bypass_test() ->
    State = #milestone_state{},
    Mode = #{reached => [bypass]},
    ?assert(is_enabled(bypass, Mode, State)).

%% fire check test - not reached
fire_check_unreached_test() ->
    State = #milestone_state{current_state = unreached},
    Result = fire(check, #{start => [init]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertEqual([], maps:get(start, ProduceMap)),
    ?assertEqual([check], maps:get(pending, ProduceMap)).

%% fire check test - already reached
fire_check_reached_test() ->
    State = #milestone_state{current_state = reached},
    Result = fire(check, #{start => [init]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertEqual([bypass], maps:get(reached, ProduceMap)).

%% fire reach_milestone test
fire_reach_milestone_test() ->
    State = #milestone_state{current_state = unreached, reached_count = 0},
    Result = fire(reach_milestone, #{pending => [check]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, NewState} = Result,
    ?assertEqual(reached, NewState#milestone_state.current_state),
    ?assertEqual(1, NewState#milestone_state.reached_count),
    ?assertEqual([reached], maps:get(reached, ProduceMap)).

%% fire pass test
fire_pass_test() ->
    State = #milestone_state{current_state = reached, name = test_ms},
    Result = fire(pass, #{}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertMatch([{milestone_passed, test_ms}], maps:get('end', ProduceMap)).

%% fire bypass test
fire_bypass_test() ->
    State = #milestone_state{name = test_ms},
    Result = fire(bypass, #{reached => [bypass]}, State),
    ?assertMatch({produce, _, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertMatch([{milestone_bypassed, test_ms}], maps:get('end', ProduceMap)).

%% workflow spec structure test
workflow_spec_structure_test() ->
    Config = #{name => my_milestone},
    Spec = new(Config),
    ?assert(is_list(maps:get(places, Spec))),
    ?assert(is_list(maps:get(transitions, Spec))),
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual('end', maps:get(end_place, Spec)),
    ?assertMatch(#{optional := #{pattern := milestone}}, Spec),
    ?assertEqual(my_milestone, maps:get(milestone_name, maps:get(optional, Spec))).

%% new with auto_pass test
new_with_auto_pass_test() ->
    Config = #{name => test_ms},
    Spec = new(Config, #{auto_pass => true}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(true, maps:get(auto_pass, Optional)).

%% new with timeout test
new_with_timeout_test() ->
    Config = #{name => test_ms},
    Spec = new(Config, #{timeout => 5000}),
    Optional = maps:get(optional, Spec),
    ?assertEqual(5000, maps:get(timeout, Optional)).

-endif.
