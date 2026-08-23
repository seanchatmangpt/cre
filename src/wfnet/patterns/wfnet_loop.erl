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
%% @doc Structured Loop Workflow Pattern (WCP-09)
%%
%% Implements the Structured Loop pattern (WCP-09/WHILE) where a body
%% activity repeats while a condition holds true. Supports configurable
%% maximum iterations and exit condition checking.
%%
%% <h3>Pattern Structure</h3>
%%
%% Places:
%%   - start: Entry point to the loop
%%   - body: Loop body activity
%%   - check: Condition checking place
%%   - condition: Holds current condition state
%%   - 'end': Exit from loop
%%
%% Transitions:
%%   - enter_body: Enter the loop body
%%   - check_condition: Evaluate loop condition
%%   - repeat: Loop back for next iteration
%%   - exit: Exit the loop when condition fails
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create a while loop with max 10 iterations
%% {ok, WF} = wfnet_loop:start_link(#{
%%     body_fun => fun(X) -> X + 1 end,
%%     condition_fun => fun(X) -> X < 5 end,
%%     max_iterations => 10
%% }).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_loop).
-behaviour(gen_wfnet).

%% API exports
-export([
    start_link/1,
    start_link/2,
    new/1,
    new/2,
    execute/4
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
-type loop_config() :: #{
    body_fun := function(),
    condition_fun := function(),
    max_iterations := pos_integer() | unlimited,
    initial_state => term()
}.

-type loop_type() :: while | until.

-type loop_state() :: #{
    body_fun := function(),
    condition_fun := function(),
    current_state := term(),
    iteration_count := non_neg_integer(),
    max_iterations := pos_integer() | unlimited,
    loop_type := loop_type()
}.

-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

-export_type([loop_config/0, loop_type/0, loop_state/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a structured loop workflow process.
%%
%% @param Config Loop configuration map
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(loop_config()) -> {ok, pid()} | {error, term()}.
start_link(Config) when is_map(Config) ->
    gen_wfnet:start_link(?MODULE, Config, []).

%%--------------------------------------------------------------------
%% @doc Start a named structured loop workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, loop_config()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Config) ->
    gen_wfnet:start_link(Name, ?MODULE, Config, []).

%%--------------------------------------------------------------------
%% @doc Create a structured loop workflow specification.
%%
%% @param Config Loop configuration
%% @returns workflow_spec()
%%
%% @end
%%--------------------------------------------------------------------
-spec new(loop_config()) -> wfnet_types:workflow_spec().
new(Config) when is_map(Config) ->
    new(Config, #{}).

%%--------------------------------------------------------------------
%% @doc Create a structured loop workflow specification with options.
%%
%% Options:
%% - loop_type: while (default) or until
%%
%% @end
%%--------------------------------------------------------------------
-spec new(loop_config(), map()) -> wfnet_types:workflow_spec().
new(Config, Options) when is_map(Config), is_map(Options) ->
    validate_config(Config),
    MaxIterations = maps:get(max_iterations, Config, 1000),
    LoopType = maps:get(loop_type, Options, while),

    %% Place names
    Start = start,
    End = 'end',
    Body = body,
    Check = check,
    Condition = condition,

    %% Transition names
    EnterBody = enter_body,
    CheckCondition = check_condition,
    Repeat = repeat,
    Exit = exit,

    %% Build places list
    Places = [Start, End, Body, Check, Condition],

    %% Build preset (transition -> input places)
    Preset = #{
        EnterBody => [Start],
        CheckCondition => [Body],
        Repeat => [Check, Condition],
        Exit => [Check, Condition]
    },

    %% Build postset (transition -> output places)
    Postset = #{
        EnterBody => [Body],
        CheckCondition => [Check],
        Repeat => [Start],
        Exit => [End]
    },

    #{
        places => Places,
        transitions => [EnterBody, CheckCondition, Repeat, Exit],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{
            pattern => structured_loop,
            loop_type => LoopType,
            max_iterations => MaxIterations
        }
    }.

%%--------------------------------------------------------------------
%% @doc Execute a loop synchronously and return the result.
%%
%% @param BodyFun Function to execute on each iteration
%% @param LoopType while or until
%% @param ConditionFun Condition checking function
%% @param InitialState Initial state value
%% @returns {ok, FinalState} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute(BodyFun :: function(),
              LoopType :: loop_type(),
              ConditionFun :: function(),
              InitialState :: term()) ->
    {ok, term()} | {error, term()}.

execute(BodyFun, while, ConditionFun, InitialState) ->
    execute_while(BodyFun, ConditionFun, InitialState, 0);
execute(BodyFun, until, ConditionFun, InitialState) ->
    execute_until(BodyFun, ConditionFun, InitialState, 0).

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
    %% This is called during init, actual spec built from state
    #{}.

%%--------------------------------------------------------------------
%% @doc Initialize the workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(loop_config()) -> {ok, loop_state()}.
init(Config) ->
    BodyFun = maps:get(body_fun, Config),
    ConditionFun = maps:get(condition_fun, Config),
    InitialState = maps:get(initial_state, Config, undefined),
    MaxIterations = maps:get(max_iterations, Config, 1000),
    LoopType = maps:get(loop_type, Config, while),

    State = #{
        body_fun => BodyFun,
        condition_fun => ConditionFun,
        current_state => InitialState,
        iteration_count => 0,
        max_iterations => MaxIterations,
        loop_type => LoopType
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), loop_state()) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(condition, _State) ->
    [evaluate];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), loop_state()) -> boolean().
is_enabled(enter_body, _Mode, _State) ->
    true;
is_enabled(check_condition, _Mode, _State) ->
    true;
is_enabled(repeat, _Mode, #{iteration_count := Count, max_iterations := Max}) when Max =:= unlimited orelse Count < Max ->
    true;
is_enabled(repeat, _Mode, _State) ->
    false;
is_enabled(exit, _Mode, _State) ->
    true;
is_enabled(_Transition, _Mode, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), loop_state()) ->
    abort | {produce, wfnet_types:produce_map()}.
fire(enter_body, _Mode, #{current_state := CurrentState} = State) ->
    %% Execute body function
    NewState = execute_body(State, CurrentState),
    {produce, #{
        body => [],
        check => [check]
    }, State#{current_state := NewState, iteration_count := maps:get(iteration_count, State, 0) + 1}};

fire(check_condition, _Mode, #{condition_fun := ConditionFun, current_state := CurrentState, loop_type := LoopType} = State) ->
    %% Evaluate condition
    ConditionMet = evaluate_condition(ConditionFun, CurrentState),
    ShouldContinue = case LoopType of
        while -> ConditionMet;
        until -> not ConditionMet
    end,
    {produce, #{
        check => [],
        condition => [ShouldContinue]
    }, State};

fire(repeat, _Mode, State) ->
    %% Loop back to start
    {produce, #{
        condition => [],
        start => [looping]
    }, State};

fire(exit, _Mode, #{current_state := CurrentState} = State) ->
    %% Exit loop
    {produce, #{
        condition => [],
        'end' => [CurrentState]
    }, State};

fire(_Transition, _Mode, _State) ->
    abort.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Validate loop configuration.
%%--------------------------------------------------------------------
validate_config(Config) ->
    BodyFun = maps:get(body_fun, Config),
    ConditionFun = maps:get(condition_fun, Config),

    true = is_function(BodyFun) orelse error({invalid_config, body_fun_must_be_function}),
    true = is_function(ConditionFun) orelse error({invalid_config, condition_fun_must_be_function}),

    MaxIterations = maps:get(max_iterations, Config, 1000),
    true = MaxIterations =:= unlimited orelse (is_integer(MaxIterations) andalso MaxIterations > 0)
        orelse error({invalid_config, max_iterations_must_be_positive_integer_or_unlimited}),

    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute the body function.
%%--------------------------------------------------------------------
execute_body(#{body_fun := BodyFun}, CurrentState) ->
    try
        BodyFun(CurrentState)
    catch
        _:_ -> CurrentState
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Evaluate the condition function.
%%--------------------------------------------------------------------
evaluate_condition(ConditionFun, CurrentState) ->
    try
        ConditionFun(CurrentState)
    catch
        _:_ -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute while loop synchronously.
%%--------------------------------------------------------------------
execute_while(_BodyFun, _ConditionFun, State, Count) when Count >= 10000 ->
    {ok, State};
execute_while(BodyFun, ConditionFun, State, Count) ->
    case evaluate_condition(ConditionFun, State) of
        true ->
            NewState = try BodyFun(State) catch _:_ -> State end,
            execute_while(BodyFun, ConditionFun, NewState, Count + 1);
        false ->
            {ok, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Execute until loop synchronously.
%%--------------------------------------------------------------------
execute_until(_BodyFun, _ConditionFun, State, Count) when Count >= 10000 ->
    {ok, State};
execute_until(BodyFun, ConditionFun, State, Count) ->
    case evaluate_condition(ConditionFun, State) of
        true ->
            {ok, State};
        false ->
            NewState = try BodyFun(State) catch _:_ -> State end,
            execute_until(BodyFun, ConditionFun, NewState, Count + 1)
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% new/2 test
new_test() ->
    Config = #{
        body_fun => fun(X) -> X + 1 end,
        condition_fun => fun(X) -> X < 5 end
    },
    Spec = new(Config),
    ?assertMatch(#{places := _, transitions := _, preset := _, postset := _}, Spec),
    ?assertEqual([start, 'end', body, check, condition], maps:get(places, Spec)).

%% new with loop_type while test
new_while_test() ->
    Config = #{
        body_fun => fun(X) -> X + 1 end,
        condition_fun => fun(X) -> X < 5 end
    },
    Spec = new(Config, #{loop_type => while}),
    Optional = maps:get(optional, Spec, #{}),
    ?assertEqual(while, maps:get(loop_type, Optional)).

%% new with loop_type until test
new_until_test() ->
    Config = #{
        body_fun => fun(X) -> X + 1 end,
        condition_fun => fun(X) -> X >= 5 end
    },
    Spec = new(Config, #{loop_type => until}),
    Optional = maps:get(optional, Spec, #{}),
    ?assertEqual(until, maps:get(loop_type, Optional)).

%% validate_config test
validate_config_valid_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end,
        max_iterations => 100
    },
    ?assertEqual(ok, validate_config(Config)).

%% validate_config error cases
validate_config_invalid_body_test() ->
    Config = #{
        body_fun => not_a_function,
        condition_fun => fun(X) -> true end
    },
    ?assertError({invalid_config, body_fun_must_be_function}, validate_config(Config)).

validate_config_invalid_condition_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => not_a_function
    },
    ?assertError({invalid_config, condition_fun_must_be_function}, validate_config(Config)).

validate_config_invalid_max_iterations_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end,
        max_iterations => -1
    },
    ?assertError({invalid_config, max_iterations_must_be_positive_integer_or_unlimited}, validate_config(Config)).

%% execute while loop test
execute_while_test() ->
    Body = fun(X) -> X + 1 end,
    Cond = fun(X) -> X < 5 end,
    ?assertEqual({ok, 5}, execute(Body, while, Cond, 0)).

%% execute until loop test
execute_until_test() ->
    Body = fun(X) -> X + 1 end,
    Cond = fun(X) -> X >= 5 end,
    ?assertEqual({ok, 5}, execute(Body, until, Cond, 0)).

%% execute with max iterations test
execute_max_iterations_test() ->
    Body = fun(X) -> X + 1 end,
    Cond = fun(_X) -> true end,
    %% Should stop at 10000 even if condition always true
    {ok, Result} = execute(Body, while, Cond, 0),
    ?assert(Result >= 10000).

%% init test
init_test() ->
    Config = #{
        body_fun => fun(X) -> X + 1 end,
        condition_fun => fun(X) -> X < 5 end,
        initial_state => 0,
        max_iterations => 100
    },
    {ok, State} = init(Config),
    ?assertEqual(0, maps:get(current_state, State)),
    ?assertEqual(0, maps:get(iteration_count, State)),
    ?assertEqual(100, maps:get(max_iterations, State)),
    ?assertEqual(while, maps:get(loop_type, State)).

%% init_marking test
init_marking_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end
    },
    {ok, State} = init(Config),
    ?assertEqual([init], init_marking(start, State)),
    ?assertEqual([evaluate], init_marking(condition, State)),
    ?assertEqual([], init_marking(body, State)),
    ?assertEqual([], init_marking(check, State)),
    ?assertEqual([], init_marking('end', State)).

%% is_enabled test
is_enabled_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end,
        max_iterations => 10
    },
    {ok, State} = init(Config),
    Mode = #{},

    ?assert(is_enabled(enter_body, Mode, State)),
    ?assert(is_enabled(check_condition, Mode, State)),
    ?assert(is_enabled(repeat, Mode, State)),
    ?assert(is_enabled(exit, Mode, State)),

    %% After max iterations, repeat should be disabled
    State2 = State#{iteration_count => 10},
    ?assertNot(is_enabled(repeat, Mode, State2)),

    %% With unlimited max iterations
    State3 = State#{iteration_count => 10000, max_iterations => unlimited},
    ?assert(is_enabled(repeat, Mode, State3)).

%% fire enter_body test
fire_enter_body_test() ->
    Config = #{
        body_fun => fun(X) -> X + 1 end,
        condition_fun => fun(X) -> X < 5 end,
        initial_state => 0
    },
    {ok, State} = init(Config),
    Mode = #{},

    Result = fire(enter_body, Mode, State),
    ?assertMatch({produce, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertEqual([], maps:get(body, ProduceMap)),
    ?assertEqual([check], maps:get(check, ProduceMap)).

%% fire check_condition test
fire_check_condition_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> X < 5 end,
        initial_state => 3
    },
    {ok, State} = init(Config),
    Mode = #{},

    Result = fire(check_condition, Mode, State),
    ?assertMatch({produce, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertEqual([true], maps:get(condition, ProduceMap)).

%% fire repeat test
fire_repeat_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end
    },
    {ok, State} = init(Config),
    Mode = #{},

    Result = fire(repeat, Mode, State),
    ?assertMatch({produce, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertEqual([looping], maps:get(start, ProduceMap)),
    ?assertEqual([], maps:get(condition, ProduceMap)).

%% fire exit test
fire_exit_test() ->
    Config = #{
        body_fun => fun(X) -> X end,
        condition_fun => fun(X) -> true end,
        initial_state => final_value
    },
    {ok, State} = init(Config),
    Mode = #{},

    Result = fire(exit, Mode, State),
    ?assertMatch({produce, _}, Result),
    {produce, ProduceMap, _NewState} = Result,
    ?assertEqual([final_value], maps:get('end', ProduceMap)).

-endif.
