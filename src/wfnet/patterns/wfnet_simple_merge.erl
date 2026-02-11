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
%% @doc Simple Merge Workflow Pattern (WCP-05)
%%
%% Implements the Simple Merge pattern (XOR-join) where multiple
%% alternative paths converge into a single flow. Exactly one input
%% path must have a token for the merge to proceed.
%%
%% <h3>Pattern Specification</h3>
%%
%% Places:
%%   - input1, input2, ..., inputN: Alternative input paths
%%   - 'end': Output place after merge completes
%%
%% Transitions:
%%   - merge: The XOR-join transition that consumes from one input
%%
%% Flow: (input1 OR input2 OR ... OR inputN) -> merge -> end
%%
%% <h3>XOR-Join Semantics</h3>
%%
%% The merge transition fires when ANY input place has a token:
%% - It consumes one token from exactly one input place
%% - It produces one token to the output place
%% - Only one branch should have completed (XOR semantics)
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create a simple merge for 3 alternative paths
%% {ok, WF} = wfnet_simple_merge:start_link([option_a, option_b, option_c]).
%%
%% %% Create a workflow spec for composition
%% Spec = wfnet_simple_merge:new([path_a, path_b]).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_simple_merge).
-behaviour(gen_wfnet).

%% API exports
-export([
    start_link/1,
    start_link/2,
    new/1,
    new/2,
    get_input_count/1
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
-type input() :: atom() | {atom(), map()}.
-type inputs() :: [input()].
-type input_index() :: pos_integer().
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(simple_merge_state, {
    inputs :: inputs(),
    input_count :: pos_integer(),
    last_input :: undefined | atom()
}).

%% Export types
-export_type([input/0, inputs/0, input_index/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a simple merge workflow process.
%%
%% @param Inputs List of input branch atoms
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(inputs()) -> {ok, pid()} | {error, term()}.
start_link(Inputs) when is_list(Inputs) ->
    gen_wfnet:start_link(?MODULE, Inputs, []).

%%--------------------------------------------------------------------
%% @doc Start a named simple merge workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, inputs()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Inputs) ->
    gen_wfnet:start_link(Name, ?MODULE, Inputs, []).

%%--------------------------------------------------------------------
%% @doc Create a simple merge workflow specification.
%%
%% Returns a workflow spec map that can be used with other
%% composition operators.
%%
%% @param Inputs List of input branch atoms
%% @returns workflow_spec()
%%
%% @end
%%--------------------------------------------------------------------
-spec new(inputs()) -> wfnet_types:workflow_spec().
new(Inputs) when is_list(Inputs) ->
    new(Inputs, #{}).

%%--------------------------------------------------------------------
%% @doc Create a simple merge workflow specification with options.
%%
%% @param Inputs List of input branch atoms
%% @param Options Configuration options
%% @returns workflow_spec()
%%
%% Supported options:
%% - auto_reset: boolean() - If true, resets merge state after each fire
%% - prioritize: [atom()] - Priority order for inputs when multiple have tokens
%%
%% @end
%%--------------------------------------------------------------------
-spec new(inputs(), map()) -> wfnet_types:workflow_spec().
new(Inputs, Options) when is_list(Inputs), is_map(Options) ->
    case Inputs of
        [] -> error(empty_inputs);
        [_] -> error(single_input_use_direct);
        _ -> build_simple_merge_spec(Inputs, Options)
    end.

%%--------------------------------------------------------------------
%% @doc Get the number of inputs in a running simple merge.
%%
%% @param Pid Process pid or registered name
%% @returns {ok, InputCount}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_input_count(gen_wfnet:name()) -> {ok, pos_integer()}.
get_input_count(Name) ->
    case gen_wfnet:usr_info(Name) of
        #simple_merge_state{input_count = Count} -> {ok, Count};
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
-spec init(inputs()) -> {ok, #simple_merge_state{}}.
init(Inputs) ->
    State = #simple_merge_state{
        inputs = normalize_inputs(Inputs),
        input_count = length(Inputs),
        last_input = undefined
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #simple_merge_state{}) -> [term()].
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% The merge transition is enabled when ANY input place has tokens.
%% This implements the XOR-join semantics.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #simple_merge_state{}) -> boolean().
is_enabled(merge, Mode, #simple_merge_state{inputs = Inputs}) ->
    %% Check that at least one input has tokens (XOR-join semantics)
    InputPlaces = [input_place(I) || I <- Inputs],
    lists:any(fun(Place) ->
        case maps:get(Place, Mode, undefined) of
            undefined -> false;
            [] -> false;
            _ -> true
        end
    end, InputPlaces);
is_enabled(_Transition, _Mode, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% When merge fires, it consumes a token from the first input place
%% that has one and produces a token to the end place.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #simple_merge_state{}) ->
    abort | {produce, wfnet_types:produce_map()} | {produce, wfnet_types:produce_map(), #simple_merge_state{}}.
fire(merge, Mode, #simple_merge_state{inputs = Inputs}) ->
    %% Find first input with tokens and consume from it
    case find_input_with_token(Inputs, Mode) of
        {ok, Input} ->
            InputPlace = input_place(Input),
            {produce, #{
                InputPlace => [],
                'end' => [merged]
            }};
        error ->
            abort
    end;
fire(_Transition, _Mode, _State) ->
    abort.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Build simple merge workflow specification.
%%--------------------------------------------------------------------
build_simple_merge_spec(Inputs, Options) ->
    Normalized = normalize_inputs(Inputs),
    N = length(Normalized),

    %% Generate place names
    End = 'end',
    InputPlaces = [input_place(I) || I <- Normalized],

    %% Generate transitions
    MergeTrans = merge,

    %% Build places list
    Places = [End | InputPlaces],

    %% Build preset (merge can consume from any input)
    Preset = #{MergeTrans => InputPlaces},

    %% Build postset (merge produces to end)
    Postset = #{MergeTrans => [End]},

    BaseSpec = #{
        places => Places,
        transitions => [MergeTrans],
        start_place => undefined,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{
            pattern => simple_merge,
            input_count => N,
            merge_type => xor_join
        }
    },

    %% Add options to optional if specified
    case maps:get(auto_reset, Options, undefined) of
        undefined -> BaseSpec;
        AutoReset -> BaseSpec#{
            optional => (maps:get(optional, BaseSpec))#{auto_reset => AutoReset}
        }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize inputs to {Name, Options} tuples.
%%--------------------------------------------------------------------
normalize_inputs(Inputs) ->
    lists:map(fun
        ({Name, Opts}) when is_atom(Name), is_map(Opts) -> {Name, Opts};
        (Name) when is_atom(Name) -> {Name, #{}}
    end, Inputs).

%%--------------------------------------------------------------------
%% @private
%% @doc Generate input place name.
%%--------------------------------------------------------------------
input_place({Input, _Opts}) ->
    list_to_atom(atom_to_list(Input) ++ "_input");
input_place(Input) ->
    list_to_atom(atom_to_list(Input) ++ "_input").

%%--------------------------------------------------------------------
%% @private
%% @doc Find first input with a token.
%%--------------------------------------------------------------------
find_input_with_token(Inputs, Mode) ->
    lists:foldl(fun(Input, _Acc) ->
        Place = input_place(Input),
        case maps:get(Place, Mode, []) of
            [] -> continue;
            _ -> {ok, Input}
        end
    end, error, Inputs).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% new test
new_test() ->
    Spec = new([a, b, c]),
    ?assertMatch(#{places := _, transitions := _}, Spec),
    ?assertEqual(1, length(maps:get(transitions, Spec))),
    ?assertEqual('end', maps:get(end_place, Spec)).

%% error cases test
new_error_test() ->
    ?assertError(empty_inputs, new([])),
    ?assertError(single_input_use_direct, new([single])).

%% normalize_inputs test
normalize_inputs_test() ->
    ?assertEqual([{a, #{}}, {b, #{}}], normalize_inputs([a, b])),
    ?assertEqual([{a, #{opt => 1}}, {b, #{}}], normalize_inputs([{a, #{opt => 1}}, b])).

%% input_place test
input_place_test() ->
    ?assertEqual(a_input, input_place(a)),
    ?assertEqual(branch_input, input_place({branch, #{}})).

%% is_enabled test - should require at least one input
is_enabled_test() ->
    State = #simple_merge_state{inputs = [a, b, c], input_count = 3},
    Mode1 = #{a_input => [token]},
    ?assert(is_enabled(merge, Mode1, State)),
    Mode2 = #{b_input => [token]},
    ?assert(is_enabled(merge, Mode2, State)),
    Mode3 = #{a_input => [], b_input => []},
    ?assertNot(is_enabled(merge, Mode3, State)).

%% fire test
fire_test() ->
    State = #simple_merge_state{inputs = [a, b]},
    Result = fire(merge, #{a_input => [token], b_input => []}, State),
    ?assertMatch({produce, _}, Result),
    {produce, ProduceMap} = Result,
    ?assertEqual([], maps:get(a_input, ProduceMap)),
    ?assertEqual([merged], maps:get('end', ProduceMap)).

%% workflow_spec structure test
workflow_spec_structure_test() ->
    Inputs = [input1, input2],
    Spec = new(Inputs),
    ?assert(is_list(maps:get(places, Spec))),
    ?assert(is_list(maps:get(transitions, Spec))),
    ?assertEqual('end', maps:get(end_place, Spec)),
    ?assertMatch(#{optional := #{pattern := simple_merge}}, Spec).

%% find_input_with_token test
find_input_with_token_test() ->
    Inputs = [a, b, c],
    Mode = #{a_input => [], b_input => [token], c_input => []},
    ?assertEqual({ok, b}, find_input_with_token(Inputs, Mode)).

-endif.
