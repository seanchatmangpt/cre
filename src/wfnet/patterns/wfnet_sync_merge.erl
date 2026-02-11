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
%% @doc Synchronous Merge Workflow Pattern (WCP-06)
%%
%% Implements the Synchronous Merge pattern (AND-join) where multiple
%% parallel paths must all complete before proceeding.
%%
%% This pattern creates a workflow that:
%% - Accepts tokens from multiple input places
%% - Waits until ALL inputs have arrived
%% - Produces a single output token when complete
%%
%% <h3>Pattern Specification</h3>
%%
%% Places:
%% - start: Initial place (optional, for workflows that include split)
%% - input1, input2, ..., inputN: Input branch places
%% - wait: Waiting place for synchronization
%% - end: Final place after merge completes
%%
%% Transitions:
%% - merge: The AND-join transition that waits for all inputs
%%
%% Flow: (input1 AND input2 AND ... AND inputN) -> wait -> merge -> end
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create a synchronous merge for 3 parallel branches
%% {ok, WF} = wfnet_sync_merge:start_link([branch_a, branch_b, branch_c]).
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_sync_merge).
-behaviour(gen_wfnet).

%% API exports
-export([
    start_link/1,
    start_link/2,
    new/1,
    new/2
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
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(sync_merge_state, {
    inputs :: inputs(),
    input_count :: pos_integer(),
    pending = [] :: [atom()],
    completed = [] :: [atom()]
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a synchronous merge workflow process.
%%
%% @param Inputs List of input branch atoms to synchronize
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(inputs()) -> {ok, pid()} | {error, term()}.
start_link(Inputs) when is_list(Inputs) ->
    gen_wfnet:start_link(?MODULE, Inputs, []).

%%--------------------------------------------------------------------
%% @doc Start a named synchronous merge workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, inputs()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Inputs) ->
    gen_wfnet:start_link(Name, ?MODULE, Inputs, []).

%%--------------------------------------------------------------------
%% @doc Create a synchronous merge workflow specification.
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
%% @doc Create a synchronous merge workflow specification with options.
%%
%% @param Inputs List of input branch atoms
%% @param Options Configuration options
%% @returns workflow_spec()
%%
%% Supported options:
%% - auto_start: boolean() - If true, creates start place and transition
%% - timeout: pos_integer() - Maximum time to wait for inputs (ms)
%%
%% @end
%%--------------------------------------------------------------------
-spec new(inputs(), map()) -> wfnet_types:workflow_spec().
new(Inputs, Options) when is_list(Inputs), is_map(Options) ->
    case Inputs of
        [] -> error(empty_inputs);
        [_] -> error(single_input_use_sequence);
        _ -> build_sync_merge_spec(Inputs, Options)
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
    %% This is called during init, actual spec built from state
    #{}.

%%--------------------------------------------------------------------
%% @doc Initialize the workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(inputs()) -> {ok, #sync_merge_state{}}.
init(Inputs) ->
    State = #sync_merge_state{
        inputs = normalize_inputs(Inputs),
        input_count = length(Inputs),
        pending = normalize_inputs(Inputs),
        completed = []
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #sync_merge_state{}) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% The merge transition is enabled when ALL input places have tokens.
%% This implements the AND-join semantics.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #sync_merge_state{}) -> boolean().
is_enabled(merge, Mode, #sync_merge_state{inputs = Inputs}) ->
    %% Check that all inputs have tokens (AND-join semantics)
    InputPlaces = [input_place(I) || I <- Inputs],
    lists:all(fun(Place) ->
        maps:is_key(Place, Mode) andalso
        case maps:get(Place, Mode) of
            [] -> false;
            _ -> true
        end
    end, InputPlaces);
is_enabled(_Transition, _Mode, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% When merge fires, it consumes tokens from all input places and
%% produces a token to the end place.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #sync_merge_state{}) ->
    abort | {produce, wfnet_types:produce_map()}.
fire(merge, _Mode, #sync_merge_state{inputs = Inputs}) ->
    %% Collect data from all inputs and produce merged output
    %% For now, produce a simple completion token
    {produce, #{'end' => [all_completed]}};
fire(_Transition, _Mode, _State) ->
    abort.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Build synchronous merge workflow specification.
%%--------------------------------------------------------------------
build_sync_merge_spec(Inputs, Options) ->
    Normalized = normalize_inputs(Inputs),
    N = length(Normalized),

    %% Generate place names
    Start = case maps:get(auto_start, Options, false) of
        true -> start;
        false -> undefined
    end,
    End = 'end',
    InputPlaces = [input_place(I) || I <- Normalized],

    %% Generate transitions
    MergeTrans = merge,

    %% Build places list
    Places = case Start of
        undefined -> [End | InputPlaces];
        _ -> [Start, End | InputPlaces]
    end,

    %% Build preset (merge consumes from all input places)
    Preset = #{MergeTrans => InputPlaces},

    %% Build postset (merge produces to end)
    Postset = #{MergeTrans => [End]},

    %% Add optional start transition if auto_start is enabled
    {Places2, Transitions, Preset2, Postset2} = case Start of
        undefined ->
            {Places, [MergeTrans], Preset, Postset};
        _ ->
            SplitTrans = split,
            Preset3 = Preset#{SplitTrans => [Start]},
            Postset3 = Postset#{SplitTrans => InputPlaces},
            {[Start | Places], [SplitTrans, MergeTrans], Preset3, Postset3}
    end,

    BaseSpec = #{
        places => Places2,
        transitions => Transitions,
        start_place => Start,
        end_place => End,
        preset => Preset2,
        postset => Postset2,
        optional => #{
            pattern => sync_merge,
            input_count => N,
            merge_type => and_join
        }
    },

    %% Add timeout to optional if specified
    case maps:get(timeout, Options, undefined) of
        undefined -> BaseSpec;
        Timeout -> BaseSpec#{
            optional => (maps:get(optional, BaseSpec))#{timeout => Timeout}
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

%% new with auto_start test
new_with_auto_start_test() ->
    Spec = new([a, b], #{auto_start => true}),
    ?assertMatch(#{places := _, transitions := _}, Spec),
    ?assertEqual(start, maps:get(start_place, Spec)),
    ?assertEqual(2, length(maps:get(transitions, Spec))).

%% error cases test
new_error_test() ->
    ?assertError(empty_inputs, new([])),
    ?assertError(single_input_use_sequence, new([single])).

%% normalize_inputs test
normalize_inputs_test() ->
    ?assertEqual([{a, #{}}, {b, #{}}], normalize_inputs([a, b])),
    ?assertEqual([{a, #{opt => 1}}, {b, #{}}], normalize_inputs([{a, #{opt => 1}}, b])).

%% input_place test
input_place_test() ->
    ?assertEqual(a_input, input_place(a)),
    ?assertEqual(branch_input, input_place({branch, #{}})).

%% is_enabled test - should require all inputs
is_enabled_test() ->
    State = #sync_merge_state{inputs = [a, b, c], input_count = 3},
    Mode1 = #{a_input => [token], b_input => [token], c_input => [token]},
    ?assert(is_enabled(merge, Mode1, State)),
    Mode2 = #{a_input => [token], b_input => [token]},
    ?assertNot(is_enabled(merge, Mode2, State)),
    Mode3 = #{a_input => [], b_input => [token], c_input => [token]},
    ?assertNot(is_enabled(merge, Mode3, State)).

%% fire test
fire_test() ->
    State = #sync_merge_state{inputs = [a, b]},
    Result = fire(merge, #{a_input => [token], b_input => [token]}, State),
    ?assertMatch({produce, #{}}, Result),
    {produce, ProdMap} = Result,
    ?assertEqual([all_completed], maps:get('end', ProdMap)).

%% workflow_spec structure test
workflow_spec_structure_test() ->
    Inputs = [input1, input2, input3],
    Spec = new(Inputs),
    ?assert(is_list(maps:get(places, Spec))),
    ?assert(is_list(maps:get(transitions, Spec))),
    ?assertEqual('end', maps:get(end_place, Spec)),
    ?assertMatch(#{optional := #{pattern := sync_merge}}, Spec).

-endif.
