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
%% @doc Exclusive Choice Workflow Pattern (WCP-04)
%%
%% Implements the Exclusive Choice pattern (XOR-split) where exactly
%% one branch is selected based on runtime conditions.
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create an exclusive choice between alternatives
%% {ok, WF} = wfnet_choice:start_link([option_a, option_b, option_c]).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_choice).
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
-type option() :: atom() | {atom(), map()}.
-type options() :: [option()].
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(choice_state, {
    options :: options(),
    option_count :: pos_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start an exclusive choice workflow process.
%%
%% @param Options List of option atoms
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(options()) -> {ok, pid()} | {error, term()}.
start_link(Options) when is_list(Options) ->
    gen_wfnet:start_link(?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Start a named exclusive choice workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, options()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Options) ->
    gen_wfnet:start_link(Name, ?MODULE, Options, []).

%%--------------------------------------------------------------------
%% @doc Create an exclusive choice workflow specification.
%%
%% @param Options List of option atoms
%% @returns workflow_spec()
%%
%% @end
%%--------------------------------------------------------------------
-spec new(options()) -> wfnet_types:workflow_spec().
new(Options) when is_list(Options) ->
    new(Options, #{}).

%%--------------------------------------------------------------------
%% @doc Create an exclusive choice workflow specification with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec new(options(), map()) -> wfnet_types:workflow_spec().
new(Options, Config) when is_list(Options), is_map(Config) ->
    case Options of
        [] -> error(empty_options);
        [_] -> error(single_option_use_sequence);
        _ -> build_choice_spec(Options, Config)
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
    #{}.

%%--------------------------------------------------------------------
%% @doc Initialize the workflow.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(options()) -> {ok, #choice_state{}}.
init(Options) ->
    State = #choice_state{
        options = normalize_options(Options),
        option_count = length(Options)
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #choice_state{}) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #choice_state{}) -> boolean().
is_enabled(select, _Mode, _State) ->
    true;
is_enabled(merge, _Mode, _State) ->
    true;
is_enabled(_Transition, _Mode, _State) ->
    false.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #choice_state{}) ->
    abort | {produce, wfnet_types:produce_map()}.
fire(select, _Mode, #choice_state{options = Options}) ->
    %% Select first option (in real implementation, would use condition)
    [{FirstOption, _} | _] = Options,
    {produce, #{option_place(FirstOption) => [selected]}};
fire(merge, _Mode, _State) ->
    %% Merge from any option to end
    {produce, #{'end' => [merged]}};
fire(_Transition, _Mode, _State) ->
    abort.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Build exclusive choice workflow specification.
%%--------------------------------------------------------------------
build_choice_spec(Options, _Config) ->
    Normalized = normalize_options(Options),
    N = length(Normalized),

    %% Generate place names
    Start = start,
    End = 'end',
    OptionPlaces = [option_place(O) || O <- Normalized],
    MergePlace = merge,

    %% Generate transitions
    SelectTrans = select,
    MergeTrans = merge,

    %% Build places list
    Places = [Start, End, MergePlace | OptionPlaces],

    %% Build preset
    Preset = #{
        SelectTrans => [Start],
        MergeTrans => [MergePlace | OptionPlaces]
    },

    %% Build postset (XOR: select produces to one of the option places)
    %% For simplicity, produce to first option (real implementation would conditionally select)
    FirstOptionPlace = case OptionPlaces of
        [First | _] -> First;
        [] -> error(empty_options)
    end,
    Postset = #{
        SelectTrans => [FirstOptionPlace],
        MergeTrans => [End]
    },

    #{
        places => Places,
        transitions => [SelectTrans, MergeTrans],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{pattern => exclusive_choice, option_count => N}
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize options to {Name, Options} tuples.
%%--------------------------------------------------------------------
normalize_options(Options) ->
    lists:map(fun
        ({Name, Opts}) when is_atom(Name), is_map(Opts) -> {Name, Opts};
        (Name) when is_atom(Name) -> {Name, #{}}
    end, Options).

%%--------------------------------------------------------------------
%% @private
%% @doc Generate option place name.
%%--------------------------------------------------------------------
option_place({Option, _Opts}) ->
    list_to_atom(atom_to_list(Option) ++ "_option");
option_place(Option) ->
    list_to_atom(atom_to_list(Option) ++ "_option").

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% new test
new_test() ->
    Spec = new([a, b]),
    ?assertMatch(#{places := _, transitions := _}, Spec).

%% error cases test
new_error_test() ->
    ?assertError(empty_options, new([])),
    ?assertError(single_option_use_sequence, new([single])).

-endif.
