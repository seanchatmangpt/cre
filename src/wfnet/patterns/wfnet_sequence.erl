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
%% @doc Sequence Workflow Pattern
%%
%% Implements the Sequence pattern (WCP-01) where activities execute
%% one after another in order.
%%
%% This pattern creates a workflow with N sequential activities.
%%
%% <h3>Example</h3>
%%
%% ```erlang
%% %% Create a 3-step sequence workflow
%% {ok, WF} = wfnet_sequence:start_link([task_a, task_b, task_c]).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_sequence).
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
-type activity() :: atom() | {atom(), map()}.
-type activities() :: [activity()].
-type registrar() :: {global, term()} | {local, term()} | {via, atom(), term()}.

%% State record
-record(sequence_state, {
    activities :: activities(),
    current_position :: pos_integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Start a sequence workflow process.
%%
%% @param Activities List of activity atoms to execute sequentially
%% @returns {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(activities()) -> {ok, pid()} | {error, term()}.
start_link(Activities) when is_list(Activities) ->
    gen_wfnet:start_link(?MODULE, Activities, []).

%%--------------------------------------------------------------------
%% @doc Start a named sequence workflow process.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link({registrar(), term()}, activities()) ->
    {ok, pid()} | {error, term()}.
start_link(Name, Activities) ->
    gen_wfnet:start_link(Name, ?MODULE, Activities, []).

%%--------------------------------------------------------------------
%% @doc Create a sequence workflow specification.
%%
%% Returns a workflow spec map that can be used with other
%% composition operators.
%%
%% @param Activities List of activity atoms
%% @returns workflow_spec()
%%
%% @end
%%--------------------------------------------------------------------
-spec new(activities()) -> wfnet_types:workflow_spec().
new(Activities) when is_list(Activities) ->
    new(Activities, #{}).

%%--------------------------------------------------------------------
%% @doc Create a sequence workflow specification with options.
%%
%% @param Activities List of activity atoms
%% @param Options Configuration options
%% @returns workflow_spec()
%%
%% @end
%%--------------------------------------------------------------------
-spec new(activities(), map()) -> wfnet_types:workflow_spec().
new(Activities, Options) when is_list(Activities), is_map(Options) ->
    case Activities of
        [] -> error(empty_activities);
        _ -> build_sequence_spec(Activities, Options)
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
-spec init(activities()) -> {ok, #sequence_state{}}.
init(Activities) ->
    State = #sequence_state{
        activities = normalize_activities(Activities),
        current_position = 1
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Return initial marking for a place.
%%
%% @end
%%--------------------------------------------------------------------
-spec init_marking(atom(), #sequence_state{}) -> [term()].
init_marking(start, _State) ->
    [init];
init_marking(_Place, _State) ->
    [].

%%--------------------------------------------------------------------
%% @doc Check if a transition is enabled.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_enabled(atom(), wfnet_types:mode(), #sequence_state{}) -> boolean().
is_enabled(_Transition, _Mode, _State) ->
    true.

%%--------------------------------------------------------------------
%% @doc Fire a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec fire(atom(), wfnet_types:mode(), #sequence_state{}) ->
    abort | {produce, wfnet_types:produce_map()}.
fire(Transition, _Mode, #sequence_state{activities = Activities}) ->
    case Transition of
        start ->
            %% Start the first activity
            {FirstActivity, _} = get_activity(Activities, 1),
            {produce, #{activity_place(FirstActivity) => [start]}};
        complete_activity ->
            %% For sequence, just produce the completion
            %% The state update happens in the gen_wfnet engine
            {produce, #{'end' => [completed]}};
        _ ->
            abort
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Build sequence workflow specification.
%%--------------------------------------------------------------------
build_sequence_spec(Activities, _Options) ->
    Normalized = normalize_activities(Activities),
    N = length(Normalized),

    %% Generate place names
    Start = start,
    End = 'end',
    ActivityPlaces = [activity_place(A) || {A, _} <- Normalized],

    %% Generate transition names
    StartTrans = start,
    CompleteTrans = complete_activity,

    %% Build places list
    Places = [Start, End | ActivityPlaces],

    %% Build preset (transition -> input places)
    Preset = #{
        StartTrans => [Start]
        %% Complete transitions consume from activity places
    },

    %% Build postset (transition -> output places)
    Postset = case Normalized of
        [{FirstActivity, _} | _] ->
            #{StartTrans => [activity_place(FirstActivity)]}
    end,

    %% For a complete sequence, we need N transitions
    %% This is a simplified structure - full implementation would
    %% generate transitions dynamically
    #{
        places => Places,
        transitions => [StartTrans | [complete_activity || _ <- Normalized]],
        start_place => Start,
        end_place => End,
        preset => Preset,
        postset => Postset,
        optional => #{pattern => sequence, activity_count => N}
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize activities to {Name, Options} tuples.
%%--------------------------------------------------------------------
normalize_activities(Activities) ->
    lists:map(fun
        ({Name, Opts}) when is_atom(Name), is_map(Opts) -> {Name, Opts};
        (Name) when is_atom(Name) -> {Name, #{}}
    end, Activities).

%%--------------------------------------------------------------------
%% @private
%% @doc Get activity at position.
%%--------------------------------------------------------------------
get_activity(Activities, Pos) when Pos >= 1, Pos =< length(Activities) ->
    lists:nth(Pos, Activities).

%%--------------------------------------------------------------------
%% @private
%% @doc Generate activity place name.
%%--------------------------------------------------------------------
activity_place(Activity) ->
    list_to_atom(atom_to_list(Activity) ++ "_place").

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% new test
new_test() ->
    Spec = new([a, b, c]),
    ?assertMatch(#{places := _, transitions := _}, Spec).

%% normalize_activities test
normalize_activities_test() ->
    ?assertEqual([{a, #{}}, {b, #{}}], normalize_activities([a, b])),
    ?assertEqual([{a, #{opt => 1}}, {b, #{}}], normalize_activities([{a, #{opt => 1}}, b])).

-endif.
