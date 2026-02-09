%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2024 CRE Team
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
%% @doc Rust NIF bindings for CRE process mining algorithms
%%
%% This module provides Erlang wrappers around the Rust NIF implementations
%% of high-performance process mining algorithms.
%%
%% The module supports:
%% - Alpha Algorithm for process discovery
%% - Heuristic Miner for noise-tolerant discovery
%% - Conformance checking for model validation
%% - Object-centric process mining
%%
%% <h3>Usage Example</h3>
%%
%% ```erlang
%% %% Load an event log (list of traces)
%% Log = [[a, b, c, d], [a, c, b, d]],
%%
%% %% Discover a model using the Alpha algorithm
%% {ok, AlphaResult} = rust_nif:alpha_discover(Log),
%%
%% %% Get the fitness score
%% Fitness = maps:get(fitness, AlphaResult).
%% ```
%%
%% <h3>Performance</h3>
%%
%% The Rust NIF implementations provide significant performance
%% improvements over pure Erlang implementations:
%% - Alpha algorithm: 10-100x faster for large logs
%% - Heuristic miner: 5-50x faster
%% - Conformance checking: 20-200x faster
%%
%% @end
%% -------------------------------------------------------------------

-module(rust_nif).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([
    %% NIF loading
    load_nif/0,
    load_nif/1,
    is_available/0,

    %% Alpha algorithm
    alpha_discover/1,
    alpha_discover/2,
    alpha_extract_relations/1,

    %% Heuristic miner
    heuristic_discover/1,
    heuristic_discover/2,
    heuristic_get_dependencies/1,

    %% Conformance checking
    conformance_check/2,
    conformance_fitness/2,
    conformance_precision/2,
    conformance_align/2,

    %% Object-centric mining
    object_centric_discover/1,
    object_centric_ocel_deserialize/1,

    %% Event log utilities
    load_xes_log/1,
    load_json_log/1,
    log_to_traces/1,
    log_statistics/1,

    %% Model utilities
    model_to_dot/1,
    model_to_json/1,
    model_validate/1,
    model_get_nodes/1,
    model_get_edges/1,

    %% Resource management
    resource_create/1,
    resource_get/1,
    resource_update/2,
    resource_delete/1,

    %% Utility functions
    version/0,
    algorithm_list/0,
    benchmark/2,

    %% gen_server callbacks
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%%====================================================================
%% Type Definitions
%%====================================================================

-type trace() :: [atom() | binary()].
-type event_log() :: [trace()].
-type algorithm_result() :: #{
    fitness := float(),
    precision := float(),
    computation_time_ms := non_neg_integer()
}.

-export_type([
    event_log/0,
    algorithm_result/0
]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Check if the Rust NIF is available
%%
%% Returns true if the NIF library loaded successfully, false otherwise.
%% @end
%%--------------------------------------------------------------------
-spec is_available() -> boolean().

is_available() ->
    case persistent_term:get(?MODULE, undefined) of
        {true, _} -> true;
        _ -> false
    end.

%%--------------------------------------------------------------------
%% @doc Load the Rust NIF library
%%
%% Tries to load the NIF library from the priv directory.
%% Returns {ok, loaded} if successful, {error, Reason} otherwise.
%% @end
%%--------------------------------------------------------------------
-spec load_nif() -> {ok, loaded} | {error, term()}.

load_nif() ->
    load_nif(filename:join([code:priv_dir(cre), "lib", "cre_rust_nif"])).

%%--------------------------------------------------------------------
%% @doc Load the Rust NIF library from a specific path
%% @end
%%--------------------------------------------------------------------
-spec load_nif(string()) -> {ok, loaded} | {error, term()}.

load_nif(LibPath) ->
    case erlang:load_nif(LibPath, []) of
        ok ->
            persistent_term:put(?MODULE, {true, LibPath}),
            {ok, loaded};
        {error, {reload, _}} ->
            persistent_term:put(?MODULE, {true, LibPath}),
            {ok, loaded};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Discover a process model using the Alpha algorithm
%%
%% <div class="warning">
%% This function requires the Rust NIF to be loaded.
%% </div>
%% @end
%%--------------------------------------------------------------------
-spec alpha_discover(event_log()) -> {ok, algorithm_result()} | {error, term()}.

alpha_discover(Log) ->
    alpha_discover(Log, #{}).

%%--------------------------------------------------------------------
%% @doc Discover a process model using the Alpha algorithm with parameters
%% @end
%%--------------------------------------------------------------------
-spec alpha_discover(event_log(), map()) -> {ok, algorithm_result()} | {error, term()}.

alpha_discover(Log, Params) ->
    case ensure_nif_loaded() of
        ok ->
            nif_alpha_discover(Log, Params);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Extract ordering relations from an event log
%% @end
%%--------------------------------------------------------------------
-spec alpha_extract_relations(event_log()) -> {ok, map()} | {error, term()}.

alpha_extract_relations(Log) ->
    case ensure_nif_loaded() of
        ok ->
            nif_alpha_extract_relations(Log);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Discover a process model using the Heuristic Miner
%% @end
%%--------------------------------------------------------------------
-spec heuristic_discover(event_log()) -> {ok, algorithm_result()} | {error, term()}.

heuristic_discover(Log) ->
    heuristic_discover(Log, #{}).

%%--------------------------------------------------------------------
%% @doc Discover a process model using Heuristic Miner with parameters
%% @end
%%--------------------------------------------------------------------
-spec heuristic_discover(event_log(), map()) -> {ok, algorithm_result()} | {error, term()}.

heuristic_discover(Log, Params) ->
    case ensure_nif_loaded() of
        ok ->
            nif_heuristic_discover(Log, Params);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Get dependency relations from the Heuristic Miner
%% @end
%%--------------------------------------------------------------------
-spec heuristic_get_dependencies(event_log()) -> {ok, map()} | {error, term()}.

heuristic_get_dependencies(Log) ->
    case ensure_nif_loaded() of
        ok ->
            nif_heuristic_get_dependencies(Log);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Perform conformance checking on a model
%% @end
%%--------------------------------------------------------------------
-spec conformance_check(event_log(), term()) -> {ok, map()} | {error, term()}.

conformance_check(Log, Model) ->
    case ensure_nif_loaded() of
        ok ->
            nif_conformance_check(Log, Model);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Calculate fitness score
%% @end
%%--------------------------------------------------------------------
-spec conformance_fitness(event_log(), term()) -> {ok, float()} | {error, term()}.

conformance_fitness(Log, Model) ->
    case ensure_nif_loaded() of
        ok ->
            nif_conformance_fitness(Log, Model);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Calculate precision score
%% @end
%%--------------------------------------------------------------------
-spec conformance_precision(event_log(), term()) -> {ok, float()} | {error, term()}.

conformance_precision(Log, Model) ->
    case ensure_nif_loaded() of
        ok ->
            nif_conformance_precision(Log, Model);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Calculate alignments between log and model
%% @end
%%--------------------------------------------------------------------
-spec conformance_align(event_log(), term()) -> {ok, [map()]} | {error, term()}.

conformance_align(Log, Model) ->
    case ensure_nif_loaded() of
        ok ->
            nif_conformance_align(Log, Model);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Discover object-centric process models
%% @end
%%--------------------------------------------------------------------
-spec object_centric_discover(binary()) -> {ok, map()} | {error, term()}.

object_centric_discover(OcelJson) ->
    case ensure_nif_loaded() of
        ok ->
            nif_object_centric_discover(OcelJson);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Deserialize an OCEL 2.0 JSON event log
%% @end
%%--------------------------------------------------------------------
-spec object_centric_ocel_deserialize(binary()) -> {ok, map()} | {error, term()}.

object_centric_ocel_deserialize(Json) ->
    case ensure_nif_loaded() of
        ok ->
            nif_object_centric_ocel_deserialize(Json);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Load an event log from XES format
%% @end
%%--------------------------------------------------------------------
-spec load_xes_log(file:filename()) -> {ok, term()} | {error, term()}.

load_xes_log(Filename) ->
    case ensure_nif_loaded() of
        ok ->
            nif_load_xes_log(Filename);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Load an event log from JSON format
%% @end
%%--------------------------------------------------------------------
-spec load_json_log(file:filename() | binary()) -> {ok, term()} | {error, term()}.

load_json_log(Input) when is_binary(Input) ->
    case ensure_nif_loaded() of
        ok ->
            nif_load_json_log(Input);
        {error, _} = Error ->
            Error
    end;
load_json_log(Filename) ->
    case file:read_file(Filename) of
        {ok, Content} ->
            load_json_log(Content);
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Convert an event log to trace format
%% @end
%%--------------------------------------------------------------------
-spec log_to_traces(term()) -> {ok, [[atom()]]} | {error, term()}.

log_to_traces(Log) ->
    case ensure_nif_loaded() of
        ok ->
            nif_log_to_traces(Log);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Get statistics about an event log
%% @end
%%--------------------------------------------------------------------
-spec log_statistics(term()) -> {ok, map()} | {error, term()}.

log_statistics(Log) ->
    case ensure_nif_loaded() of
        ok ->
            nif_log_statistics(Log);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Convert a process model to DOT format
%% @end
%%--------------------------------------------------------------------
-spec model_to_dot(term()) -> {ok, binary()} | {error, term()}.

model_to_dot(Model) ->
    case ensure_nif_loaded() of
        ok ->
            nif_model_to_dot(Model);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Convert a process model to JSON format
%% @end
%%--------------------------------------------------------------------
-spec model_to_json(term()) -> {ok, binary()} | {error, term()}.

model_to_json(Model) ->
    case ensure_nif_loaded() of
        ok ->
            nif_model_to_json(Model);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Validate a process model
%% @end
%%--------------------------------------------------------------------
-spec model_validate(term()) -> {ok, boolean()} | {error, term()}.

model_validate(Model) ->
    case ensure_nif_loaded() of
        ok ->
            nif_model_validate(Model);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Get nodes from a process model
%% @end
%%--------------------------------------------------------------------
-spec model_get_nodes(term()) -> {ok, [term()]} | {error, term()}.

model_get_nodes(Model) ->
    case ensure_nif_loaded() of
        ok ->
            nif_model_get_nodes(Model);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Get edges from a process model
%% @end
%%--------------------------------------------------------------------
-spec model_get_edges(term()) -> {ok, [term()]} | {error, term()}.

model_get_edges(Model) ->
    case ensure_nif_loaded() of
        ok ->
            nif_model_get_edges(Model);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Create a resource for long-lived Rust objects
%% @end
%%--------------------------------------------------------------------
-spec resource_create(term()) -> {ok, pos_integer()} | {error, term()}.

resource_create(Data) ->
    case ensure_nif_loaded() of
        ok ->
            nif_resource_create(Data);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Get a resource by ID
%% @end
%%--------------------------------------------------------------------
-spec resource_get(pos_integer()) -> {ok, term()} | {error, term()}.

resource_get(Id) ->
    case ensure_nif_loaded() of
        ok ->
            nif_resource_get(Id);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Update a resource
%% @end
%%--------------------------------------------------------------------
-spec resource_update(pos_integer(), term()) -> ok | {error, term()}.

resource_update(Id, Update) ->
    case ensure_nif_loaded() of
        ok ->
            nif_resource_update(Id, Update);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Delete a resource
%% @end
%%--------------------------------------------------------------------
-spec resource_delete(pos_integer()) -> ok | {error, term()}.

resource_delete(Id) ->
    case ensure_nif_loaded() of
        ok ->
            nif_resource_delete(Id);
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Get the version of the Rust NIF library
%% @end
%%--------------------------------------------------------------------
-spec version() -> {ok, binary()} | {error, term()}.

version() ->
    case ensure_nif_loaded() of
        ok ->
            nif_version();
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Get a list of available algorithms
%% @end
%%--------------------------------------------------------------------
-spec algorithm_list() -> {ok, [atom()]} | {error, term()}.

algorithm_list() ->
    case ensure_nif_loaded() of
        ok ->
            nif_algorithm_list();
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Benchmark an algorithm
%% @end
%%--------------------------------------------------------------------
-spec benchmark(atom(), event_log()) -> {ok, map()} | {error, term()}.

benchmark(Algorithm, Log) ->
    case ensure_nif_loaded() of
        ok ->
            nif_benchmark(Algorithm, Log);
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([]) ->
    case load_nif() of
        {ok, loaded} ->
            {ok, #{nif_loaded => true}};
        {error, Reason} ->
            logger:warning("Failed to load Rust NIF: ~p", [Reason]),
            {ok, #{nif_loaded => false}}
    end.

%% @private
handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
ensure_nif_loaded() ->
    case is_available() of
        true ->
            ok;
        false ->
            case load_nif() of
                {ok, loaded} -> ok;
                {error, Reason} ->
                    {error, {nif_not_loaded, Reason}}
            end
    end.

%%====================================================================
%% NIF Stubs (these are replaced when the NIF loads)
%%====================================================================

%% NIF function stubs - these will be replaced when the NIF loads

nif_alpha_discover(_Log, _Params) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_alpha_extract_relations(_Log) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_heuristic_discover(_Log, _Params) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_heuristic_get_dependencies(_Log) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_conformance_check(_Log, _Model) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_conformance_fitness(_Log, _Model) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_conformance_precision(_Log, _Model) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_conformance_align(_Log, _Model) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_object_centric_discover(_OcelJson) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_object_centric_ocel_deserialize(_Json) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_load_xes_log(_Filename) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_load_json_log(_Json) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_log_to_traces(_Log) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_log_statistics(_Log) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_model_to_dot(_Model) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_model_to_json(_Model) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_model_validate(_Model) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_model_get_nodes(_Model) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_model_get_edges(_Model) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_resource_create(_Data) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_resource_get(_Id) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_resource_update(_Id, _Update) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_resource_delete(_Id) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_version() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_algorithm_list() ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).

nif_benchmark(_Algorithm, _Log) ->
    erlang:nif_error({nif_not_loaded, ?MODULE}).
