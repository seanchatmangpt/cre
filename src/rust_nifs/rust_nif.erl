%%%-------------------------------------------------------------------
%%% @doc
%%% Rust NIF bindings for CRE Process Mining Algorithms
%%%
%%% This module provides Erlang wrappers for Rust-native implementations
%%% of process mining algorithms via NIF (Native Implemented Functions).
%%%
%%% The NIF is loaded automatically on module initialization. If the NIF
%%% library cannot be found, the module falls back to pure Erlang
%%% implementations where available.
%%%
%%% <h4>Available Algorithms:</h4>
%%% <ul>
%%%   <li><b>Alpha Algorithm</b>: Process discovery from event logs</li>
%%%   <li><b>Heuristic Miner</b>: Noise-tolerant process discovery</li>
%%%   <li><b>Conformance Checking</b>: Fitness and precision analysis</li>
%%%   <li><b>Object-Centric Mining</b>: Multi-dimensional process analysis</li>
%%% </ul>
%%%
%%% <h4>Usage Example:</h4>
%%% <pre>
%%% %% Load an event log from JSON
%%% {ok, Log} = rust_nif:load_json_log(JsonLog),
%%%
%%% %% Discover a process model
%%% {ok, Model} = rust_nif:alpha_discover(Log),
%%%
%%% %% Check conformance
%%% {ok, #{fitness := Fitness}} = rust_nif:conformance_check(Log, Model).
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(rust_nif).
-behaviour(gen_server).

%% NIF function exports
-export([
    %% Alpha algorithm
    alpha_discover/1,
    alpha_discover_with_params/2,
    alpha_extract_relations/1,

    %% Heuristic miner
    heuristic_discover/1,
    heuristic_discover_with_params/2,
    heuristic_get_dependencies/1,

    %% Conformance checking
    conformance_check/2,
    conformance_fitness/2,
    conformance_precision/2,
    conformance_align/2,

    %% Object-centric
    object_centric_discover/1,
    object_centric_ocel_deserialize/1,
    object_centric_project/2,
    object_centric_interactions/1,

    %% Event log functions
    load_xes_log/1,
    load_json_log/1,
    log_to_traces/1,
    log_statistics/1,

    %% Model functions
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
    resource_stats/0,

    %% Utility functions
    version/0,
    algorithm_list/0,
    benchmark/2
]).

%% gen_server exports
-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(SERVER, ?MODULE).

%%%-------------------------------------------------------------------
%%% NIF Loading
%%%-------------------------------------------------------------------

%% @private Load the NIF library when the module is loaded
-on_load(init/0).

-spec init() -> term().
init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            %% Fallback for development (when not installed as an app)
            AppDir = filename:dirname(code:which(?MODULE)),
            PrivDir1 = filename:join([filename:dirname(AppDir), "priv"]),
            case filelib:is_dir(PrivDir1) of
                true ->
                    filename:join(PrivDir1, "libcre_rust_nif");
                _ ->
                    %% Try src/rust_nifs/priv during development
                    SrcDir = filename:join(filename:dirname(AppDir), "rust_nifs"),
                    filename:join(SrcDir, "priv/libcre_rust_nif")
            end;
        Dir ->
            filename:join(Dir, "libcre_rust_nif")
    end,
    %% Add platform-specific extension
    FinalSo = case os:type() of
        {win32, _} -> SoName ++ ".dll";
        {unix, darwin} -> SoName ++ ".dylib";
        {unix, _} -> SoName ++ ".so"
    end,
    case erlang:load_nif(FinalSo, 0) of
        ok ->
            ok;
        {error, {load_failed, _}} = Error ->
            logger:warning("Failed to load Rust NIF library: ~p. "
                          "Falling back to pure Erlang implementations.", [Error]),
            Error;
        Error ->
            logger:warning("Failed to load Rust NIF library: ~p", [Error]),
            Error
    end.

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------

%% @doc Start the NIF server (optional, for stateful operations)
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get the NIF library version
-spec version() -> {ok, string()} | {error, term()}.
version() ->
    case nif_available() of
        true -> nif_version();
        false -> {ok, "0.1.0-erlang-fallback"}
    end.

%% @doc Get list of available algorithms
-spec algorithm_list() -> {ok, [string()]} | {error, term()}.
algorithm_list() ->
    case nif_available() of
        true -> {ok, nif_algorithm_list()};
        false -> {ok, ["alpha", "heuristic", "conformance", "object_centric"]}
    end.

%% @doc Benchmark an algorithm with input data
-spec benchmark(string(), term()) -> {ok, map()} | {error, term()}.
benchmark(Algorithm, Input) ->
    case nif_available() of
        true -> nif_benchmark(Algorithm, Input);
        false -> erlang_fallback:benchmark(Algorithm, Input)
    end.

%%%-------------------------------------------------------------------
%%% Alpha Algorithm Functions
%%%-------------------------------------------------------------------

%% @doc Discover a process model using Alpha algorithm
-spec alpha_discover(term()) -> {ok, map()} | {error, term()}.
alpha_discover(Log) ->
    case nif_available() of
        true -> nif_alpha_discover(Log);
        false -> erlang_fallback:alpha_discover(Log)
    end.

%% @doc Discover with custom parameters
-spec alpha_discover_with_params(term(), term()) -> {ok, map()} | {error, term()}.
alpha_discover_with_params(Log, Params) ->
    case nif_available() of
        true -> nif_alpha_discover_with_params(Log, Params);
        false -> erlang_fallback:alpha_discover_with_params(Log, Params)
    end.

%% @doc Extract ordering relations from event log
-spec alpha_extract_relations(term()) -> {ok, map()} | {error, term()}.
alpha_extract_relations(Log) ->
    case nif_available() of
        true -> nif_alpha_extract_relations(Log);
        false -> erlang_fallback:alpha_extract_relations(Log)
    end.

%%%-------------------------------------------------------------------
%%% Heuristic Miner Functions
%%%-------------------------------------------------------------------

%% @doc Discover using Heuristic Miner
-spec heuristic_discover(term()) -> {ok, map()} | {error, term()}.
heuristic_discover(Log) ->
    case nif_available() of
        true -> nif_heuristic_discover(Log);
        false -> erlang_fallback:heuristic_discover(Log)
    end.

%% @doc Discover Heuristic Miner with custom parameters
-spec heuristic_discover_with_params(term(), term()) -> {ok, map()} | {error, term()}.
heuristic_discover_with_params(Log, Params) ->
    case nif_available() of
        true -> nif_heuristic_discover_with_params(Log, Params);
        false -> erlang_fallback:heuristic_discover_with_params(Log, Params)
    end.

%% @doc Get dependency relations
-spec heuristic_get_dependencies(term()) -> {ok, map()} | {error, term()}.
heuristic_get_dependencies(Log) ->
    case nif_available() of
        true -> nif_heuristic_get_dependencies(Log);
        false -> erlang_fallback:heuristic_get_dependencies(Log)
    end.

%%%-------------------------------------------------------------------
%%% Conformance Checking Functions
%%%-------------------------------------------------------------------

%% @doc Perform full conformance check
-spec conformance_check(term(), term()) -> {ok, map()} | {error, term()}.
conformance_check(Log, Model) ->
    case nif_available() of
        true -> nif_conformance_check(Log, Model);
        false -> erlang_fallback:conformance_check(Log, Model)
    end.

%% @doc Calculate fitness score
-spec conformance_fitness(term(), term()) -> {ok, float()} | {error, term()}.
conformance_fitness(Log, Model) ->
    case nif_available() of
        true -> {ok, nif_conformance_fitness(Log, Model)};
        false -> erlang_fallback:conformance_fitness(Log, Model)
    end.

%% @doc Calculate precision score
-spec conformance_precision(term(), term()) -> {ok, float()} | {error, term()}.
conformance_precision(Log, Model) ->
    case nif_available() of
        true -> {ok, nif_conformance_precision(Log, Model)};
        false -> erlang_fallback:conformance_precision(Log, Model)
    end.

%% @doc Calculate trace alignments
-spec conformance_align(term(), term()) -> {ok, list()} | {error, term()}.
conformance_align(Log, Model) ->
    case nif_available() of
        true -> {ok, nif_conformance_align(Log, Model)};
        false -> erlang_fallback:conformance_align(Log, Model)
    end.

%%%-------------------------------------------------------------------
%%% Object-Centric Functions
%%%-------------------------------------------------------------------

%% @doc Discover object-centric models
-spec object_centric_discover(term()) -> {ok, map()} | {error, term()}.
object_centric_discover(OCEL) ->
    case nif_available() of
        true -> nif_object_centric_discover(OCEL);
        false -> erlang_fallback:object_centric_discover(OCEL)
    end.

%% @doc Deserialize OCEL 2.0 JSON
-spec object_centric_ocel_deserialize(string() | binary()) -> {ok, map()} | {error, term()}.
object_centric_ocel_deserialize(Json) ->
    case nif_available() of
        true -> nif_object_centric_ocel_deserialize(to_string(Json));
        false -> erlang_fallback:object_centric_ocel_deserialize(Json)
    end.

%% @doc Project OCEL to single object type
-spec object_centric_project(term(), string()) -> {ok, term()} | {error, term()}.
object_centric_project(OCEL, ObjectType) ->
    case nif_available() of
        true -> nif_object_centric_project(OCEL, ObjectType);
        false -> erlang_fallback:object_centric_project(OCEL, ObjectType)
    end.

%% @doc Get object interaction patterns
-spec object_centric_interactions(term()) -> {ok, list()} | {error, term()}.
object_centric_interactions(OCEL) ->
    case nif_available() of
        true -> nif_object_centric_interactions(OCEL);
        false -> erlang_fallback:object_centric_interactions(OCEL)
    end.

%%%-------------------------------------------------------------------
%%% Event Log Functions
%%%-------------------------------------------------------------------

%% @doc Load event log from XES file
-spec load_xes_log(file:filename_all()) -> {ok, map()} | {error, term()}.
load_xes_log(Path) ->
    case nif_available() of
        true -> nif_load_xes_log(to_string(Path));
        false -> erlang_fallback:load_xes_log(Path)
    end.

%% @doc Load event log from JSON
-spec load_json_log(string() | binary()) -> {ok, map()} | {error, term()}.
load_json_log(Json) ->
    case nif_available() of
        true -> nif_load_json_log(to_string(Json));
        false -> erlang_fallback:load_json_log(Json)
    end.

%% @doc Convert log to traces
-spec log_to_traces(term()) -> {ok, list()} | {error, term()}.
log_to_traces(Log) ->
    case nif_available() of
        true -> {ok, nif_log_to_traces(Log)};
        false -> erlang_fallback:log_to_traces(Log)
    end.

%% @doc Get log statistics
-spec log_statistics(term()) -> {ok, map()} | {error, term()}.
log_statistics(Log) ->
    case nif_available() of
        true -> {ok, nif_log_statistics(Log)};
        false -> erlang_fallback:log_statistics(Log)
    end.

%%%-------------------------------------------------------------------
%%% Model Functions
%%%-------------------------------------------------------------------

%% @doc Convert model to DOT format
-spec model_to_dot(term()) -> {ok, string()} | {error, term()}.
model_to_dot(Model) ->
    case nif_available() of
        true -> {ok, nif_model_to_dot(Model)};
        false -> erlang_fallback:model_to_dot(Model)
    end.

%% @doc Convert model to JSON
-spec model_to_json(term()) -> {ok, string()} | {error, term()}.
model_to_json(Model) ->
    case nif_available() of
        true -> {ok, nif_model_to_json(Model)};
        false -> erlang_fallback:model_to_json(Model)
    end.

%% @doc Validate model
-spec model_validate(term()) -> {ok, boolean()} | {error, term()}.
model_validate(Model) ->
    case nif_available() of
        true -> {ok, nif_model_validate(Model)};
        false -> erlang_fallback:model_validate(Model)
    end.

%% @doc Get model nodes
-spec model_get_nodes(term()) -> {ok, list()} | {error, term()}.
model_get_nodes(Model) ->
    case nif_available() of
        true -> {ok, nif_model_get_nodes(Model)};
        false -> erlang_fallback:model_get_nodes(Model)
    end.

%% @doc Get model edges
-spec model_get_edges(term()) -> {ok, list()} | {error, term()}.
model_get_edges(Model) ->
    case nif_available() of
        true -> {ok, nif_model_get_edges(Model)};
        false -> erlang_fallback:model_get_edges(Model)
    end.

%%%-------------------------------------------------------------------
%%% Resource Management Functions
%%%-------------------------------------------------------------------

%% @doc Create a resource
-spec resource_create(term()) -> {ok, {integer(), atom()}} | {error, term()}.
resource_create(Data) ->
    case nif_available() of
        true -> {ok, nif_resource_create(Data)};
        false -> erlang_fallback:resource_create(Data)
    end.

%% @doc Get a resource
-spec resource_get(integer()) -> {ok, term()} | {error, term()}.
resource_get(Id) ->
    case nif_available() of
        true -> {ok, nif_resource_get(Id)};
        false -> erlang_fallback:resource_get(Id)
    end.

%% @doc Update a resource
-spec resource_update(integer(), term()) -> {ok, atom()} | {error, term()}.
resource_update(Id, Update) ->
    case nif_available() of
        true -> {ok, nif_resource_update(Id, Update)};
        false -> erlang_fallback:resource_update(Id, Update)
    end.

%% @doc Delete a resource
-spec resource_delete(integer()) -> {ok, boolean()} | {error, term()}.
resource_delete(Id) ->
    case nif_available() of
        true -> {ok, nif_resource_delete(Id)};
        false -> erlang_fallback:resource_delete(Id)
    end.

%% @doc Get resource statistics
-spec resource_stats() -> {ok, map()} | {error, term()}.
resource_stats() ->
    case nif_available() of
        true -> {ok, nif_resource_stats()};
        false -> erlang_fallback:resource_stats()
    end.

%%%-------------------------------------------------------------------
%%% gen_server callbacks (for stateful operations)
%%%-------------------------------------------------------------------

%% @private
init([]) ->
    {ok, #{}}.

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

%%%-------------------------------------------------------------------
%%% Helper Functions
%%%-------------------------------------------------------------------

%% @private Check if NIF is available
-spec nif_available() -> boolean().
nif_available() ->
    case erlang:whereis(?MODULE) of
        undefined ->
            %% Try calling a simple NIF function to check availability
            try
                case nif_version() of
                    {ok, _} -> true;
                    _ -> false
                end
            catch
                error:_ -> false
            end;
        _Pid -> true
    end.

%% @private Convert to string
-spec to_string(binary() | string() | atom()) -> string().
to_string(B) when is_binary(B) -> binary_to_list(B);
to_string(A) when is_atom(A) -> atom_to_list(A);
to_string(S) when is_list(S) -> S.

%%%-------------------------------------------------------------------
%%% NIF Stubs (loaded from Rust library)
%%%-------------------------------------------------------------------

%% These functions are replaced by the actual NIF implementations
%% when the library is successfully loaded.

-spec nif_version() -> {ok, string()}.
nif_version() -> erlang:nif_error(nif_not_loaded).

-spec nif_algorithm_list() -> [string()].
nif_algorithm_list() -> erlang:nif_error(nif_not_loaded).

-spec nif_benchmark(string(), term()) -> {ok, map()}.
nif_benchmark(_, _) -> erlang:nif_error(nif_not_loaded).

%% Alpha NIFs
-spec nif_alpha_discover(term()) -> {ok, map()}.
nif_alpha_discover(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_alpha_discover_with_params(term(), term()) -> {ok, map()}.
nif_alpha_discover_with_params(_, _) -> erlang:nif_error(nif_not_loaded).

-spec nif_alpha_extract_relations(term()) -> {ok, map()}.
nif_alpha_extract_relations(_) -> erlang:nif_error(nif_not_loaded).

%% Heuristic NIFs
-spec nif_heuristic_discover(term()) -> {ok, map()}.
nif_heuristic_discover(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_heuristic_discover_with_params(term(), term()) -> {ok, map()}.
nif_heuristic_discover_with_params(_, _) -> erlang:nif_error(nif_not_loaded).

-spec nif_heuristic_get_dependencies(term()) -> {ok, map()}.
nif_heuristic_get_dependencies(_) -> erlang:nif_error(nif_not_loaded).

%% Conformance NIFs
-spec nif_conformance_check(term(), term()) -> {ok, map()}.
nif_conformance_check(_, _) -> erlang:nif_error(nif_not_loaded).

-spec nif_conformance_fitness(term(), term()) -> float().
nif_conformance_fitness(_, _) -> erlang:nif_error(nif_not_loaded).

-spec nif_conformance_precision(term(), term()) -> float().
nif_conformance_precision(_, _) -> erlang:nif_error(nif_not_loaded).

-spec nif_conformance_align(term(), term()) -> list().
nif_conformance_align(_, _) -> erlang:nif_error(nif_not_loaded).

%% Object-centric NIFs
-spec nif_object_centric_discover(term()) -> {ok, map()}.
nif_object_centric_discover(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_object_centric_ocel_deserialize(string()) -> {ok, map()}.
nif_object_centric_ocel_deserialize(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_object_centric_project(term(), string()) -> {ok, term()}.
nif_object_centric_project(_, _) -> erlang:nif_error(nif_not_loaded).

-spec nif_object_centric_interactions(term()) -> {ok, list()}.
nif_object_centric_interactions(_) -> erlang:nif_error(nif_not_loaded).

%% Event log NIFs
-spec nif_load_xes_log(string()) -> {ok, map()}.
nif_load_xes_log(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_load_json_log(string()) -> {ok, map()}.
nif_load_json_log(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_log_to_traces(term()) -> list().
nif_log_to_traces(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_log_statistics(term()) -> map().
nif_log_statistics(_) -> erlang:nif_error(nif_not_loaded).

%% Model NIFs
-spec nif_model_to_dot(term()) -> string().
nif_model_to_dot(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_model_to_json(term()) -> string().
nif_model_to_json(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_model_validate(term()) -> boolean().
nif_model_validate(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_model_get_nodes(term()) -> list().
nif_model_get_nodes(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_model_get_edges(term()) -> list().
nif_model_get_edges(_) -> erlang:nif_error(nif_not_loaded).

%% Resource NIFs
-spec nif_resource_create(term()) -> {integer(), atom()}.
nif_resource_create(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_resource_get(integer()) -> term().
nif_resource_get(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_resource_update(integer(), term()) -> atom().
nif_resource_update(_, _) -> erlang:nif_error(nif_not_loaded).

-spec nif_resource_delete(integer()) -> boolean().
nif_resource_delete(_) -> erlang:nif_error(nif_not_loaded).

-spec nif_resource_stats() -> map().
nif_resource_stats() -> erlang:nif_error(nif_not_loaded).
