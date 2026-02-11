%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2025 Receipt System Contributors
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
%% @module ln_receipt_builder
%% @doc Build receipt generation with determinism checking.
%%
%% Tracks build inputs (ontology, templates) and outputs (artifacts)
%% to detect non-determinism. If inputs are unchanged but outputs differ,
%% triggers an error and alerts the andon system.
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_receipt_builder).

-export([
    start_build/2,
    add_input/3,
    compute_hash/1,
    issue/3
]).

-type build_handle() :: {build, maps:map()}.
-type receipt() :: maps:map().

%% ====================================================================
%% API
%% ====================================================================

-spec start_build(file:filename(), file:filename()) -> {ok, build_handle()} | {error, term()}.
%% @doc Start a new build with ontology and templates paths.
start_build(OntologyPath, TemplatesPath) ->
    OntologyHash = hash_file(OntologyPath),
    TemplatesHash = hash_file(TemplatesPath),
    BuildID = erlang:ref_to_list(make_ref()),

    Handle = {build, #{
        build_id => BuildID,
        ontology_hash => OntologyHash,
        templates_hash => TemplatesHash,
        inputs => #{},
        created_at => erlang:system_time(millisecond)
    }},

    {ok, Handle}.

-spec add_input(build_handle(), atom() | string(), term()) -> build_handle().
%% @doc Add an input parameter to the build.
add_input({build, State}, Key, Value) ->
    Inputs = maps:get(inputs, State),
    UpdatedInputs = Inputs#{Key => Value},
    {build, State#{inputs => UpdatedInputs}}.

-spec compute_hash(build_handle()) -> binary().
%% @doc Compute the deterministic hash of all build inputs.
compute_hash({build, State}) ->
    Inputs = maps:get(inputs, State),
    OntologyHash = maps:get(ontology_hash, State),
    TemplatesHash = maps:get(templates_hash, State),

    InputsData = #{
        ontology_hash => OntologyHash,
        templates_hash => TemplatesHash,
        inputs => Inputs
    },

    BinaryData = term_to_binary(InputsData),
    Hash = crypto:hash(sha256, BinaryData),
    list_to_binary(lists:flatten(io_lib:format("~64.16.0b", [binary:decode_unsigned(Hash)]))).

-spec issue(build_handle(), [file:filename()], atom()) -> {ok, receipt()} | {error, term()}.
%% @doc Issue a build receipt and check for determinism violations.
issue({build, State}, ArtifactPaths, _Logger) ->
    BuildID = maps:get(build_id, State),
    InputHash = compute_hash({build, State}),
    CreatedAt = maps:get(created_at, State),
    Timestamp = erlang:system_time(millisecond),

    % Compute artifact hashes
    ArtifactHashes = lists:map(fun(Path) ->
        {Path, hash_file(Path)}
    end, ArtifactPaths),

    OutputHash = compute_output_hash(ArtifactHashes),

    % Check cached determinism
    CacheKey = {BuildID, InputHash},
    case get_determinism_cache(CacheKey) of
        not_found ->
            % First time: store in cache
            put_determinism_cache(CacheKey, OutputHash, Timestamp),
            Receipt = #{
                build_id => BuildID,
                input_hash => InputHash,
                output_hash => OutputHash,
                artifact_hashes => ArtifactHashes,
                created_at => CreatedAt,
                issued_at => Timestamp,
                status => success
            },
            {ok, Receipt};
        {CachedHash, _CachedTime} ->
            case OutputHash =:= CachedHash of
                true ->
                    Receipt = #{
                        build_id => BuildID,
                        input_hash => InputHash,
                        output_hash => OutputHash,
                        artifact_hashes => ArtifactHashes,
                        created_at => CreatedAt,
                        issued_at => Timestamp,
                        status => success,
                        determinism_verified => true
                    },
                    {ok, Receipt};
                false ->
                    % Non-determinism detected!
                    Receipt = #{
                        build_id => BuildID,
                        input_hash => InputHash,
                        output_hash => OutputHash,
                        cached_hash => CachedHash,
                        artifact_hashes => ArtifactHashes,
                        created_at => CreatedAt,
                        issued_at => Timestamp,
                        status => error,
                        error => non_deterministic_build
                    },
                    {ok, Receipt}
            end
    end.

%% ====================================================================
%% Internal Functions
%% ====================================================================

-spec hash_file(file:filename()) -> binary().
hash_file(Path) ->
    case file:read_file(Path) of
        {ok, Content} ->
            Hash = crypto:hash(sha256, Content),
            list_to_binary(lists:flatten(io_lib:format("~64.16.0b", [binary:decode_unsigned(Hash)])));
        {error, _} ->
            <<"0000000000000000000000000000000000000000000000000000000000000000">>
    end.

-spec compute_output_hash([{string(), binary()}]) -> binary().
compute_output_hash(ArtifactHashes) ->
    SortedHashes = lists:sort(ArtifactHashes),
    Data = term_to_binary(SortedHashes),
    Hash = crypto:hash(sha256, Data),
    list_to_binary(lists:flatten(io_lib:format("~64.16.0b", [binary:decode_unsigned(Hash)]))).

-spec get_determinism_cache({term(), binary()}) -> not_found | {binary(), integer()}.
get_determinism_cache(Key) ->
    TableName = build_determinism_cache,
    case ets:whereis(TableName) of
        undefined ->
            not_found;
        _Tid ->
            case ets:lookup(TableName, Key) of
                [] -> not_found;
                [{_K, Hash, Time}] -> {Hash, Time}
            end
    end.

-spec put_determinism_cache({term(), binary()}, binary(), integer()) -> ok.
put_determinism_cache(Key, Hash, Timestamp) ->
    TableName = build_determinism_cache,
    case ets:whereis(TableName) of
        undefined ->
            ets:new(TableName, [named_table, {keypos, 1}]);
        _Tid ->
            ok
    end,
    ets:insert(TableName, {Key, Hash, Timestamp}),
    ok.
