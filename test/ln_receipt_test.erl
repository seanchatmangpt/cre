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
%% @module ln_receipt_test
%% @doc Unit tests for receipt system (log, builder, effect, andon).
%%
%% @end
%% -------------------------------------------------------------------

-module(ln_receipt_test).

-include_lib("eunit/include/eunit.hrl").

%% ====================================================================
%% Receipt Log Tests
%% ====================================================================

append_only_test() ->
    TempFile = "/tmp/receipt_log_test_" ++ erlang:ref_to_list(make_ref()) ++ ".log",
    {ok, Handle} = ln_receipt_log:new_log(TempFile),

    try
        % Write 3 receipts
        {ok, ID1} = ln_receipt_log:append(Handle, <<"data1">>),
        {ok, ID2} = ln_receipt_log:append(Handle, <<"data2">>),
        {ok, ID3} = ln_receipt_log:append(Handle, <<"data3">>),

        % Read them back
        {ok, Receipt1} = ln_receipt_log:read(Handle, ID1),
        {ok, Receipt2} = ln_receipt_log:read(Handle, ID2),
        {ok, Receipt3} = ln_receipt_log:read(Handle, ID3),

        % Verify order
        ?assertEqual(1, maps:get(seq, Receipt1)),
        ?assertEqual(2, maps:get(seq, Receipt2)),
        ?assertEqual(3, maps:get(seq, Receipt3)),

        % Verify data
        ?assertEqual(<<"data1">>, maps:get(data, Receipt1)),
        ?assertEqual(<<"data2">>, maps:get(data, Receipt2)),
        ?assertEqual(<<"data3">>, maps:get(data, Receipt3))
    after
        case ets:whereis(receipt_log_ets) of
            undefined -> ok;
            CleanupTid1 -> ets:delete(CleanupTid1)
        end,
        file:delete(TempFile)
    end.

hash_chain_validation_test() ->
    TempFile = "/tmp/receipt_log_chain_" ++ erlang:ref_to_list(make_ref()) ++ ".log",
    {ok, Handle} = ln_receipt_log:new_log(TempFile),

    try
        % Write 3 receipts
        {ok, _ID1} = ln_receipt_log:append(Handle, <<"data1">>),
        {ok, _ID2} = ln_receipt_log:append(Handle, <<"data2">>),
        {ok, _ID3} = ln_receipt_log:append(Handle, <<"data3">>),

        % Validate chain before tampering - should succeed
        ValidResult1 = ln_receipt_log:validate_chain(Handle),
        ?assertMatch({ok, _}, ValidResult1),

        % Manually tamper with middle receipt in ETS
        EtsTableID = element(2, Handle),
        ets:delete(EtsTableID, 2),
        ets:insert(EtsTableID, {2, <<"fakehash">>, <<"someprevhash">>, erlang:system_time(millisecond), <<"data2">>}),

        % Validate chain after tampering - should fail
        Result = ln_receipt_log:validate_chain(Handle),
        ?assertEqual({error, chain_broken}, Result)
    after
        case ets:whereis(receipt_log_ets) of
            undefined -> ok;
            CleanupTid -> ets:delete(CleanupTid)
        end,
        file:delete(TempFile)
    end.

range_read_test() ->
    TempFile = "/tmp/receipt_log_range_" ++ erlang:ref_to_list(make_ref()) ++ ".log",
    {ok, Handle} = ln_receipt_log:new_log(TempFile),

    try
        % Write 5 receipts
        {ok, ID1} = ln_receipt_log:append(Handle, <<"data1">>),
        {ok, _ID2} = ln_receipt_log:append(Handle, <<"data2">>),
        {ok, _ID3} = ln_receipt_log:append(Handle, <<"data3">>),
        {ok, _ID4} = ln_receipt_log:append(Handle, <<"data4">>),
        {ok, _ID5} = ln_receipt_log:append(Handle, <<"data5">>),

        % Read range from 1-3
        Receipts = ln_receipt_log:range(Handle, ID1, 3, 100),
        ?assertEqual(3, length(Receipts))
    after
        case ets:whereis(receipt_log_ets) of
            undefined -> ok;
            CleanupTid2 -> ets:delete(CleanupTid2)
        end,
        file:delete(TempFile)
    end.

%% ====================================================================
%% Receipt Builder Tests
%% ====================================================================

determinism_same_inputs_test() ->
    OntologyFile = "/tmp/ontology_" ++ erlang:ref_to_list(make_ref()) ++ ".rdf",
    TemplatesFile = "/tmp/templates_" ++ erlang:ref_to_list(make_ref()) ++ ".txt",
    ArtifactFile = "/tmp/artifact_" ++ erlang:ref_to_list(make_ref()) ++ ".bin",

    file:write_file(OntologyFile, <<"<?xml version=\"1.0\"?><rdf></rdf>">>),
    file:write_file(TemplatesFile, <<"template1\ntemplate2">>),
    file:write_file(ArtifactFile, <<"generated output">>),

    try
        % First build
        {ok, Handle1} = ln_receipt_builder:start_build(OntologyFile, TemplatesFile),
        Handle2 = ln_receipt_builder:add_input(Handle1, param1, value1),
        Hash1 = ln_receipt_builder:compute_hash(Handle2),
        {ok, Receipt1} = ln_receipt_builder:issue(Handle2, [ArtifactFile], logger),
        InputHash1 = maps:get(input_hash, Receipt1),
        OutputHash1 = maps:get(output_hash, Receipt1),

        % Second build with same inputs
        {ok, Handle3} = ln_receipt_builder:start_build(OntologyFile, TemplatesFile),
        Handle4 = ln_receipt_builder:add_input(Handle3, param1, value1),
        Hash2 = ln_receipt_builder:compute_hash(Handle4),
        {ok, Receipt2} = ln_receipt_builder:issue(Handle4, [ArtifactFile], logger),
        InputHash2 = maps:get(input_hash, Receipt2),
        OutputHash2 = maps:get(output_hash, Receipt2),

        % Same input hash should produce same output hash
        ?assertEqual(Hash1, Hash2),
        ?assertEqual(InputHash1, InputHash2),
        ?assertEqual(OutputHash1, OutputHash2)
    after
        case ets:whereis(build_determinism_cache) of
            undefined -> ok;
            CleanupTidB -> ets:delete(CleanupTidB)
        end,
        file:delete(OntologyFile),
        file:delete(TemplatesFile),
        file:delete(ArtifactFile)
    end.

determinism_detect_change_test() ->
    OntologyFile = "/tmp/ontology_" ++ erlang:ref_to_list(make_ref()) ++ ".rdf",
    TemplatesFile = "/tmp/templates_" ++ erlang:ref_to_list(make_ref()) ++ ".txt",
    ArtifactFile1 = "/tmp/artifact1_" ++ erlang:ref_to_list(make_ref()) ++ ".bin",
    ArtifactFile2 = "/tmp/artifact2_" ++ erlang:ref_to_list(make_ref()) ++ ".bin",

    file:write_file(OntologyFile, <<"<?xml version=\"1.0\"?><rdf></rdf>">>),
    file:write_file(TemplatesFile, <<"template1\ntemplate2">>),

    try
        % First build - generate artifact with content v1
        {ok, Handle1} = ln_receipt_builder:start_build(OntologyFile, TemplatesFile),
        Handle2 = ln_receipt_builder:add_input(Handle1, param1, value1),
        file:write_file(ArtifactFile1, <<"output v1">>),
        {ok, Receipt1} = ln_receipt_builder:issue(Handle2, [ArtifactFile1], logger),
        Status1 = maps:get(status, Receipt1),

        % First time should succeed (no cache)
        ?assertEqual(success, Status1),

        % Simulate: same build, same inputs but artifact changed
        % Clear cache to simulate rebuilding with different output
        case ets:whereis(build_determinism_cache) of
            undefined -> ok;
            CacheTid -> ets:delete_all_objects(CacheTid)
        end,

        % Second build - same inputs, different artifact output
        {ok, Handle3} = ln_receipt_builder:start_build(OntologyFile, TemplatesFile),
        Handle4 = ln_receipt_builder:add_input(Handle3, param1, value1),
        file:write_file(ArtifactFile2, <<"output v2 - different">>),
        {ok, Receipt2} = ln_receipt_builder:issue(Handle4, [ArtifactFile2], logger),

        % Verify outputs differ
        OutputHash1 = maps:get(output_hash, Receipt1),
        OutputHash2 = maps:get(output_hash, Receipt2),
        ?assertNotEqual(OutputHash1, OutputHash2)
    after
        case ets:whereis(build_determinism_cache) of
            undefined -> ok;
            CleanupTidB -> ets:delete(CleanupTidB)
        end,
        file:delete(OntologyFile),
        file:delete(TemplatesFile),
        file:delete(ArtifactFile1),
        file:delete(ArtifactFile2)
    end.

input_hash_consistent_test() ->
    OntologyFile = "/tmp/ontology_" ++ erlang:ref_to_list(make_ref()) ++ ".rdf",
    TemplatesFile = "/tmp/templates_" ++ erlang:ref_to_list(make_ref()) ++ ".txt",

    file:write_file(OntologyFile, <<"<?xml version=\"1.0\"?><rdf></rdf>">>),
    file:write_file(TemplatesFile, <<"template1\ntemplate2">>),

    try
        % Build same state twice
        {ok, Handle1} = ln_receipt_builder:start_build(OntologyFile, TemplatesFile),
        H1 = ln_receipt_builder:add_input(Handle1, key1, val1),
        H2 = ln_receipt_builder:add_input(H1, key2, val2),
        Hash1 = ln_receipt_builder:compute_hash(H2),

        {ok, Handle3} = ln_receipt_builder:start_build(OntologyFile, TemplatesFile),
        H4 = ln_receipt_builder:add_input(Handle3, key1, val1),
        H5 = ln_receipt_builder:add_input(H4, key2, val2),
        Hash2 = ln_receipt_builder:compute_hash(H5),

        ?assertEqual(Hash1, Hash2)
    after
        case ets:whereis(build_determinism_cache) of
            undefined -> ok;
            CleanupTidB -> ets:delete(CleanupTidB)
        end,
        file:delete(OntologyFile),
        file:delete(TemplatesFile)
    end.

%% ====================================================================
%% Receipt Effect Tests
%% ====================================================================

idempotent_effect_test() ->
    case ets:whereis(effect_idempotency_cache) of
        undefined ->
            ets:new(effect_idempotency_cache, [named_table, {keypos, 1}, ordered_set]);
        _Tid ->
            ok
    end,

    try
        % Start effect
        {ok, Effect1} = ln_receipt_effect:start_effect(effect_123, connector_http, #{url => <<"http://api">>}),

        % Complete effect
        Result1 = <<"success_data">>,
        {ok, Receipt1} = ln_receipt_effect:complete(Effect1, Result1, 100),
        EffectID = maps:get(effect_id, Receipt1),
        InputHash = maps:get(input_hash, Receipt1),

        % Check idempotency cache - should find cached result
        {ok, CachedReceipt} = ln_receipt_effect:idempotent_receipt(EffectID, InputHash),
        CachedResult = maps:get(result, CachedReceipt),

        % Verify cached result matches
        ?assertEqual(Result1, CachedResult)
    after
        case ets:whereis(effect_idempotency_cache) of
            undefined -> ok;
            CleanupTidE -> ets:delete(CleanupTidE)
        end
    end.

effect_latency_test() ->
    case ets:whereis(effect_idempotency_cache) of
        undefined ->
            ets:new(effect_idempotency_cache, [named_table, {keypos, 1}, ordered_set]);
        _Tid ->
            ok
    end,

    try
        {ok, Effect} = ln_receipt_effect:start_effect(effect_456, connector_db, #{query => <<"SELECT *">>}),
        Result = <<"query_result">>,
        Latency = 250,
        {ok, Receipt} = ln_receipt_effect:complete(Effect, Result, Latency),

        % Verify latency recorded
        RecordedLatency = maps:get(latency_ms, Receipt),
        ?assertEqual(Latency, RecordedLatency),

        % Verify timestamps
        StartTime = maps:get(start_time, Receipt),
        EndTime = maps:get(end_time, Receipt),
        ?assert(EndTime >= StartTime)
    after
        case ets:whereis(effect_idempotency_cache) of
            undefined -> ok;
            CleanupTidE -> ets:delete(CleanupTidE)
        end
    end.

effect_failure_test() ->
    case ets:whereis(effect_idempotency_cache) of
        undefined ->
            ets:new(effect_idempotency_cache, [named_table, {keypos, 1}, ordered_set]);
        _Tid ->
            ok
    end,

    try
        {ok, Effect} = ln_receipt_effect:start_effect(effect_789, connector_api, #{endpoint => <<"api.example.com">>}),
        Error = {error, timeout, [stack_frame_1, stack_frame_2]},
        Latency = 5000,
        {ok, Receipt} = ln_receipt_effect:failed(Effect, Error, Latency),

        % Verify failure recorded
        Status = maps:get(status, Receipt),
        ?assertEqual(failed, Status),

        % Verify error details
        ErrorDetails = maps:get(error, Receipt),
        ?assert(is_map(ErrorDetails))
    after
        case ets:whereis(effect_idempotency_cache) of
            undefined -> ok;
            CleanupTidE -> ets:delete(CleanupTidE)
        end
    end.

%% ====================================================================
%% Andon Status Tests
%% ====================================================================

andon_color_sequence_test() ->
    {ok, AndonHandle} = ln_receipt_andon:new_andon(),

    try
        % Green
        ok = ln_receipt_andon:set_green(AndonHandle),
        {green, _} = ln_receipt_andon:status(AndonHandle),

        % Yellow
        ok = ln_receipt_andon:set_yellow(AndonHandle, [warning1, warning2]),
        {yellow, Details1} = ln_receipt_andon:status(AndonHandle),
        Warnings = maps:get(warnings, Details1),
        ?assertEqual([warning1, warning2], Warnings),

        % Red
        ok = ln_receipt_andon:set_red(AndonHandle, budget_exceeded),
        {red, Details2} = ln_receipt_andon:status(AndonHandle),
        Reason = maps:get(halt_reason, Details2),
        ?assertEqual(budget_exceeded, Reason)
    after
        case ets:whereis(andon_ets) of
            undefined -> ok;
            CleanupTidA -> ets:delete(CleanupTidA)
        end,
        case ets:whereis(andon_http_registry) of
            undefined -> ok;
            CleanupTidA2 -> ets:delete(CleanupTidA2)
        end
    end.

andon_status_details_test() ->
    {ok, AndonHandle} = ln_receipt_andon:new_andon(),

    try
        % Set to yellow with warnings
        Warnings = [slo_latency_high, memory_warning],
        ok = ln_receipt_andon:set_yellow(AndonHandle, Warnings),

        {Color, Details} = ln_receipt_andon:status(AndonHandle),
        ?assertEqual(yellow, Color),
        ?assert(maps:is_key(timestamp, Details)),
        ?assert(maps:is_key(warnings, Details)),
        ?assertEqual(true, maps:get(recoverable, Details))
    after
        case ets:whereis(andon_ets) of
            undefined -> ok;
            CleanupTidA -> ets:delete(CleanupTidA)
        end,
        case ets:whereis(andon_http_registry) of
            undefined -> ok;
            CleanupTidA2 -> ets:delete(CleanupTidA2)
        end
    end.
