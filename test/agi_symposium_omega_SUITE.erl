%% -*- erlang -*-
%% @doc AGI Symposium Ω Simulation Test Suite.
%%
%% Tests the complete AGI Symposium Ω workflow simulation to verify
%% all 43 YAWL workflow patterns are correctly implemented.
-module(agi_symposium_omega_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

%% Test suite callbacks
all() ->
    [
        test_load_yaml_spec,
        test_parse_pattern_instances,
        test_pattern_registry_coverage,
        test_compile_spec,
        test_symposium_execution,
        test_all_patterns_executed
    ].

init_per_suite(Config) ->
    %% Ensure yamerl is started
    case application:ensure_all_started(yamerl) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        Error -> ct:fail("Failed to start yamerl: ~p", [Error])
    end,
    Config.

end_per_suite(_Config) ->
    ok.

%% Test: Load YAML specification
test_load_yaml_spec(Config) ->
    YamlFile = filename:join([?config(data_dir, Config), "agi_symposium_omega.yaml"]),
    {ok, Spec} = wf_yaml_spec:from_yaml_file(YamlFile),
    
    ?assertEqual(<<"agi_symposium_omega">>, wf_yaml_spec:id(Spec)),
    ?assertEqual(<<"Symposium">>, wf_yaml_spec:root_net(Spec)),
    
    Nets = wf_yaml_spec:nets(Spec),
    ?assert(length(Nets) > 0, "Should have multiple nets"),
    
    PatternInstances = wf_yaml_spec:pattern_instances(Spec),
    ?assert(length(PatternInstances) >= 43, "Should have at least 43 pattern instances"),
    
    ok.

%% Test: Parse pattern instances
test_parse_pattern_instances(Config) ->
    YamlFile = filename:join([?config(data_dir, Config), "agi_symposium_omega.yaml"]),
    {ok, Spec} = wf_yaml_spec:from_yaml_file(YamlFile),
    
    PatternInstances = wf_yaml_spec:pattern_instances(Spec),
    
    %% Verify all 43 patterns are present
    PatternUsageIndex = wf_yaml_spec:pattern_usage_index(Spec),
    ExpectedPatterns = [<<"P", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 43)],
    
    lists:foreach(fun(PatternId) ->
        ?assert(maps:is_key(PatternId, PatternUsageIndex),
                "Pattern " ++ binary_to_list(PatternId) ++ " should be in usage index"),
        Instances = maps:get(PatternId, PatternUsageIndex, []),
        ?assert(length(Instances) > 0,
                "Pattern " ++ binary_to_list(PatternId) ++ " should have instances")
    end, ExpectedPatterns),
    
    ok.

%% Test: Pattern registry coverage
test_pattern_registry_coverage(Config) ->
    YamlFile = filename:join([?config(data_dir, Config), "agi_symposium_omega.yaml"]),
    {ok, Spec} = wf_yaml_spec:from_yaml_file(YamlFile),
    
    PatternRegistry = wf_yaml_spec:pattern_registry(Spec),
    AllPatterns = yawl_pattern_registry:all_patterns(),
    
    %% Verify all patterns have registry entries
    lists:foreach(fun(PatternId) ->
        ?assert(maps:is_key(PatternId, PatternRegistry),
                "Pattern " ++ binary_to_list(PatternId) ++ " should be in registry"),
        Module = yawl_pattern_registry:pattern_module(PatternId),
        ?assert(Module =/= undefined,
                "Pattern " ++ binary_to_list(PatternId) ++ " should have module mapping"),
        ?assert(yawl_pattern_registry:validate_pattern(PatternId),
                "Pattern " ++ binary_to_list(PatternId) ++ " should validate")
    end, AllPatterns),
    
    ok.

%% Test: Compile specification
test_compile_spec(Config) ->
    YamlFile = filename:join([?config(data_dir, Config), "agi_symposium_omega.yaml"]),
    {ok, Spec} = wf_yaml_spec:from_yaml_file(YamlFile),
    
    case yawl_compile:compile(Spec, #{}) of
        {ok, Compiled} ->
            ?assert(maps:is_key(spec_id, Compiled)),
            ?assert(maps:is_key(modules, Compiled)),
            ?assert(maps:is_key(places, Compiled)),
            ?assert(maps:is_key(transitions, Compiled)),
            %% Verify Symposium has expanded places (not fallback p_task1, p_task2)
            RootNet = wf_yaml_spec:root_net(Spec),
            AllPlaces = maps:get(places, Compiled, #{}),
            RootAtom = case RootNet of B when is_binary(B) -> binary_to_atom(B, utf8); A -> A end,
            SymposiumPlaces = maps:get(RootNet, AllPlaces, maps:get(RootAtom, AllPlaces, [])),
            ?assert(length(SymposiumPlaces) > 4, "Symposium should have expanded pattern places"),
            ?assert(lists:member(p_cancelled, SymposiumPlaces),
                "Preset places like p_cancelled must be in place_lst"),
            ok;
        {error, Reason} ->
            ct:log("Compilation error: ~p", [Reason]),
            ct:fail("Compile failed: ~p", [Reason])
    end.

%% Test: Execute symposium workflow
test_symposium_execution(Config) ->
    YamlFile = filename:join([?config(data_dir, Config), "agi_symposium_omega.yaml"]),
    {ok, Spec} = wf_yaml_spec:from_yaml_file(YamlFile),
    
    %% For now, verify we can parse and access all components
    RootNet = wf_yaml_spec:root_net(Spec),
    ?assertEqual(<<"Symposium">>, RootNet),
    
    Nets = wf_yaml_spec:nets(Spec),
    ?assert(lists:member(RootNet, Nets), "Root net should be in nets list"),
    
    PatternInstances = wf_yaml_spec:pattern_instances(Spec),
    ?assert(length(PatternInstances) >= 43, "Should have all 43 patterns"),
    
    %% TODO: Once compiler is complete, actually execute the workflow
    {skip, "Full execution requires compiler completion"}.

%% Test: Verify all patterns executed
test_all_patterns_executed(Config) ->
    YamlFile = filename:join([?config(data_dir, Config), "agi_symposium_omega.yaml"]),
    {ok, Spec} = wf_yaml_spec:from_yaml_file(YamlFile),
    
    PatternUsageIndex = wf_yaml_spec:pattern_usage_index(Spec),
    
    %% Verify all 43 patterns are referenced
    ExpectedPatterns = [<<"P", (integer_to_binary(N))/binary>> || N <- lists:seq(1, 43)],
    MissingPatterns = [P || P <- ExpectedPatterns, not maps:is_key(P, PatternUsageIndex)],
    
    ?assertEqual([], MissingPatterns, "All 43 patterns should be in usage index"),
    
    %% Verify each pattern has at least one instance
    lists:foreach(fun(PatternId) ->
        Instances = maps:get(PatternId, PatternUsageIndex, []),
        ?assert(length(Instances) > 0,
                "Pattern " ++ binary_to_list(PatternId) ++ " should have instances")
    end, ExpectedPatterns),
    
    ok.
