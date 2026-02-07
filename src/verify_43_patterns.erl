%% -*- erlang -*-
%% @doc Verify all 43 patterns are implemented and registered.
-module(verify_43_patterns).
-export([run/0]).

run() ->
    io:format("=== 43 YAWL Patterns Verification ===~n~n"),
    
    %% Step 1: Verify pattern registry
    io:format("Step 1: Verifying pattern registry...~n"),
    AllPatterns = yawl_pattern_registry:all_patterns(),
    io:format("  ✓ Found ~p patterns in registry~n", [length(AllPatterns)]),
    
    MissingModules = lists:foldl(fun(PatternId, Acc) ->
        Module = yawl_pattern_registry:pattern_module(PatternId),
        case Module of
            undefined ->
                [{PatternId, undefined} | Acc];
            _ ->
                case code:ensure_loaded(Module) of
                    {module, Module} ->
                        %% Verify it's a gen_yawl behavior
                        case lists:member(gen_yawl, Module:module_info(attributes)) of
                            true -> Acc;
                            false -> [{PatternId, Module, not_gen_yawl} | Acc]
                        end;
                    _ ->
                        [{PatternId, Module, not_loaded} | Acc]
                end
        end
    end, [], AllPatterns),
    
    case MissingModules of
        [] ->
            io:format("  ✓ All patterns have valid modules~n");
        _ ->
            io:format("  ✗ Issues found:~n"),
            lists:foreach(fun({P, M, Reason}) ->
                io:format("    - ~s: ~p (~p)~n", [P, M, Reason])
            end, MissingModules)
    end,
    io:format("~n"),
    
    %% Step 2: Verify pattern functions
    io:format("Step 2: Verifying pattern module functions...~n"),
    FunctionsOk = lists:foldl(fun(PatternId, Acc) ->
        Module = yawl_pattern_registry:pattern_module(PatternId),
        case Module of
            undefined -> Acc;
            _ ->
                case code:ensure_loaded(Module) of
                    {module, Module} ->
                        Required = [place_lst, trsn_lst, preset, is_enabled, fire],
                        Missing = [F || F <- Required, not erlang:function_exported(Module, F, 0) andalso not erlang:function_exported(Module, F, 1) andalso not erlang:function_exported(Module, F, 3)],
                        case Missing of
                            [] -> Acc;
                            _ -> [{PatternId, Module, Missing} | Acc]
                        end;
                    _ -> Acc
                end
        end
    end, [], AllPatterns),
    
    case FunctionsOk of
        [] ->
            io:format("  ✓ All pattern modules have required functions~n");
        _ ->
            io:format("  ✗ Missing functions:~n"),
            lists:foreach(fun({P, M, Fs}) ->
                io:format("    - ~s (~s): missing ~p~n", [P, M, Fs])
            end, FunctionsOk)
    end,
    io:format("~n"),
    
    %% Step 3: Summary
    io:format("=== Verification Summary ===~n"),
    io:format("✓ Pattern Registry: ~p patterns~n", [length(AllPatterns)]),
    io:format("✓ Modules Loaded: ~p/~p~n", [length(AllPatterns) - length(MissingModules), length(AllPatterns)]),
    io:format("✓ Functions Verified: ~p/~p~n", [length(AllPatterns) - length(FunctionsOk), length(AllPatterns)]),
    io:format("~n"),
    
    case MissingModules =:= [] andalso FunctionsOk =:= [] of
        true ->
            io:format("🎉 SUCCESS: All 43 patterns verified!~n"),
            ok;
        false ->
            io:format("⚠ WARNINGS: Some patterns need attention~n"),
            {warning, {MissingModules, FunctionsOk}}
    end.
