-module(test_persistence).
-export([run/0]).

run() ->
    io:format("Testing YAWL Persistence Module~n"),
    io:format("==============================~n~n"),
    
    %% Clean setup
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    timer:sleep(100),
    
    %% 1. Test init_schema
    io:format("1. init_schema... ", []),
    case yawl_persistence:init_schema() of
        ok -> io:format("PASS~n");
        E1 -> io:format("FAIL: ~p~n", [E1]), halt(1)
    end,
    
    %% 2. Test save_case
    io:format("2. save_case... ", []),
    CaseId = <<"test_case_001">>,
    CaseMap = #{case_id => CaseId, workflow_id => <<"test_wf">>, 
                 spec => #{}, status => running, data => #{key => val}},
    case yawl_persistence:save_case(CaseMap) of
        {ok, CaseId} -> io:format("PASS~n");
        E2 -> io:format("FAIL: ~p~n", [E2]), halt(1)
    end,
    
    %% 3. Test load_case
    io:format("3. load_case... ", []),
    case yawl_persistence:load_case(CaseId) of
        {ok, Loaded} -> 
            Status = maps:get(status, Loaded),
            Data = maps:get(data, Loaded),
            case {Status, Data} of
                {running, #{key := val}} -> io:format("PASS~n");
                _ -> io:format("FAIL: wrong data~n"), halt(1)
            end;
        E3 -> io:format("FAIL: ~p~n", [E3]), halt(1)
    end,
    
    %% 4. Test save_workitem
    io:format("4. save_workitem... ", []),
    WorkitemId = <<"wi001">>,
    WIMap = #{id => WorkitemId, case_id => CaseId, task_id => <<"t1">>, 
               status => enabled, data => #{}},
    case yawl_persistence:save_workitem(WIMap) of
        {ok, WorkitemId} -> io:format("PASS~n");
        E4 -> io:format("FAIL: ~p~n", [E4]), halt(1)
    end,
    
    %% 5. Test load_workitems
    io:format("5. load_workitems... ", []),
    case yawl_persistence:load_workitems(CaseId) of
        {ok, [WI|_]} -> 
            case maps:get(workitem_id, WI) of
                WorkitemId -> io:format("PASS~n");
                _ -> io:format("FAIL: wrong wi~n"), halt(1)
            end;
        E5 -> io:format("FAIL: ~p~n", [E5]), halt(1)
    end,
    
    %% 6. Test list_active_cases
    io:format("6. list_active_cases... ", []),
    case yawl_persistence:list_active_cases() of
        {ok, ActiveCases} when length(ActiveCases) >= 1 -> io:format("PASS (~p)~n", [length(ActiveCases)]);
        {ok, []} -> io:format("FAIL: empty~n"), halt(1);
        E6 -> io:format("FAIL: ~p~n", [E6]), halt(1)
    end,
    
    %% 7. Test get_case_count
    io:format("7. get_case_count... ", []),
    case yawl_persistence:get_case_count() of
        {ok, Count} when Count >= 1 -> io:format("PASS (~p)~n", [Count]);
        E7 -> io:format("FAIL: ~p~n", [E7]), halt(1)
    end,
    
    %% 8. Test update case
    io:format("8. update_case... ", []),
    UpdatedCase = maps:put(status, suspended, CaseMap),
    case yawl_persistence:save_case(UpdatedCase) of
        {ok, CaseId} -> 
            {ok, Loaded2} = yawl_persistence:load_case(CaseId),
            case maps:get(status, Loaded2) of
                suspended -> io:format("PASS~n");
                _ -> io:format("FAIL: not updated~n"), halt(1)
            end;
        E8 -> io:format("FAIL: ~p~n", [E8]), halt(1)
    end,
    
    %% 9. Test delete_case (cascade)
    io:format("9. delete_case (cascade)... ", []),
    case yawl_persistence:delete_case(CaseId) of
        ok -> 
            case yawl_persistence:load_workitems(CaseId) of
                {ok, []} -> io:format("PASS~n");
                _ -> io:format("FAIL: workitems remain~n"), halt(1)
            end;
        E9 -> io:format("FAIL: ~p~n", [E9]), halt(1)
    end,
    
    %% 10. Verify deletion
    io:format("10. verify deletion... ", []),
    case yawl_persistence:load_case(CaseId) of
        {error, not_found} -> io:format("PASS~n");
        _ -> io:format("FAIL: case exists~n"), halt(1)
    end,
    
    %% Cleanup
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    
    io:format("~nAll tests passed!~n"),
    init:stop().
