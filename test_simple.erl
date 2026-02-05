-module(test_simple).
-export([test/0]).

test() ->
    %% Check that workflow modules exist and have correct exports
    Modules = [ordering, payment, freight_in_transit, freight_delivered,
               carrier_appointment, order_fulfillment,
               cre_yawl, yawl_xes, cre_yawl_patterns],
    
    io:format("~n=== CRE YAWL Workflow Module Test ===~n~n"),
    
    lists:foreach(fun(Module) ->
        case code:is_loaded(Module) of
            true ->
                io:format("~s: already loaded~n", [Module]);
            false ->
                io:format("~s: loading... ", [Module]),
                case code:load_file(Module) of
                    {module, _} ->
                        io:format("OK~n");
                    {error, Reason} ->
                        io:format("FAILED: ~p~n", [Reason])
                end
        end
    end, Modules),
    
    %% Check gen_pnet behavior exports
    io:format("~n=== Checking gen_pnet Module ===~n"),
    case code:is_loaded(gen_pnet) of
        true -> io:format("gen_pnet: loaded~n");
        false -> io:format("gen_pnet: loading... "),
                 code:load_file(gen_pnet)
    end,
    
    io:format("~n=== Module Functions Check ===~n"),
    lists:foreach(fun(Module) ->
        Exports = Module:module_info(exports),
        io:format("~n~s exports:~n", [Module]),
        lists:foreach(fun({Func, Arity}) ->
            io:format("  ~p/~p~n", [Func, Arity])
        end, lists:filter(fun({_, A}) -> is_list(A); (_) -> true end, Exports))
    end, [ordering, payment, freight_in_transit]),
    
    io:format("~n=== Test Complete ===~n"),
    ok.
