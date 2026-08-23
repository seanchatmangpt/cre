-module(debug_test).
-include_lib("eunit/include/eunit.hrl").

simple_test_() ->
    [
        {"Test server start", 
            fun() ->
                io:format("Starting server...~n"),
                case predictive_mining:start_link() of
                    {ok, Pid} ->
                        io:format("Server started with pid: ~p~n", [Pid]),
                        ?assert(is_pid(Pid)),
                        predictive_mining:stop();
                    {error, Reason} ->
                        io:format("Failed to start server: ~p~n", [Reason]),
                        ?assert(false)
                end
            end}
    ].
