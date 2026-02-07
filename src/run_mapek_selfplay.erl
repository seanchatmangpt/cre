%% -*- erlang -*-
%% @doc CLI entry for MAPE-K self-play workflow creation.
%%
%% LLM generates workflows → execute → monitor → analyze → plan → iterate.
%% Requires ZAI_API_KEY.
%%
%% Usage: rebar3 shell -eval "run_mapek_selfplay:main([]), halt(0)."
%%        ZAI_API_KEY=key ./scripts/run_mapek_selfplay.sh
%% @end
-module(run_mapek_selfplay).
-export([main/1]).

main(Args) ->
    try
        do_main(Args)
    catch
        Type:Reason:Stack ->
            io:format("ERROR: ~p:~p~n~p~n", [Type, Reason, Stack]),
            halt(1)
    end.

do_main(Args) ->
    io:format("=== MAPE-K Self-Play Workflow Creation ===~n~n"),
    Goal = case Args of
        [G | _] when is_list(G) -> unicode:characters_to_binary(G);
        _ -> case os:getenv("MAPEK_GOAL") of
            false -> <<"Create a simple workflow with 2-3 tasks">>;
            Env -> unicode:characters_to_binary(Env)
        end
    end,
    MaxIter = list_to_integer(os:getenv("MAPEK_MAX_ITER", "5")),
    Opts = #{
        goal => Goal,
        max_iterations => MaxIter,
        constraints => [
            <<"Use YAML 0.2 format">>,
            <<"One net with Start, End, 2-3 tasks">>
        ]
    },
    case zai_client:get_api_key() of
        undefined ->
            io:format("ERROR: ZAI_API_KEY not configured.~n"),
            halt(1);
        _ ->
            io:format("Goal: ~s~nMax iterations: ~p~nModel: ~s~n~n", [Goal, MaxIter, zai_client:get_model()]),
            case mapek_workflow_creator:run_selfplay(Opts) of
                {ok, K} ->
                    io:format("~n=== MAPE-K Self-Play Complete ===~n"),
                    io:format("Iterations: ~p~n", [maps:get(iterations, K, 0)]),
                    case maps:get(last_result, K, #{}) of
                        #{status := S, rounds := R} ->
                            io:format("Status: ~p, Rounds: ~p~n", [S, R]);
                        M -> io:format("Result: ~p~n", [M])
                    end,
                    ok;
                {error, R} ->
                    io:format("ERROR: ~p~n", [R]),
                    halt(1)
            end
    end.
