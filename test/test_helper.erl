%% @doc Test helper for EUnit. Exports setup/cleanup for eunit_tests.
%% Ensures ranch, cowboy, and cre applications are started so doctests
%% that depend on HTTP handlers (cre, cre_history_handler, cre_status_handler) pass.
-module(test_helper).
-export([setup/0, cleanup/1]).

setup() ->
    case application:ensure_all_started(cre) of
        {ok, _} -> ok;
        {error, {already_started, cre}} -> ok;
        {error, Reason} ->
            io:format(standard_error, "Warning: cre app start failed: ~p~n", [Reason]),
            ok
    end.

cleanup(_) ->
    %% Don't stop - some tests may still be running; let VM exit clean up
    ok.
