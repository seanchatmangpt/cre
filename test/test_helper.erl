%% @doc Test helper for EUnit. Exports setup/cleanup for eunit_tests.
%% Ensures ranch, cowboy, and cre applications are started so doctests
%% that depend on HTTP handlers (cre, cre_history_handler, cre_status_handler) pass.
%%
%% Ensures cre's gen_pnet (with inject/step/drain) is loaded before the dep's version,
%% which lacks these functions and causes {undef, gen_pnet:inject} in CT.
-module(test_helper).
-export([setup/0, cleanup/1, ensure_cre_gen_pnet_loaded/0]).

setup() ->
    ok = ensure_cre_gen_pnet_loaded(),
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

%% @doc Load cre's gen_pnet (extended with inject/step/drain) so it overrides
%% the dependency's version. Call before starting cre.
-spec ensure_cre_gen_pnet_loaded() -> ok.
ensure_cre_gen_pnet_loaded() ->
    cre:ensure_cre_gen_pnet_loaded().
