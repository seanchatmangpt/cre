%% -*- erlang -*-
%% @doc Run AGI Symposium demo (used by run_agi_symposium_demo.sh).
-module(demo_runner).
-export([main/0]).

main() ->
    ok = test_helper:ensure_cre_gen_pnet_loaded(),
    case start_otel_logger() of
        ok ->
            run_demo();
        {error, _} = Err ->
            Err
    end.

start_otel_logger() ->
    case whereis(yawl_otel_logger) of
        undefined ->
            case yawl_otel_logger:start_link(#{}) of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                {error, E} ->
                    io:format(standard_error, "ERROR: yawl_otel_logger start failed: ~p~n", [E]),
                    {error, E}
            end;
        _ ->
            ok
    end.

run_demo() ->
    case os:getenv("DEMO_OMEGA") of
        "1" ->
            omega_demo_runner:run(),
            init:stop(0);
        _ ->
            run_standard_demo()
    end.

run_standard_demo() ->
    agi_symposium_otel:print_banner(),
    Spec = agi_symposium_participants:agi_symposium_spec_full(),
    WithDashboard = os:getenv("DEMO_DASHBOARD") =/= false andalso os:getenv("DEMO_DASHBOARD") =/= "",
    Opts = case os:getenv("ZAI_API_KEY") of
        false -> #{zai_enabled => false};
        "" -> #{zai_enabled => false};
        _ -> #{zai_enabled => true, model => zai_client:get_model()}
    end,
    RunOpts = maps:merge(Opts, #{live_stream => true, with_dashboard => WithDashboard}),
    case maybe_start_dashboard(RunOpts) of
        ok ->
            case agi_symposium_simulator:run(Spec, RunOpts) of
                {ok, Result} ->
                    agi_symposium_otel:print_otel_script(Result),
                    maybe_print_dashboard_url(RunOpts),
                    io:format(standard_error, "~nDemo complete.~n", []),
                    init:stop(0);
                {error, Reason} ->
                    io:format(standard_error, "ERROR: Symposium failed: ~p~n", [Reason]),
                    init:stop(1)
            end;
        {error, _} = Err ->
            Err
    end.

maybe_start_dashboard(Opts) ->
    case maps:get(with_dashboard, Opts, false) of
        true ->
            _ = application:ensure_all_started(cre),
            case yawl_web_dashboard:start_link(#{port => 8081}) of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                {error, E} ->
                    io:format(standard_error, "WARNING: Dashboard start failed: ~p~n", [E]),
                    ok
            end;
        _ ->
            ok
    end.

maybe_print_dashboard_url(Opts) ->
    case maps:get(with_dashboard, Opts, false) of
        true ->
            io:format(standard_error, "~nView trace: http://localhost:8081~n", []);
        _ ->
            ok
    end.
