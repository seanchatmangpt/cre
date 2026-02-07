%% -*- erlang -*-
%% @doc AGI Symposium Ω simulation orchestrator.
%%
%% Loads spec, starts workflow, spawns participants, coordinates execution.
%% Uses only existing APIs: wf_engine, wf_yaml_spec, yawl_compile.
-module(agi_symposium_simulator).
-export([run/0, run/1, run/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc Run simulation with default AGI Symposium spec.
-spec run() -> {ok, map()} | {error, term()}.
run() ->
    run(agi_symposium_participants:agi_symposium_spec()).

%% @doc Run simulation with given spec (no options).
-spec run(Spec :: map()) -> {ok, map()} | {error, term()}.
run(Spec) when is_map(Spec) ->
    run(Spec, #{}).

%% @doc Run simulation with spec and options.
%% Options: #{zai_enabled => true, model => <<"glm-4-plus">>}
-spec run(Spec :: map(), Options :: map()) -> {ok, map()} | {error, term()}.
run(Spec, Options) when is_map(Spec), is_map(Options) ->
    ok = test_helper:ensure_cre_gen_pnet_loaded(),
    OTelOk = case whereis(yawl_otel_logger) of
        undefined ->
            case yawl_otel_logger:start_link(#{}) of
                {ok, _} -> ok;
                {error, {already_started, _}} -> ok;
                {error, E} ->
                    error_logger:error_msg("yawl_otel_logger start failed: ~p~n", [E]),
                    {error, {otel_logger_failed, E}}
            end;
        _ -> ok
    end,
    case OTelOk of
        ok ->
            Now = erlang:monotonic_time(millisecond),
            case wf_engine:start_link(#{spec => Spec, seed => 1, now => Now}) of
                {ok, Engine} ->
                    try
                        run_simulation(Engine, Spec, Options, Now)
                    after
                        gen_server:stop(Engine)
                    end;
                {error, Reason} ->
                    {error, {engine_start_failed, Reason}}
            end;
        {error, _} = Err ->
            Err
    end.

%%====================================================================
%% Internal
%%====================================================================

run_simulation(Engine, _Spec, Options, Now) ->
    case wf_engine:start_case(Engine, #{data => #{}}, Now) of
        {ok, CaseId} ->
            agi_symposium_otel:log_workflow_start(CaseId, <<"agi_symposium_full">>),
            State = participant_state(Options, CaseId),
            Result = run_participants(Engine, CaseId, State, Now, 0, 50),
            CaseState = wf_engine:case_state(Engine, CaseId),
            agi_symposium_otel:log_workflow_complete(CaseId, CaseState),
            {ok, #{
                case_id => CaseId,
                status => CaseState,
                participant_rounds => Result,
                receipts => wf_engine:drain_receipts(Engine, CaseId)
            }};
        {error, Reason} ->
            {error, {start_case_failed, Reason}}
    end.

participant_state(Options, CaseId) ->
    Base = #{case_id => CaseId},
    WithZai = case maps:get(zai_enabled, Options, false) of
        true ->
            maps:merge(Base, #{
                zai_enabled => true,
                model => maps:get(model, Options, <<"glm-4-plus">>)
            });
        _ ->
            Base
    end,
    case maps:get(live_stream, Options, false) of
        true ->
            maps:put(on_llm_vote, fun(Role, Dec, Reason) ->
                RoleStr = agi_symposium_otel:format_role_display(Role),
                DecStr = case Dec of <<"reject">> -> agi_symposium_ansi:red("REJECT"); _ -> agi_symposium_ansi:green("ACCEPT") end,
                ReasonStr = case Reason of <<>> -> ""; _ -> " \"" ++ binary_to_list(Reason) ++ "\"" end,
                io:format(standard_error, "  ~s: ~s —~s~n", [agi_symposium_ansi:cyan(RoleStr), DecStr, ReasonStr])
            end, WithZai);
        _ ->
            WithZai
    end.

run_participants(Engine, CaseId, State, Now, Round, MaxRounds) when Round < MaxRounds ->
    Participants = [
        program_chair_agent,
        reviewer_agent,
        ops_lead_agent,
        venue_lead_agent,
        press_lead_agent
    ],
    Parent = self(),
    Refs = [make_ref() || _ <- Participants],
    lists:foreach(
        fun({AgentMod, Ref}) ->
            spawn(fun() ->
                _ = apply(agi_symposium_participants, AgentMod, [Engine, CaseId, State]),
                Parent ! {done, Ref}
            end)
        end,
        lists:zip(Participants, Refs)
    ),
    lists:foreach(fun(Ref) -> receive {done, Ref} -> ok end end, Refs),
    case wf_engine:case_state(Engine, CaseId) of
        completed ->
            Round + 1;
        _ ->
            run_participants(Engine, CaseId, State, Now + 1, Round + 1, MaxRounds)
    end;
run_participants(_Engine, _CaseId, _State, _Now, Round, _MaxRounds) ->
    Round.
