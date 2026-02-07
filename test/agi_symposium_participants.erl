%% -*- erlang -*-
%% @doc AGI Symposium Ω participant agents.
%%
%% Simulates role behavior using existing wf_engine and yawl_execution APIs only.
%% No new endpoints or APIs.
-module(agi_symposium_participants).
-export([
    agi_symposium_spec/0,
    chair_agent/2,
    program_chair_agent/3,
    reviewer_agent/3,
    ops_lead_agent/3,
    venue_lead_agent/3,
    press_lead_agent/3,
    safety_officer_agent/3
]).

%%====================================================================
%% Spec Helpers
%%====================================================================

%% @doc Returns minimal AGI Symposium spec for wf_engine (GoNoGo human task).
%% Uses existing wf_engine transition format with completion_produce.
-spec agi_symposium_spec() -> map().
agi_symposium_spec() ->
    #{
        places => [p_start, p_task, p_end],
        transitions => #{
            t_go_nogo => #{
                preset => [p_start],
                completion_produce => #{p_end => [done]},
                is_task => true,
                task_place => p_task
            }
        },
        start_token => start,
        end_place => p_end
    }.

%%====================================================================
%% Work Item Discovery (via case_log - existing API)
%%====================================================================

%% @doc Extracts offered work item IDs from case log.
%% Uses wf_engine:case_log/2 - existing API.
-spec offered_workitems(wf_engine:case_id(), pid()) -> [binary()].
offered_workitems(CaseId, Engine) ->
    case wf_engine:case_log(Engine, CaseId) of
        {error, _} ->
            [];
        Log ->
            [WiId || {task_created, _Trsn, WiId, _Now} <- Log]
    end.

%%====================================================================
%% Chair Agent
%%====================================================================

%% @doc Chair coordination: starts workflow, monitors cases, suspend/resume.
-spec chair_agent(pid(), map()) -> {ok, binary()} | {error, term()}.
chair_agent(Engine, _State) ->
    Now = erlang:monotonic_time(millisecond),
    wf_engine:start_case(Engine, #{data => #{}}, Now).

%%====================================================================
%% Program Chair Agent
%%====================================================================

%% @doc Program thread: polls worklist, allocates, completes tasks.
-spec program_chair_agent(pid(), binary(), map()) -> ok | {error, term()}.
program_chair_agent(Engine, CaseId, State) ->
    Now = erlang:monotonic_time(millisecond),
    User = <<"ProgramChair">>,
    Data = decision_data(<<"ProgramChair">>, State),
    case offered_workitems(CaseId, Engine) of
        [] ->
            case wf_engine:worklist(Engine, User) of
                [] -> ok;
                [WI | _] ->
                    complete_work_item(Engine, WI, User, Data, Now)
            end;
        [WiId | _] ->
            case wf_engine:allocate(Engine, WiId, User, Now) of
                ok ->
                    case wf_engine:start_work(Engine, WiId, User, Now) of
                        ok ->
                            wf_engine:complete(Engine, WiId, User, Data, Now);
                        Err -> Err
                    end;
                Err -> Err
            end
    end.

%%====================================================================
%% Reviewer Agent
%%====================================================================

%% @doc Reviewer: completes review tasks.
-spec reviewer_agent(pid(), binary(), map()) -> ok | {error, term()}.
reviewer_agent(Engine, CaseId, State) ->
    Now = erlang:monotonic_time(millisecond),
    User = <<"Reviewer">>,
    Data = decision_data(<<"Reviewer">>, State),
    case offered_workitems(CaseId, Engine) of
        [] ->
            case wf_engine:worklist(Engine, User) of
                [] -> ok;
                [WI | _] ->
                    complete_work_item(Engine, WI, User, Data, Now)
            end;
        [WiId | _] ->
            case wf_engine:allocate(Engine, WiId, User, Now) of
                ok ->
                    case wf_engine:start_work(Engine, WiId, User, Now) of
                        ok ->
                            wf_engine:complete(Engine, WiId, User, Data, Now);
                        Err -> Err
                    end;
                Err -> Err
            end
    end.

%%====================================================================
%% Ops Lead Agent
%%====================================================================

%% @doc Operations: uses same task completion pattern.
-spec ops_lead_agent(pid(), binary(), map()) -> ok | {error, term()}.
ops_lead_agent(Engine, CaseId, State) ->
    Now = erlang:monotonic_time(millisecond),
    User = <<"OpsLead">>,
    run_task_agent(Engine, CaseId, User, State, Now).

%%====================================================================
%% Venue Lead Agent
%%====================================================================

%% @doc Venue selection: uses same task completion pattern.
-spec venue_lead_agent(pid(), binary(), map()) -> ok | {error, term()}.
venue_lead_agent(Engine, CaseId, State) ->
    Now = erlang:monotonic_time(millisecond),
    User = <<"VenueLead">>,
    run_task_agent(Engine, CaseId, User, State, Now).

%%====================================================================
%% Press Lead Agent
%%====================================================================

%% @doc Communications: uses same task completion pattern.
-spec press_lead_agent(pid(), binary(), map()) -> ok | {error, term()}.
press_lead_agent(Engine, CaseId, State) ->
    Now = erlang:monotonic_time(millisecond),
    User = <<"PressLead">>,
    run_task_agent(Engine, CaseId, User, State, Now).

%%====================================================================
%% Safety Officer Agent
%%====================================================================

%% @doc Incident handling: can cancel case via wf_engine:cancel_case/3.
-spec safety_officer_agent(pid(), binary(), map()) -> ok | {error, term()}.
safety_officer_agent(Engine, CaseId, State) ->
    case maps:get(emergency, State, false) of
        true ->
            Now = erlang:monotonic_time(millisecond),
            wf_engine:cancel_case(Engine, CaseId, Now);
        false ->
            Now = erlang:monotonic_time(millisecond),
            User = <<"SafetyOfficer">>,
            run_task_agent(Engine, CaseId, User, State, Now)
    end.

%%====================================================================
%% Internal
%%====================================================================

%% @doc Gets decision data from Z.AI when zai_enabled, else default.
-spec decision_data(binary(), map()) -> map().
decision_data(Role, State) ->
    case maps:get(zai_enabled, State, false) of
        true ->
            case zai_decision(Role, State) of
                {ok, Data} -> Data;
                _ -> #{decision => accept}
            end;
        _ ->
            #{decision => accept}
    end.

%% @doc Calls Z.AI for role-specific decision; returns map for complete/5.
-spec zai_decision(binary(), map()) -> {ok, map()} | {error, term()}.
zai_decision(Role, State) ->
    case zai_client:get_api_key() of
        undefined -> {error, api_key_not_configured};
        _ ->
            Prompt = role_prompt(Role),
            Messages = [#{role => <<"user">>, content => Prompt}],
            Opts = #{
                model => maps:get(model, State, <<"glm-4-plus">>),
                temperature => 0.3,
                max_tokens => 128
            },
            case zai_client:chat_json(Messages, Opts) of
                {ok, Json} ->
                    case maps:get(<<"decision">>, Json, <<"accept">>) of
                        <<"reject">> -> {ok, #{decision => reject}};
                        _ -> {ok, #{decision => accept}}
                    end;
                Err -> Err
            end
    end.

role_prompt(<<"ProgramChair">>) ->
    <<"You are Program Chair. Task: Go/No-Go decision. Reply JSON: {\"decision\":\"accept\" or \"reject\",\"reason\":\"brief reason\"}">>;
role_prompt(<<"Reviewer">>) ->
    <<"You are Reviewer. Evaluate submission. Reply JSON: {\"decision\":\"accept\" or \"reject\",\"score\":1-10}">>;
role_prompt(<<"OpsLead">>) ->
    <<"You are Ops Lead. Task complete. Reply JSON: {\"decision\":\"accept\",\"done\":true}">>;
role_prompt(<<"VenueLead">>) ->
    <<"You are Venue Lead. Venue selected. Reply JSON: {\"decision\":\"accept\"}">>;
role_prompt(<<"PressLead">>) ->
    <<"You are Press Lead. Communications ready. Reply JSON: {\"decision\":\"accept\"}">>;
role_prompt(<<"SafetyOfficer">>) ->
    <<"You are Safety Officer. All clear. Reply JSON: {\"decision\":\"accept\"}">>;
role_prompt(_) ->
    <<"Reply JSON: {\"decision\":\"accept\"}">>.

run_task_agent(Engine, CaseId, User, State, Now) ->
    Data = decision_data(User, State),
    case offered_workitems(CaseId, Engine) of
        [] ->
            case wf_engine:worklist(Engine, User) of
                [] -> ok;
                [WI | _] ->
                    complete_work_item(Engine, WI, User, Data, Now)
            end;
        [WiId | _] ->
            case wf_engine:allocate(Engine, WiId, User, Now) of
                ok ->
                    case wf_engine:start_work(Engine, WiId, User, Now) of
                        ok ->
                            wf_engine:complete(Engine, WiId, User, Data, Now);
                        Err -> Err
                    end;
                Err -> Err
            end
    end.

complete_work_item(Engine, WI, User, Data, Now) ->
    WiId = element(2, WI),
    wf_engine:complete(Engine, WiId, User, Data, Now).
