%% -*- erlang -*-
%% @doc AGI Symposium Ω participant agents.
%%
%% Simulates role behavior using existing wf_engine and yawl_execution APIs only.
%% No new endpoints or APIs.
-module(agi_symposium_participants).
-include("wf_engine.hrl").
-export([
    agi_symposium_spec/0,
    agi_symposium_spec_full/0,
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

%% @doc Returns minimal AGI Symposium spec (single GoNoGo task).
-spec agi_symposium_spec() -> map().
agi_symposium_spec() ->
    agi_symposium_spec_minimal().

%% @doc Minimal spec: single GoNoGo task.
-spec agi_symposium_spec_minimal() -> map().
agi_symposium_spec_minimal() ->
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

%% @doc Full symposium spec: 5 sequential tasks (Program Chair, Reviewer, Ops, Venue, Press).
%% Each role gets exactly one task; LLMs simulate human decisions at each step.
-spec agi_symposium_spec_full() -> map().
agi_symposium_spec_full() ->
    #{
        places => [p_start, p_pc, p_rev, p_ops, p_venue, p_press, p_end],
        transitions => #{
            t_program_chair => #{
                preset => [p_start],
                completion_produce => #{p_pc => [done]},
                is_task => true,
                task_place => p_task
            },
            t_reviewer => #{
                preset => [p_pc],
                completion_produce => #{p_rev => [done]},
                is_task => true,
                task_place => p_task
            },
            t_ops_lead => #{
                preset => [p_rev],
                completion_produce => #{p_ops => [done]},
                is_task => true,
                task_place => p_task
            },
            t_venue_lead => #{
                preset => [p_ops],
                completion_produce => #{p_venue => [done]},
                is_task => true,
                task_place => p_task
            },
            t_press_lead => #{
                preset => [p_venue],
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

%% @doc Extracts offered work item IDs from case log (legacy, for minimal spec).
-spec offered_workitems(wf_engine:case_id(), pid()) -> [binary()].
offered_workitems(CaseId, Engine) ->
    case wf_engine:case_log(Engine, CaseId) of
        {error, _} -> [];
        Log -> [WiId || {task_created, _Trsn, WiId, _Now} <- Log]
    end.

%% @doc Returns WiId for offered work item matching Task, or undefined.
-spec offered_workitem_for_task(pid(), wf_engine:case_id(), atom()) -> binary() | undefined.
offered_workitem_for_task(Engine, CaseId, Task) ->
    Offered = wf_engine:offered_workitems(Engine, CaseId),
    case [WiId || {T, WiId} <- Offered, T =:= Task] of
        [WiId | _] -> WiId;
        [] -> undefined
    end.

%% @doc Role-to-task mapping for full symposium spec.
-spec role_to_task(binary()) -> atom() | undefined.
role_to_task(<<"ProgramChair">>) -> t_program_chair;
role_to_task(<<"Reviewer">>) -> t_reviewer;
role_to_task(<<"OpsLead">>) -> t_ops_lead;
role_to_task(<<"VenueLead">>) -> t_venue_lead;
role_to_task(<<"PressLead">>) -> t_press_lead;
role_to_task(_) -> undefined.

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
    run_role_agent(Engine, CaseId, <<"ProgramChair">>, State).

%%====================================================================
%% Reviewer Agent
%%====================================================================

%% @doc Reviewer: completes review tasks.
-spec reviewer_agent(pid(), binary(), map()) -> ok | {error, term()}.
reviewer_agent(Engine, CaseId, State) ->
    run_role_agent(Engine, CaseId, <<"Reviewer">>, State).

%%====================================================================
%% Ops Lead Agent
%%====================================================================

%% @doc Operations: uses same task completion pattern.
-spec ops_lead_agent(pid(), binary(), map()) -> ok | {error, term()}.
ops_lead_agent(Engine, CaseId, State) ->
    run_role_agent(Engine, CaseId, <<"OpsLead">>, State).

%%====================================================================
%% Venue Lead Agent
%%====================================================================

%% @doc Venue selection: uses same task completion pattern.
-spec venue_lead_agent(pid(), binary(), map()) -> ok | {error, term()}.
venue_lead_agent(Engine, CaseId, State) ->
    run_role_agent(Engine, CaseId, <<"VenueLead">>, State).

%%====================================================================
%% Press Lead Agent
%%====================================================================

%% @doc Communications: uses same task completion pattern.
-spec press_lead_agent(pid(), binary(), map()) -> ok | {error, term()}.
press_lead_agent(Engine, CaseId, State) ->
    run_role_agent(Engine, CaseId, <<"PressLead">>, State).

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

%% @doc Returns curated fake dialogue for dry-run (no Z.AI).
-spec dry_run_responses() -> #{binary() => #{decision => accept | reject, reason => binary()}}.
dry_run_responses() ->
    #{
        <<"ProgramChair">> => #{decision => accept, reason => <<"The track balance looks solid. Keynotes cover the right themes.">>},
        <<"Reviewer">> => #{decision => accept, reason => <<"Submissions meet the quality bar. Proceed.">>},
        <<"OpsLead">> => #{decision => accept, reason => <<"Infrastructure is ready. No blockers.">>},
        <<"VenueLead">> => #{decision => accept, reason => <<"Venue confirmed. Capacity 500.">>},
        <<"PressLead">> => #{decision => accept, reason => <<"Comms plan approved. Media list ready.">>}
    }.

%% @doc Gets decision data from Z.AI when zai_enabled, else dry-run or default.
-spec decision_data(binary(), map()) -> map().
decision_data(Role, State) ->
    case maps:get(zai_enabled, State, false) of
        true ->
            case zai_decision(Role, State) of
                {ok, Data} -> Data;
                {error, Reason} ->
                    try ct:pal("ERROR: Z.AI failed for ~s: ~p", [Role, Reason])
                    catch _:_ -> io:format(standard_error, "  ERROR: Z.AI failed for ~s: ~p~n", [Role, Reason])
                    end,
                    maybe_log_zai_error(State, Role, Reason),
                    {error, {zai_failed, Role, Reason}}
            end;
        _ ->
            DryRun = dry_run_responses(),
            case maps:find(Role, DryRun) of
                {ok, #{decision := D, reason := Reason}} ->
                    maps:put(reason, Reason, #{decision => D});
                _ ->
                    #{decision => accept}
            end
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
                model => maps:get(model, State, zai_client:get_model()),
                temperature => 0.3,
                max_tokens => 128
            },
            maybe_log_llm_request(State, Role, Prompt),
            case zai_client:chat_json(Messages, Opts) of
                {ok, Json} ->
                    Decision = maps:get(<<"decision">>, Json, <<"accept">>),
                    Reason = maps:get(<<"reason">>, Json, <<>>),
                    maybe_log_llm_vote(State, Role, Decision, Reason, Json),
                    try ct:pal("[AGI Symposium] ~s: decision=~s reason=~s", [Role, Decision, Reason])
                    catch _:_ -> io:format(standard_error, "  [AGI Symposium] ~s: decision=~s reason=~s~n",
                        [Role, Decision, Reason])
                    end,
                    case Decision of
                        <<"reject">> -> {ok, #{decision => reject}};
                        _ -> {ok, #{decision => accept}}
                    end;
                Err -> Err
            end
    end.

maybe_log_zai_error(State, Role, Reason) ->
    case maps:find(case_id, State) of
        {ok, CId} ->
            ReasonBin = iolist_to_binary(io_lib:format("Z.AI failed: ~p", [Reason])),
            agi_symposium_otel:log_llm_vote(CId, Role, <<"error">>, ReasonBin);
        error -> ok
    end.

maybe_log_llm_request(State, Role, Prompt) ->
    case maps:find(case_id, State) of
        {ok, CId} -> agi_symposium_otel:log_llm_request(CId, Role, Prompt);
        error -> ok
    end.

maybe_log_llm_vote(State, Role, Decision, Reason, Json) ->
    case maps:find(case_id, State) of
        {ok, CId} ->
            agi_symposium_otel:log_llm_vote(CId, Role, Decision, Reason),
            agi_symposium_otel:log_llm_response(CId, Role, Json);
        error -> ok
    end,
    case maps:find(on_llm_vote, State) of
        {ok, F} when is_function(F, 3) -> F(Role, Decision, Reason);
        _ -> ok
    end.

maybe_log_pattern_span(State, WiId, Pattern) ->
    case maps:find(case_id, State) of
        {ok, CId} -> agi_symposium_otel:log_pattern_span(CId, WiId, Pattern, #{});
        error -> ok
    end.

role_prompt(<<"ProgramChair">>) ->
    <<"You are Program Chair for AGI Symposium 2026. The proposed program has 3 keynotes, 12 workshops, 50 poster slots. Approve or reject. Reply JSON only: {\"decision\":\"accept\" or \"reject\",\"reason\":\"brief reason\"}">>;
role_prompt(<<"Reviewer">>) ->
    <<"You are Reviewer for AGI Symposium 2026. Evaluate the submission quality. 120 papers submitted, 50 accepted. Approve or reject. Reply JSON only: {\"decision\":\"accept\" or \"reject\",\"reason\":\"brief reason\"}">>;
role_prompt(<<"OpsLead">>) ->
    <<"You are Ops Lead for AGI Symposium 2026. Confirm operations are ready: AV, WiFi, registration system. Approve or reject. Reply JSON only: {\"decision\":\"accept\" or \"reject\",\"reason\":\"brief reason\"}">>;
role_prompt(<<"VenueLead">>) ->
    <<"You are Venue Lead for AGI Symposium 2026. Confirm venue is booked. Capacity 500, catering arranged. Approve or reject. Reply JSON only: {\"decision\":\"accept\" or \"reject\",\"reason\":\"brief reason\"}">>;
role_prompt(<<"PressLead">>) ->
    <<"You are Press Lead for AGI Symposium 2026. Confirm comms and press are ready. Media list, social plan. Approve or reject. Reply JSON only: {\"decision\":\"accept\" or \"reject\",\"reason\":\"brief reason\"}">>;
role_prompt(<<"SafetyOfficer">>) ->
    <<"You are Safety Officer. All clear. Reply JSON: {\"decision\":\"accept\"}">>;
role_prompt(_) ->
    <<"Reply JSON: {\"decision\":\"accept\"}">>.

-spec run_role_agent(pid(), binary(), binary(), map()) -> ok | {error, term()}.
run_role_agent(Engine, CaseId, User, State) ->
    Now = erlang:monotonic_time(millisecond),
    case decision_data(User, State) of
        {error, _} = Err -> Err;
        Data ->
            run_role_agent_impl(Engine, CaseId, User, State, Data, Now)
    end.

run_role_agent_impl(Engine, CaseId, User, State, Data, Now) ->
    WiId = case role_to_task(User) of
        undefined -> case offered_workitems(CaseId, Engine) of [W | _] -> W; [] -> undefined end;
        Task -> offered_workitem_for_task(Engine, CaseId, Task)
    end,
    MaybeLogVote = fun() -> maybe_log_dry_run_vote(State, User, Data) end,
    case WiId of
        undefined ->
            case wf_engine:worklist(Engine, User) of
                [] ->
                    try_allocate_any(Engine, CaseId, User, Data, Now, State);
                [WI | _] ->
                    MaybeLogVote(),
                    complete_work_item(Engine, WI, User, Data, Now, State)
            end;
        _ ->
            case wf_engine:allocate(Engine, WiId, User, Now) of
                ok ->
                    case wf_engine:start_work(Engine, WiId, User, Now) of
                        ok ->
                            Pat = case role_to_task(User) of
                                undefined -> t_go_nogo;
                                T -> T
                            end,
                            maybe_log_pattern_span(State, WiId, Pat),
                            MaybeLogVote(),
                            wf_engine:complete(Engine, WiId, User, Data, Now);
                        Err -> Err
                    end;
                Err -> Err
            end
    end.

%% @doc Log dry-run vote only when we have reason (i.e. we're completing a task).
maybe_log_dry_run_vote(State, User, Data) ->
    case maps:find(reason, Data) of
        {ok, Reason} ->
            D = maps:get(decision, Data, accept),
            DecBin = case D of accept -> <<"accept">>; reject -> <<"reject">> end,
            Json = #{<<"decision">> => DecBin, <<"reason">> => Reason},
            maybe_log_llm_vote(State, User, DecBin, Reason, Json);
        _ -> ok
    end.

-spec try_allocate_any(pid(), binary(), binary(), map(), integer(), map()) -> ok | {error, term()}.
try_allocate_any(Engine, CaseId, User, Data, Now, State) ->
    case wf_engine:case_log(Engine, CaseId) of
        {error, _} -> ok;
        Log ->
            Items = [{Trsn, WiId} || {task_created, Trsn, WiId, _Now} <- Log],
            try_allocate_loop(Engine, CaseId, Items, User, Data, Now, State)
    end.

try_allocate_loop(_Engine, _CaseId, [], _User, _Data, _Now, _State) ->
    ok;
try_allocate_loop(Engine, CaseId, [{Task, WiId} | Rest], User, Data, Now, State) ->
    case wf_engine:allocate(Engine, WiId, User, Now) of
        ok ->
            case wf_engine:start_work(Engine, WiId, User, Now) of
                ok ->
                    maybe_log_pattern_span(State, WiId, Task),
                    maybe_log_dry_run_vote(State, User, Data),
                    wf_engine:complete(Engine, WiId, User, Data, Now);
                Err -> Err
            end;
        _ ->
            try_allocate_loop(Engine, CaseId, Rest, User, Data, Now, State)
    end.

run_task_agent(Engine, CaseId, User, State, Now) ->
    run_role_agent(Engine, CaseId, User, State).

complete_work_item(Engine, WI, User, Data, Now, State) ->
    WiId = WI#work_item.wi_id,
    Task = WI#work_item.task,
    maybe_log_pattern_span(State, WiId, Task),
    maybe_log_dry_run_vote(State, User, Data),
    wf_engine:complete(Engine, WiId, User, Data, Now).
