%% -*- erlang -*-
%% @doc OTEL instrumentation for AGI Symposium simulation.
%%
%% Logs LLM communication, voting, and workflow pattern spans to yawl_otel_logger.
%% Formatted script output via print_otel_script/1.
-module(agi_symposium_otel).
-include("yawl_otel_logger.hrl").
-export([
    log_workflow_start/2,
    log_workflow_complete/2,
    log_pattern_span/4,
    log_llm_request/3,
    log_llm_vote/4,
    log_llm_response/3,
    get_trace_id/1,
    print_otel_script/1,
    print_otel_script/2,
    print_banner/0,
    format_role_display/1,
    print_full_prompt/3,
    print_full_response/2,
    print_pattern_execution/3,
    print_workflow_context/1,
    print_omega_banner/0,
    print_omega_summary/2,
    print_transcript_banner/0,
    print_transcript_line/2,
    print_transcript_end/0
]).

%%====================================================================
%% API
%%====================================================================

%% @doc Log symposium workflow start. Returns TraceId for correlation.
-spec log_workflow_start(binary(), binary()) -> ok.
log_workflow_start(CaseId, SpecId) ->
    safe_log(fun() ->
        yawl_otel_logger:log_workflow_start(CaseId, SpecId)
    end).

%% @doc Log symposium workflow completion.
-spec log_workflow_complete(binary(), atom() | binary()) -> ok.
log_workflow_complete(CaseId, Status) ->
    safe_log(fun() ->
        S = case Status of
            S0 when is_atom(S0) -> atom_to_binary(S0, utf8);
            S0 when is_binary(S0) -> S0
        end,
        yawl_otel_logger:log_workflow_complete(CaseId, S)
    end).

%% @doc Log pattern span (e.g. P1_Sequence, t_program_chair).
-spec log_pattern_span(binary(), binary(), atom(), map()) -> ok.
log_pattern_span(CaseId, SpanId, PatternId, Attributes) ->
    safe_log(fun() ->
        TraceId = yawl_otel_logger:get_trace_id_for_case(CaseId),
        case TraceId of
            undefined -> ok;
            _ ->
                Attrs = maps:merge(Attributes, #{
                    trace_id => TraceId,
                    case_id => CaseId,
                    span_id => SpanId,
                    pattern_id => PatternId
                }),
                yawl_otel_logger:log_event(pattern_span, <<"Pattern span">>, Attrs, info)
        end
    end).

%% @doc Log LLM request (before Z.AI call).
-spec log_llm_request(binary(), binary(), binary()) -> ok.
log_llm_request(CaseId, Role, Prompt) ->
    safe_log(fun() ->
        TraceId = yawl_otel_logger:get_trace_id_for_case(CaseId),
        case TraceId of
            undefined -> ok;
            _ ->
                yawl_otel_logger:log_event(llm_request, <<"LLM request">>, #{
                    trace_id => TraceId,
                    case_id => CaseId,
                    role => Role,
                    prompt => Prompt
                }, info)
        end
    end).

%% @doc Log LLM vote (decision + reason).
-spec log_llm_vote(binary(), binary(), binary(), binary()) -> ok.
log_llm_vote(CaseId, Role, Decision, Reason) ->
    safe_log(fun() ->
        TraceId = yawl_otel_logger:get_trace_id_for_case(CaseId),
        case TraceId of
            undefined -> ok;
            _ ->
                yawl_otel_logger:log_event(llm_vote, <<"LLM vote">>, #{
                    trace_id => TraceId,
                    case_id => CaseId,
                    role => Role,
                    decision => Decision,
                    reason => Reason
                }, info)
        end
    end).

%% @doc Log LLM response (after Z.AI call).
-spec log_llm_response(binary(), binary(), map()) -> ok.
log_llm_response(CaseId, Role, Json) ->
    safe_log(fun() ->
        TraceId = yawl_otel_logger:get_trace_id_for_case(CaseId),
        case TraceId of
            undefined -> ok;
            _ ->
                Decision = maps:get(<<"decision">>, Json, <<"accept">>),
                Reason = maps:get(<<"reason">>, Json, <<>>),
                yawl_otel_logger:log_event(llm_response, <<"LLM response">>, #{
                    trace_id => TraceId,
                    case_id => CaseId,
                    role => Role,
                    decision => Decision,
                    reason => Reason,
                    raw => Json
                }, info)
        end
    end).

%% @doc Get trace_id for case (for formatting).
-spec get_trace_id(binary()) -> binary() | undefined.
get_trace_id(CaseId) ->
    case whereis(yawl_otel_logger) of
        undefined -> undefined;
        _ -> yawl_otel_logger:get_trace_id_for_case(CaseId)
    end.

%% @doc Print OTEL script from case result. Uses TraceId from last trace if CaseId not in Result.
-spec print_otel_script(map()) -> ok.
print_otel_script(Result) ->
    CaseId = maps:get(case_id, Result, <<>>),
    print_otel_script(CaseId, Result).

%% @doc Print OTEL script for CaseId, optionally with Result metadata.
-spec print_otel_script(binary(), map()) -> ok.
print_otel_script(CaseId, Result) ->
    case whereis(yawl_otel_logger) of
        undefined ->
            io:format(standard_error, "%% OTEL: yawl_otel_logger not running~n", []);
        _ ->
            TraceId = yawl_otel_logger:get_trace_id_for_case(CaseId),
            Events = case TraceId of
                undefined -> yawl_otel_logger:get_events();
                _ -> yawl_otel_logger:get_events_by_trace(TraceId)
            end,
            format_and_print(CaseId, TraceId, Events, Result)
    end.

%%====================================================================
%% Internal
%%====================================================================

safe_log(F) ->
    case whereis(yawl_otel_logger) of
        undefined -> ok;
        _ -> F()
    end.

format_and_print(CaseId, TraceId, Events, Result) ->
    Sorted = lists:sort(fun(A, B) ->
        A#otel_event.timestamp =< B#otel_event.timestamp
    end, Events),
    BaseTs = case Sorted of
        [First | _] -> First#otel_event.timestamp;
        [] -> erlang:system_time(millisecond)
    end,
    VoteEvents = [E || E <- Sorted, E#otel_event.event_type =:= llm_vote],
    case VoteEvents of
        [] -> ok;
        _ ->
            io:format(standard_error, "~nDialogue:~n", []),
            lists:foreach(fun(E) -> format_dialogue_line(E, BaseTs) end, VoteEvents),
            print_summary_narrative(VoteEvents),
            io:format(standard_error, "~n", [])
    end,
    io:format(standard_error, "=== OTEL Trace ===~n", []),
    io:format(standard_error, "Trace: ~s~n", [format_trace_id(TraceId)]),
    io:format(standard_error, "Case:  ~s~n", [CaseId]),
    io:format(standard_error, "Status: ~p~n", [maps:get(status, Result, unknown)]),
    io:format(standard_error, "Rounds: ~p~n", [maps:get(participant_rounds, Result, 0)]),
    io:format(standard_error, "~n--- Events (chronological) ---~n", []),
    lists:foreach(fun(E) -> format_event(E, BaseTs) end, Sorted),
    io:format(standard_error, "~n=== End OTEL Trace ===~n", []).

format_trace_id(undefined) -> <<"n/a">>;
format_trace_id(B) when is_binary(B) -> B.

print_banner() ->
    io:format(standard_error, "~n", []),
    io:format(standard_error, "╔══════════════════════════════════════════════════════════════╗~n", []),
    io:format(standard_error, "║  AGI Symposium 2026 — Planning Committee                     ║~n", []),
    io:format(standard_error, "║  5 LLMs simulating human approval chain                      ║~n", []),
    io:format(standard_error, "╚══════════════════════════════════════════════════════════════╝~n", []).

%% @doc Omega demo banner (20 agents, 43 patterns).
-spec print_omega_banner() -> ok.
print_omega_banner() ->
    io:format(standard_error, "~n", []),
    io:format(standard_error, "╔══════════════════════════════════════════════════════════════╗~n", []),
    io:format(standard_error, "║  AGI Symposium Ω — 20 LLMs Executing All 43 Patterns         ║~n", []),
    io:format(standard_error, "║  Full transparency: prompts, responses, workflow context    ║~n", []),
    io:format(standard_error, "╚══════════════════════════════════════════════════════════════╝~n", []).

%% @doc Print full prompt in box (omega demo).
-spec print_full_prompt(binary(), binary(), iodata()) -> ok.
print_full_prompt(Role, TaskName, Prompt) ->
    BaseTs = erlang:system_time(millisecond),
    TSStr = format_ts_rel(BaseTs, BaseTs),
    PromptBin = iolist_to_binary(Prompt),
    io:format(standard_error, "[~s] [PROMPT] ~s → ~s~n", [TSStr, Role, TaskName]),
    box_print(PromptBin).

%% @doc Print full JSON response (omega demo).
-spec print_full_response(binary(), map()) -> ok.
print_full_response(Role, Json) ->
    JsonBin = jsone:encode(Json),
    io:format(standard_error, "[RESPONSE] ~s~n", [Role]),
    io:format(standard_error, "  ~s~n", [JsonBin]).

%% @doc Print pattern execution (omega demo).
-spec print_pattern_execution(binary(), atom(), binary()) -> ok.
print_pattern_execution(CaseId, Trsn, Label) ->
    _ = CaseId,
    TrsnStr = atom_to_list(Trsn),
    LabelStr = binary_to_list(Label),
    io:format(standard_error, "[PATTERN] ~s: ~s~n", [TrsnStr, LabelStr]).

%% @doc Print workflow context (omega demo).
-spec print_workflow_context(map()) -> ok.
print_workflow_context(Context) ->
    Vars = maps:get(variables, Context, #{}),
    case maps:size(Vars) of
        0 -> ok;
        _ ->
            io:format(standard_error, "~nWorkflow Context:~n", []),
            maps:foreach(fun(K, V) ->
                KStr = ensure_str(K),
                VStr = io_lib:format("~p", [V]),
                io:format(standard_error, "  ~s: ~s~n", [KStr, VStr])
            end, Vars)
    end.

ensure_str(B) when is_binary(B) -> binary_to_list(B);
ensure_str(A) when is_atom(A) -> atom_to_list(A);
ensure_str(X) -> io_lib:format("~p", [X]).

box_print(Content) ->
    Lines = binary:split(Content, <<"\n">>, [global]),
    Width = 60,
    io:format(standard_error, "  ┌~*c┐~n", [Width, $─]),
    lists:foreach(fun(Line) ->
        SubLines = split_line(Line, Width),
        lists:foreach(fun(SL) ->
            Padded = pad_right(SL, Width),
            io:format(standard_error, "  │~s│~n", [Padded])
        end, SubLines)
    end, Lines),
    io:format(standard_error, "  └~*c┘~n", [Width, $─]).

split_line(<<>>, _) -> [<<>>];
split_line(Bin, W) when byte_size(Bin) =< W -> [Bin];
split_line(Bin, W) ->
    <<Head:W/binary, Rest/binary>> = Bin,
    [Head | split_line(Rest, W)].

pad_right(Bin, W) ->
    S = byte_size(Bin),
    if S >= W -> Bin;
       true -> <<Bin/binary, (binary:copy(<<$\s>>, W - S))/binary>>
    end.

%% @doc Print omega demo summary.
-spec print_omega_summary(map(), wf_yaml_spec:yawl_yaml_spec() | undefined) -> ok.
print_omega_summary(Result, undefined) ->
    Status = maps:get(status, Result, unknown),
    Rounds = maps:get(participant_rounds, Result, maps:get(rounds, Result, 0)),
    io:format(standard_error, "~nRounds: ~p~n", [Rounds]),
    io:format(standard_error, "Final State: ~p~n", [Status]);
print_omega_summary(Result, Spec) ->
    Status = maps:get(status, Result, unknown),
    Rounds = maps:get(rounds, Result, 0),
    Instances = wf_yaml_spec:pattern_instances(Spec),
    N = length(Instances),
    io:format(standard_error, "~nPatterns: ~p instances~n", [N]),
    io:format(standard_error, "Rounds: ~p~n", [Rounds]),
    io:format(standard_error, "Final State: ~p~n", [Status]).

print_summary_narrative(VoteEvents) ->
    Accepts = [E || E <- VoteEvents,
        maps:get(decision, E#otel_event.attributes, <<>>) =:= <<"accept">>],
    Rejects = [E || E <- VoteEvents,
        maps:get(decision, E#otel_event.attributes, <<>>) =:= <<"reject">>],
    Total = length(VoteEvents),
    Acc = length(Accepts),
    Rej = length(Rejects),
    if Rej =:= 0 ->
        io:format(standard_error, "~n~s~n", [agi_symposium_ansi:bold("Committee decision: APPROVED (" ++ integer_to_list(Acc) ++ "/" ++ integer_to_list(Total) ++ ")")]);
    true ->
        RejectRoles = [format_role(maps:get(role, E#otel_event.attributes, <<>>)) || E <- Rejects],
        io:format(standard_error, "~n~s~n", [agi_symposium_ansi:bold("Committee decision: REJECTED (" ++ integer_to_list(Rej) ++ "/" ++ integer_to_list(Total) ++ ") — " ++ string:join(RejectRoles, ", "))])
    end.

format_dialogue_line(E, _BaseTs) ->
    Attrs = E#otel_event.attributes,
    Role = format_role(maps:get(role, Attrs, <<>>)),
    Dec = maps:get(decision, Attrs, <<>>),
    Reason = maps:get(reason, Attrs, <<>>),
    Colored = case Dec of
        <<"reject">> -> agi_symposium_ansi:red("REJECT");
        <<"accept">> -> agi_symposium_ansi:green("ACCEPT");
        _ -> string:uppercase(binary_to_list(Dec))
    end,
    CyanRole = agi_symposium_ansi:cyan(Role),
    ReasonStr = case Reason of
        <<>> -> "";
        _ -> " \"" ++ binary_to_list(Reason) ++ "\""
    end,
    io:format(standard_error, "  ~s: ~s —~s~n", [CyanRole, Colored, ReasonStr]).

format_role(<<"Chair">>) -> "Chair";
format_role(<<"Secretary">>) -> "Secretary";
format_role(<<"ProgramChair">>) -> "Program Chair";
format_role(<<"TrackChair">>) -> "Track Chair";
format_role(<<"AreaChair">>) -> "Area Chair";
format_role(<<"Reviewer">>) -> "Reviewer";
format_role(<<"EthicsChair">>) -> "Ethics Chair";
format_role(<<"ArtifactChair">>) -> "Artifact Chair";
format_role(<<"OpsLead">>) -> "Ops Lead";
format_role(<<"VenueLead">>) -> "Venue Lead";
format_role(<<"AVLead">>) -> "AV Lead";
format_role(<<"SafetyOfficer">>) -> "Safety Officer";
format_role(<<"SponsorshipLead">>) -> "Sponsorship Lead";
format_role(<<"FinanceLead">>) -> "Finance Lead";
format_role(<<"TravelGrantsLead">>) -> "Travel Grants Lead";
format_role(<<"PublicationsChair">>) -> "Publications Chair";
format_role(<<"PressLead">>) -> "Press Lead";
format_role(<<"Speaker">>) -> "Speaker";
format_role(<<"Attendee">>) -> "Attendee";
format_role(<<"WorkshopChair">>) -> "Workshop Chair";
format_role(R) when is_binary(R) -> binary_to_list(R);
format_role(_) -> "Unknown".

-spec format_role_display(binary()) -> string().
format_role_display(Role) ->
    format_role(Role).

format_event(E, BaseTs) ->
    Ts = E#otel_event.timestamp,
    Type = E#otel_event.event_type,
    Msg = E#otel_event.message,
    Attrs = E#otel_event.attributes,
    TSStr = format_ts_rel(Ts, BaseTs),
    PatLabel = fun(Pat) -> pattern_label(Pat) end,
    case Type of
        workflow_start ->
            io:format(standard_error, "[~s] [SPAN] workflow_start~n", [TSStr]),
            io:format(standard_error, "  pattern_id=~s~n", [PatLabel(maps:get(pattern_id, Attrs, <<>>))]);
        workflow_complete ->
            io:format(standard_error, "[~s] [SPAN] workflow_complete status=~p~n", [TSStr, maps:get(status, Attrs, ok)]);
        pattern_span ->
            Pat = maps:get(pattern_id, Attrs, <<>>),
            io:format(standard_error, "[~s] [SPAN] pattern_used ~s~n", [TSStr, PatLabel(Pat)]);
        llm_request ->
            Role = maps:get(role, Attrs, <<>>),
            io:format(standard_error, "[~s] [EVENT] llm_request role=~s~n", [TSStr, Role]);
        llm_vote ->
            Role = maps:get(role, Attrs, <<>>),
            Dec = maps:get(decision, Attrs, <<>>),
            Reason = maps:get(reason, Attrs, <<>>),
            io:format(standard_error, "[~s] [EVENT] llm_vote role=~s decision=~s reason=~s~n",
                [TSStr, Role, Dec, Reason]);
        llm_response ->
            Role = maps:get(role, Attrs, <<>>),
            Dec = maps:get(decision, Attrs, <<>>),
            Reason = maps:get(reason, Attrs, <<>>),
            io:format(standard_error, "[~s] [EVENT] llm_response role=~s decision=~s reason=~s~n",
                [TSStr, Role, Dec, Reason]);
        workitem_start ->
            Task = maps:get(task_id, Attrs, <<>>),
            io:format(standard_error, "[~s] [SPAN] workitem_start task=~s~n", [TSStr, Task]);
        workitem_complete ->
            Task = maps:get(task_id, Attrs, <<>>),
            Res = maps:get(result, Attrs, <<>>),
            io:format(standard_error, "[~s] [SPAN] workitem_complete task=~s result=~s~n", [TSStr, Task, Res]);
        _ ->
            io:format(standard_error, "[~s] [EVENT] ~p ~s~n", [TSStr, Type, Msg])
    end.

pattern_label(t_program_chair) -> "P1_Sequence (WCP-01)";
pattern_label(t_reviewer) -> "P1_Sequence (WCP-01)";
pattern_label(t_ops_lead) -> "P1_Sequence (WCP-01)";
pattern_label(t_venue_lead) -> "P1_Sequence (WCP-01)";
pattern_label(t_press_lead) -> "P1_Sequence (WCP-01)";
pattern_label(t_go_nogo) -> "P1_Sequence (WCP-01)";
pattern_label(Pat) when is_atom(Pat) -> atom_to_list(Pat);
pattern_label(P) when is_binary(P) -> binary_to_list(P);
pattern_label(P) -> io_lib:format("~p", [P]).

format_ts_rel(Ts, BaseTs) when is_integer(Ts), is_integer(BaseTs) ->
    DiffMs = Ts - BaseTs,
    Sec = DiffMs / 1000,
    io_lib:format("+~.1fs", [Sec]);
format_ts_rel(Ts, _) when is_integer(Ts) ->
    integer_to_list(Ts);
format_ts_rel(_, _) ->
    "0".

%%====================================================================
%% Transcript Mode Functions (Swarm Turing Test)
%%====================================================================

%% @doc Print transcript banner (no LLM mention).
-spec print_transcript_banner() -> ok.
print_transcript_banner() ->
    io:format(standard_error, "~n", []),
    io:format(standard_error, "AGI Symposium 2026 — Planning Committee Transcript~n", []),
    io:format(standard_error, "~n", []).

%% @doc Print a single transcript line: Role: statement.
-spec print_transcript_line(binary(), map()) -> ok.
print_transcript_line(Role, Decision) ->
    RoleDisplay = format_role_display(Role),
    Statement = maps:get(statement, Decision, maps:get(reason, Decision, <<>>)),
    case Statement of
        <<>> ->
            %% Fallback: use decision to generate minimal statement
            Dec = maps:get(decision, Decision, <<"accept">>),
            Fallback = case Dec of
                <<"accept">> -> <<"I approve.">>;
                <<"reject">> -> <<"I reject.">>;
                _ -> <<"I've reviewed and made a decision.">>
            end,
            io:format(standard_error, "~s: ~s~n", [RoleDisplay, Fallback]);
        _ ->
            StatementStr = binary_to_list(Statement),
            io:format(standard_error, "~s: ~s~n", [RoleDisplay, StatementStr])
    end.

%% @doc Print transcript end marker.
-spec print_transcript_end() -> ok.
print_transcript_end() ->
    io:format(standard_error, "~n— End of transcript —~n", []).
