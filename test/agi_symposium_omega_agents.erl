%% -*- erlang -*-
%% @doc 20 role agents for AGI Symposium Ω with rich prompts.
%%
%% Maps 20 roles from omega YAML to agent functions. Each agent builds
%% role-specific prompts with task context, pattern, and workflow variables.
%% Supports Z.AI and dry-run modes.
-module(agi_symposium_omega_agents).
-export([
    map_roles_to_agents/2,
    create_role_agent/3,
    dry_run_responses_omega/0
]).

%%====================================================================
%% API
%%====================================================================

-spec map_roles_to_agents(wf_yaml_spec:yawl_yaml_spec(), map()) -> #{binary() => fun((binary(), map()) -> map())}.
map_roles_to_agents(Spec, Opts) ->
    RawRoles = wf_yaml_spec:roles(Spec),
    Roles = [ensure_binary(R) || R <- RawRoles],
    DryRun = maps:get(dry_run, Opts, false),
    ZaiEnabled = maps:get(zai_enabled, Opts, false),
    TranscriptMode = maps:get(transcript_mode, Opts, false),
    DryMap = dry_run_responses_omega(),
    lists:foldl(fun(Role, Acc) ->
        Agent = create_role_agent(Role, DryMap, #{dry_run => DryRun, zai_enabled => ZaiEnabled, transcript_mode => TranscriptMode}),
        Acc#{Role => Agent}
    end, #{}, Roles).

-spec create_role_agent(binary(), map(), map()) -> fun((binary(), map()) -> map()).
create_role_agent(Role, DryMap, Opts) ->
    TranscriptMode = maps:get(transcript_mode, Opts, false),
    fun(TaskName, WorkflowContext) ->
        Prompt = case TranscriptMode of
            true -> build_transcript_prompt(Role, TaskName, WorkflowContext);
            false -> build_full_prompt(Role, TaskName, WorkflowContext)
        end,
        case TranscriptMode of
            false -> agi_symposium_otel:print_full_prompt(Role, TaskName, Prompt);
            true -> ok  %% Skip prompt printing in transcript mode
        end,
        MaxTokens = case TranscriptMode of
            true -> 512;
            false -> 256
        end,
        Decision = case maps:get(dry_run, Opts, false) of
            true ->
                DryResp = lookup_dry_response(Role, TaskName, DryMap),
                case TranscriptMode of
                    false -> agi_symposium_otel:print_full_response(Role, DryResp);
                    true -> ok  %% Skip JSON printing in transcript mode
                end,
                extract_decision(DryResp);
            false ->
                case maps:get(zai_enabled, Opts, false) of
                    true ->
                        case zai_client:chat_json(
                            [#{role => <<"user">>, content => Prompt}],
                            #{model => zai_client:get_model(), temperature => 0.3, max_tokens => MaxTokens}
                        ) of
                            {ok, Json} ->
                                case TranscriptMode of
                                    false -> agi_symposium_otel:print_full_response(Role, Json);
                                    true -> ok  %% Skip JSON printing in transcript mode
                                end,
                                extract_decision(Json);
                            {error, _} ->
                                #{decision => <<"accept">>}
                        end;
                    _ ->
                        DryResp = lookup_dry_response(Role, TaskName, DryMap),
                        case TranscriptMode of
                            false -> agi_symposium_otel:print_full_response(Role, DryResp);
                            true -> ok  %% Skip JSON printing in transcript mode
                        end,
                        extract_decision(DryResp)
                end
        end,
        case TranscriptMode of
            true -> agi_symposium_otel:print_transcript_line(Role, Decision);
            false -> ok
        end,
        Reason = maps:get(reason, Decision, <<>>),
        case maps:get(case_id, WorkflowContext, undefined) of
            undefined -> ok;
            CaseId -> agi_symposium_otel:log_llm_vote(CaseId, Role, maps:get(decision, Decision, <<"accept">>), Reason)
        end,
        Decision
    end.

%%====================================================================
%% Prompt Building
%%====================================================================

build_full_prompt(Role, TaskName, Context) ->
    PatternLabel = maps:get(pattern_label, Context, <<"P1 Sequence">>),
    Vars = maps:get(variables, Context, #{}),
    VarStr = format_vars(Vars),
    RoleContext = role_context(Role),
    iolist_to_binary([
        <<"You are ">>, Role, <<" for AGI Symposium 2026.\n\n">>,
        <<"Current Task: ">>, TaskName, <<"\n">>,
        <<"Pattern: ">>, PatternLabel, <<"\n">>,
        <<"Context: ">>, VarStr, <<"\n\n">>,
        RoleContext, <<"\n">>,
        <<"Decision Required: Approve or reject the current work item.\n">>,
        <<"Reply JSON only: {\"decision\":\"accept\"|\"reject\",\"reason\":\"brief reason\",\"confidence\":0.0-1.0}\n">>
    ]).

%% @doc Build transcript-mode prompt requesting 50-150 word natural committee statement.
-spec build_transcript_prompt(binary(), binary(), map()) -> binary().
build_transcript_prompt(Role, TaskName, Context) ->
    PatternLabel = maps:get(pattern_label, Context, <<"P1 Sequence">>),
    Vars = maps:get(variables, Context, #{}),
    VarStr = format_vars(Vars),
    RoleContext = role_context(Role),
    iolist_to_binary([
        <<"You are ">>, Role, <<" for AGI Symposium 2026.\n\n">>,
        <<"Current Task: ">>, TaskName, <<"\n">>,
        <<"Pattern: ">>, PatternLabel, <<"\n">>,
        <<"Context: ">>, VarStr, <<"\n\n">>,
        RoleContext, <<"\n\n">>,
        <<"Write a natural 2-4 sentence committee statement (50-150 words) as if you're speaking at the meeting. ">>,
        <<"Include your reasoning and end with a clear approve/reject. ">>,
        <<"Use natural language—no bullet points, no telegraphic style. ">>,
        <<"Reply JSON only: {\"statement\":\"your natural prose here\",\"decision\":\"accept\"|\"reject\"}\n">>
    ]).

format_vars(Vars) when is_map(Vars) ->
    Parts = maps:fold(fun(K, V, Acc) ->
        [io_lib:format("~s=~p", [ensure_binary(K), V]) | Acc]
    end, [], Vars),
    iolist_to_binary(lists:join(<<", ">>, lists:reverse(Parts)));
format_vars(_) -> <<>>.

ensure_binary(B) when is_binary(B) -> B;
ensure_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
ensure_binary(L) when is_list(L) -> iolist_to_binary(L);
ensure_binary(X) -> iolist_to_binary(io_lib:format("~p", [X])).

role_context(<<"Chair">>) -> <<"Context: Overseeing symposium. Go/No-Go and Close decisions.">>;
role_context(<<"Secretary">>) -> <<"Context: Documentation and minutes.">>;
role_context(<<"ProgramChair">>) -> <<"Context: Program structure, 3 keynotes, 12 workshops, 50 posters. Review decisions.">>;
role_context(<<"TrackChair">>) -> <<"Context: Track coordination.">>;
role_context(<<"AreaChair">>) -> <<"Context: Meta-review and author rebuttals.">>;
role_context(<<"Reviewer">>) -> <<"Context: Paper review. 120 submissions, 50 accepted.">>;
role_context(<<"EthicsChair">>) -> <<"Context: Ethics compliance triage.">>;
role_context(<<"ArtifactChair">>) -> <<"Context: Artifact review lane.">>;
role_context(<<"OpsLead">>) -> <<"Context: Operations, AV, network, speaker onboarding.">>;
role_context(<<"VenueLead">>) -> <<"Context: Venue contracting, capacity 500.">>;
role_context(<<"AVLead">>) -> <<"Context: A/V and streaming prep.">>;
role_context(<<"SafetyOfficer">>) -> <<"Context: Security clearance, incident response.">>;
role_context(<<"SponsorshipLead">>) -> <<"Context: Sponsor liaison.">>;
role_context(<<"FinanceLead">>) -> <<"Context: Travel grants, visa letters.">>;
role_context(<<"TravelGrantsLead">>) -> <<"Context: Travel grant allocation.">>;
role_context(<<"PublicationsChair">>) -> <<"Context: Camera-ready, proceedings.">>;
role_context(<<"PressLead">>) -> <<"Context: Press briefs, declass mode.">>;
role_context(<<"Speaker">>) -> <<"Context: Speaker perspective.">>;
role_context(<<"Attendee">>) -> <<"Context: Attendee perspective.">>;
role_context(<<"WorkshopChair">>) -> <<"Context: Workshop coordination.">>;
role_context(R) -> <<"Context: Role ", R/binary>>.

%%====================================================================
%% Decision Extraction
%%====================================================================

extract_decision(Json) when is_map(Json) ->
    Dec = maps:get(<<"decision">>, Json, maps:get(decision, Json, <<"accept">>)),
    Statement = maps:get(<<"statement">>, Json, maps:get(statement, Json, <<>>)),
    Reason = maps:get(<<"reason">>, Json, maps:get(reason, Json, <<>>)),
    %% Prefer statement for transcript mode, fallback to reason
    DisplayText = case Statement of
        <<>> -> Reason;
        _ -> Statement
    end,
    #{decision => ensure_binary(Dec), reason => ensure_binary(DisplayText), statement => ensure_binary(Statement)}.

%%====================================================================
%% Dry Run
%%====================================================================

-spec dry_run_responses_omega() -> #{binary() => #{binary() => map()}}.
dry_run_responses_omega() ->
    #{
        <<"Chair">> => #{
            <<"GoNoGo">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"I've reviewed the materials from ops, program, and comms. Security has signed off, venue is confirmed, and we're not seeing any red flags on the timeline. Given that, I don't see a reason to hold this up. I'm approving the split and we can move to the next phase.">>,
                <<"reason">> => <<"All readiness green">>,
                <<"confidence">> => 0.95
            },
            <<"CloseSymposium">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"Program, ops, and comms are all green. The symposium ran smoothly, attendance was strong, and we've completed all the planned sessions. I'm closing the symposium. Proceedings can go out.">>,
                <<"reason">> => <<"Symposium complete">>,
                <<"confidence">> => 1.0
            }
        },
        <<"ProgramChair">> => #{
            <<"Decision">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"We've gone through the review cycle on this one. The methodology is sound, the rebuttal addressed the concerns, and the meta-review lines up. I'm comfortable accepting this for the program. It fits our track structure.">>,
                <<"reason">> => <<"Paper meets standards">>,
                <<"confidence">> => 0.9
            },
            <<"ProgramBuild">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"The program structure is solid. We've got three keynotes confirmed, twelve workshops scheduled, and fifty posters that cover the main tracks. The timing works, and we're not seeing any conflicts. I approve the program build.">>,
                <<"reason">> => <<"Program structure solid">>,
                <<"confidence">> => 0.92
            }
        },
        <<"Reviewer">> => #{
            <<"ReviewCycle">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"I spent some time on the methodology section. The experimental design is solid and the statistical approach is appropriate. A couple of minor concerns were raised in rebuttal but the authors addressed them. I'm in favor of acceptance.">>,
                <<"reason">> => <<"Sound methodology">>,
                <<"confidence">> => 0.88
            }
        },
        <<"EthicsChair">> => #{
            <<"DeskTriage">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"I've done the triage. We've routed through ethics, artifact, and topical lanes. No compliance issues. The disclosure section looks good and we're not seeing any conflicts. Approved from my side.">>,
                <<"reason">> => <<"Ethics compliant">>,
                <<"confidence">> => 0.9
            }
        },
        <<"AreaChair">> => #{
            <<"MetaReview">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"The meta-review is consistent with the individual reviews. The reviewers were thorough, and their assessments align. The rebuttal addressed the remaining concerns. I'm comfortable accepting this for the program.">>,
                <<"reason">> => <<"Meta-review consistent">>,
                <<"confidence">> => 0.9
            },
            <<"AuthorRebuttal">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"The authors' rebuttal addressed the key concerns raised by reviewers. They provided additional analysis and clarified the methodology. While not perfect, it's sufficient. I'm accepting the rebuttal.">>,
                <<"reason">> => <<"Rebuttal addressed">>,
                <<"confidence">> => 0.85
            }
        },
        <<"VenueLead">> => #{
            <<"VenueRace">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"Contract is signed. We've got capacity for 500, AV is on the same page, and we're good for the dates. The venue team has confirmed all the logistics. Ready to proceed.">>,
                <<"reason">> => <<"Venue contract signed">>,
                <<"confidence">> => 0.95
            }
        },
        <<"SafetyOfficer">> => #{
            <<"SecurityClear">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"Security clearance complete. No red flags. We've run background checks, verified credentials, and confirmed access protocols. Incident response cell is ready. I'm approving security clearance.">>,
                <<"reason">> => <<"Security clearance complete">>,
                <<"confidence">> => 0.98
            },
            <<"IncidentIntake">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"Incident logged. We've documented the details, assigned a priority, and routed it to the appropriate response team. The intake process is complete.">>,
                <<"reason">> => <<"Incident logged">>,
                <<"confidence">> => 0.9
            },
            <<"IncidentResponse">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"Response deployed. The incident has been addressed, affected parties notified, and follow-up actions scheduled. The response cell has completed its work.">>,
                <<"reason">> => <<"Response deployed">>,
                <<"confidence">> => 0.92
            }
        },
        <<"AVLead">> => #{
            <<"AVPrep">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"A/V and streaming are set. We've run through the policy checklist, tested the equipment, and confirmed streaming endpoints. The tech team is ready. I approve.">>,
                <<"reason">> => <<"AV ready">>,
                <<"confidence">> => 0.95
            }
        },
        <<"OpsLead">> => #{
            <<"NetworkPrep">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"Network is configured. We've set up the infrastructure, tested connectivity, and confirmed bandwidth. Speaker onboarding is done. All good on the ops side.">>,
                <<"reason">> => <<"Network configured">>,
                <<"confidence">> => 0.9
            },
            <<"SpeakerOnboarding">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"Speaker onboarding is complete. We've collected bios, headshots, AV needs, travel details, and consent forms. All speakers are confirmed and ready. I'm approving this.">>,
                <<"reason">> => <<"Onboarding complete">>,
                <<"confidence">> => 0.88
            }
        },
        <<"TravelGrantsLead">> => #{
            <<"TravelGrants">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"Grants allocated. We've reviewed applications, verified need, and distributed funds according to our criteria. The allocation process is complete.">>,
                <<"reason">> => <<"Grants allocated">>,
                <<"confidence">> => 0.9
            }
        },
        <<"FinanceLead">> => #{
            <<"VisaLetters">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"Visa letters sent. We've processed all requests, verified documentation, and sent official letters to the appropriate consulates. The visa support process is complete.">>,
                <<"reason">> => <<"Visa letters sent">>,
                <<"confidence">> => 0.92
            }
        },
        <<"SponsorshipLead">> => #{
            <<"SponsorLiaison">> => #{
                <<"decision">> => <<"accept">>,
                <<"statement">> => <<"Sponsor liaison confirmed. We've coordinated with all sponsors, confirmed commitments, and aligned on deliverables. Sponsor relations are in good shape.">>,
                <<"reason">> => <<"Sponsor confirmed">>,
                <<"confidence">> => 0.9
            }
        }
    }.

lookup_dry_response(Role, TaskName, DryMap) ->
    RoleMap = maps:get(Role, DryMap, #{}),
    Default = #{
        <<"decision">> => <<"accept">>,
        <<"statement">> => <<"I've reviewed the materials and I'm comfortable proceeding. I approve.">>,
        <<"reason">> => <<"Dry run default">>,
        <<"confidence">> => 0.8
    },
    maps:get(TaskName, RoleMap, Default).
