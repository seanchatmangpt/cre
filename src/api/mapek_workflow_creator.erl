%% -*- erlang -*-
%% @doc LLM-driven workflow creation with self-play MAPE-K loop.
%%
%% MAPE-K: Monitor → Analyze → Plan → Execute, with shared Knowledge.
%% Self-play: LLM generates workflow → execute → monitor → LLM analyzes
%% → LLM plans improvement → iterate until goal or max rounds.
%%
%% Uses zai_client for LLM (Analyze + Plan + workflow generation).
%%
%% @end
-module(mapek_workflow_creator).
-export([
    run_selfplay/1,
    run_selfplay/2,
    mapek_loop/4,
    generate_workflow_yaml/2,
    analyze_and_plan/3
]).

%%====================================================================
%% Types
%%====================================================================

-type knowledge() :: #{
    iterations => non_neg_integer(),
    history => [{non_neg_integer(), map()}],
    goal => binary(),
    constraints => [binary()],
    last_workflow_yaml => binary(),
    last_result => map()
}.

-type monitor_data() :: #{
    status => completed | blocked | timeout | error,
    rounds => non_neg_integer(),
    marking => map(),
    receipts => [map()],
    error => term()
}.

-type selfplay_options() :: #{
    goal => binary(),
    max_iterations => non_neg_integer(),
    model => binary(),
    seed_spec => binary() | undefined,
    constraints => [binary()]
}.

%%====================================================================
%% API
%%====================================================================

%% @doc Run self-play MAPE-K workflow creation.
%% LLM generates workflows, system executes, LLM analyzes and plans next.
-spec run_selfplay(Options :: selfplay_options()) ->
    {ok, knowledge()} | {error, term()}.

run_selfplay(Options) when is_map(Options) ->
    run_selfplay(Options, #{}).

-spec run_selfplay(Options :: selfplay_options(), Callbacks :: map()) ->
    {ok, knowledge()} | {error, term()}.

run_selfplay(Options, Callbacks) when is_map(Options), is_map(Callbacks) ->
    case zai_client:get_api_key() of
        undefined ->
            {error, api_key_not_configured};
        _ ->
            _ = application:load(cre),
            _ = application:ensure_all_started(yamerl),
            _ = application:ensure_all_started(inets),
            Knowledge = init_knowledge(Options),
            mapek_loop(Knowledge, Options, Callbacks, 0)
    end.

%% @doc MAPE-K loop: Monitor → Analyze → Plan → Execute.
-spec mapek_loop(
    Knowledge :: knowledge(),
    Options :: selfplay_options(),
    Callbacks :: map(),
    Iteration :: non_neg_integer()
) -> {ok, knowledge()} | {error, term()}.

mapek_loop(Knowledge, Options, Callbacks, Iteration) ->
    MaxIter = maps:get(max_iterations, Options, 5),
    if Iteration >= MaxIter ->
        {ok, Knowledge#{iterations => Iteration}};
    true ->
        %% Execute: generate or use seed workflow
        EPhase = try
            execute_phase(Knowledge, Options, Callbacks)
        catch
            throw:{generate_failed, R} -> {error, R}
        end,
        case EPhase of
            {ok, NewKnowledge} ->
                %% Monitor: observe execution result
                MonitorData = monitor_phase(NewKnowledge),
                %% Analyze + Plan: LLM reasons and plans next
                case analyze_and_plan(MonitorData, NewKnowledge, Options) of
                    {continue, Plan} ->
                        NextKnowledge = NewKnowledge#{
                            history => [{Iteration + 1, MonitorData} | maps:get(history, NewKnowledge, [])],
                            plan => Plan
                        },
                        mapek_loop(NextKnowledge, Options, Callbacks, Iteration + 1);
                    {done, FinalKnowledge} ->
                        {ok, FinalKnowledge#{iterations => Iteration + 1}};
                    {error, Reason} ->
                        {error, Reason}
                end;
            {error, Err} ->
                {error, Err}
        end
    end.

%% @doc Generate workflow YAML via LLM.
-spec generate_workflow_yaml(Knowledge :: knowledge(), Options :: map()) ->
    {ok, binary()} | {error, term()}.

generate_workflow_yaml(Knowledge, Options) ->
    Goal = maps:get(goal, Knowledge, <<"Create a simple workflow with 2-3 tasks">>),
    Constraints = maps:get(constraints, Knowledge, []),
    HistoryHint = case maps:get(history, Knowledge, []) of
        [] -> <<"">>;
        _ -> <<"Previous iteration failed or was incomplete. Improve the workflow.">>
    end,
    PlanHint = case maps:get(plan, Knowledge, none) of
        none -> <<"">>;
        Plan ->
            Improvs = maps:get(improvements, Plan, []),
            R = maps:get(reason, Plan, <<"">>),
            case Improvs of
                [] -> iolist_to_binary(io_lib:format("Previous analysis: ~s~n", [R]));
                _ -> iolist_to_binary(io_lib:format("Incorporate these improvements:~n~sReason: ~s~n",
                    [iolist_to_binary([io_lib:format("- ~s~n", [I]) || I <- Improvs]), R]))
            end
    end,
    Prompt = io_lib:format(
        "You are a YAWL workflow designer. Generate a valid YAML 0.2 workflow spec.~n"
        "Goal: ~s~n"
        "Constraints: ~s~n"
        "~s~s"
        "Reply with ONLY valid YAML (no markdown fences). Minimal spec: yawl_yaml_version, "
        "specificationSet with uri, rootNet, nets (one net with nodes: Start/End conditions, "
        "2-3 tasks, flows). Use pattern P1_Sequence for a simple linear flow.~n",
        [Goal, lists:flatten([C || C <- Constraints]), HistoryHint, PlanHint]
    ),
    Msgs = [#{role => <<"user">>, content => list_to_binary(Prompt)}],
    Opts = #{
        model => maps:get(model, Options, zai_client:get_model()),
        temperature => 0.3,
        max_tokens => 2048,
        timeout => 60000
    },
    case zai_client:chat_completions(Msgs, Opts) of
        {ok, Content} ->
            Yaml = clean_yaml_response(Content),
            {ok, Yaml};
        Err -> Err
    end.

%% @doc Analyze execution result and plan next action (LLM).
-spec analyze_and_plan(
    MonitorData :: monitor_data(),
    Knowledge :: knowledge(),
    Options :: map()
) -> {continue, map()} | {done, knowledge()} | {error, term()}.

analyze_and_plan(MonitorData, Knowledge, Options) ->
    Status = maps:get(status, MonitorData, error),
    Rounds = maps:get(rounds, MonitorData, 0),
    Error = maps:get(error, MonitorData, none),
    Prompt = io_lib:format(
        "Workflow execution result:~n"
        "  Status: ~p~n"
        "  Rounds: ~p~n"
        "  Error: ~p~n"
        "Goal: ~s~n"
        "Reply JSON: {\"action\":\"continue\" or \"done\", \"reason\":\"brief reason\", "
        "\"improvements\":[\"list of suggested improvements\"]}~n",
        [Status, Rounds, Error, maps:get(goal, Knowledge, <<"">>)]
    ),
    Msgs = [#{role => <<"user">>, content => list_to_binary(Prompt)}],
    Opts = #{
        model => maps:get(model, Options, zai_client:get_model()),
        temperature => 0.2,
        max_tokens => 512
    },
    case zai_client:chat_json(Msgs, Opts) of
        {ok, Json} ->
            Action = maps:get(<<"action">>, Json, <<"continue">>),
            Plan = #{
                reason => maps:get(<<"reason">>, Json, <<"">>),
                improvements => maps:get(<<"improvements">>, Json, [])
            },
            case Action of
                <<"done">> ->
                    {done, Knowledge#{last_plan => Plan}};
                _ ->
                    {continue, Plan}
            end;
        Err -> {error, Err}
    end.

%%====================================================================
%% Internal
%%====================================================================

init_knowledge(Options) ->
    #{
        iterations => 0,
        history => [],
        goal => maps:get(goal, Options, <<"Create a runnable YAWL workflow">>),
        constraints => maps:get(constraints, Options, [
            <<"Use YAML 0.2 format">>,
            <<"One net with Start, End, 2-3 tasks">>
        ]),
        last_workflow_yaml => maps:get(seed_spec, Options, <<>>),
        last_result => #{}
    }.

execute_phase(Knowledge, Options, _Callbacks) ->
    %% Use seed only on first iteration when no plan; otherwise generate (incl. plan)
    UseSeed = maps:get(last_workflow_yaml, Knowledge, <<>>) =/= <<>>
        andalso not maps:is_key(plan, Knowledge),
    Yaml = if UseSeed ->
            maps:get(last_workflow_yaml, Knowledge, <<>>);
        true ->
            case generate_workflow_yaml(Knowledge, Options) of
                {ok, Generated} -> Generated;
                {error, R} -> throw({generate_failed, R})
            end
    end,
    case execute_workflow_yaml(Yaml, Options) of
        {ok, Result} ->
            {ok, Knowledge#{
                last_workflow_yaml => Yaml,
                last_result => Result
            }};
        {error, Reason} ->
            {ok, Knowledge#{
                last_workflow_yaml => Yaml,
                last_result => #{status => error, error => Reason}
            }}
    end.

execute_workflow_yaml(Yaml, _Options) when is_binary(Yaml) ->
    TempFile = filename:join([os:getenv("TMPDIR", "/tmp"),
        "mapek_wf_" ++ integer_to_list(erlang:unique_integer()) ++ ".yaml"]),
    try
        ok = file:write_file(TempFile, Yaml),
        case wf_yaml_spec:from_yaml_file(TempFile) of
            {ok, Spec} ->
                case wf_yawl_executor:compile_workflow(Spec, #{}) of
                    {ok, Executor} ->
                        case wf_yawl_executor:ensure_modules_loaded(Executor) of
                            {ok, _} ->
                                case wf_yawl_executor:start_workflow(Executor, #{}) of
                                    {ok, Pid, _CaseId} ->
                                        Result = run_to_completion(Pid, Executor, 0, 50),
                                        wf_yawl_executor:stop_workflow(Pid),
                                        {ok, Result};
                                    Err -> Err
                                end;
                            Err -> Err
                        end;
                    Err -> Err
                end;
            Err -> Err
        end
    after
        file:delete(TempFile)
    end.

run_to_completion(Pid, Executor, Round, MaxRounds) when Round < MaxRounds ->
    case gen_yawl:step(Pid) of
        {ok, _} -> run_to_completion(Pid, Executor, Round + 1, MaxRounds);
        abort ->
            Marking = gen_yawl:marking(Pid),
            case check_completed(Marking, Executor) of
                true -> #{status => completed, rounds => Round};
                false -> #{status => blocked, rounds => Round, marking => Marking}
            end
    end;
run_to_completion(_Pid, _Executor, Round, _MaxRounds) ->
    #{status => timeout, rounds => Round}.

check_completed(Marking, Executor) ->
    RootMod = wf_yawl_executor:get_root_module(Executor),
    try
        Places = RootMod:place_lst(),
        case Places of
            [] -> false;
            _ ->
                EndPlace = lists:last(Places),
                length(maps:get(EndPlace, Marking, [])) > 0
        end
    catch _:_ -> false
    end.

monitor_phase(Knowledge) ->
    Result = maps:get(last_result, Knowledge, #{}),
    #{
        status => maps:get(status, Result, error),
        rounds => maps:get(rounds, Result, 0),
        marking => maps:get(marking, Result, #{}),
        receipts => maps:get(receipts, Result, []),
        error => maps:get(error, Result, none)
    }.

clean_yaml_response(Content) when is_binary(Content) ->
    %% Remove markdown code fences if present
    C1 = binary:replace(Content, <<"```yaml">>, <<>>, [global]),
    C2 = binary:replace(C1, <<"```">>, <<>>, [global]),
    binary:trim(C2).
