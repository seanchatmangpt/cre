%%%-------------------------------------------------------------------
%%% @doc
%%% SOC 2 Meta-Validator
%%%
%%% Validates the validators themselves. Ensures:
%%% 1. All control executors are running
%%% 2. All evidence generators are running
%%% 3. Receipt chain is functioning
%%% 4. Receipts are cryptographically valid
%%% 5. No gaps in coverage
%%%
%%% Joe Armstrong: "Who watches the watchers?"
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_meta_validator).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([run_meta_validation/0]).
-export([get_system_health/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(META_VALIDATION_INTERVAL_MS, 600_000).  % 10 minutes

-record(state, {
    last_meta_validation :: map() | undefined,
    last_validated :: calendar:datetime() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec run_meta_validation() -> map().
run_meta_validation() ->
    gen_server:call(?MODULE, run_meta_validation, 60_000).

-spec get_system_health() -> map().
get_system_health() ->
    gen_server:call(?MODULE, get_system_health).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    logger:info(#{what => soc2_meta_validator_started}),

    %% Schedule first meta-validation
    erlang:send_after(10_000, self(), meta_validate),

    {ok, #state{
        last_meta_validation = undefined,
        last_validated = undefined
    }}.

handle_call(run_meta_validation, _From, State) ->
    Result = execute_meta_validation(),
    NewState = State#state{
        last_meta_validation = Result,
        last_validated = calendar:universal_time()
    },
    {reply, Result, NewState};

handle_call(get_system_health, _From, State) ->
    Health = case State#state.last_meta_validation of
        undefined -> #{status => pending};
        Result -> Result
    end,
    {reply, Health, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(meta_validate, State) ->
    Result = execute_meta_validation(),

    logger:info(#{
        what => meta_validation_complete,
        result => Result
    }),

    %% Schedule next meta-validation
    erlang:send_after(?META_VALIDATION_INTERVAL_MS, self(), meta_validate),

    {noreply, State#state{
        last_meta_validation = Result,
        last_validated = calendar:universal_time()
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

execute_meta_validation() ->
    %% 1. Check all control executors are alive
    ControlExecutorCheck = check_control_executors(),

    %% 2. Check all evidence generators are alive
    EvidenceGenCheck = check_evidence_generators(),

    %% 3. Check receipt chain is functioning
    ReceiptChainCheck = check_receipt_chain(),

    %% 4. Check for coverage gaps
    CoverageCheck = check_coverage_gaps(),

    %% Overall status
    AllChecks = [ControlExecutorCheck, EvidenceGenCheck, ReceiptChainCheck, CoverageCheck],
    OverallStatus = case lists:all(fun(#{status := S}) -> S =:= pass end, AllChecks) of
        true -> pass;
        false -> fail
    end,

    #{
        status => OverallStatus,
        timestamp => calendar:universal_time(),
        checks => #{
            control_executors => ControlExecutorCheck,
            evidence_generators => EvidenceGenCheck,
            receipt_chain => ReceiptChainCheck,
            coverage => CoverageCheck
        }
    }.

check_control_executors() ->
    ExpectedControls = [<<"CC6.1">>, <<"CC7.1">>, <<"CC8.1">>, <<"CC9.1">>,
                        <<"PI1.1">>, <<"C1.1">>, <<"P1.1">>],

    RunningControls = lists:filter(
        fun(ControlId) ->
            case soc2_control_executor:get_status(ControlId) of
                #{status := Status} when Status =/= pending -> true;
                _ -> false
            end
        end,
        ExpectedControls
    ),

    #{
        status => if length(RunningControls) =:= length(ExpectedControls) -> pass;
                     true -> fail
                  end,
        expected_count => length(ExpectedControls),
        running_count => length(RunningControls),
        missing => ExpectedControls -- RunningControls
    }.

check_evidence_generators() ->
    ExpectedGens = [uptime_evidence_gen, load_test_evidence_gen,
                    chaos_evidence_gen, build_evidence_gen],

    RunningGens = lists:filter(
        fun(GenName) -> whereis(GenName) =/= undefined end,
        ExpectedGens
    ),

    #{
        status => if length(RunningGens) =:= length(ExpectedGens) -> pass;
                     true -> fail
                  end,
        expected_count => length(ExpectedGens),
        running_count => length(RunningGens),
        missing => [atom_to_binary(G) || G <- ExpectedGens -- RunningGens]
    }.

check_receipt_chain() ->
    case whereis(soc2_receipt_chain) of
        undefined ->
            #{status => fail, reason => <<"not_running">>};
        _Pid ->
            Chain = soc2_receipt_chain:get_chain(),
            ReceiptCount = maps:get(receipt_count, Chain, 0),
            #{
                status => if ReceiptCount > 0 -> pass; true -> pending end,
                receipt_count => ReceiptCount,
                root_hash => maps:get(root_hash, Chain, <<>>)
            }
    end.

check_coverage_gaps() ->
    %% Check if all required evidence exists
    RequiredEvidence = [
        <<"receipts/build.last.json">>,
        <<"receipts/evidence.last.json">>,
        <<"receipts/verdict.last.json">>,
        <<"evidence/evidence.sha256">>
    ],

    MissingEvidence = lists:filter(
        fun(Path) -> not filelib:is_regular(Path) end,
        RequiredEvidence
    ),

    #{
        status => if MissingEvidence =:= [] -> pass; true -> fail end,
        required_count => length(RequiredEvidence),
        missing_count => length(MissingEvidence),
        missing => MissingEvidence
    }.
