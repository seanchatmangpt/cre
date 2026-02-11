%%%-------------------------------------------------------------------
%%% @doc
%%% SOC 2 Control Executor (Generic)
%%%
%%% Each instance validates ONE SOC 2 control continuously.
%%% Produces validation receipts on each run.
%%%
%%% Joe Armstrong Design Principles:
%%% 1. Fail fast - If validation logic crashes, supervisor restarts
%%% 2. Observable - Every validation produces a receipt
%%% 3. Traceable - Receipts include timestamps, hashes, evidence links
%%% 4. Deterministic - Same inputs produce same receipts
%%% 5. Continuous - Runs on interval (default: 5 minutes)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_control_executor).
-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([validate_now/1]).
-export([get_status/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(VALIDATION_INTERVAL_MS, 300_000).  % 5 minutes

-record(state, {
    control_id :: binary(),
    validation_spec :: map(),
    last_receipt :: map() | undefined,
    last_status :: pass | fail | pending,
    last_validated :: calendar:datetime() | undefined,
    validation_count :: non_neg_integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(binary()) -> {ok, pid()} | {error, term()}.
start_link(ControlId) ->
    gen_server:start_link({local, executor_name(ControlId)}, ?MODULE, [ControlId], []).

-spec validate_now(pid()) -> {ok, map()} | {error, term()}.
validate_now(Pid) ->
    gen_server:call(Pid, validate_now, 30_000).

-spec get_status(binary()) -> #{status := pass | fail | pending, last_validated := calendar:datetime() | undefined}.
get_status(ControlId) ->
    case whereis(executor_name(ControlId)) of
        undefined ->
            #{status => pending, last_validated => undefined};
        Pid ->
            gen_server:call(Pid, get_status)
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ControlId]) ->
    %% Load validation spec from ontology
    ValidationSpec = load_validation_spec(ControlId),

    logger:info(#{
        what => soc2_control_executor_started,
        control_id => ControlId,
        validation_spec => ValidationSpec
    }),

    %% Schedule first validation
    erlang:send_after(5000, self(), validate),

    {ok, #state{
        control_id = ControlId,
        validation_spec = ValidationSpec,
        last_receipt = undefined,
        last_status = pending,
        last_validated = undefined,
        validation_count = 0
    }}.

handle_call(validate_now, _From, State) ->
    {Receipt, NewState} = execute_validation(State),
    {reply, {ok, Receipt}, NewState};

handle_call(get_status, _From, State) ->
    Status = #{
        status => State#state.last_status,
        last_validated => State#state.last_validated,
        validation_count => State#state.validation_count
    },
    {reply, Status, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(validate, State) ->
    {_Receipt, NewState} = execute_validation(State),

    %% Schedule next validation
    erlang:send_after(?VALIDATION_INTERVAL_MS, self(), validate),

    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

executor_name(ControlId) ->
    %% e.g., <<"CC6.1">> -> soc2_control_executor_cc6_1
    SafeId = binary:replace(ControlId, <<".">>, <<"_">>, [global]),
    binary_to_atom(<<"soc2_control_executor_", (string:lowercase(SafeId))/binary>>).

load_validation_spec(ControlId) ->
    %% In production, this would query the ontology (soc2.ttl)
    %% For now, return a spec map
    #{
        control_id => ControlId,
        validators => get_validators_for_control(ControlId),
        evidence_requirements => get_evidence_requirements(ControlId)
    }.

get_validators_for_control(<<"CC6.1">>) -> [<<"access_control_validator">>];
get_validators_for_control(<<"CC7.1">>) -> [<<"detection_validator">>];
get_validators_for_control(<<"CC8.1">>) -> [<<"change_management_validator">>];
get_validators_for_control(<<"CC9.1">>) -> [<<"risk_mitigation_validator">>];
get_validators_for_control(<<"PI1.1">>) -> [<<"processing_quality_validator">>];
get_validators_for_control(<<"C1.1">>) -> [<<"confidentiality_validator">>];
get_validators_for_control(<<"P1.1">>) -> [<<"privacy_validator">>];
get_validators_for_control(_) -> [].

get_evidence_requirements(<<"CC6.1">>) -> [<<"receipts/build.last.json">>];
get_evidence_requirements(<<"CC7.1">>) -> [<<"receipts/evidence.last.json">>];
get_evidence_requirements(<<"CC8.1">>) -> [<<"receipts/build.last.json">>];
get_evidence_requirements(<<"CC9.1">>) -> [<<"receipts/verdict.last.json">>];
get_evidence_requirements(<<"PI1.1">>) -> [<<"evidence/evidence.sha256">>];
get_evidence_requirements(<<"C1.1">>) -> [<<"evidence/evidence.sha256">>];
get_evidence_requirements(<<"P1.1">>) -> [<<"receipts/verdict.last.json">>];
get_evidence_requirements(_) -> [].

execute_validation(State) ->
    ControlId = State#state.control_id,
    ValidationSpec = State#state.validation_spec,

    logger:debug(#{
        what => executing_validation,
        control_id => ControlId
    }),

    %% Execute validators
    ValidatorResults = lists:map(
        fun(ValidatorId) ->
            run_validator(ValidatorId, ValidationSpec)
        end,
        maps:get(validators, ValidationSpec, [])
    ),

    %% Check evidence existence
    EvidenceResults = lists:map(
        fun(EvidencePath) ->
            check_evidence(EvidencePath)
        end,
        maps:get(evidence_requirements, ValidationSpec, [])
    ),

    %% Determine overall status
    Status = case {all_passed(ValidatorResults), all_passed(EvidenceResults)} of
        {true, true} -> pass;
        _ -> fail
    end,

    %% Generate receipt
    Receipt = #{
        control_id => ControlId,
        status => Status,
        timestamp => calendar:universal_time(),
        validator_results => ValidatorResults,
        evidence_results => EvidenceResults,
        receipt_hash => <<>>  % Will be computed by receipt chain
    },

    %% Submit to receipt chain
    ok = soc2_receipt_chain:append_receipt(Receipt),

    NewState = State#state{
        last_receipt = Receipt,
        last_status = Status,
        last_validated = calendar:universal_time(),
        validation_count = State#state.validation_count + 1
    },

    {Receipt, NewState}.

run_validator(ValidatorId, _ValidationSpec) ->
    %% In production, this would call the actual validator module
    %% For now, simulate validation
    #{
        validator_id => ValidatorId,
        status => pass,
        timestamp => calendar:universal_time()
    }.

check_evidence(EvidencePath) ->
    %% Check if evidence file exists and is valid
    case filelib:is_regular(EvidencePath) of
        true ->
            #{
                evidence_path => EvidencePath,
                status => pass,
                exists => true
            };
        false ->
            #{
                evidence_path => EvidencePath,
                status => fail,
                exists => false
            }
    end.

all_passed(Results) ->
    lists:all(
        fun(#{status := Status}) -> Status =:= pass end,
        Results
    ).
