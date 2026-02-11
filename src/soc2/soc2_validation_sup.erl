%%%-------------------------------------------------------------------
%%% @doc
%%% SOC 2 Validation Supervisor
%%%
%%% Top-level supervisor for continuous SOC 2 compliance validation.
%%% Follows Joe Armstrong's "let it crash" philosophy - each control
%%% validator is supervised independently.
%%%
%%% Supervision Strategy:
%%% - one_for_one: If a control validator crashes, only that control
%%%   is restarted. Other controls continue running.
%%% - rest_for_one: Evidence streams and receipt chain depend on
%%%   control executors being stable.
%%%
%%% Design Philosophy (Joe Armstrong):
%%% 1. Make it observable - Every validation produces a receipt
%%% 2. Make it fault-tolerant - Validators crash independently
%%% 3. Make it provable - Receipts form cryptographic chain
%%% 4. Make it continuous - Validators run perpetually
%%% 5. Make it traceable - All evidence is manifest-linked
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_validation_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([get_validation_status/0]).
-export([get_receipt_chain/0]).
-export([validate_control/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Start the SOC 2 validation supervisor tree
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc Get real-time validation status for all controls
-spec get_validation_status() -> #{control_id := #{status := pass | fail, last_validated := calendar:datetime()}}.
get_validation_status() ->
    soc2_control_executor_sup:get_all_statuses().

%% @doc Get the complete receipt chain (Merkle tree)
-spec get_receipt_chain() -> #{root_hash := binary(), receipts := [map()]}.
get_receipt_chain() ->
    soc2_receipt_chain:get_chain().

%% @doc Trigger immediate validation for a specific control
-spec validate_control(binary()) -> {ok, map()} | {error, term()}.
validate_control(ControlId) ->
    soc2_control_executor_sup:validate_now(ControlId).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
init([]) ->
    logger:info(#{
        what => soc2_validation_supervisor_starting,
        philosophy => "Joe Armstrong: Let it crash, but prove what crashed"
    }),

    SupFlags = #{
        strategy => rest_for_one,  % Evidence streams depend on control executors
        intensity => 5,
        period => 60
    },

    %% Child specification order matters (rest_for_one):
    %% 1. Control executors (independent validators)
    %% 2. Evidence stream generators (feed executors)
    %% 3. Receipt chain (accumulates results)
    %% 4. Meta-validator (validates the validators)

    ControlExecutorSup = #{
        id => soc2_control_executor_sup,
        start => {soc2_control_executor_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [soc2_control_executor_sup]
    },

    EvidenceStreamSup = #{
        id => soc2_evidence_stream_sup,
        start => {soc2_evidence_stream_sup, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => supervisor,
        modules => [soc2_evidence_stream_sup]
    },

    ReceiptChain = #{
        id => soc2_receipt_chain,
        start => {soc2_receipt_chain, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [soc2_receipt_chain]
    },

    MetaValidator = #{
        id => soc2_meta_validator,
        start => {soc2_meta_validator, start_link, []},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [soc2_meta_validator]
    },

    ChildSpecs = [
        ControlExecutorSup,
        EvidenceStreamSup,
        ReceiptChain,
        MetaValidator
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
