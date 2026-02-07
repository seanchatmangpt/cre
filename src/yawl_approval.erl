%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Human-in-the-Loop Workflow Approval Checkpoints
%%
%% This module provides approval checkpoint management for YAWL workflow
%% execution, enabling workflows to pause at designated checkpoints and
%% require human approval (simulated or real) before continuing.
%%
%% @author CRE Team
%% @version 1.0.0
%% @doc YAWL Approval Checkpoint Management
%%
%% <h3>Overview</h3>
%% This module implements approval checkpoints for workflow execution:
%% <ul>
%%   <li>Pause workflow execution at designated checkpoints</li>
%%   <li>Request approval from human or simulated LLM approver</li>
%%   <li>Block until decision is received or timeout expires</li>
%%   <li>Log all approvals to XES for audit trail</li>
%% </ul>
%%
%% <h3>Approval Types</h3>
%% <ul>
%%   <li><b>human:</b> Requires manual human approval via API or CLI</li>
%%   <li><b>simulated:</b> Uses Claude Code headless mode for LLM approval</li>
%%   <li><b>auto:</b> Auto-approves based on configurable rules</li>
%% </ul>
%% @end
%% -------------------------------------------------------------------

-module(yawl_approval).
-author("CRE Team").

-moduledoc """
YAWL Approval Checkpoint Management.

This module implements approval checkpoints for workflow execution, enabling
workflows to pause at designated checkpoints and require human approval
(simulated or real) before continuing.

## Approval Types

* **human** - Requires manual human approval via API or CLI
* **simulated** - Uses Claude Code headless mode for LLM approval
* **auto** - Auto-approves based on configurable rules

## Checkpoint ID Format

All checkpoint IDs are prefixed binary identifiers:

```erlang
% Generate a checkpoint ID (internal function)
Id = yawl_approval:generate_checkpoint_id(),
binary_part(Id, {0, 8}).
<<"approval_">
```

## Approval Status Values

```erlang
% Valid status atoms
Statuses = [pending, approved, denied, timeout, cancelled],
length(Statuses).
5
```

## Approver Types

```erlang
% Valid approver types
ApproverTypes = [human, simulated, auto],
length(ApproverTypes).
3
```

## Decision Record

The approval_decision record contains:

```erlang
% Record fields (for reference)
Rec = #approval_decision{
    checkpoint_id = <<"test">>,
    approved = true,
    decision_maker = simulated,
    reason = <<"OK">>,
    metadata = #{},
    decided_at = 1234567890
},
Rec#approval_decision.approved.
true
```
""".

-behaviour(gen_server).

%% Include record definitions
-include("cre_yawl.hrl").
-include("cre_yawl_patterns.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% Gen Server Callbacks
-export([start_link/0, init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% Main API
-export([
    create_checkpoint/3,
    request_approval/1,
    approve/3,
    deny/3,
    check_status/1,
    wait_for_approval/1,
    simulate_approval/2,
    list_pending/0,
    list_all/0,
    cancel_checkpoint/1,
    get_checkpoint_context/1,
    get_receipt/1,
    list_receipts/0,
    generate_checkpoint_id/0
]).

%%====================================================================
%% Types
%%====================================================================

-type checkpoint_id() :: binary().
-type approver_type() :: human | simulated | auto.
-type approval_status() :: pending | approved | denied | timeout | cancelled.

%% Records are defined in cre_yawl.hrl
%% -record(approval_checkpoint, {...}).
%% -record(approval_decision, {...}).

-record(state, {
    checkpoints :: #{checkpoint_id() => #approval_checkpoint{}},
    decisions :: #{checkpoint_id() => #approval_decision{}},
    waiters :: #{checkpoint_id() => [pid()]},
    receipts :: #{checkpoint_id() => pnet_receipt:receipt()}
}).

-type approval_result() :: {ok, #approval_decision{}} | {error, term()}.

-export_type([checkpoint_id/0, approver_type/0, approval_status/0]).

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    logger:info("Starting YAWL Approval Server", [{module, ?MODULE}]),
    %% Initialize ETS table for cross-process access to checkpoints and decisions
    %% This allows external processes (like approval_worker.sh) to access state
    ets:new(?MODULE, [named_table, public, {read_concurrency, true}]),
    %% Initialize ETS table for audit receipts
    ets:new(yawl_approval_receipts, [named_table, public, {read_concurrency, true}]),
    {ok, #state{
        checkpoints = #{},
        decisions = #{},
        waiters = #{},
        receipts = #{}
    }}.

handle_call({create_checkpoint, PatternId, StepName, Context, Options}, _From, State) ->
    {Reply, NewState} = do_create_checkpoint(PatternId, StepName, Context, Options, State),
    {reply, Reply, NewState};

handle_call({request_approval, CheckpointId}, _From, State) ->
    {Reply, NewState} = do_request_approval(CheckpointId, State),
    {reply, Reply, NewState};

handle_call({approve, CheckpointId, Approver, Reason}, _From, State) ->
    {Reply, NewState} = do_approve(CheckpointId, Approver, Reason, State),
    {reply, Reply, NewState};

handle_call({deny, CheckpointId, Approver, Reason}, _From, State) ->
    {Reply, NewState} = do_deny(CheckpointId, Approver, Reason, State),
    {reply, Reply, NewState};

handle_call({check_status, CheckpointId}, _From, State) ->
    Reply = do_check_status(CheckpointId, State),
    {reply, Reply, State};

handle_call(list_pending, _From, State) ->
    Reply = do_list_pending(State),
    {reply, Reply, State};

handle_call(list_all, _From, State) ->
    Reply = do_list_all(State),
    {reply, Reply, State};

handle_call({cancel_checkpoint, CheckpointId}, _From, State) ->
    {Reply, NewState} = do_cancel_checkpoint(CheckpointId, State),
    {reply, Reply, NewState};

handle_call({get_receipt, CheckpointId}, _From, State) ->
    Reply = do_get_receipt(CheckpointId, State),
    {reply, Reply, State};

handle_call(list_receipts, _From, State) ->
    Reply = do_list_receipts(State),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({add_waiter, CheckpointId, Pid}, State) ->
    NewState = add_waiter(CheckpointId, Pid, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, CheckpointId}, State) ->
    handle_timeout(CheckpointId, State);

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new approval checkpoint.
%%
%% @param PatternId The ID of the pattern requiring approval.
%% @param StepName The name of the step requiring approval.
%% @param Options Map of approval options.
%% @return {ok, CheckpointId} or {error, Reason}
%%
%% Options:
%%   - required_approver: human | simulated | auto (default: simulated)
%%   - timeout: milliseconds or infinity (default: 30000)
%%   - approval_schema: JSON schema for validation (default: basic schema)
%%   - context: Additional context data (default: #{})
%%   - metadata: User metadata (default: #{})
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Creates a new approval checkpoint with the specified options.

Options:
  - required_approver: human | simulated | auto (default: simulated)
  - timeout: milliseconds or infinity (default: 30000)
  - approval_schema: JSON schema for validation (default: basic schema)
  - context: Additional context data (default: #{})
  - metadata: User metadata (default: #{})

```erlang
% Generate a checkpoint ID to see the format
Id = yawl_approval:generate_checkpoint_id(),
binary_part(Id, {0, 8}).
<<"approval_">
```
""".
-spec create_checkpoint(binary(), atom(), map()) ->
    {ok, checkpoint_id()} | {error, term()}.

create_checkpoint(PatternId, StepName, Options) ->
    gen_server:call(?MODULE, {create_checkpoint, PatternId, StepName,
                              maps:get(context, Options, #{}), Options}).

%%--------------------------------------------------------------------
%% @doc Requests approval for a checkpoint.
%%
%% This pauses workflow execution and waits for approval.
%% For 'simulated' approval, invokes Claude Code headless mode.
%% For 'human' approval, waits for external approval via approve/2.
%%
%% @param CheckpointId The checkpoint ID.
%% @return {ok, Decision} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec request_approval(checkpoint_id()) -> approval_result().

request_approval(CheckpointId) ->
    gen_server:call(?MODULE, {request_approval, CheckpointId}).

%%--------------------------------------------------------------------
%% @doc Approves a checkpoint.
%%
%% @param CheckpointId The checkpoint ID.
%% @param Approver The approver identifier (pid, name, or simulated).
%% @param Reason The approval reason.
%% @return ok or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Approves a pending checkpoint, allowing the workflow to continue.

```erlang
% Approval status values
Status = approved,
Status = approved.
true
```
""".
-spec approve(checkpoint_id(), term(), binary()) -> ok | {error, term()}.

approve(CheckpointId, Approver, Reason) ->
    gen_server:call(?MODULE, {approve, CheckpointId, Approver, Reason}).

%%--------------------------------------------------------------------
%% @doc Denies a checkpoint.
%%
%% @param CheckpointId The checkpoint ID.
%% @param Approver The approver identifier.
%% @param Reason The denial reason.
%% @return ok or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Denies a pending checkpoint, blocking the workflow from continuing.

```erlang
% Denial status value
Status = denied,
Status = denied.
true
```
""".
-spec deny(checkpoint_id(), term(), binary()) -> ok | {error, term()}.

deny(CheckpointId, Approver, Reason) ->
    gen_server:call(?MODULE, {deny, CheckpointId, Approver, Reason}).

%%--------------------------------------------------------------------
%% @doc Checks the status of an approval checkpoint.
%%
%% @param CheckpointId The checkpoint ID.
%% @return {ok, Status} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Checks the status of an approval checkpoint.

Status can be: pending, approved, denied, timeout, or cancelled.

```erlang
% All possible status values
Statuses = [pending, approved, denied, timeout, cancelled],
lists:member(pending, Statuses).
true
```
""".
-spec check_status(checkpoint_id()) -> {ok, approval_status()} | {error, term()}.

check_status(CheckpointId) ->
    gen_server:call(?MODULE, {check_status, CheckpointId}).

%%--------------------------------------------------------------------
%% @doc Blocks until a decision is made or timeout expires.
%%
%% @param CheckpointId The checkpoint ID.
%% @return {ok, Decision} or {error, timeout | not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_for_approval(checkpoint_id()) -> approval_result().

wait_for_approval(CheckpointId) ->
    case gen_server:call(?MODULE, {check_status, CheckpointId}) of
        {ok, Status} when Status =:= approved; Status =:= denied ->
            case get_decision(CheckpointId) of
                {ok, Decision} -> {ok, Decision};
                Error -> Error
            end;
        {ok, pending} ->
            %% Add ourselves as a waiter
            gen_server:cast(?MODULE, {add_waiter, CheckpointId, self()}),
            receive
                {approval_decision, CheckpointId, Decision} ->
                    {ok, Decision};
                {approval_timeout, CheckpointId} ->
                    {error, timeout}
            after
                %% Safety timeout - won't wait forever
                3600000 -> {error, timeout}
            end;
        {ok, timeout} ->
            {error, timeout};
        Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Simulates approval using Claude Code headless mode.
%%
%% @param CheckpointId The checkpoint ID.
%% @param PromptContext Additional context for the approval prompt.
%% @return {ok, Decision} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec simulate_approval(checkpoint_id(), map()) -> approval_result().

simulate_approval(CheckpointId, PromptContext) ->
    %% Get checkpoint details
    case gen_server:call(?MODULE, {check_status, CheckpointId}) of
        {ok, pending} ->
            case get_checkpoint(CheckpointId) of
                {ok, Checkpoint} ->
                    Prompt = generate_approval_prompt(Checkpoint, PromptContext),
                    case yawl_claude_bridge:prompt_claude(Prompt,
                            Checkpoint#approval_checkpoint.approval_schema) of
                        {ok, Response} ->
                            Approved = maps:get(<<"approved">>, Response, false),
                            Reason = maps:get(<<"reason">>, Response, <<>>),
                            DecisionMaker = simulated,
                            %% Use approve/deny instead of record_decision to ensure
                            %% proper ETS storage and waiter notification
                            case Approved of
                                true ->
                                    approve(CheckpointId, DecisionMaker, Reason);
                                false ->
                                    deny(CheckpointId, DecisionMaker, Reason)
                            end,
                            %% Return the decision
                            {ok, #approval_decision{
                                checkpoint_id = CheckpointId,
                                approved = Approved,
                                decision_maker = DecisionMaker,
                                reason = Reason,
                                metadata = maps:get(metadata, Response, #{}),
                                decided_at = erlang:system_time(millisecond)
                            }};
                        {error, Reason} ->
                            %% On Claude error, deny with error reason
                            deny(CheckpointId, simulated,
                                <<"Claude error: ", (to_binary(Reason))/binary>>),
                            {error, {claude_error, Reason}}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        {ok, Status} when Status =:= approved; Status =:= denied ->
            get_decision(CheckpointId);
        {ok, timeout} ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all pending approval checkpoints.
%%
%% @return List of pending checkpoint IDs.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Lists all pending approval checkpoints.

Returns a list of checkpoint IDs that are awaiting approval.

```erlang
% Returns empty list when no checkpoints exist
Pending = [],
is_list(Pending).
true
```
""".
-spec list_pending() -> [checkpoint_id()].

list_pending() ->
    gen_server:call(?MODULE, list_pending).

%%--------------------------------------------------------------------
%% @doc Lists all approval checkpoints.
%%
%% @return List of all checkpoint IDs with their statuses.
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Lists all approval checkpoints with their statuses.

Returns a list of {CheckpointId, Status} tuples.

```erlang
% Status tuples are of this form
StatusTuple = {<<"checkpoint_id">>, approved},
element(1, StatusTuple).
<<"checkpoint_id">
```
""".
-spec list_all() -> [{checkpoint_id(), approval_status()}].

list_all() ->
    gen_server:call(?MODULE, list_all).

%%--------------------------------------------------------------------
%% @doc Cancels a pending approval checkpoint.
%%
%% @param CheckpointId The checkpoint ID.
%% @return ok or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Cancels a pending approval checkpoint.

```erlang
% Cancelled is a valid status
Status = cancelled,
Status = cancelled.
true
```
""".
-spec cancel_checkpoint(checkpoint_id()) -> ok | {error, term()}.

cancel_checkpoint(CheckpointId) ->
    gen_server:call(?MODULE, {cancel_checkpoint, CheckpointId}).

%%--------------------------------------------------------------------
%% @doc Gets the checkpoint context as a JSON string.
%%
%% This function is used by external processes (e.g., approval_worker.sh)
%% to retrieve the context for approval decision-making.
%%
%% @param CheckpointId The checkpoint ID.
%% @return JSON string representation of the checkpoint context.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_checkpoint_context(checkpoint_id()) -> binary().

get_checkpoint_context(CheckpointId) ->
    case gen_server:call(?MODULE, {check_status, CheckpointId}) of
        {error, not_found} ->
            <<"{}">>;
        Status ->
            %% Get checkpoint info from ETS for cross-process access
            case ets:lookup(?MODULE, {checkpoint, CheckpointId}) of
                [{{checkpoint, CheckpointId}, Checkpoint}] ->
                    ContextMap = #{
                        <<"checkpoint_id">> => Checkpoint#approval_checkpoint.checkpoint_id,
                        <<"pattern_id">> => to_binary(Checkpoint#approval_checkpoint.pattern_id),
                        <<"step_name">> => atom_to_binary(Checkpoint#approval_checkpoint.step_name, utf8),
                        <<"context">> => Checkpoint#approval_checkpoint.context,
                        <<"required_approver">> => atom_to_binary(Checkpoint#approval_checkpoint.required_approver, utf8),
                        <<"status">> => atom_to_binary(element(2, Status), utf8),
                        <<"created_at">> => Checkpoint#approval_checkpoint.created_at
                    },
                    FinalMap = case Checkpoint#approval_checkpoint.expires_at of
                        undefined -> ContextMap;
                        ExpiresAt -> ContextMap#{<<"expires_at">> => ExpiresAt}
                    end,
                    context_to_json(FinalMap);
                [] ->
                    <<"{}">>
            end
    end.

%%--------------------------------------------------------------------
%% @doc Gets the audit receipt for a checkpoint decision.
%%
%% This function retrieves the pnet_receipt for audit compliance,
%% providing a verifiable record of the approval decision.
%%
%% @param CheckpointId The checkpoint ID.
%% @return {ok, Receipt} or {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Gets the audit receipt for a checkpoint decision.

The receipt contains before_hash, after_hash, move (with trsn, mode, produce),
and timestamp (ts) for audit compliance.

```erlang
% Receipt structure
Receipt = #{
    before_hash => <<123, 456>>,
    after_hash => <<789, 012>>,
    move => #{trsn => test},
    ts => 1234567890
},
is_map(Receipt).
true
```
""".
-spec get_receipt(checkpoint_id()) -> {ok, pnet_receipt:receipt()} | {error, term()}.

get_receipt(CheckpointId) ->
    gen_server:call(?MODULE, {get_receipt, CheckpointId}).

%%--------------------------------------------------------------------
%% @doc Lists all audit receipts for approval decisions.
%%
%% This function returns all stored receipts for audit compliance
%% and verification purposes.
%%
%% @return List of {CheckpointId, Receipt} tuples.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_receipts() -> [{checkpoint_id(), pnet_receipt:receipt()}].

list_receipts() ->
    gen_server:call(?MODULE, list_receipts).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Creates a new approval checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
do_create_checkpoint(PatternId, StepName, Context, Options, State) ->
    CheckpointId = generate_checkpoint_id(),
    RequiredApprover = maps:get(required_approver, Options, simulated),
    Timeout = maps:get(timeout, Options, 30000),
    DefaultSchema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"approved">> => #{<<"type">> => <<"boolean">>},
            <<"reason">> => #{<<"type">> => <<"string">>}
        },
        <<"required">> => [<<"approved">>]
    },
    ApprovalSchema = maps:get(approval_schema, Options, DefaultSchema),
    CreatedAt = erlang:system_time(millisecond),

    ExpiresAt = case Timeout of
        infinity -> undefined;
        _ -> CreatedAt + Timeout
    end,

    Checkpoint = #approval_checkpoint{
        checkpoint_id = CheckpointId,
        pattern_id = PatternId,
        step_name = StepName,
        context = Context,
        required_approver = RequiredApprover,
        timeout = Timeout,
        approval_schema = ApprovalSchema,
        created_at = CreatedAt,
        expires_at = ExpiresAt,
        metadata = maps:get(metadata, Options, #{})
    },

    %% Schedule timeout if not infinity
    case Timeout of
        infinity -> ok;
        _ -> erlang:send_after(Timeout, self(), {timeout, CheckpointId})
    end,

    %% Log checkpoint creation to XES
    log_checkpoint_event(Checkpoint, created),

    %% Store in ETS for cross-process access
    ets:insert(?MODULE, {{checkpoint, CheckpointId}, Checkpoint}),

    NewCheckpoints = maps:put(CheckpointId, Checkpoint, State#state.checkpoints),
    {{ok, CheckpointId}, State#state{checkpoints = NewCheckpoints}}.

%%--------------------------------------------------------------------
%% @private
%% @doc Requests approval for a checkpoint.
%%
%% Creates an enabled task token using wf_task:enabled/3 when approval
%% is requested, signaling the workflow is waiting for human decision.
%%
%% @end
%%--------------------------------------------------------------------
do_request_approval(CheckpointId, State) ->
    case maps:get(CheckpointId, State#state.checkpoints, undefined) of
        undefined ->
            {{error, not_found}, State};
        Checkpoint ->
            case Checkpoint#approval_checkpoint.required_approver of
                auto ->
                    %% Auto-approve based on rules
                    Decision = #approval_decision{
                        checkpoint_id = CheckpointId,
                        approved = true,
                        decision_maker = auto,
                        reason = <<"Auto-approved">>,
                        metadata = #{},
                        decided_at = erlang:system_time(millisecond)
                    },
                    NewDecisions = maps:put(CheckpointId, Decision, State#state.decisions),
                    %% Store in ETS for cross-process access
                    ets:insert(?MODULE, {{decision, CheckpointId}, Decision}),

                    %% Create done task token for auto-approval
                    _DoneToken = wf_task:done(CheckpointId, Decision, 'p_approval_complete'),

                    %% Create audit receipt for compliance
                    BeforeHash = crypto:hash(sha256, term_to_binary({checkpoint, Checkpoint})),
                    AfterHash = crypto:hash(sha256, term_to_binary({decision, Decision})),
                    Move = #{
                        trsn => auto_approve,
                        mode => #{checkpoint => CheckpointId, approver => auto},
                        produce => #{'p_approval_complete' => [{task, CheckpointId, done, Decision}]}
                    },
                    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
                    ets:insert(yawl_approval_receipts, {{receipt, CheckpointId}, Receipt}),

                    notify_waiters(CheckpointId, Decision, State#state.waiters),
                    log_checkpoint_event(Checkpoint, auto_approved),
                    NewState = State#state{decisions = NewDecisions,
                                          receipts = maps:put(CheckpointId, Receipt, State#state.receipts)},
                    {{ok, Decision}, NewState};
                simulated ->
                    %% Create enabled task token for approval workflow
                    _Token = wf_task:enabled(CheckpointId,
                                             Checkpoint#approval_checkpoint.context,
                                             'p_approval_pending'),
                    %% Spawn async process to handle simulated approval
                    spawn(fun() ->
                        Result = simulate_approval(CheckpointId, #{}),
                        case Result of
                            {ok, _Decision} -> ok;
                            {error, _} -> ok
                        end
                    end),
                    {{ok, pending_approval}, State};
                human ->
                    %% Create enabled task token for human approval
                    _Token = wf_task:enabled(CheckpointId,
                                             Checkpoint#approval_checkpoint.context,
                                             'p_approval_pending'),
                    %% Just mark as pending, wait for external approve/deny call
                    log_checkpoint_event(Checkpoint, awaiting_human),
                    {{ok, awaiting_human}, State}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Approves a checkpoint.
%%
%% Creates a done task token using wf_task:done/3 when approved,
%% and generates an audit receipt using pnet_receipt:make/3.
%%
%% @end
%%--------------------------------------------------------------------
do_approve(CheckpointId, Approver, Reason, State) ->
    case maps:get(CheckpointId, State#state.checkpoints, undefined) of
        undefined ->
            {{error, not_found}, State};
        Checkpoint ->
            case maps:get(CheckpointId, State#state.decisions, undefined) of
                undefined ->
                    Decision = #approval_decision{
                        checkpoint_id = CheckpointId,
                        approved = true,
                        decision_maker = Approver,
                        reason = to_binary(Reason),
                        metadata = #{},
                        decided_at = erlang:system_time(millisecond)
                    },
                    NewDecisions = maps:put(CheckpointId, Decision, State#state.decisions),
                    %% Store in ETS for cross-process access
                    ets:insert(?MODULE, {{decision, CheckpointId}, Decision}),

                    %% Create done task token for approval workflow
                    _DoneToken = wf_task:done(CheckpointId, Decision, 'p_approval_complete'),

                    %% Create audit receipt for compliance
                    BeforeHash = crypto:hash(sha256, term_to_binary({checkpoint, Checkpoint})),
                    AfterHash = crypto:hash(sha256, term_to_binary({decision, Decision})),
                    Move = #{
                        trsn => approve,
                        mode => #{checkpoint => CheckpointId, approver => Approver},
                        produce => #{'p_approval_complete' => [{task, CheckpointId, done, Decision}]}
                    },
                    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
                    ets:insert(yawl_approval_receipts, {{receipt, CheckpointId}, Receipt}),

                    notify_waiters(CheckpointId, Decision, State#state.waiters),
                    log_checkpoint_event(Checkpoint, approved),
                    NewState = State#state{decisions = NewDecisions,
                                          receipts = maps:put(CheckpointId, Receipt, State#state.receipts)},
                    {ok, NewState};
                _Existing ->
                    {{error, already_decided}, State}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Denies a checkpoint.
%%
%% Creates a failed task token using wf_task:failed/3 when rejected,
%% and generates an audit receipt using pnet_receipt:make/3.
%%
%% @end
%%--------------------------------------------------------------------
do_deny(CheckpointId, Approver, Reason, State) ->
    case maps:get(CheckpointId, State#state.checkpoints, undefined) of
        undefined ->
            {{error, not_found}, State};
        Checkpoint ->
            case maps:get(CheckpointId, State#state.decisions, undefined) of
                undefined ->
                    Decision = #approval_decision{
                        checkpoint_id = CheckpointId,
                        approved = false,
                        decision_maker = Approver,
                        reason = to_binary(Reason),
                        metadata = #{},
                        decided_at = erlang:system_time(millisecond)
                    },
                    NewDecisions = maps:put(CheckpointId, Decision, State#state.decisions),
                    %% Store in ETS for cross-process access
                    ets:insert(?MODULE, {{decision, CheckpointId}, Decision}),

                    %% Create failed task token for approval workflow
                    _FailedToken = wf_task:failed(CheckpointId, Reason, 'p_approval_failed'),

                    %% Create audit receipt for compliance
                    BeforeHash = crypto:hash(sha256, term_to_binary({checkpoint, Checkpoint})),
                    AfterHash = crypto:hash(sha256, term_to_binary({decision, Decision})),
                    Move = #{
                        trsn => deny,
                        mode => #{checkpoint => CheckpointId, approver => Approver},
                        produce => #{'p_approval_failed' => [{task, CheckpointId, failed, Reason}]}
                    },
                    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
                    ets:insert(yawl_approval_receipts, {{receipt, CheckpointId}, Receipt}),

                    notify_waiters(CheckpointId, Decision, State#state.waiters),
                    log_checkpoint_event(Checkpoint, denied),
                    NewState = State#state{decisions = NewDecisions,
                                          receipts = maps:put(CheckpointId, Receipt, State#state.receipts)},
                    {ok, NewState};
                _Existing ->
                    {{error, already_decided}, State}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks the status of an approval checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
do_check_status(CheckpointId, State) ->
    case maps:get(CheckpointId, State#state.decisions, undefined) of
        undefined ->
            case maps:get(CheckpointId, State#state.checkpoints, undefined) of
                undefined -> {error, not_found};
                Checkpoint ->
                    case Checkpoint#approval_checkpoint.expires_at of
                        undefined -> {ok, pending};
                        ExpiresAt ->
                            Now = erlang:system_time(millisecond),
                            case Now >= ExpiresAt of
                                true -> {ok, timeout};
                                false -> {ok, pending}
                            end
                    end
            end;
        #approval_decision{approved = true} -> {ok, approved};
        #approval_decision{approved = false} -> {ok, denied}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Lists all pending checkpoints.
%%
%% @end
%%--------------------------------------------------------------------
do_list_pending(State) ->
    Now = erlang:system_time(millisecond),
    PendingIds = [Id || {Id, Checkpoint} <- maps:to_list(State#state.checkpoints),
                        not maps:is_key(Id, State#state.decisions) andalso
                        (Checkpoint#approval_checkpoint.expires_at =:= undefined orelse
                         Now < Checkpoint#approval_checkpoint.expires_at)],
    PendingIds.

%%--------------------------------------------------------------------
%% @private
%% @doc Lists all checkpoints with their statuses.
%%
%% @end
%%--------------------------------------------------------------------
do_list_all(State) ->
    Now = erlang:system_time(millisecond),
    AllStatuses = lists:map(fun({Id, _Checkpoint}) ->
        case maps:get(Id, State#state.decisions, undefined) of
            undefined ->
                case maps:get(Id, State#state.checkpoints, undefined) of
                    undefined -> {Id, not_found};
                    Checkpoint ->
                        case Checkpoint#approval_checkpoint.expires_at of
                            undefined -> {Id, pending};
                            ExpiresAt ->
                                case Now >= ExpiresAt of
                                    true -> {Id, timeout};
                                    false -> {Id, pending}
                                end
                        end
                end;
            #approval_decision{approved = true} -> {Id, approved};
            #approval_decision{approved = false} -> {Id, denied}
        end
    end, maps:to_list(State#state.checkpoints)),
    AllStatuses.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the audit receipt for a checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
do_get_receipt(CheckpointId, _State) ->
    case ets:whereis(yawl_approval_receipts) of
        undefined -> {error, receipts_table_not_found};
        _Table ->
            case ets:lookup(yawl_approval_receipts, {receipt, CheckpointId}) of
                [{{receipt, CheckpointId}, Receipt}] -> {ok, Receipt};
                [] -> {error, not_found}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Lists all audit receipts.
%%
%% @end
%%--------------------------------------------------------------------
do_list_receipts(_State) ->
    case ets:whereis(yawl_approval_receipts) of
        undefined -> [];
        _Table ->
            ets:foldl(fun({{receipt, CheckpointId}, Receipt}, Acc) ->
                [{CheckpointId, Receipt} | Acc]
            end, [], yawl_approval_receipts)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Cancels a checkpoint.
%%
%% Creates a cancelled task token using wf_task:cancelled/3 and
%% generates an audit receipt using pnet_receipt:make/3.
%%
%% @end
%%--------------------------------------------------------------------
do_cancel_checkpoint(CheckpointId, State) ->
    case maps:get(CheckpointId, State#state.checkpoints, undefined) of
        undefined ->
            {{error, not_found}, State};
        Checkpoint ->
            case maps:get(CheckpointId, State#state.decisions, undefined) of
                undefined ->
                    Decision = #approval_decision{
                        checkpoint_id = CheckpointId,
                        approved = false,
                        decision_maker = cancelled,
                        reason = <<"Checkpoint cancelled">>,
                        metadata = #{},
                        decided_at = erlang:system_time(millisecond)
                    },
                    NewDecisions = maps:put(CheckpointId, Decision, State#state.decisions),
                    %% Store in ETS for cross-process access
                    ets:insert(?MODULE, {{decision, CheckpointId}, Decision}),

                    %% Create cancelled task token for approval workflow
                    _CancelledToken = wf_task:cancelled(CheckpointId, cancelled, 'p_approval_cancelled'),

                    %% Create audit receipt for compliance
                    BeforeHash = crypto:hash(sha256, term_to_binary({checkpoint, Checkpoint})),
                    AfterHash = crypto:hash(sha256, term_to_binary({decision, Decision})),
                    Move = #{
                        trsn => cancel,
                        mode => #{checkpoint => CheckpointId},
                        produce => #{'p_approval_cancelled' => [{task, CheckpointId, cancelled, cancelled}]}
                    },
                    Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
                    ets:insert(yawl_approval_receipts, {{receipt, CheckpointId}, Receipt}),

                    notify_waiters(CheckpointId, Decision, State#state.waiters),
                    log_checkpoint_event(Checkpoint, cancelled),
                    NewState = State#state{decisions = NewDecisions,
                                          receipts = maps:put(CheckpointId, Receipt, State#state.receipts)},
                    {ok, NewState};
                _Existing ->
                    {{error, already_decided}, State}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles checkpoint timeout.
%%
%% Creates a cancelled task token using wf_task:cancelled/3 and
%% generates an audit receipt using pnet_receipt:make/3.
%%
%% @end
%%--------------------------------------------------------------------
handle_timeout(CheckpointId, State) ->
    Checkpoint = maps:get(CheckpointId, State#state.checkpoints, undefined),
    case maps:get(CheckpointId, State#state.decisions, undefined) of
        undefined when Checkpoint =/= undefined ->
            Decision = #approval_decision{
                checkpoint_id = CheckpointId,
                approved = false,
                decision_maker = timeout,
                reason = <<"Approval timeout">>,
                metadata = #{},
                decided_at = erlang:system_time(millisecond)
            },
            NewDecisions = maps:put(CheckpointId, Decision, State#state.decisions),
            %% Store in ETS for cross-process access
            ets:insert(?MODULE, {{decision, CheckpointId}, Decision}),

            %% Create cancelled task token for approval workflow
            _CancelledToken = wf_task:cancelled(CheckpointId, timeout, 'p_approval_cancelled'),

            %% Create audit receipt for compliance
            BeforeHash = crypto:hash(sha256, term_to_binary({checkpoint, Checkpoint})),
            AfterHash = crypto:hash(sha256, term_to_binary({decision, Decision})),
            Move = #{
                trsn => timeout,
                mode => #{checkpoint => CheckpointId},
                produce => #{'p_approval_cancelled' => [{task, CheckpointId, cancelled, timeout}]}
            },
            Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
            ets:insert(yawl_approval_receipts, {{receipt, CheckpointId}, Receipt}),

            %% Notify waiters of timeout
            Waiters = maps:get(CheckpointId, State#state.waiters, []),
            send_timeout_notifications(CheckpointId, Waiters),
            log_checkpoint_event(Checkpoint, timeout),
            NewState = State#state{decisions = NewDecisions,
                                  receipts = maps:put(CheckpointId, Receipt, State#state.receipts)},
            {noreply, NewState};
        _ ->
            {noreply, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Adds a waiting process to a checkpoint.
%%
%% @end
%%--------------------------------------------------------------------
add_waiter(CheckpointId, Pid, State) ->
    CurrentWaiters = maps:get(CheckpointId, State#state.waiters, []),
    NewWaiters = case lists:member(Pid, CurrentWaiters) of
        true -> CurrentWaiters;
        false -> [Pid | CurrentWaiters]
    end,
    State#state{waiters = maps:put(CheckpointId, NewWaiters, State#state.waiters)}.

%%--------------------------------------------------------------------
%% @private
%% @doc Notifies all waiting processes of a decision.
%%
%% @end
%%--------------------------------------------------------------------
notify_waiters(CheckpointId, Decision, WaitersMap) ->
    Waiters = maps:get(CheckpointId, WaitersMap, []),
    send_decision_notifications(CheckpointId, Decision, Waiters).

%%--------------------------------------------------------------------
%% @private
%% @doc Sends decision notifications to all waiting processes.
%%
%% @end
%%--------------------------------------------------------------------
send_decision_notifications(_CheckpointId, _Decision, []) ->
    ok;
send_decision_notifications(CheckpointId, Decision, [Pid | Rest]) ->
    case erlang:is_process_alive(Pid) of
        true ->
            Pid ! {approval_decision, CheckpointId, Decision};
        false ->
            ok
    end,
    send_decision_notifications(CheckpointId, Decision, Rest).

%%--------------------------------------------------------------------
%% @private
%% @doc Sends timeout notifications to all waiting processes.
%%
%% @end
%%--------------------------------------------------------------------
send_timeout_notifications(_CheckpointId, []) ->
    ok;
send_timeout_notifications(CheckpointId, [Pid | Rest]) ->
    case erlang:is_process_alive(Pid) of
        true ->
            Pid ! {approval_timeout, CheckpointId};
        false ->
            ok
    end,
    send_timeout_notifications(CheckpointId, Rest).

%%--------------------------------------------------------------------
%% @doc Generates a unique checkpoint ID.
%%
%% Used primarily for testing and validation. IDs are prefixed with
%% <<"approval_">> followed by a hex-encoded MD5 hash.
%%
%% @return A unique binary checkpoint ID
%%
%% @end
%%--------------------------------------------------------------------
-doc """
Generates a unique checkpoint ID.

```erlang
% Checkpoint IDs have a specific prefix
Id = yawl_approval:generate_checkpoint_id(),
binary_part(Id, {0, 8}).
<<"approval_">

% IDs are binary
is_binary(Id).
true
```
""".
-spec generate_checkpoint_id() -> checkpoint_id().
generate_checkpoint_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"approval_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets a checkpoint by ID.
%%
%% @end
%%--------------------------------------------------------------------
get_checkpoint(CheckpointId) ->
    case ets:whereis(?MODULE) of
        undefined -> {error, not_found};
        _Table ->
            case ets:lookup(?MODULE, {checkpoint, CheckpointId}) of
                [{{checkpoint, CheckpointId}, Checkpoint}] -> {ok, Checkpoint};
                [] -> {error, not_found}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets a decision by ID.
%%
%% @end
%%--------------------------------------------------------------------
get_decision(CheckpointId) ->
    case ets:whereis(?MODULE) of
        undefined -> {error, not_found};
        _Table ->
            case ets:lookup(?MODULE, {decision, CheckpointId}) of
                [{{decision, CheckpointId}, Decision}] -> {ok, Decision};
                [] -> {error, not_found}
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates an approval prompt for Claude.
%%
%% @end
%%--------------------------------------------------------------------
generate_approval_prompt(Checkpoint, AdditionalContext) ->
    BasePrompt = <<
        "You are reviewing a workflow step for approval.\n\n"
        "Step: ", (atom_to_binary(Checkpoint#approval_checkpoint.step_name))/binary, "\n"
        "Pattern ID: ", (to_binary(Checkpoint#approval_checkpoint.pattern_id))/binary, "\n"
        "Context: ", (context_to_json(Checkpoint#approval_checkpoint.context))/binary, "\n\n"
        "Please review this step and respond with a JSON object:\n"
        "- approved: boolean (true to approve, false to deny)\n"
        "- reason: string (explanation for your decision)\n\n"
    >>,
    case maps:size(AdditionalContext) of
        0 -> BasePrompt;
        _ ->
            <<BasePrompt/binary,
              "Additional Context:\n",
              (context_to_json(AdditionalContext))/binary,
              "\n">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a context map to JSON string.
%%
%% @end
%%--------------------------------------------------------------------
context_to_json(Context) when is_map(Context) ->
    try
        jsone:encode(Context)
    catch
        _:_ -> <<"{}">>
    end;
context_to_json(_) -> <<"{}">>.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a term to binary.
%%
%% @end
%%--------------------------------------------------------------------
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(_) -> <<"">>.

%%--------------------------------------------------------------------
%% @private
%% @doc Logs checkpoint events to XES.
%%
%% @end
%%--------------------------------------------------------------------
log_checkpoint_event(Checkpoint, EventType) ->
    case whereis(yawl_xes) of
        undefined -> ok;
        _Pid ->
            LogId = maps:get(<<"xes_log_id">>, Checkpoint#approval_checkpoint.metadata,
                             <<"approval_default">>),
            EventName = atom_to_binary(EventType),
            yawl_xes:log_event(LogId, <<"approval">>, EventName, #{
                checkpoint_id => Checkpoint#approval_checkpoint.checkpoint_id,
                step => Checkpoint#approval_checkpoint.step_name,
                pattern_id => to_binary(Checkpoint#approval_checkpoint.pattern_id),
                approver_type => Checkpoint#approval_checkpoint.required_approver
            })
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Doctest runner for moduledoc and function documentation.
%% Tests examples embedded in documentation strings.
%%--------------------------------------------------------------------
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%%--------------------------------------------------------------------
%% @doc Test for checkpoint ID generation.
%%--------------------------------------------------------------------
checkpoint_id_format_test() ->
    Id = generate_checkpoint_id(),
    ?assert(is_binary(Id)),
    ?assertEqual(<<"approval_">>, binary_part(Id, {0, 9})),
    ?assertEqual(41, byte_size(Id)).  % "approval_" (9) + 32 hex chars

%%--------------------------------------------------------------------
%% @doc Test for valid approval status atoms.
%%--------------------------------------------------------------------
approval_status_values_test() ->
    ValidStatuses = [pending, approved, denied, timeout, cancelled],
    ?assertEqual(5, length(ValidStatuses)),
    ?assert(lists:member(pending, ValidStatuses)),
    ?assert(lists:member(approved, ValidStatuses)).

%%--------------------------------------------------------------------
%% @doc Test for valid approver types.
%%--------------------------------------------------------------------
approver_types_test() ->
    ValidTypes = [human, simulated, auto],
    ?assertEqual(3, length(ValidTypes)),
    ?assert(lists:member(human, ValidTypes)),
    ?assert(lists:member(simulated, ValidTypes)).

%%--------------------------------------------------------------------
%% @doc Test for approval_decision record structure.
%%--------------------------------------------------------------------
approval_decision_record_test() ->
    Decision = #approval_decision{
        checkpoint_id = <<"test_checkpoint">>,
        approved = true,
        decision_maker = test_approver,
        reason = <<"Test reason">>,
        metadata = #{key => value},
        decided_at = 1234567890
    },
    ?assertEqual(<<"test_checkpoint">>, Decision#approval_decision.checkpoint_id),
    ?assertEqual(true, Decision#approval_decision.approved),
    ?assertEqual(<<"Test reason">>, Decision#approval_decision.reason).

%%--------------------------------------------------------------------
%% @doc Test for approval_checkpoint record structure.
%%--------------------------------------------------------------------
approval_checkpoint_record_test() ->
    Checkpoint = #approval_checkpoint{
        checkpoint_id = <<"test_id">>,
        pattern_id = <<"test_pattern">>,
        step_name = test_step,
        context = #{data => <<"test">>},
        required_approver = simulated,
        timeout = 30000,
        created_at = erlang:system_time(millisecond)
    },
    ?assertEqual(<<"test_id">>, Checkpoint#approval_checkpoint.checkpoint_id),
    ?assertEqual(simulated, Checkpoint#approval_checkpoint.required_approver).
-endif.
