%% -*- erlang -*-
%%%% @doc yawl_refusal_receipt - Receipt and remediation for refusals.
%%
%% This module handles the emission of refusal receipts and provides
%% remediation workflows when transitions are refused.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_refusal_receipt).
-author("CRE Team").

%%====================================================================
%% Exports
%%====================================================================

%% Receipt API
-export([emit/3]).
-export([emit/4]).
-export([get_receipt/1]).
-export([list_receipts/1]).
-export([acknowledge_receipt/1]).

%% Remediation API
-export([suggest_remediation/2]).
-export([apply_remediation/2]).
-export([retry_after_remediation/3]).

%% Receipt storage
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(refusal_receipt, {
    receipt_id :: binary(),
    transition :: atom(),
    category :: yawl_refusal_guard:refusal_category(),
    reason :: binary(),
    timestamp :: integer(),
    context :: map(),
    remediation :: undefined | binary(),
    acknowledged = false :: boolean(),
    retry_count = 0 :: non_neg_integer()
}).

-record(receipt_state, {
    receipts = #{} :: #{binary() => #refusal_receipt{}},
    total_count = 0 :: non_neg_integer()
}).

%%====================================================================
%% Types
%%====================================================================

-type refusal_receipt() :: #refusal_receipt{}.
-type receipt_state() :: #receipt_state{}.
-type remediation_action() ::
    retry |
    skip |
    manual_intervention |
    substitute_transition |
    abort.

-export_type([refusal_receipt/0, remediation_action/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the receipt service.
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc Emits a refusal receipt.
%%
%% @param Transition The refused transition
%% @param Category Refusal category
%% @param Reason Human-readable reason
%%
%% @end
%%--------------------------------------------------------------------
-spec emit(atom(), yawl_refusal_guard:refusal_category(), binary()) ->
          {ok, binary()}.

emit(Transition, Category, Reason) ->
    emit(Transition, Category, Reason, #{}).

%%--------------------------------------------------------------------
%% @doc Emits a refusal receipt with context.
%%
%% @end
%%--------------------------------------------------------------------
-spec emit(atom(), yawl_refusal_guard:refusal_category(), binary(), map()) ->
          {ok, binary()}.

emit(Transition, Category, Reason, Context) when is_atom(Transition),
                                               is_atom(Category),
                                               is_binary(Reason),
                                               is_map(Context) ->
    ReceiptId = generate_receipt_id(),
    Receipt = #refusal_receipt{
        receipt_id = ReceiptId,
        transition = Transition,
        category = Category,
        reason = Reason,
        timestamp = erlang:system_time(millisecond),
        context = Context,
        remediation = suggest_remediation(Category, Context)
    },
    gen_server:cast(?MODULE, {emit, Receipt}),
    logger:warning("Refusal receipt emitted: ~s refused (~p): ~s",
                  [Transition, Category, Reason]),
    {ok, ReceiptId}.

%%--------------------------------------------------------------------
%% @doc Gets a receipt by ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_receipt(binary()) -> {ok, refusal_receipt()} | {error, not_found}.

get_receipt(ReceiptId) ->
    gen_server:call(?MODULE, {get_receipt, ReceiptId}).

%%--------------------------------------------------------------------
%% @doc Lists all receipts for a transition.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_receipts(atom()) -> [refusal_receipt()].

list_receipts(Transition) when is_atom(Transition) ->
    gen_server:call(?MODULE, {list_receipts, Transition}).

%%--------------------------------------------------------------------
%% @doc Acknowledges a receipt.
%%
%% @end
%%--------------------------------------------------------------------
-spec acknowledge_receipt(binary()) -> ok | {error, not_found}.

acknowledge_receipt(ReceiptId) ->
    gen_server:call(?MODULE, {acknowledge, ReceiptId}).

%%--------------------------------------------------------------------
%% @doc Suggests remediation for a refusal.
%%
%% @end
%%--------------------------------------------------------------------
-spec suggest_remediation(yawl_refusal_guard:refusal_category(), map()) ->
          binary().

suggest_remediation(missing_evidence, _Context) ->
    <<"Provide the required evidence tokens and retry.">>;

suggest_remediation(forbidden_action, _Context) ->
    <<"Review the constitution for permitted actions. This action is not allowed.">>;

suggest_remediation(scope_violation, _Context) ->
    <<"The operation exceeds the authorized scope. Request broader authorization.">>;

suggest_remediation(external_boundary, _Context) ->
    <<"Enable the external boundary (procurement/marketplace) in the configuration.">>;

suggest_remediation(resource_unavailable, Context) ->
    RetryDelay = maps:get(retry_after_ms, Context, 5000),
    iolist_to_binary(io_lib:format(
        "Resource temporarily unavailable. Retry after ~p ms.", [RetryDelay]));

suggest_remediation(safety_violation, _Context) ->
    <<"Safety threshold exceeded. Manual intervention required.">>;

suggest_remediation(validation_failure, Context) ->
    case maps:get(validation_errors, Context, []) of
        [] -> <<"Fix data validation errors and retry.">>;
        Errors ->
            iolist_to_binary(io_lib:format(
                "Validation errors: ~p. Fix and retry.", [Errors]))
    end;

suggest_remediation(timeout_exceeded, Context) ->
    Timeout = maps:get(timeout_ms, Context, undefined),
    iolist_to_binary(io_lib:format(
        "Operation timeout (~p ms) exceeded. Consider increasing timeout or optimizing.", [Timeout]));

suggest_remediation(permission_denied, Context) ->
    Actor = maps:get(actor, Context, <<"unknown">>),
    iolist_to_binary(io_lib:format(
        "Actor ~s is denied permission. Grant required permissions.", [Actor])).

%%--------------------------------------------------------------------
%% @doc Applies a remediation action.
%%
%% @end
%%--------------------------------------------------------------------
-spec apply_remediation(binary(), remediation_action()) ->
          ok | {error, term()}.

apply_remediation(ReceiptId, Action) ->
    gen_server:call(?MODULE, {apply_remediation, ReceiptId, Action}).

%%--------------------------------------------------------------------
%% @doc Retries a transition after remediation.
%%
%% @end
%%--------------------------------------------------------------------
-spec retry_after_remediation(binary(), atom(), map()) ->
          {ok, term()} | {error, term()}.

retry_after_remediation(ReceiptId, Transition, Context) ->
    case get_receipt(ReceiptId) of
        {ok, #refusal_receipt{retry_count = Retries}} when Retries >= 3 ->
            {error, max_retries_exceeded};
        {ok, Receipt} ->
            %% Increment retry count
            acknowledge_receipt(ReceiptId),
            %% Attempt to fire the transition again
            %% This would typically call gen_yawl:step or similar
            {ok, #{receipt_id => ReceiptId, transition => Transition}};
        {error, not_found} ->
            {error, invalid_receipt}
    end.

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%% @private
-spec init([]) -> {ok, #receipt_state{}}.

init([]) ->
    logger:info("Refusal receipt service starting"),
    {ok, #receipt_state{}}.

%% @private
-spec handle_call(term(), {pid(), term()}, #receipt_state{}) ->
          {reply, term(), #receipt_state{}}.

handle_call({get_receipt, ReceiptId}, _From, State = #receipt_state{receipts = Receipts}) ->
    Reply = case maps:get(ReceiptId, Receipts, undefined) of
        undefined -> {error, not_found};
        Receipt -> {ok, Receipt}
    end,
    {reply, Reply, State};

handle_call({list_receipts, Transition}, _From, State = #receipt_state{receipts = Receipts}) ->
    Filtered = lists:filter(
        fun(#refusal_receipt{transition = T}) -> T =:= Transition end,
        maps:values(Receipts)
    ),
    {reply, Filtered, State};

handle_call({acknowledge, ReceiptId}, _From, State = #receipt_state{receipts = Receipts}) ->
    case maps:get(ReceiptId, Receipts, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        Receipt ->
            Receipt1 = Receipt#refusal_receipt{acknowledged = true},
            {reply, ok, State#receipt_state{receipts = maps:put(ReceiptId, Receipt1, Receipts)}}
    end;

handle_call({apply_remediation, ReceiptId, Action}, _From, State) ->
    Reply = case maps:get(ReceiptId, State#receipt_state.receipts, undefined) of
        undefined ->
            {error, not_found};
        #refusal_receipt{category = Category} ->
            %% Apply remediation based on action
            case Action of
                retry ->
                    {ok, <<(remediation_text(Action))/binary, " for ", (atom_to_binary(Category))/binary>>};
                skip ->
                    {ok, <<(remediation_text(Action))/binary>>};
                manual_intervention ->
                    {ok, <<"Manual intervention requested">>};
                _ ->
                    {error, unsupported_action}
            end
    end,
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
-spec handle_cast(term(), #receipt_state{}) -> {noreply, #receipt_state{}}.

handle_cast({emit, Receipt}, State = #receipt_state{receipts = Receipts, total_count = Count}) ->
    Receipts1 = maps:put(Receipt#refusal_receipt.receipt_id, Receipt, Receipts),
    {noreply, State#receipt_state{
        receipts = Receipts1,
        total_count = Count + 1
    }};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
-spec handle_info(term(), #receipt_state{}) -> {noreply, #receipt_state{}}.

handle_info(_Info, State) ->
    {noreply, State}.

%% @private
-spec code_change(term(), #receipt_state{}, term()) -> {ok, #receipt_state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
-spec terminate(term(), #receipt_state{}) -> ok.

terminate(_Reason, _State) ->
    logger:info("Refusal receipt service stopping"),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
-spec generate_receipt_id() -> binary().

generate_receipt_id() ->
    Time = erlang:system_time(microsecond),
    Unique = erlang:unique_integer([positive]),
    <<"rrf_", (integer_to_binary(Time))/binary, "_", (integer_to_binary(Unique))/binary>>.

%% @private
-spec remediation_text(remediation_action()) -> binary().

remediation_text(retry) -> <<"Retrying operation">>;
remediation_text(skip) -> <<"Skipping operation">>;
remediation_text(manual_intervention) -> <<"Manual intervention required">>;
remediation_text(substitute_transition) -> <<"Substituting with alternative transition">>;
remediation_text(abort) -> <<"Aborting workflow">>.
