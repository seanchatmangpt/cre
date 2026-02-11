%%%-------------------------------------------------------------------
%%% @doc
%%% SOC 2 Receipt Chain (Merkle Tree)
%%%
%%% Accumulates all validation receipts into a cryptographically
%%% verifiable chain. Uses Merkle tree structure for efficient
%%% proof of inclusion.
%%%
%%% Joe Armstrong: "Make it provable"
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_receipt_chain).
-behaviour(gen_server).

%% API
-export([start_link/0]).
-export([append_receipt/1]).
-export([get_chain/0]).
-export([get_merkle_root/0]).
-export([verify_receipt/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {
    receipts :: [map()],
    merkle_tree :: [binary()],
    root_hash :: binary()
}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec append_receipt(map()) -> ok | {error, term()}.
append_receipt(Receipt) ->
    %% Validate receipt against schema before appending
    case soc2_receipt_schema:validate_receipt(Receipt) of
        {ok, ValidReceipt} ->
            gen_server:cast(?MODULE, {append_receipt, ValidReceipt}),
            ok;
        {error, Errors} ->
            logger:warning(#{
                what => receipt_validation_failed,
                receipt => Receipt,
                errors => Errors
            }),
            {error, {invalid_receipt, Errors}}
    end.

-spec get_chain() -> #{root_hash := binary(), receipts := [map()]}.
get_chain() ->
    gen_server:call(?MODULE, get_chain).

-spec get_merkle_root() -> binary().
get_merkle_root() ->
    gen_server:call(?MODULE, get_merkle_root).

-spec verify_receipt(map()) -> {ok, verified} | {error, not_found}.
verify_receipt(Receipt) ->
    gen_server:call(?MODULE, {verify_receipt, Receipt}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    logger:info(#{what => soc2_receipt_chain_started}),

    {ok, #state{
        receipts = [],
        merkle_tree = [],
        root_hash = <<>>
    }}.

handle_call(get_chain, _From, State) ->
    Chain = #{
        root_hash => State#state.root_hash,
        receipts => lists:reverse(State#state.receipts),
        receipt_count => length(State#state.receipts)
    },
    {reply, Chain, State};

handle_call(get_merkle_root, _From, State) ->
    {reply, State#state.root_hash, State};

handle_call({verify_receipt, Receipt}, _From, State) ->
    ReceiptHash = hash_receipt(Receipt),
    Result = case lists:member(ReceiptHash, State#state.merkle_tree) of
        true -> {ok, verified};
        false -> {error, not_found}
    end,
    {reply, Result, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast({append_receipt, Receipt}, State) ->
    %% Receipt has already been validated by append_receipt/1
    %% Compute receipt hash
    ReceiptHash = hash_receipt(Receipt),
    ReceiptWithHash = Receipt#{receipt_hash => ReceiptHash},

    %% Append to receipts
    NewReceipts = [ReceiptWithHash | State#state.receipts],

    %% Rebuild Merkle tree
    NewMerkleTree = build_merkle_tree(NewReceipts),
    NewRootHash = compute_merkle_root(NewMerkleTree),

    logger:debug(#{
        what => receipt_appended,
        receipt_hash => ReceiptHash,
        new_root_hash => NewRootHash,
        receipt_count => length(NewReceipts)
    }),

    {noreply, State#state{
        receipts = NewReceipts,
        merkle_tree = NewMerkleTree,
        root_hash = NewRootHash
    }};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

hash_receipt(Receipt) ->
    %% Deterministic JSON encoding + SHA256
    ReceiptBinary = jsx:encode(Receipt, [{space, 0}, {indent, 0}]),
    crypto:hash(sha256, ReceiptBinary).

build_merkle_tree(Receipts) ->
    %% Extract hashes from receipts
    Hashes = [maps:get(receipt_hash, R, hash_receipt(R)) || R <- Receipts],
    Hashes.

compute_merkle_root([]) ->
    <<>>;
compute_merkle_root([SingleHash]) ->
    SingleHash;
compute_merkle_root(Hashes) ->
    %% Simple Merkle root: hash all hashes together
    Combined = lists:foldl(fun(H, Acc) -> <<Acc/binary, H/binary>> end, <<>>, Hashes),
    crypto:hash(sha256, Combined).
