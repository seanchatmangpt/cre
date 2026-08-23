# Token-to-Token Communication Protocol (T2T-CP)

## Version: 1.0.0
## Status: Design Specification
## Context: Active Petri Net Vision for CRE

---

## 1. Overview

The Token-to-Token Communication Protocol (T2T-CP) enables direct messaging between active tokens within a Petri net execution. Tokens become first-class communicative entities capable of:

1. **Direct Messaging** - Point-to-point communication via token PID
2. **Broadcast** - One-to-all communication within a place
3. **Multicast** - One-to-group communication via token groups
4. **Request/Response** - Synchronous query patterns
5. **Asynchronous Notification** - Event-driven signaling

### 1.1 Design Principles

| Principle | Description |
|-----------|-------------|
| **Token Agency** | Tokens are active entities with PIDs, not passive data |
| **Location Transparency** | Communication works regardless of token placement |
| **Fault Isolation** | Failed tokens do not block communication |
| **Causal Ordering** | Message ordering respects causal dependencies |
| **Receipt Verification** | All communications generate auditable receipts |

### 1.2 Relationship to Existing CRE Types

```erlang
%% Existing type from pnet_types.erl
-type token() :: term().

%% New Active Token type (extension)
-type active_token() :: #{
    id := token_id(),           % Unique token identifier
    pid := pid(),               % Token process PID
    data := token(),            % Original token payload
    place := pnet_types:place(),% Current place
    birth := receipt_ref(),     % Creation receipt reference
    parent => token_id() | undefined,
    metadata => map()
}.

-type token_id() :: binary().      % UUID v4 format
-type receipt_ref() :: binary().   % Reference to pnet_receipt:receipt()
```

---

## 2. Message Format Specifications

### 2.1 Base Message Record

```erlang
-record(t2t_message, {
    id              :: token_msg_id(),     % Message UUID
    type            :: msg_type(),         % Message type
    from            :: token_id(),         % Sender token ID
    to              :: destination(),      % Recipient(s)
    payload         :: term(),             % Message payload
    correlation_id  :: correlation_id() | undefined,
    timestamp       :: integer(),          % Creation timestamp (ms)
    ttl             :: non_neg_integer(),  % Time-to-live (ms)
    priority        :: 0..7,              % Priority (0=low, 7=high)
    receipt_request :: boolean(),          % Request delivery receipt
    metadata        :: map()               % Extension metadata
}).

-type token_msg_id() :: binary().
-type msg_type() ::
    direct |          % Point-to-point message
    broadcast |       % Place-wide broadcast
    multicast |       % Group multicast
    request |         % Synchronous request
    response |        % Response to request
    notify |          % Asynchronous notification
    system.           % Internal protocol message

-type destination() ::
    {token, token_id()} |          % Single token
    {place, pnet_types:place()} |  % All tokens in place
    {group, binary()} |            % Named token group
    {tokens, [token_id()]}.        % Explicit token list

-type correlation_id() :: binary().
```

### 2.2 Message Payload Types

```erlang
%% Data payload - general data exchange
-record(payload_data, {
    schema :: binary() | undefined,
    value :: term()
}).

%% Control payload - protocol-level commands
-record(payload_control, {
    command :: control_command(),
    args :: map()
}).

-type control_command() ::
    sync |            % Synchronize state
    heartbeat |       % Liveness check
    elect |          % Leader election
    barrier |        % Synchronization barrier
    query |          % Query token state
    migrate |        % Migrate to different place
    terminate.       % Terminate token

%% State payload - state sharing
-record(payload_state, {
    key :: term(),
    value :: term(),
    version :: non_neg_integer()
}).

%% Receipt payload - delivery acknowledgment
-record(payload_receipt, {
    status :: delivery_status(),
    original_msg_id :: token_msg_id(),
    timestamp :: integer()
}).

-type delivery_status() ::
    delivered |       % Successfully delivered
    accepted |        % Accepted by recipient
    rejected |        % Rejected by recipient
    failed |          % Delivery failed
    timeout |         % Delivery timed out
    unknown_token.    % Recipient not found
```

### 2.3 Message Envelope for Transmission

```erlang
-record(t2t_envelope, {
    message :: #t2t_message{},
    signature :: binary() | undefined,  % Optional signature
    encrypted :: boolean(),              % Payload encryption flag
    compression :: boolean(),            % Compression flag
    routing_trace :: [routing_hop()]    % Journey trace
}).

-type routing_hop() :: #{
    hop_id :: binary(),
    place :: pnet_types:place(),
    timestamp :: integer(),
    action :: forward | deliver | buffer
}.
```

---

## 3. Communication Patterns

### 3.1 Direct Messaging (Token PID to Token PID)

```erlang
%% Send a direct message from one token to another
-spec send_direct(FromToken :: active_token(),
                  ToTokenId :: token_id(),
                  Payload :: term()) ->
    {ok, token_msg_id()} | {error, send_error()}.

-type send_error() ::
    unknown_token |
    token_not_active |
    place_mismatch |
    timeout |
    rate_limited.

%% Message flow:
%% 1. Sender creates #t2t_message{} with type=direct
%% 2. Message is routed to recipient's current place
%% 3. Recipient token receives message via handle_info
%% 4. Optional receipt is generated if receipt_request=true
```

**Sequence Diagram:**

```
SenderToken              T2TRouter              ReceiverToken
    |                        |                        |
    |--[send_direct]-------->|                        |
    |                        |                        |
    |<----[msg_id]-----------|                        |
    |                        |                        |
    |                        |--[message]------------>|
    |                        |                        |
    |                        |<--[optional receipt]--|
    |                        |                        |
    |<--[delivery receipt]---|                        |
```

### 3.2 Broadcast (Token to All Tokens in Place)

```erlang
%% Broadcast a message to all tokens in a place
-spec broadcast_place(FromToken :: active_token(),
                      Place :: pnet_types:place(),
                      Payload :: term()) ->
    {ok, token_msg_id(), recipient_count()} | {error, send_error()}.

%% Message flow:
%% 1. Sender creates message with type=broadcast
%% 2. T2TRouter queries place for all active tokens
%% 3. Message is delivered to each token
%% 4. Aggregated receipt is returned
```

**Fan-out Pattern:**

```
SenderToken              T2TRouter          Token1, Token2, Token3...
    |                        |                     |
    |--[broadcast]---------->|----[msg]----------->|
    |                        |                     |
    |                        |----[msg]----------->|
    |                        |                     |
    |                        |----[msg]----------->|
    |<--[aggregated receipt]--|<--[receipts]--------|
```

### 3.3 Multicast (Token to Token Group)

```erlang
%% Token group management
-spec join_group(Token :: active_token(), GroupId :: binary()) -> ok.
-spec leave_group(Token :: active_token(), GroupId :: binary()) -> ok.
-spec list_groups(Place :: pnet_types:place()) -> [{binary(), [token_id()]}].

%% Multicast to group
-spec multicast_group(FromToken :: active_token(),
                      GroupId :: binary(),
                      Payload :: term()) ->
    {ok, token_msg_id(), delivered_count()} | {error, send_error()}.

%% Group characteristics:
%% - Groups are scoped to places (place-bound)
%% - A token can belong to multiple groups
%% - Groups are ephemeral (exist while tokens exist)
%% - No group persistence across place transitions
```

**Group Hierarchy:**

```erlang
%% Predefined system groups
-define(GROUP_ALL, <<"__all__">).           % All tokens in place
-define(GROUP_LEADERS, <<"__leaders__">).   % Leader tokens
-define(GROUP_WORKERS, <<"__workers__">).   % Worker tokens
-define(GROUP_COORDINATORS, <<"__coordinators__">).

%% User-defined groups follow pattern: <<"group:Name">>
```

### 3.4 Request/Response Pattern

```erlang
%% Synchronous request with timeout
-spec request(FromToken :: active_token(),
              ToTokenId :: token_id(),
              RequestPayload :: term(),
              Timeout :: non_neg_integer()) ->
    {ok, ResponsePayload :: term()} | {error, request_error()}.

-type request_error() ::
    timeout |
    no_response |
    token_busy |
    request_rejected |
    token_terminated.

%% Async request with callback
-spec request_async(FromToken :: active_token(),
                    ToTokenId :: token_id(),
                    RequestPayload :: term(),
                    Callback :: function()) ->
    {ok, token_msg_id()} | {error, send_error()}.

%% Response handling
-spec respond(Token :: active_token(),
              RequestMsgId :: token_msg_id(),
              ResponsePayload :: term()) ->
    ok | {error, response_error()}.
```

**Request/Response Flow:**

```
Requester               T2TRouter               Responder
    |                       |                       |
    |--[request]----------->|                       |
    |                       |                       |
    |<----[correlation_id]--|                       |
    |                       |                       |
    |                       |--[request]---------->|
    |                       |                       |
    |                       |<--[response]---------|
    |                       |                       |
    |<--[response]----------|                       |
```

### 3.5 Asynchronous Notification

```erlang
%% Fire-and-forget notification
-spec notify(FromToken :: active_token(),
             ToTokenId :: token_id(),
             Event :: event()) ->
    ok.

-type event() ::
    {created, token_id()} |
    {moved, pnet_types:place(), pnet_types:place()} |
    {consumed, pnet_types:place()} |
    {produced, pnet_types:place()} |
    {state_changed, map()} |
    {error, term()} |
    {custom, binary(), term()}.

%% Event subscription
-spec subscribe(Token :: active_token(),
                EventFilter :: event_filter()) ->
    {ok, subscription_id()}.

-type event_filter() ::
    all |
    {type, [atom()]} |
    {place, [pnet_types:place()]} |
    {custom, function()}.

-spec unsubscribe(Token :: active_token(),
                  SubscriptionId :: subscription_id()) ->
    ok.
```

---

## 4. Delivery Guarantees

### 4.1 Guarantee Levels

```erlang
-type delivery_guarantee() ::
    best_effort |       % No guarantees, fire-and-forget
    at_least_once |     % May duplicate, never lost
    at_most_once |      % May lose, never duplicate
    exactly_once.       % Perfect delivery (idempotent)

%% Default guarantee: at_least_once
%% Configurable per message or per token relationship
```

### 4.2 Delivery Verification

```erlang
%% Receipt mechanism
-record(delivery_receipt, {
    msg_id :: token_msg_id(),
    recipient :: token_id(),
    status :: delivery_status(),
    timestamp :: integer(),
    ttl_remaining :: non_neg_integer() | undefined,
    retry_count :: non_neg_integer(),
    proof :: binary() | undefined    % Cryptographic proof
}).

%% Receipt verification
-spec verify_receipt(Receipt :: delivery_receipt(),
                     PublicKey :: public_key()) ->
    {ok, valid} | {error, invalid}.

%% Proof generation (for exactly-once)
-spec generate_proof(Message :: #t2t_message{},
                     PrivateKey :: private_key()) ->
    binary().
```

### 4.3 Retry Strategy

```erlang
-record(retry_policy, {
    max_attempts :: pos_integer(),
    backoff :: backoff_type(),
    max_delay :: non_neg_integer(),
    jitter :: boolean()
}).

-type backoff_type() ::
    none |
    fixed |
    exponential |
    linear.

%% Default retry policies by guarantee level
-define(RETRY_BEST_EFFORT, #retry_policy{max_attempts=1}).
-define(RETRY_AT_LEAST_ONCE, #retry_policy{
    max_attempts=3,
    backoff=exponential,
    max_delay=5000
}).
-define(RETRY_EXACTLY_ONCE, #retry_policy{
    max_attempts=5,
    backoff=exponential with jitter,
    max_delay=10000
}).
```

### 4.4 Dead Letter Queue

```erlang
%% Undeliverable messages
-record(dead_letter, {
    original_message :: #t2t_message{},
    failure_reason :: term(),
    attempts :: non_neg_integer(),
    last_attempt :: integer(),
    ttl :: non_neg_integer()
}).

%% Dead letter operations
-spec enqueue_dlq(DeadLetter :: dead_letter()) -> ok.
-spec dequeue_dlq(TokenId :: token_id()) -> [dead_letter()].
-spec purge_dlq(TokenId :: token_id()) -> non_neg_integer().
```

---

## 5. Error Handling

### 5.1 Error Categories

```erlang
-type error_category() ::
    communication |    % Message delivery failures
    lifecycle |        % Token lifecycle errors
    protocol |         % Protocol violations
    resource |         % Resource exhaustion
    security.          % Authentication/authorization

-type t2t_error :: #{
    category := error_category(),
    code := error_code(),
    message :: binary(),
    details :: map(),
    timestamp :: integer(),
    recoverable := boolean()
}.

-type error_code() ::
    %% Communication errors
    token_not_found |
    token_not_active |
    place_not_found |
    delivery_timeout |
    message_too_large |
    rate_limit_exceeded |

    %% Lifecycle errors
    token_terminated |
    token_migrated |
    token_suspended |

    %% Protocol errors
    invalid_message_format |
    unsupported_message_type |
    version_mismatch |
    deserialization_failed |

    %% Resource errors
    buffer_overflow |
    quota_exceeded |
    memory_limit |

    %% Security errors
    authentication_failed |
    authorization_failed |
    signature_invalid |
    encryption_failed.
```

### 5.2 Error Recovery Strategies

```erlang
-type recovery_strategy() ::
    retry |           % Retry with backoff
    fallback |        % Use fallback handler
    delegate |        % Delegate to supervisor
    dead_letter |     % Send to DLQ
    circuit_breaker | % Open circuit
    ignore.           % Silently ignore

-record(error_handler, {
    match :: error_matcher(),
    strategy :: recovery_strategy(),
    max_retries :: non_neg_integer() | unlimited,
    fallback_handler :: function() | undefined
}).

-type error_matcher() ::
    {category, error_category()} |
    {code, error_code()} |
    {custom, function()}.
```

### 5.3 Circuit Breaker Pattern

```erlang
-record(circuit_breaker, {
    state :: closed | open | half_open,
    failure_count :: non_neg_integer(),
    failure_threshold :: pos_integer(),
    success_threshold :: pos_integer(),
    timeout :: non_neg_integer(),
    last_failure :: integer() | undefined
}).

%% Circuit breaker operations
-spec check_breaker(TokenId :: token_id(),
                   Target :: token_id()) ->
    {ok, allowed} | {error, circuit_open}.

-spec record_failure(TokenId :: token_id(),
                     Target :: token_id()) ->
    ok.

-spec record_success(TokenId :: token_id(),
                     Target :: token_id()) ->
    ok.
```

---

## 6. Example Scenarios

### 6.1 Scenario: Distributed Barrier Synchronization

**Problem:** Multiple tokens must synchronize before proceeding.

```erlang
%% Token initiates barrier
barrier_sync(Initiator, ParticipantTokens) ->
    BarrierId = uuid:uuid4_to_string(uuid:get_v4()),

    %% Register barrier participants
    lists:foreach(fun(T) ->
        t2t_protocol:register_barrier(T, BarrierId, ParticipantTokens)
    end, ParticipantTokens),

    %% Send barrier enter message
    lists:foreach(fun(T) ->
        t2t_protocol:send_direct(Initiator, T, #{
            type => barrier,
            command => enter,
            barrier_id => BarrierId
        })
    end, ParticipantTokens),

    %% Wait for all to acknowledge
    await_barrier_complete(BarrierId, length(ParticipantTokens)).

%% Token receives barrier message
handle_barrier_message(BarrierId, Command) ->
    case Command of
        enter ->
            %% Perform pre-barrier actions
            do_pre_barrier_work(),
            %% Acknowledge entry
            reply({barrier_entered, BarrierId});
        release ->
            %% Barrier complete, proceed
            do_post_barrier_work()
    end.
```

### 6.2 Scenario: Token State Replication

**Problem:** Share token state across multiple tokens.

```erlang
%% Token publishes state change
publish_state_change(Token, Key, Value) ->
    StateMsg = #t2t_message{
        type = multicast,
        from = Token#active_token.id,
        to = {group, ?GROUP_ALL},
        payload = #payload_state{
            key = Key,
            value = Value,
            version = get_version() + 1
        },
        receipt_request = true
    },
    t2t_protocol:send_message(StateMsg).

%% Token receives state update
handle_state_update(#payload_state{key = Key, value = Value, version = Version}) ->
    case Version > get_local_version(Key) of
        true ->
            update_local_state(Key, Value, Version),
            notify_dependents(Key, Value);
        false ->
            %% Stale update, ignore
            ok
    end.
```

### 6.3 Scenario: Leader Election

**Problem:** Elect a coordinator token among peers.

```erlang
%% Initiate election
start_election(Token, Peers) ->
    ElectionId = uuid:uuid4_to_string(uuid:get_v4()),
    BullyMsg = #t2t_message{
        type = multicast,
        from = Token#active_token.id,
        to = {tokens, [P#active_token.id || P <- Peers]},
        payload = #payload_control{
            command = elect,
            args = #{
                algorithm => bully,
                election_id => ElectionId,
                candidate_id => Token#active_token.id
            }
        }
    },
    t2t_protocol:send_message(BullyMsg),
    await_election_result(ElectionId).

%% Handle election message
handle_election_message(#payload_control{command = elect, args = Args}) ->
    #{candidate_id := CandidateId, election_id := ElectionId} = Args,
    MyId = get_my_token_id(),
    Priority = get_priority(),

    case CandidateId > MyId of
        true ->
            %% Higher priority candidate, yield
            reply({accept, ElectionId, CandidateId});
        false ->
            %% Lower priority, challenge
            send_challenge(CandidateId, ElectionId, MyId)
    end.
```

### 6.4 Scenario: Token Migration Notification

**Problem:** Notify tokens when migrating between places.

```erlang
%% Before transition firing
token_migrating(Token, FromPlace, ToPlace) ->
    %% Notify current place tokens
    Notification = #t2t_message{
        type = notify,
        from = Token#active_token.id,
        to = {place, FromPlace},
        payload = {token_moving, Token#active_token.id, ToPlace},
        receipt_request = false
    },
    t2t_protocol:send_message(Notification),

    %% Announce arrival at new place
    t2t_protocol:send_message(Notification#t2t_message{
        to = {place, ToPlace},
        payload = {token_arrived, Token#active_token.id, FromPlace}
    }).

%% Token handles migration notification
handle_notify({token_moving, TokenId, ToPlace}) ->
    %% Update local token registry
    update_token_location(TokenId, ToPlace),
    %% Possibly send farewell message
    send_farewell(TokenId);

handle_notify({token_arrived, TokenId, FromPlace}) ->
    %% Add to local registry
    register_new_token(TokenId, FromPlace),
    %% Send welcome message
    send_welcome(TokenId).
```

### 6.5 Scenario: Deadlock Detection

**Problem:** Detect circular wait among tokens.

```erlang
%% Token declares wait
declare_waiting(Token, WaitingFor) ->
    WaitMsg = #t2t_message{
        type = system,
        from = Token#active_token.id,
        to = {group, ?GROUP_COORDINATORS},
        payload = #payload_control{
            command = query,
            args = #{
                action => declare_wait,
                token_id => Token#active_token.id,
                waiting_for => WaitingFor
            }
        }
    },
    t2t_protocol:send_message(WaitMsg).

%% Coordinator checks for cycles
detect_deadlock(WaitGraph) ->
    case find_cycle(WaitGraph) of
        {cycle, CycleTokens} ->
            %% Break deadlock
            lists:foreach(fun(T) ->
                send_break_message(T, CycleTokens)
            end, CycleTokens);
        acyclic ->
            ok
    end.

send_break_message(TokenId, Cycle) ->
    BreakMsg = #t2t_message{
        type = direct,
        to = TokenId,
        payload = #payload_control{
            command = migrate,
            args = #{reason => deadlock_break, cycle => Cycle}
        }
    },
    t2t_protocol:send_message(BreakMsg).
```

---

## 7. Integration with gen_pnet

### 7.1 Token Lifecycle Hooks

```erlang
%% Extend gen_pnet callbacks with token communication
-callback token_created(Place :: pnet_types:place(),
                        Token :: active_token(),
                        NetState :: #net_state{}) ->
    {ok, active_token(), #net_state{}}.

-callback token_consumed(Place :: pnet_types:place(),
                         Token :: active_token(),
                         NetState :: #net_state{}) ->
    {ok, #net_state{}}.

-callback token_produced(Place :: pnet_types:place(),
                         Token :: active_token(),
                         NetState :: #net_state{}) ->
    {ok, active_token(), #net_state{}}.

%% Message handling callback
-callback handle_token_message(Message :: #t2t_message{},
                               From :: token_id(),
                               Token :: active_token(),
                               NetState :: #net_state{}) ->
    {reply, term(), active_token(), #net_state{}} |
    {noreply, active_token(), #net_state{}} |
    {stop, term(), #net_state{}}.
```

### 7.2 Active Token Registry

```erlang
%% Token registry for tracking active tokens
-record(token_registry, {
    place_index :: #{pnet_types:place() => [token_id()]},
    token_index :: #{token_id() => active_token()},
    group_index :: #{binary() => #{place() => [token_id()]}}
}).

%% Registry operations
-spec register_token(Registry :: token_registry(),
                     Token :: active_token()) ->
    {ok, token_registry()}.

-spec unregister_token(Registry :: token_registry(),
                       TokenId :: token_id()) ->
    {ok, token_registry()}.

-spec lookup_token(Registry :: token_registry(),
                   TokenId :: token_id()) ->
    {ok, active_token()} | {error, not_found}.

-spec lookup_place_tokens(Registry :: token_registry(),
                           Place :: pnet_types:place()) ->
    [active_token()] | [].

-spec lookup_group_tokens(Registry :: token_registry(),
                          GroupId :: binary(),
                          Place :: pnet_types:place()) ->
    [active_token()] | [].
```

### 7.3 Receipt Integration

```erlang
%% Extend pnet_receipt with token communication
-record(t2t_receipt, {
    base_receipt :: pnet_receipt:receipt(),
    communication :: #{
        messages_sent :: non_neg_integer(),
        messages_received :: non_neg_integer(),
        message_ids :: [token_msg_id()],
        communication_start :: integer(),
        communication_end :: integer()
    }
}).

%% Create communication receipt
-spec create_communication_receipt(
    BaseReceipt :: pnet_receipt:receipt(),
    Messages :: [token_msg_id()]) ->
    t2t_receipt().
```

---

## 8. Security Considerations

### 8.1 Authentication

```erlang
-record(token_auth, {
    token_id :: token_id(),
    credentials :: credentials(),
    expires :: integer() | undefined
}).

-type credentials() ::
    {shared_secret, binary()} |
    {public_key, public_key()} |
    {token, binary()}.

%% Authentication operations
-spec authenticate(Token :: active_token(),
                  Credentials :: credentials()) ->
    {ok, authenticated} | {error, authentication_failed}.
```

### 8.2 Authorization

```erlang
-type permission() ::
    send | receive | broadcast | multicast | admin.

-record(acl, {
    token_id :: token_id(),
    permissions :: [permission()],
    allowed_targets :: [destination()] | all,
    denied_targets :: [destination()] | none
}).

-spec check_permission(Token :: active_token(),
                      Target :: destination(),
                      Action :: permission()) ->
    allowed | denied.
```

### 8.3 Message Encryption

```erlang
-type encryption_mode() ::
    none |
    symmetric |
    asymmetric.

-record(encryption, {
    mode :: encryption_mode(),
    key_id :: binary(),
    algorithm :: atom()
}).

%% Encrypt message payload
-spec encrypt_message(Message :: #t2t_message{},
                     Encryption :: encryption()) ->
    {ok, #t2t_message{}} | {error, encryption_failed}.

%% Decrypt message payload
-spec decrypt_message(Message :: #t2t_message{},
                     Encryption :: encryption()) ->
    {ok, #t2t_message{}} | {error, decryption_failed}.
```

---

## 9. Performance Considerations

### 9.1 Buffering and Backpressure

```erlang
-record(message_buffer, {
    capacity :: non_neg_integer(),
    current_size :: non_neg_integer(),
    overflow_strategy :: overflow_strategy(),
    messages :: queue:queue()
}).

-type overflow_strategy() ::
    drop_oldest |
    drop_newest |
    block |
    reject_new.

%% Backpressure signaling
-spec send_with_backpressure(Message :: #t2t_message{}) ->
    {ok, token_msg_id()} |
    {error, buffer_full} |
    {error, backpressure_active}.
```

### 9.2 Batch Delivery

```erlang
%% Batch multiple messages for efficiency
-record(message_batch, {
    batch_id :: binary(),
    messages :: [#t2t_message{}],
    target :: destination(),
    max_delay :: non_neg_integer()
}).

-spec send_batch(Messages :: [#t2t_message{}]) ->
    {ok, batch_id()} | {error, batch_error()}.

-spec flush_batch(BatchId :: binary()) ->
    ok | {error, batch_not_found}.
```

### 9.3 Message Prioritization

```erlang
%% Priority queue for message delivery
-record(priority_queue, {
    queues :: #{0..7 => queue:queue()},
    current_priority :: 0..7
}).

%% Enqueue with priority
-spec enqueue_priority(Queue :: priority_queue(),
                       Message :: #t2t_message{}) ->
    priority_queue().

%% Dequeue highest priority
-spec dequeue_priority(Queue :: priority_queue()) ->
    {#t2t_message{}, priority_queue()} | {empty, priority_queue()}.
```

---

## 10. Protocol Versioning

### 10.1 Version Negotiation

```erlang
-record(protocol_version, {
    major :: non_neg_integer(),
    minor :: non_neg_integer(),
    patch :: non_neg_integer()
}).

%% Version compatibility
-spec compatible_version(OurVersion :: protocol_version(),
                         TheirVersion :: protocol_version()) ->
    boolean().

%% Handshake
-spec handshake(Token :: active_token(),
                PeerVersion :: protocol_version()) ->
    {ok, negotiated_version()} | {error, incompatible_version}.
```

### 10.2 Feature Detection

```erlang
-record(protocol_features, {
    version :: protocol_version(),
    features :: [binary()],
    extensions :: #{binary() => term()}
}).

%% Feature advertisement
-spec advertise_features(Token :: active_token()) ->
    protocol_features().

%% Feature query
-spec query_features(PeerTokenId :: token_id()) ->
    {ok, protocol_features()} | {error, query_failed}.
```

---

## 11. Monitoring and Observability

### 11.1 Metrics

```erlang
-record(t2t_metrics, {
    messages_sent :: non_neg_integer(),
    messages_received :: non_neg_integer(),
    messages_failed :: non_neg_integer(),
    bytes_sent :: non_neg_integer(),
    bytes_received :: non_neg_integer(),
    latency :: histogram(),
    error_rate :: float(),
    active_channels :: non_neg_integer()
}).

%% Metrics collection
-spec get_metrics(Token :: active_token()) ->
    t2t_metrics().

-spec reset_metrics(Token :: active_token()) ->
    ok.
```

### 11.2 Tracing

```erlang
-record(message_trace, {
    trace_id :: binary(),
    message_id :: token_msg_id(),
    hops :: [routing_hop()],
    timestamps :: map(),
    annotations :: map()
}).

%% Start trace
-spec start_trace(Message :: #t2t_message{}) ->
    #message_trace{}.

%% Annotate trace
-spec annotate_trace(Trace :: message_trace(),
                     Key :: term(),
                     Value :: term()) ->
    message_trace().

%% Finish trace
-spec end_trace(Trace :: message_trace()) ->
    ok.
```

### 11.3 Logging

```erlang
%% Event types for logging
-type log_event() ::
    message_sent |
    message_received |
    message_failed |
    delivery_receipt |
    error_occurred |
    state_changed.

-spec log_event(Token :: active_token(),
                Event :: log_event(),
                Details :: map()) ->
    ok.

%% Query logs
-spec query_logs(TokenId :: token_id(),
                 Filter :: log_filter()) ->
    [log_entry()].

-type log_filter() ::
    {time_range, {integer(), integer()}} |
    {event_type, [log_event()]} |
    {severity, [log_severity()]}.

-type log_severity() :: debug | info | warning | error | critical.
```

---

## 12. Appendix: Type Reference

### Complete Type Definitions

```erlang
%% Message Types
-type token_msg_id() :: binary().
-type token_id() :: binary().
-type correlation_id() :: binary().
-type subscription_id() :: binary().
-type batch_id() :: binary().

%% Communication Types
-type msg_type() ::
    direct | broadcast | multicast | request | response | notify | system.

-type destination() ::
    {token, token_id()} |
    {place, pnet_types:place()} |
    {group, binary()} |
    {tokens, [token_id()]}.

%% Error Types
-type send_error() ::
    unknown_token | token_not_active | place_mismatch |
    timeout | rate_limited.

-type request_error() ::
    timeout | no_response | token_busy |
    request_rejected | token_terminated.

-type response_error() ::
    unknown_request | request_expired | response_rejected.

%% Delivery Types
-type delivery_guarantee() ::
    best_effort | at_least_once | at_most_once | exactly_once.

-type delivery_status() ::
    delivered | accepted | rejected | failed | timeout | unknown_token.

%% Security Types
-type credentials() ::
    {shared_secret, binary()} |
    {public_key, public_key()} |
    {token, binary()}.

-type permission() ::
    send | receive | broadcast | multicast | admin.

-type encryption_mode() ::
    none | symmetric | asymmetric.

%% Configuration Types
-type backoff_type() ::
    none | fixed | exponential | linear.

-type overflow_strategy() ::
    drop_oldest | drop_newest | block | reject_new.

-type recovery_strategy() ::
    retry | fallback | delegate | dead_letter | circuit_breaker | ignore.

%% Record Type References (for exports)
-export_type([
    token_msg_id/0,
    token_id/0,
    correlation_id/0,
    active_token/0,
    t2t_message/0,
    t2t_envelope/0,
    delivery_receipt/0,
    dead_letter/0,
    retry_policy/0,
    circuit_breaker/0,
    protocol_version/0,
    protocol_features/0,
    t2t_metrics/0,
    message_trace/0,
    delivery_guarantee/0,
    destination/0,
    permission/0,
    credentials/0
]).
```

---

## 13. Implementation Roadmap

### Phase 1: Core Protocol (MVP)
- [ ] Base message format implementation
- [ ] Direct messaging pattern
- [ ] Token registry
- [ ] Basic receipt generation

### Phase 2: Extended Patterns
- [ ] Broadcast and multicast
- [ ] Token groups
- [ ] Request/response pattern
- [ ] Asynchronous notification

### Phase 3: Reliability
- [ ] Delivery guarantees framework
- [ ] Retry policies
- [ ] Dead letter queue
- [ ] Circuit breaker

### Phase 4: Integration
- [ ] gen_pnet callback extensions
- [ ] Token lifecycle hooks
- [ ] Receipt integration with pnet_receipt

### Phase 5: Advanced Features
- [ ] Security (authentication, authorization)
- [ ] Message encryption
- [ ] Priority queues
- [ ] Batch delivery

### Phase 6: Observability
- [ ] Metrics collection
- [ ] Message tracing
- [ ] Event logging
- [ ] Monitoring hooks

---

**Document Status:** Design Complete - Ready for Implementation Review
**Last Updated:** 2026-02-07
**Author:** Designed for CRE Active Petri Net Vision
