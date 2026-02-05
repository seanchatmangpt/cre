%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @author YAWL IPC Implementation
%% @copyright 2025
%%
%% @doc YAWL Inter-Process Communication Module for CRE
%%
%% This module implements messaging and communication between workflow
%% processes, supporting cross-case messaging and event subscriptions.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Direct Messaging:</b> Send messages to specific processes</li>
%%   <li><b>Broadcasting:</b> Send messages to multiple handlers</li>
%%   <li><b>Handler Registration:</b> Register message handlers by type</li>
%%   <li><b>Case Subscriptions:</b> Subscribe to case-specific events</li>
%%   <li><b>Cross-Case Messaging:</b> Coordinate between workflow cases</li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% <pre>
%% %% Register a message handler
%% yawl_ipc:register_handler(<<"task_update">>, self()).
%%
%% %% Send a message to a handler
%% yawl_ipc:send_message(<<"task_update">>, #{status => completed}).
%%
%% %% Subscribe to case events
%% yawl_ipc:subscribe_to_case(<<"case_123">>, self()).
%% </pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_ipc).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/0,
         start_link/1,
         stop/0,

         %% Messaging
         send_message/3,
         send_message/2,
         broadcast_message/2,
         broadcast_message/3,
         reply_message/2,

         %% Handler management
         register_handler/2,
         register_handler/3,
         unregister_handler/1,
         unregister_handler/2,
         list_handlers/0,
         list_handlers/1,

         %% Case subscriptions
         subscribe_to_case/2,
         unsubscribe_from_case/1,
         unsubscribe_from_case/2,
         list_case_subscribers/1,
         publish_case_event/3,

         %% Synchronous messaging
         call_handler/3,
         call_handler/4,
         request_response/2,

         %% Message queues
         create_queue/1,
         get_queue_messages/1,
         clear_queue/1,
         delete_queue/1,

         %% Monitoring
         get_message_stats/0,
         get_pending_messages/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%====================================================================
%% Types
%%====================================================================

-type message_type() :: binary() | atom().
-type message_id() :: binary().
-type case_id() :: binary().
-type handler_id() :: reference().

-record(message, {
    id :: message_id(),
    type :: message_type(),
    payload :: term(),
    from :: pid() | undefined,
    to :: pid() | undefined,
    timestamp :: integer(),
    correlation_id :: binary() | undefined
}).

-type message() :: #message{}.

-record(handler, {
    id :: handler_id(),
    message_type :: message_type(),
    pid :: pid(),
    metadata :: map()
}).

-type handler() :: #handler{}.

-record(queue, {
    name :: binary(),
    messages :: [message()],
    max_size :: non_neg_integer(),
    created_at :: integer()
}).

-type queue() :: #queue{}.

-record(ipc_state, {
    handlers :: #{message_type() => [handler()]},
    case_subscribers :: #{case_id() => [pid()]},
    queues :: #{binary() => queue()},
    message_stats :: #{atom() => number()},
    pending_messages :: [message()]
}).

-type ipc_state() :: ipc_state().

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the IPC server with default configuration.
%%
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    start_link(#{}).

%%--------------------------------------------------------------------
%% @doc Starts the IPC server with configuration.
%%
%% @param Config Configuration map.
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Config :: map()) -> {ok, pid()} | {error, term()}.

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%--------------------------------------------------------------------
%% @doc Stops the IPC server.
%%
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.

stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc Sends a message to all handlers of a type.
%%
%% @param MessageType The message type identifier.
%% @param Payload The message payload.
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec send_message(MessageType :: message_type(), Payload :: term()) -> ok.

send_message(MessageType, Payload) ->
    send_message(MessageType, Payload, undefined).

%%--------------------------------------------------------------------
%% @doc Sends a message with correlation ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec send_message(MessageType :: message_type(),
                 Payload :: term(),
                 CorrelationId :: binary() | undefined) -> ok.

send_message(MessageType, Payload, CorrelationId) ->
    gen_server:cast(?MODULE, {send_message, MessageType, Payload, CorrelationId}).

%%--------------------------------------------------------------------
%% @doc Broadcasts a message to multiple handlers.
%%
%% @end
%%--------------------------------------------------------------------
-spec broadcast_message(Pids :: [pid()], Payload :: term()) -> ok.

broadcast_message(Pids, Payload) ->
    broadcast_message(<<"broadcast">>, Pids, Payload).

%%--------------------------------------------------------------------
%% @doc Broadcasts a typed message to multiple handlers.
%%
%% @end
%%--------------------------------------------------------------------
-spec broadcast_message(MessageType :: message_type(),
                       Pids :: [pid()],
                       Payload :: term()) -> ok.

broadcast_message(MessageType, Pids, Payload) ->
    gen_server:cast(?MODULE, {broadcast_message, MessageType, Pids, Payload}).

%%--------------------------------------------------------------------
%% @doc Replies to a received message.
%%
%% @end
%%--------------------------------------------------------------------
-spec reply_message(Message :: message(), ReplyPayload :: term()) -> ok.

reply_message(#message{from = From}, ReplyPayload) when From =/= undefined ->
    From ! {ipc_reply, ReplyPayload},
    ok;
reply_message(_, _ReplyPayload) ->
    {error, no_sender}.

%%--------------------------------------------------------------------
%% @doc Registers a handler for a message type.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_handler(MessageType :: message_type(), Pid :: pid()) ->
          {ok, handler_id()}.

register_handler(MessageType, Pid) ->
    register_handler(MessageType, Pid, #{}).

%%--------------------------------------------------------------------
%% @doc Registers a handler with metadata.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_handler(MessageType :: message_type(),
                      Pid :: pid(),
                      Metadata :: map()) ->
          {ok, handler_id()}.

register_handler(MessageType, Pid, Metadata) ->
    gen_server:call(?MODULE, {register_handler, MessageType, Pid, Metadata}).

%%--------------------------------------------------------------------
%% @doc Unregisters a specific handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_handler(HandlerId :: handler_id()) -> ok.

unregister_handler(HandlerId) ->
    gen_server:cast(?MODULE, {unregister_handler, HandlerId}).

%%--------------------------------------------------------------------
%% @doc Unregisters all handlers for a message type.
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_handler(MessageType :: message_type(), Pid :: pid()) -> ok.

unregister_handler(MessageType, Pid) ->
    gen_server:cast(?MODULE, {unregister_handler, MessageType, Pid}).

%%--------------------------------------------------------------------
%% @doc Lists all handlers.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_handlers() -> #{message_type() => [pid()]}.

list_handlers() ->
    gen_server:call(?MODULE, list_handlers).

%%--------------------------------------------------------------------
%% @doc Lists handlers for a specific message type.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_handlers(MessageType :: message_type()) -> [pid()].

list_handlers(MessageType) ->
    gen_server:call(?MODULE, {list_handlers, MessageType}).

%%--------------------------------------------------------------------
%% @doc Subscribes to events for a case.
%%
%% @end
%%--------------------------------------------------------------------
-spec subscribe_to_case(CaseId :: case_id(), Pid :: pid()) ->
          {ok, reference()}.

subscribe_to_case(CaseId, Pid) ->
    gen_server:call(?MODULE, {subscribe_to_case, CaseId, Pid}).

%%--------------------------------------------------------------------
%% @doc Unsubscribes a specific subscriber.
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe_from_case(SubscriberRef :: reference()) -> ok.

unsubscribe_from_case(SubscriberRef) ->
    gen_server:cast(?MODULE, {unsubscribe_from_case, SubscriberRef}).

%%--------------------------------------------------------------------
%% @doc Unsubscribes a pid from a case.
%%
%% @end
%%--------------------------------------------------------------------
-spec unsubscribe_from_case(CaseId :: case_id(), Pid :: pid()) -> ok.

unsubscribe_from_case(CaseId, Pid) ->
    gen_server:cast(?MODULE, {unsubscribe_from_case, CaseId, Pid}).

%%--------------------------------------------------------------------
%% @doc Lists all subscribers for a case.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_case_subscribers(CaseId :: case_id()) -> [pid()].

list_case_subscribers(CaseId) ->
    gen_server:call(?MODULE, {list_case_subscribers, CaseId}).

%%--------------------------------------------------------------------
%% @doc Publishes an event to case subscribers.
%%
%% @end
%%--------------------------------------------------------------------
-spec publish_case_event(CaseId :: case_id(),
                        EventType :: binary(),
                        EventData :: term()) -> ok.

publish_case_event(CaseId, EventType, EventData) ->
    gen_server:cast(?MODULE, {publish_case_event, CaseId, EventType, EventData}).

%%--------------------------------------------------------------------
%% @doc Synchronously calls a handler.
%%
%% @end
%%--------------------------------------------------------------------
-spec call_handler(MessageType :: message_type(),
                  Payload :: term(),
                  Timeout :: timeout()) ->
          {ok, term()} | {error, term()}.

call_handler(MessageType, Payload, Timeout) ->
    call_handler(MessageType, Payload, Timeout, undefined).

%%--------------------------------------------------------------------
%% @doc Synchronously calls a handler with correlation ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec call_handler(MessageType :: message_type(),
                  Payload :: term(),
                  Timeout :: timeout(),
                  CorrelationId :: binary() | undefined) ->
          {ok, term()} | {error, term()}.

call_handler(MessageType, Payload, Timeout, CorrelationId) ->
    gen_server:call(?MODULE, {call_handler, MessageType, Payload, Timeout, CorrelationId}).

%%--------------------------------------------------------------------
%% @doc Sends a request and waits for response.
%%
%% @end
%%--------------------------------------------------------------------
-spec request_response(Pid :: pid(), Payload :: term()) ->
          {ok, term()} | {error, term()}.

request_response(Pid, Payload) ->
    CorrelationId = generate_message_id(),
    Pid ! {ipc_request, CorrelationId, Payload, self()},

    receive
        {ipc_reply, CorrelationId, Response} ->
            {ok, Response};
        {ipc_reply, Response} ->
            {ok, Response}
    after 5000 ->
        {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @doc Creates a named message queue.
%%
%% @end
%%--------------------------------------------------------------------
-spec create_queue(Name :: binary()) -> {ok, binary()}.

create_queue(Name) ->
    gen_server:call(?MODULE, {create_queue, Name}).

%%--------------------------------------------------------------------
%% @doc Gets messages from a queue.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_queue_messages(QueueName :: binary()) -> [message()].

get_queue_messages(QueueName) ->
    gen_server:call(?MODULE, {get_queue_messages, QueueName}).

%%--------------------------------------------------------------------
%% @doc Clears a message queue.
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_queue(QueueName :: binary()) -> ok.

clear_queue(QueueName) ->
    gen_server:cast(?MODULE, {clear_queue, QueueName}).

%%--------------------------------------------------------------------
%% @doc Deletes a message queue.
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_queue(QueueName :: binary()) -> ok.

delete_queue(QueueName) ->
    gen_server:cast(?MODULE, {delete_queue, QueueName}).

%%--------------------------------------------------------------------
%% @doc Gets message statistics.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_message_stats() -> #{atom() => number()}.

get_message_stats() ->
    gen_server:call(?MODULE, get_message_stats).

%%--------------------------------------------------------------------
%% @doc Gets pending messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_pending_messages() -> [message()].

get_pending_messages() ->
    gen_server:call(?MODULE, get_pending_messages).

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the IPC server.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Config :: map()) -> {ok, ipc_state()}.

init(Config) ->
    MaxQueueSize = maps:get(max_queue_size, Config, 1000),

    State = #ipc_state{
        handlers = #{},
        case_subscribers = #{},
        queues = #{},
        message_stats = #{
            sent => 0,
            received => 0,
            errors => 0
        },
        pending_messages = []
    },

    logger:info("IPC server initialized: max_queue=~p", [MaxQueueSize],
                 [{module, ?MODULE}, {action, init}]),

    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles synchronous calls.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: term(), State :: ipc_state()) ->
          {reply, term(), ipc_state()}.

handle_call({register_handler, MessageType, Pid, Metadata}, _From, State) ->
    HandlerId = make_ref(),
    Handler = #handler{
        id = HandlerId,
        message_type = MessageType,
        pid = Pid,
        metadata = Metadata
    },

    %% Monitor the handler process
    erlang:monitor(process, Pid),

    Handlers = maps:update_with(MessageType,
        fun(Existing) -> [Handler | Existing] end,
        [Handler],
        State#ipc_state.handlers),

    {reply, {ok, HandlerId}, State#ipc_state{handlers = Handlers}};

handle_call({subscribe_to_case, CaseId, Pid}, _From, State) ->
    SubRef = make_ref(),
    erlang:monitor(process, Pid),

    Subscribers = maps:update_with(CaseId,
        fun(Existing) -> [{Pid, SubRef} | Existing] end,
        [{Pid, SubRef}],
        State#ipc_state.case_subscribers),

    {reply, {ok, SubRef}, State#ipc_state{case_subscribers = Subscribers}};

handle_call(list_handlers, _From, State) ->
    HandlersMap = maps:map(
        fun(_MessageType, HandlerList) ->
            [H#handler.pid || H <- HandlerList]
        end,
        State#ipc_state.handlers
    ),
    {reply, HandlersMap, State};

handle_call({list_handlers, MessageType}, _From, State) ->
    HandlerPids = case maps:get(MessageType, State#ipc_state.handlers, []) of
        [] -> [];
        HandlerList -> [H#handler.pid || H <- HandlerList]
    end,
    {reply, HandlerPids, State};

handle_call({list_case_subscribers, CaseId}, _From, State) ->
    Subscribers = case maps:get(CaseId, State#ipc_state.case_subscribers, []) of
        [] -> [];
        SubList -> [Pid || {Pid, _Ref} <- SubList]
    end,
    {reply, Subscribers, State};

handle_call({create_queue, Name}, _From, State) ->
    Queue = #queue{
        name = Name,
        messages = [],
        max_size = 1000,
        created_at = erlang:system_time(millisecond)
    },
    Queues = maps:put(Name, Queue, State#ipc_state.queues),
    {reply, {ok, Name}, State#ipc_state{queues = Queues}};

handle_call({get_queue_messages, QueueName}, _From, State) ->
    Messages = case maps:get(QueueName, State#ipc_state.queues, undefined) of
        undefined -> [];
        #queue{messages = Msgs} -> lists:reverse(Msgs)
    end,
    {reply, Messages, State};

handle_call(get_message_stats, _From, State) ->
    {reply, State#ipc_state.message_stats, State};

handle_call(get_pending_messages, _From, State) ->
    {reply, lists:reverse(State#ipc_state.pending_messages), State};

handle_call({call_handler, MessageType, Payload, Timeout, CorrelationId}, _From, State) ->
    %% Find a handler for the message type
    case maps:get(MessageType, State#ipc_state.handlers, []) of
        [] ->
            {reply, {error, no_handler}, State};
        [#handler{pid = Pid} | _] ->
            MessageId = case CorrelationId of
                undefined -> generate_message_id();
                _ -> CorrelationId
            end,

            %% Create a reference to track the response
            RequestRef = make_ref(),

            Message = #message{
                id = MessageId,
                type = MessageType,
                payload = Payload,
                from = {RequestRef, self()},  % Include gen_server Pid and reference
                timestamp = erlang:system_time(millisecond),
                correlation_id = CorrelationId
            },

            %% Send request to handler
            Pid ! {ipc_request, Message},

            %% Update stats
            Stats = maps:update_with(sent, fun(V) -> V + 1 end,
                    State#ipc_state.message_stats),

            %% Wait for response from handler
            receive
                {ipc_response, RequestRef, Response} ->
                    {reply, {ok, Response}, State#ipc_state{message_stats = Stats}}
            after Timeout ->
                Stats1 = maps:update_with(errors, fun(V) -> V + 1 end, Stats),
                {reply, {error, timeout}, State#ipc_state{message_stats = Stats1}}
            end
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles asynchronous casts.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: ipc_state()) ->
          {noreply, ipc_state()}.

handle_cast({send_message, MessageType, Payload, CorrelationId}, State) ->
    Message = #message{
        id = generate_message_id(),
        type = MessageType,
        payload = Payload,
        from = self(),
        timestamp = erlang:system_time(millisecond),
        correlation_id = CorrelationId
    },

    State1 = deliver_to_handlers(Message, State),
    {noreply, State1};

handle_cast({broadcast_message, MessageType, Pids, Payload}, State) ->
    Message = #message{
        id = generate_message_id(),
        type = MessageType,
        payload = Payload,
        from = self(),
        timestamp = erlang:system_time(millisecond)
    },

    lists:foreach(
        fun(Pid) ->
            Pid ! {ipc_message, Message}
        end,
        Pids
    ),

    %% Update stats
    Stats = maps:update_with(sent, fun(V) -> V + V end,
                    State#ipc_state.message_stats),
    Stats1 = maps:update_with(sent, fun(V) -> V + length(Pids) end, Stats),

    {noreply, State#ipc_state{message_stats = Stats1}};

handle_cast({unregister_handler, HandlerId}, State) ->
    Handlers = maps:map(
        fun(_MessageType, HandlerList) ->
            lists:filter(
                fun(H) -> H#handler.id =/= HandlerId end,
                HandlerList
            )
        end,
        State#ipc_state.handlers
    ),
    {noreply, State#ipc_state{handlers = Handlers}};

handle_cast({unregister_handler, MessageType, Pid}, State) ->
    Handlers = case maps:get(MessageType, State#ipc_state.handlers, []) of
        [] -> State#ipc_state.handlers;
        HandlerList ->
            Filtered = lists:filter(
                fun(H) -> H#handler.pid =/= Pid end,
                HandlerList
            ),
            maps:put(MessageType, Filtered, State#ipc_state.handlers)
    end,
    {noreply, State#ipc_state{handlers = Handlers}};

handle_cast({unsubscribe_from_case, SubscriberRef}, State) ->
    Subscribers = maps:map(
        fun(_CaseId, SubList) ->
            lists:filter(
                fun({_Pid, Ref}) -> Ref =/= SubscriberRef end,
                SubList
            )
        end,
        State#ipc_state.case_subscribers
    ),
    {noreply, State#ipc_state{case_subscribers = Subscribers}};

handle_cast({unsubscribe_from_case, CaseId, Pid}, State) ->
    Subscribers = case maps:get(CaseId, State#ipc_state.case_subscribers, []) of
        [] -> State#ipc_state.case_subscribers;
        SubList ->
            Filtered = lists:filter(
                fun({P, _Ref}) -> P =/= Pid end,
                SubList
            ),
            maps:put(CaseId, Filtered, State#ipc_state.case_subscribers)
    end,
    {noreply, State#ipc_state{case_subscribers = Subscribers}};

handle_cast({publish_case_event, CaseId, EventType, EventData}, State) ->
    Message = #message{
        id = generate_message_id(),
        type = EventType,
        payload = EventData,
        timestamp = erlang:system_time(millisecond)
    },

    %% Notify case subscribers and get count
    SubList = maps:get(CaseId, State#ipc_state.case_subscribers, []),
    SubCount = length(SubList),
    case SubList of
        [] -> ok;
        _ ->
            lists:foreach(
                fun({Pid, _Ref}) ->
                    Pid ! {ipc_case_event, CaseId, Message}
                end,
                SubList
            )
    end,

    %% Also notify general handlers for the event type
    State1 = deliver_to_handlers(Message, State),

    %% Update stats
    Stats = maps:update_with(sent, fun(V) -> V + SubCount end,
                    State1#ipc_state.message_stats),

    {noreply, State1#ipc_state{message_stats = Stats}};

handle_cast({clear_queue, QueueName}, State) ->
    Queues = case maps:get(QueueName, State#ipc_state.queues, undefined) of
        undefined -> State#ipc_state.queues;
        Queue ->
            Queue1 = Queue#queue{messages = []},
            maps:put(QueueName, Queue1, State#ipc_state.queues)
    end,
    {noreply, State#ipc_state{queues = Queues}};

handle_cast({delete_queue, QueueName}, State) ->
    Queues = maps:remove(QueueName, State#ipc_state.queues),
    {noreply, State#ipc_state{queues = Queues}};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles info messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: ipc_state()) ->
          {noreply, ipc_state()}.

handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Remove handlers for dead process
    Handlers = maps:map(
        fun(_MessageType, HandlerList) ->
            lists:filter(
                fun(H) -> H#handler.pid =/= Pid end,
                HandlerList
            )
        end,
        State#ipc_state.handlers
    ),

    %% Remove case subscribers for dead process
    Subscribers = maps:map(
        fun(_CaseId, SubList) ->
            lists:filter(
                fun({P, _Ref2}) -> P =/= Pid end,
                SubList
            )
        end,
        State#ipc_state.case_subscribers
    ),

    {noreply, State#ipc_state{
        handlers = Handlers,
        case_subscribers = Subscribers
    }};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Terminates the IPC server.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: ipc_state()) -> ok.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles code changes.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: ipc_state(), Extra :: term()) ->
          {ok, ipc_state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Delivers a message to all registered handlers.
%%
%% @end
%%--------------------------------------------------------------------
-spec deliver_to_handlers(message(), ipc_state()) -> ipc_state().

deliver_to_handlers(Message, State) ->
    MessageType = Message#message.type,

    %% Always add messages to queues (regardless of handlers)
    Queues = add_message_to_queues(Message, State#ipc_state.queues),

    case maps:get(MessageType, State#ipc_state.handlers, []) of
        [] ->
            %% No handlers, add to pending messages AND queues
            Pending = [Message | State#ipc_state.pending_messages],
            State#ipc_state{pending_messages = Pending, queues = Queues};
        Handlers ->
            %% Deliver to all handlers
            lists:foreach(
                fun(#handler{pid = Pid}) ->
                    Pid ! {ipc_message, Message}
                end,
                Handlers
            ),

            %% Update stats
            Stats = maps:update_with(sent, fun(V) -> V + length(Handlers) end,
                            State#ipc_state.message_stats),

            State#ipc_state{
                message_stats = Stats,
                queues = Queues
            }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Adds a message to all queues.
%%
%% @end
%%--------------------------------------------------------------------
-spec add_message_to_queues(message(), #{binary() => queue()}) ->
          #{binary() => queue()}.

add_message_to_queues(Message, Queues) ->
    maps:map(
        fun(_Name, Queue) ->
            Messages = Queue#queue.messages,
            %% Enforce max size
            Messages1 = case length(Messages) >= Queue#queue.max_size of
                true -> tl(Messages) ++ [Message];
                false -> Messages ++ [Message]
            end,
            Queue#queue{messages = Messages1}
        end,
        Queues
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique message ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_message_id() -> binary().

generate_message_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"msg_", Hex/binary>>.
