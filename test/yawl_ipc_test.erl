%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
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
%% @doc YAWL Inter-Process Communication Module Test Suite
%%
%% Comprehensive test suite for IPC messaging, pub/sub, and
%% cross-process communication.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_ipc_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%% Explicitly export test generators for EUnit discovery
-export([
    test_start_ipc_test_/0,
    test_stop_restart_ipc_test_/0,
    test_publish_subscribe_test_/0,
    test_event_delivery_test_/0,
    test_unsubscribe_test_/0,
    test_unsubscribe_by_type_test_/0,
    test_cross_process_publish_test_/0,
    test_multiple_subscribers_test_/0,
    test_filtered_events_test_/0,
    test_list_handlers_test_/0,
    test_case_subscription_test_/0,
    test_list_case_subscribers_test_/0,
    test_unsubscribe_from_case_test_/0,
    test_broadcast_message_test_/0,
    test_call_handler_test_/0,
    test_call_handler_no_handler_test_/0,
    test_request_response_test_/0,
    test_message_queue_test_/0,
    test_clear_queue_test_/0,
    test_delete_queue_test_/0,
    test_get_message_stats_test_/0,
    test_get_pending_messages_test_/0,
    test_handler_cleanup_on_death_test_/0,
    test_correlation_id_test_/0,
    test_complete_ipc_workflow_test_/0,
    setup/0,
    cleanup/1,
    setup_with_handler/0,
    cleanup_with_handler/1,
    test_handler_loop/1,
    cross_process_handler/2,
    multi_sub_handler/2,
    receive_count/2,
    broadcast_handler/2,
    responder_loop/0,
    publisher_loop/0,
    subscriber_loop/2,
    test/0
]).

-compile(export_all).

%%====================================================================
%% Record Definitions (imported from yawl_ipc)
%%====================================================================

-record(message, {
    id :: binary(),
    type :: binary() | atom(),
    payload :: term(),
    from :: pid() | undefined,
    to :: pid() | undefined,
    timestamp :: integer(),
    correlation_id :: binary() | undefined
}).

%%====================================================================
%% Test Setup and Teardown
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% Starts the IPC server.
%% @end
%%--------------------------------------------------------------------
setup() ->
    {ok, Pid} = yawl_ipc:start_link(#{max_queue_size => 100}),
    Pid.

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% Stops the IPC server.
%% @end
%%--------------------------------------------------------------------
cleanup(_Pid) ->
    yawl_ipc:stop(),
    timer:sleep(50),
    ok.

%%--------------------------------------------------------------------
%% @doc Setup with a test handler process.
%% @end
%%--------------------------------------------------------------------
setup_with_handler() ->
    {ok, IpcPid} = yawl_ipc:start_link(#{max_queue_size => 100}),
    TestPid = self(),
    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
    {IpcPid, HandlerPid}.

%%--------------------------------------------------------------------
%% @doc Cleanup with handler.
%% @end
%%--------------------------------------------------------------------
cleanup_with_handler({IpcPid, HandlerPid}) ->
    unlink(HandlerPid),
    exit(HandlerPid, kill),
    unlink(IpcPid),
    yawl_ipc:stop(),
    timer:sleep(50),
    ok.

%%--------------------------------------------------------------------
%% @doc Test handler loop that sends received messages back to test process.
%% @end
%%--------------------------------------------------------------------
test_handler_loop(TestPid) ->
    receive
        {ipc_message, Message} ->
            TestPid ! {handler_message, Message},
            test_handler_loop(TestPid);
        {ipc_case_event, _CaseId, Message} ->
            TestPid ! {handler_case_event, Message},
            test_handler_loop(TestPid);
        {ipc_request, Message} ->
            % Handle both old format (Pid) and new format ({Ref, Pid})
            case Message#message.from of
                {RequestRef, FromPid} when is_reference(RequestRef), is_pid(FromPid) ->
                    FromPid ! {ipc_response, RequestRef, ok};
                FromPid when is_pid(FromPid) ->
                    FromPid ! {ipc_response, Message#message.id, ok};
                _ ->
                    % Unknown format, ignore
                    ok
            end,
            test_handler_loop(TestPid);
        stop ->
            ok
    end.

%%====================================================================
%% 1. Server Lifecycle Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test starting the IPC server.
%% @end
%%--------------------------------------------------------------------
test_start_ipc_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    ?assert(is_pid(whereis(yawl_ipc)))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test stopping and restarting the IPC server.
%% @end
%%--------------------------------------------------------------------
test_stop_restart_ipc_test_() ->
    {setup,
     fun() ->
             {ok, Pid} = yawl_ipc:start_link(),
             Pid
     end,
     fun(Pid) ->
             unlink(Pid),
             yawl_ipc:stop(),
             timer:sleep(100)
     end,
     fun(_Pid) ->
         [?_test(begin
                    yawl_ipc:stop(),
                    timer:sleep(100),
                    ?assertEqual(undefined, whereis(yawl_ipc)),
                    {ok, NewPid} = yawl_ipc:start_link(),
                    ?assert(is_pid(NewPid)),
                    yawl_ipc:stop()
                end)]
     end}.

%%====================================================================
%% 2. Publish/Subscribe Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test publish and subscribe messaging.
%% @end
%%--------------------------------------------------------------------
test_publish_subscribe_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    MessageType = <<"test_event">>,
                    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
                    {ok, _HandlerId} = yawl_ipc:register_handler(MessageType, HandlerPid),
                    % Publish a message
                    yawl_ipc:send_message(MessageType, #{payload => test_data}),
                    % Give time for delivery
                    timer:sleep(100),
                    % Verify handler received the message
                    receive
                        {handler_message, Received} ->
                            ?assertNotEqual(undefined, Received),
                            ?assertEqual(MessageType, Received#message.type)
                    after 200 ->
                        ?assert(false, "Timeout waiting for message")
                    end,
                    exit(HandlerPid, kill)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test event delivery to subscribers.
%% @end
%%--------------------------------------------------------------------
test_event_delivery_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    MessageType = <<"delivery_event">>,
                    Payload = #{key => value, count => 42},
                    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
                    {ok, _HandlerId} = yawl_ipc:register_handler(MessageType, HandlerPid),
                    % Send message
                    yawl_ipc:send_message(MessageType, Payload),
                    timer:sleep(100),
                    % Check delivery
                    receive
                        {handler_message, Received} ->
                            ?assertNotEqual(undefined, Received),
                            ?assertEqual(Payload, Received#message.payload),
                            ?assert(is_integer(Received#message.timestamp))
                    after 200 ->
                        ?assert(false, "Timeout waiting for message")
                    end,
                    exit(HandlerPid, kill)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test unsubscribe from events.
%% @end
%%--------------------------------------------------------------------
test_unsubscribe_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    MessageType = <<"unsubscribe_event">>,
                    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
                    {ok, HandlerId} = yawl_ipc:register_handler(MessageType, HandlerPid),
                    % Send first message
                    yawl_ipc:send_message(MessageType, msg1),
                    receive
                        {handler_message, _Msg1} -> ok
                    after 200 ->
                        ?assert(false, "Timeout waiting for first message")
                    end,
                    % Unsubscribe
                    yawl_ipc:unregister_handler(HandlerId),
                    timer:sleep(50),
                    % Send second message - should not be received
                    yawl_ipc:send_message(MessageType, msg2),
                    timer:sleep(100),
                    receive
                        {handler_message, _Msg2} ->
                            ?assert(false, "Should not have received message after unsubscribe")
                    after 100 ->
                        ok  % Expected - no message received
                    end,
                    exit(HandlerPid, kill)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test unsubscribe by message type and pid.
%% @end
%%--------------------------------------------------------------------
test_unsubscribe_by_type_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    MessageType = <<"type_unsubscribe_event">>,
                    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
                    {ok, _HandlerId} = yawl_ipc:register_handler(MessageType, HandlerPid),
                    % Verify handler is registered
                    Handlers = yawl_ipc:list_handlers(MessageType),
                    ?assert(lists:member(HandlerPid, Handlers)),
                    % Unsubscribe
                    yawl_ipc:unregister_handler(MessageType, HandlerPid),
                    timer:sleep(50),
                    % Verify handler is removed
                    NewHandlers = yawl_ipc:list_handlers(MessageType),
                    ?assertNot(lists:member(HandlerPid, NewHandlers)),
                    exit(HandlerPid, kill)
                end)]
     end}.

%%====================================================================
%% 3. Cross-Process Messaging Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test cross-process publish/subscribe.
%% @end
%%--------------------------------------------------------------------
test_cross_process_publish_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    % Create two handler processes
                    Handler1 = spawn(fun() -> cross_process_handler(TestPid, 1) end),
                    Handler2 = spawn(fun() -> cross_process_handler(TestPid, 2) end),

                    MessageType = <<"cross_process_event">>,

                    % Register both handlers
                    {ok, _Id1} = yawl_ipc:register_handler(MessageType, Handler1),
                    {ok, _Id2} = yawl_ipc:register_handler(MessageType, Handler2),

                    % Publish message
                    yawl_ipc:send_message(MessageType, #{data => cross_process}),

                    timer:sleep(100),

                    % Both handlers should receive the message
                    receive
                        {handler_received, handler1_received, cross_process} -> ok
                    after 200 ->
                        ?assert(false, "Timeout waiting for handler1")
                    end,
                    receive
                        {handler_received, handler2_received, cross_process} -> ok
                    after 200 ->
                        ?assert(false, "Timeout waiting for handler2")
                    end,

                    % Cleanup
                    exit(Handler1, kill),
                    exit(Handler2, kill)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Helper handler for cross-process test.
%% @end
%%--------------------------------------------------------------------
cross_process_handler(TestPid, HandlerNum) ->
    receive
        {ipc_message, _Message} ->
            Key = list_to_atom("handler" ++ integer_to_list(HandlerNum) ++ "_received"),
            TestPid ! {handler_received, Key, cross_process},
            cross_process_handler(TestPid, HandlerNum);
        stop ->
            ok
    end.

%%====================================================================
%% 4. Multiple Subscribers Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test multiple subscribers receive events.
%% @end
%%--------------------------------------------------------------------
test_multiple_subscribers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    % Create multiple handler processes
                    Handlers = [spawn(fun() -> multi_sub_handler(TestPid, N) end)
                                || N <- lists:seq(1, 5)],

                    MessageType = <<"multi_sub_event">>,

                    % Register all handlers
                    lists:foreach(fun(H) ->
                        {ok, _} = yawl_ipc:register_handler(MessageType, H)
                    end, Handlers),

                    % Publish messages
                    yawl_ipc:send_message(MessageType, msg1),
                    timer:sleep(100),

                    % Count received messages
                    ReceivedCounts = receive_count(5, 0),

                    ?assertEqual(5, ReceivedCounts),

                    % Cleanup
                    lists:foreach(fun(H) -> exit(H, kill) end, Handlers)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Helper to count handler messages.
%% @end
%%--------------------------------------------------------------------
receive_count(0, Count) -> Count;
receive_count(Remaining, Count) ->
    receive
        {handler_received, _Key, received} ->
            receive_count(Remaining - 1, Count + 1)
    after 200 ->
        Count
    end.

%%--------------------------------------------------------------------
%% @doc Helper handler for multiple subscribers test.
%% @end
%%--------------------------------------------------------------------
multi_sub_handler(TestPid, HandlerNum) ->
    receive
        {ipc_message, _Message} ->
            Key = list_to_atom("multi_handler" ++ integer_to_list(HandlerNum)),
            TestPid ! {handler_received, Key, received},
            multi_sub_handler(TestPid, HandlerNum);
        stop ->
            ok
    end.

%%====================================================================
%% 5. Event Filtering Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test filtered events (different message types).
%% @end
%%--------------------------------------------------------------------
test_filtered_events_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    % Register for specific message type
                    Type1 = <<"event_type_1">>,
                    Type2 = <<"event_type_2">>,

                    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
                    {ok, _Id1} = yawl_ipc:register_handler(Type1, HandlerPid),

                    % Send message of registered type
                    yawl_ipc:send_message(Type1, msg_type1),
                    receive
                        {handler_message, _Msg1} -> ok
                    after 100 ->
                        ?assert(false, "Timeout waiting for type1 message")
                    end,

                    % Send message of different type
                    yawl_ipc:send_message(Type2, msg_type2),
                    receive
                        {handler_message, _Msg2} ->
                            ?assert(false, "Should not receive type2 message")
                    after 100 ->
                        ok  % Expected - no message for different type
                    end,

                    % Register for second type
                    {ok, _Id2} = yawl_ipc:register_handler(Type2, HandlerPid),

                    % Now should receive type 2
                    yawl_ipc:send_message(Type2, msg_type2),
                    receive
                        {handler_message, _Msg3} -> ok
                    after 100 ->
                        ?assert(false, "Timeout waiting for type2 message after registration")
                    end,

                    exit(HandlerPid, kill)
                end)]
     end}.

%%====================================================================
%% 6. Handler Management Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test listing handlers.
%% @end
%%--------------------------------------------------------------------
test_list_handlers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    % Create handler processes
                    Handler1 = spawn(fun() -> test_handler_loop(TestPid) end),
                    Handler2 = spawn(fun() -> test_handler_loop(TestPid) end),

                    Type1 = <<"handler_type_1">>,
                    Type2 = <<"handler_type_2">>,

                    % Register handlers
                    {ok, _} = yawl_ipc:register_handler(Type1, Handler1),
                    {ok, _} = yawl_ipc:register_handler(Type1, Handler2),
                    {ok, _} = yawl_ipc:register_handler(Type2, Handler1),

                    % List all handlers
                    AllHandlers = yawl_ipc:list_handlers(),
                    ?assert(is_map(AllHandlers)),

                    % List handlers for specific type
                    Type1Handlers = yawl_ipc:list_handlers(Type1),
                    ?assertEqual(2, length(Type1Handlers)),
                    ?assert(lists:member(Handler1, Type1Handlers)),
                    ?assert(lists:member(Handler2, Type1Handlers)),

                    Type2Handlers = yawl_ipc:list_handlers(Type2),
                    ?assertEqual(1, length(Type2Handlers)),
                    ?assert(lists:member(Handler1, Type2Handlers)),

                    % Cleanup
                    exit(Handler1, kill),
                    exit(Handler2, kill)
                end)]
     end}.

%%====================================================================
%% 7. Case Subscription Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test subscribing to case events.
%% @end
%%--------------------------------------------------------------------
test_case_subscription_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    CaseId = <<"case_12345">>,
                    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
                    {ok, _SubRef} = yawl_ipc:subscribe_to_case(CaseId, HandlerPid),

                    % Publish case event
                    yawl_ipc:publish_case_event(CaseId, <<"case_update">>, #{status => running}),

                    timer:sleep(100),

                    % Verify handler received case event
                    receive
                        {handler_case_event, Received} ->
                            ?assertNotEqual(undefined, Received),
                            ?assertEqual(<<"case_update">>, Received#message.type)
                    after 200 ->
                        ?assert(false, "Timeout waiting for case event")
                    end,

                    exit(HandlerPid, kill)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test listing case subscribers.
%% @end
%%--------------------------------------------------------------------
test_list_case_subscribers_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    CaseId = <<"case_list_test">>,

                    % Create subscriber processes
                    Sub1 = spawn(fun() -> test_handler_loop(TestPid) end),
                    Sub2 = spawn(fun() -> test_handler_loop(TestPid) end),

                    % Subscribe to case
                    {ok, _} = yawl_ipc:subscribe_to_case(CaseId, Sub1),
                    {ok, _} = yawl_ipc:subscribe_to_case(CaseId, Sub2),

                    % List subscribers
                    Subscribers = yawl_ipc:list_case_subscribers(CaseId),
                    ?assertEqual(2, length(Subscribers)),
                    ?assert(lists:member(Sub1, Subscribers)),
                    ?assert(lists:member(Sub2, Subscribers)),

                    % Cleanup
                    exit(Sub1, kill),
                    exit(Sub2, kill)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test unsubscribe from case.
%% @end
%%--------------------------------------------------------------------
test_unsubscribe_from_case_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    CaseId = <<"case_unsubscribe_test">>,
                    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
                    {ok, SubRef} = yawl_ipc:subscribe_to_case(CaseId, HandlerPid),

                    % Verify subscription
                    Subs = yawl_ipc:list_case_subscribers(CaseId),
                    ?assert(lists:member(HandlerPid, Subs)),

                    % Unsubscribe
                    yawl_ipc:unsubscribe_from_case(SubRef),
                    timer:sleep(50),

                    % Verify removal
                    NewSubs = yawl_ipc:list_case_subscribers(CaseId),
                    ?assertNot(lists:member(HandlerPid, NewSubs)),

                    exit(HandlerPid, kill)
                end)]
     end}.

%%====================================================================
%% 8. Broadcast Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test broadcast to multiple processes.
%% @end
%%--------------------------------------------------------------------
test_broadcast_message_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    % Create handler processes
                    Handlers = [spawn(fun() -> broadcast_handler(TestPid, N) end)
                                || N <- lists:seq(1, 3)],

                    MessageType = <<"broadcast_event">>,
                    Payload = #{broadcast => test},

                    % Broadcast message
                    yawl_ipc:broadcast_message(MessageType, Handlers, Payload),

                    timer:sleep(100),

                    % All handlers should receive the broadcast
                    receive
                        {handler_received, broadcast_handler1, broadcast_received} -> ok
                    after 200 ->
                        ?assert(false, "Timeout waiting for broadcast_handler1")
                    end,
                    receive
                        {handler_received, broadcast_handler2, broadcast_received} -> ok
                    after 200 ->
                        ?assert(false, "Timeout waiting for broadcast_handler2")
                    end,
                    receive
                        {handler_received, broadcast_handler3, broadcast_received} -> ok
                    after 200 ->
                        ?assert(false, "Timeout waiting for broadcast_handler3")
                    end,

                    % Cleanup
                    lists:foreach(fun(H) -> exit(H, kill) end, Handlers)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Helper handler for broadcast test.
%% @end
%%--------------------------------------------------------------------
broadcast_handler(TestPid, HandlerNum) ->
    receive
        {ipc_message, _Message} ->
            Key = list_to_atom("broadcast_handler" ++ integer_to_list(HandlerNum)),
            TestPid ! {handler_received, Key, broadcast_received},
            broadcast_handler(TestPid, HandlerNum);
        stop ->
            ok
    end.

%%====================================================================
%% 9. Synchronous Messaging Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test call_handler for synchronous request/response.
%% @end
%%--------------------------------------------------------------------
test_call_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    MessageType = <<"sync_request">>,
                    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
                    {ok, _HandlerId} = yawl_ipc:register_handler(MessageType, HandlerPid),

                    % Make synchronous call
                    Result = yawl_ipc:call_handler(MessageType, #{request => data}, 1000),

                    ?assertEqual({ok, ok}, Result),

                    exit(HandlerPid, kill)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test call_handler timeout when no handler registered.
%% @end
%%--------------------------------------------------------------------
test_call_handler_no_handler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    MessageType = <<"no_handler_request">>,

                    % Call without registering handler
                    Result = yawl_ipc:call_handler(MessageType, #{data => test}, 500),

                    ?assertEqual({error, no_handler}, Result)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test request_response helper.
%% @end
%%--------------------------------------------------------------------
test_request_response_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Create a responder process
                    Responder = spawn(fun() -> responder_loop() end),

                    % Make request
                    Result = yawl_ipc:request_response(Responder, #{request => data}),

                    ?assertEqual({ok, response_received}, Result),

                    % Cleanup
                    exit(Responder, kill)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Helper responder loop.
%% @end
%%--------------------------------------------------------------------
responder_loop() ->
    receive
        {ipc_request, _CorrelationId, _Payload, From} ->
            From ! {ipc_reply, response_received},
            responder_loop();
        stop ->
            ok
    end.

%%====================================================================
%% 10. Message Queue Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test message queue creation and retrieval.
%% @end
%%--------------------------------------------------------------------
test_message_queue_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    QueueName = <<"test_queue">>,

                    % Create queue
                    {ok, QueueName} = yawl_ipc:create_queue(QueueName),

                    % Send some messages (they get queued automatically)
                    yawl_ipc:send_message(<<"queue_test">>, msg1),
                    yawl_ipc:send_message(<<"queue_test">>, msg2),

                    timer:sleep(50),

                    % Get messages from queue
                    Messages = yawl_ipc:get_queue_messages(QueueName),

                    ?assert(length(Messages) >= 2)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test clearing a message queue.
%% @end
%%--------------------------------------------------------------------
test_clear_queue_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    QueueName = <<"clear_queue">>,
                    {ok, QueueName} = yawl_ipc:create_queue(QueueName),

                    % Send messages
                    yawl_ipc:send_message(<<"clear_test">>, msg1),
                    timer:sleep(50),

                    MessagesBefore = yawl_ipc:get_queue_messages(QueueName),
                    ?assert(length(MessagesBefore) > 0),

                    % Clear queue
                    yawl_ipc:clear_queue(QueueName),
                    timer:sleep(50),

                    MessagesAfter = yawl_ipc:get_queue_messages(QueueName),
                    ?assertEqual(0, length(MessagesAfter))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test deleting a message queue.
%% @end
%%--------------------------------------------------------------------
test_delete_queue_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    QueueName = <<"delete_queue">>,
                    {ok, QueueName} = yawl_ipc:create_queue(QueueName),

                    % Delete queue
                    yawl_ipc:delete_queue(QueueName),
                    timer:sleep(50),

                    % Queue should be empty/non-existent
                    Messages = yawl_ipc:get_queue_messages(QueueName),
                    ?assertEqual(0, length(Messages))
                end)]
     end}.

%%====================================================================
%% 11. Statistics and Monitoring Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test getting message statistics.
%% @end
%%--------------------------------------------------------------------
test_get_message_stats_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Get initial stats
                    Stats1 = yawl_ipc:get_message_stats(),
                    ?assert(is_map(Stats1)),
                    InitialSent = maps:get(sent, Stats1, 0),

                    % Send some messages
                    yawl_ipc:send_message(<<"stats_test">>, msg1),
                    yawl_ipc:send_message(<<"stats_test">>, msg2),
                    timer:sleep(50),

                    % Check updated stats
                    Stats2 = yawl_ipc:get_message_stats(),
                    NewSent = maps:get(sent, Stats2, 0),
                    ?assert(NewSent >= InitialSent)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test getting pending messages.
%% @end
%%--------------------------------------------------------------------
test_get_pending_messages_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    % Send message with no handlers (should be pending)
                    MessageType = <<"pending_msg_type">>,
                    yawl_ipc:send_message(MessageType, msg1),
                    timer:sleep(50),

                    % Get pending messages
                    Pending = yawl_ipc:get_pending_messages(),
                    ?assert(is_list(Pending)),
                    ?assert(length(Pending) > 0)
                end)]
     end}.

%%====================================================================
%% 12. Process Death Handling Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test handlers are cleaned up when process dies.
%% @end
%%--------------------------------------------------------------------
test_handler_cleanup_on_death_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    MessageType = <<"death_test_type">>,

                    % Create and register handler
                    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
                    {ok, _HandlerId} = yawl_ipc:register_handler(MessageType, HandlerPid),

                    % Verify handler is registered
                    Handlers1 = yawl_ipc:list_handlers(MessageType),
                    ?assert(lists:member(HandlerPid, Handlers1)),

                    % Kill handler process
                    exit(HandlerPid, kill),
                    timer:sleep(100),

                    % Handler should be removed
                    Handlers2 = yawl_ipc:list_handlers(MessageType),
                    ?assertNot(lists:member(HandlerPid, Handlers2))
                end)]
     end}.

%%====================================================================
%% 13. Correlation ID Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test message with correlation ID.
%% @end
%%--------------------------------------------------------------------
test_correlation_id_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    MessageType = <<"correlation_test">>,
                    CorrelationId = <<"corr_12345">>,

                    HandlerPid = spawn(fun() -> test_handler_loop(TestPid) end),
                    {ok, _HandlerId} = yawl_ipc:register_handler(MessageType, HandlerPid),

                    % Send message with correlation ID
                    yawl_ipc:send_message(MessageType, #{data => test}, CorrelationId),

                    timer:sleep(100),

                    % Verify correlation ID is preserved
                    receive
                        {handler_message, Received} ->
                            ?assertNotEqual(undefined, Received),
                            ?assertEqual(CorrelationId, Received#message.correlation_id)
                    after 200 ->
                        ?assert(false, "Timeout waiting for message")
                    end,

                    exit(HandlerPid, kill)
                end)]
     end}.

%%====================================================================
%% 14. Integration Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test complete IPC workflow.
%% @end
%%--------------------------------------------------------------------
test_complete_ipc_workflow_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Pid) ->
         [?_test(begin
                    TestPid = self(),
                    % 1. Create handler processes
                    Publisher = spawn(fun() -> publisher_loop() end),
                    Subscriber1 = spawn(fun() -> subscriber_loop(TestPid, 1) end),
                    Subscriber2 = spawn(fun() -> subscriber_loop(TestPid, 2) end),

                    % 2. Register subscribers
                    EventType = <<"workflow_event">>,
                    {ok, _} = yawl_ipc:register_handler(EventType, Subscriber1),
                    {ok, _} = yawl_ipc:register_handler(EventType, Subscriber2),

                    % 3. Subscribe to case
                    CaseId = <<"workflow_case_1">>,
                    {ok, _} = yawl_ipc:subscribe_to_case(CaseId, Subscriber1),

                    % 4. Publish event
                    yawl_ipc:send_message(EventType, #{step => 1, data => test}),
                    timer:sleep(100),

                    % 5. Verify both subscribers received
                    receive
                        {subscriber_received, 1} -> ok
                    after 200 ->
                        ?assert(false, "Timeout waiting for subscriber1")
                    end,
                    receive
                        {subscriber_received, 2} -> ok
                    after 200 ->
                        ?assert(false, "Timeout waiting for subscriber2")
                    end,

                    % 6. Publish case event
                    yawl_ipc:publish_case_event(CaseId, <<"case_update">>, #{status => active}),
                    timer:sleep(100),

                    % 7. Verify case event received
                    receive
                        {case_event_received, 1} -> ok
                    after 200 ->
                        ?assert(false, "Timeout waiting for case event")
                    end,

                    % 8. Check stats
                    Stats = yawl_ipc:get_message_stats(),
                    ?assert(maps:get(sent, Stats, 0) > 0),

                    % 9. Cleanup
                    exit(Publisher, kill),
                    exit(Subscriber1, kill),
                    exit(Subscriber2, kill),

                    ok
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Helper publisher loop.
%% @end
%%--------------------------------------------------------------------
publisher_loop() ->
    receive
        stop -> ok
    end.

%%--------------------------------------------------------------------
%% @doc Helper subscriber loop.
%% @end
%%--------------------------------------------------------------------
subscriber_loop(TestPid, SubscriberNum) ->
    receive
        {ipc_message, _Message} ->
            TestPid ! {subscriber_received, SubscriberNum},
            subscriber_loop(TestPid, SubscriberNum);
        {ipc_case_event, _CaseId, _Message} ->
            TestPid ! {case_event_received, SubscriberNum},
            subscriber_loop(TestPid, SubscriberNum);
        stop ->
            ok
    end.

%%====================================================================
%% EUnit Test Export
%%====================================================================
%% @doc Returns all test generators for EUnit discovery.
%% @end
%%--------------------------------------------------------------------
test() ->
    [
        test_start_ipc_test_(),
        test_stop_restart_ipc_test_(),
        test_publish_subscribe_test_(),
        test_event_delivery_test_(),
        test_unsubscribe_test_(),
        test_unsubscribe_by_type_test_(),
        test_cross_process_publish_test_(),
        test_multiple_subscribers_test_(),
        test_filtered_events_test_(),
        test_list_handlers_test_(),
        test_case_subscription_test_(),
        test_list_case_subscribers_test_(),
        test_unsubscribe_from_case_test_(),
        test_broadcast_message_test_(),
        test_call_handler_test_(),
        test_call_handler_no_handler_test_(),
        test_request_response_test_(),
        test_message_queue_test_(),
        test_clear_queue_test_(),
        test_delete_queue_test_(),
        test_get_message_stats_test_(),
        test_get_pending_messages_test_(),
        test_handler_cleanup_on_death_test_(),
        test_correlation_id_test_(),
        test_complete_ipc_workflow_test_()
    ].
