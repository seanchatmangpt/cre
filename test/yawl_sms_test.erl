%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Chicago TDD Tests for YAWL SMS Notification Module
%%
%% Test-First Development: RED -> GREEN -> REFACTOR
%%
%% @doc YAWL SMS Tests
%% @end

-module(yawl_sms_test).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%% The records are defined in yawl_sms.erl, we access them via tuple operations
%% since we don't have a separate header file.

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Mock yawl_monitor to avoid dependency
    try
        meck:new(yawl_monitor, [passthrough, no_link]),
        meck:expect(yawl_monitor, record_metric, fun(_, _, _) -> ok end),
        ok
    catch
        _:_ ->
            %% meck may not be available, skip mocking
            ok
    end.

cleanup(_Mock) ->
    try
        meck:unload(yawl_monitor),
        ok
    catch
        _:_ -> ok
    end.

%%====================================================================
%% Send SMS Tests
%%====================================================================

send_sms_default_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, MessageId} = yawl_sms:send_sms(<<"+1234567890">>, <<"Test message">>),
            ?assert(is_binary(MessageId)),
            ?assertMatch(<<"sms_", _/binary>>, MessageId)
         end)]
     end}.

send_sms_with_provider_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, MessageId} = yawl_sms:send_sms(<<"+1234567890">>, <<"Test">>, twilio),
            ?assert(is_binary(MessageId))
         end)]
     end}.

send_sms_invalid_phone_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            ?assertEqual({error, invalid_phone_number},
                yawl_sms:send_sms(<<"invalid">>, <<"Test">>))
         end)]
     end}.

send_sms_too_long_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            LongMsg = binary:copy(<<"a">>, 1601),
            ?assertEqual({error, message_too_long},
                yawl_sms:send_sms(<<"+1234567890">>, LongMsg))
         end)]
     end}.

send_sms_max_length_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            MaxMsg = binary:copy(<<"b">>, 1600),
            {ok, MessageId} = yawl_sms:send_sms(<<"+1234567890">>, MaxMsg),
            ?assert(is_binary(MessageId))
         end)]
     end}.

send_sms_empty_message_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, MessageId} = yawl_sms:send_sms(<<"+1234567890">>, <<>>),
            ?assert(is_binary(MessageId))
         end)]
     end}.

send_sms_phone_without_plus_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            ?assertEqual({error, invalid_phone_number},
                yawl_sms:send_sms(<<"1234567890">>, <<"Test">>))
         end)]
     end}.

send_sms_phone_too_short_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            ?assertEqual({error, invalid_phone_number},
                yawl_sms:send_sms(<<"+123456">>, <<"Test">>))
         end)]
     end}.

send_sms_phone_too_long_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            %% Create a phone number that's too long (16 digits after +)
            LongPhone = <<"+", (binary:copy(<<"1">>, 15))/binary>>,
            ?assertEqual({error, invalid_phone_number},
                yawl_sms:send_sms(LongPhone, <<"Test">>))
         end)]
     end}.

%%====================================================================
%% Batch Send Tests
%%====================================================================

send_batch_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            Recipients = [<<"+1111111111">>, <<"+2222222222">>, <<"+3333333333">>],
            {ok, MessageIds} = yawl_sms:send_batch(Recipients, <<"Batch test">>),
            ?assertEqual(3, length(MessageIds))
         end)]
     end}.

send_batch_empty_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, MessageIds} = yawl_sms:send_batch([], <<"Test">>),
            ?assertEqual([], MessageIds)
         end)]
     end}.

send_batch_partial_failure_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            Recipients = [<<"+1111111111">>, <<"invalid">>, <<"+3333333333">>],
            Result = yawl_sms:send_batch(Recipients, <<"Test">>),
            ?assertEqual({error, partial_failure}, Result)
         end)]
     end}.

%%====================================================================
%% Template Tests
%%====================================================================

send_template_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, MessageId} = yawl_sms:send_template(
                <<"+1234567890">>,
                verification_code,
                #{code => <<"123456">>}
            ),
            ?assert(is_binary(MessageId))
         end)]
     end}.

send_template_with_provider_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, MessageId} = yawl_sms:send_template(
                <<"+1234567890">>,
                order_ready,
                #{order_id => <<"ABC123">>},
                twilio
            ),
            ?assert(is_binary(MessageId))
         end)]
     end}.

send_template_not_found_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            ?assertEqual({error, not_found},
                yawl_sms:send_template(
                    <<"+1234567890">>,
                    unknown_template,
                    #{}
                ))
         end)]
     end}.

list_templates_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            Templates = yawl_sms:list_templates(),
            ?assert(is_list(Templates)),
            ?assert(length(Templates) >= 5)
         end)]
     end}.

template_names_exist_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            Templates = yawl_sms:list_templates(),
            %% Template records have name as element 2: {template, Name, Content, Vars}
            Names = [element(2, T) || T <- Templates],
            ?assert(lists:member(order_ready, Names)),
            ?assert(lists:member(verification_code, Names)),
            ?assert(lists:member(appointment_reminder, Names)),
            ?assert(lists:member(case_completed, Names)),
            ?assert(lists:member(payment_received, Names))
         end)]
     end}.

%%====================================================================
%% Provider Management Tests
%%====================================================================

set_provider_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            Config = #{api_key => <<"test-key">>},
            ?assertEqual(ok, yawl_sms:set_provider(twilio, Config))
         end)]
     end}.

get_provider_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            ?assertEqual(mock, yawl_sms:get_provider())
         end)]
     end}.

list_providers_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            Providers = yawl_sms:list_providers(),
            ?assert(lists:member(twilio, Providers)),
            ?assert(lists:member(nexmo, Providers)),
            ?assert(lists:member(sns, Providers)),
            ?assert(lists:member(mock, Providers))
         end)]
     end}.

%%====================================================================
%% Message Status Tests
%%====================================================================

get_message_status_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, Status} = yawl_sms:get_message_status(<<"sms_test_123">>),
            ?assert(is_tuple(Status)),
            ?assertEqual(<<"sms_test_123">>, element(2, Status))
         end)]
     end}.

get_delivery_status_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, Status} = yawl_sms:get_delivery_status(<<"sms_status_test">>),
            ?assert(Status =:= delivered orelse Status =:= sent orelse Status =:= pending)
         end)]
     end}.

%%====================================================================
%% Verification Code Tests
%%====================================================================

send_verification_code_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, MessageId} = yawl_sms:send_verification_code(
                <<"+1234567890">>,
                <<"654321">>
            ),
            ?assert(is_binary(MessageId))
         end)]
     end}.

verify_code_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, IsValid} = yawl_sms:verify_code(<<"+1234567890">>, <<"123456">>),
            ?assert(is_boolean(IsValid))
         end)]
     end}.

%%====================================================================
%% Rate Limiting Tests
%%====================================================================

get_rate_limit_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            RateInfo = yawl_sms:get_rate_limit(),
            ?assert(is_map(RateInfo)),
            ?assert(maps:is_key(limit, RateInfo)),
            ?assert(maps:is_key(remaining, RateInfo)),
            ?assert(maps:is_key(window, RateInfo)),
            ?assert(maps:is_key(resets_at, RateInfo))
         end)]
     end}.

get_remaining_messages_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            Remaining = yawl_sms:get_remaining_messages(),
            ?assert(is_integer(Remaining)),
            ?assert(Remaining >= 0),
            ?assert(Remaining =< 1000)
         end)]
     end}.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

unicode_message_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, MessageId} = yawl_sms:send_sms(<<"+1234567890">>, <<"Hello 🌍 🌎 🌏">>),
            ?assert(is_binary(MessageId))
         end)]
     end}.

newlines_in_message_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, MessageId} = yawl_sms:send_sms(
                <<"+1234567890">>,
                <<"Line 1\nLine 2\r\nLine 3">>
            ),
            ?assert(is_binary(MessageId))
         end)]
     end}.

special_chars_message_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            {ok, MessageId} = yawl_sms:send_sms(
                <<"+1234567890">>,
                <<"Test with: @, #, $, %, ^, &, *, !">>
            ),
            ?assert(is_binary(MessageId))
         end)]
     end}.

all_providers_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            Providers = [twilio, nexmo, sns, mock],
            Results = [P || P <- Providers,
                element(1, yawl_sms:send_sms(<<"+1234567890">>, <<"Test">>, P)) =:= ok
            ],
            ?assertEqual(4, length(Results))
         end)]
     end}.

%%====================================================================
%% Phone Number Validation Edge Cases
%%====================================================================

phone_with_letters_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            ?assertEqual({error, invalid_phone_number},
                yawl_sms:send_sms(<<"+1234567890a">>, <<"Test">>))
         end)]
     end}.

phone_with_spaces_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            ?assertEqual({error, invalid_phone_number},
                yawl_sms:send_sms(<<"+1234 567 890">>, <<"Test">>))
         end)]
     end}.

phone_with_special_chars_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            ?assertEqual({error, invalid_phone_number},
                yawl_sms:send_sms(<<"+1-(555)-123-4567">>, <<"Test">>))
         end)]
     end}.

valid_phone_variations_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            %% Valid E.164 format phone numbers
            {ok, _} = yawl_sms:send_sms(<<"+14155552671">>, <<"Test 1">>),
            {ok, _} = yawl_sms:send_sms(<<"+442071234567">>, <<"Test 2">>),
            {ok, _} = yawl_sms:send_sms(<<"+8613812345678">>, <<"Test 3">>),
            ?assert(true)
         end)]
     end}.

%%====================================================================
%% Template Variable Tests
%%====================================================================

template_with_multiple_vars_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            %% Test case_completed template with multiple variables
            {ok, MessageId} = yawl_sms:send_template(
                <<"+1234567890">>,
                case_completed,
                #{case_id => <<"CASE-123">>, status => <<"approved">>}
            ),
            ?assert(is_binary(MessageId))
         end)]
     end}.

template_with_payment_vars_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            %% Test payment_received template
            {ok, MessageId} = yawl_sms:send_template(
                <<"+1234567890">>,
                payment_received,
                #{amount => <<"99.99">>, order_id => <<"ORD-456">>}
            ),
            ?assert(is_binary(MessageId))
         end)]
     end}.

template_with_appointment_vars_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [?_test(begin
            %% Test appointment_reminder template
            {ok, MessageId} = yawl_sms:send_template(
                <<"+1234567890">>,
                appointment_reminder,
                #{time => <<"2:30 PM">>, date => <<"January 15th">>}
            ),
            ?assert(is_binary(MessageId))
         end)]
     end}.
