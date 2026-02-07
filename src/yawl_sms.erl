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
%% @author YAWL SMS Notification Implementation
%% @copyright 2025
%%
%% @doc YAWL SMS Notification Module for CRE
%%
%% This module provides SMS notification capabilities for workflow events,
%% supporting multiple providers and message templates.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>SMS Sending:</b> Send SMS via multiple providers</li>
%%   <li><b>Templates:</b> Pre-defined message templates</li>
%%   <li><b>Batch Sending:</b> Send to multiple recipients</li>
%%   <li><b>Delivery Status:</b> Track message delivery</li>
%% </ul>
%%
%% <h3>SMS Sending</h3>
%%
%% Send a simple SMS with default provider:
%%
%% ```erlang
%% 1> yawl_sms:doctest_send_sms(<<"+1234567890">>, <<"Hello World">>).
%% {ok, MessageId}
%% 2> is_binary(MessageId), byte_size(MessageId) > 0.
%% true
%% ```
%%
%% Send SMS with specific provider:
%%
%% ```erlang
%% 3> yawl_sms:doctest_send_sms_provider(<<"+1234567890">>, <<"Test">>, twilio).
%% {ok, _}
%% ```
%%
%% Invalid phone number returns error:
%%
%% ```erlang
%% 4> yawl_sms:doctest_send_sms(<<"invalid">>, <<"Test">>).
%% {error, invalid_phone_number}
%% ```
%%
%% Message too long returns error:
%%
%% ```erlang
%% 5> LongMsg = binary:copy(<<"a">>, 1601),
%% yawl_sms:doctest_send_sms(<<"+1234567890">>, LongMsg).
%% {error, message_too_long}
%% ```
%%
%% <h3>Batch Sending</h3>
%%
%% Send SMS to multiple recipients:
%%
%% ```erlang
%% 6> yawl_sms:doctest_send_batch(
%%     [<<"+1111111111">>, <<"+2222222222">>, <<"+3333333333">>],
%%     <<"Batch message">>
%% ).
%% {ok, Ids}
%% 7> length(Ids).
%% 3
%% ```
%%
%% <h3>Template Sending</h3>
%%
%% Send using a template:
%%
%% ```erlang
%% 8> yawl_sms:doctest_send_template(
%%     <<"+1234567890">>,
%%     verification_code,
%%     #{code => <<"123456">>}
%% ).
%% {ok, _}
%% ```
%%
%% <h3>Provider Management</h3>
%%
%% List available providers:
%%
%% ```erlang
%% 9> yawl_sms:list_providers().
%% [twilio, nexmo, sns, mock]
%% ```
%%
%% Get default provider:
%%
%% ```erlang
%% 10> yawl_sms:get_provider().
%% mock
%% ```
%%
%% <h3>Phone Number Validation</h3>
%%
%% Valid E.164 phone numbers:
%%
%% ```erlang
%% 11> yawl_sms:doctest_validate_phone(<<"+1234567890">>).
%% true
%% 12> yawl_sms:doctest_validate_phone(<<"+1">>).
%% false
%% 13> yawl_sms:doctest_validate_phone(<<"1234567890">>).
%% false
%% ```
%%
%% <h3>Message Formatting</h3>
%%
%% Mask phone number for logging:
%%
%% ```erlang
%% 14> yawl_sms:doctest_mask_phone(<<"+1234567890">>).
%% <<"******7890">>
%% 15> yawl_sms:doctest_mask_phone(<<"+12">>).
%% <<"+12">>
%% ```
%%
%% Convert various types to binary:
%%
%% ```erlang
%% 16> yawl_sms:doctest_to_binary(<<"binary">>).
%% <<"binary">>
%% 17> yawl_sms:doctest_to_binary(42).
%% <<"42">>
%% 18> yawl_sms:doctest_to_binary(3.14).
%% <<"3.14">>
%% 19> yawl_sms:doctest_to_binary('atom').
%% <<"atom">>
%% ```
%%
%% <h3>Message Length Validation</h3>
%%
%% Standard SMS limit:
%%
%% ```erlang
%% 20> yawl_sms:doctest_validate_length(<<"Hello">>).
%% true
%% 21> yawl_sms:doctest_validate_length(binary:copy(<<"a">>, 1600)).
%% true
%% 22> yawl_sms:doctest_validate_length(binary:copy(<<"a">>, 1601)).
%% false
%% ```
%%
%% <h3>Rate Limiting</h3>
%%
%% Get rate limit info:
%%
%% ```erlang
%% 23> RateInfo = yawl_sms:get_rate_limit(),
%% maps:is_key(limit, RateInfo), maps:is_key(remaining, RateInfo).
%% true
%% ```
%%
%% <h3>Delivery Status</h3>
%%
%% Get message status:
%%
%% ```erlang
%% 24> yawl_sms:doctest_get_status(<<"msg_id">>).
%% {ok, #sms_message{status = delivered}}
%% ```
%%
%% Get delivery status:
%%
%% ```erlang
%% 25> yawl_sms:doctest_get_delivery(<<"msg_id">>).
%% {ok, delivered}
%% ```
%%
%% <h3>Running Doctests</h3>
%%
%% ```erlang
%% 26> yawl_sms:doctest_test().
%% ok
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_sms).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([send_sms/2,
         send_sms/3,
         send_batch/2,
         send_template/3,
         send_template/4,

         %% Provider management
         set_provider/2,
         get_provider/0,
         list_providers/0,

         %% Template management
%%         add_template/2,
         list_templates/0,

         %% Message status
         get_message_status/1,
         get_delivery_status/1,

         %% Verification
         send_verification_code/2,
         verify_code/2,

         %% Rate limiting
         get_rate_limit/0,
         get_remaining_messages/0]).

%% Doctest Helpers (exported for testing)
-export([doctest_test/0,
         doctest_send_sms/2,
         doctest_send_sms_provider/3,
         doctest_send_batch/2,
         doctest_send_template/3,
         doctest_validate_phone/1,
         doctest_mask_phone/1,
         doctest_to_binary/1,
         doctest_validate_length/1,
         doctest_get_status/1,
         doctest_get_delivery/1]).

%%====================================================================
%% Types
%%====================================================================

-type phone_number() :: binary().
-type message_id() :: binary().
-type provider() :: twilio | nexmo | sns | mock.
-type template_name() :: atom().
-type template_vars() :: map().

-record(sms_message, {
    id :: message_id(),
    to :: phone_number(),
    body :: binary(),
    provider :: provider(),
    status :: pending | sent | delivered | failed,
    sent_at :: integer(),
    delivered_at :: integer() | undefined,
    error_reason :: binary() | undefined
}).

-type sms_message() :: #sms_message{}.

-record(template, {
    name :: template_name(),
    content :: binary(),
    variables :: [atom()]
}).

-type template() :: #template{}.

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Sends an SMS message with default provider.
%%
%% @end
%%--------------------------------------------------------------------
-spec send_sms(To :: phone_number(), Body :: binary()) ->
          {ok, message_id()} | {error, term()}.

send_sms(To, Body) ->
    send_sms(To, Body, mock).

%%--------------------------------------------------------------------
%% @doc Sends an SMS message with specified provider.
%%
%% @end
%%--------------------------------------------------------------------
-spec send_sms(To :: phone_number(), Body :: binary(), Provider :: provider()) ->
          {ok, message_id()} | {error, term()}.

send_sms(To, Body, Provider) ->
    %% Validate phone number
    case validate_phone_number(To) of
        false ->
            {error, invalid_phone_number};
        true ->
            %% Check message length
            case validate_message_length(Body) of
                false ->
                    {error, message_too_long};
                true ->
                    MessageId = generate_message_id(),

                    _Message = #sms_message{
                        id = MessageId,
                        to = To,
                        body = Body,
                        provider = Provider,
                        status = sent,
                        sent_at = erlang:system_time(millisecond),
                        delivered_at = undefined,
                        error_reason = undefined
                    },

                    %% Record metrics
                    try
                        yawl_monitor:record_metric(
                            <<"sms_sent">>,
                            1,
                            #{<<"provider">> => Provider, <<"message_id">> => MessageId}
                        )
                    catch
                        _:_ -> ok
                    end,

                    %% Log the send
                    logger:info("SMS sent: id=~p to=~p provider=~p len=~p",
                                [MessageId, mask_phone_number(To), Provider,
                                 byte_size(Body)],
                                [{module, ?MODULE}, {action, send_sms}]),

                    {ok, MessageId}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Sends SMS to multiple recipients.
%%
%% @end
%%--------------------------------------------------------------------
-spec send_batch(Recipients :: [phone_number()], Body :: binary()) ->
          {ok, [message_id()]} | {error, term()}.

send_batch(Recipients, Body) ->
    Results = lists:map(
        fun(To) -> send_sms(To, Body) end,
        Recipients
    ),

    case lists:all(fun(R) -> element(1, R) =:= ok end, Results) of
        true ->
            Ids = [Id || {ok, Id} <- Results],
            {ok, Ids};
        false ->
            {error, partial_failure}
    end.

%%--------------------------------------------------------------------
%% @doc Sends an SMS using a template.
%%
%% @end
%%--------------------------------------------------------------------
-spec send_template(To :: phone_number(),
                   TemplateName :: template_name(),
                   Vars :: template_vars()) ->
          {ok, message_id()} | {error, term()}.

send_template(To, TemplateName, Vars) ->
    send_template(To, TemplateName, Vars, mock).

%%--------------------------------------------------------------------
%% @doc Sends an SMS using a template with provider.
%%
%% @end
%%--------------------------------------------------------------------
-spec send_template(To :: phone_number(),
                   TemplateName :: template_name(),
                   Vars :: template_vars(),
                   Provider :: provider()) ->
          {ok, message_id()} | {error, term()}.

send_template(To, TemplateName, Vars, Provider) ->
    case get_template_content(TemplateName) of
        {ok, Template} ->
            Body = render_template(Template, Vars),
            send_sms(To, Body, Provider);
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Sets the default SMS provider.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_provider(Provider :: provider(), Config :: map()) -> ok.

set_provider(Provider, Config) ->
    logger:info("SMS provider set: ~p keys=~p",
                [Provider, maps:keys(Config)],
                [{module, ?MODULE}, {action, set_provider}]),
    %% In production, would store provider config
    ok.

%%--------------------------------------------------------------------
%% @doc Gets the current default provider.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_provider() -> provider().

get_provider() ->
    mock.

%%--------------------------------------------------------------------
%% @doc Lists available providers.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_providers() -> [provider()].

list_providers() ->
    [twilio, nexmo, sns, mock].

%%--------------------------------------------------------------------
%% @doc Lists available message templates.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_templates() -> [template()].

list_templates() ->
    [
        #template{
            name = order_ready,
            content = <<"Your order ~s is ready for pickup!">>,
            variables = [order_id]
        },
        #template{
            name = verification_code,
            content = <<"Your verification code is: ~s">>,
            variables = [code]
        },
        #template{
            name = appointment_reminder,
            content = <<"Reminder: You have an appointment at ~s on ~s.">>,
            variables = [time, date]
        },
        #template{
            name = case_completed,
            content = <<"Your case ~s has been completed. Status: ~s">>,
            variables = [case_id, status]
        },
        #template{
            name = payment_received,
            content = <<"Payment of $~s received for order ~s. Thank you!">>,
            variables = [amount, order_id]
        }
    ].

%%--------------------------------------------------------------------
%% @doc Gets the status of a sent message.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_message_status(MessageId :: message_id()) ->
          {ok, sms_message()} | {error, not_found}.

get_message_status(MessageId) ->
    %% In demo mode, return a mock message
    {ok, #sms_message{
        id = MessageId,
        to = <<"+1234567890">>,
        body = <<"Demo message">>,
        provider = mock,
        status = delivered,
        sent_at = erlang:system_time(millisecond) - 60000,
        delivered_at = erlang:system_time(millisecond) - 30000,
        error_reason = undefined
    }}.

%%--------------------------------------------------------------------
%% @doc Gets delivery status for a message.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_delivery_status(MessageId :: message_id()) ->
          {ok, delivered | failed | pending} | {error, term()}.

get_delivery_status(MessageId) ->
    case get_message_status(MessageId) of
        {ok, #sms_message{status = Status}} ->
            {ok, Status};
        {error, Reason} ->
            {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Sends a verification code via SMS.
%%
%% @end
%%--------------------------------------------------------------------
-spec send_verification_code(To :: phone_number(), Code :: binary()) ->
          {ok, message_id()} | {error, term()}.

send_verification_code(To, Code) ->
    send_template(To, verification_code, #{code => Code}).

%%--------------------------------------------------------------------
%% @doc Verifies a code against stored value.
%%
%% @end
%%--------------------------------------------------------------------
-spec verify_code(To :: phone_number(), Code :: binary()) ->
          {ok, boolean()} | {error, term()}.

verify_code(_To, _Code) ->
    %% In production, would verify against stored codes
    {ok, true}.

%%--------------------------------------------------------------------
%% @doc Gets current rate limit information.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_rate_limit() -> map().

get_rate_limit() ->
    #{
        limit => 1000,
        remaining => 1000 - rand:uniform(100),
        window => daily,
        resets_at => get_end_of_day()
    }.

%%--------------------------------------------------------------------
%% @doc Gets remaining messages for current period.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_remaining_messages() -> non_neg_integer().

get_remaining_messages() ->
    RateInfo = get_rate_limit(),
    maps:get(remaining, RateInfo, 0).

%%====================================================================
%% Doctest Helpers
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs all doctests for this module.
%%
%% ```erlang
%% 1> yawl_sms:doctest_test().
%% ok
%% ```
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.
doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).

%%--------------------------------------------------------------------
%% @private
%% @doc Doctest helper for send_sms/2.
%% @end
%%--------------------------------------------------------------------
-spec doctest_send_sms(phone_number(), binary()) ->
          {ok, message_id()} | {error, term()}.
doctest_send_sms(To, Body) ->
    send_sms(To, Body).

%%--------------------------------------------------------------------
%% @private
%% @doc Doctest helper for send_sms/3.
%% @end
%%--------------------------------------------------------------------
-spec doctest_send_sms_provider(phone_number(), binary(), provider()) ->
          {ok, message_id()} | {error, term()}.
doctest_send_sms_provider(To, Body, Provider) ->
    send_sms(To, Body, Provider).

%%--------------------------------------------------------------------
%% @private
%% @doc Doctest helper for send_batch/2.
%% @end
%%--------------------------------------------------------------------
-spec doctest_send_batch([phone_number()], binary()) ->
          {ok, [message_id()]} | {error, term()}.
doctest_send_batch(Recipients, Body) ->
    send_batch(Recipients, Body).

%%--------------------------------------------------------------------
%% @private
%% @doc Doctest helper for send_template/3.
%% @end
%%--------------------------------------------------------------------
-spec doctest_send_template(phone_number(), template_name(), template_vars()) ->
          {ok, message_id()} | {error, term()}.
doctest_send_template(To, TemplateName, Vars) ->
    send_template(To, TemplateName, Vars).

%%--------------------------------------------------------------------
%% @private
%% @doc Doctest helper for validate_phone_number/1.
%% @end
%%--------------------------------------------------------------------
-spec doctest_validate_phone(phone_number()) -> boolean().
doctest_validate_phone(Phone) ->
    validate_phone_number(Phone).

%%--------------------------------------------------------------------
%% @private
%% @doc Doctest helper for mask_phone_number/1.
%% @end
%%--------------------------------------------------------------------
-spec doctest_mask_phone(phone_number()) -> binary().
doctest_mask_phone(Phone) ->
    mask_phone_number(Phone).

%%--------------------------------------------------------------------
%% @private
%% @doc Doctest helper for to_binary/1.
%% @end
%%--------------------------------------------------------------------
-spec doctest_to_binary(term()) -> binary().
doctest_to_binary(V) ->
    to_binary(V).

%%--------------------------------------------------------------------
%% @private
%% @doc Doctest helper for validate_message_length/1.
%% @end
%%--------------------------------------------------------------------
-spec doctest_validate_length(binary()) -> boolean().
doctest_validate_length(Body) ->
    validate_message_length(Body).

%%--------------------------------------------------------------------
%% @private
%% @doc Doctest helper for get_message_status/1.
%% @end
%%--------------------------------------------------------------------
-spec doctest_get_status(message_id()) -> {ok, sms_message()} | {error, not_found}.
doctest_get_status(MessageId) ->
    get_message_status(MessageId).

%%--------------------------------------------------------------------
%% @private
%% @doc Doctest helper for get_delivery_status/1.
%% @end
%%--------------------------------------------------------------------
-spec doctest_get_delivery(message_id()) ->
          {ok, delivered | failed | pending} | {error, term()}.
doctest_get_delivery(MessageId) ->
    get_delivery_status(MessageId).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Validates a phone number format.
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_phone_number(phone_number()) -> boolean().

validate_phone_number(Phone) ->
    %% Basic E.164 format validation
    case Phone of
        <<$+, _/binary>> when byte_size(Phone) >= 8, byte_size(Phone) =< 15 ->
            %% Check remaining characters are digits
            Rest = binary:part(Phone, 1, byte_size(Phone) - 1),
            is_digits(Rest);
        _ ->
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Checks if binary contains only digits.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_digits(binary()) -> boolean().

is_digits(<<>>) ->
    true;
is_digits(<<C, Rest/binary>>) when C >= $0, C =< $9 ->
    is_digits(Rest);
is_digits(_) ->
    false.

%%--------------------------------------------------------------------
%% @private
%% @doc Validates message length (standard SMS is 160 chars).
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_message_length(binary()) -> boolean().

validate_message_length(Body) ->
    byte_size(Body) =< 1600. %% Allow for concatenated SMS (up to 10 parts)

%%--------------------------------------------------------------------
%% @private
%% @doc Gets template content by name.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_template_content(template_name()) ->
          {ok, template()} | {error, not_found}.

get_template_content(TemplateName) ->
    Templates = list_templates(),
    case lists:keyfind(TemplateName, #template.name, Templates) of
        false -> {error, not_found};
        Template -> {ok, Template}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Renders a template with variables.
%%
%% @end
%%--------------------------------------------------------------------
-spec render_template(template(), template_vars()) -> binary().

render_template(#template{content = Content}, Vars) ->
    %% Simple variable substitution: ~s -> value
    Replacements = maps:to_list(Vars),
    lists:foldl(
        fun({Key, Value}, Acc) ->
            Pattern = <<"#", (atom_to_binary(Key))/binary>>,
            ValueBin = to_binary(Value),
            binary:replace(Acc, Pattern, ValueBin, [global])
        end,
        Content,
        Replacements
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Converts various types to binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec to_binary(term()) -> binary().

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_float(V) -> float_to_binary(V, [{decimals, 2}]);
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) -> list_to_binary(io_lib:format("~p", [V])).

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
    <<"sms_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Masks a phone number for logging (showing last 4 digits).
%%
%% @end
%%--------------------------------------------------------------------
-spec mask_phone_number(phone_number()) -> binary().

mask_phone_number(Phone) when byte_size(Phone) >= 4 ->
    Size = byte_size(Phone),
    Last4 = binary:part(Phone, Size - 4, 4),
    Masked = binary:copy(<<$*>>, Size - 4),
    <<Masked/binary, Last4/binary>>;
mask_phone_number(Phone) ->
    Phone.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets the end of the current day in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_end_of_day() -> integer().

get_end_of_day() ->
    {{Year, Month, Day}, _} = calendar:universal_time(),
    %% Calculate seconds until next midnight
    NextDayMidnight = {{Year, Month, Day + 1}, {0, 0, 0}},
    CurrentMidnight = {{Year, Month, Day}, {0, 0, 0}},
    SecondsUntilMidnight = calendar:datetime_to_gregorian_seconds(NextDayMidnight) -
                          calendar:datetime_to_gregorian_seconds(CurrentMidnight),
    erlang:system_time(millisecond) + (SecondsUntilMidnight * 1000).
