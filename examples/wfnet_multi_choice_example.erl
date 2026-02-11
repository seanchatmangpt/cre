%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @author CRE Team
%% @version 0.3.0
%% @doc Multi-Choice Workflow Pattern (WCP-07) Example
%%
%% This example demonstrates the Multi-Choice workflow pattern (WCP-07)
%% which allows selecting multiple branches simultaneously based on
%% runtime conditions.
%%
%% <h3>Use Cases</h3>
%% <ul>
%%   <li>Document approval routing (multiple reviewers)</li>
%%   <li>Parallel processing of qualifying tasks</li>
%%   <li>Conditional notification systems</li>
%%   <li>Multi-stage validation workflows</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_multi_choice_example).
-export([
    approval_workflow/0,
    notification_workflow/1,
    pricing_strategy/1,
    risk_assessment/1
]).

%%====================================================================
%% Examples
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Document approval workflow example.
%%
%% Demonstrates selecting multiple approval branches based on
%% document type and value.
%%
%% @end
%%--------------------------------------------------------------------
approval_workflow() ->
    %% Define approval branches with conditions
    Branches = [
        {manager_approval, #{
            condition => fun(Doc) ->
                maps:get(type, Doc) =:= expense andalso
                maps:get(amount, Doc, 0) > 1000
            end,
            handler => fun(Doc) ->
                {approved, manager, maps:get(id, Doc)}
            end
        }},
        {finance_review, #{
            condition => fun(Doc) ->
                maps:get(type, Doc) =:= expense andalso
                maps:get(amount, Doc, 0) > 5000
            end,
            handler => fun(Doc) ->
                {review, finance, maps:get(id, Doc)}
            end
        }},
        {hr_notification, #{
            condition => fun(Doc) ->
                maps:get(type, Doc) =:= leave_request
            end,
            handler => fun(Doc) ->
                {notify, hr, maps:get(employee, Doc)}
            end
        }}
    ],

    %% Example documents
    LowExpense = #{type => expense, amount => 500, id => exp001},
    HighExpense = #{type => expense, amount => 6000, id => exp002},
    LeaveRequest = #{type => leave_request, employee => john_doe},

    %% Execute workflow
    io:format("Low expense (~$p):~n", [LowExpense]),
    {ok, Result1} = wfnet_multi_choice:execute(Branches, LowExpense),
    io:format("  Selected: ~p~n~n", [Result1]),

    io:format("High expense (~$p):~n", [HighExpense]),
    {ok, Result2} = wfnet_multi_choice:execute(Branches, HighExpense),
    io:format("  Selected: ~p~n~n", [Result2]),

    io:format("Leave request (~$p):~n", [LeaveRequest]),
    {ok, Result3} = wfnet_multi_choice:execute(Branches, LeaveRequest),
    io:format("  Selected: ~p~n~n", [Result3]),

    ok.

%%--------------------------------------------------------------------
%% @doc Notification workflow example.
%%
%% Sends notifications to multiple channels based on
%% user preferences and urgency.
%%
%% @end
%%--------------------------------------------------------------------
notification_workflow(UserData) ->
    Urgency = maps:get(urgency, UserData, normal),

    Branches = [
        {email_notification, #{
            condition => fun(Data) ->
                maps:get(email_enabled, Data, true) andalso
                maps:get(urgency, Data, normal) =/= critical
            end,
            handler => fun(Data) ->
                {send_email, maps:get(email, Data), maps:get(message, Data)}
            end
        }},
        {sms_notification, #{
            condition => fun(Data) ->
                (maps:get(sms_enabled, Data, false) orelse
                 maps:get(urgency, Data, normal) =:= critical) andalso
                maps:get(phone, Data, undefined) =/= undefined
            end,
            handler => fun(Data) ->
                {send_sms, maps:get(phone, Data), maps:get(message, Data)}
            end
        }},
        {push_notification, #{
            condition => fun(Data) ->
                maps:get(push_enabled, Data, true) andalso
                maps:get(device_id, Data, undefined) =/= undefined
            end,
            handler => fun(Data) ->
                {send_push, maps:get(device_id, Data), maps:get(message, Data)}
            end
        }}
    ],

    wfnet_multi_choice:execute(Branches, UserData).

%%--------------------------------------------------------------------
%% @doc Pricing strategy selection example.
%%
%% Selects multiple pricing strategies based on
%% market conditions and product attributes.
%%
%% @end
%%--------------------------------------------------------------------
pricing_strategy(ProductData) ->
    Branches = [
        {volume_discount, #{
            condition => fun(Data) ->
                maps:get(quantity, Data, 0) >= 100
            end,
            handler => fun(Data) ->
                BasePrice = maps:get(base_price, Data),
                Quantity = maps:get(quantity, Data),
                {apply_volume_discount, BasePrice * 0.9, Quantity}
            end
        }},
        {loyalty_bonus, #{
            condition => fun(Data) ->
                maps:get(tier, Data, undefined) =:= gold orelse
                maps:get(tier, Data, undefined) =:= platinum
            end,
            handler => fun(Data) ->
                BasePrice = maps:get(base_price, Data),
                {apply_loyalty_bonus, BasePrice * 0.85, maps:get(tier, Data)}
            end
        }},
        {seasonal_promotion, #{
            condition => fun(Data) ->
                lists:member(maps:get(season, Data), [spring, summer])
            end,
            handler => fun(Data) ->
                {apply_seasonal_promo, maps:get(season, Data), 0.8}
            end
        }}
    ],

    wfnet_multi_choice:execute(Branches, ProductData).

%%--------------------------------------------------------------------
%% @doc Risk assessment workflow example.
%%
%% Runs multiple risk assessment checks in parallel
%% based on transaction characteristics.
%%
%% @end
%%--------------------------------------------------------------------
risk_assessment(Transaction) ->
    Amount = maps:get(amount, Transaction, 0),
    Country = maps:get(country, Transaction, us),

    Branches = [
        {fraud_check, #{
            condition => fun(_Tx) ->
                Amount > 10000
            end,
            handler => fun(Tx) ->
                {fraud_score, check_fraud_database(T.Tx)}
            end
        }},
        {aml_check, #{
            condition => fun(Tx) ->
                maps:get(amount, Tx, 0) > 5000 andalso
                maps:get(country, Tx, us) =/= us
            end,
            handler => fun(Tx) ->
                {aml_result, check_aml_list(Tx)}
            end
        }},
        {kyc_verification, #{
            condition => fun(Tx) ->
                maps:get(customer_type, Tx, individual) =:= business
            end,
            handler => fun(Tx) ->
                {kyc_status, verify_business(Tx)}
            end
        }}
    ],

    wfnet_multi_choice:execute(Branches, Transaction#{amount => Amount, country => Country}).

%%====================================================================
%% Helper Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Simulate fraud database check.
%%--------------------------------------------------------------------
check_fraud_database(_Transaction) ->
    %% Simulated check
    random:uniform(100) > 95.  %% 5% fraud risk

%%--------------------------------------------------------------------
%% @private
%% @doc Simulate AML list check.
%%--------------------------------------------------------------------
check_aml_list(_Transaction) ->
    %% Simulated check
    clean.

%%--------------------------------------------------------------------
%% @private
%% @doc Simulate business verification.
%%--------------------------------------------------------------------
verify_business(_Transaction) ->
    %% Simulated verification
    verified.

%%====================================================================
%% Console Output Examples
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Run all examples and print results.
%%
%% Usage:
%% ```erlang
%% > wfnet_multi_choice_example:run_all().
%% ```
%%
%% @end
%%--------------------------------------------------------------------
run_all() ->
    io:format("=== Multi-Choice Workflow Pattern Examples ===~n~n"),

    io:format("1. Approval Workflow:~n"),
    approval_workflow(),

    io:format("2. Notification Workflow:~n"),
    User1 = #{
        email_enabled => true,
        sms_enabled => true,
        push_enabled => false,
        email => "user@example.com",
        phone => "+1234567890",
        urgency => high,
        message => "Your order has been shipped!"
    },
    {ok, Notifications1} = notification_workflow(User1),
    io:format("  Notifications sent: ~p~n~n", [Notifications1]),

    io:format("3. Pricing Strategy:~n"),
    Product = #{
        base_price => 100,
        quantity => 150,
        tier => gold,
        season => spring
    },
    {ok, Strategies} = pricing_strategy(Product),
    io:format("  Applied strategies: ~p~n~n", [Strategies]),

    io:format("4. Risk Assessment:~n"),
    Transaction = #{
        amount => 15000,
        country => fr,
        customer_type => business
    },
    {ok, RiskChecks} = risk_assessment(Transaction),
    io:format("  Risk check results: ~p~n~n", [RiskChecks]),

    io:format("=== Examples Complete ===~n"),
    ok.
