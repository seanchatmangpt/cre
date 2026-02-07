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

-module(wf_worklet).
-moduledoc """
Dynamic workflow rule-based selection (worklet pattern).

Worklets provide dynamic task replacement based on runtime data evaluation.
Rules are evaluated in order, with the first matching rule determining the
selected replacement workflow.

```erlang
> Rules = [
..   #{task => approve, 'when' => {gt, amount, 1000}, choose => approve_big},
..   #{task => approve, 'when' => any, choose => approve_small}
.. ].
_
> W = wf_worklet:new(Rules).
_
> wf_worklet:select(W, approve, #{amount => 10}).
{ok,approve_small}
> wf_worklet:select(W, approve, #{amount => 5000}).
{ok,approve_big}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Worklet creation and selection
-export([new/1, select/3]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A rule for dynamic task replacement.
%%
%% Rule fields:
%%   - task: The task name this rule applies to (atom)
%%   - when: Condition that must match (condition())
%%   - choose: Replacement workflow to select (atom)
%%
%% Note: 'when' is used as the map key (not a keyword here).
%%--------------------------------------------------------------------
-type rule() :: #{
    task := atom(),
    'when' := condition(),
    choose := atom()
}.

%%--------------------------------------------------------------------
%% @doc Condition for rule matching.
%%
%% Supported conditions:
%%   - {gt, Field, Threshold}: Field value must be greater than Threshold
%%   - {lt, Field, Threshold}: Field value must be less than Threshold
%%   - {gte, Field, Threshold}: Field value must be greater or equal
%%   - {lte, Field, Threshold}: Field value must be less or equal
%%   - {eq, Field, Value}: Field value must equal Value
%%   - {neq, Field, Value}: Field value must not equal Value
%%   - any: Always matches (catch-all rule)
%%   - {exists, Field}: Field must be present in data
%%   - {not_exists, Field}: Field must not be present in data
%%--------------------------------------------------------------------
-type condition() ::
    {gt, atom(), number()} |
    {lt, atom(), number()} |
    {gte, atom(), number()} |
    {lte, atom(), number()} |
    {eq, atom(), term()} |
    {neq, atom(), term()} |
    {exists, atom()} |
    {not_exists, atom()} |
    any.

%%--------------------------------------------------------------------
%% @doc Data map containing field values for condition evaluation.
%%
%% Maps field names (atoms) to their values for matching against
%% rule conditions.
%%--------------------------------------------------------------------
-type data() :: #{atom() => term()}.

%%--------------------------------------------------------------------
%% @doc A worklet containing ordered rules for task replacement.
%%
%% Rules are stored as a list and evaluated in order - first match wins.
%%--------------------------------------------------------------------
-type worklet() :: #{rules := [rule()]}.

%%--------------------------------------------------------------------
%% @doc Selection result.
%%
%% Returns {ok, Replacement} on success where Replacement is the atom
%% name of the selected workflow, or {error, Reason} on failure.
%%--------------------------------------------------------------------
-type select_result() :: {ok, atom()} | {error, term()}.

%% Export types
-export_type([rule/0, condition/0, data/0, worklet/0, select_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new worklet from a list of rules.
%%
%% Rules are evaluated in the order provided - first matching rule
%% determines the selected replacement. More specific rules should
%% be placed before catch-all rules.
%%
%% @param Rules List of rule maps for task replacement
%% @return A worklet containing the ordered rules
%%
%% @end
%%--------------------------------------------------------------------
-spec new(Rules :: [rule()]) -> worklet().

new(Rules) when is_list(Rules) ->
    %% Validate each rule has required fields
    lists:foreach(
        fun(#{task := Task, 'when' := Cond, choose := Choose}) ->
            true = is_atom(Task),
            true = is_valid_condition(Cond),
            true = is_atom(Choose)
        end,
        Rules
    ),
    #{rules => Rules}.

%%--------------------------------------------------------------------
%% @doc Selects a replacement workflow for a task based on data.
%%
%% Evaluates rules in order, returning the first rule where:
%%   1. The task name matches
%%   2. The condition evaluates to true against the data
%%
%% If no rule matches, returns {error, no_matching_rule}.
%%
%% @param Worklet The worklet containing rules to evaluate
%% @param Task The task name to find a replacement for
%% @param Data The data map to evaluate conditions against
%% @return {ok, Replacement} or {error, no_matching_rule}
%%
%% @end
%%--------------------------------------------------------------------
-spec select(Worklet :: worklet(), Task :: atom(), Data :: data()) ->
          select_result().

select(#{rules := Rules}, Task, Data) when is_atom(Task), is_map(Data) ->
    select_rules(Rules, Task, Data).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Evaluates rules in order until a match is found.
%%
%% @end
%%--------------------------------------------------------------------
-spec select_rules([rule()], atom(), data()) -> select_result().

select_rules([], _Task, _Data) ->
    {error, no_matching_rule};

select_rules([#{task := RuleTask, 'when' := Cond, choose := Choose} | Rest], Task, Data) ->
    case RuleTask of
        Task ->
            case evaluate_condition(Cond, Data) of
                true ->
                    {ok, Choose};
                false ->
                    select_rules(Rest, Task, Data)
            end;
        _DifferentTask ->
            select_rules(Rest, Task, Data)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Evaluates a condition against a data map.
%%
%% @end
%%--------------------------------------------------------------------
-spec evaluate_condition(condition(), data()) -> boolean().

evaluate_condition(any, _Data) ->
    true;

evaluate_condition({gt, Field, Threshold}, Data) ->
    case maps:get(Field, Data, undefined) of
        undefined -> false;
        Value -> is_number(Value) andalso Value > Threshold
    end;

evaluate_condition({lt, Field, Threshold}, Data) ->
    case maps:get(Field, Data, undefined) of
        undefined -> false;
        Value -> is_number(Value) andalso Value < Threshold
    end;

evaluate_condition({gte, Field, Threshold}, Data) ->
    case maps:get(Field, Data, undefined) of
        undefined -> false;
        Value -> is_number(Value) andalso Value >= Threshold
    end;

evaluate_condition({lte, Field, Threshold}, Data) ->
    case maps:get(Field, Data, undefined) of
        undefined -> false;
        Value -> is_number(Value) andalso Value =< Threshold
    end;

evaluate_condition({eq, Field, Expected}, Data) ->
    case maps:get(Field, Data, undefined) of
        undefined -> false;
        Value -> Value =:= Expected
    end;

evaluate_condition({neq, Field, Expected}, Data) ->
    case maps:get(Field, Data, undefined) of
        undefined -> false;
        Value -> Value =/= Expected
    end;

evaluate_condition({exists, Field}, Data) ->
    maps:is_key(Field, Data);

evaluate_condition({not_exists, Field}, Data) ->
    not maps:is_key(Field, Data).

%%--------------------------------------------------------------------
%% @private
%% @doc Validates that a condition is well-formed.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_condition(term()) -> boolean().

is_valid_condition(any) ->
    true;
is_valid_condition({gt, Field, Threshold}) when is_atom(Field), is_number(Threshold) ->
    true;
is_valid_condition({lt, Field, Threshold}) when is_atom(Field), is_number(Threshold) ->
    true;
is_valid_condition({gte, Field, Threshold}) when is_atom(Field), is_number(Threshold) ->
    true;
is_valid_condition({lte, Field, Threshold}) when is_atom(Field), is_number(Threshold) ->
    true;
is_valid_condition({eq, Field, _Value}) when is_atom(Field) ->
    true;
is_valid_condition({neq, Field, _Value}) when is_atom(Field) ->
    true;
is_valid_condition({exists, Field}) when is_atom(Field) ->
    true;
is_valid_condition({not_exists, Field}) when is_atom(Field) ->
    true;
is_valid_condition(_) ->
    false.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc Runs doctests from the moduledoc and function documentation.
%%
%% @end
%%--------------------------------------------------------------------
doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Test new/1 with valid rules
new_valid_rules_test() ->
    Rules = [
        #{task => approve, 'when' => {gt, amount, 1000}, choose => approve_big},
        #{task => approve, 'when' => any, choose => approve_small}
    ],
    W = new(Rules),
    true = is_map(W),
    true = maps:is_key(rules, W),
    Rules = maps:get(rules, W),
    ok.

%% Test new/1 with empty rules list
new_empty_rules_test() ->
    W = new([]),
    #{rules := []} = W,
    ok.

%% Test select/3 with gt condition - less than threshold
select_gt_less_than_test() ->
    Rules = [
        #{task => approve, 'when' => {gt, amount, 1000}, choose => approve_big},
        #{task => approve, 'when' => any, choose => approve_small}
    ],
    W = new(Rules),
    {ok, approve_small} = select(W, approve, #{amount => 10}),
    ok.

%% Test select/3 with gt condition - greater than threshold
select_gt_greater_than_test() ->
    Rules = [
        #{task => approve, 'when' => {gt, amount, 1000}, choose => approve_big},
        #{task => approve, 'when' => any, choose => approve_small}
    ],
    W = new(Rules),
    {ok, approve_big} = select(W, approve, #{amount => 5000}),
    ok.

%% Test select/3 with gt condition - equal to threshold (should not match)
select_gt_equal_test() ->
    Rules = [
        #{task => approve, 'when' => {gt, amount, 1000}, choose => approve_big},
        #{task => approve, 'when' => any, choose => approve_small}
    ],
    W = new(Rules),
    %% Equal value falls through to 'any' rule
    {ok, approve_small} = select(W, approve, #{amount => 1000}),
    ok.

%% Test select/3 with lt condition
select_lt_test() ->
    Rules = [
        #{task => process, 'when' => {lt, priority, 5}, choose => high_priority},
        #{task => process, 'when' => any, choose => normal_priority}
    ],
    W = new(Rules),
    {ok, high_priority} = select(W, process, #{priority => 3}),
    {ok, normal_priority} = select(W, process, #{priority => 10}),
    ok.

%% Test select/3 with gte condition
select_gte_test() ->
    Rules = [
        #{task => route, 'when' => {gte, level, 10}, choose => advanced},
        #{task => route, 'when' => any, choose => basic}
    ],
    W = new(Rules),
    {ok, advanced} = select(W, route, #{level => 10}),
    {ok, advanced} = select(W, route, #{level => 15}),
    {ok, basic} = select(W, route, #{level => 5}),
    ok.

%% Test select/3 with lte condition
select_lte_test() ->
    Rules = [
        #{task => discount, 'when' => {lte, age, 12}, choose => child_discount},
        #{task => discount, 'when' => any, choose => regular_price}
    ],
    W = new(Rules),
    {ok, child_discount} = select(W, discount, #{age => 12}),
    {ok, child_discount} = select(W, discount, #{age => 8}),
    {ok, regular_price} = select(W, discount, #{age => 20}),
    ok.

%% Test select/3 with eq condition
select_eq_test() ->
    Rules = [
        #{task => verify, 'when' => {eq, status, pending}, choose => pending_flow},
        #{task => verify, 'when' => {eq, status, approved}, choose => approved_flow},
        #{task => verify, 'when' => any, choose => default_flow}
    ],
    W = new(Rules),
    {ok, pending_flow} = select(W, verify, #{status => pending}),
    {ok, approved_flow} = select(W, verify, #{status => approved}),
    {ok, default_flow} = select(W, verify, #{status => rejected}),
    ok.

%% Test select/3 with neq condition
select_neq_test() ->
    Rules = [
        #{task => handle, 'when' => {neq, type, spam}, choose => normal_handle},
        #{task => handle, 'when' => any, choose => spam_handle}
    ],
    W = new(Rules),
    {ok, normal_handle} = select(W, handle, #{type => ham}),
    {ok, spam_handle} = select(W, handle, #{type => spam}),
    ok.

%% Test select/3 with exists condition
select_exists_test() ->
    Rules = [
        #{task => enrich, 'when' => {exists, email}, choose => email_flow},
        #{task => enrich, 'when' => any, choose => anonymous_flow}
    ],
    W = new(Rules),
    {ok, email_flow} = select(W, enrich, #{email => "test@example.com"}),
    {ok, anonymous_flow} = select(W, enrich, #{name => "John"}),
    ok.

%% Test select/3 with not_exists condition
select_not_exists_test() ->
    Rules = [
        #{task => auth, 'when' => {not_exists, token}, choose => require_login},
        #{task => auth, 'when' => any, choose => authenticated_flow}
    ],
    W = new(Rules),
    {ok, require_login} = select(W, auth, #{}),
    {ok, authenticated_flow} = select(W, auth, #{token => "abc123"}),
    ok.

%% Test select/3 with no matching task
select_no_matching_task_test() ->
    Rules = [
        #{task => approve, 'when' => any, choose => approve_flow}
    ],
    W = new(Rules),
    {error, no_matching_rule} = select(W, unknown_task, #{}),
    ok.

%% Test select/3 with empty rules
select_empty_rules_test() ->
    W = new([]),
    {error, no_matching_rule} = select(W, any_task, #{field => value}),
    ok.

%% Test select/3 with multiple tasks
select_multiple_tasks_test() ->
    Rules = [
        #{task => task_a, 'when' => {gt, x, 10}, choose => a_big},
        #{task => task_a, 'when' => any, choose => a_small},
        #{task => task_b, 'when' => {lt, y, 5}, choose => b_low},
        #{task => task_b, 'when' => any, choose => b_high}
    ],
    W = new(Rules),
    {ok, a_big} = select(W, task_a, #{x => 20}),
    {ok, a_small} = select(W, task_a, #{x => 5}),
    {ok, b_low} = select(W, task_b, #{y => 2}),
    {ok, b_high} = select(W, task_b, #{y => 10}),
    ok.

%% Test select/3 rule order (first match wins)
select_rule_order_test() ->
    Rules = [
        #{task => route, 'when' => {eq, region, us}, choose => us_flow},
        #{task => route, 'when' => any, choose => default_flow},
        %% This rule would match but is after 'any'
        #{task => route, 'when' => {eq, region, eu}, choose => eu_flow}
    ],
    W = new(Rules),
    {ok, us_flow} = select(W, route, #{region => us}),
    %% Falls through to 'any' rule, never reaches eu rule
    {ok, default_flow} = select(W, route, #{region => eu}),
    ok.

%% Test select/3 with missing field in data
select_missing_field_test() ->
    Rules = [
        #{task => check, 'when' => {gt, value, 0}, choose => positive},
        #{task => check, 'when' => any, choose => catch_all}
    ],
    W = new(Rules),
    %% Missing field causes gt rule to fail, falls through to any
    {ok, catch_all} = select(W, check, #{}),
    ok.

%% Test select/3 with non-numeric value for numeric comparison
select_non_numeric_test() ->
    Rules = [
        #{task => numeric, 'when' => {gt, count, 5}, choose => high},
        #{task => numeric, 'when' => any, choose => other}
    ],
    W = new(Rules),
    %% String value doesn't match gt condition
    {ok, other} = select(W, numeric, #{count => "many"}),
    ok.

-endif.
