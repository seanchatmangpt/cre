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
%% @doc YAWL Workflow Pattern Module
%%
%% This module implements YAWL workflow patterns as composable
%% pattern records and functions.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_patterns).

%%====================================================================
%% Exports
%%====================================================================

%% Basic Control Flow Patterns
-export([implicit_termination/1, multiple_instances_no_sync/3]).
-export([deferred_choice/3]).

%% Exception Handling Patterns
-export([error_handler/2, retry/2, compensate/2]).
-export([triggered_compensation/3, consecutive_compensate/1]).

%% Data Flow Patterns (WDP-1 to WDP-5)
%% Note: param_pass and data_transform are implemented in cre_yawl module

%% Resource Patterns (WRP-1 to WRP-5)
%% Note: role_allocate is implemented in cre_yawl module

%% Pattern Constructors
-export([new_pattern/2]).

%%====================================================================
%% Records
%%====================================================================

-record(pattern, {
    pattern_type :: atom(),
    subprocess :: module() | function() | undefined,
    instance_count :: non_neg_integer() | unlimited | undefined,
    max_instances :: non_neg_integer() | unlimited | undefined,
    pending_instances = [] :: list(),
    active_instances = [] :: list(),
    completed_instances = [] :: list(),
    choice_data = #{} :: map(),
    branch_queue = [] :: list()
}).

-record(implicit_termination, {
    trigger_function :: function()
}).

-record(multiple_instances_no_sync, {
    map_function :: function(),
    instance_count :: non_neg_integer()
}).

-record(deferred_choice, {
    option_a_fun :: function(),
    option_b_fun :: function(),
    choice_fun :: function()
}).

-record(error_handler, {
    risky_function :: function(),
    handler_function :: function()
}).

-record(retry, {
    work_function :: function(),
    policy :: term()
}).

-record(compensate, {
    activity_function :: function(),
    compensation_function :: function()
}).

-record(triggered_compensation, {
    activity_function :: function(),
    compensation_function :: function(),
    trigger_function :: function()
}).

-record(consecutive_compensate, {
    activities :: [{function(), function()}]
}).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an implicit termination pattern.
%% @end
%%--------------------------------------------------------------------
-spec implicit_termination(TriggerFun :: function()) -> #implicit_termination{}.
implicit_termination(TriggerFun) when is_function(TriggerFun) ->
    #implicit_termination{trigger_function = TriggerFun}.

%%--------------------------------------------------------------------
%% @doc Creates a multiple instances no synchronization pattern.
%% @end
%%--------------------------------------------------------------------
-spec multiple_instances_no_sync(MapFun :: function(), Count :: pos_integer(),
                                   Input :: list()) -> #multiple_instances_no_sync{}.
multiple_instances_no_sync(MapFun, Count, _Input) when is_function(MapFun), is_integer(Count), Count > 0 ->
    #multiple_instances_no_sync{map_function = MapFun, instance_count = Count}.

%%--------------------------------------------------------------------
%% @doc Creates a deferred choice pattern.
%% @end
%%--------------------------------------------------------------------
-spec deferred_choice(OptionAFun :: function(), OptionBFun :: function(),
                      ChoiceFun :: function()) -> #deferred_choice{}.
deferred_choice(OptionAFun, OptionBFun, ChoiceFun) ->
    #deferred_choice{
        option_a_fun = OptionAFun,
        option_b_fun = OptionBFun,
        choice_fun = ChoiceFun
    }.

%%--------------------------------------------------------------------
%% @doc Creates an error handler pattern.
%% @end
%%--------------------------------------------------------------------
-spec error_handler(RiskyFun :: function(), HandlerFun :: function()) -> #error_handler{}.
error_handler(RiskyFun, HandlerFun) ->
    #error_handler{
        risky_function = RiskyFun,
        handler_function = HandlerFun
    }.

%%--------------------------------------------------------------------
%% @doc Creates a retry pattern.
%% @end
%%--------------------------------------------------------------------
-spec retry(WorkFun :: function(), Policy :: term()) -> #retry{}.
retry(WorkFun, Policy) when is_function(WorkFun) ->
    #retry{work_function = WorkFun, policy = Policy}.

%%--------------------------------------------------------------------
%% @doc Creates a compensation pattern.
%% @end
%%--------------------------------------------------------------------
-spec compensate(ActivityFun :: function(), CompensationFun :: function()) ->
          #compensate{}.
compensate(ActivityFun, CompensationFun) ->
    #compensate{
        activity_function = ActivityFun,
        compensation_function = CompensationFun
    }.

%%--------------------------------------------------------------------
%% @doc Creates a triggered compensation pattern.
%% @end
%%--------------------------------------------------------------------
-spec triggered_compensation(ActivityFun :: function(), CompensationFun :: function(),
                              TriggerFun :: function()) -> #triggered_compensation{}.
triggered_compensation(ActivityFun, CompensationFun, TriggerFun) ->
    #triggered_compensation{
        activity_function = ActivityFun,
        compensation_function = CompensationFun,
        trigger_function = TriggerFun
    }.

%%--------------------------------------------------------------------
%% @doc Creates a consecutive compensation pattern.
%% @end
%%--------------------------------------------------------------------
-spec consecutive_compensate(Activities :: [{function(), function()}]) ->
          #consecutive_compensate{}.
consecutive_compensate(Activities) when is_list(Activities) ->
    #consecutive_compensate{activities = Activities}.

%%--------------------------------------------------------------------
%% @doc Creates a new pattern record.
%% @end
%%--------------------------------------------------------------------
-spec new_pattern(Type :: atom(), Data :: term()) -> #pattern{}.
new_pattern(Type, Data) ->
    #pattern{
        pattern_type = Type,
        subprocess = undefined,
        instance_count = 0,
        max_instances = unlimited,
        pending_instances = [],
        active_instances = [],
        completed_instances = [],
        choice_data = Data,
        branch_queue = []
    }.
