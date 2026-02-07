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
%% @author YAWL Cost Tracking Implementation
%% @copyright 2025
%%
%% @doc YAWL Cost Tracking Module for CRE
%%
%% This module implements comprehensive cost tracking for workflow execution,
%% supporting multiple cost types, currencies, and resource rates.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Cost Types:</b> Execution, resource, time, and penalty costs</li>
%%   <li><b>Multi-Currency:</b> Support for multiple currencies with conversion</li>
%%   <li><b>Resource Rates:</b> Configurable per-resource rates</li>
%%   <li><b>Cost Reports:</b> Generate detailed cost breakdowns</li>
%%   <li><b>Real-time Tracking:</b> Track costs as workflows execute</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_cost).
-behaviour(gen_server).

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/0,
         start_link/1,
         stop/0,

         %% Cost assignment
         assign_cost/4,
         assign_cost/5,
         assign_resource_cost/3,
         assign_time_cost/4,
         assign_penalty_cost/3,

         %% Cost queries
         get_case_cost/1,
         get_case_cost/2,
         get_task_cost/1,
         get_task_cost/2,
         calculate_total_cost/1,
         calculate_total_cost/2,
         get_cost_breakdown/1,
         get_cost_breakdown/2,

         %% Resource rates
         set_resource_rate/2,
         set_resource_rate/3,
         get_resource_rate/1,
         get_resource_rate/2,
         list_resource_rates/0,
         clear_resource_rate/1,

         %% Currency conversion
         set_exchange_rate/3,
         convert_currency/3,
         get_supported_currencies/0,

         %% Reporting
         generate_cost_report/1,
         generate_cost_report/2,
         export_cost_data/2,
         get_cost_summary/0,

         %% Management
         reset_costs/0,
         reset_case_costs/1,
         archive_costs/1,
         doctest_test/0]).

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

-type case_id() :: binary().
-type task_id() :: binary().
-type resource_id() :: binary().
-type currency() :: binary().

-type cost_type() :: execution_cost | resource_cost | time_cost | penalty_cost.
-type amount() :: number().

-record(cost_entry, {
    id :: binary(),
    case_id :: case_id(),
    task_id :: task_id() | undefined,
    cost_type :: cost_type(),
    amount :: amount(),
    currency :: currency(),
    timestamp :: integer(),
    metadata :: map()
}).

-type cost_entry() :: #cost_entry{}.

-record(resource_rate, {
    resource_id :: resource_id(),
    rate :: amount(),
    currency :: currency(),
    unit :: per_minute | per_hour | per_execution,
    updated_at :: integer()
}).

-type resource_rate() :: #resource_rate{}.

-record(exchange_rate, {
    from_currency :: currency(),
    to_currency :: currency(),
    rate :: number(),
    updated_at :: integer()
}).

-type exchange_rate() :: #exchange_rate{}.

-export_type([exchange_rate/0]).

-record(cost_state, {
    costs :: [cost_entry()],
    resource_rates :: #{resource_id() => resource_rate()},
    exchange_rates :: #{{currency(), currency()} => number()},
    default_currency :: currency(),
    cost_index :: #{case_id() => [cost_entry()]}
}).

-type cost_state() :: cost_state().

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts the cost tracker with default configuration.
%%
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    start_link(#{}).

%%--------------------------------------------------------------------
%% @doc Starts the cost tracker with configuration.
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
%% @doc Stops the cost tracker.
%%
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop() -> ok.

stop() ->
    gen_server:stop(?MODULE).

%%--------------------------------------------------------------------
%% @doc Assigns a cost to a case/task.
%%
%% @end
%%--------------------------------------------------------------------
-spec assign_cost(CaseId :: case_id(),
                 TaskId :: task_id() | undefined,
                 CostType :: cost_type(),
                 Amount :: amount()) -> ok.

assign_cost(CaseId, TaskId, CostType, Amount) ->
    assign_cost(CaseId, TaskId, CostType, Amount, <<"USD">>).

%%--------------------------------------------------------------------
%% @doc Assigns a cost with specified currency.
%%
%% @end
%%--------------------------------------------------------------------
-spec assign_cost(CaseId :: case_id(),
                 TaskId :: task_id() | undefined,
                 CostType :: cost_type(),
                 Amount :: amount(),
                 Currency :: currency()) -> ok.

assign_cost(CaseId, TaskId, CostType, Amount, Currency) ->
    gen_server:cast(?MODULE, {assign_cost, CaseId, TaskId, CostType, Amount, Currency}).

%%--------------------------------------------------------------------
%% @doc Assigns resource-based cost.
%%
%% @end
%%--------------------------------------------------------------------
-spec assign_resource_cost(CaseId :: case_id(),
                          ResourceId :: resource_id(),
                          Duration :: amount()) -> ok.

assign_resource_cost(CaseId, ResourceId, Duration) ->
    gen_server:cast(?MODULE, {assign_resource_cost, CaseId, ResourceId, Duration}).

%%--------------------------------------------------------------------
%% @doc Assigns time-based cost.
%%
%% @end
%%--------------------------------------------------------------------
-spec assign_time_cost(CaseId :: case_id(),
                      TaskId :: task_id(),
                      DurationMs :: amount(),
                      RatePerHour :: amount()) -> ok.

assign_time_cost(CaseId, TaskId, DurationMs, RatePerHour) ->
    gen_server:cast(?MODULE, {assign_time_cost, CaseId, TaskId, DurationMs, RatePerHour}).

%%--------------------------------------------------------------------
%% @doc Assigns penalty cost.
%%
%% @end
%%--------------------------------------------------------------------
-spec assign_penalty_cost(CaseId :: case_id(),
                         Reason :: binary(),
                         Amount :: amount()) -> ok.

assign_penalty_cost(CaseId, Reason, Amount) ->
    gen_server:cast(?MODULE, {assign_penalty_cost, CaseId, Reason, Amount}).

%%--------------------------------------------------------------------
%% @doc Gets total cost for a case in default currency.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_cost(CaseId :: case_id()) -> {ok, amount()} | {error, term()}.

get_case_cost(CaseId) ->
    gen_server:call(?MODULE, {get_case_cost, CaseId, undefined}).

%%--------------------------------------------------------------------
%% @doc Gets total cost for a case in specified currency.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_case_cost(CaseId :: case_id(), Currency :: currency()) ->
          {ok, amount()} | {error, term()}.

get_case_cost(CaseId, Currency) ->
    gen_server:call(?MODULE, {get_case_cost, CaseId, Currency}).

%%--------------------------------------------------------------------
%% @doc Gets cost for a specific task.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_task_cost(TaskId :: task_id()) -> {ok, amount()} | {error, term()}.

get_task_cost(TaskId) ->
    get_task_cost(TaskId, undefined).

%%--------------------------------------------------------------------
%% @doc Gets cost for a specific task in specified currency.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_task_cost(TaskId :: task_id(), Currency :: currency() | undefined) ->
          {ok, amount()} | {error, term()}.

get_task_cost(TaskId, Currency) ->
    gen_server:call(?MODULE, {get_task_cost, TaskId, Currency}).

%%--------------------------------------------------------------------
%% @doc Calculates total cost across all cases.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_total_cost(CostType :: cost_type() | all) -> {ok, amount()}.

calculate_total_cost(CostType) ->
    calculate_total_cost(CostType, undefined).

%%--------------------------------------------------------------------
%% @doc Calculates total cost with currency conversion.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_total_cost(CostType :: cost_type() | all, Currency :: currency() | undefined) ->
          {ok, amount()}.

calculate_total_cost(CostType, Currency) ->
    gen_server:call(?MODULE, {calculate_total_cost, CostType, Currency}).

%%--------------------------------------------------------------------
%% @doc Gets cost breakdown for a case.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_cost_breakdown(CaseId :: case_id()) -> #{cost_type() => amount()}.

get_cost_breakdown(CaseId) ->
    get_cost_breakdown(CaseId, undefined).

%%--------------------------------------------------------------------
%% @doc Gets cost breakdown for a case in specified currency.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_cost_breakdown(CaseId :: case_id(), Currency :: currency() | undefined) ->
          #{cost_type() => amount()}.

get_cost_breakdown(CaseId, Currency) ->
    gen_server:call(?MODULE, {get_cost_breakdown, CaseId, Currency}).

%%--------------------------------------------------------------------
%% @doc Sets rate for a resource.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_resource_rate(ResourceId :: resource_id(), Rate :: amount()) -> ok.

set_resource_rate(ResourceId, Rate) ->
    set_resource_rate(ResourceId, Rate, <<"USD">>).

%%--------------------------------------------------------------------
%% @doc Sets rate for a resource with currency and unit.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_resource_rate(ResourceId :: resource_id(),
                       Rate :: amount(),
                       Currency :: currency()) -> ok.

set_resource_rate(ResourceId, Rate, Currency) ->
    gen_server:cast(?MODULE, {set_resource_rate, ResourceId, Rate, Currency}).

%%--------------------------------------------------------------------
%% @doc Gets rate for a resource.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_resource_rate(ResourceId :: resource_id()) ->
          {ok, amount()} | {error, not_found}.

get_resource_rate(ResourceId) ->
    get_resource_rate(ResourceId, undefined).

%%--------------------------------------------------------------------
%% @doc Gets rate for a resource with currency conversion.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_resource_rate(ResourceId :: resource_id(), Currency :: currency() | undefined) ->
          {ok, amount()} | {error, not_found}.

get_resource_rate(ResourceId, Currency) ->
    gen_server:call(?MODULE, {get_resource_rate, ResourceId, Currency}).

%%--------------------------------------------------------------------
%% @doc Lists all resource rates.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_resource_rates() -> #{resource_id() => {amount(), currency()}}.

list_resource_rates() ->
    gen_server:call(?MODULE, list_resource_rates).

%%--------------------------------------------------------------------
%% @doc Clears a resource rate.
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_resource_rate(ResourceId :: resource_id()) -> ok.

clear_resource_rate(ResourceId) ->
    gen_server:cast(?MODULE, {clear_resource_rate, ResourceId}).

%%--------------------------------------------------------------------
%% @doc Sets exchange rate between currencies.
%%
%% @end
%%--------------------------------------------------------------------
-spec set_exchange_rate(From :: currency(), To :: currency(), Rate :: number()) -> ok.

set_exchange_rate(From, To, Rate) ->
    gen_server:cast(?MODULE, {set_exchange_rate, From, To, Rate}).

%%--------------------------------------------------------------------
%% @doc Converts amount between currencies.
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_currency(Amount :: amount(),
                      From :: currency(),
                      To :: currency()) -> {ok, amount()} | {error, term()}.

convert_currency(Amount, From, To) ->
    gen_server:call(?MODULE, {convert_currency, Amount, From, To}).

%%--------------------------------------------------------------------
%% @doc Gets list of supported currencies.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_supported_currencies() -> [currency()].

get_supported_currencies() ->
    gen_server:call(?MODULE, get_supported_currencies).

%%--------------------------------------------------------------------
%% @doc Generates cost report for a case.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_cost_report(CaseId :: case_id()) -> {ok, map()} | {error, term()}.

generate_cost_report(CaseId) ->
    generate_cost_report(CaseId, <<"USD">>).

%%--------------------------------------------------------------------
%% @doc Generates cost report for a case in specified currency.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_cost_report(CaseId :: case_id(), Currency :: currency()) ->
          {ok, map()} | {error, term()}.

generate_cost_report(CaseId, Currency) ->
    gen_server:call(?MODULE, {generate_cost_report, CaseId, Currency}).

%%--------------------------------------------------------------------
%% @doc Exports cost data in specified format.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_cost_data(CaseId :: case_id(), Format :: json | csv | xml) ->
          {ok, binary()} | {error, term()}.

export_cost_data(CaseId, Format) ->
    gen_server:call(?MODULE, {export_cost_data, CaseId, Format}).

%%--------------------------------------------------------------------
%% @doc Gets overall cost summary.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_cost_summary() -> map().

get_cost_summary() ->
    gen_server:call(?MODULE, get_cost_summary).

%%--------------------------------------------------------------------
%% @doc Resets all costs.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_costs() -> ok.

reset_costs() ->
    gen_server:cast(?MODULE, reset_costs).

%%--------------------------------------------------------------------
%% @doc Resets costs for a specific case.
%%
%% @end
%%--------------------------------------------------------------------
-spec reset_case_costs(CaseId :: case_id()) -> ok.

reset_case_costs(CaseId) ->
    gen_server:cast(?MODULE, {reset_case_costs, CaseId}).

%%--------------------------------------------------------------------
%% @doc Archives costs for a case.
%%
%% @end
%%--------------------------------------------------------------------
-spec archive_costs(CaseId :: case_id()) -> {ok, binary()} | {error, term()}.

archive_costs(CaseId) ->
    gen_server:call(?MODULE, {archive_costs, CaseId}).

%%--------------------------------------------------------------------
%% @doc Runs doctests for the yawl_cost module.
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Resource rate operations
    {ok, _Pid} = start_link(),
    ok = set_resource_rate(<<"worker_1">>, 50.0, <<"USD">>),
    {ok, 50.0} = get_resource_rate(<<"worker_1">>),
    {error, not_found} = get_resource_rate(<<"nonexistent">>),

    %% Test 2: List and clear resource rates
    Rates = list_resource_rates(),
    true = is_map(Rates),
    {50.0, <<"USD">>} = maps:get(<<"worker_1">>, Rates),
    ok = clear_resource_rate(<<"worker_1">>),
    {error, not_found} = get_resource_rate(<<"worker_1">>),

    %% Test 3: Set multiple resource rates
    ok = set_resource_rate(<<"server_a">>, 100.0, <<"USD">>),
    ok = set_resource_rate(<<"server_b">>, 75.0, <<"EUR">>),
    Rates2 = list_resource_rates(),
    2 = map_size(Rates2),

    %% Test 4: Exchange rate operations
    ok = set_exchange_rate(<<"BTC">>, <<"USD">>, 45000.0),
    {ok, 450000.0} = convert_currency(10.0, <<"BTC">>, <<"USD">>),

    %% Test 5: Get supported currencies
    Currencies = get_supported_currencies(),
    true = is_list(Currencies),
    true = lists:member(<<"USD">>, Currencies),
    true = lists:member(<<"EUR">>, Currencies),

    %% Test 6: Cost summary
    Summary = get_cost_summary(),
    true = is_map(Summary),
    +0.0 = maps:get(total_cost, Summary),
    0 = maps:get(total_entries, Summary),

    %% Test 7: Reset costs
    ok = reset_costs(),

    %% Cleanup
    ok = stop().

%%====================================================================
%% gen_server Callback Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes the cost tracker.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Config :: map()) -> {ok, cost_state()}.

init(Config) ->
    DefaultCurrency = maps:get(default_currency, Config, <<"USD">>),

    %% Initialize with default exchange rates
    ExchangeRates = initialize_exchange_rates(),

    State = #cost_state{
        costs = [],
        resource_rates = #{},
        exchange_rates = ExchangeRates,
        default_currency = DefaultCurrency,
        cost_index = #{}
    },

    logger:info("Cost tracker initialized",
                [{module, ?MODULE}, {default_currency, DefaultCurrency}]),

    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles synchronous calls.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: term(), State :: cost_state()) ->
          {reply, term(), cost_state()}.

handle_call({get_case_cost, CaseId, TargetCurrency}, _From, State) ->
    Costs = maps:get(CaseId, State#cost_state.cost_index, []),
    Total = lists:foldl(
        fun(#cost_entry{amount = A, currency = C}, Acc) ->
            Converted = convert_amount(A, C, TargetCurrency, State),
            Acc + Converted
        end,
        0,
        Costs
    ),
    {reply, {ok, Total}, State};

handle_call({get_task_cost, TaskId, TargetCurrency}, _From, State) ->
    TaskCosts = lists:filter(
        fun(#cost_entry{task_id = T}) -> T =:= TaskId end,
        State#cost_state.costs
    ),
    Total = lists:foldl(
        fun(#cost_entry{amount = A, currency = C}, Acc) ->
            Converted = convert_amount(A, C, TargetCurrency, State),
            Acc + Converted
        end,
        0,
        TaskCosts
    ),
    {reply, {ok, Total}, State};

handle_call({calculate_total_cost, CostType, TargetCurrency}, _From, State) ->
    FilteredCosts = case CostType of
        all -> State#cost_state.costs;
        _ -> lists:filter(
            fun(#cost_entry{cost_type = CT}) -> CT =:= CostType end,
            State#cost_state.costs
        )
    end,

    Total = lists:foldl(
        fun(#cost_entry{amount = A, currency = C}, Acc) ->
            Converted = convert_amount(A, C, TargetCurrency, State),
            Acc + Converted
        end,
        0,
        FilteredCosts
    ),
    {reply, {ok, Total}, State};

handle_call({get_cost_breakdown, CaseId, TargetCurrency}, _From, State) ->
    Costs = maps:get(CaseId, State#cost_state.cost_index, []),

    Breakdown = lists:foldl(
        fun(#cost_entry{cost_type = CT, amount = A, currency = C}, Acc) ->
            Converted = convert_amount(A, C, TargetCurrency, State),
            maps:update_with(CT, fun(V) -> V + Converted end, Converted, Acc)
        end,
        #{execution_cost => 0, resource_cost => 0, time_cost => 0, penalty_cost => 0},
        Costs
    ),
    {reply, Breakdown, State};

handle_call({get_resource_rate, ResourceId, TargetCurrency}, _From, State) ->
    case maps:get(ResourceId, State#cost_state.resource_rates, undefined) of
        undefined ->
            {reply, {error, not_found}, State};
        #resource_rate{rate = Rate, currency = Currency} ->
            Converted = convert_amount(Rate, Currency, TargetCurrency, State),
            {reply, {ok, Converted}, State}
    end;

handle_call(list_resource_rates, _From, State) ->
    Rates = maps:fold(
        fun(ResourceId, #resource_rate{rate = Rate, currency = Currency}, Acc) ->
            Acc#{ResourceId => {Rate, Currency}}
        end,
        #{},
        State#cost_state.resource_rates
    ),
    {reply, Rates, State};

handle_call({convert_currency, Amount, From, To}, _From, State) ->
    Converted = convert_amount(Amount, From, To, State),
    {reply, {ok, Converted}, State};

handle_call(get_supported_currencies, _From, State) ->
    Currencies = lists:usort([
        From || {{From, _To}, _Rate} <- maps:keys(State#cost_state.exchange_rates)
    ]) ++ [State#cost_state.default_currency],
    {reply, lists:usort(Currencies), State};

handle_call({generate_cost_report, CaseId, Currency}, _From, State) ->
    Costs = maps:get(CaseId, State#cost_state.cost_index, []),
    Breakdown = get_cost_breakdown_internal(Costs, Currency, State),

    Report = #{
        case_id => CaseId,
        currency => Currency,
        total_cost => maps:fold(fun(_K, V, Acc) -> Acc + V end, 0, Breakdown),
        breakdown => Breakdown,
        cost_entries => length(Costs),
        generated_at => erlang:system_time(millisecond)
    },
    {reply, {ok, Report}, State};

handle_call({export_cost_data, CaseId, Format}, _From, State) ->
    Costs = maps:get(CaseId, State#cost_state.cost_index, []),
    Result = case Format of
        json -> export_costs_json(Costs, State);
        csv -> export_costs_csv(Costs, State);
        xml -> export_costs_xml(Costs, State)
    end,
    {reply, {ok, Result}, State};

handle_call(get_cost_summary, _From, State) ->
    Summary = calculate_cost_summary(State),
    {reply, Summary, State};

handle_call({archive_costs, CaseId}, _From, State) ->
    Costs = maps:get(CaseId, State#cost_state.cost_index, []),

    ArchiveId = generate_archive_id(CaseId),
    _ArchiveData = #{
        archive_id => ArchiveId,
        case_id => CaseId,
        costs => Costs,
        archived_at => erlang:system_time(millisecond)
    },

    %% Remove from active costs
    Costs1 = lists:filter(
        fun(#cost_entry{case_id = C}) -> C =/= CaseId end,
        State#cost_state.costs
    ),
    CostIndex1 = maps:remove(CaseId, State#cost_state.cost_index),

    %% In production, would persist to disk/database
    logger:info("Costs archived",
                [{module, ?MODULE}, {case_id, CaseId},
                 {archive_id, ArchiveId}, {entries_count, length(Costs)}]),

    {reply, {ok, ArchiveId}, State#cost_state{costs = Costs1, cost_index = CostIndex1}};

handle_call(_Request, _From, State) ->
    {reply, {error, bad_msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles asynchronous casts.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: cost_state()) ->
          {noreply, cost_state()}.

handle_cast({assign_cost, CaseId, TaskId, CostType, Amount, Currency}, State) ->
    Cost = #cost_entry{
        id = generate_cost_id(),
        case_id = CaseId,
        task_id = TaskId,
        cost_type = CostType,
        amount = Amount,
        currency = Currency,
        timestamp = erlang:system_time(millisecond),
        metadata = #{}
    },

    Costs = [Cost | State#cost_state.costs],
    CostIndex = maps:update_with(CaseId,
        fun(Existing) -> [Cost | Existing] end,
        [Cost],
        State#cost_state.cost_index),

    %% Record to monitor
    try
        yawl_monitor:record_metric(
            <<"cost_incurred">>,
            Amount,
            #{<<"case_id">> => CaseId, <<"cost_type">> => CostType}
        )
    catch
        _:_ -> ok
    end,

    {noreply, State#cost_state{costs = Costs, cost_index = CostIndex}};

handle_cast({assign_resource_cost, CaseId, ResourceId, Duration}, State) ->
    case maps:get(ResourceId, State#cost_state.resource_rates, undefined) of
        undefined ->
            logger:warning("No resource rate for ~p", [ResourceId],
                         [{module, ?MODULE}]),
            {noreply, State};
        #resource_rate{rate = Rate, currency = Currency, unit = Unit} ->
            Amount = calculate_resource_cost(Duration, Rate, Unit),
            Cost = #cost_entry{
                id = generate_cost_id(),
                case_id = CaseId,
                task_id = ResourceId,
                cost_type = resource_cost,
                amount = Amount,
                currency = Currency,
                timestamp = erlang:system_time(millisecond),
                metadata = #{resource_id => ResourceId, duration => Duration}
            },

            Costs = [Cost | State#cost_state.costs],
            CostIndex = maps:update_with(CaseId,
                fun(Existing) -> [Cost | Existing] end,
                [Cost],
                State#cost_state.cost_index),

            {noreply, State#cost_state{costs = Costs, cost_index = CostIndex}}
    end;

handle_cast({assign_time_cost, CaseId, TaskId, DurationMs, RatePerHour}, State) ->
    DurationHours = DurationMs / (1000 * 60 * 60),
    Amount = DurationHours * RatePerHour,

    Cost = #cost_entry{
        id = generate_cost_id(),
        case_id = CaseId,
        task_id = TaskId,
        cost_type = time_cost,
        amount = Amount,
        currency = State#cost_state.default_currency,
        timestamp = erlang:system_time(millisecond),
        metadata = #{duration_ms => DurationMs, rate_per_hour => RatePerHour}
    },

    Costs = [Cost | State#cost_state.costs],
    CostIndex = maps:update_with(CaseId,
        fun(Existing) -> [Cost | Existing] end,
        [Cost],
        State#cost_state.cost_index),

    {noreply, State#cost_state{costs = Costs, cost_index = CostIndex}};

handle_cast({assign_penalty_cost, CaseId, Reason, Amount}, State) ->
    Cost = #cost_entry{
        id = generate_cost_id(),
        case_id = CaseId,
        task_id = undefined,
        cost_type = penalty_cost,
        amount = Amount,
        currency = State#cost_state.default_currency,
        timestamp = erlang:system_time(millisecond),
        metadata = #{reason => Reason}
    },

    Costs = [Cost | State#cost_state.costs],
    CostIndex = maps:update_with(CaseId,
        fun(Existing) -> [Cost | Existing] end,
        [Cost],
        State#cost_state.cost_index),

    {noreply, State#cost_state{costs = Costs, cost_index = CostIndex}};

handle_cast({set_resource_rate, ResourceId, Rate, Currency}, State) ->
    ResourceRate = #resource_rate{
        resource_id = ResourceId,
        rate = Rate,
        currency = Currency,
        unit = per_hour,
        updated_at = erlang:system_time(millisecond)
    },
    ResourceRates = maps:put(ResourceId, ResourceRate, State#cost_state.resource_rates),

    logger:info("Resource rate set: ~p rate=~p currency=~s",
                [ResourceId, Rate, Currency],
                [{module, ?MODULE}]),

    {noreply, State#cost_state{resource_rates = ResourceRates}};

handle_cast({clear_resource_rate, ResourceId}, State) ->
    ResourceRates = maps:remove(ResourceId, State#cost_state.resource_rates),
    {noreply, State#cost_state{resource_rates = ResourceRates}};

handle_cast({set_exchange_rate, From, To, Rate}, State) ->
    ExchangeRates = maps:put({From, To}, Rate, State#cost_state.exchange_rates),

    %% Also set inverse rate
    ExchangeRates1 = case Rate of
        0 -> ExchangeRates;
        _ -> maps:put({To, From}, 1 / Rate, ExchangeRates)
    end,

    {noreply, State#cost_state{exchange_rates = ExchangeRates1}};

handle_cast(reset_costs, State) ->
    logger:info("Costs reset", [{module, ?MODULE}]),
    {noreply, State#cost_state{costs = [], cost_index = #{}}};

handle_cast({reset_case_costs, CaseId}, State) ->
    Costs = lists:filter(
        fun(#cost_entry{case_id = C}) -> C =/= CaseId end,
        State#cost_state.costs
    ),
    CostIndex = maps:remove(CaseId, State#cost_state.cost_index),
    {noreply, State#cost_state{costs = Costs, cost_index = CostIndex}};

handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles info messages.
%%
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: term(), State :: cost_state()) ->
          {noreply, cost_state()}.

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Terminates the cost tracker.
%%
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: cost_state()) -> ok.

terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Handles code changes.
%%
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: cost_state(), Extra :: term()) ->
          {ok, cost_state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Converts amount between currencies.
%%
%% @end
%%--------------------------------------------------------------------
-spec convert_amount(amount(), currency(), currency() | undefined, cost_state()) ->
          amount().

convert_amount(Amount, _Currency, undefined, _State) ->
    Amount;
convert_amount(Amount, Currency, Currency, _State) ->
    Amount;
convert_amount(Amount, From, To, State) ->
    case maps:get({From, To}, State#cost_state.exchange_rates, undefined) of
        undefined ->
            %% Try to convert via default currency
            Default = State#cost_state.default_currency,
            case maps:get({From, Default}, State#cost_state.exchange_rates, undefined) of
                undefined ->
                    logger:warning("No exchange rate: from=~p to=~p",
                                  [From, To],
                                  [{module, ?MODULE}, {action, convert_amount},
                                   {warning, no_exchange_rate}]),
                    Amount;
                Rate1 ->
                    case maps:get({Default, To}, State#cost_state.exchange_rates, undefined) of
                        undefined -> Amount * Rate1;
                        Rate2 -> Amount * Rate1 * Rate2
                    end
            end;
        Rate ->
            Amount * Rate
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates resource cost based on duration and rate unit.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_resource_cost(amount(), amount(), per_minute | per_hour | per_execution) ->
          amount().

calculate_resource_cost(_Duration, Rate, per_execution) ->
    Rate;
calculate_resource_cost(Duration, Rate, per_minute) ->
    (Duration / 60000) * Rate;
calculate_resource_cost(Duration, Rate, per_hour) ->
    (Duration / 3600000) * Rate.

%%--------------------------------------------------------------------
%% @private
%% @doc Initializes exchange rates with common currencies.
%%
%% @end
%%--------------------------------------------------------------------
-spec initialize_exchange_rates() -> #{{currency(), currency()} => number()}.

initialize_exchange_rates() ->
    #{
        {<<"USD">>, <<"EUR">>} => 0.92,
        {<<"EUR">>, <<"USD">>} => 1.09,
        {<<"USD">>, <<"GBP">>} => 0.79,
        {<<"GBP">>, <<"USD">>} => 1.27,
        {<<"USD">>, <<"JPY">>} => 149.50,
        {<<"JPY">>, <<"USD">>} => 0.0067,
        {<<"USD">>, <<"CAD">>} => 1.36,
        {<<"CAD">>, <<"USD">>} => 0.74,
        {<<"USD">>, <<"AUD">>} => 1.53,
        {<<"AUD">>, <<"USD">>} => 0.65
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Gets cost breakdown internally.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_cost_breakdown_internal([cost_entry()], currency() | undefined, cost_state()) ->
          #{cost_type() => amount()}.

get_cost_breakdown_internal(Costs, TargetCurrency, State) ->
    lists:foldl(
        fun(#cost_entry{cost_type = CT, amount = A, currency = C}, Acc) ->
            Converted = convert_amount(A, C, TargetCurrency, State),
            maps:update_with(CT, fun(V) -> V + Converted end, Converted, Acc)
        end,
        #{execution_cost => 0, resource_cost => 0, time_cost => 0, penalty_cost => 0},
        Costs
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Calculates overall cost summary.
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_cost_summary(cost_state()) -> map().

calculate_cost_summary(State) ->
    TotalCost = lists:foldl(
        fun(#cost_entry{amount = A}, Acc) -> Acc + A end,
        0,
        State#cost_state.costs
    ),

    CostByType = lists:foldl(
        fun(#cost_entry{cost_type = CT, amount = A}, Acc) ->
            maps:update_with(CT, fun(V) -> V + A end, A, Acc)
        end,
        #{execution_cost => 0, resource_cost => 0, time_cost => 0, penalty_cost => 0},
        State#cost_state.costs
    ),

    #{
        total_cost => TotalCost,
        total_entries => length(State#cost_state.costs),
        cost_by_type => CostByType,
        active_cases => maps:size(State#cost_state.cost_index),
        currency => State#cost_state.default_currency
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Exports costs as JSON.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_costs_json([cost_entry()], cost_state()) -> binary().

export_costs_json(Costs, State) ->
    JsonList = [cost_entry_to_json(C, State) || C <- Costs],
    try
        jsone:encode(#{costs => JsonList})
    catch
        _:_ ->
            lists:flatten(["[", [cost_entry_to_json_simple(C) || C <- Costs], "]"])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Exports costs as CSV.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_costs_csv([cost_entry()], cost_state()) -> binary().

export_costs_csv(Costs, _State) ->
    Header = <<"id,case_id,task_id,cost_type,amount,currency,timestamp\n">>,
    Rows = [cost_entry_to_csv(C) || C <- Costs],
    iolist_to_binary([Header, lists:join($\n, Rows)]).

%%--------------------------------------------------------------------
%% @private
%% @doc Exports costs as XML.
%%
%% @end
%%--------------------------------------------------------------------
-spec export_costs_xml([cost_entry()], cost_state()) -> binary().

export_costs_xml(Costs, State) ->
    Header = <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<costs>\n">>,
    Entries = [cost_entry_to_xml(C, State) || C <- Costs],
    Footer = <<"</costs>">>,
    iolist_to_binary([Header, Entries, Footer]).

%%--------------------------------------------------------------------
%% @private
%% @doc Converts cost entry to JSON map.
%%
%% @end
%%--------------------------------------------------------------------
-spec cost_entry_to_json(cost_entry(), cost_state()) -> map().

cost_entry_to_json(#cost_entry{id = Id, case_id = CaseId, task_id = TaskId,
                               cost_type = CT, amount = A, currency = C,
                               timestamp = TS, metadata = M}, _State) ->
    #{
        id => Id,
        case_id => CaseId,
        task_id => TaskId,
        cost_type => CT,
        amount => A,
        currency => C,
        timestamp => TS,
        metadata => M
    }.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts cost entry to simple JSON.
%%
%% @end
%%--------------------------------------------------------------------
-spec cost_entry_to_json_simple(cost_entry()) -> iolist().

cost_entry_to_json_simple(#cost_entry{id = Id, case_id = CaseId,
                                     cost_type = CT, amount = A, currency = C}) ->
    io_lib:format("{\"id\":\"~s\",\"case_id\":\"~s\",\"type\":\"~p\","
                  "\"amount\":~w,\"currency\":\"~s\"}",
                  [Id, CaseId, CT, A, C]).

%%--------------------------------------------------------------------
%% @private
%% @doc Converts cost entry to CSV row.
%%
%% @end
%%--------------------------------------------------------------------
-spec cost_entry_to_csv(cost_entry()) -> binary().

cost_entry_to_csv(#cost_entry{id = Id, case_id = CaseId, task_id = TaskId,
                             cost_type = CT, amount = A, currency = C,
                             timestamp = TS}) ->
    TaskIdStr = case TaskId of undefined -> <<>>; T -> T end,
    iolist_to_binary([
        Id, $,, CaseId, $,, TaskIdStr, $,,
        atom_to_binary(CT, utf8), $,,
        float_to_binary(A, [{decimals, 2}]), $,, C, $,,
        integer_to_binary(TS)
    ]).

%%--------------------------------------------------------------------
%% @private
%% @doc Converts cost entry to XML element.
%%
%% @end
%%--------------------------------------------------------------------
-spec cost_entry_to_xml(cost_entry(), cost_state()) -> binary().

cost_entry_to_xml(#cost_entry{id = Id, case_id = CaseId, task_id = TaskId,
                              cost_type = CT, amount = A, currency = C,
                              timestamp = TS}, _State) ->
    TaskIdStr = case TaskId of undefined -> <<>>; T -> T end,
    io_lib:format(
        "  <cost>\n"
        "    <id>~s</id>\n"
        "    <case_id>~s</case_id>\n"
        "    <task_id>~s</task_id>\n"
        "    <type>~p</type>\n"
        "    <amount>~f</amount>\n"
        "    <currency>~s</currency>\n"
        "    <timestamp>~w</timestamp>\n"
        "  </cost>\n",
        [Id, CaseId, TaskIdStr, CT, A, C, TS]
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique cost ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_cost_id() -> binary().

generate_cost_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"cost_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique archive ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_archive_id(binary()) -> binary().

generate_archive_id(CaseId) ->
    TS = integer_to_binary(erlang:system_time(millisecond)),
    Unique = crypto:hash(md5, term_to_binary({CaseId, TS})),
    Hex = binary:encode_hex(Unique),
    <<"archive_", Hex/binary>>.
