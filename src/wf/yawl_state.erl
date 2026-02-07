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
%% @doc Workflow state tracking for YAWL executions.
%%
%% This module provides a pure functional interface for managing workflow
%% case state including status, work items, data variables, and timestamps.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Pure functional state management - all operations return updated state</li>
%%   <li>Status lifecycle: created -> running -> suspended/completed/cancelled</li>
%%   <li>Work item tracking with atomic add/remove operations</li>
%%   <li>Data variable management with map merge support</li>
%%   <li>Timestamp tracking for all lifecycle events</li>
%%   <li>Serialization to/from maps for persistence</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Creating a new state:
%% ```erlang
%% > S0 = yawl_state:new(<<"case-123">>).
%% #{case_id => <<"case-123">>, status => created, ...}
%%
%% > S1 = yawl_state:set_status(S0, running).
%% #{case_id => <<"case-123">>, status => running, ...}
%% ```
%%
%% Managing work items:
%% ```erlang
%% > WI = #{wi_id => <<"wi-1">>, task => approve, status => offered}.
%% > S2 = yawl_state:add_workitem(S1, <<"wi-1">>, WI).
%% > yawl_state:get_workitems(S2).
%% #{<<"wi-1">> => #{...}}
%% ```
%%
%% Managing data variables:
%% ```erlang
%% > S3 = yawl_state:set_data(S2, #{amount => 100, user => <<"alice">>}).
%% > yawl_state:get_data(S3).
%% #{amount => 100, user => <<"alice">>}
%%
%% > S4 = yawl_state:update_data(S3, #{amount => 200}).
%% > yawl_state:get_data(S4).
%% #{amount => 200, user => <<"alice">>}
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_state).

%%====================================================================
%% Exports
%%====================================================================

%% State construction
-export([new/1, new/2]).

%% Status management
-export([get_status/1, set_status/2]).
-export([is_created/1, is_running/1, is_suspended/1]).
-export([is_completed/1, is_cancelled/1, is_terminal/1]).

%% Data variable management
-export([get_data/1, set_data/2, update_data/2]).
-export([get_data_value/2, set_data_value/3]).

%% Work item management
-export([get_workitems/1, get_workitem/2]).
-export([add_workitem/2, add_workitem/3]).
-export([remove_workitem/2]).
-export([list_workitems/1, list_workitems_by_status/2]).

%% Timestamp management
-export([get_timestamps/1, set_timestamp/2]).
-export([get_timestamp/2]).
-export([mark_created/1, mark_started/1, mark_completed/1]).
-export([mark_suspended/1, mark_cancelled/1]).

%% Serialization
-export([to_map/1, from_map/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Unique case identifier.
%%
%% Typically a binary string for URL-safe encoding.
%%--------------------------------------------------------------------
-type case_id() :: binary().

%%--------------------------------------------------------------------
%% @doc Workflow case status.
%%
%% Status lifecycle:
%% - created: Initial state, case exists but not started
%% - running: Case is actively executing
%% - suspended: Case paused, can be resumed
%% - completed: Case finished successfully
%% - cancelled: Case was terminated before completion
%%--------------------------------------------------------------------
-type status() :: created | running | suspended | completed | cancelled.

%%--------------------------------------------------------------------
%% @doc Work item status.
%%
%% Work items represent tasks assigned to users/roles:
%% - offered: Task is available for allocation
%% - allocated: Task allocated to a specific user
%% - started: User has begun working on the task
%% - completed: Task finished successfully
%% - failed: Task failed and requires recovery
%%--------------------------------------------------------------------
-type workitem_status() :: offered | allocated | started | completed | failed.

%%--------------------------------------------------------------------
%% @doc Work item identifier.
%%
%% Unique identifier for a work item within a case.
%%--------------------------------------------------------------------
-type workitem_id() :: binary().

%%--------------------------------------------------------------------
%% @doc Work item map.
%%
%% Required keys:
%% - wi_id: Unique work item identifier
%% - task: Atom name of the task
%% - status: Current work item status
%%
%% Optional keys:
%% - assigned_to: User or role the work item is assigned to
%% - data: Work item data map
%% - created_at: Creation timestamp (milliseconds)
%% - started_at: Start timestamp (milliseconds)
%% - completed_at: Completion timestamp (milliseconds)
%%--------------------------------------------------------------------
-type workitem() :: #{
    wi_id := workitem_id(),
    task := atom(),
    status := workitem_status(),
    assigned_to => binary() | atom() | undefined,
    data => map(),
    created_at => integer(),
    started_at => integer(),
    completed_at => integer()
}.

%%--------------------------------------------------------------------
%% @doc Work items map.
%%
%% Maps work item IDs to their work item maps.
%%--------------------------------------------------------------------
-type workitems() :: #{workitem_id() => workitem()}.

%%--------------------------------------------------------------------
%% @doc Data variables map.
%%
%% Workflow case data variables as a generic map.
%% Keys are typically atoms or binaries, values can be any Erlang term.
%%--------------------------------------------------------------------
-type data() :: map().

%%--------------------------------------------------------------------
%% @doc Timestamp key name.
%%
%% Keys for tracking lifecycle timestamps.
%%--------------------------------------------------------------------
-type timestamp_key() ::
    created_at |
    started_at |
    suspended_at |
    resumed_at |
    completed_at |
    cancelled_at |
    updated_at.

%%--------------------------------------------------------------------
%% @doc Timestamps map.
%%
%% Maps timestamp keys to millisecond timestamps.
%%--------------------------------------------------------------------
-type timestamps() :: #{timestamp_key() => integer()}.

%%--------------------------------------------------------------------
%% @doc YAWL workflow state.
%%
%% Complete state of a workflow case including status, work items,
%% data variables, and lifecycle timestamps.
%%
%% Required keys:
%% - case_id: Unique case identifier
%% - status: Current workflow status
%%
%% Optional keys:
%% - workitems: Map of work item ID to work item data
%% - data: Workflow data variables
%% - timestamps: Lifecycle event timestamps
%%--------------------------------------------------------------------
-type t() :: #{
    case_id := case_id(),
    status := status(),
    workitems => workitems(),
    data => data(),
    timestamps => timestamps()
}.

%% Export types
-export_type([
    case_id/0,
    status/0,
    workitem_status/0,
    workitem_id/0,
    workitem/0,
    workitems/0,
    data/0,
    timestamp_key/0,
    timestamps/0,
    t/0
]).

%%====================================================================
%% Constants
%%====================================================================

%% Default timestamps for new state
-define(DEFAULT_TIMESTAMPS, #{
    created_at => undefined,
    started_at => undefined,
    suspended_at => undefined,
    resumed_at => undefined,
    completed_at => undefined,
    cancelled_at => undefined,
    updated_at => undefined
}).

%%====================================================================
%% State Construction
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a new workflow state with default values.
%%
%% Initializes state with:
%% - case_id: Provided ID
%% - status: created
%% - workitems: empty map
%% - data: empty map
%% - timestamps: map with all keys set to undefined
%%
%% ```erlang
%% > S = yawl_state:new(<<"case-abc">>).
%% #{case_id => <<"case-abc">>, status => created, ...}
%% ```
%%
%% @param CaseId Unique case identifier
%% @return New workflow state
%%
%% @end
%%--------------------------------------------------------------------
-spec new(CaseId :: case_id()) -> t().

new(CaseId) when is_binary(CaseId) ->
    #{
        case_id => CaseId,
        status => created,
        workitems => #{},
        data => #{},
        timestamps => ?DEFAULT_TIMESTAMPS
    }.

%%--------------------------------------------------------------------
%% @doc Creates a new workflow state with initial data.
%%
%% Same as new/1 but allows setting initial data variables.
%%
%% ```erlang
%% > S = yawl_state:new(<<"case-xyz">>, #{amount => 100}).
%% > yawl_state:get_data(S).
%% #{amount => 100}
%% ```
%%
%% @param CaseId Unique case identifier
%% @param InitialData Initial data variables
%% @return New workflow state with initial data
%%
%% @end
%%--------------------------------------------------------------------
-spec new(CaseId :: case_id(), InitialData :: data()) -> t().

new(CaseId, InitialData) when is_binary(CaseId), is_map(InitialData) ->
    (new(CaseId))#{data => InitialData}.

%%====================================================================
%% Status Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets the current status of the workflow case.
%%
%% ```erlang
%% > S = yawl_state:new(<<"case-1">>),
%% > yawl_state:get_status(S).
%% created
%%
%% > S1 = S#{status => running},
%% > yawl_state:get_status(S1).
%% running
%% ```
%%
%% @param State Workflow state
%% @return Current status atom
%%
%% @end
%%--------------------------------------------------------------------
-spec get_status(State :: t()) -> status().

get_status(#{status := Status}) -> Status.

%%--------------------------------------------------------------------
%% @doc Sets the status of the workflow case.
%%
%% Also updates the 'updated_at' timestamp to the current time.
%% Use specific mark_* functions for automatic timestamp handling.
%%
%% ```erlang
%% > S = yawl_state:new(<<"case-1">>),
%% > S1 = yawl_state:set_status(S, running),
%% > yawl_state:get_status(S1).
%% running
%% ```
%%
%% @param State Workflow state
%% @param Status New status
%% @return Updated state with new status
%%
%% @end
%%--------------------------------------------------------------------
-spec set_status(State :: t(), Status :: status()) -> t().

set_status(#{timestamps := Ts} = State, Status)
  when is_atom(Status) ->
    Now = erlang:system_time(millisecond),
    State#{
        status => Status,
        timestamps => Ts#{updated_at => Now}
    }.

%%--------------------------------------------------------------------
%% @doc Checks if the workflow case is in 'created' status.
%%
%% @param State Workflow state
%% @return true if status is created
%%
%% @end
%%--------------------------------------------------------------------
-spec is_created(State :: t()) -> boolean().

is_created(#{status := created}) -> true;
is_created(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if the workflow case is in 'running' status.
%%
%% @param State Workflow state
%% @return true if status is running
%%
%% @end
%%--------------------------------------------------------------------
-spec is_running(State :: t()) -> boolean().

is_running(#{status := running}) -> true;
is_running(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if the workflow case is in 'suspended' status.
%%
%% @param State Workflow state
%% @return true if status is suspended
%%
%% @end
%%--------------------------------------------------------------------
-spec is_suspended(State :: t()) -> boolean().

is_suspended(#{status := suspended}) -> true;
is_suspended(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if the workflow case is in 'completed' status.
%%
%% @param State Workflow state
%% @return true if status is completed
%%
%% @end
%%--------------------------------------------------------------------
-spec is_completed(State :: t()) -> boolean().

is_completed(#{status := completed}) -> true;
is_completed(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if the workflow case is in 'cancelled' status.
%%
%% @param State Workflow state
%% @return true if status is cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec is_cancelled(State :: t()) -> boolean().

is_cancelled(#{status := cancelled}) -> true;
is_cancelled(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if the workflow case is in a terminal status.
%%
%% Terminal statuses are: completed, cancelled
%%
%% @param State Workflow state
%% @return true if status is terminal
%%
%% @end
%%--------------------------------------------------------------------
-spec is_terminal(State :: t()) -> boolean().

is_terminal(#{status := completed}) -> true;
is_terminal(#{status := cancelled}) -> true;
is_terminal(_) -> false.

%%====================================================================
%% Data Variable Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets the entire data variables map.
%%
%% ```erlang
%% > S = yawl_state:new(<<"case-1">>, #{x => 1, y => 2}),
%% > yawl_state:get_data(S).
%% #{x => 1, y => 2}
%% ```
%%
%% @param State Workflow state
%% @return Data variables map
%%
%% @end
%%--------------------------------------------------------------------
-spec get_data(State :: t()) -> data().

get_data(#{data := Data}) -> Data.

%%--------------------------------------------------------------------
%% @doc Sets the entire data variables map.
%%
%% Replaces all existing data variables with the provided map.
%% Use update_data/2 for merging instead of replacing.
%%
%% ```erlang
%% > S = yawl_state:new(<<"case-1">>),
%% > S1 = yawl_state:set_data(S, #{total => 100}),
%% > yawl_state:get_data(S1).
%% #{total => 100}
%% ```
%%
%% @param State Workflow state
%% @param Data New data variables map
%% @return Updated state with new data
%%
%% @end
%%--------------------------------------------------------------------
-spec set_data(State :: t(), Data :: data()) -> t().

set_data(State, Data) when is_map(Data) ->
    State#{data => Data}.

%%--------------------------------------------------------------------
%% @doc Updates data variables by merging with provided map.
%%
%% Existing keys are updated, new keys are added.
%% This is a shallow merge - nested maps are replaced, not recursively merged.
%%
%% ```erlang
%% > S = yawl_state:new(<<"case-1">>, #{x => 1, y => 2}),
%% > S1 = yawl_state:update_data(S, #{y => 20, z => 3}),
%% > yawl_state:get_data(S1).
%% #{x => 1, y => 20, z => 3}
%% ```
%%
%% @param State Workflow state
%% @param Updates Data variables to merge
%% @return Updated state with merged data
%%
%% @end
%%--------------------------------------------------------------------
-spec update_data(State :: t(), Updates :: data()) -> t().

update_data(#{data := Data} = State, Updates) when is_map(Updates) ->
    State#{data => maps:merge(Data, Updates)}.

%%--------------------------------------------------------------------
%% @doc Gets a single data variable value.
%%
%% Returns undefined if the key doesn't exist.
%%
%% ```erlang
%% > S = yawl_state:new(<<"case-1">>, #{amount => 100}),
%% > yawl_state:get_data_value(S, amount).
%% 100
%% > yawl_state:get_data_value(S, missing).
%% undefined
%% ```
%%
%% @param State Workflow state
%% @param Key Data variable key
%% @return Data variable value or undefined
%%
%% @end
%%--------------------------------------------------------------------
-spec get_data_value(State :: t(), Key :: term()) -> term() | undefined.

get_data_value(#{data := Data}, Key) ->
    maps:get(Key, Data, undefined).

%%--------------------------------------------------------------------
%% @doc Sets a single data variable value.
%%
%% ```erlang
%% > S = yawl_state:new(<<"case-1">>),
%% > S1 = yawl_state:set_data_value(S, count, 5),
%% > yawl_state:get_data_value(S1, count).
%% 5
%% ```
%%
%% @param State Workflow state
%% @param Key Data variable key
%% @param Value Data variable value
%% @return Updated state with new data value
%%
%% @end
%%--------------------------------------------------------------------
-spec set_data_value(State :: t(), Key :: term(), Value :: term()) -> t().

set_data_value(#{data := Data} = State, Key, Value) ->
    State#{data => Data#{Key => Value}}.

%%====================================================================
%% Work Item Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets all work items for the workflow case.
%%
%% ```erlang
%% > WI = #{wi_id => <<"wi-1">>, task => approve, status => offered},
%% > S = yawl_state:add_workitem(yawl_state:new(<<"c1">>), WI),
%% > yawl_state:get_workitems(S).
%% #{<<"wi-1">> => #{wi_id => <<"wi-1">>, ...}}
%% ```
%%
%% @param State Workflow state
%% @return Map of work item ID to work item data
%%
%% @end
%%--------------------------------------------------------------------
-spec get_workitems(State :: t()) -> workitems().

get_workitems(#{workitems := WIs}) -> WIs.

%%--------------------------------------------------------------------
%% @doc Gets a specific work item by ID.
%%
%% Returns undefined if the work item doesn't exist.
%%
%% ```erlang
%% > WI = #{wi_id => <<"wi-1">>, task => approve, status => offered},
%% > S = yawl_state:add_workitem(yawl_state:new(<<"c1">>), WI),
%% > yawl_state:get_workitem(S, <<"wi-1">>).
%% #{wi_id => <<"wi-1">>, task => approve, ...}
%% ```
%%
%% @param State Workflow state
%% @param WorkitemId Work item ID
%% @return Work item map or undefined
%%
%% @end
%%--------------------------------------------------------------------
-spec get_workitem(State :: t(), WorkitemId :: workitem_id()) -> workitem() | undefined.

get_workitem(#{workitems := WIs}, WorkitemId) ->
    maps:get(WorkitemId, WIs, undefined).

%%--------------------------------------------------------------------
%% @doc Adds a work item using its wi_id as the map key.
%%
%% Extracts wi_id from the work item map and uses it as the key.
%% The work item map must contain a wi_id key.
%%
%% ```erlang
%% > WI = #{wi_id => <<"wi-1">>, task => approve, status => offered},
%% > S = yawl_state:new(<<"c1">>),
%% > S1 = yawl_state:add_workitem(S, WI).
%% ```
%%
%% @param State Workflow state
%% @param Workitem Work item map with wi_id key
%% @return Updated state with work item added
%%
%% @end
%%--------------------------------------------------------------------
-spec add_workitem(State :: t(), Workitem :: workitem()) -> t().

add_workitem(#{workitems := WIs} = State, #{wi_id := WiId} = Workitem) ->
    State#{workitems => WIs#{WiId => Workitem}}.

%%--------------------------------------------------------------------
%% @doc Adds a work item with an explicit ID key.
%%
%% Uses the provided WorkitemId as the map key instead of extracting
%% from the work item map.
%%
%% ```erlang
%% > WI = #{task => approve, status => offered},
%% > S = yawl_state:new(<<"c1">>),
%% > S1 = yawl_state:add_workitem(S, <<"wi-1">>, WI).
%% ```
%%
%% @param State Workflow state
%% @param WorkitemId Work item ID to use as key
%% @param Workitem Work item data
%% @return Updated state with work item added
%%
%% @end
%%--------------------------------------------------------------------
-spec add_workitem(State :: t(), WorkitemId :: workitem_id(), Workitem :: workitem()) -> t().

add_workitem(#{workitems := WIs} = State, WorkitemId, Workitem)
  when is_binary(WorkitemId), is_map(Workitem) ->
    State#{workitems => WIs#{WorkitemId => Workitem#{wi_id => WorkitemId}}}.

%%--------------------------------------------------------------------
%% @doc Removes a work item by ID.
%%
%% If the work item doesn't exist, the state is returned unchanged.
%%
%% ```erlang
%% > WI = #{wi_id => <<"wi-1">>, task => approve, status => offered},
%% > S = yawl_state:add_workitem(yawl_state:new(<<"c1">>), WI),
%% > S1 = yawl_state:remove_workitem(S, <<"wi-1">>),
%% > yawl_state:get_workitems(S1).
%% #{}
%% ```
%%
%% @param State Workflow state
%% @param WorkitemId Work item ID to remove
%% @return Updated state with work item removed
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_workitem(State :: t(), WorkitemId :: workitem_id()) -> t().

remove_workitem(#{workitems := WIs} = State, WorkitemId) ->
    State#{workitems => maps:remove(WorkitemId, WIs)}.

%%--------------------------------------------------------------------
%% @doc Lists all work items as a list.
%%
%% Returns work item maps in arbitrary order.
%%
%% ```erlang
%% > S0 = yawl_state:new(<<"c1">>),
%% > S1 = yawl_state:add_workitem(S0, #{wi_id => <<"a">>, task => t1, status => offered}),
%% > S2 = yawl_state:add_workitem(S1, #{wi_id => <<"b">>, task => t2, status => offered}),
%% > yawl_state:list_workitems(S2).
%% [#{wi_id => <<"a">>, ...}, #{wi_id => <<"b">>, ...}]
%% ```
%%
%% @param State Workflow state
%% @return List of work item maps
%%
%% @end
%%--------------------------------------------------------------------
-spec list_workitems(State :: t()) -> [workitem()].

list_workitems(#{workitems := WIs}) ->
    maps:values(WIs).

%%--------------------------------------------------------------------
%% @doc Lists work items filtered by status.
%%
%% Returns work item maps in arbitrary order.
%%
%% ```erlang
%% > WI1 = #{wi_id => <<"a">>, task => t1, status => offered},
%% > WI2 = #{wi_id => <<"b">>, task => t2, status => completed},
%% > S0 = yawl_state:add_workitem(yawl_state:add_workitem(yawl_state:new(<<"c1">>), WI1), WI2),
%% > yawl_state:list_workitems_by_status(S0, offered).
%% [#{wi_id => <<"a">>, task => t1, status => offered}]
%% ```
%%
%% @param State Workflow state
%% @param Status Work item status to filter by
%% @return List of work item maps with matching status
%%
%% @end
%%--------------------------------------------------------------------
-spec list_workitems_by_status(State :: t(), Status :: workitem_status()) -> [workitem()].

list_workitems_by_status(#{workitems := WIs}, Status) ->
    lists:filter(
        fun(#{status := S}) -> S =:= Status;
           (_) -> false
        end,
        maps:values(WIs)
    ).

%%====================================================================
%% Timestamp Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Gets all timestamps for the workflow case.
%%
%% ```erlang
%% > S = yawl_state:new(<<"c1">>),
%% > yawl_state:get_timestamps(S).
%% #{created_at => undefined, started_at => undefined, ...}
%% ```
%%
%% @param State Workflow state
%% @return Timestamps map
%%
%% @end
%%--------------------------------------------------------------------
-spec get_timestamps(State :: t()) -> timestamps().

get_timestamps(#{timestamps := Ts}) -> Ts.

%%--------------------------------------------------------------------
%% @doc Gets a specific timestamp value.
%%
%% Returns undefined if the timestamp key doesn't exist.
%%
%% ```erlang
%% > S = yawl_state:mark_created(yawl_state:new(<<"c1">>)),
%% > Created = yawl_state:get_timestamp(S, created_at),
%% > is_integer(Created).
%% true
%% ```
%%
%% @param State Workflow state
%% @param Key Timestamp key
%% @return Timestamp value or undefined
%%
%% @end
%%--------------------------------------------------------------------
-spec get_timestamp(State :: t(), Key :: timestamp_key()) -> integer() | undefined.

get_timestamp(#{timestamps := Ts}, Key) ->
    maps:get(Key, Ts, undefined).

%%--------------------------------------------------------------------
%% @doc Sets a specific timestamp to the current time.
%%
%% Sets the timestamp key to the current monotonic time in milliseconds.
%% Also updates the 'updated_at' timestamp.
%%
%% ```erlang
%% > S = yawl_state:new(<<"c1">>),
%% > S1 = yawl_state:set_timestamp(S, custom_event),
%% > yawl_state:get_timestamp(S1, custom_event).
%% 1234567890
%% ```
%%
%% @param State Workflow state
%% @param Key Timestamp key to set
%% @return Updated state with timestamp set
%%
%% @end
%%--------------------------------------------------------------------
-spec set_timestamp(State :: t(), Key :: timestamp_key() | atom()) -> t().

set_timestamp(#{timestamps := Ts} = State, Key) when is_atom(Key) ->
    Now = erlang:system_time(millisecond),
    State#{timestamps => Ts#{Key => Now, updated_at => Now}}.

%%--------------------------------------------------------------------
%% @doc Marks the case as created and sets created_at timestamp.
%%
%% ```erlang
%% > S = yawl_state:mark_created(yawl_state:new(<<"c1">>)),
%% > yawl_state:get_timestamp(S, created_at) =/= undefined.
%% true
%% ```
%%
%% @param State Workflow state
%% @return Updated state with created_at timestamp set
%%
%% @end
%%--------------------------------------------------------------------
-spec mark_created(State :: t()) -> t().

mark_created(State) ->
    set_timestamp(State, created_at).

%%--------------------------------------------------------------------
%% @doc Marks the case as started and sets started_at timestamp.
%%
%% Also updates status to running.
%%
%% ```erlang
%% > S = yawl_state:mark_started(yawl_state:new(<<"c1">>)),
%% > yawl_state:get_status(S).
%% running
%% > yawl_state:get_timestamp(S, started_at) =/= undefined.
%% true
%% ```
%%
%% @param State Workflow state
%% @return Updated state with started_at timestamp set and status running
%%
%% @end
%%--------------------------------------------------------------------
-spec mark_started(State :: t()) -> t().

mark_started(State) ->
    S1 = set_timestamp(State, started_at),
    S1#{status => running}.

%%--------------------------------------------------------------------
%% @doc Marks the case as completed and sets completed_at timestamp.
%%
%% Also updates status to completed.
%%
%% ```erlang
%% > S = yawl_state:mark_completed(yawl_state:new(<<"c1">>)),
%% > yawl_state:get_status(S).
%% completed
%% > yawl_state:get_timestamp(S, completed_at) =/= undefined.
%% true
%% ```
%%
%% @param State Workflow state
%% @return Updated state with completed_at timestamp set and status completed
%%
%% @end
%%--------------------------------------------------------------------
-spec mark_completed(State :: t()) -> t().

mark_completed(State) ->
    S1 = set_timestamp(State, completed_at),
    S1#{status => completed}.

%%--------------------------------------------------------------------
%% @doc Marks the case as suspended and sets suspended_at timestamp.
%%
%% Also updates status to suspended.
%%
%% ```erlang
%% > S = yawl_state:mark_suspended(yawl_state:new(<<"c1">>)),
%% > yawl_state:get_status(S).
%% suspended
%% > yawl_state:get_timestamp(S, suspended_at) =/= undefined.
%% true
%% ```
%%
%% @param State Workflow state
%% @return Updated state with suspended_at timestamp set and status suspended
%%
%% @end
%%--------------------------------------------------------------------
-spec mark_suspended(State :: t()) -> t().

mark_suspended(State) ->
    S1 = set_timestamp(State, suspended_at),
    S1#{status => suspended}.

%%--------------------------------------------------------------------
%% @doc Marks the case as cancelled and sets cancelled_at timestamp.
%%
%% Also updates status to cancelled.
%%
%% ```erlang
%% > S = yawl_state:mark_cancelled(yawl_state:new(<<"c1">>)),
%% > yawl_state:get_status(S).
%% cancelled
%% > yawl_state:get_timestamp(S, cancelled_at) =/= undefined.
%% true
%% ```
%%
%% @param State Workflow state
%% @return Updated state with cancelled_at timestamp set and status cancelled
%%
%% @end
%%--------------------------------------------------------------------
-spec mark_cancelled(State :: t()) -> t().

mark_cancelled(State) ->
    S1 = set_timestamp(State, cancelled_at),
    S1#{status => cancelled}.

%%====================================================================
%% Serialization
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Converts workflow state to a plain map for serialization.
%%
%% All state data is converted to a plain map suitable for JSON encoding
%% or persistent storage.
%%
%% ```erlang
%% > S = yawl_state:new(<<"case-1">>),
%% > Map = yawl_state:to_map(S),
%% > maps:get(status, Map).
%% created
%% ```
%%
%% @param State Workflow state
%% @return Plain map representation
%%
%% @end
%%--------------------------------------------------------------------
-spec to_map(State :: t()) -> map().

to_map(State) when is_map(State) ->
    State.

%%--------------------------------------------------------------------
%% @doc Creates workflow state from a plain map.
%%
%% Validates and reconstructs state from a serialized representation.
%% Returns the state as-is if all required keys are present.
%%
%% ```erlang
%% > Map = #{case_id => <<"case-1">>, status => running, data => #{}},
%% > S = yawl_state:from_map(Map),
%% > yawl_state:get_status(S).
%% running
%% ```
%%
%% @param Map Plain map representation
%% @return Workflow state
%%
%% @end
%%--------------------------------------------------------------------
-spec from_map(Map :: map()) -> t().

from_map(Map) when is_map(Map) ->
    %% Ensure required keys exist with defaults
    CaseId = maps:get(case_id, Map, <<>>),
    Status = maps:get(status, Map, created),
    Workitems = maps:get(workitems, Map, #{}),
    Data = maps:get(data, Map, #{}),
    Timestamps = maps:get(timestamps, Map, ?DEFAULT_TIMESTAMPS),

    #{
        case_id => CaseId,
        status => Status,
        workitems => Workitems,
        data => Data,
        timestamps => Timestamps
    }.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc EUnit test runner for the module.
%%--------------------------------------------------------------------
doctest_test() ->
    ok.

%%====================================================================
%% State Construction Tests
%%====================================================================

new_creates_default_state_test() ->
    S = new(<<"case-1">>),
    ?assertEqual(<<"case-1">>, maps:get(case_id, S)),
    ?assertEqual(created, get_status(S)),
    ?assertEqual(#{}, get_workitems(S)),
    ?assertEqual(#{}, get_data(S)),
    ?assertEqual(?DEFAULT_TIMESTAMPS, get_timestamps(S)).

new_with_initial_data_test() ->
    S = new(<<"case-2">>, #{x => 1, y => 2}),
    ?assertEqual(<<"case-2">>, maps:get(case_id, S)),
    ?assertEqual(#{x => 1, y => 2}, get_data(S)).

%%====================================================================
%% Status Management Tests
%%====================================================================

get_status_test() ->
    S = new(<<"case-1">>),
    ?assertEqual(created, get_status(S)),

    S1 = S#{status => running},
    ?assertEqual(running, get_status(S1)).

set_status_test() ->
    S = new(<<"case-1">>),
    S1 = set_status(S, running),
    ?assertEqual(running, get_status(S1)),
    ?assert(maps:get(updated_at, get_timestamps(S1)) =/= undefined).

is_status_predicates_test() ->
    Created = new(<<"c">>),
    ?assert(is_created(Created)),
    ?assertNot(is_running(Created)),
    ?assertNot(is_suspended(Created)),
    ?assertNot(is_completed(Created)),
    ?assertNot(is_cancelled(Created)),
    ?assertNot(is_terminal(Created)),

    Running = set_status(Created, running),
    ?assertNot(is_created(Running)),
    ?assert(is_running(Running)),
    ?assertNot(is_suspended(Running)),
    ?assertNot(is_completed(Running)),
    ?assertNot(is_cancelled(Running)),
    ?assertNot(is_terminal(Running)),

    Suspended = set_status(Running, suspended),
    ?assertNot(is_created(Suspended)),
    ?assertNot(is_running(Suspended)),
    ?assert(is_suspended(Suspended)),
    ?assertNot(is_completed(Suspended)),
    ?assertNot(is_cancelled(Suspended)),
    ?assertNot(is_terminal(Suspended)),

    Completed = set_status(Suspended, completed),
    ?assertNot(is_created(Completed)),
    ?assertNot(is_running(Completed)),
    ?assertNot(is_suspended(Completed)),
    ?assert(is_completed(Completed)),
    ?assertNot(is_cancelled(Completed)),
    ?assert(is_terminal(Completed)),

    Cancelled = set_status(Created, cancelled),
    ?assertNot(is_created(Cancelled)),
    ?assertNot(is_running(Cancelled)),
    ?assertNot(is_suspended(Cancelled)),
    ?assertNot(is_completed(Cancelled)),
    ?assert(is_cancelled(Cancelled)),
    ?assert(is_terminal(Cancelled)).

%%====================================================================
%% Data Variable Management Tests
%%====================================================================

get_data_test() ->
    S = new(<<"case-1">>, #{a => 1}),
    ?assertEqual(#{a => 1}, get_data(S)).

set_data_test() ->
    S = new(<<"case-1">>),
    S1 = set_data(S, #{x => 10}),
    ?assertEqual(#{x => 10}, get_data(S1)),

    S2 = set_data(S1, #{y => 20}),
    ?assertEqual(#{y => 20}, get_data(S2)).

update_data_test() ->
    S = new(<<"case-1">>, #{a => 1, b => 2}),
    S1 = update_data(S, #{b => 20, c => 3}),
    ?assertEqual(#{a => 1, b => 20, c => 3}, get_data(S1)).

get_data_value_test() ->
    S = new(<<"case-1">>, #{x => 1, y => 2}),
    ?assertEqual(1, get_data_value(S, x)),
    ?assertEqual(2, get_data_value(S, y)),
    ?assertEqual(undefined, get_data_value(S, missing)).

set_data_value_test() ->
    S = new(<<"case-1">>),
    S1 = set_data_value(S, count, 5),
    ?assertEqual(5, get_data_value(S1, count)),

    S2 = set_data_value(S1, total, 100),
    ?assertEqual(5, get_data_value(S2, count)),
    ?assertEqual(100, get_data_value(S2, total)).

%%====================================================================
%% Work Item Management Tests
%%====================================================================

get_workitems_test() ->
    S = new(<<"case-1">>),
    ?assertEqual(#{}, get_workitems(S)),

    WI = #{wi_id => <<"wi-1">>, task => t1, status => offered},
    S1 = add_workitem(S, WI),
    WIs = get_workitems(S1),
    ?assertEqual(1, maps:size(WIs)),
    ?assertEqual(WI, maps:get(<<"wi-1">>, WIs)).

get_workitem_test() ->
    WI = #{wi_id => <<"wi-1">>, task => t1, status => offered},
    S = add_workitem(new(<<"case-1">>), WI),
    ?assertEqual(WI, get_workitem(S, <<"wi-1">>)),
    ?assertEqual(undefined, get_workitem(S, <<"missing">>)).

add_workitem_test() ->
    S = new(<<"case-1">>),
    WI = #{wi_id => <<"wi-1">>, task => t1, status => offered},
    S1 = add_workitem(S, WI),
    ?assertEqual(1, maps:size(get_workitems(S1))),
    ?assertEqual(WI, get_workitem(S1, <<"wi-1">>)).

add_workitem_with_id_test() ->
    S = new(<<"case-1">>),
    WI = #{task => t1, status => offered},
    S1 = add_workitem(S, <<"wi-1">>, WI),
    ?assertEqual(1, maps:size(get_workitems(S1))),
    Retrieved = get_workitem(S1, <<"wi-1">>),
    ?assertEqual(<<"wi-1">>, maps:get(wi_id, Retrieved)).

remove_workitem_test() ->
    WI1 = #{wi_id => <<"wi-1">>, task => t1, status => offered},
    WI2 = #{wi_id => <<"wi-2">>, task => t2, status => offered},
    S = add_workitem(add_workitem(new(<<"case-1">>), WI1), WI2),
    ?assertEqual(2, maps:size(get_workitems(S))),

    S1 = remove_workitem(S, <<"wi-1">>),
    ?assertEqual(1, maps:size(get_workitems(S1))),
    ?assertEqual(undefined, get_workitem(S1, <<"wi-1">>)),
    ?assertEqual(WI2, get_workitem(S1, <<"wi-2">>)),

    %% Removing non-existent is no-op
    S2 = remove_workitem(S1, <<"missing">>),
    ?assertEqual(1, maps:size(get_workitems(S2))).

list_workitems_test() ->
    WI1 = #{wi_id => <<"a">>, task => t1, status => offered},
    WI2 = #{wi_id => <<"b">>, task => t2, status => completed},
    S = add_workitem(add_workitem(new(<<"c1">>), WI1), WI2),
    List = list_workitems(S),
    ?assertEqual(2, length(List)),
    ?assert(lists:any(fun(#{wi_id := Id}) -> Id =:= <<"a">> end, List)),
    ?assert(lists:any(fun(#{wi_id := Id}) -> Id =:= <<"b">> end, List)).

list_workitems_by_status_test() ->
    WI1 = #{wi_id => <<"a">>, task => t1, status => offered},
    WI2 = #{wi_id => <<"b">>, task => t2, status => completed},
    WI3 = #{wi_id => <<"c">>, task => t3, status => offered},
    S0 = new(<<"c1">>),
    S1 = add_workitem(add_workitem(add_workitem(S0, WI1), WI2), WI3),

    Offered = list_workitems_by_status(S1, offered),
    ?assertEqual(2, length(Offered)),

    Completed = list_workitems_by_status(S1, completed),
    ?assertEqual(1, length(Completed)),

    Failed = list_workitems_by_status(S1, failed),
    ?assertEqual(0, length(Failed)).

%%====================================================================
%% Timestamp Management Tests
%%====================================================================

get_timestamps_test() ->
    S = new(<<"case-1">>),
    Ts = get_timestamps(S),
    ?assertEqual(undefined, maps:get(created_at, Ts)),
    ?assertEqual(undefined, maps:get(started_at, Ts)).

get_timestamp_test() ->
    S = new(<<"case-1">>),
    ?assertEqual(undefined, get_timestamp(S, created_at)),
    ?assertEqual(undefined, get_timestamp(S, missing_key)).

set_timestamp_test() ->
    S = new(<<"case-1">>),
    S1 = set_timestamp(S, custom_event),
    Ts = get_timestamps(S1),
    ?assert(is_integer(maps:get(custom_event, Ts))),
    ?assert(maps:get(custom_event, Ts) > 0),
    ?assert(is_integer(maps:get(updated_at, Ts))).

mark_created_test() ->
    S = new(<<"case-1">>),
    S1 = mark_created(S),
    ?assert(is_integer(get_timestamp(S1, created_at))),
    ?assert(get_timestamp(S1, created_at) > 0).

mark_started_test() ->
    S = new(<<"case-1">>),
    S1 = mark_started(S),
    ?assertEqual(running, get_status(S1)),
    ?assert(is_integer(get_timestamp(S1, started_at))),
    ?assert(get_timestamp(S1, started_at) > 0).

mark_completed_test() ->
    S = new(<<"case-1">>),
    S1 = mark_completed(S),
    ?assertEqual(completed, get_status(S1)),
    ?assert(is_integer(get_timestamp(S1, completed_at))),
    ?assert(get_timestamp(S1, completed_at) > 0).

mark_suspended_test() ->
    S = new(<<"case-1">>),
    S1 = mark_suspended(S),
    ?assertEqual(suspended, get_status(S1)),
    ?assert(is_integer(get_timestamp(S1, suspended_at))),
    ?assert(get_timestamp(S1, suspended_at) > 0).

mark_cancelled_test() ->
    S = new(<<"case-1">>),
    S1 = mark_cancelled(S),
    ?assertEqual(cancelled, get_status(S1)),
    ?assert(is_integer(get_timestamp(S1, cancelled_at))),
    ?assert(get_timestamp(S1, cancelled_at) > 0).

%%====================================================================
%% Serialization Tests
%%====================================================================

to_map_test() ->
    S = new(<<"case-1">>),
    Map = to_map(S),
    ?assertEqual(<<"case-1">>, maps:get(case_id, Map)),
    ?assertEqual(created, maps:get(status, Map)),
    ?assertEqual(#{}, maps:get(workitems, Map)),
    ?assertEqual(#{}, maps:get(data, Map)).

from_map_test() ->
    Map = #{
        case_id => <<"case-1">>,
        status => running,
        workitems => #{},
        data => #{x => 1},
        timestamps => ?DEFAULT_TIMESTAMPS
    },
    S = from_map(Map),
    ?assertEqual(<<"case-1">>, maps:get(case_id, S)),
    ?assertEqual(running, get_status(S)),
    ?assertEqual(#{x => 1}, get_data(S)).

from_map_with_defaults_test() ->
    %% Minimal map - defaults should be filled
    Map = #{case_id => <<"case-1">>, status => running},
    S = from_map(Map),
    ?assertEqual(<<"case-1">>, maps:get(case_id, S)),
    ?assertEqual(running, get_status(S)),
    ?assertEqual(#{}, get_workitems(S)),
    ?assertEqual(#{}, get_data(S)),
    ?assertEqual(?DEFAULT_TIMESTAMPS, get_timestamps(S)).

round_trip_test() ->
    Original = new(<<"case-1">>, #{amount => 100}),
    Original1 = add_workitem(Original, #{wi_id => <<"wi-1">>, task => t1, status => offered}),
    Original2 = mark_started(Original1),

    Map = to_map(Original2),
    Restored = from_map(Map),

    ?assertEqual(get_status(Original2), get_status(Restored)),
    ?assertEqual(get_workitems(Original2), get_workitems(Restored)),
    ?assertEqual(get_data(Original2), get_data(Restored)).

%%====================================================================
%% Integration Tests
%%====================================================================

workflow_lifecycle_test() ->
    %% Create new case
    S0 = new(<<"workflow-123">>, #{amount => 1000}),

    %% Mark as created
    S1 = mark_created(S0),
    ?assert(is_created(S1)),
    ?assert(is_integer(get_timestamp(S1, created_at))),

    %% Start the workflow
    S2 = mark_started(S1),
    ?assert(is_running(S2)),
    ?assert(is_integer(get_timestamp(S2, started_at))),

    %% Add work items
    WI1 = #{wi_id => <<"wi-1">>, task => review, status => offered},
    WI2 = #{wi_id => <<"wi-2">>, task => approve, status => offered},
    S3 = add_workitem(add_workitem(S2, WI1), WI2),
    ?assertEqual(2, maps:size(get_workitems(S3))),

    %% Allocate first work item
    S4 = update_data(S3, #{reviewer => <<"alice">>}),
    ?assertEqual(<<"alice">>, get_data_value(S4, reviewer)),

    %% Suspend workflow
    S5 = mark_suspended(S4),
    ?assert(is_suspended(S5)),
    ?assert(is_integer(get_timestamp(S5, suspended_at))),

    %% Resume workflow
    S6 = mark_started(S5),
    ?assert(is_running(S6)),

    %% Complete workflow
    S7 = mark_completed(S6),
    ?assert(is_completed(S7)),
    ?assert(is_terminal(S7)),
    ?assert(is_integer(get_timestamp(S7, completed_at))),

    %% Serialize and restore
    Map = to_map(S7),
    S8 = from_map(Map),
    ?assertEqual(get_status(S7), get_status(S8)),
    ?assertEqual(get_data(S7), get_data(S8)),

    ok.

-endif.
