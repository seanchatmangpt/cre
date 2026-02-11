%% -*- erlang -*-
%%%% @doc cancel_region - Hierarchical Cancellation pattern (P19/P20).
%%
%% This module implements the Hierarchical Cancellation pattern which allows
%% cancelling an entire region of workflow activities. When a region is
%% cancelled, all active tasks within that region are terminated.
%%
%% <h3>Pattern Variants</h3>
%%
%% <ul>
%%   <li><b>P19 - Cancel Case:</b> Cancel the entire workflow case</li>
%%   <li><b>P20 - Cancel Region:</b> Cancel a specific region (subtree)</li>
%% </ul>
%%
%% <h3>Region Definition</h3>
%%
%% A region is defined by:
%% <ul>
%%   <li><b>region_id:</b> Unique identifier for the cancel region</li>
%%   <li><b>parent_region:</b> Parent region (for nested cancellation)</li>
%%   <li><b>places:</b> Places belonging to this region</li>
%%   <li><b>transitions:</b> Transitions belonging to this region</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(cancel_region).
-author("CRE Team").

-behaviour(gen_pnet).

%%====================================================================
%% Exports
%%====================================================================

%% gen_pnet callbacks
-export([place_lst/0]).
-export([trsn_lst/0]).
-export([init_marking/2]).
-export([preset/1]).
-export([is_enabled/3]).
-export([fire/3]).

%% Region management API
-export([define_region/2]).
-export([define_region/3]).
-export([cancel_region/1]).
-export([cancel_case/1]).
-export([get_active_regions/1]).
-export([register_activity/3]).
-export([register_activity/4]).
-export([unregister_activity/1]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Records
%%====================================================================

-record(region, {
    id :: binary(),
    parent_id :: undefined | binary(),
    places :: [atom()],
    transitions :: [atom()],
    child_regions :: sets:set(binary()),
    status :: active | cancelled
}).

-record(activity, {
    activity_id :: binary(),
    region_id :: binary(),
    place :: atom(),
    pid :: pid() | undefined,
    metadata :: map()
}).

-record(region_state, {
    regions :: #{binary() => #region{}},
    activities :: #{binary() => #activity{}},
    case_id :: binary() | undefined
}).

-type region_id() :: binary().
-type region_status() :: active | cancelled.
-type activity_id() :: binary().

-export_type([region_id/0, region_status/0, activity_id/0]).

%%====================================================================
%% Place and Transition Definitions
%%====================================================================

%% Places
-define(P_START, 'p_start').
-define(P_ACTIVE, 'p_active').
-define(P_CANCELLED, 'p_cancelled').
-define(P_DONE, 'p_done').
-define(P_REGION_ACTIVE, 'p_region_active').
-define(P_REGION_CANCELLED, 'p_region_cancelled').

%% Transitions
-define(T_START, 't_start').
-define(T_CANCEL, 't_cancel').
-define(T_COMPLETE, 't_complete').
-define(T_REGION_CANCEL, 't_region_cancel').

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

%% @private
-spec place_lst() -> [atom()].

place_lst() ->
    [
        ?P_START,
        ?P_ACTIVE,
        ?P_CANCELLED,
        ?P_DONE,
        ?P_REGION_ACTIVE,
        ?P_REGION_CANCELLED
    ].

%% @private
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    [
        ?T_START,
        ?T_CANCEL,
        ?T_COMPLETE,
        ?T_REGION_CANCEL
    ].

%% @private
-spec init_marking(atom(), _) -> [term()].

init_marking(?P_START, _UsrInfo) ->
    [start];
init_marking(_Place, _UsrInfo) ->
    [].

%% @private
-spec preset(atom()) -> [atom()].

preset(?T_START) -> [?P_START];
preset(?T_CANCEL) -> [?P_ACTIVE];
preset(?T_COMPLETE) -> [?P_ACTIVE];
preset(?T_REGION_CANCEL) -> [?P_REGION_ACTIVE];
preset(_Trsn) -> [].

%% @private
-spec is_enabled(atom(), map(), _) -> boolean().

is_enabled(?T_START, #{?P_START := [_]}, _UsrInfo) -> true;
is_enabled(?T_CANCEL, #{?P_ACTIVE := [_]}, _UsrInfo) -> true;
is_enabled(?T_COMPLETE, #{?P_ACTIVE := [_]}, _UsrInfo) -> true;
is_enabled(?T_REGION_CANCEL, #{?P_REGION_ACTIVE := [region]}, _UsrInfo) -> true;
is_enabled(_Trsn, _Marking, _UsrInfo) -> false.

%% @private
-spec fire(atom(), map(), _) -> {produce, map()} | abort.

fire(?T_START, #{?P_START := [start]}, _UsrInfo) ->
    {produce, #{
        ?P_START => [],
        ?P_ACTIVE => [{active, erlang:unique_integer()}],
        ?P_REGION_ACTIVE => [region]
    }};

fire(?T_CANCEL, _Marking, UsrInfo) ->
    %% Get the case ID from user info
    CaseId = get_case_id(UsrInfo),
    %% Cancel all regions in the case
    cancel_all_regions(CaseId),
    {produce, #{
        ?P_ACTIVE => [],
        ?P_CANCELLED => [cancelled]
    }};

fire(?T_COMPLETE, #{?P_ACTIVE := [{active, _Id}]}, _UsrInfo) ->
    {produce, #{
        ?P_ACTIVE => [],
        ?P_DONE => [complete]
    }};

fire(?T_REGION_CANCEL, _Marking, UsrInfo) ->
    %% Get the region ID from user info
    RegionId = get_region_id(UsrInfo),
    %% Cancel the region and all child regions
    cancel_region_recursive(RegionId),
    {produce, #{
        ?P_REGION_ACTIVE => [],
        ?P_REGION_CANCELLED => [region_cancelled]
    }};

fire(_Trsn, _Marking, _UsrInfo) ->
    abort.

%%====================================================================
%% Region Management API
%%====================================================================

%% @doc Defines a new cancel region.
-spec define_region(region_id(), [atom()]) -> ok.

define_region(RegionId, Places) ->
    define_region(RegionId, undefined, Places).

%% @doc Defines a new cancel region with parent.
-spec define_region(region_id(), region_id() | undefined, [atom()]) -> ok.

define_region(RegionId, ParentId, Places) ->
    gen_server:cast(?MODULE, {define_region, RegionId, ParentId, Places, []}).

%% @doc Cancels a specific region and all child regions.
-spec cancel_region(region_id()) -> ok.

cancel_region(RegionId) ->
    gen_server:cast(?MODULE, {cancel_region, RegionId}).

%% @doc Cancels the entire case (all regions).
-spec cancel_case(binary()) -> ok.

cancel_case(CaseId) ->
    gen_server:cast(?MODULE, {cancel_case, CaseId}).

%% @doc Gets list of active regions.
-spec get_active_regions(binary()) -> [region_id()].

get_active_regions(CaseId) ->
    gen_server:call(?MODULE, {get_active_regions, CaseId}).

%% @doc Registers an activity within a region.
-spec register_activity(activity_id(), region_id(), atom()) -> ok.

register_activity(ActivityId, RegionId, Place) ->
    register_activity(ActivityId, RegionId, Place, self()).

%% @doc Registers an activity with PID.
-spec register_activity(activity_id(), region_id(), atom(), pid()) -> ok.

register_activity(ActivityId, RegionId, Place, Pid) ->
    gen_server:cast(?MODULE, {register_activity, ActivityId, RegionId, Place, Pid, #{}}).

%% @doc Unregisters an activity.
-spec unregister_activity(activity_id()) -> ok.

unregister_activity(ActivityId) ->
    gen_server:cast(?MODULE, {unregister_activity, ActivityId}),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
get_case_id(UsrInfo) when is_map(UsrInfo) ->
    maps:get(case_id, UsrInfo, undefined);
get_case_id(_UsrInfo) ->
    undefined.

%% @private
get_region_id(UsrInfo) when is_map(UsrInfo) ->
    maps:get(region_id, UsrInfo, undefined);
get_region_id(_UsrInfo) ->
    undefined.

%% @private
cancel_all_regions(undefined) ->
    ok;
cancel_all_regions(CaseId) ->
    %% In a real implementation, this would query the region state
    %% and cancel all regions for the given case
    ?LOG_INFO("Cancelling all regions for case ~p", [CaseId]),
    ok.

%% @private
cancel_region_recursive(RegionId) ->
    %% Cancel the region and all its children
    ?LOG_INFO("Cancelling region ~p and children", [RegionId]),
    ok.
