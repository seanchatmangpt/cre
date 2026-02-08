%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% @doc Worklist observer pattern - message-based notifications for work items.
%%
%% Implements Joe Armstrong's message-passing philosophy: observers subscribe
%% via gen_event and receive notifications when work items change state.
%%
%% <h3>Event Types</h3>
%% <ul>
%%   <li><b>work_item_offered</b> - Work item offered to participant</li>
%%   <li><b>work_item_allocated</b> - Work item allocated to participant</li>
%%   <li><b>work_item_started</b> - Participant started work on item</li>
%%   <li><b>work_item_completed</b> - Work item completed</li>
%%   <li><b>work_item_failed</b> - Work item marked as failed</li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% Subscribe an observer:
%% ```erlang
%% gen_event:add_handler(yawl_worklist, my_observer, []).
%% ```
%%
%% Notify observers (from workflow engine):
%% ```erlang
%% yawl_worklist:notify({work_item_offered, CaseId, Participant, WorkItem}).
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_worklist).

%%====================================================================
%% Exports
%%====================================================================

-export([start_link/0]).
-export([add_handler/2, add_handler/3]).
-export([delete_handler/2]).
-export([notify/1]).
-export([sync_notify/1]).

%%====================================================================
%% Types
%%====================================================================

-type worklist_event() ::
    {work_item_offered, binary(), binary(), map()}
    | {work_item_allocated, binary(), binary(), map()}
    | {work_item_started, binary(), binary(), map()}
    | {work_item_completed, binary(), binary(), map()}
    | {work_item_failed, binary(), binary(), map()}.

%%====================================================================
%% API
%%====================================================================

%% @doc Starts the worklist event manager.
%%
%% The manager must be started before adding handlers or notifying.
%% Typically started as a child of cre_sup.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_event:start_link({local, ?MODULE}).

%% @doc Adds an event handler.
-spec add_handler(Handler :: module(), Args :: term()) -> ok | {error, term()}.
add_handler(Handler, Args) ->
    gen_event:add_handler(?MODULE, Handler, Args).

%% @doc Adds an event handler with options.
-spec add_handler(Handler :: module(), Args :: term(), Options :: [term()]) ->
    ok | {error, term()}.
add_handler(Handler, Args, Options) ->
    gen_event:add_handler(?MODULE, Handler, Args, Options).

%% @doc Removes an event handler.
-spec delete_handler(Handler :: module(), Args :: term()) -> term().
delete_handler(Handler, Args) ->
    gen_event:delete_handler(?MODULE, Handler, Args).

%% @doc Notifies all observers of a worklist event (async).
-spec notify(Event :: worklist_event()) -> ok.
notify(Event) ->
    try
        gen_event:notify(?MODULE, Event)
    catch
        _:_ -> ok
    end.

%% @doc Notifies all observers of a worklist event (sync).
-spec sync_notify(Event :: worklist_event()) -> ok.
sync_notify(Event) ->
    try
        gen_event:sync_notify(?MODULE, Event)
    catch
        _:_ -> ok
    end.

