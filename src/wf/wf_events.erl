%% -*- erlang -*-
%%%% @doc Event streaming for workflows
%%
%% This module provides a gen_event based event streaming system for
%% workflow engines. It manages event handlers and publishes workflow
%% events including transition firings and state changes.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>gen_event based event management</li>
%%   <li>Transition event emission</li>
%%   <li>State change event emission</li>
%%   <li>Handler subscription/unsubscription</li>
%%   <li>Synchronous and asynchronous event posting</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Starting an event manager:
%% ```erlang
%% > {ok, Manager} = wf_events:start_link(#{name => workflow_events}).
%% {ok, <0.123.0>}
%% ```
%%
%% Adding an event handler:
%% ```erlang
%% > ok = wf_events:add_handler(Manager, wf_events_logger, []).
%% ok
%% ```
%%
%% Publishing a transition event:
%% ```erlang
%% > Event = #{
%% >   type => transition,
%% >   case_id => <<"c1">>,
%% >   transition => t1,
%% >   timestamp => 1000
%% > }.
%% > ok = wf_events:notify(Manager, Event).
%% ok
%% ```
%%
%% Publishing a state change event:
%% ```erlang
%% > Event = #{
%% >   type => state_change,
%% >   case_id => <<"c1">>,
%% >   old_state => running,
%% >   new_state => suspended,
%% >   timestamp => 1001
%% > }.
%% > ok = wf_events:notify(Manager, Event).
%% ok
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_events).

%%====================================================================
%% Exports
%%====================================================================

%% API functions
-export([start_link/1,
         add_handler/3, add_handler/4,
         remove_handler/2,
         notify/2,
         sync_notify/2,
         swap_handler/4,
         swap_handler/5,
         delete_handler/2,
         which_handlers/1,
         stop/1,
         doctest_test/0]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Event manager reference.
%%
%% Can be a pid or a registered atom name.
%%--------------------------------------------------------------------
-type manager() :: atom() | pid().

%%--------------------------------------------------------------------
%% @doc Event record for transition firing.
%%
%% Maps containing:
%% - <b>type:</b> atom 'transition'
%% - <b>case_id:</b> Binary case identifier
%% - <b>transition:</b> Atom transition name
%% - <b>timestamp:</b> Integer timestamp
%% - <b>mode:</b> Optional mode map of consumed tokens
%% - <b>produce:</b> Optional produce map of produced tokens
%%--------------------------------------------------------------------
-type transition_event() :: #{
    type := transition,
    case_id := binary(),
    transition := atom(),
    timestamp := integer(),
    mode => map(),
    produce => map()
}.

%%--------------------------------------------------------------------
%% @doc Event record for state changes.
%%
%% Maps containing:
%% - <b>type:</b> atom 'state_change'
%% - <b>case_id:</b> Binary case identifier
%% - <b>old_state:</b> Previous state atom
%% - <b>new_state:</b> New state atom
%% - <b>timestamp:</b> Integer timestamp
%% - <b>reason:</b> Optional reason for state change
%%--------------------------------------------------------------------
-type state_change_event() :: #{
    type := state_change,
    case_id := binary(),
    old_state := atom(),
    new_state := atom(),
    timestamp := integer(),
    reason => atom()
}.

%%--------------------------------------------------------------------
%% @doc Event record for work item operations.
%%
%% Maps containing:
%% - <b>type:</b> atom 'work_item'
%% - <b>case_id:</b> Binary case identifier
%% - <b>wi_id:</b> Binary work item identifier
%% - <b>task:</b> Atom task name
%% - <b>operation:</b> offered | allocated | started | completed
%% - <b>timestamp:</b> Integer timestamp
%% - <b>user:</b> Optional user who performed operation
%%--------------------------------------------------------------------
-type work_item_event() :: #{
    type := work_item,
    case_id := binary(),
    wi_id := binary(),
    task := atom(),
    operation := atom(),
    timestamp := integer(),
    user => atom() | binary()
}.

%%--------------------------------------------------------------------
%% @doc Generic event record.
%%
%% Any map containing at minimum a type field.
%%--------------------------------------------------------------------
-type event() :: transition_event() |
                 state_change_event() |
                 work_item_event() |
                 #{
                     type := atom(),
                     _ => _
                 }.

%%--------------------------------------------------------------------
%% @doc Configuration map for event manager.
%%
%% Required keys:
%% - <b>name:</b> Atom name for the manager
%%
%% Optional keys:
%% - <b>buffer_size:</b> Maximum events to buffer (default: 1000)
%%--------------------------------------------------------------------
-type config() :: #{
    name := atom(),
    buffer_size => non_neg_integer()
}.

%% Export types
-export_type([manager/0, event/0, transition_event/0, state_change_event/0,
              work_item_event/0, config/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts an event manager with configuration.
%%
%% Creates a new gen_event manager for workflow event streaming.
%% The manager can be accessed by name or pid.
%%
%% <h4>Configuration Options</h4>
%% <ul>
%%   <li><b>name:</b> Registered name for the manager (required)</li>
%%   <li><b>buffer_size:</b> Maximum buffered events (default: 1000)</li>
%% </ul>
%%
%% @param Config Configuration map
%% @returns {ok, Pid} on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Config :: config()) -> {ok, pid()} | {error, term()}.

start_link(#{name := Name} = Config) ->
    gen_event:start_link({local, Name});
start_link(_) ->
    {error, badarg}.

%%--------------------------------------------------------------------
%% @doc Adds an event handler to the manager.
%%
%% Registers a new event handler module with the manager. The handler
%% will receive all published events matching its interests.
%%
%% <h4>Handler Initialization</h4>
%% The handler module's init/1 callback is called with Args to initialize
%% the handler state. If initialization fails, the handler is not added.
%%
%% @param Manager Event manager reference
%% @param Handler Handler module name
%% @param Args Arguments passed to handler's init/1
%% @returns ok on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec add_handler(Manager :: manager(), Handler :: atom(), Args :: term()) ->
          ok | {error, term()}.

add_handler(Manager, Handler, Args) ->
    gen_event:add_handler(Manager, Handler, Args).

%%--------------------------------------------------------------------
%% @doc Adds an event handler with a unique identifier.
%%
%% Same as add_handler/3 but allows specifying a handler ID for
%% distinguishing multiple instances of the same handler module.
%%
%% @param Manager Event manager reference
%% @param Handler Handler module name
%% @param Id Unique handler identifier
%% @param Args Arguments passed to handler's init/1
%% @returns ok on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec add_handler(Manager :: manager(), Handler :: atom(),
                  Id :: term(), Args :: term()) ->
          ok | {error, term()}.

add_handler(Manager, Handler, Id, Args) ->
    gen_event:add_handler(Manager, {Handler, Id}, Args).

%%--------------------------------------------------------------------
%% @doc Removes an event handler from the manager.
%%
%% Unregisters a handler so it no longer receives events.
%% The handler's terminate/2 callback is called for cleanup.
%%
%% @param Manager Event manager reference
%% @param Handler Handler module name or {Module, Id} tuple
%% @returns ok on success, {error, Reason} on failure
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_handler(Manager :: manager(), Handler :: atom() | {atom(), term()}) ->
          ok | {error, term()}.

remove_handler(Manager, Handler) ->
    gen_event:delete_handler(Manager, Handler, remove).

%%--------------------------------------------------------------------
%% @doc Publishes an event asynchronously.
%%
%% Sends an event to all registered handlers. The call returns
%% immediately without waiting for handlers to process the event.
%%
%% @param Manager Event manager reference
%% @param Event Event map to publish
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec notify(Manager :: manager(), Event :: event()) -> ok.

notify(Manager, Event) when is_map(Event) ->
    gen_event:notify(Manager, Event);
notify(_, _) ->
    {error, badarg}.

%%--------------------------------------------------------------------
%% @doc Publishes an event synchronously.
%%
%% Sends an event to all registered handlers and waits for all
%% handlers to process it before returning.
%%
%% @param Manager Event manager reference
%% @param Event Event map to publish
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec sync_notify(Manager :: manager(), Event :: event()) ->
          ok | {error, term()}.

sync_notify(Manager, Event) when is_map(Event) ->
    gen_event:sync_notify(Manager, Event);
sync_notify(_, _) ->
    {error, badarg}.

%%--------------------------------------------------------------------
%% @doc Swaps one event handler for another.
%%
%% Atomically removes an old handler and adds a new one.
%% Useful for upgrading handler implementations.
%%
%% @param Manager Event manager reference
%% @param OldHandler Old handler module to remove
%% @param NewHandler New handler module to add
%% @param Args Arguments for new handler's init/1
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec swap_handler(Manager :: manager(), OldHandler :: atom() | {atom(), term()},
                   NewHandler :: atom(), Args :: term()) ->
          ok | {error, term()}.

swap_handler(Manager, OldHandler, NewHandler, Args) ->
    gen_event:swap_handler(Manager, OldHandler, remove, NewHandler, Args).

%%--------------------------------------------------------------------
%% @doc Swaps one event handler for another with explicit termination reason.
%%
%% Same as swap_handler/4 but allows specifying the termination reason
%% passed to the old handler's terminate/2 callback.
%%
%% @param Manager Event manager reference
%% @param OldHandler Old handler to remove
%% @param Reason Termination reason for old handler
%% @param NewHandler New handler module to add
%% @param Args Arguments for new handler's init/1
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec swap_handler(Manager :: manager(), OldHandler :: atom() | {atom(), term()},
                   Reason :: term(), NewHandler :: atom(), Args :: term()) ->
          ok | {error, term()}.

swap_handler(Manager, OldHandler, Reason, NewHandler, Args) ->
    gen_event:swap_handler(Manager, OldHandler, Reason, NewHandler, Args).

%%--------------------------------------------------------------------
%% @doc Deletes an event handler with a specific reason.
%%
%% Removes a handler and passes the specified reason to its
%% terminate/2 callback.
%%
%% @param Manager Event manager reference
%% @param Handler Handler to delete
%% @param Reason Termination reason
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_handler(Manager :: manager(), Handler :: atom() | {atom(), term()},
                     Reason :: term()) ->
          ok | {error, term()}.

delete_handler(Manager, Handler, Reason) ->
    gen_event:delete_handler(Manager, Handler, Reason).

%%--------------------------------------------------------------------
%% @doc Returns list of installed event handlers.
%%
%% Returns the handler modules (or {module, id} tuples) currently
%% registered with the event manager.
%%
%% @param Manager Event manager reference
%% @returns [atom() | {atom(), term()}] List of handlers
%%
%% @end
%%--------------------------------------------------------------------
-spec which_handlers(Manager :: manager()) -> [atom() | {atom(), term()}].

which_handlers(Manager) ->
    gen_event:which_handlers(Manager).

%%--------------------------------------------------------------------
%% @doc Stops the event manager.
%%
%% Terminates the event manager and all registered handlers.
%%
%% @param Manager Event manager reference
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Manager :: manager()) -> ok.

stop(Manager) ->
    gen_event:stop(Manager).

%%====================================================================
%% Doctest
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests for the module.
%%
%% Tests event manager creation, handler registration, and event
%% publishing.
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).
