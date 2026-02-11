%%%-------------------------------------------------------------------
%%% @doc ln_frames - Frame management for execution context.
%%%
%%% Frames represent execution contexts with local bindings and
%%% control flow information.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_frames).

%% API
-export([new/5, new/7]).
-export([set_binding/3, get_binding/2, delete_binding/2]).
-export([set_pc/2, increment_pc/1, get_pc/1]).
-export([set_return_pc/2, get_return_pc/1]).
-export([set_scope/2, get_scope/1]).
-export([get_id/1, get_parent/1]).
-export([to_map/1]).

%% Stack operations
-export([push/2, pop/1, peek/1, depth/1]).

%% Types
-export_type([frame/0, frame_id/0, stack/0, bindings/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type frame_id() :: reference().

-type bindings() :: #{atom() => term()}.

-type scope_id() :: term().

-record(frame, {
    id :: frame_id(),
    parent :: frame_id() | undefined,
    bindings :: bindings(),
    pc :: non_neg_integer(),
    code :: term(),
    return_pc :: non_neg_integer() | undefined,
    scope_id :: scope_id()
}).

-opaque frame() :: #frame{}.

-type stack() :: [frame()].

%%%-------------------------------------------------------------------
%%% Frame construction
%%%-------------------------------------------------------------------

%% @doc Create a new frame.
-spec new(frame_id(), frame_id() | undefined, bindings(),
          non_neg_integer(), term()) -> frame().
new(Id, Parent, Bindings, PC, Code) ->
    #frame{
        id = Id,
        parent = Parent,
        bindings = Bindings,
        pc = PC,
        code = Code,
        return_pc = undefined,
        scope_id = undefined
    }.

%% @doc Create a new frame with return PC and scope.
-spec new(frame_id(), frame_id() | undefined, bindings(),
          non_neg_integer(), term(), non_neg_integer() | undefined, scope_id()) -> frame().
new(Id, Parent, Bindings, PC, Code, ReturnPC, ScopeId) ->
    #frame{
        id = Id,
        parent = Parent,
        bindings = Bindings,
        pc = PC,
        code = Code,
        return_pc = ReturnPC,
        scope_id = ScopeId
    }.

%%%-------------------------------------------------------------------
%%% Binding operations
%%%-------------------------------------------------------------------

%% @doc Set a binding in the frame.
-spec set_binding(frame(), atom(), term()) -> frame().
set_binding(#frame{bindings = Bindings} = Frame, Key, Value) ->
    Frame#frame{bindings = Bindings#{Key => Value}}.

%% @doc Get a binding from the frame.
-spec get_binding(frame(), atom()) -> {ok, term()} | error.
get_binding(#frame{bindings = Bindings}, Key) ->
    case maps:find(Key, Bindings) of
        {ok, _} = Result -> Result;
        error -> error
    end.

%% @doc Delete a binding from the frame.
-spec delete_binding(frame(), atom()) -> frame().
delete_binding(#frame{bindings = Bindings} = Frame, Key) ->
    Frame#frame{bindings = maps:remove(Key, Bindings)}.

%%%-------------------------------------------------------------------
%%% PC operations
%%%-------------------------------------------------------------------

%% @doc Set the program counter.
-spec set_pc(frame(), non_neg_integer()) -> frame().
set_pc(Frame, PC) ->
    Frame#frame{pc = PC}.

%% @doc Increment the program counter.
-spec increment_pc(frame()) -> frame().
increment_pc(#frame{pc = PC} = Frame) ->
    Frame#frame{pc = PC + 1}.

%% @doc Get the program counter.
-spec get_pc(frame()) -> non_neg_integer().
get_pc(#frame{pc = PC}) ->
    PC.

%% @doc Set the return address.
-spec set_return_pc(frame(), non_neg_integer() | undefined) -> frame().
set_return_pc(Frame, ReturnPC) ->
    Frame#frame{return_pc = ReturnPC}.

%% @doc Get the return address.
-spec get_return_pc(frame()) -> non_neg_integer() | undefined.
get_return_pc(#frame{return_pc = ReturnPC}) ->
    ReturnPC.

%%%-------------------------------------------------------------------
%%% Scope operations
%%%-------------------------------------------------------------------

%% @doc Set the scope ID.
-spec set_scope(frame(), scope_id()) -> frame().
set_scope(Frame, ScopeId) ->
    Frame#frame{scope_id = ScopeId}.

%% @doc Get the scope ID.
-spec get_scope(frame()) -> scope_id().
get_scope(#frame{scope_id = ScopeId}) ->
    ScopeId.

%%%-------------------------------------------------------------------
%%% Frame introspection
%%%-------------------------------------------------------------------

%% @doc Get the frame ID.
-spec get_id(frame()) -> frame_id().
get_id(#frame{id = Id}) ->
    Id.

%% @doc Get the parent frame ID.
-spec get_parent(frame()) -> frame_id() | undefined.
get_parent(#frame{parent = Parent}) ->
    Parent.

%% @doc Convert frame to map for inspection.
-spec to_map(frame()) -> map().
to_map(#frame{id = Id, parent = Parent, bindings = Bindings,
              pc = PC, return_pc = ReturnPC, scope_id = ScopeId}) ->
    #{
        id => Id,
        parent => Parent,
        bindings => Bindings,
        pc => PC,
        return_pc => ReturnPC,
        scope_id => ScopeId
    }.

%%%-------------------------------------------------------------------
%%% Stack operations
%%%-------------------------------------------------------------------

%% @doc Push a frame onto the stack.
-spec push(stack(), frame()) -> stack().
push(Stack, Frame) ->
    [Frame | Stack].

%% @doc Pop a frame from the stack.
-spec pop(stack()) -> {ok, frame(), stack()} | error.
pop([]) ->
    error;
pop([Frame | Rest]) ->
    {ok, Frame, Rest}.

%% @doc Peek at the top frame.
-spec peek(stack()) -> {ok, frame()} | error.
peek([]) ->
    error;
peek([Frame | _]) ->
    {ok, Frame}.

%% @doc Get stack depth.
-spec depth(stack()) -> non_neg_integer().
depth(Stack) ->
    length(Stack).
