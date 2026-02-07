%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
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

-module(pnet_types).
-moduledoc """
Type validators for Petri net data structures.

All validators are total: they return true/false and never crash.

```erlang
> pnet_types:is_place(p1).
true
> pnet_types:is_place("p1").
false

> pnet_types:is_trsn(t1).
true
> pnet_types:is_trsn(123).
false

> pnet_types:is_token({any, term, is, ok}).
true
```

```erlang
> M0 = #{p1 => [a,b], p2 => []}.
> pnet_types:is_marking(M0).
true
> pnet_types:is_marking(#{p1 => a}).
false
> pnet_types:is_marking([]).
false
```

```erlang
> Mode = #{p1 => [a], p2 => [b]}.
> pnet_types:is_mode(Mode).
true
> pnet_types:is_mode(#{p1 => a}).
false
```

```erlang
> pnet_types:is_var(x).
true
> pnet_types:is_var(<<"x">>).
false

> Bind = #{x => 42, y => <<"ok">>}.
> pnet_types:is_binding(Bind).
true
> pnet_types:is_binding(#{<<"x">> => 1}).
false

> CMode = {Bind, #{p1 => [a]}}.
> pnet_types:is_cmode(CMode).
true
> pnet_types:is_cmode({#{}, not_a_mode}).
false
```

```erlang
> Move = #{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}}.
> pnet_types:is_move(Move).
true
> pnet_types:is_move(#{trsn => t1}).
false

> R = #{before_hash => <<1>>, after_hash => <<2>>, move => Move, ts => 123}.
> pnet_types:is_receipt(R).
true
> pnet_types:is_receipt(R#{ts => <<"nope">>}).
false
```

<h3>Type Categories</h3>
<ul>
  <li><strong>Basic Types:</strong> place, trsn, token</li>
  <li><strong>State Types:</strong> marking, mode, consume_map, produce_map</li>
  <li><strong>Colored Types:</strong> var, binding, cmode</li>
  <li><strong>Execution Types:</strong> move, receipt</li>
</ul>

<h3>Usage</h3>
All validation functions are total - they return boolean() and
never crash, making them safe to use in guards and assertions.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Basic Type Validators
-export([is_place/1,
         is_trsn/1,
         is_token/1,
         is_var/1]).

%% State Type Validators
-export([is_marking/1,
         is_consume_map/1,
         is_produce_map/1,
         is_mode/1]).

%% Colored Type Validators
-export([is_binding/1,
         is_cmode/1]).

%% Execution Type Validators
-export([is_move/1,
         is_receipt/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A place in the Petri net.
%%
%% Places are nodes where tokens reside. Represented as atoms for
%% efficient pattern matching and comparison.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A transition in the Petri net.
%%
%% Transitions are nodes that consume tokens from input places and
%% produce tokens to output places when enabled and fired.
%%--------------------------------------------------------------------
-type trsn() :: atom().

%%--------------------------------------------------------------------
%% @doc A token in the Petri net.
%%
%% Tokens can be any Erlang term, allowing for flexible data flow
%% through the net.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%
%% Each place atom maps to a list of tokens currently in that place.
%% Empty lists represent places with no tokens.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A consume map specifies tokens to be consumed.
%%
%% Maps places to the specific list of tokens that will be removed
%% during transition firing.
%%--------------------------------------------------------------------
-type consume_map() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A produce map specifies tokens to be produced.
%%
%% Maps places to the list of tokens that will be added during
%% transition firing.
%%--------------------------------------------------------------------
-type produce_map() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A mode specifies token availability for transition firing.
%%
%% Maps each input place to the list of tokens that enable the
%% transition. A transition is enabled when its mode is satisfied.
%%--------------------------------------------------------------------
-type mode() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A variable name in colored Petri nets.
%%
%% Variables allow token values to be bound and referenced in
%% colored Petri net expressions.
%%--------------------------------------------------------------------
-type var() :: atom().

%%--------------------------------------------------------------------
%% @doc A binding maps variables to their concrete values.
%%
%% Used in colored Petri nets to instantiate variable-based
%% transition guards and arc expressions.
%%--------------------------------------------------------------------
-type binding() :: #{var() => term()}.

%%--------------------------------------------------------------------
%% @doc A colored mode combines a binding with a token mode.
%%
%% Extends the basic mode with variable bindings for colored Petri
%% net execution. The binding provides values for variables used
%% in transition evaluation.
%%--------------------------------------------------------------------
-type cmode() :: {binding(), mode()}.

%%--------------------------------------------------------------------
%% @doc A move represents a complete transition firing.
%%
%% Contains the transition being fired, the mode (or cmode) under
%% which it fires, and the produce map of output tokens. This is
%% the unit of execution in Petri net semantics.
%%--------------------------------------------------------------------
-type move() :: #{trsn := trsn(),
                  mode := mode() | cmode(),
                  produce := produce_map()}.

%%--------------------------------------------------------------------
%% @doc A receipt records the execution of a move.
%%
%% Contains hashes of the marking before and after execution,
%% the move that was executed, and a timestamp for ordering
%% and verification purposes.
%%--------------------------------------------------------------------
-type receipt() :: #{before_hash := binary(),
                     after_hash := binary(),
                     move := move(),
                     ts := integer()}.

-export_type([place/0,
              trsn/0,
              token/0,
              marking/0,
              consume_map/0,
              produce_map/0,
              mode/0,
              var/0,
              binding/0,
              cmode/0,
              move/0,
              receipt/0]).

%%====================================================================
%% Validation Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid place.
%%
%% A valid place is an atom. Places are nodes where tokens reside.
%% The function never crashes.
%%
%% ```erlang
%% > pnet_types:is_place(p1).
%% true
%% > pnet_types:is_place("p1").
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_place(term()) -> boolean().

is_place(Term) when is_atom(Term) -> true;
is_place(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid transition.
%%
%% A valid transition is an atom. Transitions consume and produce tokens.
%% The function never crashes.
%%
%% ```erlang
%% > pnet_types:is_trsn(t1).
%% true
%% > pnet_types:is_trsn(123).
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_trsn(term()) -> boolean().

is_trsn(Term) when is_atom(Term) -> true;
is_trsn(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid token.
%%
%% Tokens can be any Erlang term. All terms are valid tokens.
%% The function never crashes.
%%
%% ```erlang
%% > pnet_types:is_token({any, term, is, ok}).
%% true
%% > pnet_types:is_token(undefined).
%% true
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_token(term()) -> boolean().

is_token(_Term) -> true.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid variable name.
%%
%% A valid variable is an atom, used in colored Petri nets for
%% variable bindings. The function never crashes.
%%
%% ```erlang
%% > pnet_types:is_var(x).
%% true
%% > pnet_types:is_var(<<"x">>).
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_var(term()) -> boolean().

is_var(Term) when is_atom(Term) -> true;
is_var(_) -> false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid marking.
%%
%% A valid marking is a map where all keys are atoms (places) and
%% all values are lists (of tokens). The function never crashes.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_marking(term()) -> boolean().

is_marking(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_marking(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid consume_map.
%%
%% A valid consume_map is a map where all keys are atoms (places)
%% and all values are lists (of tokens to consume). The function
%% never crashes.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_consume_map(term()) -> boolean().

is_consume_map(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_consume_map(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid produce_map.
%%
%% A valid produce_map is a map where all keys are atoms (places)
%% and all values are lists (of tokens to produce). The function
%% never crashes.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_produce_map(term()) -> boolean().

is_produce_map(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_produce_map(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid mode.
%%
%% A valid mode is a map where all keys are atoms (places) and
%% all values are lists (of tokens). The function never crashes.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_mode(term()) -> boolean().

is_mode(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, V, _) when is_atom(K), is_list(V) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_mode(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid binding.
%%
%% A valid binding is a map where all keys are atoms (variables)
%% and all values are any term (the bound values). The function
%% never crashes.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_binding(term()) -> boolean().

is_binding(Term) when is_map(Term) ->
    try
        maps:fold(fun
            (K, _, _) when is_atom(K) -> ok;
            (_, _, _) -> throw(error)
        end, ok, Term),
        true
    catch
        throw:_ -> false;
        error:_ -> false;
        _:_ -> false
    end;
is_binding(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid cmode.
%%
%% A valid cmode is a 2-tuple where the first element is a binding
%% and the second element is a mode. The function never crashes.
%%
%% @end
%%--------------------------------------------------------------------
-spec is_cmode(term()) -> boolean().

is_cmode({Binding, Mode}) when is_tuple(Binding), tuple_size(Binding) =:= 0;
                                is_map(Binding);
                                is_list(Binding) ->
    %% For binding: accept empty tuple as map sentinel, or actual map
    %% The gen_pnet library uses {} as an empty binding representation
    IsBindingValid = case Binding of
        {} -> true;
        _ when is_map(Binding) -> is_binding(Binding);
        _ -> false
    end,
    IsModeValid = is_mode(Mode),
    IsBindingValid andalso IsModeValid;
is_cmode({Binding, Mode}) ->
    %% Try as map binding
    is_binding(Binding) andalso is_mode(Mode);
is_cmode(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid move.
%%
%% A valid move is a map with required keys: trsn (atom), mode (valid mode or cmode),
%% and produce (valid produce_map). The function never crashes.
%%
%% ```erlang
%% > Move = #{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}}.
%% > pnet_types:is_move(Move).
%% true
%% > pnet_types:is_move(#{trsn => t1}).
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_move(term()) -> boolean().

is_move(Term) when is_map(Term) ->
    case Term of
        #{trsn := Trsn, mode := Mode, produce := Produce} ->
            is_trsn(Trsn) andalso
            is_mode(Mode) andalso
            is_produce_map(Produce);
        _ ->
            false
    end;
is_move(_) ->
    false.

%%--------------------------------------------------------------------
%% @doc Checks if a term is a valid receipt.
%%
%% A valid receipt is a map with required keys: before_hash (binary),
%% after_hash (binary), move (valid move), and ts (integer).
%% The function never crashes.
%%
%% ```erlang
%% > Move = #{trsn => t1, mode => #{p1 => [a]}, produce => #{p2 => [b]}}.
%% > R = #{before_hash => <<1>>, after_hash => <<2>>, move => Move, ts => 123}.
%% > pnet_types:is_receipt(R).
%% true
%% > pnet_types:is_receipt(R#{ts => <<"nope">>}).
%% false
%% ```
%% @end
%%--------------------------------------------------------------------
-spec is_receipt(term()) -> boolean().

is_receipt(Term) when is_map(Term) ->
    case Term of
        #{before_hash := BeforeHash, after_hash := AfterHash, move := Move, ts := Ts} ->
            is_binary(BeforeHash) andalso
            is_binary(AfterHash) andalso
            is_move(Move) andalso
            is_integer(Ts);
        _ ->
            false
    end;
is_receipt(_) ->
    false.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.
-endif.
