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

-module(wf_scope).

-moduledoc """
Scope boundary mapping (parent places <-> child places).

enter/3 renames parent keys to child keys using BindingTable[ScopeId].
leave/3 renames child keys back to parent keys.
bindings/2 returns mapping or {error, unknown_scope}.

```erlang
> BT = #{scope1 => #{parent_in => child_in, parent_out => child_out}}.
_

> wf_scope:bindings(BT, scope1).
#{parent_in => child_in, parent_out => child_out}
> wf_scope:bindings(BT, missing).
{error, unknown_scope}

> wf_scope:enter(BT, scope1, #{parent_in => [a]}).
#{child_in => [a]}

> wf_scope:leave(BT, scope1, #{child_out => [r]}).
#{parent_out => [r]}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% Scope operations
-export([enter/3, leave/3, bindings/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Unique identifier for a scope/subflow.
%%
%% Can be any Erlang term - typically an atom or binary.
%%--------------------------------------------------------------------
-type scope_id() :: term().

%%--------------------------------------------------------------------
%% @doc A place in the Petri net.
%%
%% Places are atoms representing locations where tokens reside.
%%--------------------------------------------------------------------
-type place() :: atom().

%%--------------------------------------------------------------------
%% @doc A token in the Petri net.
%%
%% Tokens can be any Erlang term.
%%--------------------------------------------------------------------
-type token() :: term().

%%--------------------------------------------------------------------
%% @doc A marking maps places to their token multisets.
%%
%% Used to represent token collections when entering/leaving scopes.
%%--------------------------------------------------------------------
-type marking() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A produce map specifies tokens to be produced.
%%
%% The standard gen_pnet format for token injection.
%%--------------------------------------------------------------------
-type produce_map() :: #{place() => [token()]}.

%%--------------------------------------------------------------------
%% @doc A binding table maps scopes to their place mappings.
%%
%% Each scope maps parent places to child places for translation.
%%--------------------------------------------------------------------
-type binding_table() :: #{scope_id() => #{place() => place()}}.

%%--------------------------------------------------------------------
%% @doc Input can be a marking, produce map, or list of places.
%%
%% Accepts flexible input formats for convenience.
%%--------------------------------------------------------------------
-type input() :: marking() | produce_map() | [place()].

%% Export types
-export_type([scope_id/0, binding_table/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Translates parent-level tokens to child-level tokens.
%%
%% When entering a scope (subflow), parent tokens must be translated
%% to the child's place namespace using the binding table. Each parent
%% place is mapped to its corresponding child place.
%%
%% @param BindingTable The binding table containing scope mappings
%% @param ScopeId The ID of the scope being entered
%% @param ParentDeltaOrMarking Tokens to translate (marking or produce_map)
%% @return Produce map with tokens mapped to child places
%%
%% @end
%%--------------------------------------------------------------------
-spec enter(BindingTable :: binding_table(),
           ScopeId :: scope_id(),
           ParentDeltaOrMarking :: input()) ->
          produce_map().

enter(BindingTable, ScopeId, ParentDeltaOrMarking)
  when is_map(BindingTable), is_map(ParentDeltaOrMarking) ->

    case maps:get(ScopeId, BindingTable, undefined) of
        undefined ->
            %% No binding for this scope - return tokens unchanged
            normalize_to_produce_map(ParentDeltaOrMarking);
        Mapping when is_map(Mapping) ->
            %% Translate each place from parent to child namespace
            translate_places(ParentDeltaOrMarking, Mapping)
    end;
enter(_BindingTable, _ScopeId, ParentDeltaOrMarking) when is_list(ParentDeltaOrMarking) ->
    %% List of places - convert to empty produce map
    maps:from_list([{P, []} || P <- ParentDeltaOrMarking]).

%%--------------------------------------------------------------------
%% @doc Translates child-level tokens back to parent-level tokens.
%%
%% When leaving a scope (subflow), child tokens must be translated
%% back to the parent's place namespace. This is the inverse of enter/3,
%% using the reverse of the binding table mapping.
%%
%% @param BindingTable The binding table containing scope mappings
%% @param ScopeId The ID of the scope being left
%% @param ChildDeltaOrMarking Tokens to translate (marking or produce_map)
%% @return Produce map with tokens mapped to parent places
%%
%% @end
%%--------------------------------------------------------------------
-spec leave(BindingTable :: binding_table(),
           ScopeId :: scope_id(),
           ChildDeltaOrMarking :: input()) ->
          produce_map().

leave(BindingTable, ScopeId, ChildDeltaOrMarking)
  when is_map(BindingTable), is_map(ChildDeltaOrMarking) ->

    case maps:get(ScopeId, BindingTable, undefined) of
        undefined ->
            %% No binding for this scope - return tokens unchanged
            normalize_to_produce_map(ChildDeltaOrMarking);
        Mapping when is_map(Mapping) ->
            %% Create reverse mapping (child -> parent)
            ReverseMapping = maps:from_list(
                [{ChildPlace, ParentPlace} ||
                 {ParentPlace, ChildPlace} <- maps:to_list(Mapping)]
            ),
            %% Translate each place from child to parent namespace
            translate_places(ChildDeltaOrMarking, ReverseMapping)
    end;
leave(_BindingTable, _ScopeId, ChildDeltaOrMarking) when is_list(ChildDeltaOrMarking) ->
    %% List of places - convert to empty produce map
    maps:from_list([{P, []} || P <- ChildDeltaOrMarking]).

%%--------------------------------------------------------------------
%% @doc Gets the place mapping for a specific scope.
%%
%% Returns the mapping from parent places to child places for the
%% given scope ID.
%%
%% @param BindingTable The binding table containing scope mappings
%% @param ScopeId The ID of the scope to query
%% @return Map of parent places to child places, or {error, unknown_scope}
%%
%% @end
%%--------------------------------------------------------------------
-spec bindings(BindingTable :: binding_table(),
              ScopeId :: scope_id()) ->
          #{place() => place()} | {error, unknown_scope}.

bindings(BindingTable, ScopeId) when is_map(BindingTable) ->
    case maps:get(ScopeId, BindingTable, undefined) of
        undefined -> {error, unknown_scope};
        Mapping when is_map(Mapping) -> Mapping
    end;
bindings(_BindingTable, _ScopeId) ->
    {error, unknown_scope}.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Normalizes various input formats to a produce map.
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_to_produce_map(input()) -> produce_map().

normalize_to_produce_map(Input) when is_map(Input) ->
    %% Already a map - assume it's a produce_map
    Input;
normalize_to_produce_map(Input) when is_list(Input) ->
    %% List of places - create produce map with empty tokens
    maps:from_list([{P, []} || P <- Input]).

%%--------------------------------------------------------------------
%% @private
%% @doc Translates places in a marking/produce_map using a mapping.
%%
%% Each place key in the input is looked up in the mapping and
%% replaced with its mapped value. Places not in the mapping
%% are preserved unchanged.
%%
%% @end
%%--------------------------------------------------------------------
-spec translate_places(marking() | produce_map(),
                      #{place() => place()}) ->
          produce_map().

translate_places(Input, Mapping) when is_map(Input), is_map(Mapping) ->
    maps:fold(fun
        (Place, Tokens, Acc) when is_atom(Place) ->
            NewPlace = maps:get(Place, Mapping, Place),
            Acc#{NewPlace => Tokens};
        (Place, Tokens, Acc) ->
            %% Preserve non-atom places unchanged
            Acc#{Place => Tokens}
    end, #{}, Input).

%%--------------------------------------------------------------------
%% EUnit Tests
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc EUnit test runner for the module.
%% Tests the doctest examples from the moduledoc.
%%--------------------------------------------------------------------
doctest_test() ->
    %% Setup binding table
    BT = #{s1 => #{pin => cin, pout => cout}},

    %% Test bindings/2
    ?assertEqual(#{pin => cin, pout => cout}, bindings(BT, s1)),
    ?assertEqual({error, unknown_scope}, bindings(BT, missing)),

    %% Test enter/3
    Parent = #{pin => [a, b]},
    ?assertEqual(#{cin => [a, b]}, enter(BT, s1, Parent)),

    %% Test leave/3
    Child = #{cout => [ok]},
    ?assertEqual(#{pout => [ok]}, leave(BT, s1, Child)),

    %% Test unknown scope => identity
    ?assertEqual(#{p1 => [x]}, enter(BT, missing, #{p1 => [x]})),

    %% Additional coverage tests
    ?assertEqual(#{}, enter(BT, s1, #{})),
    ?assertEqual(#{}, leave(BT, s1, #{})),

    %% Test list input (edge case)
    ?assertEqual(#{p1 => []}, enter(BT, missing, [p1])),
    ?assertEqual(#{p2 => []}, leave(BT, missing, [p2])),

    ok.

%%--------------------------------------------------------------------
%% @doc Test that enter/3 properly translates all keys in mapping.
%%--------------------------------------------------------------------
enter_full_mapping_test() ->
    BT = #{scope => #{a => x, b => y, c => z}},
    Input = #{a => [1], b => [2], d => [4]},
    ?assertEqual(#{x => [1], y => [2], d => [4]}, enter(BT, scope, Input)).

%%--------------------------------------------------------------------
%% @doc Test that leave/3 properly reverses the mapping.
%%--------------------------------------------------------------------
leave_full_mapping_test() ->
    BT = #{scope => #{a => x, b => y}},
    Input = #{x => [1], y => [2], z => [3]},
    ?assertEqual(#{a => [1], b => [2], z => [3]}, leave(BT, scope, Input)).

%%--------------------------------------------------------------------
%% @doc Test empty binding table.
%%--------------------------------------------------------------------
empty_binding_table_test() ->
    BT = #{},
    Input = #{a => [1]},
    ?assertEqual(Input, enter(BT, any, Input)),
    ?assertEqual(Input, leave(BT, any, Input)).

-endif.
