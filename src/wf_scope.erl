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
%% @doc Workflow Scope Boundary Mapping Module
%%
%% This module provides boundary mapping helpers for hierarchical
%% workflows in the gen_pnet Petri net framework. Scopes represent
%% nested subflows or subprocess boundaries where place names may
%% differ between parent and child contexts.
%%
%% <h3>Key Concepts</h3>
%%
%% A <em>scope</em> represents a boundary in a hierarchical workflow, such
%% as a subflow or subprocess. Tokens entering or leaving a scope must be
%% translated between the parent's place namespace and the child's
%% place namespace.
%%
%% The <em>binding table</em> maps scope IDs to place mappings, defining
%% how tokens should be translated when crossing scope boundaries.
%%
%% <h3>Usage Example</h3>
%% <pre><code>
%% %% Define a binding table for a subflow
%% BindingTable => #{
%%   my_subflow => #{
%%     parent_input => child_input,
%%     parent_output => child_output
%%   }
%% },
%%
%% %% Enter scope: translate parent tokens to child places
%% ParentTokens = #{parent_input => [data1, data2]},
%% ChildProduceMap = wf_scope:enter(BindingTable, my_subflow, ParentTokens),
%% %% => #{child_input => [data1, data2]}
%%
%% %% Leave scope: translate child results back to parent places
%% ChildTokens = #{child_output => [result]},
%% ParentProduceMap = wf_scope:leave(BindingTable, my_subflow, ChildTokens),
%% %% => #{parent_output => [result]}
%% </code></pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_scope).

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
        (_Place, _Tokens, _Acc) ->
            %% Skip non-atom places
            error
    end, #{}, Input).
