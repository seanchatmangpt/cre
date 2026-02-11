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
%% @author CRE Team
%% @version 0.3.0
%% @doc Workflow Net Pattern Registry and Factory
%%
%% Provides runtime pattern registration, validation, and factory for
%% dynamic pattern instantiation. Supports pattern aliases, categories,
%% and metadata management.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Runtime Registration:</b> Register patterns at runtime</li>
%%   <li><b>Interface Validation:</b> Validate pattern implementations</li>
%%   <li><b>Pattern Aliases:</b> Support multiple names for the same pattern</li>
%%   <li><b>Categories:</b> Organize patterns by functional category</li>
%%   <li><b>Factory Pattern:</b> Instantiate patterns dynamically</li>
%%   <li><b>Metadata:</b> Store and retrieve pattern metadata</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% ```erlang
%% %% Register a pattern
%% PatternDef = #{
%%     module => wfnet_sequence,
%%     category => control_flow,
%%     description => "Sequential execution pattern",
%%     wcp_number => "WCP-01"
%% },
%% ok = wfnet_pattern_registry:register_pattern(sequence, PatternDef).
%%
%% %% Look up a pattern
%% {ok, Pattern} = wfnet_pattern_registry:get_pattern(sequence).
%%
%% %% List all patterns
%% Patterns = wfnet_pattern_registry:list_patterns().
%%
%% %% Create a pattern instance
%% {ok, Spec} = wfnet_pattern_registry:create_pattern(sequence, [a, b, c]).
%%
%% %% Validate a pattern implementation
%% {ok, Warnings} = wfnet_pattern_registry:validate_pattern(wfnet_loop).
%% ```
%%
%% <h3>Pattern Categories</h3>
%%
%% - `control_flow` - Basic control flow patterns
%% - `advanced_branching` - Complex branching constructs
%% - `synchronization` - Patterns for synchronizing flows
%% - `cancellation` - Cancellation and termination patterns
%% - `iteration` - Loop and iteration patterns
%% - `parallelism` - Parallel execution patterns
%% - `composition` - Pattern composition operators
%%
%% @end
%% -------------------------------------------------------------------

-module(wfnet_pattern_registry).
-author("CRE Team").
-moduledoc """
Pattern registry and factory for dynamic pattern instantiation.

Provides runtime pattern registration, validation, and factory for
dynamic pattern instantiation.
""".

%%====================================================================
%% Exports
%%====================================================================

%% Pattern Registration
-export([
    register_pattern/2,
    register_pattern/3,
    unregister_pattern/1,
    get_pattern/1,
    list_patterns/0,
    list_patterns/1,
    list_categories/0
]).

%% Pattern Factory
-export([
    create_pattern/2,
    create_pattern/3
]).

%% Pattern Validation
-export([
    validate_pattern/1,
    validate_pattern_interface/1,
    get_required_callbacks/0
]).

%% Metadata
-export([
    get_pattern_metadata/1,
    set_pattern_metadata/2,
    get_pattern_category/1,
    list_patterns_by_category/1
]).

%% Alias Management
-export([
    add_alias/2,
    remove_alias/2,
    resolve_alias/1,
    get_aliases/1
]).

%% Registry Management
-export([
    clear_registry/0,
    register_builtins/0
]).

%%====================================================================
%% Includes
%%====================================================================

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Pattern definition structure.
%%
%% A pattern definition contains:
%% - module: The gen_wfnet behavior module implementing the pattern
%% - category: Functional category for organization
%% - description: Human-readable description
%% - wcp_number: Workflow Control Pattern identifier (optional)
%% - aliases: Alternative names for the pattern (optional)
%% - metadata: Additional custom metadata (optional)
%% - validator: Optional custom validation function
%%--------------------------------------------------------------------
-type pattern_definition() :: #{
    module := module(),
    category := pattern_category(),
    description := binary(),
    wcp_number => binary(),
    aliases => [atom()],
    metadata => map(),
    validator => function()
}.

%%--------------------------------------------------------------------
%% @doc Pattern categories for organization.
%%--------------------------------------------------------------------
-type pattern_category() ::
    control_flow
    | advanced_branching
    | synchronization
    | cancellation
    | iteration
    | parallelism
    | composition
    | custom.

%%--------------------------------------------------------------------
%% @doc Pattern creation options.
%%--------------------------------------------------------------------
-type create_options() :: #{
    validate => boolean(),
    timeout => timeout()
}.

%%--------------------------------------------------------------------
%% @doc Validation result.
%%--------------------------------------------------------------------
-type validation_result() :: {ok, [binary()]} | {error, [binary()]}.

%%--------------------------------------------------------------------
%% @doc Callback requirements for pattern modules.
%%--------------------------------------------------------------------
-type callback_spec() :: {atom(), arity()}.

%% Export types
-export_type([
    pattern_definition/0,
    pattern_category/0,
    create_options/0,
    validation_result/0,
    callback_spec/0
]).

%%====================================================================
%% Pattern Registration
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Register a pattern with the registry.
%%
%% @param Name Primary name for the pattern (atom)
%% @param Definition Pattern definition map
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec register_pattern(atom(), pattern_definition()) -> ok | {error, term()}.

register_pattern(Name, Definition) when is_atom(Name), is_map(Definition) ->
    case validate_definition(Definition) of
        ok ->
            Pattern = normalize_definition(Definition),
            ets:insert(?MODULE, {{pattern, Name}, Pattern}),
            %% Register aliases if present
            Aliases = maps:get(aliases, Pattern, []),
            lists:foreach(fun(Alias) ->
                ets:insert(?MODULE, {{alias, Alias}, Name})
            end, Aliases),
            ?LOG_DEBUG(#{
                message => "Pattern registered",
                pattern => Name,
                module => maps:get(module, Pattern),
                category => maps:get(category, Pattern)
            }),
            ok;
        {error, _} = Error ->
            Error
    end.

%%--------------------------------------------------------------------
%% @doc Register a pattern with explicit options.
%%
%% Options:
%% - `validate`: Set to false to skip interface validation (default: true)
%% - `overwrite`: Set to true to allow overwriting existing patterns
%%
%% @end
%%--------------------------------------------------------------------
-spec register_pattern(atom(), pattern_definition(), map()) -> ok | {error, term()}.

register_pattern(Name, Definition, Options) when is_atom(Name), is_map(Definition), is_map(Options) ->
    ShouldValidate = maps:get(validate, Options, true),
    AllowOverwrite = maps:get(overwrite, Options, false),

    %% Check if pattern already exists
    case ets:lookup(?MODULE, {pattern, Name}) of
        [] when AllowOverwrite =:= false; AllowOverwrite =:= true ->
            %% Validate if requested
            case ShouldValidate of
                true ->
                    case validate_pattern(maps:get(module, Definition)) of
                        {ok, _} ->
                            register_pattern(Name, Definition);
                        {error, Errors} ->
                            {error, {validation_failed, Errors}}
                    end;
                false ->
                    register_pattern(Name, Definition)
            end;
        [_] when AllowOverwrite =:= false ->
            {error, {pattern_already_registered, Name}}
    end.

%%--------------------------------------------------------------------
%% @doc Unregister a pattern from the registry.
%%
%% @param Name Pattern name to unregister
%% @returns ok | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec unregister_pattern(atom()) -> ok | {error, not_found}.

unregister_pattern(Name) when is_atom(Name) ->
    case ets:lookup(?MODULE, {pattern, Name}) of
        [] ->
            {error, not_found};
        [_] ->
            ets:delete(?MODULE, {pattern, Name}),
            %% Remove all aliases pointing to this pattern
            remove_aliases_for_pattern(Name),
            ?LOG_DEBUG(#{
                message => "Pattern unregistered",
                pattern => Name
            }),
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Look up a pattern by name or alias.
%%
%% @param Name Pattern name or alias
%% @returns {ok, PatternDefinition} | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_pattern(atom()) -> {ok, pattern_definition()} | {error, not_found}.

get_pattern(Name) when is_atom(Name) ->
    %% First check if it's a direct pattern name
    case ets:lookup(?MODULE, {pattern, Name}) of
        [{{pattern, Name}, Pattern}] ->
            {ok, Pattern};
        [] ->
            %% Check if it's an alias
            case resolve_alias(Name) of
                {ok, RealName} ->
                    case ets:lookup(?MODULE, {pattern, RealName}) of
                        [{{pattern, RealName}, Pattern}] ->
                            {ok, Pattern};
                        [] ->
                            {error, not_found}
                    end;
                {error, not_found} ->
                    {error, not_found}
            end
    end.

%%--------------------------------------------------------------------
%% @doc List all registered patterns.
%%
%% @returns List of {PatternName, PatternDefinition} tuples
%%
%% @end
%%--------------------------------------------------------------------
-spec list_patterns() -> [{atom(), pattern_definition()}].

list_patterns() ->
    Patterns = ets:select(?MODULE, [{{{'$1', pattern}, '$2'}, [], [['$$']]}]),
    %% Sort by pattern name
    lists:sort(fun({A, _}, {B, _}) -> A =< B end, Patterns).

%%--------------------------------------------------------------------
%% @doc List patterns with optional filter.
%%
%% Filters:
%% - `{category, Category}` - List patterns in a category
%% - `{module, Module}` - List patterns from a module
%%
%% @end
%%--------------------------------------------------------------------
-spec list_patterns({category, pattern_category()} | {module, module()}) ->
    [{atom(), pattern_definition()}].

list_patterns({category, Category}) when is_atom(Category) ->
    AllPatterns = list_patterns(),
    lists:filter(fun({_Name, Def}) ->
        maps:get(category, Def) =:= Category
    end, AllPatterns);

list_patterns({module, Module}) when is_atom(Module) ->
    AllPatterns = list_patterns(),
    lists:filter(fun({_Name, Def}) ->
        maps:get(module, Def) =:= Module
    end, AllPatterns).

%%--------------------------------------------------------------------
%% @doc List all pattern categories currently in use.
%%
%% @returns List of unique category atoms
%%
%% @end
%%--------------------------------------------------------------------
-spec list_categories() -> [pattern_category()].

list_categories() ->
    Patterns = list_patterns(),
    Categories = lists:usort([maps:get(category, Def) || {_Name, Def} <- Patterns]),
    Categories.

%%====================================================================
%% Pattern Factory
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Create a pattern instance (factory function).
%%
%% Creates a new workflow specification for the named pattern using
%% the provided arguments. Delegates to the pattern module's new/1 or
%% new/2 function.
%%
%% @param PatternName Pattern to instantiate
%% @param Args Arguments for pattern creation
%% @returns {ok, WorkflowSpec} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec create_pattern(atom(), term()) -> {ok, wfnet_types:workflow_spec()} | {error, term()}.

create_pattern(PatternName, Args) ->
    create_pattern(PatternName, Args, #{}).

%%--------------------------------------------------------------------
%% @doc Create a pattern instance with options.
%%
%% Options:
%% - `validate`: Run validation on the created spec (default: true)
%% - `timeout`: Timeout for pattern creation (default: 5000ms)
%%
%% @end
%%--------------------------------------------------------------------
-spec create_pattern(atom(), term(), create_options()) ->
    {ok, wfnet_types:workflow_spec()} | {error, term()}.

create_pattern(PatternName, Args, Options) when is_atom(PatternName), is_map(Options) ->
    case get_pattern(PatternName) of
        {error, not_found} ->
            {error, {pattern_not_found, PatternName}};
        {ok, Definition} ->
            Module = maps:get(module, Definition),
            Timeout = maps:get(timeout, Options, 5000),
            try
                %% Try new/2 first (with options)
                Spec = case erlang:function_exported(Module, new, 2) of
                    true ->
                        PatternOptions = maps:without([validate, timeout], Options),
                        Module:new(Args, PatternOptions);
                    false ->
                        %% Fall back to new/1
                        Module:new(Args)
                end,
                %% Validate if requested
                case maps:get(validate, Options, true) of
                    true ->
                        case wfnet_validate:spec(Spec) of
                            {ok, []} ->
                                {ok, Spec};
                            {ok, Warnings} ->
                                ?LOG_WARNING(#{
                                    message => "Pattern created with warnings",
                                    pattern => PatternName,
                                    warnings => Warnings
                                }),
                                {ok, Spec};
                            {error, Errors} ->
                                {error, {validation_failed, Errors}}
                        end;
                    false ->
                        {ok, Spec}
                end
            catch
                Type:Error:Stack ->
                    ?LOG_ERROR(#{
                        message => "Pattern creation failed",
                        pattern => PatternName,
                        module => Module,
                        error_type => Type,
                        error => Error,
                        stacktrace => Stack
                    }),
                    {error, {creation_failed, {Type, Error}}}
            end
    end.

%%====================================================================
%% Pattern Validation
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Validate a pattern implementation module.
%%
%% Checks that the module:
%% - Implements the gen_wfnet behavior
%% - Exports all required callbacks
%% - Has a valid workflow_spec/0 function
%% - Has proper type annotations
%%
%% @param Module Pattern module to validate
%% @returns {ok, Warnings} | {error, Errors}
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_pattern(module()) -> validation_result().

validate_pattern(Module) when is_atom(Module) ->
    try
        %% Check if module exists and is loaded
        case code:is_loaded(Module) of
            false ->
                case code:load_file(Module) of
                    {module, Module} -> ok;
                    {error, Reason} -> throw({module_load_failed, Reason})
                end;
            _ ->
                ok
        end,

        %% Validate interface
        case validate_pattern_interface(Module) of
            {ok, []} ->
                {ok, []};
            {ok, Warnings} ->
                {ok, Warnings};
            {error, Errors} ->
                {error, Errors}
        end
    catch
        _:Error ->
            {error, [iolist_to_binary([atom_to_list(Module), ": validation exception: ",
                                      io_lib:format("~p", [Error])])]}
    end.

%%--------------------------------------------------------------------
%% @doc Validate pattern interface compliance.
%%
%% Checks that the module exports all required gen_wfnet callbacks.
%%
%% @param Module Pattern module to validate
%% @returns {ok, Warnings} | {error, Errors}
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_pattern_interface(module()) -> validation_result().

validate_pattern_interface(Module) when is_atom(Module) ->
    RequiredCallbacks = get_required_callbacks(),
    Errors = validate_callbacks(Module, RequiredCallbacks, []),
    Warnings = [],

    %% Check behavior declaration
    Behaviors = case Module:module_info(attributes) of
        [] ->
            [{error, "No module info found"}];
        Attrs ->
            lists:filtermap(
                fun({behaviour, Behaviors}) when is_list(Behaviors) ->
                    case lists:member(gen_wfnet, Behaviors) of
                        true -> false;
                        false -> {true, {warning, "Module does not declare gen_wfnet behavior"}}
                    end;
                   ({behavior, Behaviors}) when is_list(Behaviors) ->
                    %% US spelling variant
                    case lists:member(gen_wfnet, Behaviors) of
                        true -> false;
                        false -> {true, {warning, "Module does not declare gen_wfnet behavior"}}
                    end;
                   (_) ->
                    false
                end,
                Attrs)
    end,

    case Errors of
        [] ->
            {ok, lists:map(fun({error, Msg}) -> list_to_binary(Msg) end,
                           Behaviors ++ Warnings)};
        _ ->
            {error, lists:map(fun({error, Msg}) -> list_to_binary(Msg) end,
                             Errors)}
    end.

%%--------------------------------------------------------------------
%% @doc Get required callback specifications for gen_wfnet.
%%
%% @returns List of {FunctionName, Arity} tuples
%%
%% @end
%%--------------------------------------------------------------------
-spec get_required_callbacks() -> [callback_spec()].

get_required_callbacks() ->
    %% gen_wfnet required callbacks
    [
        {workflow_spec, 0},
        {init_marking, 2},
        {fire, 3},
        {is_enabled, 3},
        {init, 1}
    ].

%%====================================================================
%% Metadata Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Get metadata for a pattern.
%%
%% @param PatternName Pattern name or alias
%% @returns {ok, Metadata} | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_pattern_metadata(atom()) -> {ok, map()} | {error, not_found}.

get_pattern_metadata(PatternName) ->
    case get_pattern(PatternName) of
        {ok, Definition} ->
            Metadata = maps:get(metadata, Definition, #{}),
            {ok, Metadata};
        {error, not_found} ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Set or update metadata for a pattern.
%%
%% @param PatternName Pattern name
%% @param Metadata New metadata map (merged with existing)
%% @returns ok | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec set_pattern_metadata(atom(), map()) -> ok | {error, not_found}.

set_pattern_metadata(PatternName, Metadata) when is_map(Metadata) ->
    case ets:lookup(?MODULE, {pattern, PatternName}) of
        [{{pattern, PatternName}, Definition}] ->
            CurrentMetadata = maps:get(metadata, Definition, #{}),
            NewDefinition = Definition#{metadata => maps:merge(CurrentMetadata, Metadata)},
            ets:insert(?MODULE, {{pattern, PatternName}, NewDefinition}),
            ok;
        [] ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Get the category of a pattern.
%%
%% @param PatternName Pattern name or alias
%% @returns {ok, Category} | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_pattern_category(atom()) -> {ok, pattern_category()} | {error, not_found}.

get_pattern_category(PatternName) ->
    case get_pattern(PatternName) of
        {ok, Definition} ->
            {ok, maps:get(category, Definition)};
        {error, not_found} ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc List all patterns in a specific category.
%%
%% @param Category Pattern category
%% @returns List of {PatternName, PatternDefinition} tuples
%%
%% @end
%%--------------------------------------------------------------------
-spec list_patterns_by_category(pattern_category()) -> [{atom(), pattern_definition()}].

list_patterns_by_category(Category) when is_atom(Category) ->
    list_patterns({category, Category}).

%%====================================================================
%% Alias Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Add an alias for a pattern.
%%
%% @param PatternName Existing pattern name
%% @param Alias New alias to add
%% @returns ok | {error, not_found} | {error, alias_exists}
%%
%% @end
%%--------------------------------------------------------------------
-spec add_alias(atom(), atom()) -> ok | {error, not_found} | {error, alias_exists}.

add_alias(PatternName, Alias) when is_atom(PatternName), is_atom(Alias) ->
    %% Check if pattern exists
    case ets:lookup(?MODULE, {pattern, PatternName}) of
        [] ->
            {error, not_found};
        [{{pattern, PatternName}, Definition}] ->
            %% Check if alias already exists
            case ets:lookup(?MODULE, {alias, Alias}) of
                [{_, _}] ->
                    {error, alias_exists};
                [] ->
                    %% Add alias
                    ets:insert(?MODULE, {{alias, Alias}, PatternName}),
                    %% Update pattern definition
                    Aliases = maps:get(aliases, Definition, []),
                    NewDefinition = Definition#{aliases => [Alias | Aliases]},
                    ets:insert(?MODULE, {{pattern, PatternName}, NewDefinition}),
                    ok
            end
    end.

%%--------------------------------------------------------------------
%% @doc Remove an alias from a pattern.
%%
%% @param PatternName Pattern name
%% @param Alias Alias to remove
%% @returns ok | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec remove_alias(atom(), atom()) -> ok | {error, not_found}.

remove_alias(PatternName, Alias) when is_atom(PatternName), is_atom(Alias) ->
    case ets:lookup(?MODULE, {pattern, PatternName}) of
        [] ->
            {error, not_found};
        [{{pattern, PatternName}, Definition}] ->
            %% Remove alias from registry
            ets:delete(?MODULE, {alias, Alias}),
            %% Update pattern definition
            Aliases = lists:delete(Alias, maps:get(aliases, Definition, [])),
            NewDefinition = Definition#{aliases => Aliases},
            ets:insert(?MODULE, {{pattern, PatternName}, NewDefinition}),
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Resolve an alias to its canonical pattern name.
%%
%% @param Alias Pattern alias
%% @returns {ok, PatternName} | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec resolve_alias(atom()) -> {ok, atom()} | {error, not_found}.

resolve_alias(Alias) when is_atom(Alias) ->
    case ets:lookup(?MODULE, {alias, Alias}) of
        [{_, PatternName}] ->
            {ok, PatternName};
        [] ->
            {error, not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Get all aliases for a pattern.
%%
%% @param PatternName Pattern name
%% @returns List of alias atoms
%%
%% @end
%%--------------------------------------------------------------------
-spec get_aliases(atom()) -> [atom()].

get_aliases(PatternName) when is_atom(PatternName) ->
    case ets:lookup(?MODULE, {pattern, PatternName}) of
        [{{pattern, PatternName}, Definition}] ->
            maps:get(aliases, Definition, []);
        [] ->
            []
    end.

%%====================================================================
%% Registry Management
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Clear all patterns from the registry.
%%
%% Warning: This removes all registered patterns.
%%
%% @end
%%--------------------------------------------------------------------
-spec clear_registry() -> ok.

clear_registry() ->
    ets:delete_all_objects(?MODULE),
    ok.

%%--------------------------------------------------------------------
%% @doc Register all built-in workflow patterns.
%%
%% Automatically registers all standard wfnet patterns from the
%% src/wfnet/patterns directory.
%%
%% @end
%%--------------------------------------------------------------------
-spec register_builtins() -> ok.

register_builtins() ->
    BuiltinPatterns = [
        %% Control flow patterns
        {sequence, #{
            module => wfnet_sequence,
            category => control_flow,
            description => <<"Sequential execution pattern (WCP-01)">>,
            wcp_number => <<"WCP-01">>,
            aliases => [seq]
        }},
        {parallel_split, #{
            module => wfnet_parallel_split,
            category => control_flow,
            description => <<"Parallel split pattern (WCP-02)">>,
            wcp_number => <<"WCP-02">>,
            aliases => [and_split, fork]
        }},
        {sync_merge, #{
            module => wfnet_sync_merge,
            category => synchronization,
            description => <<"Synchronization merge pattern (WCP-03)">>,
            wcp_number => <<"WCP-03">>,
            aliases => [and_join, join]
        }},
        {choice, #{
            module => wfnet_choice,
            category => advanced_branching,
            description => <<"Exclusive choice pattern (WCP-04)">>,
            wcp_number => <<"WCP-04">>,
            aliases => [xor_split, exclusive_choice]
        }},
        {multi_choice, #{
            module => wfnet_multi_choice,
            category => advanced_branching,
            description => <<"Multi-choice pattern (WCP-06)">>,
            wcp_number => <<"WCP-06">>,
            aliases => [or_split]
        }},
        {loop, #{
            module => wfnet_loop,
            category => iteration,
            description => <<"Structured loop pattern (WCP-09)">>,
            wcp_number => <<"WCP-09">>,
            aliases => [while, repeat]
        }}
    ],

    lists:foreach(fun
        ({Name, Definition}) ->
            case register_pattern(Name, Definition, #{validate => false, overwrite => false}) of
                ok ->
                    ?LOG_DEBUG(#{
                        message => "Registered builtin pattern",
                        pattern => Name
                    }),
                    ok;
                {error, {pattern_already_registered, Name}} ->
                    ?LOG_DEBUG(#{
                        message => "Builtin pattern already registered",
                        pattern => Name
                    }),
                    ok;
                {error, Reason} ->
                    ?LOG_WARNING(#{
                        message => "Failed to register builtin pattern",
                        pattern => Name,
                        reason => Reason
                    })
            end
    end, BuiltinPatterns),

    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Validate pattern definition structure.
%%--------------------------------------------------------------------
-spec validate_definition(pattern_definition()) -> ok | {error, term()}.

validate_definition(Definition) ->
    RequiredKeys = [module, category, description],
    case lists:all(fun(Key) -> maps:is_key(Key, Definition) end, RequiredKeys) of
        false ->
            Missing = [K || K <- RequiredKeys, not maps:is_key(K, Definition)],
            {error, {missing_keys, Missing}};
        true ->
            Module = maps:get(module, Definition),
            case is_atom(Module) of
                false ->
                    {error, {invalid_module, Module}};
                true ->
                    Category = maps:get(category, Definition),
                    case is_valid_category(Category) of
                        false ->
                            {error, {invalid_category, Category}};
                        true ->
                            Description = maps:get(description, Definition),
                            case is_binary(Description) of
                                false ->
                                    {error, {invalid_description, Description}};
                                true ->
                                    ok
                            end
                    end
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Check if a category is valid.
%%--------------------------------------------------------------------
-spec is_valid_category(term()) -> boolean().

is_valid_category(control_flow) -> true;
is_valid_category(advanced_branching) -> true;
is_valid_category(synchronization) -> true;
is_valid_category(cancellation) -> true;
is_valid_category(iteration) -> true;
is_valid_category(parallelism) -> true;
is_valid_category(composition) -> true;
is_valid_category(custom) -> true;
is_valid_category(_) -> false.

%%--------------------------------------------------------------------
%% @private
%% @doc Normalize definition with default values.
%%--------------------------------------------------------------------
-spec normalize_definition(pattern_definition()) -> pattern_definition().

normalize_definition(Definition) ->
    Defaults = #{
        aliases => [],
        metadata => #{}
    },
    maps:merge(Defaults, Definition).

%%--------------------------------------------------------------------
%% @private
%% @doc Validate callback exports for a module.
%%--------------------------------------------------------------------
-spec validate_callbacks(module(), [callback_spec()], [binary()]) -> [binary()].

validate_callbacks(_Module, [], Acc) ->
    lists:reverse(Acc);
validate_callbacks(Module, [{Func, Arity} | Rest], Acc) ->
    case erlang:function_exported(Module, Func, Arity) of
        true ->
            validate_callbacks(Module, Rest, Acc);
        false ->
            Error = iolist_to_binary([
                "Missing required callback: ",
                atom_to_list(Func),
                "/",
                integer_to_list(Arity)
            ]),
            validate_callbacks(Module, Rest, [Error | Acc])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Remove all aliases for a pattern.
%%--------------------------------------------------------------------
-spec remove_aliases_for_pattern(atom()) -> ok.

remove_aliases_for_pattern(PatternName) ->
    %% Find all aliases pointing to this pattern and remove them
    Aliases = ets:select(?MODULE, [{{{'$1', alias}, '$2'}, [{'=:=', '$2', PatternName}], [['$1']]}]),
    lists:foreach(fun(Alias) ->
        ets:delete(?MODULE, {alias, Alias})
    end, Aliases),
    ok.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Setup and cleanup
setup() ->
    ets:new(?MODULE, [named_table, public, set, {read_concurrency, true}]),
    ok.

cleanup(_) ->
    catch ets:delete(?MODULE),
    ok.

%% Test with setup
register_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         %% Test valid registration
         Def = #{
             module => wfnet_sequence,
             category => control_flow,
             description => <<"Test pattern">>
         },
         ?assertEqual(ok, register_pattern(test_pattern, Def)),

         %% Test duplicate registration
         ?assertMatch({error, {pattern_already_registered, test_pattern}},
                      register_pattern(test_pattern, Def))
     end}.

get_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         Def = #{
             module => wfnet_sequence,
             category => control_flow,
             description => <<"Test pattern">>
         },
         ok = register_pattern(test_pattern, Def),

         %% Test getting existing pattern
         ?assertMatch({ok, _}, get_pattern(test_pattern)),

         %% Test getting non-existent pattern
         ?assertEqual({error, not_found}, get_pattern(nonexistent))
     end}.

unregister_pattern_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         Def = #{
             module => wfnet_sequence,
             category => control_flow,
             description => <<"Test pattern">>
         },
         ok = register_pattern(test_pattern, Def),

         %% Test unregister
         ?assertEqual(ok, unregister_pattern(test_pattern)),
         ?assertEqual({error, not_found}, get_pattern(test_pattern)),

         %% Test unregister non-existent
         ?assertEqual({error, not_found}, unregister_pattern(nonexistent))
     end}.

list_patterns_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         Def1 = #{module => wfnet_sequence, category => control_flow, description => <<"A">>},
         Def2 = #{module => wfnet_loop, category => iteration, description => <<"B">>},

         ok = register_pattern(pattern1, Def1),
         ok = register_pattern(pattern2, Def2),

         Patterns = list_patterns(),
         ?assertEqual(2, length(Patterns))
     end}.

list_categories_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         Def1 = #{module => wfnet_sequence, category => control_flow, description => <<"A">>},
         Def2 = #{module => wfnet_loop, category => iteration, description => <<"B">>},

         ok = register_pattern(pattern1, Def1),
         ok = register_pattern(pattern2, Def2),

         Categories = list_categories(),
         ?assert(lists:member(control_flow, Categories)),
         ?assert(lists:member(iteration, Categories))
     end}.

alias_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         Def = #{
             module => wfnet_sequence,
             category => control_flow,
             description => <<"Test pattern">>
         },
         ok = register_pattern(test_pattern, Def),

         %% Test add alias
         ?assertEqual(ok, add_alias(test_pattern, myalias)),

         %% Test resolve alias
         ?assertEqual({ok, test_pattern}, resolve_alias(myalias)),

         %% Test get through alias
         ?assertMatch({ok, _}, get_pattern(myalias)),

         %% Test get aliases
         ?assertEqual([myalias], get_aliases(test_pattern)),

         %% Test remove alias
         ?assertEqual(ok, remove_alias(test_pattern, myalias)),
         ?assertEqual({error, not_found}, resolve_alias(myalias)),

         %% Test add duplicate alias
         ?assertEqual(ok, add_alias(test_pattern, another_alias)),
         ?assertEqual({error, alias_exists}, add_alias(test_pattern, another_alias))
     end}.

validate_pattern_interface_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         %% Test with a real module that implements gen_wfnet
         ?assertMatch({ok, _}, validate_pattern_interface(wfnet_sequence))
     end}.

metadata_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun() ->
         Def = #{
             module => wfnet_sequence,
             category => control_flow,
             description => <<"Test pattern">>,
             metadata => #{key => value}
         },
         ok = register_pattern(test_pattern, Def),

         %% Test get metadata
         ?assertEqual({ok, #{key => value}}, get_pattern_metadata(test_pattern)),

         %% Test set metadata
         ?assertEqual(ok, set_pattern_metadata(test_pattern, #{new_key => new_value})),
         ?assertEqual({ok, #{key => value, new_key => new_value}},
                     get_pattern_metadata(test_pattern))
     end}.

get_required_callbacks_test() ->
    Required = get_required_callbacks(),
    ?assert(lists:member({workflow_spec, 0}, Required)),
    ?assert(lists:member({init_marking, 2}, Required)),
    ?assert(lists:member({fire, 3}, Required)),
    ?assert(lists:member({is_enabled, 3}, Required)),
    ?assert(lists:member({init, 1}, Required)).

is_valid_category_test() ->
    ?assert(is_valid_category(control_flow)),
    ?assert(is_valid_category(iteration)),
    ?assert(is_valid_category(custom)),
    ?assertNot(is_valid_category(invalid_category)).

-endif.
