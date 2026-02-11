%%% @doc Template Rendering Engine for ggen Manufacturing
%%% Minimal Tera-compatible template engine for Erlang code generation
%%% Supports: Variables, loops, filters for manufacturing pipelines
%%% Version: 1.0.0 - MVP grade
-module(ggen_template).

-export([
    render/2,
    render_file/2,
    load_template/1
]).

-type template() :: string().
-type context() :: #{atom() => term()}.

%%% ===========================================================================
%%% PUBLIC API
%%% ===========================================================================

%% @doc Render template string with context
-spec render(template(), context()) -> {ok, string()} | {error, term()}.
render(Template, Context) ->
    try
        Result = render_template(Template, Context),
        {ok, Result}
    catch
        error:Reason -> {error, Reason};
        throw:Reason -> {error, Reason}
    end.

%% @doc Load and render template file
-spec render_file(file:filename(), context()) -> {ok, string()} | {error, term()}.
render_file(Filename, Context) ->
    case load_template(Filename) of
        {ok, Template} ->
            render(Template, Context);
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Load template from file
-spec load_template(file:filename()) -> {ok, template()} | {error, term()}.
load_template(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary} ->
            {ok, binary_to_list(Binary)};
        {error, Reason} ->
            {error, Reason}
    end.

%%% ===========================================================================
%%% TEMPLATE RENDERING
%%% ===========================================================================

%% Render template with context
-spec render_template(template(), context()) -> string().
render_template(Template, Context) ->
    %% Phase 1: Process loops ({% for %})
    WithLoops = process_loops(Template, Context),

    %% Phase 2: Process conditionals ({% if %})
    WithConds = process_conditionals(WithLoops, Context),

    %% Phase 3: Process variables ({{ var }})
    WithVars = process_variables(WithConds, Context),

    %% Phase 4: Process filters ({{ var | filter }})
    WithFilters = process_filters(WithVars, Context),

    WithFilters.

%%% ===========================================================================
%%% LOOP PROCESSING
%%% ===========================================================================

%% Process {% for item in items %} loops
-spec process_loops(template(), context()) -> string().
process_loops(Template, Context) ->
    case re:run(Template, "\\{%\\s*for\\s+(\\w+)\\s+in\\s+(\\w+)\\s*%\\}(.+?)\\{%\\s*endfor\\s*%\\}",
                [dotall, {capture, all_but_first, list}]) of
        {match, [ItemVar, ListVar, Body]} ->
            ListVarAtom = list_to_atom(ListVar),
            case maps:get(ListVarAtom, Context, []) of
                [] ->
                    %% Empty list: remove loop entirely
                    re:replace(Template, "\\{%\\s*for\\s+\\w+\\s+in\\s+\\w+\\s*%\\}.+?\\{%\\s*endfor\\s*%\\}", "",
                              [dotall, {return, list}]);
                Items when is_list(Items) ->
                    %% Render loop body for each item
                    Rendered = lists:map(
                        fun(Item) ->
                            ItemCtx = maps:put(list_to_atom(ItemVar), Item, Context),
                            render_template(Body, ItemCtx)
                        end,
                        Items
                    ),
                    %% Replace loop with concatenated results
                    Result = string:join(Rendered, ""),
                    LoopPattern = "\\{%\\s*for\\s+" ++ ItemVar ++ "\\s+in\\s+" ++ ListVar ++ "\\s*%\\}.+?\\{%\\s*endfor\\s*%\\}",
                    re:replace(Template, LoopPattern, Result, [dotall, {return, list}]);
                _ ->
                    Template
            end;
        nomatch ->
            Template
    end.

%%% ===========================================================================
%%% CONDITIONAL PROCESSING
%%% ===========================================================================

%% Process {% if condition %} blocks
-spec process_conditionals(template(), context()) -> string().
process_conditionals(Template, Context) ->
    case re:run(Template, "\\{%\\s*if\\s+(\\w+)\\s*%\\}(.+?)\\{%\\s*endif\\s*%\\}",
                [dotall, {capture, all_but_first, list}]) of
        {match, [CondVar, Body]} ->
            CondVarAtom = list_to_atom(CondVar),
            ShouldInclude = case maps:get(CondVarAtom, Context, false) of
                true -> true;
                false -> false;
                undefined -> false;
                [] -> false;
                _ -> true
            end,

            Result = case ShouldInclude of
                true -> Body;
                false -> ""
            end,

            Pattern = "\\{%\\s*if\\s+" ++ CondVar ++ "\\s*%\\}.+?\\{%\\s*endif\\s*%\\}",
            re:replace(Template, Pattern, Result, [dotall, {return, list}]);
        nomatch ->
            Template
    end.

%%% ===========================================================================
%%% VARIABLE SUBSTITUTION
%%% ===========================================================================

%% Process {{ variable }} substitutions
-spec process_variables(template(), context()) -> string().
process_variables(Template, Context) ->
    case re:run(Template, "\\{\\{\\s*(\\w+)\\s*\\}\\}", [{capture, all_but_first, list}]) of
        {match, [VarName]} ->
            VarAtom = list_to_atom(VarName),
            Value = case maps:get(VarAtom, Context, undefined) of
                undefined -> "";
                V when is_binary(V) -> binary_to_list(V);
                V when is_atom(V) -> atom_to_list(V);
                V when is_integer(V) -> integer_to_list(V);
                V when is_list(V) -> V;
                V -> io_lib:format("~p", [V])
            end,
            Pattern = "\\{\\{\\s*" ++ VarName ++ "\\s*\\}\\}",
            Updated = re:replace(Template, Pattern, Value, [{return, list}]),
            %% Recursively process remaining variables
            process_variables(Updated, Context);
        nomatch ->
            Template
    end.

%%% ===========================================================================
%%% FILTER PROCESSING
%%% ===========================================================================

%% Process {{ variable | filter }} expressions
-spec process_filters(template(), context()) -> string().
process_filters(Template, _Context) ->
    case re:run(Template, "\\{\\{\\s*(\\w+)\\s*\\|\\s*(\\w+)\\s*\\}\\}",
                [{capture, all_but_first, list}]) of
        {match, [VarName, FilterName]} ->
            %% Value should already be substituted by process_variables
            %% This handles cases where filter is applied to literal
            Pattern = "\\{\\{\\s*" ++ VarName ++ "\\s*\\|\\s*" ++ FilterName ++ "\\s*\\}\\}",

            %% Apply filter to VarName
            Filtered = apply_filter(VarName, FilterName),

            Updated = re:replace(Template, Pattern, Filtered, [{return, list}]),
            %% Recursively process remaining filters
            process_filters(Updated, _Context);
        nomatch ->
            Template
    end.

%% Apply named filter
-spec apply_filter(string(), string()) -> string().
apply_filter(Value, "snake_case") ->
    snake_case(Value);
apply_filter(Value, "upper") ->
    string:to_upper(Value);
apply_filter(Value, "lower") ->
    string:to_lower(Value);
apply_filter(Value, "capitalize") ->
    capitalize(Value);
apply_filter(Value, _UnknownFilter) ->
    Value.

%%% ===========================================================================
%%% FILTER IMPLEMENTATIONS
%%% ===========================================================================

%% Convert to snake_case
-spec snake_case(string()) -> string().
snake_case(Str) ->
    %% Convert camelCase to snake_case
    WithUnderscores = re:replace(Str, "([a-z])([A-Z])", "\\1_\\2", [global, {return, list}]),
    string:to_lower(WithUnderscores).

%% Capitalize first letter
-spec capitalize(string()) -> string().
capitalize([]) -> [];
capitalize([First | Rest]) ->
    [string:to_upper([First]) | Rest].
