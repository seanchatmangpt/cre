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

%% @doc Workflow Form Schema Generation
%%
%% This module provides form schema generation for workflow tasks.
%% Forms define the structure of user input required for task execution,
%% particularly for human-in-the-loop approval workflows.
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Generate default form schemas from task specifications</li>
%%   <li>Register custom form templates for specific tasks</li>
%%   <li>Resolve form schemas (custom takes precedence over auto-generated)</li>
%%   <li>Infer field types from task characteristics (e.g., approval tasks)</li>
%% </ul>
%%
%% <h3>Form Registry</h3>
%%
%% The forms registry is a pure map structure mapping task IDs to custom
%% form configurations. Use {@link new/0} to create an empty registry.
%%
%% <h3>Examples</h3>
%%
%% Create an empty registry:
%% ```erlang
%% > Forms = wf_forms:new().
%% #{}
%% ```
%%
%% Generate default form for task:
%% ```erlang
%% > Spec = #{tasks => #{<<"t1">> => #{kind => approval}}}.
%% > Form = wf_forms:task_form(Spec, t1).
%% #{task => t1, fields => [{approved, boolean}]}
%% ```
%%
%% Register and resolve custom form:
%% ```erlang
%% > Forms1 = wf_forms:register_custom(wf_forms:new(), t1, #{template => <<"approve.html">>}).
%% > wf_forms:resolve(Forms1, Spec, t1).
%% {custom,#{template => <<"approve.html">>}}
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_forms).

%%====================================================================
%% Exports
%%====================================================================

%% Registry management
-export([new/0, register_custom/3, resolve/3]).

%% Form generation
-export([task_form/2]).

%% HTML rendering and validation

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A workflow specification containing task definitions.
%%
%% Similar to yawl_schema:specification() with at least a `tasks` map
%% mapping task IDs (binary or atom) to task configuration maps.
%%--------------------------------------------------------------------
-type spec() :: #{
          tasks => #{task_id() => task_config()}
        }.

%%--------------------------------------------------------------------
%% @doc Task identifier (atom or binary).
%%--------------------------------------------------------------------
-type task_id() :: atom() | binary().

%%--------------------------------------------------------------------
%% @doc Task configuration map.
%%
%% Contains task metadata including optional `kind` field that can be
%% used to infer appropriate form fields (e.g., `approval` kind infers
%% an `approved` boolean field).
%%--------------------------------------------------------------------
-type task_config() :: #{
          kind := task_kind() | undefined,
          name := binary() | undefined,
          type := atomic | composite | multi_instance | undefined
        }.

%%--------------------------------------------------------------------
%% @doc Task kind indicating the type of work.
%%
%% Used to infer appropriate form fields:
%% <ul>
%%   <li><b>approval:</b> Adds `approved` boolean field</li>
%%   <li><b>input:</b> Adds generic input text field</li>
%%   <li><b>review:</b> Adds comment and rating fields</li>
%% </ul>
%%--------------------------------------------------------------------
-type task_kind() :: approval | input | review | custom | atom().

%%--------------------------------------------------------------------
%% @doc Form field type for HTML rendering.
%%
%% Determines the HTML input element and validation:
%% <ul>
%%   <li><b>text:</b> Single-line text input</li>
%%   <li><b>number:</b> Numeric input (integer or float)</li>
%%   <li><b>boolean:</b> Checkbox for true/false</li>
%%   <li><b>date:</b> Date picker input</li>
%%   <li><b>select:</b> Dropdown selection (requires options)</li>
%%   <li><b>textarea:</b> Multi-line text input</li>
%%   <li><b>email:</b> Email input with validation</li>
%% </ul>
%%--------------------------------------------------------------------
-type field_type() :: text | number | boolean | date | select | textarea | email
                    | string | integer | binary | atom().

%%--------------------------------------------------------------------
%% @doc Form field schema with validation rules.
%%
%% A map containing field definition and validation metadata:
%% <ul>
%%   <li><b>type:</b> Field type (text, number, boolean, date, select, textarea, email)</li>
%%   <li><b>required:</b> Whether the field must have a value</li>
%%   <li><b>label:</b> Human-readable label for the field</li>
%%   <li><b>min:</b> Minimum value (for numbers) or minimum length (for text)</li>
%%   <li><b>max:</b> Maximum value (for numbers) or maximum length (for text)</li>
%%   <li><b>pattern:</b> Regex pattern for string validation</li>
%%   <li><b>options:</b> List of {Value, Label} tuples for select fields</li>
%%   <li><b>default:</b> Default value for the field</li>
%%   <li><b>placeholder:</b> Placeholder text for text inputs</li>
%%   <li><b>help_text:</b> Additional help text displayed below the field</li>
%% </ul>
%%--------------------------------------------------------------------
-type field_schema() :: #{
    type := field_type(),
    required => boolean(),
    label => binary(),
    min => number(),
    max => number(),
    pattern => binary(),
    options => [{Value :: term(), Label :: binary()}],
    default => term(),
    placeholder => binary(),
    help_text => binary()
}.

%%--------------------------------------------------------------------
%% @doc Form schema map from field names to field schemas.
%%--------------------------------------------------------------------
-type form_schema() :: #{Field :: atom() => field_schema()}.

%%--------------------------------------------------------------------
%% @doc CSS framework for form styling.
%%
%% Determines the CSS classes applied to form elements.
%%--------------------------------------------------------------------
-type css_framework() :: bootstrap | tailwind | none | binary().

%%--------------------------------------------------------------------
%% @doc Render options for HTML form generation.
%%
%% <ul>
%%   <li><b>css:</b> CSS framework (bootstrap, tailwind, none, or custom class prefix)</li>
%%   <li><b>action:</b> Form action URL</li>
%%   <li><b>method:</b> HTTP method (post, get)</li>
%%   <li><b>enctype:</b> Encoding type (defaults to multipart/form-data)</li>
%%   <li><b>csrf_token:</b> CSRF token for form security</li>
%%   <li><b>form_class:</b> Additional CSS classes for the form element</li>
%%   <li><b>submit_label:</b> Label for the submit button</li>
%%   <li><b>submit_class:</b> CSS classes for the submit button</li>
%% </ul>
%%--------------------------------------------------------------------
-type render_options() :: #{
    css => css_framework(),
    action => binary(),
    method => binary(),
    enctype => binary(),
    csrf_token => binary(),
    form_class => binary(),
    submit_label => binary(),
    submit_class => binary()
}.

%%--------------------------------------------------------------------
%% @doc Validation result.
%%
%% <ul>
%%   <li><b>ok:</b> Validation passed</li>
%%   <li><b>{error, Errors}:</b> Validation failed, Errors is a map of field names to error messages</li>
%% </ul>
%%--------------------------------------------------------------------
-type validation_result() :: ok | {error, #{Field :: atom() => binary()}}.

%%--------------------------------------------------------------------
%% @doc Form submission data map.
%%
%% Map of field names to submitted values (typically from HTTP params).
%%--------------------------------------------------------------------
-type form_data() :: #{Field :: atom() | binary() => term()}.

%%--------------------------------------------------------------------
%% @doc Form field definition tuple.
%%
%% {FieldName, FieldType} where FieldName is an atom and FieldType
%% specifies the expected value type.
%%--------------------------------------------------------------------
-type field() :: {FieldName :: atom(), FieldType :: field_type()}.

%%--------------------------------------------------------------------
%% @doc Auto-generated form schema.
%%
%% Contains the task ID and list of field definitions inferred from
%% the task specification.
%%--------------------------------------------------------------------
-type auto_form() :: #{
          task := task_id(),
          fields := [field()]
        }.

%%--------------------------------------------------------------------
%% @doc Custom form configuration.
%%
%% Stores arbitrary custom form metadata (e.g., template path,
%% custom field definitions, UI hints).
%%--------------------------------------------------------------------
-type custom_config() :: map().

%%--------------------------------------------------------------------
%% @doc Form registry mapping task IDs to custom configurations.
%%
%% Pure map structure - no process state. Use {@link new/0} to create.
%%--------------------------------------------------------------------
-type registry() :: #{task_id() => custom_config()}.

%%--------------------------------------------------------------------
%% @doc Resolved form result.
%%
%% Either a custom form configuration (if registered) or an auto-generated
%% form schema wrapped in the `auto` tag.
%%--------------------------------------------------------------------
-type resolved_form() :: {custom, custom_config()} | {auto, auto_form()}.

%% Export types
-export_type([spec/0, task_id/0, task_config/0, task_kind/0,
              field_type/0, field/0, auto_form/0, custom_config/0,
              registry/0, resolved_form/0, field_schema/0, form_schema/0,
              css_framework/0, render_options/0, validation_result/0,
              form_data/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates an empty form registry.
%%
%% Returns a map with no registered custom forms.
%%
%% Example:
%% ```erlang
%% > Registry = wf_forms:new().
%% #{}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec new() -> registry().

new() ->
    #{}.

%%--------------------------------------------------------------------
%% @doc Generates a default form schema for a task.
%%
%% Analyzes the task configuration to infer appropriate fields:
%% <ul>
%%   <li>approval kind: adds `approved` boolean field</li>
%%   <li>input kind: adds `value` string field</li>
%%   <li>review kind: adds `comment` string and `rating` integer fields</li>
%%   <li>other kinds: adds generic `data` string field</li>
%% </ul>
%%
%% @param Spec Workflow specification containing task definitions
%% @param TaskId ID of the task (atom or binary)
%% @returns Auto-generated form schema with task and fields
%%
%% Example:
%% ```erlang
%% > Spec = #{tasks => #{<<"t1">> => #{kind => approval}}}.
%% > Form = wf_forms:task_form(Spec, t1).
%% #{task => t1, fields => [{approved, boolean}]}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec task_form(Spec :: spec(), TaskId :: task_id()) -> auto_form().

task_form(Spec, TaskId) when is_atom(TaskId) ->
    task_form(Spec, atom_to_binary(TaskId));
task_form(#{tasks := Tasks}, TaskId) when is_binary(TaskId) ->
    %% Try to find task by binary key
    TaskConfig = case maps:find(TaskId, Tasks) of
        {ok, Config} -> Config;
        error ->
            %% Try atom key
            AtomKey = binary_to_existing_atom(TaskId, utf8),
            maps:get(AtomKey, Tasks, #{})
    end,
    %% Infer fields from task kind
    Fields = infer_fields(maps:get(kind, TaskConfig, undefined)),
    #{
        task => normalize_task_id(TaskId),
        fields => Fields
    };
task_form(_Spec, TaskId) ->
    %% Fallback for missing tasks
    #{
        task => TaskId,
        fields => [{data, string}]
    }.

%%--------------------------------------------------------------------
%% @doc Registers a custom form configuration for a task.
%%
%% Custom forms take precedence over auto-generated forms during
%% resolution. The configuration can contain arbitrary metadata.
%%
%% @param Registry Existing form registry
%% @param TaskId ID of the task to register custom form for
%% @param Config Custom form configuration map
%% @returns Updated registry with custom form registered
%%
%% Example:
%% ```erlang
%% > Forms0 = wf_forms:new().
%% > Forms1 = wf_forms:register_custom(Forms0, t1, #{template => <<"approve.html">>}).
%% > maps:get(t1, Forms1).
%% #{template => <<"approve.html">>}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec register_custom(Registry :: registry(), TaskId :: task_id(),
                      Config :: custom_config()) -> registry().

register_custom(Registry, TaskId, Config) when is_atom(TaskId) ->
    Registry#{TaskId => Config};
register_custom(Registry, TaskId, Config) when is_binary(TaskId) ->
    %% Store with atom key for consistency
    AtomKey = try
        binary_to_existing_atom(TaskId, utf8)
    catch
        error:badarg ->
            %% Create new atom if it doesn't exist
            binary_to_atom(TaskId, utf8)
    end,
    Registry#{AtomKey => Config}.

%%--------------------------------------------------------------------
%% @doc Resolves a form schema for a task.
%%
%% Returns the custom form if registered for the task, otherwise
%% returns an auto-generated form schema based on the specification.
%%
%% @param Registry Form registry with custom form configurations
%% @param Spec Workflow specification containing task definitions
%% @param TaskId ID of the task to resolve form for
%% @returns {custom, Config} if custom form registered, {auto, Form} otherwise
%%
%% Example:
%% ```erlang
%% > Spec = #{tasks => #{<<"t1">> => #{kind => approval}}}.
%% > Forms1 = wf_forms:register_custom(wf_forms:new(), t1, #{template => <<"approve.html">>}).
%% > wf_forms:resolve(Forms1, Spec, t1).
%% {custom,#{template => <<"approve.html">>}}
%%
%% > Forms2 = wf_forms:new().
%% > wf_forms:resolve(Forms2, Spec, t1).
%% {auto,#{task => t1,fields => [{approved,boolean}]}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec resolve(Registry :: registry(), Spec :: spec(),
              TaskId :: task_id()) -> resolved_form().

resolve(Registry, Spec, TaskId) when is_binary(TaskId) ->
    resolve(Registry, Spec, normalize_task_id(TaskId));
resolve(Registry, Spec, TaskId) when is_atom(TaskId) ->
    case maps:find(TaskId, Registry) of
        {ok, Config} ->
            {custom, Config};
        error ->
            AutoForm = task_form(Spec, TaskId),
            {auto, AutoForm}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the field schema for a given field type.
%%
%% Returns a default field schema map for the specified field type,
%% which can be customized with additional options.
%%
%% @param FieldType The base field type
%% @returns A field schema map with defaults for the field type
%%
%% Example:
%% ```erlang
%% > wf_forms:field_schema(text).
%% #{type => text, required => false, label => <<"Field">>}
%%
%% > wf_forms:field_schema(number).
%% #{type => number, required => false, label => <<"Number">>}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec field_schema(FieldType :: field_type()) -> field_schema().

field_schema(text) ->
    #{
        type => text,
        required => false,
        label => <<"Field">>
    };
field_schema(number) ->
    #{
        type => number,
        required => false,
        label => <<"Number">>
    };
field_schema(boolean) ->
    #{
        type => boolean,
        required => false,
        label => <<"Checkbox">>
    };
field_schema(date) ->
    #{
        type => date,
        required => false,
        label => <<"Date">>
    };
field_schema(select) ->
    #{
        type => select,
        required => false,
        label => <<"Select">>,
        options => []
    };
field_schema(textarea) ->
    #{
        type => textarea,
        required => false,
        label => <<"Text Area">>
    };
field_schema(email) ->
    #{
        type => email,
        required => false,
        label => <<"Email">>,
        pattern => <<"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$">>
    };
field_schema(string) ->
    field_schema(text);
field_schema(integer) ->
    #{
        type => number,
        required => false,
        label => <<"Integer">>
    };
field_schema(_Type) ->
    field_schema(text).

%%--------------------------------------------------------------------
%% @doc Renders a form schema as an HTML form.
%%
%% Generates an HTML form from a form schema using iolists for
%% efficient building. Supports Bootstrap and Tailwind CSS frameworks.
%%
%% @param Schema Form schema map from field names to field schemas
%% @param Options Render options for CSS framework, action, method, etc.
%% @returns iolist() representing the complete HTML form
%%
%% Example:
%% ```erlang
%% > Schema = #{
%%     name => #{type => text, required => true, label => <<"Name">>},
%%     age => #{type => number, required => true, label => <<"Age">>, min => 18},
%%     approved => #{type => boolean, label => <<"Approve">>}
%% }.
%% > HTML = wf_forms:render_html(Schema, #{css => bootstrap}).
%% > iolist_to_binary(HTML).
%% <<"<form...">>
%% ```
%% @end
%%--------------------------------------------------------------------
-spec render_html(Schema :: form_schema(), Options :: render_options()) ->
          iolist().

render_html(Schema, Options) when is_map(Schema), is_map(Options) ->
    CSSFramework = maps:get(css, Options, none),
    Action = maps:get(action, Options, <<"">>),
    Method = maps:get(method, Options, <<"post">>),
    Enctype = maps:get(enctype, Options, <<"multipart/form-data">>),
    CsrfToken = maps:get(csrf_token, Options, undefined),
    FormClass = maps:get(form_class, Options, <<"">>),
    SubmitLabel = maps:get(submit_label, Options, <<"Submit">>),
    SubmitClass = maps:get(submit_class, Options, <<"">>),

    %% Build form tag with attributes
    FormTag = [
        <<"<form">>,
        form_class_attr(CSSFramework, FormClass),
        <<" action=\"">>, escape_html(Action), "\"",
        <<" method=\"">>, escape_html(Method), "\"",
        <<" enctype=\"">>, escape_html(Enctype), "\">"
    ],

    %% CSRF token hidden field if provided
    CsrfField = case CsrfToken of
        undefined -> [];
        Token -> [
            <<"<input type=\"hidden\" name=\"csrf_token\" value=\"">>,
            escape_html(Token),
            <<"\" />">>
        ]
    end,

    %% Render each field
    FieldRows = lists:map(
        fun({FieldName, FieldDef}) ->
            render_field(FieldName, FieldDef, CSSFramework)
        end,
        maps:to_list(Schema)
    ),

    %% Submit button
    SubmitButton = render_submit_button(SubmitLabel, SubmitClass, CSSFramework),

    %% Combine all parts
    [
        FormTag,
        CsrfField,
        FieldRows,
        SubmitButton,
        <<"</form>">>
    ].

%%--------------------------------------------------------------------
%% @doc Validates form submission data against a form schema.
%%
%% Checks all validation rules including required fields, numeric ranges,
%% pattern matching, and field types. Returns ok if all validations pass,
%% or {error, Errors} with a map of field names to error messages.
%%
%% @param Schema Form schema map from field names to field schemas
%% @param Data Form submission data (typically from HTTP params)
%% @returns ok if validation passes, {error, Errors} otherwise
%%
%% Example:
%% ```erlang
%% > Schema = #{
%%     email => #{type => email, required => true},
%%     age => #{type => number, min => 18, max => 120}
%% }.
%% > Data = #{email => <<"test@example.com">>, age => 25}.
%% > wf_forms:validate(Schema, Data).
%% ok
%%
%% > Data2 = #{email => <<>>}.
%% > wf_forms:validate(Schema, Data2).
%% {error,#{email => <<"Email is required">>}}
%% ```
%% @end
%%--------------------------------------------------------------------
-spec validate(Schema :: form_schema(), Data :: form_data()) ->
          validation_result().

validate(Schema, Data) when is_map(Schema), is_map(Data) ->
    %% Normalize data keys to atoms for consistent lookup
    NormalizedData = normalize_data_keys(Data),

    %% Collect all validation errors
    Errors = lists:foldl(
        fun({FieldName, FieldDef}, AccErrors) ->
            case validate_field(FieldName, FieldDef, NormalizedData) of
                ok -> AccErrors;
                {error, Msg} -> AccErrors#{FieldName => Msg}
            end
        end,
        #{},
        maps:to_list(Schema)
    ),

    case maps:size(Errors) of
        0 -> ok;
        _ -> {error, Errors}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Infers form fields based on task kind.
-spec infer_fields(task_kind() | undefined) -> [field()].

infer_fields(approval) ->
    [{approved, boolean}];
infer_fields(input) ->
    [{value, string}];
infer_fields(review) ->
    [{comment, string}, {rating, integer}];
infer_fields(_) ->
    [{data, string}].

%% @private
%% @doc Normalizes task ID to atom.
-spec normalize_task_id(task_id()) -> atom().

normalize_task_id(TaskId) when is_atom(TaskId) ->
    TaskId;
normalize_task_id(TaskId) when is_binary(TaskId) ->
    try
        binary_to_existing_atom(TaskId, utf8)
    catch
        error:badarg ->
            binary_to_atom(TaskId, utf8)
    end.

%% @private
%% @doc Normalizes form data keys to atoms.
-spec normalize_data_keys(form_data()) -> #{atom() => term()}.

normalize_data_keys(Data) when is_map(Data) ->
    maps:fold(fun
        (Key, Value, Acc) when is_atom(Key) ->
            Acc#{Key => Value};
        (Key, Value, Acc) when is_binary(Key) ->
            try
                Atom = binary_to_existing_atom(Key, utf8),
                Acc#{Atom => Value}
            catch
                error:badarg ->
                    Acc#{Key => Value}
            end
    end, #{}, Data).

%% @private
%% @doc Escapes HTML special characters.
-spec escape_html(binary() | atom() | number()) -> iolist().

escape_html(undefined) ->
    <<"">>;
escape_html(Value) when is_atom(Value) ->
    escape_html(atom_to_binary(Value, utf8));
escape_html(Value) when is_integer(Value) ->
    integer_to_binary(Value);
escape_html(Value) when is_float(Value) ->
    %% Simple float to binary conversion
    %% For production, consider using a proper library
    float_to_binary(Value, [{decimals, 10}, compact]);
escape_html(Value) when is_binary(Value) ->
    %% Escape HTML special characters
    Escaped0 = binary:replace(Value, <<"&">>, <<"&amp;">>, [global]),
    Escaped1 = binary:replace(Escaped0, <<"<">>, <<"&lt;">>, [global]),
    Escaped2 = binary:replace(Escaped1, <<">">>, <<"&gt;">>, [global]),
    Escaped3 = binary:replace(Escaped2, <<"\"">>, <<"&quot;">>, [global]),
    binary:replace(Escaped3, <<"'">>, <<"&#39;">>, [global]).

%% @private
%% @doc Generates CSS class attribute for form tag.
-spec form_class_attr(css_framework(), binary()) -> iolist().

form_class_attr(none, <<"">>) ->
    <<" class=\"\"">>;
form_class_attr(none, Class) ->
    [<<" class=\"">>, Class, <<"\"">>];
form_class_attr(bootstrap, _) ->
    <<" class=\"form\"">>;
form_class_attr(tailwind, _) ->
    <<" class=\"space-y-4\"">>;
form_class_attr(Custom, _) when is_binary(Custom) ->
    [<<" class=\"">>, Custom, <<"\"">>].

%% @private
%% @doc Renders a single form field based on its type and CSS framework.
-spec render_field(atom(), field_schema(), css_framework()) -> iolist().

render_field(FieldName, FieldDef, CSSFramework) ->
    Type = maps:get(type, FieldDef, text),
    Required = maps:get(required, FieldDef, false),
    Label = maps:get(label, FieldDef, atom_to_binary(FieldName, utf8)),
    Placeholder = maps:get(placeholder, FieldDef, <<"">>),
    HelpText = maps:get(help_text, FieldDef, <<"">>),
    Min = maps:get(min, FieldDef, undefined),
    Max = maps:get(max, FieldDef, undefined),
    Pattern = maps:get(pattern, FieldDef, undefined),
    Options = maps:get(options, FieldDef, []),
    Default = maps:get(default, FieldDef, undefined),

    FieldNameStr = atom_to_binary(FieldName, utf8),
    RequiredAttr = case Required of
        true -> <<" required">>;
        false -> <<"">>
    end,

    %% Build field container and label
    {ContainerClass, LabelClass, InputClass} = field_classes(CSSFramework),

    ContainerStart = case ContainerClass of
        <<"">> -> <<"">>;
        _ -> [<<"<div class=\"">>, ContainerClass, <<"\">">>]
    end,

    LabelTag = [
        <<"<label for=\"">>, FieldNameStr, "\" class=\"", LabelClass, "\">",
        Label,
        case Required of true -> <<" <span class=\"required\">*</span>">>; false -> <<"">> end,
        <<"</label>">>
    ],

    %% Build input element based on type
    InputTag = render_input(
        FieldNameStr, Type, InputClass, Placeholder,
        RequiredAttr, Min, Max, Pattern, Options, Default, CSSFramework
    ),

    %% Help text if provided
    HelpBlock = case HelpText of
        <<"">> -> [];
        _ when CSSFramework =:= bootstrap ->
            [<<"<small class=\"form-text text-muted\">">>, HelpText, <<"</small>">>];
        _ when CSSFramework =:= tailwind ->
            [<<"<p class=\"text-sm text-gray-500\">">>, HelpText, <<"</p>">>];
        _ ->
            [<<"<small>">>, HelpText, <<"</small>">>]
    end,

    ContainerEnd = case ContainerClass of
        <<"">> -> <<"">>;
        _ -> <<"</div>">>
    end,

    [ContainerStart, LabelTag, InputTag, HelpBlock, ContainerEnd].

%% @private
%% @doc Renders input element based on field type.
-spec render_input(binary(), field_type(), binary(), binary(),
                   binary(), number() | undefined, number() | undefined,
                   binary() | undefined, [{term(), binary()}], term(),
                   css_framework()) -> iolist().

render_input(FieldName, text, Class, Placeholder, Required, Min, Max, Pattern, [], _Default, _CSS) ->
    %% Text input
    MinAttr = attr_min_max(minlength, Min),
    MaxAttr = attr_min_max(maxlength, Max),
    PatternAttr = case Pattern of
        undefined -> <<"">>;
        _ -> [<<" pattern=\"">>, Pattern, <<"\"">>]
    end,
    [
        <<"<input type=\"text\" name=\"">>, FieldName,
        <<"\" id=\"">>, FieldName,
        <<"\" class=\"">>, Class, <<"\"">>,
        case Placeholder of <<"">> -> <<"">>; P -> [<<" placeholder=\"">>, escape_html(P), <<"\"">>] end,
        Required,
        MinAttr,
        MaxAttr,
        PatternAttr,
        <<" />">>
    ];

render_input(FieldName, number, Class, Placeholder, Required, Min, Max, _Pattern, [], _Default, _CSS) ->
    %% Number input
    MinAttr = case Min of
        undefined -> <<"">>;
        _ -> [<<" min=\"">>, number_to_binary(Min), <<"\"">>]
    end,
    MaxAttr = case Max of
        undefined -> <<"">>;
        _ -> [<<" max=\"">>, number_to_binary(Max), <<"\"">>]
    end,
    [
        <<"<input type=\"number\" name=\"">>, FieldName,
        <<"\" id=\"">>, FieldName,
        <<"\" class=\"">>, Class, <<"\"">>,
        case Placeholder of <<"">> -> <<"">>; P -> [<<" placeholder=\"">>, escape_html(P), <<"\"">>] end,
        Required,
        MinAttr,
        MaxAttr,
        <<" />">>
    ];

render_input(FieldName, boolean, _Class, _Placeholder, _Required, _Min, _Max, _Pattern, [], Default, CSS) ->
    %% Boolean checkbox
    Checked = case Default of
        true -> <<" checked">>;
        _ -> <<"">>
    end,
    WrapperClass = case CSS of
        bootstrap -> <<"form-check">>;
        tailwind -> <<"flex items-center">>;
        _ -> <<"">>
    end,
    InputClass = case CSS of
        bootstrap -> <<"form-check-input">>;
        tailwind -> <<"h-4 w-4 text-blue-600">>;
        _ -> <<"">>
    end,
    LabelClass = case CSS of
        bootstrap -> <<"form-check-label">>;
        tailwind -> <<"ml-2">>;
        _ -> <<"">>
    end,
    [
        <<"<div class=\"">>, WrapperClass, <<"\">">>,
        <<"<input type=\"checkbox\" name=\"">>, FieldName,
        <<"\" id=\"">>, FieldName,
        <<"\" class=\"">>, InputClass, <<"\" value=\"true\"">>, Checked, <<" />">>,
        <<"<label class=\"">>, LabelClass, <<"\" for=\"">>, FieldName, <<"\">">>,
        <<"Yes"/utf8>>,
        <<"</label>">>,
        <<"</div>">>
    ];

render_input(FieldName, date, Class, _Placeholder, Required, _Min, _Max, _Pattern, [], _Default, _CSS) ->
    %% Date input
    [
        <<"<input type=\"date\" name=\"">>, FieldName,
        <<"\" id=\"">>, FieldName,
        <<"\" class=\"">>, Class, <<"\"">>,
        Required,
        <<" />">>
    ];

render_input(FieldName, select, Class, _Placeholder, Required, _Min, _Max, _Pattern, Options, Default, _CSS) ->
    %% Select dropdown
    OptionsHtml = lists:map(fun
        ({Value, Label}) when Value =:= Default ->
            [<<"<option value=\"">>, escape_html(value_to_binary(Value)), <<"\" selected>">>,
             escape_html(Label), <<"</option>">>];
        ({Value, Label}) ->
            [<<"<option value=\"">>, escape_html(value_to_binary(Value)), <<"\">">>,
             escape_html(Label), <<"</option>">>]
    end, Options),
    [
        <<"<select name=\"">>, FieldName,
        <<"\" id=\"">>, FieldName,
        <<"\" class=\"">>, Class, <<"\"">>,
        Required,
        <<">">>,
        OptionsHtml,
        <<"</select>">>
    ];

render_input(FieldName, textarea, Class, Placeholder, Required, Min, Max, _Pattern, [], _Default, _CSS) ->
    %% Textarea
    MinAttr = attr_min_max(minlength, Min),
    MaxAttr = attr_min_max(maxlength, Max),
    [
        <<"<textarea name=\"">>, FieldName,
        <<"\" id=\"">>, FieldName,
        <<"\" class=\"">>, Class, <<"\"">>,
        case Placeholder of <<"">> -> <<"">>; P -> [<<" placeholder=\"">>, escape_html(P), <<"\"">>] end,
        Required,
        MinAttr,
        MaxAttr,
        <<" rows=\"4\"></textarea>">>
    ];

render_input(FieldName, email, Class, Placeholder, Required, Min, Max, Pattern, [], _Default, _CSS) ->
    %% Email input
    MinAttr = attr_min_max(minlength, Min),
    MaxAttr = attr_min_max(maxlength, Max),
    EmailPattern = case Pattern of
        undefined -> <<"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$">>;
        _ -> Pattern
    end,
    [
        <<"<input type=\"email\" name=\"">>, FieldName,
        <<"\" id=\"">>, FieldName,
        <<"\" class=\"">>, Class, <<"\"">>,
        case Placeholder of <<"">> -> <<"">>; P -> [<<" placeholder=\"">>, escape_html(P), <<"\"">>] end,
        Required,
        MinAttr,
        MaxAttr,
        <<" pattern=\"">>, EmailPattern, <<"\"">>,
        <<" />">>
    ];

render_input(FieldName, _Type, Class, Placeholder, Required, Min, Max, Pattern, [], Default, CSS) ->
    %% Default to text input for unknown types
    render_input(FieldName, text, Class, Placeholder, Required, Min, Max, Pattern, [], Default, CSS).

%% @private
%% @doc Generates min/max attribute string.
-spec attr_min_max(binary(), number() | undefined) -> iolist().

attr_min_max(_AttrName, undefined) ->
    <<"">>;
attr_min_max(AttrName, Value) when is_integer(Value) ->
    [<<" \"">>, AttrName, <<"=\"">>, integer_to_binary(Value), <<"\"">>];
attr_min_max(AttrName, Value) when is_float(Value) ->
    [<<" \"">>, AttrName, <<"=\"">>, float_to_binary(Value, [{decimals, 2}, compact]), <<"\"">>].

%% @private
%% @doc Returns CSS classes for form elements based on framework.
-spec field_classes(css_framework()) -> {ContainerClass :: binary(),
                                          LabelClass :: binary(),
                                          InputClass :: binary()}.

field_classes(bootstrap) ->
    {<<"mb-3">>, <<"form-label">>, <<"form-control">>};
field_classes(tailwind) ->
    {<<"">>, <<"block text-sm font-medium text-gray-700">>,
     <<"mt-1 block w-full rounded-md border-gray-300 shadow-sm focus:border-blue-500 focus:ring-blue-500 sm:text-sm">>};
field_classes(none) ->
    {<<"form-field">>, <<"form-label">>, <<"form-input">>};
field_classes(_Custom) ->
    {<<"form-field">>, <<"form-label">>, <<"form-input">>}.

%% @private
%% @doc Renders the submit button.
-spec render_submit_button(binary(), binary(), css_framework()) -> iolist().

render_submit_button(Label, Class, bootstrap) when Class =:= <<"">> ->
    [<<"<button type=\"submit\" class=\"btn btn-primary\">">>, Label, <<"</button>">>];
render_submit_button(Label, Class, bootstrap) ->
    [<<"<button type=\"submit\" class=\"btn btn-primary ">>, Class, <<"\">">>, Label, <<"</button>">>];
render_submit_button(Label, Class, tailwind) when Class =:= <<"">> ->
    [<<"<button type=\"submit\" class=\"inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-blue-600 hover:bg-blue-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-blue-500\">">>,
     Label, <<"</button>">>];
render_submit_button(Label, Class, tailwind) ->
    [<<"<button type=\"submit\" class=\"">>, Class, <<"\">">>, Label, <<"</button>">>];
render_submit_button(Label, Class, none) when Class =:= <<"">> ->
    [<<"<button type=\"submit\" class=\"btn-submit\">">>, Label, <<"</button>">>];
render_submit_button(Label, Class, none) ->
    [<<"<button type=\"submit\" class=\"btn-submit ">>, Class, <<"\">">>, Label, <<"</button>">>];
render_submit_button(Label, Class, _Custom) ->
    [<<"<button type=\"submit\" class=\"">>, Class, <<"\">">>, Label, <<"</button>">>].

%% @private
%% @doc Validates a single field against its schema.
-spec validate_field(atom(), field_schema(), #{atom() => term()}) ->
          ok | {error, binary()}.

validate_field(FieldName, FieldDef, Data) ->
    Required = maps:get(required, FieldDef, false),
    Type = maps:get(type, FieldDef, text),
    Min = maps:get(min, FieldDef, undefined),
    Max = maps:get(max, FieldDef, undefined),
    Pattern = maps:get(pattern, FieldDef, undefined),

    %% Get value from data
    Value = maps:get(FieldName, Data, undefined),

    %% Check required first
    case {Required, is_empty(Value)} of
        {true, true} ->
            Label = maps:get(label, FieldDef, atom_to_binary(FieldName)),
            {error, <<Label/binary, " is required">>};
        {_, true} ->
            ok;
        {_, false} ->
            %% Value exists, validate based on type
            validate_field_type(FieldName, Type, Value, Min, Max, Pattern, FieldDef)
    end.

%% @private
%% @doc Validates field value against its type and constraints.
-spec validate_field_type(atom(), field_type(), term(),
                          number() | undefined, number() | undefined,
                          binary() | undefined, field_schema()) ->
          ok | {error, binary()}.

validate_field_type(_FieldName, text, Value, Min, Max, Pattern, _FieldDef) ->
    %% Text validation
    BinValue = to_binary(Value),
    Length = byte_size(BinValue),

    %% Validate range first
    case validate_range(Length, Min, Max) of
        {error, Msg} -> {error, Msg};
        ok ->
            %% Pattern validation
            case Pattern of
                undefined -> ok;
                _ ->
                    case re:run(BinValue, Pattern, [{capture, none}]) of
                        match -> ok;
                        nomatch ->
                            {error, <<"Value does not match the required pattern">>}
                    end
            end
    end;

validate_field_type(_FieldName, number, Value, Min, Max, _Pattern, _FieldDef) ->
    %% Number validation
    NumValue = to_number(Value),
    case validate_range(NumValue, Min, Max) of
        ok -> ok;
        {error, Msg} -> {error, Msg}
    end;

validate_field_type(_FieldName, boolean, _Value, _Min, _Max, _Pattern, _FieldDef) ->
    %% Boolean is always valid if present (checked or unchecked)
    ok;

validate_field_type(_FieldName, date, Value, _Min, _Max, _Pattern, _FieldDef) ->
    %% Date validation - check format YYYY-MM-DD
    BinValue = to_binary(Value),
    case re:run(BinValue, <<"^[0-9]{4}-[0-9]{2}-[0-9]{2}$">>, [{capture, none}]) of
        match -> ok;
        nomatch ->
            {error, <<"Invalid date format (use YYYY-MM-DD)">>}
    end;

validate_field_type(_FieldName, select, Value, _Min, _Max, _Pattern, FieldDef) ->
    %% Select validation - check value is in options
    Options = maps:get(options, FieldDef, []),
    case lists:keyfind(Value, 1, Options) of
        false -> {error, <<"Selected value is not valid">>};
        _ -> ok
    end;

validate_field_type(_FieldName, textarea, Value, Min, Max, _Pattern, _FieldDef) ->
    %% Textarea validation - same as text
    BinValue = to_binary(Value),
    Length = byte_size(BinValue),
    validate_range(Length, Min, Max);

validate_field_type(_FieldName, email, Value, _Min, _Max, Pattern, _FieldDef) ->
    %% Email validation
    BinValue = to_binary(Value),
    EmailPattern = case Pattern of
        undefined -> <<"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$">>;
        _ -> Pattern
    end,
    case re:run(BinValue, EmailPattern, [{capture, none}]) of
        match -> ok;
        nomatch ->
            {error, <<"Invalid email address">>}
    end;

validate_field_type(_FieldName, _Type, _Value, _Min, _Max, _Pattern, _FieldDef) ->
    ok.

%% @private
%% @doc Validates a value is within min/max range.
-spec validate_range(number() | integer(), number() | undefined, number() | undefined) ->
          ok | {error, binary()}.

validate_range(_Value, undefined, undefined) ->
    ok;
validate_range(Value, Min, undefined) when Min =/= undefined ->
    case Value >= Min of
        true -> ok;
        false -> {error, <<"Value is too small">>}
    end;
validate_range(Value, undefined, Max) when Max =/= undefined ->
    case Value =< Max of
        true -> ok;
        false -> {error, <<"Value is too large">>}
    end;
validate_range(Value, Min, Max) when Min =/= undefined, Max =/= undefined ->
    case Value >= Min andalso Value =< Max of
        true -> ok;
        false -> {error, <<"Value is out of range">>}
    end.

%% @private
%% @doc Checks if a value is empty.
-spec is_empty(term()) -> boolean().

is_empty(undefined) -> true;
is_empty(<<"">>) -> true;
is_empty([]) -> true;
is_empty(null) -> true;
is_empty(_) -> false.

%% @private
%% @doc Converts value to binary.
-spec to_binary(term()) -> binary().

to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_float(V) -> float_to_binary(V, [{decimals, 10}, compact]);
to_binary(V) when is_list(V) -> iolist_to_binary(V).

%% @private
%% @doc Converts value to number.
-spec to_number(term()) -> number().

to_number(V) when is_integer(V) -> V;
to_number(V) when is_float(V) -> V;
to_number(V) when is_binary(V) ->
    try binary_to_integer(V) of
        Int -> Int
    catch
        error:badarg ->
            binary_to_float(V)
    end.

%% @private
%% @doc Converts term to binary for HTML attributes.
-spec value_to_binary(term()) -> binary().

value_to_binary(V) when is_binary(V) -> V;
value_to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
value_to_binary(V) when is_integer(V) -> integer_to_binary(V);
value_to_binary(V) when is_float(V) -> float_to_binary(V, [{decimals, 10}, compact]);
value_to_binary(V) -> list_to_binary(io_lib:format("~p", [V])).

%% @private
%% @doc Converts number to binary.
-spec number_to_binary(number()) -> binary().

number_to_binary(N) when is_integer(N) -> integer_to_binary(N);
number_to_binary(N) when is_float(N) -> float_to_binary(N, [{decimals, 10}, compact]).

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% @doc Run doctests for the wf_forms module.
%%
%% This function validates:
%% <ul>
%%   <li>Creating new empty registry</li>
%%   <li>Generating task form with approval kind</li>
%%   <li>Registering custom form for task</li>
%%   <li>Resolving custom form when registered</li>
%%   <li>Resolving auto form when no custom registered</li>
%%   <li>Task field contains correct task ID</li>
%%   <li>Fields list contains correct field tuples</li>
%%   <li>Handling binary and atom task IDs</li>
%% </ul>
%%
%% Running the doctests:
%%
%% ```erlang
%% 1> wf_forms:doctest_test().
%% ok
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Create new registry
    Registry0 = new(),
    0 = map_size(Registry0),

    %% Test 2: Generate task form for approval task (atom TaskId)
    SpecAtom = #{
        tasks => #{
            t1 => #{kind => approval, name => <<"Approve">>, type => atomic}
        }
    },
    Form0 = task_form(SpecAtom, t1),
    t1 = maps:get(task, Form0),
    true = lists:member({approved, boolean}, maps:get(fields, Form0)),

    %% Test 3: Generate task form for approval task (binary TaskId)
    SpecBinary = #{
        tasks => #{
            <<"t1">> => #{kind => approval}
        }
    },
    Form1 = task_form(SpecBinary, t1),
    t1 = maps:get(task, Form1),
    true = lists:member({approved, boolean}, maps:get(fields, Form1)),

    %% Test 4: Register custom form (atom TaskId)
    Registry1 = register_custom(new(), t1, #{template => <<"approve.html">>}),
    1 = map_size(Registry1),

    %% Test 5: Resolve custom form when registered
    {custom, CustomConfig} = resolve(Registry1, SpecAtom, t1),
    <<"approve.html">> = maps:get(template, CustomConfig),

    %% Test 6: Resolve auto form when no custom registered
    {auto, AutoForm} = resolve(new(), SpecAtom, t1),
    t1 = maps:get(task, AutoForm),
    true = lists:member({approved, boolean}, maps:get(fields, AutoForm)),

    %% Test 7: Input task kind inference
    SpecInput = #{
        tasks => #{
            t2 => #{kind => input}
        }
    },
    FormInput = task_form(SpecInput, t2),
    true = lists:member({value, string}, maps:get(fields, FormInput)),

    %% Test 8: Review task kind inference
    SpecReview = #{
        tasks => #{
            t3 => #{kind => review}
        }
    },
    FormReview = task_form(SpecReview, t3),
    true = lists:member({comment, string}, maps:get(fields, FormReview)),
    true = lists:member({rating, integer}, maps:get(fields, FormReview)),

    %% Test 9: Unknown task kind falls back to data field
    SpecUnknown = #{
        tasks => #{
            t4 => #{kind => unknown_kind}
        }
    },
    FormUnknown = task_form(SpecUnknown, t4),
    true = lists:member({data, string}, maps:get(fields, FormUnknown)),

    %% Test 10: Missing task in spec returns default form
    SpecEmpty = #{tasks => #{}},
    FormEmpty = task_form(SpecEmpty, t5),
    t5 = maps:get(task, FormEmpty),
    true = lists:member({data, string}, maps:get(fields, FormEmpty)),

    %% Test 11: Register custom with binary TaskId
    Registry2 = register_custom(new(), <<"t1">>, #{path => <<"/forms/t1">>}),
    1 = map_size(Registry2),

    %% Test 12: Resolve with binary TaskId
    {custom, BinaryConfig} = resolve(Registry2, SpecAtom, <<"t1">>),
    <<"/forms/t1">> = maps:get(path, BinaryConfig),

    %% Test 13: Custom form can have arbitrary metadata
    CustomMeta = #{
        template => <<"custom.html">>,
        title => <<"Custom Form">>,
        fields => [name, email, message],
        css => <<"/static/custom.css">>
    },
    Registry3 = register_custom(new(), t1, CustomMeta),
    {custom, ResolvedMeta} = resolve(Registry3, SpecAtom, t1),
    <<"custom.html">> = maps:get(template, ResolvedMeta),
    <<"Custom Form">> = maps:get(title, ResolvedMeta),

    %% Test 14: Resolve binary TaskId from atom-keyed registry
    {custom, _} = resolve(Registry3, SpecAtom, <<"t1">>),

    %% Test 15: Multiple custom forms in registry
    Registry4 = lists:foldl(
        fun({Tid, Cfg}, Acc) -> register_custom(Acc, Tid, Cfg) end,
        new(),
        [{t1, #{a => 1}}, {t2, #{b => 2}}, {t3, #{c => 3}}]
    ),
    3 = map_size(Registry4),

    %% Test 16: field_schema returns correct default schema for text
    TextSchema = field_schema(text),
    text = maps:get(type, TextSchema),
    false = maps:get(required, TextSchema),

    %% Test 17: field_schema returns correct default schema for number
    NumberSchema = field_schema(number),
    number = maps:get(type, NumberSchema),

    %% Test 18: field_schema returns correct default schema for email
    EmailSchema = field_schema(email),
    email = maps:get(type, EmailSchema),
    Pattern = maps:get(pattern, EmailSchema),
    true = byte_size(Pattern) > 0,

    %% Test 19: field_schema handles legacy string type
    StringSchema = field_schema(string),
    text = maps:get(type, StringSchema),

    %% Test 20: render_html generates valid form HTML
    FormSchema = #{
        name => #{type => text, required => true, label => <<"Name">>},
        age => #{type => number, label => <<"Age">>, min => 18, max => 120}
    },
    HTML = render_html(FormSchema, #{css => none, method => <<"post">>}),
    HTMLBinary = iolist_to_binary(HTML),
    true = binary:match(HTMLBinary, <<"<form">>) =/= nomatch,
    true = binary:match(HTMLBinary, <<"name=\"name\"">>) =/= nomatch,
    true = binary:match(HTMLBinary, <<"type=\"text\"">>) =/= nomatch,
    true = binary:match(HTMLBinary, <<"required">>) =/= nomatch,

    %% Test 21: render_html with bootstrap CSS includes form-control class
    HTMLBootstrap = render_html(FormSchema, #{css => bootstrap}),
    BootstrapBinary = iolist_to_binary(HTMLBootstrap),
    true = binary:match(BootstrapBinary, <<"form-control">>) =/= nomatch,

    %% Test 22: render_html with tailwind CSS includes appropriate classes
    HTMLTailwind = render_html(FormSchema, #{css => tailwind}),
    TailwindBinary = iolist_to_binary(HTMLTailwind),
    true = binary:match(TailwindBinary, <<"text-gray-700">>) =/= nomatch,

    %% Test 23: render_html includes CSRF token when provided
    HTMLWithCSRF = render_html(FormSchema, #{
        css => none,
        csrf_token => <<"secret-token-123">>
    }),
    CSRFBinary = iolist_to_binary(HTMLWithCSRF),
    true = binary:match(CSRFBinary, <<"csrf_token">>) =/= nomatch,
    true = binary:match(CSRFBinary, <<"secret-token-123">>) =/= nomatch,

    %% Test 24: validate returns ok for valid data
    ValidData = #{name => <<"Alice">>, age => 25},
    ok = validate(FormSchema, ValidData),

    %% Test 25: validate checks required fields
    MissingRequired = #{age => 25},
    {error, Errors1} = validate(FormSchema, MissingRequired),
    1 = map_size(Errors1),
    NameError = maps:get(name, Errors1),
    true = binary:match(NameError, <<"required">>) =/= nomatch,

    %% Test 26: validate checks numeric ranges (min)
    TooYoung = #{name => <<"Bob">>, age => 15},
    {error, Errors2} = validate(FormSchema, TooYoung),
    true = maps:is_key(age, Errors2),

    %% Test 27: validate checks numeric ranges (max)
    TooOld = #{name => <<"Charlie">>, age => 150},
    {error, Errors3} = validate(FormSchema, TooOld),
    true = maps:is_key(age, Errors3),

    %% Test 28: validate checks pattern constraints (email)
    EmailSchema2 = #{
        email => #{type => email, required => true, label => <<"Email">>}
    },
    InvalidEmail = #{email => <<"not-an-email">>},
    {error, Errors4} = validate(EmailSchema2, InvalidEmail),
    true = maps:is_key(email, Errors4),

    %% Test 29: validate accepts valid email
    ValidEmail = #{email => <<"test@example.com">>},
    ok = validate(EmailSchema2, ValidEmail),

    %% Test 30: render_html generates boolean checkbox
    BooleanSchema = #{
        approved => #{type => boolean, label => <<"Approve">>}
    },
    BooleanHTML = iolist_to_binary(render_html(BooleanSchema, #{css => none})),
    true = binary:match(BooleanHTML, <<"type=\"checkbox\"">>) =/= nomatch,

    %% Test 31: render_html generates select dropdown
    SelectSchema = #{
        choice => #{
            type => select,
            label => <<"Choose">>,
            options => [{a, <<"Option A">>}, {b, <<"Option B">>}]
        }
    },
    SelectHTML = iolist_to_binary(render_html(SelectSchema, #{css => none})),
    true = binary:match(SelectHTML, <<"<select">>) =/= nomatch,
    true = binary:match(SelectHTML, <<"Option A">>) =/= nomatch,

    %% Test 32: validate checks select value is in options
    InvalidSelect = #{choice => c},
    {error, Errors5} = validate(SelectSchema, InvalidSelect),
    true = maps:is_key(choice, Errors5),

    %% Test 33: validate accepts valid select value
    ValidSelect = #{choice => a},
    ok = validate(SelectSchema, ValidSelect),

    %% Test 34: render_html generates date input
    DateSchema = #{
        birthdate => #{type => date, label => <<"Birthday">>}
    },
    DateHTML = iolist_to_binary(render_html(DateSchema, #{css => none})),
    true = binary:match(DateHTML, <<"type=\"date\"">>) =/= nomatch,

    %% Test 35: validate checks date format
    InvalidDate = #{birthdate => <<"not-a-date">>},
    {error, Errors6} = validate(DateSchema, InvalidDate),
    true = maps:is_key(birthdate, Errors6),

    %% Test 36: validate accepts valid date format
    ValidDate = #{birthdate => <<"2024-01-15">>},
    ok = validate(DateSchema, ValidDate),

    %% Test 37: render_html generates textarea
    TextareaSchema = #{
        comment => #{type => textarea, label => <<"Comments">>}
    },
    TextareaHTML = iolist_to_binary(render_html(TextareaSchema, #{css => none})),
    true = binary:match(TextareaHTML, <<"<textarea">>) =/= nomatch,

    %% Test 38: render_html with custom submit label
    CustomSubmit = iolist_to_binary(render_html(FormSchema, #{
        css => none,
        submit_label => <<"Save Changes">>
    })),
    true = binary:match(CustomSubmit, <<"Save Changes">>) =/= nomatch,

    %% Test 39: validate handles binary keys in data map
    BinaryKeyData = #{<<"name">> => <<"David">>, <<"age">> => 30},
    ok = validate(FormSchema, BinaryKeyData),

    %% Test 40: escape_html prevents XSS
    EscapedHTML = escape_html(<<"<script>alert('xss')</script>">>),
    EscapedBinary = iolist_to_binary(EscapedHTML),
    false = binary:match(EscapedBinary, <<"<script>">>) =/= nomatch,
    true = binary:match(EscapedBinary, <<"&lt;script&gt;">>) =/= nomatch,

    ok.
-endif.
