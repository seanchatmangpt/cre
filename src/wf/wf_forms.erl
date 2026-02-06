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
%% @doc Form field type.
%%
%% Determines the expected data type for form field values.
%%--------------------------------------------------------------------
-type field_type() :: boolean | string | integer | number | binary | atom().

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
              registry/0, resolved_form/0]).

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

    ok.
-endif.
