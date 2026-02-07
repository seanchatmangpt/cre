%%-------------------------------------------------------------------
%% @doc
%% Record definitions for yawl_schema module
%%
%% This header file contains record definitions that are shared between
%% the yawl_schema module and its test files.
%%
%% @end
%%-------------------------------------------------------------------

%% Constraints record - validation constraints for types
-record(constraints, {
    min :: number() | undefined,
    max :: number() | undefined,
    pattern :: binary() | undefined,
    enum :: [binary()],
    required :: boolean(),
    custom :: #{atom() => any()}
}).

%% Field definition record - schema field definition
-record(field_definition, {
    name :: binary(),
    type :: binary(),
    required :: boolean(),
    constraints :: #constraints{}
}).

%% Type definition record - schema type definition
-record(type_definition, {
    name :: binary(),
    base :: atom() | undefined,
    required :: boolean(),
    fields :: #{binary() => #field_definition{}},
    enum_values :: [binary()],
    pattern :: binary() | undefined,
    min :: number() | undefined,
    max :: number() | undefined,
    custom_validator :: module() | undefined,
    constraints :: #constraints{}
}).

%% Schema record - YAWL schema definition
-record(schema, {
    name :: binary(),
    types :: #{binary() => #type_definition{}},
    constraints :: #{binary() => #constraints{}}
}).
