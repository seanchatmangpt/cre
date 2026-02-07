# YAWL Schema Validation Module Integration Guide

## Overview

The `yawl_schema` module provides comprehensive schema validation functionality for YAWL workflows. It can be used independently or integrated with `wf_spec` for schema extraction from YAWL specifications.

## Module Features

### Core Functions

```erlang
%% Extract all schema definitions from YAWL spec
extract_schemas(Spec) -> {ok, [Schema]} | {error, Reason}

%% Parse a single type definition from XML element
parse_type_definition(TypeXml) -> {ok, TypeDef} | {error, Reason}

%% Validate a value against a type definition
validate_value(Value, TypeDef) -> ok | {error, Reason}

%% Validate all data map values against schemas
validate_data(Data, Schemas) -> {ok, ValidatedData} | {error, [Reason]}

%% Extract constraints for a type from schema
get_type_constraints(Schema, TypeName) -> {ok, Constraints} | undefined
```

### Supported Schema Features

#### Basic Types
- `string` - Text strings
- `integer` - Integer numbers
- `boolean` - True/False values
- `float` - Floating point numbers
- `binary` - Binary data

#### Constraints
- **Required/Optional Fields**
- **Min/Max Values** - For numeric types
- **Pattern Matching** - For string validation using regex
- **Enumerated Values** - Predefined allowed values
- **Custom Validators** - Module-based validation functions

#### Complex Types
- Nested field definitions
- Required/optional field specification
- Field-level constraints
- Type composition

## Integration with wf_spec

To integrate schema validation with `wf_spec` for YAWL specification processing:

```erlang
%% Extract and validate workflow schemas
case wf_spec:extract_schemas(YAWLSpec) of
    {ok, Schemas} ->
        %% Validate workflow data against schemas
        case yawl_schema:validate_data(WorkflowData, Schemas) of
            {ok, ValidatedData} ->
                %% Proceed with validated data
                process_workflow(ValidatedData);
            {error, Errors} ->
                %% Handle validation errors
                handle_validation_errors(Errors)
        end;
    {error, Reason} ->
        %% Handle schema extraction errors
        handle_schema_error(Reason)
end.
```

## Usage Examples

### Basic Type Validation

```erlang
%% Define a simple type
SimpleType = #{
    <<"name">> => <<"Username">>,
    <<"base">> => <<"string">>,
    <<"required">> => true,
    <<"min">> => 3,
    <<"max">> => 20
},

%% Parse the type definition
{ok, TypeDef} = yawl_schema:parse_type_definition(SimpleType),

%% Validate values
ok = yawl_schema:validate_value(<<"john_doe">>, TypeDef),
{error, Reason} = yawl_schema:validate_value(<<"j">>, TypeDef)  %% Too short
```

### Complex Type Validation

```erlang
%% Define a complex type
AddressType = #{
    <<"name">> => <<"Address">>,
    <<"base">> => <<"complex">>,
    <<"fields">> => [
        #{<<"name">> => <<"street">>, <<"type">> => <<"string">>, <<"required">> => true},
        #{<<"name">> => <<"city">>, <<"type">> => <<"string">>, <<"required">> => true},
        #{<<"name">> => <<"zip">>, <<"type">> => <<"string">>, <<"required">> => false}
    ]
},

%% Parse and validate
{ok, ParsedType} = yawl_schema:parse_type_definition(AddressType),
ValidAddress = #{<<"street">> => <<"123 Main">>, <<"city">> => <<"Anytown">>},
ok = yawl_schema:validate_value(ValidAddress, ParsedType)
```

### Enum Validation

```erlang
EnumType = #{
    <<"name">> => <<"Status">>,
    <<"base">> => <<"string">>,
    <<"enum">> => <<"active,inactive,pending">>
},

{ok, StatusType} = yawl_schema:parse_type_definition(EnumType),
ok = yawl_schema:validate_value(<<"active">>, StatusType),
{error, {enum_violation, _, _}} = yawl_schema:validate_value(<<"unknown">>, StatusType)
```

## Error Handling

The module provides detailed error information:

```erlang
%% Type mismatch errors
{error, {type_mismatch, ExpectedType, ActualValue}}

%% Constraint violations
{error, {min_constraint_violated, Min, ActualValue}}
{error, {max_constraint_violated, Max, ActualValue}}
{error, {pattern_mismatch, Pattern, ActualValue}}
{error, {enum_violation, AllowedValues, ActualValue}}

## Missing required values
{error, {missing_required_value, TypeName}}
{error, {missing_required_field, FieldName}}
```

## Testing

Run the test suite to verify functionality:

```bash
rebar3 eunit --module yawl_schema_tests
```

Or run the demo:

```erl
c(schema_demo).
schema_demo:run().
```

## Architecture

The module follows the Joe Armstrong design principle:
- **Single OTP Process**: Only maintains minimal state
- **Pure Utilities**: Most functions are stateless helpers
- **Message Contracts**: Clean inter-process communication
- **Schema Validation**: Comprehensive constraint checking

## Future Enhancements

1. **Schema Versioning**: Support for versioned schema definitions
2. **Custom Validators**: Plugin-based validation extensions
3. **Schema Evolution**: Migration between schema versions
4. **Performance Optimization**: Caching for frequently used schemas
5. **JSON Schema Integration**: Import/export from JSON Schema format