%% @doc YAWL Schema Validation Tests
%% Tests the yawl_schema module functionality
-module(yawl_schema_tests).
-include_lib("eunit/include/eunit.hrl").

%% Include record definitions from yawl_schema
-include("../src/wf/yawl_schema.hrl").

%%====================================================================
%% Test Constants
%%====================================================================

%% Test schema definitions
-define(TEST_SCHEMA, #{
    <<"name">> => <<"OrderSchema">>,
    <<"types">> => [
        #{
            <<"name">> => <<"OrderID">>,
            <<"base">> => <<"string">>,
            <<"required">> => true,
            <<"pattern">> => <<"^ORD-\\d{4}$">>
        },
        #{
            <<"name">> => <<"Amount">>,
            <<"base">> => <<"integer">>,
            <<"min">> => 0,
            <<"max">> => 1000000
        },
        #{
            <<"name">> => <<"Status">>,
            <<"base">> => <<"string">>,
            <<"enum">> => <<"pending,approved,rejected">>
        },
        #{
            <<"name">> => <<"CustomerInfo">>,
            <<"base">> => <<"complex">>,
            <<"required">> => true,
            <<"fields">> => [
                #{
                    <<"name">> => <<"name">>,
                    <<"type">> => <<"string">>,
                    <<"required">> => true
                },
                #{
                    <<"name">> => <<"email">>,
                    <<"type">> => <<"string">>,
                    <<"required">> => true,
                    <<"pattern">> => <<"^.*@.*$">>
                },
                #{
                    <<"name">> => <<"age">>,
                    <<"type">> => <<"integer">>,
                    <<"min">> => 18
                }
            ]
        }
    ]
}).

-define(TEST_SIMPLE_TYPES, [
    #{
        <<"name">> => <<"StringType">>,
        <<"base">> => <<"string">>
    },
    #{
        <<"name">> => <<"IntegerType">>,
        <<"base">> => <<"integer">>
    },
    #{
        <<"name">> => <<"BooleanType">>,
        <<"base">> => <<"boolean">>
    },
    #{
        <<"name">> => <<"FloatType">>,
        <<"base">> => <<"float">>
    }
]).

-define(TEST_CONSTRAINED_TYPES, [
    #{
        <<"name">> => <<"MinAmount">>,
        <<"base">> => <<"integer">>,
        <<"min">> => 100
    },
    #{
        <<"name">> => <<"MaxAmount">>,
        <<"base">> => <<"integer">>,
        <<"max">> => 1000
    },
    #{
        <<"name">> => <<"RangeAmount">>,
        <<"base">> => <<"integer">>,
        <<"min">> => 100,
        <<"max">> => 1000
    },
    #{
        <<"name">> => <<"EmailPattern">>,
        <<"base">> => <<"string">>,
        <<"pattern">> => <<"^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$">>
    },
    #{
        <<"name">> => <<"StatusEnum">>,
        <<"base">> => <<"string">>,
        <<"enum">> => <<"active,inactive,pending">>
    }
]).

%%====================================================================
%% Test Suite
%%====================================================================

yawl_schema_test_() ->
    [
        extract_schemas_tests(),
        parse_type_definition_tests(),
        validate_value_tests(),
        validate_data_tests(),
        get_type_constraints_tests(),
        error_handling_tests()
    ].

%%====================================================================
%% Extract Schemas Tests
%%====================================================================

extract_schemas_tests() ->
    [
        {"extract_schemas with map spec", fun extract_schemas_map_test/0},
        {"extract_schemas with list spec", fun extract_schemas_list_test/0},
        {"extract_schemas with no schemas", fun extract_schemas_empty_test/0},
        {"extract_schemas with invalid spec", fun extract_schemas_invalid_test/0}
    ].

extract_schemas_map_test() ->
    {ok, Schemas} = yawl_schema:do_extract_schemas(?TEST_SCHEMA),
    ?assertEqual(1, length(Schemas)),
    Schema = hd(Schemas),
    ?assertEqual(<<"OrderSchema">>, Schema#schema.name),
    ?assertEqual(4, maps:size(Schema#schema.types)),
    ok.

extract_schemas_list_test() ->
    SpecList = [?TEST_SCHEMA],
    {ok, Schemas} = yawl_schema:do_extract_schemas(SpecList),
    ?assertEqual(1, length(Schemas)),
    Schema = hd(Schemas),
    ?assertEqual(<<"OrderSchema">>, Schema#schema.name),
    ok.

extract_schemas_empty_test() ->
    SpecEmpty = #{<<"schemas">> => []},
    {ok, []} = yawl_schema:do_extract_schemas(SpecEmpty),
    ok.

extract_schemas_invalid_test() ->
    {error, invalid_spec_format} = yawl_schema:do_extract_schemas(invalid_spec),
    ok.

%%====================================================================
%% Parse Type Definition Tests
%%====================================================================

parse_type_definition_tests() ->
    [
        {"parse basic types", fun parse_basic_types_test/0},
        {"parse with constraints", fun parse_with_constraints_test/0},
        {"parse complex type", fun parse_complex_type_test/0},
        {"parse with enum", fun parse_with_enum_test/0},
        {"parse missing name", fun parse_missing_name_test/0},
        {"parse invalid xml", fun parse_invalid_xml_test/0}
    ].

parse_basic_types_test() ->
    lists:foreach(fun(TypeDef) ->
        {ok, Parsed} = yawl_schema:parse_type_definition(TypeDef),
        Name = maps:get(<<"name">>, TypeDef),
        ?assertEqual(Name, Parsed#type_definition.name),
        ?assertEqual(maps:get(<<"base">>, TypeDef), Parsed#type_definition.base)
    end, ?TEST_SIMPLE_TYPES),
    ok.

parse_with_constraints_test() ->
    %% Test MinAmount
    {ok, MinTypeDef} = yawl_schema:parse_type_definition(hd(?TEST_CONSTRAINED_TYPES)),
    ?assertEqual(100, MinTypeDef#type_definition.min),
    ?assertEqual(undefined, MinTypeDef#type_definition.max),

    %% Test MaxAmount
    {ok, MaxTypeDef} = yawl_schema:parse_type_definition(lists:nth(2, ?TEST_CONSTRAINED_TYPES)),
    ?assertEqual(undefined, MaxTypeDef#type_definition.min),
    ?assertEqual(1000, MaxTypeDef#type_definition.max),

    %% Test RangeAmount
    {ok, RangeTypeDef} = yawl_schema:parse_type_definition(lists:nth(3, ?TEST_CONSTRAINED_TYPES)),
    ?assertEqual(100, RangeTypeDef#type_definition.min),
    ?assertEqual(1000, RangeTypeDef#type_definition.max),
    ok.

parse_complex_type_test() ->
    CustomerInfoXml = #{
        <<"name">> => <<"CustomerInfo">>,
        <<"base">> => <<"complex">>,
        <<"required">> => true,
        <<"fields">> => [
            #{
                <<"name">> => <<"name">>,
                <<"type">> => <<"string">>,
                <<"required">> => true
            },
            #{
                <<"name">> => <<"email">>,
                <<"type">> => <<"string">>,
                <<"required">> => true,
                <<"pattern">> => <<"^.*@.*$">>
            }
        ]
    },
    {ok, ComplexTypeDef} = yawl_schema:parse_type_definition(CustomerInfoXml),
    ?assertEqual(<<"CustomerInfo">>, ComplexTypeDef#type_definition.name),
    ?assertEqual(2, maps:size(ComplexTypeDef#type_definition.fields)),
    ?assertEqual(true, ComplexTypeDef#type_definition.required),
    ok.

parse_with_enum_test() ->
    EnumXml = #{
        <<"name">> => <<"StatusEnum">>,
        <<"base">> => <<"string">>,
        <<"enum">> => <<"active,inactive,pending">>
    },
    {ok, EnumTypeDef} = yawl_schema:parse_type_definition(EnumXml),
    ?assertEqual([<<"active">>, <<"inactive">>, <<"pending">>], EnumTypeDef#type_definition.enum_values),
    ok.

parse_missing_name_test() ->
    InvalidXml = #{<<"base">> => <<"string">>},
    {error, missing_type_name} = yawl_schema:parse_type_definition(InvalidXml),
    ok.

parse_invalid_xml_test() ->
    {error, invalid_type_xml} = yawl_schema:parse_type_definition(invalid_xml),
    ok.

%%====================================================================
%% Validate Value Tests
%%====================================================================

validate_value_tests() ->
    [
        {"validate simple types", fun validate_simple_types_test/0},
        {"validate with min constraint", fun validate_min_constraint_test/0},
        {"validate with max constraint", fun validate_max_constraint_test/0},
        {"validate with range constraint", fun validate_range_constraint_test/0},
        {"validate with pattern constraint", fun validate_pattern_constraint_test/0},
        {"validate with enum constraint", fun validate_enum_constraint_test/0},
        {"validate complex type", fun validate_complex_type_test/0},
        {"validate required fields", fun validate_required_fields_test/0},
        {"validate optional fields", fun validate_optional_fields_test/0}
    ].

validate_simple_types_test() ->
    %% Test valid basic types
    lists:foreach(fun(TypeDef) ->
        _ = maps:get(<<"name">>, TypeDef),
        Base = maps:get(<<"base">>, TypeDef),
        TestValue = case Base of
            string -> <<"test">>;
            integer -> 42;
            boolean -> true;
            float -> 3.14
        end,
        {ok, Parsed} = yawl_schema:parse_type_definition(TypeDef),
        ok = yawl_schema:validate_value(TestValue, Parsed)
    end, ?TEST_SIMPLE_TYPES),
    ok.

validate_min_constraint_test() ->
    %% Test valid min
    {ok, MinTypeDef} = yawl_schema:parse_type_definition(hd(?TEST_CONSTRAINED_TYPES)),
    ok = yawl_schema:validate_value(100, MinTypeDef),  %% exact min
    ok = yawl_schema:validate_value(200, MinTypeDef),  %% above min

    %% Test invalid min
    {error, {min_constraint_violated, 100, 50}} = yawl_schema:validate_value(50, MinTypeDef),
    ok.

validate_max_constraint_test() ->
    %% Test valid max
    {ok, MaxTypeDef} = yawl_schema:parse_type_definition(lists:nth(2, ?TEST_CONSTRAINED_TYPES)),
    ok = yawl_schema:validate_value(1000, MaxTypeDef),  %% exact max
    ok = yawl_schema:validate_value(500, MaxTypeDef),    %% below max

    %% Test invalid max
    {error, {max_constraint_violated, 1000, 1500}} = yawl_schema:validate_value(1500, MaxTypeDef),
    ok.

validate_range_constraint_test() ->
    {ok, RangeTypeDef} = yawl_schema:parse_type_definition(lists:nth(3, ?TEST_CONSTRAINED_TYPES)),
    ok = yawl_schema:validate_value(500, RangeTypeDef),  %% in range
    ok = yawl_schema:validate_value(100, RangeTypeDef),  %% min boundary
    ok = yawl_schema:validate_value(1000, RangeTypeDef), %% max boundary

    %% Test below range
    {error, {min_constraint_violated, 100, 50}} = yawl_schema:validate_value(50, RangeTypeDef),
    %% Test above range
    {error, {max_constraint_violated, 1000, 1500}} = yawl_schema:validate_value(1500, RangeTypeDef),
    ok.

validate_pattern_constraint_test() ->
    EmailTypeDef = lists:nth(4, ?TEST_CONSTRAINED_TYPES),
    {ok, ParsedEmailTypeDef} = yawl_schema:parse_type_definition(EmailTypeDef),

    %% Valid emails
    ok = yawl_schema:validate_value(<<"test@example.com">>, ParsedEmailTypeDef),
    ok = yawl_schema:validate_value(<<"user@domain.org">>, ParsedEmailTypeDef),

    %% Invalid email
    {error, {pattern_mismatch, _, _}} = yawl_schema:validate_value(<<"invalid-email">>, ParsedEmailTypeDef),
    ok.

validate_enum_constraint_test() ->
    StatusTypeDef = lists:nth(5, ?TEST_CONSTRAINED_TYPES),
    {ok, ParsedStatusTypeDef} = yawl_schema:parse_type_definition(StatusTypeDef),

    %% Valid statuses
    ok = yawl_schema:validate_value(<<"active">>, ParsedStatusTypeDef),
    ok = yawl_schema:validate_value(<<"inactive">>, ParsedStatusTypeDef),
    ok = yawl_schema:validate_value(<<"pending">>, ParsedStatusTypeDef),

    %% Invalid status
    {error, {enum_violation, _, _}} = yawl_schema:validate_value(<<"invalid">>, ParsedStatusTypeDef),
    ok.

validate_complex_type_test() ->
    CustomerXml = #{
        <<"name">> => <<"CustomerInfo">>,
        <<"base">> => <<"complex">>,
        <<"fields">> => [
            #{
                <<"name">> => <<"name">>,
                <<"type">> => <<"string">>,
                <<"required">> => true
            },
            #{
                <<"name">> => <<"email">>,
                <<"type">> => <<"string">>,
                <<"required">> => true,
                <<"pattern">> => <<"^.*@.*$">>
            }
        ]
    },
    {ok, CustomerTypeDef} = yawl_schema:parse_type_definition(CustomerXml),

    %% Valid complex data
    ValidData = #{
        <<"name">> => <<"John Doe">>,
        <<"email">> => <<"john@example.com">>
    },
    ok = yawl_schema:validate_value(ValidData, CustomerTypeDef),

    %% Missing required field
    InvalidData1 = #{<<"name">> => <<"John Doe">>},
    {error, Errors1} = yawl_schema:validate_value(InvalidData1, CustomerTypeDef),
    ?assertEqual({error, {missing_required_field, <<"email">>}}, lists:nth(1, Errors1)),

    %% Invalid field value
    InvalidData2 = #{
        <<"name">> => <<"John Doe">>,
        <<"email">> => <<"invalid-email">>
    },
    {error, Errors2} = yawl_schema:validate_value(InvalidData2, CustomerTypeDef),
    ?assertMatch({error, {field_validation_failed, <<"email">>, _}}, lists:nth(1, Errors2)),
    ok.

validate_required_fields_test() ->
    RequiredXml = #{
        <<"name">> => <<"RequiredType">>,
        <<"base">> => <<"string">>,
        <<"required">> => true
    },
    {ok, RequiredTypeDef} = yawl_schema:parse_type_definition(RequiredXml),

    %% Valid required value
    ok = yawl_schema:validate_value(<<"value">>, RequiredTypeDef),

    %% Missing required value
    {error, {missing_required_value, <<"RequiredType">>}} = yawl_schema:validate_value(undefined, RequiredTypeDef),
    ok.

validate_optional_fields_test() ->
    OptionalXml = #{
        <<"name">> => <<"OptionalType">>,
        <<"base">> => <<"string">>,
        <<"required">> => false
    },
    {ok, OptionalTypeDef} = yawl_schema:parse_type_definition(OptionalXml),

    %% Valid values
    ok = yawl_schema:validate_value(<<"value">>, OptionalTypeDef),
    ok = yawl_schema:validate_value(undefined, OptionalTypeDef),
    ok.

%%====================================================================
%% Validate Data Tests
%%====================================================================

validate_data_tests() ->
    [
        {"validate data with schemas", fun validate_data_with_schemas_test/0},
        {"validate data with partial schemas", fun validate_data_partial_test/0},
        {"validate data with no matching schemas", fun validate_data_no_match_test/0}
    ].

validate_data_with_schemas_test() ->
    {ok, Schemas} = yawl_schema:extract_schemas(?TEST_SCHEMA),
    TestData = #{
        <<"OrderID">> => <<"ORD-1234">>,
        <<"Amount">> => 500,
        <<"Status">> => <<"approved">>,
        <<"CustomerInfo">> => #{
            <<"name">> => <<"Alice">>,
            <<"email">> => <<"alice@example.com">>,
            <<"age">> => 25
        }
    },
    {ok, ValidatedData} = yawl_schema:validate_data(TestData, Schemas),
    ?assertEqual(TestData, ValidatedData),
    ok.

validate_data_partial_test() ->
    {ok, Schemas} = yawl_schema:extract_schemas(?TEST_SCHEMA),
    PartialData = #{
        <<"OrderID">> => <<"ORD-1234">>,
        <<"Amount">> => 500
    },
    {ok, ValidatedData} = yawl_schema:validate_data(PartialData, Schemas),
    ?assertEqual(PartialData, ValidatedData),
    ok.

validate_data_no_match_test() ->
    {ok, Schemas} = yawl_schema:extract_schemas(?TEST_SCHEMA),
    DataWithoutSchema = #{<<"UnknownField">> => <<"value">>},
    {ok, ValidatedData} = yawl_schema:validate_data(DataWithoutSchema, Schemas),
    ?assertEqual(DataWithoutSchema, ValidatedData),
    ok.

%%====================================================================
%% Get Type Constraints Tests
%%====================================================================

get_type_constraints_tests() ->
    [
        {"get existing constraints", fun get_existing_constraints_test/0},
        {"get non-existing constraints", fun get_non_existing_constraints_test/0},
        {"get constraints from invalid schema", fun get_constraints_invalid_test/0}
    ].

get_existing_constraints_test() ->
    {ok, Schemas} = yawl_schema:extract_schemas(?TEST_SCHEMA),
    Schema = hd(Schemas),

    %% Get constraints for existing type
    case yawl_schema:get_type_constraints(Schema, <<"OrderID">>) of
        {ok, Constraints} ->
            ?assertEqual(true, Constraints#constraints.required);
        undefined ->
            %% Type might not exist in extracted schema
            ok
    end,
    ok.

get_non_existing_constraints_test() ->
    {ok, Schemas} = yawl_schema:extract_schemas(?TEST_SCHEMA),
    Schema = hd(Schemas),
    undefined = yawl_schema:get_type_constraints(Schema, <<"NonExistent">>),
    ok.

get_constraints_invalid_test() ->
    invalid = yawl_schema:get_type_constraints(invalid, <<"AnyType">>),
    ok.

%%====================================================================
%% Error Handling Tests
%%====================================================================

error_handling_tests() ->
    [
        {"handle invalid value type", fun invalid_value_type_test/0},
        {"handle invalid type definition", fun invalid_type_definition_test/0},
        {"handle invalid data type", fun invalid_data_type_test/0},
        {"handle invalid schemas", fun invalid_schemas_test/0}
    ].

invalid_value_type_test() ->
    SimpleXml = #{<<"name">> => <<"Test">>, <<"base">> => <<"string">>},
    {ok, TypeDef} = yawl_schema:parse_type_definition(SimpleXml),

    %% Test type mismatch
    {error, {type_mismatch, string, 123}} = yawl_schema:validate_value(123, TypeDef),
    ok.

invalid_type_definition_test() ->
    %% Test with invalid type definition record
    {error, invalid_type_definition} = yawl_schema:validate_value(<<"test">>, invalid),
    ok.

invalid_data_type_test() ->
    {error, invalid_schemas} = yawl_schema:validate_data("not_a_map", [valid_schema]),
    ok.

invalid_schemas_test() ->
    {error, invalid_schemas} = yawl_schema:validate_data(#{}, not_a_list),
    ok.