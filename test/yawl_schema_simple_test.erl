%% @doc Simple YAWL Schema Validation Tests
%% Tests the yawl_schema module functionality without gen_server dependency
-module(yawl_schema_simple_test).
-include_lib("eunit/include/eunit.hrl").
-include("../src/wf/yawl_schema.hrl").

%% Simple test data
-define(TEST_TYPE_STRING, #{<<"name">> => <<"StringType">>, <<"base">> => <<"string">>}).
-define(TEST_TYPE_INTEGER, #{<<"name">> => <<"IntegerType">>, <<"base">> => <<"integer">>}).
-define(TEST_TYPE_MIN, #{<<"name">> => <<"MinAmount">>, <<"base">> => <<"integer">>, <<"min">> => 100}).
-define(TEST_TYPE_MAX, #{<<"name">> => <<"MaxAmount">>, <<"base">> => <<"integer">>, <<"max">> => 1000}).
-define(TEST_TYPE_ENUM, #{<<"name">> => <<"Status">>, <<"base">> => <<"string">>, <<"enum">> => <<"active,inactive">>}).
-define(TEST_TYPE_COMPLEX, #{
    <<"name">> => <<"Address">>,
    <<"base">> => <<"complex">>,
    <<"fields">> => [
        #{<<"name">> => <<"street">>, <<"type">> => <<"string">>, <<"required">> => true},
        #{<<"name">> => <<"city">>, <<"type">> => <<"string">>, <<"required">> => true},
        #{<<"name">> => <<"zip">>, <<"type">> => <<"string">>, <<"required">> => false}
    ]
}).

%%====================================================================
%% Test Suite
%%====================================================================

yawl_schema_simple_test_() ->
    [
        {"parse string type", fun parse_string_type_test/0},
        {"parse integer type", fun parse_integer_type_test/0},
        {"parse with min constraint", fun parse_min_constraint_test/0},
        {"parse with max constraint", fun parse_max_constraint_test/0},
        {"parse with enum constraint", fun parse_enum_constraint_test/0},
        {"parse complex type", fun parse_complex_type_test/0},
        {"validate string value", fun validate_string_test/0},
        {"validate integer value", fun validate_integer_test/0},
        {"validate min constraint", fun validate_min_test/0},
        {"validate max constraint", fun validate_max_test/0},
        {"validate enum constraint", fun validate_enum_test/0},
        {"validate complex type", fun validate_complex_test/0},
        {"validate required field", fun validate_required_test/0},
        {"validate optional field", fun validate_optional_test/0}
    ].

%%====================================================================
%% Parse Type Definition Tests
%%====================================================================

parse_string_type_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_STRING),
    ?assertEqual(<<"StringType">>, TypeDef#type_definition.name),
    ?assertEqual(string, TypeDef#type_definition.base),
    ?assertEqual(false, TypeDef#type_definition.required),
    ok.

parse_integer_type_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_INTEGER),
    ?assertEqual(<<"IntegerType">>, TypeDef#type_definition.name),
    ?assertEqual(integer, TypeDef#type_definition.base),
    ok.

parse_min_constraint_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_MIN),
    ?assertEqual(100, TypeDef#type_definition.min),
    ?assertEqual(undefined, TypeDef#type_definition.max),
    ok.

parse_max_constraint_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_MAX),
    ?assertEqual(undefined, TypeDef#type_definition.min),
    ?assertEqual(1000, TypeDef#type_definition.max),
    ok.

parse_enum_constraint_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_ENUM),
    ?assertEqual([<<"active">>, <<"inactive">>], TypeDef#type_definition.enum_values),
    ok.

parse_complex_type_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_COMPLEX),
    ?assertEqual(<<"Address">>, TypeDef#type_definition.name),
    ?assertEqual(complex, TypeDef#type_definition.base),
    ?assertEqual(3, maps:size(TypeDef#type_definition.fields)),
    ok.

%%====================================================================
%% Value Validation Tests
%%====================================================================

validate_string_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_STRING),
    ok = yawl_schema:do_validate_value(<<"test">>, TypeDef),
    ok = yawl_schema:do_validate_value(<<"">>, TypeDef),
    ok = yawl_schema:do_validate_value(undefined, TypeDef),
    {error, {type_mismatch, string, 123}} = yawl_schema:do_validate_value(123, TypeDef),
    ok.

validate_integer_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_INTEGER),
    ok = yawl_schema:do_validate_value(42, TypeDef),
    ok = yawl_schema:do_validate_value(0, TypeDef),
    ok = yawl_schema:do_validate_value(-100, TypeDef),
    {error, {type_mismatch, integer, <<"test">>}} = yawl_schema:do_validate_value(<<"test">>, TypeDef),
    ok.

validate_min_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_MIN),
    ok = yawl_schema:do_validate_value(100, TypeDef),  %% exact min
    ok = yawl_schema:do_validate_value(200, TypeDef),  %% above min
    {error, {min_constraint_violated, 100, 50}} = yawl_schema:do_validate_value(50, TypeDef),
    {error, {min_constraint_violated, 100, 99}} = yawl_schema:do_validate_value(99, TypeDef),
    ok.

validate_max_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_MAX),
    ok = yawl_schema:do_validate_value(1000, TypeDef),  %% exact max
    ok = yawl_schema:do_validate_value(500, TypeDef),   %% below max
    {error, {max_constraint_violated, 1000, 1500}} = yawl_schema:do_validate_value(1500, TypeDef),
    {error, {max_constraint_violated, 1000, 1001}} = yawl_schema:do_validate_value(1001, TypeDef),
    ok.

validate_enum_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_ENUM),
    ok = yawl_schema:do_validate_value(<<"active">>, TypeDef),
    ok = yawl_schema:do_validate_value(<<"inactive">>, TypeDef),
    {error, {enum_violation, _, _}} = yawl_schema:do_validate_value(<<"pending">>, TypeDef),
    {error, {enum_violation, _, _}} = yawl_schema:do_validate_value(<<"unknown">>, TypeDef),
    ok.

validate_complex_test() ->
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(?TEST_TYPE_COMPLEX),

    %% Valid complex data
    ValidData = #{
        <<"street">> => <<"123 Main St">>,
        <<"city">> => <<"Anytown">>,
        <<"zip">> => <<"12345">>
    },
    ok = yawl_schema:do_validate_value(ValidData, TypeDef),

    %% Missing optional field
    ValidData2 = #{
        <<"street">> => <<"123 Main St">>,
        <<"city">> => <<"Anytown">>
    },
    ok = yawl_schema:do_validate_value(ValidData2, TypeDef),

    %% Missing required field
    InvalidData = #{
        <<"street">> => <<"123 Main St">>,
        <<"zip">> => <<"12345">>
    },
    {error, _} = yawl_schema:do_validate_value(InvalidData, TypeDef),

    %% Wrong type for field
    InvalidData2 = #{
        <<"street">> => 123,
        <<"city">> => <<"Anytown">>
    },
    {error, _} = yawl_schema:do_validate_value(InvalidData2, TypeDef),
    ok.

validate_required_test() ->
    RequiredType = #{<<"name">> => <<"Required">>, <<"base">> => <<"string">>, <<"required">> => true},
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(RequiredType),
    ok = yawl_schema:do_validate_value(<<"value">>, TypeDef),
    {error, {missing_required_value, <<"Required">>}} = yawl_schema:do_validate_value(undefined, TypeDef),
    ok.

validate_optional_test() ->
    OptionalType = #{<<"name">> => <<"Optional">>, <<"base">> => <<"string">>, <<"required">> => false},
    {ok, TypeDef} = yawl_schema:do_parse_type_definition(OptionalType),
    ok = yawl_schema:do_validate_value(<<"value">>, TypeDef),
    ok = yawl_schema:do_validate_value(undefined, TypeDef),
    ok.