%% @doc Schema Validation Demo
%% Demonstrate yawl_schema functionality without gen_server dependency
-module(schema_demo).
-export([run/0]).

run() ->
    io:format("=== YAWL Schema Validation Demo ===~n~n"),

    %% Test 1: Parse a simple type
    io:format("1. Parsing simple type:~n"),
    SimpleType = #{<<"name">> => <<"Username">>, <<"base">> => <<"string">>, <<"required">> => true},
    case yawl_schema:do_parse_type_definition(SimpleType) of
        {ok, TypeDef} ->
            io:format("   Successfully parsed: ~p~n", [TypeDef#type_definition.name]);
        {error, Reason} ->
            io:format("   Error: ~p~n", [Reason])
    end,

    %% Test 2: Parse type with constraints
    io:format("~n2. Parsing type with constraints:~n"),
    ConstrainedType = #{
        <<"name">> => <<"Age">>,
        <<"base">> => <<"integer">>,
        <<"min">> => 0,
        <<"max">> => 150
    },
    case yawl_schema:do_parse_type_definition(ConstrainedType) of
        {ok, TypeDef2} ->
            io:format("   Successfully parsed: ~p with min=~p, max=~p~n",
                     [TypeDef2#type_definition.name, TypeDef2#type_definition.min, TypeDef2#type_definition.max]);
        {error, Reason2} ->
            io:format("   Error: ~p~n", [Reason2])
    end,

    %% Test 3: Parse complex type
    io:format("~n3. Parsing complex type:~n"),
    ComplexType = #{
        <<"name">> => <<"Address">>,
        <<"base">> => <<"complex">>,
        <<"fields">> => [
            #{<<"name">> => <<"street">>, <<"type">> => <<"string">>, <<"required">> => true},
            #{<<"name">> => <<"city">>, <<"type">> => <<"string">>, <<"required">> => true},
            #{<<"name">> => <<"zip">>, <<"type">> => <<"string">>, <<"required">> => false}
        ]
    },
    case yawl_schema:do_parse_type_definition(ComplexType) of
        {ok, TypeDef3} ->
            io:format("   Successfully parsed complex type with ~p fields~n", [maps:size(TypeDef3#type_definition.fields)]);
        {error, Reason3} ->
            io:format("   Error: ~p~n", [Reason3])
    end,

    %% Test 4: Validate values
    io:format("~n4. Validating values:~n"),
    {ok, StringType} = yawl_schema:do_parse_type_definition(SimpleType),

    %% Valid value
    case yawl_schema:do_validate_value(<<"john_doe">>, StringType) of
        ok -> io:format("   Valid string: 'john_doe'~n");
        {error, _} -> io:format("   Invalid string: 'john_doe'~n")
    end,

    %% Invalid value (wrong type)
    case yawl_schema:do_validate_value(123, StringType) of
        ok -> io:format("   Valid string: 123~n");
        {error, Reason4} -> io:format("   Invalid string 123: ~p~n", [Reason4])
    end,

    %% Test 5: Validate complex data
    io:format("~n5. Validating complex data:~n"),
    case yawl_schema:do_parse_type_definition(ComplexType) of
        {ok, AddressType} ->
            ValidAddress = #{
                <<"street">> => <<"123 Main St">>,
                <<"city">> => <<"Anytown">>,
                <<"zip">> => <<"12345">>
            },
            case yawl_schema:do_validate_value(ValidAddress, AddressType) of
                ok -> io:format("   Valid address accepted~n");
                {error, _} -> io:format("   Valid address rejected~n")
            end,

            %% Invalid address (missing required field)
            InvalidAddress = #{
                <<"street">> => <<"123 Main St">>,
                <<"zip">> => <<"12345">>
            },
            case yawl_schema:do_validate_value(InvalidAddress, AddressType) of
                ok -> io:format("   Invalid address accepted~n");
                {error, Reason5} -> io:format("   Invalid address rejected: ~p~n", [Reason5])
            end;
        {error, _} ->
            io:format("   Could not parse complex type~n")
    end,

    io:format("~n=== Demo Complete ===~n").