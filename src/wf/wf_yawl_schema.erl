%% @doc YAWL Schema Validation Module
%%
%% This module provides functionality to parse and validate YAWL schema type definitions.
%% It supports extracting schemas from YAWL specifications, parsing type definitions,
%% and validating data against those schemas.
%%
%% Schema Format (in YAWL XML):
%% ```xml
%% <schema name="OrderSchema">
%%   <type name="OrderID" base="string" required="true" pattern="^ORD-\d{4}$"/>
%%   <type name="Amount" base="integer" min="0" max="1000000"/>
%%   <type name="Status" base="string" enum="pending,approved,rejected"/>
%%   <type name="CustomerInfo">
%%     <field name="name" type="string" required="true"/>
%%     <field name="email" type="email" required="true" pattern="^.*@.*$"/>
%%     <field name="age" type="integer" min="18"/>
%%   </type>
%% </schema>
%% ```
-module(wf_yawl_schema).
-behaviour(gen_server).

%% Include record definitions
-include("yawl_schema.hrl").

%% API
-export([start_link/0, stop/0, extract_schemas/1, parse_type_definition/1,
         validate_value/2, validate_data/2, get_type_constraints/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Type aliases
-type type_name() :: binary().
-type base_type() :: string | integer | boolean | float | binary.
-type field_name() :: binary().
-type field_definition() :: #field_definition{}.
-type constraints() :: #constraints{}.
-type type_definition() :: #type_definition{}.
-type schema_definition() :: #schema{}.
-type schema_list() :: [schema_definition()].

-export_type([type_name/0, base_type/0, field_name/0, field_definition/0, constraints/0, type_definition/0,
              schema_definition/0, schema_list/0]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the schema server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Stop the schema server
stop() ->
    gen_server:call(?MODULE, stop).

%% @doc Extract all schema definitions from YAWL spec
%% @spec extract_schemas(Spec :: term()) -> {ok, schema_list()} | {error, Reason :: term()}
extract_schemas(Spec) when is_map(Spec) orelse is_list(Spec) ->
    gen_server:call(?MODULE, {extract_schemas, Spec}).

%% @doc Parse a single type definition from XML element
%% @spec parse_type_definition(TypeXml :: term()) -> {ok, type_definition()} | {error, Reason :: term()}
parse_type_definition(TypeXml) when is_map(TypeXml) orelse is_list(TypeXml) ->
    gen_server:call(?MODULE, {parse_type_definition, TypeXml}).

%% @doc Validate a value against a type definition
%% @spec validate_value(Value :: term(), TypeDef :: type_definition()) -> ok | {error, Reason :: term()}
validate_value(Value, TypeDef) when is_record(TypeDef, type_definition) ->
    gen_server:call(?MODULE, {validate_value, Value, TypeDef}).

%% @doc Validate all data map values against schemas
%% @spec validate_data(Data :: map(), Schemas :: schema_list()) -> {ok, map()} | {error, [Reason :: term()]}
validate_data(Data, Schemas) when is_map(Data), is_list(Schemas) ->
    gen_server:call(?MODULE, {validate_data, Data, Schemas}).

%% @doc Extract constraints for a type from schema
%% @spec get_type_constraints(Schema :: schema_definition(), TypeName :: type_name()) -> {ok, constraints()} | undefined
get_type_constraints(#schema{} = Schema, TypeName) ->
    case Schema#schema.types of
        #{TypeName := TypeDef} ->
            Constraints = TypeDef#type_definition.constraints,
            {ok, Constraints};
        _ ->
            undefined
    end;
get_type_constraints(_InvalidSchema, _TypeName) ->
    undefined.

%%====================================================================
%% Gen Server Callbacks
%%====================================================================

init([]) ->
    {ok, #{}}.

handle_call({extract_schemas, Spec}, _From, State) ->
    Result = do_extract_schemas(Spec),
    {reply, Result, State};

handle_call({parse_type_definition, TypeXml}, _From, State) ->
    Result = do_parse_type_definition(TypeXml),
    {reply, Result, State};

handle_call({validate_value, Value, TypeDef}, _From, State) ->
    Result = do_validate_value(Value, TypeDef),
    {reply, Result, State};

handle_call({validate_data, Data, Schemas}, _From, State) ->
    Result = do_validate_data(Data, Schemas),
    {reply, Result, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

do_extract_schemas(Spec) when is_map(Spec) ->
    case maps:get("schemas", Spec, []) of
        [] -> {ok, []};
        SchemaXmls ->
            SchemaXmlList = case is_map(SchemaXmls) of
                            true -> [SchemaXmls];
                            false -> SchemaXmls
                        end,
            Schemas = lists:map(fun extract_schema/1, SchemaXmlList),
            {ok, Schemas}
    end;
do_extract_schemas(Spec) when is_list(Spec) ->
    %% Handle list of schema XML elements
    Schemas = lists:map(fun extract_schema/1, Spec),
    {ok, Schemas};
do_extract_schemas(_InvalidSpec) ->
    {error, invalid_spec_format}.

extract_schema(SchemaXml) when is_map(SchemaXml) ->
    Name = case maps:get("name", SchemaXml) of
        undefined -> <<"unnamed">>;
        N when is_binary(N) -> N;
        N when is_list(N) -> unicode:characters_to_binary(N, utf8)
    end,

    TypesXml = case maps:get("types", SchemaXml, []) of
        [] -> [];
        T when is_list(T) -> T;
        T when is_map(T) -> [T]
    end,

    Types = lists:foldl(fun(TypeXml, Acc) ->
        case parse_type_definition_internal(TypeXml) of
            {ok, TypeDef} -> maps:put(TypeDef#type_definition.name, TypeDef, Acc);
            {error, _} -> Acc
        end
    end, #{}, TypesXml),

    Constraints = lists:foldl(fun(TypeDef, Acc) ->
        maps:put(TypeDef#type_definition.name, TypeDef#type_definition.constraints, Acc)
    end, #{}, maps:values(Types)),

    #schema{
        name = Name,
        types = Types,
        constraints = Constraints
    };
extract_schema(SchemaXml) when is_list(SchemaXml) ->
    %% Handle XML list or structure
    Extracted = xml_to_map(SchemaXml),
    extract_schema(Extracted).

do_parse_type_definition(TypeXml) when is_map(TypeXml) ->
    case parse_type_definition_internal(TypeXml) of
        {ok, TypeDef} -> {ok, TypeDef};
        {error, Reason} -> {error, Reason}
    end;
do_parse_type_definition(TypeXml) when is_list(TypeXml) ->
    MapXml = xml_to_map(TypeXml),
    do_parse_type_definition(MapXml);
do_parse_type_definition(_InvalidXml) ->
    {error, invalid_type_xml}.

parse_type_definition_internal(TypeXml) when is_map(TypeXml) ->
    Name = case maps:get("name", TypeXml) of
        undefined -> {error, missing_type_name};
        N when is_binary(N) -> N;
        N when is_list(N) -> unicode:characters_to_binary(N, utf8)
    end,

    case Name of
        {error, _} = Error -> Error;
        _ ->
            Base = case maps:get("base", TypeXml, "string") of
                "string" -> string;
                "integer" -> integer;
                "boolean" -> boolean;
                "float" -> float;
                "binary" -> binary;
                B when is_list(B) -> unicode:characters_to_binary(B, utf8);
                B when is_binary(B) -> B
            end,

            Required = case maps:get("required", TypeXml, false) of
                true -> true;
                false -> false;
                "true" -> true;
                "false" -> false
            end,

            Pattern = case maps:get("pattern", TypeXml) of
                undefined -> undefined;
                P when is_binary(P) -> P;
                P when is_list(P) -> unicode:characters_to_binary(P, utf8)
            end,

            MinValue = case maps:get("min", TypeXml) of
                undefined -> undefined;
                Min when is_integer(Min) -> Min;
                Min when is_float(Min) -> Min;
                MinStr when is_list(MinStr) ->
                    case string:to_integer(MinStr) of
                        {MinIntVal, ""} -> MinIntVal;
                        _ -> undefined
                    end
            end,

            MaxValue = case maps:get("max", TypeXml) of
                undefined -> undefined;
                Max when is_integer(Max) -> Max;
                Max when is_float(Max) -> Max;
                MaxStr when is_list(MaxStr) ->
                    case string:to_integer(MaxStr) of
                        {MaxIntVal, ""} -> MaxIntVal;
                        _ -> undefined
                    end
            end,

            Enum = case maps:get("enum", TypeXml) of
                undefined -> [];
                E when is_binary(E) -> binary:split(E, <<",">>, [global, trim]);
                E when is_list(E) -> lists:map(fun unicode:characters_to_binary/1, E)
            end,

            CustomValidator = case maps:get("custom_validator", TypeXml) of
                undefined -> undefined;
                CV when is_atom(CV) -> CV;
                CV when is_list(CV) -> list_to_atom(CV)
            end,

            %% Handle complex type with fields
            FieldsXml = case maps:get("fields", TypeXml, []) of
                [] -> #{};
                F when is_map(F) -> F;
                F when is_list(F) -> lists:foldl(fun(Field, Acc) ->
                    FieldDef = parse_field_definition(Field),
                    case FieldDef of
                        {error, _} -> Acc;
                        FieldRecord -> maps:put(FieldRecord#field_definition.name, FieldRecord, Acc)
                    end
                end, #{}, F)
            end,

            Fields = lists:foldl(fun(FieldXml, Acc) ->
                case parse_field_definition(FieldXml) of
                    {error, _} -> Acc;
                    FieldRecord -> maps:put(FieldRecord#field_definition.name, FieldRecord, Acc)
                end
            end, #{}, FieldsXml),

            TypeConstraints = #constraints{
                min = MinValue,
                max = MaxValue,
                pattern = Pattern,
                enum = Enum,
                required = Required,
                custom = #{custom_validator => CustomValidator}
            },

            TypeDef = #type_definition{
                name = Name,
                base = Base,
                required = Required,
                fields = Fields,
                enum_values = Enum,
                pattern = Pattern,
                min = MinValue,
                max = MaxValue,
                custom_validator = CustomValidator,
                constraints = TypeConstraints
            },

            {ok, TypeDef}
    end;
parse_type_definition_internal(_InvalidXml) ->
    {error, invalid_type_xml}.

parse_field_definition(FieldXml) when is_map(FieldXml) ->
    Name = case maps:get("name", FieldXml) of
        undefined -> {error, missing_field_name};
        N when is_binary(N) -> N;
        N when is_list(N) -> unicode:characters_to_binary(N, utf8)
    end,

    case Name of
        {error, _} = Error -> Error;
        _ ->
            Type = case maps:get("type", FieldXml) of
                undefined -> {error, missing_field_type};
                T when is_binary(T) -> T;
                T when is_list(T) -> unicode:characters_to_binary(T, utf8)
            end,

            case Type of
                {error, _} = Error2 -> Error2;
                _ ->
                    Required = case maps:get("required", FieldXml, false) of
                        true -> true;
                        false -> false;
                        "true" -> true;
                        "false" -> false
                    end,

                    MinField = case maps:get("min", FieldXml) of
                        undefined -> undefined;
                        Min when is_integer(Min) -> Min;
                        Min when is_float(Min) -> Min
                    end,

                    MaxField = case maps:get("max", FieldXml) of
                        undefined -> undefined;
                        Max when is_integer(Max) -> Max;
                        Max when is_float(Max) -> Max
                    end,

                    Pattern = case maps:get("pattern", FieldXml) of
                        undefined -> undefined;
                        P when is_binary(P) -> P;
                        P when is_list(P) -> unicode:characters_to_binary(P, utf8)
                    end,

                    FieldConstraints = #constraints{
                        min = MinField,
                        max = MaxField,
                        pattern = Pattern,
                        enum = [],
                        required = Required,
                        custom = #{}
                    },

                    FieldDef = #field_definition{
                        name = Name,
                        type = Type,
                        required = Required,
                        constraints = FieldConstraints
                    },

                    {ok, FieldDef}
            end
    end;
parse_field_definition(_InvalidFieldXml) ->
    {error, invalid_field_xml}.

do_validate_value(Value, TypeDef) when is_record(TypeDef, type_definition) ->
    case TypeDef#type_definition.fields of
        Fields when map_size(Fields) > 0 -> %% Complex type with fields
            validate_complex_type(Value, TypeDef);
        _ -> %% Simple type
            validate_simple_type(Value, TypeDef)
    end;
do_validate_value(_Value, _InvalidTypeDef) ->
    {error, invalid_type_definition}.

validate_simple_type(Value, TypeDef) ->
    Constraints = TypeDef#type_definition.constraints,

    %% Check if required and value is missing
    case Constraints#constraints.required of
        true ->
            case Value of
                undefined -> {error, {missing_required_value, TypeDef#type_definition.name}};
                _ -> continue_validation(Value, TypeDef)
            end;
        false ->
            case Value of
                undefined -> ok;
                _ -> continue_validation(Value, TypeDef)
            end
    end.

continue_validation(Value, TypeDef) ->
    Base = TypeDef#type_definition.base,
    Constraints = TypeDef#type_definition.constraints,

    %% Check type
    TypeResult = check_type(Value, Base),
    case TypeResult of
        ok -> check_constraints(Value, Constraints);
        {error, _} = Error -> Error
    end.

check_type(Value, Type) ->
    case Type of
        string when is_binary(Value) -> ok;
        string when is_list(Value) -> ok;
        integer when is_integer(Value) -> ok;
        boolean when is_boolean(Value) -> ok;
        float when is_float(Value) -> ok;
        float when is_integer(Value) -> ok;
        binary when is_binary(Value) -> ok;
        _ -> {error, {type_mismatch, Type, Value}}
    end.

check_constraints(Value, Constraints) ->
    %% Check min constraint
    case Constraints#constraints.min of
        undefined -> ok;
        Min when is_integer(Min) andalso is_integer(Value) ->
            if Value < Min -> {error, {min_constraint_violated, Min, Value}};
               true -> ok
            end;
        Min when is_integer(Min) andalso is_float(Value) ->
            if Value < Min -> {error, {min_constraint_violated, Min, Value}};
               true -> ok
            end;
        Min when is_float(Min) andalso is_integer(Value) ->
            if Value < Min -> {error, {min_constraint_violated, Min, Value}};
               true -> ok
            end;
        Min when is_float(Min) andalso is_float(Value) ->
            if Value < Min -> {error, {min_constraint_violated, Min, Value}};
               true -> ok
            end
    end,

    case Constraints#constraints.max of
        undefined -> ok;
        Max when is_integer(Max) andalso is_integer(Value) ->
            if Value > Max -> {error, {max_constraint_violated, Max, Value}};
               true -> ok
            end;
        Max when is_integer(Max) andalso is_float(Value) ->
            if Value > Max -> {error, {max_constraint_violated, Max, Value}};
               true -> ok
            end;
        Max when is_float(Max) andalso is_integer(Value) ->
            if Value > Max -> {error, {max_constraint_violated, Max, Value}};
               true -> ok
            end;
        Max when is_float(Max) andalso is_float(Value) ->
            if Value > Max -> {error, {max_constraint_violated, Max, Value}};
               true -> ok
            end
    end,

    case Constraints#constraints.pattern of
        undefined -> ok;
        Pattern when is_binary(Pattern) ->
            case re:run(Value, Pattern) of
                nomatch -> {error, {pattern_mismatch, Pattern, Value}};
                _ -> ok
            end
    end,

    case Constraints#constraints.enum of
        [] -> ok;
        EnumList ->
            case lists:member(Value, EnumList) of
                true -> ok;
                false -> {error, {enum_violation, EnumList, Value}}
            end
    end.

validate_complex_type(Value, TypeDef) when is_map(Value) ->
    Constraints = TypeDef#type_definition.constraints,

    %% Check if required and value is missing
    case Constraints#constraints.required of
        true ->
            case Value of
                undefined -> {error, {missing_required_value, TypeDef#type_definition.name}};
                _ -> validate_complex_fields(Value, TypeDef)
            end;
        false ->
            case Value of
                undefined -> ok;
                _ -> validate_complex_fields(Value, TypeDef)
            end
    end;
validate_complex_type(Value, TypeDef) ->
    {error, {complex_type_must_be_map, TypeDef#type_definition.name, Value}}.

validate_complex_fields(Value, TypeDef) ->
    Fields = TypeDef#type_definition.fields,
    Errors = lists:foldl(fun(FieldName, Acc) ->
        FieldDef = maps:get(FieldName, Fields),
        case maps:get(FieldName, Value, undefined) of
            undefined ->
                case FieldDef#field_definition.required of
                    true -> [ {error, {missing_required_field, FieldName}} | Acc ];
                    false -> Acc
                end;
            FieldValue ->
                case validate_value(FieldValue, FieldDef) of
                    ok -> Acc;
                    {error, Reason} -> [ {error, {field_validation_failed, FieldName, Reason}} | Acc ]
                end
        end
    end, [], maps:keys(Fields)),

    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

do_validate_data(Data, Schemas) when is_list(Schemas) ->
    Result = lists:foldl(fun(Schema, Acc) ->
        case validate_data_against_schema(Data, Schema) of
            {ok, Validated} -> maps:merge(Acc, Validated);
            {error, _Errors} -> Acc
        end
    end, #{}, Schemas),
    {ok, Result};
do_validate_data(_Data, _InvalidSchemas) ->
    {error, invalid_schemas}.

validate_data_against_schema(Data, Schema) when is_map(Data), is_record(Schema, schema) ->
    SchemaTypes = Schema#schema.types,
    lists:foldl(fun(TypeName, Acc) ->
        case maps:get(TypeName, Data, undefined) of
            undefined -> Acc;
            Value ->
                case maps:get(TypeName, SchemaTypes) of
                    TypeDef when is_record(TypeDef, type_definition) ->
                        case validate_value(Value, TypeDef) of
                            ok -> Acc;
                            {error, Reason} -> [ {error, {schema_validation_failed, TypeName, Reason}} | Acc ]
                        end;
                    undefined -> Acc
                end
        end
    end, [], maps:keys(Data)),
    {ok, Data};
validate_data_against_schema(_Data, _InvalidSchema) ->
    {error, invalid_schema}.

xml_to_map(Xml) when is_list(Xml) ->
    %% Simple XML to map conversion
    lists:foldl(fun(Node, Acc) ->
        case Node of
            {element, Name, Attrs, Children} ->
                ChildrenMap = lists:foldl(fun(C, CAcc) ->
                    case C of
                        {text, Text} -> case maps:get("text", CAcc, undefined) of
                                           undefined -> CAcc#{<<"text">> => unicode:characters_to_binary(Text, utf8)};
                                           _ -> CAcc
                                       end;
                        _ -> CAcc
                    end
                end, #{}, Children),
                Acc#{Name => maps:merge(Attrs, ChildrenMap)};
            _ -> Acc
        end
    end, #{}, Xml);
xml_to_map(Xml) when is_map(Xml) ->
    Xml;
xml_to_map(_Invalid) ->
    #{}.