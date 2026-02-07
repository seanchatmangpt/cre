-module(wf_yawl_data_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test Data
%%====================================================================

%% Test schema for order data
-define(TEST_SCHEMA, #{
    <<"order_id">> => {type, required, [{type, binary}, {min_length, 10}, {pattern, <<"^ORD\\d{6}$">>}]},
    <<"customer_id">> => {type, required, [{type, binary}, {min_length, 1}]},
    <<"amount">> => {type, required, [{type, float}, {min, 0}, {max, 1000000}]},
    <<"discount">> => {type, optional, [{type, float}, {min, 0}, {max, 100}]},
    <<"items">> => {type, required, [{type, list}]},
    <<"metadata">> => {type, optional, [{type, map}]}
}).

%% Test data for order
-define(TEST_DATA, #{
    <<"order_id">> => <<"ORD123456">>,
    <<"customer_id">> => <<"cust_001">>,
    <<"amount">> => 999.99,
    <<"discount">> => 10.0,
    <<"items">> => [#{<<"id">> => 1, <<"name">> => <<"item1">>, <<"price">> => 500.0}],
    <<"metadata">> => #{<<"source">> => <<"web">>, <<"priority">> => high}
}).

%% Invalid test data
-define(INVALID_DATA, #{
    <<"order_id">> => <<"ORD123">>,      % Too short
    <<"customer_id">> => undefined,       % Required but undefined
    <<"amount">> => -50.0,               % Below min
    <<"items">> => not_a_list            % Wrong type
}).

%%====================================================================
%% API Tests
%%====================================================================

extract_data_test_() ->
    [
        {"Extract data from valid spec",
         fun() ->
             %% Mock wf_spec:task_params
             meck:new(wf_spec, [passthrough]),
             meck:expect(wf_spec, task_params,
                        fun(_Spec, _TaskId) ->
                            {ok, [{<<"order_id">>, <<"ORD123456">>},
                                  {<<"customer_id">>, <<"cust_001">>}]}
                        end),

             %% Test extraction
             Spec = mock_spec(),
             {ok, Data} = yawl_data:extract_data(Spec, <<"net1">>, <<"task1">>),

             ?assertEqual(#{<<"order_id">> => <<"ORD123456">>,
                          <<"customer_id">> => <<"cust_001">>}, Data),

             meck:unload(wf_spec)
         end},

        {"Handle extraction error",
         fun() ->
             meck:new(wf_spec, [passthrough]),
             meck:expect(wf_spec, task_params,
                        fun(_Spec, _TaskId) ->
                            {error, not_found}
                        end),

             Spec = mock_spec(),
             {error, Reason} = yawl_data:extract_data(Spec, <<"net1">>, <<"task1">>),
             ?assertMatch({data_extraction_failed, _, _}, Reason),

             meck:unload(wf_spec)
         end}
    ].

validate_data_test_() ->
    [
        {"Validate valid data",
         fun() ->
             {ok, Validated} = yawl_data:validate_data(?TEST_DATA, ?TEST_SCHEMA),
             ?assertEqual(?TEST_DATA, Validated)
         end},

        {"Reject undefined required field",
         fun() ->
             BadData = maps:remove(<<"customer_id">>, ?TEST_DATA),
             {error, Reasons} = yawl_data:validate_data(BadData, ?TEST_SCHEMA),
             ?assertMatch([undefined_required_variable], Reasons)
         end},

        {"Reject wrong data type",
         fun() ->
             BadData = ?TEST_DATA#{<<"amount">> => not_a_number},
             {error, Reasons} = yawl_data:validate_data(BadData, ?TEST_SCHEMA),
             ?assertMatch([{type_mismatch, _, _}], Reasons)
         end},

        {"Reject value below minimum",
         fun() ->
             BadData = ?TEST_DATA#{<<"amount">> => -1.0},
             {error, Reasons} = yawl_data:validate_data(BadData, ?TEST_SCHEMA),
             ?assertMatch([below_min], Reasons)
         end},

        {"Reject value above maximum",
         fun() ->
             BadData = ?TEST_DATA#{<<"amount">> => 2000000.0},
             {error, Reasons} = yawl_data:validate_data(BadData, ?TEST_SCHEMA),
             ?assertMatch([above_max], Reasons)
         end},

        {"Reject string too short",
         fun() ->
             BadData = ?TEST_DATA#{<<"order_id">> => <<"SHORT">>},
             {error, Reasons} = yawl_data:validate_data(BadData, ?TEST_SCHEMA),
             ?assertMatch([too_short], Reasons)
         end},

        {"Reject string pattern mismatch",
         fun() ->
             BadData = ?TEST_DATA#{<<"order_id">> => <<"INVALID123">>},
             {error, Reasons} = yawl_data:validate_data(BadData, ?TEST_SCHEMA),
             ?assertMatch([pattern_mismatch], Reasons)
         end},

        {"Allow optional undefined fields",
         fun() ->
             DataNoDiscount = maps:remove(<<"discount">>, ?TEST_DATA),
             {ok, Validated} = yawl_data:validate_data(DataNoDiscount, ?TEST_SCHEMA),
             ?assertEqual(DataNoDiscount, Validated)
         end},

        {"Allow extra fields not in schema",
         fun() ->
             ExtraData = ?TEST_DATA#{<<"extra">> => <<"value">>},
             {ok, Validated} = yawl_data:validate_data(ExtraData, ?TEST_SCHEMA),
             ?assertEqual(ExtraData, Validated)
         end},

        {"Return multiple validation errors",
         fun() ->
             {error, Reasons} = yawl_data:validate_data(?INVALID_DATA, ?TEST_SCHEMA),
             ?assertEqual(4, length(Reasons)),
             %% Check all error types are present
             ErrorTypes = lists:map(fun(E) ->
                 case E of
                     {undefined_required_variable, _} -> undefined_req;
                     {type_mismatch, _, _} -> type_mismatch;
                     below_min -> below_min;
                     too_short -> too_short;
                     _ -> other
                 end
             end, Reasons),
             ?assert(lists:member(undefined_req, ErrorTypes)),
             ?assert(lists:member(type_mismatch, ErrorTypes)),
             ?assert(lists:member(below_min, ErrorTypes)),
             ?assert(lists:member(too_short, ErrorTypes))
         end}
    ].

merge_data_test_() ->
    [
        {"Merge empty maps",
         fun() ->
             ?assertEqual(#{}, yawl_data:merge_data(#{}, #{}))
         end},

        {"Merge with new data",
         fun() ->
             Current = #{<<"a">> => 1, <<"b">> => 2},
             New = #{<<"c">> => 3},
             Merged = yawl_data:merge_data(Current, New),
             ?assertEqual(#{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3}, Merged)
         end},

        {"Merge with existing data update",
         fun() ->
             Current = #{<<"a">> => 1, <<"b">> => 2},
             New = #{<<"a">> => 10, <<"c">> => 3},
             Merged = yawl_data:merge_data(Current, New),
             ?assertEqual(#{<<"a">> => 10, <<"b">> => 2, <<"c">> => 3}, Merged)
         end}
    ].

get_variable_test_() ->
    [
        {"Get existing variable",
         fun() ->
             ?assertMatch({ok, 999.99}, yawl_data:get_variable(?TEST_DATA, <<"amount">>))
         end},

        {"Get non-existing variable",
         fun() ->
             ?assertEqual({error, undefined},
                         yawl_data:get_variable(?TEST_DATA, <<"nonexistent">>))
         end}
    ].

set_variable_test_() ->
    [
        {"Set new variable",
         fun() ->
             Result = yawl_data:set_variable(?TEST_DATA, <<"new_var">>, <<"value">>),
             Expected = ?TEST_DATA#{<<"new_var">> => <<"value">>},
             ?assertEqual(Expected, Result)
         end},

        {"Update existing variable",
         fun() ->
             Result = yawl_data:set_variable(?TEST_DATA, <<"amount">>, 1111.11),
             Expected = ?TEST_DATA#{<<"amount">> => 1111.11},
             ?assertEqual(Expected, Result)
         end}
    ].

to_marking_data_test_() ->
    [
        {"Convert data to marking tokens",
         fun() ->
             Tokens = yawl_data:to_marking_data(?TEST_DATA),
             ExpectedTokens = [
                 {data, <<"metadata">>, #{<<"source">> => <<"web">>,
                                        <<"priority">> => high}},
                 {data, <<"items">>, [#{<<"id">> => 1, <<"name">> => <<"item1">>,
                                      <<"price">> => 500.0}]},
                 {data, <<"discount">>, 10.0},
                 {data, <<"amount">>, 999.99},
                 {data, <<"customer_id">>, <<"cust_001">>},
                 {data, <<"order_id">>, <<"ORD123456">>}
             ],
             ?assertEqual(lists:sort(ExpectedTokens), lists:sort(Tokens))
         end},

        {"Handle empty data",
         fun() ->
             Tokens = yawl_data:to_marking_data(#{}),
             ?assertEqual([], Tokens)
         end}
    ].

%%====================================================================
%% Type Tests
%%====================================================================

type_tests_() ->
    [
        {"String type validation",
         fun() ->
             Constraints = [{type, binary}],
             ?assertEqual(ok, yawl_data:check_basic_type(<<"test">>, Constraints)),
             ?assertEqual(ok, yawl_data:check_basic_type("test", Constraints)),
             ?assertMatch({error, _}, yawl_data:check_basic_type(123, Constraints))
         end},

        {"Integer type validation",
         fun() ->
             Constraints = [{type, integer}],
             ?assertEqual(ok, yawl_data:check_basic_type(123, Constraints)),
             ?assertMatch({error, _}, yawl_data:check_basic_type(123.0, Constraints)),
             ?assertMatch({error, _}, yawl_data:check_basic_type(<<"123">>, Constraints))
         end},

        {"Float type validation",
         fun() ->
             Constraints = [{type, float}],
             ?assertEqual(ok, yawl_data:check_basic_type(123.0, Constraints)),
             ?assertEqual(ok, yawl_data:check_basic_type(123, Constraints)),  % int to float
             ?assertMatch({error, _}, yawl_data:check_basic_type(<<"123.0">>, Constraints))
         end},

        {"Boolean type validation",
         fun() ->
             Constraints = [{type, boolean}],
             ?assertEqual(ok, yawl_data:check_basic_type(true, Constraints)),
             ?assertEqual(ok, yawl_data:check_basic_type(false, Constraints)),
             ?assertMatch({error, _}, yawl_data:check_basic_type(<<"true">>, Constraints))
         end},

        {"Map type validation",
         fun() ->
             Constraints = [{type, map}],
             ?assertEqual(ok, yawl_data:check_basic_type(#{}, Constraints)),
             ?assertEqual(ok, yawl_data:check_basic_type(#{<<"a">> => 1}, Constraints)),
             ?assertMatch({error, _}, yawl_data:check_basic_type([1], Constraints))
         end},

        {"List type validation",
         fun() ->
             Constraints = [{type, list}],
             ?assertEqual(ok, yawl_data:check_basic_type([], Constraints)),
             ?assertEqual(ok, yawl_data:check_basic_type([1, 2, 3], Constraints)),
             ?assertMatch({error, _}, yawl_data:check_basic_type(#{}, Constraints))
         end}
    ].

constraint_tests_() ->
    [
        {"Min constraint for numbers",
         fun() ->
             ?assertEqual(ok, yawl_data:check_constraint(<<"var">>, 5.0, {min, 0})),
             ?assertEqual(ok, yawl_data:check_constraint(<<"var">>, 5, {min, 0})),
             ?assertMatch({error, _}, yawl_data:check_constraint(<<"var">>, -1, {min, 0}))
         end},

        {"Max constraint for numbers",
         fun() ->
             ?assertEqual(ok, yawl_data:check_constraint(<<"var">>, 100, {max, 200})),
             ?assertMatch({error, _}, yawl_data:check_constraint(<<"var">>, 300, {max, 200}))
         end},

        {"Min length for binary",
         fun() ->
             ?assertEqual(ok, yawl_data:check_constraint(<<"var">>, <<"abc">>, {min_length, 1})),
             ?assertEqual(ok, yawl_data:check_constraint(<<"var">>, <<"abc">>, {min_length, 3})),
             ?assertMatch({error, _}, yawl_data:check_constraint(<<"var">>, <<"a">>, {min_length, 3}))
         end},

        {"Max length for binary",
         fun() ->
             ?assertEqual(ok, yawl_data:check_constraint(<<"var">>, <<"abc">>, {max_length, 10})),
             ?assertMatch({error, _}, yawl_data:check_constraint(<<"var">>, <<"toolong">>, {max_length, 5}))
         end},

        {"Allowed values constraint",
         fun() ->
             ?assertEqual(ok, yawl_data:check_constraint(<<"var">>, active, {allowed, [active, inactive]})),
             ?assertMatch({error, _}, yawl_data:check_constraint(<<"var">>, pending, {allowed, [active, inactive]}))
         end},

        {"Pattern constraint",
         fun() ->
             ?assertEqual(ok, yawl_data:check_constraint(<<"var">>, <<"ORD123456">>,
                                                        {pattern, <<"^ORD\\d{6}$">>})),
             ?assertMatch({error, _}, yawl_data:check_constraint(<<"var">>, <<"ABC123456">>,
                                                              {pattern, <<"^ORD\\d{6}$">>}))
         end}
    ].

%%====================================================================
%% Helper Functions
%%====================================================================

%% Mock YAWL specification
mock_spec() ->
    %% Return a spec that wf_spec:task_params can handle
    #{nets => #{<<"net1">> => #{tasks => #{<<"task1">> => #{params =>
        [{<<"order_id">>, <<"ORD123456">>},
         {<<"customer_id">>, <<"cust_001">>}]
    }}}}}.