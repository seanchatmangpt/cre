%% Test helper records for yawl_schema tests
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