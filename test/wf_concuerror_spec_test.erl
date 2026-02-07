%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @doc wf_conc spec/1 Concuerror Spec Generation Tests
%%
%% Test Slice 8 of compound doctests.
%%
%% Verifies:
%% 1. wf_conc:spec/1 generates valid spec map
%% 2. Spec has tool => concuerror
%% 3. Spec has args list
%% 4. Invalid input returns {error, bad_spec}
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_concuerror_spec_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% EUnit Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test that wf_conc:spec/1 generates a valid spec map.
%%
%% Verifies the spec map structure with tool and args keys.
%%--------------------------------------------------------------------
spec_generates_valid_map_test() ->
    Spec = wf_conc:spec(#{
        module => wf_store,
        entry  => claim_race,
        timeout_ms => 2000
    }),
    ?assert(is_map(Spec)),
    ?assertEqual(concuerror, maps:get(tool, Spec)),
    ?assert(is_list(maps:get(args, Spec))),
    ?assert(maps:is_key(tool, Spec)),
    ?assert(maps:is_key(args, Spec)).

%%--------------------------------------------------------------------
%% @doc Test that spec contains all required Concuerror arguments.
%%
%% The args list should contain --module, --test, and --timeout flags
%% along with their respective values.
%%--------------------------------------------------------------------
spec_contains_required_args_test() ->
    Spec = wf_conc:spec(#{
        module => wf_store,
        entry  => claim_race,
        timeout_ms => 2000
    }),
    Args = maps:get(args, Spec),
    ?assert(lists:member("--module", Args)),
    ?assert(lists:member("--test", Args)),
    ?assert(lists:member("--timeout", Args)),
    ?assert(lists:member("wf_store", Args)),
    ?assert(lists:member("claim_race", Args)),
    ?assert(lists:member("2000", Args)).

%%--------------------------------------------------------------------
%% @doc Test that invalid input (empty map) returns {error, bad_spec}.
%%--------------------------------------------------------------------
spec_empty_map_returns_error_test() ->
    ?assertEqual({error, bad_spec}, wf_conc:spec(#{})).

%%--------------------------------------------------------------------
%% @doc Test that missing module key returns {error, bad_spec}.
%%--------------------------------------------------------------------
spec_missing_module_returns_error_test() ->
    ?assertEqual({error, bad_spec}, wf_conc:spec(#{
        entry => smoke,
        timeout_ms => 1000
    })).

%%--------------------------------------------------------------------
%% @doc Test that missing entry key returns {error, bad_spec}.
%%--------------------------------------------------------------------
spec_missing_entry_returns_error_test() ->
    ?assertEqual({error, bad_spec}, wf_conc:spec(#{
        module => wf_store,
        timeout_ms => 1000
    })).

%%--------------------------------------------------------------------
%% @doc Test that missing timeout_ms key returns {error, bad_spec}.
%%--------------------------------------------------------------------
spec_missing_timeout_returns_error_test() ->
    ?assertEqual({error, bad_spec}, wf_conc:spec(#{
        module => wf_store,
        entry => smoke
    })).

%%--------------------------------------------------------------------
%% @doc Test that negative timeout_ms returns {error, bad_spec}.
%%--------------------------------------------------------------------
spec_negative_timeout_returns_error_test() ->
    ?assertEqual({error, bad_spec}, wf_conc:spec(#{
        module => wf_store,
        entry => smoke,
        timeout_ms => -1
    })).

%%--------------------------------------------------------------------
%% @doc Test that zero timeout_ms is valid.
%%--------------------------------------------------------------------
spec_zero_timeout_is_valid_test() ->
    Spec = wf_conc:spec(#{
        module => wf_store,
        entry => quick_test,
        timeout_ms => 0
    }),
    ?assertEqual(concuerror, maps:get(tool, Spec)),
    ?assert(lists:member("0", maps:get(args, Spec))).

%%--------------------------------------------------------------------
%% @doc Test that wrong type for module returns {error, bad_spec}.
%%--------------------------------------------------------------------
spec_invalid_module_type_returns_error_test() ->
    ?assertEqual({error, bad_spec}, wf_conc:spec(#{
        module => "wf_store",  %% Should be atom
        entry => smoke,
        timeout_ms => 1000
    })).

%%--------------------------------------------------------------------
%% @doc Test that wrong type for entry returns {error, bad_spec}.
%%--------------------------------------------------------------------
spec_invalid_entry_type_returns_error_test() ->
    ?assertEqual({error, bad_spec}, wf_conc:spec(#{
        module => wf_store,
        entry => "smoke",  %% Should be atom
        timeout_ms => 1000
    })).

%%--------------------------------------------------------------------
%% @doc Test that wrong type for timeout_ms returns {error, bad_spec}.
%%--------------------------------------------------------------------
spec_invalid_timeout_type_returns_error_test() ->
    ?assertEqual({error, bad_spec}, wf_conc:spec(#{
        module => wf_store,
        entry => smoke,
        timeout_ms => "1000"  %% Should be integer
    })).

%%--------------------------------------------------------------------
%% @doc Test args list order for different modules.
%%
%% Ensures the spec generation works for arbitrary module/function names.
%%--------------------------------------------------------------------
spec_different_modules_test() ->
    Spec1 = wf_conc:spec(#{
        module => test_module_a,
        entry => test_func_x,
        timeout_ms => 5000
    }),
    Args1 = maps:get(args, Spec1),
    ?assert(lists:member("test_module_a", Args1)),
    ?assert(lists:member("test_func_x", Args1)),
    ?assert(lists:member("5000", Args1)),

    %% atom_to_list converts atoms to strings without quotes
    Spec2 = wf_conc:spec(#{
        module => 'another-module',
        entry => 'another-test',
        timeout_ms => 10000
    }),
    Args2 = maps:get(args, Spec2),
    ?assert(lists:member("another-module", Args2)),
    ?assert(lists:member("another-test", Args2)),
    ?assert(lists:member("10000", Args2)).

%%--------------------------------------------------------------------
%% @doc Test that spec describes a runnable test without executing it.
%%
%% This test verifies the spec structure without actually running Concuerror.
%% The spec should have all the information needed to run Concuerror.
%%--------------------------------------------------------------------
spec_describes_runnable_test_test() ->
    Spec = wf_conc:spec(#{
        module => wf_store,
        entry  => claim_race,
        timeout_ms => 2000
    }),
    %% Verify spec has all components needed to run Concuerror
    Tool = maps:get(tool, Spec),
    Args = maps:get(args, Spec),

    %% Tool identifies the execution method
    ?assertEqual(concuerror, Tool),

    %% Args should be a proper command-line argument list
    ?assert(is_list(Args)),
    ?assert(length(Args) > 0),

    %% Args should be in flag-value pairs: [Flag1, Val1, Flag2, Val2, ...]
    ?assert(length(Args) rem 2 =:= 0),

    %% Verify flag positions
    ?assertEqual("--module", lists:nth(1, Args)),
    ?assertEqual("--test", lists:nth(3, Args)),
    ?assertEqual("--timeout", lists:nth(5, Args)).

%%--------------------------------------------------------------------
%% @doc Test doctest example from moduledoc.
%%
%% This is the exact flow example from the module documentation.
%%--------------------------------------------------------------------
doctest_example_test() ->
    Spec = wf_conc:spec(#{
        module => wf_store,
        entry  => smoke,
        timeout_ms => 1000
    }),
    ?assert(is_map(Spec)),
    ?assertEqual(concuerror, maps:get(tool, Spec)),
    ?assert(is_list(maps:get(args, Spec))),
    ?assert(lists:member("--module", maps:get(args, Spec))),
    ?assert(lists:member("--test", maps:get(args, Spec))),
    ?assert(lists:member("--timeout", maps:get(args, Spec))).
