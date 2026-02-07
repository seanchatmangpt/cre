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

-module(wf_prop).

-moduledoc """
PropEr test harness wrapper for property-based testing.

Provides a convenient wrapper around PropEr's quickcheck functionality
for running property-based tests with configurable options.

```erlang
> Outer =
..   proper:numtests(10,
..     proper:test_to_outer_test(
..       proper:equals(1, 1))).
> proper:quickcheck(Outer, [quiet]).
true
```
""".

%%====================================================================
%% Exports
%%====================================================================

-export([quickcheck/2, numtests/2]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A PropEr test specification.
%%
%% Can be a raw test wrapped with test_to_outer_test or
%% already wrapped with numtests.
%%--------------------------------------------------------------------
-type proper_test() :: term().

%%--------------------------------------------------------------------
%% @doc PropEr quickcheck options.
%%
%% Common options include:
%% - quiet: Suppress output
%% - {on_output, fun}: Custom output handler
%% - long_result: Return detailed result
%%--------------------------------------------------------------------
-type proper_options() :: [proper:opt()].

%% Export types
-export_type([proper_test/0, proper_options/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs a PropEr test with given options.
%%
%% Thin wrapper around proper:quickcheck/2 that handles
%% optional error handling when PropEr is not available.
%%
%% @param Test The test to run (proper_test format)
%% @param Options List of PropEr options (e.g., [quiet])
%% @return true if test passes, {error, Reason} if PropEr unavailable
%%
%% @end
%%--------------------------------------------------------------------
-spec quickcheck(Test :: proper_test(), Options :: proper_options()) ->
          true | {error, term()}.

quickcheck(Test, Options) when is_list(Options) ->
    case code:which(proper) of
        non_existing ->
            %% PropEr not available
            {error, proper_not_available};
        _ ->
            %% Use proper:quickcheck
            try proper:quickcheck(Test, Options)
            catch error:undef ->
                %% Function not available (old version?)
                {error, proper_undef}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Sets the number of test cases for a PropEr test.
%%
%% Wrapper around proper:numtests/2 that applies the test count
%% to a given test specification.
%%
%% @param N Number of tests to run (positive integer)
%% @param Test The test to wrap
%% @return Wrapped test with N test cases, or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec numtests(N :: pos_integer(), Test :: proper_test()) ->
          proper_test() | {error, term()}.

numtests(N, Test) when is_integer(N), N > 0 ->
    case code:which(proper) of
        non_existing ->
            %% PropEr not available
            {error, proper_not_available};
        _ ->
            %% Use proper:numtests
            try proper:numtests(N, Test)
            catch error:undef ->
                %% Function not available (old version?)
                {error, proper_undef}
            end
    end;
numtests(_BadN, _Test) ->
    error(badarg).

%%====================================================================
%% Internal Functions
%%====================================================================

%% No internal functions - this is a pure wrapper module.

%%--------------------------------------------------------------------
%% EUnit Tests
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% @doc EUnit test runner for the module.
%% Tests the wrapper behavior (without requiring PropEr).
%%--------------------------------------------------------------------
doctest_test() ->
    %% When PropEr is not available, numtests returns error
    case code:which(proper) of
        non_existing ->
            %% Test numtests with proper not available
            ?assertEqual({error, proper_not_available}, numtests(10, dummy_test)),
            ?assertEqual({error, proper_not_available},
                quickcheck(dummy_test, []));

        _ ->
            %% PropEr is available - test with actual proper calls
            Outer = proper:numtests(10,
                proper:test_to_outer_test(
                    proper:equals(1, 1))),
            ?assertEqual(true, proper:quickcheck(Outer, [quiet])),

            %% Test our wrappers
            ?assertEqual(true, quickcheck(Outer, [quiet])),
            ?assert(is_tuple(numtests(5, proper:equals(2, 2))))
    end,

    ok.

%%--------------------------------------------------------------------
%% @doc Test numtests/2 with valid N.
%%--------------------------------------------------------------------
numtests_valid_n_test() ->
    case code:which(proper) of
        non_existing ->
            ?assertEqual({error, proper_not_available}, numtests(1, test)),
            ?assertEqual({error, proper_not_available}, numtests(100, test));
        _ ->
            Result = numtests(5, proper:equals(true, true)),
            ?assert(is_tuple(Result))
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc Test numtests/2 with invalid N.
%%--------------------------------------------------------------------
numtests_invalid_n_test() ->
    ?assertError(badarg, numtests(0, test)),
    ?assertError(badarg, numtests(-1, test)),
    ?assertError(badarg, numtests(abc, test)),
    ok.

%%--------------------------------------------------------------------
%% @doc Test quickcheck/2 error handling.
%%--------------------------------------------------------------------
quickcheck_options_test() ->
    case code:which(proper) of
        non_existing ->
            ?assertEqual({error, proper_not_available},
                quickcheck(test, [])),
            ?assertEqual({error, proper_not_available},
                quickcheck(test, [quiet]));
        _ ->
            Outer = proper:numtests(1,
                proper:test_to_outer_test(
                    proper:equals(ok, ok))),
            ?assertEqual(true, quickcheck(Outer, [quiet])),
            ?assertEqual(true, quickcheck(Outer, []))
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc Test quickcheck/2 with various option formats.
%%--------------------------------------------------------------------
quickcheck_various_options_test() ->
    case code:which(proper) of
        non_existing ->
            ?assertEqual({error, proper_not_available},
                quickcheck(test, [quiet, long_result])),
            ?assertEqual({error, proper_not_available},
                quickcheck(test, []));
        _ ->
            Outer = proper:numtests(1,
                proper:test_to_outer_test(
                    proper:equals(1, 1))),
            ?assert(is_boolean(quickcheck(Outer, [quiet]))),
            ?assert(is_boolean(quickcheck(Outer, [])))
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc Test that both functions handle empty options.
%%--------------------------------------------------------------------
empty_options_test() ->
    case code:which(proper) of
        non_existing ->
            ?assertEqual({error, proper_not_available},
                quickcheck(test, []));
        _ ->
            Outer = proper:numtests(1,
                proper:test_to_outer_test(
                    proper:equals(true, true))),
            ?assertEqual(true, quickcheck(Outer, []))
    end,
    ok.

%%--------------------------------------------------------------------
%% @doc Test large numtests value.
%%--------------------------------------------------------------------
large_numtests_test() ->
    case code:which(proper) of
        non_existing ->
            ?assertEqual({error, proper_not_available},
                numtests(10000, test));
        _ ->
            %% Should handle large N gracefully
            Result = numtests(1000, proper:equals(1, 1)),
            ?assert(is_tuple(Result))
    end,
    ok.

-endif.
