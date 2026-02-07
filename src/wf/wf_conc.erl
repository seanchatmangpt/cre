%% -*- erlang -*-
%%%% @doc Concuerror Runner Spec Generator
%%
%% This module generates specifications for Concuerror, the Erlang
%% concurrency bug finder. It does NOT run Concuerror itself - it only
%% builds specification maps that describe what should be run.
%%
%% Concuerror is a tool for systematically exploring interleavings of
%% concurrent Erlang processes to find race conditions, deadlocks, and
%% other concurrency bugs.
%%
%% <h3>Usage</h3>
%%
%% Generate a Concuerror spec:
%%
%% ```erlang
%% 1> Spec = wf_conc:spec(#{
%%    .. module => wf_store,
%%    .. entry => smoke,
%%    .. timeout_ms => 1000
%%    .. }).
%% 2> maps:get(tool, Spec).
%% concuerror
%% 3> is_list(maps:get(args, Spec)).
%% true
%% '''
%%
%% <h3>Spec Structure</h3>
%%
%% The returned spec map contains:
%% <ul>
%%   <li><b>tool:</b> Always <code>concuerror</code></li>
%%   <li><b>args:</b> List of command-line arguments for Concuerror</li>
%% </ul>
%%
%% <h3>Required Input Keys</h3>
%% <ul>
%%   <li><b>module:</b> The module containing the test function (atom)</li>
%%   <li><b>entry:</b> The test function to run (atom)</li>
%%   <li><b>timeout_ms:</b> Timeout in milliseconds (non_neg_integer)</li>
%% </ul>
%%
%% @end
%% -------------------------------------------------------------------

-module(wf_conc).

%%====================================================================
%% Exports
%%====================================================================

%% Spec generation
-export([spec/1]).

%% Doctests
-export([doctest_test/0]).

%%====================================================================
%% Types
%%%%====================================================================

%%--------------------------------------------------------------------
%% @doc Input specification for Concuerror test generation.
%%
%% Required keys:
%%   - module: The module containing the test function (atom)
%%   - entry: The test function to run (atom)
%%   - timeout_ms: Timeout in milliseconds (non_neg_integer)
%%--------------------------------------------------------------------
-type input_spec() :: #{
    module := atom(),
    entry := atom(),
    timeout_ms := non_neg_integer()
}.

%%--------------------------------------------------------------------
%% @doc Output specification describing what to run.
%%
%% Contains:
%%   - tool: Always 'concuerror'
%%   - args: List of command-line arguments for Concuerror
%%--------------------------------------------------------------------
-type output_spec() :: #{
    tool := concuerror,
    args := [string()]
}.

%%--------------------------------------------------------------------
%% @doc Error returned when input spec is invalid.
%%--------------------------------------------------------------------
-type error_reason() :: bad_spec | {missing_key, atom()} | {invalid_type, atom(), term()}.

-export_type([input_spec/0, output_spec/0, error_reason/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Generates a Concuerror specification map.
%%
%% Takes an input specification with required keys (module, entry, timeout_ms)
%% and returns a map describing what Concuerror should run.
%%
%% The returned map has:
%%   - tool: concuerror (identifies the tool to use)
%%   - args: Command-line arguments for Concuerror
%%
%% @param InputSpec Map with module, entry, and timeout_ms keys
%% @return #{tool => concuerror, args => [Args]} or {error, bad_spec}
%%
%% @end
%%--------------------------------------------------------------------
-spec spec(InputSpec :: input_spec()) -> output_spec() | {error, error_reason()}.

spec(#{module := Module, entry := Entry, timeout_ms := TimeoutMs})
  when is_atom(Module), is_atom(Entry), is_integer(TimeoutMs), TimeoutMs >= 0 ->
    %% Build Concuerror command-line arguments
    %% --module: The module to test
    %% --test: The test function to run
    %% --timeout: Maximum time to explore interleavings (in milliseconds)
    Args = [
        "--module", atom_to_list(Module),
        "--test", atom_to_list(Entry),
        "--timeout", integer_to_list(TimeoutMs)
    ],

    %% Return the spec map
    #{
        tool => concuerror,
        args => Args
    };

spec(_InputSpec) ->
    %% Input spec missing required keys or has wrong types
    {error, bad_spec}.

%%====================================================================
%% Doctests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Runs doctests from the module documentation.
%%
%% Tests all examples from moduledoc and function docs.
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.

doctest_test() ->
    %% Test 1: Valid spec returns proper structure
    Spec1 = spec(#{module => wf_store, entry => smoke, timeout_ms => 1000}),
    true = is_map(Spec1),
    concuerror = maps:get(tool, Spec1),
    true = is_list(maps:get(args, Spec1)),

    %% Test 2: Empty map returns error
    {error, bad_spec} = spec(#{}),

    %% Test 3: Spec with only module returns error (missing entry, timeout_ms)
    {error, bad_spec} = spec(#{module => wf_store}),

    %% Test 4: Spec with module and entry but no timeout returns error
    {error, bad_spec} = spec(#{module => wf_store, entry => smoke}),

    %% Test 5: Spec with all keys but wrong type for timeout returns error
    {error, bad_spec} = spec(#{module => wf_store, entry => smoke, timeout_ms => -1}),

    %% Test 6: Verify args list content
    Spec2 = spec(#{module => test_module, entry => test_func, timeout_ms => 5000}),
    Args = maps:get(args, Spec2),
    true = lists:member("--module", Args),
    true = lists:member("--test", Args),
    true = lists:member("--timeout", Args),
    true = lists:member("test_module", Args),
    true = lists:member("test_func", Args),
    true = lists:member("5000", Args),

    %% Test 7: Zero timeout is valid
    Spec3 = spec(#{module => wf_store, entry => quick_test, timeout_ms => 0}),
    concuerror = maps:get(tool, Spec3),
    true = is_list(maps:get(args, Spec3)),

    ok.
