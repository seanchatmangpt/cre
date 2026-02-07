%% -*- erlang -*-
%%%% @doc Workflow execution helpers for YAWL compiled nets.
%%
%% This module provides utilities for executing compiled YAWL workflows
%% with gen_pnet. It offers a high-level API for starting workflows,
%% injecting input data, executing steps, draining workflows to completion,
%% and extracting results.
%%
%% == Basic Workflow Execution ==
%%
%% ```erlang
%% > {ok, Pid} = yawl_execution:start_link(my_workflow, #{}).
%% {ok, <0.123.0>}
%%
%% > {ok, Receipts} = yawl_execution:inject_input(Pid, #{input => [data]}).
%% {ok, []}
%%
%% > {ok, StepReceipt} = yawl_execution:execute_step(Pid).
%% {ok, #{trsn => t1, mode => #{}, produce => #{output => [result]}}}
%%
%% > {ok, Result} = yawl_execution:get_result(Pid, output).
%% {ok, [result]}
%% ```
%%
%% == Execute Until Complete ==
%%
%% ```erlang
%% > {ok, Pid} = yawl_execution:start_workflow(my_workflow, #{}, #{}).
%% {ok, <0.124.0>}
%%
%% > {ok, Receipts} = yawl_execution:execute_until_complete(Pid, 100).
%% {ok, [#{trsn => t1, ...}, #{trsn => t2, ...}]}
%%
%% > {ok, FinalMarking} = yawl_execution:drain_workflow(Pid, 10).
%% {ok, #{final => [done]}}
%% ```
%% @end
%% -------------------------------------------------------------------

-module(yawl_execution).

%%====================================================================
%% Includes
%%====================================================================

-include("../../include/gen_pnet.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% Workflow lifecycle
-export([start_link/2, start_link/3]).
-export([start_workflow/2, start_workflow/3]).
-export([stop/1]).

%% Token injection and execution
-export([inject_input/2, execute_step/1]).
-export([drain_workflow/2, get_result/2]).
-export([execute_until_complete/2]).

%% State inspection
-export([get_marking/1, is_quiescent/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A gen_pnet name or pid reference.
%%--------------------------------------------------------------------
-type name() :: atom() |
                {local, atom()} |
                {global, atom()} |
                {via, atom(), term()} |
                pid().

%%--------------------------------------------------------------------
%% @doc A produce map for token injection.
%%
%% Maps place atoms to lists of tokens to be added to those places.
%%--------------------------------------------------------------------
-type produce_map() :: #{atom() => [term()]}.

%%--------------------------------------------------------------------
%% @doc A receipt from a transition firing.
%%
%% Contains the transition fired, the mode under which it fired,
%% and the tokens produced.
%%--------------------------------------------------------------------
-type receipt() :: #{trsn := atom(),
                     mode => #{atom() => [term()]},
                     produce := produce_map()}.

%%--------------------------------------------------------------------
%% @doc Workflow execution result.
%%
%% Structured result from workflow operations containing receipts
%% and final marking.
%%--------------------------------------------------------------------
-type execution_result() :: {ok, [receipt()], #{atom() => [term()]}} |
                            {error, term()}.

%%--------------------------------------------------------------------
%% @doc Net argument passed to workflow module init/1.
%%
%% User-defined initialization data for the workflow.
%%--------------------------------------------------------------------
-type net_arg() :: term().

%%--------------------------------------------------------------------
%% @doc Options for gen_pnet start.
%%
%% Standard gen_server options like timeout, debug, spawn_opt.
%%--------------------------------------------------------------------
-type options() :: [term()].

%% Export types
-export_type([name/0, produce_map/0, receipt/0, execution_result/0,
              net_arg/0, options/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts an unregistered workflow instance.
%%
%% Creates a new gen_pnet process for the compiled YAWL workflow
%% without registering it. The NetArg is passed to the module's
%% init/1 callback.
%%
%% ```erlang
%% > {ok, Pid} = yawl_execution:start_link(my_workflow, #{}).
%% {ok, <0.123.0>}
%% '''
%%
%% @param NetMod The compiled workflow module (implementing gen_pnet callbacks)
%% @param NetArg Argument passed to the module's init/1 callback
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(NetMod :: atom(), NetArg :: net_arg()) ->
          {ok, pid()} | {error, term()}.

start_link(NetMod, NetArg) when is_atom(NetMod) ->
    gen_pnet:start_link(NetMod, NetArg, []).

%%--------------------------------------------------------------------
%% @doc Starts a registered workflow instance.
%%
%% Creates a new gen_pnet process and registers it as ServerName.
%% ServerName follows gen_server conventions: {local, Name},
%% {global, Name}, or {via, Module, ViaName}.
%%
%% ```erlang
%% > {ok, Pid} = yawl_execution:start_link({local, my_wf}, my_workflow, #{}).
%% {ok, <0.124.0>}
%% '''
%%
%% @param ServerName Registration name for the process
%% @param NetMod The compiled workflow module
%% @param NetArg Argument passed to the module's init/1 callback
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(ServerName :: gen_pnet:server_name(),
                 NetMod :: atom(),
                 NetArg :: net_arg()) ->
          {ok, pid()} | {error, term()}.

start_link(ServerName, NetMod, NetArg) when is_atom(NetMod) ->
    gen_pnet:start_link(ServerName, NetMod, NetArg, []).

%%--------------------------------------------------------------------
%% @doc Starts a workflow with a new process (unregistered).
%%
%% Convenience function that starts a workflow and returns the pid.
%% Equivalent to start_link/2 but with a more semantic name.
%%
%% ```erlang
%% > {ok, Pid} = yawl_execution:start_workflow(my_workflow, #{}).
%% {ok, <0.125.0>}
%% '''
%%
%% @param NetMod The compiled workflow module
%% @param NetArg Argument passed to the module's init/1 callback
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_workflow(NetMod :: atom(), NetArg :: net_arg()) ->
          {ok, pid()} | {error, term()}.

start_workflow(NetMod, NetArg) ->
    start_link(NetMod, NetArg).

%%--------------------------------------------------------------------
%% @doc Starts a workflow with a new process (registered).
%%
%% Convenience function that starts a registered workflow.
%% Equivalent to start_link/3 but with a more semantic name.
%%
%% ```erlang
%% > {ok, Pid} = yawl_execution:start_workflow({local, my_wf}, my_workflow, #{}).
%% {ok, <0.126.0>}
%% '''
%%
%% @param ServerName Registration name for the process
%% @param NetMod The compiled workflow module
%% @param NetArg Argument passed to the module's init/1 callback
%% @return {ok, Pid} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_workflow(ServerName :: gen_pnet:server_name(),
                     NetMod :: atom(),
                     NetArg :: net_arg()) ->
          {ok, pid()} | {error, term()}.

start_workflow(ServerName, NetMod, NetArg) ->
    start_link(ServerName, NetMod, NetArg).

%%--------------------------------------------------------------------
%% @doc Stops a workflow instance.
%%
%% Gracefully terminates the gen_pnet process.
%%
%% ```erlang
%% > ok = yawl_execution:stop(Pid).
%% ok
%% '''
%%
%% @param Name The workflow process (pid, atom, or server name)
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Name :: name()) -> ok.

stop(Name) ->
    gen_pnet:stop(Name).

%%--------------------------------------------------------------------
%% @doc Injects input tokens into the workflow.
%%
%% Adds tokens to places specified in the ProduceMap. This is the
%% primary way to provide input data to a running workflow.
%%
%% Returns {ok, Receipt} with the injection receipt, or {error, Reason}
%% if the injection fails.
%%
%% ```erlang
%% > {ok, Receipt} = yawl_execution:inject_input(Pid, #{input => [data]}).
%% {ok, #{input => [data]}}
%% '''
%%
%% @param Name The workflow process
%% @param ProduceMap Map of places to token lists to inject
%% @return {ok, Receipt} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec inject_input(Name :: name(), ProduceMap :: produce_map()) ->
          {ok, produce_map()} | {error, term()}.

inject_input(Name, ProduceMap) when is_map(ProduceMap) ->
    try gen_pnet:inject(Name, ProduceMap) of
        Result -> Result
    catch
        exit:{noproc, _} -> {error, no_process};
        exit:{normal, _} -> {error, terminated};
        exit:{{shutdown, _}, _} -> {error, shutdown};
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Executes a single step of the workflow.
%%
%% Attempts to fire exactly one enabled transition. Returns the receipt
%% from the fired transition, or abort if no transition is enabled.
%%
%% ```erlang
%% > {ok, Receipt} = yawl_execution:execute_step(Pid).
%% {ok, #{trsn => t1, mode => #{}, produce => #{output => [result]}}}
%%
%% > abort = yawl_execution:execute_step(Pid).
%% abort
%% '''
%%
%% @param Name The workflow process
%% @return {ok, Receipt} | abort | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_step(Name :: name()) -> {ok, receipt()} | abort | {error, term()}.

execute_step(Name) ->
    try gen_pnet:step(Name) of
        abort -> abort;
        {ok, Receipt} -> {ok, Receipt}
    catch
        exit:{noproc, _} -> {error, no_process};
        exit:{normal, _} -> {error, terminated};
        exit:{{shutdown, _}, _} -> {error, shutdown};
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Drains the workflow until quiescent or MaxSteps reached.
%%
%% Repeatedly fires enabled transitions, collecting receipts from each
%% firing. Stops when no transitions are enabled or MaxSteps is reached.
%%
%% Returns {ok, Receipts} with receipts in firing order, or
%% {error, limit} if MaxSteps was reached before quiescence.
%%
%% ```erlang
%% > {ok, Receipts} = yawl_execution:drain_workflow(Pid, 100).
%% {ok, [#{trsn => t1, ...}, #{trsn => t2, ...}]}
%%
%% > {error, limit} = yawl_execution:drain_workflow(Pid, 5).
%% {error, limit}
%% '''
%%
%% @param Name The workflow process
%% @param MaxSteps Maximum number of transitions to fire
%% @return {ok, [Receipt]} | {error, limit | term()}
%%
%% @end
%%--------------------------------------------------------------------
-spec drain_workflow(Name :: name(), MaxSteps :: non_neg_integer()) ->
          {ok, [receipt()]} | {error, term()}.

drain_workflow(Name, MaxSteps) when is_integer(MaxSteps), MaxSteps >= 0 ->
    try gen_pnet:drain(Name, MaxSteps) of
        {ok, Receipts} -> {ok, Receipts};
        {error, limit} -> {error, limit}
    catch
        exit:{noproc, _} -> {error, no_process};
        exit:{normal, _} -> {error, terminated};
        exit:{{shutdown, _}, _} -> {error, shutdown};
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Gets the tokens at a specific place.
%%
%% Returns {ok, Tokens} where Tokens is the list at the place.
%% Returns {error, bad_place} if the place does not exist in the net.
%%
%% ```erlang
%% > {ok, Tokens} = yawl_execution:get_result(Pid, output).
%% {ok, [result]}
%%
%% > {error, bad_place} = yawl_execution:get_result(Pid, nonexistent).
%% {error, bad_place}
%% '''
%%
%% @param Name The workflow process
%% @param Place The place atom to query
%% @return {ok, [term()]} | {error, bad_place | term()}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_result(Name :: name(), Place :: atom()) ->
          {ok, [term()]} | {error, term()}.

get_result(Name, Place) when is_atom(Place) ->
    try gen_pnet:ls(Name, Place) of
        {ok, _Tokens} = Result -> Result;
        {error, #bad_place{}} -> {error, bad_place}
    catch
        exit:{noproc, _} -> {error, no_process};
        exit:{normal, _} -> {error, terminated};
        exit:{{shutdown, _}, _} -> {error, shutdown};
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Executes the workflow until completion.
%%
%% Convenience function that drains the workflow and returns the
%% receipts. Returns {ok, Receipts} if the workflow reaches quiescence
%% within MaxSteps, or {error, limit} if not.
%%
%% ```erlang
%% > {ok, Receipts} = yawl_execution:execute_until_complete(Pid, 1000).
%% {ok, [#{trsn => t1, ...}, #{trsn => t2, ...}, #{trsn => t3, ...}]}
%% '''
%%
%% @param Name The workflow process
%% @param MaxSteps Maximum number of transitions to fire
%% @return {ok, [Receipt]} | {error, limit | term()}
%%
%% @end
%%--------------------------------------------------------------------
-spec execute_until_complete(Name :: name(), MaxSteps :: non_neg_integer()) ->
          {ok, [receipt()]} | {error, term()}.

execute_until_complete(Name, MaxSteps) ->
    drain_workflow(Name, MaxSteps).

%%--------------------------------------------------------------------
%% @doc Gets the complete marking of the workflow.
%%
%% Returns a map of all places to their current token lists.
%% Useful for inspecting the complete state of a workflow.
%%
%% ```erlang
%% > Marking = yawl_execution:get_marking(Pid).
%% #{input => [], output => [result], working => []}
%% '''
%%
%% @param Name The workflow process
%% @return Marking map or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_marking(Name :: name()) ->
          #{atom() => [term()]} | {error, term()}.

get_marking(Name) ->
    try gen_pnet:marking(Name) of
        Marking when is_map(Marking) -> Marking
    catch
        exit:{noproc, _} -> {error, no_process};
        exit:{normal, _} -> {error, terminated};
        exit:{{shutdown, _}, _} -> {error, shutdown};
        Error:Reason:Stack ->
            {error, {Error, Reason, Stack}}
    end.

%%--------------------------------------------------------------------
%% @doc Checks if the workflow is quiescent (no enabled transitions).
%%
%% Returns true if no transitions are enabled, false otherwise.
%% A quiescent workflow has reached a stable state where no further
%% automatic progress can be made.
%%
%% ```erlang
%% > true = yawl_execution:is_quiescent(Pid).
%% true
%% '''
%%
%% @param Name The workflow process
%% @return boolean()
%%
%% @end
%%--------------------------------------------------------------------
-spec is_quiescent(Name :: name()) -> boolean().

is_quiescent(Name) ->
    case execute_step(Name) of
        abort -> true;
        {ok, _Receipt} -> false;
        {error, _} -> true
    end.

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.

%% Unit tests for basic workflow execution operations

start_stop_test() ->
    {ok, Pid} = start_link(wf_test_net_basic, #{}),
    ?assert(is_pid(Pid)),
    ?assertEqual(ok, stop(Pid)).

start_workflow_test() ->
    {ok, Pid} = start_workflow(wf_test_net_basic, #{}),
    ?assert(is_pid(Pid)),
    ?assertEqual(ok, stop(Pid)).

get_marking_test() ->
    {ok, Pid} = start_link(wf_test_net_basic, #{}),
    Marking = get_marking(Pid),
    ?assert(is_map(Marking)),
    ?assert(maps:is_key(p, Marking)),
    ?assertEqual([], maps:get(p, Marking)),
    stop(Pid).

get_result_test() ->
    {ok, Pid} = start_link(wf_test_net_basic, #{}),
    ?assertEqual({ok, []}, get_result(Pid, p)),
    ?assertMatch({error, bad_place}, get_result(Pid, nonexistent)),
    stop(Pid).

inject_input_test() ->
    {ok, Pid} = start_link(wf_test_net_basic, #{}),
    ?assertMatch({ok, _}, inject_input(Pid, #{p => [token]})),
    %% Verify token was injected
    {ok, Marking} = {ok, get_marking(Pid)},
    ?assert(lists:member(token, maps:get(p, Marking))),
    stop(Pid).

execute_step_abort_test() ->
    %% No tokens, so no transitions should be enabled
    {ok, Pid} = start_link(wf_test_net_basic, #{}),
    ?assertEqual(abort, execute_step(Pid)),
    stop(Pid).

execute_step_with_token_test() ->
    {ok, Pid} = start_link(wf_test_net_basic, #{}),
    %% Inject a token to enable t1
    {ok, _} = inject_input(Pid, #{p => [token]}),
    %% Should be able to fire t1 now
    ?assertMatch({ok, _}, execute_step(Pid)),
    %% After firing, place p should be empty
    {ok, Marking} = {ok, get_marking(Pid)},
    ?assertEqual([], maps:get(p, Marking)),
    stop(Pid).

drain_workflow_test() ->
    {ok, Pid} = start_link(wf_test_net_basic, #{}),
    %% Inject tokens
    {ok, _} = inject_input(Pid, #{p => [t1]}),
    %% Drain should fire one transition
    ?assertMatch({ok, [_]}, drain_workflow(Pid, 10)),
    stop(Pid).

drain_workflow_limit_test() ->
    {ok, Pid} = start_link(wf_test_net_basic, #{}),
    %% Inject many tokens
    {ok, _} = inject_input(Pid, #{p => [a, b, c, d, e, f]}),
    %% Limit to 3 steps, should hit limit
    ?assertEqual({error, limit}, drain_workflow(Pid, 3)),
    stop(Pid).

is_quiescent_test() ->
    {ok, Pid} = start_link(wf_test_net_basic, #{}),
    ?assert(is_quiescent(Pid)),
    %% Add a token to enable transition
    {ok, _} = inject_input(Pid, #{p => [token]}),
    ?assertNot(is_quiescent(Pid)),
    stop(Pid).

execute_until_complete_test() ->
    {ok, Pid} = start_link(wf_test_net_basic, #{}),
    {ok, _} = inject_input(Pid, #{p => [token]}),
    ?assertMatch({ok, [_]}, execute_until_complete(Pid, 10)),
    stop(Pid).

error_handling_noproc_test() ->
    %% Use a dead pid - self() triggers calling_self; dead pid gives no_process or terminated
    DeadPid = spawn(fun() -> ok end),
    ?assert(lists:member(get_marking(DeadPid), [{error, no_process}, {error, terminated}])),
    ?assert(lists:member(execute_step(DeadPid), [abort, {error, no_process}, {error, terminated}])),
    ?assert(lists:member(inject_input(DeadPid, #{p => [a]}), [{error, no_process}, {error, terminated}])),
    ?assert(lists:member(get_result(DeadPid, p), [{error, no_process}, {error, terminated}])).

-endif.
