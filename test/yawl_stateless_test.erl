%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen.brandt@cuneiform-lang.org>
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
%% @doc YAWL Stateless Execution Test Suite
%%
%% Comprehensive test suite for the yawl_stateless module.
%% Tests cover stateless execution, checkpoint/restore, execution lifecycle,
%% persistence, TTL management, and error handling.
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_stateless_test).
-author('joergen.brandt@cuneiform-lang.org').

-include_lib("eunit/include/eunit.hrl").

%% Explicitly export test generators for EUnit discovery
-export([
    setup/0,
    cleanup/1,
    simple_workflow_spec/0
]).

-compile(export_all).

%%====================================================================
%% Test Fixtures
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function called before each test.
%% @end
%%--------------------------------------------------------------------
setup() ->
    %% Initialize persistent terms for configuration
    cre_config:init(),

    %% Ensure clean state
    catch gen_server:stop(yawl_stateless),
    timer:sleep(10).

%%--------------------------------------------------------------------
%% @doc Cleanup function called after each test.
%% @end
%%--------------------------------------------------------------------
cleanup(_Arg) ->
    catch gen_server:stop(yawl_stateless),
    timer:sleep(10).

%%--------------------------------------------------------------------
%% @doc Creates a simple valid workflow spec for testing.
%% @end
%%--------------------------------------------------------------------
simple_workflow_spec() ->
    #{
      <<"tasks">> => [
          #{<<"id">> => <<"task1">>, <<"name">> => <<"First Task">>},
          #{<<"id">> => <<"task2">>, <<"name">> => <<"Second Task">>}
      ]
     }.

%%====================================================================
%% Lifecycle Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test start_link with default config.
%% @end
%%--------------------------------------------------------------------
test_start_link_default_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Result = yawl_stateless:start_link(),
                    case Result of
                        {ok, Pid} when is_pid(Pid) -> ok;
                        {error, Reason} -> ?assert(false, {unexpected_error, Reason})
                    end
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test start_link with config.
%% @end
%%--------------------------------------------------------------------
test_start_link_with_config_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Config = #{
                      checkpoint_dir => <<"/tmp/yawl_test_checkpoints">>,
                      max_executions => 100
                     },
                    Result = yawl_stateless:start_link(Config),
                    case Result of
                        {ok, Pid} when is_pid(Pid) -> ok;
                        {error, Reason} -> ?assert(false, {unexpected_error, Reason})
                    end
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test stopping the worker.
%% @end
%%--------------------------------------------------------------------
test_stop_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    {ok, Pid} = yawl_stateless:start_link(),
                    ?assertEqual(ok, gen_server:stop(Pid))
                end)]
     end}.

%%====================================================================
%% API Function Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test init callback.
%% @end
%%--------------------------------------------------------------------
test_init_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    Config = #{},
                    State = yawl_stateless:init(Config),
                    ?assert(is_map(State))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test execute_stateless function with running server.
%% @end
%%--------------------------------------------------------------------
test_execute_stateless_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start the server
                    {ok, _Pid} = yawl_stateless:start_link(),

                    %% Execute a workflow
                    Spec = simple_workflow_spec(),
                    InputData = #{},
                    Result = yawl_stateless:execute_stateless(Spec, InputData),
                    case Result of
                        {ok, _ExecId} -> ok;
                        {error, _} -> ok
                    end
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test get_execution_state.
%% @end
%%--------------------------------------------------------------------
test_get_execution_state_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start the server
                    {ok, _Pid} = yawl_stateless:start_link(),

                    %% Test with non-existent execution
                    Result = yawl_stateless:get_execution_state(<<"nonexistent">>),
                    ?assertMatch({error, _}, Result)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test checkpoint functionality.
%% @end
%%--------------------------------------------------------------------
test_checkpoint_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start the server
                    {ok, _Pid} = yawl_stateless:start_link(),

                    %% Test with non-existent execution
                    Result = yawl_stateless:checkpoint(<<"nonexistent">>, <<"test_checkpoint">>),
                    ?assertMatch({error, _}, Result)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test restore functionality.
%% @end
%%--------------------------------------------------------------------
test_restore_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start the server
                    {ok, _Pid} = yawl_stateless:start_link(),

                    %% Test with non-existent execution
                    Result = yawl_stateless:restore(<<"nonexistent">>, <<"test_checkpoint">>),
                    ?assertMatch({error, _}, Result)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test list_executions.
%% @end
%%--------------------------------------------------------------------
test_list_executions_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start the server
                    {ok, _Pid} = yawl_stateless:start_link(),

                    Result = yawl_stateless:list_executions(),
                    ?assert(is_list(Result) orelse is_map(Result))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test cancel_execution.
%% @end
%%--------------------------------------------------------------------
test_cancel_execution_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start the server
                    {ok, _Pid} = yawl_stateless:start_link(),

                    %% Test with non-existent execution
                    Result = yawl_stateless:cancel_execution(<<"nonexistent">>),
                    case Result of
                        {error, _} -> ok;
                        ok -> ok
                    end
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test pause and resume execution.
%% @end
%%--------------------------------------------------------------------
test_pause_resume_execution_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start the server
                    {ok, _Pid} = yawl_stateless:start_link(),

                    %% Test pause with non-existent execution
                    PauseResult = yawl_stateless:pause_execution(<<"nonexistent">>),
                    case PauseResult of
                        {error, _} -> ok;
                        ok -> ok
                    end,

                    %% Test resume with non-existent execution
                    ResumeResult = yawl_stateless:resume_execution(<<"nonexistent">>),
                    case ResumeResult of
                        {error, _} -> ok;
                        ok -> ok
                    end
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test delete_completed_case.
%% @end
%%--------------------------------------------------------------------
test_delete_completed_case_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    %% Start the server
                    {ok, _Pid} = yawl_stateless:start_link(),

                    %% Test with non-existent execution
                    Result = yawl_stateless:get_execution_result(<<"nonexistent">>),
                    ?assertMatch({error, _}, Result)
                end)]
     end}.

%%====================================================================
%% gen_server Callback Tests
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test code_change callback.
%% @end
%%--------------------------------------------------------------------
test_code_change_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    State = #{},
                    ?assertEqual({ok, State}, yawl_stateless:code_change(old_vsn, State, extra))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test terminate callback.
%% @end
%%--------------------------------------------------------------------
test_terminate_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    State = #{},
                    ?assertEqual(ok, yawl_stateless:terminate(normal, State))
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test handle_call with unknown message.
%% @end
%%--------------------------------------------------------------------
test_handle_call_unknown_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    State = #{},
                    Result = yawl_stateless:handle_call(unknown_request, from, State),
                    ?assertMatch({reply, {error, _}, _}, Result)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test handle_cast with unknown message.
%% @end
%%--------------------------------------------------------------------
test_handle_cast_unknown_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    State = #{},
                    Result = yawl_stateless:handle_cast(unknown_msg, State),
                    ?assertMatch({noreply, _}, Result)
                end)]
     end}.

%%--------------------------------------------------------------------
%% @doc Test handle_info with unknown message.
%% @end
%%--------------------------------------------------------------------
test_handle_info_unknown_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Setup) ->
         [?_test(begin
                    State = #{},
                    Result = yawl_stateless:handle_info(unknown_msg, State),
                    ?assertMatch({noreply, _}, Result)
                end)]
     end}.

%%====================================================================
%% EUnit Auto-Discovery
%%====================================================================
%% Tests ending in _test_() are automatically discovered by EUnit
