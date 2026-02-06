%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Chicago TDD Tests for YAWL Web Dashboard
%%
%% Test-First Development: RED -> GREEN -> REFACTOR
%%
%% @doc YAWL Web Dashboard Tests
%% @end

-module(yawl_web_dashboard_test).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").
-include("yawl_otel_logger.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    % Start otel logger first
    {ok, _} = yawl_otel_logger:start_link(#{}),
    % Start dashboard on different port for testing
    {ok, Pid} = yawl_web_dashboard:start_link(#{port => 8082}),
    Pid.

cleanup(_Pid) ->
    yawl_otel_logger:clear_events(),
    gen_server:stop(yawl_web_dashboard),
    gen_server:stop(yawl_otel_logger),
    ok.

%%====================================================================
%% start_link/0 Tests
%%====================================================================

start_link_default_test_() ->
    {foreach,
     fun() ->
         ?assertCmd(cleanup_full),
         ?assertCmd(start_default_returns_ok)
     end}.

start_default_returns_ok() ->
    gen_server:stop(whereis(yawl_web_dashboard), normal, 1000),
    gen_server:stop(whereis(yawl_otel_logger), normal, 1000),
    timer:sleep(100),
    {ok, _} = yawl_otel_logger:start_link(#{}),
    {ok, Pid} = yawl_web_dashboard:start_link(),
    ?assert(is_pid(Pid)),
    gen_server:stop(yawl_web_dashboard),
    gen_server:stop(yawl_otel_logger).

cleanup_full() ->
    try
        cowboy:stop_listener(yawl_dashboard_http),
        catch _:_ -> ok
    end,
    try
        cowboy:stop_listener(yawl_dashboard_http), % Try default name
        catch _:_ -> ok
    end,
    gen_server:stop(whereis(yawl_web_dashboard), normal, 1000),
    gen_server:stop(whereis(yawl_otel_logger), normal, 1000),
    timer:sleep(100).

%%====================================================================
%% start_link/1 Tests
%%====================================================================

start_link_custom_port_test_() ->
    {foreach,
     fun() ->
         ?assertCmd(cleanup_full),
         ?assertCmd(custom_port_works)
     end}.

custom_port_works() ->
    {ok, _} = yawl_otel_logger:start_link(#{}),
    {ok, _Pid} = yawl_web_dashboard:start_link(#{port => 8083}),
    gen_server:stop(yawl_web_dashboard),
    gen_server:stop(yawl_otel_logger).

%%====================================================================
%% stop/0 Tests
%%====================================================================

stop_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) ->
         ok = yawl_web_dashboard:stop(),
         % Verify processes stopped
         ?assertEqual(undefined, whereis(yawl_web_dashboard))
     end}.

%%====================================================================
%% API Endpoint Tests
%%====================================================================

api_stats_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("GET /api/stats returns statistics"),
          {ok, 200, _, Response} = make_request(get, "/api/stats"),
          ?assertEqual(<<"application/json">>, proplists:get_value(<<"content-type">>, Response)),
          Body = jsx:decode(Response, [return_maps]),
          ?assert(maps:is_key(<<"event_count">>, Body)),
          ?assert(maps:is_key(<<"trace_count">>, Body))
         ]
     end}.

api_events_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("GET /api/events returns events list"),
          yawl_otel_logger:log_event(<<"test">>, <<"Test message">>, #{}),
          {ok, 200, _, Response} = make_request(get, "/api/events"),
          Body = jsx:decode(Response, [return_maps]),
          ?assert(maps:is_key(<<"events">>, Body)),
          ?assert(maps:is_key(<<"timestamp">>, Body))
         ]
     end}.

api_events_with_filter_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("GET /api/events filters by level"),
          yawl_otel_logger:log_event(<<"e1">>, <<"Msg1">>, #{}, info),
          yawl_otel_logger:log_event(<<"e2">>, <<"Msg2">>, #{}, debug),

          {ok, 200, _, Response} = make_request(get, "/api/events?level=info"),
          Body = jsx:decode(Response, [return_maps]),
          ?assertEqual(1, length(maps:get(<<"events">>, Body, [])))
         ]
     end}.

api_events_with_limit_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("GET /api/events limits results"),
          lists:foreach(fun(I) ->
              yawl_otel_logger:log_event(<<"e">>, integer_to_binary(I), #{})
          end, lists:seq(1, 10)),

          {ok, 200, _, Response} = make_request(get, "/api/events?limit=3"),
          Body = jsx:decode(Response, [return_maps]),
          ?assertEqual(3, length(maps:get(<<"events">>, Body, [])))
         ]
     end}.

api_traces_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("GET /api/traces returns traces"),
          yawl_otel_logger:log_workflow_start(<<"case-1">>, <<"wf-1">>),
          {ok, 200, _, Response} = make_request(get, "/api/traces"),
          Body = jsx:decode(Response, [return_maps]),
          ?assert(maps:is_key(<<"traces">>, Body)),
          ?assert(maps:is_key(<<"count">>, Body))
         ]
     end}.

api_events_by_trace_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("GET /api/events/:trace_id returns trace events"),
          yawl_otel_logger:log_event(<<"trace_test">>, <<"Msg">>, #{trace_id => <<"trace-abc">>}),
          {ok, 200, _, Response} = make_request(get, "/api/events/trace-abc"),
          Body = jsx:decode(Response, [return_maps]),
          ?assertEqual(<<"trace-abc">>, maps:get(<<"trace_id">>, Body)),
          ?assertEqual(1, maps:get(<<"count">>, Body))
         ]
     end}.

api_clear_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("DELETE /api/clear clears events"),
          yawl_otel_logger:log_event(<<"e1">>, <<"Msg1">>, #{}),
          yawl_otel_logger:log_event(<<"e2">>, <<"Msg2">>, #{}),
          ?assertEqual(2, length(yawl_otel_logger:get_events())),

          {ok, 200, _, Response} = make_request(delete, "/api/clear"),
          Body = jsx:decode(Response, [return_maps]),
          ?assertEqual(<<"ok">>, maps:get(<<"status">>, Body)),

          ?assertEqual([], yawl_otel_logger:get_events())
         ]
     end}.

api_404_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Unknown path returns 404"),
          {ok, 404, _, _} = make_request(get, "/api/unknown")
         ]
     end}.

%%====================================================================
%% HTML Dashboard Tests
%%====================================================================

html_dashboard_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("GET / returns HTML dashboard"),
          {ok, 200, _, Response} = make_request(get, "/"),
          ?assertEqual(<<"text/html">>, proplists:get_value(<<"content-type">>, Response)),
          ?assert(list_size(Response) > 1000),  % Substantial HTML
          ?assert(string:str(<<"YAWL Workflow Dashboard">>, Response))
         ]
     end}.

%%====================================================================
%% Cowboy Handler Callback Tests
%%====================================================================

init_cowboy_root_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("init/2 handles root path"),
          % Create a minimal cowboy_req for testing
          Req = cowboy_req:uri(<<"http://localhost:8082/">>),
          {cowboy_rest, Req2, State} = yawl_web_dashboard:init(Req, #{}),
          ?assertNotEqual(undefined, Req2)
         ]
     end}.

allowed_methods_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("allowed_methods/2 returns supported methods"),
          Req = cowboy_req:method(<<"GET">>),
          {Methods, _, _} = yawl_web_dashboard:allowed_methods(Req, #{}),
          ?assert(lists:member(<<"GET">>, Methods)),
          ?assert(lists:member(<<"DELETE">>, Methods)),
          ?assert(lists:member(<<"OPTIONS">>, Methods))
         ]
     end}.

content_types_provided_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("content_types_provided/2 returns JSON"),
          Req = cowboy_req:uri(<<"http://localhost:8082/api/events">>),
          {CT, _, _} = yawl_web_dashboard:content_types_provided(Req, #{}),
          ?assertEqual(1, length(CT)),
          [{{<<"application">>, <<"json">>, []}}] = CT
         ]
     end}.

%%====================================================================
%% Internal Function Tests
%%====================================================================

format_event_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         Event = #otel_event{
             id = <<"event_test">>,
             trace_id => <<"trace_123">>,
             timestamp = 12345,
             event_type = workflow_start,
             level = info,
             user_id = <<"user1">>,
             case_id => <<"case1">>,
             task_id => <<"task1">>,
             pattern_id => <<"pat1">>,
             message = <<"Test message">>,
             attributes = #{key => <<"value">>}
         },
         Formatted = yawl_web_dashboard:format_event(Event),
         ?assertEqual(<<"event_test">>, maps:get(<<"id">>, Formatted)),
         ?assertEqual(<<"trace_123">>, maps:get(<<"trace_id">>, Formatted)),
         ?assertEqual(<<"user1">>, maps:get(<<"user_id">>, Formatted)),
         ?assertEqual(<<"case1">>, maps:get(<<"case_id">>, Formatted))
     end}.

format_trace_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         Trace = #otel_trace{
             trace_id => <<"trace_abc">>,
             case_id => <<"case-xyz">>,
             pattern_id => <<"pattern-1">>,
             start_time => 1000,
             end_time => 2000,
             status => completed,
             span_count => 5
         },
         Formatted = yawl_web_dashboard:format_trace(Trace),
         ?assertEqual(<<"trace_abc">>, maps:get(<<"trace_id">>, Formatted)),
         ?assertEqual(1000, maps:get(<<"duration_ms">>, Formatted)),
         ?assertEqual(completed, maps:get(<<"status">>, Formatted))
     end}.

format_maybe_undefined_test_() ->
    ?assertEqual(<<>>, yawl_web_dashboard:format_maybe(undefined)),
    ?assertEqual(<<"bin">>, yawl_web_dashboard:format_maybe(<<"bin">>)),
    ?assertEqual(<<"atom">>, yawl_web_dashboard:format_maybe(atom)).

format_attributes_test_() ->
    Attrs = #{binary_key => <<"value">>, atom_key => value, int_key => 123},
    Formatted = yawl_web_dashboard:format_attributes(Attrs),
    ?assertEqual(<<"value">>, maps:get(<<"binary_key">>, Formatted)),
    ?assertEqual(<<"value">>, maps:get(<<"atom_key">>, Formatted)),
    ?assertEqual(<<"123">>, maps:get(<<"int_key">>, Formatted)).

to_binary_test_() ->
    ?assertEqual(<<"bin">>, yawl_web_dashboard:to_binary(<<"bin">>)),
    ?assertEqual(<<"list">>, yawl_web_dashboard:to_binary(<<"list">>)),
    ?assertEqual(<<"atom">>, yawl_web_dashboard:to_binary(atom)),
    ?assertEqual(<<"123">>, yawl_web_dashboard:to_binary(123)),
    ?assertEqual(<<>>, yawl_web_dashboard:to_binary(undefined)).

%%====================================================================
%% Helper Functions
%%====================================================================

make_request(Method, Path) ->
    Port = 8082,
    Host = "localhost",
    URL = lists:flatten(["http://", Host, ":", integer_to_list(Port), Path]),
    Headers = #{<<"accept">> => <<"application/json">>},
    httpc:request(Method, {URL, Headers}, [], 5000).

%%====================================================================
%% Concurrent Request Tests
%%====================================================================

concurrent_requests_test_() ->
    {setup,
     fun setup/0,
     fun(_Pid) -> cleanup(_Pid) end,
     fun(_Pid) ->
         [
          ?_test("Handles concurrent requests"),
          Pids = [spawn(fun() ->
              make_request(get, "/api/stats")
          end) || _ <- lists:seq(1, 10)],

          lists:foreach(fun(Pid) ->
              receive {Pid, _} -> ok
              after 5000 -> timeout
              end
          end, Pids),

          ?assertEqual(10, length([P || P <- Pids, P =:= ok]))
         ]
     end}.
