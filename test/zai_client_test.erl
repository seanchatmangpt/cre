%% -*- erlang -*-
%% @doc Unit tests for Z.AI HTTP client.
%%
%% Uses meck to mock httpc for HTTP responses.
-module(zai_client_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Tests
%%====================================================================

chat_completions_ok_test_() ->
    {setup,
     fun setup_httpc_ok/0,
     fun teardown/1,
     fun(ok) ->
         [
             {"Returns content from mock HTTP 200",
              fun() ->
                  Msgs = [#{role => <<"user">>, content => <<"Hello">>}],
                  Opts = #{},
                  case zai_client:chat_completions(Msgs, Opts) of
                      {ok, Content} ->
                          ?assert(is_binary(Content)),
                          ?assertEqual(<<"{\"decision\":\"accept\"}">>, Content);
                      {error, Reason} ->
                          ct:fail("Expected ok, got ~p", [Reason])
                  end
              end}
         ]
     end}.

chat_json_ok_test_() ->
    {setup,
     fun setup_httpc_ok/0,
     fun teardown/1,
     fun(ok) ->
         [
             {"Returns decoded map from JSON response",
              fun() ->
                  Msgs = [#{role => <<"user">>, content => <<"Reply JSON">>}],
                  Opts = #{model => <<"glm-4-plus">>},
                  case zai_client:chat_json(Msgs, Opts) of
                      {ok, Json} ->
                          ?assertEqual(<<"accept">>, maps:get(<<"decision">>, Json));
                      {error, Reason} ->
                          ct:fail("Expected ok, got ~p", [Reason])
                  end
              end}
         ]
     end}.

chat_completions_api_key_missing_test_() ->
    {setup,
     fun setup_no_api_key/0,
     fun teardown/1,
     fun(ok) ->
         [
             {"Returns error when API key not configured",
              fun() ->
                  Msgs = [#{role => <<"user">>, content => <<"Hi">>}],
                  {error, Reason} = zai_client:chat_completions(Msgs, #{}),
                  ?assertEqual(api_key_not_configured, Reason)
              end}
         ]
     end}.

chat_completions_http_error_test_() ->
    {setup,
     fun setup_httpc_error/0,
     fun teardown/1,
     fun(ok) ->
         [
             {"Returns error on HTTP 429",
              fun() ->
                  Msgs = [#{role => <<"user">>, content => <<"Hi">>}],
                  {error, {http_error, 429, _}} = zai_client:chat_completions(Msgs, #{})
              end}
         ]
     end}.

%%====================================================================
%% Setup/Teardown
%%====================================================================

setup_httpc_ok() ->
    case code:ensure_loaded(meck) of
        {module, meck} ->
            meck:new(httpc, [unstick]),
            meck:expect(httpc, request,
                fun(post, {_Url, _Headers, _ContentType, _Body}, _HttpOpts, _Opts) ->
                    RespBody = <<"{\"choices\":[{\"message\":{\"content\":\"{\\\"decision\\\":\\\"accept\\\"}\"}}]}">>,
                    {ok, {{"HTTP/1.1", 200, "OK"}, [], RespBody}}
                end),
            application:ensure_all_started(inets),
            set_api_key("test-key"),
            ok;
        _ ->
            ok
    end.

setup_no_api_key() ->
    unset_api_key(),
    ok.

setup_httpc_error() ->
    case code:ensure_loaded(meck) of
        {module, meck} ->
            meck:new(httpc, [unstick]),
            meck:expect(httpc, request,
                fun(_Method, _Req, _HttpOpts, _Opts) ->
                    {ok, {{"HTTP/1.1", 429, "Too Many Requests"}, [], <<"{}">>}}
                end),
            application:ensure_all_started(inets),
            set_api_key("test-key"),
            ok;
        _ ->
            ok
    end.

teardown(ok) ->
    unset_api_key(),
    case code:ensure_loaded(meck) of
        {module, meck} ->
            catch meck:unload(httpc);
        _ ->
            ok
    end,
    ok.

set_api_key(Key) ->
    application:load(cre),
    application:set_env(cre, zai_api_key, Key).

unset_api_key() ->
    application:unset_env(cre, zai_api_key).
