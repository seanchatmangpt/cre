%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Chicago TDD Tests for YAWL Twitter/X Integration Module
%%
%% Test-First Development: RED -> GREEN -> REFACTOR
%%
%% @doc YAWL Twitter Tests
%% @end

-module(yawl_twitter_test).
-author("CRE Team").

-include_lib("eunit/include/eunit.hrl").

%% Include record definitions from the module under test
-include("yawl_twitter.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

setup() ->
    %% Start yawl_monitor gen_server for metrics recording
    case whereis(yawl_monitor) of
        undefined ->
            {ok, _Pid} = yawl_monitor:start_monitor(#{});
        _ ->
            ok
    end,
    ok.

cleanup(_Mock) ->
    %% Keep monitor running for other tests
    ok.

%%====================================================================
%% Tweet Posting Tests
%%====================================================================

post_tweet_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, TweetId} = yawl_twitter:post_tweet(<<"Hello, Twitter!">>),
               ?assert(is_binary(TweetId)),
               ?assert(binary:match(TweetId, <<"tweet_">>) =/= nomatch)
           end)
         ]
     end}.

post_tweet_with_options_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               Options = #{case_id => <<"case-123">>},
               {ok, TweetId} = yawl_twitter:post_tweet(<<"Test tweet">>, Options),
               ?assert(is_binary(TweetId))
           end)
         ]
     end}.

post_tweet_too_long_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               LongText = binary:copy(<<"a">>, 281),
               ?assertEqual({error, text_too_long}, yawl_twitter:post_tweet(LongText))
           end)
         ]
     end}.

post_tweet_with_media_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, TweetId} = yawl_twitter:post_tweet_with_media(
                   <<"Check this out!">>,
                   <<"https://example.com/image.jpg">>
               ),
               ?assert(is_binary(TweetId))
           end)
         ]
     end}.

post_tweet_with_media_too_long_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               LongText = binary:copy(<<"b">>, 281),
               ?assertEqual({error, text_too_long},
                   yawl_twitter:post_tweet_with_media(LongText, <<"http://example.com">>))
           end)
         ]
     end}.

%%====================================================================
%% Direct Message Tests
%%====================================================================

send_direct_message_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, DMId} = yawl_twitter:send_direct_message(<<"@user">>, <<"Hello!">>),
               ?assert(is_binary(DMId)),
               ?assert(binary:match(DMId, <<"dm_">>) =/= nomatch)
           end)
         ]
     end}.

send_direct_message_with_options_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               Options = #{case_id => <<"case-dm">>},
               {ok, DMId} = yawl_twitter:send_direct_message(
                   <<"@recipient">>,
                   <<"Message here">>,
                   Options
               ),
               ?assert(is_binary(DMId))
           end)
         ]
     end}.

%%====================================================================
%% Search and Monitoring Tests
%%====================================================================

search_tweets_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, Tweets} = yawl_twitter:search_tweets(<<"#erlang">>, 5),
               ?assert(is_list(Tweets)),
               ?assertEqual(5, length(Tweets)),
               [First | _] = Tweets,
               ?assertMatch(#tweet{}, First),
               ?assertEqual(<<"@mockuser">>, First#tweet.author)
           end)
         ]
     end}.

search_tweets_max_results_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, Tweets} = yawl_twitter:search_tweets(<<"test">>, 3),
               ?assertEqual(3, length(Tweets))
           end)
         ]
     end}.

search_tweets_limit_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               %% Request more than the internal limit
               {ok, Tweets} = yawl_twitter:search_tweets(<<"test">>, 100),
               ?assertEqual(10, length(Tweets))
           end)
         ]
     end}.

monitor_hashtag_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, MonitorId} = yawl_twitter:monitor_hashtag(<<"#erlang">>, self()),
               ?assert(is_reference(MonitorId))
           end)
         ]
     end}.

monitor_hashtag_without_hash_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, _MonitorId} = yawl_twitter:monitor_hashtag(<<"erlang">>, self()),
               ?assert(true)
           end)
         ]
     end}.

monitor_mentions_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, MonitorId} = yawl_twitter:monitor_mentions(<<"@myhandle">>, self()),
               ?assert(is_reference(MonitorId))
           end)
         ]
     end}.

monitor_mentions_without_at_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, _MonitorId} = yawl_twitter:monitor_mentions(<<"myhandle">>, self()),
               ?assert(true)
           end)
         ]
     end}.

stop_monitoring_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, MonitorId} = yawl_twitter:monitor_hashtag(<<"#test">>, self()),
               ?assertEqual(ok, yawl_twitter:stop_monitoring(MonitorId))
           end)
         ]
     end}.

%%====================================================================
%% Authentication Tests
%%====================================================================

set_credentials_bearer_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_assertEqual(ok,
              yawl_twitter:set_credentials(<<"app_key">>, <<"bearer_token">>)
          )
         ]
     end}.

set_credentials_oauth_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_assertEqual(ok,
              yawl_twitter:set_credentials(
                  <<"consumer_key">>,
                  <<"consumer_secret">>,
                  <<"access_token">>,
                  <<"access_secret">>
              )
          )
         ]
     end}.

clear_credentials_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_assertEqual(ok, yawl_twitter:clear_credentials())
         ]
     end}.

%%====================================================================
%% Webhook Tests
%%====================================================================

verify_webhook_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, Challenge} = yawl_twitter:verify_webhook(),
               ?assert(is_binary(Challenge)),
               ?assertEqual(32, byte_size(Challenge))
           end)
         ]
     end}.

%%====================================================================
%% Rate Limiting Tests
%%====================================================================

get_rate_limit_status_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               Status = yawl_twitter:get_rate_limit_status(),
               ?assert(is_map(Status)),
               ?assert(maps:is_key(endpoint, Status)),
               ?assert(maps:is_key(limit, Status)),
               ?assert(maps:is_key(remaining, Status)),
               ?assert(maps:is_key(reset_at, Status))
           end)
         ]
     end}.

get_remaining_tweets_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               Remaining = yawl_twitter:get_remaining_tweets(),
               ?assert(is_integer(Remaining)),
               ?assert(Remaining >= 0),
               ?assert(Remaining =< 100)
           end)
         ]
     end}.

%%====================================================================
%% Edge Cases Tests
%%====================================================================

empty_tweet_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, TweetId} = yawl_twitter:post_tweet(<<>>),
               ?assert(is_binary(TweetId))
           end)
         ]
     end}.

max_length_tweet_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               Tweet280 = binary:copy(<<"a">>, 280),
               {ok, TweetId} = yawl_twitter:post_tweet(Tweet280),
               ?assert(is_binary(TweetId))
           end)
         ]
     end}.

unicode_tweet_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, TweetId} = yawl_twitter:post_tweet(<<"Hello 🌍🌎🌏">>),
               ?assert(is_binary(TweetId))
           end)
         ]
     end}.

special_chars_tweet_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, TweetId} = yawl_twitter:post_tweet(
                   <<"Test with @mention and #hashtag and http://example.com">>
               ),
               ?assert(is_binary(TweetId))
           end)
         ]
     end}.

empty_search_query_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, Tweets} = yawl_twitter:search_tweets(<<>>, 5),
               ?assert(is_list(Tweets)),
               ?assertEqual(5, length(Tweets))
           end)
         ]
     end}.

zero_max_results_test_() ->
    {setup,
     fun setup/0,
     fun(Mock) -> cleanup(Mock) end,
     fun(_Mock) ->
         [
          ?_test(begin
               {ok, Tweets} = yawl_twitter:search_tweets(<<"test">>, 0),
               ?assertEqual([], Tweets)
           end)
         ]
     end}.
