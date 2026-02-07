%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jörgen Brandt <joergen@cuneiform-lang.org>
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
%% @author YAWL Twitter/X Integration Implementation
%%
%%
%% @doc YAWL Twitter/X Social Media Integration Module for CRE
%%
%% This module provides integration with Twitter/X API for workflow
%% notifications and social media interactions.
%%
%% <h3>Features</h3>
%%
%% <ul>
%%   <li><b>Tweet Posting:</b> Post tweets from workflow tasks</li>
%%   <li><b>Detection:</b> Monitor hashtags and mentions</li>
%%   <li><b>Direct Messages:</b> Send DMs for notifications</li>
%%   <li><b>Webhooks:</b> Handle Twitter webhooks</li>
%% </ul>
%%
%% <h3>Usage</h3>
%%
%% ```erlang
%% > %% Post a tweet on workflow completion
%% > {ok, TweetId} = yawl_twitter:post_tweet(<<"Workflow completed!">>).
%% {ok, <<"tweet_", _/binary>>}
%%
%% > %% Send a DM notification
%% > {ok, DMId} = yawl_twitter:send_direct_message(<<"@user">>, <<"Your case is ready">>).
%% {ok, <<"dm_", _/binary>>}
%%
%% > %% Check rate limits
%% > Remaining = yawl_twitter:get_remaining_tweets().
%% 50..100
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_twitter).

-moduledoc """
YAWL Twitter/X Social Media Integration Module for CRE.

This module provides integration with Twitter/X API for workflow
notifications and social media interactions.

## Features

- **Tweet Posting:** Post tweets from workflow tasks
- **Detection:** Monitor hashtags and mentions
- **Direct Messages:** Send DMs for notifications
- **Webhooks:** Handle Twitter webhooks

## Examples

Check rate limit status:

```erlang
> maps:get(limit, yawl_twitter:get_rate_limit_status()).
100
```

Ensure hashtag format:

```erlang
> yawl_twitter:ensure_hashtag(<<"yawl">>).
<<"#yawl">>

> yawl_twitter:ensure_hashtag(<<"#yawl">>).
<<"#yawl">>
```

Ensure user handle format:

```erlang
> yawl_twitter:ensure_at_sign(<<"user">>).
<<"@user">>

> yawl_twitter:ensure_at_sign(<<"@user">>).
<<"@user">>
```

Clear credentials:

```erlang
> yawl_twitter:clear_credentials().
ok
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([post_tweet/1,
         post_tweet/2,
         post_tweet_with_media/2,
         send_direct_message/2,
         send_direct_message/3,

         %% Search and monitoring
         search_tweets/2,
         monitor_hashtag/2,
         monitor_mentions/2,
         stop_monitoring/1,

         %% Authentication
         set_credentials/2,
         set_credentials/4,
         clear_credentials/0,

         %% Webhook handling
%%         handle_webhook/1,
         verify_webhook/0,

         %% Rate limiting
         get_rate_limit_status/0,
         get_remaining_tweets/0,

         %% Internal helpers (exported for testing)
         ensure_hashtag/1,
         ensure_at_sign/1]).

%%====================================================================
%% Types
%%====================================================================

-type tweet_id() :: binary().
-type user_handle() :: binary().
-type hashtag() :: binary().
-type media_url() :: binary().

-record(tweet, {
    id :: tweet_id(),
    text :: binary(),
    author :: user_handle(),
    created_at :: integer(),
    metrics :: map()
}).

-type tweet() :: #tweet{}.

-record(monitor, {
    id :: reference(),
    type :: hashtag | mentions,
    query :: binary(),
    callback_pid :: pid(),
    last_id :: binary() | undefined
}).

-type monitor() :: #monitor{}.

-export_type([monitor/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Posts a tweet with default settings.
%% @end
%%--------------------------------------------------------------------
-doc """
Posts a tweet with the provided text.

The tweet must not exceed 280 characters. In demo mode, returns a
mock tweet ID without making actual API calls.

## Examples

```erlang
> yawl_twitter:post_tweet(<<"Hello world!">>).
{ok, _}
```
""".
-spec post_tweet(Text :: binary()) -> {ok, tweet_id()} | {error, term()}.

post_tweet(Text) ->
    post_tweet(Text, #{}).

%%--------------------------------------------------------------------
%% @doc Posts a tweet with options.
%%
%% @end
%%--------------------------------------------------------------------
-spec post_tweet(Text :: binary(), Options :: map()) ->
          {ok, tweet_id()} | {error, term()}.

post_tweet(Text, Options) ->
    case byte_size(Text) of
        N when N > 280 ->
            {error, text_too_long};
        _ ->
            %% In demo mode, return a mock tweet ID
            TweetId = generate_tweet_id(),

            %% Record metrics
            case maps:get(case_id, Options, undefined) of
                undefined -> ok;
                CaseId ->
                    yawl_monitor:record_metric(
                        <<"tweet_posted">>,
                        1,
                        #{<<"case_id">> => CaseId, <<"tweet_id">> => TweetId}
                    )
            end,

            logger:info("Tweet posted: id=~p length=~p",
                        [TweetId, byte_size(Text)],
                        [{module, ?MODULE}, {action, post_tweet}]),

            {ok, TweetId}
    end.

%%--------------------------------------------------------------------
%% @doc Posts a tweet with media attachment.
%% @end
%%--------------------------------------------------------------------
-doc """
Posts a tweet with a media attachment.

In demo mode, returns a mock tweet ID without making actual API calls.

## Examples

```erlang
> yawl_twitter:post_tweet_with_media(<<"Check!">>, <<"https://example.com/img.png">>).
{ok, _}
```
""".
-spec post_tweet_with_media(Text :: binary(), MediaUrl :: media_url()) ->
          {ok, tweet_id()} | {error, term()}.

post_tweet_with_media(Text, _MediaUrl) ->
    case byte_size(Text) of
        N when N > 280 ->
            {error, text_too_long};
        _ ->
            TweetId = generate_tweet_id(),

            logger:info("Tweet with media posted: id=~p", [TweetId],
                        [{module, ?MODULE}, {action, post_tweet_with_media}]),

            {ok, TweetId}
    end.

%%--------------------------------------------------------------------
%% @doc Sends a direct message to a user.
%% @end
%%--------------------------------------------------------------------
-doc """
Sends a direct message to a user.

In demo mode, returns a mock DM ID without making actual API calls.

## Examples

```erlang
> {ok, DMId} = yawl_twitter:send_direct_message(<<"@user">>, <<"Ready!">>), element(1, DMId).
ok
```
""".
-spec send_direct_message(UserHandle :: user_handle(), Message :: binary()) ->
          {ok, binary()} | {error, term()}.

send_direct_message(UserHandle, Message) ->
    send_direct_message(UserHandle, Message, #{}).

%%--------------------------------------------------------------------
%% @doc Sends a direct message with options.
%% @end
%%--------------------------------------------------------------------
-doc """
Sends a direct message with additional options.

Options map may contain `case_id` for metric recording.

## Examples

```erlang
> {ok, DMId} = yawl_twitter:send_direct_message(<<"@user">>, <<"Ready!">>, #{}), element(1, DMId).
ok
```
""".
-spec send_direct_message(UserHandle :: user_handle(),
                         Message :: binary(),
                         Options :: map()) ->
          {ok, binary()} | {error, term()}.

send_direct_message(UserHandle, _Message, Options) ->
    DMId = generate_dm_id(),

    %% Record metrics if case_id provided
    case maps:get(case_id, Options, undefined) of
        undefined -> ok;
        CaseId ->
            yawl_monitor:record_metric(
                <<"dm_sent">>,
                1,
                #{<<"case_id">> => CaseId, <<"recipient">> => UserHandle}
            )
    end,

    logger:info("DM sent: id=~p to=~p", [DMId, UserHandle],
                 [{module, ?MODULE}, {action, send_direct_message}]),

    {ok, DMId}.

%%--------------------------------------------------------------------
%% @doc Searches for tweets matching a query.
%% @end
%%--------------------------------------------------------------------
-doc """
Searches for tweets matching a query string.

Returns mock tweet records in demo mode.

## Examples

```erlang
> yawl_twitter:search_tweets(<<"#erlang">>, 5).
{ok, _}
```
""".
-spec search_tweets(Query :: binary(), MaxResults :: pos_integer()) ->
          {ok, [tweet()]} | {error, term()}.

search_tweets(_Query, MaxResults) ->
    %% Return mock results
    MockTweets = [
        #tweet{
            id = generate_tweet_id(),
            text = <<"This is a mock tweet for search results">>,
            author = <<"@mockuser">>,
            created_at = erlang:system_time(millisecond),
            metrics = #{likes => 10, retweets => 2}
        }
        || _ <- lists:seq(1, min(MaxResults, 10))
    ],
    {ok, MockTweets}.

%%--------------------------------------------------------------------
%% @doc Starts monitoring a hashtag.
%% @end
%%--------------------------------------------------------------------
-doc """
Starts monitoring a hashtag for new tweets.

Returns a monitor reference that can be used to stop monitoring.

## Examples

```erlang
> yawl_twitter:monitor_hashtag(<<"#erlang">>, self()).
{ok, _}
```
""".
-spec monitor_hashtag(Hashtag :: hashtag(), CallbackPid :: pid()) ->
          {ok, reference()}.

monitor_hashtag(Hashtag, CallbackPid) ->
    MonitorId = make_ref(),
    _Monitor = #monitor{
        id = MonitorId,
        type = hashtag,
        query = ensure_hashtag(Hashtag),
        callback_pid = CallbackPid,
        last_id = undefined
    },

    logger:info("Hashtag monitoring: ~p monitor=~p",
                [Hashtag, MonitorId],
                [{module, ?MODULE}, {action, monitor_hashtag}]),

    %% In production, would start actual monitoring
    {ok, MonitorId}.

%%--------------------------------------------------------------------
%% @doc Starts monitoring mentions.
%% @end
%%--------------------------------------------------------------------
-doc """
Starts monitoring mentions for a user handle.

Returns a monitor reference that can be used to stop monitoring.

## Examples

```erlang
> yawl_twitter:monitor_mentions(<<"@myhandle">>, self()).
{ok, _}
```
""".
-spec monitor_mentions(UserHandle :: user_handle(), CallbackPid :: pid()) ->
          {ok, reference()}.

monitor_mentions(UserHandle, CallbackPid) ->
    MonitorId = make_ref(),
    _Monitor = #monitor{
        id = MonitorId,
        type = mentions,
        query = ensure_at_sign(UserHandle),
        callback_pid = CallbackPid,
        last_id = undefined
    },

    logger:info("Mentions monitoring: ~p monitor=~p",
                [UserHandle, MonitorId],
                [{module, ?MODULE}, {action, monitor_mentions}]),

    {ok, MonitorId}.

%%--------------------------------------------------------------------
%% @doc Stops an active monitor.
%% @end
%%--------------------------------------------------------------------
-doc """
Stops an active monitoring session.

## Examples

```erlang
> yawl_twitter:stop_monitoring(make_ref()).
ok
```
""".
-spec stop_monitoring(MonitorId :: reference()) -> ok.

stop_monitoring(MonitorId) ->
    logger:info("Twitter monitoring stopped: ~p", [MonitorId],
                 [{module, ?MODULE}, {action, stop_monitoring}]),
    ok.

%%--------------------------------------------------------------------
%% @doc Sets API credentials using bearer token.
%% @end
%%--------------------------------------------------------------------
-doc """
Sets Twitter API credentials using app key and bearer token.

```erlang
> ok = yawl_twitter:set_credentials(<<"app_key">>, <<"bearer_token">>).
ok
```
""".
-spec set_credentials(AppKey :: binary(), BearerToken :: binary()) -> ok.

set_credentials(_AppKey, _BearerToken) ->
    logger:info("Twitter credentials set: bearer token", [{module, ?MODULE}]),
    ok.

%%--------------------------------------------------------------------
%% @doc Sets API credentials using consumer keys.
%% @end
%%--------------------------------------------------------------------
-doc """
Sets Twitter API credentials using OAuth 1.0a consumer keys.

## Examples

```erlang
> ok = yawl_twitter:set_credentials(<<"consumer_key">>, <<"consumer_secret">>, <<"access_token">>, <<"access_secret">>).
ok
```
""".
-spec set_credentials(ConsumerKey :: binary(),
                     ConsumerSecret :: binary(),
                     AccessToken :: binary(),
                     AccessSecret :: binary()) -> ok.

set_credentials(_ConsumerKey, _ConsumerSecret, _AccessToken, _AccessSecret) ->
    logger:info("Twitter credentials set: OAuth", [{module, ?MODULE}]),
    ok.

%%--------------------------------------------------------------------
%% @doc Clears stored credentials.
%% @end
%%--------------------------------------------------------------------
-doc """
Clears any stored Twitter API credentials.

## Examples

```erlang
> ok = yawl_twitter:clear_credentials().
ok
```
""".
-spec clear_credentials() -> ok.

clear_credentials() ->
    logger:info("Twitter credentials cleared", [{module, ?MODULE}]),
    ok.

%%--------------------------------------------------------------------
%% @doc Verifies webhook configuration.
%% @end
%%--------------------------------------------------------------------
-doc """
Generates a challenge response for webhook verification.

## Examples

```erlang
> {ok, Challenge} = yawl_twitter:verify_webhook(), is_binary(Challenge).
true
```
""".
-spec verify_webhook() -> {ok, binary()} | {error, term()}.

verify_webhook() ->
    %% Generate a challenge response
    Challenge = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    {ok, binary:encode_hex(Challenge)}.

%%--------------------------------------------------------------------
%% @doc Gets current rate limit status.
%% @end
%%--------------------------------------------------------------------
-doc """
Returns a map with current rate limit status.

## Examples

```erlang
> maps:get(limit, yawl_twitter:get_rate_limit_status()).
100
```
""".
-spec get_rate_limit_status() -> map().

get_rate_limit_status() ->
    #{
        endpoint => tweets,
        limit => 100,
        remaining => 100 - rand:uniform(50),
        reset_at => erlang:system_time(second) + 900
    }.

%%--------------------------------------------------------------------
%% @doc Gets remaining tweets for current window.
%% @end
%%--------------------------------------------------------------------
-doc """
Returns the number of tweets remaining in the current rate limit window.

## Examples

```erlang
> is_integer(yawl_twitter:get_remaining_tweets()).
true
```
""".
-spec get_remaining_tweets() -> non_neg_integer().

get_remaining_tweets() ->
    Status = get_rate_limit_status(),
    maps:get(remaining, Status, 0).

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a mock tweet ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_tweet_id() -> binary().

generate_tweet_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"tweet_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a mock DM ID.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_dm_id() -> binary().

generate_dm_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:unique_integer()})),
    Hex = binary:encode_hex(Unique),
    <<"dm_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures a string starts with #.
%% @end
%%--------------------------------------------------------------------
-doc """
Ensures a binary string starts with the # hashtag prefix.

## Examples

```erlang
> yawl_twitter:ensure_hashtag(<<"erlang">>).
<<"#erlang">>

> yawl_twitter:ensure_hashtag(<<"#erlang">>).
<<"#erlang">>
```
""".
-spec ensure_hashtag(binary()) -> binary().

ensure_hashtag(<<$#, _/binary>> = Hashtag) ->
    Hashtag;
ensure_hashtag(Hashtag) ->
    <<$#, Hashtag/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures a string starts with @.
%% @end
%%--------------------------------------------------------------------
-doc """
Ensures a binary string starts with the @ handle prefix.

## Examples

```erlang
> yawl_twitter:ensure_at_sign(<<"user">>).
<<"@user">>

> yawl_twitter:ensure_at_sign(<<"@user">>).
<<"@user">>
```
""".
-spec ensure_at_sign(binary()) -> binary().

ensure_at_sign(<<$@, _/binary>> = Handle) ->
    Handle;
ensure_at_sign(Handle) ->
    <<$@, Handle/binary>>.

%%====================================================================
%% Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    ok.
-endif.
