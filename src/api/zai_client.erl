%% -*- erlang -*-
%% @doc Z.AI Chat Completion API client.
%%
%% Erlang HTTP client for the Z.AI (Zhipu AI) Chat Completion API.
%% Uses OTP httpc (inets). API key from ZAI_API_KEY env or cre app env.
%%
%% @end
-module(zai_client).
-export([
    chat_completions/2,
    chat_json/2,
    get_api_key/0
]).

%%====================================================================
%% Types
%%====================================================================

-type message() :: #{role := binary(), content := binary()}.
-type messages() :: [message()].
-type options() :: #{
    model => binary(),
    temperature => float(),
    max_tokens => pos_integer(),
    stream => boolean(),
    response_format => text | json_object,
    timeout => pos_integer()
}.
-type content_result() :: {ok, binary()} | {error, term()}.
-type json_result() :: {ok, map()} | {error, term()}.

%%====================================================================
%% Constants
%%====================================================================

-define(BASE_URL, "https://api.z.ai/api/paas/v4/chat/completions").
-define(DEFAULT_MODEL, <<"glm-4-plus">>).
-define(DEFAULT_TEMPERATURE, 0.7).
-define(DEFAULT_MAX_TOKENS, 256).
-define(DEFAULT_TIMEOUT, 30000).

%%====================================================================
%% API
%%====================================================================

%% @doc Returns configured API key or undefined.
-spec get_api_key() -> binary() | undefined.
get_api_key() ->
    case application:get_env(cre, zai_api_key) of
        {ok, Key} when is_list(Key) -> list_to_binary(Key);
        {ok, Key} when is_binary(Key) -> Key;
        _ ->
            case os:getenv("ZAI_API_KEY") of
                false -> undefined;
                Key -> list_to_binary(Key)
            end
    end.

%% @doc Non-streaming chat completion.
%%
%% Messages: [{role, content}] where role is <<"user">>, <<"system">>, <<"assistant">>
%% Options: model, temperature, max_tokens, stream, response_format, timeout
%%
-spec chat_completions(Messages :: messages(), Options :: options()) -> content_result().
chat_completions(Messages, Options) ->
    case do_request(Messages, Options) of
        {ok, Body} ->
            extract_content(Body);
        {error, _} = Err ->
            Err
    end.

%% @doc Chat completion with JSON response; decodes and returns map.
-spec chat_json(Messages :: messages(), Options :: options()) -> json_result().
chat_json(Messages, Options) ->
    Opts = Options#{response_format => json_object},
    case chat_completions(Messages, Opts) of
        {ok, Content} ->
            decode_json(Content);
        {error, _} = Err ->
            Err
    end.

%%====================================================================
%% Internal
%%====================================================================

do_request(Messages, Options) ->
    case get_api_key() of
        undefined ->
            logger:warning("Z.AI API key not configured (ZAI_API_KEY or cre.zai_api_key)"),
            {error, api_key_not_configured};
        ApiKey ->
            ensure_inets(),
            Body = build_request_body(Messages, Options),
            Headers = [
                {"Authorization", "Bearer " ++ binary_to_list(ApiKey)},
                {"Content-Type", "application/json"},
                {"Accept-Language", "en-US,en"}
            ],
            Timeout = maps:get(timeout, Options, ?DEFAULT_TIMEOUT),
            case httpc:request(
                post,
                {?BASE_URL, Headers, "application/json", Body},
                [{timeout, Timeout}, {connect_timeout, 10000}],
                [{body_format, binary}]
            ) of
                {ok, {{_, 200, _}, _, RespBody}} ->
                    {ok, RespBody};
                {ok, {{_, Code, _}, _, RespBody}} ->
                    {error, {http_error, Code, parse_error_body(RespBody)}};
                {error, Reason} ->
                    {error, {http_error, Reason}}
            end
    end.

ensure_inets() ->
    case application:ensure_all_started(inets) of
        {ok, _} -> ok;
        {error, {already_started, _}} -> ok;
        {error, Reason} -> throw({inets_start_failed, Reason})
    end.

build_request_body(Messages, Options) ->
    Model = maps:get(model, Options, ?DEFAULT_MODEL),
    Temperature = maps:get(temperature, Options, ?DEFAULT_TEMPERATURE),
    MaxTokens = maps:get(max_tokens, Options, ?DEFAULT_MAX_TOKENS),
    Stream = maps:get(stream, Options, false),
    RespFmt = maps:get(response_format, Options, text),
    Msgs = [msg_to_map(M) || M <- Messages],
    Base = #{
        <<"model">> => Model,
        <<"messages">> => Msgs,
        <<"temperature">> => Temperature,
        <<"max_tokens">> => MaxTokens,
        <<"stream">> => Stream
    },
    Body = case RespFmt of
        json_object -> Base#{<<"response_format">> => #{<<"type">> => <<"json_object">>}};
        _ -> Base
    end,
    jsone:encode(Body).

msg_to_map(#{role := Role, content := Content}) ->
    #{<<"role">> => Role, <<"content">> => Content}.

extract_content(Body) ->
    try
        Obj = jsone:decode(Body),
        case Obj of
            #{<<"choices">> := [#{<<"message">> := #{<<"content">> := Content}} | _]} ->
                {ok, Content};
            #{<<"error">> := Err} ->
                {error, {api_error, Err}};
            _ ->
                {error, {invalid_response, no_choices}}
        end
    catch
        _:Reason ->
            {error, {decode_error, Reason}}
    end.

parse_error_body(<<>>) -> <<>>;
parse_error_body(Body) ->
    try
        #{<<"error">> := #{<<"message">> := Msg}} = jsone:decode(Body),
        Msg
    catch
        _:_ -> Body
    end.

decode_json(Content) when is_binary(Content) ->
    try
        {ok, jsone:decode(Content)}
    catch
        _:Reason ->
            {error, {json_decode_error, Reason}}
    end.
