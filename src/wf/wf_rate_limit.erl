-module(wf_rate_limit).

-export([
    new/2,
    acquire/2,
    acquire/3,
    available/1,
    reset/1
]).

-type rate_limiter() :: #{
    max_tokens => number(),
    tokens => number(),
    refill_rate => number(),
    last_refill => integer()
}.

-export_type([rate_limiter/0]).

-spec new(MaxTokens :: number(), RefillRate :: number()) -> rate_limiter().
new(MaxTokens, RefillRate) when MaxTokens > 0, RefillRate > 0 ->
    #{
        max_tokens => MaxTokens,
        tokens => MaxTokens,
        refill_rate => RefillRate,
        last_refill => erlang:system_time(millisecond)
    }.

-spec acquire(NumTokens :: number(), RateLimiter :: rate_limiter()) ->
    {ok, rate_limiter()} | {error, insufficient_tokens}.
acquire(NumTokens, RateLimiter) ->
    acquire(NumTokens, RateLimiter, infinity).

-spec acquire(NumTokens :: number(), RateLimiter :: rate_limiter(), Timeout :: number() | infinity) ->
    {ok, rate_limiter()} | {error, insufficient_tokens}.
acquire(NumTokens, RateLimiter, _Timeout) when NumTokens < 0 ->
    {error, insufficient_tokens};
acquire(NumTokens, RateLimiter, _Timeout) ->
    Refilled = refill_tokens(RateLimiter),
    Tokens = maps:get(tokens, Refilled),
    case Tokens >= NumTokens of
        true ->
            NewRateLimiter = Refilled#{tokens := Tokens - NumTokens},
            {ok, NewRateLimiter};
        false ->
            {error, insufficient_tokens}
    end.

-spec available(RateLimiter :: rate_limiter()) -> number().
available(RateLimiter) ->
    Refilled = refill_tokens(RateLimiter),
    maps:get(tokens, Refilled).

-spec reset(RateLimiter :: rate_limiter()) -> rate_limiter().
reset(RateLimiter) ->
    MaxTokens = maps:get(max_tokens, RateLimiter),
    RateLimiter#{
        tokens := MaxTokens,
        last_refill := erlang:system_time(millisecond)
    }.

-spec refill_tokens(RateLimiter :: rate_limiter()) -> rate_limiter().
refill_tokens(RateLimiter) ->
    Now = erlang:system_time(millisecond),
    LastRefill = maps:get(last_refill, RateLimiter),
    ElapsedMs = Now - LastRefill,
    RefillRate = maps:get(refill_rate, RateLimiter),
    MaxTokens = maps:get(max_tokens, RateLimiter),
    CurrentTokens = maps:get(tokens, RateLimiter),

    TokensToAdd = (ElapsedMs * RefillRate) / 1000,
    NewTokens = min(CurrentTokens + TokensToAdd, MaxTokens),

    RateLimiter#{
        tokens := NewTokens,
        last_refill := Now
    }.
