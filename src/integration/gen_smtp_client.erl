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
%% @doc gen_smtp_client stub module
%%
%% This module provides a stub implementation when gen_smtp_client
%% is not available as a dependency. The real gen_smtp_client
%% will be used at runtime if available via code:ensure_loaded/1.
%%
%% To use the real gen_smtp_client, add it to rebar.config deps:
%%   {gen_smtp, "0.6.0"}
%%
%% <h2>Examples</h2>
%%
%% Send email with options proplist (embedded From/To):
%%
%% ```erlang
%% 1> gen_smtp_client:send([
%%     {relay, "smtp.example.com"},
%%     {port, 587},
%%     {username, "user"},
%%     {password, "pass"},
%%     {ssl, false},
%%     {tls, always},
%%     {auth, always},
%%     {from, "sender@example.com"},
%%     {to, ["recipient@example.com"]},
%%     {subject, "Test Email"},
%%     {body, <<"Email body text">>}
%% ]).
%% {error, gen_smtp_not_available}
%% ```
%%
%% Send email with From binary and options proplist:
%%
%% ```erlang
%% 2> gen_smtp_client:send(
%%     <<"sender@example.com">>,
%%     [
%%         {relay, "smtp.example.com"},
%%         {to, ["recipient@example.com"]},
%%         {subject, "Test Subject"},
%%         {body, <<"Email body">>}
%%     ]
%% ).
%% {error, gen_smtp_not_available}
%% ```
%%
%% Run doctests:
%%
%% ```erlang
%% 3> gen_smtp_client:doctest_test().
%% ok
%% ```
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_smtp_client).

%% Export stub functions to satisfy xref
%% gen_smtp_client API: send/1, send/2
-export([send/1, send/2, doctest_test/0]).

%%--------------------------------------------------------------------
%% @doc Stub send/1 function.
%% Takes a single argument - options proplist with embedded From/To.
%%
%% Returns `{error, gen_smtp_not_available}` when gen_smtp is not loaded.
%% At runtime, yawl_mail checks code:ensure_loaded/1 and uses
%% the real gen_smtp_client if available.
%%
%% ## Examples
%%
%% ```erlang
%% 1> gen_smtp_client:send([
%%     {relay, "smtp.example.com"},
%%     {to, ["recipient@example.com"]},
%%     {from, "sender@example.com"}
%% ]).
%% {error, gen_smtp_not_available}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec send(proplists:proplist()) -> {ok, pid()} | {error, term()}.
-doc("""
Stub send/1 function.

Returns `{error, gen_smtp_not_available}` when gen_smtp is not loaded.
This stub allows compilation to succeed. At runtime, the real
gen_smtp_client will be used if available.

## Examples

```erlang
1> gen_smtp_client:send([
    {relay, "smtp.example.com"},
    {to, ["recipient@example.com"]},
    {from, "sender@example.com"}
]).
{error, gen_smtp_not_available}
```
""").
send(_Options) ->
    %% Stub implementation - this is only used for compilation
    %% At runtime, yawl_mail checks code:ensure_loaded/1 and uses
    %% the real gen_smtp_client if available
    {error, gen_smtp_not_available}.

%%--------------------------------------------------------------------
%% @doc Stub send/2 function.
%% Takes From binary and Options proplist.
%% Note: Real gen_smtp_client has different signature, this is a stub.
%%
%% ## Examples
%%
%% ```erlang
%% 1> gen_smtp_client:send(
%%     <<"sender@example.com">>,
%%     [{relay, "smtp.example.com"}]
%% ).
%% {error, gen_smtp_not_available}
%% ```
%%
%% @end
%%--------------------------------------------------------------------
-spec send(binary(), proplists:proplist()) -> {ok, pid()} | {error, term()}.
-doc("""
Stub send/2 function.

Returns `{error, gen_smtp_not_available}` when gen_smtp is not loaded.

## Examples

```erlang
1> gen_smtp_client:send(
    <<"sender@example.com">>,
    [{relay, "smtp.example.com"}]
).
{error, gen_smtp_not_available}
```
""").
send(_From, _Options) ->
    {error, gen_smtp_not_available}.

%%--------------------------------------------------------------------
%% @doc Runs doctests for the gen_smtp_client module.
%%
%% @end
%%--------------------------------------------------------------------
-spec doctest_test() -> ok.
-doc("""
Run doctests for the gen_smtp_client module.

This function provides a simple test entry point that verifies
stub functionality. All stub functions return `{error, gen_smtp_not_available}`.

## Examples

```erlang
1> gen_smtp_client:doctest_test().
ok
```
""").
doctest_test() ->
    %% Test 1: send/1 returns error for stub
    {error, gen_smtp_not_available} = send([{relay, "smtp.example.com"}]),

    %% Test 2: send/2 returns error for stub
    {error, gen_smtp_not_available} = send(<<"sender@example.com">>, []),

    %% Test 3: send/1 with empty options
    {error, gen_smtp_not_available} = send([]),

    %% Test 4: send/2 with from and various options
    Options = [
        {relay, "smtp.example.com"},
        {port, 587},
        {username, <<"user">>},
        {password, <<"pass">>},
        {tls, always}
    ],
    {error, gen_smtp_not_available} = send(<<"from@example.com">>, Options),

    %% Test 5: Verify return tuple structure
    case send([]) of
        {error, gen_smtp_not_available} -> ok;
        _ -> error(unexpected_return_value)
    end,

    ok.
