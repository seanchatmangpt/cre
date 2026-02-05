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
%% @end
%% -------------------------------------------------------------------

-module(gen_smtp_client).

%% Export stub functions to satisfy xref
%% gen_smtp_client API: send/1, send/2
-export([send/1, send/2]).

%%--------------------------------------------------------------------
%% @doc Stub send/1 function.
%% Takes a single argument - options proplist with embedded From/To.
%% @end
%%--------------------------------------------------------------------
-spec send(proplists:proplist()) -> {ok, pid()} | {error, term()}.
send(_Options) ->
    %% Stub implementation - this is only used for compilation
    %% At runtime, yawl_mail checks code:ensure_loaded/1 and uses
    %% the real gen_smtp_client if available
    {error, gen_smtp_not_available}.

%%--------------------------------------------------------------------
%% @doc Stub send/2 function.
%% Takes From binary and Options proplist.
%% Note: Real gen_smtp_client has different signature, this is a stub.
%% @end
%%--------------------------------------------------------------------
-spec send(binary(), proplists:proplist()) -> {ok, pid()} | {error, term()}.
send(_From, _Options) ->
    {error, gen_smtp_not_available}.
