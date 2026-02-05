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
%% @doc telemetry stub module
%%
%% This module provides a stub implementation when telemetry
%% is not available as a dependency. The real telemetry
%% will be used at runtime if available via code:is_loaded/1.
%%
%% To use the real telemetry library, add it to rebar.config deps:
%%   {telemetry, "1.2.1"}
%%
%% @end
%% -------------------------------------------------------------------

-module(telemetry).

%% Export stub functions to satisfy xref
-export([execute/2]).

%%--------------------------------------------------------------------
%% @doc Stub execute function.
%% The real telemetry:execute/2 will be used if available.
%% @end
%%--------------------------------------------------------------------
-spec execute(_, _) -> ok.
execute(_EventName, _Measurements) ->
    %% Stub implementation - this is only used for compilation
    %% At runtime, yawl_monitor checks code:is_loaded/1 and uses
    %% the real telemetry if available
    ok.
