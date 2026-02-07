%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @doc YAWL Recovery Records
%%
%% Record definitions for yawl_recovery module.
%%
%% @end
%% -------------------------------------------------------------------

-record(yawl_checkpoint, {
    checkpoint_id :: binary(),  % Primary key - MUST be first for Mnesia
    spec_id :: binary(),
    case_id :: binary(),
    marking :: pnet_types:marking(),
    data :: map(),
    timestamp :: integer(),
    version :: non_neg_integer()
}).
