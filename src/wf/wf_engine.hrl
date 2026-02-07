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
%% @doc Workflow Engine Records Header
%%
%% This file contains record definitions used by wf_engine and related modules.
%% @end
%% -------------------------------------------------------------------

-ifndef(WF_ENGINE_HRL).
-define(WF_ENGINE_HRL, true).

%%--------------------------------------------------------------------
%% @doc Work item record.
%%--------------------------------------------------------------------
-record(work_item, {
    wi_id :: binary(),
    case_id :: binary(),
    task :: atom(),
    status :: offered | allocated | started | completed,
    assigned_to :: atom() | binary() | undefined,
    data :: map() | undefined,
    created_at :: integer()
}).

%%--------------------------------------------------------------------
%% @doc Workflow case record.
%%--------------------------------------------------------------------
-record(wf_case, {
    case_id :: binary(),
    status :: pending | running | suspended | cancelled | completed | failed | scheduled,
    work_items = #{} :: #{binary() => #work_item{}},
    data = #{} :: map(),
    receipts = [] :: [term()],
    events = [] :: [term()],
    log = [] :: [term()],
    marking :: map(),
    rng_state :: {non_neg_integer(), non_neg_integer(), non_neg_integer()} |
                {non_neg_integer(), non_neg_integer(), non_neg_integer(), non_neg_integer()},
    scheduled_at :: integer() | undefined,
    timestamps :: map()
}).

-endif.
