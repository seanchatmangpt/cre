%% -*- erlang -*-
%%%% @doc Workflow Engine Records Header
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
