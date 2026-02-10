%% -*- erlang -*-
%%%% -------------------------------------------------------------------
%%%% @doc gen_wfnet record definitions
%%%%
%%%% Workflow Net (WF-Net) behavior for BPMN-style workflows
%%%% with Petri net foundation and built-in validation.
%%%%
%%%% Note: This header does NOT include gen_pnet.hrl to avoid duplicate
%%%% record definitions. Include gen_pnet.hrl separately before this file.
%%%% -------------------------------------------------------------------

-ifndef(GEN_WFNET_HRL).
-define(GEN_WFNET_HRL, 1).

%%--------------------------------------------------------------------
%% @doc Subscription filter for workflow events.
%%
%% Filters can be:
%% - `all` - Receive all events
%% - `{event_type, [atom()]}` - Receive specific event types
%% - `{case_id, binary()}` - Receive events for specific case
%%--------------------------------------------------------------------
-type subscription_filter() :: all | {event_type, [atom()]} | {case_id, binary()}.

-endif.
