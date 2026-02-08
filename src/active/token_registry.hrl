%% -*- erlang -*-
%%-------------------------------------------------------------------
%% @doc
%% Record definitions for token_registry module
%%
%% @end
%%-------------------------------------------------------------------

-ifndef(TOKEN_REGISTRY_HRL).
-define(TOKEN_REGISTRY_HRL, true).

%%--------------------------------------------------------------------
%% @doc Token Information Record
%%
%% Tracks metadata for each registered token.
%%--------------------------------------------------------------------
-record(token_info, {
    token_id :: binary(),
    pid :: pid(),
    place :: atom(),
    parent_id :: binary() | undefined,
    created_at :: integer(),
    metadata = #{} :: map()
}).

-endif.
