%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% YAWL Twitter/X Integration Module - Record Definitions
%%
%% @doc Record definitions for YAWL Twitter module
%% @end

%%====================================================================
%% Records
%%====================================================================

-record(tweet, {
    id :: binary(),
    text :: binary(),
    author :: binary(),
    created_at :: integer(),
    metrics :: map()
}).

-record(monitor, {
    id :: reference(),
    type :: hashtag | mentions,
    query :: binary(),
    callback_pid :: pid(),
    last_id :: binary() | undefined
}).
