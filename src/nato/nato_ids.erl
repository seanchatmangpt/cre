%% -*- erlang -*-
%%%% @doc NATO ID and hashing utilities.
%%
%% Provides draft hashing for audit chain integrity.
%%
%% @end
%% -------------------------------------------------------------------

-module(nato_ids).

-export([hash_draft/2]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec hash_draft(pid(), binary()) -> binary() | {error, term()}.

hash_draft(Pid, DraftId) ->
    case nato_conf:get_draft(Pid, DraftId) of
        {ok, Content} ->
            crypto:hash(sha256, term_to_binary(Content));
        {error, _} = Err ->
            Err
    end.
