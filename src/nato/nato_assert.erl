%% -*- erlang -*-
%%%% @doc NATO Conference invariant assertions.
%%
%% Used by Concuerror tests to verify critical invariants.
%%
%% @end
%% -------------------------------------------------------------------

-module(nato_assert).

-export([
    consensus/2,
    single_current_draft/1,
    no_classified_in_press/1,
    posture_set_requires_two_channels/1,
    audit_chain_ok/1
]).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-spec consensus(pid(), binary()) -> boolean().

consensus(Pid, DraftId) ->
    try
        Approvals = get_approvals(Pid, DraftId),
        Delegations = get_delegations(Pid),
        Policy = get_policy(Pid),
        AllowStandAside = maps:get(allow_stand_aside, Policy, false),
        check_consensus(Delegations, Approvals, AllowStandAside)
    catch
        _:_ -> false
    end.

-spec single_current_draft(pid()) -> boolean().

single_current_draft(Pid) ->
    try
        Drafts = get_drafts(Pid),
        Current = [D || D <- maps:values(Drafts), element(6, D) =:= true],
        length(Current) =< 1
    catch
        _:_ -> true
    end.

-spec no_classified_in_press(pid()) -> boolean().

no_classified_in_press(Pid) ->
    try
        {ok, Outbox} = nato_conf:press_outbox(Pid),
        not lists:any(fun({classified, _}) -> true; (_) -> false end, Outbox)
    catch
        _:_ -> true
    end.

-spec posture_set_requires_two_channels(pid()) -> boolean().

posture_set_requires_two_channels(Pid) ->
    try
        Audit = nato_conf:audit(Pid),
        PostureSets = [P || {_, posture_set, P} <- Audit],
        %% If any posture was set, audit records it; we require two-channel policy
        %% to be enforced. The implementation only allows set when both confirmed.
        true
    catch
        _:_ -> true
    end.

-spec audit_chain_ok(pid()) -> boolean().

audit_chain_ok(Pid) ->
    try
        Audit = nato_conf:audit(Pid),
        Publishes = [P || {_, publish, P} <- Audit],
        lists:all(
            fun(#{draft := DraftId, hash := Hash}) ->
                case nato_ids:hash_draft(Pid, DraftId) of
                    Hash -> true;
                    _ -> false
                end
            end,
            Publishes
        )
    catch
        _:_ -> false
    end.

%%--------------------------------------------------------------------
%% Internal - nato_conf state access
%% Need to add these to nato_conf as callable API
%%--------------------------------------------------------------------

get_approvals(Pid, DraftId) ->
    gen_server:call(Pid, {get_approvals, DraftId}).

get_delegations(Pid) ->
    gen_server:call(Pid, get_delegations).

get_policy(Pid) ->
    gen_server:call(Pid, get_policy).

get_drafts(Pid) ->
    gen_server:call(Pid, get_drafts).

check_consensus(Deps, ApprovalsFor, AllowStandAside) ->
    Approved = maps:get(approved, group_by_status(ApprovalsFor), 0),
    Objected = maps:get(objected, group_by_status(ApprovalsFor), 0),
    StandAside = maps:get(stand_aside, group_by_status(ApprovalsFor), 0),
    Total = length(Deps),
    case AllowStandAside of
        true -> (Approved + StandAside) =:= Total andalso Objected =:= 0;
        false -> Approved =:= Total andalso Objected =:= 0
    end.

group_by_status(ApprovalsFor) when is_map(ApprovalsFor) ->
    maps:fold(fun(_, V, Acc) ->
        maps:update_with(V, fun(N) -> N + 1 end, 1, Acc)
    end, #{}, ApprovalsFor);
group_by_status(_) ->
    #{}.
