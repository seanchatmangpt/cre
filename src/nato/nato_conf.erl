%% -*- erlang -*-
%%%% @doc NATO Conference Coordinator - gen_server for governance coordination.
%%
%% Models a conference with drafts, consensus, two-channel posture gate,
%% press filtering, and audit trail. Used by Concuerror stress tests.
%%
%% @end
%% -------------------------------------------------------------------

-module(nato_conf).
-behaviour(gen_server).

%% API
-export([start_link/1, stop/1]).
-export([new_draft/3, propose_edit/4, approve/3, object/4, publish/3]).
-export([recommend_posture/4, confirm_channel/3, set_posture/4]).
-export([push_press/2, press_outbox/1]).
-export([close_session/3, result/1]).
-export([audit/1, get_draft/2]).

%% gen_server
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% Records
%%--------------------------------------------------------------------

-record(draft, {
    id :: binary(),
    title :: binary(),
    content = #{} :: map(),
    created_by :: atom(),
    current = true :: boolean()
}).

-record(state, {
    delegations = [] :: [atom()],
    policy = #{} :: map(),
    drafts = #{} :: #{binary() => #draft{}},
    draft_ids = [] :: [binary()],
    approvals = #{} :: #{binary() => #{atom() => approved | objected | stand_aside}},
    posture_recommendations = #{} :: #{binary() => #{atom() => integer()}},
    posture_confirmations = #{} :: #{binary() => #{atom() => boolean()}},
    posture = undefined :: integer() | undefined,
    posture_set_ok = false :: boolean(),
    press_outbox_list = [] :: [term()],
    audit = [] :: [term()],
    result = in_progress :: published | chair_summary | adjourned | in_progress,
    rounds = 0 :: non_neg_integer(),
    next_pub_id = 0 :: non_neg_integer()
}).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

start_link(#{delegations := Deps} = Config) when is_list(Deps) ->
    Policy = maps:get(policy, Config, #{}),
    gen_server:start_link(?MODULE, {Deps, Policy}, []).

stop(Pid) ->
    gen_server:stop(Pid).

%% Draft lifecycle
new_draft(Pid, By, TitleBin) ->
    gen_server:call(Pid, {new_draft, By, TitleBin}).

propose_edit(Pid, By, DraftId, Edit) ->
    gen_server:call(Pid, {propose_edit, By, DraftId, Edit}).

approve(Pid, By, DraftId) ->
    gen_server:call(Pid, {approve, By, DraftId}).

object(Pid, By, DraftId, Reason) ->
    gen_server:call(Pid, {object, By, DraftId, Reason}).

publish(Pid, By, DraftId) ->
    gen_server:call(Pid, {publish, By, DraftId}).

%% Posture gate
recommend_posture(Pid, Source, Level, EvidenceId) ->
    gen_server:call(Pid, {recommend_posture, Source, Level, EvidenceId}).

confirm_channel(Pid, Channel, EvidenceId) ->
    gen_server:call(Pid, {confirm_channel, Channel, EvidenceId}).

set_posture(Pid, By, Level, EvidenceId) ->
    gen_server:call(Pid, {set_posture, By, Level, EvidenceId}).

%% Press
push_press(Pid, Payload) ->
    gen_server:call(Pid, {push_press, Payload}).

press_outbox(Pid) ->
    gen_server:call(Pid, press_outbox).

%% Termination
close_session(Pid, By, Reason) ->
    gen_server:call(Pid, {close_session, By, Reason}).

result(Pid) ->
    gen_server:call(Pid, result).

%% Audit
audit(Pid) ->
    gen_server:call(Pid, audit).

get_draft(Pid, DraftId) ->
    gen_server:call(Pid, {get_draft, DraftId}).

%%--------------------------------------------------------------------
%% gen_server callbacks
%%--------------------------------------------------------------------

init({Deps, Policy}) ->
    Audit = [{erlang:system_time(millisecond), session_started, #{delegations => Deps}}],
    {ok, #state{
        delegations = Deps,
        policy = Policy,
        audit = Audit
    }}.

handle_call({new_draft, By, TitleBin}, _From, #state{drafts = Drafts, draft_ids = Ids} = S) ->
    Id = <<"draft_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>,
    Draft = #draft{id = Id, title = TitleBin, created_by = By, content = #{}},
    %% Mark all drafts as not current
    Drafts1 = maps:map(fun(_, D) -> D#draft{current = false} end, Drafts),
    Drafts2 = Drafts1#{Id => Draft},
    Ids1 = [Id | Ids],
    Audit = append_audit(S#state.audit, draft_created, #{id => Id, by => By, title => TitleBin}),
    {reply, Id, S#state{drafts = Drafts2, draft_ids = Ids1, audit = Audit}};

handle_call({propose_edit, _By, DraftId, {replace, Key, Value}}, _From, #state{drafts = Drafts} = S) ->
    case maps:get(DraftId, Drafts, undefined) of
        #draft{content = C} = D ->
            C1 = maps:put(Key, Value, C),
            D1 = D#draft{content = C1},
            Drafts1 = maps:put(DraftId, D1, Drafts),
            Audit = append_audit(S#state.audit, edit, #{draft => DraftId, op => replace, key => Key}),
            {reply, ok, S#state{drafts = Drafts1, audit = Audit}};
        _ ->
            {reply, {error, not_found}, S}
    end;

handle_call({propose_edit, _By, DraftId, {append, Key, Value}}, _From, #state{drafts = Drafts} = S) ->
    case maps:get(DraftId, Drafts, undefined) of
        #draft{content = C} = D ->
            Existing = maps:get(Key, C, <<>>),
            Appended = case is_binary(Existing) of
                true -> <<Existing/binary, Value/binary>>;
                false -> Existing
            end,
            C1 = maps:put(Key, Appended, C),
            D1 = D#draft{content = C1},
            Drafts1 = maps:put(DraftId, D1, Drafts),
            Audit = append_audit(S#state.audit, edit, #{draft => DraftId, op => append, key => Key}),
            {reply, ok, S#state{drafts = Drafts1, audit = Audit}};
        _ ->
            {reply, {error, not_found}, S}
    end;

handle_call({approve, By, DraftId}, _From, #state{delegations = Deps, approvals = Approvals} = S) ->
    ApprovalsFor = maps:get(DraftId, Approvals, #{}),
    ApprovalsFor1 = maps:put(By, approved, ApprovalsFor),
    Approvals1 = maps:put(DraftId, ApprovalsFor1, Approvals),
    Audit = append_audit(S#state.audit, approval, #{draft => DraftId, by => By}),
    {reply, ok, S#state{approvals = Approvals1, audit = Audit}};

handle_call({object, By, DraftId, Reason}, _From, #state{approvals = Approvals} = S) ->
    ApprovalsFor = maps:get(DraftId, Approvals, #{}),
    ApprovalsFor1 = maps:put(By, objected, ApprovalsFor),
    Approvals1 = maps:put(DraftId, ApprovalsFor1, Approvals),
    Audit = append_audit(S#state.audit, objection, #{draft => DraftId, by => By, reason => Reason}),
    {reply, ok, S#state{approvals = Approvals1, audit = Audit}};

handle_call({publish, By, DraftId}, _From, #state{delegations = Deps, approvals = Approvals,
        policy = Policy, drafts = Drafts, next_pub_id = N} = S) ->
    case maps:get(DraftId, Drafts, undefined) of
        #draft{content = Content} = _D ->
            ApprovalsFor = maps:get(DraftId, Approvals, #{}),
            Unanimous = maps:get(publish_requires, Policy, unanimous) =:= unanimous,
            AllowStandAside = maps:get(allow_stand_aside, Policy, false),
            Consensus = consensus_met(Deps, ApprovalsFor, Unanimous, AllowStandAside),
            if Consensus ->
                PubId = <<"pub_", (integer_to_binary(N))/binary>>,
                Hash = crypto:hash(sha256, term_to_binary(Content)),
                Audit = append_audit(S#state.audit, publish, #{draft => DraftId, by => By,
                    pub_id => PubId, hash => Hash}),
                {reply, {ok, PubId}, S#state{audit = Audit, result = published,
                    next_pub_id = N + 1}};
            true ->
                {reply, {error, not_consensus}, S}
            end;
        _ ->
            {reply, {error, not_found}, S}
    end;

handle_call({recommend_posture, Source, Level, EvidenceId}, _From,
        #state{posture_recommendations = Recs} = S) ->
    RecsFor = maps:get(EvidenceId, Recs, #{}),
    RecsFor1 = maps:put(Source, Level, RecsFor),
    Recs1 = maps:put(EvidenceId, RecsFor1, Recs),
    {reply, ok, S#state{posture_recommendations = Recs1}};

handle_call({confirm_channel, Channel, EvidenceId}, _From,
        #state{posture_confirmations = Confs} = S) ->
    ConfsFor = maps:get(EvidenceId, Confs, #{}),
    ConfsFor1 = maps:put(Channel, true, ConfsFor),
    Confs1 = maps:put(EvidenceId, ConfsFor1, Confs),
    {reply, ok, S#state{posture_confirmations = Confs1}};

handle_call({set_posture, By, Level, EvidenceId}, _From,
        #state{posture_confirmations = Confs, policy = Policy} = S) ->
    RequireTwo = maps:get(posture_requires_two_channels, Policy, true),
    ConfsFor = maps:get(EvidenceId, Confs, #{}),
    Intel = maps:get(intel, ConfsFor, false),
    Mil = maps:get(mil, ConfsFor, false),
    Ok = case RequireTwo of
        true -> Intel andalso Mil;
        false -> true
    end,
    if Ok ->
        Audit = append_audit(S#state.audit, posture_set, #{by => By, level => Level,
            evidence_id => EvidenceId}),
        {reply, {ok, <<"set_", EvidenceId/binary>>},
            S#state{posture = Level, posture_set_ok = true, audit = Audit}};
    true ->
        {reply, {error, missing_confirm}, S}
    end;

handle_call({push_press, Payload}, _From, #state{press_outbox_list = Outbox, policy = Policy} = S) ->
    Filter = maps:get(press_receives, Policy, declassified_only),
    NewOutbox = case Filter of
        declassified_only ->
            case Payload of
                {declassified, _} -> Outbox ++ [Payload];
                _ -> Outbox
            end;
        _ ->
            Outbox ++ [Payload]
    end,
    {reply, ok, S#state{press_outbox_list = NewOutbox}};

handle_call(press_outbox, _From, #state{press_outbox_list = Outbox} = S) ->
    {reply, Outbox, S};

handle_call({close_session, By, Reason}, _From, #state{policy = Policy, result = Res, rounds = Rounds} = S) ->
    Rounds1 = Rounds + 1,
    NewRes = case Reason of
        timeout_rounds ->
            case maps:get(fallback, Policy, undefined) of
                chair_summary_after_rounds ->
                    MaxRounds = maps:get(max_rounds, Policy, 2),
                    %% One close_session = one round; after max_rounds without consensus -> chair_summary
                    if Rounds1 >= MaxRounds -> chair_summary;
                       Res =:= published -> published;
                       true -> in_progress
                    end;
                _ -> Res
            end;
        _ ->
            adjourned
    end,
    Audit = append_audit(S#state.audit, session_closed, #{by => By, reason => Reason}),
    {reply, ok, S#state{result = NewRes, rounds = Rounds1, audit = Audit}};

handle_call(result, _From, S) ->
    {reply, S#state.result, S};

handle_call(audit, _From, S) ->
    {reply, S#state.audit, S};

handle_call({get_draft, DraftId}, _From, #state{drafts = Drafts} = S) ->
    case maps:get(DraftId, Drafts, undefined) of
        #draft{content = C} -> {reply, {ok, C}, S};
        _ -> {reply, {error, not_found}, S}
    end;

handle_call({get_approvals, DraftId}, _From, #state{approvals = Approvals} = S) ->
    ApprovalsFor = maps:get(DraftId, Approvals, #{}),
    {reply, ApprovalsFor, S};

handle_call(get_delegations, _From, #state{delegations = Deps} = S) ->
    {reply, Deps, S};

handle_call(get_policy, _From, #state{policy = Policy} = S) ->
    {reply, Policy, S};

handle_call(get_drafts, _From, #state{drafts = Drafts} = S) ->
    {reply, Drafts, S};

handle_call(_, _From, S) ->
    {reply, {error, unknown}, S}.

handle_cast(_, S) ->
    {noreply, S}.

handle_info(_, S) ->
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

code_change(_OldVsn, S, _Extra) ->
    {ok, S}.

%%--------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------

consensus_met(Deps, ApprovalsFor, _Unanimous, AllowStandAside) ->
    Approved = maps:get(approved, group_by_status(ApprovalsFor), 0),
    Objected = maps:get(objected, group_by_status(ApprovalsFor), 0),
    StandAside = maps:get(stand_aside, group_by_status(ApprovalsFor), 0),
    Total = length(Deps),
    case AllowStandAside of
        true -> (Approved + StandAside) =:= Total andalso Objected =:= 0;
        false -> Approved =:= Total andalso Objected =:= 0
    end.

group_by_status(ApprovalsFor) ->
    maps:fold(fun(_, V, Acc) ->
        maps:update_with(V, fun(N) -> N + 1 end, 1, Acc)
    end, #{}, ApprovalsFor).

append_audit(Audit, Type, Payload) ->
    Audit ++ [{erlang:system_time(millisecond), Type, Payload}].
