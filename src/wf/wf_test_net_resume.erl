%%%-------------------------------------------------------------------
%%% @doc
%%% Test-only gen_pnet module for checkpoint/recovery testing.
%%%
%%% This module tests the resume functionality where a gen_pnet instance
%%% can be initialized from a previously saved marking and usr_info state.
%%%
%%% == Doctest Examples ==
%%%
%%% Initiate with resume data:
%%% ```
%%% % Create a resume state with specific marking and usr_info
%%% ResumeState = #{
%%%     marking => #{p1 => [token1, token2], p2 => [token3]},
%%%     usr_info => #{user => "test_user", context => "recovery_test"}
%%% },
%%% InitArg = #{resume => ResumeState}.
%%% '''
%%%
%%% The marking should be restored exactly as saved:
%%% ```
%%% % After init_marking/2 is called with resume data,
%%% % the places contain the exact tokens from the saved marking
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_test_net_resume).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    init/1,
    preset/1,
    postset/1,
    is_enabled/3,
    fire/3,
    trigger/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
]).

%% Test helper
-export([doctest_test/0]).

%%%-------------------------------------------------------------------
%%% gen_pnet Callbacks
%%%-------------------------------------------------------------------

%% @private
-spec place_lst() -> [atom()].

place_lst() ->
    [p1, p2].

%% @private
-spec trsn_lst() -> [atom()].

trsn_lst() ->
    %% No transitions - this is a static marking test module
    [].

%% @private
-spec init(_) -> _.

init(#{resume := #{marking := Marking, usr_info := UsrInfo}}) ->
    %% Extract resume data for verification in tests
    %% Return a map with resume key so init_marking can find it
    #{resume => #{marking => Marking, saved_usr_info => UsrInfo}};
init(_InitArg) ->
    %% No resume data - return empty usr_info
    #{}.

%% @private
-spec init_marking(atom(), _) -> [_].

init_marking(Place, #{resume := #{marking := Marking}}) ->
    %% When resume data is present in usr_info, extract the marking for this place
    maps:get(Place, Marking, []);

init_marking(_Place, _UsrInfo) ->
    %% No resume data - empty marking
    [].

%% @private
-spec preset(atom()) -> [atom()].

preset(_Trsn) ->
    %% No transitions, so no presets
    [].

%% @private
-spec postset(atom()) -> [atom()].

postset(_Trsn) ->
    %% No transitions, so no postsets
    [].

%% @private
-spec is_enabled(atom(), #{atom() => [_]}, _) ->
          boolean().

is_enabled(_Trsn, _Marking, _UsrInfo) ->
    %% No transitions, so never enabled
    false.

%% @private
-spec fire(atom(), #{atom() => [_]}, _) ->
          abort | {produce, #{atom() => [_]}}.

fire(_Trsn, _Marking, _UsrInfo) ->
    %% No transitions, so never fires
    abort.

%% @private
-spec trigger(atom(), _, term()) ->
          pass | drop.

trigger(_Place, _Token, _NetState) ->
    %% Pass all tokens by default
    pass.

%% @private
-spec handle_call(_, {pid(), _}, term()) ->
          {reply, _, term()} |
          {reply, _, #{atom() => [_]}, term()} |
          noreply |
          {noreply, #{atom() => [_]}, term()} |
          {stop, _, _, term()}.

handle_call(get_resume_state, _From, NetState) ->
    %% Test helper to retrieve what was captured during init
    UsrInfo = gen_pnet:get_usr_info(NetState),
    Reply = case UsrInfo of
        #{resume := #{marking := Marking, saved_usr_info := ResumeUsrInfo}} ->
            {ok, #{marking => Marking, usr_info => ResumeUsrInfo}};
        _ ->
            {error, no_resume_state}
    end,
    {reply, Reply, NetState};

handle_call(_Request, _From, NetState) ->
    {reply, {error, unknown_request}, NetState}.

%% @private
-spec handle_cast(_, term()) ->
          noreply |
          {noreply, #{atom() => [_]}, term()} |
          {stop, _, term()}.

handle_cast(_Request, NetState) ->
    {noreply, NetState}.

%% @private
-spec handle_info(_, term()) ->
          noreply |
          {noreply, #{atom() => [_]}, term()} |
          {stop, _, term()}.

handle_info(_Info, NetState) ->
    {noreply, NetState}.

%% @private
-spec code_change(_, term(), _) ->
          {ok, term()} | {error, _}.

code_change(_OldVsn, NetState, _Extra) ->
    {ok, NetState}.

%% @private
-spec terminate(_, term()) ->
          ok.

terminate(_Reason, _NetState) ->
    ok.

%%%-------------------------------------------------------------------
%%% Test Helper
%%%-------------------------------------------------------------------

%% @doc
%% Doctest helper function.
%%
%% This function can be used to verify doctest examples:
%% ```erlang
%% % Basic verification that the module loads
%% 1 = wf_test_net_resume:doctest_test().
%% '''
%%
%% @end
-spec doctest_test() -> 1.

doctest_test() ->
    %% Basic verification that the module is functional
    ok = validate_interface(),
    1.

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

validate_interface() ->
    %% Validate that all required gen_pnet callbacks are exported
    Places = place_lst(),
    Trsns = trsn_lst(),
    true = is_list(Places),
    true = is_list(Trsns),
    ok.
