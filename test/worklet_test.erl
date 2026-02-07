%% Worklet integration test
-module(worklet_test).
-include_lib("eunit/include/eunit.hrl").

%% Test apply_worklet with matching rules
apply_worklet_match_test() ->
    Rules = [
        #{task => approve, 'when' => {gt, amount, 1000}, choose => approve_big},
        #{task => approve, 'when' => any, choose => approve_small}
    ],
    Worklet = wf_worklet:new(Rules),

    %% Test gt condition match
    {ok, approve_big} = wf_engine:apply_worklet(Worklet, approve, #{amount => 5000}, 0),

    %% Test any condition match (catch-all)
    {ok, approve_small} = wf_engine:apply_worklet(Worklet, approve, #{amount => 100}, 0),

    %% Test no matching rule
    {error, no_matching_rule} = wf_engine:apply_worklet(Worklet, unknown_task, #{}, 0),

    ok.

%% Test apply_worklet with eq condition
apply_worklet_eq_test() ->
    Rules = [
        #{task => route, 'when' => {eq, region, us}, choose => us_flow},
        #{task => route, 'when' => {eq, region, eu}, choose => eu_flow},
        #{task => route, 'when' => any, choose => default_flow}
    ],
    Worklet = wf_worklet:new(Rules),

    %% Test eq matches
    {ok, us_flow} = wf_engine:apply_worklet(Worklet, route, #{region => us}, 0),
    {ok, eu_flow} = wf_engine:apply_worklet(Worklet, route, #{region => eu}, 0),
    {ok, default_flow} = wf_engine:apply_worklet(Worklet, route, #{region => asia}, 0),

    ok.

%% Test apply_worklet with exists condition
apply_worklet_exists_test() ->
    Rules = [
        #{task => auth, 'when' => {exists, token}, choose => authenticated_flow},
        #{task => auth, 'when' => any, choose => require_login}
    ],
    Worklet = wf_worklet:new(Rules),

    %% Test exists match
    {ok, authenticated_flow} = wf_engine:apply_worklet(Worklet, auth, #{token => "abc123"}, 0),
    {ok, require_login} = wf_engine:apply_worklet(Worklet, auth, #{}, 0),

    ok.
