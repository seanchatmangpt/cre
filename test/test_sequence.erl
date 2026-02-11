-module(test_sequence).
-include("cre_yawl.hrl").
-export([test/0]).

test() ->
    Pattern = yawl_pattern_reference:sequence([task1, task2]),

    %% Get pattern info
    IsEnabled = Pattern#wcp_pattern.is_enabled,
    Fire = Pattern#wcp_pattern.fire,
    Preset = Pattern#wcp_pattern.preset,
    Postset = Pattern#wcp_pattern.postset,

    io:format("Preset: ~p~n", [Preset]),
    io:format("Postset: ~p~n", [Postset]),

    %% Simulate execution
    InitialMarking = maps:merge(Pattern#wcp_pattern.initial_marking, #{p_input => [input]}),
    io:format("Initial: ~p~n", [InitialMarking]),

    %% Step 1: t_start
    case Fire(t_start, InitialMarking) of
        {produce, AfterTStart} ->
            io:format("After t_start: ~p~n", [AfterTStart]),

            %% Check enabled
            Enabled = [T || T <- Pattern#wcp_pattern.transitions, IsEnabled(T, AfterTStart)],
            io:format("Enabled: ~p~n", [Enabled]),

            %% Step 2: transition 1
            case lists:member(1, Enabled) of
                true ->
                    {produce, After1} = Fire(1, AfterTStart),
                    io:format("After transition 1: ~p~n", [After1]),

                    %% Check enabled again
                    Enabled2 = [T || T <- Pattern#wcp_pattern.transitions, IsEnabled(T, After1)],
                    io:format("Enabled after 1: ~p~n", [Enabled2]),

                    halt(0);
                false ->
                    io:format("Transition 1 NOT enabled!~n"),
                    halt(1)
            end;
        Other ->
            io:format("Unexpected result from t_start: ~p~n", [Other]),
            halt(1)
    end.
