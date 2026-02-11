%%%-------------------------------------------------------------------
%%% @doc ln_trace_replay - Trace replay and verification.
%%%
%%% Provides functionality to replay workflow execution and verify
%%% that the trace matches exactly.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ln_trace_replay).

%% API
-export([verify/2]).
-export([diff/2]).

%% Types
-export_type([diff_result/0]).

%%%-------------------------------------------------------------------
%%% Types
%%%-------------------------------------------------------------------

-type diff_result() :: #{
    missing := [ln_trace:event()],
    extra := [ln_trace:event()],
    different := [diff_detail()]
}.

-type diff_detail() :: #{
    seq := non_neg_integer(),
    expected := ln_trace:event(),
    actual := ln_trace:event()
}.

%%%-------------------------------------------------------------------
%%% API
%%%-------------------------------------------------------------------

%% @doc Verify two traces match exactly.
-spec verify(ln_trace:state(), ln_trace:state()) -> ok | {error, diff_result()}.
verify(Trace1, Trace2) ->
    Events1 = ln_trace:get_all(Trace1),
    Events2 = ln_trace:get_all(Trace2),
    case Events1 =:= Events2 of
        true ->
            ok;
        false ->
            {error, diff(Trace1, Trace2)}
    end.

%% @doc Compute differences between two traces.
-spec diff(ln_trace:state(), ln_trace:state()) -> diff_result().
diff(Trace1, Trace2) ->
    %% Get events from both traces
    Events1 = ln_trace:get_all(Trace1),
    Events2 = ln_trace:get_all(Trace2),

    %% Index events by sequence number for efficient comparison
    Map1 = index_by_seq(Events1),
    Map2 = index_by_seq(Events2),

    %% Get all sequence numbers from both traces
    Seqs1 = maps:keys(Map1),
    Seqs2 = maps:keys(Map2),
    AllSeqs = lists:usort(Seqs1 ++ Seqs2),

    %% Find missing, extra, and different events
    %% Missing = events in trace 2 but not in trace 1 (expected but not found)
    %% Extra = events in trace 1 but not in trace 2 (found but not expected)
    {Missing, Extra, Different} = lists:foldl(fun(Seq, {Mis, Ext, Diff}) ->
        case {maps:find(Seq, Map1), maps:find(Seq, Map2)} of
            {{ok, Event1}, error} ->
                %% Event is in trace 1 but not in trace 2 (extra in trace 1)
                {Mis, Ext ++ [Event1], Diff};
            {error, {ok, Event2}} ->
                %% Event is in trace 2 but not in trace 1 (missing from trace 1)
                {Mis ++ [Event2], Ext, Diff};
            {{ok, Event1}, {ok, Event2}} when Event1 =:= Event2 ->
                %% Events match
                {Mis, Ext, Diff};
            {{ok, Event1}, {ok, Event2}} ->
                %% Events differ
                {Mis, Ext, Diff ++ [#{
                    seq => Seq,
                    expected => Event1,
                    actual => Event2
                }]}
        end
    end, {[], [], []}, AllSeqs),

    #{
        missing => lists:reverse(Missing),
        extra => lists:reverse(Extra),
        different => lists:reverse(Different)
    }.

%%%-------------------------------------------------------------------
%%% Internal functions
%%%-------------------------------------------------------------------

%% @doc Index events by sequence number.
index_by_seq(Events) ->
    maps:from_list([{maps:get(seq, E), E} || E <- Events]).
