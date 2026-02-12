%%% @doc Scheduler policy record definitions
%%%
%%% Include this file to access scheduler policy records.
%%%
%%% @end

-ifndef(LN_CTRL_SCHED_HRL).
-define(LN_CTRL_SCHED_HRL, true).

%%% TYPES ====================================================================

-record(deterministic, {
    name = deterministic :: atom()
}).

-record(nondeterministic, {
    name = nondeterministic :: atom(),
    choice_log = [] :: [{non_neg_integer(), non_neg_integer()}]
}).

-record(replay, {
    name = replay :: atom(),
    choice_log :: [{non_neg_integer(), non_neg_integer()}] | undefined,
    index = 0 :: non_neg_integer()
}).

-endif.
