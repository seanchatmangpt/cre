%% -*- erlang -*-
%%%% -------------------------------------------------------------------
%%%% @doc wfnet_multi_choice record definitions
%%%%
%%%% Multi-Choice Workflow Pattern (WCP-07) state record.
%%%% -------------------------------------------------------------------

-ifndef(WFNET_MULTI_CHOICE_HRL).
-define(WFNET_MULTI_CHOICE_HRL, 1).

%%--------------------------------------------------------------------
%% @doc Multi-choice workflow state record.
%%
%% Tracks the state of a multi-choice workflow pattern.
%%--------------------------------------------------------------------
-record(multi_choice_state, {
    branches :: #{atom() => map()},           %% Branch configurations
    branch_count :: pos_integer(),             %% Number of branches
    selected = [] :: [atom()],                 %% Currently selected branches
    completed = [] :: [atom()],                %% Completed branches
    selection_mode = some :: atom(),           %% all | some | one
    allow_none = false :: boolean(),           %% Allow no branches selected
    merge_mode = sync :: atom(),               %% sync | async merge
    results = #{} :: #{atom() => term()}       %% Branch execution results
}).

-endif.
