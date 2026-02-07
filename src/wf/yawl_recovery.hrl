%%-------------------------------------------------------------------
%% @doc
%% Record definitions for yawl_recovery module
%%
%% @end
%%-------------------------------------------------------------------

-record(yawl_checkpoint, {
    spec_id :: binary(),
    case_id :: binary(),
    checkpoint_id :: binary(),
    marking :: term(),
    data :: map(),
    timestamp :: integer(),
    version :: non_neg_integer()
}).
