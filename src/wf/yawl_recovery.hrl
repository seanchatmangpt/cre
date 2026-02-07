%%-------------------------------------------------------------------
%% @doc
%% Record definitions for yawl_recovery module
%%
%% @end
%%-------------------------------------------------------------------

%% checkpoint_id first - it is the Mnesia table key for mnesia:read/2
-record(yawl_checkpoint, {
    checkpoint_id :: binary(),
    spec_id :: binary(),
    case_id :: binary(),
    marking :: term(),
    data :: map(),
    timestamp :: integer(),
    version :: non_neg_integer()
}).
