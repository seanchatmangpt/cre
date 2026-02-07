%% -*- erlang -*-
%%%% @doc YAWL Recovery Records
%%
%% Record definitions for yawl_recovery module.
%%
%% @end
%% -------------------------------------------------------------------

-record(yawl_checkpoint, {
    checkpoint_id :: binary(),  % Primary key - MUST be first for Mnesia
    spec_id :: binary(),
    case_id :: binary(),
    marking :: pnet_types:marking(),
    data :: map(),
    timestamp :: integer(),
    version :: non_neg_integer()
}).
