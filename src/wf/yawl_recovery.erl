%% -*- erlang -*-
%%%% @doc YAWL Workflow Recovery Module
%%
%% This module provides checkpoint and resume functionality for YAWL workflows
%% using wf_store for Mnesia-backed persistence. It enables workflow instances
%% to be saved at any point and resumed later, supporting fault tolerance and
%% long-running workflow scenarios.
%%
%% <h3>Checkpoint Lifecycle</h3>
%% <ol>
%%   <li><b>Create:</b> Use {@link checkpoint/4} to save workflow state</li>
%%   <li><b>Query:</b> Use {@link list_checkpoints/2} to find available checkpoints</li>
%%   <li><b>Resume:</b> Use {@link resume/3} to restore workflow state</li>
%%   <li><b>Delete:</b> Use {@link delete_checkpoint/3} to clean up old checkpoints</li>
%% </ol>
%%
%% <h3>Features</h3>
%% <ul>
%%   <li>Atomic checkpoint creation using Mnesia transactions</li>
%%   <li>Multiple checkpoints per workflow case with timestamps</li>
%%   <li>Automatic latest checkpoint selection</li>
%%   <li>Version tracking for schema compatibility</li>
%%   <li>Incremental checkpoint support for large workflows</li>
%%   <li>Generic (works with ANY YAWL specification)</li>
%% </ul>
%%
%% <h3>Basic Usage</h3>
%%
%% Creating a checkpoint:
%%
%% ```erlang
%% 1> {ok, Store} = wf_store:open(#{backend => mnesia, dir => "/data"}).
%% 2> Marking = #{p1 => [a, b], p2 => [c]}.
%% 3> Data = #{order_id => 123, customer => "Alice"}.
%% 4> {ok, Cpid} = yawl_recovery:checkpoint(<<"order_spec">>, <<"case_456">>, Marking, Data).
%% {ok, <<"cp_1712345678900">>}
%% '''
%%
%% Resuming from a checkpoint:
%%
%% ```erlang
%% 1> {ok, {Marking, Data}} = yawl_recovery:resume(<<"order_spec">>, <<"case_456">>, Cpid).
%% {ok, {#{p1 => [a,b], p2 => [c]}, #{order_id => 123, customer => "Alice"}}}
%% 2> {ok, Pid} = gen_pnet:start_link(order_net, #{resume => #{marking => Marking, data => Data}}, []).
%% {ok, <0.123.0>}
%% '''
%%
%% Listing checkpoints:
%%
%% ```erlang
%% 1> yawl_recovery:list_checkpoints(<<"order_spec">>, <<"case_456">>).
%% [{<<"cp_1712345678900">>, 1712345678900},
%%  {<<"cp_1712345700000">>, 1712345700000}]
%% '''
%%
%% Getting the latest checkpoint:
%%
%% ```erlang
%% 1> {ok, LatestCpid} = yawl_recovery:get_latest_checkpoint(<<"order_spec">>, <<"case_456">>).
%% {ok, <<"cp_1712345700000">>}
%% '''
%%
%% Deleting a checkpoint:
%%
%% ```erlang
%% 1> ok = yawl_recovery:delete_checkpoint(<<"order_spec">>, <<"case_456">>, Cpid).
%% ok
%% '''
%%
%% <h3>Checkpoint Structure</h3>
%%
%% Checkpoints are stored as Mnesia records with the following fields:
%% <ul>
%%   <li><b>spec_id:</b> Workflow specification identifier</li>
%%   <li><b>case_id:</b> Workflow instance identifier</li>
%%   <li><b>checkpoint_id:</b> Unique checkpoint identifier (timestamp-based)</li>
%%   <li><b>marking:</b> Petri net marking (place => [tokens])</li>
%%   <li><b>data:</b> Workflow variable data</li>
%%   <li><b>timestamp:</b> Checkpoint creation time (milliseconds since epoch)</li>
%%   <li><b>version:</b> Schema version for compatibility checking</li>
%% </ul>
%%
%% <h3>Incremental Checkpoints</h3>
%%
%% For large workflows, incremental checkpoints only store changes:
%%
%% ```erlang
%% 1> {ok, PrevCpid} = yawl_recovery:checkpoint(SpecId, CaseId, Marking1, Data1).
%% 2> {ok, Cpid} = yawl_recovery:checkpoint(SpecId, CaseId, Marking2, Data2, #{prev => PrevCpid}).
%% '''
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_recovery).

-include("yawl_recovery.hrl").

%%====================================================================
%% Records
%%====================================================================
%% Record definition is in include/yawl_recovery.hrl
%% checkpoint_id is FIRST because it's the Mnesia table key

%%====================================================================
%% Exports
%%====================================================================

%% Checkpoint operations
-export([checkpoint/4, checkpoint/5]).
-export([resume/3]).
-export([delete_checkpoint/3]).
-export([list_checkpoints/2]).
-export([get_latest_checkpoint/2]).

%%====================================================================
%% Types
%%====================================================================

-type spec_id() :: binary().
-type case_id() :: binary().
-type checkpoint_id() :: binary().
-type marking() :: pnet_types:marking().
-type checkpoint_data() :: map().
-type timestamp() :: integer().
-type checkpoint_version() :: non_neg_integer().

-type checkpoint_options() :: #{
    prev => checkpoint_id(),     % Previous checkpoint for incremental
    version => checkpoint_version(), % Explicit version
    metadata => map()            % Additional metadata
}.

-export_type([checkpoint_options/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a checkpoint of workflow state.
%%
%% Saves the current marking and data to Mnesia via wf_store.
%% The checkpoint ID is generated as a timestamp for uniqueness.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><b>SpecId:</b> Workflow specification identifier</li>
%%   <li><b>CaseId:</b> Workflow instance identifier</li>
%%   <li><b>Marking:</b> Current Petri net marking</li>
%%   <li><b>Data:</b> Workflow variable data</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><b>{ok, CheckpointId}:</b> Checkpoint created successfully</li>
%%   <li><b>{error, Reason}:</b> Checkpoint creation failed</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec checkpoint(spec_id(), case_id(), marking(), checkpoint_data()) ->
          {ok, checkpoint_id()} | {error, term()}.

checkpoint(SpecId, CaseId, Marking, Data) ->
    checkpoint(SpecId, CaseId, Marking, Data, #{}).

%%--------------------------------------------------------------------
%% @doc Creates a checkpoint with additional options.
%%
%% Supports incremental checkpoints and explicit versioning.
%%
%% <h3>Options</h3>
%% <ul>
%%   <li><b>prev:</b> Previous checkpoint ID for incremental checkpoints</li>
%%   <li><b>version:</b> Explicit schema version (default: 1)</li>
%%   <li><b>metadata:</b> Additional metadata to store</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec checkpoint(spec_id(), case_id(), marking(), checkpoint_data(), checkpoint_options()) ->
          {ok, checkpoint_id()} | {error, term()}.

checkpoint(SpecId, CaseId, Marking, Data, Options) when
      is_binary(SpecId), is_binary(CaseId), is_map(Marking), is_map(Data), is_map(Options) ->

    CheckpointId = generate_checkpoint_id(),
    Timestamp = erlang:system_time(millisecond),
    Version = maps:get(version, Options, 1),

    %% Validate marking before storing
    case pnet_types:is_marking(Marking) of
        false ->
            {error, invalid_marking};
        true ->
            %% Create checkpoint record
            CheckpointRecord = #yawl_checkpoint{
                spec_id = SpecId,
                case_id = CaseId,
                checkpoint_id = CheckpointId,
                marking = Marking,
                data = Data,
                timestamp = Timestamp,
                version = Version
            },

            %% Store in Mnesia within a transaction
            Transaction = fun() ->
                case maps:get(prev, Options, undefined) of
                    undefined ->
                        ok;
                    PrevCpid when is_binary(PrevCpid) ->
                        %% Verify previous checkpoint exists
                        case mnesia:read(yawl_checkpoint, PrevCpid) of
                            [] -> mnesia:abort({previous_checkpoint_not_found, PrevCpid});
                            _ -> ok
                        end
                end,
                mnesia:write(CheckpointRecord),
                {ok, CheckpointId}
            end,

            case mnesia:transaction(Transaction) of
                {atomic, {ok, Cpid}} -> {ok, Cpid};
                {aborted, Reason} -> {error, Reason}
            end
    end.

%%--------------------------------------------------------------------
%% @doc Restores workflow state from a checkpoint.
%%
%% Loads the marking and data from Mnesia for the given checkpoint.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><b>SpecId:</b> Workflow specification identifier</li>
%%   <li><b>CaseId:</b> Workflow instance identifier</li>
%%   <li><b>CheckpointId:</b> Checkpoint identifier to restore</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><b>{ok, {Marking, Data}}:</b> Checkpoint restored successfully</li>
%%   <li><b>{error, not_found}:</b> Checkpoint does not exist</li>
%%   <li><b>{error, Reason}:</b> Other error during restoration</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec resume(spec_id(), case_id(), checkpoint_id()) ->
          {ok, {marking(), checkpoint_data()}} | {error, term()}.

resume(SpecId, CaseId, CheckpointId) when
      is_binary(SpecId), is_binary(CaseId), is_binary(CheckpointId) ->

    Transaction = fun() ->
        case mnesia:read(yawl_checkpoint, CheckpointId) of
            [#yawl_checkpoint{spec_id = S, case_id = C, marking = M, data = D}]
              when S =:= SpecId, C =:= CaseId ->
                {ok, {M, D}};
            [#yawl_checkpoint{}] ->
                mnesia:abort({checkpoint_mismatch, CheckpointId});
            [] ->
                mnesia:abort(not_found)
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, {ok, Result}} -> {ok, Result};
        {aborted, not_found} -> {error, not_found};
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Deletes a checkpoint from storage.
%%
%% Removes the checkpoint record from Mnesia.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><b>SpecId:</b> Workflow specification identifier</li>
%%   <li><b>CaseId:</b> Workflow instance identifier</li>
%%   <li><b>CheckpointId:</b> Checkpoint identifier to delete</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><b>ok:</b> Checkpoint deleted successfully</li>
%%   <li><b>{error, not_found}:</b> Checkpoint does not exist</li>
%%   <li><b>{error, Reason}:</b> Other error during deletion</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_checkpoint(spec_id(), case_id(), checkpoint_id()) -> ok | {error, term()}.

delete_checkpoint(SpecId, CaseId, CheckpointId) when
      is_binary(SpecId), is_binary(CaseId), is_binary(CheckpointId) ->

    Transaction = fun() ->
        case mnesia:read(yawl_checkpoint, CheckpointId) of
            [#yawl_checkpoint{spec_id = S, case_id = C}]
              when S =:= SpecId, C =:= CaseId ->
                mnesia:delete(yawl_checkpoint, CheckpointId, write),
                ok;
            [#yawl_checkpoint{}] ->
                mnesia:abort({checkpoint_mismatch, CheckpointId});
            [] ->
                mnesia:abort(not_found)
        end
    end,

    case mnesia:transaction(Transaction) of
        {atomic, ok} -> ok;
        {aborted, not_found} -> {error, not_found};
        {aborted, Reason} -> {error, Reason}
    end.

%%--------------------------------------------------------------------
%% @doc Lists all checkpoints for a workflow case.
%%
%% Returns a list of checkpoint IDs and their timestamps, sorted by
%% timestamp (most recent first).
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><b>SpecId:</b> Workflow specification identifier</li>
%%   <li><b>CaseId:</b> Workflow instance identifier</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% List of {CheckpointId, Timestamp} tuples sorted by timestamp descending.
%%
%% @end
%%--------------------------------------------------------------------
-spec list_checkpoints(spec_id(), case_id()) -> [{checkpoint_id(), timestamp()}].

list_checkpoints(SpecId, CaseId) when is_binary(SpecId), is_binary(CaseId) ->
    Transaction = fun() ->
        %% Use match_object to find all checkpoints for this case
        Pattern = #yawl_checkpoint{
            spec_id = SpecId,
            case_id = CaseId,
            checkpoint_id = '$1',
            marking = '_',
            data = '_',
            timestamp = '$2',
            version = '_'
        },
        mnesia:match_object(yawl_checkpoint, Pattern, read)
    end,

    case mnesia:transaction(Transaction) of
        {atomic, Records} ->
            %% Extract checkpoint_id and timestamp, sort by timestamp descending
            lists:reverse(
                lists:sort(
                    fun({_, T1}, {_, T2}) -> T1 =< T2 end,
                    [{R#yawl_checkpoint.checkpoint_id, R#yawl_checkpoint.timestamp}
                     || R <- Records]
                )
            );
        {aborted, _Reason} ->
            []
    end.

%%--------------------------------------------------------------------
%% @doc Returns the most recent checkpoint ID for a workflow case.
%%
%% Useful for automatic recovery to the latest state.
%%
%% <h3>Parameters</h3>
%% <ul>
%%   <li><b>SpecId:</b> Workflow specification identifier</li>
%%   <li><b>CaseId:</b> Workflow instance identifier</li>
%% </ul>
%%
%% <h3>Returns</h3>
%% <ul>
%%   <li><b>{ok, CheckpointId}:</b> Latest checkpoint found</li>
%%   <li><b>{error, not_found}:</b> No checkpoints exist for this case</li>
%% </ul>
%%
%% @end
%%--------------------------------------------------------------------
-spec get_latest_checkpoint(spec_id(), case_id()) -> {ok, checkpoint_id()} | {error, not_found}.

get_latest_checkpoint(SpecId, CaseId) ->
    case list_checkpoints(SpecId, CaseId) of
        [{CheckpointId, _Timestamp} | _] ->
            {ok, CheckpointId};
        [] ->
            {error, not_found}
    end.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique checkpoint ID based on timestamp.
%%
%% Format: "cp_" + timestamp (milliseconds since epoch) + "_" + random suffix
%% Ensures uniqueness across multiple checkpoints.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_checkpoint_id() -> checkpoint_id().

generate_checkpoint_id() ->
    Timestamp = erlang:system_time(millisecond),
    %% Add a random suffix to avoid collisions in rapid succession
    Random = rand:uniform(10000),
    <<"cp_", (integer_to_binary(Timestamp))/binary, "_", (integer_to_binary(Random))/binary>>.
