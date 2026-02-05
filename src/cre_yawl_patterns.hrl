%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%% Record definitions for YAWL workflow patterns (extended)
%%
%% This file contains record definitions extracted from cre_yawl_patterns.erl
%% for use by other modules that need to work with YAWL patterns.
%%

%%====================================================================
%% Pattern State Records
%%====================================================================

-record(pattern_state, {
          pattern_type :: atom(),
          subprocess :: module() | function() | undefined,
          instance_count :: non_neg_integer() | undefined,
          max_instances :: non_neg_integer() | unlimited | undefined,
          pending_instances = [] :: list(),
          active_instances = [] :: list(),
          completed_instances = [] :: list(),
          choice_data = #{} :: map(),
          branch_queue = [] :: list()
         }).

-record(instance_token, {
          instance_id :: reference(),
          data :: term()
         }).

-record(branch_token, {
          branch_id :: atom(),
          data :: term(),
          index :: non_neg_integer()
         }).

-record(sync_token, {
          activity_id :: reference(),
          data :: term(),
          completed :: boolean()
         }).

-record(loop_state, {
          iteration :: non_neg_integer(),
          condition_result :: boolean(),
          body_data :: term()
         }).

-record(recursion_token, {
          level :: non_neg_integer(),
          data :: term(),
          is_base_case :: boolean()
         }).

-record(protocol_state, {
          request :: term(),
          response :: term(),
          timeout_ref :: reference() | undefined,
          start_time :: integer()
         }).

-record(catch_state, {
          exception_type :: atom(),
          exception_reason :: term(),
          handled :: boolean()
         }).
