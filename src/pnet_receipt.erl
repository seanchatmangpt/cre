%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015 Jorgen Brandt <joergen@cuneiform-lang.org>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% -------------------------------------------------------------------
%% @doc Petri Net Receipts Module
%%
%% This module provides receipt creation and effects extraction for the
%% gen_pnet Petri net framework in CRE. Receipts are immutable records
%% of state transitions that provide an audit trail for all Petri net
%% executions.
%%
%% <h3>Purpose</h3>
%%
%% Receipts provide:
%% <ul>
%%   <li><b>Audit Trail:</b> Complete record of all state transitions</li>
%%   <li><b>Observability:</b> Debugging and inspection capabilities</li>
%%   <li><b>Replay Capability:</b> State reconstruction for testing</li>
%%   <li><b>Proofs of Execution:</b> Verifiable execution records</li>
%% </ul>
%%
%% <h3>Receipt Structure</h3>
%%
%% A receipt contains:
%% <ul>
%%   <li><code>before_hash</code> - Hash of the marking before execution</li>
%%   <li><code>after_hash</code> - Hash of the marking after execution</li>
%%   <li><code>move</code> - The transition firing that was executed</li>
%%   <li><code>ts</code> - Timestamp in milliseconds since epoch</li>
%% </ul>
%%
%% <h3>Effects</h3>
%%
%% Effects represent side effects produced by transition execution.
%% By default, receipts have no effects (empty list). Callback modules
%% can extend this behavior by implementing custom effects extraction.
%%
%% <h3>Usage Example</h3>
%%
%% <pre>
%% %% Create a receipt from state transition
%% BeforeHash = crypto:hash(sha256, term_to_binary(BeforeMarking)),
%% AfterHash = crypto:hash(sha256, term_to_binary(AfterMarking)),
%% Move = #{trsn => my_transition,
%%          mode => #{input => [token]},
%%          produce => #{output => [new_token]}},
%% Receipt = pnet_receipt:make(BeforeHash, AfterHash, Move),
%%
%% %% Extract effects (empty by default)
%% Effects = pnet_receipt:effects(Receipt).
%% </pre>
%%
%% @end
%% -------------------------------------------------------------------

-module(pnet_receipt).

%%====================================================================
%% Exports
%%====================================================================

%% Receipt creation
-export([make/3,
         timestamp/0,
         effects/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A move represents a complete transition firing.
%%
%% Contains the transition being fired, the mode (or cmode) under
%% which it fires, and the produce map of output tokens.
%%--------------------------------------------------------------------
-type move() :: #{trsn := atom(),                  % transition name
                  mode := pnet_types:mode() | pnet_types:cmode(),
                  produce := pnet_types:produce_map()}.

%%--------------------------------------------------------------------
%% @doc A receipt records the execution of a move.
%%
%% Contains hashes of the marking before and after execution,
%% the move that was executed, and a timestamp for ordering
%% and verification purposes.
%%--------------------------------------------------------------------
-type receipt() :: #{before_hash := binary(),
                     after_hash := binary(),
                     move := move(),
                     ts := integer()}.

%%--------------------------------------------------------------------
%% @doc An effect represents a side effect of execution.
%%
%% Effects can be extended by callback modules to represent
%% I/O operations, state changes, or other observable behaviors.
%%--------------------------------------------------------------------
-type effect() :: term().

-export_type([move/0, receipt/0, effect/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Creates a receipt from a state transition.
%%
%% Takes the hash of the marking before execution, the hash of the
%% marking after execution, and the move (transition firing) that
%% was performed. Returns a complete receipt with timestamp.
%%
%% @param BeforeHash Hash of the marking before the transition fires.
%% @param AfterHash Hash of the marking after the transition fires.
%% @param Move The move (transition firing) that was executed.
%% @return A receipt map containing all provided data plus timestamp.
%%
%% @end
%%--------------------------------------------------------------------
-spec make(BeforeHash :: binary(),
           AfterHash :: binary(),
           Move :: move()) -> receipt().

make(BeforeHash, AfterHash, Move)
  when is_binary(BeforeHash), is_binary(AfterHash), is_map(Move) ->

    %% Validate the move contains required fields
    #{
      trsn := Trsn,
      mode := Mode,
      produce := ProduceMap
    } = Move,

    %% Validate types within the move
    true = is_atom(Trsn),
    true = is_map(Mode),
    true = is_map(ProduceMap),

    %% Create the receipt with current timestamp
    #{
      before_hash => BeforeHash,
      after_hash => AfterHash,
      move => Move,
      ts => timestamp()
    }.

%%--------------------------------------------------------------------
%% @doc Gets the current timestamp in milliseconds.
%%
%% Uses erlang:system_time(millisecond) for consistency across the
%% codebase and to provide monotonically increasing timestamps suitable
%% for ordering receipts in a sequence.
%%
%% @return Current time in milliseconds since Unix epoch.
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> integer().

timestamp() ->
    erlang:system_time(millisecond).

%%--------------------------------------------------------------------
%% @doc Extracts effects from a receipt.
%%
%% By default, receipts have no associated effects and this function
%% returns an empty list. This design allows callback modules to
%% extend the receipt system with custom effects extraction logic.
%%
%% Effects represent side effects produced during transition execution
%% such as I/O operations, external system calls, or state mutations
%% outside the Petri net marking.
%%
%% @param Receipt The receipt to extract effects from.
%% @return List of effects (empty by default).
%%
%% @end
%%--------------------------------------------------------------------
-spec effects(Receipt :: receipt()) -> [effect()].

effects(#{}) ->
    %% Default implementation: no effects
    %% Callback modules can override by pattern matching on specific
    %% receipt structures and extracting their own effect metadata
    [].

