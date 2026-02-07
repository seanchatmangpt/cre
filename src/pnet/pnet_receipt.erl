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

-module(pnet_receipt).

-moduledoc """
Receipts are immutable audit records for state transitions.

Time choice: timestamp/0 returns monotonic milliseconds (integer).

```erlang
> T1 = pnet_receipt:timestamp(), T2 = pnet_receipt:timestamp(), T2 >= T1.
true

> Move = #{trsn => t1, mode => #{}, produce => #{p => [a]}}.
_
> R = pnet_receipt:make(<<1>>, <<2>>, Move).
_
> maps:get(before_hash, R).
<<1>>
> maps:get(after_hash, R).
<<2>>
> maps:get(move, R) =:= Move.
true
> is_integer(maps:get(ts, R)).
true
```
""".

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

-export_type([move/0, receipt/0]).

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
%% Uses erlang:monotonic_time(millisecond) for monotonic timestamps
%% suitable for ordering receipts in a sequence.
%%
%% @return Current monotonic time in milliseconds.
%%
%% @end
%%--------------------------------------------------------------------
-spec timestamp() -> integer().

timestamp() ->
    erlang:monotonic_time(millisecond).

%%--------------------------------------------------------------------
%% @doc Classifies a receipt by its production effects.
%%
%% Analyzes the produce map in the receipt's move to classify the
%% transition firing as:
%% - `{silent, Receipt}`: No tokens produced (empty produce map)
%% - `{single_production, Receipt}`: Tokens produced to exactly one place
%% - `{multiple_production, Receipt}`: Tokens produced to multiple places
%%
%% @param Receipt The receipt to classify.
%% @return A tuple `{Classification, Receipt}` where Classification is
%%         one of: silent, single_production, or multiple_production.
%%
%% @end
%%--------------------------------------------------------------------
-spec effects(Receipt :: receipt()) ->
    {silent, receipt()} |
    {single_production, receipt()} |
    {multiple_production, receipt()}.

effects(Receipt = #{move := #{produce := ProduceMap}}) ->
    ProduceCount = maps:size(ProduceMap),
    case ProduceCount of
        0 -> {silent, Receipt};
        1 -> {single_production, Receipt};
        _ -> {multiple_production, Receipt}
    end.

%%====================================================================
%% Internal Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

doctest_test() ->
    doctest:module(?MODULE, #{moduledoc => true, doc => true}).
-endif.

