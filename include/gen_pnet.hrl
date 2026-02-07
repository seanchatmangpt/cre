%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Copyright 2015-2025 CRE Team
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
%% @doc gen_pnet record definitions
%%
%% This header file defines the records used by gen_pnet:
%% - #bad_place{} - Error record for non-existent places
%% - #net_state{} - Internal state of a gen_pnet instance
%% - #stat{} - Single statistics measurement
%% - #stats{} - Aggregated statistics (current, hi, lo)
%% @end
%% -------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Error record for invalid place references.
%%
%% Returned by ls/2 when querying a non-existent place.
%%--------------------------------------------------------------------
-record(bad_place, {name}).

%%--------------------------------------------------------------------
%% @doc Internal state record for gen_pnet instances.
%%
%% This record contains the complete state of a Petri net process:
%% - `marking' - Map of places to their token lists
%% - `net_mod' - The callback module implementing the net behavior
%% - `usr_info' - User-defined state from init/1
%% - `stats' - Throughput statistics (#stats{} or undefined)
%% - `tstart' - Timestamp for statistics calculation start
%% - `cnt' - Counter for statistics (fires between measurements)
%%--------------------------------------------------------------------
-record(net_state, {
          marking,
          net_mod,
          usr_info,
          stats,
          tstart,
          cnt
         }).

%%--------------------------------------------------------------------
%% @doc Single statistics measurement.
%%
%% Records one measurement of throughput:
%% - `t' - Timestamp of the measurement
%% - `fps' - Firings per second at this timestamp
%%--------------------------------------------------------------------
-record(stat, {t, fps}).

%%--------------------------------------------------------------------
%% @doc Aggregated statistics for a net instance.
%%
%% Tracks throughput over time:
%% - `current' - Most recent #stat{} measurement
%% - `hi' - #stat{} with highest fps observed
%% - `lo' - #stat{} with lowest fps observed
%%--------------------------------------------------------------------
-record(stats, {current, hi, lo}).
