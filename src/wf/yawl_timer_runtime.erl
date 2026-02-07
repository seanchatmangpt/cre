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
%% @doc Timer runtime integration for YAWL workflows.
%%
%% This module provides the integration layer between YAWL workflow specifications
%% and the timer system, connecting `wf_timer` (duration parsing) with
%% `wf_timerq` (deadline queue management) for managing timeout tasks in YAWL workflows.
%%
%% == Timer Lifecycle ==
%%
%% YAWL timers follow a specific lifecycle:
%% 1. **Configuration**: Timer is defined in YAWL spec with ISO 8601 duration
%% 2. **Arming**: Timer is armed when a task begins execution
%% 3. **Polling**: Timer queue is polled for expired timers
%% 4. **Injection**: Timeout tokens are injected into appropriate places
%% 5. **Cancellation**: Timer may be cancelled if task completes early
%%
%% == Basic Usage ==
%%
%% ```erlang
%% % Arm a timer from spec
%% Spec = #{timer => #{duration => <<"PT30S">>,
%%                    timeout_place => timeout_place,
%%                    task_id => task1}},
%% {ok, TimerQ} = yawl_timer_runtime:arm_timer_from_spec(task1, Spec, net1, wf_timerq:new()).
%%
%% % Poll for expired timers
%% Now = erlang:monotonic_time(millisecond),
%% {Expired, UpdatedQ} = yawl_timer_runtime:poll_expired_timers(TimerQ, net1).
%%
%% % Get timer deadline
%% {ok, Deadline} = yawl_timer_runtime:get_timer_deadline(TimerQ, task1).
%%
%% % Cancel timer
%% UpdatedQ = yawl_timer_runtime:cancel_timer(TimerQ, task1).
%% ```
%%
%% == Supported Duration Formats ==
%%
%% Timers support ISO 8601 duration strings:
%% - `PTnS` - seconds (e.g., PT30S = 30 seconds)
%% - `PTnM` - minutes (e.g., PT5M = 5 minutes)
%% - `PTnH` - hours (e.g., PT2H = 2 hours)
%% - `PnD` - days (e.g., P3D = 3 days)
%% - Combined: PnDTnHnMnS (e.g., P1DT2H30M45S)
%%
%% == Integration with gen_pnet ==
%%
%% This module is designed to work with gen_pnet workflow instances:
%% - Timers are typically stored in the workflow's state
%% - Polling happens during workflow execution loops
%% - Timeout tokens trigger cancellation or alternate paths
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_timer_runtime).

%%====================================================================
%% Exports
%%====================================================================

%% Timer lifecycle API
-export([arm_timer_from_spec/4]).
-export([poll_expired_timers/2]).
-export([get_timer_deadline/2]).
-export([cancel_timer/2]).
-export([create_timeout_token/1]).

%% Utility functions
-export([extract_timer_config/1]).
-export([validate_duration/1]).

%%====================================================================
%% Types
%%====================================================================

%%--------------------------------------------------------------------
%% @doc A timer queue from wf_timerq.
%%
%% This is the opaque timer queue type from the wf_timerq module.
%%--------------------------------------------------------------------
-type timerq() :: wf_timerq:timerq().

%%--------------------------------------------------------------------
%% @doc A unique identifier for a timer.
%%
%% Typically the task ID or transition ID that the timer is associated with.
%%--------------------------------------------------------------------
-type timer_key() :: wf_timerq:timer_key().

%%--------------------------------------------------------------------
%% @doc An ISO 8601 duration string.
%%
%% Binary format like <<"PT30S">>, <<"P1DT2H">>, etc.
%%--------------------------------------------------------------------
-type duration() :: wf_timer:duration().

%%--------------------------------------------------------------------
%% @doc A timeout token created when a timer expires.
%%
%% Contains the task ID and expiration timestamp.
%%--------------------------------------------------------------------
-type timeout_token() :: {timeout, timer_key(), integer()}.

%%--------------------------------------------------------------------
%% @doc Timer configuration extracted from a YAWL spec.
%%
%% Contains the duration, place to inject timeout token, and task ID.
%%--------------------------------------------------------------------
-type timer_config() :: #{
    duration => duration(),
    timeout_place => atom(),
    task_id => timer_key()
}.

%%--------------------------------------------------------------------
%% @doc YAWL task or transition specification.
%%
%% A map containing timer configuration and other task metadata.
%%--------------------------------------------------------------------
-type spec() :: map().

%%--------------------------------------------------------------------
%% @doc Network identifier for the workflow instance.
%%
%% Identifies which workflow net the timer belongs to.
%%--------------------------------------------------------------------
-type net_id() :: term().

%%--------------------------------------------------------------------
%% @doc Timer state for storing in workflow state.
%%
%% The timer queue and any additional timer-related metadata.
%%--------------------------------------------------------------------
-type timer_state() :: #{timerq := timerq()}.

%%--------------------------------------------------------------------
%% @doc Error reasons for timer operations.
%%--------------------------------------------------------------------
-type timer_error() :: {invalid_duration, term()} |
                       {missing_config, term()} |
                       {parse_error, term()}.

%%--------------------------------------------------------------------
%% @doc Result of timer operations.
%%--------------------------------------------------------------------
-type timer_result() :: {ok, timerq()} | {error, timer_error()}.

%% Export types
-export_type([timerq/0, timer_key/0, duration/0, timeout_token/0,
              timer_config/0, spec/0, net_id/0, timer_state/0,
              timer_error/0, timer_result/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Arms a timer from a YAWL task specification.
%%
%% Extracts timer configuration from the spec, parses the duration,
%% and arms a timer in the timer queue. The timer will fire after
%% the specified duration, injecting a timeout token into the
%% configured place.
%%
%% Returns {ok, UpdatedTimerQ} on success, or {error, Reason} on failure.
%%
%% ```erlang
%% Spec = #{
%%   timer => #{
%%     duration => <<"PT30S">>,
%%     timeout_place => timeout_place,
%%     task_id => task1
%%   }
%% },
%% {ok, TimerQ} = yawl_timer_runtime:arm_timer_from_spec(task1, Spec, net1, wf_timerq:new()).
%% '''
%%
%% @param TaskId Unique identifier for the task (used as timer key)
%% @param Spec YAWL task specification containing timer configuration
%% @param NetId Network identifier for the workflow
%% @param TimerState Current timer state containing timer queue
%% @return {ok, UpdatedTimerQ} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec arm_timer_from_spec(TaskId :: timer_key(),
                          Spec :: spec(),
                          NetId :: net_id(),
                          TimerState :: timer_state()) ->
    timer_result().

arm_timer_from_spec(TaskId, Spec, _NetId, TimerState) ->
    case extract_timer_config(Spec) of
        {ok, Config} ->
            TimerQ = maps:get(timerq, TimerState, wf_timerq:new()),
            Duration = maps:get(duration, Config),
            TimeoutPlace = maps:get(timeout_place, Config),
            Event = {inject, TimeoutPlace, create_timeout_token(TaskId)},
            case wf_timerq:arm_duration(TimerQ, TaskId, Duration, Event) of
                {ok, UpdatedQ} ->
                    {ok, TimerState#{timerq => UpdatedQ}};
                {error, Reason} ->
                    {error, {parse_error, Reason}}
            end;
        {error, Reason} ->
            {error, {missing_config, Reason}}
    end.

%%--------------------------------------------------------------------
%% @doc Polls the timer queue for expired timers.
%%
%% Returns all events whose deadline has passed, along with the
%% updated timer queue. Expired events are returned in deadline order
%% (oldest first).
%%
%% This function uses the current monotonic time to determine which
%% timers have expired.
%%
%% ```erlang
%% {Expired, UpdatedQ} = yawl_timer_runtime:poll_expired_timers(TimerQ, net1).
%% case Expired of
%%     [{inject, Place, Token} | Rest] ->
%%         % Inject timeout token into workflow
%%         gen_pnet:inject(NetId, #{Place => [Token]});
%%     [] ->
%%         % No expired timers
%%         ok
%% end.
%% '''
%%
%% @param TimerQ The current timer queue
%% @param NetId Network identifier (for logging/debugging)
%% @return {ExpiredEvents, UpdatedTimerQ}
%%
%% @end
%%--------------------------------------------------------------------
-spec poll_expired_timers(TimerQ :: timerq(), NetId :: net_id()) ->
          {[term()], timerq()}.

poll_expired_timers(TimerQ, _NetId) ->
    Now = erlang:monotonic_time(millisecond),
    wf_timerq:poll(TimerQ, Now).

%%--------------------------------------------------------------------
%% @doc Gets the deadline for a specific timer.
%%
%% Returns {ok, Deadline} if the timer exists, or undefined if not found.
%% The deadline is the monotonic millisecond timestamp when the timer
%% will fire.
%%
%% ```erlang
%% case yawl_timer_runtime:get_timer_deadline(TimerQ, task1) of
%%     {ok, Deadline} ->
%%         Now = erlang:monotonic_time(millisecond),
%%         RemainingMs = Deadline - Now,
%%         {ok, RemainingMs};
%%     undefined ->
%%         {error, not_found}
%% end.
%% '''
%%
%% @param TimerQ The timer queue
%% @param TaskId The timer key to look up
%% @return {ok, Deadline} | undefined
%%
%% @end
%%--------------------------------------------------------------------
-spec get_timer_deadline(TimerQ :: timerq(), TaskId :: timer_key()) ->
          {ok, integer()} | undefined.

get_timer_deadline(TimerQ, TaskId) ->
    wf_timerq:get_deadline(TimerQ, TaskId).

%%--------------------------------------------------------------------
%% @doc Cancels (disarms) a timer by its task ID.
%%
%% Removes the timer from the queue, preventing it from firing.
%% If no timer with the given ID exists, the queue is returned unchanged.
%%
%% This is useful when a task completes before its timeout expires.
%%
%% ```erlang
%% UpdatedQ = yawl_timer_runtime:cancel_timer(TimerQ, task1).
%% '''
%%
%% @param TimerQ The current timer queue
%% @param TaskId The timer key to cancel
%% @return Updated timer queue
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_timer(TimerQ :: timerq(), TaskId :: timer_key()) -> timerq().

cancel_timer(TimerQ, TaskId) ->
    wf_timerq:disarm(TimerQ, TaskId).

%%--------------------------------------------------------------------
%% @doc Creates a timeout token for a task.
%%
%% Returns a timeout token tuple that will be injected into the
%% workflow when the timer expires. The token contains the task ID
%% and the current timestamp.
%%
%% ```erlang
%% Token = yawl_timer_runtime:create_timeout_token(task1).
%% % {timeout, task1, 1736208000000}
%% '''
%%
%% @param TaskId The task ID for the timeout token
%% @return Timeout token tuple
%%
%% @end
%%--------------------------------------------------------------------
-spec create_timeout_token(TaskId :: timer_key()) -> timeout_token().

create_timeout_token(TaskId) ->
    Timestamp = erlang:system_time(millisecond),
    {timeout, TaskId, Timestamp}.

%%====================================================================
%% Utility Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Extracts timer configuration from a YAWL spec.
%%
%% Parses the spec map to extract timer-related configuration.
%% Returns {ok, Config} if timer config is present, or {error, Reason}
%% if missing or invalid.
%%
%% The spec should contain a 'timer' key with a map containing:
%% - duration: ISO 8601 duration string (required)
%% - timeout_place: Place atom for timeout token injection (required)
%% - task_id: Timer key (optional, defaults to spec task_id)
%%
%% ```erlang
%% Spec = #{timer => #{duration => <<"PT30S">>,
%%                     timeout_place => timeout_place,
%%                     task_id => task1}},
%% {ok, Config} = yawl_timer_runtime:extract_timer_config(Spec).
%% '''
%%
%% @param Spec YAWL task specification
%% @return {ok, TimerConfig} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec extract_timer_config(Spec :: spec()) ->
          {ok, timer_config()} | {error, term()}.

extract_timer_config(Spec) when is_map(Spec) ->
    case maps:get(timer, Spec, undefined) of
        undefined ->
            {error, no_timer_config};
        TimerConfig when is_map(TimerConfig) ->
            case validate_timer_config(TimerConfig) of
                ok ->
                    {ok, TimerConfig};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, invalid_timer_config}
    end;
extract_timer_config(_) ->
    {error, invalid_spec}.

%%--------------------------------------------------------------------
%% @doc Validates an ISO 8601 duration string.
%%
%% Returns {ok, DurationMs} if valid, or {error, Reason} if invalid.
%% Uses wf_timer:parse_duration/1 for parsing.
%%
%% ```erlang
%% {ok, _} = yawl_timer_runtime:validate_duration(<<"PT30S">>),
%% {error, _} = yawl_timer_runtime:validate_duration(<<"invalid">>).
%% '''
%%
%% @param Duration ISO 8601 duration string to validate
%% @return {ok, Milliseconds} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_duration(Duration :: duration()) ->
          {ok, non_neg_integer()} | {error, term()}.

validate_duration(Duration) ->
    wf_timer:parse_duration(Duration).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private
%% @doc Validates timer configuration completeness.
%%
%% Ensures all required fields are present and valid.
%% @end
%%--------------------------------------------------------------------
-spec validate_timer_config(map()) -> ok | {error, term()}.

validate_timer_config(Config) ->
    RequiredKeys = [duration, timeout_place],
    lists:foldl(fun
        (Key, ok) ->
            case maps:get(Key, Config, undefined) of
                undefined -> {error, {missing_key, Key}};
                _ -> ok
            end;
        (_, {error, _} = Error) ->
            Error
    end, ok, RequiredKeys).

%%====================================================================
%% EUnit Tests
%%====================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% arm_timer_from_spec Tests
%%====================================================================

arm_timer_from_spec_valid_test() ->
    Spec = #{
        timer => #{
            duration => <<"PT30S">>,
            timeout_place => timeout_place,
            task_id => task1
        }
    },
    TimerState = #{timerq => wf_timerq:new()},
    Result = arm_timer_from_spec(task1, Spec, net1, TimerState),
    ?assertMatch({ok, _}, Result),
    {ok, UpdatedState} = Result,
    ?assertMatch(#{timerq := _}, UpdatedState).

arm_timer_from_spec_missing_config_test() ->
    Spec = #{},  % No timer config
    TimerState = #{timerq => wf_timerq:new()},
    Result = arm_timer_from_spec(task1, Spec, net1, TimerState),
    ?assertMatch({error, {missing_config, _}}, Result).

arm_timer_from_spec_invalid_duration_test() ->
    Spec = #{
        timer => #{
            duration => <<"invalid">>,
            timeout_place => timeout_place
        }
    },
    TimerState = #{timerq => wf_timerq:new()},
    Result = arm_timer_from_spec(task1, Spec, net1, TimerState),
    ?assertMatch({error, {parse_error, _}}, Result).

%%====================================================================
%% poll_expired_timers Tests
%%====================================================================

poll_expired_timers_none_test() ->
    Q0 = wf_timerq:new(),
    {Expired, Q1} = poll_expired_timers(Q0, net1),
    ?assertEqual([], Expired),
    ?assertEqual(true, wf_timerq:is_empty(Q1)).

poll_expired_timers_with_ready_test() ->
    Q0 = wf_timerq:new(),
    % Arm timer with 0ms duration (already expired)
    {ok, Q1} = wf_timerq:arm_duration(Q0, k1, <<"PT0S">>, {inject, p1, token}),
    {Expired, Q2} = poll_expired_timers(Q1, net1),
    ?assertEqual([{inject, p1, token}], Expired),
    ?assertEqual(true, wf_timerq:is_empty(Q2)).

poll_expired_timers_future_test() ->
    Q0 = wf_timerq:new(),
    {ok, Q1} = wf_timerq:arm_duration(Q0, k1, <<"PT1H">>, {inject, p1, token}),
    {Expired, Q2} = poll_expired_timers(Q1, net1),
    ?assertEqual([], Expired),
    ?assertEqual(1, wf_timerq:size(Q2)).

%%====================================================================
%% get_timer_deadline Tests
%%====================================================================

get_timer_deadline_exists_test() ->
    Q0 = wf_timerq:new(),
    Q1 = wf_timerq:arm(Q0, k1, 1500, {inject, p1, token}),
    Result = get_timer_deadline(Q1, k1),
    ?assertMatch({ok, 1500}, Result).

get_timer_deadline_not_exists_test() ->
    Q0 = wf_timerq:new(),
    Q1 = wf_timerq:arm(Q0, k1, 1500, {inject, p1, token}),
    Result = get_timer_deadline(Q1, k999),
    ?assertEqual(undefined, Result).

%%====================================================================
%% cancel_timer Tests
%%====================================================================

cancel_timer_exists_test() ->
    Q0 = wf_timerq:new(),
    Q1 = wf_timerq:arm(Q0, k1, 1500, {inject, p1, token}),
    ?assertEqual(1, wf_timerq:size(Q1)),
    Q2 = cancel_timer(Q1, k1),
    ?assertEqual(0, wf_timerq:size(Q2)),
    ?assertEqual(undefined, get_timer_deadline(Q2, k1)).

cancel_timer_not_exists_test() ->
    Q0 = wf_timerq:new(),
    Q1 = wf_timerq:arm(Q0, k1, 1500, {inject, p1, token}),
    ?assertEqual(1, wf_timerq:size(Q1)),
    Q2 = cancel_timer(Q1, k999),  % Non-existent key
    ?assertEqual(1, wf_timerq:size(Q2)).

%%====================================================================
%% create_timeout_token Tests
%%====================================================================

create_timeout_token_format_test() ->
    Token = create_timeout_token(task1),
    ?assertMatch({timeout, task1, _}, Token),
    {timeout, task1, Timestamp} = Token,
    ?assert(is_integer(Timestamp)),
    ?assert(Timestamp > 0).

create_timeout_token_unique_test() ->
    Token1 = create_timeout_token(task1),
    timer:sleep(2),  % Small delay
    Token2 = create_timeout_token(task1),
    ?assertNotEqual(Token1, Token2).

%%====================================================================
%% extract_timer_config Tests
%%====================================================================

extract_timer_config_valid_test() ->
    Spec = #{
        timer => #{
            duration => <<"PT30S">>,
            timeout_place => timeout_place
        }
    },
    Result = extract_timer_config(Spec),
    ?assertMatch({ok, #{}}, Result),
    {ok, Config} = Result,
    ?assertEqual(<<"PT30S">>, maps:get(duration, Config)),
    ?assertEqual(timeout_place, maps:get(timeout_place, Config)).

extract_timer_config_missing_test() ->
    Spec = #{},  % No timer key
    Result = extract_timer_config(Spec),
    ?assertMatch({error, no_timer_config}, Result).

extract_timer_config_invalid_test() ->
    Spec = #{timer => not_a_map},
    Result = extract_timer_config(Spec),
    ?assertMatch({error, invalid_timer_config}, Result).

extract_timer_config_missing_duration_test() ->
    Spec = #{
        timer => #{
            timeout_place => timeout_place
            % Missing duration
        }
    },
    Result = extract_timer_config(Spec),
    ?assertMatch({error, {missing_key, duration}}, Result).

extract_timer_config_missing_place_test() ->
    Spec = #{
        timer => #{
            duration => <<"PT30S">>
            % Missing timeout_place
        }
    },
    Result = extract_timer_config(Spec),
    ?assertMatch({error, {missing_key, timeout_place}}, Result).

%%====================================================================
%% validate_duration Tests
%%====================================================================

validate_duration_valid_test() ->
    ?assertEqual({ok, 30000}, validate_duration(<<"PT30S">>)),
    ?assertEqual({ok, 60000}, validate_duration(<<"PT1M">>)),
    ?assertEqual({ok, 3600000}, validate_duration(<<"PT1H">>)),
    ?assertEqual({ok, 86400000}, validate_duration(<<"P1D">>)).

validate_duration_combined_test() ->
    ?assertEqual({ok, 95445000}, validate_duration(<<"P1DT2H30M45S">>)),
    ?assertEqual({ok, 19800000}, validate_duration(<<"PT5H30M">>)).

validate_duration_invalid_test() ->
    ?assertEqual({error, invalid_format}, validate_duration(<<"invalid">>)),
    ?assertEqual({error, invalid_format}, validate_duration(<<>>)),
    ?assertEqual({error, invalid_format}, validate_duration(<<"30S">>)).

%%====================================================================
%% Integration Tests
%%====================================================================

integration_full_lifecycle_test() ->
    % Create spec
    Spec = #{
        timer => #{
            duration => <<"PT0S">>,  % Immediate expiration
            timeout_place => timeout_place
        }
    },
    TimerState = #{timerq => wf_timerq:new()},

    % Arm timer
    {ok, State1} = arm_timer_from_spec(task1, Spec, net1, TimerState),
    Q1 = maps:get(timerq, State1),
    ?assertEqual(1, wf_timerq:size(Q1)),

    % Poll expired timers
    {Expired, Q2} = poll_expired_timers(Q1, net1),
    ?assertEqual(1, length(Expired)),
    [{inject, timeout_place, Token}] = Expired,
    ?assertMatch({timeout, task1, _}, Token),

    % Timer should be removed
    ?assertEqual(0, wf_timerq:size(Q2)),

    % Cancel should be no-op
    Q3 = cancel_timer(Q2, task1),
    ?assertEqual(0, wf_timerq:size(Q3)).

integration_multiple_timers_test() ->
    Spec1 = #{timer => #{duration => <<"PT0S">>, timeout_place => p1}},
    Spec2 = #{timer => #{duration => <<"PT1H">>, timeout_place => p2}},

    TimerState = #{timerq => wf_timerq:new()},

    % Arm two timers
    {ok, State1} = arm_timer_from_spec(task1, Spec1, net1, TimerState),
    {ok, State2} = arm_timer_from_spec(task2, Spec2, net1, State1),

    Q2 = maps:get(timerq, State2),
    ?assertEqual(2, wf_timerq:size(Q2)),

    % Poll - only first should be expired
    {Expired, Q3} = poll_expired_timers(Q2, net1),
    ?assertEqual(1, length(Expired)),
    ?assertEqual(1, wf_timerq:size(Q3)),

    % Cancel remaining timer
    Q4 = cancel_timer(Q3, task2),
    ?assertEqual(0, wf_timerq:size(Q4)).

integration_timeout_token_format_test() ->
    Spec = #{
        timer => #{
            duration => <<"PT0S">>,
            timeout_place => p1
        }
    },
    {ok, State} = arm_timer_from_spec(task1, Spec, net1, #{timerq => wf_timerq:new()}),
    Q = maps:get(timerq, State),
    {Expired, _} = poll_expired_timers(Q, net1),
    [{inject, p1, Token}] = Expired,
    ?assertMatch({timeout, task1, _}, Token),
    {timeout, task1, Timestamp} = Token,
    ?assert(is_integer(Timestamp)),
    ?assert(Timestamp > 0).

-endif.
