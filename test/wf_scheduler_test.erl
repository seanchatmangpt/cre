%%%-------------------------------------------------------------------
%%% @doc
%%% EUnit tests for wf_scheduler - workflow priority queue scheduler
%%% with rate limiting and job lifecycle tracking.
%%%
%%% Test coverage:
%%% 1. Scheduler initialization and configuration validation
%%% 2. Job enqueue operations (default and explicit priority)
%%% 3. Priority queue ordering (lower priority values first)
%%% 4. Rate limiting (bounded concurrency)
%%% 5. Job status transitions (pending -> executing -> completed/failed)
%%% 6. Queue statistics and metrics
%%% 7. Job retrieval, cancellation, and error handling
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wf_scheduler_test).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% EUnit Test Groups
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Main test generator with setup/cleanup.
%%--------------------------------------------------------------------
wf_scheduler_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_Ctx) ->
         [
          {"scheduler initialization", fun scheduler_init_test/0},
          {"invalid configuration", fun invalid_config_test/0},
          {"enqueue default priority", fun enqueue_default_priority_test/0},
          {"enqueue explicit priority", fun enqueue_explicit_priority_test/0},
          {"enqueue duplicate job", fun enqueue_duplicate_job_test/0},
          {"priority queue ordering", fun priority_queue_ordering_test/0},
          {"peek ready respects rate limit", fun peek_ready_respects_limit_test/0},
          {"dequeue respects rate limit", fun dequeue_respects_limit_test/0},
          {"start execution transitions state", fun start_execution_test/0},
          {"complete execution success", fun complete_execution_success_test/0},
          {"complete execution failure", fun complete_execution_failure_test/0},
          {"queue stats accuracy", fun queue_stats_accuracy_test/0},
          {"get job by id", fun get_job_test/0},
          {"cancel pending job", fun cancel_pending_job_test/0},
          {"cancel executing job fails", fun cancel_executing_job_fails_test/0},
          {"rate limiting multiple jobs", fun rate_limiting_multiple_jobs_test/0},
          {"dequeue with limit", fun dequeue_with_limit_test/0},
          {"peek ready empty queue", fun peek_ready_empty_queue_test/0},
          {"job data preservation", fun job_data_preservation_test/0},
          {"concurrent slot management", fun concurrent_slot_management_test/0},
          {"failed count tracking", fun failed_count_tracking_test/0},
          {"job status access", fun job_status_access_test/0},
          {"start execution non_existent job", fun start_execution_nonexistent_test/0},
          {"complete execution non_existent job", fun complete_execution_nonexistent_test/0},
          {"multiple priority levels", fun multiple_priority_levels_test/0}
         ]
     end}.

%%====================================================================
%% Setup and Cleanup
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Setup function - ensures no leftover schedulers.
%%--------------------------------------------------------------------
setup() ->
    %% Stop any existing scheduler processes
    catch wf_scheduler:stop(test_sched),
    catch wf_scheduler:stop(test_sched2),
    catch wf_scheduler:stop(test_sched_limits),
    timer:sleep(10),
    ok.

%%--------------------------------------------------------------------
%% @doc Cleanup function - stops test schedulers.
%%--------------------------------------------------------------------
cleanup(_Ctx) ->
    %% Stop all test schedulers
    catch wf_scheduler:stop(test_sched),
    catch wf_scheduler:stop(test_sched2),
    catch wf_scheduler:stop(test_sched_limits),
    catch wf_scheduler:stop(test_sched_multi),
    catch wf_scheduler:stop(test_sched_cancel),
    catch wf_scheduler:stop(test_sched_stats),
    catch wf_scheduler:stop(test_sched_data),
    catch wf_scheduler:stop(test_sched_slots),
    catch wf_scheduler:stop(test_sched_fail),
    catch wf_scheduler:stop(test_sched_status),
    timer:sleep(10),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Test scheduler initialization with valid configuration.
%%--------------------------------------------------------------------
scheduler_init_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 10
    }),
    ?assert(is_pid(Sched)),

    %% Verify scheduler is running
    Stats = wf_scheduler:queue_stats(Sched),
    ?assertMatch(#{pending := 0, executing := 0, available_slots := 10}, Stats),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test scheduler rejects invalid max_concurrent value.
%%--------------------------------------------------------------------
invalid_config_test() ->
    %% Max concurrent must be >= 1
    {error, invalid_max_concurrent} = wf_scheduler:start_link(#{
        name => test_sched2,
        max_concurrent => 0
    }),

    {error, invalid_max_concurrent} = wf_scheduler:start_link(#{
        name => test_sched2,
        max_concurrent => -5
    }).

%%--------------------------------------------------------------------
%% @doc Test enqueue with default priority (100).
%%--------------------------------------------------------------------
enqueue_default_priority_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    ok = wf_scheduler:enqueue(Sched, job1, task1, #{data => test}),

    %% Verify job is in queue
    {ok, Job} = wf_scheduler:get_job(Sched, job1),
    ?assertEqual(100, Job#job.priority),
    ?assertEqual(pending, Job#job.status),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test enqueue with explicit priority.
%%--------------------------------------------------------------------
enqueue_explicit_priority_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    ok = wf_scheduler:enqueue(Sched, job1, task1, #{data => test}, 5),
    ok = wf_scheduler:enqueue(Sched, job2, task2, #{data => test}, 10),

    {ok, Job1} = wf_scheduler:get_job(Sched, job1),
    {ok, Job2} = wf_scheduler:get_job(Sched, job2),

    ?assertEqual(5, Job1#job.priority),
    ?assertEqual(10, Job2#job.priority),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test enqueue rejects duplicate job ID.
%%--------------------------------------------------------------------
enqueue_duplicate_job_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    ok = wf_scheduler:enqueue(Sched, job1, task1, #{data => test}),
    {error, job_already_exists} = wf_scheduler:enqueue(Sched, job1, task2, #{data => test}),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test priority queue maintains sorted order (lower priority first).
%%--------------------------------------------------------------------
priority_queue_ordering_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 1
    }),

    %% Enqueue jobs with varying priorities
    ok = wf_scheduler:enqueue(Sched, job_high, task1, #{}, 50),
    ok = wf_scheduler:enqueue(Sched, job_low, task2, #{}, 200),
    ok = wf_scheduler:enqueue(Sched, job_mid, task3, #{}, 100),

    %% Peek ready should return jobs in priority order
    [Job1] = wf_scheduler:peek_ready(Sched, 1),
    ?assertEqual(job_high, Job1#job.job_id),

    [Job1_2, Job2] = wf_scheduler:peek_ready(Sched, 2),
    ?assertEqual(job_high, Job1_2#job.job_id),
    ?assertEqual(job_mid, Job2#job.job_id),

    [Job1_3, Job2_3, Job3] = wf_scheduler:peek_ready(Sched, 3),
    ?assertEqual(job_high, Job1_3#job.job_id),
    ?assertEqual(job_mid, Job2_3#job.job_id),
    ?assertEqual(job_low, Job3#job.job_id),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test peek_ready respects rate limiting.
%%--------------------------------------------------------------------
peek_ready_respects_limit_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 2
    }),

    %% Enqueue 5 jobs
    ok = wf_scheduler:enqueue(Sched, job1, task1, #{}),
    ok = wf_scheduler:enqueue(Sched, job2, task2, #{}),
    ok = wf_scheduler:enqueue(Sched, job3, task3, #{}),
    ok = wf_scheduler:enqueue(Sched, job4, task4, #{}),
    ok = wf_scheduler:enqueue(Sched, job5, task5, #{}),

    %% With 2 slots available and 5 jobs queued,
    %% peek_ready should only return 2 jobs
    Ready = wf_scheduler:peek_ready(Sched, 5),
    ?assertEqual(2, length(Ready)),

    %% After starting execution for first job, still 1 slot available
    {ok, execution_started} = wf_scheduler:start_execution(Sched, job1),
    Ready2 = wf_scheduler:peek_ready(Sched, 5),
    ?assertEqual(1, length(Ready2)),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test dequeue respects rate limiting.
%%--------------------------------------------------------------------
dequeue_respects_limit_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 2
    }),

    %% Enqueue 5 jobs
    ok = wf_scheduler:enqueue(Sched, job1, task1, #{}),
    ok = wf_scheduler:enqueue(Sched, job2, task2, #{}),
    ok = wf_scheduler:enqueue(Sched, job3, task3, #{}),
    ok = wf_scheduler:enqueue(Sched, job4, task4, #{}),
    ok = wf_scheduler:enqueue(Sched, job5, task5, #{}),

    %% Dequeue all available slots (should be 2)
    Started = wf_scheduler:dequeue(Sched),
    ?assertEqual(2, length(Started)),

    Stats = wf_scheduler:queue_stats(Sched),
    ?assertEqual(3, maps:get(pending, Stats)),
    ?assertEqual(2, maps:get(executing, Stats)),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test start_execution transitions job to executing state.
%%--------------------------------------------------------------------
start_execution_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    ok = wf_scheduler:enqueue(Sched, job1, task1, #{data => test}),

    {ok, Job0} = wf_scheduler:get_job(Sched, job1),
    ?assertEqual(pending, Job0#job.status),

    {ok, execution_started} = wf_scheduler:start_execution(Sched, job1),

    {ok, Job1} = wf_scheduler:get_job(Sched, job1),
    ?assertEqual(executing, Job1#job.status),
    ?assertNotEqual(undefined, Job1#job.started_at),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test complete_execution with success result.
%%--------------------------------------------------------------------
complete_execution_success_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    ok = wf_scheduler:enqueue(Sched, job1, task1, #{data => test}),
    {ok, execution_started} = wf_scheduler:start_execution(Sched, job1),

    {ok, execution_complete} = wf_scheduler:complete_execution(Sched, job1, success),

    {ok, Job} = wf_scheduler:get_job(Sched, job1),
    ?assertEqual(completed, Job#job.status),
    ?assertEqual(success, Job#job.result),
    ?assertNotEqual(undefined, Job#job.completed_at),

    Stats = wf_scheduler:queue_stats(Sched),
    ?assertEqual(1, maps:get(total_completed, Stats)),
    ?assertEqual(0, maps:get(total_failed, Stats)),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test complete_execution with failure result.
%%--------------------------------------------------------------------
complete_execution_failure_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    ok = wf_scheduler:enqueue(Sched, job1, task1, #{data => test}),
    {ok, execution_started} = wf_scheduler:start_execution(Sched, job1),

    {ok, execution_complete} = wf_scheduler:complete_execution(Sched, job1, failure),

    {ok, Job} = wf_scheduler:get_job(Sched, job1),
    ?assertEqual(failed, Job#job.status),
    ?assertEqual(failure, Job#job.result),

    Stats = wf_scheduler:queue_stats(Sched),
    ?assertEqual(0, maps:get(total_completed, Stats)),
    ?assertEqual(1, maps:get(total_failed, Stats)),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test queue_stats accuracy.
%%--------------------------------------------------------------------
queue_stats_accuracy_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 3
    }),

    %% Initial state
    Stats0 = wf_scheduler:queue_stats(Sched),
    ?assertEqual(0, maps:get(pending, Stats0)),
    ?assertEqual(0, maps:get(executing, Stats0)),
    ?assertEqual(0, maps:get(total_completed, Stats0)),
    ?assertEqual(0, maps:get(total_failed, Stats0)),
    ?assertEqual(3, maps:get(available_slots, Stats0)),

    %% Enqueue 5 jobs
    [ok = wf_scheduler:enqueue(Sched, list_to_atom(lists:flatten(io_lib:format("job~B", [I]))),
                               list_to_atom(lists:flatten(io_lib:format("task~B", [I]))), {})
     || I <- lists:seq(1, 5)],

    Stats1 = wf_scheduler:queue_stats(Sched),
    ?assertEqual(5, maps:get(pending, Stats1)),
    ?assertEqual(3, maps:get(available_slots, Stats1)),

    %% Start 2 jobs
    [wf_scheduler:start_execution(Sched, list_to_atom(lists:flatten(io_lib:format("job~B", [I]))))
     || I <- [1, 2]],

    Stats2 = wf_scheduler:queue_stats(Sched),
    ?assertEqual(3, maps:get(pending, Stats2)),
    ?assertEqual(2, maps:get(executing, Stats2)),
    ?assertEqual(1, maps:get(available_slots, Stats2)),

    %% Complete 1 job successfully
    wf_scheduler:complete_execution(Sched, job1, success),

    Stats3 = wf_scheduler:queue_stats(Sched),
    ?assertEqual(1, maps:get(executing, Stats3)),
    ?assertEqual(1, maps:get(total_completed, Stats3)),
    ?assertEqual(2, maps:get(available_slots, Stats3)),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test get_job retrieves job by ID.
%%--------------------------------------------------------------------
get_job_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    JobData = #{key => value, nested => #{data => 123}},
    ok = wf_scheduler:enqueue(Sched, job1, task1, JobData, 42),

    {ok, Job} = wf_scheduler:get_job(Sched, job1),
    ?assertEqual(job1, Job#job.job_id),
    ?assertEqual(task1, Job#job.task_id),
    ?assertEqual(42, Job#job.priority),
    ?assertEqual(JobData, Job#job.data),
    ?assertEqual(pending, Job#job.status),

    %% Non-existent job returns error
    {error, not_found} = wf_scheduler:get_job(Sched, nonexistent),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test cancel pending job.
%%--------------------------------------------------------------------
cancel_pending_job_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    ok = wf_scheduler:enqueue(Sched, job1, task1, #{}),
    ok = wf_scheduler:enqueue(Sched, job2, task2, #{}),

    Stats0 = wf_scheduler:queue_stats(Sched),
    ?assertEqual(2, maps:get(pending, Stats0)),

    %% Cancel pending job
    ok = wf_scheduler:cancel_job(Sched, job1),

    {error, not_found} = wf_scheduler:get_job(Sched, job1),
    {ok, _} = wf_scheduler:get_job(Sched, job2),

    Stats1 = wf_scheduler:queue_stats(Sched),
    ?assertEqual(1, maps:get(pending, Stats1)),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test cancel executing job fails.
%%--------------------------------------------------------------------
cancel_executing_job_fails_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    ok = wf_scheduler:enqueue(Sched, job1, task1, #{}),
    {ok, execution_started} = wf_scheduler:start_execution(Sched, job1),

    %% Cannot cancel executing job
    {error, {cannot_cancel, executing}} = wf_scheduler:cancel_job(Sched, job1),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test rate limiting with multiple jobs.
%%--------------------------------------------------------------------
rate_limiting_multiple_jobs_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 2
    }),

    %% Enqueue 5 jobs
    [ok = wf_scheduler:enqueue(Sched, list_to_atom(lists:flatten(io_lib:format("job~B", [I]))),
                               list_to_atom(lists:flatten(io_lib:format("task~B", [I]))), {})
     || I <- lists:seq(1, 5)],

    %% Dequeue all (should be limited to 2)
    Started = wf_scheduler:dequeue(Sched),
    ?assertEqual(2, length(Started)),

    Stats1 = wf_scheduler:queue_stats(Sched),
    ?assertEqual(2, maps:get(executing, Stats1)),
    ?assertEqual(3, maps:get(pending, Stats1)),

    %% Complete one execution to free a slot
    Job1 = hd(Started),
    {ok, execution_complete} = wf_scheduler:complete_execution(Sched, Job1, success),

    Stats2 = wf_scheduler:queue_stats(Sched),
    ?assertEqual(1, maps:get(executing, Stats2)),
    ?assertEqual(1, maps:get(available_slots, Stats2)),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test dequeue with N limit.
%%--------------------------------------------------------------------
dequeue_with_limit_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 10
    }),

    %% Enqueue 5 jobs
    [ok = wf_scheduler:enqueue(Sched, list_to_atom(lists:flatten(io_lib:format("job~B", [I]))),
                               list_to_atom(lists:flatten(io_lib:format("task~B", [I]))), {})
     || I <- lists:seq(1, 5)],

    %% Dequeue with limit of 2
    Started = wf_scheduler:dequeue(Sched, 2),
    ?assertEqual(2, length(Started)),

    Stats = wf_scheduler:queue_stats(Sched),
    ?assertEqual(2, maps:get(executing, Stats)),
    ?assertEqual(3, maps:get(pending, Stats)),

    %% Dequeue with limit of 3 (but only 3 available)
    Started2 = wf_scheduler:dequeue(Sched, 3),
    ?assertEqual(3, length(Started2)),

    Stats2 = wf_scheduler:queue_stats(Sched),
    ?assertEqual(5, maps:get(executing, Stats2)),
    ?assertEqual(0, maps:get(pending, Stats2)),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test peek_ready on empty queue.
%%--------------------------------------------------------------------
peek_ready_empty_queue_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    %% Peeking at empty queue returns empty list
    Ready = wf_scheduler:peek_ready(Sched, 5),
    ?assertEqual([], Ready),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test job data preservation.
%%--------------------------------------------------------------------
job_data_preservation_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    %% Complex job data
    JobData = #{
        id => <<"complex-id">>,
        params => [1, 2, 3],
        nested => #{level1 => #{level2 => <<"deep">>}},
        list => [a, b, c],
        bool => true
    },

    ok = wf_scheduler:enqueue(Sched, job1, task1, JobData),

    {ok, Job} = wf_scheduler:get_job(Sched, job1),
    ?assertEqual(JobData, Job#job.data),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test concurrent slot management.
%%--------------------------------------------------------------------
concurrent_slot_management_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 3
    }),

    %% Enqueue 5 jobs
    [ok = wf_scheduler:enqueue(Sched, list_to_atom(lists:flatten(io_lib:format("job~B", [I]))),
                               list_to_atom(lists:flatten(io_lib:format("task~B", [I]))), {})
     || I <- lists:seq(1, 5)],

    %% Dequeue all available slots
    Started1 = wf_scheduler:dequeue(Sched),
    ?assertEqual(3, length(Started1)),

    %% Complete one job
    Job1 = lists:nth(1, Started1),
    {ok, execution_complete} = wf_scheduler:complete_execution(Sched, Job1, success),

    %% Should now be able to dequeue one more
    Started2 = wf_scheduler:dequeue(Sched),
    ?assertEqual(1, length(Started2)),

    %% Complete another
    Job2 = lists:nth(2, Started1),
    {ok, execution_complete} = wf_scheduler:complete_execution(Sched, Job2, success),

    %% Should be able to dequeue one more
    Started3 = wf_scheduler:dequeue(Sched),
    ?assertEqual(1, length(Started3)),

    Stats = wf_scheduler:queue_stats(Sched),
    ?assertEqual(3, maps:get(executing, Stats)),
    ?assertEqual(0, maps:get(pending, Stats)),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test failed_count tracking.
%%--------------------------------------------------------------------
failed_count_tracking_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    %% Enqueue and execute jobs with different results
    ok = wf_scheduler:enqueue(Sched, job1, task1, #{}),
    ok = wf_scheduler:enqueue(Sched, job2, task2, #{}),
    ok = wf_scheduler:enqueue(Sched, job3, task3, #{}),

    {ok, execution_started} = wf_scheduler:start_execution(Sched, job1),
    {ok, execution_started} = wf_scheduler:start_execution(Sched, job2),
    {ok, execution_started} = wf_scheduler:start_execution(Sched, job3),

    %% Complete with different results
    {ok, execution_complete} = wf_scheduler:complete_execution(Sched, job1, success),
    {ok, execution_complete} = wf_scheduler:complete_execution(Sched, job2, failure),
    {ok, execution_complete} = wf_scheduler:complete_execution(Sched, job3, {error, crashed}),

    Stats = wf_scheduler:queue_stats(Sched),
    ?assertEqual(1, maps:get(total_completed, Stats)),
    ?assertEqual(2, maps:get(total_failed, Stats)),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test job status access.
%%--------------------------------------------------------------------
job_status_access_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    ok = wf_scheduler:enqueue(Sched, job1, task1, #{}),

    {ok, Job1} = wf_scheduler:get_job(Sched, job1),
    ?assertEqual(pending, Job1#job.status),

    {ok, execution_started} = wf_scheduler:start_execution(Sched, job1),
    {ok, Job2} = wf_scheduler:get_job(Sched, job1),
    ?assertEqual(executing, Job2#job.status),

    {ok, execution_complete} = wf_scheduler:complete_execution(Sched, job1, success),
    {ok, Job3} = wf_scheduler:get_job(Sched, job1),
    ?assertEqual(completed, Job3#job.status),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test start_execution on non-existent job.
%%--------------------------------------------------------------------
start_execution_nonexistent_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    {error, job_not_found} = wf_scheduler:start_execution(Sched, nonexistent),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test complete_execution on non-existent job.
%%--------------------------------------------------------------------
complete_execution_nonexistent_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 5
    }),

    {error, job_not_found} = wf_scheduler:complete_execution(Sched, nonexistent, success),

    wf_scheduler:stop(Sched).

%%--------------------------------------------------------------------
%% @doc Test multiple priority levels.
%%--------------------------------------------------------------------
multiple_priority_levels_test() ->
    {ok, Sched} = wf_scheduler:start_link(#{
        name => test_sched,
        max_concurrent => 1
    }),

    %% Enqueue jobs with 10 different priority levels
    [ok = wf_scheduler:enqueue(Sched, list_to_atom(lists:flatten(io_lib:format("job~B", [I]))),
                               list_to_atom(lists:flatten(io_lib:format("task~B", [I]))), {}, I * 10)
     || I <- lists:seq(1, 10)],

    %% Peek all jobs and verify they're in correct order
    Jobs = wf_scheduler:peek_ready(Sched, 10),

    %% All 10 jobs should be peekable (though only 1 can execute due to rate limit)
    %% Actually, with max_concurrent=1, peek_ready should only return 1 job
    ?assertEqual(1, length(Jobs)),

    %% The first job should have lowest priority value
    [FirstJob] = Jobs,
    ?assertEqual(10, FirstJob#job.priority),

    wf_scheduler:stop(Sched).
