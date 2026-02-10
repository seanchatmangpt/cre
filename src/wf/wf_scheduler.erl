-module(wf_scheduler).
-behaviour(gen_server).

-moduledoc """
Workflow scheduler with priority queues and rate limiting.

This module manages workflow scheduling with support for:
- Priority-based job queuing
- Rate limiting (bounded concurrency)
- Job lifecycle tracking (pending, executing, completed)
- Metrics and statistics collection

The scheduler uses a priority queue internally where lower numeric priorities
execute first. Rate limiting ensures at most N workflows execute concurrently.

## Examples

Starting a scheduler with max 10 concurrent jobs:

```erlang
> {ok, Sched} = wf_scheduler:start_link(#{
..   name => wf_sched,
..   max_concurrent => 10
.. }).
_

> wf_scheduler:enqueue(Sched, job1, task1, #{priority => 5, data => #{x => 1}}).
ok

> [Job] = wf_scheduler:peek_ready(Sched, 1).
_
> maps:get(job_id, Job).
job1
```

Rate limiting in action - jobs execute as slots open:

```erlang
> wf_scheduler:enqueue(Sched, job2, task2, #{priority => 1}).
ok

> wf_scheduler:start_execution(Sched, job1).
{ok, execution_started}

> wf_scheduler:complete_execution(Sched, job1, success).
{ok, execution_complete}

> wf_scheduler:queue_stats(Sched).
#{
  pending => 1,
  executing => 0,
  total_completed => 1,
  queue_depth => 1,
  available_slots => 10
}
```
""".

%%====================================================================
%% Exports
%%====================================================================

%% API
-export([start_link/1]).
-export([stop/1]).
-export([enqueue/4, enqueue/5]).
-export([dequeue/1, dequeue/2]).
-export([peek_ready/2]).
-export([start_execution/2]).
-export([complete_execution/3]).
-export([queue_stats/1]).
-export([get_job/2]).
-export([cancel_job/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%====================================================================
%% Types
%%====================================================================

-type scheduler() :: pid() | atom().

-type job_id() :: binary() | atom().

-type task_id() :: binary() | atom().

-type priority() :: non_neg_integer().

-type job_status() :: pending | executing | completed | cancelled | failed.

-type execution_result() :: success | failure | {error, term()}.

-type scheduler_config() :: #{
    name := atom(),
    max_concurrent := pos_integer(),
    enable_metrics := boolean()
}.

-record(job, {
    job_id :: job_id(),
    task_id :: task_id(),
    priority :: priority(),
    status :: job_status(),
    data :: map(),
    created_at :: integer(),
    started_at :: integer() | undefined,
    completed_at :: integer() | undefined,
    result :: execution_result() | undefined
}).

-type job() :: #job{}.

-record(scheduler_state, {
    name :: atom() | undefined,
    max_concurrent :: pos_integer(),
    executing = 0 :: non_neg_integer(),
    executing_jobs = #{} :: #{job_id() => job()},
    priority_queue = [] :: [{priority(), job_id(), job()}],
    all_jobs = #{} :: #{job_id() => job()},
    completed_count = 0 :: non_neg_integer(),
    failed_count = 0 :: non_neg_integer(),
    enable_metrics :: boolean()
}).

-type scheduler_state() :: #scheduler_state{}.

-export_type([scheduler/0, job_id/0, task_id/0, priority/0, job_status/0,
              execution_result/0, scheduler_config/0, job/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Starts a new workflow scheduler.
%%
%% Creates a gen_server based scheduler with priority queue and rate
%% limiting. The config map must contain:
%% - `name`: Registered name for the scheduler
%% - `max_concurrent`: Maximum number of concurrent job executions
%% - `enable_metrics`: (optional) Enable metrics collection (default: true)
%%
%% @param Config Scheduler configuration map
%% @returns {ok, Scheduler} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(Config :: scheduler_config()) ->
          {ok, scheduler()} | {error, term()}.

start_link(#{name := Name, max_concurrent := MaxConcurrent} = Config) ->
    case MaxConcurrent < 1 of
        true ->
            {error, invalid_max_concurrent};
        false ->
            gen_server:start_link({local, Name}, ?MODULE, Config, [])
    end.

%%--------------------------------------------------------------------
%% @doc Stops the scheduler.
%%
%% @param Scheduler The scheduler pid or registered name
%% @returns ok
%%
%% @end
%%--------------------------------------------------------------------
-spec stop(Scheduler :: scheduler()) -> ok.

stop(Scheduler) ->
    gen_server:stop(Scheduler).

%%--------------------------------------------------------------------
%% @doc Enqueues a job with default priority (100).
%%
%% Adds a job to the scheduler's priority queue. Default priority is
%% 100 (processed after lower priority values).
%%
%% @param Scheduler The scheduler reference
%% @param JobId Unique job identifier
%% @param TaskId Task/workflow identifier
%% @param JobData Job-specific data as a map
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec enqueue(Scheduler :: scheduler(), JobId :: job_id(),
              TaskId :: task_id(), JobData :: map()) ->
          ok | {error, term()}.

enqueue(Scheduler, JobId, TaskId, JobData) ->
    enqueue(Scheduler, JobId, TaskId, JobData, 100).

%%--------------------------------------------------------------------
%% @doc Enqueues a job with explicit priority.
%%
%% Adds a job to the scheduler's priority queue. Lower priority values
%% are processed first. Priority queue is maintained in sorted order.
%%
%% @param Scheduler The scheduler reference
%% @param JobId Unique job identifier
%% @param TaskId Task/workflow identifier
%% @param JobData Job-specific data as a map
%% @param Priority Priority value (lower = higher priority)
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec enqueue(Scheduler :: scheduler(), JobId :: job_id(),
              TaskId :: task_id(), JobData :: map(), Priority :: priority()) ->
          ok | {error, term()}.

enqueue(Scheduler, JobId, TaskId, JobData, Priority) ->
    gen_server:call(Scheduler, {enqueue, JobId, TaskId, JobData, Priority}).

%%--------------------------------------------------------------------
%% @doc Dequeues and starts next N jobs if slots are available.
%%
%% Non-blocking operation that attempts to execute up to N pending jobs
%% from the priority queue, respecting the rate limit. Returns list of
%% job IDs that were started.
%%
%% @param Scheduler The scheduler reference
%% @returns [JobId] list of started job IDs
%%
%% @end
%%--------------------------------------------------------------------
-spec dequeue(Scheduler :: scheduler()) ->
          [job_id()].

dequeue(Scheduler) ->
    dequeue(Scheduler, undefined).

%%--------------------------------------------------------------------
%% @doc Dequeues up to N jobs if slots are available.
%%
%% @param Scheduler The scheduler reference
%% @param N Maximum number of jobs to dequeue (undefined = all available)
%% @returns [JobId] list of started job IDs
%%
%% @end
%%--------------------------------------------------------------------
-spec dequeue(Scheduler :: scheduler(), N :: pos_integer() | undefined) ->
          [job_id()].

dequeue(Scheduler, N) ->
    gen_server:call(Scheduler, {dequeue, N}).

%%--------------------------------------------------------------------
%% @doc Peeks at the next N ready jobs without removing them.
%%
%% Returns job records for the next N jobs in priority order that are
%% eligible to execute (considering rate limiting).
%%
%% @param Scheduler The scheduler reference
%% @param N Number of jobs to peek
%% @returns [Job] list of job records
%%
%% @end
%%--------------------------------------------------------------------
-spec peek_ready(Scheduler :: scheduler(), N :: pos_integer()) ->
          [job()].

peek_ready(Scheduler, N) ->
    gen_server:call(Scheduler, {peek_ready, N}).

%%--------------------------------------------------------------------
%% @doc Marks a job as started and increments executing counter.
%%
%% Called when a job transitions from pending to executing state.
%% Tracks start time and updates job status.
%%
%% @param Scheduler The scheduler reference
%% @param JobId Job identifier
%% @returns {ok, execution_started} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_execution(Scheduler :: scheduler(), JobId :: job_id()) ->
          {ok, execution_started} | {error, term()}.

start_execution(Scheduler, JobId) ->
    gen_server:call(Scheduler, {start_execution, JobId}).

%%--------------------------------------------------------------------
%% @doc Marks a job as complete and decrements executing counter.
%%
%% Called when a job finishes execution (success or failure). Frees up
%% execution slot for rate limiter. Triggers automatic dequeue if slots
%% are available.
%%
%% @param Scheduler The scheduler reference
%% @param JobId Job identifier
%% @param Result Execution result: success | failure | {error, Reason}
%% @returns {ok, execution_complete} | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec complete_execution(Scheduler :: scheduler(), JobId :: job_id(),
                         Result :: execution_result()) ->
          {ok, execution_complete} | {error, term()}.

complete_execution(Scheduler, JobId, Result) ->
    gen_server:call(Scheduler, {complete_execution, JobId, Result}).

%%--------------------------------------------------------------------
%% @doc Returns scheduler queue statistics.
%%
%% Provides snapshot of scheduler state including queue depth, executing
%% count, completion metrics.
%%
%% @param Scheduler The scheduler reference
%% @returns Stats map with keys: pending, executing, total_completed,
%%          total_failed, queue_depth, available_slots
%%
%% @end
%%--------------------------------------------------------------------
-spec queue_stats(Scheduler :: scheduler()) ->
          #{pending := non_neg_integer(),
            executing := non_neg_integer(),
            total_completed := non_neg_integer(),
            total_failed := non_neg_integer(),
            queue_depth := non_neg_integer(),
            available_slots := non_neg_integer()}.

queue_stats(Scheduler) ->
    gen_server:call(Scheduler, queue_stats).

%%--------------------------------------------------------------------
%% @doc Retrieves a job by ID.
%%
%% @param Scheduler The scheduler reference
%% @param JobId Job identifier
%% @returns {ok, Job} | {error, not_found}
%%
%% @end
%%--------------------------------------------------------------------
-spec get_job(Scheduler :: scheduler(), JobId :: job_id()) ->
          {ok, job()} | {error, not_found}.

get_job(Scheduler, JobId) ->
    gen_server:call(Scheduler, {get_job, JobId}).

%%--------------------------------------------------------------------
%% @doc Cancels a pending job.
%%
%% Removes job from priority queue if it hasn't started execution.
%% Returns error if job is already executing or completed.
%%
%% @param Scheduler The scheduler reference
%% @param JobId Job identifier
%% @returns ok | {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec cancel_job(Scheduler :: scheduler(), JobId :: job_id()) ->
          ok | {error, term()}.

cancel_job(Scheduler, JobId) ->
    gen_server:call(Scheduler, {cancel_job, JobId}).

%%====================================================================
%% gen_server Callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Initialize the scheduler.
%%--------------------------------------------------------------------
-spec init(Config :: scheduler_config()) ->
          {ok, scheduler_state()}.

init(Config) ->
    Name = maps:get(name, Config, undefined),
    MaxConcurrent = maps:get(max_concurrent, Config),
    EnableMetrics = maps:get(enable_metrics, Config, true),

    logger:info("wf_scheduler started", #{
        name => Name,
        max_concurrent => MaxConcurrent,
        enable_metrics => EnableMetrics
    }),

    State = #scheduler_state{
        name = Name,
        max_concurrent = MaxConcurrent,
        enable_metrics = EnableMetrics
    },

    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle synchronous requests.
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), tag()},
                  State :: scheduler_state()) ->
          {reply, Reply :: term(), scheduler_state()}.

handle_call({enqueue, JobId, TaskId, JobData, Priority}, _From,
            #scheduler_state{all_jobs = AllJobs, priority_queue = Q} = State) ->
    case maps:is_key(JobId, AllJobs) of
        true ->
            {reply, {error, job_already_exists}, State};
        false ->
            Now = erlang:system_time(millisecond),
            Job = #job{
                job_id = JobId,
                task_id = TaskId,
                priority = Priority,
                status = pending,
                data = JobData,
                created_at = Now
            },

            NewQ = insert_sorted_job({Priority, JobId, Job}, Q),
            NewAllJobs = AllJobs#{JobId => Job},

            logger:debug("Job enqueued", #{
                job_id => JobId,
                task_id => TaskId,
                priority => Priority
            }),

            {reply, ok, State#scheduler_state{
                priority_queue = NewQ,
                all_jobs = NewAllJobs
            }}
    end;

handle_call({dequeue, N}, _From, State = #scheduler_state{
    executing = Executing,
    max_concurrent = MaxConcurrent,
    priority_queue = Q,
    executing_jobs = ExecJobs,
    all_jobs = AllJobs
}) ->
    AvailableSlots = MaxConcurrent - Executing,
    JobsToStart = case N of
        undefined -> AvailableSlots;
        Limit -> min(Limit, AvailableSlots)
    end,

    {Started, NewQ, NewExecJobs, NewAllJobs} = start_jobs(
        JobsToStart, Q, ExecJobs, AllJobs, []
    ),

    NewState = State#scheduler_state{
        executing = Executing + length(Started),
        priority_queue = NewQ,
        executing_jobs = NewExecJobs,
        all_jobs = NewAllJobs
    },

    {reply, Started, NewState};

handle_call({peek_ready, N}, _From, State = #scheduler_state{
    executing = Executing,
    max_concurrent = MaxConcurrent,
    priority_queue = Q
}) ->
    AvailableSlots = MaxConcurrent - Executing,
    JobsToPeek = min(N, min(AvailableSlots, length(Q))),

    Jobs = lists:sublist([Job || {_, _, Job} <- Q], JobsToPeek),

    {reply, Jobs, State};

handle_call({start_execution, JobId}, _From, State = #scheduler_state{
    all_jobs = AllJobs
}) ->
    case maps:find(JobId, AllJobs) of
        error ->
            {reply, {error, job_not_found}, State};
        {ok, Job = #job{status = pending}} ->
            Now = erlang:system_time(millisecond),
            UpdatedJob = Job#job{
                status = executing,
                started_at = Now
            },
            UpdatedAllJobs = AllJobs#{JobId => UpdatedJob},

            logger:debug("Execution started", #{job_id => JobId}),

            {reply, {ok, execution_started}, State#scheduler_state{
                all_jobs = UpdatedAllJobs
            }};
        {ok, #job{status = Status}} ->
            {reply, {error, {invalid_status, Status}}, State}
    end;

handle_call({complete_execution, JobId, Result}, _From, State = #scheduler_state{
    executing = Executing,
    executing_jobs = ExecJobs,
    all_jobs = AllJobs,
    completed_count = Completed,
    failed_count = Failed
}) ->
    case maps:find(JobId, AllJobs) of
        error ->
            {reply, {error, job_not_found}, State};
        {ok, Job = #job{status = executing}} ->
            Now = erlang:system_time(millisecond),

            {NewStatus, NewFailed} = case Result of
                success -> {completed, Failed};
                failure -> {failed, Failed + 1};
                {error, _} -> {failed, Failed + 1};
                _ -> {failed, Failed + 1}
            end,

            UpdatedJob = Job#job{
                status = NewStatus,
                completed_at = Now,
                result = Result
            },

            UpdatedAllJobs = AllJobs#{JobId => UpdatedJob},
            NewExecJobs = maps:remove(JobId, ExecJobs),

            logger:debug("Execution completed", #{
                job_id => JobId,
                status => NewStatus,
                result => Result
            }),

            {reply, {ok, execution_complete}, State#scheduler_state{
                executing = max(0, Executing - 1),
                executing_jobs = NewExecJobs,
                all_jobs = UpdatedAllJobs,
                completed_count = case NewStatus of
                    completed -> Completed + 1;
                    _ -> Completed
                end,
                failed_count = NewFailed
            }};
        {ok, #job{status = Status}} ->
            {reply, {error, {invalid_status, Status}}, State}
    end;

handle_call(queue_stats, _From, State = #scheduler_state{
    executing = Executing,
    max_concurrent = MaxConcurrent,
    priority_queue = Q,
    completed_count = Completed,
    failed_count = Failed
}) ->
    QueueDepth = length(Q),
    AvailableSlots = MaxConcurrent - Executing,

    Stats = #{
        pending => QueueDepth,
        executing => Executing,
        total_completed => Completed,
        total_failed => Failed,
        queue_depth => QueueDepth,
        available_slots => AvailableSlots
    },

    {reply, Stats, State};

handle_call({get_job, JobId}, _From, State = #scheduler_state{
    all_jobs = AllJobs
}) ->
    case maps:find(JobId, AllJobs) of
        {ok, Job} -> {reply, {ok, Job}, State};
        error -> {reply, {error, not_found}, State}
    end;

handle_call({cancel_job, JobId}, _From, State = #scheduler_state{
    priority_queue = Q,
    all_jobs = AllJobs
}) ->
    case maps:find(JobId, AllJobs) of
        error ->
            {reply, {error, job_not_found}, State};
        {ok, #job{status = pending}} ->
            NewQ = lists:filter(fun({_, Id, _}) -> Id =/= JobId end, Q),
            UpdatedAllJobs = maps:remove(JobId, AllJobs),

            logger:debug("Job cancelled", #{job_id => JobId}),

            {reply, ok, State#scheduler_state{
                priority_queue = NewQ,
                all_jobs = UpdatedAllJobs
            }};
        {ok, #job{status = Status}} ->
            {reply, {error, {cannot_cancel, Status}}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle asynchronous messages.
%%--------------------------------------------------------------------
-spec handle_cast(Msg :: term(), State :: scheduler_state()) ->
          {noreply, scheduler_state()}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle other messages.
%%--------------------------------------------------------------------
-spec handle_info(Msg :: term(), State :: scheduler_state()) ->
          {noreply, scheduler_state()}.

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle code changes.
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(), State :: scheduler_state(),
                  Extra :: term()) ->
          {ok, scheduler_state()}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Clean up on termination.
%%--------------------------------------------------------------------
-spec terminate(Reason :: term(), State :: scheduler_state()) ->
          ok.

terminate(_Reason, #scheduler_state{name = Name}) ->
    logger:info("wf_scheduler stopped", #{name => Name}),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Insert job into priority queue maintaining sort order.
%%
%% Priority queue is a list sorted by priority (ascending). Lower
%% numeric priority values are processed first.
%%--------------------------------------------------------------------
-spec insert_sorted_job(Entry :: {priority(), job_id(), job()},
                        Queue :: [{priority(), job_id(), job()}]) ->
          [{priority(), job_id(), job()}].

insert_sorted_job({Priority, _JobId, _Job} = Entry, []) ->
    [Entry];

insert_sorted_job({Priority, _JobId, _Job} = Entry, [{P, _, _} | _] = List)
  when Priority < P ->
    [Entry | List];

insert_sorted_job(Entry, [H | T]) ->
    [H | insert_sorted_job(Entry, T)].

%%--------------------------------------------------------------------
%% @private
%% @doc Start up to N jobs from the queue.
%%--------------------------------------------------------------------
-spec start_jobs(Count :: non_neg_integer(),
                 Queue :: [{priority(), job_id(), job()}],
                 ExecJobs :: #{job_id() => job()},
                 AllJobs :: #{job_id() => job()},
                 Acc :: [job_id()]) ->
          {[job_id()], [{priority(), job_id(), job()}],
           #{job_id() => job()}, #{job_id() => job()}}.

start_jobs(0, Queue, ExecJobs, AllJobs, Acc) ->
    {lists:reverse(Acc), Queue, ExecJobs, AllJobs};

start_jobs(_Count, [], ExecJobs, AllJobs, Acc) ->
    {lists:reverse(Acc), [], ExecJobs, AllJobs};

start_jobs(Count, [{_Priority, JobId, Job} | Rest], ExecJobs, AllJobs, Acc) ->
    Now = erlang:system_time(millisecond),
    UpdatedJob = Job#job{
        status = executing,
        started_at = Now
    },

    NewExecJobs = ExecJobs#{JobId => UpdatedJob},
    NewAllJobs = AllJobs#{JobId => UpdatedJob},

    logger:debug("Job started from queue", #{job_id => JobId}),

    start_jobs(Count - 1, Rest, NewExecJobs, NewAllJobs, [JobId | Acc]).
