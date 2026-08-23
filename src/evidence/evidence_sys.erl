%%%-------------------------------------------------------------------
%%% @doc
%%% System statistics evidence collection using sys:statistics.
%%% Provides process-level sampling for forensic evidence collection.
%%% @end
%%%-------------------------------------------------------------------
-module(evidence_sys).

-behaviour(gen_server).

%% API
-export([sample_pid/2, sample_all/1, stats_to_json/1, save_stats/2]).
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%% Type definitions
-type pid_info() :: #{
    pid => pid() | undefined,
    registered_name => atom() | undefined,
    initial_status => term(),
    final_status => term(),
    stats => stats_map()
}.

-type stats_map() :: #{
    duration_ms => non_neg_integer(),
    reductions => #{
        initial => non_neg_integer(),
        final => non_neg_integer(),
        delta => non_neg_integer()
    },
    message_queue_len => #{
        initial => non_neg_integer(),
        final => non_neg_integer(),
        delta => integer()
    },
    memory => #{
        initial => non_neg_integer(),
        final => non_neg_integer(),
        delta => integer(),
        heap_initial => non_neg_integer() | undefined,
        heap_final => non_neg_integer() | undefined,
        stack_initial => non_neg_integer() | undefined,
        stack_final => non_neg_integer() | undefined
    },
    garbage_collection => #{
        count_initial => non_neg_integer() | undefined,
        count_final => non_neg_integer() | undefined,
        count_delta => integer() | undefined,
        words_reclaimed_initial => non_neg_integer() | undefined,
        words_reclaimed_final => non_neg_integer() | undefined,
        words_reclaimed_delta => integer() | undefined
    },
    sampling_timestamp => #{
        start_ts => integer(),
        end_ts => integer()
    }
}.

-type sampling_result() :: {ok, pid_info()} | {error, term()}.

-export_type([pid_info/0, stats_map/0, sampling_result/0]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Start the evidence_sys server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Stop the evidence_sys server.
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc Sample sys:statistics for a specific PID.
%% Takes two measurements: before and after Duration ms to calculate deltas.
%% For non-gen_server processes, falls back to process_info sampling.
-spec sample_pid(pid() | atom(), pos_integer()) -> sampling_result().
sample_pid(TargetPid, Duration) when is_atom(TargetPid) ->
    case whereis(TargetPid) of
        undefined -> {error, {not_registered, TargetPid}};
        Pid -> sample_pid(Pid, Duration)
    end;
sample_pid(TargetPid, Duration) when is_pid(TargetPid), is_integer(Duration), Duration > 0 ->
    StartTime = erlang:system_time(millisecond),

    % Check if process is alive first
    case erlang:is_process_alive(TargetPid) of
        false ->
            {error, process_not_alive};
        true ->
            % Try to enable sys:statistics (works for gen_server, gen_statem, etc.)
            case safe_sys_statistics(TargetPid, true, 50) of
                ok ->
                    InitialStatus = get_process_status(TargetPid),
                    InitialStats = get_raw_stats(TargetPid),

                    % Wait for sampling duration
                    timer:sleep(Duration),

                    % Get final state
                    FinalStatus = get_process_status(TargetPid),
                    FinalStats = get_raw_stats(TargetPid),

                    % Disable statistics
                    sys:statistics(TargetPid, false),

                    EndTime = erlang:system_time(millisecond),

                    % Build info map
                    RegName = get_registered_name(TargetPid),
                    Info = #{
                        pid => TargetPid,
                        registered_name => RegName,
                        initial_status => InitialStatus,
                        final_status => FinalStatus,
                        stats => compute_stats(InitialStats, FinalStats, Duration, StartTime, EndTime)
                    },
                    {ok, Info};
                {error, _} ->
                    % Fallback: use process_info only for non-compliant processes
                    InitialStats = get_raw_stats(TargetPid),
                    timer:sleep(Duration),
                    FinalStats = get_raw_stats(TargetPid),
                    EndTime = erlang:system_time(millisecond),

                    RegName = get_registered_name(TargetPid),
                    Info = #{
                        pid => TargetPid,
                        registered_name => RegName,
                        initial_status => {status, TargetPid, {module, unknown}, []},
                        final_status => {status, TargetPid, {module, unknown}, []},
                        stats => compute_stats(InitialStats, FinalStats, Duration, StartTime, EndTime)
                    },
                    {ok, Info}
            end
    end.

%% @doc Sample sys:statistics for all registered processes.
%% Returns a map of registered_name => sampling_result().
%% Skips certain system processes known to not respond.
-spec sample_all(pos_integer()) -> #{atom() => sampling_result() | {error, term()}}.
sample_all(Duration) ->
    % Skip processes that don't handle system messages or are critical
    SkipProcesses = [
        code_server, erl_prim_loader, file_server_2,
        global_name_server, init, kernel_sup, net_kernel,
        rex, user, error_logger, logger, logger_handler_watcher,
        logger_proxy, logger_std_h, logger_disk_log_h,
        application_controller, erl_reply, auth, boot_server
    ],
    Registered = registered(),
    Filtered = [N || N <- Registered, not lists:member(N, SkipProcesses)],
    lists:foldl(fun(Name, Acc) ->
        case whereis(Name) of
            undefined ->
                Acc#{Name => {error, {no_process, Name}}};
            Pid ->
                % Avoid duplicate entries for processes with multiple aliases
                case lists:keymember(Pid, 2, maps:to_list(Acc)) of
                    true ->
                        Acc;
                    false ->
                        Acc#{Name => sample_pid(Pid, Duration)}
                end
        end
    end, #{}, lists:sort(Filtered)).

%% @doc Convert stats map to JSON-compatible format.
-spec stats_to_json(pid_info() | [pid_info()] | #{atom() => sampling_result()}) ->
    jsx:json_term().
stats_to_json(#{pid := _Pid, stats := Stats} = Info) ->
    #{
        <<"pid">> => format_pid(maps:get(pid, Info)),
        <<"registered_name">> => format_name(maps:get(registered_name, Info)),
        <<"sampling">> => format_sampling_stats(Stats),
        <<"status_summary">> => format_status_summary(Info)
    };
stats_to_json(Infos) when is_list(Infos) ->
    [stats_to_json(I) || I <- Infos];
stats_to_json(ResultsMap) when is_map(ResultsMap) ->
    maps:fold(fun(_Name, {ok, Info}, Acc) ->
        [stats_to_json(Info) | Acc];
       (_Name, {error, _Reason}, Acc) ->
        Acc
    end, [], ResultsMap).

%% @doc Save stats to a file in JSON format.
-spec save_stats(pid_info() | [pid_info()] | #{atom() => sampling_result()}, file:filename_all()) ->
    ok | {error, term()}.
save_stats(Stats, Filename) ->
    Json = stats_to_json(Stats),
    try
        Formatted = jsx:prettify(jsx:encode(Json)),
        file:write_file(Filename, Formatted)
    catch
        _:Error ->
            % Fallback if jsx is not available
            {error, {jsx_error, Error}}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #{}}.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Safe sys:statistics call with timeout
%% Only works for processes that handle system messages (gen_server, gen_statem, etc.)
-spec safe_sys_statistics(pid(), boolean(), pos_integer()) -> ok | {error, term()}.
safe_sys_statistics(Pid, Enable, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Worker = spawn(fun() ->
        Result = sys:statistics(Pid, Enable),
        Parent ! {Ref, Result}
    end),
    receive
        {Ref, Result} -> Result
    after Timeout ->
        case erlang:is_process_alive(Worker) of
            true -> exit(Worker, kill), {error, timeout};
            false -> {error, no_sys_support}
        end
    end.

%% @private Get process status using sys:get_status/1
-spec get_process_status(pid()) -> term().
get_process_status(Pid) ->
    try sys:get_status(Pid) of
        Status -> Status
    catch
        _:_Reason -> {error, get_status_failed}
    end.

%% @private Get raw statistics from process_info
-spec get_raw_stats(pid()) -> map().
get_raw_stats(Pid) ->
    Keys = [reductions, message_queue_len, memory, heap_size, stack_size,
            garbage_collection],
    lists:foldl(fun(Key, Acc) ->
        case process_info(Pid, Key) of
            {Key, Value} -> Acc#{Key => Value};
            undefined -> Acc
        end
    end, #{}, Keys).

%% @private Compute statistics with deltas
-spec compute_stats(map(), map(), pos_integer(), integer(), integer()) -> stats_map().
compute_stats(Initial, Final, Duration, StartTime, EndTime) ->
    InitReds = maps:get(reductions, Initial, 0),
    FinalReds = maps:get(reductions, Final, 0),

    InitMql = maps:get(message_queue_len, Initial, 0),
    FinalMql = maps:get(message_queue_len, Final, 0),

    InitMem = maps:get(memory, Initial, 0),
    FinalMem = maps:get(memory, Final, 0),

    InitHeap = maps:get(heap_size, Initial, undefined),
    FinalHeap = maps:get(heap_size, Final, undefined),

    InitStack = maps:get(stack_size, Initial, undefined),
    FinalStack = maps:get(stack_size, Final, undefined),

    {InitGCCount, InitGCReclaimed} = parse_gc(maps:get(garbage_collection, Initial, undefined)),
    {FinalGCCount, FinalGCReclaimed} = parse_gc(maps:get(garbage_collection, Final, undefined)),

    #{
        duration_ms => Duration,
        reductions => #{
            initial => InitReds,
            final => FinalReds,
            delta => FinalReds - InitReds
        },
        message_queue_len => #{
            initial => InitMql,
            final => FinalMql,
            delta => FinalMql - InitMql
        },
        memory => #{
            initial => InitMem,
            final => FinalMem,
            delta => FinalMem - InitMem,
            heap_initial => InitHeap,
            heap_final => FinalHeap,
            stack_initial => InitStack,
            stack_final => FinalStack
        },
        garbage_collection => #{
            count_initial => InitGCCount,
            count_final => FinalGCCount,
            count_delta => safe_delta(FinalGCCount, InitGCCount),
            words_reclaimed_initial => InitGCReclaimed,
            words_reclaimed_final => FinalGCReclaimed,
            words_reclaimed_delta => safe_delta(FinalGCReclaimed, InitGCReclaimed)
        },
        sampling_timestamp => #{
            start_ts => StartTime,
            end_ts => EndTime
        }
    }.

%% @private Parse garbage_collection info
-spec parse_gc(term()) -> {non_neg_integer() | undefined, non_neg_integer() | undefined}.
parse_gc(undefined) ->
    {undefined, undefined};
parse_gc({GCCount, _, WordsReclaimed, _}) when is_integer(GCCount), is_integer(WordsReclaimed) ->
    {GCCount, WordsReclaimed};
parse_gc({GCCount, WordsReclaimed}) when is_integer(GCCount), is_integer(WordsReclaimed) ->
    {GCCount, WordsReclaimed};
parse_gc(_) ->
    {undefined, undefined}.

%% @private Safe delta calculation handling undefined values
-spec safe_delta(integer() | undefined, integer() | undefined) -> integer() | undefined.
safe_delta(Final, undefined) -> Final;
safe_delta(undefined, Initial) -> -(Initial);
safe_delta(Final, Initial) -> Final - Initial.

%% @private Get registered name for a PID
-spec get_registered_name(pid()) -> atom() | undefined.
get_registered_name(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Name} -> Name;
        _ -> undefined
    end.

%% @private Format PID for JSON output
-spec format_pid(pid() | undefined) -> binary().
format_pid(undefined) -> <<"undefined">>;
format_pid(Pid) when is_pid(Pid) ->
    list_to_binary(pid_to_list(Pid)).

%% @private Format name for JSON output
-spec format_name(atom() | undefined) -> binary().
format_name(undefined) -> <<"undefined">>;
format_name(Name) when is_atom(Name) ->
    atom_to_binary(Name, utf8).

%% @private Format sampling stats for JSON
-spec format_sampling_stats(stats_map()) -> map().
format_sampling_stats(Stats) ->
    #{
        <<"duration_ms">> => maps:get(duration_ms, Stats),
        <<"reductions">> => format_reductions(maps:get(reductions, Stats)),
        <<"message_queue_len">> => format_mql(maps:get(message_queue_len, Stats)),
        <<"memory">> => format_memory(maps:get(memory, Stats)),
        <<"garbage_collection">> => format_gc(maps:get(garbage_collection, Stats)),
        <<"timestamp">> => maps:get(sampling_timestamp, Stats)
    }.

%% @private Format reductions for JSON
format_reductions(Reds) ->
    #{
        <<"total_initial">> => maps:get(initial, Reds),
        <<"total_final">> => maps:get(final, Reds),
        <<"delta">> => maps:get(delta, Reds)
    }.

%% @private Format message queue length for JSON
format_mql(Mql) ->
    #{
        <<"initial">> => maps:get(initial, Mql),
        <<"final">> => maps:get(final, Mql),
        <<"delta">> => maps:get(delta, Mql)
    }.

%% @private Format memory for JSON
format_memory(Mem) ->
    #{
        <<"total_initial">> => maps:get(initial, Mem),
        <<"total_final">> => maps:get(final, Mem),
        <<"delta">> => maps:get(delta, Mem),
        <<"heap_initial">> => format_undefined(maps:get(heap_initial, Mem)),
        <<"heap_final">> => format_undefined(maps:get(heap_final, Mem)),
        <<"stack_initial">> => format_undefined(maps:get(stack_initial, Mem)),
        <<"stack_final">> => format_undefined(maps:get(stack_final, Mem))
    }.

%% @private Format garbage collection for JSON
format_gc(GC) ->
    #{
        <<"count_initial">> => format_undefined(maps:get(count_initial, GC)),
        <<"count_final">> => format_undefined(maps:get(count_final, GC)),
        <<"count_delta">> => format_undefined(maps:get(count_delta, GC)),
        <<"words_reclaimed_initial">> => format_undefined(maps:get(words_reclaimed_initial, GC)),
        <<"words_reclaimed_final">> => format_undefined(maps:get(words_reclaimed_final, GC)),
        <<"words_reclaimed_delta">> => format_undefined(maps:get(words_reclaimed_delta, GC))
    }.

%% @private Format status summary for JSON
-spec format_status_summary(pid_info()) -> map().
format_status_summary(#{initial_status := Init, final_status := Final}) ->
    #{
        <<"initial_state">> => extract_state(Init),
        <<"final_state">> => extract_state(Final)
    }.

%% @private Extract state from sys:get_status result
-spec extract_state(term()) -> binary().
extract_state({status, _Pid, {module, _Mod}, [_PDict, _SysState, _Parent, _Db, _Heap]}) ->
    <<"running">>;
extract_state({status, _Pid, {module, _Mod}, [_PDict, _SysState, _Parent, _Db]}) ->
    <<"running">>;
extract_state({exit_status, _Reason}) ->
    <<"exited">>;
extract_state({error, _Reason}) ->
    <<"error">>;
extract_state(_) ->
    <<"unknown">>.

%% @private Convert undefined to null binary for JSON
format_undefined(undefined) -> null;
format_undefined(Value) -> Value.
