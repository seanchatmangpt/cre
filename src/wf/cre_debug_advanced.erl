%% -*- erlang -*-
%% @doc Advanced debugging utilities for CRE workflows.
%%
%% Provides comprehensive debugging tools including:
%% <ul>
%%   <li>Process inspection and monitoring</li>
%%   <li>Memory analysis and leak detection</li>
%%   <li>Message queue analysis</li>
%%   <li>ETS table inspection</li>
%%   <li>System-wide statistics</li>
%%   <li>Process tree visualization</li>
%%   <li>Deadlock detection</li>
%%   <li>Performance bottleneck identification</li>
%% </ul>
%%
%% @end
-module(cre_debug_advanced).
-export([
    %% Process inspection
    inspect_process/1,
    inspect_process_tree/1,
    find_processes/1,
    find_registered/1,
    
    %% Memory analysis
    memory_summary/0,
    memory_by_module/0,
    memory_leak_check/0,
    top_memory_processes/1,
    
    %% Message queue analysis
    mailbox_summary/0,
    top_mailboxes/1,
    analyze_mailbox/1,
    
    %% ETS analysis
    ets_summary/0,
    ets_info/1,
    find_ets_by_pattern/1,
    
    %% System statistics
    system_stats/0,
    scheduler_stats/0,
    gc_stats/0,
    
    %% Deadlock detection
    detect_deadlocks/0,
    detect_message_loops/0,
    
    %% Performance analysis
    find_bottlenecks/0,
    process_reductions/1,
    top_reductions/1,
    
    %% Workflow-specific debugging
    debug_workflow/1,
    debug_workflow_state/1,
    trace_workflow/2,
    
    %% Crash dump analysis
    analyze_crash_dump/1,
    extract_process_info/2,
    
    %% Observer integration
    start_observer/0,
    observer_connect/1
]).

-include_lib("kernel/include/logger.hrl").

%%====================================================================
%% Process Inspection
%%====================================================================

%% @doc Inspect a process in detail.
-spec inspect_process(pid() | atom()) -> map().
inspect_process(Pid) when is_pid(Pid) ->
    case erlang:process_info(Pid) of
        undefined ->
            #{error => process_not_found};
        Info ->
            #{pid => Pid,
              registered_name => proplists:get_value(registered_name, Info),
              status => proplists:get_value(status, Info),
              message_queue_len => proplists:get_value(message_queue_len, Info),
              heap_size => proplists:get_value(heap_size, Info),
              total_heap_size => proplists:get_value(total_heap_size, Info),
              memory => proplists:get_value(memory, Info),
              reductions => proplists:get_value(reductions, Info),
              current_function => proplists:get_value(current_function, Info),
              current_stacktrace => proplists:get_value(current_stacktrace, Info),
              initial_call => proplists:get_value(initial_call, Info),
              dictionary => proplists:get_value(dictionary, Info),
              links => proplists:get_value(links, Info),
              monitors => proplists:get_value(monitors, Info),
              monitored_by => proplists:get_value(monitored_by, Info),
              trap_exit => proplists:get_value(trap_exit, Info),
              error_handler => proplists:get_value(error_handler, Info),
              priority => proplists:get_value(priority, Info),
              group_leader => proplists:get_value(group_leader, Info),
              suspending => proplists:get_value(suspending, Info),
              sequential_trace_token => proplists:get_value(sequential_trace_token, Info),
              catch_level => proplists:get_value(catch_level, Info),
              backtrace => proplists:get_value(backtrace, Info)}
    end;
inspect_process(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> #{error => not_registered};
        Pid -> inspect_process(Pid)
    end.

%% @doc Inspect process tree starting from a root process.
-spec inspect_process_tree(pid() | atom()) -> #{atom() => term()}.
inspect_process_tree(Root) ->
    Pid = case Root of
        P when is_pid(P) -> P;
        N when is_atom(N) -> whereis(N)
    end,
    case Pid of
        undefined -> #{error => process_not_found};
        _ -> build_process_tree(Pid, #{visited => sets:new()})
    end.

build_process_tree(Pid, #{visited := Visited} = Acc) ->
    case sets:is_element(Pid, Visited) of
        true -> #{pid => Pid, circular => true};
        false ->
            Visited1 = sets:add_element(Pid, Visited),
            Info = inspect_process(Pid),
            Links = maps:get(links, Info, []),
            Children = [build_process_tree(Child, Acc#{visited => Visited1}) || Child <- Links],
            Info#{children => Children}
    end.

%% @doc Find processes matching criteria.
-spec find_processes(fun((map()) -> boolean())) -> [pid()].
find_processes(Pred) ->
    [P || P <- processes(), 
          Info <- [inspect_process(P)],
          maps:get(error, Info, undefined) =:= undefined,
          Pred(Info)].

%% @doc Find registered processes by name pattern.
-spec find_registered(string() | binary()) -> [{atom(), pid()}].
find_registered(Pattern) ->
    PatternStr = case Pattern of
        B when is_binary(B) -> binary_to_list(B);
        S when is_list(S) -> S
    end,
    All = registered(),
    [R || R <- All,
          atom_to_list(element(1, R)) =/= [],
          string:find(atom_to_list(element(1, R)), PatternStr) =/= nomatch].

%%====================================================================
%% Memory Analysis
%%====================================================================

%% @doc Get memory summary for the system.
-spec memory_summary() -> map().
memory_summary() ->
    Mem = erlang:memory(),
    #{total => proplists:get_value(total, Mem),
      processes => proplists:get_value(processes, Mem),
      processes_used => proplists:get_value(processes_used, Mem),
      system => proplists:get_value(system, Mem),
      atom => proplists:get_value(atom, Mem),
      atom_used => proplists:get_value(atom_used, Mem),
      binary => proplists:get_value(binary, Mem),
      code => proplists:get_value(code, Mem),
      ets => proplists:get_value(ets, Mem),
      maximum => proplists:get_value(maximum, Mem)}.

%% @doc Get memory usage by module (code size).
-spec memory_by_module() -> [{atom(), integer()}].
memory_by_module() ->
    Mods = [M || {M, _} <- code:all_loaded()],
    [{Mod, code:memory(Mod)} || Mod <- Mods, code:memory(Mod) > 0].

%% @doc Check for potential memory leaks (processes with large heaps).
-spec memory_leak_check() -> [{pid(), integer(), map()}].
memory_leak_check() ->
    Threshold = 10 * 1024 * 1024,  % 10MB
    [begin
         Info = inspect_process(P),
         Mem = maps:get(memory, Info, 0),
         {P, Mem, Info}
     end || P <- processes(),
            begin
                Info = inspect_process(P),
                Mem = maps:get(memory, Info, 0),
                Mem > Threshold
            end].

%% @doc Get top N processes by memory usage.
-spec top_memory_processes(integer()) -> [{pid(), integer(), map()}].
top_memory_processes(N) ->
    Processes = [begin
                     Info = inspect_process(P),
                     Mem = maps:get(memory, Info, 0),
                     {Mem, P, Info}
                 end || P <- processes(),
                        begin
                            Info = inspect_process(P),
                            maps:get(error, Info, undefined) =:= undefined
                        end],
    Sorted = lists:reverse(lists:sort(Processes)),
    [{P, Mem, Info} || {Mem, P, Info} <- lists:sublist(Sorted, N)].

%%====================================================================
%% Message Queue Analysis
%%====================================================================

%% @doc Get summary of message queues across all processes.
-spec mailbox_summary() -> map().
mailbox_summary() ->
    Queues = [begin
                  Info = inspect_process(P),
                  maps:get(message_queue_len, Info, 0)
              end || P <- processes(),
                     begin
                         Info = inspect_process(P),
                         maps:get(error, Info, undefined) =:= undefined
                     end],
    #{total_processes => length(Queues),
      processes_with_messages => length([Q || Q <- Queues, Q > 0]),
      total_messages => lists:sum(Queues),
      max_queue_length => case Queues of [] -> 0; _ -> lists:max(Queues) end,
      average_queue_length => case Queues of [] -> 0.0; _ -> lists:sum(Queues) / length(Queues) end}.

%% @doc Get top N processes by mailbox size.
-spec top_mailboxes(integer()) -> [{pid(), integer(), map()}].
top_mailboxes(N) ->
    Processes = [begin
                     Info = inspect_process(P),
                     QLen = maps:get(message_queue_len, Info, 0),
                     {QLen, P, Info}
                 end || P <- processes(),
                        begin
                            Info = inspect_process(P),
                            maps:get(error, Info, undefined) =:= undefined
                        end],
    Sorted = lists:reverse(lists:sort(Processes)),
    [{P, QLen, Info} || {QLen, P, Info} <- lists:sublist(Sorted, N)].

%% @doc Analyze mailbox contents (peek at messages).
-spec analyze_mailbox(pid()) -> map().
analyze_mailbox(Pid) ->
    case erlang:process_info(Pid, messages) of
        undefined -> #{error => process_not_found};
        {messages, Msgs} ->
            #{message_count => length(Msgs),
              message_types => count_message_types(Msgs),
              sample_messages => lists:sublist(Msgs, 10)}
    end.

count_message_types(Msgs) ->
    maps:from_list(
        lists:foldl(fun(Msg, Acc) ->
            Type = element(1, Msg),
            Count = maps:get(Type, Acc, 0),
            maps:put(Type, Count + 1, Acc)
        end, #{}, Msgs)).

%%====================================================================
%% ETS Analysis
%%====================================================================

%% @doc Get summary of all ETS tables.
-spec ets_summary() -> [map()].
ets_summary() ->
    Tables = ets:all(),
    [ets_info(T) || T <- Tables].

%% @doc Get detailed info about an ETS table.
-spec ets_info(atom() | integer()) -> map().
ets_info(Table) ->
    case catch ets:info(Table) of
        undefined -> #{error => table_not_found};
        Info when is_list(Info) ->
            #{name => proplists:get_value(name, Info),
              id => proplists:get_value(id, Info),
              type => proplists:get_value(type, Info),
              size => proplists:get_value(size, Info),
              memory => proplists:get_value(memory, Info),
              owner => proplists:get_value(owner, Info),
              protection => proplists:get_value(protection, Info),
              keypos => proplists:get_value(keypos, Info),
              read_concurrency => proplists:get_value(read_concurrency, Info),
              write_concurrency => proplists:get_value(write_concurrency, Info),
              compressed => proplists:get_value(compressed, Info)}
    end.

%% @doc Find ETS tables by name pattern.
-spec find_ets_by_pattern(string() | binary()) -> [atom()].
find_ets_by_pattern(Pattern) ->
    PatternStr = case Pattern of
        B when is_binary(B) -> binary_to_list(B);
        S when is_list(S) -> S
    end,
    Tables = ets:all(),
    [T || T <- Tables,
          case ets:info(T, name) of
              undefined -> false;
              Name when is_atom(Name) ->
                  string:find(atom_to_list(Name), PatternStr) =/= nomatch;
              _ -> false
          end].

%%====================================================================
%% System Statistics
%%====================================================================

%% @doc Get comprehensive system statistics.
-spec system_stats() -> map().
system_stats() ->
    #{memory => memory_summary(),
      processes => erlang:system_info(process_count),
      ports => erlang:system_info(port_count),
      ets_tables => length(ets:all()),
      atoms => erlang:system_info(atom_count),
      uptime => erlang:statistics(wall_clock),
      reductions => erlang:statistics(reductions),
      run_queue => erlang:statistics(run_queue),
      io => erlang:statistics(io),
      runtime => erlang:statistics(runtime),
      garbage_collection => erlang:statistics(garbage_collection),
      context_switches => erlang:statistics(context_switches),
      exact_reductions => erlang:statistics(exact_reductions)}.

%% @doc Get scheduler statistics.
-spec scheduler_stats() -> map().
scheduler_stats() ->
    #{schedulers => erlang:system_info(schedulers),
      schedulers_online => erlang:system_info(schedulers_online),
      scheduler_bindings => erlang:system_info(scheduler_bindings),
      cpu_topology => erlang:system_info(cpu_topology),
      logical_processors => erlang:system_info(logical_processors),
      logical_processors_available => erlang:system_info(logical_processors_available),
      logical_processors_online => erlang:system_info(logical_processors_online)}.

%% @doc Get garbage collection statistics.
-spec gc_stats() -> map().
gc_stats() ->
    GC = erlang:statistics(garbage_collection),
    #{number_of_gcs => element(1, GC),
      words_reclaimed => element(2, GC),
      minor_gcs => erlang:statistics(minor_gcs),
      major_gcs => erlang:statistics(major_gcs)}.

%%====================================================================
%% Deadlock Detection
%%====================================================================

%% @doc Detect potential deadlocks (processes waiting on each other).
-spec detect_deadlocks() -> [{pid(), pid(), term()}].
detect_deadlocks() ->
    Processes = [P || P <- processes()],
    Deadlocks = [],
    detect_deadlocks(Processes, Processes, Deadlocks).

detect_deadlocks([], _, Acc) -> Acc;
detect_deadlocks([P1 | Rest], All, Acc) ->
    Info1 = inspect_process(P1),
    Links1 = maps:get(links, Info1, []),
    Monitors1 = maps:get(monitors, Info1, []),
    Acc1 = lists:foldl(fun(P2, AccIn) ->
        Info2 = inspect_process(P2),
        Links2 = maps:get(links, Info2, []),
        Monitors2 = maps:get(monitors, Info2, []),
        case (lists:member(P1, Links2) orelse lists:member(P1, Monitors2)) andalso
             (lists:member(P2, Links1) orelse lists:member(P2, Monitors1)) of
            true -> [{P1, P2, circular_dependency} | AccIn];
            false -> AccIn
        end
    end, Acc, Rest),
    detect_deadlocks(Rest, All, Acc1).

%% @doc Detect message loops (processes sending messages to themselves).
-spec detect_message_loops() -> [{pid(), integer()}].
detect_message_loops() ->
    [begin
         Info = inspect_process(P),
         QLen = maps:get(message_queue_len, Info, 0),
         {P, QLen}
     end || P <- processes(),
            begin
                Info = inspect_process(P),
                QLen = maps:get(message_queue_len, Info, 0),
                QLen > 1000  % Threshold for potential loop
            end].

%%====================================================================
%% Performance Analysis
%%====================================================================

%% @doc Find performance bottlenecks.
-spec find_bottlenecks() -> [{pid(), map()}].
find_bottlenecks() ->
    Processes = [begin
                     Info = inspect_process(P),
                     Reductions = maps:get(reductions, Info, 0),
                     Memory = maps:get(memory, Info, 0),
                     QueueLen = maps:get(message_queue_len, Info, 0),
                     Score = Reductions + (Memory div 1024) + (QueueLen * 10),
                     {Score, P, Info}
                 end || P <- processes(),
                        begin
                            Info = inspect_process(P),
                            maps:get(error, Info, undefined) =:= undefined
                        end],
    Sorted = lists:reverse(lists:sort(Processes)),
    [{P, Info} || {_Score, P, Info} <- lists:sublist(Sorted, 10)].

%% @doc Get reductions for a process.
-spec process_reductions(pid()) -> integer().
process_reductions(Pid) ->
    Info = inspect_process(Pid),
    maps:get(reductions, Info, 0).

%% @doc Get top N processes by reductions.
-spec top_reductions(integer()) -> [{pid(), integer(), map()}].
top_reductions(N) ->
    Processes = [begin
                     Info = inspect_process(P),
                     Red = maps:get(reductions, Info, 0),
                     {Red, P, Info}
                 end || P <- processes(),
                        begin
                            Info = inspect_process(P),
                            maps:get(error, Info, undefined) =:= undefined
                        end],
    Sorted = lists:reverse(lists:sort(Processes)),
    [{P, Red, Info} || {Red, P, Info} <- lists:sublist(Sorted, N)].

%%====================================================================
%% Workflow-Specific Debugging
%%====================================================================

%% @doc Debug a workflow process comprehensively.
-spec debug_workflow(pid() | atom()) -> map().
debug_workflow(WorkflowPid) ->
    Pid = case WorkflowPid of
        P when is_pid(P) -> P;
        N when is_atom(N) -> whereis(N)
    end,
    case Pid of
        undefined -> #{error => workflow_not_found};
        _ ->
            ProcessInfo = inspect_process(Pid),
            #{process => ProcessInfo,
              workflow_state => debug_workflow_state(Pid),
              memory => maps:get(memory, ProcessInfo, 0),
              message_queue => maps:get(message_queue_len, ProcessInfo, 0),
              reductions => maps:get(reductions, ProcessInfo, 0)}
    end.

%% @doc Get workflow-specific state information.
-spec debug_workflow_state(pid()) -> map().
debug_workflow_state(Pid) ->
    try
        #{marking => gen_yawl:marking(Pid),
          enabled_transitions => gen_yawl:enabled_transitions(Pid),
          stats => gen_yawl:stats(Pid)}
    catch
        _:Reason -> #{error => Reason}
    end.

%% @doc Trace workflow execution with detailed logging.
-spec trace_workflow(pid(), map()) -> ok.
trace_workflow(Pid, Opts) ->
    Module = maps:get(module, Opts, gen_yawl),
    Level = maps:get(level, Opts, all),
    logger:set_application_level(cre, debug),
    case Level of
        all ->
            dbg:tracer(),
            dbg:p(Pid, [c, m, timestamp]),
            dbg:tpl(Module, cx);
        transitions ->
            dbg:tracer(),
            dbg:p(Pid, [c]),
            dbg:tpl(Module, step, x),
            dbg:tpl(Module, fire, x);
        marking ->
            dbg:tracer(),
            dbg:p(Pid, [c]),
            dbg:tpl(Module, marking, x),
            dbg:tpl(Module, inject, x);
        _ -> ok
    end,
    ok.

%%====================================================================
%% Crash Dump Analysis
%%====================================================================

%% @doc Analyze an Erlang crash dump file.
-spec analyze_crash_dump(file:filename()) -> map().
analyze_crash_dump(Filename) ->
    case file:read_file(Filename) of
        {ok, Contents} ->
            analyze_crash_dump_content(Contents);
        {error, Reason} ->
            #{error => Reason}
    end.

analyze_crash_dump_content(Contents) ->
    Lines = binary:split(Contents, <<"\n">>, [global]),
    #{total_lines => length(Lines),
      processes => count_pattern(Lines, <<"=proc:">>),
      ports => count_pattern(Lines, <<"=port:">>),
      ets_tables => count_pattern(Lines, <<"=ets_table:">>),
      atoms => count_pattern(Lines, <<"=atom:">>),
      nodes => count_pattern(Lines, <<"=node:">>),
      loaded_modules => count_pattern(Lines, <<"=loaded:">>)}.

count_pattern(Lines, Pattern) ->
    length([L || L <- Lines, binary:match(L, Pattern) =/= nomatch]).

%% @doc Extract process information from crash dump.
-spec extract_process_info(file:filename(), pid() | atom()) -> map().
extract_process_info(Filename, PidOrName) ->
    case file:read_file(Filename) of
        {ok, Contents} ->
            extract_process_from_dump(Contents, PidOrName);
        {error, Reason} ->
            #{error => Reason}
    end.

extract_process_from_dump(Contents, PidOrName) ->
    PidStr = case PidOrName of
        P when is_pid(P) -> pid_to_list(P);
        N when is_atom(N) -> atom_to_list(N)
    end,
    Lines = binary:split(Contents, <<"\n">>, [global]),
    ProcessLines = extract_process_lines(Lines, PidStr),
    #{pid => PidOrName,
      dump_lines => ProcessLines,
      line_count => length(ProcessLines)}.

extract_process_lines(Lines, PidStr) ->
    PidBin = list_to_binary(PidStr),
    InProcess = false,
    extract_process_lines(Lines, PidBin, InProcess, []).

extract_process_lines([], _, _, Acc) -> lists:reverse(Acc);
extract_process_lines([Line | Rest], PidBin, InProcess, Acc) ->
    case binary:match(Line, PidBin) =/= nomatch of
        true ->
            extract_process_lines(Rest, PidBin, true, [Line | Acc]);
        false when InProcess ->
            case binary:match(Line, <<"=">>) =/= nomatch andalso
                 binary:match(Line, <<":">>) =/= nomatch of
                true -> lists:reverse(Acc);  % Next section
                false -> extract_process_lines(Rest, PidBin, true, [Line | Acc])
            end;
        false ->
            extract_process_lines(Rest, PidBin, false, Acc)
    end.

%%====================================================================
%% Observer Integration
%%====================================================================

%% @doc Start observer GUI.
-spec start_observer() -> {ok, pid()} | {error, term()}.
start_observer() ->
    observer:start().

%% @doc Connect observer to a remote node.
-spec observer_connect(node()) -> ok | {error, term()}.
observer_connect(Node) ->
    observer:start(),
    observer:start(Node).
