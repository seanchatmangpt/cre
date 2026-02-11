%%% @doc TTB Trace Collection Tests
%%%
%%% Tests for evidence_ttb module including:
%%% - Starting and stopping traces
%%% - Trace pattern generation
%%% - Event filtering by process/module/function
%%% - Summary generation
%%% - Cleanup operations
%%%
%%% @end
-module(evidence_ttb_test).

-include_lib("eunit/include/eunit.hrl").

%%% TESTS ===================================================================

%%--------------------------------------------------------------------
%% Test 1: Trace pattern returns expected patterns for gen_yawl
%%--------------------------------------------------------------------
gen_yawl_trace_pattern_test() ->
    Patterns = evidence_ttb:trace_pattern(gen_yawl),

    %% Check for key gen_yawl callbacks
    ?assert(lists:keymember(gen_yawl, 1, Patterns)),
    ?assert(lists:keymember({gen_yawl, fire, 3}, 1, Patterns) orelse
            lists:keymember(gen_yawl, 1, Patterns)),

    %% Verify all patterns are 3-tuples
    ?assert(lists:all(fun
        ({_M, _F, _A}) -> true;
        (_) -> false
    end, Patterns)).

%%--------------------------------------------------------------------
%% Test 2: Trace pattern returns expected patterns for wf_audit_log
%%--------------------------------------------------------------------
wf_audit_log_trace_pattern_test() ->
    Patterns = evidence_ttb:trace_pattern(wf_audit_log),

    %% Check for append function
    ?assert(lists:keymember(wf_audit_log, 1, Patterns)),

    %% Verify structure
    ?assert(lists:all(fun
        ({_M, _F, _A}) -> true;
        (_) -> false
    end, Patterns)).

%%--------------------------------------------------------------------
%% Test 3: Trace pattern returns expected patterns for supervisor
%%--------------------------------------------------------------------
supervisor_trace_pattern_test() ->
    Patterns = evidence_ttb:trace_pattern(supervisor),

    %% Check for start_child
    ?assert(lists:keymember(supervisor, 1, Patterns)),

    %% Verify structure
    ?assert(lists:all(fun
        ({_M, _F, _A}) -> true;
        (_) -> false
    end, Patterns)).

%%--------------------------------------------------------------------
%% Test 4: Trace pattern returns expected patterns for ln_ctrl
%%--------------------------------------------------------------------
ln_ctrl_trace_pattern_test() ->
    Patterns = evidence_ttb:trace_pattern(ln_ctrl),

    %% Check for init callback
    ?assert(lists:keymember(ln_ctrl, 1, Patterns)),

    %% Verify structure
    ?assert(lists:all(fun
        ({_M, _F, _A}) -> true;
        (_) -> false
    end, Patterns)).

%%--------------------------------------------------------------------
%% Test 5: Default trace pattern includes all categories
%%--------------------------------------------------------------------
default_trace_pattern_test() ->
    Patterns = evidence_ttb:trace_pattern(default),
    AllPatterns = evidence_ttb:trace_pattern(all),

    %% Default should include all
    ?assertEqual(AllPatterns, Patterns).

%%--------------------------------------------------------------------
%% Test 6: Unknown pattern category returns empty list
%%--------------------------------------------------------------------
unknown_trace_pattern_test() ->
    Patterns = evidence_ttb:trace_pattern(unknown_category),
    ?assertEqual([], Patterns).

%%--------------------------------------------------------------------
%% Test 7: All trace pattern combines all categories
%%--------------------------------------------------------------------
all_trace_pattern_test() ->
    GenYawl = evidence_ttb:trace_pattern(gen_yawl),
    WfAudit = evidence_ttb:trace_pattern(wf_audit_log),
    Sup = evidence_ttb:trace_pattern(supervisor),
    LnCtrl = evidence_ttb:trace_pattern(ln_ctrl),
    All = evidence_ttb:trace_pattern(all),

    Expected = GenYawl ++ WfAudit ++ Sup ++ LnCtrl,
    ?assertEqual(Expected, All).

%%--------------------------------------------------------------------
%% Test 8: Start trace creates a trace file
%%--------------------------------------------------------------------
start_trace_test() ->
    Name = 'test_trace_start',

    %% Start a trace
    {ok, TraceFile} = evidence_ttb:start_trace(Name),

    %% Verify file path is a string
    ?assert(is_list(TraceFile)),

    %% Stop and cleanup
    {ok, _} = evidence_ttb:stop_trace(Name),
    ok = evidence_ttb:cleanup_trace(TraceFile).

%%--------------------------------------------------------------------
%% Test 9: Start trace with custom directory
%%--------------------------------------------------------------------
start_trace_with_dir_test() ->
    Name = 'test_trace_dir',
    Dir = "/tmp",

    {ok, TraceFile} = evidence_ttb:start_trace(Name, #{dir => Dir}),

    %% Verify file is in the specified directory
    ?assertEqual(Dir, filename:dirname(TraceFile)),

    %% Stop and cleanup
    {ok, _} = evidence_ttb:stop_trace(Name),
    ok = evidence_ttb:cleanup_trace(TraceFile).

%%--------------------------------------------------------------------
%% Test 10: Start trace with custom size option
%%--------------------------------------------------------------------
start_trace_with_size_test() ->
    Name = 'test_trace_size',

    %% Start with 5MB size limit
    {ok, TraceFile} = evidence_ttb:start_trace(Name, #{size => 5}),

    ?assert(is_list(TraceFile)),

    %% Stop and cleanup
    {ok, _} = evidence_ttb:stop_trace(Name),
    ok = evidence_ttb:cleanup_trace(TraceFile).

%%--------------------------------------------------------------------
%% Test 11: Stop trace without active trace returns error
%%--------------------------------------------------------------------
stop_trace_no_active_test() ->
    %% First ensure no active trace
    erase(evidence_ttb_state),

    %% Attempt to stop without active trace
    ?assertEqual({error, no_active_trace}, evidence_ttb:stop_trace()).

%%--------------------------------------------------------------------
%% Test 12: Stop named trace
%%--------------------------------------------------------------------
stop_named_trace_test() ->
    Name = 'test_stop_named',

    %% Start trace
    {ok, TraceFile} = evidence_ttb:start_trace(Name),

    %% Stop by name
    {ok, StoppedFile} = evidence_ttb:stop_trace(Name),

    %% Cleanup
    ok = evidence_ttb:cleanup_trace(TraceFile).

%%--------------------------------------------------------------------
%% Test 13: Filter trace by module returns matching events
%%--------------------------------------------------------------------
filter_trace_by_module_test() ->
    %% Create sample trace events
    Events = [
        {trace, self(), call, {gen_yawl, fire, 3}},
        {trace, self(), call, {wf_audit_log, append, 2}},
        {trace, self(), call, {other_module, foo, 0}}
    ],

    %% Write to temp file
    TempFile = "/tmp/filter_mod_test.trace",
    ok = file:write_file(TempFile, term_to_binary(Events)),

    %% Filter by wf_audit_log module
    {ok, Filtered} = evidence_ttb:filter_trace(TempFile, #{module => wf_audit_log}),

    %% Should match only the append call
    ?assertEqual(1, length(Filtered)),

    %% Cleanup
    ok = file:delete(TempFile).

%%--------------------------------------------------------------------
%% Test 14: Filter trace by PID returns matching events
%%--------------------------------------------------------------------
filter_trace_by_pid_test() ->
    Pid = self(),

    %% Create sample events with different PIDs
    Events = [
        {trace, Pid, call, {gen_yawl, fire, 3}},
        {trace, list_to_pid("<0.999.0>"), call, {wf_audit_log, append, 2}}
    ],

    TempFile = "/tmp/filter_pid_test.trace",
    ok = file:write_file(TempFile, term_to_binary(Events)),

    %% Filter by our PID
    {ok, Filtered} = evidence_ttb:filter_trace(TempFile, #{pid => Pid}),

    %% Should match only our event
    ?assertEqual(1, length(Filtered)),

    ok = file:delete(TempFile).

%%--------------------------------------------------------------------
%% Test 15: Filter trace by function name
%%--------------------------------------------------------------------
filter_trace_by_function_test() ->
    Events = [
        {trace, self(), call, {gen_yawl, fire, 3}},
        {trace, self(), call, {gen_yawl, init, 1}},
        {trace, self(), call, {gen_yawl, fire, 3}}
    ],

    TempFile = "/tmp/filter_fun_test.trace",
    ok = file:write_file(TempFile, term_to_binary(Events)),

    %% Filter by fire/3
    {ok, Filtered} = evidence_ttb:filter_trace(TempFile, #{function => {fire, 3}}),

    %% Should match only fire/3 calls
    ?assertEqual(2, length(Filtered)),

    ok = file:delete(TempFile).

%%--------------------------------------------------------------------
%% Test 16: Summarize trace returns expected structure
%%--------------------------------------------------------------------
summarize_trace_structure_test() ->
    %% Create sample trace data
    Events = [
        {trace, self(), call, {gen_yawl, fire, 3}},
        {trace, self(), call, {gen_yawl, fire, 3}},
        {trace, self(), call, {wf_audit_log, append, 2}},
        {trace, self(), call, {supervisor, start_child, 2}}
    ],

    TempFile = "/tmp/summarize_test.trace",
    ok = file:write_file(TempFile, term_to_binary(Events)),

    %% Generate summary
    Summary = evidence_ttb:summarize_trace(TempFile),

    %% Verify structure
    ?assert(is_map(Summary)),
    ?assert(maps:is_key(trace_file, Summary)),
    ?assert(maps:is_key(event_count, Summary)),
    ?assert(maps:is_key(duration_ms, Summary)),
    ?assert(maps:is_key(modules_traced, Summary)),
    ?assert(maps:is_key(top_functions, Summary)),

    %% Verify event count
    ?assertEqual(4, maps:get(event_count, Summary)),

    %% Cleanup
    ok = file:delete(TempFile).

%%--------------------------------------------------------------------
%% Test 17: Summarize trace counts modules correctly
%%--------------------------------------------------------------------
summarize_modules_test() ->
    Events = [
        {trace, self(), call, {gen_yawl, fire, 3}},
        {trace, self(), call, {wf_audit_log, append, 2}},
        {trace, self(), call, {supervisor, start_child, 2}}
    ],

    TempFile = "/tmp/summarize_mod_test.trace",
    ok = file:write_file(TempFile, term_to_binary(Events)),

    Summary = evidence_ttb:summarize_trace(TempFile),
    Modules = maps:get(modules_traced, Summary),

    %% Should have three unique modules
    ?assert(lists:member(gen_yawl, Modules)),
    ?assert(lists:member(wf_audit_log, Modules)),
    ?assert(lists:member(supervisor, Modules)),

    ok = file:delete(TempFile).

%%--------------------------------------------------------------------
%% Test 18: Summarize trace counts top functions
%%--------------------------------------------------------------------
summarize_top_functions_test() ->
    Events = [
        {trace, self(), call, {gen_yawl, fire, 3}},
        {trace, self(), call, {gen_yawl, fire, 3}},
        {trace, self(), call, {gen_yawl, fire, 3}},
        {trace, self(), call, {wf_audit_log, append, 2}},
        {trace, self(), call, {wf_audit_log, append, 2}}
    ],

    TempFile = "/tmp/summarize_top_test.trace",
    ok = file:write_file(TempFile, term_to_binary(Events)),

    %% Get top 2 functions
    Summary = evidence_ttb:summarize_trace(TempFile, #{top_n => 2}),
    TopFuns = maps:get(top_functions, Summary),

    %% fire/3 should be first with count 3
    ?assertMatch([{{gen_yawl, fire, 3}, 3}, {{wf_audit_log, append, 2}, 2}], TopFuns),

    ok = file:delete(TempFile).

%%--------------------------------------------------------------------
%% Test 19: Format summary returns iolist
%%--------------------------------------------------------------------
format_summary_test() ->
    Summary = #{
        trace_file => "/tmp/test.trace",
        event_count => 100,
        duration_ms => 500,
        modules_traced => [gen_yawl, wf_audit_log],
        top_functions => [{{gen_yawl, fire, 3}, 50}]
    },

    Formatted = evidence_ttb:format_summary(Summary),

    %% Should return an iolist
    ?assert(is_list(Formatted)),

    %% Should contain key strings
    Flat = lists:flatten(Formatted),
    ?assert(string:str(Flat, "Trace File") > 0),
    ?assert(string:str(Flat, "Event Count") > 0),
    ?assert(string:str(Flat, "Duration") > 0).

%%--------------------------------------------------------------------
%% Test 20: Cleanup trace removes the file
%%--------------------------------------------------------------------
cleanup_trace_test() ->
    %% Create a temp file
    TempFile = "/tmp/cleanup_test.trace",
    ok = file:write_file(TempFile, <<"test data">>),

    %% Verify it exists
    {ok, _} = file:read_file_info(TempFile),

    %% Cleanup
    ok = evidence_ttb:cleanup_trace(TempFile),

    %% Verify it's gone
    {error, enoent} = file:read_file_info(TempFile).

%%--------------------------------------------------------------------
%% Test 21: Cleanup non-existent file returns ok
%%--------------------------------------------------------------------
cleanup_nonexistent_trace_test() ->
    %% Should not error on missing file
    ?assertEqual(ok, evidence_ttb:cleanup_trace("/tmp/nonexistent_file_12345.trace")).

%%--------------------------------------------------------------------
%% Test 22: Filter with empty filter returns all events
%%--------------------------------------------------------------------
filter_empty_filter_test() ->
    Events = [
        {trace, self(), call, {gen_yawl, fire, 3}},
        {trace, self(), call, {wf_audit_log, append, 2}}
    ],

    TempFile = "/tmp/filter_empty_test.trace",
    ok = file:write_file(TempFile, term_to_binary(Events)),

    %% Empty filter should return all events
    {ok, Filtered} = evidence_ttb:filter_trace(TempFile, #{}),
    ?assertEqual(2, length(Filtered)),

    ok = file:delete(TempFile).

%%--------------------------------------------------------------------
%% Test 23: Summarize empty trace returns zeros
%%--------------------------------------------------------------------
summarize_empty_trace_test() ->
    Events = [],

    TempFile = "/tmp/summarize_empty_test.trace",
    ok = file:write_file(TempFile, term_to_binary(Events)),

    Summary = evidence_ttb:summarize_trace(TempFile),

    ?assertEqual(0, maps:get(event_count, Summary)),
    ?assertEqual([], maps:get(modules_traced, Summary)),
    ?assertEqual([], maps:get(top_functions, Summary)),

    ok = file:delete(TempFile).

%%--------------------------------------------------------------------
%% Test 24: Multiple trace sessions can be active
%%--------------------------------------------------------------------
multiple_trace_sessions_test() ->
    Name1 = 'test_trace_1',
    Name2 = 'test_trace_2',

    %% Start two traces
    {ok, File1} = evidence_ttb:start_trace(Name1),
    {ok, File2} = evidence_ttb:start_trace(Name2),

    %% Should have different files
    ?assert(File1 =/= File2),

    %% Stop both
    {ok, _} = evidence_ttb:stop_trace(Name1),
    {ok, _} = evidence_ttb:stop_trace(Name2),

    %% Cleanup
    ok = evidence_ttb:cleanup_trace(File1),
    ok = evidence_ttb:cleanup_trace(File2).

%%--------------------------------------------------------------------
%% Test 25: Trace pattern for ln_ctrl includes key callbacks
%%--------------------------------------------------------------------
ln_ctrl_pattern_callbacks_test() ->
    Patterns = evidence_ttb:trace_pattern(ln_ctrl),

    %% Check for handle_call, handle_cast, init
    ModFuns = [{M, F} || {M, F, _A} <- Patterns],
    ?assert(lists:member({ln_ctrl, handle_call}, ModFuns)),
    ?assert(lists:member({ln_ctrl, handle_cast}, ModFuns)),
    ?assert(lists:member({ln_ctrl, init}, ModFuns)),
    ?assert(lists:member({ln_ctrl, terminate}, ModFuns)).
