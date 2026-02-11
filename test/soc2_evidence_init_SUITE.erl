%%%-------------------------------------------------------------------
%%% @doc
%%% Common Test suite for soc2_evidence_init module
%%%
%%% Tests directory creation, permission handling, and gitkeep files.
%%% @end
%%%-------------------------------------------------------------------

-module(soc2_evidence_init_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Suite callbacks
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Test cases
-export([
    test_get_evidence_base_dir/1,
    test_get_evidence_directories/1,
    test_ensure_directory_creates_dir/1,
    test_ensure_directory_idempotent/1,
    test_create_gitkeep/1,
    test_ensure_directories_all/1,
    test_directory_structure/1
]).

%%====================================================================
%% Suite Callbacks
%%====================================================================

all() ->
    [
        test_get_evidence_base_dir,
        test_get_evidence_directories,
        test_ensure_directory_creates_dir,
        test_ensure_directory_idempotent,
        test_create_gitkeep,
        test_ensure_directories_all,
        test_directory_structure
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    %% Create a temporary directory for each test
    TmpDir = mk_tmp_dir(),
    [{tmp_dir, TmpDir} | Config].

end_per_testcase(_TestCase, Config) ->
    %% Clean up temporary directory
    TmpDir = ?config(tmp_dir, Config),
    rm_rf(TmpDir),
    ok.

%%====================================================================
%% Test Cases
%%====================================================================

test_get_evidence_base_dir(_Config) ->
    %% Test that get_evidence_base_dir returns expected value
    "evidence" = soc2_evidence_init:get_evidence_base_dir().

test_get_evidence_directories(_Config) ->
    %% Test that get_evidence_directories returns all subdirectories
    Dirs = soc2_evidence_init:get_evidence_directories(),

    %% Should be 4 subdirectories
    4 = length(Dirs),

    %% Check all expected directories are present
    true = lists:member("evidence/uptime", Dirs),
    true = lists:member("evidence/load_tests", Dirs),
    true = lists:member("evidence/chaos", Dirs),
    true = lists:member("evidence/period", Dirs),

    %% No other directories
    4 = length(Dirs).

test_ensure_directory_creates_dir(Config) ->
    TmpDir = ?config(tmp_dir, Config),
    TestDir = filename:join(TmpDir, "test_dir"),

    %% Directory shouldn't exist yet
    false = filelib:is_dir(TestDir),

    %% Create directory
    ok = soc2_evidence_init:ensure_directory(TestDir),

    %% Now it should exist
    true = filelib:is_dir(TestDir),

    %% .gitkeep should be created
    GitkeepPath = filename:join(TestDir, ".gitkeep"),
    true = filelib:is_file(GitkeepPath).

test_ensure_directory_idempotent(Config) ->
    TmpDir = ?config(tmp_dir, Config),
    TestDir = filename:join(TmpDir, "idempotent_dir"),

    %% Create directory first time
    ok = soc2_evidence_init:ensure_directory(TestDir),
    true = filelib:is_dir(TestDir),

    %% Create again - should succeed without error
    ok = soc2_evidence_init:ensure_directory(TestDir),
    true = filelib:is_dir(TestDir).

test_create_gitkeep(Config) ->
    TmpDir = ?config(tmp_dir, Config),
    TestDir = filename:join(TmpDir, "gitkeep_test"),

    %% Create directory first
    ok = file:make_dir(TestDir),

    %% Create .gitkeep
    ok = soc2_evidence_init:create_gitkeep(TestDir),

    %% Verify file exists
    GitkeepPath = filename:join(TestDir, ".gitkeep"),
    true = filelib:is_file(GitkeepPath),

    %% Verify it's empty
    {ok, <<>>} = file:read_file(GitkeepPath).

test_ensure_directories_all(Config) ->
    TmpDir = ?config(tmp_dir, Config),

    %% Change to temp directory for this test
    {ok, OldDir} = file:get_cwd(),
    ok = file:set_cwd(TmpDir),

    try
        %% Ensure all directories
        ok = soc2_evidence_init:ensure_directories(),

        %% Verify base directory
        true = filelib:is_dir("evidence"),

        %% Verify all subdirectories
        true = filelib:is_dir("evidence/uptime"),
        true = filelib:is_dir("evidence/load_tests"),
        true = filelib:is_dir("evidence/chaos"),
        true = filelib:is_dir("evidence/period"),

        %% Verify receipts directory
        true = filelib:is_dir("receipts"),

        %% Verify gitkeep files exist in all subdirectories
        true = filelib:is_file("evidence/uptime/.gitkeep"),
        true = filelib:is_file("evidence/load_tests/.gitkeep"),
        true = filelib:is_file("evidence/chaos/.gitkeep"),
        true = filelib:is_file("evidence/period/.gitkeep"),
        true = filelib:is_file("receipts/.gitkeep")

    after
        file:set_cwd(OldDir)
    end.

test_directory_structure(Config) ->
    TmpDir = ?config(tmp_dir, Config),

    %% Change to temp directory
    {ok, OldDir} = file:get_cwd(),
    ok = file:set_cwd(TmpDir),

    try
        %% Initialize directories
        ok = soc2_evidence_init:init_directories(),

        %% Verify complete directory structure
        ExpectedDirs = [
            "evidence",
            "evidence/uptime",
            "evidence/load_tests",
            "evidence/chaos",
            "evidence/period",
            "receipts"
        ],

        lists:foreach(fun(Dir) ->
            ?assert(filelib:is_dir(Dir), "Missing directory: " ++ Dir),
            GitkeepPath = filename:join(Dir, ".gitkeep"),
            ?assert(filelib:is_file(GitkeepPath), "Missing .gitkeep in: " ++ Dir)
        end, ExpectedDirs)

    after
        file:set_cwd(OldDir)
    end.

%%====================================================================
%% Helper Functions
%%====================================================================

%% Create a temporary directory
mk_tmp_dir() ->
    Timestamp = integer_to_list(erlang:monotonic_time(microsecond)),
    Pid = integer_to_list(erlang:phash2(self())),
    UniqueName = lists:flatten(io_lib:format("cre_test_~s_~s", [Timestamp, Pid])),
    TmpDir = filename:join("/tmp", UniqueName),
    case file:make_dir(TmpDir) of
        ok -> TmpDir;
        {error, eexist} -> TmpDir
    end.

%% Recursively remove directory tree
rm_rf(Path) ->
    case filelib:is_dir(Path) of
        true ->
            {ok, Files} = file:list_dir(Path),
            lists:foreach(fun(F) ->
                rm_rf(filename:join(Path, F))
            end, Files),
            file:del_dir(Path);
        false ->
            file:delete(Path)
    end.
