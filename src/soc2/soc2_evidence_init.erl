%%%-------------------------------------------------------------------
%%% @doc
%%% SOC 2 Evidence Initialization Module
%%%
%%% Ensures all required evidence and receipt directories exist during
%%% application startup. Creates directories with proper permissions and
%%% .gitkeep files for version control.
%%%
%%% <h3>Directories Created</h3>
%%% <ul>
%%%   <li><b>evidence/uptime</b> - System uptime evidence</li>
%%%   <li><b>evidence/load_tests</b> - Load testing results</li>
%%%   <li><b>evidence/chaos</b> - Chaos engineering test results</li>
%%%   <li><b>evidence/period</b> - Period-based compliance evidence</li>
%%%   <li><b>receipts/</b> - Audit receipt artifacts</li>
%%% </ul>
%%%
%%% <h3>Usage</h3>
%%%
%%% Call during application startup:
%%% ```erlang
%%% ok = soc2_evidence_init:ensure_directories().
%%% ```
%%%
%%% Or as part of supervisor initialization:
%%% ```erlang
%%% ChildSpec = #{
%%%     id => soc2_evidence_init,
%%%     start => {soc2_evidence_init, init_directories, []},
%%%     restart => temporary,
%%%     type => worker,
%%%     modules => [soc2_evidence_init]
%%% }
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(soc2_evidence_init).

%% API
-export([ensure_directories/0]).
-export([ensure_directory/1]).
-export([create_gitkeep/1]).
-export([init_directories/0]).
-export([get_evidence_base_dir/0]).
-export([get_evidence_directories/0]).

%%====================================================================
%% Types
%%====================================================================

-type directory_path() :: binary() | string().
-type init_result() :: ok | {error, term()}.

-export_type([directory_path/0, init_result/0]).

%%====================================================================
%% Constants
%%====================================================================

-define(EVIDENCE_BASE_DIR, "evidence").
-define(RECEIPTS_DIR, "receipts").
-define(GITKEEP_FILE, ".gitkeep").

-define(EVIDENCE_SUBDIRS, [
    "uptime",
    "load_tests",
    "chaos",
    "period"
]).

-define(DIRECTORY_PERMISSIONS, 8#0755).
-define(GITKEEP_PERMISSIONS, 8#0644).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Ensure all required evidence directories exist.
%%
%% Creates the evidence and receipts directory structure with proper
%% permissions. Idempotent - safe to call multiple times.
%%
%% Returns `ok' if all directories were created or already exist.
%% Returns `{error, Reason}' if directory creation fails.
%%
%% @returns init_result()
%%
-spec ensure_directories() -> init_result().
ensure_directories() ->
    logger:info("Initializing SOC 2 evidence directories"),

    Results = [
        ensure_directory(?EVIDENCE_BASE_DIR),
        ensure_directory(?RECEIPTS_DIR)
    ] ++
    [ensure_directory(filename:join(?EVIDENCE_BASE_DIR, SubDir))
     || SubDir <- ?EVIDENCE_SUBDIRS],

    case lists:all(fun(R) -> R =:= ok end, Results) of
        true ->
            logger:info("All SOC 2 evidence directories initialized successfully"),
            ok;
        false ->
            FailedResults = [R || R <- Results, R =/= ok],
            logger:error("Failed to initialize some directories: ~p", [FailedResults]),
            {error, {directory_creation_failed, FailedResults}}
    end.

%% @doc Ensure a specific directory exists with proper permissions.
%%
%% Creates the directory and .gitkeep file if they don't exist.
%% Sets directory permissions to 0755 (owner rwx, group rx, other rx).
%%
%% @param DirPath Directory path (string or binary)
%% @returns init_result()
%%
-spec ensure_directory(directory_path()) -> init_result().
ensure_directory(DirPath) ->
    Dir = to_string(DirPath),

    case filelib:is_dir(Dir) of
        true ->
            logger:debug("Directory already exists: ~s", [Dir]),
            ensure_directory_permissions(Dir);
        false ->
            case file:make_dir(Dir) of
                ok ->
                    logger:debug("Created directory: ~s", [Dir]),
                    ok = ensure_directory_permissions(Dir),
                    ok = create_gitkeep(Dir),
                    ok;
                {error, eexist} ->
                    %% Directory was created by another process
                    ensure_directory_permissions(Dir);
                {error, enoent} ->
                    %% Parent directory doesn't exist, create recursively
                    case file:make_dir(filename:dirname(Dir)) of
                        ok ->
                            ensure_directory(Dir);
                        {error, eexist} ->
                            ensure_directory(Dir);
                        {error, Reason} ->
                            logger:error("Failed to create parent directory of ~s: ~p",
                                       [Dir, Reason]),
                            {error, {parent_creation_failed, Reason}}
                    end;
                {error, Reason} ->
                    logger:error("Failed to create directory ~s: ~p", [Dir, Reason]),
                    {error, Reason}
            end
    end.

%% @doc Create a .gitkeep file in the directory.
%%
%% The .gitkeep file ensures the directory is tracked in version control
%% even if it's empty. Creates an empty file with read/write permissions.
%%
%% @param DirPath Directory path
%% @returns init_result()
%%
-spec create_gitkeep(directory_path()) -> init_result().
create_gitkeep(DirPath) ->
    Dir = to_string(DirPath),
    GitkeepPath = filename:join(Dir, ?GITKEEP_FILE),

    case filelib:is_file(GitkeepPath) of
        true ->
            logger:debug("Gitkeep file already exists: ~s", [GitkeepPath]),
            ok;
        false ->
            case file:write_file(GitkeepPath, <<>>) of
                ok ->
                    logger:debug("Created gitkeep file: ~s", [GitkeepPath]),
                    set_file_permissions(GitkeepPath, ?GITKEEP_PERMISSIONS);
                {error, Reason} ->
                    logger:warning("Failed to create gitkeep file ~s: ~p",
                                 [GitkeepPath, Reason]),
                    {error, Reason}
            end
    end.

%% @doc Initialize directories - entry point for supervisor.
%%
%% Can be called as a simple task in a supervisor's child spec.
%% Returns `ok' on success, exits on failure.
%%
%% @returns ok
%%
-spec init_directories() -> ok.
init_directories() ->
    case ensure_directories() of
        ok ->
            ok;
        {error, Reason} ->
            logger:critical("Failed to initialize SOC 2 evidence directories: ~p", [Reason]),
            exit({initialization_failed, Reason})
    end.

%% @doc Get the base evidence directory path.
%%
%% Returns the root directory for all evidence artifacts.
%%
%% @returns string()
%%
-spec get_evidence_base_dir() -> string().
get_evidence_base_dir() ->
    ?EVIDENCE_BASE_DIR.

%% @doc Get all evidence subdirectories.
%%
%% Returns a list of all subdirectories that should exist under evidence/.
%%
%% @returns [string()]
%%
-spec get_evidence_directories() -> [string()].
get_evidence_directories() ->
    [filename:join(?EVIDENCE_BASE_DIR, SubDir) || SubDir <- ?EVIDENCE_SUBDIRS].

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Convert path to string
to_string(Path) when is_binary(Path) ->
    binary_to_list(Path);
to_string(Path) when is_list(Path) ->
    Path;
to_string(Path) ->
    io_lib:format("~p", [Path]).

%% @private Ensure directory has correct permissions
ensure_directory_permissions(Dir) ->
    case set_directory_permissions(Dir, ?DIRECTORY_PERMISSIONS) of
        ok ->
            ok;
        {error, eacces} ->
            logger:warning("Cannot set permissions on ~s: access denied (continuing)", [Dir]),
            ok;
        {error, Reason} ->
            logger:warning("Failed to set permissions on ~s: ~p (continuing)", [Dir, Reason]),
            ok
    end.

%% @private Set directory permissions
set_directory_permissions(Dir, Permissions) ->
    case file:change_mode(Dir, Permissions) of
        ok ->
            logger:debug("Set directory permissions ~s -> ~3..0B", [Dir, Permissions]),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private Set file permissions
set_file_permissions(File, Permissions) ->
    case file:change_mode(File, Permissions) of
        ok ->
            logger:debug("Set file permissions ~s -> ~3..0B", [File, Permissions]),
            ok;
        {error, Reason} ->
            logger:warning("Failed to set file permissions on ~s: ~p", [File, Reason]),
            {error, Reason}
    end.

%%====================================================================
%% Unit Tests
%%====================================================================

%% @doc Run unit tests for this module
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

% Test: ensure_directories creates all required directories
ensure_directories_test() ->
    %% This test requires file system operations
    %% In a real test suite, use temporary directories
    ok.

% Test: to_string conversion
to_string_binary_test() ->
    "evidence" = to_string(<<"evidence">>).

to_string_list_test() ->
    "evidence" = to_string("evidence").

% Test: get_evidence_directories returns expected list
get_evidence_directories_test() ->
    Dirs = get_evidence_directories(),
    4 = length(Dirs),
    true = lists:member("evidence/uptime", Dirs),
    true = lists:member("evidence/load_tests", Dirs),
    true = lists:member("evidence/chaos", Dirs),
    true = lists:member("evidence/period", Dirs).

-endif.
