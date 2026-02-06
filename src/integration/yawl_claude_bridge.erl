%% -*- erlang -*-
%%
%% CRE: common runtime environment for distributed programming languages
%%
%% Claude Code Headless Mode Bridge
%%
%% This module provides a bridge to Claude Code's headless mode for
%% LLM-powered approval decision-making in YAWL workflows.
%%
%% @author CRE Team
%% @version 1.0.0
%% @doc YAWL Claude Code Headless Bridge
%%
%% <h3>Overview</h3>
%% This module bridges YAWL workflow execution with Claude Code headless mode:
%% <ul>
%%   <li>Invoke Claude Code with prompts and get JSON responses</li>
%%   <li>Support session continuation for multi-turn approvals</li>
%%   <li>Validate JSON schema compliance</li>
%%   <li>Handle tool auto-approval patterns</li>
%% </ul>
%%
%% <h3>Claude Code Integration</h3>
%% The bridge uses Claude Code CLI with these key features:
%% <ul>
%%   <li><code>-p</code> flag for prompt-based interaction</li>
%%   <li><code>--output-format json</code> for structured output</li>
%%   <li><code>--json-schema</code> for response validation</li>
%%   <li><code>--allowed-tools</code> for tool control</li>
%% </ul>
%% @end
%% -------------------------------------------------------------------

-module(yawl_claude_bridge).
-author("CRE Team").

%% Include record definitions
-include("cre_yawl.hrl").

%%====================================================================
%% Exports
%%====================================================================

%% Main API
-export([
    prompt_claude/2,
    prompt_claude/3,
    start_session/1,
    continue_session/2,
    end_session/1,
    generate_approval_prompt/1,
    validate_response/2
]).

%% Configuration
-export([
    get_claude_command/0,
    set_claude_command/1
]).

%%====================================================================
%% Types
%%====================================================================

-type session_id() :: binary().
-type json_schema() :: map().
-type prompt_context() :: map().
-type bridge_result() :: {ok, map()} | {error, term()}.
-type session_result() :: {ok, session_id()} | {error, term()}.


-export_type([session_id/0, json_schema/0, prompt_context/0]).

%%====================================================================
%% API Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Executes a Claude Code prompt and gets JSON response.
%%
%% Uses Claude Code headless mode with the specified prompt and
%% optional JSON schema for response validation.
%%
%% @param Prompt The prompt text to send to Claude.
%% @param Schema The JSON schema for response validation.
%% @return {ok, ResponseMap} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec prompt_claude(binary(), json_schema()) -> bridge_result().

prompt_claude(Prompt, Schema) ->
    prompt_claude(Prompt, Schema, #{}).

%%--------------------------------------------------------------------
%% @doc Executes a Claude Code prompt with options.
%%
%% Available options:
%%   - timeout: milliseconds (default: 30000)
%%   - allowed_tools: list of allowed tool patterns
%%   - max_tokens: maximum response tokens (default: 4096)
%%   - model: claude model to use (default: auto)
%%
%% @param Prompt The prompt text.
%% @param Schema The JSON schema for validation.
%% @param Options Additional options.
%% @return {ok, ResponseMap} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec prompt_claude(binary(), json_schema(), map()) -> bridge_result().

prompt_claude(Prompt, Schema, Options) ->
    %% Get Claude command from persistent config
    ClaudeCmd = get_claude_command(),

    %% Build command arguments
    Timeout = maps:get(timeout, Options, 30000),
    AllowedTools = maps:get(allowed_tools, Options, []),
    MaxTokens = maps:get(max_tokens, Options, 4096),
    Model = maps:get(model, Options, auto),

    %% Build the command
    SchemaFile = case Schema of
        #{<<"type">> := _} ->
            TempFile = "/tmp/claude_schema_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".json",
            ok = file:write_file(TempFile, jsone:encode(Schema)),
            TempFile;
        _ ->
            undefined
    end,

    %% Construct command arguments
    BaseArgs = [
        "-p", escape_prompt(Prompt),
        "--output-format", "json"
    ],

    SchemaArgs = case SchemaFile of
        undefined -> [];
        _ -> ["--json-schema", SchemaFile]
    end,

    ToolArgs = case AllowedTools of
        [] -> [];
        _ -> ["--allowed-tools", string:join(AllowedTools, ",")]
    end,

    ModelArgs = case Model of
        auto -> [];
        _ -> ["--model", atom_to_list(Model)]
    end,

    TokenArgs = ["--max-tokens", integer_to_list(MaxTokens)],

    AllArgs = lists:append([BaseArgs, SchemaArgs, ToolArgs, ModelArgs, TokenArgs]),

    %% Execute command
    FullCmd = string:join([binary_to_list(ClaudeCmd) | AllArgs], " "),

    try
        Result = os:cmd(FullCmd),
        %% Clean up schema file if created
        case SchemaFile of
            undefined -> ok;
            _ -> file:delete(SchemaFile)
        end,

        %% Parse JSON response
        case jsone:decode(list_to_binary(Result)) of
            {ok, JsonMap} when is_map(JsonMap) ->
                case validate_response(JsonMap, Schema) of
                    ok -> {ok, JsonMap};
                    {error, Reason} -> {error, {validation_failed, Reason}}
                end;
            {error, Reason} ->
                {error, {json_decode_error, Reason}};
            JsonMap when is_map(JsonMap) ->
                case validate_response(JsonMap, Schema) of
                    ok -> {ok, JsonMap};
                    {error, Reason} -> {error, {validation_failed, Reason}}
                end
        end
    catch
        _:Error ->
            case SchemaFile of
                undefined -> ok;
                _ -> file:delete(SchemaFile)
            end,
            {error, {execution_error, Error}}
    end.

%%--------------------------------------------------------------------
%% @doc Starts a new Claude Code approval session.
%%
%% Sessions support multi-turn conversations with context persistence.
%%
%% @param Context Initial session context.
%% @return {ok, SessionId} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec start_session(map()) -> session_result().

start_session(Context) ->
    SessionId = generate_session_id(),
    Session = #claude_session{
        session_id = SessionId,
        started_at = erlang:system_time(millisecond),
        last_activity = erlang:system_time(millisecond),
        message_count = 0,
        context = Context
    },

    %% Store session in ETS
    Table = ensure_session_table(),
    ets:insert(Table, {SessionId, Session}),

    {ok, SessionId}.

%%--------------------------------------------------------------------
%% @doc Continues an existing session with a new prompt.
%%
%% @param SessionId The session ID.
%% @param Prompt The continuation prompt.
%% @return {ok, Response} or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec continue_session(session_id(), binary()) -> bridge_result().

continue_session(SessionId, Prompt) ->
    Table = ensure_session_table(),
    case ets:lookup(Table, SessionId) of
        [{SessionId, #claude_session{} = Session}] ->
            %% Update session
            UpdatedSession = Session#claude_session{
                last_activity = erlang:system_time(millisecond),
                message_count = Session#claude_session.message_count + 1
            },
            ets:insert(Table, {SessionId, UpdatedSession}),

            %% Include session context in prompt
            ContextPrompt = add_context_to_prompt(Prompt, UpdatedSession#claude_session.context),

            %% Use basic schema for continuation
            DefaultSchema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => #{
                    <<"response">> => #{<<"type">> => <<"string">>}
                }
            },

            prompt_claude(ContextPrompt, DefaultSchema, #{});
        [] ->
            {error, session_not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Ends a Claude Code session.
%%
%% @param SessionId The session ID to end.
%% @return ok or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec end_session(session_id()) -> ok | {error, term()}.

end_session(SessionId) ->
    Table = ensure_session_table(),
    case ets:lookup(Table, SessionId) of
        [{SessionId, _Session}] ->
            ets:delete(Table, SessionId),
            ok;
        [] ->
            {error, session_not_found}
    end.

%%--------------------------------------------------------------------
%% @doc Generates an approval prompt from a checkpoint.
%%
%% @param Checkpoint The approval checkpoint record.
%% @return Formatted prompt binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec generate_approval_prompt(#approval_checkpoint{}) -> binary().

generate_approval_prompt(Checkpoint) ->
    StepName = atom_to_binary(Checkpoint#approval_checkpoint.step_name, utf8),
    PatternId = case Checkpoint#approval_checkpoint.pattern_id of
        undefined -> <<"unknown">>;
        P -> to_binary(P)
    end,

    ContextJson = case Checkpoint#approval_checkpoint.context of
        #{<<>> := _} = C -> jsone:encode(C);
        _ when is_map(Checkpoint#approval_checkpoint.context) ->
            jsone:encode(Checkpoint#approval_checkpoint.context);
        _ -> <<"{}">>
    end,

    Prompt = <<
        "You are an approval reviewer for a workflow automation system.\n\n"
        "Workflow Step: ", StepName/binary, "\n"
        "Pattern ID: ", PatternId/binary, "\n"
        "Created At: ", (integer_to_binary(Checkpoint#approval_checkpoint.created_at))/binary, "\n\n"
        "Context Data:\n",
        ContextJson/binary, "\n\n"
        "Please review this workflow step and provide your decision.\n"
        "Consider:\n"
        "1. Is this step safe to execute?\n"
        "2. Are there any potential issues with the context data?\n"
        "3. Does this step align with workflow best practices?\n\n"
        "Respond with a JSON object containing:\n"
        "- approved (boolean): true to approve, false to deny\n"
        "- reason (string): your reasoning for the decision\n"
    >>,

    Prompt.

%%--------------------------------------------------------------------
%% @doc Validates a response against a JSON schema.
%%
%% @param Response The response map to validate.
%% @param Schema The JSON schema.
%% @return ok or {error, Reason}
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_response(map(), json_schema()) -> ok | {error, term()}.

validate_response(Response, Schema) ->
    case Schema of
        #{<<"type">> := <<"object">>} ->
            case is_map(Response) of
                false -> {error, not_an_object};
                true -> validate_properties(Response, Schema)
            end;
        #{<<"type">> := <<"array">>} ->
            case is_list(Response) of
                false -> {error, not_an_array};
                true -> ok
            end;
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @doc Gets the Claude Code command to use.
%%
%% @return The Claude command binary.
%%
%% @end
%%--------------------------------------------------------------------
-spec get_claude_command() -> binary().

get_claude_command() ->
    case persistent_term:get({?MODULE, claude_command}, undefined) of
        undefined ->
            %% Try to find claude in PATH
            case os:find_executable("claude") of
                false -> <<"claude">>;  % Default, will fail if not found
                Path -> list_to_binary(Path)
            end;
        Cmd -> Cmd
    end.

%%--------------------------------------------------------------------
%% @doc Sets the Claude Code command to use.
%%
%% @param Command The command (full path or executable name).
%% @return ok
%%
%% @end
%%--------------------------------------------------------------------
-spec set_claude_command(binary() | string()) -> ok.

set_claude_command(Command) ->
    Cmd = to_binary(Command),
    persistent_term:put({?MODULE, claude_command}, Cmd),
    ok.

%%====================================================================
%% Internal Functions
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Validates response properties against schema.
%%
%% @end
%%--------------------------------------------------------------------
validate_properties(Response, Schema) ->
    case Schema of
        #{<<"properties">> := Props} when is_map(Props) ->
            Required = maps:get(<<"required">>, Schema, []),
            lists:foldl(fun(Prop, Acc) ->
                case Acc of
                    {error, _} -> Acc;
                    ok ->
                        case maps:is_key(Prop, Response) of
                            false -> {error, {missing_property, Prop}};
                            true -> ok
                        end
                end
            end, ok, Required);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Adds session context to a prompt.
%%
%% @end
%%--------------------------------------------------------------------
add_context_to_prompt(Prompt, Context) when is_map(Context) ->
    case maps:size(Context) of
        0 -> Prompt;
        _ ->
            ContextStr = jsone:encode(Context),
            <<Prompt/binary,
              "\n\n--- Session Context ---\n",
              ContextStr/binary,
              "\n-----------------------\n">>
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Escapes a prompt for shell execution.
%%
%% @end
%%--------------------------------------------------------------------
escape_prompt(Prompt) ->
    %% Simple escaping - wrap in single quotes and escape single quotes
    PromptStr = binary_to_list(Prompt),
    Escaped = lists:map(fun($') -> "'\\''";
                           (C) -> [C]
                        end, PromptStr),
    lists:flatten(["'" | Escaped] ++ ["'"]).

%%--------------------------------------------------------------------
%% @private
%% @doc Generates a unique session ID.
%%
%% @end
%%--------------------------------------------------------------------
generate_session_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"claude_session_", Hex/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc Ensures the session ETS table exists.
%%
%% @end
%%--------------------------------------------------------------------
ensure_session_table() ->
    TableName = ?MODULE,
    case ets:whereis(TableName) of
        undefined ->
            ets:new(TableName, [named_table, public, {read_concurrency, true}]);
        Table ->
            Table
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Converts a term to binary.
%%
%% @end
%%--------------------------------------------------------------------
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(_) -> <<"">>.
