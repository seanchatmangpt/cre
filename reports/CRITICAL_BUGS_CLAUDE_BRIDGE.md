# CRITICAL BUGS: YAWL Claude Bridge (yawl_claude_bridge.erl)

**Review Date**: 2025-02-05
**Reviewer**: Hostile Code Review - External Integration Specialist
**Severity**: DEMO-KILLING - These bugs WILL break your demo
**File**: `/Users/sac/cre/src/yawl_claude_bridge.erl`
**Lines of Code**: 497

---

## Executive Summary

**DEMO WILL FAIL**. This integration has **23 critical bugs** including:
- 8 **DEMO-BREAKING** bugs that will cause visible failures during presentation
- 3 **SECURITY VULNERABILITIES** including command injection
- 12 **DATA LOSS** and crash scenarios
- Complete lack of timeout handling, input validation, and error recovery

**ESTIMATE**: 40-60 hours of work to fix all critical issues properly.

---

## 🔴 CRITICAL: Demo-Breaking Bugs (8)

### BUG #1: TIMEOUT COMPLETELY IGNORED
**Lines**: 132, 176
**Severity**: 🔴 DEMO-KILLER
**Impact**: Demo FREEZES if Claude hangs

```erlang
%% Line 132: Timeout is extracted from options
Timeout = maps:get(timeout, Options, 30000),

%% Line 176: BUT NEVER USED!
Result = os:cmd(FullCmd),  % Blocks FOREVER if claude hangs
```

**Problem**: `os:cmd/1` is synchronous and has NO timeout parameter. If Claude CLI hangs (network issue, API problem, etc.), the entire demo freezes with no error message, no recovery, and no way to cancel.

**What happens in demo**:
1. Presenter runs workflow
2. Claude CLI hangs (e.g., API rate limit)
3. Demo freezes at 30 seconds, then 1 minute, then 2 minutes
4. Audience waits awkwardly
5. Presenter has to kill the Erlang VM
6. Demo ruined

**Fix**: Use `open_port({spawn, Cmd}, [exit_status, binary])` with selective receive and timeout.

---

### BUG #2: NO CLAUDE CLI VERSION CHECK
**Lines**: 372-381
**Severity**: 🔴 DEMO-KILLER
**Impact**: Immediate failure on wrong version

```erlang
get_claude_command() ->
    case persistent_term:get({?MODULE, claude_command}, undefined) of
        undefined ->
            case os:find_executable("claude") of
                false -> <<"claude">>;  % Will FAIL SILENTLY
                Path -> list_to_binary(Path)
            end;
        Cmd -> Cmd
    end.
```

**Problems**:
1. Falls back to `"claude"` if not found - fails with cryptic error
2. **No version check** - Claude Code 1.x doesn't have `--json-schema` flag
3. No validation that it's actually Claude Code CLI (could be any `claude` command)

**What happens in demo**:
1. Presenter's machine has claude 1.x installed
2. First approval prompt runs
3. Error: `Unknown option: --json-schema`
4. Confusion: "It worked on my laptop!"
5. Demo breaks immediately

**Fix**: Run `claude --version`, parse output, ensure it's 2.x+, fail fast with clear error.

---

### BUG #3: NEWLINE INJECTION - FLAG INJECTION
**Lines**: 449-458, 173
**Severity**: 🔴 DEMO-KILLER
**Impact**: Prompt can inject CLI flags

```erlang
escape_prompt(Prompt) ->
    %% Only escapes single quotes - NOT newlines!
    PromptStr = binary_to_list(Prompt),
    lists:flatten([
        "'",
        lists:map(fun($') -> "'\\''"; (C) -> C end, PromptStr),
        "'"
    ]).

%% Line 173: Command construction
FullCmd = string:join([binary_to_list(ClaudeCmd) | AllArgs], " "),
```

**Attack Vector**:
```erlang
%% Malicious prompt:
Prompt = <<"Approve?\n--output-format text\nYes">>,

%% Becomes:
claude -p 'Approve?
--output-format text
Yes' --output-format json

%% Result: --output-format text OVERRIDES correct flag!
%% Claude returns plain text, not JSON
%% jsone:decode fails with {error, invalid_json}
```

**What happens in demo**:
1. Workflow prompt contains newline (user input, file content, etc.)
2. Prompt injection changes Claude CLI behavior
3. Returns wrong format or executes wrong command
4. Cryptic JSON decode error
5. Presenter has no idea what went wrong

**Fix**: Replace newlines with `\n` literal, or use proper shell escaping (replace `\n` with `\\n`), or use argument list API instead of shell string.

---

### BUG #4: EMPTY OUTPUT NOT HANDLED
**Lines**: 176, 184-196
**Severity**: 🔴 DEMO-KILLER
**Impact**: Crashes with cryptic error when Claude fails

```erlang
%% Line 176: What if claude crashes and returns empty string?
Result = os:cmd(FullCmd),  % Could be ""

%% Line 184:
case jsone:decode(list_to_binary(Result)) of
    {ok, JsonMap} when is_map(JsonMap) ->
        ...
end.
```

**Problem**: If `Result = ""`, then:
- `jsone:decode(<<>>) = {error, invalid_json}`
- Error message: `{json_decode_error, invalid_json}`
- **No indication** that Claude CLI crashed!
- Could be: Claude not installed, API key missing, network down, etc.

**What happens in demo**:
1. Claude CLI crashes (e.g., missing API key)
2. Returns empty output
3. Error: "Invalid JSON"
4. Presenter spends 10 minutes debugging JSON parsing
5. Actual problem: Missing API key (never detected!)

**Fix**: Check if Result is empty before JSON parsing. If empty, run claude CLI with `--help` to detect if it's installed. Return helpful error.

---

### BUG #5: SESSION MEMORY LEAK
**Lines**: 219-233, 477-484
**Severity**: 🔴 DEMO-KILLER (slow)
**Impact**: Demo VM runs out of RAM after extended use

```erlang
start_session(Context) ->
    SessionId = generate_session_id(),
    Session = #claude_session{
        session_id = SessionId,
        started_at = erlang:system_time(millisecond),
        last_activity = erlang:system_time(millisecond),
        message_count = 0,
        context = Context
    },
    Table = ensure_session_table(),
    ets:insert(Table, {SessionId, Session}),  % NEVER DELETED!
    {ok, SessionId}.
```

**Problems**:
1. Sessions created but **NEVER cleaned up**
2. No TTL expiration
3. No max session limit
4. `end_session/1` is optional - if not called, session leaks forever

**What happens in demo**:
1. Short demo: No problem
2. Extended demo (30+ minutes): 1000+ sessions created
3. Each session holds context (could be large maps)
4. Memory usage grows: 100MB → 500MB → 2GB → crash
5. Demo VM runs out of RAM, Erlang VM killed by OOM killer

**Fix**: Add TTL to sessions (e.g., 1 hour), cleanup expired sessions on every 10th session creation, add max session limit with LRU eviction.

---

### BUG #6: TEMP FILE LEAK
**Lines**: 140-141, 180, 202
**Severity**: 🔴 DEMO-KILLER (slow)
**Impact**: /tmp fills up, demo stops working

```erlang
%% Line 140-141: Create temp file
TempFile = "/tmp/claude_schema_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".json",
ok = file:write_file(TempFile, jsone:encode(Schema)),

%% Line 180: Cleanup - but failure is IGNORED!
case SchemaFile of
    undefined -> ok;
    _ -> file:delete(SchemaFile)  % Returns ok | {error, Reason} - IGNORED!
end
```

**Problems**:
1. `file:delete/1` can fail (permission denied, file locked, Windows)
2. Return value is **ignored** - no retry, no logging
3. If disk is full, `file:write_file/2` fails but temp file name is already used
4. `/tmp` accumulates `claude_schema_*.json` files forever

**What happens in demo**:
1. Every approval prompt creates a temp file
2. Some deletes fail (e.g., file locked on Windows, permission issue)
3. After 1000 approvals: 1000 orphaned temp files
4. `/tmp` partition fills up
5. Error: "No space left on device"
6. Demo stops working

**Fix**: Check return value of `file:delete/1`, log errors, use unique temp directory per session, cleanup all temp files on startup, use `mktemp` with O_EXCL flag.

---

### BUG #7: BACKTICK COMMAND INJECTION
**Lines**: 449-458, 173
**Severity**: 🔴 SECURITY VULNERABILITY
**Impact**: **SYSTEM COMPROMISE** - Backticks execute commands!

```erlang
escape_prompt(Prompt) ->
    %% Escapes single quotes ONLY - NOT backticks!
    lists:map(fun($') -> "'\\''"; (C) -> C end, PromptStr).

%% If prompt contains backticks:
Prompt = <<"Review this: `rm -rf /`">>,

%% Escaped to: 'Review this: `rm -rf /`'
%% Shell executes: rm -rf / when parsing command!
```

**Attack Demo**:
```bash
# What the Erlang code executes:
claude -p 'Review this: `rm -rf /`' --output-format json

# Shell parses this as:
claude -p 'Review this: ' + $(rm -rf /) + ' --output-format json'

# Result: rm -rf / EXECUTES!
```

**Impact**:
- If run as root: **SYSTEM WIPED**
- If Claude has special privileges: **PRIVILEGE ESCALATION**
- User input directly controls shell execution: **ARBITRARY CODE EXECUTION**

**What happens in demo**:
1. Worst case: Presenter's machine is wiped
2. More likely: Demo data files deleted
3. Or: Environment variables changed, breaking demo
4. Chaos: "Why is my laptop acting weird?"

**Fix**: Escape backticks as `\``, or better: use argument list API (not shell string), or use base64 encoding: `echo "$PROMPT" | claude -p -`

---

### BUG #8: RACE CONDITION IN SESSION UPDATE
**Lines**: 248-255
**Severity**: 🔴 DEMO-KILLER
**Impact**: Session state corrupted in concurrent workflows

```erlang
continue_session(SessionId, Prompt) ->
    case ets:lookup(Table, SessionId) of
        [{SessionId, #claude_session{} = Session}] ->
            %% NOT ATOMIC!
            UpdatedSession = Session#claude_session{
                last_activity = erlang:system_time(millisecond),
                message_count = Session#claude_session.message_count + 1
            },
            ets:insert(Table, {SessionId, UpdatedSession}),
```

**Race Condition**:
```
Time  Process A (prompt 1)           Process B (prompt 2)
----  ---------------------------    ---------------------------
T1    lookup: message_count=0
T2                                 lookup: message_count=0
T3    update: message_count=1
T4                                 update: message_count=1 (WRONG!)
T5    insert
T6                                 insert (OVERWRITES A!)
```

**Result**: `message_count` is 1 instead of 2. Session tracking is corrupted.

**What happens in demo**:
1. Two workflow branches call `continue_session/2` concurrently
2. Both read same `message_count`
3. Both increment to same value
4. Session state corrupted
5. Workflow logic that depends on `message_count` breaks
6. Demo shows wrong/approval count

**Fix**: Use `ets:update_counter/3` for atomic increment:
```erlang
ets:update_counter(Table, SessionId, {#claude_session.message_count, 1})
```

---

## 🔴 CRITICAL: Security Vulnerabilities (3)

### BUG #9: DOLLAR SIGN SUBSHELL INJECTION
**Lines**: 449-458
**Severity**: 🔴 SECURITY
**Impact**: Arbitrary command execution

```erlang
%% Dollar signs not escaped!
Prompt = <<"Price: $(whoami)">>,

%% Shell executes:
claude -p 'Price: $(whoami)' --output-format json
%% → $(whoami) is replaced with username!
```

**Fix**: Escape `$` as `\$` or use argument list API.

---

### BUG #10: PIPE INJECTION
**Lines**: 449-458
**Severity**: 🔴 SECURITY
**Impact**: Command chaining, data exfiltration

```erlang
%% Pipes not escaped!
Prompt = <<"Check: cat /etc/passwd | grep root">>,

%% Shell executes:
claude -p 'Check: cat /etc/passwd | grep root' --output-format json
%% → grep root is executed! Output leaked!
```

**Fix**: Escape `|` as `\|` or use argument list API.

---

### BUG #11: PATH TRAVERSAL IN SCHEMA FILE
**Lines**: 140, 155
**Severity**: 🔴 SECURITY
**Impact**: Read arbitrary files, inject malicious schemas

```erlang
%% Line 140: TempFile is constructed
TempFile = "/tmp/claude_schema_" ++ integer_to_list(erlang:unique_integer([positive])) ++ ".json",

%% Line 155: Used directly in command
["--json-schema", SchemaFile]

%% BUT: What if Schema contains malicious data before encoding?
%% Schema = #{<<"file">> => "../../../etc/passwd">>},
%% SchemaFile = "../../../etc/passwd" (if manipulated)
%% → claude --json-schema ../../../etc/passwd
%% → Reads /etc/passwd!
```

**Fix**: Validate SchemaFile path doesn't contain `..`, use absolute paths only.

---

## 🔴 CRITICAL: Data Loss & Crash Bugs (12)

### BUG #12: validate_response/2 IS USELESS
**Lines**: 347-361
**Severity**: 🟡 DATA CORRUPTION
**Impact**: Invalid responses accepted as valid

```erlang
validate_response(Response, Schema) ->
    case Schema of
        #{<<"type">> := <<"object">>} ->
            case is_map(Response) of
                false -> {error, not_an_object};
                true -> validate_properties(Response, Schema)  % Checks required fields only
            end;
        ...
    end.

validate_properties(Response, Schema) ->
    #{<<"properties">> := Props} when is_map(Props) ->
        Required = maps:get(<<"required">>, Schema, []),  % Gets required list
        %% ONLY checks if required keys exist - NOT types, not values!
        ...
    end.
```

**Problems**:
1. Doesn't validate property **types** (e.g., "approved" should be boolean)
2. Doesn't validate property **values** (e.g., "status" should be one of: pending, approved, denied)
3. Doesn't validate **nested objects** or arrays
4. Ignores Schema except for "required" field

**Impact**: Claude can return garbage and it's accepted as valid:
```erlang
%% Schema expects: #{<<"approved">> => boolean(), <<"reason">> => binary()}
%% But Claude returns: #{<<"approved">> => <<"yes">>, <<"reason">> => 123}
%% validate_response/2 says: ok (both keys present!)
%% Later code crashes when it expects boolean but gets binary!
```

**Fix**: Implement proper JSON Schema validation (use jesse library or similar).

---

### BUG #13: NO VALIDATION OF PROMPT INPUT
**Lines**: 127
**Severity**: 🟡 CRASH RISK
**Impact**: Empty prompts, DOS attacks

```erlang
-spec prompt_claude(binary(), json_schema(), map()) -> bridge_result().
prompt_claude(Prompt, Schema, Options) ->
    %% No validation of Prompt!
    %% Could be:
    %% - <<>> (empty binary)
    %% - 100MB binary (DOS attack)
    %% - Invalid UTF-8
```

**Fix**: Validate Prompt is non-empty, reasonable size (<1MB), valid UTF-8.

---

### BUG #14: SCHEMA VALIDATION IS WEAK
**Lines**: 138-145
**Severity**: 🟡 DATA CORRUPTION
**Impact**: Invalid schemas accepted

```erlang
SchemaFile = case Schema of
    #{<<"type">> := _} ->  % ANY map with "type" key passes!
        ...
    end.
```

**Accepts**:
- `#{<<"type">> => <<"garbage">>}` - not a valid JSON Schema type
- `#{<<"type">> => 123}` - not a string
- `#{<<"type">> => <<"object">>}` - but missing "properties"

**Fix**: Validate against JSON Schema meta-schema or use schema validation library.

---

### BUG #15: DEAD CODE IN JSON PARSING
**Lines**: 184-196
**Severity**: 🟢 CODE QUALITY
**Impact**: Confusing logic, impossible to reach

```erlang
case jsone:decode(list_to_binary(Result)) of
    {ok, JsonMap} when is_map(JsonMap) ->
        ...;
    {error, Reason} ->
        ...;
    JsonMap when is_map(JsonMap) ->  % NEVER REACHED!
        ...
end.
```

**Problem**: `jsone:decode/1` ALWAYS returns `{ok, Term}` or `{error, Reason}`. The third clause `JsonMap when is_map(JsonMap)` is **dead code**.

**Fix**: Remove dead code clause.

---

### BUG #16: NO ERROR CONTEXT PRESERVED
**Lines**: 190-191
**Severity**: 🟡 DEBUGGING HELL
**Impact**: Impossible to debug JSON errors

```erlang
{error, Reason} ->
    {error, {json_decode_error, Reason}}  % Lost the actual JSON!
```

**Problem**: When JSON parsing fails, you don't see WHAT failed to parse. Just generic "invalid JSON" error.

**Fix**: Include snippet of invalid JSON: `{error, {json_decode_error, Reason, binary:part(Result, 0, 100)}}`

---

### BUG #17: to_binary/1 IS DANGEROUS
**Lines**: 492-496
**Severity**: 🟡 DATA LOSS
**Impact**: Silently converts complex terms to empty binary

```erlang
to_binary(B) when is_binary(B) -> B;
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(L) when is_list(L) -> list_to_binary(L);  % No UTF-8 validation!
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(_) -> <<"">>.  % Floats, tuples, maps → empty binary!
```

**Loses data**:
- `to_binary(3.14) → <<""`  (float lost)
- `to_binary({a, b}) → <<""`  (tuple lost)
- `to_binary(#{<<"a">> => 1}) → <<""`  (map lost)

**Fix**: Add clauses for floats, tuples, maps, or throw error for unsupported types.

---

### BUG #18: PATTERNID TO_BINARY IS UNSAFE
**Lines**: 305-308
**Severity**: 🟡 DATA LOSS
**Impact**: Empty pattern_id confuses Claude

```erlang
PatternId = case Checkpoint#approval_checkpoint.pattern_id of
    undefined -> <<"unknown">>;
    P -> to_binary(P)  % If P is complex term → <<"">>!
end.
```

**Fix**: Validate `pattern_id` is simple type (binary, atom, integer, string list).

---

### BUG #19: CONTEXTJSON ENCODING CAN FAIL
**Lines**: 310-315
**Severity**: 🟡 CRASH RISK
**Impact**: Unhandled exception crashes process

```erlang
ContextJson = case Checkpoint#approval_checkpoint.context of
    #{<<>> := _} = C -> jsone:encode(C);  % Weird pattern!
    _ when is_map(Checkpoint#approval_checkpoint.context) ->
        jsone:encode(Checkpoint#approval_checkpoint.context);  % Can throw!
    _ -> <<"{}">>
end.
```

**Problems**:
1. First pattern: `#{<<>> := _} = C` matches maps with **empty binary key** (weird, probably a bug - should be just `is_map(C)`)
2. `jsone:encode/1` throws exception if context contains unencodable terms (pids, refs, functions, ports)
3. No `try...catch` to handle encoding errors

**Fix**: Wrap `jsone:encode/1` in `try...catch`, filter out unencodable terms before encoding.

---

### BUG #20: ADD_CONTEXT_TO_PROMPT CAN CREATE MASSIVE PROMPTS
**Lines**: 432-441
**Severity**: 🟡 DOS RISK
**Impact**: Claude API call fails or hangs

```erlang
add_context_to_prompt(Prompt, Context) when is_map(Context) ->
    case maps:size(Context) of
        0 -> Prompt;
        _ ->
            ContextStr = jsone:encode(Context),  % Could be 10MB!
            <<Prompt/binary,
              "\n\n--- Session Context ---\n",
              ContextStr/binary,  % No size checking!
              "\n-----------------------\n">>
    end.
```

**Problem**: If Context is large (e.g., 10MB map), prompt becomes 10MB+, which:
- Exceeds Claude's token limits (200k tokens ~ 1.5MB for Sonnet)
- Causes `os:cmd/1` to hang or fail
- Wastes API quota

**Fix**: Check size before adding context, truncate large contexts, or exclude large keys.

---

### BUG #21: NO VALIDATION OF ALLOWED_TOOLS FORMAT
**Lines**: 160
**Severity**: 🟡 CRASH RISK
**Impact**: String operations crash on non-strings

```erlang
ToolArgs = case AllowedTools of
    [] -> [];
    _ -> ["--allowed-tools", string:join(AllowedTools, ",")]  % Assumes list of strings!
end.
```

**Problem**: If `AllowedTools = [123, {a}, #{<<"a">> => 1}]`, then `string:join/2` crashes with `badarg`.

**Fix**: Validate `AllowedTools` is a list of strings/binaries.

---

### BUG #22: SESSION ID COLLISION POSSIBLE
**Lines**: 467
**Severity**: 🟡 DATA CORRUPTION
**Impact**: Two sessions get same ID, state corrupted

```erlang
generate_session_id() ->
    Unique = crypto:hash(md5, term_to_binary({self(), erlang:timestamp()})),
    Hex = binary:encode_hex(Unique),
    <<"claude_session_", Hex/binary>>.
```

**Problems**:
1. `erlang:timestamp/0` is **DEPRECATED** since OTP 18 (use `erlang:system_time/1`)
2. MD5 is **NOT cryptographically secure** for session IDs (collision possible)
3. If two processes create sessions at the exact same microsecond, collision possible

**Fix**: Use `crypto:strong_rand_bytes/1` or `erlang:unique_integer([positive, monotonic])` + SHA-256.

---

### BUG #23: ETS TABLE ORPHANED ON PROCESS DEATH
**Lines**: 477-484
**Severity**: 🟡 RESOURCE LEAK
**Impact**: Orphaned ETS table consumes memory

```erlang
ensure_session_table() ->
    TableName = ?MODULE,
    case ets:whereis(TableName) of
        undefined ->
            ets:new(TableName, [named_table, public, {read_concurrency, true}]);
            %% MISSING: {heir, none, Pid} or owner death handling!
        Table ->
            Table
    end.
```

**Problem**: If process that created ETS table dies, table becomes **orphaned** (still exists but no owner). Can't be deleted normally.

**Fix**: Add `{heir, none, Pid}` or use `ets:give_away/3` before process terminates.

---

## Summary Statistics

| Category | Count | Lines |
|----------|-------|-------|
| Demo-Breaking Bugs | 8 | 132, 176, 372-381, 449-458, 140, 248-255, 219-233 |
| Security Vulnerabilities | 3 | 449-458, 140, 155 |
| Data Loss / Crash Bugs | 12 | 347-361, 127, 138-145, 492-496, etc. |
| **TOTAL CRITICAL BUGS** | **23** | **Throughout file** |

---

## Recommended Fixes (Priority Order)

### P0 - MUST FIX FOR DEMO (8-16 hours)
1. **Fix timeout handling** (BUG #1) - Use `open_port` with timeout
2. **Add Claude version check** (BUG #2) - Fail fast with clear error
3. **Fix newline injection** (BUG #3) - Proper shell escaping
4. **Handle empty output** (BUG #4) - Detect Claude crashes
5. **Fix backtick injection** (BUG #7) - Escape backticks or use arg list
6. **Fix session memory leak** (BUG #5) - Add TTL expiration
7. **Fix temp file leak** (BUG #6) - Check delete return value
8. **Fix race condition** (BUG #8) - Use `ets:update_counter/3`

### P1 - SECURITY FIXES (4-8 hours)
9. Escape dollar signs (BUG #9)
10. Escape pipes (BUG #10)
11. Validate schema file paths (BUG #11)

### P2 - DATA INTEGRITY FIXES (8-16 hours)
12. Implement proper JSON Schema validation (BUG #12)
13. Add prompt validation (BUG #13)
14. Improve schema validation (BUG #14)
15. Fix to_binary/1 data loss (BUG #17)
16. Fix context encoding errors (BUG #19)

### P3 - CODE QUALITY (4-8 hours)
17. Remove dead code (BUG #15)
18. Add error context (BUG #16)
19. Fix session ID generation (BUG #22)
20. Fix ETS table ownership (BUG #23)

---

## Testing Recommendations

Before demo, test these scenarios:

1. **Claude not installed**: Should fail with clear error
2. **Claude 1.x installed**: Should fail with "Need Claude 2.x+" message
3. **Claude hangs**: Should timeout after 30s with clear error
4. **Prompt with newlines**: Should not inject flags
5. **Prompt with backticks**: Should not execute commands
6. **Empty Claude output**: Should detect and report crash
7. **1000 session creations**: Should not leak memory
8. **Concurrent session updates**: Should not corrupt state
9. **Full /tmp disk**: Should handle temp file creation failure
10. **Malicious schema**: Should validate and reject

---

## Conclusion

**This code is NOT ready for demo.** It has 8 demo-breaking bugs that WILL cause visible failures, plus 3 security vulnerabilities that could compromise the system.

**ESTIMATED FIX TIME**: 24-48 hours for critical (P0 + P1) bugs.

**RECOMMENDATION**: Do NOT use this code in demo until at least P0 bugs are fixed. Consider using a simpler integration (e.g., direct API call instead of CLI wrapper) for demo purposes.

---

**Reviewed by**: Hostile Code Review Agent
**Date**: 2025-02-05
**Severity**: 🔴 DEMO-KILLING - DO NOT DEPLOY
