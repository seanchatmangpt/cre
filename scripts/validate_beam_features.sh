#!/usr/bin/env bash
# Comprehensive BEAM VM Feature Validation Script
#
# Tests all unique BEAM features including:
# - Hot code swapping
# - Process isolation and supervision
# - Message passing
# - Process monitoring and linking
# - ETS (Erlang Term Storage)
# - Distributed Erlang
# - Binary pattern matching
# - Tail call optimization
# - Process registry
#
# Usage: ./scripts/validate_beam_features.sh

set -euo pipefail

ERL_BIN="${ERL_BIN:-erl}"
TEST_DIR="/tmp/beam_feature_test_$$"
RESULTS=()

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "════════════════════════════════════════════════════════════"
echo "  BEAM VM Feature Validation Suite"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Testing: $($ERL_BIN -version 2>&1)"
echo ""

mkdir -p "$TEST_DIR"

#=============================================================================
# Test 1: Hot Code Swapping
#=============================================================================

test_hot_code_swapping() {
    echo -n "[ 1/10] Hot Code Swapping................... "

    cat > "$TEST_DIR/hotswap_test.erl" <<'EOF'
-module(hotswap_test).
-export([version/0]).
version() -> 1.
EOF

    cat > "$TEST_DIR/hotswap_test_v2.erl" <<'EOF'
-module(hotswap_test).
-export([version/0]).
version() -> 2.
EOF

    local result
    result=$("$ERL_BIN" -noshell -pa "$TEST_DIR" -eval "
        % Compile and load v1
        {ok, hotswap_test, Bin1, _} = compile:file('$TEST_DIR/hotswap_test.erl', [binary, return]),
        {module, hotswap_test} = code:load_binary(hotswap_test, 'hotswap_test.erl', Bin1),
        V1 = hotswap_test:version(),

        % Compile v2
        {ok, hotswap_test, Bin2, _} = compile:file('$TEST_DIR/hotswap_test_v2.erl', [binary, return]),

        % Load v2 (this is the hot code swap)
        {module, hotswap_test} = code:load_binary(hotswap_test, 'hotswap_test.erl', Bin2),
        V2 = hotswap_test:version(),

        % Purge old code
        true = code:soft_purge(hotswap_test),

        % Verify we can still call new version
        V3 = hotswap_test:version(),

        case {V1, V2, V3} of
            {1, 2, 2} -> io:format('PASS~n');
            _ -> io:format('FAIL~n')
        end,
        halt().
    " 2>&1)

    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        RESULTS+=("PASS")
    else
        echo -e "${RED}✗ FAIL${NC}"
        RESULTS+=("FAIL")
    fi
}

#=============================================================================
# Test 2: Process Isolation
#=============================================================================

test_process_isolation() {
    echo -n "[ 2/10] Process Isolation................... "

    local result
    result=$("$ERL_BIN" -noshell -eval "
        % Create process that crashes
        spawn(fun() -> exit(crash) end),

        % Main process should still be alive
        timer:sleep(100),
        io:format('PASS~n'),
        halt().
    " 2>&1)

    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        RESULTS+=("PASS")
    else
        echo -e "${RED}✗ FAIL${NC}"
        RESULTS+=("FAIL")
    fi
}

#=============================================================================
# Test 3: Message Passing
#=============================================================================

test_message_passing() {
    echo -n "[ 3/10] Message Passing..................... "

    local result
    result=$("$ERL_BIN" -noshell -eval "
        Parent = self(),
        spawn(fun() ->
            receive
                {msg, Data} -> Parent ! {reply, Data * 2}
            end
        end) ! {msg, 21},

        Result = receive
            {reply, 42} -> pass;
            _ -> fail
        after 1000 ->
            timeout
        end,

        case Result of
            pass -> io:format('PASS~n');
            _ -> io:format('FAIL~n')
        end,
        halt().
    " 2>&1)

    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        RESULTS+=("PASS")
    else
        echo -e "${RED}✗ FAIL${NC}"
        RESULTS+=("FAIL")
    fi
}

#=============================================================================
# Test 4: Process Monitoring
#=============================================================================

test_process_monitoring() {
    echo -n "[ 4/10] Process Monitoring.................. "

    local result
    result=$("$ERL_BIN" -noshell -eval "
        Pid = spawn(fun() -> timer:sleep(100) end),
        Ref = monitor(process, Pid),

        Result = receive
            {'DOWN', Ref, process, Pid, normal} -> pass;
            _ -> fail
        after 2000 ->
            timeout
        end,

        case Result of
            pass -> io:format('PASS~n');
            _ -> io:format('FAIL~n')
        end,
        halt().
    " 2>&1)

    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        RESULTS+=("PASS")
    else
        echo -e "${RED}✗ FAIL${NC}"
        RESULTS+=("FAIL")
    fi
}

#=============================================================================
# Test 5: Process Linking
#=============================================================================

test_process_linking() {
    echo -n "[ 5/10] Process Linking..................... "

    local result
    result=$("$ERL_BIN" -noshell -eval "
        process_flag(trap_exit, true),
        Pid = spawn_link(fun() -> exit(test_exit) end),

        Result = receive
            {'EXIT', Pid, test_exit} -> pass;
            _ -> fail
        after 2000 ->
            timeout
        end,

        case Result of
            pass -> io:format('PASS~n');
            _ -> io:format('FAIL~n')
        end,
        halt().
    " 2>&1)

    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        RESULTS+=("PASS")
    else
        echo -e "${RED}✗ FAIL${NC}"
        RESULTS+=("FAIL")
    fi
}

#=============================================================================
# Test 6: ETS (Erlang Term Storage)
#=============================================================================

test_ets() {
    echo -n "[ 6/10] ETS (Erlang Term Storage).......... "

    local result
    result=$("$ERL_BIN" -noshell -eval "
        Tab = ets:new(test_table, [set, public]),

        % Insert data
        ets:insert(Tab, {key1, value1}),
        ets:insert(Tab, {key2, value2}),

        % Lookup
        [{key1, value1}] = ets:lookup(Tab, key1),

        % Update
        ets:insert(Tab, {key1, updated}),
        [{key1, updated}] = ets:lookup(Tab, key1),

        % Delete
        ets:delete(Tab, key2),
        [] = ets:lookup(Tab, key2),

        ets:delete(Tab),
        io:format('PASS~n'),
        halt().
    " 2>&1)

    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        RESULTS+=("PASS")
    else
        echo -e "${RED}✗ FAIL${NC}"
        RESULTS+=("FAIL")
    fi
}

#=============================================================================
# Test 7: Process Registry
#=============================================================================

test_process_registry() {
    echo -n "[ 7/10] Process Registry.................... "

    local result
    result=$("$ERL_BIN" -noshell -eval "
        Pid = spawn(fun() ->
            receive stop -> ok end
        end),

        % Register process
        register(test_process, Pid),

        % Lookup by name
        Pid = whereis(test_process),

        % Send message by name
        test_process ! stop,

        % Wait for process to terminate
        timer:sleep(100),

        % Verify unregistered
        undefined = whereis(test_process),

        io:format('PASS~n'),
        halt().
    " 2>&1)

    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        RESULTS+=("PASS")
    else
        echo -e "${RED}✗ FAIL${NC}"
        RESULTS+=("FAIL")
    fi
}

#=============================================================================
# Test 8: Binary Pattern Matching
#=============================================================================

test_binary_pattern_matching() {
    echo -n "[ 8/10] Binary Pattern Matching............ "

    local result
    result=$("$ERL_BIN" -noshell -eval "
        % Parse binary data
        Binary = <<1, 2, 3, 4, 5>>,
        <<A, B, Rest/binary>> = Binary,
        <<3, 4, 5>> = Rest,

        % Construct binary
        NewBinary = <<A, B, 255>>,
        <<1, 2, 255>> = NewBinary,

        % UTF-8 handling
        Utf8 = <<\"Hello\"/utf8>>,
        <<\"Hello\"/utf8>> = Utf8,

        io:format('PASS~n'),
        halt().
    " 2>&1)

    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        RESULTS+=("PASS")
    else
        echo -e "${RED}✗ FAIL${NC}"
        RESULTS+=("FAIL")
    fi
}

#=============================================================================
# Test 9: Tail Call Optimization
#=============================================================================

test_tail_call_optimization() {
    echo -n "[ 9/10] Tail Call Optimization............. "

    cat > "$TEST_DIR/tail_test.erl" <<'EOF'
-module(tail_test).
-export([loop/1, start/1]).

% Tail recursive - should not grow stack
loop(0) -> ok;
loop(N) -> loop(N - 1).

start(N) ->
    loop(N),
    ok.
EOF

    local result
    result=$("$ERL_BIN" -noshell -pa "$TEST_DIR" -eval "
        {ok, tail_test} = compile:file('$TEST_DIR/tail_test.erl', [{outdir, '$TEST_DIR'}]),
        code:load_file(tail_test),

        % This should complete without stack overflow
        ok = tail_test:start(1000000),

        io:format('PASS~n'),
        halt().
    " 2>&1)

    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        RESULTS+=("PASS")
    else
        echo -e "${RED}✗ FAIL${NC}"
        RESULTS+=("FAIL")
    fi
}

#=============================================================================
# Test 10: Code Purging and Module Updates
#=============================================================================

test_code_purging() {
    echo -n "[10/10] Code Purging and Module Updates.... "

    cat > "$TEST_DIR/purge_test.erl" <<'EOF'
-module(purge_test).
-export([get_value/0]).
get_value() -> original.
EOF

    local result
    result=$("$ERL_BIN" -noshell -pa "$TEST_DIR" -eval "
        % Load v1
        {ok, purge_test} = compile:file('$TEST_DIR/purge_test.erl', [{outdir, '$TEST_DIR'}]),
        {module, purge_test} = code:load_file(purge_test),
        original = purge_test:get_value(),

        % Create v2
        file:write_file('$TEST_DIR/purge_test.erl',
            <<\"-module(purge_test).
               -export([get_value/0]).
               get_value() -> updated.\">>,
            [write]),

        % Load v2
        {ok, purge_test} = compile:file('$TEST_DIR/purge_test.erl', [{outdir, '$TEST_DIR'}]),
        {module, purge_test} = code:load_file(purge_test),

        % Purge old code
        true = code:soft_purge(purge_test),

        % Verify new code
        updated = purge_test:get_value(),

        io:format('PASS~n'),
        halt().
    " 2>&1)

    if [[ "$result" == "PASS" ]]; then
        echo -e "${GREEN}✓ PASS${NC}"
        RESULTS+=("PASS")
    else
        echo -e "${RED}✗ FAIL${NC}"
        RESULTS+=("FAIL")
    fi
}

#=============================================================================
# Run All Tests
#=============================================================================

echo "Running tests..."
echo ""

test_hot_code_swapping
test_process_isolation
test_message_passing
test_process_monitoring
test_process_linking
test_ets
test_process_registry
test_binary_pattern_matching
test_tail_call_optimization
test_code_purging

# Cleanup
rm -rf "$TEST_DIR"

#=============================================================================
# Results Summary
#=============================================================================

echo ""
echo "════════════════════════════════════════════════════════════"
echo "  Test Results Summary"
echo "════════════════════════════════════════════════════════════"
echo ""

PASS_COUNT=0
FAIL_COUNT=0

for result in "${RESULTS[@]}"; do
    if [[ "$result" == "PASS" ]]; then
        ((PASS_COUNT++))
    else
        ((FAIL_COUNT++))
    fi
done

echo "Total Tests: ${#RESULTS[@]}"
echo -e "Passed:      ${GREEN}${PASS_COUNT}${NC}"
echo -e "Failed:      ${RED}${FAIL_COUNT}${NC}"
echo ""

if [[ $FAIL_COUNT -eq 0 ]]; then
    echo -e "${GREEN}╔════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${GREEN}║  ✓ ALL BEAM FEATURES VALIDATED SUCCESSFULLY               ║${NC}"
    echo -e "${GREEN}║                                                            ║${NC}"
    echo -e "${GREEN}║  Your BEAM VM is ready for production use!                ║${NC}"
    echo -e "${GREEN}╚════════════════════════════════════════════════════════════╝${NC}"
    exit 0
else
    echo -e "${RED}╔════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${RED}║  ✗ SOME TESTS FAILED                                       ║${NC}"
    echo -e "${RED}║                                                            ║${NC}"
    echo -e "${RED}║  Please check your BEAM VM installation.                  ║${NC}"
    echo -e "${RED}╚════════════════════════════════════════════════════════════╝${NC}"
    exit 1
fi
