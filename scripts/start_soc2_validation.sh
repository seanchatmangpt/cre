#!/bin/sh
###-------------------------------------------------------------------
### SOC 2 Validation Startup Script
###
### Starts the SOC 2 validation supervision tree for continuous
### compliance monitoring.
###
### Usage:
###   ./scripts/start_soc2_validation.sh
###
### Joe Armstrong Philosophy:
###   "Supervise everything. Let it crash. Prove it works."
###
###-------------------------------------------------------------------

set -e

echo "=== Starting SOC 2 Validation System ==="
echo ""
echo "Philosophy: Joe Armstrong-level capability validation"
echo "  - Continuous validation (not point-in-time)"
echo "  - Fault-tolerant (supervisors restart failed validators)"
echo "  - Observable (all receipts logged)"
echo "  - Provable (Merkle tree cryptographic proof)"
echo ""

# Ensure directories exist
mkdir -p evidence/uptime
mkdir -p evidence/load_tests
mkdir -p evidence/chaos
mkdir -p receipts

# Start Erlang shell with SOC 2 validation
erl -pa _build/default/lib/*/ebin \
    -eval 'application:ensure_all_started(cre)' \
    -eval '{ok, _} = soc2_validation_sup:start_link()' \
    -eval 'io:format("~n=== SOC 2 Validation Tree Started ===~n")' \
    -eval 'io:format("Control Executors: 7~n")' \
    -eval 'io:format("Evidence Generators: 4~n")' \
    -eval 'io:format("Receipt Chain: Active~n")' \
    -eval 'io:format("Meta-Validator: Running~n~n")' \
    -eval 'io:format("Commands:~n")' \
    -eval 'io:format("  soc2_validation_sup:get_validation_status().~n")' \
    -eval 'io:format("  soc2_receipt_chain:get_chain().~n")' \
    -eval 'io:format("  soc2_meta_validator:run_meta_validation().~n~n")' \
    -noshell

echo ""
echo "=== SOC 2 Validation System Stopped ==="
