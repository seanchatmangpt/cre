# Proof Demos

Scripts demonstrating manufacturing, execution, and stop-the-line capability.

## Demo 1: Manufacturing (`demo_manufacture.sh`)

Shows: Ontology → generated artifacts with deterministic receipts.

```bash
demo_manufacture.sh
  1. Start with baseline ontology
  2. Generate artifacts v1 (capture receipt hash)
  3. Modify ontology (add new incident type)
  4. Regenerate artifacts v2 (capture new receipt hash)
  5. Compare: if receipt hash changed → success
  6. Verify: v1 artifacts ≠ v2 artifacts (deterministic changes only)
```

## Demo 2: Run Line (`demo_run_line.sh`)

Shows: Line execution with effects and receipts.

```bash
demo_run_line.sh
  1. Load incident sample data
  2. Run soc_triage_line
  3. Emit receipt after each gate/effect
  4. Output: trace of steps + effect results + final receipt
  5. Verify: all steps present, no dropped effects
```

## Demo 3: Stop the Line (`demo_stop_the_line.sh`)

Shows: Cancellation, restart, and replay.

```bash
demo_stop_the_line.sh
  1. Start incident triage
  2. At gate 2 (severity decision): issue CANCEL signal
  3. Verify: line halts immediately
  4. Verify: no further effects executed post-cancel
  5. Restart line (from same incident)
  6. Verify: trace is identical (replay)
  7. Output: side-by-side trace comparison
```

## Sample Data

- `incident_samples.json` - 5 test incidents (varying severity, classification)
- `expected_traces.json` - Expected execution traces for each incident

## Running

```bash
cd /home/user/cre/demo
bash demo_manufacture.sh
bash demo_run_line.sh
bash demo_stop_the_line.sh
```

Expected output: 3 pass/fail test reports with metrics.
