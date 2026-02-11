# Receipt and Evidence Schema Specification

## Canonical JSON Schema (Enforced at Runtime)

All receipts use OTP 28 `json:encode/1` with **stable key ordering** (alphabetical).

### 1. receipts/build.last.json

```erlang
#{
    chain => #{
        prev_hash => binary() | null,  % sha256 hex of previous receipt
        this_hash => binary()           % sha256 hex of this receipt (excluding this field)
    },
    counts => #{
        apps => integer(),
        modules => integer(),
        loc => integer(),
        tests => integer()
    },
    environment_fingerprint => #{
        arch => binary(),              % erlang:system_info(system_architecture)
        emulator => binary(),           % erlang:system_info(version)
        os => binary(),                 % OS type
        otp_version => binary()         % erlang:system_info(otp_release)
    },
    generator_version => binary(),      % git commit hash
    ontology_hash => binary(),          % sha256 of canonical ontology input
    timestamp => binary(),              % ISO8601 UTC
    timings => #{
        generation_us => integer(),
        validation_us => integer()
    }
}.
```

### 2. receipts/evidence.last.json

```erlang
#{
    chain => #{
        prev_hash => binary() | null,
        this_hash => binary()
    },
    environment_fingerprint => #{...},
    evidence_files => [
        #{
            path => binary(),
            sha256 => binary(),
            size_bytes => integer()
        }
    ],
    generator_version => binary(),
    manifest_hash => binary(),          % sha256 of evidence/evidence.sha256
    ontology_hash => binary(),
    timestamp => binary()
}.
```

### 3. receipts/verdict.last.json

```erlang
#{
    apps_generated => [binary()],
    chain => #{
        prev_hash => binary() | null,
        this_hash => binary()
    },
    environment_fingerprint => #{...},
    failing_tests => [
        #{
            test_id => binary(),
            reason => binary()
        }
    ],
    generator_version => binary(),
    ontology_hash => binary(),
    proofs_summary => #{
        validator_id() => #{
            passed => boolean(),
            proof => term()             % JSON-serializable proof object
        }
    },
    suite => binary(),                  % "nine_nines" | customer_id
    tests_passed => boolean(),
    timestamp => binary()
}.
```

## Deterministic Generation Rules

1. **Stable Ordering**: All maps use alphabetically sorted keys
2. **Canonical Timestamps**: Generation timestamps in evidence only, not in source
3. **Deterministic UUIDs**: Use hash-based UUIDs, not random
4. **Sorted Collections**: All lists sorted by stable key
5. **Normalized Paths**: Relative paths, forward slashes

## Hash Chain Protocol

```
receipt_0 (genesis):
    prev_hash = null
    this_hash = sha256(canonical_json(receipt_0 - {this_hash}))

receipt_i (i > 0):
    prev_hash = receipt_{i-1}.this_hash
    this_hash = sha256(canonical_json(receipt_i - {this_hash}))
```

## Verification

Every receipt includes a self-verification function that MUST pass:
- Schema validation
- Hash chain integrity
- Environment fingerprint consistency
