# SOC 2 Receipt Schema Validator

## Overview

The `soc2_receipt_schema` module provides comprehensive validation of receipt JSON against the canonical schema defined in `docs/RECEIPT_SCHEMA.md`. It ensures all required fields, types, and structure conform to specification before receipts are accepted into the cryptographically-verified receipt chain.

**Location**: `/home/user/cre/src/soc2/soc2_receipt_schema.erl`

## Module API

### Type Definitions

```erlang
-type receipt() :: map().
-type receipt_type() :: build | evidence | verdict | unknown.
-type validation_error() :: {invalid, string()} | {missing_field, string()}.
-type validation_result() :: {ok, receipt()} | {error, [validation_error()]}.
```

### Core Functions

#### `validate_receipt/1` - Auto-detect and validate

```erlang
-spec validate_receipt(receipt()) -> validation_result().
```

Automatically detects receipt type and validates against the appropriate schema.

**Example**:
```erlang
Receipt = #{
    <<"chain">> => #{...},
    <<"counts">> => #{...},
    ...
},
{ok, ValidReceipt} = soc2_receipt_schema:validate_receipt(Receipt).
```

#### `validate_receipt/2` - Explicit type validation

```erlang
-spec validate_receipt(receipt(), atom() | auto) -> validation_result().
```

Validates receipt with optional explicit type specification. Useful when you know the expected type and want to catch type mismatches.

**Example**:
```erlang
{ok, _} = soc2_receipt_schema:validate_receipt(Receipt, build),
{error, _} = soc2_receipt_schema:validate_receipt(Receipt, evidence).
```

#### `validate_build_receipt/1` - Build receipt validation

```erlang
-spec validate_build_receipt(receipt()) -> validation_result().
```

Validates build receipts (from `receipts/build.last.json`).

**Validates**:
- Common fields (chain, environment_fingerprint, timestamp)
- `counts`: apps, modules, loc, tests (all integers)
- `timings`: generation_us, validation_us (all integers)
- `generator_version`: binary
- `ontology_hash`: binary

#### `validate_evidence_receipt/1` - Evidence receipt validation

```erlang
-spec validate_evidence_receipt(receipt()) -> validation_result().
```

Validates evidence receipts (from `receipts/evidence.last.json`).

**Validates**:
- Common fields
- `evidence_files`: list of file entries with path, sha256, size_bytes
- `manifest_hash`: binary (sha256 of evidence manifest)
- `generator_version`: binary
- `ontology_hash`: binary

**Evidence file entry structure**:
```erlang
#{
    <<"path">> => binary(),           % Relative path to file
    <<"sha256">> => binary(),         % SHA256 hash (hex string)
    <<"size_bytes">> => integer()     % File size in bytes
}
```

#### `validate_verdict_receipt/1` - Verdict receipt validation

```erlang
-spec validate_verdict_receipt(receipt()) -> validation_result().
```

Validates verdict receipts (from `receipts/verdict.last.json`).

**Validates**:
- Common fields
- `suite`: binary (test suite identifier like "nine_nines")
- `tests_passed`: boolean
- `apps_generated`: list of binaries (app names)
- `failing_tests`: list of test entries
- `proofs_summary`: map of validator proofs
- `generator_version`: binary
- `ontology_hash`: binary

**Failing test entry structure**:
```erlang
#{
    <<"test_id">> => binary(),        % Test identifier
    <<"reason">> => binary()          % Reason for failure
}
```

**Proof entry structure**:
```erlang
#{
    <<"passed">> => boolean(),        % Whether proof passed
    <<"proof">> => term()             % Proof data (any JSON-serializable term)
}
```

#### `get_receipt_type/1` - Type detection

```erlang
-spec get_receipt_type(receipt()) -> receipt_type().
```

Detects receipt type by inspecting discriminating fields:
- **build**: Has `counts` field
- **evidence**: Has `evidence_files` field
- **verdict**: Has `tests_passed` and `suite` fields
- **unknown**: Cannot determine type

**Example**:
```erlang
build = soc2_receipt_schema:get_receipt_type(BuildReceipt),
evidence = soc2_receipt_schema:get_receipt_type(EvidenceReceipt),
verdict = soc2_receipt_schema:get_receipt_type(VerdictReceipt).
```

## Validation Rules

### Common Fields (All Receipts)

All receipt types must include:

1. **chain** (map, required)
   - `this_hash` (binary): SHA256 hash of receipt (hex-encoded)
   - `prev_hash` (binary | null): SHA256 of previous receipt, or null for genesis

2. **environment_fingerprint** (map, required)
   - `arch` (binary): System architecture
   - `emulator` (binary): Erlang emulator version
   - `os` (binary): Operating system
   - `otp_version` (binary): OTP release version

3. **timestamp** (binary, required)
   - ISO8601 format with timezone (e.g., "2026-02-11T14:03:36+00:00")

### Build Receipt Fields

```erlang
#{
    chain => #{...},
    counts => #{
        apps => integer(),
        modules => integer(),
        loc => integer(),
        tests => integer()
    },
    environment_fingerprint => #{...},
    generator_version => binary(),
    ontology_hash => binary(),
    timestamp => binary(),
    timings => #{
        generation_us => integer(),
        validation_us => integer()
    }
}
```

### Evidence Receipt Fields

```erlang
#{
    chain => #{...},
    environment_fingerprint => #{...},
    evidence_files => [
        #{
            path => binary(),
            sha256 => binary(),
            size_bytes => integer()
        }
    ],
    generator_version => binary(),
    manifest_hash => binary(),
    ontology_hash => binary(),
    timestamp => binary()
}
```

### Verdict Receipt Fields

```erlang
#{
    apps_generated => [binary()],
    chain => #{...},
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
            proof => term()
        }
    },
    suite => binary(),
    tests_passed => boolean(),
    timestamp => binary()
}
```

## Integration with Receipt Chain

The `soc2_receipt_chain` module now validates all receipts before accepting them:

```erlang
%% In soc2_receipt_chain:append_receipt/1
append_receipt(Receipt) ->
    case soc2_receipt_schema:validate_receipt(Receipt) of
        {ok, ValidReceipt} ->
            gen_server:cast(?MODULE, {append_receipt, ValidReceipt}),
            ok;
        {error, Errors} ->
            logger:warning(#{
                what => receipt_validation_failed,
                receipt => Receipt,
                errors => Errors
            }),
            {error, {invalid_receipt, Errors}}
    end.
```

## Error Handling

Validation failures return `{error, [ValidationError]}` with detailed error messages:

```erlang
{invalid, "Field counts must be a map"}
{missing_field, "chain.this_hash"}
{invalid, "timestamp must be valid ISO8601 format"}
```

**Example**:
```erlang
case soc2_receipt_schema:validate_receipt(Receipt) of
    {ok, ValidReceipt} ->
        soc2_receipt_chain:append_receipt(ValidReceipt);
    {error, Errors} ->
        logger:error(#{
            what => invalid_receipt,
            errors => Errors,
            receipt => Receipt
        })
end.
```

## Testing

Comprehensive unit tests are provided in `/home/user/cre/test/soc2_receipt_schema_test.erl`:

```bash
# Run tests in Docker
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit

# Or locally with Erlang installed
erl -compile test/soc2_receipt_schema_test.erl
erl -noshell -eval "eunit:test(soc2_receipt_schema_test, [verbose])" -s init stop
```

**Test Coverage**:
- ✓ Type detection for all receipt types
- ✓ Build receipt validation (valid and invalid)
- ✓ Evidence receipt validation (valid, invalid files, missing hashes)
- ✓ Verdict receipt validation (valid, invalid proofs, missing tests)
- ✓ Common field validation (chain, timestamp, environment)
- ✓ Edge cases (empty lists, null prev_hash, invalid timestamps)

## Examples

### Example 1: Simple validation

```erlang
Receipt = #{
    <<"chain">> => #{
        <<"prev_hash">> => null,
        <<"this_hash">> => <<"abc123">>
    },
    <<"counts">> => #{
        <<"apps">> => 1,
        <<"modules">> => 10,
        <<"loc">> => 1000,
        <<"tests">> => 5
    },
    <<"environment_fingerprint">> => #{
        <<"arch">> => <<"x86_64">>,
        <<"emulator">> => <<"16.2">>,
        <<"os">> => <<"linux">>,
        <<"otp_version">> => <<"28">>
    },
    <<"generator_version">> => <<"commit_hash">>,
    <<"ontology_hash">> => <<"ontology_hash">>,
    <<"timestamp">> => <<"2026-02-11T14:00:00Z">>,
    <<"timings">> => #{
        <<"generation_us">> => 1000,
        <<"validation_us">> => 500
    }
},

{ok, ValidReceipt} = soc2_receipt_schema:validate_receipt(Receipt),
soc2_receipt_chain:append_receipt(ValidReceipt).
```

### Example 2: Handling validation errors

```erlang
BadReceipt = #{
    <<"chain">> => #{
        <<"this_hash">> => <<"hash">>
        %% Missing prev_hash!
    }
},

case soc2_receipt_schema:validate_receipt(BadReceipt) of
    {ok, _} ->
        io:format("Receipt valid~n");
    {error, Errors} ->
        io:format("Validation errors:~n"),
        lists:foreach(fun(Error) ->
            io:format("  - ~p~n", [Error])
        end, Errors)
end.
```

### Example 3: Explicit type validation

```erlang
%% This will succeed
{ok, _} = soc2_receipt_schema:validate_receipt(BuildReceipt, build),

%% This will fail with type mismatch
{error, _} = soc2_receipt_schema:validate_receipt(BuildReceipt, evidence).
```

## Design Philosophy

The validator follows these principles:

1. **Fail Fast**: Returns all validation errors at once for efficient debugging
2. **Strict Typing**: All fields must be correct types (no implicit conversions)
3. **Complete Structure**: Validates nested maps recursively
4. **Clear Errors**: Error messages indicate exactly which field failed
5. **Deterministic**: Uses binary keys (consistent with JSON schema)

## Performance

Validation is O(n) where n is the number of fields and nested structures. For typical receipt sizes (< 100KB), validation completes in < 1ms.

## Future Enhancements

- Hash chain verification (validate prev_hash links)
- Cryptographic signature validation
- Schema versioning support
- Custom validation rules per receipt type
