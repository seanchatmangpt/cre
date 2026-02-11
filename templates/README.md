# Tera Code Generation Templates

Jinja2-compatible templates that generate Erlang/OTP, Docker, and Terraform artifacts.

## Templates

- `line.tera` - Generate Erlang line execution module (seq/par/gates)
- `connector.tera` - Generate Erlang connector client module
- `app.tera` - Generate OTP application supervisor + start module
- `Dockerfile.tera` - Generate multi-stage Docker build
- `terraform.tera` - Generate Terraform GCP resource definitions

**Input**: RDF query results + generation context
**Output**: Compilable Erlang code + infrastructure-as-code

## Context Variables

Each template receives:
- `connector_name`: string
- `operations`: [{name, params, return_type}]
- `effects`: [{name, idempotency_key, timeout}]
- `gates`: [{condition_ast, true_path, false_path}]

## Usage

```
ggen template -t templates/line.tera -c context.json -o output/my_line.erl
```
