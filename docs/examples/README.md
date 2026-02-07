# CRE Workflow Examples

This directory contains practical examples demonstrating CRE's YAWL workflow capabilities.

## Examples

### 1. Basic Workflow (`basic_workflow.erl`)

A simple YAWL workflow demonstrating:
- Basic task execution
- Manual approval simulation
- Fraud detection workflow
- Sequential task execution

**To run:**
```bash
# Compile
erlc -I../include -o../src ../examples/basic_workflow.erl

# Execute
erl -pa ../src -pa ../test -s basic_workflow demo -s init stop
```

### 2. Advanced Approval Workflow (`approval_workflow.erl`)

An advanced workflow featuring:
- Multi-tier approval architecture
- LLM-powered approval decisions
- Human-in-the-loop with escalation
- OpenTelemetry integration

**Requirements:**
- Include `yawl_otel_logger` module
- Configure LLM provider

**To run:**
```bash
# Compile (requires yawl_otel_logger)
erlc -I../include -o../src ../examples/approval_workflow.erl

# Execute (with telemetry)
erl -pa ../src -pa ../test -s approval_workflow demo -s init stop
```

## Key Features Demonstrated

| Example | Patterns | Human-in-the-Loop | Telemetry | Complexity |
|---------|----------|-------------------|-----------|------------|
| Basic Workflow | Sequential, Atomic | Manual approval | Basic | Beginner |
| Approval Workflow | Multi-tier, LLM integration | Escalation, LLM decisions | OpenTelemetry | Advanced |

## Common Workflow Patterns

1. **Sequential Flow**: Tasks execute in order
2. **Approval Gates**: Human review required
3. **LLM Integration**: AI-powered decisions
4. **Telemetry**: Monitoring and observability
5. **Error Handling**: Graceful failure management

## Integration Notes

- All examples require CRE v0.2.1+
- Basic workflows can run standalone
- Advanced features require additional modules
- OpenTelemetry provides comprehensive monitoring