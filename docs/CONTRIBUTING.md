# Contributing to CRE YAWL Workflow Engine

Thank you for your interest in contributing to CRE! This document provides guidelines for contributing code, reporting issues, and participating in the development community.

## 🤝 Code of Conduct

Please note that this project follows a standard Code of Conduct. By participating, you agree to follow these [guidelines](https://github.com/your-org/cre/blob/main/CODE_OF_CONDUCT.md).

## 📋 Development Workflow

### 1. Setup Development Environment

```bash
# Clone the repository
git clone https://github.com/your-org/cre.git
cd cre

# Install Erlang/OTP 25.0+
asdf install erlang 25.3.2.9

# Install rebar3
curl -L -o rebar3 https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3

# Install dependencies
rebar3 deps
```

### 2. Running Tests

```bash
# Run all tests
rebar3 ct

# Run specific test directory
rebar3 ct -d test/

# Run with coverage
rebar3 do eunit ct cover

# Run specific test case
rebar3 ct --verbose --case yawl_otel_logger_SUITE
```

### 3. Code Quality Checks

```bash
# Format code (Ruff formatter)
rebar3 fmt

# Lint code (Ruff with all rules)
rebar3 lint

# Type checking (Mypy)
rebar3 dialyzer

# Security scan (Bandit)
rebar3 xref security
```

## 🎯 Development Standards

### Code Style

**Erlang Style Guide:**
- Use 4-space indentation
- Maximum line length: 80 characters
- Function names use snake_case
- Module names use camelCase
- Always include module documentation

**Example:**
```erlang
%% @doc
%% This module implements YAWL workflow pattern validation.
%%
%% @end
-module(cre_yawl_validator).

-export([validate_workflow/1, validate_pattern/2]).

%% Types
-type workflow() :: #workflow{}.
-type error_reason() :: invalid_pattern | missing_task | connection_error.

%% @doc
%% Validates a workflow structure against YAWL patterns.
%% @end
-spec validate_workflow(Workflow :: workflow()) ->
    {ok, workflow()} | {error, error_reason()}.
validate_workflow(Workflow) ->
    % Implementation
    ok.
```

### Testing Requirements

**Test Coverage Requirements:**
- Minimum 80% test coverage
- All new features must have tests
- Critical paths must have 100% coverage
- Include property-based testing for complex logic

**Test Structure:**
```erlang
%% @doc
%% Tests for workflow pattern validation.
%% @end
-module(yawl_otel_logger_SUITE).

-export([
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test cases
basic_logging_test(_Config) ->
    % Test basic logging functionality
    ok.

telemetry_metrics_test(_Config) ->
    % Test metrics collection
    ok.

%% Helper functions
setup_test_data() ->
    % Create test workflow
    ok.
```

### Documentation Requirements

**Docstring Requirements:**
- All public functions must have docstrings
- Follow Erlang docstring conventions
- Include @spec for type signatures
- Document parameters and return values

**Example:**
```erlang
%% @doc
%% Creates a new task with the specified configuration.
%%
%% @param Workflow The workflow to add the task to
%% @param TaskName Unique name for the task
%% @param Opts Task configuration options
%% @return {ok, UpdatedWorkflow} on success
%% @return {error, Reason} if task creation fails
%% @end
-spec add_task(Workflow :: workflow(), TaskName :: binary(), Opts :: proplists:proplist()) ->
    {ok, workflow()} | {error, term()}.
add_task(Workflow, TaskName, Opts) ->
    % Implementation
    ok.
```

## 🚀 Adding New Features

### 1. Feature Request

Before implementing a feature:

1. Check if the feature already exists
2. Create an issue describing the feature
3. Get approval from maintainers
4. Discuss implementation approach

### 2. Implementation Steps

1. **Write Tests First** (TDD approach):
```erlang
% Test the feature before implementation
new_feature_test_() ->
    [
        {"basic functionality", ?_assertEqual(expected, actual)},
        {"edge case handling", ?_assertMatch({error, _}, actual)}
    ].
```

2. **Implement the Feature**:
```erlang
% Feature implementation
-spec new_feature(Arg :: term()) -> term().
new_feature(Arg) ->
    % Implementation logic
    Result.
```

3. **Add Documentation**:
```erlang
%% @doc
%% Description of the new feature.
%% @end
-spec new_feature(Arg :: term()) -> term().
```

4. **Update Examples**:
```erlang
% Add to examples/new_feature.erl
-module(new_feature).
-export([example/0]).

example() ->
    % Example usage
    ok.
```

### 3. New YAWL Patterns

To add a new YAWL pattern:

1. **Update Pattern Registry**:
```erlang
% src/cre_yawl_patterns.erl
-export([new_pattern/1]).

new_pattern(<<"pattern_name">>) ->
    #pattern{
        id = <<"pattern_name">>,
        name = "Pattern Name",
        type = pattern_type,
        description = "Description of the pattern",
        inputs = [input1, input2],
        outputs = [output1],
        validation_fun = fun validate/1,
        execution_fun = fun execute/2
    }.
```

2. **Add Pattern Tests**:
```erlang
% test/yawl_patterns_SUITE.erl
pattern_name_test(_Config) ->
    Workflow = cre_yawl:add_pattern(Workflow, <<"pattern_name">>, []),
    Result = cre_yawl:execute(Workflow),
    ?assertEqual(expected, Result).
```

## 🔧 Pull Request Process

### 1. Branch Strategy

```bash
# Create feature branch
git checkout -b feature/new-pattern

# Or bugfix branch
git checkout -b fix/issue-123

# Or chore branch
git checkout -b chore/update-dependencies
```

### 2. Commit Message Guidelines

**Format:**
```
<type>(<scope>): <description>

[body]

[closes #<issue>]
```

**Types:**
- `feat` - New feature
- `fix` - Bug fix
- `docs` - Documentation change
- `test` - Test-related change
- `chore` - Maintenance or build changes

**Examples:**
```
feat(yawl): add new approval pattern

Implements human-in-the-loop approval workflow with LLM integration.
Adds support for approval queues and deadline management.

closes #456
```

```
fix(telemetry): resolve memory leak in OTel logger

Fixes memory leak in telemetry collection by properly cleaning up
spans after export.

Fixes #789
```

### 3. Pull Request Template

```markdown
## Description
Brief description of the changes.

## Changes Made
- [ ] Added new YAWL pattern
- [ ] Fixed bug in workflow execution
- [ ] Updated documentation
- [ ] Added tests
- [ ] Performance improvements

## Testing
- [ ] Unit tests pass
- [ ] Integration tests pass
- [ ] Manual testing completed
- [ ] Performance regression tested

## Breaking Changes
- [ ] Yes
- [ ] No

## Related Issues
Closes #123

## Screenshots (if applicable)
<!-- Add screenshots to show changes -->
```

### 4. Review Process

1. **Self-Review** - Check your own code
2. **CI Checks** - All automated checks must pass
3. **Peer Review** - Get feedback from other developers
4. **Maintainer Review** - Final approval
5. **Testing** - Additional testing may be required

## 📦 Dependencies

### Adding Dependencies

```erlang
% rebar.config
{deps, [
    {new_dependency, "1.0.0"},
    % Existing dependencies...
]}.
```

### Version Pinning

Use specific versions for reproducible builds:
```erlang
{deps, [
    {cre, {git, "https://github.com/your-org/cre", {tag, "v0.2.1"}}}
]}.
```

## 🚨 Common Pitfalls

### 1. Memory Leaks

- Use `rebar3 cover` to check coverage
- Monitor memory usage with `observer:start()`
- Properly clean up process dictionaries

### 2. Performance Issues

- Use `fprof` for profiling
- Avoid deep recursion in hot paths
- Use appropriate data structures

### 3. Race Conditions

- Use `gen_server` with proper state management
- Test with `common_test` for concurrent scenarios
- Avoid global state

### 4. Error Handling

- Use specific error types
- Include enough context in error messages
- Handle both transient and permanent errors

## 📈 Performance Guidelines

### Benchmarking

```erlang
% Use benchmarks for performance-critical code
-define(BENCHMARK(Name, Fun),
    timer:tc(Fun))).

benchmark_workflow_creation() ->
    ?BENCHMARK("create workflow", fun() ->
        cre_yawl:new_workflow(<<"benchmark">>)
    end).
```

### Optimization Tips

1. **Use maps instead of proplists** for better performance
2. **Pre-allocate lists** when possible
3. **Use BIFs** instead of manual implementation
4. **Consider NIFs** for CPU-intensive operations

## 🔍 Debugging

### Common Debug Tools

```erlang
% Start interactive shell
rebar3 shell

% Use observer for process monitoring
observer:start().

% Enable verbose logging
logger:set_application_level(cre, debug).
```

### Logging

```erlang
% Use logger for structured logging
?debug("Debug message: ~p", [Data]),
?info("Info message: ~p", [Data]),
?warning("Warning: ~p", [Data]),
?error("Error: ~p", [Error]).
```

## 📚 Resources

### Documentation

- [Erlang Documentation](https://www.erlang.org/doc/)
- [OTP Design Principles](https://learnyousomeerlang.com/)
- [rebar3 Guide](https://rebar3.org/docs/getting-started/)

### Tools

- **IDE**: VS Code with Erlang plugin
- **Debugger**: `int.erl` or IDE debugger
- **Profiler**: `fprof` or `eper`
- **Coverage**: `cover` module

### Community

- **GitHub Issues**: Report bugs and request features
- **Discussions**: General discussions and questions
- **Stack Overflow**: Tag with `erlang` and `yawl`
- **Erlang Forums**: Community discussions

## 🏆 Contributor Recognition

Contributors will be recognized in:

1. **CHANGELOG.md** - For significant contributions
2. **Contributors list** - In the repository README
3. **Release announcements** - Major releases

## 🚨 Release Process

### Pre-Release Checklist

- [ ] All tests pass (100%)
- [ ] Documentation updated
- [ ] CHANGELOG.md updated
- [ ] Version bumped in `src/cre.app.src`
- [ ] Dependencies updated
- [ ] Security scan completed
- [ ] Performance benchmarked

### Release Steps

1. Update version numbers
2. Tag the release
3. Create release notes
4. Push to GitHub
5. Publish to Hex.pm

---

Thank you for contributing to CRE! We look forward to working with you.