# Quick Start Guide - CRE YAWL Workflow Engine

Get started with CRE in just 5 minutes! This guide will help you install CRE and run your first workflow.

## 🎯 Prerequisites

- **Erlang/OTP** 25.0 or later [Download](https://www.erlang.org/downloads)
- **rebar3** 3.18.0 or later [Install](https://rebar3.org/docs/getting-started/)
- **Git** for cloning the repository

## 🚀 Installation

Choose one of these three methods to install CRE:

### Method 1: From Hex (Recommended)

```bash
# Add CRE to your project dependencies
echo '{deps, [{cre, ".*", {git, "https://github.com/your-org/cre.git", {tag, "v0.3.0"}}}]}' > rebar.config
rebar3 deps
```

### Method 2: From GitHub

```bash
# Clone the repository
git clone https://github.com/your-org/cre.git
cd cre

# Install dependencies
rebar3 deps

# Compile the project
rebar3 compile
```

### Method 3: Using EScript

```bash
# Download the precompiled escript
wget https://github.com/your-org/cre/releases/download/v0.3.0/cre.escript

# Make executable
chmod +x cre.escript

# Run directly
./cre.escript --version
```

## 🎭 Your First Workflow - "Hello World"

Let's create a simple workflow that prints "Hello, World!" and shows the basic CRE workflow structure.

### Step 1: Create a simple workflow module

```erlang
% hello_world.erl
-module(hello_world).
-export([run/0]).

run() ->
    % Create a new workflow
    Workflow = cre_yawl:new_workflow(<<"hello_world_workflow">>),

    % Add an atomic task (simple step)
    Task1 = cre_yawl:add_task(Workflow, <<"greeting">>,
                             [{type, atomic},
                              {module, hello_world},
                              {function, say_hello}]),

    % Execute the workflow
    Result = cre_yawl:execute(Task1),
    io:format("Workflow result: ~p~n", [Result]).

% Task implementation
say_hello() ->
    io:format("Hello, World!~n"),
    ok.
```

### Step 2: Compile and run

```bash
# Compile your module
erlc hello_world.erl

# Start CRE shell
rebar3 shell

% In the shell:
c(hello_world).
hello_world:run().
```

Expected output:
```
Hello, World!
Workflow result: ok
```

## 🏢 Approval Workflow Example

Let's try a more complex example with human approval:

```erlang
% approval_demo.erl
-module(approval_demo).
-export([run/0]).

run() ->
    % Create approval workflow
    Workflow = cre_yawl:new_workflow(<<"approval_demo">>),

    % Add request task
    Request = cre_yawl:add_task(Workflow, <<"submit_request">>,
                               [{type, atomic},
                                {module, approval_demo},
                                {function, submit_request}]),

    % Add approval task (human-in-the-loop)
    Approval = cre_yawl:add_task(Workflow, <<"manager_approval">>,
                                [{type, approval},
                                 {module, approval_demo},
                                 {function, approve_request}]),

    % Connect tasks
    cre_yawl:connect(Workflow, Request, Approval),

    % Execute
    Result = cre_yawl:execute(Workflow),
    io:format("Approval workflow result: ~p~n", [Result]).

submit_request() ->
    io:format("Submitting request for approval...~n"),
    {ok, "Request submitted"}.

approve_request() ->
    % This would normally be handled by a human
    % For demo, we auto-approve
    io:format("Request approved by manager~n"),
    {ok, "Request approved"}.
```

Run it:
```erlang
c(approval_demo).
approval_demo:run().
```

## ⚙️ Basic Configuration

Create a `sys.config` file for basic CRE configuration:

```erlang
% sys.config
[
    {cre, [
        % Enable telemetry
        {telemetry_enabled, true},

        % Set timeout for tasks (seconds)
        {task_timeout, 30},

        % Enable human-in-the-loop
        {human_in_the_loop, true},

        % Web dashboard configuration
        {dashboard, [
            {port, 8080},
            {enabled, true}
        ]}
    ]}
].
```

Start with configuration:
```bash
rebar3 shell --config sys.config
```

## 🎯 Next Steps

### 1. Explore the Patterns
Learn about the 43 YAWL patterns:
```erlang
% Create a complex workflow with patterns
Workflow = cre_yawl:new_workflow(<<"complex_workflow">>),

% Add a multi-instance task
MultiInstance = cre_yawl:add_task(Workflow, <<"parallel_tasks">>,
                                 [{type, multi_instance},
                                  {cardinality, 5}]),

% Add a gateway
Gateway = cre_yawl:add_task(Workflow, <<"decision_point">>,
                            [{type, gateway},
                             {type, exclusive}]),
```

### 2. Try the Web Dashboard
Start CRE and navigate to `http://localhost:8080` to see:
- Real-time workflow execution
- Task status monitoring
- Performance metrics
- Error tracking

### 3. Read More Documentation
- [Complete API Reference](./API_REFERENCE.md) - All functions and options
- [YAWL Patterns Guide](./YAWL_PATTERNS_REFERENCE.md) - Pattern library
- [Human-in-the-Loop](./HUMAN_IN_THE_LOOP.md) - Approval workflows
- [Architecture Overview](./ARCHITECTURE.md) - System design

### 4. Try the Examples
Copy files from the [`examples/`](../examples/) directory and experiment with:
- Basic workflows
- Approval flows
- Parallel processing
- Complex patterns

## 🔧 Troubleshooting

### Common Issues

1. **OTP Version Error**
   ```bash
   # Check Erlang version
   erl
   % Output should show "Erlang/OTP [25|26|27|28]"
   ```

2. **Module Not Found**
   ```erlang
   % In CRE shell
   code:add_patha(".").  % Add current directory to path
   c(your_module).
   ```

3. **Port Already in Use (Dashboard)**
   ```erlang
   % Change dashboard port
   {dashboard, [{port, 8081}, {enabled, true}]}
   ```

### Getting Help

- **Check the logs**: Look for `{ok, _} = application:start(cre)`
- **Test with simple examples**: Start with "Hello World"
- **Review configuration**: Ensure `sys.config` is correct
- **Check Erlang/OTP**: Verify version compatibility

## 🎉 Congratulations!

You've successfully installed CRE and run your first workflow! You're ready to:

- ✅ Build complex workflows with YAWL patterns
- ✅ Implement human approval processes
- ✅ Monitor workflows with OpenTelemetry
- ✅ Visualize workflows in the web dashboard

**Next**: Explore the [API Reference](./API_REFERENCE.md) to see what CRE can do!