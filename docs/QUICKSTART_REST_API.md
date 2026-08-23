# Quick Start Guide - CRE REST API

Get started with the CRE Workflow Management REST API in 5 minutes.

## Prerequisites

- CRE 0.3.0 or later
- Erlang/OTP 28+
- Docker (for production deployment)

## Installation

### 1. Build CRE with Docker

```bash
# Build multi-arch Docker image
docker buildx bake --load

# Or build for specific platform
docker build -t cre:0.3.0 .
```

### 2. Start CRE in Development Mode

```bash
# Run CRE container with API enabled
docker run -it --rm \
  -p 8080:8080 \
  -v $(pwd):/work \
  -w /work \
  cre:0.3.0 sh

# Inside container, start Erlang shell
erl -pa _build/default/lib/*/ebin
```

### 3. Start the REST API Server

```erlang
% Start CRE application
application:ensure_all_started(cre).

% Start HTTP API server on port 8080
cre_http_handler:start_listener(8080).
```

## Your First Workflow

### Step 1: Create a Workflow

```bash
curl -X POST http://localhost:8080/workflows \
  -H "Content-Type: application/json" \
  -d '{
    "workflow_module": "example_simple_workflow",
    "case_id": "my-first-workflow",
    "init_args": {
      "user": "alice"
    },
    "options": []
  }'
```

**Response:**
```json
{
  "status": "created",
  "case_id": "my-first-workflow",
  "pid": "<0.245.0>",
  "message": "Workflow created successfully"
}
```

### Step 2: Check Workflow Status

```bash
curl http://localhost:8080/workflows/my-first-workflow
```

**Response:**
```json
{
  "status": "running",
  "case_id": "my-first-workflow",
  "pid": "<0.245.0>",
  "marking": {
    "start": [],
    "processing": ["token"],
    "done": []
  },
  "usr_info": {
    "user": "alice",
    "start_time": 1707664800000
  }
}
```

### Step 3: List All Workflows

```bash
curl http://localhost:8080/workflows
```

**Response:**
```json
{
  "status": "ok",
  "workflows": [
    {
      "case_id": "my-first-workflow",
      "pid": "<0.245.0>"
    }
  ],
  "count": 1
}
```

### Step 4: Stop the Workflow

```bash
curl -X POST http://localhost:8080/workflows/my-first-workflow/stop
```

**Response:**
```json
{
  "status": "stopped",
  "case_id": "my-first-workflow",
  "message": "Workflow stopped successfully"
}
```

## Creating Your Own Workflow

### Minimal Workflow Module

Create `my_workflow.erl`:

```erlang
-module(my_workflow).
-behaviour(gen_yawl).

%% Export callbacks
-export([place_lst/0, trsn_lst/0, init_marking/2, preset/1,
         is_enabled/3, fire/3]).
-export([init/1, code_change/3, handle_call/3, handle_cast/2,
         handle_info/2]).

%% Define places and transitions
place_lst() -> [p1, p2, p3].
trsn_lst() -> [t1, t2].

%% Initial marking - token in p1
init_marking(p1, _) -> [token];
init_marking(_, _) -> [].

%% Transition structure
preset(t1) -> [p1];
preset(t2) -> [p2].

%% Enable conditions
is_enabled(t1, Mode, _) ->
    case maps:get(p1, Mode, []) of
        [_] -> true;
        _ -> false
    end;
is_enabled(t2, Mode, _) ->
    case maps:get(p2, Mode, []) of
        [_] -> true;
        _ -> false
    end.

%% Transition firing
fire(t1, _Mode, UsrInfo) ->
    {produce, #{p2 => [token]}, UsrInfo};
fire(t2, _Mode, UsrInfo) ->
    {produce, #{p3 => [token]}, UsrInfo}.

%% Interface callbacks
init(Args) -> Args.
code_change(_, State, _) -> {ok, State}.
handle_call(_, _, State) -> {reply, ok}.
handle_cast(_, State) -> noreply.
handle_info(_, State) -> noreply.
```

### Compile and Load

```bash
# Compile
erlc -I include -o _build/default/lib/cre/ebin my_workflow.erl

# Or in Erlang shell
c(my_workflow).
```

### Use Your Workflow

```bash
curl -X POST http://localhost:8080/workflows \
  -H "Content-Type: application/json" \
  -d '{
    "workflow_module": "my_workflow",
    "case_id": "test-001",
    "init_args": {},
    "options": []
  }'
```

## Common Patterns

### 1. Workflow with Data Flow

```erlang
fire(t1, Mode, UsrInfo) ->
    %% Get input data from token
    [InputToken] = maps:get(p1, Mode),
    ProcessedData = process(InputToken),

    %% Update user info
    NewUsrInfo = UsrInfo#{
        last_processed => ProcessedData,
        count => maps:get(count, UsrInfo, 0) + 1
    },

    {produce, #{p2 => [ProcessedData]}, NewUsrInfo}.
```

### 2. Conditional Branching

```erlang
fire(t_check, Mode, UsrInfo) ->
    [Token] = maps:get(p_input, Mode),

    case check_condition(Token) of
        true ->
            {produce, #{p_true_branch => [Token]}, UsrInfo};
        false ->
            {produce, #{p_false_branch => [Token]}, UsrInfo}
    end.
```

### 3. Error Handling

```erlang
fire(t_risky, Mode, UsrInfo) ->
    try
        [Token] = maps:get(p_input, Mode),
        Result = risky_operation(Token),
        {produce, #{p_success => [Result]}, UsrInfo}
    catch
        _:Error ->
            logger:error("Workflow error: ~p", [Error]),
            NewUsrInfo = UsrInfo#{error => Error},
            {produce, #{p_error => [Error]}, NewUsrInfo}
    end.
```

## Testing

### Unit Tests

```bash
# Run all tests
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 rebar3 eunit

# Run specific module
docker run --rm -v $(pwd):/work -w /work cre:0.3.0 \
  rebar3 eunit --module=cre_http_handler_test
```

### Manual Testing Script

Create `test_api.sh`:

```bash
#!/bin/bash

BASE_URL="http://localhost:8080"

echo "1. Create workflow"
curl -X POST $BASE_URL/workflows \
  -H "Content-Type: application/json" \
  -d '{"workflow_module":"example_simple_workflow","case_id":"test-001","init_args":{},"options":[]}'
echo -e "\n"

echo "2. Get workflow status"
curl $BASE_URL/workflows/test-001
echo -e "\n"

echo "3. List all workflows"
curl $BASE_URL/workflows
echo -e "\n"

echo "4. Stop workflow"
curl -X POST $BASE_URL/workflows/test-001/stop
echo -e "\n"
```

Make it executable and run:

```bash
chmod +x test_api.sh
./test_api.sh
```

## Deployment

### Production Docker Deployment

```bash
# Build for production
docker buildx bake --load

# Run with proper configuration
docker run -d \
  --name cre-api \
  -p 8080:8080 \
  -e CRE_HTTP_PORT=8080 \
  --restart unless-stopped \
  cre:0.3.0

# Check logs
docker logs -f cre-api
```

### Kubernetes Deployment

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cre-api
spec:
  replicas: 3
  selector:
    matchLabels:
      app: cre-api
  template:
    metadata:
      labels:
        app: cre-api
    spec:
      containers:
      - name: cre
        image: cre:0.3.0
        ports:
        - containerPort: 8080
        env:
        - name: CRE_HTTP_PORT
          value: "8080"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 10
          periodSeconds: 5
---
apiVersion: v1
kind: Service
metadata:
  name: cre-api
spec:
  selector:
    app: cre-api
  ports:
  - port: 80
    targetPort: 8080
  type: LoadBalancer
```

## Troubleshooting

### Issue: "Connection refused"

**Solution:** Ensure the API server is started:

```erlang
cre_http_handler:start_listener(8080).
```

### Issue: "Workflow module does not exist"

**Solution:** Compile and load the module first:

```erlang
c(my_workflow).
```

### Issue: "Workflow already exists"

**Solution:** Use a different case_id or stop the existing workflow:

```bash
curl -X POST http://localhost:8080/workflows/existing-id/stop
```

### Issue: "Port already in use"

**Solution:** Stop the existing listener or use a different port:

```erlang
cre_http_handler:stop_listener().
cre_http_handler:start_listener(9090).
```

## Next Steps

1. **Read the full API documentation**: [REST_API.md](REST_API.md)
2. **Explore example workflows**: Check `src/patterns/example_*.erl`
3. **Learn YAWL patterns**: Read the pattern documentation
4. **Deploy to production**: Follow GCP deployment guide

## Resources

- [CRE Documentation](https://github.com/joergen7/cre)
- [YAWL Workflow Patterns](http://www.workflowpatterns.com/)
- [Cowboy HTTP Server](https://ninenines.eu/docs/en/cowboy/2.14/guide/)
- [Erlang/OTP Documentation](https://www.erlang.org/doc/)

## Support

- **Issues**: https://github.com/joergen7/cre/issues
- **Discussions**: https://github.com/joergen7/cre/discussions
- **License**: Apache-2.0
