# Getting Started with YAWL and gen_pnet

**A hands-on introduction to building your first workflow**

---

## What You'll Learn

In this tutorial, you will:

- Understand the basic concepts of YAWL workflows and gen_pnet
- Set up your development environment
- Create and run your first workflow
- Add tasks and connect them
- Execute and monitor workflow completion

**Time required**: 30 minutes
**Prerequisites**: Basic knowledge of Erlang, no prior YAWL experience needed

---

## Tutorial 1: Understanding the Basics

### What is YAWL?

YAWL (Yet Another Workflow Language) is a workflow language based on workflow patterns. It provides a formal foundation for modeling and executing business processes using Petri net semantics.

### What is gen_pnet?

gen_pnet is an OTP behavior that implements colored Petri nets. In CRE, YAWL workflows are built on top of gen_pnet, providing:

- **Formal semantics**: Every workflow has a precise mathematical definition
- **Colored tokens**: Tokens can carry data through the workflow
- **Soundness guarantees**: Workflows can be verified for correctness
- **Concurrent execution**: Parallel tasks execute truly concurrently

### Key Concepts

| Concept | Description | Example |
|---------|-------------|---------|
| **Place** | A state location that holds tokens | `p_start`, `p_active`, `p_done` |
| **Transition** | An action that consumes/produces tokens | `t_start`, `t_complete`, `t_cancel` |
| **Token** | Represents workflow state or data | Initial token, task token, data token |
| **Marking** | Distribution of tokens across places | Current workflow state |
| **Firing** | Execution of an enabled transition | Transition `t_start` fires, moving tokens |

---

## Tutorial 2: Setting Up Your Environment

### Step 1: Install Prerequisites

Ensure you have Erlang/OTP 25+ and rebar3 installed:

```bash
# Check Erlang version
erl
% Output should show "Erlang/OTP [25|26|27|28]"

# Check rebar3
rebar3 version
% Should output rebar3 version
```

### Step 2: Create a New Project

```bash
# Create your project directory
mkdir my_yawl_project
cd my_yawl_project

# Initialize rebar3 project
rebar3 new release my_yawl_project
cd my_yawl_project

# Add CRE as a dependency
cat >> rebar.config <<'EOF'
{deps, [
    {cre, {git, "https://github.com/your-org/cre.git", {branch, "main"}}}
]}.
EOF

# Download dependencies
rebar3 deps
```

### Step 3: Compile CRE

```bash
# Compile the project
rebar3 compile

# Start an Erlang shell with CRE loaded
rebar3 shell
```

You should see the Erlang shell start successfully.

---

## Tutorial 3: Your First Workflow - "Hello World"

Let's create a simple workflow that prints "Hello, YAWL!" to understand the basic structure.

### Step 1: Create the Workflow Module

Create a file named `hello_yawl.erl`:

```erlang
%% @doc A simple "Hello World" workflow using YAWL and gen_pnet
-module(hello_yawl).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% API
-export([run/0, run/1]).

%%====================================================================
%% Records
%%====================================================================

-record(colored_token, {
    id          :: binary(),
    type        :: atom(),
    payload     :: term(),
    metadata    :: map(),
    created_at  :: integer()
}).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Runs the hello world workflow with default message
run() ->
    run(<<"Hello, YAWL!">>).

%% @doc Runs the hello world workflow with a custom message
run(Message) when is_binary(Message) ->
    %% Start the gen_pnet instance
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{message => Message}),

    %% Wait for completion (simple synchronous wait)
    wait_for_completion(Pid, 5000).

wait_for_completion(Pid, Timeout) ->
    Start = erlang:system_time(millisecond),
    receive
        {gen_pnet_done, Pid} ->
            io:format("Workflow completed successfully!~n"),
            ok;
        {gen_pnet_error, Pid, Reason} ->
            io:format("Workflow error: ~p~n", [Reason]),
            error
    after Timeout ->
        io:format("Workflow timeout~n"),
        timeout
    end.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

place_lst() ->
    %% Define the places (states) in our Petri net
    [p_start, p_ready, p_active, p_done].

trsn_lst() ->
    %% Define the transitions (actions) in our Petri net
    [t_start, t_say_hello, t_complete].

init_marking(p_start, _UsrInfo) ->
    %% Initial token in the start place
    [#colored_token{
        id = generate_id(),
        type = initial,
        payload = #{},
        metadata = #{},
        created_at = erlang:system_time(millisecond)
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_start) -> [p_start];
preset(t_say_hello) -> [p_ready];
preset(t_complete) -> [p_active];
preset(_Transition) -> [].

is_enabled(t_start, #{p_start := [Token]}, _UsrInfo) when Token =/= [] ->
    true;
is_enabled(t_say_hello, #{p_ready := [Token]}, _UsrInfo) when Token =/= [] ->
    true;
is_enabled(t_complete, #{p_active := [Token]}, _UsrInfo) when Token =/= [] ->
    true;
is_enabled(_Transition, _Mode, _UsrInfo) ->
    false.

fire(t_start, _Mode, UsrInfo) ->
    %% Get the message from user info
    Message = maps:get(message, UsrInfo, <<"Hello, YAWL!">>),
    io:format("Starting workflow with message: ~s~n", [Message]),

    %% Create token with the message payload
    Token = #colored_token{
        id = generate_id(),
        type = task_start,
        payload = #{message => Message},
        metadata = #{},
        created_at = erlang:system_time(millisecond)
    },

    {produce, #{
        p_ready => [Token],
        p_start => []
    }};

fire(t_say_hello, #{p_ready := [Token]}, _UsrInfo) ->
    %% Extract message from token
    Message = maps:get(message, Token#colored_token.payload, <<"Hello!">>),
    io:format("~s~n", [Message]),

    %% Create completion token
    DoneToken = Token#colored_token{
        type = task_complete,
        payload = maps:put(status, printed, Token#colored_token.payload)
    },

    {produce, #{
        p_active => [DoneToken],
        p_ready => []
    }};

fire(t_complete, _Mode, _UsrInfo) ->
    io:format("Workflow complete!~n"),

    %% Send completion notification
    self() ! {gen_pnet_done, self()},

    {produce, #{
        p_done => [],
        p_active => []
    }};

fire(_Transition, _Mode, _UsrInfo) ->
    abort.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
handle_call(_Request, _From, State) -> {reply, unknown, State}.
handle_cast(_Request, State) -> {noreply, State}.
handle_info(_Request, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.

%%====================================================================
%% Internal Functions
%%====================================================================

generate_id() ->
    %% Generate a unique token ID
    binary:list_to_bin([
        <<"token_">>,
        integer_to_binary(erlang:unique_integer([positive])),
        <<"_">>,
        integer_to_binary(erlang:system_time(millisecond))
    ]).
```

### Step 2: Run Your First Workflow

```bash
# In the Erlang shell
c(hello_yawl).
hello_yawl:run().
```

Expected output:
```
Starting workflow with message: "Hello, YAWL!"
Hello, YAWL!
Workflow complete!
Workflow completed successfully!
```

### Step 3: Try with a Custom Message

```erlang
hello_yawl:run(<<"Custom message here!">>).
```

---

## Tutorial 4: Building a Multi-Task Workflow

Now let's create a more realistic workflow with multiple connected tasks - a simple document approval process.

### The Document Approval Workflow

```
[Submit] --> [Review] --> [Approve/Reject] --> [Notify]
```

Create `approval_workflow.erl`:

```erlang
%% @doc Document approval workflow
-module(approval_workflow).
-behaviour(gen_pnet).

%% gen_pnet callbacks
-export([
    place_lst/0,
    trsn_lst/0,
    init_marking/2,
    preset/1,
    is_enabled/3,
    fire/3,
    code_change/3,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% API
-export([run/0, run/1]).

%% Token record
-record(colored_token, {
    id          :: binary(),
    type        :: atom(),
    payload     :: map(),
    created_at  :: integer()
}).

%%====================================================================
%% API
%%====================================================================

run() ->
    run(#{document => <<"Annual Report 2024">>}).

run(Document) when is_map(Document) ->
    {ok, Pid} = gen_pnet:start_link(?MODULE, #{
        document => Document,
        submitter => <<"user@example.com">>,
        reviewer => <<"reviewer@example.com">>
    }),
    wait_for_completion(Pid).

wait_for_completion(Pid) ->
    receive
        {approval_complete, Result} -> {ok, Result};
        {approval_rejected, Reason} -> {rejected, Reason}
    after 30000 ->
        timeout
    end.

%%====================================================================
%% gen_pnet Callbacks
%%====================================================================

place_lst() ->
    [
        p_input,           % Initial document input
        p_submitted,       % Document submitted
        p_reviewing,       % Under review
        p_approved,        % Approved
        p_rejected,        % Rejected
        p_completed        % Workflow complete
    ].

trsn_lst() ->
    [
        t_submit,          % Submit document
        t_start_review,    % Start review
        t_approve,         % Approve document
        t_reject,          % Reject document
        t_notify_complete  % Send notification
    ].

init_marking(p_input, UsrInfo) ->
    Document = maps:get(document, UsrInfo, #{}),
    [#colored_token{
        id = make_id(),
        type = initial,
        payload = Document,
        created_at = erlang:system_time(millisecond)
    }];
init_marking(_Place, _UsrInfo) ->
    [].

preset(t_submit) -> [p_input];
preset(t_start_review) -> [p_submitted];
preset(t_approve) -> [p_reviewing];
preset(t_reject) -> [p_reviewing];
preset(t_notify_complete) -> [p_approved, p_rejected];
preset(_T) -> [].

is_enabled(t_submit, #{p_input := [T]}, _) when T =/= [] -> true;
is_enabled(t_start_review, #{p_submitted := [T]}, _) when T =/= [] -> true;
is_enabled(t_approve, #{p_reviewing := [T]}, _) when T =/= [] -> true;
is_enabled(t_reject, #{p_reviewing := [T]}, _) when T =/= [] -> true;
is_enabled(t_notify_complete, Mode, _) ->
    maps:get(p_approved, Mode, []) =/= [] orelse
    maps:get(p_rejected, Mode, []) =/= [];
is_enabled(_, _, _) -> false.

fire(t_submit, #{p_input := [Token]}, UsrInfo) ->
    io:format("Document submitted: ~p~n", [Token#colored_token.payload]),

    NewToken = Token#colored_token{
        type = submitted,
        payload = Token#colored_token.payload#{
            submitted_at => erlang:system_time(millisecond),
            submitter => maps:get(submitter, UsrInfo)
        }
    },

    {produce, #{p_submitted => [NewToken], p_input => []}};

fire(t_start_review, #{p_submitted := [Token]}, UsrInfo) ->
    io:format("Starting review by: ~p~n", [maps:get(reviewer, UsrInfo)]),

    NewToken = Token#colored_token{
        type = reviewing,
        payload = Token#colored_token.payload#{
            reviewer => maps:get(reviewer, UsrInfo),
            review_start => erlang:system_time(millisecond)
        }
    },

    {produce, #{p_reviewing => [NewToken], p_submitted => []}};

fire(t_approve, #{p_reviewing := [Token]}, _UsrInfo) ->
    io:format("Document APPROVED~n"),

    NewToken = Token#colored_token{
        type = approved,
        payload = Token#colored_token.payload#{
            decision => approved,
            reviewed_at => erlang:system_time(millisecond)
        }
    },

    {produce, #{p_approved => [NewToken], p_reviewing => []}};

fire(t_reject, #{p_reviewing := [Token]}, _UsrInfo) ->
    io:format("Document REJECTED~n"),

    NewToken = Token#colored_token{
        type = rejected,
        payload = Token#colored_token.payload#{
            decision => rejected,
            reviewed_at => erlang:system_time(millisecond)
        }
    },

    {produce, #{p_rejected => [NewToken], p_reviewing => []}};

fire(t_notify_complete, Mode, _UsrInfo) ->
    case {maps:get(p_approved, Mode, []), maps:get(p_rejected, Mode, [])} of
        {[Token], []} ->
            io:format("Sending approval notification for: ~p~n", [Token#colored_token.payload]),
            self() ! {approval_complete, Token#colored_token.payload},
            {produce, #{p_approved => [], p_completed => []}};
        {[], [Token]} ->
            io:format("Sending rejection notification for: ~p~n", [Token#colored_token.payload]),
            self() ! {approval_rejected, Token#colored_token.payload},
            {produce, #{p_rejected => [], p_completed => []}};
        _ ->
            abort
    end;

fire(_, _, _) ->
    abort.

code_change(_, S, _) -> {ok, S}.
handle_call(_, _, S) -> {reply, unknown, S}.
handle_cast(_, S) -> {noreply, S}.
handle_info(_, S) -> {noreply, S}.
terminate(_, _) -> ok.

%%====================================================================
%% Internal
%%====================================================================

make_id() ->
    <<(integer_to_binary(erlang:unique_integer([positive])))/binary,
      "_",(integer_to_binary(erlang:monotonic_time(millisecond)))/binary>>.
```

### Running the Approval Workflow

```erlang
c(approval_workflow).
approval_workflow:run().
```

---

## Tutorial 5: Understanding Token Flow

Let's visualize how tokens move through our approval workflow:

```
Initial State:
p_input: [Token{type=initial, payload=document}]
p_submitted: []
p_reviewing: []
p_approved: []
p_rejected: []

After t_submit fires:
p_input: []
p_submitted: [Token{type=submitted, payload=document, meta=submitted_at}]
p_reviewing: []
p_approved: []
p_rejected: []

After t_start_review fires:
p_input: []
p_submitted: []
p_reviewing: [Token{type=reviewing, payload=document, meta=reviewer}]
p_approved: []
p_rejected: []

After t_approve fires:
p_input: []
p_submitted: []
p_reviewing: []
p_approved: [Token{type=approved, payload=document, meta=decision}]
p_rejected: []
```

---

## Exercise: Add a Revision Step

Modify the approval workflow to add a revision step between rejection and resubmission.

**Hint**: You'll need to:
1. Add a new place: `p_revision`
2. Add a new transition: `t_request_revision`
3. Update `place_lst/0` and `trsn_lst/0`
4. Add preset and is_enabled rules
5. Implement the fire logic

---

## Summary

In this tutorial, you learned:

- YAWL workflow basics: places, transitions, tokens, and markings
- How to set up a CRE development environment
- How to create a simple "Hello World" workflow
- How to build a multi-task approval workflow
- How tokens flow through a workflow

**Next Steps**: Continue to [Basic Patterns Tutorial](basic_patterns_tutorial.md) to learn about workflow control patterns.

---

**Need Help?**

- Check the [YAWL Patterns Reference](../YAWL_PATTERNS_REFERENCE.md)
- Review the [gen_pnet API Specification](../yawl_patterns/GEN_PNET_API_SPECIFICATION.md)
- See examples in `docs/examples/`
