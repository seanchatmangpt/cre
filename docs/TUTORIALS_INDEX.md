# Tutorials Index

This index provides an overview of all tutorials included in the architectural refactoring. Each tutorial is designed to guide users through different aspects of the new CRE architecture and its utility modules.

## Tutorial Structure

All tutorials follow the same structure:
- **Learning Objectives**: What you'll learn
- **Prerequisites**: Required knowledge
- **Step-by-Step Examples**: Hands-on implementation
- **Best Practices**: Common patterns and pitfalls
- **Exercises**: Try-it-yourself challenges
- **Further Reading**: Next steps and advanced topics

---

## Available Tutorials

### 1. [Getting Started](tutorials/getting_started.md)

**Level**: Beginner
**Time**: 30 minutes
**Focus**: Basic workflow creation and execution

#### Learning Objectives
- Understand the new architecture concepts
- Create your first YAWL workflow
- Execute basic patterns with `gen_pnet`
- Use core utility modules

#### Topics Covered
- Joe Armstrong architecture principles
- `pnet_net` behavior implementation
- Basic place/transition definition
- Simple execution patterns
- Integration with helper modules

#### Key Examples
```erlang
% Simple linear workflow
-module(linear_workflow).
-behaviour(pnet_net).
```

---

### 2. [Basic Patterns Tutorial](tutorials/basic_patterns_tutorial.md)

**Level**: Beginner
**Time**: 45 minutes
**Focus**: Core YAWL workflow patterns

#### Learning Objectives
- Implement fundamental YAWL patterns
- Understand pattern semantics and use cases
- Combine patterns for complex workflows
- Handle state transitions properly

#### Topics Covered
- Sequence patterns
- Parallel split and merge
- Exclusive choice
- Simple merge
- Pattern composition techniques

#### Key Examples
```erlang
% Parallel split implementation
-module(parallel_split_demo).
-behaviour(pnet_net).
```

---

### 3. [Advanced Patterns Tutorial](tutorials/advanced_patterns_tutorial.md)

**Level**: Intermediate
**Time**: 60 minutes
**Focus**: Complex pattern composition and advanced workflows

#### Learning Objectives
- Implement sophisticated YAWL patterns
- Handle complex branching logic
- Manage synchronization and timing
- Create hierarchical workflows

#### Topics Covered
- N-out-of-M patterns
- Interleaved routing
- Discriminator and milestone patterns
- Multiple merge scenarios
- Error handling and recovery

#### Key Examples
```erlang
% Complex routing with conditions
-module(complex_routing_workflow).
-behaviour(pnet_net).
```

---

### 4. [Colored Tokens Tutorial](tutorials/colored_tokens_tutorial.md)

**Level**: Intermediate
**Time**: 45 minutes
**Focus**: Colored Petri nets and advanced token handling

#### Learning Objectives
- Implement colored Petri nets
- Use variable bindings in transitions
- Handle complex token structures
- Create data-driven workflows

#### Topics Covered
- Variable definitions and bindings
- Colored mode enumeration
- Token transformation patterns
- Data flow through workflows
- Complex state management

#### Key Examples
```erlang
% Colored net with variable binding
-module(colored_workflow).
-behaviour(pnet_net).

modes(t_process, Marking, Ctx) ->
    %% Handle variable binding
    case pnet_types:is_binding(Ctx) of
        true -> enumerate_colored_modes(Marking, Ctx);
        false -> basic_mode_enumeration(Marking)
    end.
```

---

### 5. [Workflow Migration Tutorial](tutorials/workflow_migration_tutorial.md)

**Level**: Advanced
**Time**: 90 minutes
**Focus**: Migrating from old CRE architecture to new

#### Learning Objectives
- Understand architectural differences
- Migrate existing workflows
- Handle breaking changes
- Optimize for new architecture

#### Topics Covered
- Old vs new architecture comparison
- Migration step-by-step
- Common migration issues
- Performance considerations
- Testing migrated workflows

#### Key Examples
```erlang
% Migration example: old approach vs new
% Old: cre_worker processes
% New: gen_pnet with helper modules
```

---

## Tutorial Prerequisites

### Required Knowledge
- Basic Erlang/OTP programming
- Understanding of Petri nets
- Workflow modeling concepts
- General programming experience

### Software Requirements
- Erlang/OTP 25+
- rebar3 build tool
- CRE project source code
- Basic text editor or IDE

### Before Starting
1. Set up development environment:
   ```bash
   rebar3 deps
   rebar3 compile
   ```

2. Understand basic concepts:
   - Places and transitions
   - Tokens and markings
   - Workflows and patterns

---

## Tutorial Roadmap

### For Beginners
1. **Start with**: Getting Started
2. **Then**: Basic Patterns Tutorial
3. **Next**: Colored Tokens Tutorial (optional)

### For Intermediate Developers
1. **Start with**: Basic Patterns Tutorial (if needed)
2. **Then**: Advanced Patterns Tutorial
3. **Next**: Colored Tokens Tutorial
4. **Finally**: Workflow Migration Tutorial (if migrating)

### For Advanced Users
1. **Review**: Getting Started (for new concepts)
2. **Then**: Advanced Patterns Tutorial
3. **Next**: Colored Tokens Tutorial
4. **Optionally**: Workflow Migration Tutorial

---

## Learning Path Recommendations

### Path 1: New to CRE
```
Getting Started → Basic Patterns → Advanced Patterns → Colored Tokens
```

### Path 2: Migrating from Old CRE
```
Getting Started → Workflow Migration → Basic Patterns → Advanced Patterns
```

### Path 3: Experienced with YAWL
```
Getting Started (for new architecture) → Advanced Patterns → Colored Tokens
```

---

## Tutorial Features

### Interactive Examples
- All tutorials include working code examples
- Examples can be copied and executed
- Output shown for each example
- Common errors and solutions explained

### Hands-on Exercises
- Each tutorial includes exercises
- Solutions available in appendix
- Progressive difficulty levels
- Real-world use cases

### Best Practices
- Pattern-specific recommendations
- Performance optimization tips
- Error handling strategies
- Code organization guidelines

### Common Patterns
- Reusable workflow patterns
- Helper module usage patterns
- Integration techniques
- Debugging strategies

---

## Running the Tutorials

### Setup
```bash
% Navigate to CRE project
cd /path/to/cre

% Compile all modules
rebar3 compile

% Start interactive shell
rebar3 shell
```

### Testing Examples
```erlang
% In the shell
c(yawl_patterns).
c(getting_started_example).
getting_started_example:demo().
```

### Running Tests
```bash
% Run specific tutorial tests
rebar3 ct -v -c test/tutorial_SUITE

% Run all tests
rebar3 ct
```

---

## Troubleshooting

### Common Issues

1. **Compilation Errors**
   - Ensure all dependencies are installed
   - Check module names match file names
   - Verify function exports

2. **Runtime Errors**
   - Check place/transition definitions
   - Validate token structures
   - Verify mode enumeration

3. **Performance Issues**
   - Profile with `pnet_marking` operations
   - Check for mode enumeration bottlenecks
   - Consider state optimization

### Getting Help
1. Check documentation in `/docs/`
2. Review examples in `/examples/`
3. Run tests to isolate issues
4. Check error messages carefully

---

## Advanced Topics

### After Completing Tutorials
1. **Read the specifications**:
   - `docs/yawl_patterns/GEN_PNET_API_SPECIFICATION.md`
   - `docs/COMPLETE_API_REFERENCE.md`

2. **Explore integration**:
   - Web service integration
   - External task management
   - Timer-based workflows

3. **Custom patterns**:
   - Create your own YAWL patterns
   - Extend existing patterns
   - Implement domain-specific workflows

4. **Production deployment**:
   - Scaling considerations
   - Monitoring and observability
   - Performance optimization

---

## Tutorial Maintenance

The tutorials are actively maintained as the architecture evolves:

- **Updates**: Regular updates for architectural changes
- **New Examples**: Additional use cases as they emerge
- **Bug Fixes**: Corrections based on user feedback
- **Performance**: Optimizations as patterns mature

### Contributing
If you find issues or have suggestions:
1. Check existing documentation
2. Search for similar issues
3. Create bug report with reproduction steps
4. Submit pull requests with examples

---

This tutorial index provides a comprehensive roadmap for learning the new CRE architecture. Each tutorial builds on previous concepts, allowing users to progress from basic workflow creation to advanced pattern implementation and migration strategies.