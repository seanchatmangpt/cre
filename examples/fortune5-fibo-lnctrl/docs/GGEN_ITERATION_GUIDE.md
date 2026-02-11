# ggen Sync Iteration Guide

**Purpose**: Remember how to iterate on code generation using ontology → templates → code workflow

**Last Updated**: 2026-02-11
**Session**: https://claude.ai/code/session_01AqyFjzD4x2WfBL3qeigtBs

---

## 🎯 Core Principle

**NEVER edit generated code directly**. Always fix:
1. The ontology (source of truth)
2. The templates (transformation rules)
3. The generator script (execution engine)

Then **regenerate everything**.

---

## 📁 File Structure

```
examples/fortune5-fibo-lnctrl/
├── ontology/              # Source of truth
│   └── f5_line_control.ttl
├── templates/             # Code templates
│   └── (currently inline in Python)
├── sparql/                # Graph queries
│   └── (queries for extracting from ontology)
├── scripts/
│   └── generate.py        # Generator engine
└── apps/                  # Generated code (DO NOT EDIT)
    ├── f5_app_02/
    ├── f5_app_03/
    └── f5_connectors/
```

---

## 🔄 Iteration Workflow

### Step 1: Identify Issue

Examples:
- App won't start → missing .app file in ebin/
- Test fails → template has wrong assertion
- Runtime error → generated code has bug
- LOC count wrong → counting logic in generator

### Step 2: Locate Root Cause

Ask: "Where is this coming from?"

| Problem | Root Cause File |
|---------|----------------|
| Module structure wrong | `scripts/generate.py` (module template) |
| Wrong exports | `scripts/generate.py` (export generation) |
| Missing .app file | `scripts/generate.py` (app file handling) |
| Test syntax error | `scripts/generate.py` (test template) |
| Wrong connector operations | `scripts/generate.py` (CONNECTORS list) |
| Ontology data wrong | `ontology/f5_line_control.ttl` |

### Step 3: Fix the Source

**Example 1: Fix broken test template**

```python
# BAD (in generator):
Results = [{ops[0]}_test(#{test => I}) || I <- lists:seq(1, 10)]

# GOOD:
Results = [test_operation(I) || I <- lists:seq(1, 10)]
```

**Example 2: Add .app files to ebin**

```python
# Before:
(app_dir / "src" / f"{app_name}.app.src").write_text(app_src_content)

# After:
(app_dir / "src" / f"{app_name}.app.src").write_text(app_src_content)
(app_dir / "ebin").mkdir(parents=True, exist_ok=True)
(app_dir / "ebin" / f"{app_name}.app").write_text(app_src_content)
```

### Step 4: Regenerate

```bash
cd examples/fortune5-fibo-lnctrl
python3 scripts/generate.py
```

### Step 5: Verify

```bash
# Compile a sample
erlc -o apps/f5_app_02/ebin apps/f5_app_02/src/*.erl

# Run tests
erlc -DTEST -o apps/f5_app_02/ebin apps/f5_app_02/src/f5_app_02_mod_01.erl
erl -pa apps/f5_app_02/ebin -noshell -eval 'eunit:test(f5_app_02_mod_01), halt().'

# Start app
erl -pa apps/*/ebin -noshell -eval 'application:start(f5_app_02), halt().'
```

### Step 6: Commit Only the Source

```bash
# GOOD - commit generator changes
git add scripts/generate.py
git commit -m "Fix: Add .app files to ebin/"

# BAD - don't commit generated code changes manually
# (They'll be regenerated anyway)
```

---

## 🛠️ Common Iteration Patterns

### Pattern 1: Fix Template Syntax

**Problem**: Generated code has Erlang syntax error

**Solution**:
1. Find the template in `scripts/generate.py`
2. Look for Python f-string interpolation issues
3. Test the template string in isolation
4. Fix escaping (use `{{` for literal `{`, `}}` for literal `}`)
5. Regenerate

**Example**:
```python
# Wrong:
f"{{ok, {value}}}"  # Produces: {ok, {value}} (syntax error)

# Right:
f"{{{{ok, {value}}}}}"  # Produces: {{ok, some_value}}
```

### Pattern 2: Add New Connector

**Problem**: Need to add a new external service connector

**Solution**:
1. Edit `scripts/generate.py` → `CONNECTORS` list
2. Add new entry:
   ```python
   {
       "id": "fraud_detection",
       "name": "FraudDetection",
       "auth": "api_key",
       "rate_limit": 1000,
       "operations": [
           "ScreenTransaction", "GetRiskScore", "BlockAccount"
       ]
   }
   ```
3. Regenerate
4. New `f5_connector_fraud_detection.erl` appears in `apps/f5_connectors/src/`

### Pattern 3: Change Module Template

**Problem**: Want to add logging to all modules

**Solution**:
1. Edit `generate_connector_module()` or module generation code
2. Add to template:
   ```python
   op_functions.append(f'''
   {op_snake}(Params) ->
       logger:info("Calling {op} with ~p", [Params]),  # NEW
       gen_server:call(?MODULE, {{{op_snake}, Params}}).
   ''')
   ```
3. Regenerate
4. All 8,642 modules now have logging

### Pattern 4: Fix Counting/Metrics

**Problem**: LOC count is wrong in receipt

**Solution**:
1. Find the counting logic in `scripts/generate.py`
2. Currently uses: `total_loc += len(module_content.split('\n'))`
3. This counts Python string lines, not file lines
4. Better: `total_loc += len(module_file.read_text().split('\n'))`
5. Regenerate
6. Receipt now shows accurate count

---

## 📊 Verification Checklist

After each iteration:

- [ ] Generator runs without Python errors
- [ ] All modules compile (`erlc -o ebin src/*.erl`)
- [ ] Sample app starts (`application:start(f5_app_02)`)
- [ ] Tests pass (`eunit:test(f5_app_02_mod_01)`)
- [ ] Receipt hash changes (proves regeneration happened)
- [ ] LOC count is accurate
- [ ] No uncommitted generated code (only source changes)

---

## 🚫 Anti-Patterns (DON'T DO THIS)

### ❌ Editing Generated Code
```bash
# WRONG
vim apps/f5_app_02/src/f5_app_02_mod_01.erl  # Edit generated file
git commit -m "Fix module"
```

**Problem**: Next regeneration will wipe out your changes

### ❌ Mixing Generated and Hand-Written
```bash
# WRONG
touch apps/f5_app_02/src/custom_module.erl  # Hand-written
python3 scripts/generate.py  # Regenerates, might conflict
```

**Problem**: Generator might delete or overwrite hand-written code

### ❌ Committing Both Source and Generated
```bash
# WRONG
git add scripts/generate.py apps/
git commit -m "Fix generator and regenerate"
```

**Problem**: Bloats commits. Generated code should be reproducible from source.

### ❌ Patching Generated Code
```bash
# WRONG
python3 scripts/generate.py
sed -i 's/foo/bar/g' apps/*/src/*.erl  # Patch output
```

**Problem**: Next regeneration loses patches. Fix the template instead!

---

## ✅ Best Practices

### 1. Version Control Strategy

**Commit**:
- ✅ Ontology changes (`ontology/*.ttl`)
- ✅ Template changes (`templates/*.tera` or inline in `generate.py`)
- ✅ Generator changes (`scripts/generate.py`)
- ✅ Receipts (`receipts/build.last.json`)

**Don't Commit** (add to `.gitignore`):
- ❌ Generated code (`apps/*/src/*.erl`) if frequently regenerated
- ❌ Compiled artifacts (`apps/*/ebin/*.beam`)
- ❌ Temporary files

**Exception**: For demonstration/archival, commit generated code periodically with clear message: "Snapshot: generated code from commit XYZ"

### 2. Testing Strategy

**After each generator change**:
1. Regenerate
2. Compile a sample app
3. Run one EUnit test
4. Start one app via `application:start/1`

**Before committing**:
1. Full regeneration
2. Compile all apps (or representative sample)
3. Run test suite on sample apps
4. Verify receipt metrics

### 3. Documentation

**Update when**:
- Adding new connector type → document in `CONNECTORS` comment
- Changing template → add comment explaining pattern
- Modifying generation logic → update this guide

### 4. Performance

**Regeneration time**:
- 8,642 modules: ~4-5 seconds ✓
- If slower, profile generator
- Consider parallel generation for 20+ seconds

---

## 🔍 Debugging Tips

### Problem: "Application won't start"

**Check**:
1. Does `.app` file exist in `apps/*/ebin/`?
2. Is `{modules, [...]}` list complete?
3. Does supervisor module compile?
4. Run: `application:start(AppName)` in `erl` shell for error details

### Problem: "Module won't compile"

**Check**:
1. Syntax error in template (look for unescaped `{` `}`)
2. Missing `-module()` directive
3. Missing `-export()` for called functions
4. Run: `erlc -W0 src/module.erl` for detailed errors

### Problem: "Tests fail"

**Check**:
1. Test compiled with `-DTEST` flag?
2. EUnit header included: `-include_lib("eunit/include/eunit.hrl")`?
3. Test function named with `_test` suffix?
4. Assertions use correct macros (`?assertEqual`, not `?assert(A == B)`)?

### Problem: "Receipt shows wrong counts"

**Check**:
1. Counting logic in generator
2. Are you counting template string lines or actual file lines?
3. Use: `len(file.read_text().split('\n'))` not `len(template_string.split('\n'))`

---

## 📝 Example Session

```bash
# 1. Identify issue
erl -pa apps/f5_connectors/ebin -noshell -eval 'application:start(f5_connectors), halt().'
# Error: {error,{bad_application_specification,...}}

# 2. Diagnose
ls apps/f5_connectors/ebin/
# Missing f5_connectors.app!

# 3. Fix generator
vim scripts/generate.py
# Add: (connectors_app / "ebin" / "f5_connectors.app").write_text(...)

# 4. Regenerate
python3 scripts/generate.py
# Generated 206 apps in 4.3s

# 5. Verify fix
ls apps/f5_connectors/ebin/
# f5_connectors.app now exists ✓

erl -pa apps/f5_connectors/ebin -noshell -eval 'application:start(f5_connectors), halt().'
# Success ✓

# 6. Commit source change only
git add scripts/generate.py
git commit -m "Fix: Generate .app files in ebin/ for runtime"
git push
```

---

## 🎓 Key Takeaways

1. **Source of Truth**: Ontology + Templates + Generator
2. **Never**: Edit generated code manually
3. **Always**: Fix source, regenerate, verify
4. **Commit**: Only source changes (generator, ontology, templates)
5. **Test**: Compile → Unit test → App start → Full integration
6. **Document**: Update this guide when patterns change

---

## 📚 References

- Generator: `scripts/generate.py`
- Current ontology: `ontology/f5_line_control.ttl` (future)
- Receipt format: `receipts/build.last.json`
- Evidence format: `evidence/` directory
- Thesis: `thesis/main.tex` (theoretical foundation)

---

**Remember**: Manufacturing > Hand-writing. Fix the factory, not the product.
