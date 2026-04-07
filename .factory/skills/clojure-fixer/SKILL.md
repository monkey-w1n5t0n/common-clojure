---
name: clojure-fixer
description: Fixes failing Clojure test files by implementing missing features and fixing bugs in the eval system
---

# Clojure Test Fixer Worker

NOTE: Startup and cleanup are handled by `worker-base`. This skill defines the WORK PROCEDURE.

## When to Use This Skill

This skill is used when a feature describes fixing a specific failing Clojure test file or group of related test files. Each feature targets one or more test files to fix.

## Required Skills

None - this is a pure Common Lisp implementation task.

## Work Procedure

### 1. Reproduce the Failure

First, confirm the test fails and understand the exact error:

```bash
# Run the full suite to see current state
sbcl --script run-tests.lisp 2>&1 | tail -40

# Run the specific failing test to see the error
sbcl --noinform --disable-debugger --non-interactive \
  --load sbcl-init.lisp --load package.lisp --load cl-clojure-syntax.lisp \
  --eval '(let ((*readtable* (copy-readtable nil))) (load "cl-clojure-eval.lisp") (load "cl-clojure-transducers.lisp"))' \
  --eval '(handler-case (cl-clojure-eval::eval-file "clojure-tests/TESTNAME.clj") (error (c) (format t "FAIL: ~A~%" c) (sb-ext:exit :code 1))) (sb-ext:exit :code 0)'
```

### 2. Investigate the Root Cause

- Read the test file in `clojure-tests/` to understand what it tests
- Find the specific form that fails (add debug output to eval if needed)
- Check if the failure is:
  - Missing function/special form → need to implement
  - Type error → need to fix type handling
  - Reader error → need to fix reader in cl-clojure-syntax.lisp
  - Logic error → need to fix existing implementation

**Key files:**
- `cl-clojure-eval.lisp` — Main eval (9663 lines). ALL functions and special forms live here.
- `cl-clojure-syntax.lisp` — Reader (843 lines)
- `cl-clojure-transducers.lisp` — Transducers (424 lines)

**To find relevant code in cl-clojure-eval.lisp:**
- Search for the function name or special form name
- Core functions are registered in `setup-core-functions` (around line 3777)
- Special forms are dispatched in `clojure-eval` (around line 8865)
- Java interop/namespace functions are in `eval-java-interop`

### 3. Implement the Fix

**For missing core functions:**
1. Add function definition (use `clojure-` prefix)
2. Add forward declaration at top of file if needed
3. Register in `setup-core-functions` using `register-core-function`

**For missing special forms:**
1. Add evaluator function (e.g., `eval-my-special-form`)
2. Add case in `clojure-eval` dispatch

**For type handling bugs:**
1. Find the function that has the type error
2. Add type checking/guard before the operation
3. Use `typecase` or `etypecase` for dispatch

**For reader bugs:**
1. Find the relevant macro character handler in `cl-clojure-syntax.lisp`
2. Fix the parsing logic

### 4. Key Gotchas

- Use `string=` (case-sensitive) NOT `eq` for symbol name comparison
- Wrap closures with `ensure-callable` before passing to CL functions
- Metadata format is `(meta-wrapper value metadata)` — value is `cadr`, metadata is `cddr`
- NaN handling: use `sb-ext:float-nan-p` before any numeric comparison
- Hash-tables (Clojure maps) need explicit seq conversion — don't assume they're sequences
- When adding to `eval-java-interop`, check for Java interop tests first (those are out of scope)
- `and` and `or` are special forms — when used as values they need function wrappers

### 5. Verify the Fix

```bash
# Run the specific test
sbcl --noinform --disable-debugger --non-interactive \
  --load sbcl-init.lisp --load package.lisp --load cl-clojure-syntax.lisp \
  --eval '(let ((*readtable* (copy-readtable nil))) (load "cl-clojure-eval.lisp") (load "cl-clojure-transducers.lisp"))' \
  --eval '(handler-case (cl-clojure-eval::eval-file "clojure-tests/TESTNAME.clj") (error (c) (format t "FAIL: ~A~%" c) (sb-ext:exit :code 1))) (sb-ext:exit :code 0)'

# Run FULL suite to check for regressions
sbcl --script run-tests.lisp
```

MUST verify: the target test passes AND all 68 previously passing tests still pass.

### 6. Commit

Use conventional commit format:
```
feat(eval): implement X function
fix(eval): fix Y type handling
fix(reader): fix Z parsing
```

## Example Handoff

```json
{
  "salientSummary": "Fixed vectors.clj test by adding hash-table to sequence conversion in clojure-seq. The test was failing because maps (hash-tables) were being passed to functions expecting sequences.",
  "whatWasImplemented": "Updated clojure-seq to handle hash-table type by converting to list of pairs. Added typecase dispatch for hash-table in seq function. Also added hash-table support to clojure-into.",
  "whatWasLeftUndone": "",
  "verification": {
    "commandsRun": [
      {"command": "sbcl --script run-tests.lisp", "exitCode": 1, "observation": "69 passed, 33 failed (vectors now passes, was failing before)"}
    ],
    "interactiveChecks": []
  },
  "tests": {
    "added": []
  },
  "discoveredIssues": []
}
```

## When to Return to Orchestrator

- The fix requires changes to fundamental eval architecture (not just adding functions)
- Cannot determine what the test expects after reading the test file
- Multiple attempts (3+) to fix the issue have failed
- The test requires Java interop that cannot be stubbed
- A previously passing test has regressed and you cannot fix the regression
