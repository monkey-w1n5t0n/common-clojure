---
name: clojure-impl
description: Use when implementing or fixing Clojure language features in this Common Lisp runtime against the official test suite. NOT for Java-interop work or architecture-wide redesigns.
---

# Clojure Implementation Worker

NOTE: Startup and cleanup are handled by `worker-base`. This skill defines the WORK PROCEDURE.

## When to Use This Skill

This skill is used for implementing Clojure language features in Common Lisp:
- Adding new Clojure functions (e.g., `clojure.string/split`)
- Implementing macros and special forms (e.g., `try`/`catch`, `for` improvements)
- Adding namespace support (e.g., `clojure.walk`)
- Fixing bugs in existing implementations
- Enhancing lazy sequence semantics

## Required Skills

None - this is a pure Common Lisp implementation task.

## Work Procedure

### 1. Understand the Requirement
- Read the relevant test file(s) in `clojure-tests/` to understand expected behavior
- Inspect assigned work with `ergo show <id>`, or use `ergo ready --all` to see
  unblocked tasks from the migrated project queue; claim one with `ergo claim <id>`
- Consult DEVLOG.md for similar implementations and gotchas

### 2. Write Tests First (TDD)
- If adding a new feature, create a minimal test file in `clojure-tests/` or modify an existing one
- Run `./tests.sh` to confirm the test fails (red)

### 3. Implement the Feature
Location depends on feature type:

**For functions:**
- Add function in `cl-clojure-eval.lisp` with `clojure-` prefix
- Add forward declaration at top of file
- Register in `setup-core-functions`

**For macros/special forms:**
- Add evaluator function (e.g., `eval-try-catch`)
- Add dispatch case in `clojure-eval` function

**For namespace functions (e.g., `clojure.string`):**
- Add case in `eval-java-interop` function for the namespace
- Implement each function with proper Clojure semantics

### 4. Key Implementation Patterns

```lisp
;; Function with multiple arities
(defun clojure-my-fn (&rest args)
  (case (length args)
    (0 (my-fn-0-arity))
    (1 (my-fn-1-arity (first args)))
    (2 (my-fn-2-arity (first args) (second args)))
    (t (apply #'my-fn-n-arity args))))

;; Register core function
(register-core-function env "my-fn" #'clojure-my-fn)

;; Namespace function dispatch
((string-equal class-name "string")
 (cond
   ((string-equal member-name "trim")
    (string-trim '(#\Space #\Tab #\Newline) (first args)))
   ...))
```

### 5. Handle Gotchas
- Use `string=` not `eq` for symbol comparison
- Wrap closures with `ensure-callable` before passing to CL functions
- Use `safe-math-fn1`/`safe-math-fn2` for NaN-safe math
- Metadata format: `(meta-wrapper value metadata)` - value is SECOND

### 6. Verify Implementation
```bash
# Run full test suite
./tests.sh

# Run specific test file for debugging
sbcl --load run-tests.lisp --eval '(progn (try-run-clojure-file "clojure-tests/for.clj") (sb-ext:quit))'
```

### 7. Update Tracking
- Update DEVLOG.md with what was implemented
- Complete the Ergo task, if applicable, only after verification:
  `ergo done <id> --reason "Implemented and tests passed"`
- Commit with conventional commit message: `feat(string): implement clojure.string/split`

### 8. Verify Completion
- `./tests.sh` exits successfully for the intended change
- `ergo show <id>` reports the expected terminal state when work was task-tracked
- `git diff --check` reports no whitespace errors

## Example Handoff

```json
{
  "salientSummary": "Implemented clojure.string namespace with split, join, trim, replace, reverse functions. Fixed string.clj test - now passes.",
  "whatWasImplemented": "Added clojure.string functions in eval-java-interop: split (with regex and limit), join (with separator), trim/triml/trimr, replace/replace-first (with regex support), reverse, upper-case, lower-case, capitalize. Added forward declarations and registered functions.",
  "whatWasLeftUndone": "escape function not yet implemented - string.clj has test for it but it's a minor feature",
  "verification": {
    "commandsRun": [
      {"command": "./tests.sh", "exitCode": 0, "observation": "69 passed, 33 failed (was 68 passed, 34 failed)"},
      {"command": "ergo done <id> --reason \"Implemented and tests passed\"", "exitCode": 0, "observation": "Task completed"}
    ],
    "interactiveChecks": []
  },
  "tests": {
    "added": [
      {"file": "clojure-tests/string.clj", "cases": [{"name": "t-split", "verifies": "split function works with regex and limit"}, {"name": "t-join", "verifies": "join works with and without separator"}]}
    ]
  },
  "discoveredIssues": [
    {"severity": "low", "description": "escape function in clojure.string not implemented", "suggestedFix": "Add escape case to string namespace dispatch"}
  ]
}
```

## When to Return to Orchestrator

- Feature requires architectural changes beyond adding functions
- Cannot understand test expectations from test file
- Blocking dependency on unimplemented feature (e.g., protocols needed for multimethods)
- Java interop required (out of scope - skip those tests)
- Multiple failed attempts to implement a feature
