# Clojure on SBCL - Test Fixer Loop

You are implementing Clojure on top of SBCL (Steel Bank Common Lisp). The goal is to make the official Clojure test suite pass.

## Project Context

This is a TDD project - tests drive development. The `clojure-tests/` directory contains 68 test files from the official Clojure repository.

### Key Files

- `cl-clojure-syntax.lisp` - Reader implementation (parses Clojure syntax into Lisp data)
- `test-runner.lisp` - Scans test files and reports parse status
- `tests.lisp` - Common Lisp tests (legacy)

## Current Status

Run this to check current status:
```bash
sbcl --script test-runner.lisp
```

As of the latest run:
- **57 test files** can be parsed successfully
- **11 test files** have parse errors: clearing, compilation, errors, java_interop, method_thunks, param_tags, protocols, reducers, test_helper, transducers, vectors

## Available Work (from beads)

Run `bd ready` to see issues with no blockers:

1. Reader: Implement anonymous function literal
2. Reader: Implement character literals
3. Reader: Implement dispatch literals
4. Reader: Implement deref and atom literals
5. Eval: Implement environment and var system
6. Eval: Implement def and defn
7. Eval: Implement fn syntax
8. Eval: Implement if conditional
9. Eval: Implement let and loop
10. Eval: Implement core arithmetic functions

## Your Workflow

1. **Check status**: `sbcl --script test-runner.lisp`
2. **Pick a task**: Choose a failing test or ready bead issue
3. **Implement**: Follow patterns in `cl-clojure-syntax.lisp`
4. **Test**: `sbcl --script test-runner.lisp`
5. **Run CL tests**: `sbcl --eval "(asdf:test-system :cl-clojure-syntax)"`
6. **Track work**:
   - `bd set-state in-progress <issue-id>` to claim
   - `bd close <issue-id>` when done
7. **Commit**: Use conventional commits: `feat(scope): description`

## Implementation Notes

### Reader Macros
The reader uses `set-macro-character` and `set-dispatch-macro-character`:

```lisp
;; For single-char macros like [ {
(set-macro-character #\[ #'read-vector nil *clojure-readtable*)

;; For dispatch macros like #"(regex)" #"(set)
(set-dispatch-macro-character #\# #\" #'read-regex *clojure-readtable*)
```

### Adding a New Reader Macro

```lisp
(defun read-<feature> (stream char)
  (declare (ignore char))
  ;; parse and return Lisp data
  )

(set-dispatch-macro-character #\# #\<char> #'read-<feature> *clojure-readtable*)
```

Use `ensure-clojure-readtable()` to get the Clojure readtable.

## Principles

- **Work in layers**: Reader → Eval → Collections → Concurrency → Java Interop
- **Don't skip ahead**: Features depend on earlier work
- **Parse before eval**: Can't run tests without being able to read them
- **Eval before assert**: Can't check results without evaluation

## Completion Signal

Output **COMPLETE** when:
- ALL 68 test files can be parsed, AND
- At least 50% can evaluate without errors

This is a long-term goal. Each iteration should make incremental progress.
