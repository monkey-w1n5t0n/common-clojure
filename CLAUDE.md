# CLAUDE.md - Clojure on SBCL

This file provides guidance to Claude Code when working on the Clojure on SBCL implementation.

## Project Goal

Implement Clojure on top of SBCL, making the official Clojure test suite pass. This is a TDD project - tests drive development.

## North Star

The `clojure-tests/` directory contains 68 test files from the official Clojure repository. **Making these tests pass is the goal.** Everything else is a means to that end.

## How to Work

### 1. Check What's Ready

```bash
cd /home/w1n5t0n/src/common-clojure
bd ready
```

Only work on issues with no blockers. Dependencies are intentionally chained so we build features in the right order.

### 2. Pick Up a Task

```bash
# Mark as in-progress
bd set-state in-progress common-clojure-<id>
```

### 3. Implement and Test

```bash
# Run test scanner to see current status
sbcl --script test-runner.lisp
```

The test runner shows:
- Which test files can now be parsed
- Which still error
- What features each test file needs

### 4. Close When Done

```bash
bd close common-clojure-<id>
```

## Implementation Layers

Work through layers in order. Don't jump ahead.

1. **Reader** - Must parse Clojure syntax before anything else
2. **Eval** - Can't run tests without evaluation
3. **Collections** - Tests use sequences, maps, sets heavily
4. **Concurrency** - Refs, atoms, agents come after core works
5. **Java Interop** - SBCL needs Java bridge for this

## Key Files

| File | Purpose |
|------|---------|
| `cl-clojure-syntax.lisp` | Reader implementation (macros for `[` `{`) |
| `test-runner.lisp` | Scans Clojure tests, reports parse status |
| `tests.lisp` | Original Common Lisp tests (legacy) |
| `clojure-tests/` | Official Clojure test suite (68 files) |

## Testing Strategy

1. **Parse First** - Can we read the test file?
2. **Eval Second** - Can we evaluate the forms?
3. **Assert Third** - Do the test assertions pass?

Don't try to run tests before the reader can parse them. Don't try to assert before eval works.

## Reader Implementation Notes

The reader uses Common Lisp's `set-macro-character` to hook into the parser:

```lisp
(set-macro-character #\[ #'read-vector)
(set-macro-character #\{ #'read-map)
```

New reader macros follow this pattern. Each dispatch macro (starting with `#`) is registered via `set-dispatch-macro-character`.

## Common Patterns

### Adding a New Reader Macro

```lisp
(defun read-<feature> (stream char)
  (declare (ignore char))
  ;; parse and return Lisp data
  )

(set-dispatch-macro-character #\# #\<char> #'read-<feature>)
```

### Testing Reader Changes

```lisp
;; In test-runner.lisp context
(enable-clojure-syntax)
(read-from-string "<clojure-syntax>")  ; should return Lisp data
```

## Commit Messages

Use Conventional Commits with scope:

```
feat(reader): implement keyword syntax :foo
fix(eval): handle nil in if conditional
test: add scanner for Clojure test files
docs: update README with current status
```

## Don't

- Don't work on issues blocked by open dependencies
- Don't skip ahead to later layers
- Don't add features not tracked in beads (create issue first)
- Don't close issues without verifying tests pass

## Do

- Run `bd ready` before picking up work
- Run `test-runner.lisp` to check progress
- Mark issues in-progress when working
- Close issues when tests actually pass
- Ask if blocked or unsure about approach
