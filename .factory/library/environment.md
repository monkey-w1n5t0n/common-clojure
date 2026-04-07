# Environment

Environment variables, external dependencies, and setup notes.

**What belongs here:** Required env vars, external dependencies, SBCL version notes.
**What does NOT belong here:** Service ports/commands (use `.factory/services.yaml`).

---

## Dependencies

- **SBCL** (Steel Bank Common Lisp) — Primary CL implementation, already installed
- No external services, databases, or network dependencies required

## Running Tests

```bash
sbcl --script run-tests.lisp     # Full test suite (outputs passed/failed lists)
./tests.sh                        # Wrapper script
```

Individual test (for debugging):
```bash
sbcl --noinform --disable-debugger --non-interactive \
  --load sbcl-init.lisp --load package.lisp --load cl-clojure-syntax.lisp \
  --eval '(let ((*readtable* (copy-readtable nil))) (load "cl-clojure-eval.lisp") (load "cl-clojure-transducers.lisp"))' \
  --eval '(handler-case (cl-clojure-eval::eval-file "clojure-tests/NAME.clj") (error (c) (format t "FAIL: ~A~%" c) (sb-ext:exit :code 1))) (sb-ext:exit :code 0)'
```

## Test Status (Baseline)

68/102 tests passing (66.7%). Target: 88/102 (86%).

## Excluded Tests (14 — Java Interop, NOT to be fixed)

These require JVM features and are permanently out of scope:
- clojure_xml, evaluation, genclass, java_interop, main
- method_thunks, param_tags, predicates, reflect, rt
- serialization, streams, string, transducers

## Compilation Warnings

Several SBCL compilation warnings are expected (type conflicts with coerce/vector). These are non-blocking.
