# Legacy implementation assessment

**Assessed:** 2026-07-17
**Main snapshot:** `e0e365f`
**Compiler-sketch snapshot:** `feat/clojure-on-common-lisp` at `d354b2d`

This document records what was learned from the implementation that predates the native
compiler direction. It is an evidence inventory, not a behavioral specification or a
backlog. Normative behavior belongs in [the spec corpus](specs/MAIN.md), and migration
policy is fixed by the accepted decisions in [`docs/decisions/`](decisions/).

## Main branch

### Reader

`cl-clojure-syntax.lisp` is a 908-line Common Lisp readtable experiment with useful
syntax cases and lexical algorithms. Its current values cannot be the new reader/form
ABI:

- `read-map` constructs mutable CL hash tables (`cl-clojure-syntax.lisp:84-92`).
- reader metadata becomes a synthetic `(with-meta form metadata)` call
  (`cl-clojure-syntax.lisp:139-150`), losing the separation between source information,
  compiler hints, and runtime metadata;
- normalization maps both Clojure `nil` and `false` to CL `nil`
  (`cl-clojure-syntax.lisp:821-833`);
- forms do not carry the complete, stable source spans required for diagnostics and
  macroexpansion traces.

The readtable mechanics, token cases, and error examples are reusable only after their
outputs are recast as lossless Clojure form values.

### Evaluator

`cl-clojure-eval.lisp` is 9,901 lines centered on explicit evaluator state:

- `env` stores Var tables, lexical alists, parent environments, and `letfn` tables
  (`cl-clojure-eval.lisp:65-70`);
- `closure` retains parameters, bodies, captured environments, and macro flags
  (`cl-clojure-eval.lisp:85-92`);
- `clojure-eval` recursively dispatches on source forms
  (`cl-clojure-eval.lisp:9104` onward);
- namespace, loading, Java, concurrency, and testing behaviors include many silent
  stand-ins that return nil or approximate JVM facilities.

That control path directly contradicts native compilation. The file is a semantic
quarry: individual pure algorithms and examples may be ported after receiving tests
against the new runtime seam. The evaluator, environments, interpreted closures, and
stub dispatch are not reusable architecture and must never become a fallback.

### Test harness

`run-tests.lisp` loads the reader and evaluator into one shared global environment, then
counts a `.clj` file as passing when `eval-file` returns without a serious condition.
That is a load-survival scanner, not a Clojure test runner:

- `eval-deftest` eagerly evaluates the body instead of registering and running a test
  (`cl-clojure-eval.lisp:1980-1991`);
- normal `eval-is` evaluates and returns its expression but does not signal or record a
  false assertion (`cl-clojure-eval.lisp:8831-8866`);
- namespace and other unimplemented forms can silently return nil;
- state can leak between files because the environment is reused.

At the 2026-07-17 audit, all 102 vendored `.clj` files could be read and the harness
reported 70 files without serious conditions and 32 with conditions. Those numbers are
useful only for legacy characterization. They make no correctness or native-performance
claim.

## `feat/clojure-on-common-lisp`

The branch points in the right direction—lowering forms to Common Lisp—but remains a
sketch rather than a merge base:

- its top-level path emits a CL form and invokes CL `eval` rather than producing a
  coherent module/FASL artifact;
- symbol resolution relies on CL interning and global tables rather than lexical
  identities plus semantic namespace/Var resolution;
- macroexpansion is effectively incomplete;
- `loop`/`recur` lowering is unfinished and contains invalid control-flow construction;
- value, equality, namespace, and persistent collection semantics are not coherent with
  the approved contract.

Useful evidence includes its proposed separation of compiler concerns, direct-emission
experiments, and tests that expose lowering problems. Ideas must be re-derived behind the
new compiler seam; the branch must not be merged wholesale.

## Reuse boundary

| Asset | Treatment |
|---|---|
| Reader syntax cases and lexical algorithms | Mine into the lossless reader, preserving source spans and new value identities. |
| Individual Clojure corpus assertions | Recast as focused semantic conformance cases where they match the declared platform contract. |
| Pure arithmetic, printing, sequence, or transducer algorithms | Port selectively behind new runtime tests; do not copy evaluator dependencies. |
| Evaluator `env`, `closure`, and recursive dispatch | Reject. No new compiler or runtime system may depend on them. |
| Mutable hash tables/simple vectors exposed as Clojure persistent values | Reject as public representation. |
| Java and namespace stubs | Reject. Adapt explicitly or signal a source-located unsupported-platform condition. |
| Compiler-sketch module names and emitted-form examples | Treat as design prompts only; verify independently against specs and SBCL output. |

## Migration boundary

The native compiler may be built alongside the legacy files long enough to prove the
first vertical slice, but it starts in separate ASDF systems and must have no dependency
edge to `cl-clojure-eval`. No feature may be implemented by routing unsupported forms to
the evaluator. Once the vertical-slice artifact, semantic smoke tests, and native
performance gate pass, the old evaluator path is removed from the shipped system; git
history preserves the experiment.
