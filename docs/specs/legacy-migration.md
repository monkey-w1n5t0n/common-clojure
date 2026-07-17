---
stability: aspirational
layer: cross-cutting
audience: implementer
---

# Legacy Isolation and Migration

> Spec: how the existing tree walker, reader experiment, upstream tests, and alternate compiler branch may inform—but never define—the native replacement. Counterpart to [MAIN.md](MAIN.md).
> See also [language-and-compatibility.md](language-and-compatibility.md), [compiler-and-artifacts.md](compiler-and-artifacts.md), and [native-vertical-slice.md](native-vertical-slice.md).

## Source files

- `cl-clojure-eval.lisp` — legacy tree-walking evaluator and semantic notebook.
- `cl-clojure-syntax.lisp` — legacy reader experiment.
- `cl-clojure-case.lisp` — evaluator-specific special-form implementation.
- `cl-clojure-transducers.lisp` — legacy runtime algorithms tied to evaluator types.
- `tests.lisp` — legacy Common Lisp tests.
- `clojure-tests/` — upstream-derived reference corpus.
- `cl-clojure.asd` — legacy system dependency graph.

---

## 1. Frame

1.1 Migration replaces the runtime execution architecture rather than incrementally disguising the tree walker behind compiled wrappers.

**Why:** Retaining evaluator environments, interpreted closures, or recursive form dispatch would preserve the central problem while making it harder to see.

1.2 Existing code and branches are evidence of syntax cases, semantic questions, and failed assumptions. They are not compatibility contracts and do not outrank this corpus.

---

## 2. Isolation rules

2.1 The conforming compiler and runtime systems MUST NOT depend on, load, or call `cl-clojure-eval.lisp` or evaluator-specific helper modules.

2.2 Application FASLs MUST load and run in a fresh SBCL process containing the conforming runtime but not the reader, compiler, legacy evaluator, or legacy test harness.

2.3 Legacy commands MAY remain available as explicitly named comparison/oracle tools while migration is active, but their packages, globals, caches, and namespaces MUST be isolated from conformance tests.

2.4 No unsupported native form may delegate to the legacy evaluator. Unsupported behavior signals the typed phase failure owned by [diagnostics.md §2.1](diagnostics.md).

2.5 A temporary adapter between legacy data and conforming values MUST have a named test consumer and removal condition; it MUST NOT become a second runtime representation accepted implicitly everywhere.

---

## 3. Reuse policy

3.1 Reader delimiter techniques, syntax examples, pure algorithms, and focused tests MAY be transplanted only after their outputs are rewritten to satisfy [reader-and-forms.md §2.1](reader-and-forms.md) and [value-model.md §2.1](value-model.md).

3.2 An old runtime function MAY be reused only when it is independent of evaluator environments and interpreted closures, has behavior pinned at the new public seam, and uses conforming values and failures.

3.3 Environment lookup fallbacks, hard-coded namespaces, Java emulation tables, arbitrary lazy realization limits, assertion stubs, and “return nil” compatibility placeholders MUST NOT migrate.

3.4 The `feat/clojure-on-common-lisp` branch is a design sketch, not a merge base. Its file decomposition and tests MAY inform work, but compiler, reader, namespace, and collection implementations require independent conformance review before any selective transplant.

**Why:** That branch points toward CL emission but contains malformed symbol lowering, incomplete recur/macro behavior, and whole-collection copying.

3.5 Migration SHOULD proceed by building a new compiler/runtime system alongside the isolated legacy system until the native vertical slice passes, then moving conformance surfaces rather than modifying the evaluator into the compiler.

---

## 4. Upstream test evidence

4.1 Each imported upstream test is classified as host-independent semantic evidence, adaptable JVM-coupled evidence, or out of scope.

4.2 A host-independent assertion MAY become a conformance test only after its setup and assertion mechanism execute correctly and its behavior is owned by a spec claim.

4.3 JVM class, reflection, bytecode, Java concurrency, serialization, or implementation-specific exception assertions do not become requirements by presence in the corpus.

4.4 Test reporting distinguishes assertions from file loading as required by [diagnostics.md §5.2](diagnostics.md); no aggregate “files passed” count represents language correctness.

---

## 5. Cutover evidence

5.1 The first cutover gate is complete only when every acceptance item in [native-vertical-slice.md §3.1](native-vertical-slice.md) passes through the new public compiler seam.

5.2 Dependency inspection MUST show no compiler or evaluator dependency in the loaded artifact, and native function inspection MUST show compiled functions and a loop back-edge for recur.

5.3 The legacy evaluator remains frozen during comparison. New semantic behavior is implemented only in the conforming compiler/runtime path.

**Why:** Dual active implementations would cause fixes and tests to land on the wrong execution path.

5.4 Legacy removal is a separate authorized operation after every remaining named consumer has moved; migration does not authorize deletion of historical tests, branches, diagnostics, or user-owned untracked files.

---

## Open / Deferred

- **Should the isolated evaluator remain as a long-term differential oracle after native cutover?** Keep it only if it finds real regressions without becoming a maintenance burden.
- **Which pure runtime algorithms are worth salvaging rather than rewriting?** Decide one behavior at a time through seam tests, not by file ownership.
- **When should the legacy ASDF system name be reassigned to the conforming runtime?** The cutover must not make old scripts silently exercise a different incomplete surface.
