---
stability: aspirational
layer: cross-cutting
audience: both
---

# Compiler and Artifacts

> Spec: the native compilation session, shared source pipeline, module artifacts, runtime ABI, and development adapters. Counterpart to [MAIN.md](MAIN.md).
> See also [diagnostics.md](diagnostics.md), [performance.md](performance.md), and [namespaces-vars-and-macros.md](namespaces-vars-and-macros.md).

## Source files

- `cl-clojure.asd` — legacy system composition to be replaced; evidence only.
- `cl-clojure-eval.lisp` — forbidden runtime evaluator dependency.
- `run-tests.lisp` — legacy file-loading harness and evidence of phase ambiguity.
- `sbcl-init.lisp` — existing SBCL compiler policy experiment.

---

## 1. Frame

1.1 Common Clojure is implemented as a compiler to Common Lisp forms and SBCL native artifacts, not as a tree-walking evaluator.

**Why:** Native SBCL execution is the defining architecture and performance contract.

1.2 Interactive evaluation, ahead-of-time compilation, ASDF compilation, and any embedding adapter share one compiler module and one semantic pipeline.

**Why:** Separate paths would drift in macro timing, name resolution, diagnostics, and runtime behavior.

1.3 The compiler's analyzed representation is private and closed to application code.

**Why:** The representation must evolve freely as semantics and optimization mature; ordinary macros, protocols, tagged literals, and CL bridges provide the approved extension surfaces.

---

## 2. Compiler ontology and seam

2.1 A **compilation session** owns source roots, namespace and macro state, compiler policy, source caches, and the runtime ABI target for one coherent development/build context.

2.2 A **module** is an ordered Common Clojure source unit with a declared namespace and dependencies.

2.3 A **native artifact** is an SBCL-loadable FASL plus enough manifest information to reject an incompatible runtime before executing module initialization.

2.4 The public compiler seam has three behavioral roles: construct a session, compile a source file to a native artifact, and compile then execute interactive source text. Exact package and function names are not frozen by this spec.

**Why:** Three roles cover AOT, REPL, ASDF, tests, and embedding without exposing phase internals.

2.5 A compilation session is stateful and MUST serialize mutations to its namespace and macro world. Independent sessions MAY compile concurrently when they do not publish into the same runtime world.

---

## 3. Shared native pipeline

3.1 Every execution surface follows this ordered pipeline:

1. source text;
2. semantically lossless forms and source spans;
3. sequential execution of native macro expansion;
4. semantic analysis into a private closed representation;
5. emission of Common Lisp forms;
6. SBCL `compile` or `compile-file`;
7. native function invocation or FASL load.

**Why:** The order makes phase behavior explicit and excludes runtime interpretation.

3.2 No successful path MAY skip semantic analysis, execute a legacy evaluator, retain interpreted closure bodies, or fall back to walking an AST at runtime.

3.3 Compile-time macro functions are themselves SBCL-compiled functions. A module processes top-level forms in source order so a macro defined earlier can expand a later form as specified by [namespaces-vars-and-macros.md §5.2](namespaces-vars-and-macros.md).

3.4 Higher-level language forms SHOULD be ordinary Common Clojure macros over the small kernel in [functions-and-control-flow.md §4.1](functions-and-control-flow.md), not additional evaluator cases.

3.5 SBCL is called directly. The compiler MUST NOT introduce a generic backend interface until a second backend becomes an approved requirement.

---

## 4. File compilation and FASLs

4.1 File compilation reads all module forms, resolves and compiles required modules, stages compile-time definitions, emits Common Lisp, and invokes SBCL `compile-file` to produce the requested artifact.

4.2 A successful artifact MUST contain SBCL-compiled definitions and module initialization data. It MUST NOT contain source ASTs for execution, evaluator environments, interpreted closure bodies, or calls to the legacy evaluator.

4.3 Loading a normal application artifact MAY depend on the Common Clojure runtime but MUST NOT require the compiler, reader, test harness, or legacy evaluator to be loaded.

**Why:** Deployed code should carry only the runtime semantics it uses, not development machinery.

4.4 Artifact creation MUST use a temporary output and publish the target path only after successful compilation. An existing good artifact remains intact after failure.

4.5 Each artifact MUST declare its Common Clojure runtime ABI, relevant compiler ABI, and SBCL FASL compatibility identity. A mismatch MUST be detected before module-owned namespace or Var initialization runs.

4.6 Loading a module registers its namespace and definitions in dependency order. Compiler-owned publication is atomic as bounded by [diagnostics.md §4.1](diagnostics.md).

---

## 5. Interactive evaluation

5.1 Interactive source text is read into ordered top-level forms; each form is macroexpanded, analyzed, emitted, SBCL-compiled into a native thunk, and invoked before the next form begins.

5.2 Definitions and macro state from each successful top-level form are visible to subsequent forms in the same input and session.

5.3 If a later top-level form fails, effects of earlier successful forms remain visible. The failing form MUST NOT publish a partial definition.

**Why:** Interactive development is sequential and inspectable, while per-form atomicity prevents corrupt definitions.

5.4 A function value returned or installed by interactive evaluation MUST satisfy Common Lisp `compiled-function-p` unless it is an explicitly declared host callable adapter.

5.5 A language-level `eval`, if provided, MUST delegate to the current native compilation session and MUST NOT introduce an interpreter.

---

## 6. Adapters and reproducibility

6.1 The REPL is a thin reader/printer adapter over interactive evaluation. ASDF is a thin component adapter over file compilation and standard artifact load.

6.2 An embedding macro MAY expose Common Clojure forms inside Common Lisp source, but it MUST delegate to the same analyzer and emitter and MUST NOT become a second language implementation.

6.3 Given identical source bytes, dependency artifacts, session policy, runtime ABI, and SBCL build, compilation SHOULD produce behaviorally equivalent artifacts. Generated internal names MUST be deterministic except where hygiene requires fresh identities.

6.4 Debug output MAY expose emitted Common Lisp and source maps for inspection, but those representations are diagnostics rather than a stable public compiler interface.

---

## Open / Deferred

- **What exact public package and function names should realize the three compiler roles?** Settle alongside the first vertical slice without expanding the role count.
- **Should artifact bytes be reproducible, or only artifact behavior and manifests?** SBCL may embed build-local data; measure before promising byte identity.
- **How should sessions isolate simultaneous test worlds that declare the same namespace names?** The first implementation may serialize them, but the long-term behavioral need is unsettled.
