---
stability: aspirational
layer: cross-cutting
audience: both
---

# Diagnostics and Failure Atomicity

> Spec: typed source-located failures, macro traces, native source mapping, warnings, and publication atomicity. Counterpart to [MAIN.md](MAIN.md).
> See also [compiler-and-artifacts.md](compiler-and-artifacts.md) and [reader-and-forms.md](reader-and-forms.md).

## Source files

- `run-tests.lisp` — legacy harness showing undifferentiated load success/failure.
- `cl-clojure-eval.lisp` — legacy error strings and swallowed/stubbed behavior to replace.
- `sbcl-init.lisp` — existing SBCL warning policy experiment.

---

## 1. Frame

1.1 Every failure is assigned to the earliest reliable language phase and retains the original host cause when one exists.

**Why:** A reader, expansion, analysis, native compilation, load, runtime, and assertion failure require different corrective action and must never collapse into “file failed.”

1.2 Diagnostics are Common Lisp conditions with machine-readable fields and a human-readable report; callers MAY handle them without parsing strings.

1.3 No unsupported construct is converted to nil, a placeholder value, a warning-only success, or an interpreter fallback.

**Why:** Legacy stubs allowed tests to load while their assertions had no valid semantics.

---

## 2. Typed conditions

2.1 `reader-error` identifies origin, smallest reliable span, reader construct, and cause or expected syntax.

2.2 `macroexpansion-error` identifies macro Var, call-site span, ordered expansion stack, most recent generated form when printable, and original condition.

2.3 `analysis-error` identifies source span, semantic category such as unresolved symbol, invalid binding, arity, illegal recur, or type assertion, and relevant resolved identities.

2.4 `native-compile-error` identifies emitted-definition identity, corresponding Clojure span, compiler policy, and wrapped SBCL condition.

2.5 `artifact-error` distinguishes artifact creation, ABI rejection, dependency resolution, and module initialization failures.

2.6 Runtime language conditions include at least arity, not-callable, not-seqable, lookup/index, protocol missing/ambiguity, multimethod missing/ambiguity, transient state, metadata, assertion, and interop categories.

2.7 Condition type names in this spec describe behavioral categories; exact package-qualified class names are not frozen until the public runtime package is implemented.

---

## 3. Source mapping and reports

3.1 Every diagnostic originating from written or macro-generated code reports the Clojure source name, line, column, and containing span when known.

3.2 Generated code reports both the generated-form location and the nearest written macro call site, with an ordered expansion trace rather than replacing one with the other.

3.3 Runtime stack reports SHOULD show Clojure namespace/function names and source locations alongside optional generated CL frames.

3.4 A compiler report MUST preserve SBCL notes and restarts as nested host detail without exposing generated host names as the only explanation.

3.5 Printing a condition MUST be bounded even when values, environments, causes, or data structures are cyclic or very large.

---

## 4. Failure atomicity

4.1 File compilation stages compiler-owned namespace, Var, type, protocol, and macro publication. A failed module compilation publishes none of that staged state and leaves the previously valid artifact unchanged.

**Why:** Sequential macro staging is necessary inside a module, but failed experiments must not poison later builds.

4.2 Artifact loading checks runtime/compiler ABI compatibility before module initialization. A mismatch publishes no module-owned state.

4.3 Module initialization stages compiler-owned namespace and Var registration until all compiler-generated initialization succeeds. Arbitrary external side effects explicitly performed by user top-level code cannot be rolled back and MUST be identified as outside the atomicity guarantee.

4.4 Interactive evaluation commits after each successful top-level form and preserves earlier successes if a later form fails, as owned by [compiler-and-artifacts.md §5.1](compiler-and-artifacts.md).

4.5 A failing interactive definition MUST leave the prior root and callable fdefinition coherent and visible; it MUST NOT expose a half-installed replacement.

4.6 Cleanup, dynamic binding restoration, and temporary redefinition restoration run on every supported non-local exit.

---

## 5. Warnings and test evidence

5.1 Warnings identify category and source span and remain distinguishable from fatal conditions. Session policy MAY suppress or promote named warning categories but MUST NOT silently suppress errors.

5.2 The conformance harness records separate counts for forms read, forms expanded, definitions analyzed, modules compiled, artifacts loaded, tests executed, assertions passed, assertions failed, and unexpected conditions.

5.3 A false assertion is a failure even when its containing file loaded without a condition.

5.4 Test isolation MUST prevent namespace, Var, macro, hierarchy, multimethod, and dynamic-binding state from changing another test's outcome unless shared state is the behavior under test.

---

## Open / Deferred

- **Which diagnostic fields are serialized for editor and CI integrations?** First stabilize the condition ontology and source spans.
- **How much generated Common Lisp should default reports show?** It is useful for compiler debugging but noise for application authors.
- **Can all compiler-owned module initialization be staged on SBCL without restricting legitimate top-level definitions?** Any exception must be explicit and source-located.
