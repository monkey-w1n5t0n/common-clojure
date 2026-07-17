---
stability: aspirational
layer: cross-cutting
audience: both
---

# First Native Vertical Slice

> Spec: the smallest end-to-end program and evidence set that proves Common Clojure reads, stages macros, compiles, loads, and runs natively on SBCL. Counterpart to [MAIN.md](MAIN.md).
> See also [compiler-and-artifacts.md](compiler-and-artifacts.md), [performance.md](performance.md), and [legacy-migration.md](legacy-migration.md).

## Source files

No conforming implementation source exists yet; this spec defines the first replacement slice.

- `cl-clojure-syntax.lisp` — legacy syntax evidence only.
- `cl-clojure-eval.lisp` — forbidden execution dependency used for negative dependency checks.
- `cl-clojure.asd` — legacy system graph used for before/after dependency inspection.
- `tests.lisp` — legacy tests from which focused assertions may be rewritten.

---

## 1. Frame

1.1 The first vertical slice proves the approved architecture through one interactive session and one loadable source module; it is not a broad feature-count milestone.

**Why:** An end-to-end native proof invalidates the tree-walking foundation before work expands into the full language.

1.2 The slice includes only the semantics needed to prove value distinctions, namespaces and Vars, native functions, sequential macros, calls, conditionals, lexical bindings, loop/recur, diagnostics, FASL loading, and the initial performance gate.

1.3 Persistent collection families, protocols, records, multimethods, and target library adaptations are not prerequisites for this slice except for the minimal form/value structures needed by the reader and macros.

---

## 2. Demonstration module

2.1 The slice MUST compile a source module behaviorally equivalent to:

```clojure
(ns native.slice)

(defmacro unless [test expression]
  (list 'if test nil expression))

(def classify
  (fn* [x]
    (unless x :falsey)))

(def sum-to
  (fn* [^fixnum n]
    (loop* [^fixnum i n
            ^fixnum acc 0]
      (if (zero? i)
        acc
        (recur (dec i) (+ acc i))))))
```

2.2 The macro definition MUST be SBCL-compiled and installed before `classify` expands. The compiled `classify` body MUST contain native conditional code, not a runtime call that walks the macro output.

2.3 Calling `classify` with nil or false returns `:falsey`; calling it with the empty list returns nil because the empty list is truthy.

2.4 Calling `sum-to` with 1,000,000 returns `500000500000` in bounded stack space. Its typed recur path observes simultaneous rebinding and has no steady-state allocation.

---

## 3. Acceptance evidence

3.1 The vertical slice is conforming only when all of the following pass:

1. the reader produces distinct nil, false, true, and empty-list values with source spans;
2. interactive evaluation observes nil and false as falsey, empty list as truthy, and all three as pairwise unequal;
3. a macro defined in one top-level form expands a later form in the same input/session;
4. the demonstration module compiles through the file-compilation role to an SBCL FASL;
5. a fresh SBCL process loads only the conforming runtime and that FASL, with no compiler, reader, legacy evaluator, or test harness loaded;
6. resolved `classify` and `sum-to` values satisfy `compiled-function-p`;
7. AOT and interactive definitions return equal results for the same inputs;
8. `sum-to` satisfies [performance.md §3.1](performance.md), including bounded stack, zero steady-state bytes consed, and the controlled 2.0× comparison;
9. invalid non-tail or wrong-arity recur produces a source-located `analysis-error`, publishes no replacement definition, and produces no new artifact;
10. an artifact runtime-ABI mismatch is rejected before module definitions publish;
11. dependency and call inspection finds no reference to the legacy evaluator or runtime AST-walking entry point.

**Why:** Passing only a return-value test could conceal an interpreted body, broken staging, partial failure, or deployment dependency.

3.2 Assertion evidence MUST be reported by phase using [diagnostics.md §5.2](diagnostics.md), not as a file-load count.

3.3 The native loop inspection MAY use SBCL disassembly or equivalent compiler metadata, but acceptance records the function identity, SBCL build, optimization policy, and evidence of a back-edge.

---

## 4. Shared-pipeline checks

4.1 The interactive and file paths MUST use the same reader semantics, macro functions, analyzer rules, Common Lisp emitter, runtime ABI, and error categories.

4.2 Test instrumentation MAY identify pipeline phase entry, but it MUST NOT require a public IR or backend interface.

4.3 A semantic test is run once through interactive source and once from the loaded FASL. Divergent result, condition type, Var resolution, or macro expansion is a failure.

4.4 REPL and ASDF adapters are accepted only when they delegate to the compiler roles in [compiler-and-artifacts.md §2.4](compiler-and-artifacts.md) without duplicating analysis or emission.

---

## 5. Failure and state checks

5.1 After a failed interactive redefinition, the previous compiled function root and native fdefinition remain callable and coherent.

5.2 After a failed file compilation, the previous good FASL remains byte-for-byte untouched and the session exposes none of the failed module's staged names or macros.

5.3 A macro expansion failure reports the macro Var, written call-site span, expansion stack, and original condition.

5.4 Any observed fallback to nil, warning-only success, placeholder closure, or evaluator call fails the slice even if later assertions happen to pass.

---

## 6. Exit criterion

6.1 Passing this slice authorizes expanding the native path one focused semantic area at a time; it does not declare the legacy corpus or any target library compatible.

6.2 Every subsequent feature must remain reachable through the same compiler seam and preserve the negative dependency and native-function checks from §3.1.

---

## Open / Deferred

- **Should the demonstration use the final public type-hint spelling or a provisional one?** The behavior requires an asserted fixnum path; exact surface syntax can settle with the reader/compiler implementation.
- **Which tooling records the no-evaluator dependency and disassembly evidence in CI?** The evidence is required, but the harness shape is not yet fixed.
