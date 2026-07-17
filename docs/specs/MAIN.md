---
stability: aspirational
layer: cross-cutting
audience: both
---

# Common Clojure Behavioral Specifications

> Entry point for the Common Clojure contract: Clojure semantics for new and adapted programs, compiled natively by SBCL. Normative promises live in the focused child specs indexed below.

## Source files

- `AGENTS.md` — current canonical agent contract and native architecture guardrails.
- `README.md` — current human-facing mission, status, and project orientation.
- `cl-clojure-syntax.lisp` — existing reader experiment; evidence only.
- `cl-clojure-eval.lisp` — legacy tree-walking evaluator that conforming artifacts must not depend on.
- `cl-clojure.asd` — existing legacy system composition.
- `clojure-tests/` — upstream-derived reference evidence, not the product North Star.

---

## 1. Frame

1.1 This corpus is the canonical behavioral contract for the replacement Common Clojure implementation.

1.2 The intended reader is an implementer or reviewer who must be able to reconstruct observable semantics without treating legacy code as architecture.

1.3 Each child spec restarts numbering at §1.1. Cross-references name both the owning file and stable section handle.

---

## 2. Orientation path

2.1 Begin with [language-and-compatibility.md §1.1](language-and-compatibility.md), [value-model.md §1.1](value-model.md), and [compiler-and-artifacts.md §1.1](compiler-and-artifacts.md).

2.2 Continue through reader, names, functions, collections, and type dispatch before consulting interop or library adaptations.

2.3 Use [native-vertical-slice.md §1.1](native-vertical-slice.md) as the first end-to-end conformance target, and [legacy-migration.md §1.1](legacy-migration.md) when deciding whether existing code is reusable.

---

## 3. Sub-specs

3.1 [Language and compatibility](language-and-compatibility.md) — mission, commitment tiers, compatibility limits, and explicit non-goals.

3.2 [Reader and forms](reader-and-forms.md) — lossless semantic reading, spans, form identity, tagged literals, and read failures.

3.3 [Value model](value-model.md) — nil, booleans, empty list, truthiness, equality, hashing, and numeric values.

3.4 [Compiler and artifacts](compiler-and-artifacts.md) — sessions, shared native pipeline, modules, FASLs, runtime ABI, and adapters.

3.5 [Namespaces, Vars, and macros](namespaces-vars-and-macros.md) — resolution, stable Vars, dynamic binding, macro staging, and syntax quote.

3.6 [Functions and control flow](functions-and-control-flow.md) — invocation, arities, closures, special forms, exceptions, and native recur.

3.7 [Collections and sequences](collections-and-sequences.md) — persistent collections, sequence behavior, laziness, transients, and collection calls.

3.8 [Protocols, types, and metadata](protocols-types-and-metadata.md) — protocols, records, reification, multimethods, and metadata.

3.9 [Common Lisp interop](common-lisp-interop.md) — explicit host bridges, value conversion, CLOS integration, and condition crossing.

3.10 [Diagnostics](diagnostics.md) — typed source-located failures, expansion traces, atomicity, and runtime source mapping.

3.11 [Performance](performance.md) — performance tiers, measurement controls, native-code gates, and permitted semantic costs.

3.12 [Library adaptation](library-adaptation.md) — milestone contracts for core.match, spec.alpha, O'Doyle Rules, and core.async.

3.13 [Legacy migration](legacy-migration.md) — isolation of the tree walker, use of upstream tests as evidence, and reuse rules.

3.14 [Native vertical slice](native-vertical-slice.md) — the first executable proof of the approved architecture and semantics.

---

## Open / Deferred

- Corpus-wide questions are recorded in the focused spec that owns the affected behavior; this entry file intentionally owns none.
