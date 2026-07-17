---
stability: aspirational
layer: behavioural
audience: both
---

# Language and Compatibility

> Spec: the product mission, compatibility commitment, and scope of Common Clojure. Counterpart to [MAIN.md](MAIN.md).
> See also [compiler-and-artifacts.md](compiler-and-artifacts.md) and [library-adaptation.md](library-adaptation.md).

## Source files

- `AGENTS.md` — current canonical agent contract and native architecture guardrails.
- `README.md` — current human-facing mission, status, and project orientation.
- `clojure-tests/` — reference examples from upstream Clojure.
- `cl-clojure-eval.lisp` — legacy architecture excluded by this contract.

---

## 1. Frame

1.1 Common Clojure is a language for developing new programs and selectively adapting libraries with Clojure semantics on top of SBCL.

**Why:** The product is a practical native language environment, not a compatibility exercise whose success is measured by arbitrary JVM projects.

1.2 Clojure's data, namespace, function, macro, collection, protocol, and concurrency ideas are the semantic reference unless this corpus explicitly adapts them for Common Lisp.

**Why:** Familiar Clojure meaning is the source of leverage for users and AI-driven development; silent semantic drift would defeat that purpose.

1.3 SBCL is the sole required host and compiler backend. Other Common Lisp implementations are outside the committed surface.

**Why:** A generic backend would add abstraction and constrain optimization without a second implementation requirement.

---

## 2. Product contract

2.1 Conforming source MUST compile to native SBCL code through the pipeline owned by [compiler-and-artifacts.md §3.1](compiler-and-artifacts.md); production execution MUST NOT walk source forms or interpreted closure bodies.

**Why:** Native execution and use of SBCL's compiler infrastructure are defining product behavior, not an implementation preference.

2.2 The language MUST support an interactive development loop, ahead-of-time modules, and ASDF integration without assigning different semantics to those surfaces.

**Why:** Code must not change meaning when moved from exploration into a built project.

2.3 Selective source adaptation, replacement of JVM-specific internals, and small Common Lisp bridge modules are compatible with the mission.

**Why:** The target libraries derive their value from semantics and abstractions, not from preserving Java implementation details.

2.4 A feature is conforming only when observable behavior is pinned by focused acceptance evidence; merely parsing or loading a form is not success.

**Why:** The legacy project counted non-failing loads and inherited stubs, which obscured whether assertions and semantics were real.

---

## 3. Commitment ladder

3.1 **Stable target core** consists of the semantics owned by this corpus: value identity and truthiness, native compilation, namespaces and Vars, macros, functions and recur, persistent collections and sequences, protocol/type dispatch, metadata, explicit CL interop, and typed failures.

**Why:** Implementers need one coherent surface on which new projects and adapted libraries can rely.

3.2 **Compatibility cuts** include familiar `.clj` syntax, common `clojure.core` names, and upstream tests that express host-independent behavior. A cut MAY be adapted or removed when it encodes JVM classes, Java reflection, bytecode, or implementation accidents.

**Why:** Familiarity is valuable, but carrying JVM machinery would work against the SBCL-native mission.

3.3 Every retained compatibility cut MUST have a named consumer or conformance example; speculative shims are not part of the contract.

**Why:** Unowned compatibility paths create parallel semantics and permanent complexity.

3.4 The official Clojure test corpus is reference evidence only. Passing all upstream files is neither necessary nor sufficient for conformance.

**Why:** Many upstream assertions bind to the JVM, while native behavior absent from that suite is central here.

---

## 4. Explicit non-goals

4.1 Common Clojure does not promise JVM bytecode, Java class emulation, Java reflection, Maven compatibility, or unchanged execution of arbitrary Clojure projects.

4.2 Common Clojure does not expose a public compiler IR, compiler-plugin interface, or generic backend protocol.

**Why:** The approved extension surface is ordinary language constructs and explicit host bridges; exposing compiler internals would freeze the least stable part of the system.

4.3 Common Clojure does not provide an interpreter or evaluator fallback when native compilation fails.

**Why:** A fallback would create two execution semantics and allow unsupported forms to appear to work.

4.4 Exact JVM implementation artifacts, exception class names, collection representations, and performance quirks are not compatibility promises unless a focused spec adopts them.

---

## 5. Conformance evidence

5.1 The first required evidence is the end-to-end contract in [native-vertical-slice.md §3.1](native-vertical-slice.md).

5.2 Broader readiness is demonstrated by focused semantic tests plus the adaptation milestones in [library-adaptation.md §3.1](library-adaptation.md), not by a single aggregate file-load count.

5.3 Every test harness MUST distinguish read, expansion, analysis, native compilation, load, execution, and assertion failures.

**Why:** Phase-specific results make unsupported semantics visible and actionable.

---

## Open / Deferred

- **Which additional Clojure libraries should become named compatibility milestones after the first four?** Evidence from real Common Clojure applications should decide.
- **Which familiar `clojure.core` names may be intentionally renamed?** Settle only when a Common Lisp collision or semantic mismatch is demonstrated.
